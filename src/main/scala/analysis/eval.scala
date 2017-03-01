package smartchess

////////////////////////////////////////////////////////////////////

import javafx.application._
import javafx.stage._
import javafx.scene._
import javafx.scene.layout._
import javafx.scene.control._
import javafx.scene.canvas._
import javafx.scene.input._
import javafx.scene.paint._
import javafx.scene.text._
import javafx.scene.web._
import javafx.scene.image._
import javafx.event._
import javafx.geometry._
import javafx.beans.value._
import javafx.collections._

import collection.JavaConversions._

import java.io._
import scala.io._

import Builder._

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

import org.apache.commons.lang.time.DurationFormatUtils.formatDuration
import org.apache.commons.lang.time.DateFormatUtils._

////////////////////////////////////////////////////////////////////

case class EvaluateMsg(
	val san:String,
	val depth:Int,
	val sans:List[String]=List[String]()
){}

case class SearchmovesMsg(
	val depth:Int,
	val sans:List[String]=List[String](),
	val num:Int=1,
	val index:Int=0,
	val searchmoves:List[String]
){}

case class EvaluationResultMsg(
	val san:String,
	val score:Int,
	val depth:Int
){}

case class EvaluationResultMsgSearchmoves(
	val san:String,
	val score:Int,
	val depth:Int,
	val algeb:String
){}

case object EvaluatorTick
case object EvaluatorTickSearchmoves

case class Evaluator() extends Actor
{
	var scheduler: Cancellable = null

	var em:EvaluateMsg = null

	var sm:SearchmovesMsg = null

	def maxtime=Builder.GD("{components}#{evaltimeout}",60.0).toInt

	def IsTimeOut:Boolean = if( ( !usetimeout ) || ( timer == null ) ) false else ( timer.elapsed > maxtime )

	def el = if(timer!=null) s"<small>[ timeout %.0f / $maxtime ]</small>".format(timer.elapsed) else ""

	def Evaluate
	{
		if(em==null) return

		val san=em.san
		val depth=em.depth		
		val sans=em.sans

		val i=sans.indexOf(san)+1
		val l=sans.length

		Eval.UpdateLog(s"""
			|Evaluating <b><font color="blue" size="4">$san</font></b> / 
			|<font color="purple">${DataUtils.TrimNice(sans,10).mkString(" ")}</font>
			|<br>
			|<b><font color="blue" size="4">$i</font></b> / <font color="purple">$l</font>, 
			|current score <font color="green" size="3">${-EngineManager.GetCurrentScoreNumerical}</font> , 
			|depth <font color="red" size="3">${EngineManager.GetCurrentDepth} / ${em.depth}</font></b> 
			|$el
		""".stripMargin
		)

		val b=new board
		b.set_from_fen(MyApp.g.report_fen)
		b.genMoveList
		
		if((b.status==board.IS_MATE)||(b.status==board.IS_STALEMATE))
		{
			Eval.UndoMove

			var score=0
			if(b.status==board.IS_MATE)
			{
				if(board.variant=="Antichess") score = -butils.MATE_SCORE else score = butils.MATE_SCORE
			}
			val depth=0
			val bestmove960=""

			scheduler.cancel()
			scheduler=null

			Eval.evalmanager ! EvaluationResultMsg(san=em.san,score=score,depth=depth)
		}

		if(IsReady(depth))
		{
			Eval.UndoMove

			val score= -EngineManager.GetCurrentScoreNumerical
			val depth=EngineManager.GetCurrentDepth
			val bestmove960=EngineManager.GetCurrentBestmove960			

			scheduler.cancel()
			scheduler=null			

			Eval.evalmanager ! EvaluationResultMsg(san=em.san,score=score,depth=depth)
		}
	}

	var olddepth=0

	def IsReady(reqdepth:Int):Boolean=
	{
		val cas=EngineManager.access_cas()

		if(cas==null) return false

		val bestmove960=cas.bestmove960
		val bestmoveok=MyApp.g.b.isAlgebLegal(bestmove960)		

		if(!bestmoveok)
		{			
			return false
		}

		if(cas.depth>olddepth)
		{
			olddepth=cas.depth
			if(usetimeout) timer=new Timer()
		}

		if(IsTimeOut) return true

		cas.depth>=reqdepth
	}

	def EvaluateSearchmoves
	{
		if(sm==null) return

		val depth=sm.depth		
		val sans=sm.sans
		val num=sm.num
		val index=sm.index

		val ewhat=if(sans.length==0) "none" else sans.mkString(" ")

		Eval.UpdateLog(s"""
			|Searching for move excluding <font color="purple">$ewhat</font>
			|<br>
			|Move <b><font color="blue" size="4">$index</font></b> / <font color="purple">$num</font> is
			|<b><font color="blue" size="4">${EngineManager.GetCurrentBestmoveSan}</font></b> , 
			|score <font color="green" size="3">${-EngineManager.GetCurrentScoreNumerical}</font> , 
			|depth <font color="red" size="3">${EngineManager.GetCurrentDepth} / ${sm.depth}</font></b> 
			|$el
		""".stripMargin
		)

		if(IsReady(depth))
		{
			val score=EngineManager.GetCurrentScoreNumerical
			val depth=EngineManager.GetCurrentDepth
			val bestmove960=EngineManager.GetCurrentBestmove960			
			val bestmovesan=EngineManager.GetCurrentBestmoveSan
			scheduler.cancel()
			scheduler=null			
			Eval.evalmanager ! EvaluationResultMsgSearchmoves(san=bestmovesan,score=score,depth=depth,algeb=bestmove960)
		}
	}

	var timer:Timer = null

	def usetimeout = Builder.GB("{components}#{useevaltimeout}",false)

	def init_scheduler(msg:Any)
	{
		if(usetimeout) timer=new Timer()
		olddepth=0
		scheduler = context.system.scheduler.schedule(
			initialDelay = 250 milliseconds,
			interval = 250 milliseconds,
			receiver = self,
			message = msg
		)
	}

	def receive =
	{
		case setem:EvaluateMsg =>
		{
			em=setem

			Eval.MakeSanMove(em.san)

			EngineManager.enginelist ! StartAllMsg()

			init_scheduler(EvaluatorTick)
		}

		case EvaluatorTick => Evaluate

		case setsm:SearchmovesMsg =>
		{
			sm=setsm

			EngineManager.enginelist ! StartAllMsg(searchmoves=sm.searchmoves)

			init_scheduler(EvaluatorTickSearchmoves)
		}

		case EvaluatorTickSearchmoves => EvaluateSearchmoves

		case _ => println("that was unexpected")
	}

	override def postStop():Unit=
	{
		if(scheduler!=null) scheduler.cancel()
	}
}

case class EvalJobItem(
	var done:Boolean=false,
	var score:Int=0,
	var depth:Int=0
)
{	
}

case class InitEvalJobMsg(
	val sans:List[String],
	val depth:Int,
	val remalgebs:List[String]=null,
	val num:Int=1,
	val callback:()=>Unit=null
)
{	
}

case object AbortEvalJobMsg

case class EvalManager() extends Actor
{	
	var depth=0
	var sans=List[String]()
	var num:Int=1
	var items=Map[String,EvalJobItem]()
	var index=0
	var evaluator:ActorRef=null
	var roottfen:String=null
	var remalgebs:List[String]=null
	var callback:()=>Unit=null

	def IsRunning = (evaluator!=null)

	def AllDone:Boolean =
	{
		for((san,item) <- items) if(!item.done) return false
		true
	}

	def CurrentSan:String = sans(index)

	def SendEvaluateMsgForIndex
	{
		EngineManager.enginelist ! AwaitStopAllAndSendMessageMsg(
			evaluator,EvaluateMsg(san=CurrentSan,depth=depth,sans=sans))
	}

	def SendEvaluateMsgForIndexSearchmoves
	{
		if(remalgebs.length<=0)
		{
			AbortEvalJob()
			return
		}
		index+=1		
		EngineManager.enginelist ! AwaitStopAllAndSendMessageMsg(
			evaluator,SearchmovesMsg(depth=depth,sans=sans,num=num,index=index,searchmoves=remalgebs))
	}

	def InitEvalJob(ej:InitEvalJobMsg)
	{
		evaluator=Eval.evalsystem.actorOf(Props[Evaluator], name = "Evaluator")

		roottfen=MyApp.g.report_trunc_fen

		depth=ej.depth

		sans=ej.sans

		num=ej.num

		remalgebs=ej.remalgebs

		callback=ej.callback

		MyApp.SaveGameState(name="evalall")

		index=0

		if(remalgebs==null)
		{
			items=(for(san <- sans) yield (san -> EvalJobItem())).toMap

			SendEvaluateMsgForIndex
		}
		else
		{
			items=Map[String,EvalJobItem]()

			SendEvaluateMsgForIndexSearchmoves
		}
	}

	def AbortEvalJob(userabort:Boolean=false)
	{
		Eval.evalsystem.stop(evaluator)
		evaluator=null
		items=Map[String,EvalJobItem]()

		EngineManager.enginelist ! StopAllMsg()

		Eval.CloseEvalAllDialog

		MyApp.LoadGameState(name="evalall")

		roottfen=null

		MyApp.prev_fen=null
		Eval.Update

		EngineManager.enginehtmlupdateallowed=true

		if(userabort) Eval.evalaborted=true

		if(callback!=null) callback()
	}

	def UpdateItems
	{
		val sortedsans=items.keys.toList.sortWith(items(_).score > items(_).score)

		var i=0
		val itemscontent=(for(san <- sortedsans if(items(san).done)) yield {
			val item=items(san)
			i+=1
			s"""
				|<tr>
				|<td align="center">$i.</td>
				|<td align="center"><font color="blue" size="4">$san</font></td>
				|<td align="center"><font color="green" size="3">${item.score}</font></td>
				|<td align="center"><font color="red" size="3">${item.depth}</font></td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val content=s"""
			|<table cellpadding="3">
			|<tr>
			|<td width="40" align="center"><small><i>No.</i></small></td>
			|<td width="80" align="center"><small><i>Move</i></small></td>
			|<td width="80" align="center"><small><i>Score</i></small></td>
			|<td width="80" align="center"><small><i>Depth</i></small></td>
			|</tr>
			|$itemscontent
			|</table>
		""".stripMargin

		Eval.UpdateResults(content)
	}

	def ProcessEvaluationResult(er:EvaluationResultMsg)
	{
		val san=er.san
		val score=er.score
		val actdepth=er.depth

		if((!IsRunning)||(!sans.contains(san)))
		{
			println("dead evaluation result letter")
			return
		}

		items(san).done=true
		items(san).score=er.score
		items(san).depth=actdepth

		UpdateItems

		val bpos=MyApp.AddPositionToBook(roottfen)

		val entry=bpos.add_move(san)

		entry.SetEngineScore(score,actdepth,docolor=Eval.GetColorByScore)

		if(AllDone)
		{
			AbortEvalJob()
		}
		else
		{
			index+=1
			SendEvaluateMsgForIndex
		}
	}

	def ProcessEvaluationResultSearchmoves(ers:EvaluationResultMsgSearchmoves)
	{	
		val san=ers.san
		val score=ers.score
		val actdepth=ers.depth
		val algeb=ers.algeb

		val bpos=MyApp.AddPositionToBook(roottfen)

		val entry=bpos.add_move(san)

		entry.SetEngineScore(score,actdepth,docolor=Eval.GetColorByScore)

		remalgebs=remalgebs.filter(x => x!=algeb)

		items+=(san -> EvalJobItem(done=true,score=score,depth=actdepth))

		UpdateItems

		if(index>=num)
		{
			AbortEvalJob()
		}
		else
		{
			SendEvaluateMsgForIndexSearchmoves
		}
	}

	def receive =
	{		case ej:InitEvalJobMsg => InitEvalJob(ej)

		case AbortEvalJobMsg => AbortEvalJob(userabort=true)

		case er:EvaluationResultMsg => ProcessEvaluationResult(er)

		case ers:EvaluationResultMsgSearchmoves => ProcessEvaluationResultSearchmoves(ers)

		case _ => println("that was unexpected")
	}
}

object Eval extends Module
{	
	var evalsystem:ActorSystem=null

	var evalmanager:ActorRef=null

	def GetDepth = Builder.GD("{components}#{evaldepth}",20.0).toInt
	def GetBonus = Builder.GD("{components}#{evalbonus}",5.0).toInt
	def GetColorByScore = Builder.GB("{components}#{colorbyscore}",false)

	def MakeSanMove(san:String)
	{
		MyApp.g.makeSanMove(san)

		Update
	}

	def UndoMove
	{
		MyApp.g.delete

		Update
	}

	def Update
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Eval.MakeSanMove",code=new Runnable{def run{
			MyApp.Update
			val mainboard=MyApp.GetMainBoard
			mainboard.clear_engine
			mainboard.clear_score
		}})		
	}

	def GetSans(numberlimit:Int = -1,belowdepthlimit:Int = -1):List[String] =
	{	
		if(numberlimit == -1)
		{
			val b=new board
			b.set_from_fen(MyApp.g.report_fen)
			b.genMoveList
			b.move_list_sans
		}
		else
		{			
			val bpos=MyApp.AddPositionToBook(MyApp.g.report_trunc_fen)	
			bpos.TopShallowSans(belowdepthlimit=belowdepthlimit,numberlimit=numberlimit)
		}
	}

	def GetEvaledSans:List[String] =
	{
		val bpos=MyApp.AddPositionToBook(MyApp.g.report_trunc_fen)	
		bpos.TopShallowSans(belowdepthlimit= -1,numberlimit= -1)
	}

	def CloseEvalAllDialog
	{
		if(Builder.HasStage("{evalalldialog}"))
		{
			MyActor.queuedExecutor ! ExecutionItem(client="Eval.CloseEvalAllDialog",code=new Runnable{def run{
				Builder.CloseStage("{evalalldialog}")
			}})			
		}
	}

	def UpdateLog(content:String)
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Eval.UpdateLog",code=new Runnable{def run{
			Builder.LoadWebContent("{evallogwebview}",content)
		}})			
	}

	def UpdateResults(content:String)
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Eval.UpdateResults",code=new Runnable{def run{
			Builder.LoadWebContent("{evalresultswebview}",content)
		}})			
	}

	var sline:String=null

	def add_node_recursive(depth:Int = 0,line:List[String]=List[String]()):Boolean =
	{
		val maxdepth=Builder.GD("{components}#{minimaxdepth}",50.0).toInt

		if(depth>maxdepth) return false

		val b=new board
		b.set_from_fen(MyApp.g.report_fen)		
		b.genMoveList

		val sans=b.move_list_sans

		if(sans.length<=0) return false

		val bpos=MyApp.book.add_position(MyApp.g.report_trunc_fen)

		for(san <- sans) if(!bpos.entries.contains(san))
		{
			sline=line.mkString(" ")
			return true
		}

		val sanswithevalsorted=bpos.GetSansWithEvalSorted
		val filteredsans=sanswithevalsorted.filter(x => !bpos.entries(x).IsMateOrMated)

		if(filteredsans.length<=0)
		{
			return false
		}

		val factor=Builder.GD("{components}#{searchfactor}",80.0).toInt

		val depthbonus=Builder.GD("{components}#{searchdepthbonus}",5.0).toInt

		var realfactor=factor+depth*depthbonus

		if(realfactor>100) realfactor=100

		val selsan=DataUtils.SelectByFactor(realfactor,filteredsans)

		MyApp.g.makeSanMove(selsan)

		add_node_recursive(depth+1,line:+selsan)
	}

	def add_node:Boolean =
	{
		val bpos=MyApp.book.add_position(MyApp.g.report_trunc_fen)

		val sans=bpos.GetSansWithEvalSorted

		if(sans.length>0)
		{
			if(bpos.entries(sans(0)).GetScore > butils.MATE_THRESOLD) return false
		}

		for(i <- 1 to 20)
		{
			if(add_node_recursive())
			{
				MyActor.queuedExecutor ! ExecutionItem(client="Eval.SearchFunc",code=new Runnable{def run{
					Builder.GetMyText("{searchline}").SetText(s"""( $evalcnt / $minimaxafter ) $sline""")
				}})

				EvalAll(callback=AfterEvalAll,modal=false)

				return true
			}
			else
			{
				ToSearchRoot
			}
		}

		Builder.SystemPopUp("Search message","""<font color="red"><b>Could not add node!</b></font>""",3000)

		false
	}

	def ToSearchRoot
	{
		MyApp.LoadGameState("search")
	}

	def CloseSearchStage
	{
		try{Thread.sleep(1000)}catch{case e:Throwable=>}

		MyActor.queuedExecutor ! ExecutionItem(client="Eval.CloseSearchDialog",code=new Runnable{def run{
			Builder.CloseStage("{searchdialog}")

			ToSearchRoot

			MyApp.prev_fen=null

			MyApp.Update
		}})
	}

	def AfterEvalAll()
	{
		if(IsSearchAborted)
		{
			CloseSearchStage
			return
		}	

		ToSearchRoot

		evalcnt+=1

		if(evalcnt>minimaxafter)
		{
			evalcnt=1

			try{Thread.sleep(1000)}catch{case e:Throwable=>}

			Minimax(callback=SearchFunc,modal=false)			
		}
		else
		{
			MyActor.queuedExecutor ! ExecutionItem(client="Eval.SearchFunc",code=new Runnable{def run{
				MyApp.prev_fen = null
				MyApp.Update

				Future
				{
					SearchFunc
				}
			}})
		}
	}

	var searchnodes=0

	def SearchFunc()
	{
		if(IsSearchAborted)
		{
			CloseSearchStage
			return
		}

		ToSearchRoot

		val tfen=MyApp.g.report_trunc_fen

		val bpos=MyApp.book.add_position(tfen)

		val bookcontent=bpos.reportHTML(MyApp.g.current_node.childs.keys.toList,book=MyApp.book,short=true,nostats=true,collectpv=true)

		val content=s"""
			|nodes: $searchnodes
			|<hr>
			|$bookcontent
		""".stripMargin

		searchnodes+=1

		MyActor.queuedExecutor ! ExecutionItem(client="Eval.SearchFunc",code=new Runnable{def run{
			Builder.LoadWebContent("{searchresultwebview}",content)
		}})

		try{Thread.sleep(1000)}catch{case e:Throwable=>}

		if(!add_node)
		{
			CloseSearchStage
			return
		}
	}

	var searchaborted=false
	var evalaborted=false
	var minimaxaborted=false

	def IsSearchAborted = ( searchaborted || evalaborted || minimaxaborted )

	var minimaxafter=1

	var evalcnt=1

	def Search(factor:Int=80)
	{
		searchaborted=false
		evalaborted=false
		minimaxaborted=false

		searchnodes=0

		evalcnt=1

		minimaxafter=GD("{components}#{minimaxafter}",1.0).toInt

		val blob=s"""
					|<vbox>
					|<hbox>
					|<button id="{abort}" text="Abort"/>
					|<label id="{searchline}"/>
					|</hbox>
					|<webview id="{searchresultwebview}"/>
					|</vbox>
				""".stripMargin

		def handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{abort}")
				{
					searchaborted=true
				}
			}
		}

		Builder.MyStage("{searchdialog}","Search",
			modal=true,unclosable=true,usewidth=true,useheight=true,handler=handler,blob=blob)

		MyApp.SaveGameState(name="search")

		Future
		{
			SearchFunc
		}
	}

	def EvalAll(
		numberlimit:Int = -1,
		deep:Boolean=false,
		add:Boolean=false,
		num:Int=1,
		thismove:String=null,
		callback:()=>Unit=null,
		modal:Boolean=true
	)
	{
		evalaborted=false

		EngineManager.enginehtmlupdateallowed=false

		val depth=(if(deep) GetDepth+GetBonus else GetDepth)
		var sans=GetSans(numberlimit,depth)
		if(thismove!=null) sans=List(thismove)
		val evaledsans=GetEvaledSans

		var remalgebs:List[String] = null

		if(add)
		{
			val allsans=GetSans(-1,-1)			
			val remsans=(for(san <- allsans if(!evaledsans.contains(san))) yield san).toList
			val b=new board
			b.set_from_fen(MyApp.g.report_fen)
			remalgebs=remsans.map(x => b.to_true_algeb(b.sanToMove(x).toAlgeb))
			if(remalgebs.length<=0)
			{
				if(callback!=null) callback()
				return
			}
		}
		else
		{
			if(sans.length<=0)
			{
				if(callback!=null) callback()
				return
			}
		}

		val blob=s"""
					|<vbox>
					|<hbox>
					|<button id="{abort}" text="Abort"/>
					|</hbox>
					|<webview id="{evallogwebview}" height="60.0"/>
					|<webview id="{evalresultswebview}"/>
					|</vbox>
				""".stripMargin

		def handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{abort}")
				{					
					Eval.evalmanager ! AbortEvalJobMsg
				}
			}
		}

		MyActor.queuedExecutor ! ExecutionItem(client="EvalAll.openstage",code=new Runnable{def run{
			Builder.MyStage("{evalalldialog}","Eval at depth "+depth,blob,
				modal=modal,unclosable=true,usewidth=true,useheight=true,handler=handler)

			Eval.evalmanager ! InitEvalJobMsg(
				sans=if(add) evaledsans else sans,
				depth=depth,
				remalgebs=remalgebs,
				num=num,
				callback=callback
			)
		}})						
	}

	////////////////////////////////////////////////////////////////////
	// Module interface	

	def Name:String = "Eval"

	def Startup
	{
		evalsystem=ActorSystem("EvalSystem")		
		evalmanager=evalsystem.actorOf(Props[EvalManager], name = "EvalManager")
	}

	def Shutdown
	{
		evalsystem.terminate()
	}

	////////////////////////////////////////////////////////////////////

	var nodes=0
	var rootfen:String=null
	var maxdepth=0
	var actmaxdepth=0
	var mg:game=null
	var lines:scala.collection.mutable.ArrayBuffer[List[String]]=null

	case object MinimaxLogTick

	var minimaxlog:ActorRef = null

	case class MinimaxLog() extends Actor
	{
		var scheduler: Cancellable = null

		var timer:Timer = null

		override def preStart():Unit =
		{
			timer=new Timer()

			scheduler = context.system.scheduler.schedule(
				initialDelay = 500 milliseconds,
				interval = 500 milliseconds,
				receiver = self,
				message = MinimaxLogTick
			)
		}

		def Update(content:String)
		{
			MyActor.queuedExecutor ! ExecutionItem(client="Eval.CloseMinimaxDialog",code=new Runnable{def run{
				Builder.LoadWebContent("{minimaxlogwebview}",content)
			}})			
		}

		def UpdateLog
		{
			val elapsedf=timer.formatted("HH:mm:ss")
			val timemilli=timer.elapsedmilli
			var nps=0.0
			if(timemilli>0) nps=nodes.toDouble/timemilli.toDouble*1000.0
			val npsf="%.1f".format(nps)
			val linescontent=lines.map(x => DataUtils.TrimNice(x,15).mkString(" ")).mkString("<br>\n")
			val contenthead=s"""
				|maxdepth $actmaxdepth elapsed $elapsedf nodes $nodes nps $npsf
				|<hr>
			""".stripMargin
			val content=contenthead+linescontent
			Update(content)
		}

		def receive =
		{
			case MinimaxLogTick => UpdateLog
			case _ => println("that was unexpected")
		}

		override def postStop():Unit =
		{
			if(scheduler!=null) scheduler.cancel()
		}
	}

	def CloseMinimaxDialog
	{
		evalsystem.stop(minimaxlog)

		if(Builder.HasStage("{minimaxdialog}"))
		{
			MyActor.queuedExecutor ! ExecutionItem(client="Eval.CloseMinimaxDialog",code=new Runnable{def run{
				Builder.CloseStage("{minimaxdialog}")
				MyApp.prev_fen=null
				MyApp.Update
			}})			
		}
	}

	var colormoves=false

	var remove=false

	var addtobook:Book=null

	var storeeval:Boolean=true

	var dominimaxlog:Boolean=false

	def Minimax(
		setrootfen:String=MyApp.g.report_fen,
		setmaxdepth:Int=Builder.GD("{components}#{minimaxdepth}",50.0).toInt,
		setcolormoves:Boolean=Builder.GB("{components}#{colorbyscore}",false),
		setremove:Boolean=false,
		setaddtobook:Book=null,
		callback:()=>Unit=null,
		modal:Boolean=true
	)
	{
		colormoves=setcolormoves

		remove=setremove

		addtobook=setaddtobook

		storeeval=( (!remove) && (addtobook==null) )

		dominimaxlog = Builder.GB("{components}#{dominimaxlog}",false)

		val blob=s"""
					|<vbox>
					|<hbox>
					|<button id="{abort}" text="Abort"/>
					|</hbox>
					|<webview id="{minimaxlogwebview}"/>
					|</vbox>
				""".stripMargin

		def handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{abort}")
				{
					minimaxaborted=true
				}
			}
		}

		nodes=0
		lines=scala.collection.mutable.ArrayBuffer[List[String]]()
		rootfen=setrootfen
		maxdepth=setmaxdepth
		if(remove) maxdepth=100
		actmaxdepth=0
		mg=new game
		mg.set_from_fen(rootfen)

		minimaxlog=Eval.evalsystem.actorOf(Props[MinimaxLog], name = "MinimaxLog")

		minimaxaborted=false

		MyActor.queuedExecutor ! ExecutionItem(client="Minimax.openstage",code=new Runnable{def run{
			Builder.MyStage("{minimaxdialog}","Minimax",blob,
				modal=modal,unclosable=true,usewidth=true,useheight=true,handler=handler)		

			Future
			{
				linebuff=scala.collection.mutable.ArrayBuffer[String]()

				MyApp.book.clear_level

				minimax_recursive(0,List[String](),List[String]())

				DataUtils.WriteStringToFile("stuff/mimimaxlog.txt",linebuff.mkString("\n"))

				if((addtobook!=null)&&(!minimaxaborted))
				{
					addtobook.Save
				}

				CloseMinimaxDialog

				if(callback!=null) callback()
			}
		}})					
	}

	var linebuff:scala.collection.mutable.ArrayBuffer[String]=null

	def minimax_recursive(
		depth:Int,
		line:List[String],
		positions:List[String]
	):Int =
	{
		var alpha = -(butils.INFINITE_SCORE)

		if(minimaxaborted) return alpha

		if(depth>maxdepth) return alpha

		if(depth>actmaxdepth) actmaxdepth=depth

		val tfen=mg.report_trunc_fen

		if(positions.contains(tfen)) return 0

		val bpos=MyApp.book.add_position(tfen)

		var sans=bpos.GetSansWithEngineScore()

		if(!storeeval)
		{
			sans=bpos.entries.keys.toList
		}

		if(sans.length<=0)
		{
			if(remove)
			{
				MyApp.book.positions-=tfen
			}
			
			return alpha
		}

		var abpos:BookPosition = null

		if(addtobook!=null)
		{
			abpos=addtobook.add_position(tfen)
		}

		for(san <- sans)
		{
			nodes+=1

			val extendedline=line:+san

			lines+=extendedline

			val extendedpositions=positions:+tfen

			if(lines.length>100) lines.remove(0)

			val entry=bpos.add_move(san)

			val score=entry.enginescore

			if(dominimaxlog)
			{
				linebuff+="%8d %8d | ".format(alpha,score)+extendedline.mkString(" ")
			}

				mg.makeSanMove(san)			

				val nodes0=nodes

				var eval = -minimax_recursive(depth+1,extendedline,extendedpositions)

				val dnodes=nodes-nodes0

				mg.back

			if(eval == butils.INFINITE_SCORE)
			{
				eval=score
			}

			if(storeeval)
			{
				if(entry.comment!="solution")
				{
					if(depth < entry.level)
					{
						entry.SetMinimaxScore(eval,depth,dnodes,docolor=colormoves)						

						entry.level=depth
					}
				}
			}

			if(abpos!=null)
			{
				val aentry=abpos.add_move(san)

				aentry.copy(entry)
			}

			if(eval>alpha) alpha=eval			
		}

		if(remove)
		{
			MyApp.book.positions-=tfen
		}

		alpha
	}

}