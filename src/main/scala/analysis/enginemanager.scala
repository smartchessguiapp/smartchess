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

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import collection.JavaConverters._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

////////////////////////////////////////////////////////////////////

object EngineManager extends Module
{	
	var enginelist:ActorRef=null

	var enginelistsystem:ActorSystem=null

	def Name = "EngineManager"

	var cas:CurrentAnalysisState=null

	def access_cas(update:Boolean=false,newcas:CurrentAnalysisState=null):CurrentAnalysisState =
	{
		this.synchronized
		{
			if(update) cas=newcas
			cas
		}
	}

	var enginehtmlupdateallowed=true

	def GetCurrentDepth:Int =
	{
		val acas=access_cas()
		if(acas==null) return 0
		acas.depth
	}

	def GetCurrentBestmove:String =
	{
		val acas=access_cas()
		if(acas==null) return ""
		acas.bestmove
	}

	def GetCurrentBestmove960:String =
	{
		val acas=access_cas()
		if(acas==null) return ""
		acas.bestmove960
	}

	def GetCurrentBestmoveSan:String =
	{
		val acas=access_cas()
		if(acas==null) return ""
		acas.bestmovesan
	}

	def GetCurrentScoreNumerical:Int =
	{
		val acas=access_cas()
		if(acas==null) return 0
		acas.scorenumerical
	}

	def GetRootFen:String = MyApp.g.report_fen

	def Startup
	{
		enginelist=MyActor.system.actorOf(Props[EngineList], name = "EngineList")

		enginelistsystem=ActorSystem("EngineListSystem")
	}

	def Shutdown
	{
		enginelistsystem.terminate()
	}
}

case class AddEngineMsg(path:String,kind:String="uci")
case class OpenEngineMsg(index:Int)
case class TopEngineMsg(index:Int)
case class UpEngineMsg(index:Int)
case class DownEngineMsg(index:Int)
case class BottomEngineMsg(index:Int)
case class DeleteEngineMsg(index:Int)
case class StartAllMsg(searchmoves:List[String]=null)
case class RestartAllMsg(searchmoves:List[String]=null)
case class StopAllMsg()
case object RootFenChangedMsg
case object LoadMsg
case object SaveMsg
case object UpdateMsg
case class ActiveChangedMsg(index:Int,value:Boolean)
case class ShutDownAllMsg(restartactorsystem:Boolean=true)
case object StartupAllMsg
case object Tick
case class HasLoadedEnginesMsg(noenginescallback:() => Unit,hasenginescallback:() => Unit)

case class AwaitStopAllAndSendMessageMsg(aref:ActorRef,msg:Any)

case class EngineList(
) extends Actor
{
	var engines=scala.collection.mutable.ArrayBuffer[Engine]()

	def path = "stuff/"+board.variant+"/engines.xml"

	def lv=Builder.GetMyEditableListView("{engineslistview}")

	var scheduler:Cancellable=null

	override def preStart():Unit=
	{
		scheduler = context.system.scheduler.schedule(
			initialDelay = 2 seconds,
			interval = 500 milliseconds,
			receiver = self,
			message = Tick
		)
	}

	override def postStop():Unit=
	{
		scheduler.cancel()
	}

	def GetCurrentAnalysisState:CurrentAnalysisState =
	{
		val mainrunningengine=GetRunningEngines(0)

		val cas=mainrunningengine.GetCurrentAnalysisState

		cas
	}

	def GetEnginesByUniqueId:Map[Int,Engine] =
	{
		(for(e <- engines) yield ( e.uniqueid -> e )).toMap
	}

	def CreateUniqueId:Int =
	{
		val eids=GetEnginesByUniqueId

		var i=0

		do
		{
			i+=1
		} while(eids.contains(i))

		i
	}

	def ReportList:List[MyEditableListViewItem] =
	{
		for(e<-engines.toList) yield
		{
			val text=e.getName+" ( "+e.kind+" )"
			MyEditableListViewItem(text,e.autoload)			
		}
	}

	def Update
	{
		if(lv!=null)
		{
			MyActor.queuedExecutor ! ExecutionItem(client="EngineList.Update",code=new Runnable{def run{
				lv.SetItems(ReportList)
				lv.Select(-1)
			}})			
		}
	}

	def toXml=
	{
		val engine_list=(for(e<-engines) yield e.toXml).toList

		{
			<enginelist>				
				{engine_list}
			</enginelist>
		}
	}

	def fromXml(xml:scala.xml.NodeSeq)
	{
		engines=scala.collection.mutable.ArrayBuffer[Engine]()
		
		for(engine_xml<-(xml \ "engine"))
		{
			val engine=Engine()

			engine.fromXml(engine_xml)

			engines=engines:+engine
		}
	}

	def Load
	{
		if(!new java.io.File(path).exists())
		{
			engines=scala.collection.mutable.ArrayBuffer[Engine]()
		}
		else
		{
			val xml=scala.xml.XML.loadFile(path)
			fromXml(xml)
		}

		Update
	}

	def ShutDownAll(restartactorsystem:Boolean=true)
	{
		for(e <- engines) e.ShutDown

		EngineManager.enginelistsystem.terminate()

		if(restartactorsystem) EngineManager.enginelistsystem=ActorSystem("EngineListSystem")
	}

	def StartupAll
	{
		for(e <- engines) e.Startup
	}

	def Save
	{
		scala.xml.XML.save(path,toXml)
	}

	def GetLoadedEngines:List[Engine] =
	{
		for(e <- engines.toList if(e.IsLoaded)) yield e
	}

	def HasLoadedEngines:Boolean = ( GetLoadedEngines.length > 0 )

	def GetRunningEngines:List[Engine] =
	{
		for(e <- GetLoadedEngines if(e.running)) yield e
	}

	def GetOpenedEngines:List[Engine] =
	{
		for(e <- engines.toList if(e.IsConsoleOpen)) yield e
	}

	def StartAll(searchmoves:List[String]=null)
	{
		EngineManager.access_cas(update=true,null)

		val loaded=GetLoadedEngines

		for(e <- loaded)
		{
			e.SmartStart(searchmoves)
		}
	}

	def StopAll
	{
		val loaded=GetLoadedEngines

		for(e <- loaded)
		{
			e.Stop
		}
	}

	var runningatclearbestmovereceived=List[Engine]()

	def ClearBestMoveReceivedAll
	{
		runningatclearbestmovereceived=GetRunningEngines

		for(e <- runningatclearbestmovereceived)
		{
			e.ClearBestMoveReceived
		}
	}

	def AwaitBestMoveReceivedAll
	{
		for(e <- runningatclearbestmovereceived)
		{
			e.AwaitBestMoveReceived
		}
	}

	def AwaitStopAll
	{
		ClearBestMoveReceivedAll

		for(e <- runningatclearbestmovereceived)
		{
			e.Stop
		}

		AwaitBestMoveReceivedAll
	}

	def AwaitStopAllAndSendMessage(aref:ActorRef,msg:Any)
	{
		AwaitStopAll

		aref ! msg
	}

	def RestartAll(searchmoves:List[String]=null)
	{
		AwaitStopAll

		for(e <- runningatclearbestmovereceived)
		{
			e.Start(searchmoves)
		}
	}

	def RootFenChanged
	{
		RestartAll()
	}

	var wasrunning=false

	def DoTick
	{
		val openedengines=GetOpenedEngines

		if(openedengines.length<=0) return

		for(e <- openedengines)
		{
			e.UpdateButtons
			if(e.IsLoaded)
			{
				if((!e.optionsshown)&&(!e.startup))
				{
					e.optionsshown=true
					e.UpdateOptions
					e.UpdateEngineOut
				}
			}
			else
			{
				if(e.optionsshown)
				{
					e.optionsshown=false
					e.ClearEngineSettingsVBox
				}
			}
		}

		val runningengines=GetRunningEngines

		val mainboard=Builder.GetGuiBoard("{mainboard}")

		if(runningengines.length<=0)
		{			
			if((mainboard!=null)&&wasrunning)
			{
				MyActor.queuedExecutor ! ExecutionItem(client="EngineList.DoTick.clear_score",code=new Runnable{def run{										
					mainboard.clear_score
				}})		
			}
			wasrunning=false
			return
		}

		val cas=GetCurrentAnalysisState

		EngineManager.access_cas(update=true,cas)

		if(cas!=null)
		{
			wasrunning=true

			if((mainboard!=null)&&(cas.depth>=5))
			{
				MyActor.queuedExecutor ! ExecutionItem(client="EngineList.DoTick.print_score",code=new Runnable{def run{										
					mainboard.print_score(cas.scorenumerical)
					mainboard.highlight_engine_move(cas.bestmove,cas.scorenumerical)					
				}})		
			}
		}
	}

	def receive =
	{
		case Tick =>
		{
			DoTick
		}

		case sa:StartAllMsg =>
		{
			StartAll(sa.searchmoves)
		}

		case ra:RestartAllMsg =>
		{
			RestartAll(ra.searchmoves)
		}

		case sa:StopAllMsg =>
		{
			StopAll
		}

		case RootFenChangedMsg =>
		{
			RootFenChanged
		}

		case ae:AddEngineMsg =>
		{
			val e=Engine()
			e.path=ae.path
			e.kind=ae.kind						
			e.variant=board.variant
			engines=engines:+e

			e.SetUniqueId(CreateUniqueId)

			Save

			Update
		}

		case oe:OpenEngineMsg =>
		{
			val e=engines(oe.index)

			e.OpenConsole

			Update
		}

		case te:TopEngineMsg =>
		{
			val index=te.index

			val old=engines(index)

			engines.remove(index)

			engines.insert(0,old)

			Save

			Update
		}

		case ue:UpEngineMsg =>
		{
			val index=ue.index

			if( index > 0 )
			{
				val old=engines(index-1)

				engines(index-1)=engines(index)

				engines(index)=old

				Save

				Update
			}
		}

		case de:DownEngineMsg =>
		{
			val index=de.index

			if( index < (engines.length-1) )
			{
				val old=engines(index+1)

				engines(index+1)=engines(index)

				engines(index)=old

				Save

				Update
			}
		}

		case be:BottomEngineMsg =>
		{
			val index=be.index

			val old=engines(index)

			engines.remove(index)

			engines+=old

			Save

			Update
		}

		case de:DeleteEngineMsg =>
		{
			val index=de.index

			val e=engines(index)

			val uniqueid=e.uniqueid

			e.Terminate

			// clear settings
			Builder.Set(Builder.Cve(s"{engineoptions}#{$uniqueid}"),MapData())

			engines.remove(index)

			Save

			Update
		}

		case LoadMsg => Load

		case SaveMsg => Save

		case UpdateMsg => Update

		case sda:ShutDownAllMsg => ShutDownAll(sda.restartactorsystem)

		case StartupAllMsg => StartupAll

		case hle:HasLoadedEnginesMsg => if(!HasLoadedEngines) hle.noenginescallback() else hle.hasenginescallback()

		case ac:ActiveChangedMsg =>
		{
			engines(ac.index).SmartSetAutoLoad(ac.value)
		}

		case as:AwaitStopAllAndSendMessageMsg => AwaitStopAllAndSendMessage(as.aref,as.msg)

		case _ => println("that was unexpected")
	}
}

