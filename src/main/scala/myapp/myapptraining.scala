package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppTraining
{
	import MyApp._

	def MakePlayMove
	{
		MyActor.queuedExecutor ! ExecutionItem(client="MakePlayMove",code=new Runnable{def run{
			SelectMovesTab
		}})						

		Eval.EvalAll(add=true,callback=MakePlayMoveCallback)
	}

	def Hint
	{
		Eval.EvalAll(add=true,deep=true,callback=HintCallback)
	}

	def MakePlayMoveCallback()
	{
		val playmovefactor=GD("{components}#{playmovefactor}",50.0).toInt
		val playmovelimit=GD("{components}#{playmovelimit}",-butils.MATE_THRESOLD.toDouble).toInt
		MakeEngineMove(playmovefactor,playmovelimit)
	}

	def HintCallback()
	{
		val hintfactor=GD("{components}#{hintfactor}",80.0).toInt
		val hintlimit=GD("{components}#{hintlimit}",-300.0).toInt
		MakeEngineMove(hintfactor,hintlimit)
	}

	def MakeEngineMove(factor:Int=50,limit:Int= -butils.MATE_THRESOLD)
	{
		val bpos=AddPositionToBook(g.report_trunc_fen)

		val sans=bpos.GetSansWithEvalSorted()

		if(sans.length<=0) return

		val filteredsans=sans.filter(x => bpos.entries(x).GetScore >= limit)

		val san=DataUtils.SelectByFactor(factor,filteredsans,addanyway=sans)

		MakeSanMove(san)

		val score=bpos.entries(san).GetScore

		MyActor.queuedExecutor ! ExecutionItem(client="Eval.MakeSanMove",code=new Runnable{def run{
			if(IsAnyTraining) ShowTrainingScore(-score)

			Update
		}})
	}

	def TrainControlVboxClicked
	{
		GetMyCheckBox("{trainingmode}").SetAndStoreChecked(false)
		GetMyCheckBox("{buildtrainingmode}").SetAndStoreChecked(false)

		Update
	}

	def BuildTrainingMove
	{
		// first check if there is an active engine
		EngineManager.enginelist ! HasLoadedEnginesMsg(noenginescallback=NoEnginesCallback,hasenginescallback=HasEnginesCallback)

		def NoEnginesCallback()
		{
			Builder.SystemPopUp("Training error","""<font color="red"><b>No loaded engine.</b></font>""")				
		}

		def HasEnginesCallback()
		{
			val fen=g.report_fen
			val tfen=g.report_trunc_fen
			val b=new board
			b.set_from_fen(fen)
			b.genMoveList
			val bpos=book.add_position(tfen)
			for(san <- b.move_list_sans)
			{
				val entry=bpos.add_move(san)
				entry.annot="??"
			}
			val vsans=bpos.GetVirginSans(book)
			if(vsans.length>0)
			{
				val san=vsans(0)

				MakeSanMove(san)

				MyActor.queuedExecutor ! ExecutionItem(client="BuildTrainingMove",code=new Runnable{def run{
					Update
				}})									

				val b=new board
				b.set_from_fen(g.report_fen)
				b.genMoveList

				if(b.move_list_sans.length<=0)
				{
					SystemPopUp("Training message","""<font color="green"><b>Training problem solved.</b></font>""")				

					return
				}

				MyActor.queuedExecutor ! ExecutionItem(client="BuildTrainingMove",code=new Runnable{def run{
					SelectBookTab
				}})									

				Eval.EvalAll(add=true,deep=true)

				return
			}

			MyActor.queuedExecutor ! ExecutionItem(client="BuildTrainingMove",code=new Runnable{def run{
				SelectMovesTab
			}})

			MakeTrainingMove
		}
	}

	def MakeTrainingMove
	{
		val san=g.current_node.genSan

		val ppos=AddPositionToBook(g.current_node.genTruncFen)

		var psans=ppos.GetSansWithAnnotSorted()

		if(psans.length<=0)
		{
			g.back

			LookUpSolution()

			psans=ppos.GetSansWithAnnotSorted()

			g.forward

			MyActor.queuedExecutor ! ExecutionItem(client="MakeTrainingMove",code=new Runnable{def run{
				Update
			}})
		}

		val hasmove=psans.contains(san)		

		val bpos=AddPositionToBook(g.report_trunc_fen)

		var sans=bpos.GetSansWithAnnotSorted()

		if(sans.length<=0)
		{
			LookUpSolution()

			sans=bpos.GetSansWithAnnotSorted()

			MyActor.queuedExecutor ! ExecutionItem(client="MakeTrainingMove",code=new Runnable{def run{
				Update
			}})
		}

		if(sans.length<=0)
		{
			if(!hasmove)
			{
				SystemPopUp("Training message","""<font color="red"><b>Sorry, no moves for this position.</b></font>""")

				g.back

				MyActor.queuedExecutor ! ExecutionItem(client="MakeTrainingMove",code=new Runnable{def run{
					Update
				}})

				return
			}
			else
			{
				SystemPopUp("Training message","""<font color="green"><b>Training problem solved.</b></font>""")

				MyActor.queuedExecutor ! ExecutionItem(client="MakeTrainingMove",code=new Runnable{def run{
					Update
				}})

				return
			}
		}

		val r=(new scala.util.Random).nextInt(sans.length)

		val tsan=sans(r)

		g.makeSanMove(tsan)

		MyActor.queuedExecutor ! ExecutionItem(client="MakeTrainingMove",code=new Runnable{def run{
			SelectMovesTab

			Update
		}})		
	}

	def HideTrainingScore
	{
		if(HasStage("{showscoredialog}")) CloseStage("{showscoredialog}")
	}

	def ShowTrainingScore(score:Int)
	{
		val blob=s"""
			|<vbox>
			|<webview id="{showscorewebview}"/>
			|</vbox>
		""".stripMargin

		if(!HasStage("{showscoredialog}")) MyStage(s"{showscoredialog}","Training score",blob,
					modal=false,usewidth=true,useheight=true,handler=handler)

		val color=butils.get_score_color(score)

		val content=s"""
			|Training score:&nbsp;<font size="5" color="$color">&nbsp;&nbsp;&nbsp;<b>$score</b></font>
		""".stripMargin

		LoadWebContent("{showscorewebview}",content)

		GetStage("{showscoredialog}").ToFront
	}

	def AddToTraining
	{
		val fen=g.report_fen

		val added=DataUtils.AddToPersistentStringList(trainingpath,fen)

		if(!added) SystemPopUp("Training message","""<font color="red"><b>Position already added to training.</b></font>""") else
			SystemPopUp("Training message","""<font color="green"><b>Position added to training.</b></font>""")

		prev_training_fen=fen
	}

	def DeleteFromTraining
	{
		val fen=g.report_fen

		val deleted=DataUtils.DeleteFromPersistentStringList(trainingpath,fen)

		if(!deleted) SystemPopUp("Training message","""<font color="red"><b>Position did not belong training.</b></font>""") else
			SystemPopUp("Training message","""<font color="green"><b>Position deleted from training.</b></font>""")
	}

	def TrainingBack
	{
		if(prev_training_fen==null) TrainingRandom

		g.set_from_fen(prev_training_fen)

		if(IsTrainingMode) SelectMovesTab

		Update
	}

	def TrainingRandom
	{
		val fen=DataUtils.GetRandomStringFromPersistentStringList(trainingpath,excluding=prev_training_fen)

		if(fen==null)
		{
			SystemPopUp("Training message","No training positions yet.")
			return
		}

		g.set_from_fen(fen)		
		prev_training_fen=fen

		if(IsTrainingMode) SelectMovesTab

		Update
	}
}