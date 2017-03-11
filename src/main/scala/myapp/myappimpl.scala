package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppImpl
{
	import MyApp._

	////////////////////////////////////////////////////////////////////
	// set variant

	def SetVariant(newvariant:String=null)
	{
		if(book!=null) book.Save

		pgnlist.close
		pgnlist.reset
		pgnlist.set_path(null)

		CloseAuxBooks

		EngineManager.enginelist ! ShutDownAllMsg()

		val variant=if(newvariant!=null) newvariant else GetVariant

		Set("{settings}#{variant}",variant)

		board.variant=variant

		movetable.init(variant)

		g = new game

		SetCurrentBook(GetCurentBook,saveold=false)

		EngineManager.enginelist ! LoadMsg

		EngineManager.enginelist ! StartupAllMsg

		OpenAuxBooks()

		val pgnlistpath=GS(Cve("{currentpgnlist}"),"")

		if(pgnlistpath!="")
		{
			pgnlist.set_path(pgnlistpath)
			pgnlist.open
		}

		prev_books=List[String]()

		prev_selbook=""
	}

	def GuiSetVariant(variant:String)
	{
		if(board.SUPPORTED_VARIANTS.contains(variant))
		{
			SetVariant(variant)

			Update
		}
	}

	////////////////////////////////////////////////////////////////////

	def SetGuiFlip(newflip:Boolean=false)
	{
		SetFlip(newflip)

		GetGuiBoard("{mainboard}").SetFlip(newflip)

		Update
	}

	def CheckProfile
	{
		val myhandle=GS("{settings}#{myhandle}")

		val white=g.get_header("White")
		val black=g.get_header("Black")

		if(myhandle==white)
		{
			SetGuiFlip(false)
		}

		if(myhandle==black)
		{
			SetGuiFlip(true)
		}

		if(GB("{components}#{autoselectpgntab}",false))
		{
			SelectPgnTab
		}
	}

	////////////////////////////////////////////////////////////////////

	def ManualMoveMade(san:String)
	{
		if(IsBuildTrainingMode)
		{
			def BuildTrainingMoveCallback()
			{
				ManualMoveMade(san)
			}

			val ppos=AddPositionToBook(g.report_trunc_fen)

			val sans=ppos.GetSansWithEvalSorted()

			val hasmove=sans.contains(san)

			if(!hasmove)
			{
				MyActor.queuedExecutor ! ExecutionItem(client="ManualMoveMade.Update",code=new Runnable{def run{
					Update
				}})				

				Eval.EvalAll(1,deep=true,thismove=san,callback=BuildTrainingMoveCallback)

				return
			}

			val ismate=ppos.entries(san).IsMate

			if(!ismate)
			{
				MyActor.queuedExecutor ! ExecutionItem(client="ManualMoveMade.Update",code=new Runnable{def run{
					Update
				}})				

				return
			}
		}

		MakeSanMove(san)

		MyActor.queuedExecutor ! ExecutionItem(client="ManualMoveMade.Update",code=new Runnable{def run{
			Update
		}})				

		if(IsPlayMode) MakePlayMove else
		if(IsBuildTrainingMode) BuildTrainingMove else
		if(IsTrainingMode) MakeTrainingMove
	}

	def GuiBoardClicked(file:Int)
	{
		if(file>3) Forward else Back

		Update
	}

	////////////////////////////////////////////////////////////////////

	def SetSelectedMainTab(i:Int)
	{
		selectedmaintab=i
		Set("{selectedmaintab}",""+selectedmaintab)
	}

	////////////////////////////////////////////////////////////////////

	def BrowseAllGames
	{
		val pgb=GetPgnGameBrowser

		val allmd5s=DataUtils.getListOfFileNamesWithExt("stuff/"+board.variant+"/games","pgn")

		pgb.load_list(allmd5s)

		pgb.update

		SelectPgnGamesTab
	}

	def AddEngine(kind:String="uci")
	{
		val fc=new FileChooser()

		if(new java.io.File(GetEngineDir).exists)
		{
			fc.setInitialDirectory(new java.io.File(GetEngineDir))
		}

		val f=fc.showOpenDialog(new Stage())

		if(f!=null)
		{
			val dir=f.getParent()

			SetEngineDir(dir)

			val path=f.getPath()

			EngineManager.enginelist ! AddEngineMsg(path,kind)
		}
	}

	////////////////////////////////////////////////////////////////////
	// elementary game operations

	def Reset
	{
		prev_fen=null
		g.reset

		Set(Cve("{savepgnas}#{saveasname}"),"default.pgn")
	}

	def GuiReset
	{
		Reset

		Update
	}

	def MakeSanMove(san:String)
	{
		g.makeSanMove(san)

		AutoAdd
	}

	def MakeAlgebMove(algeb:String)
	{
		g.makeAlgebMove(algeb)

		AutoAdd
	}

	def Delete
	{
		g.delete
	}

	def GuiDelete
	{
		Delete

		Update
	}

	def ToBegin
	{
		g.tobegin
	}

	def GuiToBegin
	{
		ToBegin

		Update
	}

	def Back
	{
		g.back
	}

	def GuiBack
	{
		Back

		Update
	}

	def Forward
	{
		g.forward
	}

	def GuiForward
	{
		Forward

		Update
	}

	def ToEnd
	{
		g.toend
	}

	def GuiToEnd
	{
		ToEnd

		Update
	}

	def Star
	{
		star=g.current_node
	}

	def ToStar
	{
		g.tonode(star)

		Update
	}

	////////////////////////////////////////////////////////////////////
	// game state

	def LoadGameState(
		name:String="current"
	)
	{
		if(new java.io.File(gpath(name)).exists)
		{
			val pgn=DataUtils.ReadFileToString(gpath(name))
			g.set_from_pgn(pgn)
			if(new java.io.File("stuff/currentlinealgeb.txt").exists)
			{
				val currentlinealgeb=DataUtils.ReadFileToString(lpath(name))
				if(currentlinealgeb.length>3)
				{
					val algebs=currentlinealgeb.split(" ")
					for(algeb<-algebs) MakeAlgebMove(algeb)
				}
			}
		}
	}

	def SaveGameState(
		pgn:String=g.report_pgn,
		currentlinealgeb:String=g.current_line_algeb,
		name:String="current"
	)
	{
		DataUtils.WriteStringToFile(gpath(name),pgn)
		DataUtils.WriteStringToFile(lpath(name),currentlinealgeb)
	}

	////////////////////////////////////////////////////////////////////
	// help

	def HelpTopics
	{
		if(HasStage("{helptopicsstage}")) return

		Help.helptopics.open
	}

	def CreateWiki
	{
		Help.helptopics.create_wiki
	}

}