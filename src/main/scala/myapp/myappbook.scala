package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppBook
{
	import MyApp._

	def AddCurrentPositionToBook = AddPositionToBook(g.report_trunc_fen)

	def AddPreviousPositionToBook = AddPositionToBook(g.current_node.genTruncFen)

	def AutoAdd
	{
		if(GB("{components}#{autoaddmove}",false))
		{
			AddMoveToBook

			if(GB("{components}#{autoincplays}",false)) IncPlays
		}
	}

	def DelAllMoves
	{
		val bpos=AddCurrentPositionToBook

		bpos.entries=Map[String,BookEntry]()
	}

	def SaveBook
	{
		val size=book.SaveAndTellSize

		SetBookSizeText(DataUtils.FormatBytes(size))
	}

	def SetCurrentBook(bookname:String,saveold:Boolean=true)
	{
		if( (book!=null) && saveold ) book.Save

		val l=GetBooks

		book=Book(bookname)

		if( !l.contains(bookname) )
		{
			book.Save
		}

		Set(Cve("{selectedbook}"),bookname)

		book.Load

		MyActor.queuedExecutor ! ExecutionItem(client="SetCurrentBook",code=new Runnable{def run{
			OpenAuxBooks()
		}})						            		
	}

	def AddPositionToBook(tfen:String):BookPosition =
	{
		book.add_position(tfen)
	}

	def AddMoveToBook:BookEntry =
	{
		val bpos=AddPreviousPositionToBook

		val san=g.current_node.genSan

		bpos.add_move(san)
	}

	def AnnotateMove(annot:String=null)
	{
		val entry=AddMoveToBook

		if(annot!=null) entry.annot=annot
	}

	def IncPlays
	{
		val entry=AddMoveToBook

		entry.plays=entry.plays+1
	}

	def DelMove
	{
		val bpos=AddPreviousPositionToBook

		bpos.delete_move(g.current_node.genSan)
	}

	def CreateNewBook
	{
		Set("{components}#{textinputs}#{Book name}","")

		val r=InputTexts("Enter book name",List("Book name"),applyname="Create")

		if(r.canceled) return

		var bookname=RemoveSpecials(r.texts("Book name"))

		SetCurrentBook(bookname)
	}

	def DelBook(name:String)
	{
		val lf=DataUtils.getListOfFiles(bdir)
		
		for(f <- lf)
		{
			val fn=f.getName
			if(fn==(name+".xml"))
			{
				f.delete
			}
		}

		val lnew=GetBooks

		if(lnew.length==0)
		{
			SetCurrentBook("default",saveold=false)

			return
		}

		SetCurrentBook(lnew(0),saveold=false)
	}

	def SpecialMake(backmake:Boolean,storemake:Boolean)
	{
		val cas=EngineManager.access_cas()

		if(cas==null) return

		val bestmove960=cas.bestmove960

		if(g.b.isAlgebLegal(bestmove960))
		{
			if(storemake)
			{
				val san=cas.bestmovesan

				val bpos=AddPositionToBook(g.report_trunc_fen)

				val entry=bpos.add_move(san)

				val score=cas.scorenumerical

				val depth=cas.depth

				entry.SetEngineScore(score,depth)
			}

			if(backmake) EngineManager.enginelist ! StopAllMsg()
			else MakeAlgebMove(bestmove960)

			Update
		}
	}

	def GuiAddToBook
	{
		AddMoveToBook

		Back

		Update
	}

	def GuiAddBook
	{
		CreateNewBook

		Update
	}

	def GuiDelAllMoves
	{
		DelAllMoves

		Update
	}

	def GuiSearch
	{
		val factor=GD("{components}#{searchfactor}",80.0).toInt

		Eval.Search(factor=factor)
	}

	def GuiEvalThis
	{
		val san=g.current_node.genSan

		Back

		Update
		
		Eval.EvalAll(1,deep=true,thismove=san)
	}

	def GuiDelBook
	{
		DelBook(GetCurentBook)

		Update
	}

	def GuiSetCurrentBook(name:String)
	{
		SetCurrentBook(name)

		prev_fen=null
		Update
	}

	def GuiAnnotateMove(annot:String)
	{
		AnnotateMove(annot)

		Update
	}

	def Minimax(remove:Boolean=false,addtobook:Book=null)
	{
		Eval.Minimax(setremove=remove,setaddtobook=addtobook)
	}
}