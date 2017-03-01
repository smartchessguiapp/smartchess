package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppPgn
{
	import MyApp._

	def OpenMultipleGamePgn
	{
		SelectPgnGamesTab

		val f=ChooseFile("openmultipgn")

		if(f==null) return

		if(f.exists)
		{
			val pgnscontent=DataUtils.ReadFileToString(f.getAbsolutePath())

			val pgns=game.split_pgn(pgnscontent).toList

			GetPgnGameBrowser.LoadPgns(pgns)
		}
	}

	def AddPgnGamesToBook
	{
		SelectBookTab

		val maxbookdepth=GD("{components}#{maxbookdepth}",20.0).toInt
		val checkreplica=GB("{components}#{checkreplica}",false)

		GetPgnGameBrowser.AddGamesToBook(book=book,maxdepth=maxbookdepth,checkreplica=checkreplica)
	}

	def SavePgnAs
	{
		val pgn=g.report_pgn

		SaveFileAsDialog(title="Save PGN as",id="savepgnas",content=pgn,setdirafter="openpgn")
	}

	def OpenPgn
	{
		val f=ChooseFile("openpgn",setdirafter="savepgnas")

		if(f==null) return

		if(f.exists())
		{
			val path=f.getAbsolutePath()

			val pgn=DataUtils.ReadFileToString(path)

			g.set_from_pgn(pgn)

			CheckProfile

			Update
		}
	}
}