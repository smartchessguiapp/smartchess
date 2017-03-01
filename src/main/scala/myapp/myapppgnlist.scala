package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppPgnList
{
	import MyApp._

	def PgnListSavedCallback(path:String)
	{
		pgnlist.close
		pgnlist.set_path(path)
		pgnlist.open
	}

	def NewPgnList
	{
		pgnlist.reset

		SaveFileAsDialog(
			title="New PGN list",
			id="savepgnlistas",
			content=pgnlist.toXmlString,
			ext="xml",
			create=true,
			successcallback=PgnListSavedCallback,
			setdirafter="openpgnlist")
	}

	def SavePgnListAs
	{
		if(pgnlist.path==null) NewPgnList else
		{
			SaveFileAsDialog(
				title="Save PGN list as",
				id="savepgnlistas",
				content=pgnlist.toXmlString,
				ext="xml",
				successcallback=PgnListSavedCallback,
				setdirafter="openpgnlist")
		}
	}

	def OpenPgnList
	{
		val f=ChooseFile("openpgnlist",setdirafter="savepgnlistas")

		if(f==null) return

		if(f.exists())
		{
			val path=f.getAbsolutePath()

			pgnlist.close

			pgnlist.set_path(path)

			pgnlist.open
		}
	}

	def AddAllGamesToPgnList
	{
		if(pgnlist.path!=null)
		{
			pgnlist.open

			pgnlist.add_all_md5(GetPgnGameBrowser.game_list)
		}
	}
}