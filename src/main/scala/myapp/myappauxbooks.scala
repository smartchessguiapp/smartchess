package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppAuxBooks
{
	import MyApp._

	def AuxBookImport(bookname:String)
	{
		val ab=auxbooks(bookname)

		Minimax(addtobook=ab)
	}

	def GuiShowAuxBooks
	{
		if(HasStage("{showauxbooksdialog}"))
		{
			CloseStage("{showauxbooksdialog}")					
		}

		OpenAuxBooks()
	}

	def ShowAuxiliaryBooks
	{
		val books=GetBooks

		var i=0
		val bookscontent=(for(b <- books) yield{
			i+=1
			s"""
				|<label text="$b" r="$i" c="1"/>
				|<checkbox prefixset="variant" prefixget="variant" id="{auxbook $b}" r="$i" c="2"/>
			""".stripMargin
		}).mkString("\n")

		val blob=s"""
			|<vbox width="400.0">
			|<gridpane hgap="10" vgap="10">					
			|$bookscontent
			|</gridpane>
			|<button id="{showauxbooks}" text="Show"/>
			|</vbox>
		""".stripMargin

		MyStage("{showauxbooksdialog}","Show auxiliary books",blob,
			modal=true,usewidth=false,useheight=false,handler=handler)
	}

	def OpenAuxBooks(close_only:Boolean=false)
	{
		val books=GetBooks		

		for(b<-books)
		{	
			val bon=GB(Cve(s"{auxbook $b}"),false)
			val hs=HasStage(s"{auxbook $b stage}")
			val isc=(b==GetCurentBook)

			if(bon&&(!isc)&&(!close_only))
			{
				val blob=s"""
					|<vbox>
					|<hbox>
					|<button id="{auxbookimport}#{$b}" text="Import tree"/>
					|<label id="{auxbookinfo $b}"/>
					|</hbox>
					|<webview id="{auxbookwebview $b}"/>
					|</vbox>
				""".stripMargin

				if(!auxbooks.contains(b))
				{
					val ab=Book(b)

					ab.Load

					auxbooks+=(b->ab)
				}

				if(!hs) MyStage(s"{auxbook $b stage}",b,blob,
					modal=false,usewidth=true,useheight=true,handler=handler)
			}

			if((!bon)||isc)
			{
				if(hs) CloseStage(s"{auxbook $b stage}")
			}
		}

		prev_fen=null

		Update
	}

	def CloseAuxBooks
	{
		val books=GetBooks

		auxbooks=Map[String,Book]()

		for(b<-books)
		{
			if(HasStage(s"{auxbook $b stage}"))
			{
				CloseStage(s"{auxbook $b stage}")
			}
		}
	}
}