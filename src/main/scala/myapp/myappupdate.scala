package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppUpdate
{
	import MyApp._

	def HighlightLastMove
	{
		GetMainBoard.clear_highlight
		if(g.current_node!=g.root)
		{
			val dummy=new board
			dummy.set_from_fen(g.current_node.parent.fen)
			val m=dummy.sanToMove(g.current_node.genSan)
			if(m!=null)
			{
				val ram=dummy.toRealAlgebMove(m)
				GetMainBoard.highlight_move(ram)
			}
		}
	}

	def UpdatePgnHtml
	{
		Future{
			val pgnhtmlcontent=g.report_pgn_html(g.current_node)

			MyActor.queuedExecutor ! ExecutionItem(client="UpdatePgnHtml",code=new Runnable{def run{
				val mi=pgnhtmlcontent.indexOf("padding: 3px")

				val len=pgnhtmlcontent.length.toDouble

				var v=0.0

				if(mi>2000) {
					v=(mi-2000)/len
				}

				LoadWebContentAndScrollTo("{pgnwebview}",pgnhtmlcontent,v)
			}})			
		}
	}

	def UpdateBookHtml
	{
		Future{
			val bpos=AddCurrentPositionToBook

			val bookhtmlcontent=bpos.reportHTML(
				madesans=g.current_node.childs.keys.toList,
				book=book,
				hideresultstats=GB("{components}#{hideresultstats}",false),
				showmetbymove=GB("{components}#{showmetbymove}",false)
				)

			val nopos=book.positions.size

			MyActor.queuedExecutor ! ExecutionItem(client="UpdateBookHtml",code=new Runnable{def run{
				SetBookSizeText(""+nopos)
				LoadWebContent("{bookwebview}",bookhtmlcontent)
			}})			
		}
	}

	def UpdateMovesHtml
	{
		Future{
			val moveshtmlcontent=g.b.genPrintableMoveList(html=true)

			MyActor.queuedExecutor ! ExecutionItem(client="UpdateMovesHtml",code=new Runnable{def run{
				LoadWebContent("{moveswebview}",moveshtmlcontent)
			}})			
		}
	}

	def UpdateBookGameBrowser
	{
		Future{
			val bpos=AddCurrentPositionToBook

			val md5s=bpos.game_list.filter(x => (new java.io.File(GameBrowser.game_path(x))).exists())

			val bgb=GetBookGameBrowser

			bgb.load_list(md5s)			

			MyActor.queuedExecutor ! ExecutionItem(client="UpdateBookGameBrowser",code=new Runnable{def run{
				bgb.update
			}})			
		}
	}

	def UpdateAuxBooks
	{
		Future{
			val books=GetBooks
			val tfen=g.report_trunc_fen

			for(b<-books)
			{				
				val hasstage=HasStage(s"{auxbook $b stage}")
				val hasbook=auxbooks.contains(b)

				if(hasstage&&hasbook)
				{					
					val ab=auxbooks(b)
					val abpos=ab.add_position(tfen)

					val abhtmlcontent=abpos.reportHTML(madesans=g.current_node.childs.keys.toList,short=true)
					val absize=ab.positions.size

					MyActor.queuedExecutor ! ExecutionItem(client="UpdateAuxBook",code=new Runnable{def run{
						GetMyText(s"{auxbookinfo $b}").SetText(""+absize)
						LoadWebContent(s"{auxbookwebview $b}",abhtmlcontent)
					}})					
				}
			}			
		}
	}

	def UpdateTitle
	{
		Future{
			val playerwhite=g.get_header("White")
			val playerwhiterating=g.get_header("WhiteElo")
			val playerblack=g.get_header("Black")
			val playerblackrating=g.get_header("BlackElo")
			val result=g.get_header("Result")
			val timecontrol=g.get_header("TimeControl")
			val termination=g.get_header("Termination")
			val plycount=g.get_header("PlyCount")

			val title=s"""smartchess ${board.variant} | $playerwhite $playerwhiterating - $playerblack $playerblackrating | $timecontrol $termination $plycount | $result"""

			MyActor.queuedExecutor ! ExecutionItem(client="UpdateTitle",code=new Runnable{def run{
				GetStage("{mainstage}").SetTitle(title)
			}})			
		}
	}

	def CheckTrainingChanged
	{
		val current_isanytraining=IsAnyTraining

		if(current_isanytraining!=prev_anytraining)
		{
			if(current_isanytraining)
			{
				SelectMovesTab
			}
			else
			{
				SelectBookTab

				HideTrainingScore
			}

			prev_anytraining=current_isanytraining
		}
	}

	def UpdateBooksChanged
	{
		GetMyComboBox("{bookcombo}").CreateFromItems(GetBooks,GetCurentBook)
	}

	def CheckBooksChanged
	{
		val books=GetBooks
		val selbook=GetCurentBook

		if((books!=prev_books)||(selbook!=prev_selbook))
		{
			UpdateBooksChanged
		}

		prev_books=books
		prev_selbook=selbook
	}

	def SetBoard
	{
		val fen=g.report_fen

		GetMainBoard.set_from_fen(fen)

		HighlightLastMove
	}

	def UpdateFenChanged
	{
		UpdateMovesHtml

		EngineManager.enginelist ! RootFenChangedMsg
		GetMainBoard.clear_engine
		GetMainBoard.clear_score

		UpdateBookGameBrowser

		UpdateAuxBooks
	}

	def CheckFenChanged
	{
		val fen=g.report_fen

		if(prev_fen != fen)
		{
			UpdateFenChanged
		}

		prev_fen=fen
	}

	def UpdateAll
	{
		SetBoard

		UpdatePgnHtml

		UpdateBookHtml

		UpdateTitle

		CheckBooksChanged

		CheckFenChanged

		CheckTrainingChanged
	}

	def Update
	{
		UpdateAll
	}	
}