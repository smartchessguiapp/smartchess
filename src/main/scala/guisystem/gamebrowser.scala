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
import javafx.concurrent.Worker._

import collection.JavaConversions._

////////////////////////////////////////////////////////////////////

import java.io._
import scala.io._

////////////////////////////////////////////////////////////////////

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

import org.apache.commons.codec.digest.DigestUtils._

////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// MyGameBrowser
////////////////////////////////////////////////////////////////////

// pgn games browser ( composite widget )

object GameBrowser
{
	def game_path(md5Hex:String):String=
		"stuff/"+board.variant+"/games/"+md5Hex+".pgn"

	var aborted=false

	def DigestPgns(pgns:List[String]):List[String] =
	{
		val md5s=scala.collection.mutable.ArrayBuffer[String]()

		var i=0
		var total=0

		for(pgn <- pgns)
		{
			if(aborted) return md5s.toList

			total+=1

			val dummy=new game
			dummy.parse_pgn(pgn,head_only=true)

			val variant=dummy.get_header("Variant")

			val variantok=(variant==board.variant)

			if(variantok)
			{
				i+=1

				val md5=md5Hex(pgn)

				val path=game_path(md5)

				if(!((new java.io.File(path)).exists))
				{
					DataUtils.WriteStringToFile(path,pgn)
				}

				md5s+=md5

				val info_line=dummy.get_info_line

				val info=s"""$i / $total $info_line"""

				MyActor.Log(info)
			}
		}

		md5s.toList
	}
}

class GameBrowser() extends MyComponent
{
	import GameBrowser._

	var game_list:List[String]=null
	var step:Int=0
	var from:Int=0
	var vb:VBox=null
	var hb:HBox=null
	var sort_text:TextField=null
	var sort_combo:ComboBox[String]=null
	var w:WebView=null
	var lastclicked:Int= -1

	var stab:Int=0

	def SelectLogTab
	{
		aborted=false

		stab=MyApp.selectedmaintab

		MyActor.queuedExecutor ! ExecutionItem(client="GameBrowser.LoadPgns.SelectLogTab",code=new Runnable{def run{
			MyApp.SelectLogTab
			Builder.AbortDialog(title="Abort building book",callback=()=>{aborted=true})
		}})					
	}

	def SelectMainTab
	{
		MyActor.queuedExecutor ! ExecutionItem(client="GameBrowser.LoadPgns.SelectMainTab",code=new Runnable{def run{
			Builder.CloseAbortDialog
			MyApp.SelectMainTab(stab)
			update
			MyApp.prev_fen=null
			MyApp.Update
		}})			
	}

	def LoadPgns(pgns:List[String])
	{
		Future
		{
			SelectLogTab

			load_list(DigestPgns(pgns))

			SelectMainTab
		}
	}

	def AddGamesToBook(book:Book,maxdepth:Int=20,checkreplica:Boolean=false)
	{
		Future
		{
			SelectLogTab

			add_games_to_book(book,maxdepth,checkreplica)

			SelectMainTab
		}
	}

	def resetindex
	{
		from=0
		lastclicked= -1
	}

	def load_list(md5s:List[String])
	{
		game_list=md5s

		resetindex
	}

	def add_games_to_book(book:Book,maxdepth:Int=20,checkreplica:Boolean=false)
	{
		var total=0

		for(md5 <- game_list)
		{
			if(aborted) return

			total+=1

			val path=game_path(md5)

			if((new java.io.File(path)).exists())
			{
				val dummy=new game

				val pgn=DataUtils.ReadFileToString(path)

				dummy.parse_pgn(pgn,addtobook=book,maxdepth=maxdepth,checkreplica=checkreplica)

				val infoline=dummy.get_info_line

				MyActor.Log(s"$total. added "+infoline)
			}
		}
	}

	def OwnCreateNode:Node=
	{		
		game_list=List[String]()
		step=20
		from=0
		vb=new VBox(5)
		hb=new HBox(5)
		sort_text=new TextField()
		sort_combo=new ComboBox[String]()
		w=new WebView()

		hb.setPadding(new Insets(5,5,5,5))
		sort_combo.getItems().addAll(FXCollections.observableList(List("White","WhiteElo","Black","BlackElo","Date","Event","Result")))
		hb.getChildren().addAll(
			new MyCallbackButton("|<",tobegin),
			new MyCallbackButton("<<",fastback),
			new MyCallbackButton("<",back),

			new MyCallbackButton(">",forward),
			new MyCallbackButton(">>",fastforward),
			new MyCallbackButton(">|",toend),
			new Label("Sort by"),
			sort_combo,
			new MyCallbackButton("Sort",do_selected_sort),
			new Label("Search for: "),
			sort_text,
			new MyCallbackButton("Search",do_selected_search)
			)

		sort_combo.setOnAction(new EventHandler[ActionEvent]
		{
			def handle(ev:ActionEvent)
			{			
				do_selected_sort
			}
		})

		w.setOnMouseClicked(new EventHandler[MouseEvent]
		{			
			def handle(mouseEvent:MouseEvent)
			{
		        var etype:String=mouseEvent.getEventType().toString()

	            if(etype=="MOUSE_CLICKED")
	            {
	            	val xobj=w.getEngine().executeScript("x")

	            	if(xobj==null) return

	            	val x=xobj.toString()

	            	if(DataUtils.IsInt(x))
	            	{
	            		val index=x.toInt

	            		lastclicked=index

	            		val md5=game_list(index-1)

	            		val path=game_path(md5)

	            		if(!new java.io.File(path).exists) return

	            		val pgn=DataUtils.ReadFileToString(path)

	            		MyApp.g.set_from_pgn(pgn)

	            		MyApp.SelectBookTab

						MyActor.Log(s"gamebrowser ${GetId} open: "+pgn)

						MyApp.CheckProfile

						MyApp.Update
	            		
	            		Fire("gamebrowser game loaded",""+index)

	            		update
	            	}
	            }
	        }
	    })

		vb.getChildren().addAll(hb,w)

		node=vb

		node
	}

	def OwnReportPrintable(level:Int=0,buff:String=""):String=s"gamebrowser"

	def setStep(set_step:Int)
	{
		step=set_step
		update
	}

	def update
	{
		val len=game_list.length
		if(from>=len) from=len-step
		if(from< 0) from=0

		val highlight=lastclicked
		w.getEngine().loadContent(printableGameList(from,from+step,true,game_list,highlight))
	}

	def tobegin()
	{
		from=0
		update
	}

	def back()
	{
		from-=step
		update
	}

	def fastback()
	{
		from-=10*step
		update
	}

	def fastforward()
	{
		from+=10*step
		update
	}

	def forward()
	{
		from+=step
		update
	}

	def toend()
	{
		from=game_list.length
		update
	}

	def sort_selected(sort:Boolean)
	{
		val selobj=sort_combo.getSelectionModel().getSelectedItem()

		val sel=if(selobj==null) "" else selobj.toString()

		var games=(for(md5<-game_list) yield
		{
			val g=new game

			val v=board.variant

			val path=game_path(md5)

			val pgn=org.apache.commons.io.FileUtils.readFileToString(
				new File(path),
				null.asInstanceOf[String]
			)

			g.parse_pgn(pgn,head_only=true)

			Tuple2[String,game](md5,g)
		}).toArray

		val search=sort_text.getText()
		sort_text.setText("")

		//println("search for _"+search+"_")

		def sortfunc(a:Tuple2[String,game],b:Tuple2[String,game]):Boolean=
		{
			if(sort) return a._2.get_header(sel)< b._2.get_header(sel)
			var matcha=false
			for((k,v)<-a._2.pgn_headers)
			{
				if(v.contains(search)) matcha=true
			}
			matcha
		}

		games=games.sortWith(sortfunc)

		if(sel.contains("Elo")||(sel=="Result")) games=games.reverse

		game_list=(for(item<-games) yield item._1).toList

		from=0

		update

	}

	def do_selected_sort()
	{
		sort_selected(true)
	}

	def do_selected_search()
	{
		sort_selected(false)
	}

	def printableGameLine(md5Hex:String,index:Int,html:Boolean=false,selected:Boolean=false):String=
	{
		val path=game_path(md5Hex)

		val dummy=new game
		dummy.parse_pgn(DataUtils.ReadFileToString(path),head_only=true)

		val White=dummy.get_header("White")
		val WhiteElo=dummy.get_header("WhiteElo")
		val Black=dummy.get_header("Black")
		val BlackElo=dummy.get_header("BlackElo")
		val Date=dummy.get_header("Date")
		val Event=dummy.get_header("Event")
		val Round=dummy.get_header("Round")
		val Result=dummy.get_header("Result")

		val selstyle=if(selected) "background-color: #ffff7f;"
		else if((index % 2)==0) "background-color: #efefef;" else "background-color: #f7f7f7;"

		if(html) return(
			s"""
			|<a name="start"></a>
			|$index. <a href="#start" style="text-decoration: none; padding: 3px; margin: 2px; font-family: monospace; $selstyle" onmousedown="setx('$index');">
			|<font color="#007f00">$White ($WhiteElo)</font> 
			|<font color="#7f0000">$Black ($BlackElo)</font> 
			|<font color="#00007f"><b>$Result</b></font> 
			|<font color="#af00af"><i>$Date<i></font> 
			|<font color='#007f7f'><i>$Event #$Round<i></font>
			|</a>
			""".stripMargin.replaceAll("\n","")
			)

		s"$White - $Black $Result ( $Event )"
	}

	def printableGameList(from:Int,to:Int,html:Boolean=false,gl:List[String],selind:Int= -1):String=
	{

		if(gl.length==0)
		{
			var html_script=""
			if(html)
			{
				html_script="<script>var x='';</script>"
			}
			return html_script+"no games found\n\n"
		}

		val len=gl.length

		var content=s"total of $len games found , listing from "+(from+1)+" :\n\n"

		if(from>=len) return content

		val tom=(if(to>len) len else to)

		val gls=gl.slice(from,tom)

		var i=from
		val l=(for(md5Hex<-gls) yield { i+=1; printableGameLine(md5Hex,i,html=html,i==selind) }).mkString("\n")+"\n"

		content+=l

		if(html)
		{
			content=content.replaceAll("\n","<br>")

			content="""
			|<script>
			|var x="";
			|function setx(sx)
			|{
				x=sx;
			|}
			|</script>
			|""".stripMargin+
			content
		}

		content

	}
}