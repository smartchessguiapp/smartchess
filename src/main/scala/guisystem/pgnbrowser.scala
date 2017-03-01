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

import scala.xml._

import java.io._
import java.nio.file._

import scala.io.Source

import org.apache.commons.io.FileUtils._

////////////////////////////////////////////////////////////////////

case class PgnState(
	var pgn:String = null,
	var movelistsan:String = null,
	var movelistalgeb:String = null,
	var fen:String = null,
	var flip:Boolean = false
)
{
	pgn=DataUtils.Normalize(pgn)

	pgn=pgn.replaceAll("^[\r\n]*","")

	def toXml=
	{
		{
			<pgnstate>
				<pgn>{pgn}</pgn>
				<movelistsan>{movelistsan}</movelistsan>
				<movelistalgeb>{movelistalgeb}</movelistalgeb>
				<fen>{fen}</fen>
				<flip>{flip}</flip>
			</pgnstate>
		}
	}

	def fromXml(xml:NodeSeq)
	{
		pgn = (xml \ "pgn").text
		movelistsan = (xml \ "movelistsan").text
		movelistalgeb = (xml \ "movelistalgeb").text
		fen = (xml \ "fen").text
		flip = DataUtils.ParseBoolean((xml \ "flip").text,false)
	}
}

object pgnlist
{
	var pgns:scala.collection.mutable.ArrayBuffer[PgnState]=scala.collection.mutable.ArrayBuffer[PgnState]()
	var path:String=null

	def multipgnpath:String =
	{
		if(path==null) return null
		val f=new java.io.File(path)
		val name=f.getName
		DataUtils.PathFromList(List("stuff",board.variant,"multipgns",name.replaceAll("xml$","pgn")))
	}

	def reset
	{
		pgns=scala.collection.mutable.ArrayBuffer[PgnState]()
	}

	def set_path(setpath:String)
	{
		path=setpath
	}

	def add(ps:PgnState)
	{
		pgns.insert(0,ps)

		save

		update
	}

	def add_all_md5(md5s:List[String]=List[String]())
	{
		for(md5 <- md5s)
		{
			val path=GameBrowser.game_path(md5)

			if(new java.io.File(path).exists())
			{
				val pgn=DataUtils.ReadFileToString(path)

				val dummy=new game

				dummy.parse_pgn(pgn,head_only=true)

				val myhandle=Builder.GS("{settings}#{myhandle}")		

				val black=dummy.get_header("Black")

				val flip=(myhandle==black)

				val fen=dummy.report_fen

				val ps=PgnState(pgn=pgn,movelistsan="",movelistalgeb="",fen=fen,flip=flip)

				pgns+=ps
			}
		}

		save

		update
	}

	def toXml=
	{
		val pgn_list=(for(v<-pgns) yield v.toXml).toList

		{
			<pgnlist>
				{pgn_list}
			</pgnlist>
		}
	}

	def toXmlString:String =
	{
		toXml.toString
	}	

	def fromXml(xml:NodeSeq)
	{
		pgns=scala.collection.mutable.ArrayBuffer[PgnState]()

		val pgnstatesxml = (xml \ "pgnstate")

		for(pgnstatexml <- pgnstatesxml)
		{
			val pgnstate=PgnState()

			pgnstate.fromXml(pgnstatexml)

			pgns+=pgnstate
		}
	}	

	def save
	{
		if(path==null) return

		val xml=toXml

		scala.xml.XML.save(path,xml)

		val pgncontent=(for(pgn <- pgns) yield pgn.pgn).mkString("\n\n\n")

		DataUtils.WriteStringToFile(multipgnpath,pgncontent)
	}

	def load
	{
		reset

		if(!((new File(path)).exists)) return

		val xml=scala.xml.XML.loadFile(path)

		fromXml(xml)
	}

	def id = s"{pgnlists}#{$path}"

	def nopages = ( pgns.length / pagesize + ( if((pgns.length % pagesize)!=0) 1 else 0 ) )

	def currentpage = currentindex / pagesize + 1

	def update	
	{
		for(i <- 0 to (pagesize-1))
		{
			val truei=currentindex+i

			if(truei< pgns.length)
			{
				Builder.GetMyText(s"{pgntext $i}").SetText(pgns(truei).pgn)
				Builder.GetMyText(s"{movelisttext $i}").SetText(pgns(truei).movelistsan)
				val db=Builder.GetGuiBoard(s"{demoboard $i}")
				db.flip=pgns(truei).flip
				db.set_from_fen(pgns(truei).fen)
				db.draw_board
				Builder.GetMyBox(s"{listpanel $i}").SetDisable(false)
			}
			else
			{
				Builder.GetMyText(s"{pgntext $i}").SetText("")
				Builder.GetMyText(s"{movelisttext $i}").SetText("")
				val db=Builder.GetGuiBoard(s"{demoboard $i}")
				db.flip=false
				db.set_from_fen("8/8/8/8/8/8/8/8 w - - 0 1")
				db.draw_board
				Builder.GetMyBox(s"{listpanel $i}").SetDisable(true)
			}
		}

		Builder.GetMyText("{indextext}").SetText(currentpage+" / "+nopages)
	}

	def get_pgnstate:PgnState = PgnState(
		pgn=MyApp.g.report_pgn,
		movelistsan=MyApp.g.current_line_pgn,
		movelistalgeb=MyApp.g.current_line_algeb,
		fen=MyApp.g.report_fen,
		flip=MyApp.GetFlip
	)

	def loadbyindex(i:Int)
	{		
		if(i< pgns.length)
		{
			val ps=pgns(i)

			MyApp.SaveGameState(ps.pgn,ps.movelistalgeb)
			MyApp.LoadGameState()

			MyApp.SetGuiFlip(ps.flip)

			MyApp.Update
		}
	}

	def toindex(index:Int)
	{
		if(index<0) currentindex=0
		else if(index>=pgns.length) currentindex=pgns.length-1
		else currentindex=index

		currentindex = currentindex - ( currentindex % pagesize )

		update
	}

	def pgnlist_handler(ev:MyEvent)
	{
		val parts=ev.Id.replaceAll("[\\{\\}]","").split(" ").toList

		if(ev.kind=="stage closed")
		{
			if(ev.Id==id)
			{
				Builder.Set(Builder.Cve("{currentpgnlist}"),"")
			}
		}

		if(ev.kind=="vbox clicked")
		{			
			if(parts.length > 1)
			{
				if(parts(0)=="loadv")
				{
					val i=parts(1).toInt+currentindex
					loadbyindex(i)
				}
			}
		}

		if(ev.kind=="button pressed")
		{
			if(ev.Id=="{add}")
			{
				add(get_pgnstate)
				toindex(0)
				update
				save
			}			
			
			if(ev.Id=="{tobegin}")
			{
				toindex(0)
			}			

			if(ev.Id=="{fastback}")
			{
				toindex(currentindex-pagesize-pgns.length/9)
			}			

			if(ev.Id=="{back}")
			{
				toindex(currentindex-pagesize)
			}			

			if(ev.Id=="{forward}")
			{
				toindex(currentindex+pagesize)
			}			

			if(ev.Id=="{fastforward}")
			{
				toindex(currentindex+pagesize+pgns.length/9)
			}			

			if(ev.Id=="{toend}")
			{
				toindex(pgns.length)
			}			

			if(ev.Id=="{deleteall}")
			{
				if(Builder.Confirm("Do you really want to delete all games ?"))
				{
					pgns=scala.collection.mutable.ArrayBuffer[PgnState]()

					update
					save
				}
			}			

			if(parts.length>1)
			{
				val i=parts(1).toInt+currentindex

				if(i>=pgns.length) return

				if(parts(0)=="top")
				{
					val old=pgns(i)
					pgns.remove(i)
					pgns.insert(0,old)					
					save
					toindex(0)
					update
				}

				if(parts(0)=="up")
				{
					if(i==0) return
					val old=pgns(i)
					pgns.remove(i)
					pgns.insert(i-1,old)
					save
					toindex(i-1)
					update
				}

				if(parts(0)=="down")
				{
					if(i>=(pgns.length-1)) return
					val old=pgns(i)
					pgns.remove(i)
					pgns.insert(i+1,old)
					save
					toindex(i+1)
					update
				}

				if(parts(0)=="bottom")
				{
					val old=pgns(i)
					pgns.remove(i)
					pgns+=old
					save
					toindex(pgns.length-1)
					update
				}

				if(parts(0)=="del")
				{
					pgns.remove(i)
					save
					update
				}

				if(parts(0)=="load")
				{
					loadbyindex(i)
				}

				if(parts(0)=="update")
				{					
					pgns(i)=get_pgnstate

					save
					update
				}
			}
		}
	}

	var currentindex=0

	var pagesize = 3

	def build
	{
		pagesize = Builder.GD("{components}#{pgnlistpagesize}",3.0).toInt

		load

		val listcontent=(for(i <- 0 to (pagesize-1)) yield {
			val pgncontent=s"""
				|<vbox paddings="0,0,0,0">
				|<textarea id="{pgntext $i}" width="600.0" height="120.0"/>
				|<label id="{movelisttext $i}" width="600.0"/>
				|</vbox>
			""".stripMargin
			val boardcontent=s"""
				|<vbox id="{loadv $i}" width="180" height="180" padding="0">
				|<guiboard id="{demoboard $i}" demo="true" piecesize="22"/>
				|</vbox>
			""".stripMargin
			val controlpanelcontent=s"""
				|<hbox>
				|
				|<button id="{load $i}" text="Load" img="icons/forwardt.png"/>
				|<button id="{update $i}" text="Update" img="icons/paste.png"/>
				|<button id="{top $i}" text="Top" img="icons/top.png"/>
				|<button id="{up $i}" text="Up" img="icons/up.png"/>
				|<button id="{down $i}" text="Down" img="icons/down.png"/>			
				|<button id="{bottom $i}" text="Bottom" img="icons/bottom.png"/>
				|<button id="{del $i}" text="Del" img="icons/delt.png"/>
				|
				|</hbox>
			""".stripMargin
			s"""
				|<vbox id="{listpanel $i}">
				|
				|<hbox>
				|
				|$boardcontent
				|
				|<vbox paddings="0,0,0,0">
				|
				|$pgncontent
				|
				|$controlpanelcontent
				|
				|</vbox>
				|
				|</hbox>
				|
				|</vbox>
			""".stripMargin
		}).mkString("\n")

		val blob=s"""
			|<vbox gap="0">
			|
			|<hbox>
			|
			|<label id="{indextext}" width="80.0"/>
			|<button id="{add}" text="Add current PGN" img="icons/add.png"/>			
			|<button id="{tobegin}" text="Begin" img="icons/begint.png"/>
			|<button id="{fastback}" text="Fastback" img="icons/fastback.png"/>
			|<button id="{back}" text="Back" img="icons/backt.png"/>
			|<button id="{forward}" text="Forward" img="icons/forwardt.png"/>
			|<button id="{fastforward}" text="Fastforward" img="icons/fastforward.png"/>
			|<button id="{toend}" text="End" img="icons/endt.png"/>
			|<button id="{deleteall}" text="Delete all" img="icons/del.png"/>
			|
			|</hbox>
			|
			|$listcontent
			|
			|</vbox>
		""".stripMargin

		val s=Builder.MyStage(id,path,blob,modal=false,
			handler=pgnlist_handler,usewidth=true,useheight=true)

		s.SetWidth(850)
		s.SetHeight(90+pagesize*205)

		currentindex=0

		update

		Builder.Set(Builder.Cve("{currentpgnlist}"),path)
	}

	def open
	{
		if(!Builder.HasStage(id))
		{
			build
		}
	}

	def close
	{
		if(Builder.HasStage(id))
		{
			Builder.CloseStage(id)
		}
	}
}