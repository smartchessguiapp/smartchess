package smartchess

////////////////////////////////////////////////////////////////////

import scala.xml._

import java.io._
import java.nio.file._

import scala.io.Source

import org.apache.commons.io.FileUtils._

////////////////////////////////////////////////////////////////////

case class Annotation(
	val annot:String,
	val limit:Int,
	val color:String,
	val backgroundcolor:String
)
{
}

object butils
{
	val INFINITE_SCORE = 100000
	val MATE_SCORE = 10000
	val MATE_THRESOLD = 9000

	val MAX_DEPTH = 100

	val ANNOTATIONS = scala.collection.immutable.ListMap[String,Annotation](
		"!!"->Annotation("!!",MATE_THRESOLD,"#00ff00","#afffaf"),
		"!"->Annotation("!",500,"#007f00","#7fff7f"),
		"!?"->Annotation("!?",300,"#0000ff","#cfcfff"),
		"-"->Annotation("-",-300,"#0000af","#afafff"),
		"?!"->Annotation("?!",-500,"#00007f","#9f9fff"),
		"?"->Annotation("?",-MATE_THRESOLD,"#7f0000","#ffafaf"),
		"??"->Annotation("??",-INFINITE_SCORE,"#ff0000","#ff7f7f")
	)

	val NO_ANNOTATION=Annotation("",-INFINITE_SCORE,"#000000","#ffffff")

	def get_annot_by_score(score:Int):Annotation =
	{
		for((k,v) <- ANNOTATIONS)
		{
			if(score > v.limit) return v
		}
		NO_ANNOTATION
	}

	def get_score_color(score:Int):String = get_annot_by_score(score).color

	def get_annot_by_key(key:String):Annotation =
	{
		if(ANNOTATIONS.contains(key)) return ANNOTATIONS(key)
		NO_ANNOTATION
	}

	def get_annot_color(annot:String):String = get_annot_by_key(annot).color

	val pborders=Map[Int,String](
		0  -> "",
		1  -> "border-style: solid; border-width: 1px; border-color: #00af00;",
		2  -> "border-style: solid; border-width: 2px; border-color: #ff0000;",
		3  -> "border-style: solid; border-width: 3px; border-color: #00af00;",
		4  -> "border-style: solid; border-width: 4px; border-color: #00ff00;",
		5  -> "border-style: solid; border-width: 5px; border-color: #00ff00;",
		6  -> "border-style: solid; border-width: 6px; border-color: #00af00;",
		7  -> "border-style: solid; border-width: 7px; border-color: #ff0000;",
		8  -> "border-style: solid; border-width: 8px; border-color: #00af00;",
		9  -> "border-style: solid; border-width: 9px; border-color: #00ff00;",
		10 -> "border-style: solid; border-width: 10px; border-color: #00ff00;"
	)

	val commentcols=Map[String,String](
		"solution" -> "#ffff9f"
	)
}

case class BookEntry(
	var san:String="",
	var plays:Int=0,
	var annot:String="",
	var wins:Int=0,
	var draws:Int=0,
	var losses:Int=0,
	var comment:String="-",
	var uci:String="-",
	var priority:Int=0,
	var enginescore:Int=0,
	var enginedepth:Int=0,
	var hasenginescore:Boolean=false,
	var minimaxscore:Int=0,
	var minimaxdepth:Int=0,
	var hasminimaxscore:Boolean=false
)
{
	var level=butils.MAX_DEPTH

	def clear_level
	{
		level=butils.MAX_DEPTH
	}

	def copy(what:BookEntry)
	{
		san=what.san
		plays=what.plays
		annot=what.annot
		wins=what.wins
		draws=what.draws
		losses=what.losses
		comment=what.comment
		uci=what.uci
		priority=what.priority
		enginescore=what.enginescore
		enginedepth=what.enginedepth
		hasenginescore=what.hasenginescore
		minimaxscore=what.minimaxscore
		minimaxdepth=what.minimaxdepth
		hasminimaxscore=what.hasminimaxscore
	}

	def HasScore = hasenginescore || hasminimaxscore

	def HasAnnot = annot!=""

	def GetScore:Int = 
	{
		if(hasminimaxscore) return minimaxscore
		enginescore
	}

	def IsMate:Boolean = GetScore > butils.MATE_THRESOLD
	def IsMated:Boolean = GetScore < -butils.MATE_THRESOLD

	def IsMateOrMated:Boolean = IsMate || IsMated

	def ColoredScore(score:Int):String =
	{
		val color=if(score<0) "red" else "green"
		s"""<font color="$color"><b>$score</b></font>"""
	}

	def SetEngineScore(
		score:Int,
		depth:Int,
		docolor:Boolean=true,
		docomment:Boolean=true,
		clearminimaxscore:Boolean=true
	)
	{
		enginescore=score
		enginedepth=depth
		hasenginescore=true

		if(clearminimaxscore)
		{
			minimaxscore=0
			minimaxdepth=0
			hasminimaxscore=false
		}

		if(docolor) AnnotByScore

		if(docomment) comment=s"""<small>E</small> ${ColoredScore(score)} <small>( <font color="blue">$depth</font> )</small>"""
	}

	def SetMinimaxScore(
		eval:Int,
		depth:Int,
		dnodes:Int,
		docolor:Boolean=true,
		docomment:Boolean=true,
		dosetplays:Boolean=true
	)
	{
		minimaxscore=eval
		hasminimaxscore=true

		if(docolor) AnnotByScore

		if(docomment) comment=s"""<small>M</small> ${ColoredScore(eval)} <small>( $depth,<font color="blue">$dnodes</font> )</small>"""

		if(dosetplays) plays=dnodes
	}

	def GetAnnotByScore:String =
	{
		if(!HasScore) return ""
		butils.get_annot_by_score(GetScore).annot
	}

	def AnnotByScore
	{
		annot=GetAnnotByScore
	}

	def toXml = toXmlCompact

	def toXmlVerbose=
	{
		<move san={san}>
		<plays>{plays}</plays>
		<annot>{annot}</annot>
		<wins>{wins}</wins>
		<draws>{draws}</draws>
		<losses>{losses}</losses>
		<comment>{comment}</comment>
		<uci>{uci}</uci>
		<priority>{priority}</priority>

		<enginescore>{enginescore}</enginescore>
		<enginedepth>{enginedepth}</enginedepth>
		<minimaxscore>{minimaxscore}</minimaxscore>
		<minimaxdepth>{minimaxdepth}</minimaxdepth>
		<hasenginescore>{hasenginescore}</hasenginescore>
		<hasminimaxscore>{hasminimaxscore}</hasminimaxscore>
		</move>
	}

	def toXmlCompact=
	{
		<move s={san}>
		<p>{plays}</p>
		<a>{annot}</a>
		<w>{wins}</w>
		<d>{draws}</d>
		<l>{losses}</l>
		<c>{comment}</c>
		<u>{uci}</u>
		<pr>{priority}</pr>

		<es>{enginescore}</es>
		<ed>{enginedepth}</ed>
		<ms>{minimaxscore}</ms>
		<md>{minimaxdepth}</md>
		<hes>{hasenginescore}</hes>
		<hms>{hasminimaxscore}</hms>
		</move>
	}

	def fromXml(move:NodeSeq)
	{
		san=(move \ "@san").text+(move \ "@s").text

		plays=DataUtils.ParseInt((move \ "plays").text+(move \ "p").text,0)
		annot=(move \ "annot").text+(move \ "a").text
		wins=DataUtils.ParseInt((move \ "wins").text+(move \ "w").text,0)
		draws=DataUtils.ParseInt((move \ "draws").text+(move \ "d").text,0)
		losses=DataUtils.ParseInt((move \ "losses").text+(move \ "l").text,0)
		comment=(move \ "comment").text+(move \ "c").text
		if((comment==null)||(comment=="")) comment="-"
		uci=(move \ "uci").text+(move \ "u").text
		if((uci==null)||(uci=="")) uci="-"
		priority=DataUtils.ParseInt((move \ "priority").text+(move \ "pr").text,0)

		enginescore=DataUtils.ParseInt((move \ "enginescore").text+(move \ "es").text,0)
		enginedepth=DataUtils.ParseInt((move \ "enginedepth").text+(move \ "ed").text,0)
		minimaxscore=DataUtils.ParseInt((move \ "minimaxscore").text+(move \ "ms").text,0)
		minimaxdepth=DataUtils.ParseInt((move \ "minimaxdepth").text+(move \ "md").text,0)
		hasenginescore=DataUtils.ParseBoolean((move \ "hasenginescore").text+(move \ "hes").text,false)
		hasminimaxscore=DataUtils.ParseBoolean((move \ "hasminimaxscore").text+(move \ "hms").text,false)
	}	
}

case class BookPosition(
	tfen:String
)
{
	var entries=Map[String,BookEntry]()

	var game_list=List[String]()

	def reset
	{
		entries=Map[String,BookEntry]()
	}

	def clear_level
	{
		for((k,v) <- entries) v.clear_level
	}

	def add_move(san:String):BookEntry =
	{
		if(!entries.contains(san)) entries+=(san->BookEntry(san))
		entries(san)
	}

	def delete_move(san:String)
	{
		if(entries.contains(san)) entries-=san
	}

	def TopShallowSans(belowdepthlimit:Int= -1,numberlimit:Int = -1):List[String] =
	{
		def IsOk(entry:BookEntry):Boolean =
		{
			if((belowdepthlimit >= 0)&&(entry.enginedepth>=belowdepthlimit)) return false
			true
		}

		val filteredsans=GetSansWithEvalSorted().filter(san => IsOk(entries(san)))

		if(numberlimit == -1) return filteredsans

		if(filteredsans.length <= numberlimit) return filteredsans

		filteredsans.slice(0,numberlimit)
	}

	def GetSansWithEvalSorted():List[String] =
	{
		val sanswithscore=entries.keys.toList.filter(entries(_).HasScore)

		sanswithscore.sortWith(entries(_).GetScore > entries(_).GetScore)
	}

	def GetSansWithEngineScore():List[String] =
	{
		entries.keys.toList.filter(entries(_).hasenginescore)
	}

	def GetSansWithAnnotSorted():List[String] =
	{
		val skeys=sortedKeys.toList

		skeys.filter(x => entries(x).HasAnnot)
	}

	def toXml=
	{
		val entry_list=(for((k,v)<-entries) yield v.toXml).toList

		{
			<position tfen={tfen}>
			<movelist>
				{entry_list}
			</movelist>
			<gamelist>
				{for(g<-game_list) yield <md5>{g}</md5>}
			</gamelist>
			</position>
		}
	}

	def fromXml(xml:NodeSeq)
	{
		entries=Map[String,BookEntry]()

		val moves=(xml \ "movelist" \ "move")
		
		for(move<-moves)
		{

			val entry=BookEntry()

			entry.fromXml(move)

			entries+=(entry.san->entry)

		}

		val games=xml \ "gamelist" \ "md5"

		game_list=games.map(g => g.text).toList
	}

	def add_game(md5:String)
	{
		if(game_list.contains(md5)) return

		game_list=game_list:+md5
	}

	def sortedKeys:Array[String]=
	{
		def sortfunc(ak:String,bk:String):Boolean=
		{
			val a=entries(ak)
			val b=entries(bk)

			val apr=a.priority
			val bpr=b.priority

			val aa=butils.ANNOTATIONS.keys.toList.reverse.indexOf(a.annot)
			val ba=butils.ANNOTATIONS.keys.toList.reverse.indexOf(b.annot)

			val ap=a.plays
			val bp=b.plays

			val ah=a.HasScore
			val bh=b.HasScore

			val as=a.GetScore
			val bs=b.GetScore

			def AnnotScorePlays:Boolean =
			{
				// annotations
				if(aa!=ba) return aa > ba

				// score
				if(ah&&bh)
				{				
					if(as==bs) return ap > bp
					return as > bs
				}
				if(ah||bh) return ah

				// plays
				ap > bp
			}

			// high priorities
			if((apr>5)||(bpr>5))
			{				
				if(apr==bpr) return AnnotScorePlays
				return apr > bpr
			}			

			AnnotScorePlays
		}

		entries.keys.toArray.sortWith(sortfunc)
	}

	def Branches(san:String,book:Book):Boolean =
	{
		if(book==null) return false
		
		val b=new board
		b.set_from_fen(tfen)
		b.makeSanMove(san)
		val rtfen=b.report_trunc_fen
		if(book.positions.contains(rtfen))
		{
			if(book.positions(rtfen).entries.keys.toList.length>0) return true
		}

		false
	}

	def GetVirginSans(book:Book):List[String] =
	{
		entries.keys.toList.filter(x => !Branches(x,book))
	}

	var pvgame:game=null

	def collect_pv_recursive(depth:Int,book:Book,maxdepth:Int=10)
	{
		if(depth>maxdepth) return
		val bpos=book.add_position(pvgame.report_trunc_fen)
		val sans=(if(maxdepth==1) bpos.sortedKeys.toList else bpos.GetSansWithEvalSorted)
		if(sans.length<=0) return
		val san=sans(0)
		pvgame.makeSanMove(san)
		collect_pv_recursive(depth+1,book,maxdepth)
	}

	def collect_pv(san:String,book:Book,maxdepth:Int=10):String =
	{
		pvgame= new game
		pvgame.set_from_fen(tfen)

		pvgame.makeSanMove(san)

		collect_pv_recursive(0,book,maxdepth)

		if(maxdepth > 1) return pvgame.current_line_pgn

		val moves=pvgame.current_line_moves

		if(moves.length <= 1) return "?"

		moves(1)
	}

	def reportHTML(
		madesans:List[String]=List[String](),
		short:Boolean=false,
		book:Book=null,
		nostats:Boolean=false,
		collectpv:Boolean=false,
		hideresultstats:Boolean=false,
		showmetbymove:Boolean=false
	):String =
	{
		val td="""<td align="center">"""

		val skeys=sortedKeys

		val items=(for(k<-skeys) yield {

			val annot=entries(k).annot
			val plays=entries(k).plays
			val wins=entries(k).wins
			val draws=entries(k).draws
			val losses=entries(k).losses
			val comment=entries(k).comment
			val uci=entries(k).uci
			var commentcol="#FFFFFF"
			var priority=entries(k).priority

			if(butils.commentcols.contains(comment)) commentcol=butils.commentcols(comment)

			val col=butils.get_annot_color(annot)

			val annots=(for((ak,av)<-butils.ANNOTATIONS) yield
			{
				s"""
					|<td align="center" onmousedown="setclick('$k','annot','$ak','$uci');">
					|<span style="cursor: pointer;">
					|<font color="${av.color}" size="3">$ak</font>
					|</span>
					|</td>
				""".stripMargin
			}).mkString("\n")			

			var border=butils.pborders(priority)

			val uri=new File("web/forwardt.png").toURI()

			val made=if(madesans.contains(k))
				s"""<img src="$uri">&nbsp;"""
			else ""

			val branches=if(Branches(k,book)) "..." else ""

			val prioritycontent=if(short) "" else s"""				|
				|<td align="center" onmousedown="setclick('$k','incpriority','','$uci');">
				|<a name="dummy2"></a>
				|<a href="#dummy2" style="text-decoration: none;">
				|<font color="#7fff7f" size="3">up</font>
				|</a>
				|</td>
				|
				|<td align="center" onmousedown="setclick('$k','setpriority','','$uci');">
				|<a name="dummy2"></a>
				|<a href="#dummy2" style="text-decoration: none;">
				|<font color="#7f7fff" size="4">$priority</font>
				|</a>
				|</td>
				|
				|<td align="center" onmousedown="setclick('$k','decpriority','','$uci');">
				|<a name="dummy3"></a>
				|<a href="#dummy3" style="text-decoration: none;">
				|<font color="ff7f7f" size="3">dn</font>
				|</a>
				|</td>
			""".stripMargin

			val commentcontent=s"""
				|$td
				|<span style="cursor: pointer;">
				|<font color="#000000" size="3" onmousedown="setclick('$k','comment','','$uci');">$comment</font>
				|</span>
				|</td>
			""".stripMargin

			val annotcontrolcontent=if(short) "" else s"""
				|$annots
				|
				|$td
				|<span style="cursor: pointer;">
				|<font color="#ff0000" size="3" onmousedown="setclick('$k','del','','$uci');">X</font>
				|</span>
				|</td>
			""".stripMargin

			val movecontent=s"""
				|<td width="125" align="center" onmousedown="setclick('$k','make','','$uci');">
				|<a name="dummy"></a>
				|<a href="#dummy" style="text-decoration: none;">
				|<font color="$col" size="6"><b>
				|$made$k<small>$branches</small>
				|</b></font>
				|</a>
				|</td>
				|
				|$td<font color="$col" size="5"><b>$annot</b></font></td>
			""".stripMargin

			val resultstatscontent=if(hideresultstats) "" else s"""
				|$td<font color="#007f00" size="4"><b>$wins<b></font></td>
				|$td<font color="#00007f" size="4"><b>$draws<b></font></td>
				|$td<font color="#7f0000" size="4"><b>$losses<b></font></td>
			""".stripMargin

			val statscontent=if(nostats) "" else s"""
				|$td<font color="#000000" size="4"><b>$plays</b></font></td>
				|$resultstatscontent
			""".stripMargin

			val pvcontent=if(!collectpv) "" else s"""
				|<td>
				|<font color="blue"><b>${collect_pv(k,book)}</b></font>
				|</td>
			""".stripMargin

			val metbymovecontent=if(!showmetbymove) "" else s"""
				|<td align="center">
				|<font size="4"><b>${collect_pv(k,book,1)}</b></font>
				|</td>
			""".stripMargin

			s"""
				|<tr style="background-color: $commentcol; $border">
				|
				|$movecontent
				|
				|$metbymovecontent
				|
				|$prioritycontent
				|
				|$statscontent
				|
				|$annotcontrolcontent
				|
				|$commentcontent
				|
				|$pvcontent
				|
				|</tr>
				|
				|<tr><td></td></tr>
			""".stripMargin
			}).mkString("\n")

		val numannots=butils.ANNOTATIONS.size

		val b=new board
		b.set_from_fen(tfen)
		b.genMoveList
		val nolegalmoves=b.move_list.length
		val nobookmoves=skeys.length

		s"""
			|<script>
			|var key="";
			|var action="";
			|var param="";
			|var uci="";
			|function setclick(setkey,setaction,setparam,setuci)
			|{
			|	key=setkey;
			|	action=setaction;
			|	param=setparam;
			|	uci=setuci;
			|}
			|</script>
			|<table border="0" cellpadding="3" cellspacing="3" style="border-collapse: collapse;">
			|<tr>
			|$td <i>move $nobookmoves / $nolegalmoves</i></td>			
			|$td <i>annot</i></td>
			|${if(showmetbymove) """<td align="center"><i>met by</i></td>""" else ""}
			|${if(short) "" else """<td align="center" colspan="3"><i>priority</i></td>"""}
			|${if(nostats) "" else s"""$td <i>plays</i></td>"""}
			|${if(nostats||hideresultstats) "" else s"""$td <i>white wins</i></td>"""}
			|${if(nostats||hideresultstats) "" else s"""$td <i>draws</i></td>"""}
			|${if(nostats||hideresultstats) "" else s"""$td <i>black wins</i></td>"""}
			|${if(short) "" else s"""<td align="center" colspan="$numannots"><i>annotate</i></td>"""}
			|${if(short) "" else """<td align="center"><i>del</i></td>"""}
			|<td align="center"><i>comment</i></td>
			|${if(!collectpv) "" else """<td align="center"><i>pv</i></td>"""}
			|</tr>
			|$items
			|</table>
		""".stripMargin
	}
}

case class Book(
	name:String
)
{
	var positions=Map[String,BookPosition]()

	def reset
	{
		positions=Map[String,BookPosition]()
	}

	def clear_level
	{
		for((k,v) <- positions) v.clear_level
	}

	def add_position(tfen:String):BookPosition =
	{
		if(!positions.contains(tfen)) positions+=(tfen->BookPosition(tfen))
		positions(tfen)
	}

	def toXml=
	{
		val position_list=(for((k,v)<-positions) yield v.toXml).toList

		{
			<book name={name}>
			<positionlist>
				{position_list}
			</positionlist>
			</book>
		}
	}

	def fromXml(xml:NodeSeq)
	{
		positions=Map[String,BookPosition]()

		val positionentries=( xml \ "positionlist" \ "position" )

		for(positionentry<-positionentries)
		{
			val tfen=(positionentry \ "@tfen").text

			val position=BookPosition(tfen)

			position.fromXml(positionentry)

			positions+=(tfen->position)
		}
	}

	def path = "stuff/"+board.variant+"/books/"+name+".xml"

	def Save
	{
		for((k,v) <- positions)
		{
			if(v.entries.size<=0) positions-=k
		}		

		DataUtils.mkdirs(List("stuff",board.variant,"books"))

		scala.xml.XML.save(path,toXml)
	}

	def SaveAndTellSize:Int =
	{
		Save

		DataUtils.GetFileSize(path)
	}

	def Load
	{
		if(!((new File(path)).exists)) return

		val xml=scala.xml.XML.loadFile(path)

		fromXml(xml)
	}
}