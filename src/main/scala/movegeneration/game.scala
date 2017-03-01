package smartchess

import board._
import move._
import square._
import piece._

import org.apache.commons.codec.digest.DigestUtils._

// game is responsible for representing, reporting and manipulating a chess game

object game
{
	def split_pgn(fpgn:String):scala.collection.mutable.ArrayBuffer[String]=
	{

		var pgn=fpgn.replaceAll("\r\n","\n")
		pgn=fpgn.replaceAll("\r","")

		val lines=pgn.split("\n"):+""

		var pgn_list=scala.collection.mutable.ArrayBuffer[String]()

		val READING_HEAD=0
		val READING_BODY=1

		var status=READING_HEAD

		var content=""

		var i=0
		while(i< lines.length)
		{

			var line=lines(i)

			val empty=(line=="")

			line=line+"\n"

			if(empty)
			{

				if(status==READING_BODY)
				{

					pgn_list+=content

					content=""

					status=READING_HEAD

				}
				else
				{

					content+="\n"

				}

			}
			else
			{

				if(status==READING_BODY)
				{

					content+=line

				}
				else
				{

					if(line(0)=='[')
					{

						content+=line

					}
					else
					{

						status=READING_BODY

						content+=line

					}

				}

			}

			i+=1
		}

		pgn_list

	}
}

// GameNode holds a game node

case class GameNode(
	genSan:String="",
	genAlgeb:String="",
	genTrueAlgeb:String="",
	genFen:String="",
	genTruncFen:String="",
	genPriority:Int=0,
	fen:String="",	
	parent:GameNode=null,
	fullmove_number:Int=0,
	turn:Char=' ',
	num_checks:Map[TColor,Int]=Map(WHITE->0,BLACK->0),	
	var comment:String=""
	)
{
	var priority=0
	var childs=Map[String,GameNode]()

	def sortedSans:List[String] = childs.keys.toList.sortWith(childs(_).genPriority < childs(_).genPriority)

	def get_fullmove_number:Int=
	{
		if(parent==null)
		{
			return 0
		}
		return parent.fullmove_number
	}

	def get_turn:Char=
	{
		if(parent==null)
		{
			return ' '
		}
		return parent.turn
	}

	def get_move_no:String=
	{
		if(parent==null)
		{
			return ""
		}
		var move_no=""
		if(parent.fullmove_number>0)
		{
			move_no=parent.fullmove_number+"."
			if(parent.turn=='b') move_no+=" ..."
		}
		move_no
	}

	def commented_san:String=
	{
		if(comment=="") return genSan
		s"$genSan {$comment}"
	}
}

// game represents a chess game

class game
{

	var b=new board

	var root=GameNode()

	var current_node=GameNode()

	var pgn_headers=Map[String,String]()

	reset

	def has_moves:Boolean= ( root.childs.keys.toList.length > 0 )

	def set_from_fen(fen:String,clear_headers:Boolean=true)
	{

		b.set_from_fen(fen)

		root=GameNode(
				genSan="*",
				fen=fen,
				genPriority=0,
				parent=null,
				fullmove_number=b.fullmove_number,
				turn=colorLetterOf(b.turn)
			)

		current_node=root

		if(clear_headers)
		{
			pgn_headers=Map[String,String]()
		}

	}

	def set_from_fen_extended(fen:String,set_num_checks:Map[TColor,Int])
	{
		b.set_from_fen(fen)
		b.num_checks=Map(WHITE->set_num_checks(WHITE),BLACK->set_num_checks(BLACK))
	}

	def reset
	{
		b.reset
		set_from_fen(b.report_fen)
	}

	def delete
	{
		if(current_node.parent!=null)
		{
			val delSan=current_node.genSan
			current_node=current_node.parent
			current_node.childs=current_node.childs-delSan
			b.set_from_fen(current_node.fen)
		}
	}

	def hasnode(gn:GameNode,cn:GameNode):Boolean =
	{
		if(gn==null) return false
		if(gn==cn) return true
		for((san,tn) <- cn.childs) if(hasnode(gn,tn)) return true
		false
	}

	def tonode(gn:GameNode)
	{
		if(!hasnode(gn,root)) return
		current_node=gn
		set_from_fen_extended(current_node.fen,current_node.num_checks)
	}

	def tobegin
	{
		current_node=root
		set_from_fen_extended(current_node.fen,current_node.num_checks)
	}

	def forward_node:Boolean=
	{
		if(current_node.childs.keys.toList.length>0)
		{
			val main_san=current_node.sortedSans(0)
			current_node=current_node.childs(main_san)
			return true
		}
		return false
	}

	def forward
	{
		if(forward_node)
		{
			set_from_fen_extended(current_node.fen,current_node.num_checks)
		}
	}

	def toend
	{
		while(forward_node)
		{

		}
		set_from_fen_extended(current_node.fen,current_node.num_checks)
	}

	def back
	{
		if(current_node.parent!=null)
		{
			current_node=current_node.parent
			set_from_fen_extended(current_node.fen,current_node.num_checks)
		}
	}

	def report_fen:String = current_node.fen

	def report_trunc_fen:String = b.report_trunc_fen

	def add_move_to_book(tfen:String,san:String,addtobook:Book,md5:String,maxdepth:Int=20,checkreplica:Boolean=false)
	{
		if(addtobook==null) return

		if(b.fullmove_number>maxdepth) return

		val bpos=addtobook.add_position(tfen)

		val entry=bpos.add_move(san)

		if(!(checkreplica&&bpos.game_list.contains(md5)))
		{
			val plays=entry.plays
			val wins=entry.wins
			val draws=entry.draws
			val losses=entry.losses

			val result=get_result
			
			entry.plays=plays+1

			result match
			{
				case "1-0" => entry.wins=wins+1
				case "1/2-1/2" => entry.draws=draws+1
				case "0-1" => entry.losses=losses+1
				case _ =>
			}

			bpos.add_game(md5)
		}		
	}

	def makeMove(m:move,addcomment:String="",addtobook:Book=null,md5:String=null,maxdepth:Int=20,checkreplica:Boolean=false)
	{
		val genFen=report_fen
		val genTruncFen=report_trunc_fen

		val san=b.toSan(m)
		val algeb=m.toAlgeb
		val true_algeb=b.to_true_algeb(algeb)

		if(san!=null)
		{
			////////////////////////////////////////////
			add_move_to_book(genTruncFen,san,addtobook,md5,maxdepth,checkreplica)
			////////////////////////////////////////////

			b.makeMove(m)
			if(current_node.childs.contains(san))
			{
				current_node=current_node.childs(san)
			}
			else
			{
				current_node.priority+=1
				val newNode=GameNode(
						genSan=san,
						genAlgeb=algeb,
						genTrueAlgeb=true_algeb,
						genFen=genFen,
						genTruncFen=genTruncFen,
						fen=b.report_fen,
						genPriority=current_node.priority,
						parent=current_node,
						fullmove_number=b.fullmove_number,
						turn=colorLetterOf(b.turn),
						num_checks=Map(WHITE->b.num_checks(WHITE),BLACK->b.num_checks(BLACK)),						
						comment=addcomment
					)
				current_node.childs+=(san->newNode)
				current_node=newNode
			}
		}
	}

	def makeSanMove(san:String,addcomment:String="")
	{
		val m=b.sanToMove(san)
		if(m!=null)
		{
			makeMove(m,addcomment)
		}
	}

	def makeAlgebMove(algeb:String,addcomment:String="")
	{
		val m=move(fromalgeb=algeb)
		makeMove(m,addcomment)
	}

	var html_pgn_nodes=scala.collection.mutable.ArrayBuffer[GameNode]()

	def report_pgn_move_list_html(cn:GameNode):String=
	{

		var pgn=""

		html_pgn_nodes=scala.collection.mutable.ArrayBuffer[GameNode]()

		def report_pgn_recursive(gn:GameNode,sub:Boolean,level:Int=0)
		{

			val sortedSans=gn.sortedSans

			val numSans=sortedSans.length

			var i=0
			for(san<-sortedSans)
			{

				val is_sub=(i>0)

				val child=gn.childs(san)

				var move_no=child.get_move_no

				if(is_sub)
				{
					pgn+=s"""
						|<font color="#ff0000">($move_no
					""".stripMargin
				}
				else if(child.get_turn=='w')
				{
					val color=if(level==0) "#0000ff" else "#ff0000"
					pgn+=s"""
						|<font color="$color">$move_no</font>
					""".stripMargin
				}

				//val addsan=child.genSan

				val addsan=child.commented_san

				val addlen=addsan.length

				val index=html_pgn_nodes.length

				val sannode=gn.childs(san)

				html_pgn_nodes+=sannode

				var style="padding: 4px;"

				var action=""

				if(cn==sannode)
				{
					style="background-color: #cfffcf; border-style: solid; border-width: 1px; border-color: #000000; border-radius: 10px; padding: 3px;"
					action="editcomment"
				}

				pgn+=s""" <span id="san$index" onmousedown="x='$index';action='$action';" style="cursor: pointer; $style">$addsan</span> """

				if(is_sub) report_pgn_recursive(gn.childs(san),true,level+1)

				i+=1

			}

			if(numSans>0)
			{
				val main_child=gn.childs(sortedSans(0))

				val sortedSansMain=main_child.sortedSans

				val numSansMain=sortedSansMain.length

				if((numSansMain>0)&&(numSans>1))
				{
					val mains_main_child=main_child.childs(sortedSansMain(0))

					val moveno=mains_main_child.get_move_no

					if(main_child.turn=='b')
					{
						pgn+=s"""
							|<font color="#0000ff">$moveno</font> 
						""".stripMargin
					}
				}

				report_pgn_recursive(main_child,sub,level)
			}
			else if(sub)
			{
				pgn+="""|)</font> 
				""".stripMargin
			}

		}

		val dummy=new board
		dummy.set_from_fen(root.fen)

		if(dummy.getturn==BLACK)
		{
			val fmn=dummy.fullmove_number
			pgn+=s"""
				|<font color="#0000ff">$fmn. ...</font> 
			""".stripMargin
		}
		else if(!has_moves)
		{
			val fmn=dummy.fullmove_number
			pgn+=s"""
				|<font color="#0000ff">$fmn. </font> 
			""".stripMargin
		}

		if(has_moves)
		{
			report_pgn_recursive(root,false)
		}

		pgn=pgn.replaceAll(" +"," ")
		pgn=pgn.replaceAll(" +\\)",")")

		pgn
	}

	val preferred_pgn_headers:List[String]=List("Event","Site","Date","Round","White","Black","Result","TimeControl",
		"Time","TimeZone","Variant","FEN","Termination","Annotator","StartPosition","Opening","PlyCount","ECO")

	def get_result:String =
	{
		get_header("Result")
	}

	def get_termination:String=
	{
		var term=""

		if(pgn_headers.contains("Result"))
		{
			term+=" "+pgn_headers("Result")
		} else {
			term+=" *"
		}

		if(pgn_headers.contains("Termination"))
		{
			term+=" {"+pgn_headers("Termination")+"}"
		}

		term
	}

	def pgn_header_sort_func(a:String,b:String):Boolean=
	{
		val ai=preferred_pgn_headers.indexOf(a)
		val bi=preferred_pgn_headers.indexOf(b)
		if((ai< 0)&&(bi< 0)) return false
		if(ai< 0) return false
		if(bi< 0) return true
		ai< bi
	}

	def sorted_pgn_header_keys:List[String]=
	{
		pgn_headers.keys.toList.sortWith(pgn_header_sort_func)
	}

	var report_headers_html=""

	def report_pgn_html(cn:GameNode):String=
	{
		pgn_headers+=("FEN"->root.fen)
		pgn_headers+=("Variant"->board.variant)

		report_headers_html=(for(k<-sorted_pgn_header_keys) yield 
		{
			val v=pgn_headers(k)
			s"""
				|<tr onmousedown="x='edit'; field='$k';">
				|<td><font color="#7f0000">$k</font></td>
				|<td>&nbsp;&nbsp;&nbsp;<font color="#00007f">$v</font></td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val move_list_html=report_pgn_move_list_html(cn)

		val term=get_termination

		s"""
			|<script>
			|var x="";
			|var field="";
			|var action="";
			|</script>
			|<div style="font-family: monospace; font-size: 28px; font-weight: bold;">
			|<table cellpadding="0" cellspacing="0">
			|$report_headers_html
			|</table>
			|<br>
			|$move_list_html
			|$term
			|</div>
		""".stripMargin
	}

	def report_pgn_move_list:String=
	{

		var pgn=""

		def report_pgn_recursive(gn:GameNode,sub:Boolean)
		{

			val sortedSans=gn.sortedSans

			val numSans=sortedSans.length

			var i=0
			for(san<-sortedSans)
			{

				val is_sub=(i>0)

				val child=gn.childs(san)

				var move_no=child.get_move_no

				if(is_sub)
				{
					pgn+="("+move_no
				}
				else if(child.get_turn=='w')
				{
					pgn+=move_no
				}

				//val addsan=child.genSan

				val addsan=child.commented_san

				pgn+=" "+addsan+" "

				//if(child==current_node) pgn+=" {*} "

				if(is_sub) report_pgn_recursive(gn.childs(san),true)

				i+=1

			}

			if(numSans>0)
			{
				val main_child=gn.childs(sortedSans(0))

				val sortedSansMain=main_child.sortedSans

				val numSansMain=sortedSansMain.length

				if((numSansMain>0)&&(numSans>1))
				{
					val mains_main_child=main_child.childs(sortedSansMain(0))

					if(main_child.turn=='b')
					{
						pgn+=" "+mains_main_child.get_move_no+" "
					}
				}

				report_pgn_recursive(main_child,sub)
			}
			else if(sub)
			{
				pgn+=") "
			}

		}

		val dummy=new board
		dummy.set_from_fen(root.fen)

		if(dummy.getturn==BLACK)
		{
			pgn+=dummy.fullmove_number+". ... "
		}
		else if(!has_moves)
		{
			pgn+=dummy.fullmove_number+". "
		}

		if(has_moves)
		{
			report_pgn_recursive(root,false)
		}		

		pgn=pgn.replaceAll(" +"," ")
		pgn=pgn.replaceAll(" +\\)",")")

		pgn
	}

	var report_headers=""

	var replen=0

	def report_pgn:String=
	{
		pgn_headers+=("FEN"->root.fen)
		pgn_headers+=("Variant"->board.variant)

		report_headers=(for(k<-sorted_pgn_header_keys) yield { val v=pgn_headers(k); s"""[$k "$v"]""" } ).mkString("\n")

		replen=report_headers.length+2

		val move_list=report_pgn_move_list

		List(report_headers,move_list).mkString("\n\n")+get_termination
	}

	def get_header(key:String):String=
		(if(pgn_headers.contains(key)) pgn_headers(key) else "?")

	var PARSING=false

	def preprocess_pgn(set_pgn:String):String=
	{
		val prpath="stuff"+java.io.File.separator+"preprocesspgn.xml"
		if(!new java.io.File(prpath).exists()) return set_pgn
		var pgn=set_pgn
		val xml=scala.xml.XML.loadFile(prpath)
		val replacealls=xml \ "replaceall"
		for(replaceall<-replacealls)
		{
			val regexp=(replaceall \ "regexp").text
			val replacement=(replaceall \ "replacement").text
			pgn=pgn.replaceAll(regexp,replacement)
		}
		pgn
	}

	def parse_pgn(set_pgn:String,head_only:Boolean=false,addtobook:Book=null,maxdepth:Int=20,checkreplica:Boolean=false)
	{

		PARSING=true

		val READING_HEAD=0
		val READING_BODY=1

		var status=READING_HEAD

		var pgn=set_pgn
		val md5=md5Hex(pgn)

		pgn=preprocess_pgn(pgn)

		pgn=pgn.replaceAll("\r\n","\n")
		pgn=pgn.replaceAll("\r","")

		val lines=pgn.split("\n")

		var move_list=""

		reset

		var i=0
		var done=false
		while((i< lines.length)&&(!done))
		{

			val line=lines(i)

			val empty=(line=="")

			if(empty)
			{
				if(status==READING_BODY)
				{
					done=true
				}
			}
			else
			{

				if(status==READING_BODY)
				{
					move_list+=(line+" ")
				}
				else
				{
					if(line(0)=='[')
					{
						val pline=line.replaceAll("\\[|\\]","")

						val parts=pline.split(" +"+'"')

						val key=parts(0)

						val quot='"'

						val value=parts(1).replaceAll(quot+"$","")

						pgn_headers+=(key->value)
					}
					else
					{
						status=READING_BODY
						move_list+=(line+" ")
					}
				}
			}

			i+=1
		}

		if(head_only)
		{
			PARSING=false
			return
		}

		//////////////////////////////////////////////////////////

		if(pgn_headers.contains("Variant"))
		{
			val pgn_variant=pgn_headers("Variant")
			if(pgn_variant!=board.variant)
			{
				println("Warning: wrong variant, PGN move list could not be parsed.")
				PARSING=false
				return
			}
		}

		if(pgn_headers.contains("FEN"))
		{
			set_from_fen(pgn_headers("FEN"),clear_headers=false)
		}

		val White=get_header("White")
		val Black=get_header("Black")

		// convert white spaces to space
		move_list=move_list.replaceAll("[\r\n\t]"," ")

		// separate comments
		move_list=move_list.replaceAll("\\}","} ")
		move_list=move_list.replaceAll("\\{"," {")

		// replace multiple spaces with single space
		move_list=move_list.replaceAll(" +"," ")
		// remove leading and trailing spaces
		move_list=move_list.replaceAll("^ | $","")

		// variation opening joined with move
		move_list=move_list.replaceAll("\\( ","(")
		// variation closing joined with move
		move_list=move_list.replaceAll(" \\)",")")
		// separate multiple closings
		move_list=move_list.replaceAll("\\)\\)",") )")

		// separate closing from comment end
		move_list=move_list.replaceAll("\\}\\)","} )")

		var moves=move_list.split(" ")

		var commentbuff=""

		var previouswasmove=false

		def parse_moves_recursive(addtobook:Book=addtobook)
		{

			while(moves.length>0)
			{

				var move=moves.head

				moves=moves.tail

				if(move.length>0)
				{

					def addcomment
					{
						commentbuff=commentbuff.replaceAll("^\\{|\\}$","")
						if(previouswasmove)
						{
							current_node.comment=commentbuff
						}
						commentbuff=""
						previouswasmove=false
					}

					if(DataUtils.BeginsWith(move,'{'))
					{
						commentbuff=move
						if(DataUtils.EndsWith(move,'}')) addcomment
					}
					else if(DataUtils.EndsWith(move,'}'))
					{
						commentbuff+=" "+move
						addcomment
					}
					else if(commentbuff!="")
					{
						commentbuff+=" "+move
					}
					else
					{

						// remove line numbers, dots from moves
						move=move.replaceAll("^[0-9]*[\\.]*","")

						var open_sub=false

						var close_sub=false

						if(DataUtils.BeginsWith(move,'('))
						{
							open_sub=true
							move=move.substring(1)
						}

						if(DataUtils.EndsWith(move,')'))
						{
							close_sub=true
							move=move.substring(0,move.length-1)
						}

						if(open_sub)
						{
							val save_current_node=current_node
							back
							val m=b.sanToMove(move)
							if(m!=null)
							{
								makeMove(m,addtobook=addtobook,md5=md5,maxdepth=maxdepth,checkreplica=checkreplica)
								previouswasmove=true
							} else {
								previouswasmove=false
							}
							parse_moves_recursive()
							current_node=save_current_node
							b.set_from_fen(current_node.fen)							
						}
						else
						{
							if(move=="")
							{
								// commented moves can generate empty closings
								previouswasmove=false
							}
							else
							{
								val m=b.sanToMove(move)

								if(m!=null)
								{
									makeMove(m,addtobook=addtobook,md5=md5,maxdepth=maxdepth,checkreplica=checkreplica)
									previouswasmove=true
								}
								else
								{
									previouswasmove=false
								}
							}
						}

						if(close_sub)
						{
							return
						}

					}

				}

			}

		}

		parse_moves_recursive()

		tobegin

		PARSING=false

	}

	def set_from_pgn(pgn:String)
	{
		parse_pgn(pgn)
	}

	def current_line_pgn:String=
	{

		var fullmove_number=root.fullmove_number
		var turn=root.turn

		var a=scala.collection.mutable.ArrayBuffer[String]()

		var first=true
		for(san<-current_line_moves)
		{
			var psan=san
			if(turn=='w')
			{
				psan=fullmove_number+". "+san
			}
			else if(first==true)
			{
				psan=fullmove_number+". ... "+san
			}
			first=false
			if(turn=='b') fullmove_number+=1
			turn=(if(turn=='w') 'b' else 'w')
			a+=psan
		}

		a.mkString(" ")

	}

	def current_line_length:Int=
	{
		val cal=current_line_algeb
		if(cal=="") return 0
		cal.split(" ").length
	}

	def current_line_algeb:String=
	{

		var a=scala.collection.mutable.ArrayBuffer[String]()

		var first=true
		for(algeb<-current_line_moves_algeb)
		{			
			a+=algeb
		}

		a.mkString(" ")

	}

	def current_line:String=
	{
		current_line_moves.mkString(" ")
	}

	def current_line_moves:scala.collection.mutable.ArrayBuffer[String]=
	{
		var a=scala.collection.mutable.ArrayBuffer[String]()

		var cn=current_node

		while(cn!=root)
		{

			a+=cn.genSan

			cn=cn.parent

		}

		a.reverse
	}

	def current_line_moves_algeb:scala.collection.mutable.ArrayBuffer[String]=
	{
		var a=scala.collection.mutable.ArrayBuffer[String]()

		var cn=current_node

		while(cn!=root)
		{

			a+=cn.genAlgeb

			cn=cn.parent

		}

		a.reverse
	}

	def get_info_line:String =
	{
		val white=get_header("White")
		val black=get_header("Black")

		s"""$white - $black"""
	}

}