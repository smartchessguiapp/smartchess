package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppSolution
{
	import MyApp._

	def GuiLookUpSolution
	{
		LookUpSolution()

		Update
	}

	def GuiLookUpSolutionTree
	{
		LookUpSolution(tree=true)

		Update
	}

	def LookUpSolution(tree:Boolean=false)
	{
		val spath=GS("{components}#{antichessdir}","")+java.io.File.separator
		val algebline=g.current_line_algeb
		val alen=algebline.length
		var list=DataUtils.getListOfFileNames(spath)
		val lists=Map[String,List[String]](
			"e2e3 b7b5" -> List[String]("e3b5.lines.txt"),
			"e2e3 c7c5" -> List[String]("e3c5.lines.txt"),
			"e2e3 g7g5" -> List[String]("e3g5.lines.txt"),
			"e2e3 c7c6" -> List[String]("e3c6.lines.txt"),
			"e2e3 e7e6" -> List[String]("e3e6.lines.txt"),
			"e2e3 g8h6" -> List[String]("e3Nh6.lines.txt"),
			"e2e3 b8c6" -> List[String]("e3Nc6.lines.txt"),
			"e2e3 b7b6" -> List[String]("e3b6.lines.part1.txt","e3b6.lines.part2.txt")
			)
		if(algebline.length>=9)
		{
			val key=algebline.substring(0,9)
			if(lists.contains(key)) list=lists(key)
		}
		println("\n------------\nLooking for : "+algebline+"\n------------\n")
		var allmoves=Map[String,Int]()
		var sublines=scala.collection.mutable.ArrayBuffer[String]()
		for(name<-list)
		{
			print("Reading : "+name+" ")
			val page=DataUtils.ReadFileToString(spath+name)
			print("( size : "+page.length+" ) lines : ")
			val lines=page.split("\n")
			println(lines.length)
			var count=0
			var moves=Map[String,Int]()
			for(line<-lines)
			{
				if(line.length>=alen)
				{
					if(line.substring(0,alen)==algebline)
					{
						count+=1

						if(line.length>=(alen+5))
						{
							val offset=(if(algebline=="") 0 else 1)
							var move=line.substring(alen+offset,alen+offset+4)
							var subline=line.substring(alen+offset)
							sublines+=subline
							if(line.length>=(alen+6))
							{
								val pp=line.substring(alen+offset+4,alen+offset+5)
								if(pp!=" ") move+=pp
							}
							if(moves.contains(move))
							{
								moves+=(move->(moves(move)+1))
							}
							else
							{
								moves+=(move->1)
							}
							if(allmoves.contains(move))
							{
								allmoves+=(move->(allmoves(move)+1))
							}
							else
							{
								allmoves+=(move->1)
							}
						}
					}					
				}
			}
			if(count>0)
			{
				println("Matches : "+count+" Moves : "+moves)
			}
		}
		val allmovessorted=scala.collection.immutable.ListMap(allmoves.toSeq.sortWith(_._2 > _._2):_*)
		println("\n\nSummary :\n\n"+(for((move,num)<-allmovessorted) yield (move+" ( "+num+" )" )).mkString("\n"))

		if(tree)
		{
			def UpdateTreeLookup
			{
				Builder.CloseAbortDialog
				MyActor.queuedExecutor ! ExecutionItem(client="MyApp.UpdateTreeLookup",code=new Runnable{def run{					
					SelectBookTab
					Update
				}})
			}
			var lookupaborted=false
			Builder.AbortDialog(title="Abort building solution tree",callback=()=>{lookupaborted=true})
			SelectLogTab
			Future
			{
				var i=0
				for(subline <- sublines)
				{
					if(lookupaborted)
					{						
						UpdateTreeLookup					
						return
					}
					val lg=new game
					lg.set_from_fen(g.report_fen)
					val algebs=subline.split(" ").toList
					for(algeb <- algebs)
					{
						if(lg.b.isAlgebLegal(algeb))
						{
							val san=lg.b.toSan(move(fromalgeb=algeb))
							val bpos=book.add_position(lg.report_trunc_fen)
							val entry=bpos.add_move(san)
							val annot=if(lg.b.getturn==piece.WHITE) "!" else "!?"							
							entry.comment="solution"
							entry.annot=annot

							val score=if(lg.b.getturn==piece.WHITE) 9990 else 0
							entry.hasenginescore=true
							entry.enginescore=score

							entry.plays=entry.plays+1
							lg.makeSanMove(san)
						}
					}
					i+=1
					MyActor.Log(i+". lookup line added : "+subline)
				}
				UpdateTreeLookup
			}
		}
		else
		{
			var i=0
			val madesans=g.current_node.childs.keys.toList
			for((algeb,num)<-allmovessorted)
			{
				if(g.b.isAlgebLegal(algeb))
				{
					val m=move()
					m.from_algeb(algeb)
					val san=g.b.toSan(m)
					val annot=if(g.b.getturn==piece.WHITE) "!" else "!?"
					val bpos=AddPositionToBook(g.report_trunc_fen)
					val entry=bpos.add_move(san)
					entry.comment="solution"
					entry.annot=annot

					val score=if(g.b.getturn==piece.WHITE) 9990 else 0
					entry.hasenginescore=true
					entry.enginescore=score

					entry.plays=num
					i+=1
				}
			}
		}
	}
}