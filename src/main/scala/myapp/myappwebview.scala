package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppWebview
{
	import MyApp._

	def MovesClicked
	{
		val e=GetWebEngine("{moveswebview}")
		if(e==null) return
		val san=e.executeScript("x").toString()

		g.makeSanMove(san)

		Update
	}

	def SetPriority(san:String)
	{
		val buttons=(for(i <- butils.pborders.keys.toList.sorted.reverse) yield
		{
			val style=butils.pborders(i).replaceAll("border","-fx-border")
			s"""
				|<button id="{setpriority}#{$i}" width="300.0" style="$style" text="$i"/>
			""".stripMargin
		}).mkString("\n")

		val blob=s"""
					|<vbox padding="5" gap="5">					
					|$buttons
					|</vbox>
				""".stripMargin

		def setpriority_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.parts(0)=="setpriority")
				{
					val i=ev.parts(1).toInt

					val bpos=AddCurrentPositionToBook

					val entry=bpos.entries(san)

					entry.priority=i

					CloseStage("{setprioritydialog}")

					Update
				}
			}
		}		

		MyStage("{setprioritydialog}","Set priority",blob,
			modal=true,usewidth=false,useheight=false,handler=setpriority_handler)		
	}

	def BookClicked
	{
		val e=GetWebEngine("{bookwebview}")
		if(e==null) return

		val bpos=AddCurrentPositionToBook

		val key=e.executeScript("key").toString()

		if(!bpos.entries.contains(key)) return

		val entry=bpos.entries(key)

		val action=e.executeScript("action").toString()
		val param=e.executeScript("param").toString()
		val uci=e.executeScript("uci").toString()		

		if(action=="annot")
		{
			entry.annot=param
		}
		else if(action=="make")
		{
			MakeSanMove(key)
		}
		else if(action=="del")
		{
			bpos.delete_move(key)
		}
		else if(action=="comment")
		{
			val comment=bpos.entries(key).comment

			Set("{components}#{textinputs}#{Comment}",comment)

			val r=InputTexts("Edit book comment",List("Comment"),
					applyname="Apply changes",candelete=true,deletemsg="Delete comment")

			if(r.canceled) return

			if(r.deleteitem)
			{
				entry.comment="-"
			}
			else
			{
				entry.comment=r.texts("Comment")
			}
		}
		else if(action=="decpriority")
		{
			entry.priority=entry.priority-1
			if(entry.priority<0) entry.priority=0
		}
		else if(action=="setpriority")
		{
			SetPriority(key)
		}
		else if(action=="incpriority")
		{
			entry.priority=entry.priority+1
			val max=butils.pborders.keys.toList.length-1
			if(entry.priority>max) entry.priority=max
		}

		Update
	}

	def PgnClicked
	{
		val index_str=ExecuteWebScript("{pgnwebview}","x")

		if(index_str!=null)
		{
			if(index_str=="edit")
			{
				val field=ExecuteWebScript("{pgnwebview}","field")

				val value=g.get_header(field)

				Set("{components}#{textinputs}#{Field name}",field)
				Set("{components}#{textinputs}#{Field value}",value)

				val r=InputTexts("Edit PGN field",List("Field name","Field value"),
					applyname="Apply changes",candelete=true,deletemsg="Delete this field")

				if(r.canceled) return

				if(r.deleteitem)
				{
					g.pgn_headers-=field
				}
				else
				{
					g.pgn_headers+=(r.texts("Field name")->r.texts("Field value"))
				}

				Update
			}
			else if(DataUtils.IsInt(index_str))
			{
				val index=index_str.toInt

				val gn=g.html_pgn_nodes(index)

				val action=ExecuteWebScript("{pgnwebview}","action")

				if(action=="editcomment")
				{
					val comment=gn.comment
					
					Set("{components}#{textinputs}#{Comment}",comment)

					val r=InputTexts("Edit PGN comment",List("Comment"),applyname="Apply changes",
						candelete=true,deletemsg="Delete comment")

					if(r.canceled) return

					if(r.deleteitem)
					{
						g.current_node.comment=""
					}
					else
					{
						g.current_node.comment=r.texts("Comment")
					}

					Update
				}
				else
				{
        			g.tonode(gn)

        			Update
				}
    		}
		}
	}
}