package smartchess

object DocUtils
{
	def wikidir = "../smartchess.wiki"
	val ipathes=Map[String,String]("web"->"web/","icon"->"src/main/resources/icons/")
	def mdipath(name:String,path:String="web"):String =
		"https://raw.githubusercontent.com/smartchessguiapp/smartchess/master/"+ipathes(path)+name
	def htmlipath(name:String,path:String="web"):String =
		(new java.io.File(ipathes(path)+name)).toURI.toString

	def as_id(what:String):String = what.toLowerCase.replaceAll(" ","-")

	def get_html_file_uri(path:String,bookmark:String=null):String =
	{
		(new java.io.File(path)).toURI.toString+(if(bookmark!=null) "#"+bookmark else "")
	}

	def Embed(what:String) = s"""
		|<script>
		|var chapter=0;
		|var paragraph=0;
		|var clicked=0;
		|var evkind="";
		|var evid="";
		|var evvalue="";
		|var chaptertitle="";
		|var paragraphtitle="";
		|function setclick(setchapter,setparagraph)
		|{
		|clicked=1;
		|chapter=setchapter;
		|paragraph=setparagraph;
		|}		
		|function setappclick(setevkind,setevid,setevvalue)
		|{
		|clicked=2;
		|evkind=setevkind;
		|evid=setevid;
		|evvalue=setevvalue;
		|}
		|function setdocclick(setchaptertitle,setparagraphtitle)
		|{
		|clicked=3;
		|chaptertitle=setchaptertitle;
		|paragraphtitle=setparagraphtitle;
		|}
		|</script>
		|
		|$what
	""".stripMargin
}

abstract class DItem
{
	def ReportMDRow:String // abstract
	def ReportHTMLTableRow:String // abstract
}

abstract class TSubItem
{
	def ReportMDContent:String // abstract
	def ReportHTMLContent:String // abstract
}

case class RawText(
	text:String=""
) extends TSubItem
{
	def stext = text.replaceAll("[\\r\\n\\t]","")

	def ReportMDContent:String =
	{
		stext
	}

	def ReportHTMLContent:String =
	{
		stext
	}
}

case class AppLink(
	text:String="",
	evkind:String="menuitem clicked",
	evid:String="{profilesettingsmenu}",
	evvalue:String="Profile"
) extends TSubItem
{
	def stext = text.replaceAll("[\\r\\n\\t]","")

	def ReportMDContent:String =
	{
		stext
	}

	def ReportHTMLContent:String =
	{
		s"""
			|<span style="cursor: pointer;" onmousedown='setappclick("$evkind","$evid","$evvalue");'>
			|<font color="blue">
			|$stext
			|</font>
			|</span>
		""".stripMargin
	}
}

case class DocLink(
	text:String="",
	chaptertitle:String="Chapter Title",
	paragraphtitle:String="Paragraph title"
) extends TSubItem
{
	def stext = text.replaceAll("[\\r\\n\\t]","")

	def ReportMDContent:String =
	{
		val titleasid=DocUtils.as_id(paragraphtitle)
		s"[$stext](${chaptertitle.replaceAll(" ","-")}#$titleasid)"
	}

	def ReportHTMLContent:String =
	{
		s"""
			|<span style="cursor: pointer;" onmousedown='setdocclick("$chaptertitle","$paragraphtitle");'>
			|<font color="blue">
			|$stext
			|</font>
			|</span>
		""".stripMargin
	}
}

case class CTextItem(
	val items:List[TSubItem]=List[TSubItem]()
) extends DItem
{
	def ReportMDRow =
	{
		items.map(x => x.ReportMDContent).mkString(" ")
	}

	def ReportHTMLTableRow =
	{
		val itemscontent=items.map(x => x.ReportHTMLContent).mkString("\n")

		s"""
			|<tr>
			|<td colspan="2">
			|$itemscontent
			|</td>
			|</tr>
		""".stripMargin
	}
}

case class TextItem(
	val text:String=""
) extends DItem
{
	def ReportMDRow =
	{
		text
	}

	def ReportHTMLTableRow =
	{
		s"""
			|<tr>
			|<td colspan="2">
			|$text
			|</td>
			|</tr>
		""".stripMargin
	}
}

case class ImageItem(
	val name:String="image.jpg",
	val path:String="web",
	val width:Int= -1
) extends DItem
{
	def ReportMDRow =
	{
		s"![alt tag](${DocUtils.mdipath(name,path)})"
	}

	def ReportHTMLTableRow =
	{
		s"""
			|<tr>
			|<td colspan="2">
			|<img src="${DocUtils.htmlipath(name,path)}"${if(width>=0) s""" width="$width"""" else ""}>
			|</td>
			|</tr>
		""".stripMargin
	}
}

case class Paragraph(
	title:String="Untitled",
	ditems:List[DItem]=List[DItem]()
)
{
	var chapter:Int=0
	var paragraph:Int=0

	def title_as_id:String = DocUtils.as_id(title)

	def ReportMDRow:String =
	{
		val itemscontent=(for(ditem <- ditems) yield ditem.ReportMDRow).mkString("\n")

		s"# $title\n[top](#top)\n\n$itemscontent"
	}	

	def ReportHTMLTableRow:String =
	{
		val itemscontent=(for(ditem <- ditems) yield ditem.ReportHTMLTableRow).mkString("\n")

		s"""
			|<tr>
			|<td><font size="5">&nbsp;</font></td>
			|</tr>
			|<tr>
			|<td>
			|<h3 id="c${chapter}p${paragraph}" style="cursor: pointer;">${title}</h3>
			|</td>
			|<td>
			|[
			|<a href="#top" style="text-decoration: none;">			
			|top			
			|</a>
			|,
			|<span style="cursor: pointer;" onmousedown='setclick(-1,0);'>
			|<font color="blue">
			|TOC			
			|</font>
			|</span>
			|]
			|</td>
			|</tr>
			|
			|$itemscontent
		""".stripMargin
	}
}

case class Chapter(
	title:String="Untitled",
	paragraphs:List[Paragraph]=List[Paragraph]()
)
{
	var chapter:Int=0

	def update_paragraph_indices
	{
		var i= -1
		for(p <- paragraphs)
		{
			i+=1
			p.chapter=chapter
			p.paragraph=i
		}
	}

	def get_paragraph_index_by_title(paragraphtitle:String):Int =
	{
		var i= -1
		for(p <- paragraphs)
		{
			i+=1
			if(p.title == paragraphtitle) return i
		}

		0
	}

	def ReportMD:String =
	{
		var i= -1
		val tcontent=(for(p <- paragraphs) yield
		{
			val titleasid=p.title_as_id
			s"""[${p.title}](#$titleasid)"""
		}).mkString("\n\n")

		i= -1
		val pcontent=(for(p <- paragraphs) yield
		{
			i+=1
			p.ReportMDRow
		}).mkString("\n")	

		val content=s"""# <a name="top"></a>$title${"\n\n"}$tcontent${"\n\n"}$pcontent"""

		content
	}

	def ReportHTML:String =
	{

		var i= -1
		val tcontent=(for(p <- paragraphs) yield
		{
			i+=1
			s"""
				|<tr>
				|<td>
				|<a href="#c${chapter}p${i}" style="text-decoration: none;">
				|${p.title}
				|</a>
				|</td>
				|<td>
				|</td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		i= -1
		val pcontent=(for(p <- paragraphs) yield
		{
			i+=1
			p.ReportHTMLTableRow
		}).mkString("\n")	

		val content=s"""
			|<a id="top"></a>
			|<table>
			|<tr>
			|<td>
			|<h2>$title</h2>
			|</td>
			|<td onmousedown="setclick(-1,0);">
			|<span style="cursor: pointer;">
			|<font color="blue">
			|Back to Table of contents [ TOC ]
			|</font>
			|</span>
			|</td>			
			|</tr>
			|<tr>
			|<td colspan="2">
			|<hr>
			|</td>
			|</tr>
			|$tcontent
			|$pcontent
			|</table>
		""".stripMargin

		DocUtils.Embed(content)
	}
}

case class Document(
	id:String,
	title:String="Untitled",
	chapters:List[Chapter]=List[Chapter]()
)
{
	update_chapter_indices

	def update_chapter_indices
	{
		var i= -1
		for(c <- chapters)
		{
			i+=1
			c.chapter=i
			c.update_paragraph_indices
		}
	}

	def WebviewId = s"{document webview $id}"
	def StageId = s"{document stage $id}"

	def handler(ev:MyEvent)
	{
		ev.kind match
		{
			case "webview clicked" =>
			{
				if(ev.Id==WebviewId)
				{
					WebViewClicked
				}
			}
			case _ =>
		}
	}

	def open
	{
		if(Builder.HasStage(StageId)) return

		val blob=s"""
			|<vbox>
			|<webview id="$WebviewId"/>
			|</vbox>
		""".stripMargin

		Builder.MyStage(StageId,title,blob,
			modal=false,usewidth=true,useheight=true,handler=handler)

		LoadContent(TableOfContents,title="Table of Contents")
	}

	def TableOfContents:String =
	{
		var i= -1
		val ccontent=(for(c <- chapters) yield
		{
			i+=1
			s"""
				|<tr>
				|<td onmousedown="setclick($i,0);">
				|<h2 style="cursor: pointer;">${c.title}</h2>
				|</td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val content=s"""
			|<table cellpadding="0" cellspacing="0">
			|<tr>
			|<td>
			|<h1>$title - Table of Contents</h1>
			|</td>
			|</tr>
			|$ccontent
			|</table>
		""".stripMargin

		DocUtils.Embed(content)
	}

	def chapter_ok(i:Int):Boolean =
	{
		if(i<0) return false
		if(i >= chapters.length) return false
		true
	}

	def get_chapter_index_by_title(chaptertitle:String):Int =
	{
		var i= -1
		for(c <- chapters)
		{
			i+=1
			if(c.title == chaptertitle) return i
		}

		0
	}

	def LoadContent(what:String,bookmark:String=null,title:String="")
	{
		DataUtils.WriteStringToFile("stuff/temp.html",what)
		val uri=DocUtils.get_html_file_uri("stuff/temp.html",bookmark)
		Builder.GetMyWebView(WebviewId).GetNode.asInstanceOf[javafx.scene.web.WebView].getEngine().
			load(uri)
		Builder.GetStage(StageId).SetTitle("Help topics  |  "+title)
	}

	def WebViewClicked
	{
		val e=Builder.GetWebEngine(WebviewId)
		if(e==null) return

		val clicked=DataUtils.ParseInt(e.executeScript("clicked").toString(),0)
		if(clicked==0) return

		e.executeScript("clicked=0;")

		val chapter=DataUtils.ParseInt(e.executeScript("chapter").toString(),0)
		val paragraph=DataUtils.ParseInt(e.executeScript("paragraph").toString(),0)
		val evkind=e.executeScript("evkind").toString()
		val evid=e.executeScript("evid").toString()
		val evvalue=e.executeScript("evvalue").toString()
		val chaptertitle=e.executeScript("chaptertitle").toString()
		val paragraphtitle=e.executeScript("paragraphtitle").toString()

		if(clicked==2)
		{
			val comp=new MyDummy(evid)
			val ev=MyEvent(evkind,comp,evvalue)			
			MyActor.queuedExecutor ! ExecutionItem(client=s"Document.Fire",code=new Runnable{def run{
				MyApp.handler(ev)
			}})			
			return
		}

		if(clicked==3)
		{
			val ci=get_chapter_index_by_title(chaptertitle)

			val c=chapters(ci)

			val pi=c.get_paragraph_index_by_title(paragraphtitle)

			val bookmark=s"c${ci}p${pi}"

			val title=s"$chaptertitle - $paragraphtitle"

			LoadContent(c.ReportHTML,bookmark,title=title)
			LoadContent(c.ReportHTML,bookmark,title=title)

			return
		}

		if(chapter == -1)
		{
			LoadContent(TableOfContents,title="Table of Contents")
			return
		}

		if(!chapter_ok(chapter)) return

		val c=chapters(chapter)

		LoadContent(c.ReportHTML,title=c.title)
	}

	def create_wiki
	{
		new java.io.File(DocUtils.wikidir).mkdir

		var i= -1
		for(c <- chapters)
		{
			i+=1
			val md=c.ReportMD

			val path=DataUtils.PathFromList(List(DocUtils.wikidir,c.title+".md"))

			DataUtils.WriteStringToFile(path,md)
		}
	}
}