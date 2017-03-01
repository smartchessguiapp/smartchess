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

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import collection.JavaConverters._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

////////////////////////////////////////////////////////////////////

case class CurrentAnalysisState
(
	var bestmove:String="",
	var bestmove960:String="",
	var bestmovesan:String="",
	var depth:Int=0,
	var scorenumerical:Int=0
)
{	
}

case class EngineLogItem(effline:String,ftime:String="",out:Boolean=true)

case class AddToEngineLogMsg(e:Engine,eli:EngineLogItem)

case class EngineLogActor(
) extends Actor
{
	def receive =
	{
		case ael:AddToEngineLogMsg =>
		{
			ael.e.AddToEngineOutBuff(ael.eli)

			if(ael.e.IsConsoleOpen)
			{
				ael.e.UpdateEngineOut
			}
		}
		case _ => println("that was unexpected")
	}
}

case class Engine(
)
{
	var uniqueid:Int= -1

	def SetUniqueId(setid:Int)
	{
		uniqueid=setid
		CreateLogActor
	}

	var path:String=null
	var kind:String=null	
	var variant:String= null
	var autoload:Boolean=false

	var engineprocess:Process=null
	var enginein:java.io.InputStream=null
	var engineout:java.io.OutputStream=null
	var enginereadthread:Thread=null

	var commandline=""

	var logactor:ActorRef=null

	var thinkingoutput:ThinkingOutput=null

	var running:Boolean=false

	var bestmovereceived=false

	def ClearBestMoveReceived
	{
		bestmovereceived=false
	}

	def AwaitBestMoveReceived
	{
		if(kind=="uci")
		{
			var timeout=20
			while((!bestmovereceived)&&(timeout>0))
			{				
				try{Thread.sleep(50)}catch{case e:Throwable=>}
				timeout-=1
			}
		}
		else
		{
			try{Thread.sleep(100)}catch{case e:Throwable=>}
		}
	}

	def GetCurrentAnalysisState:CurrentAnalysisState =
	{
		if(thinkingoutput==null)
		{
			return null
		}

		val depth=thinkingoutput.access_maxdepth()

		val bestmove=thinkingoutput.access_bestmove()

		val scorenumerical=thinkingoutput.access_scorenumerical()

		if(bestmove==null)
		{
			return null
		}

		val b=new board
		b.set_from_fen(EngineManager.GetRootFen)

		val bestmove960=b.to_chess960_algeb(bestmove)

		val bestmovesan=b.toSan(move(fromalgeb=bestmove960))		

		val cas=CurrentAnalysisState(
			bestmove=bestmove,
			bestmove960=bestmove960,
			bestmovesan=bestmovesan,
			depth=depth,
			scorenumerical=scorenumerical)

		cas
	}

	def IsLoaded:Boolean =
	{
		engineprocess != null
	}

	def IsRunning = running

	def UpdateButtons
	{
		if(!IsConsoleOpen) return

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.UpdateButtons",dolog=false,code=new Runnable{def run{
			Builder.GetMyButton(s"{load$uniqueid}").SetDisable(IsLoaded)
			Builder.GetMyButton(s"{unload$uniqueid}").SetDisable(!IsLoaded)
			Builder.GetMyButton(s"{start$uniqueid}").SetDisable(IsRunning)
			Builder.GetMyButton(s"{stop$uniqueid}").SetDisable(!IsRunning)
		}})		
	}

	def GetNameFromId(id:String):String=
	{
		val parts=id.split("#").toList
		parts.reverse.head.replaceAll("[\\{\\}]","")
	}

	def Apply(o:Option,reset:Boolean=false)
	{
		if((o.kind!="button")&&(o.dosend==true))
		{
			val name=o.name
			var id=s"{engineoptions}#{$uniqueid}#{$name}"

			var sliderid=id+"#{slider}"
			var textid=id+"#{text}"
			val comboid=id+"#{selected}"

			var value=if(o.kind=="combo") Builder.GS(Builder.Cve(comboid))
			else Builder.GS(Builder.Cve(if(o.kind=="spin") textid else id),o.defaultstr)

			if((o.kind=="spin")&&(value!=o.defaultstr)) value=""+value.toDouble.toInt

			if(reset)
			{
				if(o.kind=="combo")
				{
					Builder.Set(Builder.Cve(comboid),o.defaultstr)
				}
				else if(kind=="spin")
				{
					Builder.Set(Builder.Cve(textid),o.defaultstr)
					Builder.Set(Builder.Cve(sliderid),o.defaultstr)
				}
				else
				{
					Builder.Set(Builder.Cve(id),o.defaultstr)
				}
				value=o.defaultstr
			}

			if(kind=="uci")
			{
				IssueCommand("setoption name "+name+" value "+value)
			}

			if(kind=="xboard")
			{
				var xboardvalue=value
				if(kind=="check")
				{
					xboardvalue=if(value=="true") "1" else "0"
				}

				IssueCommand("option "+name+"="+xboardvalue)
			}
		}
	}

	def ApplyAllOptions
	{		
		for(option<-options.options) Apply(option)
	}

	def ResetAllOptions
	{
		for(option<-options.options) Apply(option,reset=true)
	}

	def options_handler(ev:MyEvent)
	{
		if(ev.kind=="textfield entered")
		{
			if(ev.comp.GS("qualifier")=="text")
			{
				val trueid=ev.TrunkId

				var slidername=GetNameFromId(trueid)

				val valuestr=Builder.GetMyText(ev.Id).GetText

				if(DataUtils.IsInt(valuestr))
				{

					val intvalue=valuestr.toInt

					val sliderid=trueid+"#{slider}"

					Builder.Set(Builder.Cve(sliderid),""+intvalue.toDouble)

					Builder.GetMySlider(sliderid).SetValue(intvalue.toDouble)

					if(kind=="uci")
					{
						IssueCommand("setoption name "+slidername+" value "+intvalue)
					}
					if(kind=="xboard")
					{
						IssueCommand("option "+slidername+"="+intvalue)
					}

				}
			}
		}

		if(ev.kind=="button pressed")
		{
			val isapply=ev.comp.GS("qualifier")=="apply"

			val trueid=if(isapply) ev.TrunkId else ev.Id

			val buttonname=GetNameFromId(trueid)

			if(buttonname=="Reset defaults")
			{	
				ResetAllOptions
				BuildOptions
				return
			}

			if(isapply)
			{

				var value=""

				val comp=Builder.GetComponent(trueid)

				if(comp!=null)
				{
					value=comp.asInstanceOf[MyText].GetText
					Builder.Set(Builder.Cve(trueid),value)
				}

				if(kind=="uci")
				{
					IssueCommand("setoption name "+buttonname+" value "+value)
				}
				if(kind=="xboard")
				{
					IssueCommand("option "+buttonname+"="+value)
				}

			} else {

				if(kind=="uci")
				{
					IssueCommand("setoption name "+buttonname+" value ")
				}
				if(kind=="xboard")
				{
					IssueCommand("option "+buttonname)
				}

			}
		}

		if(ev.kind=="slider changed")
		{
			val trunkid=ev.TrunkId
			val sliderid=trunkid+"#{slider}"
			val textid=trunkid+"#{text}"

			Builder.Set(Builder.Cve(textid),StringData(ev.value))

			val slidername=GetNameFromId(trunkid)

			val sliderint=ev.value.toDouble.toInt

			Builder.GetMyText(textid).SetText(""+sliderint)

			if(kind=="uci")
			{
				IssueCommand("setoption name "+slidername+" value "+sliderint)
			}
			if(kind=="xboard")
			{
				IssueCommand("option "+slidername+"="+sliderint)
			}
		}

		if(ev.kind=="checkbox changed")
		{
			Builder.Set(Builder.Cve(ev.Id),StringData(ev.value))

			val checkboxname=GetNameFromId(ev.Id)

			val checkboxbool=ev.value

			if(kind=="uci")
			{
				IssueCommand("setoption name "+checkboxname+" value "+checkboxbool)
			}
			if(kind=="xboard")
			{
				val xboardcheckboxbool=if(checkboxbool=="true") "1" else "0"
				IssueCommand("option "+checkboxname+"="+xboardcheckboxbool)
			}
		}

		if(ev.kind=="combobox selected")
		{
			val comboboxname=GetNameFromId(ev.Id)

			val value=ev.value

			if(kind=="uci")
			{
				IssueCommand("setoption name "+comboboxname+" value "+value)
			}
			if(kind=="xboard")
			{
				IssueCommand("option "+comboboxname+"="+value)
			}
		}

	}

	def GetEngineSettingsVBox:VBox =
	{
		val esvboxcomp=Builder.GetMyBox(s"{enginesettingsvbox$uniqueid}")
		if(esvboxcomp==null) return null
		esvboxcomp.GetNode.asInstanceOf[VBox]
	}

	def ClearEngineSettingsVBox
	{
		val esvbox=GetEngineSettingsVBox
		if(esvbox==null) return
		MyActor.queuedExecutor ! ExecutionItem(client="Engine.ClearEngineSettingsVBox",code=new Runnable{def run{
			esvbox.getChildren.clear()
			esvbox.getChildren.add(new Label("Load engine for settings."))
		}})		
	}

	def BuildOptions
	{
		val svbox=GetEngineSettingsVBox
		if(svbox==null) return
		val optionscontent=options.ReportBlob
		val blob=s"""
			|<scrollpane id="{enginesettingsscrollpane$uniqueid}">
			|<gridpane hgap="10" vgap="5">
			|$optionscontent
			|</gridpane>
			|</scrollpane>
		""".stripMargin
		val scenegraph=MyComponent.FromBlob(blob,options_handler)
		scenegraph.CreateNode
		svbox.getChildren().clear()
		val parent=scenegraph.GetParent
		svbox.getChildren().add(parent)
	}

	def UpdateOptions
	{
		if(!IsConsoleOpen) return

		if(options==null) return

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.UpdateOptions",code=new Runnable{def run{
			BuildOptions
		}})		
	}

	def SmartStart(searchmoves:List[String]=null)
	{
		OpenConsole

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.SmartStart",code=new Runnable{def run{
			Builder.SelectTab(s"{tabpane$uniqueid}","Search output")
		}})		

		Start(searchmoves)
	}

	def Start(searchmoves:List[String]=null)
	{
		if(!IsLoaded) return

		if(running) return

		///////////////////////////////////////////////////////////
		thinkingoutput=ThinkingOutput(protocol=kind)

		running=true
		///////////////////////////////////////////////////////////

		val fen=EngineManager.GetRootFen

		if(kind=="uci")
		{
			IssueCommand("position fen "+fen)

			if(searchmoves==null)
			{
				IssueCommand("go infinite")
			}
			else
			{
				IssueCommand("go infinite searchmoves "+searchmoves.mkString(" "))
			}
		}

		if(kind=="xboard")
		{
			if(!features.setboard) return

			IssueCommand("force")
			IssueCommand("post")

			IssueCommand(s"setboard $fen")

			IssueCommand("analyze")
		}
	}

	def Stop
	{
		if(!IsLoaded) return

		if(!running) return

		if(kind=="uci")
		{
			IssueCommand("stop")
		}

		if(kind=="xboard")
		{
			IssueCommand("exit")
		}

		running=false
	}

	def CreateLogActor
	{
		if(logactor!=null) return
		val name=s"ELA $variant $uniqueid".replaceAll(" ","")
		logactor=EngineManager.enginelistsystem.actorOf(Props[EngineLogActor], name = name)
	}	

	def EngineConsoleStageId = "{engineconsole "+variant+" "+uniqueid+"}"

	def IsConsoleOpen:Boolean = Builder.HasStage(EngineConsoleStageId)

	def SmartIssueCommand(command:String)
	{
		if(!IsConsoleOpen) return

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.SmartIssueCommand",code=new Runnable{def run{
			Builder.SelectTab(s"{tabpane$uniqueid}","Console")
		}})		

		IssueCommand(command)
	}

	def IssueCommand(command:String)
	{
		if(command==null) return		

		val fullcommand=command+"\n"

		try
		{
			engineout.write(fullcommand.getBytes())
			engineout.flush()
			
			logactor ! AddToEngineLogMsg(this,EngineLogItem(command,ftime=MyActor.GetFormattedTime,out=false))
		}
		catch
		{
			case e: Throwable =>
			{
				val emsg=s"engine write IO exception, command: $command, id: $uniqueid, path: $path"
				println(emsg)
				logactor ! AddToEngineLogMsg(this,EngineLogItem(emsg,ftime=MyActor.GetFormattedTime,out=false))
				//e.printStackTrace
			}
		}
	}

	def DeleteCommandTextfield
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Engine.DeleteCommandTextfield",code=new Runnable{def run{
			Builder.GetMyText(s"{command$uniqueid}").SetText("")
		}})		
	}

	def SmartSetAutoLoad(value:Boolean)
	{
		autoload=value
		EngineManager.enginelist ! SaveMsg
		if(autoload)
		{
			OpenConsole
			
			Load
		}
		else
		{
			ShutDown
		}
		EngineManager.enginelist ! UpdateMsg
	}

	def OpenConsole
	{
		if(IsConsoleOpen) return

		val blob=s"""
			|<vbox>
			|<hbox>
			|<button id="{load$uniqueid}" text="Load"/>
			|<button id="{unload$uniqueid}" text="Unload"/>
			|<textfield id="{command$uniqueid}"/>
			|<button id="{issue$uniqueid}" text="Issue command"/>			
			|<label text="Auto load"/>
			|<checkbox id="{autoload$uniqueid}" forcedefault="true" checked="${""+autoload}" prefixget="variant" prefixset="variant"/>
			|<button id="{start$uniqueid}" text="Start"/>
			|<button id="{stop$uniqueid}" text="Stop"/>			
			|</hbox>
			|<tabpane id="{tabpane$uniqueid}">
			|<tab caption="Console">
			|<vbox>
			|<webview id="{engineoutlog$uniqueid}"/>
			|</vbox>
			|</tab>
			|<tab caption="Search output">
			|<vbox>			
			|<webview id="{searchoutput$uniqueid}"/>
			|</vbox>
			|</tab>
			|<tab caption="Settings">
			|<vbox id="{enginesettingsvbox$uniqueid}">
			|<label id="{enginesettingsvboxlabel$uniqueid}" text="Load engine for settings."/>
			|</vbox>
			|</tab>
			|</tabpane>
			|</vbox>
		""".stripMargin

		def engine_handler(ev:MyEvent)
		{
			if(ev.kind=="checkbox changed")
			{
				val value=ev.value.toBoolean
				if(ev.Id==s"{autoload$uniqueid}")
				{					
					SmartSetAutoLoad(value)
				}				
			}

			if(ev.kind=="button pressed")
			{
				if(ev.Id==s"{load$uniqueid}")
				{
					Load
				}

				if(ev.Id==s"{unload$uniqueid}")
				{
					Unload
				}

				if(ev.Id==s"{issue$uniqueid}")
				{
					val command=Builder.GetMyText(s"{command$uniqueid}").GetText

					SmartIssueCommand(command)

					DeleteCommandTextfield
				}

				if(ev.Id==s"{start$uniqueid}")
				{
					SmartStart()
				}

				if(ev.Id==s"{stop$uniqueid}")
				{
					Stop
				}
			}

			if(ev.kind=="textfield entered")
			{
				if(ev.Id==s"{command$uniqueid}")
				{
					val command=ev.value

					SmartIssueCommand(command)

					DeleteCommandTextfield
				}
			}
		}

		val commandid=s"{command$uniqueid}"
		val consoletitle=getName+" console"
		val consoleid=EngineConsoleStageId

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.OpenConsole",code=new Runnable{def run{
			Builder.MyStage(id=consoleid,title=consoletitle,blob=blob,handler=engine_handler)
			val commandtext=Builder.GetMyText(commandid)
			if(commandtext!=null) commandtext.SetText("")			
		}})			
		
	}

	def CloseConsole
	{
		val stageid=EngineConsoleStageId

		if(!IsConsoleOpen) return

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.CloseConsole",code=new Runnable{def run{
			Builder.CloseStage(stageid)
		}})			
	}

	var engine_out_buff=scala.collection.mutable.ArrayBuffer[EngineLogItem]()

	val max_engine_out_buff_length=200

	def UpdateEngineOut
	{
		val content=ReportEngineOut()

		MyActor.queuedExecutor ! ExecutionItem(client="Engine.UpdateEngineOut",code=new Runnable{def run{
			Builder.LoadWebContent(s"{engineoutlog$uniqueid}",content)
		}})			
	}

	def AddToEngineOutBuff(eli:EngineLogItem)
	{
		if( engine_out_buff.length > max_engine_out_buff_length ) engine_out_buff.remove(0)
		engine_out_buff+=eli
	}

	var options:Options=null

	var features:XboardFeatures=null

	var startup:Boolean=false

	var optionsshown:Boolean=false

	def ProcessEngineOut(line:String)
	{
		// remove closing carriage return
		val effline=line.replaceAll("\\r$","")

		val tokenizer=Tokenizer(effline)

		val head=tokenizer.Poll

		if(head=="bestmove") bestmovereceived=true

		if(EngineManager.enginehtmlupdateallowed) logactor ! AddToEngineLogMsg(this,EngineLogItem(effline,ftime=MyActor.GetFormattedTime,out=true))

		if(running && (thinkingoutput != null))
		{
			thinkingoutput.ParseLine(effline)			
		
			if(EngineManager.enginehtmlupdateallowed) MyActor.queuedExecutor ! ExecutionItem(client="Engine.ProcessEngineOut.ThinkingOutput",code=new Runnable{def run{				
				Builder.LoadWebContent(s"{searchoutput$uniqueid}",thinkingoutput.ReportHTML)
			}})			
		}

		if(startup)
		{
			if(kind=="uci")
			{				
				if(head=="uciok")
				{
					startup=false
					ApplyAllOptions
				} else {
					val option=Option().ParseLine(effline)
					if(option!=null)
					{					
						options.Add(option)					
					}
				}
			}

			if(kind=="xboard")
			{
				features.ParseLine(line)
				if(features.done)
				{
					startup=false
					options=features.options
					ApplyAllOptions
				}
			}
		}
	}

	def ReportEngineOut(reverse:Boolean=true):String =
	{
		val last=(engine_out_buff.length-1)
		val rows=(for(i <- 0 to last) yield
		{
			var effi=if(reverse) last-i else i
			val linei=engine_out_buff(effi)
			val font=if(linei.out) """<font color="#00007f">""" else """<font color="#7f0000">"""
			s"""
				|<tr>
				|<td>
				|<font color="#007f00">
				|<small>${linei.ftime}</small>
				|</font>
				|</td>
				|<td>
				|$font
				|${linei.effline}
				|</font>
				|</td>
				|</tr>
			""".stripMargin
		}).mkString("\n")
		s"""
			|<table cellpadding="3">
			|$rows
			|</table>
		""".stripMargin
	}

	def CreateEngineReadThread:Thread =
	{
		val truethis=this
		new Thread(new Runnable{def run{
			var buffer=""
			while (!Thread.currentThread().isInterrupted()){
			{
				try
				{
					val chunkobj=enginein.read()
					try
					{ 
						val chunk=chunkobj.toChar
						if(chunk=='\n')
						{						
							ProcessEngineOut(buffer)
							buffer=""
						} else {
							buffer+=chunk
						}
					}
					catch
					{
						case e: Throwable => 
						{
							val emsg=s"engine read not a char exception, chunk: $chunkobj, id: $uniqueid, path: $path"
							println(emsg)
							logactor ! AddToEngineLogMsg(truethis,EngineLogItem(emsg,ftime=MyActor.GetFormattedTime,out=false))
						}
					}
				}
				catch
				{
					case e: Throwable =>
					{
						val emsg=s"engine read IO exception, id: $uniqueid, path: $path"
						println(emsg)
						logactor ! AddToEngineLogMsg(truethis,EngineLogItem(emsg,ftime=MyActor.GetFormattedTime,out=false))
					}
				}
			}
		}}})
	}

	def Load:Boolean =
	{
		if(engineprocess!=null)
		{
			MyActor.Log(s"load engine $path failed, already loaded")
			return false
		}

		val progandargs:List[String] = path+:commandline.split(" ").toList
		val processbuilder=new ProcessBuilder(progandargs.asJava)
		val epf=new java.io.File(path)

		MyActor.Log("starting engine process "+progandargs.mkString(" "))

		if(epf.exists())
		{
			processbuilder.directory(new java.io.File(epf.getParent()))
		}
		else
		{
			MyActor.Log(s"engine $path failed, directory does not exist")
			return false
		}
		try
		{
			engineprocess=processbuilder.start()
		}
		catch
		{
			case e: Throwable =>
			{
				MyActor.Log(s"engine $path failed, process could no be created")
				return false
			}
		}
		enginein=engineprocess.getInputStream()
        engineout=engineprocess.getOutputStream()
        enginereadthread=CreateEngineReadThread
        enginereadthread.start()

        MyActor.Log(s"success, $path started")        

        options=null

        features=null

        if(kind=="uci")
        {
        	options=Options(uniqueid=uniqueid)
        	optionsshown=false
        	startup=true
        	IssueCommand("uci")
        }

        if(kind=="xboard")
        {
        	features=XboardFeatures(uniqueid=uniqueid)
        	optionsshown=false
        	startup=true
        	IssueCommand("xboard")
			IssueCommand("protover 2")
        }

        true
	}

	def SendQuit
	{

	}

	def Unload
	{	

		SendQuit

		if(enginereadthread!=null)
		{
			enginereadthread.interrupt()
			enginereadthread=null
		}

		if(engineprocess!=null)
		{
			engineprocess.destroy()
			engineprocess=null
		}

		enginein=null
		engineout=null

		MyActor.Log(s"engine $path unloaded")

	}

	def ShutDown
	{
		Unload

		CloseConsole
	}

	def Terminate
	{
		ShutDown

		EngineManager.enginelistsystem.stop(logactor)
	}

	def Startup
	{
		if(autoload)
		{
			OpenConsole

			Load
		}
	}

	def getName:String=
	{
		if(path==null) return "?nopath?"

		val f=new java.io.File(path)

		if(f.exists)
		{
			return f.getName
		}

		"?invalidpath?"
	}

	def toXml=
	{
		<engine>
		<path>{path}</path>
		<kind>{kind}</kind>
		<uniqueid>{uniqueid}</uniqueid>
		<variant>{variant}</variant>
		<autoload>{autoload}</autoload>
		</engine>
	}

	def fromXml(engine:scala.xml.NodeSeq)
	{
		path=(engine \ "path").text
		kind=(engine \ "kind").text		
		variant=(engine \ "variant").text
		autoload=(engine \ "autoload").text.toBoolean

		SetUniqueId((engine \ "uniqueid").text.toInt)
	}	
}

