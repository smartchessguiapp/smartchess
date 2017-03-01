package smartchess

////////////////////////////////////////////////////////////////////

import collection.JavaConverters._

////////////////////////////////////////////////////////////////////

object ProcessManager extends Module
{
	var processes = Map[String,MyProcess]()

	def Startup
	{
		
	}

	def Shutdown
	{
		for((k,v) <- processes)
		{
			v.Shutdown
		}
	}

	def Name = "ProcessManager"

	def Create(id:String,progandargs:List[String])
	{
		if(id==null) return

		if(processes.contains(id))
		{
			processes(id).Terminate
		}

		val p=MyProcess(id)

		processes+=(id -> p)

		p.Create(progandargs)
	}

}

case class MyProcess(
	id:String
)
{
	var progandargs=List[String]()

	var process:Process=null
	var in:java.io.InputStream=null
	var out:java.io.OutputStream=null
	var error:java.io.InputStream=null
	var readthread:Thread=null
	var errorreadthread:Thread=null

	case class LogItem(line:String,out:Boolean=true,error:Boolean=false)
	{
		def ReportHTMLTableRow:String =
		{			
			s"""
				|<tr>
				|<td>
				|<font color="${if(error) "red" else if(out) "blue" else "green"}">
				|<pre>$line</pre>
				|</fonr>
				|</td>
				|</tr>
			""".stripMargin
		}
	}

	var logbuff = scala.collection.mutable.ArrayBuffer[LogItem]() 

	def Log(li:LogItem)
	{
		this.synchronized
		{
			logbuff+=li

			val content=s"""
				|<table>
				|${(for(li <- logbuff.reverse) yield li.ReportHTMLTableRow).mkString("\n")}
				|</table>
			""".stripMargin

			MyActor.queuedExecutor ! ExecutionItem(client=s"Process $id Log",code=new Runnable{def run{
				Builder.LoadWebContent(s"{process $id webview}",content)
			}})		
		}
	}

	var outbuff = scala.collection.mutable.ArrayBuffer[String]()
	var errorbuff = scala.collection.mutable.ArrayBuffer[String]()

	def ProcessOut(line:String)
	{
		val effline=line.replaceAll("\\r$","")

		outbuff+=effline

		Log(LogItem(effline))
	}

	def ProcessError(line:String)
	{
		val effline=line.replaceAll("\\r$","")

		errorbuff+=effline

		Log(LogItem(effline,error=true))
	}

	def CreateReadThread:Thread =
	{
		new Thread(new Runnable{def run{
			var buffer=""
			while (!Thread.currentThread().isInterrupted()){
			{
				try
				{
					val chunkobj=in.read()
					try
					{ 
						val chunk=chunkobj.toChar
						if(chunk=='\n')
						{						
							ProcessOut(buffer)
							buffer=""
						} else {
							buffer+=chunk
						}
					}
					catch
					{
						case e: Throwable => 
						{
							val emsg=s"$id read not a char exception, chunk: $chunkobj"
							println(emsg)
							MyActor.Log(emsg)
						}
					}
				}
				catch
				{
					case e: Throwable =>
					{
						val emsg=s"$id read IO exception"
						println(emsg)
						MyActor.Log(emsg)
					}
				}
			}
		}}})
	}

	def CreateErrorReadThread:Thread =
	{
		new Thread(new Runnable{def run{
			var buffer=""
			while (!Thread.currentThread().isInterrupted()){
			{
				try
				{
					val chunkobj=error.read()
					try
					{ 
						val chunk=chunkobj.toChar
						if(chunk=='\n')
						{						
							ProcessError(buffer)
							buffer=""
						} else {
							buffer+=chunk
						}
					}
					catch
					{
						case e: Throwable => 
						{
							val emsg=s"$id error not a char exception, chunk: $chunkobj"
							println(emsg)
							MyActor.Log(emsg)
						}
					}
				}
				catch
				{
					case e: Throwable =>
					{
						val emsg=s"$id error IO exception"
						println(emsg)
						MyActor.Log(emsg)
					}
				}
			}
		}}})
	}

	var watchthread:Thread = null

	def CreateWatchThread:Thread =
	{
		def Update(content:String)
		{
			if(IsConsoleOpen)
			{
				MyActor.queuedExecutor ! ExecutionItem(client=s"Process $id Watch",code=new Runnable{def run{					
					Builder.GetMyText(s"{elapsed $id}").SetText(content)
				}})		
			}
		}

		new Thread(new Runnable{def run{
			var wasrunning=false			
			while (IsAlive)
			{
				wasrunning=true
				val tf=timer.formatted("HH:mm:ss")
				Update(s"Running for $tf")

				try{Thread.sleep(1000)}catch{case e:Throwable=>}
			}

			val tf=timer.formatted("HH:mm:ss")

			var timeout=if(wasrunning) 1 else 10
			while((!IsConsoleOpen)&&(timeout>0)) try{timeout-=1;println(s"wait console $id");Thread.sleep(1000)}catch{case e:Throwable=>}

			Update(s"Terminated after $tf")
		}})
	}

	var timer:Timer = null

	def Create(setprograndargs:List[String]=null):Boolean =
	{
		progandargs=setprograndargs

		if(progandargs==null) return false

		val processbuilder=new ProcessBuilder(progandargs.asJava)

		MyActor.Log(s"starting process $id")

		try
		{
			process=processbuilder.start()
		}
		catch
		{
			case e: Throwable =>
			{
				MyActor.Log(s"starting process $id failed")
				return false
			}
		}

		in=process.getInputStream()
        out=process.getOutputStream()
        error=process.getErrorStream()
        readthread=CreateReadThread
        readthread.start()

        timer = new Timer()

        OpenConsole

        watchthread=CreateWatchThread
        watchthread.start()
        errorreadthread=CreateErrorReadThread
        errorreadthread.start()

        MyActor.Log(s"success, $id started")

        true
	}

	def IssueCommand(command:String)
	{
		if(command==null) return		

		val fullcommand=command+"\n"

		try
		{
			out.write(fullcommand.getBytes())
			out.flush()

			Log(LogItem(command,out=false))
		}
		catch
		{
			case e: Throwable =>
			{
				val emsg=s"$id write IO exception, command: $command"
				println(emsg)
				MyActor.Log(emsg)
				//e.printStackTrace
			}
		}
	}

	def IsAlive:Boolean = 
	{
		if(process==null) return false

		process.isAlive()
	}

	def Terminate
	{
		if(readthread!=null)
		{
			readthread.interrupt()
			readthread=null
		}

		if(errorreadthread!=null)
		{
			errorreadthread.interrupt()
			errorreadthread=null
		}

		if(process!=null)
		{
			process.destroy()
			process=null
		}

		in=null
		out=null

		MyActor.Log(s"$id terminated")
	}

	def Shutdown
	{
		Terminate

		CloseConsole
	}

	def DeleteCommandTextfield
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Process.DeleteCommandTextfield",code=new Runnable{def run{
			Builder.GetMyText(s"{command $id}").SetText("")
		}})		
	}

	def IsConsoleOpen:Boolean = Builder.HasStage(s"{process stage $id}")

	def OpenConsole
	{
		if(IsConsoleOpen) return

		val blob=s"""
			|<vbox>
			|<hbox>
			|<textfield id="{command $id}"/>
			|<button id="{issue $id}" text="Issue command"/>			
			|<label id="{elapsed $id}"/>
			|</hbox>
			|<webview id="{process $id webview}"/>
			|</vbox>
		""".stripMargin

		def handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id==s"{issue $id}")
				{
					val command=Builder.GetMyText(s"{command $id}").GetText

					IssueCommand(command)

					DeleteCommandTextfield
				}
			}

			if(ev.kind=="textfield entered")
			{
				if(ev.Id==s"{command $id}")
				{
					val command=ev.value

					IssueCommand(command)

					DeleteCommandTextfield
				}
			}
		}

		MyActor.queuedExecutor ! ExecutionItem(client="Process.OpenConsole",code=new Runnable{def run{
			Builder.MyStage(id=s"{process stage $id}",title=s"$id console",blob=blob,handler=handler)
			val commandtext=Builder.GetMyText(s"{command $id}")
			if(commandtext!=null) commandtext.SetText("")			
		}})			
		
	}

	def CloseConsole
	{
		if(!IsConsoleOpen) return

		MyActor.queuedExecutor ! ExecutionItem(client="Process.CloseConsole",code=new Runnable{def run{
			Builder.CloseStage(s"{process stage $id}")
		}})			
	}

}