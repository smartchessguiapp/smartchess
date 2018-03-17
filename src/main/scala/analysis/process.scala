package smartchess

////////////////////////////////////////////////////////////////////

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import collection.JavaConverters._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

////////////////////////////////////////////////////////////////////

case class Proc(    
    dir:String="",
    progandargs:List[String]=List[String](),
    ProcessOut:(String)=>Unit=null
){  
    val path:String=progandargs(0)

    var process:Process=null
	var processin:java.io.InputStream=null
	var processout:java.io.OutputStream=null
	var processreadthread:Thread=null

    def CreateProcessReadThread:Thread =
	{
		val truethis=this
		new Thread(new Runnable{def run{
			var buffer=""
			while (!Thread.currentThread().isInterrupted()){
			{
				try
				{
					val chunkobj=processin.read()
					try
					{ 
						val chunk=chunkobj.toChar
						if(chunk=='\n')
						{					
                            if(ProcessOut!=null) ProcessOut(buffer)
							buffer=""
						} else {
							buffer+=chunk
						}
					}
					catch
					{
						case e: Throwable => 
						{
							val emsg=s"process read not a char exception, chunk: $chunkobj"
							println(emsg)
						}
					}
				}
				catch
				{
					case e: Throwable =>
					{
						val emsg=s"process read IO exception"
						println(emsg)
					}
				}
			}
		}}})
	}

    def Start:Boolean =
	{
		if(process!=null)
		{
			MyActor.Log(s"load process $path failed, already loaded")
			return false
		}

		val processbuilder=new ProcessBuilder(progandargs.asJava)
		val epf=new java.io.File(dir)

		MyActor.Log("starting process "+progandargs.mkString(" "))

		if(epf.exists())
		{
			processbuilder.directory(epf)
		}
		else
		{
			MyActor.Log(s"process $path failed, directory does not exist")
			return false
		}
		try
		{
			process=processbuilder.start()
		}
		catch
		{
			case e: Throwable =>
			{
				MyActor.Log(s"process $path failed, process could no be created")
				return false
			}
		}

		processin=process.getInputStream()
        processout=process.getOutputStream()
        processreadthread=CreateProcessReadThread
        processreadthread.start()

        MyActor.Log(s"success, $path started")        

        true
	}

    def Destroy
	{	

		if(processreadthread!=null)
		{
			processreadthread.interrupt()
			processreadthread=null
		}

		if(process!=null)
		{
			process.destroy()
			process=null
		}

		processin=null
		processout=null

		MyActor.Log(s"process $path unloaded")

	}
}