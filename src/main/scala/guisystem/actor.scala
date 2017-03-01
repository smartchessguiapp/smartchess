package smartchess

////////////////////////////////////////////////////////////////////

import javafx.application._

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

import HeapSize._

////////////////////////////////////////////////////////////////////

// actor implements the akka actor framework

case class CustomException(
	customcause:String
) extends Exception
{
}

//usage: throw CustomException("custom error")

object ExecutionItem
{
	var id=0
}

case class ExecutionItem(
	client:String="unknown",
	dolog:Boolean=true,
	code:Runnable
)
{
	ExecutionItem.id+=1
	val id=ExecutionItem.id
}

case object ExecutionOk
case object ExecutionFailed

class QueuedExecutor extends Actor
{
	var items=List[ExecutionItem]()

	var execution_counter=0

	var total_execution_time=0

	var max_queue_length=0

	var clients=Map[String,ClientItem]()

	private var scheduler:Cancellable=null

	case object UpdateMsg

	case class ClientItem(
		name:String,
		var execution_counter:Int=0
	)
	{		
	}

	override def preStart():Unit=
	{
		scheduler = context.system.scheduler.schedule(
			initialDelay = 1 seconds,
			interval = 1 seconds,
			receiver = self,
			message = UpdateMsg
		)
	}

	def ExecNext
	{
		val queue_length=items.length
		if(queue_length>0)
		{
			if(queue_length>max_queue_length) max_queue_length=queue_length
			execution_counter+=1
			val ei=items.head
			items=items.tail
			if(!clients.contains(ei.client)) clients+=(ei.client->ClientItem(ei.client))
			clients(ei.client).execution_counter+=1

			if((ei.client != "MyLogActor")&&(ei.dolog))
			{
				val stime=MyActor.GetFormattedTime
				val logline=s"""
					|<tr>
					|<td><b><font color="brown">${ei.id}</font></b></td>
					|<td><b><font color="red">$stime</font></b></td>
					|<td><b><font color="green">${ei.client}</font></b></td>
					|</tr>
				""".stripMargin
				MyActor.logSupervisor ! AddItem("{execqueuelog}",MyLogItem(logline,html=true))
			}

			Platform.runLater(new Runnable{def run{
				try
				{					
					val t=new Timer()
					ei.code.run
					if((ei.client != "MyLogActor")&&(ei.dolog))
					{						
						val etime=t.elapsedmicro.toInt
						total_execution_time+=etime
						val logline=s"""
							|<tr>
							|<td><b><font color="brown">${ei.id}</font></b></td>
							|<td><b><font color="green">${ei.client}</font></b></td>
							|<td><b><font color="blue">$etime</font></b></td>
							|</tr>
						""".stripMargin
						MyActor.logSupervisor ! AddItem("{execqueuelog}",MyLogItem(logline,html=true))
					}															
				}
				catch
				{
					case e:Throwable =>
					{
						if(e.isInstanceOf[CustomException])						
						{
							//println("custom exception "+e.asInstanceOf[CustomException].customcause)
						}
						e.printStackTrace()
						self ! ExecutionFailed
						return
					}
				}
				self ! ExecutionOk
			}})
		}
	}

	def ReportHTML:String=
	{
		val clientscontent=(for((k,client)<-clients) yield {		
		s"""
			|<tr>
			|<td><b><font color="green">$k</font></b></td>
			|<td><b><font color="blue">${client.execution_counter}</font></b></td>
			|</tr>
		""".stripMargin}).mkString("\n")
		s"""
			|<table border="1">
			|<tr>
			|<td colspan="2">
			|<i>Execution counter</i> <b><font color="blue">$execution_counter</font></b> |
			|<i>Total execution time</i> <b><font color="blue">$total_execution_time</font></b> |
			|<i>Max queue length</i> <b><font color="blue">$max_queue_length</font></b>
			|</td>
			|</tr>
			|<tr>
			|<td><i>Client</i></td>
			|<td><i>Invocation</i></td>
			|</tr>
			|$clientscontent
			|</table>
		""".stripMargin
	}

	def Update
	{
		Platform.runLater(new Runnable{def run{
			Builder.LoadWebContent("{execqueue}",ReportHTML)
		}})
	}

	def receive=
	{
		case ei:ExecutionItem =>
		{
			items=items:+ei
			ExecNext
		}
		case ExecutionOk =>
		{
			ExecNext
		}
		case ExecutionFailed =>
		{
			println("execution failed")
			ExecNext
		}
		case UpdateMsg =>
		{
			Update
		}
		case _ => println("that was unexpected")
	}
}

// SystemTicker defines an actor that schedules repeat timer

case class SubmitEventRequest(handler:(MyEvent)=>Unit,ev:MyEvent,delay:Int=3){}

class SystemTicker extends Actor {

	private val Tick="Tick"

	private var scheduler: Cancellable = _

	override def preStart():Unit=
	{
		val sync=Builder.GD("{components}#{timingssynchronizer}",100.0).toInt
		scheduler = context.system.scheduler.schedule(
			initialDelay = 2 seconds,
			interval = sync milliseconds,
			receiver = self,
			message = Tick
		)
	}

	override def postStop():Unit=
	{
		scheduler.cancel()
	}

	private var timers=Map[String,Int]()
	private var requests=Map[String,SubmitEventRequest]()

	var tickcnt=0

	def receive=
	{
		case ser:SubmitEventRequest =>
		{
			val id=ser.ev.Id
			requests+=(id->ser)
			timers+=(id->ser.delay)
		}
		case Tick =>
		try
		{
			tickcnt+=1
			for((k,v)<-timers)
			{
				if(v==0)
				{
					val r=requests(k)        		
					timers-=k
					requests-=k
					MyActor.Log("delayed "+r.ev.ToPrintable)
					MyActor.queuedExecutor ! ExecutionItem(client="System Ticker",code=new Runnable{def run{						
						if((r.ev.kind=="slider changed")&&(r.ev.Id!=null))
						{
							val sc=Builder.GetMySlider(r.ev.Id)
							if(sc!=null)
							{
								sc.SetValue(r.ev.value.toDouble,dotrigger=false)
							}
						}
						r.handler(r.ev)
					}})
				}
				else
				{
					timers+=(k->(v-1))
				}
			}
		}
		catch
		{
			case t: Throwable =>
			// report errors
		}
	} 
}
 
// MyActor module start the actor system on startup and shuts it down on shutdown

object MyActor extends Module
{

	var system:ActorSystem=null
	var repeatActor:ActorRef=null
	var logSupervisor:ActorRef=null
	var queuedExecutor:ActorRef=null
	implicit val timeout = Timeout(5 seconds)

	def GetFormattedTime = org.apache.commons.lang.time.DateFormatUtils.format(new java.util.Date(),"HH:mm:ss.SSS")

	def Log(what:String)
	{
		MyActor.queuedExecutor ! ExecutionItem(client="System Log",code=new Runnable{def run{
			val stime=GetFormattedTime
			val logline=s"""
				|<tr>
				|<td><b><font color="red">$stime</font></b></td>
				|<td><font color="green">$what</font></td>
				|</tr>
			""".stripMargin
			MyActor.logSupervisor ! AddItem("{systemlog}",MyLogItem(logline,html=true))
		}})
	}
	
	def Startup
	{
		// create the system and actor
		system=ActorSystem("MyActorSystem")		
		repeatActor=system.actorOf(Props(new SystemTicker), name = "RepeatActor")
		logSupervisor=system.actorOf(Props[MyLogSupervisor], name = "LogSupervisor")
		queuedExecutor=system.actorOf(Props[QueuedExecutor], name = "QueuedExecutor")

		logSupervisor ! MyLog("{execqueuelog}",border=1)
		logSupervisor ! MyLog("{systemlog}",border=1)
	}

	def Shutdown
	{
		// shut down actor system
		system.terminate()
	}

	def Name="MyActor"
	
}

case class MyLogItem(
	text:String="",html:Boolean=false
)
{	
	def ReportHTMLTableRow:String=
	{
		if(html) text else
		s"""
			|<tr>
			|<td>$text</td>
			|</tr>
		""".stripMargin
	}
}

case class MyLog(
	wid:String=null,
	buffersize:Int=200,
	border:Int=0,
	var items:scala.collection.mutable.ArrayBuffer[MyLogItem]=scala.collection.mutable.ArrayBuffer[MyLogItem]()
)
{
	def Clear
	{
		items=scala.collection.mutable.ArrayBuffer[MyLogItem]()
	}

	def Add(item:MyLogItem)
	{
		items+=item
		while(items.length>buffersize)
		{
			items=items.tail
		}
	}

	def ReportHTML:String=
	{
		val itemscontent=(for(item<-items.reverse) yield item.ReportHTMLTableRow).mkString("\n")
		s"""
			|<table border="$border" cellpadding="2">
			|$itemscontent
			|</table>
		""".stripMargin
	}

	def Update
	{
		Builder.LoadWebContent(wid,ReportHTML)
	}
}

case class AddItem(
	wid:String,
	item:MyLogItem
)
{	
}

class MyLogSupervisor extends Actor
{
	var logs=Map[String,ActorRef]()

	def receive=
	{
		case l:MyLog =>
		{
			if(l.wid!=null)
			{
				if(!logs.contains(l.wid))
				{
					logs+=(l.wid->context.actorOf(Props[MyLogActor], name = Builder.RemoveSpecials(l.wid)))
				}
				logs(l.wid) ! l
			}
		}

		case ai:AddItem =>
		{
			if(logs.contains(ai.wid))
			{
				logs(ai.wid) ! ai.item
			}
		}

		case _ => println("that was unexpected")
	}
}

class MyLogActor extends Actor
{
	var log:MyLog=null

	def receive=
	{
		case l:MyLog => log=l

		case li:MyLogItem =>
		{
			if(log!=null)
			{
				log.Add(li)
				MyActor.queuedExecutor ! ExecutionItem(client="MyLogActor",code=new Runnable{def run{
					log.Update
				}})
			}
		}

		case _ => println("that was unexpected")
	}
}
