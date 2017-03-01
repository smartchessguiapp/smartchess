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

import collection.JavaConversions._

import java.awt.Toolkit

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

import java.io._
import scala.io._

////////////////////////////////////////////////////////////////////

// builder.scala is responsible for building up the application

////////////////////////////////////////////////////////////////////
// GuiClass
////////////////////////////////////////////////////////////////////

// GuiClass is the class passed by the main function for execution as a JavaFX application
// it is minimal, the actual implementation is contained in MyApp ( defined in myapp.scala )

class GuiClass extends Application
{
	override def start(primaryStage: Stage)
	{
		// Init sets up ModuleManager by adding modules necessary for the application
		MyApp.Init

		// starting modules
		ModuleManager.Startup

		// starting application with all modules already up and running
		MyApp.Start(primaryStage)
	}

	override def stop()
	{
		// stopping application
		MyApp.Stop

		// shutting down modules
		ModuleManager.Shutdown
	}
}

////////////////////////////////////////////////////////////////////
// Module
////////////////////////////////////////////////////////////////////

// Module defines the minimum functionality of a module
// namely it should have a Startup function, a Shutdown funtcion
// and should report its Name

trait Module
{
	def Startup:Unit // abstract
	def Shutdown:Unit // abstract
	def Name:String // abstract
}

////////////////////////////////////////////////////////////////////
// ModuleManager
////////////////////////////////////////////////////////////////////

// ModuleManager is responsible for startup and shutdown of modules
// it starts modules on startup
// and shuts them down on shutdown
object ModuleManager
{
	// items lists the modules, Builder is default module
	private var items=scala.collection.mutable.ArrayBuffer[Module](Builder)

	// add a module
	def Add(m:Module)
	{
		items+=m
	}

	// start the modules
	def Startup
	{
		for(item<-items)
		{
			print("starting "+item.Name+" ... ")
			item.Startup
			println("done")
		}
	}

	// shut down the modules
	def Shutdown
	{
		for(item<-items.reverse)
		{
			print("shutting down "+item.Name+" ... ")
			item.Shutdown
			println("done")
		}
	}

}

////////////////////////////////////////////////////////////////////
// MyEvent
////////////////////////////////////////////////////////////////////

// MyEvent is the event used for the application's event handling

case class MyEvent(
	// event kind
	var kind:String,
	// component generating the event
	var comp:MyComponent,
	// evant value
	var value:String,
	// index is used for indexed events of list like components
	var index:Int=0,
	// togglegroup is used by toggle group menu events
	var togglegroup:String=null
) extends PrintableInterface
{
	// determine parts of the generating component's id ( names/indices of path elements )
	// useful for passing parameters in the event id
	var parts=List[String]()

	if(comp!=null)
	{
		parts=comp.GetIdParts
	}

	// log the event ( except for "slider changed" which is handled in guisystem/actor.scala )
	if(kind!="slider changed")
	{
		MyActor.Log(ToPrintable)
	}

	// return event id as the id of the component
	def Id:String=
	{
		if(comp==null) return ""
		comp.GetId
	}

	// return event trunk id as the trunk id of the component
	def TrunkId:String=
	{
		if(comp==null) return ""
		comp.GetTrunkId
	}

	// report the event in printable form
	def ReportPrintable(level:Int=0,buff:String=""):String=
	{
		var reportcomp="null"
		if(comp!=null)
		{
			reportcomp=Id+" [ "+comp.kind+" ] "
		}
		kind+" = "+value+" @ "+reportcomp
	}
}

////////////////////////////////////////////////////////////////////
// Resource
////////////////////////////////////////////////////////////////////

// Resource provides access to Java resources

object Resource
{
	def asStream(path:String):InputStream=
	{
		getClass().getClassLoader().getResourceAsStream(path)
	}

	def asSource(path:String):Source=
	{
		Source.fromInputStream(asStream(path))
	}

	def asString(path:String):String=
	{
		asSource(path).mkString
	}
}

////////////////////////////////////////////////////////////////////
// Builder
////////////////////////////////////////////////////////////////////

// object Builder
// - keeps track of the application's state
//  by loading values on startup
//  and saving them on shutdown
// - provides functions for creating and manipulating stages
// - provides access to components, frequently used component methods and values

object Builder extends Module with GetSetTypedDataValueWithDefault
{	

	// value members

	// values is a data structure that holds the application's state
	private var values:Data=MapData()

	// components lists the components of the application by id
	private var components=Map[String,MyComponent]()

	// stages lists the stages opened by the application by id
	private var stages=Map[String,MyStage]()

	// GetStage reports the stages
	def GetStages = stages

	// SetComponent adds/sets a component by id
	def SetComponent(key:String,comp:MyComponent)
	{
		components+=(key->comp)
	}

	// method members

	////////////////////////////////////////////////////////////////////
	// Module interface	

	def Name:String="Builder"

	def Startup
	{
		DataUtils.mkdir("stuff")
		values=Data.FromXMLFile(valuespath)
	}

	def Shutdown
	{
		values.SaveToXMLFile(valuespath)
		values.SaveToXMLFilePretty(valuesprettypath)
	}

	////////////////////////////////////////////////////////////////////

	// pathes for storing values and their pretty print
	def valuespath="stuff"+File.separator+"values.xml"
	def valuesprettypath="stuff"+File.separator+"valuespretty.xml"

	////////////////////////////////////////////////////////////////////

	// prefix bindings calculate a prefix to the path of a component
	// this allows components to store their state at a path that is calculated
	// by adding a prefix to their id path ( useful for example to store component
	// states as per chess variant )
	// the default is to store and retrieve their state under the "{components}" prefix

	var PrefixBindingGet:(String)=>Path=DefaultPrefixBindingGet

	var PrefixBindingSet:(String)=>Path=DefaultPrefixBindingSet

	def SetPrefixBindingGet(prefixbinding:(String)=>Path)
	{
		PrefixBindingGet=prefixbinding
	}

	def SetPrefixBindingSet(prefixbinding:(String)=>Path)
	{
		PrefixBindingSet=prefixbinding
	}

	def DefaultPrefixBindingGet(prefixget:String="components"):Path=
	{
		if(prefixget=="settings") return Path.FromString("{settings}")
		if(prefixget=="variant") return Path.FromString(Cve(null))
		Path.FromString("{components}")
	}

	def DefaultPrefixBindingSet(prefixset:String="components"):Path=
	{
		if(prefixset=="variant") return Path.FromString(Cve(null))
		Path.FromString("{components}")
	}

	////////////////////////////////////////////////////////////////////

	// ClearValues clears all values

	def ClearValues
	{
		values = MapData()
	}

	// Set sets a value stored at path in values an returns values
	def Set(path:Path,value:Data):Data=
	{
		if(path==null) return values
		values=values.Set(path,value)
		values
	}

	// same as above but the path is given as a string
	def Set(pathstr:String,value:Data)
	{
		Set(Path.FromString(pathstr),value)
	}

	// same as above but the value is a string
	def Set(pathstr:String,value:String)
	{
		Set(Path.FromString(pathstr),StringData(value))
	}

	// Cve ( current variant entry ) is the prefix determined by the current variant
	def Cve(pathstr:String):String=
	{
		val pref="{variantentries}#{"+board.variant+"}"
		if(pathstr==null) return pref
		s"$pref#$pathstr"
	}

	// Get return the value at path, can be null
	def Get(path:Path):Data=values.Get(path)

	// the same as above but the path is given as a string
	def Get(pathstr:String):Data=values.Get(Path.FromString(pathstr))

	// the following functions return values stored at id of different types
	// with a default used in the case the id does not point to value

	def GetStringWithDefault(id:String,default:String)=values.GS(id,default)

	def GetIntWithDefault(id:String,default:Int)=values.GI(id,default)

	def GetDoubleWithDefault(id:String,default:Double)=values.GD(id,default)

	def GetBooleanWithDefault(id:String,default:Boolean)=values.GB(id,default)

	// RegisterComponent adds/sets a component by its id in components
	def RegisterComponent(id:String,comp:MyComponent)
	{
		if(id==null) return
		if(comp==null) return
		components+=(id->comp)
	}

	// GetComponent returns the component by id, can be null
	def GetComponent(id:String):MyComponent=
	{
		if(id==null) return null
		if(components.contains(id)) return components(id)
		null
	}

	// the following functions look up and return different types of MyComponents by id
	// avoids type casting when accessing those components

	def GetMyBox(id:String):MyBox=GetComponent(id).asInstanceOf[MyBox]
	def GetMyButton(id:String):MyButton=GetComponent(id).asInstanceOf[MyButton]
	def GetMyText(id:String):MyText=GetComponent(id).asInstanceOf[MyText]
	def GetMyWebView(id:String):MyWebView=GetComponent(id).asInstanceOf[MyWebView]
	def GetMyCheckBox(id:String):MyCheckBox=GetComponent(id).asInstanceOf[MyCheckBox]
	def GetMySlider(id:String):MySlider=GetComponent(id).asInstanceOf[MySlider]
	def GetMyColorPicker(id:String):MyColorPicker=GetComponent(id).asInstanceOf[MyColorPicker]
	def GetMyEditableListView(id:String):MyEditableListView=GetComponent(id).asInstanceOf[MyEditableListView]
	def GetMyListView(id:String):MyListView=GetComponent(id).asInstanceOf[MyListView]
	def GetMyComboBox(id:String):MyComboBox=GetComponent(id).asInstanceOf[MyComboBox]
	def GetMyTabPane(id:String):MyTabPane=GetComponent(id).asInstanceOf[MyTabPane]
	def GetMyScrollPane(id:String):MyScrollPane=GetComponent(id).asInstanceOf[MyScrollPane]
	def GetGuiBoard(id:String):GuiBoard=GetComponent(id).asInstanceOf[GuiBoard]
	def GetGameBrowser(id:String):GameBrowser=GetComponent(id).asInstanceOf[GameBrowser]

	// ExecuteScript executes a web script on the webview of id
	def ExecuteWebScript(id:String,script:String):String=
	{
		val w=GetMyWebView(id)
		if(w==null) return null
		w.ExecuteScript(script)
	}

	// GetWebEngine returns the web engine of the webview of id
	def GetWebEngine(id:String):WebEngine=
	{
		val w=GetMyWebView(id)
		if(w==null) return null
		w.GetEngine
	}

	// LoadContentAndScrollTo loads content to a webview of id and scrolls to a given y
	// asynchronous
	def LoadWebContentAndScrollTo(id:String,content:String,yscroll:Double)
	{
		val w=GetMyWebView(id)
		if(w!=null) w.LoadContentAndScrollTo(content,yscroll)
	}

	// LoadWebContent loads content to a webview of id
	// asynchronous
	def LoadWebContent(id:String,content:String)
	{
		val w=GetMyWebView(id)
		if(w!=null) w.LoadContent(content)
	}

	// WriteWebContent writes content to a webview of id
	// this is a synchronous call, should be avoided when possible
	def WriteWebContent(id:String,content:String)
	{
		val w=GetMyWebView(id)
		if(w!=null) w.WriteContent(content)
	}

	// MyStage defines and manipulates a stage ( operation system window )
	case class MyStage(
		// id
		id:String,
		// title
		title:String=null,
		// blob defines the sceengraph
		blob:String=null,
		// s is the JavaFX stage
		s:Stage=new Stage(),
		// handler is the event handler of the stage
		handler:(MyEvent)=>Unit=MyComponent.default_handler,
		// show specifies whether to show the stage
		show:Boolean=true,
		// andwait when set to true waits for the stage to close
		andwait:Boolean=false,
		// if usewidth is set the stage width is set to the stored value
		usewidth:Boolean=true,
		// if useheight is set the stage height is set to the stored value
		useheight:Boolean=true,
		// unclosable when set prevents the stage from closing by clicking on the 'x' in the top right corner
		// important for protecting modal stages of long operations from being closed by the user
		unclosable:Boolean=false,
		// modal is the modality of the stage
		modal:Boolean=false,
		// store specifies whether to store the stage's coordinates and size upon user dragging/resizing
		store:Boolean=true
	)
	{
		// the following functions set up the JavaFX stage and its handlers

		def SetTitle(title:String)
		{
			s.setTitle(title)
		}

		if(title!=null) SetTitle(title)

		s.setX(GD(s"{stages}#$id#{x}",10.0))
		s.setY(GD(s"{stages}#$id#{y}",10.0))
		
		if(usewidth)
		{
			s.setWidth(GD(s"{stages}#$id#{width}",600.0))
		}

		if(useheight)
		{
			s.setHeight(GD(s"{stages}#$id#{height}",400.0))
		}

		s.xProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					if(store) Set(s"{stages}#$id#{x}", new_val.toString())
				}
			}
		)

		s.yProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					if(store) Set(s"{stages}#$id#{y}", new_val.toString())
				}
			}
		)

		s.widthProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					val width=new_val.toString()
					if(store) Set(s"{stages}#$id#{width}",width)
					Fire(id,handler,"stage width resized",width)
				}
			}
		)

		s.heightProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					val height=new_val.toString()
					if(store) Set(s"{stages}#$id#{height}",height)
					Fire(id,handler,"stage height resized",height)
				}
			}
		)

		if(blob!=null)
		{
			val comp=MyComponent.FromBlob(blob,handler)

			comp.CreateNode

			s.setScene(new Scene(comp.GetParent))
		}

		if(modal)
		{
			s.initModality(Modality.APPLICATION_MODAL)
		}

		if(unclosable)
		{
			s.setOnCloseRequest(new EventHandler[WindowEvent]
			{
				def handle(ev:WindowEvent)
				{
					ev.consume()
				}
			})
		} else {
			s.setOnCloseRequest(new EventHandler[WindowEvent]
			{
				def handle(ev:WindowEvent)
				{
					RemoveStage(id)
					Fire(id,handler,"stage closed","")
				}
			})
		}

		// the stage is now set up

		// add to stages

		stages+=(id->this)

		// show the stage ( and wait ) if necessary

		if(show)
		{
			if(andwait)
			{
				s.showAndWait()
			}
			else
			{
				s.show()
			}
		}

		// the following functions are proxies to the corresponding JavaFX stage functions

		def SetScene(p:Parent){ s.setScene(new Scene(p)) }
		def Close{ s.close() }
		def Show{ s.show() }
		def SetX(w:Double) { s.setX(w) }
		def SetY(w:Double){ s.setY(w) }
		def GetX():Double=s.getX()
		def GetY():Double=s.getY()
		def SetWidth(w:Double){	s.setWidth(w) }
		def SetHeight(h:Double){ s.setHeight(h) }
		def ToFront{ s.toFront() }
	}

	// Fire fires an event
	def Fire(id:String,handler:(MyEvent)=>Unit,kind:String,value:String)
	{
		val comp=new MyDummy(id)
		MyActor.queuedExecutor ! ExecutionItem(client=s"Builder.MyStage.Fire",code=new Runnable{def run{
			handler(MyEvent(kind,comp,value))
		}})
	}

	// HasStage determines whether a stage of id exists
	def HasStage(id:String):Boolean=stages.contains(id)

	// GetStage returns the stage of id, can be null
	def GetStage(id:String):MyStage=
	{
		if(HasStage(id)) return stages(id)
		null
	}

	// RemoveStage removes stage of id from stages ( does not close it )
	def RemoveStage(id:String)
	{
		if(HasStage(id))
		{
			stages-=id
		}
	}

	// CloseStage removes and closes stage of id
	def CloseStage(id:String)
	{
		if(HasStage(id))
		{
			stages(id).Close
			stages-=id
		}
	}

	// SystemPopUp pops up a system message in a thread safe way
	def SystemPopUp(title:String,content:String,dur:Int=1500)
	{		
		MyActor.queuedExecutor ! ExecutionItem(client="Builder.SystemPopup",code=new Runnable{def run{
			Builder.DoSystemPopUp(title,content,dur)
		}})
	}

	// DoSystemPopUp is the implementation of popping up a system message
	var popupcnt=0
	def DoSystemPopUp(title:String,content:String,dur:Int=1500)
	{
		val id=s"{systempopup$popupcnt}"
		popupcnt+=1
		val wid=id+"#{web}"
		val blob=s"""
			|<vbox>
			|<webview id="$wid"/>
			|</vbox>
		""".stripMargin
		val s=MyStage(id,title,blob,modal=true,unclosable=false,store=false)
		s.SetWidth(300.0)
		s.SetHeight(200.0)
		val m=GetStage("{main}")
		if(m!=null)
		{
			val shift=(popupcnt%3)*25
			s.SetX(m.GetX+50.0+shift)
			s.SetY(m.GetY+50.0+shift)
		}
		WriteWebContent(wid,content)
		Future
		{
			Thread.sleep(dur)
			MyActor.queuedExecutor ! ExecutionItem(client="Builder.DoSystemPopUp",code=new Runnable{def run{
				CloseStage(id)
			}})
		}
	}

	// WriteDocumentScript writes a document script based on content
	def WriteDocumentScript(content:String):String=
	{
		def replq(content:String):String=(for(char<-content.replaceAll("\\s"," ").toList) yield
		{					
			char match {
				case '"' => """'"'"""
				case '\\' => """"\\""""
				case _ => s""""$char""""
			}
		}).mkString("+")
		"document.open();\n"+(for(line<-content.split("\n").map(replq))
			yield s"""document.write($line);""").mkString("\n")+
		"document.close();"
	}

	// InputTexts opens a dialog that inputs a list of texts
	def InputTexts(title:String=null,prompts:List[String]=null,
		applyname:String="Apply",candelete:Boolean=false,deletemsg:String="Delete this item"):InputTextResult=
	{
		var canceled=true
		var delete=false
		var results:Map[String,String]=null

		def GetResults
		{
			results=(for(prompt<-prompts) yield 
			(prompt->GetMyText(s"{textinputs}#{$prompt}").GetText)).toMap
		}

		def GetResultsAndCloseStage
		{
			GetResults
			CloseStage("{inputtextdialog}")
		}

		def input_text_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{textinputdelete}")
				{
					canceled=false
					delete=true
					GetResultsAndCloseStage
				}

				if(ev.Id=="{textinputcancel}")
				{
					canceled=true
					GetResultsAndCloseStage
				}

				if(ev.Id=="{textinputok}")
				{
					canceled=false
					delete=false
					GetResultsAndCloseStage
				}
			}

			if(ev.kind=="textfield entered")
			{
				canceled=false
				delete=false
				GetResultsAndCloseStage
			}
		}

		if((title==null)||(prompts==null)) return InputTextResult()

		var r=0
		val promptscontent=(for(prompt<-prompts) yield
		{
			r+=1
			s"""
				|<label text="$prompt" r="$r" c="1"/>
				|<textfield id="{textinputs}#{$prompt}" width="300.0" style="-fx-font-size: 24px;" r="$r" c="2"/>
			""".stripMargin
		}).mkString("\n")

		val deletecontent=if(!candelete) "" else s"""			
			|<button id="{textinputdelete}" style="-fx-background-color: #ffafaf;" text="$deletemsg"/>
		""".stripMargin

		r+=1
		val blob=s"""
			|<vbox>
			|<gridpane vgap="5" hgap="10">
			|$promptscontent
			|<button id="{textinputcancel}" style="-fx-font-size: 20px;" text="Cancel" r="$r" c="1"/>
			|<button id="{textinputok}" width="300.0" style="-fx-font-size: 24px;" r="$r" c="2" text="$applyname"/>
			|</gridpane>
			|$deletecontent
			|</vbox>
		""".stripMargin

		MyStage("{inputtextdialog}",title,blob,modal=true,unclosable=false,
			andwait=true,usewidth=false,useheight=false,handler=input_text_handler)

		InputTextResult(canceled,delete,results)
	}

	// Confirm pops up a confirm dialog and waits for user response, returns true if confirmed, false otherwise
	def Confirm(title:String=null):Boolean=
	{
		var canceled=true

		def confirm_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{confirmcancel}")
				{
					canceled=true
					CloseStage("{confirmdialog}")
				}

				if(ev.Id=="{confirmok}")
				{
					canceled=false
					CloseStage("{confirmdialog}")
				}
			}
		}

		if(title==null) return false

		val blob=s"""
			|<vbox padding="10">
			|<gridpane vgap="10" hgap="10">
			|<button id="{confirmok}" width="300.0" style="-fx-font-size: 24px;" text="Ok" r="1" c="2"/>
			|<button id="{confirmcancel}" style="-fx-font-size: 20px;" text="Cancel" r="2" c="1"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		MyStage("{confirmdialog}",title,blob,modal=true,unclosable=false,
			andwait=true,usewidth=false,useheight=false,handler=confirm_handler)

		!canceled
	}

	// RemoveSpecials removes special characters from a string
	// useful for generating a safely formatted name
	def RemoveSpecials(fromwhat:String):String=
	{
		fromwhat.replaceAll("[^a-zA-z0-9\\s_]","")
	}

	// GetTabList returns the tab names of MyTabPane of id and its JavaFX tabpane as a touple
	def GetTabList(id:String):Tuple2[List[String],TabPane]=
	{
		val mytabpane=GetMyTabPane(id)

		if(mytabpane==null) return null

		val tabspane=mytabpane.GetNode.asInstanceOf[TabPane]

		val tabs=tabspane.getTabs()

		Tuple2((for(i<-0 to tabs.size-1) yield tabs.get(i).getText()).toList,tabspane)
	}

	// SelectTab selects a tab in MyTabPane of id by name and returns the selected tab's index
	def SelectTab(id:String,tabname:String):Int =
	{
		val tablist=GetTabList(id)

		val i=tablist._1.indexOf(tabname)

		if(i< 0) return 0

		tablist._2.getSelectionModel().select(i)

		i
	}

	// the same as above just selects the tab by index rather than name
	def SelectTab(id:String,tabindex:Int):Int =
	{
		val tablist=GetTabList(id)
		
		tablist._2.getSelectionModel().select(tabindex)

		tabindex
	}

	// ChooseFile chooses a file and stores / retrieves parameters by id
	def ChooseFile(id:String,setdirafter:String=null):java.io.File=
	{
		val fc=new FileChooser()

		var initial_dir=GS(Cve(s"{filechooser}#{$id}#{dir}"),"")
		
		if(new File(initial_dir).exists())
		{
			fc.setInitialDirectory(new File(initial_dir))
		}

		val s=new Stage()

		s.initModality(Modality.APPLICATION_MODAL)

		val f=fc.showOpenDialog(s)

		if(f!=null)
		{
			val path=f.getAbsolutePath()

			val name=f.getName()

			val dir=f.getParent()

			Set(Cve(s"{filechooser}#{$id}#{path}"),path)

			Set(Cve(s"{filechooser}#{$id}#{name}"),name)

			Set(Cve(s"{filechooser}#{$id}#{dir}"),dir)

			if(setdirafter!=null)
			{
				Set(Cve(s"{savefileas}#{$setdirafter}#{path}"),path)

				Set(Cve(s"{savefileas}#{$setdirafter}#{name}"),name)

				Set(Cve(s"{savefileas}#{$setdirafter}#{dir}"),dir)
			}
		}

		f
	}


	// SaveFileAsDialog is a dialog that saves a file at a given name and stores / retrieves parameters by id
	def SaveFileAsDialog(
		title:String,
		id:String,
		content:String,
		ext:String="pgn",
		create:Boolean=false,
		successcallback:(String)=>Unit=null,
		setdirafter:String=null
	)
	{
		var saveasdirtext:MyText=null
		var saveasnametext:MyText=null

		def DoSave
		{
			var name=GetMyText(s"{savefileas}#{$id}#{name}").GetText

			if(name=="") name="default"

			val parts=name.split("\\.").toList

			if(parts(parts.length-1).toLowerCase!=ext)
			{
				name+="."+ext
			}

			val initial_dir=saveasdirtext.GetText
			
			var savepath=initial_dir+File.separator+name

			val initialdirexists=new File(initial_dir).exists()

			if(!initialdirexists) savepath=name

			Set(Cve(s"{savefileas}#{$id}#{path}"),savepath)

			Set(Cve(s"{savefileas}#{$id}#{dir}"),if(initialdirexists) initial_dir else "")

			Set(Cve(s"{savefileas}#{$id}#{name}"),name)

			var confirm=true

			if(new File(savepath).exists()) confirm=Confirm("File already exists, replace?")

			if(confirm)
			{
				DataUtils.WriteStringToFile(savepath,content)

				Fire("{savefileasdialog}",MyComponent.default_handler,"file saved as",savepath+" : "+content)

				CloseStage("{savefileasdialog}")

				if(setdirafter!=null)
				{
					Set(Cve(s"{filechooser}#{$setdirafter}#{path}"),savepath)

					Set(Cve(s"{filechooser}#{$setdirafter}#{name}"),name)

					Set(Cve(s"{filechooser}#{$setdirafter}#{dir}"),initial_dir)
				}

				if(successcallback!=null) successcallback(savepath)
			}
		}

		def saveas_handler(ev:MyEvent)
		{
			if(ev.kind=="textfield entered")
			{
				if(ev.Id==s"{savefileas}#{$id}#{name}")
				{
					DoSave
				}
			}

			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{choosefile}")
				{
					val f=ChooseFile(if(setdirafter!=null) setdirafter else "dummy")

					if(f!=null)
					{
						val name=f.getName()

						val dir=f.getParent()

						saveasnametext.SetText(name)

						saveasdirtext.SetText(dir)
					}
				}

				if(ev.Id=="{cancel}")
				{
					CloseStage("{savefileasdialog}")
				}

				if(ev.Id=="{save}")
				{
					DoSave
				}

				if(ev.Id=="{choosesaveasdir}")
				{
					val dc=new DirectoryChooser()

					val initial_dir=saveasdirtext.GetText

					if(new File(initial_dir).exists)
					{
						dc.setInitialDirectory(new File(initial_dir))
					}

					val f=dc.showDialog(new Stage())

					if(f!=null)
					{
						val dir=f.getAbsolutePath()

						println("dir "+dir)

						saveasdirtext.SetText(dir)
					}
				}
			}
		}

		val save=if(create) "Create" else "Save"

		val blob=s"""
			|<vbox>
			|<gridpane hgap="10" vgap="10">
			|<label text="File name" r="1" c="1"/>
			|<textfield prefixget="variant" prefixset="variant" id="{savefileas}#{$id}#{name}" style="-fx-font-size: 24px;" r="1" c="2"/>
			|<button id="{choosefile}" text="Choose" r="1" c="3"/>
			|<label text="Directory" r="2" c="1"/>
			|<label prefixget="variant" prefixset="variant" id="{savefileas}#{$id}#{dir}" width="400.0" style="-fx-font-size: 10px;" r="2" c="2"/>
			|<button id="{choosesaveasdir}" text="Choose directory" r="3" c="2"/>
			|<button id="{cancel}" style="-fx-font-size: 20px;" text="Cancel" r="4" c="1"/>
			|<button id="{save}" width="400.0" style="-fx-font-size: 24px;" text="$save" r="4" c="2" cs="2"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		val s=MyStage("{savefileasdialog}",title,blob,modal=true,
			handler=saveas_handler,usewidth=false,useheight=false)

		saveasnametext=GetMyText(s"{savefileas}#{$id}#{name}")
		if(saveasnametext.GetText=="") saveasnametext.SetText("default")
		saveasdirtext=GetMyText(s"{savefileas}#{$id}#{dir}")
	}

	// AbortDialog is a dialog that protects a long operation and allows the user to abort it
	def AbortDialog(title:String="Abort operation",callback:()=>Unit=null)
	{
		val blob=s"""
			|<vbox>
			|<button id="{abortoperation}" width="300.0" style="-fx-font-size: 24px;" text="Abort"/>
			|</vbox>
		""".stripMargin

		def abort_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{abortoperation}")
				{
					CloseAbortDialog
					callback()
				}
			}
		}

		val s=MyStage("{abortoperationdialog}",title,blob,modal=true,unclosable=true,
			handler=abort_handler,usewidth=false,useheight=false)

		val m=GetStage("{main}")
		if(m!=null)
		{			
			s.SetX(m.GetX+50.0)
			s.SetY(m.GetY+50.0)
		}
	}

	// closes AbortDialog in a thread safe way
	def CloseAbortDialog
	{
		MyActor.queuedExecutor ! ExecutionItem(client="Builder.CloseAbortDialog",code=new Runnable{def run{
			CloseStage("{abortoperationdialog}")
		}})
	}

	// CloseAllStages closes all stages
	def CloseAllStages
	{
		for((id,stage)<-stages) CloseStage(id)
	}

	// SetVboxSceneGraph sets the scene graph of a VBox
	def SetVboxSceneGraph(
		id:String,
		blob:String,
		handler:(MyEvent)=>Unit
	)
	{		
		val comp=MyComponent.FromBlob(blob,handler)

		comp.CreateNode

		val box=GetMyBox(id)

		val vbox=box.GetNode.asInstanceOf[VBox]
	
		vbox.getChildren().clear()

		vbox.getChildren().add(comp.GetParent)
	}

}

// InputTextResult holds the result of an input text dialog

case class InputTextResult(
	canceled:Boolean=true,
	deleteitem:Boolean=false,
	texts:Map[String,String]=null
)
{	
}

// Timer is a system timer

class Timer
{
	var t0=System.nanoTime()
	def elapsednano:Double = (System.nanoTime() - t0)
	def elapsedmicro:Double = (System.nanoTime() - t0)/1.0e3
	def elapsedmilli:Double = (System.nanoTime() - t0)/1.0e6
	def elapsed:Double = (System.nanoTime() - t0)/1.0e9	

	def formatted(format:String) = org.apache.commons.lang.time.DurationFormatUtils.formatDuration(elapsedmilli.toLong,format)
}

// HeapSize reports the system heap size in Mbytes

object HeapSize
{
	def heapsize = Runtime.getRuntime().totalMemory()/1000000
	def heapsize_B = Runtime.getRuntime().totalMemory()
}

// ClipboardSimple provides access to the system clipboard

object ClipboardSimple extends java.awt.datatransfer.ClipboardOwner
{

	override def lostOwnership(aClipboard:java.awt.datatransfer.Clipboard,aContents:java.awt.datatransfer.Transferable)
	{
		//do nothing
	}

	def clipget:String=getClipboardContents

	def getClipboardContents:String=
	{
		var result=""

		val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()

		val contents = clipboard.getContents(null)

		val hasTransferableText =
			(contents != null) &&
			contents.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.stringFlavor)

		if(hasTransferableText)
		{
			try
			{
				result = contents.getTransferData(java.awt.datatransfer.DataFlavor.stringFlavor).toString
			}
			catch
			{
				case _ : Throwable => result=""
			}
		}

		result
	}

	def clipset(content:String)
	{
		setClipboardContents(content)
	}

	def setClipboardContents(content:String)
	{
		val stringSelection = new java.awt.datatransfer.StringSelection(content)

		val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()

		clipboard.setContents(stringSelection, this)
	}

}

