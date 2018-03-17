package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppCore
{
	import MyApp._

	def Init	
	{
		// add modules
		ModuleManager.Add(MyActor)
		ModuleManager.Add(EngineManager)
		ModuleManager.Add(Eval)
	}

	val stockfishvariants=Map[String,String](
		"Standard" -> "chess",
		"Atomic" -> "atomic",
		"King of the Hill" -> "kingofthehill",
		"Chess960" -> "chess",
		"Antichess" -> "giveaway",		
		"Horde" -> "horde",				
		"From Position" -> "chess",
		"Three-check" -> "threecheck",
		"Racing Kings" -> "racingkings"
	)

	def personal_xml_path = DataUtils.PathFromList(List("..","personal.xml"))

	def set_defaults
	{
		val epath=DataUtils.PathFromList(List("Multivariant","stockfish.exe"))
		for(v <- board.SUPPORTED_VARIANTS)
		{
			def set_file_chooser(dir:String,openid:String,saveasid:String)
			{
				DataUtils.mkdirs(List("stuff",v,dir))
				Set(s"{variantentries}#{$v}#{filechooser}#{$openid}#{dir}",
					DataUtils.PathFromList(List("stuff",v,dir)))
				Set(s"{variantentries}#{$v}#{savefileas}#{$saveasid}#{dir}",
					DataUtils.PathFromList(List("stuff",v,dir)))
			}

			DataUtils.mkdirs(List("stuff",v))

			set_file_chooser("pgns","openpgn","savepgnas")
			set_file_chooser("multipgns","openmultipgn","savemultipgnas")
			set_file_chooser("pgnlists","openpgnlist","savepgnlistas")

			val xmlstr=s"""
				|<enginelist>				
				|<engine>
				|<path>$epath</path>
				|<kind>uci</kind>
				|<uniqueid>1</uniqueid>
				|<variant>$v</variant>
				|<autoload>false</autoload>
				|</engine>
				|</enginelist>
			""".stripMargin
			val xmlpath=DataUtils.PathFromList(List("stuff",v,"engines.xml"))
			DataUtils.WriteStringToFile(xmlpath,xmlstr)
			val svar=stockfishvariants(v)			
			val vpath=s"{variantentries}#{$v}#{engineoptions}#{1}#{UCI_Variant}#{selected}"
			Set(vpath,svar)
			println("Creating "+xmlpath)
			println("Setting "+vpath+" to "+svar)
		}
		Set("{components}#{colorbyscore}","true")
		Set("{components}#{autoselectpgntab}","true")
		Set("{components}#{autoselectpgntab}","true")
		Set("{components}#{checkreplica}","true")
		Set("{components}#{disableboardclick}","true")
		Set("{components}#{antichessdir}","AntichessSolutionLong")		

		if((new java.io.File(personal_xml_path)).exists)
		{
			val xml=scala.xml.XML.loadFile(personal_xml_path)

			val myhandle=(xml \ "myhandle").text
			val autoselectpgntab=DataUtils.ParseBoolean((xml \ "autoselectpgntab").text,true)						

			Set("{settings}#{myhandle}",myhandle)
			Set("{components}#{autoselectpgntab}",""+autoselectpgntab)

			val variant=(xml \ "variant").text

			if(variant != "") Set("{settings}#{variant}",variant)
		}
	}

	def reset_all_settings
	{
		val vf = new java.io.File(DataUtils.PathFromList(List("stuff","values.xml")))
		if(vf.exists) vf.delete

		ClearValues

		CloseStage("{mainstage}")

		Stop

		Start(ps)
	}

	def handler(ev:MyEvent)
	{
		def unhandled
		{
			val emsg="unhandled : "+ev.ReportPrintable()
			println(emsg)
		}

		ev.kind match
		{
			case "checkbox changed" =>
			{
				ev.Id match {
					case "{trainingmode}" => Update
					case "{buildtrainingmode}" => Update
					case "{hideresultstats}" => Update
					case "{showmetbymove}" => Update
					case _ => unhandled
				}
			}
			case "stage closed" =>
			{
				ev.Id match
				{
					case "{mainstage}" => CloseAllStages
					case _ =>
					{
						for(b <- GetBooks)
						{
							if(ev.Id==s"{auxbook $b stage}")
							{
								Set(Cve(s"{auxbook $b}"),"false")
							}
						}
					}
				}				
			}
			case "listview selected" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! OpenEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview open" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! OpenEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview delete" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! DeleteEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview top" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! TopEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview up" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! UpEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview down" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! DownEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listview bottom" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! BottomEngineMsg(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "tab selected" =>
			{
				ev.Id match
				{
					case "{maintabpane}" => SetSelectedMainTab(ev.value.toInt)
					case _ => unhandled
				}
			}
			case "listviewitem active changed" =>
			{
				ev.Id match
				{
					case "{engineslistview}" => EngineManager.enginelist ! ActiveChangedMsg(ev.index,ev.value.toBoolean)
					case _ => unhandled
				}
			}
			case "combobox selected" =>
			{
				ev.Id match
				{
					case "{bookcombo}" => GuiSetCurrentBook(ev.value)
					case _ => unhandled
				}
			}
			case "button pressed" =>
			{
				val backmake=(ev.Id=="{makesb}")
				val storemake=(ev.Id=="{makes}")||backmake

				if((ev.Id=="{make}")||storemake)
				{
					SpecialMake(backmake,storemake)
				}
				else ev.Id match
				{
					case "{addtotraining}" => AddToTraining
					case "{deletefromtraining}" => DeleteFromTraining
					case "{trainingback}" => TrainingBack
					case "{trainingrandom}" => TrainingRandom
					case "{star}" => Star
					case "{tostar}" => ToStar
					case "{hint}" => Hint
					case "{applyboardsettings}" => ApplyBoardSettings					
					case "{showauxbooks}" => GuiShowAuxBooks
					case "{adduciengine}" => AddEngine(kind="uci")
					case "{addxboardengine}" => AddEngine(kind="xboard")
					case "{addtobook}" => GuiAddToBook
					case "{flip}" => SetGuiFlip(!GetFlip)
					case "{reset}" => GuiReset
					case "{tobegin}" => GuiToBegin
					case "{back}" => GuiBack
					case "{forward}" => GuiForward
					case "{toend}" => GuiToEnd
					case "{delete}" => GuiDelete
					case "{start}" => EngineManager.enginelist ! StartAllMsg()
					case "{stop}" => EngineManager.enginelist ! StopAllMsg()
					case "{pastepgnbutton}" => PastePgn
					case "{addbook}" => GuiAddBook
					case "{savebook}" => SaveBook
					case "{lookupsolution}" => GuiLookUpSolution
					case "{lookupsolutiontree}" => GuiLookUpSolutionTree
					case "{delallmoves}" => GuiDelAllMoves
					case "{search}" => GuiSearch
					case "{evalall}" => Eval.EvalAll()
					case "{evalthis}" => GuiEvalThis
					case "{add1p}" => Eval.EvalAll(add=true,deep=true)
					case "{add6}" => Eval.EvalAll(add=true,num=6)
					case "{refine1}" => Eval.EvalAll(1,deep=true)
					case "{refine3}" => Eval.EvalAll(3,deep=true)
					case "{minimax}" => Minimax()
					case "{minimaxremove}" => if(Confirm("Do you really want to remove subtree ?")) Minimax(remove=true)
					case _ =>
					{
						if(ev.parts.length>1)
						{
							ev.parts(0) match
							{
								case "auxbookimport" => AuxBookImport(ev.parts(1))
								case "annot" => GuiAnnotateMove(ev.parts(1))
								case _ => unhandled
							}					
						}						
					}
				}		
			}
			case "board clicked" =>
			{
				ev.Id match {
					case "{mainboard}" => if(!GB("{components}#{disableboardclick}",false)) GuiBoardClicked(ev.value.toInt)
					case _ => unhandled
				}				
			}
			case "menuitem clicked" =>
			{
				ev.Id match
				{
					case "{closeauxbooks}" => CloseAuxBooks
					case "{openpgn}" => OpenPgn
					case "{openpgnlist}" => OpenPgnList
					case "{openmultipgn}" => OpenMultipleGamePgn
					case "{savepgnas}" => SavePgnAs
					case "{newpgnlist}" => NewPgnList
					case "{savepgnlistas}" => SavePgnListAs
					case "{synclichessbook}" => PgnSync.SyncLichessBook
					case "{ratingcharts}" => RatingCharts
					case "{showauxbooks}" => ShowAuxiliaryBooks
					case "{addpgngamestobook}" => AddPgnGamesToBook
					case "{gen960}" => board.GenAll960
					case "{delbook}" => if(Confirm("Do you really want to delete < "+GetCurentBook+" > book ?")) GuiDelBook
					case "{browseallgames}" => BrowseAllGames
					case "{addallgamestopgnlist}" => AddAllGamesToPgnList
					case "{filterpgn}" => FilterPGN
					case "{setupboard}" => SetupBoard
					case "{copyfenmenu}" => GuiCopyFen
					case "{copypgnmenu}" => GuiCopyPgn
					case "{copycurrentlinemenu}" => GuiCopyCurrentLine
					case "{copycurrentlinealgebmenu}" => GuiCopyCurrentLineAlgeb
					case "{pastefenmenu}" => GuiPasteFen
					case "{pastepgnmenu}" => PastePgn
					case "{booksettingsmenu}" => BookSettingsMenu
					case "{boardsettingsmenu}" => BoardSettingsMenu
					case "{profilesettingsmenu}" => ProfileSettingsMenu
					case "{evalsettingsmenu}" => EvalSettingsMenu
					case "{evalextrasettingsmenu}" => EvalExtraSettingsMenu
					case "{resetallsettings}" => if(Confirm("Are you sure ?")) reset_all_settings
					case "{helptopics}" => HelpTopics
					case "{createwiki}" => CreateWiki
					case _ => unhandled
				}
			}
			case "vbox clicked" =>
			{
				ev.Id match
				{
					case "{traincontrolvbox}" => TrainControlVboxClicked
					case "{mainboardvbox}" => // handled by "board clicked"
					case _ => unhandled
				}
			}
			case "radiomenuitem clicked" =>
			{
				ev.togglegroup match
				{
					case "rmvariant" => GuiSetVariant(ev.value)
					case _ => unhandled
				}
			}
			case "manual move made" =>
			{
				ev.Id match
				{
					case "{mainboard}" => ManualMoveMade(ev.value)
					case _ => unhandled
				}
			}
			case "webview clicked" =>
			{
				ev.Id match
				{
					case "{pgnwebview}" => PgnClicked
					case "{moveswebview}" => MovesClicked
					case "{bookwebview}" => BookClicked
					case "{systemlog}" =>
					case "{execqueue}" =>
					case "{execqueuelog}" =>
					case _ => unhandled
				}
			}
			case "stage width resized" =>
			{

			}
			case "stage height resized" =>
			{

			}
			case _ => unhandled
		}
	}

	def Start(primaryStage:Stage)
	{
		ps=primaryStage

		if((!(new java.io.File(DataUtils.PathFromList(List("stuff","values.xml")))).exists)) set_defaults

		val rmvariantcontent=(for(v<-board.SUPPORTED_VARIANTS) yield {
			val selected=if(v==GetVariant) """ selected="true"""" else ""
			s"""<radiomenuitem id="{rm$v}"$selected togglegroup="rmvariant" text="$v"/>"""
		}).mkString("\n")

		val annotbuttons=(for((k,v) <- butils.ANNOTATIONS) yield {
			s"""<button id="{annot}#{$k}" text="$k" style="-fx-background-color: ${v.backgroundcolor};"/>"""
		}).mkString("\n")

		val blob=s"""
			|<vbox gap="0">
			|<menubar>
			|<menu text="File">
			|<menuitem id="{openpgn}" text="Open PGN"/>
			|<menuitem id="{openmultipgn}" text="Open multiple game PGN"/>			
			|<menuitem id="{savepgnas}" text="Save PGN as"/>
			|<menuitem id="{openpgnlist}" text="Open PGN list"/>
			|<menuitem id="{newpgnlist}" text="New PGN list"/>
			|<menuitem id="{savepgnlistas}" text="Save PGN list as"/>
			|</menu>
			|<menu text="Copy">
			|<menuitem id="{copyfenmenu}" text="Copy FEN to clipboard"/>
			|<menuitem id="{copypgnmenu}" text="Copy PGN to clipboard"/>			
			|<menuitem id="{copycurrentlinemenu}" text="Copy current line"/>
			|<menuitem id="{copycurrentlinealgebmenu}" text="Copy current line algeb"/>
			|</menu>
			|<menu text="Paste">
			|<menuitem id="{pastefenmenu}" text="Paste FEN from clipboard"/>
			|<menuitem id="{pastepgnmenu}" text="Paste PGN from clipboard"/>
			|</menu>
			|<menu text="Tools">
			|<menuitem id="{synclichessbook}" text="Sync lichess book"/>
			|<menuitem id="{ratingcharts}" text="Rating charts"/>
			|<menuitem id="{showauxbooks}" text="Show auxiliary books"/>
			|<menuitem id="{closeauxbooks}" text="Close auxiliary books"/>
			|<menuitem id="{addpgngamestobook}" text="Add PGN games to book"/>			
			|<menuitem id="{filterpgn}" text="Filter PGN"/>
			|<menuitem id="{setupboard}" text="Setup board manually"/>
			|<menuitem id="{browseallgames}" text="Browse all games"/>			
			|<menuitem id="{addallgamestopgnlist}" text="Add all games to PGN list"/>			
			|<menuitem id="{gen960}" text="Generate all 960 positions"/>			
			|<menuitem id="{delbook}" text="Delete current book"/>			
			|</menu>
			|<menu text="Settings">
			|<menuitem id="{evalsettingsmenu}" text="Eval"/>
			|<menuitem id="{evalextrasettingsmenu}" text="Eval extra"/>
			|<menuitem id="{booksettingsmenu}" text="Book"/>
			|<menuitem id="{boardsettingsmenu}" text="Board"/>
			|<menuitem id="{profilesettingsmenu}" text="Profile"/>
			|<menuitem id="{resetallsettings}" text="Reset all"/>
			|</menu>
			|<menu text="Help">
			|<menuitem id="{helptopics}" text="Topics"/>
			|<menuitem id="{createwiki}" text="Create Wiki"/>
			|</menu>
			|<menu text="Variant">
			|$rmvariantcontent
			|</menu>
			|</menubar>
			|<hbox gap="5" padding="0">			
			|<vbox id="{mainboardvbox}">
			|<guiboard id="{mainboard}"/>
			|<vbox bimage="control.jpg" cover="false">
			|<hbox padding="2">
			|<button id="{flip}" img="icons/flip.png" style="round"/>
			|<button id="{reset}" img="icons/resett.png" style="round"/>
			|<button id="{tobegin}" img="icons/begint.png" style="round"/>
			|<button id="{back}" img="icons/backt.png" style="round"/>
			|<button id="{forward}" img="icons/forwardt.png" style="round"/>
			|<button id="{toend}" img="icons/endt.png" style="round"/>
			|<button id="{delete}" img="icons/delt.png" style="round"/>
			|<button id="{start}" img="icons/startt.png" style="round"/>
			|<button id="{stop}" img="icons/stopt.png" style="round"/>
			|<button id="{make}" img="icons/maket.png" style="round"/>
			|</hbox>			
			|<hbox paddings="7,2,2,5" style="-fx-background-color: #7f7f7f; -fx-border-width: 1px; -fx-border-radius: 15px; -fx-border-style: solid;">
			|<vbox id="{traincontrolvbox}" padding="0">
			|<hbox paddings="3,5,1,7" height="26.0" style="-fx-background-color: #afafaf; -fx-border-width: 1px; -fx-border-radius: 15px; -fx-border-style: solid; -fx-border-color: #5f5f5f;">
			|<label text="Train"/>
			|<checkbox id="{trainingmode}"/>
			|<label text="Build"/>
			|<checkbox id="{buildtrainingmode}"/>			
			|</hbox>
			|</vbox>
			|<button id="{addtotraining}" img="icons/addto.png" style="transp"/>
			|<button id="{deletefromtraining}" img="icons/remove.png" style="transp"/>
			|<button id="{trainingback}" img="icons/goback.png" style="transp"/>
			|<button id="{trainingrandom}" img="icons/roulette.png" style="transp"/>
			|<button id="{star}" img="icons/anchor.png" style="transp"/>
			|<button id="{tostar}" img="icons/favourite.png" style="transp"/>
			|<button id="{hint}" img="icons/hint.png" style="transp"/>
			|</hbox>
			|</vbox>
			|</vbox>
			|<tabpane id="{maintabpane}">
			|<tab caption="Book">
			|<vbox gap="2" padding="4">
			|<hbox padding="1">
			|<button id="{pastepgnbutton}" img="icons/paste.png"/>			
			|<combobox id="{bookcombo}"/>
			|<button id="{addbook}" img="icons/add.png"/>			
			|<button id="{savebook}" img="icons/savet.png"/>			
			|<label id="{booksize}"/>
			|<button id="{lookupsolution}" style="-fx-background-color: #afffaf;" img="icons/look.png"/>			
			|<button id="{lookupsolutiontree}" style="-fx-background-color: #ffafaf;" img="icons/look.png"/>
			|<button id="{addtobook}" img="icons/addto.png" sizex="15" sizey="15"/>
			|$annotbuttons
			|<button id="{delallmoves}" img="icons/caution.png"/>			
			|<button id="{search}" text="Search"/>			
			|</hbox>
			|<hbox padding="1">
			|<button id="{tobegin}" img="icons/begint.png" style="transp"/>
			|<button id="{back}" img="icons/backt.png" style="transp"/>
			|<button id="{forward}" img="icons/forwardt.png" style="transp"/>
			|<button id="{toend}" img="icons/endt.png" style="transp"/>
			|<button id="{delete}" img="icons/delt.png" style="transp"/>			
			|<button id="{start}" img="icons/startt.png" style="transp"/>
			|<button id="{stop}" img="icons/stopt.png" style="transp"/>
			|<button id="{makes}" img="icons/maket.png" style="transp"/>
			|<button id="{makesb}" img="icons/goback.png" sizex="15" sizey="15" style="transp"/>			
			|<button id="{add1p}" img="icons/add.png" text="1+"/>
			|<button id="{add6}" img="icons/add.png" text="6"/>
			|<button id="{evalthis}" text="Eval this"/>
			|<button id="{refine1}" img="icons/angrybird.png" text="1"/>
			|<button id="{refine3}" img="icons/angrybird.png" text="3"/>
			|<button id="{evalall}" text="Eval all"/>
			|<button id="{minimax}" text="Minimax"/>	
			|<button id="{minimaxremove}" text="Remove"/>	
			|</hbox>
			|<webview id="{bookwebview}"/>
			|</vbox>
			|</tab>
			|<tab caption="PGN">
			|<webview id="{pgnwebview}"/>
			|</tab>
			|<tab caption="PGN games">
			|<gamebrowser id="{pgngamebrowser}"/>
			|</tab>
			|<tab caption="Book games">
			|<gamebrowser id="{bookgamebrowser}"/>
			|</tab>
			|<tab caption="Moves">
			|<webview id="{moveswebview}"/>
			|</tab>
			|<tab caption="Material">
			|<webview id="{materialwebview}"/>
			|</tab>
			|<tab caption="Engines">
			|<vbox>
			|<hbox>
			|<button id="{adduciengine}" text="Add UCI engine"/>
			|<button id="{addxboardengine}" text="Add XBOARD engine"/>
			|</hbox>
			|<editablelistview id="{engineslistview}" style="-fx-font-size: 16px;"/>
			|</vbox>
			|</tab>			
			|<tab caption="Systemlog">
			|<webview id="{systemlog}"/>
			|</tab>
			|<tab caption="Execqueue">
			|<webview id="{execqueue}"/>
			|</tab>
			|<tab caption="Queuelog">
			|<webview id="{execqueuelog}"/>
			|</tab>
			|</tabpane>
			|</hbox>
			|</vbox>
		""".stripMargin

		MyStage(id="{mainstage}",s=primaryStage,title="smartchess",blob=blob,handler=handler)

		SetVariant(GetVariant)

		LoadGameState()

		selectedmaintab=GI("{selectedmaintab}",selectedmaintab)

		SelectTab("{maintabpane}",selectedmaintab)

		val mb=GetMainBoard

		val cs=mb.canvas_size

		val ms=GetStage("{mainstage}")

		ms.SetHeight(cs+185)
		ms.SetWidth(cs+825)

		prev_anytraining=IsAnyTraining

		Star

		Update

		PgnSync.startPgnSync
	}

	def Stop
	{
		CloseAuxBooks

		book.Save		

		SaveGameState()

		EngineManager.enginelist ! ShutDownAllMsg(restartactorsystem=false)

		pgnsyncProc.Destroy
	}
}