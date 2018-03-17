package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppSettings
{
	import MyApp._

	def BookSettingsMenu
	{
		val blob=s"""
			|<vbox width="800.0">
			|<gridpane hgap="10" vgap="10">					
			|<label text="Auto add move" r="1" c="1"/>
			|<checkbox id="{autoaddmove}" r="1" c="2"/>
			|<label text="Inc. plays" r="2" c="1"/>
			|<checkbox id="{autoincplays}" r="2" c="2"/>
			|<label text="Antichess dir" r="3" c="1"/>
			|<directorychooser id="{antichessdir}" r="3" c="2"/>
			|<label text="Max book depth" r="4" c="1"/>
			|<slider roundto="1" id="{maxbookdepth}" r="4" c="2" value="20.0" width="450.0" height="50.0" min="0.0" max="50.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Check replica" r="5" c="1"/>
			|<checkbox id="{checkreplica}" r="5" c="2"/>
			|<label text="Hide result stats" r="6" c="1"/>
			|<checkbox id="{hideresultstats}" r="6" c="2"/>
			|<label text="Show met by move" r="7" c="1"/>
			|<checkbox id="{showmetbymove}" r="7" c="2"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		MyStage("{booksettingsdialog}","Book settings",blob,
			modal=true,usewidth=false,useheight=false,handler=handler)
	}

	def ApplyBoardSettings
	{
		CloseStage("{boardsettingsdialog}")

		CloseStage("{mainstage}")

		Stop

		Start(ps)
	}

	def BoardSettingsMenu
	{
		val blob=s"""
			|<vbox>
			|<gridpane hgap="10" vgap="10">
			|<label text="Piece size" r="1" c="1"/>
			|<slider roundto="1" id="{mainboard}#{piecesize}" value="${GuiBoard.PIECESIZE}" r="1" cs="3" c="2" width="300.0" min="20.0" max="100.0" majortickunit="10.0" minortickcount="2" showticklabels="true" showtickmarks="true"/>
			|<label text="White Pc.Col." r="2" c="1"/>
			|<colorpicker r="2" c="2" color="${GuiBoard.WHITEPIECECOLOR}" id="{mainboard}#{whitepiececolor}"/>
			|<label text="Black Pc.Col." r="2" c="3"/>
			|<colorpicker r="2" c="4" color="${GuiBoard.BLACKPIECECOLOR}" id="{mainboard}#{blackpiececolor}"/>
			|<label text="Light Sq.Col." r="3" c="1"/>
			|<colorpicker r="3" c="2" color="${GuiBoard.LIGHTSQUARECOLOR}" id="{mainboard}#{lightsquarecolor}"/>
			|<label text="Dark Sq.Col." r="3" c="3"/>
			|<colorpicker r="3" c="4" color="${GuiBoard.DARKSQUARECOLOR}" id="{mainboard}#{darksquarecolor}"/>
			|<label text="Font" r="4" c="1"/>
			|<combobox r="4" c="2" selected="${GuiBoard.FONT}" id="{mainboard}#{font}"/>
			|<label text="Material" r="4" c="3"/>
			|<combobox r="4" c="4" selected="${GuiBoard.MATERIAL}" id="{mainboard}#{material}"/>
			|<label text="Brd. Opacity" r="5" c="1"/>
			|<slider id="{mainboard}#{boardopacity}" value="${GuiBoard.BOARDOPACITY}" r="5" cs="3" c="2" width="300.0" min="0.0" max="1.0" majortickunit="0.1" minortickcount="2" showticklabels="true" showtickmarks="true"/>
			|<label text="Piece factor" r="6" c="1"/>
			|<slider id="{mainboard}#{piecefactor}" value="${GuiBoard.PIECEFACTOR}" r="6" cs="3" c="2" width="300.0" min="0.0" max="1.0" majortickunit="0.1" minortickcount="2" showticklabels="true" showtickmarks="true"/>
			|</gridpane>
			|<button id="{applyboardsettings}" text="Apply changes"/>
			|</vbox>
		""".stripMargin

		MyStage("{boardsettingsdialog}","Board settings",blob,
			modal=true,usewidth=false,useheight=false,handler=handler)

		val mc=GetMyComboBox("{mainboard}#{material}")
		mc.CreateFromItems(List("wood","marble","rock"))
		mc.Select(GS("{components}#{mainboard}#{material}#{selected}","wood"))

		val fc=GetMyComboBox("{mainboard}#{font}")
		fc.CreateFromItems(List("AVENFONT","CASEFONT","LUCEFONT","MARRFONT","MERIFONTNEW"))
		fc.Select(GS("{components}#{mainboard}#{font}#{selected}","MERIFONTNEW"))
	}

	def ProfileSettingsMenu
	{
		def profilesettings_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{profilesettingsok}")
				{
					val myhandle=GetMyText("{myhandle}").GetText

					Set("{settings}#{myhandle}",myhandle)

					CloseStage("{profilesettingsdialog}")

					PgnSync.startPgnSync
				}
			}
		}

		val blob=s"""
			|<vbox>
			|<gridpane vgap="5" hgap="5">
			|<label text="My handle" r="1" c="1"/>
			|<textfield id="{myhandle}" width="250.0" style="-fx-font-size: 18px;" r="1" c="2"/>					
			|<label text="Auto select PGN tab" r="2" c="1"/>
			|<checkbox id="{autoselectpgntab}" r="2" c="2"/>
			|<label text="Pgn list page size" r="3" c="1"/>
			|<slider roundto="1" id="{pgnlistpagesize}" r="3" c="2" value="3.0" width="250.0" height="50.0" min="1.0" max="10.0" majortickunit="1.0" minortickcount="0" showticklabels="true" showtickmarks="true"/>
			|<label text="Disable board click navigation" r="4" c="1"/>
			|<checkbox id="{disableboardclick}" r="4" c="2"/>
			|<button id="{profilesettingsok}" width="380.0" style="-fx-font-size: 18px;" text="Ok" r="10" c="1" cs="2"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		MyStage("{profilesettingsdialog}","Profile settings",blob,
			modal=true,usewidth=false,useheight=false,handler=profilesettings_handler)

		val myhandle=GS("{settings}#{myhandle}")

		GetMyText("{myhandle}").SetText(myhandle)
	}

	def EvalSettingsMenu
	{
		val sheight="35.0"
		val swidth="800.0"
		val blob=s"""
			|<vbox>
			|<gridpane hgap="10" vgap="10">					
			|<label text="Depth" r="1" c="1"/>
			|<slider roundto="1" id="{evaldepth}" r="1" c="2" value="20.0" width="$swidth" height="$sheight" min="0.0" max="50.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Bonus" r="2" c="1"/>
			|<slider roundto="1" id="{evalbonus}" r="2" c="2" value="5.0" width="$swidth" height="$sheight" min="0.0" max="20.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Color moves by score" r="3" c="1"/>
			|<checkbox id="{colorbyscore}" r="3" c="2"/>
			|<label text="Minimax depth" r="4" c="1"/>
			|<slider roundto="1" id="{minimaxdepth}" r="4" c="2" value="50.0" width="$swidth" height="$sheight" min="0.0" max="100.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Play move factor" r="5" c="1"/>
			|<slider roundto="1" id="{playmovefactor}" r="5" c="2" value="50.0" width="$swidth" height="$sheight" min="0.0" max="100.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>					
			|<label text="Play move limit" r="6" c="1"/>
			|<slider roundto="25" id="{playmovelimit}" r="6" c="2" value="-10000.0" width="$swidth" height="$sheight" min="-10000.0" max="0.0" majortickunit="100.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>					
			|<label text="Hint factor" r="7" c="1"/>
			|<slider roundto="1" id="{hintfactor}" r="7" c="2" value="80.0" width="$swidth" height="$sheight" min="0.0" max="100.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>					
			|<label text="Hint limit" r="8" c="1"/>
			|<slider roundto="1" id="{hintlimit}" r="8" c="2" value="-300.0" width="$swidth" height="$sheight" min="-1000.0" max="1000.0" majortickunit="100.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>	
			|<label text="Search factor" r="9" c="1"/>
			|<slider roundto="1" id="{searchfactor}" r="9" c="2" value="80.0" width="$swidth" height="$sheight" min="0.0" max="100.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>									
			|<label text="Search depth bonus" r="10" c="1"/>
			|<slider roundto="1" id="{searchdepthbonus}" r="10" c="2" value="5.0" width="$swidth" height="$sheight" min="0.0" max="50.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Minimax after" r="11" c="1"/>
			|<slider roundto="1" id="{minimaxafter}" r="11" c="2" value="1.0" width="$swidth" height="$sheight" min="0.0" max="25.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Create minimax log" r="12" c="1"/>
			|<checkbox id="{dominimaxlog}" r="12" c="2"/>
			|<label text="Use eval timeout" r="14" c="1"/>
			|<checkbox id="{useevaltimeout}" r="14" c="2"/>
			|<label text="Eval timeout" r="15" c="1"/>
			|<slider roundto="1" id="{evaltimeout}" r="15" c="2" value="60.0" width="$swidth" height="$sheight" min="0.0" max="600.0" majortickunit="60.0" minortickcount="1" showticklabels="true" showtickmarks="true"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		MyStage("{evalsettingsdialog}","Eval settings",blob,
			modal=true,usewidth=false,useheight=false,handler=handler)
	}

	def EvalExtraSettingsMenu
	{
		val sheight="35.0"
		val swidth="800.0"
		val blob=s"""
			|<vbox>
			|<gridpane hgap="10" vgap="10">					
			|<label text="Minimax after chunk" r="1" c="1"/>
			|<slider roundto="1" id="{minimaxafterchunk}" r="1" c="2" value="1.0" width="$swidth" height="$sheight" min="0.0" max="400.0" majortickunit="50.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Weight by evals" r="2" c="1"/>
			|<checkbox id="{weightbyevals}" r="2" c="2"/>
			|<label text="Weight by evals bias" r="3" c="1"/>
			|<slider id="{weightbyevalsbias}" r="3" c="2" value="150.0" width="$swidth" height="$sheight" min="0.0" max="1000.0" majortickunit="50.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|<label text="Weight by evals divisor" r="4" c="1"/>
			|<slider id="{weightbyevalsdivisor}" r="4" c="2" value="50.0" width="$swidth" height="$sheight" min="0.0" max="500.0" majortickunit="50.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|</gridpane>
			|</vbox>
		""".stripMargin

		MyStage("{evalextrasettingsdialog}","Eval extra settings",blob,
			modal=true,usewidth=false,useheight=false,handler=handler)
	}

		def SetupBoard
	{
		def CloseSetupBoardStage
		{
			CloseStage("{setupboarddialog}")
		}

		val fen=g.report_fen

		val posfen=g.b.report_pos_fen
		val turnfen=g.b.report_turn_fen
		val castlingfen=g.b.report_castling_fen
		val epfen=g.b.report_ep_fen
		val halfmovefen=g.b.report_halfmove_fen
		val fullmovefen=g.b.report_fullmove_fen

		def setup_board_handler(ev:MyEvent)
		{
			if(ev.kind=="button pressed")
			{
				if(ev.Id=="{clearsetupboard}")
				{
					GetGuiBoard("{setupboard}").clear_board
				}

				if(ev.Id=="{cancelsetupboard}")
				{
					CloseSetupBoardStage
				}

				if(ev.Id=="{applysetupboard}")
				{
					val newposfen=GetGuiBoard("{setupboard}").b.report_pos_fen
					val newturnfen=if(GetMyComboBox("{turncombo}").GetSelected=="White") "w" else "b"
					val newcastlingfen=GetMyComboBox("{castlingrightscombo}").GetSelected
					val newepfen=GetMyComboBox("{epsquarecombo}").GetSelected
					val newhalfmovefen=DataUtils.ParseInt(GetMyText("{halfmoveclocktext}").GetText,0)
					val newfullmovefen=DataUtils.ParseInt(GetMyText("{fullmovenumbertext}").GetText,1)

					val newfen=s"$newposfen $newturnfen $newcastlingfen $newepfen $newhalfmovefen $newfullmovefen"							

					CloseSetupBoardStage

					g.set_from_fen(newfen)

					Update
				}
			}
		}

		val blob=s"""
			|<hbox>
			|<guiboard id="{setupboard}" setup="true"/>
			|<vbox>
			|<button id="{clearsetupboard}" text="Clear board"/>
			|<label text="Turn:"/>
			|<combobox width="75.0" id="{turncombo}"/>
			|<label text="Castling rights:"/>
			|<combobox width="150.0" id="{castlingrightscombo}"/>
			|<label text="Ep square:"/>
			|<combobox width="75.0" id="{epsquarecombo}"/>
			|<label text="Halfmove clock:"/>
			|<textfield width="75.0" forcedefault="true" text="$halfmovefen" id="{halfmoveclocktext}"/>
			|<label text="Fullmove number:"/>
			|<textfield width="75.0" forcedefault="true" text="$fullmovefen" id="{fullmovenumbertext}"/>
			|<button id="{applysetupboard}" text="Apply changes"/>
			|<button id="{cancelsetupboard}" text="Cancel"/>
			|</vbox>
			|</hbox>
		""".stripMargin

		MyStage("{setupboarddialog}","Setup board manually",blob,
			modal=true,usewidth=false,useheight=false,handler=setup_board_handler)

		GetGuiBoard("{setupboard}").set_from_fen(fen)

		GetMyComboBox("{turncombo}").CreateFromItems(List(
			"White","Black"),if(turnfen=="w") "White" else "Black")
		GetMyComboBox("{castlingrightscombo}").CreateFromItems(List(
			"-","KQkq","KQ","kq","K","Q","k","q","KQk","KQq","Kkq","Qkq","Kk","Kq","Qk","qq"),castlingfen)
		GetMyComboBox("{epsquarecombo}").CreateFromItems(List(
			"-","a3","a6","b3","b6","c3","c6","d3","d6","e3","e6","f3","f6","g3","g6","h3","h6"),epfen)
	}
}