package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import Builder._

////////////////////////////////////////////////////////////////////

object MyApp
{
	////////////////////////////////////////////////////////////////////
	// global variables

	var g:game=null

	var book:Book=null

	var ps:Stage=null

	var selectedmaintab=0

	var prev_fen:String=null

	var prev_training_fen:String=null

	var prev_books=List[String]()

	var prev_selbook=""

	var prev_anytraining=false

	var auxbooks=Map[String,Book]()

	var star:GameNode=null	

	////////////////////////////////////////////////////////////////////
	// access shorthands

	def gpath(name:String) = "stuff/"+name+"game.pgn"
	def lpath(name:String) = "stuff/"+name+"linealgeb.txt"

	def bdir:String = "stuff/"+board.variant+"/books"

	def trainingpath = "stuff/"+board.variant+"/training.txt"

	def GetVariant:String = GS("{settings}#{variant}","Standard")

	def GetMainBoard:GuiBoard = GetGuiBoard("{mainboard}")

	def GetFlip:Boolean = GB("{settings}#{flip}",false)
	def SetFlip(newflip:Boolean=false) = Set("{settings}#{flip}",""+newflip)

	def GetMyHandle = GS("{settings}#{myhandle}","")

	def GetBooks = DataUtils.getListOfFileNamesWithExt(bdir,"xml")
	def GetCurentBook:String = GS(Cve("{selectedbook}"),"default")

	def GetPgnGameBrowser = GetGameBrowser("{pgngamebrowser}")
	def GetBookGameBrowser = GetGameBrowser("{bookgamebrowser}")

	def GetEngineDir = GS(Cve("{enginedir}"),"")
	def SetEngineDir(dir:String) = Set(Cve("{enginedir}"),dir)

	def SetBookSizeText(what:String) = GetMyText("{booksize}").SetText(what)

	def IsAnyTraining = GB("{components}#{trainingmode}",false) || GB("{components}#{buildtrainingmode}",false)
	def IsPlayMode:Boolean = GB("{components}#{trainingmode}",false) && GB("{components}#{buildtrainingmode}",false)
	def IsTrainingMode:Boolean = GB("{components}#{trainingmode}",false) && (!GB("{components}#{buildtrainingmode}",false))
	def IsBuildTrainingMode:Boolean = GB("{components}#{buildtrainingmode}",false) && (!GB("{components}#{trainingmode}",false))

	def SelectBookTab() = selectedmaintab=SelectTab("{maintabpane}","Book")
	def SelectMovesTab() = selectedmaintab=SelectTab("{maintabpane}","Moves")
	def SelectLogTab() = selectedmaintab=SelectTab("{maintabpane}","Systemlog")
	def SelectPgnGamesTab() = selectedmaintab=SelectTab("{maintabpane}","PGN games")
	def SelectPgnTab() = selectedmaintab=SelectTab("{maintabpane}","PGN")
	def SelectMainTab(index:Int) = selectedmaintab=SelectTab("{maintabpane}",index)

	////////////////////////////////////////////////////////////////////
	// exported functions

	////////////////////////////////////////////////////////////////////
	// core
	def Init = MyAppCore.Init
	def Start(primaryStage:Stage) = MyAppCore.Start(primaryStage)
	def Stop = MyAppCore.Stop
	////////////////////////////////////////////////////////////////////
	def set_defaults = MyAppCore.set_defaults
	def handler(ev:MyEvent) = MyAppCore.handler(ev)
	////////////////////////////////////////////////////////////////////

	def Update = MyAppUpdate.Update

	def SaveGameState(pgn:String=g.report_pgn,currentlinealgeb:String=g.current_line_algeb,name:String="current") = 
		MyAppImpl.SaveGameState(pgn,currentlinealgeb,name)
	def LoadGameState(name:String="current") = MyAppImpl.LoadGameState(name)

	def SetGuiFlip(flip:Boolean) = MyAppImpl.SetGuiFlip(flip)
	def CheckProfile = MyAppImpl.CheckProfile

	def AddPositionToBook(tfen:String):BookPosition = MyAppBook.AddPositionToBook(tfen)

	////////////////////////////////////////////////////////////////////
	// internally used functions

	// myappwebview
	def SetPriority(san:String) = MyAppWebview.SetPriority(san)
	def BookClicked = MyAppWebview.BookClicked
	def MovesClicked = MyAppWebview.MovesClicked
	def PgnClicked = MyAppWebview.PgnClicked
	// myappsettings
	def EvalSettingsMenu = MyAppSettings.EvalSettingsMenu
	def ProfileSettingsMenu = MyAppSettings.ProfileSettingsMenu
	def BoardSettingsMenu = MyAppSettings.BoardSettingsMenu
	def ApplyBoardSettings = MyAppSettings.ApplyBoardSettings
	def BookSettingsMenu = MyAppSettings.BookSettingsMenu
	def SetupBoard = MyAppSettings.SetupBoard
	// myappcopypaste
	def GuiPasteFen = MyAppCopyPaste.GuiPasteFen
	def PastePgn = MyAppCopyPaste.PastePgn
	def GuiCopyCurrentLineAlgeb = MyAppCopyPaste.GuiCopyCurrentLineAlgeb
	def GuiCopyCurrentLine = MyAppCopyPaste.GuiCopyCurrentLine
	def GuiCopyPgn = MyAppCopyPaste.GuiCopyPgn
	def GuiCopyFen = MyAppCopyPaste.GuiCopyFen
	// myappsolution	
	def LookUpSolution(tree:Boolean=false) = MyAppSolution.LookUpSolution(tree)
	def GuiLookUpSolutionTree = MyAppSolution.GuiLookUpSolutionTree
	def GuiLookUpSolution = MyAppSolution.GuiLookUpSolution		
	// myappbook
	def GuiAnnotateMove(annot:String) = MyAppBook.GuiAnnotateMove(annot)
	def GuiAddToBook = MyAppBook.GuiAddToBook	
	def GuiSetCurrentBook(name:String) = MyAppBook.GuiSetCurrentBook(name)
	def GuiAddBook = MyAppBook.GuiAddBook
	def SpecialMake(backmake:Boolean,storemake:Boolean) = MyAppBook.SpecialMake(backmake,storemake)
	def GuiDelAllMoves = MyAppBook.GuiDelAllMoves
	def Minimax(remove:Boolean=false,addtobook:Book=null) = MyAppBook.Minimax(remove,addtobook)
	def GuiDelBook = MyAppBook.GuiDelBook
	def GuiEvalThis = MyAppBook.GuiEvalThis
	def GuiSearch = MyAppBook.GuiSearch	
	// myapppgnlist	
	def AddAllGamesToPgnList = MyAppPgnList.AddAllGamesToPgnList
	def OpenPgnList = MyAppPgnList.OpenPgnList
	def SavePgnListAs = MyAppPgnList.SavePgnListAs
	def NewPgnList = MyAppPgnList.NewPgnList
	def PgnListSavedCallback(path:String) = MyAppPgnList.PgnListSavedCallback(path)	
	// myapptraining
	def MakeEngineMove(factor:Int=50,limit:Int= -butils.MATE_THRESOLD) = MyAppTraining.MakeEngineMove(factor,limit)
	def HintCallback() = MyAppTraining.HintCallback()
	def MakePlayMoveCallback() = MyAppTraining.MakePlayMoveCallback()
	def Hint = MyAppTraining.Hint
	def MakePlayMove = MyAppTraining.MakePlayMove
	def TrainControlVboxClicked = MyAppTraining.TrainControlVboxClicked
	def TrainingRandom = MyAppTraining.TrainingRandom
	def TrainingBack = MyAppTraining.TrainingBack
	def DeleteFromTraining = MyAppTraining.DeleteFromTraining
	def AddToTraining = MyAppTraining.AddToTraining
	def ShowTrainingScore(score:Int) = MyAppTraining.ShowTrainingScore(score)
	def HideTrainingScore = MyAppTraining.HideTrainingScore
	def MakeTrainingMove = MyAppTraining.MakeTrainingMove
	def BuildTrainingMove = MyAppTraining.BuildTrainingMove
	// myappauxbooks	
	def AuxBookImport(bookname:String) = MyAppAuxBooks.AuxBookImport(bookname)
	def GuiShowAuxBooks = MyAppAuxBooks.GuiShowAuxBooks
	def CloseAuxBooks = MyAppAuxBooks.CloseAuxBooks
	def OpenAuxBooks(close_only:Boolean=false) = MyAppAuxBooks.OpenAuxBooks(close_only)
	def ShowAuxiliaryBooks = MyAppAuxBooks.ShowAuxiliaryBooks
	// myapppgn
	def AddPgnGamesToBook = MyAppPgn.AddPgnGamesToBook
	def OpenMultipleGamePgn = MyAppPgn.OpenMultipleGamePgn
	def OpenPgn = MyAppPgn.OpenPgn
	def SavePgnAs = MyAppPgn.SavePgnAs
	// myappfilterpgn
	def FilterSearchFunc = MyAppFilterPgn.FilterSearchFunc
	def FilterSearch = MyAppFilterPgn.FilterSearch
	def FilterPGN = MyAppFilterPgn.FilterPGN
	def SetSearchProfile(filterfilename:String,filterplayerwhite:String,filterplayerblack:String) =
		MyAppFilterPgn.SetSearchProfile(filterfilename,filterplayerwhite,filterplayerblack)
	// myappbook	
	def DelBook(name:String) = MyAppBook.DelBook(name)
	def CreateNewBook = MyAppBook.CreateNewBook
	def DelMove = MyAppBook.DelMove
	def IncPlays = MyAppBook.IncPlays
	def AnnotateMove(annot:String=null) = MyAppBook.AnnotateMove(annot)
	def AddMoveToBook:BookEntry = MyAppBook.AddMoveToBook
	def SetCurrentBook(bookname:String,saveold:Boolean=true) = MyAppBook.SetCurrentBook(bookname,saveold)
	def AutoAdd = MyAppBook.AutoAdd
	def SaveBook = MyAppBook.SaveBook
	def DelAllMoves = MyAppBook.DelAllMoves	
	def AddCurrentPositionToBook = MyAppBook.AddCurrentPositionToBook
	def AddPreviousPositionToBook = MyAppBook.AddPreviousPositionToBook	
	// myappupdate
	def HighlightLastMove = MyAppUpdate.HighlightLastMove
	def UpdateAll = MyAppUpdate.UpdateAll
	def CheckFenChanged = MyAppUpdate.CheckFenChanged
	def UpdateFenChanged = MyAppUpdate.UpdateFenChanged
	def SetBoard = MyAppUpdate.SetBoard
	def CheckBooksChanged = MyAppUpdate.CheckBooksChanged
	def UpdateBooksChanged = MyAppUpdate.UpdateBooksChanged
	def CheckTrainingChanged = MyAppUpdate.CheckTrainingChanged
	def UpdateTitle = MyAppUpdate.UpdateTitle
	def UpdateAuxBooks = MyAppUpdate.UpdateAuxBooks
	def UpdateBookGameBrowser = MyAppUpdate.UpdateBookGameBrowser
	def UpdateMovesHtml = MyAppUpdate.UpdateMovesHtml
	def UpdateBookHtml = MyAppUpdate.UpdateBookHtml
	def UpdatePgnHtml = MyAppUpdate.UpdatePgnHtml
	// myappimpl
	def GuiSetVariant(variant:String) = MyAppImpl.GuiSetVariant(variant)
	def GuiBoardClicked(file:Int) = MyAppImpl.GuiBoardClicked(file)
	def GuiDelete = MyAppImpl.GuiDelete
	def GuiToEnd = MyAppImpl.GuiToEnd
	def GuiForward = MyAppImpl.GuiForward
	def GuiBack = MyAppImpl.GuiBack
	def GuiToBegin = MyAppImpl.GuiToBegin
	def GuiReset = MyAppImpl.GuiReset
	def SetSelectedMainTab(i:Int) = MyAppImpl.SetSelectedMainTab(i)
	def ManualMoveMade(san:String) = MyAppImpl.ManualMoveMade(san)
	def ToStar = MyAppImpl.ToStar
	def Star = MyAppImpl.Star
	def BrowseAllGames = MyAppImpl.BrowseAllGames		
	def AddEngine(kind:String="uci") = MyAppImpl.AddEngine(kind)	
	def MakeAlgebMove(algeb:String) = MyAppImpl.MakeAlgebMove(algeb)
	def MakeSanMove(san:String) = MyAppImpl.MakeSanMove(san)
	def ToEnd = MyAppImpl.ToEnd
	def Forward = MyAppImpl.Forward
	def Back = MyAppImpl.Back
	def ToBegin = MyAppImpl.ToBegin
	def Delete = MyAppImpl.Delete
	def Reset = MyAppImpl.Reset	
	def SetVariant(newvariant:String=null) = MyAppImpl.SetVariant(newvariant)
	def HelpTopics = MyAppImpl.HelpTopics
	def CreateWiki = MyAppImpl.CreateWiki
}

