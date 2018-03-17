package smartchess

////////////////////////////////////////////////////////////////////

object PgnSync{
    import MyApp._
    import Builder._

    def startPgnSync{
        if(pgnsyncProc!=null){
            pgnsyncProc.Destroy
        }

        pgnsyncProc=Proc(
			dir="pgnsync",
			progandargs=List("node.exe","index.js",GetMyHandle),
			ProcessOut=PgnSync.handlePgnSyncOut
		)

		pgnsyncProc.Start
    }

    def handlePgnSyncOut(buffer:String){
        MyActor.Log(buffer)
    }

    def bookName(color:String):String=GetMyHandle+"_"+color
    def bookPgnPath(color:String):String="stuff"+java.io.File.separator+board.variant+java.io.File.separator+"filtered"+java.io.File.separator+bookName(color)+".pgn"
    def pgnPath:String="pgnsync"+java.io.File.separator+"games"+java.io.File.separator+GetMyHandle+".pgn"

    def SyncLichessBook{
        Set("{components}#{filterinputfilename}#{path}",pgnPath)
        for(color <- List("black","white")){
            println("color "+color)
            SetCurrentBook(bookName(color))            
            FilterPGN
            MyAppFilterPgn.HandleColor(color)
            println("filtering pgn")
            FilterSearchFunc            
            val bpp=bookPgnPath(color)
            println("opening "+bpp)
            val pgnscontent=DataUtils.ReadFileToString(bpp)
			val pgns=game.split_pgn(pgnscontent).toList
            println("num games "+pgns.length)
            val pgb=GetPgnGameBrowser
            println("loading list")
            pgb.load_list(GameBrowser.DigestPgns(pgns))
            val maxbookdepth=GD("{components}#{maxbookdepth}",20.0).toInt
            println("adding games at max depth "+maxbookdepth)
            pgb.add_games_to_book(book,maxbookdepth,true)
            CloseStage("{filterpgndialog}")
        }
        SelectBookTab
        Update
    }
}