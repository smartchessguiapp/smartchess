package smartchess

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

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
        //MyActor.Log(buffer)
        println(buffer)
        val parts=buffer.split(" ").toList
        if(parts.length>1){
            if(parts(0)=="status:"){
                syncstatus=parts.tail.mkString(" ")
                UpdateTitle
            }
        }
    }

    def bookName(color:String):String=GetMyHandle+"_"+color
    def bookPgnPath(color:String):String="stuff"+java.io.File.separator+board.variant+java.io.File.separator+"filtered"+java.io.File.separator+bookName(color)+".pgn"
    def pgnPath:String="pgnsync"+java.io.File.separator+"games"+java.io.File.separator+GetMyHandle+".pgn"

    var pgb:GameBrowser=null

    def FilterColor(color:String){        
        MyActor.Log("filtering pgn")
        FilterSearchFunc            
        val bpp=bookPgnPath(color)
        MyActor.Log("opening "+bpp)
        val pgnscontent=DataUtils.ReadFileToString(bpp)
        if(syncaborted) return
        MyActor.Log("splitting")
        val pgns=game.split_pgn(pgnscontent).toList        
        MyActor.Log("num games "+pgns.length)
        if(syncaborted) return        
        MyActor.Log("loading list")
        pgb.load_list(GameBrowser.DigestPgns(pgns))
        if(syncaborted) return
        val maxbookdepth=GD("{components}#{maxbookdepth}",20.0).toInt
        MyActor.Log("adding games at max depth "+maxbookdepth)
        pgb.add_games_to_book(book,maxbookdepth,true)        
        MyActor.Log("adding games done")    
    }

    var syncaborted=false

    def SyncLichessBook{
        pgb=GetPgnGameBrowser
        syncaborted=false
        GameBrowser.aborted=false        
        MyActor.Log("synchronizing lichess book")
        Set("{components}#{filterinputfilename}#{path}",pgnPath)
        FilterPGN
        SelectLogTab
        MyActor.Log("synchronizing colors")
        AbortDialog("Abort synchronizing book",()=>{
            GameBrowser.aborted=true
            MyAppFilterPgn.filteraborted=true
            syncaborted=true
        })        
        Future{
            MyActor.Log("colors loop")
            for(color <- List("black","white")){   
                if(!syncaborted){
                    MyActor.Log("setting current book")
                    SetCurrentBook(bookName(color))                        
                    MyActor.Log("filtering color "+color)
                    MyAppFilterPgn.handlecolordone=false
                    MyActor.queuedExecutor ! ExecutionItem(client="SyncLichessBook",code=new Runnable{def run{
                        MyAppFilterPgn.HandleColor(color)
                    }})						            
                    while(MyAppFilterPgn.handlecolordone==false){
                        println("waiting for handle color update")
                        try{Thread.sleep(50)}catch{case e:Throwable=>}
                    }
                    FilterColor(color)           
                    MyActor.Log("moving to next color")
                }
            }
            MyActor.Log("sync done")
            MyActor.queuedExecutor ! ExecutionItem(client="SyncLichessBook",code=new Runnable{def run{
                CloseStage("{filterpgndialog}")
                SelectBookTab
                Update
                CloseAbortDialog
            }})						        
        }        
    }
}