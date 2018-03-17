package smartchess

////////////////////////////////////////////////////////////////////

object PgnSync{
    import MyApp._

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

    def SyncLichessBook{
        for(color <- List("black","white")) SetCurrentBook(bookName(color))
    }
}