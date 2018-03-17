package smartchess

////////////////////////////////////////////////////////////////////

object PgnSync{
    def startPgnSync{
        if(MyApp.pgnsyncProc!=null){
            MyApp.pgnsyncProc.Destroy
        }

        MyApp.pgnsyncProc=Proc(
			dir="pgnsync",
			progandargs=List("node.exe","index.js",MyApp.GetMyHandle),
			ProcessOut=PgnSync.handlePgnSyncOut
		)

		MyApp.pgnsyncProc.Start
    }

    def handlePgnSyncOut(buffer:String){
        MyActor.Log(buffer)
    }
}