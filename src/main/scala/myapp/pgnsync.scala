package smartchess

////////////////////////////////////////////////////////////////////

object PgnSync{
    def handlePgnSyncOut(buffer:String){
        MyActor.Log(buffer)
    }
}