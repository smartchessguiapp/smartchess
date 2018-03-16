// server startup

if(!fileExists("games")){
    if(!createDir("games")){
        logErr("fatal: could not create games directory")
        process.exit(1)
    }else{
        console.log("games directory created ok")
    }
}else{
    console.log("games directory exists")
}

setHandle("versenyzo")

schedule.scheduleJob(`0 * * * * *`, function(){
    fetchGames()
})