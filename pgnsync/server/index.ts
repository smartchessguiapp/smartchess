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

if(!fileExists("chartpgns")){
    if(!createDir("chartpgns")){
        logErr("fatal: could not create chartpgns directory")
        process.exit(1)
    }else{
        console.log("chartpgns directory created ok")
    }
}else{
    console.log("chartpgns directory exists")
}

setHandle(process.argv[2])

schedule.scheduleJob(`0 * * * * *`, function(){
    fetchGames()
})