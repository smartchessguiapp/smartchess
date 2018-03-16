let games:any[]=[]

function handlePathJson(handle:string=LICHESS_HANDLE):string{
    return `games/${handle}.json`
}

function saveHandleJson(handle:string=LICHESS_HANDLE){
    let jsonText=JSON.stringify(games)

    writeTextFile(handlePathJson(),jsonText)
}

function setHandle(handle:string){
    LICHESS_HANDLE=handle

    games=[]

    let jsonText=readTextFile(handlePathJson())

    try{
        games=JSON.parse(jsonText)
    }catch(err){
        logErr(err)
    }

    console.log(`setting handle to ${handle}, games stored ${games.length}`)

    saveHandleJson()
}

function fetchGames(){
    console.log(`fetching games for ${LICHESS_HANDLE}`)

    if(LICHESS_HANDLE=="") return

    let lgs=new LichessGames(LICHESS_HANDLE,10,1)

    lgs.fetch(function(){
        if(lgs.nbResults>0){
            console.log(`fetched current page, results: ${lgs.nbResults}`)        
        }else{
            console.log(`no games were returned from fetch`)
        }
    })
}
