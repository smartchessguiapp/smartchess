let games:any[]=[]

let lastNbResults:number|null=null

function handlePathJson(handle:string=LICHESS_HANDLE):string{
    return `games/${handle}.json`
}

function handlePathPgn(handle:string=LICHESS_HANDLE):string{
    return `games/${handle}.pgn`
}

function saveHandleJson(handle:string=LICHESS_HANDLE){
    if(handle==""){
        console.log(`status: sync no handle specified`)
    }else{
        let jsonText=JSON.stringify(games)

        writeTextFile(handlePathJson(),jsonText)

        console.log(`status: sync ${handle} games synced ${games.length}${lastNbResults==null?` querying...`:`total ${lastNbResults}`}`)
    }    
}

function saveHandlePgn(handle:string=LICHESS_HANDLE){
    if(handle=="") return

    let pgn=games.map((game:any)=>new LichessGame().fromJson(game).reportPgn()).join("\n\n")

    writeTextFile(handlePathPgn(),pgn)
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
    saveHandlePgn()
}

function fetchGames(handle:string=LICHESS_HANDLE){
    console.log(`fetching games for ${LICHESS_HANDLE}`)

    if(LICHESS_HANDLE=="") return

    let lgs=new LichessGames(LICHESS_HANDLE,10,1)

    lgs.fetch(function(){
        if(lgs.nbResults>0){
            let nbResults=lgs.nbResults
            lastNbResults=nbResults
            console.log(`fetched current page, results: ${nbResults}`)        
            let numGames=games.length            
            let bestNetAdd=0
            let bestPage=1
            if(numGames>=nbResults){
                console.log(`games up to date`)
                console.log(`status: sync ${handle} games synced ${games.length} up to date`)
                return
            }
            let bestNb=10
            for(let tryNb=10;tryNb<=100;tryNb++){
                let tryPage=1
                let fromIndex
                while( ( fromIndex = nbResults - tryPage * tryNb ) > numGames ) tryPage++
                let netAdd=fromIndex+tryNb-numGames
                if(netAdd>bestNetAdd){
                    bestNetAdd=netAdd
                    bestNb=tryNb
                    bestPage=tryPage
                }
            }
            console.log(`fetching nb ${bestNb} page ${bestPage} net add ${bestNetAdd}`)
            lgs=new LichessGames(LICHESS_HANDLE,bestNb,bestPage)
            lgs.with_moves=1
            lgs.fetch(function(){
                let newgames=lgs.currentPageResults
                console.log(`loaded ${newgames.length} games`)
                let allids=games.map(game=>game.id)
                let totalpushed=0
                for(let newgame of newgames){
                    if(allids.indexOf(newgame.id)<0){
                        totalpushed++
                        console.log(`pushing id ${newgame.id} total ${totalpushed}`)
                        games.push(newgame)
                    }
                }
                console.log(`games updated , total games ${games.length}`)
                saveHandleJson()
                saveHandlePgn()
            })            
        }else{
            console.log(`no games were returned from fetch`)
        }
    })
}
