class Perf{
    games:number=0
    rating:number=1500
    rd:number=350
    prog:number=0

    fromJson(json:any):Perf{
        //console.log(`creating perf from json`,json)
        if(json.games!=undefined) this.games=json.games
        if(json.rating!=undefined) this.rating=json.rating
        if(json.rd!=undefined) this.rd=json.rd
        if(json.prog!=undefined) this.prog=json.prog
        return this
    }
}

class LichessProfile{
    username:string=""

    invalid:boolean=true

    nbFollowers:number=0
    createdAt:number=new Date().getTime()
    createdAtF:string=this.createdAt.toLocaleString()
    perfs:{[id:string]:Perf}={}

    constructor(username:string){
        this.username=username
    }

    fromJson(json:any):LichessProfile{
        try{
            //console.log(`creating profile for ${this.username} from json`,json)
            this.invalid=true
            if(json==undefined) return this            
            if(json.nbFollowers!=undefined) this.nbFollowers=json.nbFollowers
            if(json.createdAt!=undefined) this.createdAt=json.createdAt
            this.createdAtF=new Date(this.createdAt).toLocaleString()
            if(json.perfs!=undefined){
                for(let variant in json.perfs){
                    let perfJson=json.perfs[variant]
                    this.perfs[variant]=new Perf().fromJson(perfJson)
                }
            }
            this.invalid=false
            return this
        }catch(err){
            logErr(err)
        }
        return this
    }

    fetch(callback:any){
        this.invalid=true

        try{
            fetch_(`https://lichess.org/api/user/${this.username}`).then(
                (response:any)=>{
                    response.text().then(
                        (content:any)=>{
                            try{
                                let json=JSON.parse(content)
                                this.fromJson(json)
                                callback(this)
                            }catch(err){
                                logErr(err)
                                callback(this)
                            }
                        }
                    )
                },
                (err:any)=>{
                    logErr(err)
                    callback(this)
                }
            )
        }catch(err){
            logErr(err)
        }
    }

    asDiscordString():string{
        let content=
            `__lichess profile__ of **${this.username}**\n\n`+
            `__member since__ : **${this.createdAtF}**\n`+
            `__followers__ : **${this.nbFollowers}**\n\n`+
            `__perfs__ :\n\n`

        let perfsContent:string=Object.keys(this.perfs).map((variant:string)=>{
            let perf=this.perfs[variant]
            if(perf.games<=0) return ""
            let vpref=variant==DEFAULT_VARIANT?"**":"*"
            return `${vpref}${variant}${vpref} : rating : **${perf.rating}** , games : __${perf.games}__ , rd : ${perf.rd} , progress : ${perf.prog}\n`
        }).join("")

        content+=perfsContent

        return content
    }
}

class LichessClock{
    initial:number=180
    increment:number=0
    totalTime:number=180

    fromJson(json:any):LichessClock{
        if(json==undefined) return this

        if(json.initial!=undefined) this.initial=json.initial
        if(json.increment!=undefined) this.increment=json.increment
        if(json.totalTime!=undefined) this.totalTime=json.totalTime

        return this
    }
}

class LichessPlayer{
    userId:string=""
    rating:number=1500
    ratingDiff:number=0

    userIdLower():string{
        return this.userId.toLowerCase()
    }

    fromJson(json:any):LichessPlayer{
        if(json==undefined) return this

        if(json.userId!=undefined) this.userId=json.userId
        if(json.rating!=undefined) this.rating=json.rating
        if(json.ratingDiff!=undefined) this.ratingDiff=json.ratingDiff

        return this
    }
}

class LichessPlayers{
    white:LichessPlayer=new LichessPlayer()
    black:LichessPlayer=new LichessPlayer()

    fromJson(json:any):LichessPlayers{
        if(json==undefined) return this

        if(json.white!=undefined) this.white.fromJson(json.white)
        if(json.black!=undefined) this.black.fromJson(json.black)

        return this
    }
}

class LichessGame{
    id:string=""
    rated:boolean=true
    variant:string=DEFAULT_VARIANT
    speed:string="blitz"
    perf:string=DEFAULT_VARIANT
    createdAt:number=new Date().getTime()
    createdAtF:string=new Date(this.createdAt).toLocaleString()
    lastMoveAt:number=new Date().getTime()
    lastMoveAtF:string=new Date(this.lastMoveAt).toLocaleString()
    turns:number=0
    color:string="white"
    status:string="resign"
    clock:LichessClock=new LichessClock()
    players:LichessPlayers=new LichessPlayers()
    winner:string=""
    url:string=""

    isUserWhite(userId:string):boolean{
        return this.players.white.userIdLower()==userId.toLowerCase()
    }

    isUserBlack(userId:string):boolean{
        return this.players.black.userIdLower()==userId.toLowerCase()
    }

    resultF():string{        
        if(this.winner=="white") return "1-0"
        if(this.winner=="black") return "0-1"
        return "1/2-1/2"
    }

    result():number{
        if(this.winner=="white") return 1
        if(this.winner=="black") return 0
        return 0.5
    }

    resultForUser(userId:string):number{
        let result=this.result()                
        if(this.isUserWhite(userId)) return result
        if(this.isUserBlack(userId)) return 1-result
        return result
    }

    ratingForUser(userId:string):number{
        if(this.isUserWhite(userId)) return this.players.white.rating
        if(this.isUserBlack(userId)) return this.players.black.rating
        return 1500
    }

    ratingDiffForUser(userId:string):number{
        if(this.isUserWhite(userId)) return this.players.white.ratingDiff
        if(this.isUserBlack(userId)) return this.players.black.ratingDiff
        return 0
    }

    fromJson(json:any):LichessGame{
        if(json==undefined) return this

        if(json.id!=undefined) this.id=json.id
        if(json.rated!=undefined) this.rated=json.rated
        if(json.variant!=undefined) this.variant=json.variant
        if(json.speed!=undefined) this.speed=json.speed
        if(json.perf!=undefined) this.perf=json.perf
        if(json.createdAt!=undefined) this.createdAt=json.createdAt
        this.createdAtF=new Date(this.createdAt).toLocaleString()
        if(json.lastMoveAt!=undefined) this.lastMoveAt=json.lastMoveAt
        this.lastMoveAtF=new Date(this.lastMoveAt).toLocaleString()
        if(json.turns!=undefined) this.turns=json.turns
        if(json.color!=undefined) this.color=json.color
        if(json.status!=undefined) this.status=json.status
        if(json.clock!=undefined) this.clock=new LichessClock().fromJson(json.clock)
        if(json.players!=undefined) this.players=new LichessPlayers().fromJson(json.players)
        this.winner=""
        if(json.winner!=undefined) this.winner=json.winner
        if(json.url!=undefined) this.url=json.url

        return this
    }

    shortUrl():string{
        return this.url.replace(/\/white$|\/black$/,"")
    }

    asDiscordStringForUser(userId:string):string{
        let prefWhite=this.isUserWhite(userId)?"**":""
        let prefBlack=this.isUserBlack(userId)?"**":""

        return `${prefWhite}${this.players.white.userId}${prefWhite} ( ${this.players.white.rating} ) - ${prefBlack}${this.players.black.userId}${prefBlack} ( ${this.players.black.rating} )  **${this.resultF()}**  *${this.lastMoveAtF}*  <${this.shortUrl()}>`
    }
}

class LichessGames{
    username:string=""

    invalid:boolean=true

    // request
    nb:number=10
    page:number=1
    with_analysis:number=0
    with_moves:number=0
    with_opening:number=0
    with_movetimes:number=0
    rated:number=1
    playing:number=0

    // response
    currentPage:number=1
    maxPerPage:number=this.nb
    currentPageResults:any=[]
    nbResults:number=0
    previousPage:any=null
    nextPage:any=null
    nbPages:number=0

    // games
    games:LichessGame[]=[]

    constructor(username:string,nb:number,page:number){
        this.username=username
        this.nb=nb
        this.page=page
    }

    fetch(callback:any){
        this.invalid=true

        try{
            let url=`https://lichess.org/api/user/${this.username}/games?nb=${this.nb}&page=${this.page}&with_analysis=${this.with_analysis}&with_moves=${this.with_moves}&with_opening=${this.with_opening}&with_movetimes=${this.with_movetimes}&rated=${this.rated}&playing=${this.playing}`
            console.log(`fetching url ${url}`)
            fetch_(url).then(
                (response:any)=>{
                    response.text().then(
                        (content:any)=>{
                            try{
                                let json=JSON.parse(content)
                                this.fromJson(json)
                                callback(this)
                            }catch(err){
                                logErr(err)
                                callback(this)
                            }
                        }
                    )
                },
                (err:any)=>{
                    logErr(err)
                    callback(this)
                }
            )
        }catch(err){
            logErr(err)
        }
    }

    fromJson(json:any):LichessGames{
        try{
            //console.log(`creating lichess games for ${this.username} nb ${this.nb} page ${this.page} from json`,json)
            
            this.invalid=true

            if(json==undefined) return this      
            
            if(json.maxPerPage!=undefined) this.maxPerPage=json.maxPerPage            
            this.currentPageResults=json.currentPageResults
            if(json.nbResults!=undefined) this.nbResults=json.nbResults
            if(json.previousPage!=undefined) this.previousPage=json.previousPage
            if(json.nextPage!=undefined) this.nextPage=json.nextPage
            if(json.nbPages!=undefined) this.nbPages=json.nbPages

            this.games=this.currentPageResults.map((gameJson:any)=>new LichessGame().fromJson(gameJson))
            
            this.invalid=false

            return this
        }catch(err){
            logErr(err)
        }
        return this
    }

    wins:number=0
    losses:number=0
    draws:number=0
    minRating:number=1500
    maxRating:number=1500
    avgRating:number=1500

    numGames:number=0

    createStats(variant:string){
        this.wins=0
        this.losses=0
        this.draws=0
        this.minRating=4000
        this.maxRating=0

        this.numGames=0

        let cumRating=0

        for(let game of this.games){
            if(game.variant==variant){
                let resultForUser=game.resultForUser(this.username)                
                if(resultForUser==1) this.wins++
                else if(resultForUser==0) this.losses++
                else this.draws++

                let rating=game.ratingForUser(this.username)

                if(rating<this.minRating) this.minRating=rating
                if(rating>this.maxRating) this.maxRating=rating

                cumRating+=rating

                this.numGames++
            }            
        }

        if(this.numGames>0){
            this.avgRating=cumRating/this.numGames
        }
    }

    statsAsDiscordString(variant:string,showing:number=10):string{
        this.createStats(variant)

        if(this.numGames<10) showing=this.numGames

        let content=
            `out of last ${this.nb} lichess games **${this.username}** played **${this.numGames}** ${variant} games\n`+
            `won **${this.wins}** game(s), lost **${this.losses}** game(s), drawn **${this.draws}** game(s)\n`+
            `min rating **${this.minRating}** , max rating **${this.maxRating}** , average rating **${this.avgRating.toLocaleString()}**\n`+
            `showing last ${showing} games\n\n`

        let gamesContent=this.games.filter((game:LichessGame)=>game.variant==variant).slice(0,showing).map((game:LichessGame)=>game.asDiscordStringForUser(this.username)).join("\n")

        content+=gamesContent

        return content
    }

    ratingData(variant:string):number[]{
        let filtered=this.games.filter((game:LichessGame)=>game.variant==variant)
        let ratings=filtered.map((game:LichessGame)=>game.ratingForUser(this.username))        
        if(filtered.length>0){
            let lastgame=filtered[0]
            let lastrating=lastgame.ratingForUser(this.username)
            let lastratingdiff=lastgame.ratingDiffForUser(this.username)            
            ratings.unshift(lastrating+lastratingdiff)
        }        
        return ratings
    }
}