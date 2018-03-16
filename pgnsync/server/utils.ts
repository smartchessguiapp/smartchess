function createDir(path:string):boolean{
    try{
        fs.mkdirSync(path)
        return true
    }catch(err){
        logErr(err)
        return false
    }
}

function fileExists(path:string):boolean{
    try{
        fs.accessSync(path)
        return true
    }catch(err){
        logErr(err)
        return false
    }
}

function readTextFile(path:string):string{
    try{
        let content=fs.readFileSync(path)
        return content
    }catch(err){
        logErr(err)
        return ""
    }
}

function writeTextFile(path:string, content:string):boolean{
    console.log(`writing text file <${path}> content ${content.length}`)
    try{
        fs.writeFileSync(path,content)
        return true
    }catch(err){
        logErr(err)
        return false
    }
}
