"use strict";

// system
const fetch_ = require('node-fetch')
const schedule = require('node-schedule')

const DEFAULT_VARIANT = "atomic"

const ALL_VARIANTS=[
    "bullet",
    "blitz",
    "rapid",
    "classical",
    "ultraBullet",
    "crazyhouse",
    "chess960",
    "kingOfTheHill",
    "threeCheck",
    "antichess",
    "atomic",
    "horde",
    "racingKings"
]

const VARIANT_DISPLAY_NAMES:{[id:string]:string}={
    bullet:"Bullet",
    blitz:"Blitz",
    rapid:"Rapid",
    classical:"Classical",
    ultraBullet:"Ultra Bullet",
    crazyhouse:"Crazyhouse",
    chess960:"Chess960",
    kingOfTheHill:"King of the Hill",
    threeCheck:"Three Check",
    antichess:"Antichess",
    atomic:"Atomic",
    horde:"Horde",
    racingKings:"Racing Kings"
}

function logErr(err:any){
    console.log("***")
    let errContent=(""+err)
    let lines=errContent.split(/[\n\r]+/)
    console.log(lines.join(" \\ "))
    console.log("***")
}

