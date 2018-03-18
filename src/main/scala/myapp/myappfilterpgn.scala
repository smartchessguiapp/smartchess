package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppFilterPgn
{
	import MyApp._

	def SetSearchProfile(filterfilename:String,filterplayerwhite:String,filterplayerblack:String)
	{
		GetMyText("{filterfilename}").SetText(filterfilename)
		GetMyText("{filterplayerwhite}").SetText(filterplayerwhite)
		GetMyText("{filterplayerblack}").SetText(filterplayerblack)
	}

	var handlecolordone:Boolean=false

	def HandleColor(color:String){		
		if(color=="white") SetSearchProfile(PgnSync.bookName("white"),GetMyHandle,"")
		else SetSearchProfile(PgnSync.bookName("black"),"",GetMyHandle)
		handlecolordone=true
	}

	def FilterPGN
	{
		val blob=s"""
			|<vbox>	
			|				
			|<hbox gap="5" padding="5">
			|<label text="Input file name"/>
			|<filechooser id="{filterinputfilename}"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Output file name"/>
			|<textfield id="{filterfilename}"/>
			|<label text="Player white"/>
			|<textfield id="{filterplayerwhite}"/>
			|<label text="Player black"/>
			|<textfield id="{filterplayerblack}"/>
			|<button id="{filtersearch}" width="200.0" text="Search"/>
			|</hbox>
			|
			|<hbox>
			|<button id="{handlewhite}" text="Handle white"/>
			|<button id="{handleblack}" text="Handle black"/>
			|<label text="Ignore won"/>
			|<checkbox prefixget="variant" prefixset="variant" id="{ignorewon}"/>
			|<label text="Ignore lost"/>
			|<checkbox prefixget="variant" prefixset="variant" id="{ignorelost}"/>
			|<label text="Ignore draw"/>
			|<checkbox prefixget="variant" prefixset="variant" id="{ignoredraw}"/>
			|<label text="For all"/>
			|<checkbox prefixget="variant" prefixset="variant" id="{ignoreforall}"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Rating"/>
			|<slider delay="5" prefixget="variant" prefixset="variant" id="{filterrating}" value="2000.0" width="800.0" height="50.0" min="1000.0" max="3000.0" majortickunit="100.0" minortickcount="0" showticklabels="true" showtickmarks="true"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Max games"/>
			|<slider delay="5" prefixget="variant" prefixset="variant" id="{filtermaxgames}" value="1000.0" width="800.0" height="50.0" min="0.0" max="2000.0" majortickunit="100.0" minortickcount="0" showticklabels="true" showtickmarks="true"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Min plies"/>
			|<slider delay="5" prefixget="variant" prefixset="variant" id="{filterminplies}" value="5.0" width="800.0" height="50.0" min="0.0" max="50.0" majortickunit="5.0" minortickcount="4" showticklabels="true" showtickmarks="true"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Min time"/>
			|<slider delay="5" prefixget="variant" prefixset="variant" id="{filtermintime}" value="60.0" width="800.0" height="50.0" min="0.0" max="600.0" majortickunit="60.0" minortickcount="3" showticklabels="true" showtickmarks="true"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Max time"/>
			|<slider delay="5" prefixget="variant" prefixset="variant" id="{filtermaxtime}" value="300.0" width="800.0" height="50.0" min="0.0" max="1200.0" majortickunit="60.0" minortickcount="3" showticklabels="true" showtickmarks="true"/>
			|</hbox>
			|
			|<hbox gap="5" padding="5">
			|<label text="Date from YYYY.MM.DD"/>
			|<textfield id="{filterdatefrom}"/>			
			|<label text="Date to YYYY.MM.DD"/>
			|<textfield id="{filterdateto}"/>			
			|</hbox>
			|</vbox>
		""".stripMargin

		def filter_handler(ev:MyEvent)
		{
			val id=ev.Id
			val value=ev.value

			if(ev.kind=="button pressed")
			{
				if(id=="{filtersearch}")
				{
					SelectLogTab

					FilterSearch
				}

				if(id=="{handlewhite}")
				{
					HandleColor("white")
				}

				if(id=="{handleblack}")
				{
					HandleColor("black")
				}
			}

			if(ev.kind=="stage closed")
			{
				SelectBookTab
			}
		}		

		MyStage("{filterpgndialog}","Filter PGN",blob,
			modal=true,usewidth=false,useheight=false,handler=filter_handler)

		val defaultfilterfilename=GS(Cve("{defaultfilterfilename}"),"filtered")		
		val defaultfilterplayerwhite=GS(Cve("{defaultfilterplayerwhite}"),"")		
		val defaultfilterplayerblack=GS(Cve("{defaultfilterplayerblack}"),"")		
		val defaultfilterdatefrom=GS(Cve("{defaultfilterdatefrom}"),"2017.01.01")		
		val defaultfilterdateto=GS(Cve("{defaultfilterdateto}"),"")		

		SetSearchProfile(defaultfilterfilename,defaultfilterplayerwhite,defaultfilterplayerblack)
		GetMyText("{filterdatefrom}").SetText(defaultfilterdatefrom)
		GetMyText("{filterdateto}").SetText(defaultfilterdateto)
	}

	def FilterSearch
	{
		Future
		{
			FilterSearchFunc
		}
	}

	var filteraborted=false

	def FilterSearchFunc
	{
		filteraborted=false

		DataUtils.mkdirs(List("stuff",board.variant,"filtered"))

		val filterinputfilename=GS("{components}#{filterinputfilename}#{path}","")
		val filterinputfile=new java.io.File(filterinputfilename)

		if(!filterinputfile.exists) return

		val filename=GetMyText("{filterfilename}").GetText
		Set(Cve("{defaultfilterfilename}"),filename)
		val filterplayerwhite=GetMyText("{filterplayerwhite}").GetText
		Set(Cve("{defaultfilterplayerwhite}"),filterplayerwhite)
		val filterplayerblack=GetMyText("{filterplayerblack}").GetText
		Set(Cve("{defaultfilterplayerblack}"),filterplayerblack)
		val datefrom=GetMyText("{filterdatefrom}").GetText
		Set(Cve("{defaultfilterdatefrom}"),datefrom)
		val dateto=GetMyText("{filterdateto}").GetText
		Set(Cve("{defaultfilterdateto}"),dateto)
		val filterrating=GD(Cve("{filterrating}"),2000.0).toInt
		val filtermaxgames=GD(Cve("{filtermaxgames}"),1000.0).toInt
		val filterminplies=GD(Cve("{filterminplies}"),5.0).toInt
		val filtermintime=GD(Cve("{filtermintime}"),60.0).toInt
		val filtermaxtime=GD(Cve("{filtermaxtime}"),300.0).toInt
		val ignorelost=GB(Cve("{ignorelost}"),false)
		val ignorewon=GB(Cve("{ignorewon}"),false)
		val ignoredraw=GB(Cve("{ignoredraw}"),false)
		val ignoreforall=GB(Cve("{ignoreforall}"),false)
		val handle=GetMyHandle

		MyActor.Log(s"reading $filterinputfilename")
		
		val pgnfile=DataUtils.ReadFileToString(filterinputfilename)

		MyActor.Log("length "+pgnfile.length)

		MyActor.Log("splitting file")

		val dummy=new game
		val pgns=game.split_pgn(pgnfile)

		MyActor.Log("done")
		
		var total=0
		var count=0

		val collection=scala.collection.mutable.ArrayBuffer[String]()

		for(pgn <- pgns if(count < filtermaxgames))
		{
			if(filteraborted) return

			total+=1

			dummy.parse_pgn(pgn,head_only=true)

			val playerwhite=dummy.get_header("White")
			val playerblack=dummy.get_header("Black")
			val dofilterplayerwhite=( filterplayerwhite != "" )
			val dofilterplayerblack=( filterplayerblack != "" )
			val playerwhiteishandle=( playerwhite == handle )
			val playerblackishandle=( playerblack == handle )
			val playerwhitematches=( playerwhite == filterplayerwhite )
			val playerblackmatches=( playerblack == filterplayerblack )
			val playerwhiteok=( (!dofilterplayerwhite) || playerwhitematches )
			val playerblackok=( (!dofilterplayerblack) || playerblackmatches )
			var ignore=false
			val result=dummy.get_header("Result")
			val whitewon=( result == "1-0" )
			val blackwon=( result == "0-1" )
			val draw=( result == "1/2-1/2" )
			if( playerwhiteishandle && whitewon && ignorewon ) ignore=true
			if( playerwhiteishandle && blackwon && ignorelost ) ignore=true
			if( playerblackishandle && blackwon && ignorewon ) ignore=true
			if( playerblackishandle && whitewon && ignorelost ) ignore=true
			if( ( playerwhiteishandle || playerblackishandle ) && draw && ignoredraw ) ignore=true
			if( ignoreforall && whitewon && ignorewon ) ignore=true
			if( ignoreforall && blackwon && ignorelost ) ignore=true
			if( ignoreforall && draw && ignoredraw ) ignore=true			
			var whiterating=2000
			var blackrating=2000
			try{
				whiterating=dummy.get_header("WhiteElo").toInt
				blackrating=dummy.get_header("BlackElo").toInt
			}catch{case e:Throwable=>{}}
			var plycount=0
			try{
				plycount=dummy.get_header("PlyCount").toInt
			}catch{case e:Throwable=>{}}
			val plycountok=plycount>=filterminplies
			var time=60
			try{
				val timeh=dummy.get_header("TimeControl")
				val parts=timeh.split("\\+")
				time=parts(0).toInt
			}catch{case e:Throwable=>{}}
			val timeok=(time>=filtermintime)&&(time<=filtermaxtime)
			val rating=(whiterating+blackrating)/2.toInt
			val ratingok=rating>=filterrating
			val variant=dummy.get_header("Variant")
			val variantok=(variant==board.variant)
			val date=dummy.get_header("Date")
			val dateok=(SimpleDate(date).isLaterThan(SimpleDate(datefrom)))&&
				(SimpleDate(date).isEarlierThan(SimpleDate(dateto)))

			if(
				playerwhiteok &&
				playerblackok &&
				ratingok &&
				variantok &&
				plycountok &&
				timeok &&
				dateok &&
				(!ignore)
			)
			{
				count+=1
				collection+=pgn

				val readyp=(total.toDouble/pgns.length.toDouble*100.0).toInt

				MyActor.Log(readyp+" % "+variant+" "+playerwhite+" - "+playerblack+" "+count+" / "+total)
			}
		}

		MyActor.Log("done, found "+collection.length)

		val path="stuff/"+board.variant+"/filtered/"+filename+".pgn"

		DataUtils.WriteStringToFile(path,collection.mkString("\n\n"))
	}
}