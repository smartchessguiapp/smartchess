package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppRatingCharts
{
	import MyApp._

	val WIDTH=800
	val HEIGHT=450
	val LEFTMARGIN=40
	val CHARTWIDTH=WIDTH-LEFTMARGIN

	var lastcat=""

	case class DayData(		
		var first:Int=1500,
		var open:Int=1500,
		var high:Int=0,
		var low:Int=5000,
		var close:Int=1500,
		var empty:Boolean=true,
		var hasfirst:Boolean=false
	)
	{
		def getboxlow:Int = if(open < close) open else close
		def getboxhigh:Int = if(open > close) open else close
		def getcolor:String = if(close > open) "#00ff00" else "#ff0000"

		def add(rating:Int)
		{
			if(!hasfirst)
			{
				first=rating
				open=rating
				hasfirst=true
			}
			else
			{				
				close=rating				
				empty=false
			}
			if(rating < low) low=rating
			if(rating > high) high=rating
		}

		def toPrintable:String = s"hasfirst $hasfirst empty $empty f $first o $open h $high l $low c $close"
	}

	case class HistoryData(
		var dates:Map[String,DayData]=Map[String,DayData]()		
	)
	{
		var count:Int=0
		var corrected:Boolean=false

		def add(date:String,rating:Int)
		{
			if(!dates.contains(date))
			{
				dates+=(date->DayData())				
			}
			dates(date).add(rating)
			count+=1
		}

		def sortedkeys:List[String]=dates.keys.toList.sortWith((a,b) => SimpleDate(a).isEarlierThan(SimpleDate(b)))

		def toPrintable:String =
		{
			(for(date <- sortedkeys) yield
				s"$date ${dates(date).toPrintable}"
			).mkString("\n")
		}
	}

	case class CategoryData(
		var histories:Map[String,HistoryData]=Map[String,HistoryData]()		
	)
	{	
		var count:Int=0

		def add(category:String,date:String,rating:Int)
		{
			if(!histories.contains(category))
			{
				histories+=(category->HistoryData())
			}
			histories(category).add(date,rating)
			count+=1
		}

		def sortedkeys:List[String]=histories.keys.toList.sortWith((a,b) => histories(a).count > histories(b).count)

		def catkeys=sortedkeys.map(x => x+" ( "+histories(x).count+" )")
	}

	case class HandleData(
		var categories:Map[String,CategoryData]=null
	)
	{
		def reset
		{
			categories=Map[String,CategoryData]()
		}

		def add(handle:String,category:String,date:String,rating:Int)
		{
			if(!categories.contains(handle))
			{
				categories+=(handle -> CategoryData())				
			}
			categories(handle).add(category,date,rating)
		}

		def sortedkeys:List[String]=categories.keys.toList.sortWith((a,b) => categories(a).count > categories(b).count)
	}

	val handledata=HandleData()

	var categorydata:CategoryData=CategoryData()

	def CreateSvg(cat:String,dayslimit:String):String=
	{
		def normalize(what:Int,dir:Int):Int=
		{
			(what/100 + dir)*100
		}
		val h=categorydata.histories(cat)

		var hks=h.sortedkeys
		if(!h.corrected)
		{
			val lasti=hks.length-1
			for(i <- 0 to lasti)
			{		
				val date1=hks(i)		
				val d1=h.dates(date1)
				if(i > 0)
				{										
					val date0=hks(i-1)					
					val d0=h.dates(date0)										
					if(d1.hasfirst)
					{
						d0.add(d1.first)
					}
					if(i == lasti)
					{
						if(d1.empty) h.dates-=(date1)
					}
				}				
			}			
			h.corrected=true
			hks=h.sortedkeys
		}

		if(dayslimit!="ALL")
		{
			try{
				var nd=dayslimit.toInt
				if(nd>hks.length) nd=hks.length
				hks=hks.slice(hks.length-nd,hks.length)
			}catch{case e:Throwable=>{}}			
		}
		val games=h.count
		val days=hks.length
		var min=5000
		var max=0
		for(date <- hks)
		{
			val d=h.dates(date)
			if(d.high > max) max=d.high
			if(d.low < min) min=d.low
		}
		val cmin=normalize(min,0)
		val cmax=normalize(max,1)
		val crange=cmax-cmin
		val vunits=crange/100
		val widthr=CHARTWIDTH.toDouble/days.toDouble
		val heightr=HEIGHT.toDouble/crange.toDouble
		var barwidth=widthr.toInt-2
		if(barwidth<2) barwidth=2
		var i= -1
		def getcy(r:Int):Int = (((r-cmin).toDouble * heightr).toInt)
		val gridcontent=(for(i <- 1 to vunits-1) yield
		{
			val cy=getcy(cmin+i*100)
			val r=cmin+(vunits-i)*100
			s"""
				|<rect width="$WIDTH" x="0" y="$cy" height="1" style="fill:#0000ff;stroke-width:0;stroke:#000000;"/>				
				|<text x="0" y="${cy-3}" font-size="12" fill="blue">
				|$r
				|</text>
				|
			""".stripMargin
		}).mkString("\n")
		val svgbarscontent=(for(date <- hks) yield
		{
			i+=1
			val bcx=LEFTMARGIN+(i.toDouble*widthr).toInt+1
			val d=h.dates(date)
			var bcy=getcy(d.getboxlow)
			val bheight=getcy(d.getboxhigh)-bcy+1
			val bcolor=d.getcolor
			val ccx=bcx+barwidth/2
			var ccy=getcy(d.low)
			val cheight=getcy(d.high)-ccy+1			
			bcy=HEIGHT-bcy-bheight
			ccy=HEIGHT-ccy-cheight
			s"""
				|<rect width="1" x="$ccx" y="$ccy" height="$cheight" style="fill:#000000;stroke-width:1;stroke:#000000;"/>				
				|<rect id="rect$i" width="$barwidth" x="$bcx" y="$bcy" height="$bheight" style="fill:$bcolor;stroke-width:1;stroke:#000000;"/>				
				|<text id="thepopup" x="0" y="30" font-size="25" fill="black" visibility="hidden">
				|$date open ${d.open} high ${d.high} low ${d.low} close ${d.close}
    			|<set attributeName="visibility" from="hidden" to="visible" begin="rect$i.mouseover" end="rect$i.mouseout"/>
  				|</text>
			""".stripMargin
		}).mkString("\n")
		s"""
			|$cat, games: $games, days: $days, highest: $max, lowest: $min
			|<hr>
			|<svg width="$WIDTH" height="$HEIGHT">						
			|$gridcontent
			|$svgbarscontent
			|</svg>
		""".stripMargin
	}

	def UpdateChart(cat:String)
	{
		if(categorydata.histories.contains(cat))
		{
			lastcat=cat
			val dayslimit=GS("{components}#{dayscombo}#{selected}","ALL")
			MyActor.queuedExecutor ! ExecutionItem(client="UpdateChart",code=new Runnable{def run{
				val svg=CreateSvg(cat,dayslimit)					
				LoadWebContent("{chartswebview}",s"""					
					|$svg
				""".stripMargin)
			}})					
		}					
	}

	def CreateChartsForHandle(handle:String)
	{
		categorydata=handledata.categories(handle)

		val sks=categorydata.sortedkeys
		val cks=categorydata.catkeys

		MyActor.queuedExecutor ! ExecutionItem(client="CreateChartsForHandle",code=new Runnable{def run{					
			GetMyComboBox("{catscombo}").CreateFromItems(cks,cks(0))			
		}})

		UpdateChart(sks(0))		
	}

	def RatingCharts
	{
		val storeselectedmaintab=selectedmaintab

		val blob=s"""
			|<vbox width="${WIDTH+100}.0" height="${HEIGHT+200}.0">
			|<hbox gap="5" padding="5">
			|<label text="PGN file"/>
			|<filechooser id="{ratingchartspgnpath}"/>
			|</hbox>
			|<button id="{ratingchartscreate}" width="200.0" text="Create charts"/>
			|<hbox gap="5" padding="5">
			|<combobox id="{catscombo}"/>
			|<combobox id="{dayscombo}"/>
			|</hbox>
			|<webview id="{chartswebview}" width="${WIDTH+50}.0" height="${HEIGHT+75}.0"/>
			|</vbox>
		""".stripMargin

		var chartsaborted=false
		var path=""

		def CreateRatingChartsFunc
		{
			handledata.reset

			MyActor.Log(s"reading $path")
		
			val pgnfile=DataUtils.ReadFileToString(path)

			MyActor.Log("length "+pgnfile.length)

			MyActor.Log("splitting file")

			val pgns=game.split_pgn(pgnfile).reverse

			MyActor.Log("done , PGN has "+pgns.length+" games")
		
			var count=0			

			for(pgn <- pgns if((!chartsaborted)))
			{
				val dummy=new game

				dummy.parse_pgn(pgn,head_only=true)

				val playerwhite=dummy.get_header("White")
				val playerblack=dummy.get_header("Black")								
				var whiterating=2000
				var blackrating=2000
				try{
					whiterating=dummy.get_header("WhiteElo").toInt
					blackrating=dummy.get_header("BlackElo").toInt
				}catch{case e:Throwable=>{}}
				val date=dummy.get_header("Date")				
				val variant=dummy.get_header("Variant")
				val timecontrol=dummy.get_header("TimeControl")				
				var timesecs=300
				try{
					val tcparts=timecontrol.split("\\+")
					val minsecs=tcparts(0).toInt
					val incsecs=tcparts(1).toInt
					timesecs=minsecs+incsecs*40
				}catch{case e:Throwable=>{}}

				var category=variant

				if(variant=="Standard")
				{
					if(timesecs<=15) category="Ultra Bullet"
					else if(timesecs<180) category="Bullet"
					else if(timesecs<480) category="Blitz"
					else category="Classical"
				}

				handledata.add(playerwhite,category,date,whiterating)
				handledata.add(playerblack,category,date,blackrating)

				count+=1

				if((count%500)==0) MyActor.Log("processing PGN %.2f".
					format(count.toDouble / pgns.length.toDouble * 100.0)+" % complete")
			}

			println("")

			val sk=handledata.sortedkeys

			if(sk.length > 0)
			{
				val handle=sk(0)

				MyActor.Log("selected handle: "+handle)

				CreateChartsForHandle(handle)
			}

			Builder.CloseAbortDialog			
		}

		def ratingcharts_handler(ev:MyEvent)
		{
			val id=ev.Id
			val value=ev.value

			if(ev.kind=="button pressed")
			{
				if(id=="{ratingchartscreate}")
				{
					path=GS("{components}#{ratingchartspgnpath}#{path}","")

					if(!new java.io.File(path).exists())
					{
						Builder.SystemPopUp("Rating charts error","""<font color="red"><b>PGN file does not exist.</b></font>""")
					}
					else
					{
						SelectLogTab

						MyActor.Log("creating charts")

						chartsaborted=false
						Builder.AbortDialog(title="Abort creating charts",callback=()=>{chartsaborted=true})

						Future
						{
							CreateRatingChartsFunc
						}
					}
				}
			}

			if(ev.kind=="combobox selected")
			{
				if(id=="{catscombo}")
				{
					val parts=ev.value.split("\\(")
					val cat=parts(0).replaceAll("\\s*$","")

					UpdateChart(cat)
				}

				if(id=="{dayscombo}")
				{
					UpdateChart(lastcat)
				}
			}

			if(ev.kind=="stage closed")
			{
				SelectMainTab(storeselectedmaintab)
			}
		}		

		MyStage("{ratingchartsdialog}","Rating charts",blob,
			modal=true,usewidth=false,useheight=false,handler=ratingcharts_handler)

		GetMyComboBox("{catscombo}").CreateFromItems(List[String](),"")
		GetMyComboBox("{dayscombo}").CreateFromItems(List[String]("ALL","100","50","20"),"ALL")
	}
}