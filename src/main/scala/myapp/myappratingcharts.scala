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

	case class DayData(		
		var open:Int=1500,
		var high:Int=0,
		var low:Int=5000,
		var close:Int=1500,
		var empty:Boolean=true
	)
	{
		def add(rating:Int)
		{
			if(empty)
			{
				open=rating
				empty=false
			}
			close=rating
			if(rating < low) low=rating
			if(rating > high) high=rating
		}
	}

	case class HistoryData(
		var dates:Map[String,DayData]=Map[String,DayData]()		
	)
	{
		var count:Int=0

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

	def UpdateChart(cat:String)
	{
		if(categorydata.histories.contains(cat))
		{
			MyActor.queuedExecutor ! ExecutionItem(client="UpdateChart",code=new Runnable{def run{					
				println(categorydata.histories(cat))
			}})					
		}					
	}

	def CreateChartsForHandle(handle:String)
	{
		categorydata=handledata.categories(handle)

		val cks=categorydata.catkeys
		val sel=cks(0)

		MyActor.queuedExecutor ! ExecutionItem(client="CreateChartsForHandle",code=new Runnable{def run{					
			GetMyComboBox("{catscombo}").CreateFromItems(cks,sel)
			UpdateChart(sel)
		}})		
	}

	def RatingCharts
	{
		val storeselectedmaintab=selectedmaintab

		val blob=s"""
			|<vbox width="800.0">
			|<hbox gap="5" padding="5">
			|<label text="PGN file"/>
			|<filechooser id="{ratingchartspgnpath}"/>
			|</hbox>
			|<button id="{ratingchartscreate}" width="200.0" text="Create charts"/>
			|<hbox gap="5" padding="5">
			|<combobox id="{catscombo}"/>
			|</hbox>			
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
					else if(timesecs<=180) category="Bullet"
					else if(timesecs<=480) category="Blitz"
					else category="Classical"
				}

				handledata.add(playerwhite,category,date,whiterating)
				handledata.add(playerblack,category,date,blackrating)

				count+=1

				if((count%500)==0) MyActor.Log("processing PGN %.2f".
					format(count.toDouble / pgns.length.toDouble * 100.0)+" % complete")
			}

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
			}

			if(ev.kind=="stage closed")
			{
				SelectMainTab(storeselectedmaintab)
			}
		}		

		MyStage("{ratingchartsdialog}","Rating charts",blob,
			modal=true,usewidth=false,useheight=false,handler=ratingcharts_handler)
	}
}