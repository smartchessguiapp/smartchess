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
			|</vbox>
		""".stripMargin

		var chartsaborted=false

		def CreateRatingChartsFunc
		{
			def UpdateRatingCharts
			{
				Builder.CloseAbortDialog
				MyActor.queuedExecutor ! ExecutionItem(client="CreateRatingChartsFunc.UpdateRatingCharts",code=new Runnable{def run{					
					
				}})
			}			

			while(!chartsaborted)
			{
				try{Thread.sleep(1000)}catch{case e:Throwable=>}

				MyActor.Log("creating charts")
			}

			UpdateRatingCharts
		}

		def ratingcharts_handler(ev:MyEvent)
		{
			val id=ev.Id
			val value=ev.value

			if(ev.kind=="button pressed")
			{
				if(id=="{ratingchartscreate}")
				{
					val path=GS("{components}#{ratingchartspgnpath}#{path}","")

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

			if(ev.kind=="stage closed")
			{
				SelectMainTab(storeselectedmaintab)
			}
		}		

		MyStage("{ratingchartsdialog}","Rating charts",blob,
			modal=true,usewidth=false,useheight=false,handler=ratingcharts_handler)
	}
}