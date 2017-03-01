package smartchess

////////////////////////////////////////////////////////////////////

import javafx.application._
import javafx.stage._
import javafx.scene._
import javafx.scene.layout._
import javafx.scene.control._
import javafx.scene.canvas._
import javafx.scene.input._
import javafx.scene.paint._
import javafx.scene.text._
import javafx.scene.web._
import javafx.scene.image._
import javafx.event._
import javafx.geometry._
import javafx.beans.value._
import javafx.collections._

import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._

import collection.JavaConverters._

import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

////////////////////////////////////////////////////////////////////

case class Tokenizer(line:String="")
{
	var tokens=Tokenizer.Tokens(line)

	def Get:String=
	{
		if(tokens.length>0)
		{
			val token=tokens.head
			tokens=tokens.tail
			return token
		}
		return null
	}

	def Poll:String=
	{
		if(tokens.length>0)
		{
			val token=tokens.head
			return token
		}
		return null
	}

	def GetRest:String=
	{
		if(tokens.length>0)
		{
			val str=tokens.mkString(" ")
			tokens=List[String]()
			return str
		}
		return null
	}

	def HasToken:Boolean=
	{
		tokens.length>0
	}
}

object Tokenizer
{
	def Tokens(line:String):List[String]=
	{
		Strip(line).split(" ").toList
	}

	def Strip(line:String):String=
	{
		var sline=line
		sline=sline.replaceAll("\\r|\\n|^\\s+|\\s+$","")
		sline=sline.replaceAll("\\s+"," ")
		sline
	}
}

object PvItem
{
	var SHORTPVSAN_MAXLENGTH=5
}

case class PvItem(	
	var multipv:Int=0,
	var hasmultipv:Boolean=false,
	var depth:Int=0,
	var hasdepth:Boolean=false,
	var nodes:Int=0,
	var nodesverbal:String="",
	var hasnodes:Boolean=false,
	var time:Int=0,
	var hastime:Boolean=false,
	var nps:Int=0,
	var npsverbal:String="",
	var hasnps:Boolean=false,
	var scorestr:String="",
	var scorekind:String="",
	var scorecp:Int=0,
	var scoremate:Int=0,
	var scorenumerical:Int=0,
	var signedscorenumerical:String="",
	var scoreverbal:String="",
	var hasscore:Boolean=false,
	var pv:String="",
	var pvrest:List[String]=List[String](),
	var pvreststr:String="",
	var haspv:Boolean=false,
	var bestmove:String="",
	var bestmovesan:String="",
	var pvsan:String="",
	var shortpvsan:String="",
	var protocol:String="uci"
)
{
	import PvItem._

	def AsString:String=
	{
		s"$bestmove $signedscorenumerical depth $depth nodes $nodes nps $nps pv $pvreststr"
	}
	def FormatNodes(nodes:Int,unit:Int):String=
	{
		"%.2f".format(nodes.toDouble/unit.toDouble)
	}
	def GetNodesVerbal(nodes:Int):String=
	{
		if(nodes< 1000) return ""+nodes else
		if(nodes< 1000000) return ""+FormatNodes(nodes,1000)+" kN" else return FormatNodes(nodes,1000000)+" MN"
	}
	def GetNpsVerbal(nodes:Int):String=
	{
		if(nps< 1000) return ""+nps else
		if(nps< 1000000) return ""+FormatNodes(nps,1000)+" kN/s" else return FormatNodes(nps,1000000)+" MN/s"
	}

	def pvToSan(pv:String)
	{
		val dummy=new game

		dummy.set_from_fen(EngineManager.GetRootFen)

		val algebparts=pv.split(" ")

		bestmovesan=""
		shortpvsan=""

		var i=0
		for(uci<-algebparts)
		{
			val algeb=dummy.b.to_chess960_algeb(uci)
			if(dummy.b.isAlgebLegal(algeb))
			{
				val m=move(fromalgeb=algeb)
				if(i==0)
				{
					bestmovesan=dummy.b.toSan(m)
				}
				dummy.makeMove(m)
				i+=1
				if(i==SHORTPVSAN_MAXLENGTH) shortpvsan=dummy.current_line_pgn
				if(i==(SHORTPVSAN_MAXLENGTH+1)) shortpvsan+=" ..."
			}
		}

		pvsan=dummy.current_line_pgn
		if(shortpvsan=="") shortpvsan=pvsan
	}

	def ParseLine(line:String):PvItem=
	{
		val tokenizer=Tokenizer(line)
		if(protocol=="uci")
		{
			if(!tokenizer.HasToken) return this
			if(tokenizer.Get!="info") return this
			while(tokenizer.HasToken)
			{
				val name=tokenizer.Get
				if(name=="multipv")
				{
					multipv=DataUtils.ParseInt(tokenizer.Get,multipv)
					hasmultipv=true
				}
				if(name=="score")
				{
					val kind=tokenizer.Get
					val value=DataUtils.ParseInt(tokenizer.Get,if(kind=="mate") scoremate else scorecp)
					scorestr=kind+" "+value
					if(kind=="mate")
					{
						scoremate=value
						if(value>=0)
						{
							scorenumerical=10000-value
						} else {
							scorenumerical= -10000-value
						}
					} else {
						scorecp=value
						scorenumerical=value
					}
					signedscorenumerical=if(scorenumerical>0) "+"+scorenumerical else ""+scorenumerical
					scoreverbal=if(kind=="mate") "mate "+scoremate else signedscorenumerical
					scorekind=kind
					hasscore=true
				}
				if(name=="depth")
				{
					depth=DataUtils.ParseInt(tokenizer.Get,depth)
					hasdepth=true
				}
				if(name=="nodes")
				{
					nodes=DataUtils.ParseInt(tokenizer.Get,nodes)
					nodesverbal=GetNodesVerbal(nodes)
					hasnodes=true
				}
				if(name=="nps")
				{
					nps=DataUtils.ParseInt(tokenizer.Get,nps)
					npsverbal=GetNodesVerbal(nps)
					hasnps=true
				}
				if(name=="time")
				{
					time=DataUtils.ParseInt(tokenizer.Get,time)
					hastime=true
				}
				if(name=="pv")
				{
					pv=tokenizer.GetRest
					if(pv!=null)
					{
						haspv=true
						val pvtokens=Tokenizer.Tokens(pv)
						bestmove=pvtokens.head
						pvrest=pvtokens.tail
						pvreststr=pvrest.mkString(" ")
						pvToSan(pv)
					}
				}
			}
		}
		if(protocol=="xboard")
		{
			val parts=line.split(" ").toList
			val len=parts.length
			if(len< 5) return this
			if(DataUtils.IsInt(parts(0))&&DataUtils.IsInt(parts(1))&&DataUtils.IsInt(parts(2))&&DataUtils.IsInt(parts(3)))
			{
				depth=DataUtils.ParseInt(parts(0),depth)
				hasdepth=true

				scorenumerical=DataUtils.ParseInt(parts(1),depth)
				signedscorenumerical=if(scorenumerical>0) "+"+scorenumerical else ""+scorenumerical
				scoreverbal=if(scorenumerical>0) "+"+scorenumerical else ""+scorenumerical
				hasscore=true

				time=DataUtils.ParseInt(parts(2),time)*10
				hastime=true

				nodes=DataUtils.ParseInt(parts(3),nodes)
				nodesverbal=GetNodesVerbal(nodes)
				hasnodes=true

				tokenizer.Get
				tokenizer.Get
				tokenizer.Get
				tokenizer.Get
				pv=tokenizer.GetRest
				haspv=true
				val pvtokens=Tokenizer.Tokens(pv)
				bestmove=pvtokens.head
				pvrest=pvtokens.tail
				pvreststr=pvrest.mkString(" ")
				pvToSan(pv)
			}
		}
		return this
	}

	def UpdateWith(ui:PvItem):PvItem=
	{
		if(ui.hasmultipv) multipv=ui.multipv ; hasmultipv=true
		if(ui.hasdepth) depth=ui.depth ; hasdepth=true
		if(ui.hasnodes) nodes=ui.nodes ; hasnodes=true
		if(ui.hastime) time=ui.time ; hastime=true
		if(ui.hasnps) nps=ui.nps ; hasnps=true
		if(ui.haspv)
		{
			pv=ui.pv
			pvrest=ui.pvrest
			pvreststr=ui.pvreststr
			bestmove=ui.bestmove
			bestmovesan=ui.bestmovesan
			pvsan=ui.pvsan
			shortpvsan=ui.shortpvsan
			haspv=true
		}
		if(ui.hasscore)
		{
			scorestr=ui.scorestr
			scorekind=ui.scorekind
			scorecp=ui.scorecp
			scoremate=ui.scoremate
			scorenumerical=ui.scorenumerical
			signedscorenumerical=ui.signedscorenumerical
			scoreverbal=ui.scoreverbal
			nodesverbal=ui.nodesverbal
			npsverbal=ui.npsverbal
			hasscore=true
		}
		return this
	}

	def ReportHTMLTableRow:String=
	{
		val scorecolor=if(scorenumerical>=0) "#007f00" else "#7f0000"
		val timeformatted=org.apache.commons.lang.time.DurationFormatUtils.formatDuration(time,"mm:ss")
		s"""
		|<tr>
		|<td><font color="blue"><b>$bestmovesan</b></font></td>
		|<td><font color="$scorecolor"><b>$scoreverbal</b></font></td>
		|<td><font color="blue"><b>$depth</b></font></td>
		|<td><font color="#007f7f"><small>$timeformatted<small></font></td>
		|<td><small>$nodesverbal</small></td>
		|<td><small>$npsverbal</small></td>
		|<td><font color="#0000af"><small>$shortpvsan</small></font></td>
		|</tr>
		""".stripMargin
	}
}

case class DepthItem(depth:Int=1,protocol:String="uci")
{
	var maxmultipv=1
	var pvitems=Map[Int,PvItem]()

	def SortedMultipvs:List[Int]=pvitems.keys.toList.sorted

	def ExtremePv(lowest:Boolean=true):PvItem=
	{
		val sortedmultipvs=SortedMultipvs.filter(x => pvitems(x).haspv)
		if(sortedmultipvs.length>0)
		{
			if(lowest)
			{
				return pvitems(sortedmultipvs(0))
			} else {
				return pvitems(sortedmultipvs(sortedmultipvs.length-1))
			}
		} else {
			return PvItem(protocol=protocol)
		}
	}

	def ParseLine(line:String,parent:ThinkingOutput=null)
	{
		val pvitem=PvItem(protocol=protocol).ParseLine(line)
		if(pvitem.haspv)
		{
			val multipv=if(pvitem.hasmultipv) pvitem.multipv else { maxmultipv+=1 ; maxmultipv }

			if(!pvitems.contains(multipv)) pvitems+=(multipv->PvItem(protocol=protocol))

			pvitems+=(multipv->pvitems(multipv).UpdateWith(pvitem))

			if(parent!=null)
			{
				parent.access_bestmove(update=true,newbestmove=pvitem.bestmove)
			}
		}
		if(pvitem.hasscore)
		{
			if(parent!=null)
			{
				parent.access_scorenumerical(update=true,newscorenumerical=pvitem.scorenumerical)
			}
		}
	}

	def ReportHTML:String=
	{
		val multipvs=SortedMultipvs
		var haspv=false
		val multipvscontent=(for(multipv<-multipvs) yield
		{
			if(pvitems(multipv).haspv) haspv=true
			pvitems(multipv).ReportHTMLTableRow
		}).mkString("\n")
		if(!haspv) return ""
		s"""
			|<tr style="font-size: 12px;">
			|<td width="40">Move</td>
			|<td width="40">Score</td>
			|<td width="20">Dpt</td>
			|<td width="27">Time</td>
			|<td width="60">Nodes</td>
			|<td width="60">Nps</td>
			|<td>Pv</td>
			|</tr>
			|$multipvscontent
			|<tr>
			|<td colspan="7"><hr></td>
			|</tr>
		""".stripMargin
	}
}

case class ThinkingOutput(protocol:String="uci")
{
	var maxdepth=1
	var depthitems=Map[Int,DepthItem]()

	var bestmove:String=null
	var scorenumerical:Int=0

	def access_maxdepth(update:Boolean=false,newmaxdepth:Int=1):Int=
	{
		this.synchronized
		{
			if(update) maxdepth=newmaxdepth
			maxdepth
		}
	}

	def access_bestmove(update:Boolean=false,newbestmove:String=null):String=
	{
		this.synchronized
		{
			if(update) bestmove=newbestmove
			bestmove
		}
	}

	def access_scorenumerical(update:Boolean=false,newscorenumerical:Int=0):Int=
	{
		this.synchronized
		{
			if(update) scorenumerical=newscorenumerical
			scorenumerical
		}
	}

	def ParseLine(line:String)
	{
		val pvitem:PvItem = PvItem(protocol=protocol).ParseLine(line)
		val depth=if(pvitem.hasdepth) pvitem.depth else maxdepth
		if(depth>maxdepth) access_maxdepth(update=true,newmaxdepth=depth)
		if(!depthitems.contains(depth)) depthitems+=(depth->DepthItem(depth,protocol=protocol))
		depthitems(depth).ParseLine(line,this)
	}

	def SortedDepths:List[Int]=depthitems.keys.toList.sorted.reverse

	def HighestDepthItem:DepthItem=
	{
		val sorteddepths=SortedDepths
		val len=sorteddepths.length
		for(i<- 0 to (len-1)) if(depthitems(sorteddepths(i)).ExtremePv().haspv) return depthitems(sorteddepths(i))
		DepthItem(protocol=protocol)
	}

	def ExtremePv(lowest:Boolean=true):PvItem=
	{
		HighestDepthItem.ExtremePv(lowest)
	}

	def ReportHTML:String=
	{
		val depths=SortedDepths
		val depthscontent=(for(depth<-depths) yield depthitems(depth).ReportHTML).mkString("\n")
		s"""
			|<table cellpadding="3" cellspacing="3">
			|$depthscontent
			|<table>
		""".stripMargin
	}
}

case class Option(	
	var name:String="",
	var kind:String="",
	var minstr:String="",
	var maxstr:String="",
	var defaultstr:String="",
	var send:Boolean=true,
	var subkind:String="",
	var vars:List[String]=List[String](),
	var protocol:String="uci",
	var uniqueid:Int= -1
)
{
	def dosend:Boolean=
	{
		if(kind=="separator") return false
		send
	}

	def ParseLine(line:String):Option=
	{
		val tokenizer=Tokenizer(line)
		val head=tokenizer.Poll
		if(head==null) return null

		if(protocol=="uci")
		{
			if(tokenizer.Get!="option") return null

			val reservedtokens=List("name","type","min","max","default","var")

			def IsReserved(token:String)=reservedtokens.contains(token)

			while(tokenizer.HasToken)
			{
				var currenttoken=tokenizer.Get

				if(!IsReserved(currenttoken))
				{
					if((name=="")||(kind=="")) return null

					return this
				}

				var fieldbuff=List[String]()

				while(tokenizer.HasToken&&(!IsReserved(tokenizer.Poll)))
				{
					fieldbuff=fieldbuff:+tokenizer.Get
				}

				val field=fieldbuff.mkString(" ")

				if(currenttoken=="name") name=field
				if(currenttoken=="type") kind=field
				if(currenttoken=="min") minstr=field
				if(currenttoken=="max") maxstr=field
				if(currenttoken=="default") defaultstr=field
				if(currenttoken=="var") vars=vars:+field
			}
		}

		return this
	}

	def ReportBlob(i:Int):String=
	{
		var td1=""
		var td2=""
		var td3=""
		val id=s"{engineoptions}#{$uniqueid}#{$name}"
		if(kind=="button")
		{
			td1=s"""
				|<button id="$id" text="$name"/>
			""".stripMargin
		}
		if(kind=="check")
		{
			td1=s"""
				|<label text="$name"/>
			""".stripMargin
			var value=Builder.GS(Builder.Cve(id),defaultstr)
			Builder.Set(Builder.Cve(id),StringData(value))				
			td2=s"""
			|<checkbox id="$id" prefixget="variant" prefixset="variant"/>
			""".stripMargin
		}
		if(kind=="string")
		{
			td1=s"""
				|<label text="$name"/>
			""".stripMargin
			var value=Builder.GS(Builder.Cve(id),defaultstr)
			Builder.Set(Builder.Cve(id),StringData(value))				
			td2=s"""
			|<textfield id="$id" prefixget="variant" prefixset="variant"/>
			""".stripMargin
			td3=s"""
				|<button id="$id#{apply}" qualifier="apply" text="Apply"/>
			""".stripMargin
		}
		if(kind=="spin")
		{				
			val minv=DataUtils.ParseInt(minstr,0)
			val maxv=DataUtils.ParseInt(maxstr,100)
			var span=maxv-minv
			if(minv==1) span+=1
			var unit=1
			if(span>10)
			{
				unit=span/10
			}
			val textid=id+"#{text}"
			val sliderid=id+"#{slider}"
			var value=Builder.GS(Builder.Cve(textid),""+defaultstr.toDouble)
			val intvalue=value.toDouble.toInt
			Builder.Set(Builder.Cve(textid),""+intvalue)
			Builder.Set(Builder.Cve(sliderid),value)				
			td1=s"""
				|<label text="$name"/>
			""".stripMargin
			td2=s"""
			|<slider width="300.0" id="$id#{slider}" prefixget="variant" prefixset="variant" min="$minstr" max="$maxstr" majortickunit="$unit" showticklabels="true"/>
			""".stripMargin
			td3=s"""
			|<textfield id="$id#{text}" qualifier="text" prefixget="variant" prefixset="variant" text="$intvalue" width="100.0" />
			""".stripMargin
		}
		if(kind=="combo")
		{
			td1=s"""
				|<label text="$name"/>
			""".stripMargin
			var value=Builder.GS(Builder.Cve(id),defaultstr)
			val items=(for(v <- vars) yield ("<s>"+v+"</s>")).mkString("\n")
			val sel=Builder.GS(Builder.Cve(id)+"#{selected}",defaultstr)
			val data=Data.FromXMLString(s"""
				|<m>
				|<a key="items">
				|$items
				|</a>
				|<s key="selected">$sel</s>
				|</m>
			""".stripMargin)
			Builder.Set(Builder.Cve(id),data)
			td2=s"""
			|<combobox id="$id" prefixget="variant" prefixset="variant"/>
			""".stripMargin
		}
		if(kind=="separator")
		{
			td1=s"""
				|<hbox width="600.0" height="1.0" style="-fx-border-style:solid; -fx-border-width: 1px;" cs="3" />
			""".stripMargin
		}
		def tdrc(td:String,c:Int):String=
		{
			td.replaceAll(".>",s""" r="$i" c="$c" />""")
		}
		td1=tdrc(td1,1); td2=tdrc(td2,2); td3=tdrc(td3,3);
		s"""
			|$td1
			|$td2
			|$td3
		""".stripMargin
	}

}

case class Options(var options:List[Option]=List[Option](),var uniqueid:Int= -1)
{
	var hasmultipv=false
	var minmultipv=1
	var maxmultipv=1

	InitOptions

	def InitOptions
	{
		Add(Option(name="Reset defaults",kind="button"))
		Add(Option(kind="separator"))
	}

	def Add(o:Option)
	{
		if(o.name=="MultiPV")
		{
			if(!DataUtils.IsInt(o.minstr)) return
			if(!DataUtils.IsInt(o.maxstr)) return
			hasmultipv=true
			minmultipv=o.minstr.toInt
			maxmultipv=o.maxstr.toInt
		}
		o.uniqueid=uniqueid
		options=options:+o
	}

	def ReportBlob:String=
	{
		var i=0;
		val content=(for(option<-options) yield { i+=1; option.ReportBlob(i) }).mkString("\n")
		content
	}
}

case class Id(
	var name:String="",
	var author:String="",
	var protocol:String="uci"
)
{
	def ParseLine(line:String)
	{
		val tokenizer=Tokenizer(line)
		val head=tokenizer.Get
		if(head!=null)
		{
			if(protocol=="uci")
			{
				if(head=="id")
				{
					val token=tokenizer.Get
					val value=tokenizer.GetRest
					if(value==null) return
					if(token=="name") name=value
					if(token=="author") author=value
				}
			}
		}
	}
}

case class XboardFeatures(
	var setboard:Boolean=false,
	var analyze:Boolean=true,
	var colors:Boolean=true,
	var done:Boolean=false,
	var reuse:Boolean=true,
	var myname:String="",
	var uniqueid:Int= -1
)
{
	var options:Options=Options(uniqueid=uniqueid)

	def ParseXBOARDBool(str:String,default:Boolean):Boolean=
	{
		if(str==null) return default
		if(str=="1") return true
		if(str=="0") return false
		return default
	}

	def ParseLine(line:String)
	{
		val tokenizer=Tokenizer(line)
		val head=tokenizer.Get
		if(head==null) return
		if(head!="feature") return
		while(tokenizer.HasToken)
		{
			val token=tokenizer.Get
			val parts=token.split("=").toList
			if(parts.length==2)
			{
				val feature=parts(0)
				var value=parts(1)

				if(value.length>0)
				{
					var needjoin=false
					var joinparts=List[String]()
					if(value(0)=='"')
					{
						if(value.length>1)
						{
							if(value(value.length-1)=='"')
							{
								value=value.substring(1,value.length-1)
							} else {
								joinparts=List[String](value.substring(1))
								needjoin=true
							}
						} else {
							joinparts=List[String]("")
							needjoin=true
						}
						var closed=false
						if(needjoin)
						{
							while(tokenizer.HasToken&&(!closed))
							{
								var part=tokenizer.Get
								if(part.length>0)
								{
									if(part(part.length-1)=='"')
									{
										part=part.substring(0,part.length-1)
										closed=true
									}
								}
								joinparts=joinparts:+part
							}
							value=joinparts.mkString(" ")
						}
					}
				}

				if(feature=="myname") myname=value
				if((feature=="setboard")&&(value=="1")) setboard=true
				if((feature=="analyze")&&(value=="0")) analyze=false
				if((feature=="colors")&&(value=="0")) colors=false
				if((feature=="reuse")&&(value=="0")) reuse=false
				if((feature=="done")&&(value=="1")) done=true

				if(feature=="option")
				{
					val vtokenizer=Tokenizer(value)
					var nameparts=List[String]()
					var nameend=false
					while(vtokenizer.HasToken&&(!nameend))
					{
						val token=vtokenizer.Poll
						if(token.length>0)
						{
							if(token(0)=='-')
							{
								nameend=true
							} else {
								vtokenizer.Get
								nameparts=nameparts:+token
							}
						}
					}
					var kind=vtokenizer.Get
					if(kind!=null)
					{
						if(kind.length>0)
						{
							val name=nameparts.mkString(" ")
							kind=kind.substring(1)

							if(kind=="check")
							{
								val checkdefaulttoken=vtokenizer.Get
								val checkdefaultstr=""+ParseXBOARDBool(checkdefaulttoken,false)									
								val option=Option(kind=kind,name=name,defaultstr=checkdefaultstr)
								options.Add(option)
							} else if((kind=="spin")||(kind=="slider")) {
								val spindefaulttoken=vtokenizer.Get
								val spindefaultstr=""+DataUtils.ParseInt(spindefaulttoken,1)
								val spinmintoken=vtokenizer.Get
								val spinminstr=""+DataUtils.ParseInt(spinmintoken,0)
								val spinmaxtoken=vtokenizer.Get
								val spinmaxstr=""+DataUtils.ParseInt(spinmaxtoken,0)
								val option=Option(kind="spin",name=name,
									defaultstr=spindefaultstr,minstr=spinminstr,maxstr=spinmaxstr,subkind=kind)
								options.Add(option)
							} else if((kind=="button")||(kind=="save")||(kind=="reset")) {
								val option=Option(kind="button",name=name,subkind=kind)
								options.Add(option)
							} else if((kind=="string")||(kind=="file")||(kind=="path")) {
								val stringdefaulttoken=vtokenizer.GetRest
								val stringdefaultstr=if(stringdefaulttoken==null) "" else stringdefaulttoken
								val option=Option(kind="string",name=name,defaultstr=stringdefaultstr,subkind=kind)
								options.Add(option)
							} else if(kind=="combo") {
								// not implemented
							} else {
								// unknown option
							}
						}
					}
				}
			}
		}
	}
}