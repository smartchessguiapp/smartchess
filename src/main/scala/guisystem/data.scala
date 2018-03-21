package smartchess

///////////////////////////////////////////////////////////////////////////////////
// data is a framework to build a tree like data structure where nodes can be:
// leaf node:
//   StringData ( encapsulates a String )
// branching nodes:
//   ArrayData ( array of Data )
//   MapData ( map of Data )
///////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////
// PrintableInterface
///////////////////////////////////////////////////////////////////////////////////

// PrintableInterface is a trait that requires a class to be able
// to represent itself in a printable form

trait PrintableInterface
{
	def ReportPrintable(level:Int=0,buff:String=""):String // abstract

	def ToPrintable:String=
	{
		ReportPrintable(0)
	}
}

///////////////////////////////////////////////////////////////////////////////////
// GetSetTypedDataValueWithDefault
///////////////////////////////////////////////////////////////////////////////////

// GetSetTypedDataValueWithDefault is a trait that ensures that a class can return
// string values from a map by path key as a given type with a default provided
// for the case when the key is not present in the map
// and can set a map value

trait GetSetTypedDataValueWithDefault
{
	def Get(path:Path):Data // abstract, can return null

	def Set(path:Path,value:Data):Data // abstract

	def GetStringWithDefault(pathstr:String,default:String=""):String // abstract

	def GetIntWithDefault(pathstr:String,default:Int=0):Int // abstract

	def GetDoubleWithDefault(pathstr:String,default:Double=0.0):Double // abstract

	def GetBooleanWithDefault(pathstr:String,default:Boolean=false):Boolean // abstract

	// convenience functions

	def G(pathstr:String):Data=Get(Path.FromString(pathstr))

	def S(pathstr:String,value:Data):Data=Set(Path.FromString(pathstr),value)

	def GS(pathstr:String,default:String=""):String=GetStringWithDefault(pathstr,default)

	def GI(pathstr:String,default:Int=0):Int=GetIntWithDefault(pathstr,default)

	def GD(pathstr:String,default:Double=0.0):Double=GetDoubleWithDefault(pathstr,default)

	def GB(pathstr:String,default:Boolean=false):Boolean=GetBooleanWithDefault(pathstr,default)
}

///////////////////////////////////////////////////////////////////////////////////
// GetSetTypedStringValueWithDefault
///////////////////////////////////////////////////////////////////////////////////

// GetSetTypedStringValueWithDefault is a trait that ensures that a class can return
// string values from a map by key as a given type with a default provided
// for the case when the key is not present in the map
// and can set a map value

trait GetSetTypedStringValueWithDefault
{
	def Get(id:String):String // abstract, can return null

	def Set(id:String,value:String) // abstract

	def GetStringWithDefault(id:String,default:String=""):String // abstract

	def GetIntWithDefault(id:String,default:Int=0):Int // abstract

	def GetDoubleWithDefault(id:String,default:Double=0.0):Double // abstract

	def GetBooleanWithDefault(id:String,default:Boolean=false):Boolean // abstract

	// convenience functions

	def G(id:String):String=Get(id)

	def S(id:String,value:String)=Set(id,value)

	def GS(id:String,default:String=""):String=GetStringWithDefault(id,default)

	def GI(id:String,default:Int=0):Int=GetIntWithDefault(id,default)

	def GD(id:String,default:Double=0.0):Double=GetDoubleWithDefault(id,default)

	def GB(id:String,default:Boolean=false):Boolean=GetBooleanWithDefault(id,default)
}

///////////////////////////////////////////////////////////////////////////////////
// DataUtils
///////////////////////////////////////////////////////////////////////////////////

case class EraseFilesResult(
	var nofiles:Int=0,
	var nodirs:Int=0,
	var size:Int=0
)
{	
}

// utility functions for Data
object DataUtils
{
	def mkdirs(path: List[String])=path.tail.foldLeft(new java.io.File(path.head)){(a,b) => a.mkdir; new java.io.File(a,b)}.mkdir
	def mkdir(path: String)=mkdirs(List(path))

	def getListOfFiles(dir: String):List[java.io.File] =
	{
		val d = new java.io.File(dir)
		if (d.exists && d.isDirectory)
		{
			d.listFiles.filter(_.isFile).toList
		}
		else
		{
			List[java.io.File]()
		}
	}
	
	def getListOfFileNames(dir: String):List[String] =
		for(f<-getListOfFiles(dir)) yield f.getName

	def getListOfFileNamesWithExt(dir:String,ext:String):List[String]=
	{
		var l=scala.collection.mutable.ArrayBuffer[String]()
		for(name<-getListOfFileNames(dir))
		{
			val parts=name.split("\\.")
			if(parts.length==2)
			{
				if(parts(1)==ext)
				{
					l+=parts(0)
				}
			}
		}
		l.toList
	}

	def Sep = java.io.File.separator

	def PathFromList(dirs:List[String]):String = dirs.mkString(java.io.File.separator)
	def FileFromList(dirs:List[String]) = new java.io.File(PathFromList(dirs))

	def EraseFiles(path:String,dofiles:Boolean=true,dodirs:Boolean=false,recursive:Boolean=false,
		setefr:EraseFilesResult=EraseFilesResult(),reportonly:Boolean=false):EraseFilesResult =
	{
		val d = new java.io.File(path)

		var efr=setefr

		if(!(d.exists && d.isDirectory)) return efr

		val all=d.listFiles
		val dirs=all.filter(_.isDirectory).toList
		val files=all.filter(_.isFile).toList

		if(recursive) for(dir <- dirs) efr=EraseFiles(dir.getAbsolutePath,dofiles,dodirs,true,efr,reportonly)

		if(dofiles) for(f <- files)
		{
			efr.size+=f.length.toInt
			if(!reportonly) f.delete //else println("file to be deleted "+f.getAbsolutePath())
			efr.nofiles+=1
		}

		if(dodirs) for(dir <- dirs)
		{			
			val dpath=PathFromList(List(path,dir.getName))
			if(!reportonly) java.nio.file.Files.delete(java.nio.file.Paths.get(dpath))
				else println("directory to be deleted "+dpath)
			efr.nodirs+=1
		}

		efr
	}

	def CollectFiles(path:String,recursive:Boolean=false,setfiles:List[String]=List[String]()):List[String] =
	{
		val d = new java.io.File(path)

		if(!(d.exists && d.isDirectory)) return setfiles

		var allfiles=setfiles

		val all=d.listFiles
		val dirs=all.filter(_.isDirectory)		
		val files=all.filter(_.isFile)

		if(recursive) for(dir <- dirs) allfiles = CollectFiles(dir.getAbsolutePath,recursive,allfiles)

		allfiles = allfiles ::: ((for(f <- files) yield f.getAbsolutePath()).toList)

		allfiles
	}

	def DeleteDir(path:String)
	{
		val f=new java.io.File(path)
		
		if(!f.exists) return
		if(!f.isDirectory) return

		java.nio.file.Files.delete(java.nio.file.Paths.get(path))
	}

	def sigmoid(x:Double):Double = 1.0 / ( 1.0 + scala.math.exp( -x ) )

	def sigmoidM(x:Double, bias:Double, divisor:Double) = sigmoid( ( x + bias ) / divisor ) / sigmoid( bias / divisor )

	def SelectByFactor(
		factor:Int,
		items:List[String],
		addanyway:List[String]=List[String](),		
		bias:Double=0.0,
		divisor:Double=1.0,
		scoredeltas:List[Double]=null
	):String =
	{
		val r=(new scala.util.Random)

		var alloc:Double=1000.0

		val sanbuff=scala.collection.mutable.ArrayBuffer[String]()

		for(item <- addanyway) sanbuff+=item		

		var index=0

		for(item <- items)
		{			
			val scoredelta:Double = if(scoredeltas==null) 0.0 else scoredeltas(index)
			val modfactor = DataUtils.sigmoidM(scoredelta,bias,divisor)
			val modalloc = alloc * modfactor
			//println("scoredelta %5.0f modfactor %1.6f alloc %10.5f modalloc %10.5f".format(scoredelta,modfactor,alloc,modalloc))
			for(i <- 1 to modalloc.toInt) sanbuff+=item
			alloc = alloc * ( 1000 - 10 * factor ).toDouble / 1000.0			
			index+=1
		}

		val sel=r.nextInt(sanbuff.length)

		sanbuff(sel)
	}

	def Normalize(what:String):String =
	{
		if(what==null) return ""

		val out=scala.collection.mutable.ArrayBuffer[Character]()

		val norm=java.text.Normalizer.normalize(what,java.text.Normalizer.Form.NFD)

		for(c <- norm.toCharArray)
		{
			if (c <= '\u007F') out+=c
		}

		out.mkString
	}

	def AddToPersistentStringList(path:String,what:String,add:Boolean=true):Boolean =
	{
		var lines=List[String]()
		val f=new java.io.File(path)
		if(f.exists) lines=ReadFileToString(path).split("[\\r?\\n]").toList
		var ret=true
		if(lines.contains(what))
		{
			if(add) return false else lines=lines.filter(x => x!=what)
		}
		else
		{
			if(add) lines=lines:+what else ret=false
		}
		if(lines.length>=0)
		{
			WriteStringToFile(path,lines.mkString("\n"))
		}
		else
		{
			f.delete()
		}
		ret
	}

	def DeleteFromPersistentStringList(path:String,what:String):Boolean =
	{
		AddToPersistentStringList(path,what,add=false)
	}

	def GetRandomStringFromPersistentStringList(path:String,excluding:String=null):String =
	{
		if(!new java.io.File(path).exists) return null
		val lines=ReadFileToString(path).split("[\\r?\\n]").toList
		val r=(new scala.util.Random).nextInt(lines.length)
		if(excluding==null) return lines(r)
		if(lines(r)!=excluding) return lines(r)
		if(r<(lines.length-1)) return lines(r+1)
		return lines(0)
	}

	def FormatBytes(n:Int):String =
	{		
		if(n<1000) return ""+n
		val nf=n.toDouble
		if(nf<1e6) return "%.1f kB".format(nf/1e3)
		if(nf<1e9) return "%.1f MB".format(nf/1e6)
		"%.1f GB".format(nf/1e9)
	}

	def GetFileSize(path:String):Int =
	{
		val f=new java.io.File(path)
		if(!f.exists) return -1
		f.length().toInt
	}

	def TrimNice(list:List[String],max:Int):List[String] =
	{
		if(list==null) return List[String]("...")
		if(list.length < max) return list
		list.slice(0,max):+"..."
	}

	def BeginsWith(str:String,what:Character):Boolean=
	{
		if(str==null) return false
		if(str.length==0) return false
		if(str(0)==what) return true
		false
	}

	def EndsWith(str:String,what:Character):Boolean=
	{
		if(str==null) return false
		if(str.length==0) return false
		if(str(str.length-1)==what) return true
		false
	}

	def Tabs(level:Int)=Iterator.continually(" ").take(level).mkString

	def WriteStringToFile(path:String,content:String)
	{
		org.apache.commons.io.FileUtils.writeStringToFile(
				new java.io.File(path),
				content,
				null.asInstanceOf[String]
			)
	}

	def ReadFileToString(path:String):String=
	{
		val f=new java.io.File(path)
		if(!f.exists()) return null
		org.apache.commons.io.FileUtils.readFileToString(
						f,
						null.asInstanceOf[String]
					)
	}

	def StartsWith(str:String,c:Char):Boolean=
	{
		if(str==null) return false
		if(str.length<=0) return false
		str(0)==c
	}

	def EndsWith(str:String,c:Char):Boolean=
	{
		if(str==null) return false
		if(str.length<=0) return false
		str(str.length-1)==c
	}

	def StartsAndEndsWith(str:String,start:Char,end:Char,reqsep:Boolean=true):Boolean=
	{
		if(!StartsWith(str,start)) return false
		if(!EndsWith(str,end)) return false
		if( (start==end) && (str.length< 2)) return false
		true
	}

	def InnerString(str:String):String=
	{
		if(str==null) return ""
		if(str.length<= 2) return ""
		str.substring(1,str.length-1)
	}

	def IsInt(str:String):Boolean=
	{
		try
		{
			val intvalue=str.toInt
		}
		catch
		{
			case e:Throwable => return false
		}
		true
	}

	def IsDouble(str:String):Boolean=
	{
		try
		{
			val doublevalue=str.toDouble
		}
		catch
		{
			case e:Throwable => return false
		}
		true
	}

	def ParseString(str:String,default:String=""):String=
	{
		if(str==null) return default
		str
	}

	def ParseInt(str:String,default:Int=0):Int=
	{
		var intvalue:Int=default
		try
		{
			intvalue=str.toInt
		}
		catch
		{
			case e:Throwable => return default
		}
		intvalue
	}

	def ParseDouble(str:String,default:Double=0.0):Double=
	{
		var doublevalue:Double=default
		try
		{
			doublevalue=str.toDouble
		}
		catch
		{
			case e:Throwable => return default
		}
		doublevalue
	}

	def ParseBoolean(str:String,default:Boolean=false):Boolean=
	{
		val value=ParseString(str)
		if(value=="true") return true
		if(value=="false") return false
		default
	}
}

///////////////////////////////////////////////////////////////////////////////////
// class Tag
///////////////////////////////////////////////////////////////////////////////////

// Tag represents an XML tag

case class Tag(
	var kind:String="",
	var children:scala.collection.mutable.ArrayBuffer[Tag]=scala.collection.mutable.ArrayBuffer(),
	var attributes:Map[String,String]=Map(),
	var text:String=""
) extends PrintableInterface
{
	def ReportPrintable(level:Int=0,buff:String=""):String=
	{
		val childrenPrintable=((for(child<-children) yield child.ReportPrintable(level+1,buff)).mkString("\n"))
		buff+DataUtils.Tabs(level)+kind+" "+attributes+" "+text+(if(children.length==0) "" else "\n"+childrenPrintable)
	}

	def GetAttribute(key:String,default:String=null):String=
	{
		if(attributes.contains(key)) return attributes(key)
		default
	}
}

///////////////////////////////////////////////////////////////////////////////////
// object Tag
///////////////////////////////////////////////////////////////////////////////////

// object Tag holds static Tag functions

object Tag
{
	def FromXML(e:scala.xml.Elem):Tag=
	{
		def getAttributes(e:scala.xml.Elem):Map[String,String]=
		{
			var attr=e.attributes
			val it=Iterator.continually(attr).takeWhile(curr_attr=>
			{
				if(attr.next!=null) attr=attr.next
				curr_attr.next!=null
			})
			(for(attr<-it) yield (attr.key->attr.value.mkString)).toMap
		}

		val attributes=getAttributes(e)

		val children=scala.collection.mutable.ArrayBuffer[Tag]()
		
		for(child<-e.child)
		{
			if(child.isInstanceOf[scala.xml.Elem]) children+=FromXML(child.asInstanceOf[scala.xml.Elem])
		}

		Tag(e.label,children,attributes,if(e.child.length==1) e.text else "")
	}

	def FromXMLFile(path:String):Tag=
	{
		FromXML(scala.xml.XML.loadFile(path))
	}

	def FromXMLString(content:String):Tag=
	{
		FromXML(scala.xml.XML.loadString(content))
	}
}

///////////////////////////////////////////////////////////////////////////////////
// Path constituents
///////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////
// trait PathElement
///////////////////////////////////////////////////////////////////////////////////

// PathElement identifies an edge in the data tree

sealed trait PathElement
{
	def IsEqualTo(pe:PathElement):Boolean // abstract

	def AsString:String // abstract

	def InnerString:String // abstract

	def CreateClone:PathElement // abstract
}

///////////////////////////////////////////////////////////////////////////////////
// ArrayIndex
///////////////////////////////////////////////////////////////////////////////////

// ArrayIndex is a PathElement identifying an index in an array

case class ArrayIndex(
	var index:Int=0
) extends PathElement
{
	def IsEqualTo(pe:PathElement):Boolean=
	{
		if(pe==null) return false
		if(!pe.isInstanceOf[ArrayIndex]) return false
		if(pe.asInstanceOf[ArrayIndex].index!=index) return false
		true
	}

	def AsString:String = s"[$index]"

	def InnerString:String = ""+index

	def CreateClone:PathElement=ArrayIndex(index)
}

///////////////////////////////////////////////////////////////////////////////////
// MapKey
///////////////////////////////////////////////////////////////////////////////////

// MapKey is a PathElement identifying a key in a map

case class MapKey(
	var key:String=""
) extends PathElement
{
	def IsEqualTo(pe:PathElement):Boolean=
	{
		if(pe==null) return false
		if(!pe.isInstanceOf[MapKey]) return false
		if(pe.asInstanceOf[MapKey].key!=key) return false
		true
	}

	def AsString:String = s"{$key}"

	def InnerString = key

	def CreateClone:PathElement=MapKey(key)
}

///////////////////////////////////////////////////////////////////////////////////
// class Path
///////////////////////////////////////////////////////////////////////////////////

// Path identifies an element in the Data tree

case class Path(
	var elements:scala.collection.mutable.ArrayBuffer[PathElement]=scala.collection.mutable.ArrayBuffer[PathElement]()
)
{
	def HasElements:Boolean= ( elements.length > 0 )

	def IsEmpty:Boolean= !HasElements

	def +(p:Path):Path=
	{
		var newp=Path()
		for(e<-elements) newp.elements+=e.CreateClone
		if(p==null) return newp
		for(e<-p.elements) newp.elements+=e.CreateClone
		newp
	}

	def Head:PathElement=
	{
		if( elements.length <= 0 ) return null
		elements.head
	}

	def Tail:Path=
	{
		if( elements.length <= 0 ) return this
		Path(elements.tail)
	}

	def Trunk:Path=
	{
		if( elements.length <= 0 ) return this
		Path(elements.reverse.tail.reverse)
	}

	def IsEqualTo(path:Path):Boolean=
	{
		if(path==null) return false
		if(path.elements.length!=elements.length) return false
		for( i <- 0 to (elements.length-1) ) if(!path.elements(i).IsEqualTo(elements(i))) return false
		true
	}

	def Add(pe:PathElement)
	{
		if(pe==null) return
		elements+=pe
	}

	def AsString:String=
	{
		if( elements.length <= 0 ) return ""

		val parts=for(element<-elements) yield element.AsString

		parts.mkString("#")
	}

	def Parts:List[String]=
	{
		(for(element<-elements) yield element.InnerString).toList
	}
}

///////////////////////////////////////////////////////////////////////////////////
// object Pathelement
///////////////////////////////////////////////////////////////////////////////////

// object PathElement holds static PathElement functions

object PathElement
{
	def FromString(pestr:String):PathElement=
	{
		if(pestr==null) return null
		if(DataUtils.StartsAndEndsWith(pestr,'[',']'))
		{
			val innerstr=DataUtils.InnerString(pestr)
			if(!DataUtils.IsInt(innerstr)) return null			
			return ArrayIndex(DataUtils.ParseInt(innerstr))
		}
		if(DataUtils.StartsAndEndsWith(pestr,'{','}'))
		{
			val key=DataUtils.InnerString(pestr)
			return MapKey(key)
		}
		null
	}
}

///////////////////////////////////////////////////////////////////////////////////
// object Path
///////////////////////////////////////////////////////////////////////////////////

// object Path holds static path functions

object Path
{
	def AddToId(addleft:String,baseid:String,addright:String):String=
	{
		if(baseid==null) return null
		List(addleft,baseid,addright).filter(_!=null).mkString("#")
	}

	def FromString(pathstr:String):Path=
	{
		if(pathstr==null) return null
		var path=Path()
		// the only legitimate way to encode an empty path is an empty string
		if(pathstr=="") return path
		val parts=pathstr.split("#")
		for(part<-parts)
		{
			val pe=PathElement.FromString(part)
			// invalid path elements make the whole path invalid
			if(pe==null) return null
			path.Add(pe)
		}
		path
	}
}

///////////////////////////////////////////////////////////////////////////////////
// trait Data
///////////////////////////////////////////////////////////////////////////////////

// Data is an abstract trait, all members of the hierarchy derive from this trait

sealed trait Data extends PrintableInterface with GetSetTypedDataValueWithDefault
{
	// should be able to represent itself as XML
	def ReportXML(key:String=null):scala.xml.Elem // abstract

	///////////////////////////////////////

	def GetStringWithDefault(pathstr:String,default:String=""):String=
	{
		val stringobj=G(pathstr)
		if(stringobj==null) return default
		if(!stringobj.isInstanceOf[StringData]) return default
		DataUtils.ParseString(stringobj.asInstanceOf[StringData].value,default)
	}

	def GetIntWithDefault(pathstr:String,default:Int=0):Int=
	{
		val intobj=G(pathstr)
		if(intobj==null) return default
		if(!intobj.isInstanceOf[StringData]) return default
		DataUtils.ParseInt(intobj.asInstanceOf[StringData].value,default)
	}

	def GetDoubleWithDefault(pathstr:String,default:Double=0.0):Double=
	{
		val doubleobj=G(pathstr)
		if(doubleobj==null) return default
		if(!doubleobj.isInstanceOf[StringData]) return default
		DataUtils.ParseDouble(doubleobj.asInstanceOf[StringData].value,default)
	}

	def GetBooleanWithDefault(pathstr:String,default:Boolean=false):Boolean=
	{
		val intobj=G(pathstr)
		if(intobj==null) return default
		if(!intobj.isInstanceOf[StringData]) return default
		DataUtils.ParseBoolean(intobj.asInstanceOf[StringData].value,default)
	}

	def ReportXMLPretty(width:Int=80,tabwidth:Int=3):String=
	{
		new scala.xml.PrettyPrinter(width,tabwidth).format(ReportXML())
	}

	def SaveToXMLFile(path:String)
	{
		scala.xml.XML.save(path,ReportXML())
	}

	def SaveToXMLFilePretty(path:String,width:Int=80,tabwidth:Int=3)
	{
		DataUtils.WriteStringToFile(path,ReportXMLPretty(width,tabwidth))
	}
}

///////////////////////////////////////////////////////////////////////////////////
// StringData
///////////////////////////////////////////////////////////////////////////////////

// StringData - encapsulates a String

case class StringData(var value:String="") extends Data
{
	def ReportPrintable(level:Int=0,buff:String=""):String=
	{
		buff+DataUtils.Tabs(level)+"string\n"+DataUtils.Tabs(level+1)+value
	}

	def ReportXML(key:String=null):scala.xml.Elem=
	{
		return if(key==null) <s>{value}</s>
		else <s key={key}>{value}</s>
	}

	def Get(path:Path):Data=
	{
		if(path==null) return null
		if(path.HasElements) return null
		this
	}

	def Set(path:Path,value:Data):Data=
	{
		if(path==null) return this
		if(path.IsEmpty) return value
		Data.Create(path,value)
	}
}

///////////////////////////////////////////////////////////////////////////////////
// ArrayData
///////////////////////////////////////////////////////////////////////////////////

// ArrayData - an array of Data objects

case class ArrayData(var array:scala.collection.mutable.ArrayBuffer[Data]=scala.collection.mutable.ArrayBuffer[Data]()) extends Data
{
	def ReportPrintable(level:Int=0,buff:String=""):String=
	{
		val arrayPrintable=(for(d<-array) yield 
			if(d!=null) d.ReportPrintable(level+1,buff) else DataUtils.Tabs(level)+"!null").mkString("\n")
		buff+DataUtils.Tabs(level)+"array"+(if(array.length==0) "" else "\n"+arrayPrintable)
	}

	def ReportXML(key:String=null):scala.xml.Elem=
	{
		val innerXML=for(d<-array) yield d.ReportXML()
		return if(key==null) <a>{innerXML}</a>
		else <a key={key}>{innerXML}</a>
	}

	def Get(path:Path):Data=
	{
		if(path==null) return null
		if(path.IsEmpty) return this
		val head=path.Head
		if(!head.isInstanceOf[ArrayIndex]) return null
		val index=head.asInstanceOf[ArrayIndex].index
		if(index< array.length) return array(index).Get(path.Tail)
		null
	}

	def Set(path:Path,value:Data):Data=
	{
		if(path==null) return this
		if(path.IsEmpty) return value
		val head=path.Head
		if(head.isInstanceOf[ArrayIndex])
		{
			val index=head.asInstanceOf[ArrayIndex].index
			if(index< 0) return null
			if(HasIndex(index))
			{
				Add(index,array(index).Set(path.Tail,value))
				return this
			}
			else
			{
				Add(index,Data.Create(path.Tail,value))
				return this
			}
		}
		Data.Create(path,value)
	}

	///////////////////////////////////////

	def HasIndex(index:Int):Boolean=
	{
		if(index< 0) return false
		if(index< array.length)
		{
			if( array(index) == null ) return false
			return true
		}
		false
	}

	def Add(index:Int,value:Data):Boolean=
	{
		if(index< 0) return false
		if(index< array.length)
		{
			array(index)=value
			return true
		}
		// if array does not have enough elements, add empty strings
		while(array.length<= index) array+=StringData("")
		array(index)=value
		true
	}
}

///////////////////////////////////////////////////////////////////////////////////
// MapData
///////////////////////////////////////////////////////////////////////////////////

// MapData - a map of Data objects

case class MapData(var map:Map[String,Data]=Map[String,Data]()) extends Data
{
	def ReportPrintable(level:Int=0,buff:String=""):String=
	{
		val mapPrintable=(for((k,v)<-map) yield DataUtils.Tabs(level+1)+k+"->\n"+v.ReportPrintable(level+2,buff)).mkString("\n")
		buff+DataUtils.Tabs(level)+"map"+(if(map.keys.toList.length==0) "" else "\n"+mapPrintable)
	}

	def ReportXML(key:String=null):scala.xml.Elem=
	{
		val innerXML=for((k,v)<-map) yield v.ReportXML(k)
		return if(key==null) <m>{innerXML}</m>
		else <m key={key}>{innerXML}</m>
	}

	def Get(path:Path):Data=
	{
		if(path==null) return null
		if(path.IsEmpty) return this
		val head=path.Head
		if(!head.isInstanceOf[MapKey]) return null
		val key=head.asInstanceOf[MapKey].key
		if(map.contains(key)) return map(key).Get(path.Tail)
		null
	}

	def Set(path:Path,value:Data):Data=
	{
		if(path==null) return this
		if(path.IsEmpty) return value
		val head=path.Head
		if(head.isInstanceOf[MapKey])
		{
			val key=head.asInstanceOf[MapKey].key			
			if(HasKey(key))
			{
				Add(key,map(key).Set(path.Tail,value))
				return this
			}
			else
			{
				Add(key,Data.Create(path.Tail,value))
				return this
			}
		}
		Data.Create(path,value)
	}

	///////////////////////////////////////

	def HasKey(key:String):Boolean=
	{
		if(key==null) return false
		if(map.contains(key))
		{
			return ( map(key) != null )
		}
		false
	}

	def Add(key:String,value:Data):Boolean=
	{
		if(key==null) return false
		map+=(key->value)
		true
	}
}

///////////////////////////////////////////////////////////////////////////////////
// object Data
///////////////////////////////////////////////////////////////////////////////////

// object Data holds static Data functions

object Data
{
	def FromTag(t:Tag):Data=
	{
		t.kind match
		{
			case "s" => StringData(t.text)
			case "a" => ArrayData(t.children.map(FromTag))
			case "m" => MapData(t.children.map(child=>(child.GetAttribute("key")->FromTag(child))).toMap)
			case _ => StringData("")
		}
	}

	def FromXML(e:scala.xml.Elem):Data=
	{
		FromTag(Tag.FromXML(e))
	}

	def FromXMLFile(path:String):Data=
	{
		if(new java.io.File(path).exists) FromXML(scala.xml.XML.loadFile(path))
		else MapData()
	}

	def FromXMLString(content:String):Data=
	{
		FromXML(scala.xml.XML.loadString(content))
	}

	// create a minimal empty tree from path
	def Create(path:Path,value:Data):Data=
	{
		if(path==null) return null
		if(path.IsEmpty) return value
		val head=path.Head
		if(head.isInstanceOf[ArrayIndex])
		{
			val arraydata=ArrayData()
			arraydata.Add(head.asInstanceOf[ArrayIndex].index,Create(path.Tail,value))
			return arraydata
		}
		else if(head.isInstanceOf[MapKey])
		{
			val mapdata=MapData()
			mapdata.Add(head.asInstanceOf[MapKey].key,Create(path.Tail,value))
			return mapdata
		}
		else
		{
			// this should not happen
			return null
		}
	}

	def ArrayDataFromListString(l:List[String])=
	{
		val arraydata=ArrayData()
		for(e<-l) arraydata.array+=StringData(e)
		arraydata
	}
}

case class SimpleDate(
	var date:String=null,
	var formats:Array[String]=Array("yyyy.MM.dd"),
	var time:String="00:00:00"
)
{
	var d:java.util.Date=null

	try
	{
		d=org.apache.commons.lang.time.DateUtils.parseDate(date,formats)			
	}
	catch
	{
		case e:Throwable => 
	}

	def isLaterThan(what:SimpleDate):Boolean =
	{
		if((d==null)||(what.d==null)) return true
		val cmp=d.compareTo(what.d)
		if(cmp==0){
			return time > what.time
		}
		cmp>=0
	}

	def isEarlierThan(what:SimpleDate):Boolean =
	{
		if((d==null)||(what.d==null)) return true
		val cmp=d.compareTo(what.d)
		if(cmp==0){
			return time < what.time
		}
		cmp<=0
	}
}