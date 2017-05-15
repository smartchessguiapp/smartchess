package smartchess

object square
{

	type TSquare=Int
	type TFile=Int
	type TRank=Int

	val BOARD_SIZE_SHIFT=3

	val BOARD_SIZE=1 << BOARD_SIZE_SHIFT

	val HALF_BOARD_SIZE=BOARD_SIZE/2
	val HALF_BOARD_SIZE_MINUS_ONE=HALF_BOARD_SIZE-1

	val BOARD_AREA_SHIFT=BOARD_SIZE_SHIFT*2

	val BOARD_AREA=BOARD_SIZE*BOARD_SIZE

	val BOARD_MASK=BOARD_SIZE-1

	val RANK_MASK=BOARD_MASK << BOARD_SIZE_SHIFT

	val FILE_MASK=BOARD_MASK

	val NO_SQUARE=1 << BOARD_AREA_SHIFT

	val DARK=0
	val LIGHT=1

	def algebFileToFile(c: Char):TFile=c-'a'
	def algebRankToRank(c: Char):TRank='8'-c

	def fromFileRank(f:TFile,r:TRank):TSquare=
	{
		if((!fileOk(f))||(!rankOk(r))) return NO_SQUARE
		
		f+ (r << BOARD_SIZE_SHIFT)
	}

	def fromAlgeb(algeb:String):TSquare=
	{
		if(algeb.length < 2) return NO_SQUARE

		val f=algebFileToFile( algeb(0) )
		val r=algebRankToRank( algeb(1) )

		if(fileRankOk(f,r))
		{
			f | ( r << 3 )
		}
		else
		{
			NO_SQUARE
		}
	}

	def fileOf(s:TSquare):TFile=(s & FILE_MASK)
	def rankOf(s:TSquare):TRank=(s & RANK_MASK) >> BOARD_SIZE_SHIFT

	def fileOk(f:TFile):Boolean=(f>=0)&&(f< BOARD_SIZE)
	def rankOk(r:TRank):Boolean=(r>=0)&&(r< BOARD_SIZE)

	def fileRankOk(f:TFile,r:TRank):Boolean=
		fileOk(f)&&rankOk(r)

	def squareOk(s:TSquare):Boolean=
	{
		if(s<0) return false
		if(s>=BOARD_AREA) return false
		true
	}

	def fileToAlgebFile(f:TFile):Char=('a'.toInt+f).toChar
	def rankToAlgebRank(r:TRank):Char=('8'.toInt-r).toChar

	def isCentralRank(r:TRank):Boolean=((r==HALF_BOARD_SIZE)||(r==HALF_BOARD_SIZE_MINUS_ONE))
	def isCentralFile(f:TFile):Boolean=((f==HALF_BOARD_SIZE)||(f==HALF_BOARD_SIZE_MINUS_ONE))

	def colorIndexOf(sq:TSquare):Int=(1-(fileOf(sq)+rankOf(sq))%2)

	def isCentralSquare(sq:TSquare):Boolean=
	{
		if(sq==NO_SQUARE) return false
		return isCentralFile(fileOf(sq))&&isCentralRank(rankOf(sq))
	}

	def toAlgeb(s:TSquare):String=
		if(s!=NO_SQUARE) ""+fileToAlgebFile(fileOf(s))+rankToAlgebRank(rankOf(s)) else "-"

}