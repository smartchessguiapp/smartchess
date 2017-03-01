package smartchess

import piece._
import square._

case class move(

	val fromalgeb:String="",

	var from:TSquare=NO_SQUARE,
	var from_piece:TPiece=NO_PIECE,
	var to:TSquare=NO_SQUARE,
	var to_piece:TPiece=NO_PIECE,
	var prom_piece:TPiece=NO_PIECE,

	var king_move:Boolean=false,

	var pawn_move:Boolean=false,
	var pawn_capture:Boolean=false,
	var pawn_promotion:Boolean=false,
	var pawn_push:Boolean=false,
	var pawn_single_push:Boolean=false,
	var pawn_double_push:Boolean=false,
	var pawn_passing_square:TSquare=NO_SQUARE,

	var ep_capture:Boolean=false,
	var capture:Boolean=false,
	var ep_clear_square:TSquare=NO_SQUARE,
	var castling_move:Boolean=false,
	var castling_side:Int=0,

	var status:String="",

	var end_vector:Boolean=false,
	var end_piece:Boolean=false,
	var next_vector:Int=0

	)
{

	if(fromalgeb!="")
	{
		from_algeb(fromalgeb)
	}

	def cclone:move=move(
			"",

			from,
			from_piece,
			to,
			to_piece,
			prom_piece,

			king_move,

			pawn_move,
			pawn_capture,
			pawn_promotion,
			pawn_push,
			pawn_single_push,
			pawn_double_push,
			pawn_passing_square,

			ep_capture,
			capture,
			ep_clear_square,
			castling_move,
			castling_side,

			status,

			end_vector,
			end_piece,
			next_vector
		)

	def from_algeb(algeb:String)
	{
		val parts=algeb.grouped(2).toArray

		prom_piece=NO_PIECE

		if(parts.length>=2)
		{
			from=fromAlgeb(parts(0))
			to=fromAlgeb(parts(1))

			if(parts.length>=3)
			{
				prom_piece=fromFenChar(parts(2)(0))
			}
		}
		else
		{
			from=NO_SQUARE
			to=NO_SQUARE
		}
	}

	def toRawAlgeb=
		square.toAlgeb(from)+square.toAlgeb(to)

	def ppStr=
		(if(prom_piece!=NO_PIECE) toFenChar(toBlack(prom_piece)) else "")

	def squaresOk=
		(from!=NO_SQUARE)&&(to!=NO_SQUARE)

	def toAlgeb:String=
		{
			if(squaresOk)
			{
				toRawAlgeb+ppStr
			}
			else
			{
				"none"
			}
		}

	def statuspostfix:String=if(status=="") "" else " "+status

	def info:String=toSan("fr")

	def toSan(q:String=""):String=
	{

		if(castling_move)
		{
			return if(castling_side==0) "O-O" else "O-O-O"
		}

		var piecePrefix=""+toPieceChar(from_piece)

		if(pawn_move) piecePrefix=""

		var fromPrefix=""

		if(pawn_move)
		{
			if(capture) fromPrefix+=fileToAlgebFile(fileOf(from))
		}
		else
		{
			if(q.contains("f")) fromPrefix+=fileToAlgebFile(fileOf(from))
			if(q.contains("r")) fromPrefix+=rankToAlgebRank(rankOf(from))
		}

		var captureStr=""

		if(capture) captureStr="x"

		var toStr=square.toAlgeb(to)

		var promStr=""

		if(pawn_promotion)
		{
			promStr="="+toPieceChar(prom_piece)
		}

		piecePrefix+fromPrefix+captureStr+toStr+promStr

	}

}