package smartchess

object piece
{

	type TPiece=Int
	type TColor=Int

	val YES=1
	val NO=0

	val IS_PIECE_BIT=7
	val COLOR_BIT=6
	val IS_SLIDING_BIT=5
	val IS_STRAIGHT_BIT=4
	val IS_DIAGONAL_BIT=3
	val IS_SINGLE_BIT=2
	val IS_KNIGHT_BIT=1
	val IS_PAWN_BIT=0

	val IS_PIECE_MASK=1 << IS_PIECE_BIT
	val COLOR_MASK=1 << COLOR_BIT
	val IS_SLIDING_MASK=1 << IS_SLIDING_BIT
	val IS_STRAIGHT_MASK=1 << IS_STRAIGHT_BIT
	val IS_DIAGONAL_MASK=1 << IS_DIAGONAL_BIT
	val IS_SINGLE_MASK=1 << IS_SINGLE_BIT
	val IS_KNIGHT_MASK=1 << IS_KNIGHT_BIT
	val IS_PAWN_MASK=1 << IS_PAWN_BIT

	val PIECE_TYPE_MASK=IS_PIECE_MASK|IS_SLIDING_MASK|IS_STRAIGHT_MASK|IS_DIAGONAL_MASK|IS_SINGLE_MASK|IS_KNIGHT_MASK|IS_PAWN_MASK

	val IS_PIECE=IS_PIECE_MASK * YES
	val NO_PIECE=IS_PIECE_MASK * NO

	val WHITE=COLOR_MASK * YES
	val BLACK=COLOR_MASK * NO

	val IS_SLIDING=IS_SLIDING_MASK
	val IS_STRAIGHT=IS_STRAIGHT_MASK
	val IS_DIAGONAL=IS_DIAGONAL_MASK
	val IS_SINGLE=IS_SINGLE_MASK
	val IS_KNIGHT=IS_KNIGHT_MASK
	val IS_PAWN=IS_PAWN_MASK

	val KING=IS_PIECE|IS_STRAIGHT|IS_DIAGONAL|IS_SINGLE
	val QUEEN=IS_PIECE|IS_STRAIGHT|IS_DIAGONAL|IS_SLIDING
	val ROOK=IS_PIECE|IS_STRAIGHT|IS_SLIDING
	val BISHOP=IS_PIECE|IS_DIAGONAL|IS_SLIDING
	val KNIGHT=IS_PIECE|IS_KNIGHT|IS_SINGLE
	val PAWN=IS_PIECE|IS_PAWN|IS_SINGLE

	val EMPTY=NO_PIECE

	def pieceColor(p:TPiece):TColor=(p & COLOR_MASK)
	def colorOf(p:TPiece):TColor=pieceColor(p)

	def isWhite(p:TPiece):Boolean=(pieceColor(p)==WHITE)
	def isBlack(p:TPiece):Boolean=(pieceColor(p)==BLACK)

	def isStraight(p:TPiece):Boolean=((p & IS_STRAIGHT_MASK)==IS_STRAIGHT)
	def isDiagonal(p:TPiece):Boolean=((p & IS_DIAGONAL_MASK)==IS_DIAGONAL)
	def isSingle(p:TPiece):Boolean=((p & IS_SINGLE_MASK)==IS_SINGLE)
	def isSliding(p:TPiece):Boolean=((p & IS_SLIDING_MASK)==IS_SLIDING)
	def isKnight(p:TPiece):Boolean=((p & IS_KNIGHT)==IS_KNIGHT)
	def isPawn(p:TPiece):Boolean=((p & IS_PAWN)==IS_PAWN)

	def toColor(p:TPiece,c:TColor):TPiece=(p & ~COLOR_MASK)|c
	def toWhite(p:TPiece):TPiece=toColor(p,WHITE)
	def toBlack(p:TPiece):TPiece=toColor(p,BLACK)
	def toInvColor(p:TPiece):TPiece=if(p==NO_PIECE) NO_PIECE else if(colorOf(p)==WHITE) toBlack(p) else toWhite(p)

	def invertColor(c:TColor):TColor=if(c==WHITE) BLACK else WHITE
	def inverseColorOf(c:TColor):TColor=invertColor(c)
	def inverseTurnOf(c:TColor):TColor=invertColor(c)

	def colorNameOf(c:TColor):String=if(c==WHITE) "white" else "black"
	def colorLetterOf(c:TColor):Char=if(c==WHITE) 'w' else 'b'

	def pieceType(p:TPiece):TPiece=(p & PIECE_TYPE_MASK)
	def typeOf(p:TPiece):TPiece=pieceType(p)

	def toFenChar(p:TPiece):Char=
	{

		val c=pieceType(p) match
		{
			case KING => 'k'
			case QUEEN => 'q'
			case ROOK => 'r'
			case BISHOP => 'b'
			case KNIGHT => 'n'
			case PAWN => 'p'
			case _ => ' '
		}

		if(c==' ') return ' '

		if(isBlack(p)) return c

		c.toUpper
	}

	def toPieceChar(p:TPiece):Char=toFenChar(p).toUpper

	def fromFenChar(c:Char):TPiece=
	{

		val cu=c.toUpper

		val pt=cu match
		{
			case 'K' => KING
			case 'Q' => QUEEN
			case 'R' => ROOK
			case 'B' => BISHOP
			case 'N' => KNIGHT
			case 'P' => PAWN
			case _ => NO_PIECE
		}

		if(pt==NO_PIECE) return NO_PIECE

		if(c==cu) return toWhite(pt)

		toBlack(pt)

	}

}