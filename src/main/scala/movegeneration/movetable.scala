package smartchess

import piece._
import square._
import move._
import board._

object movetable
{

	// generate move table

	type TMoveTablePtr=Tuple2[TSquare,TPiece]

	var move_table_ptrs=Map[TMoveTablePtr,Int]()

	var move_table=scala.collection.mutable.ArrayBuffer[move]()

	var move_table_ptr=0

	def init(v:String)
	{

		move_table_ptrs=Map[TMoveTablePtr,Int]()

		move_table=scala.collection.mutable.ArrayBuffer[move]()

		move_table_ptr=0

		for( f<-(0 to BOARD_SIZE-1) ; r<-(0 to BOARD_SIZE-1) )
		{
			
			val sq=fromFileRank(f,r)

			for(pt<-List(BLACK|PAWN,WHITE|PAWN,KNIGHT,KING,QUEEN,ROOK,BISHOP))
			{

				if(isPawn(pt))
				{
					move_table_ptrs+=(Tuple2(sq,pt) -> move_table_ptr)
				}
				else
				{
					move_table_ptrs+=(Tuple2(sq,toBlack(pt)) -> move_table_ptr)
					move_table_ptrs+=(Tuple2(sq,toWhite(pt)) -> move_table_ptr)
				}

				for( i<-(-2 to 2) ; j<-(-2 to 2) )
				{

					var vector_ok=false

					val sumabs=Math.abs(i)+Math.abs(j)
					val prodabs=Math.abs(i*j)

					var set_pawn_capture=false
					var set_pawn_promotion=false
					var set_pawn_push=false
					var set_pawn_double_push=false
					var set_pawn_passing_square=NO_SQUARE
					var set_ep_clear_square=NO_SQUARE

					if(isPawn(pt))
					{
						if(j*pawnDir(pt)>0)
						{
							if(Math.abs(j)< 2)
							{
								if(Math.abs(i)< 2)
								{
									vector_ok=true

									if(i==0)
									{
										set_pawn_push=true
									}
									else
									{
										set_pawn_capture=true
									}

									if(pawnDistFromProm(pt,sq)==1)
									{
										set_pawn_promotion=true
									}
								}
							}
							else if(i==0)
							{
								if(pawnDistFromBase(pt,sq)< 2)
								{
									set_pawn_push=true
									set_pawn_double_push=true
									vector_ok=true
								}
							}
						}
					}
					if(isKnight(pt))
					{

						if(Math.abs(i*j)==2)
						{
							vector_ok=true
						}

					}
					else if( (Math.abs(i) < 2)&&(Math.abs(j) < 2) )
					{

						if(isStraight(pt))
						{
							if((sumabs>0) && (i*j==0)) vector_ok=true
						}

						if(isDiagonal(pt))
						{
							if(prodabs==1) vector_ok=true
						}

					}

					val vector_start=move_table_ptr

					if(vector_ok)
					{
						var cf=f+i
						var cr=r+j

						var ok=fileRankOk(cf,cr)

						while(ok)
						{

							val plist=
								if(set_pawn_promotion)
									if(v=="Antichess") List(QUEEN,ROOK,BISHOP,KNIGHT,KING)
										else List(QUEEN,ROOK,BISHOP,KNIGHT)
								else
									List(NO_PIECE)

							if(set_pawn_double_push)
							{
								set_pawn_passing_square=fromFileRank(cf,r+j/2)
							}

							if(set_pawn_capture)
							{
								set_ep_clear_square=fromFileRank(cf,r)
							}

							for(set_prom_piece<-plist)
							{

								move_table+=move(
									from_piece=pt,
									from=fromFileRank(f,r),
									to=fromFileRank(cf,cr),
									pawn_capture=set_pawn_capture,
									pawn_promotion=set_pawn_promotion,
									prom_piece=set_prom_piece,
									pawn_push=set_pawn_push,
									pawn_double_push=set_pawn_double_push,
									pawn_passing_square=set_pawn_passing_square,
									capture=set_pawn_capture,
									ep_clear_square=set_ep_clear_square
								)

								move_table_ptr+=1

							}

							cf=cf+i
							cr=cr+j

							ok=fileRankOk(cf,cr)

							if(isSingle(pt)) ok=false

							if(!ok)
							{
								move_table(move_table_ptr-1).end_vector=true
							}
						}
					}

					for(ptr<-(vector_start to (move_table_ptr-1)))
					{
						move_table(ptr).next_vector=move_table_ptr
					}

				}

				move_table+=move(end_piece=true)

				move_table_ptr+=1

			}
		}

	}

	/*var ptr=(-1)

	saveTxt( "movetable.txt" , ( for(m<-move_table) yield {ptr+=1;ptr+" - "+m.info} ).toList.mkString("\n") )*/

}