package smartchess

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

import square._
import piece._
import move._
import board._

import Resource._

////////////////////////////////////////////////////////////////////
// class GuiBoard
////////////////////////////////////////////////////////////////////

// GuiBoard is a MyComponent that is a drag and drop move input chess board

class GuiBoard() extends MyComponent
{
	import GuiBoard._

	// root hbox is immutable, serves as the JavaFX node of the widget
	val rooth:HBox=new HBox()

	// board is immutable, represents the position displayed by guiboard
	val b:board=new board

	// reset it only upon construction so that it holds a valid position
	b.reset

	// everything else can change
	var root:Group=null
	var piece_size:Int=0
	var piece_factor:Double=0.0
	var padding_factor:Double=0.0
	var piece_padding:Int=0
	var fontpath:String=null
	var chess_font:Font=null
	var small_chess_font:Font=null
	var material:String=null
	var controlsdisabled:Boolean=false
	var board_size:Int=0
	var margin:Int=0
	var showcoords:Boolean=false
	var canvas_size:Int=0
	var canvas_width:Int=0
	var canvas_height:Int=0
	var translits:Map[Int,Map[Char,Char]]=null
	var flip:Boolean=false
	var setup:Boolean=false
	var setup_piece=NO_PIECE
	var board_canvas_handler:myMouseEventHandler=null
	var HIGHLIGHT_COLOR:Color=null
	var ENGINE_HIGHLIGHT_COLOR:Color=null
	var ENGINE_SCORE_POS_COLOR:Color=null
	var ENGINE_SCORE_NEG_COLOR:Color=null
	var PIECE_COLORS:Map[Int,Color]=null
	var SQUARE_COLORS:Map[Int,Color]=null
	var color_mappings:Map[String,Int]=null
	var board_opacity:Double=0.0
	var layers:Map[String,Layer]=null
	var layers_list:List[String]=null
	var piece_size_id:String=null
	var piece_factor_id:String=null
	var font_id:String=null
	var material_id:String=null
	var opacity_id:String=null
	var demo:Boolean=false

	def Build
	{		
		root=new Group()		
		setup=GB("setup",false)
		demo=GB("demo",false)
		flip=Builder.GB("{settings}#{flip}",false)
		if(demo) flip=GB("flip",false)
		piece_size_id=Path.AddToId("{components}",GetId,"{piecesize}")
		piece_size=Builder.GD(piece_size_id,PIECESIZE).toInt		
		if(setup) piece_size=(piece_size*8)/10
		if(piece_size< 20) piece_size=20
		if(demo) piece_size=GI("piecesize",20)
		piece_factor_id=Path.AddToId("{components}",GetId,"{piecefactor}")
		piece_factor=Builder.GD(piece_factor_id,PIECEFACTOR)
		padding_factor=(1-piece_factor)/2
		piece_padding=(padding_factor*piece_size).toInt
		font_id=Path.AddToId("{components}",GetId,"{font}#{selected}")
		fontpath="fonts/"+Builder.GS(font_id,FONT)+".TTF"
		chess_font=Font.loadFont(Resource.asStream(fontpath),piece_size*piece_factor)
		small_chess_font=Font.loadFont(Resource.asStream(fontpath),22)
		material_id=Path.AddToId("{components}",GetId,"{material}#{selected}")
		material=Builder.GS(material_id,MATERIAL)
		controlsdisabled=false
		board_size=piece_size*BOARD_SIZE
		margin=Builder.GS("{components}#{boardmargin}","30.0").toDouble.toInt
		if(demo) margin=2
		showcoords=Builder.GS("{components}#{showcoords}","true").toBoolean
		if(demo) showcoords=false
		canvas_size=board_size+2*margin
		canvas_width=canvas_size
		canvas_height=canvas_size	
		translits=Map(DARK->translit_dark,LIGHT->translit_light)
		setup_piece=NO_PIECE
		board_canvas_handler=new myMouseEventHandler

		HIGHLIGHT_COLOR=MyComponent.HexToColor("#FFFF00")
		ENGINE_HIGHLIGHT_COLOR=MyComponent.HexToColor("#7F7FFF")
		ENGINE_SCORE_POS_COLOR=MyComponent.HexToColor("#00FF00")
		ENGINE_SCORE_NEG_COLOR=MyComponent.HexToColor("#FF0000")

		PIECE_COLORS=Map(
			WHITE->MyComponent.HexToColor("#FFFFFF"),
			BLACK->MyComponent.HexToColor("#000000"))

		SQUARE_COLORS=Map(
			LIGHT->MyComponent.HexToColor(LIGHTSQUARECOLOR),
			DARK->MyComponent.HexToColor(DARKSQUARECOLOR))

		color_mappings=Map(
			"whitepiececolor"->WHITE,
			"blackpiececolor"->BLACK,
			"lightsquarecolor"->LIGHT,
			"darksquarecolor"->DARK
		)

		opacity_id=Path.AddToId("{components}",GetId,"{boardopacity}")
		board_opacity=Builder.GD(opacity_id,BOARDOPACITY)

		layers=Map(
				"boardsq"->Layer(board_opacity),
				"board"->Layer(1.0),
				"boardcoord"->Layer(1.0),
				"highlight"->Layer(0.35),
				"engine"->Layer(0.35),
				"enginearrow"->Layer(1.0),
				"enginescore"->Layer(1.0),
				"drag"->Layer(1.0)
			)

		layers_list=List("boardsq","board","boardcoord","highlight","engine","enginearrow","enginescore","drag")

		if(demo)
		{
			layers=Map(
				"boardsq"->Layer(board_opacity),
				"board"->Layer(1.0)
			)

			layers_list=List("boardsq","board")
		}

		if(!demo)
		{
			root.setOnMouseDragged(board_canvas_handler)
		    root.setOnMouseClicked(board_canvas_handler)
		    root.setOnMouseReleased(board_canvas_handler)
		}

	    rooth.setBackground(new Background(new BackgroundImage(
				new Image(Resource.asStream(s"backgroundimages/$material.jpg")),
				BackgroundRepeat.REPEAT,BackgroundRepeat.REPEAT,BackgroundPosition.CENTER,BackgroundSize.DEFAULT
				)))

	    rooth.getChildren().clear()
		rooth.getChildren().add(root)

		if(setup)
		{
			val pvbox=new VBox(3)
			pvbox.setPadding(new Insets(50,25,5,5))
			def change_piece(p:Char)()
			{
				setup_piece=fromFenChar(p)
			}

			for(c<-List('K','Q','R','B','N','P'))
			{
				val phbox=new HBox(3)
				val bw=new MyCallbackButton(""+translit_light(c),change_piece(c))
				bw.setFont(small_chess_font)
				phbox.getChildren().add(bw)
				val bb=new MyCallbackButton(""+translit_light(c.toLower),change_piece(c.toLower))
				bb.setFont(small_chess_font)
				phbox.getChildren().add(bb)
				pvbox.getChildren().add(phbox)
			}

			rooth.getChildren().add(pvbox)
			
		}

		for((k,v)<-color_mappings)
		{
			val pathstr=Path.AddToId("{components}",GetId,s"{$k}")
			if(pathstr!=null) if(Builder.G(pathstr)!=null)
			{
				if(k.contains("piece"))
				{
					PIECE_COLORS+=(v->MyComponent.HexToColor(Builder.GS(pathstr)))
				}			
				else
				{
					SQUARE_COLORS+=(v->MyComponent.HexToColor(Builder.GS(pathstr)))
				}
			}
		}

		for(name<-layers_list) root.getChildren.add(layers(name).canvas)

		if((demo)&&HasAttribute("fen")) b.set_from_fen(GS("fen"))

		draw_board
	}

	def OwnCreateNode:Node=
	{
		Build

		node=rooth

		node
	}

	def OwnReportPrintable(level:Int=0,buff:String=""):String="guiboard"

	def DisableControls {controlsdisabled=true}

	def EnableControls {controlsdisabled=false}

	def SetFlip(flip:Boolean)
	{
		this.flip=flip
		draw_board
	}

	case class Layer(opacity:Double=1.0)
	{
		val canvas=new Canvas(canvas_width,canvas_height)

		canvas.setOpacity(opacity)

		val gc=canvas.getGraphicsContext2D()
	}

    def true_index(index:Int):Int=
    {
        if(flip) (7-index) else index
    }
    
    def true_index_inv(index:Int):Int=
    {
        (7-true_index(index))
    }
    
    def px_to_index(px:Int):Int=
    {
        true_index((px-margin)/piece_size)
    }

    def manual_move_made(m:move)
    {

    	val san=b.toSan(m)

    	b.makeMove(m)

        draw_board

        Fire("manual move made",san)

    }

	class myMouseEventHandler extends EventHandler[MouseEvent]
	{

		var is_drag_going:Boolean=false
	    var drag_from:TSquare=NO_SQUARE
	    var drag_to:TSquare=NO_SQUARE
	    var drag_dx:Int=0
	    var drag_dy:Int=0
	    var drag_piece:TPiece=NO_PIECE
	    var mouse_released:Boolean=false
	    var dragreleased:Boolean=false

		def handle(mouseEvent:MouseEvent)
		{
			var x:Int=mouseEvent.getX().toInt
            var y:Int=mouseEvent.getY().toInt
            var etype:String=mouseEvent.getEventType().toString()

            if(controlsdisabled) return            

            if(etype=="MOUSE_RELEASED")
            {

                if(is_drag_going)
                {

                	var do_draw=true
                    
                    is_drag_going=false

                    b.rep(drag_from)=drag_piece
                    
                    drag_to=fromFileRank(px_to_index(x),px_to_index(y))

                    dragreleased= ( drag_to != drag_from )
                    
                    if(drag_to!=NO_SQUARE)
                    {
                        
                        if(setup)
                        {
                            
                            if(drag_from!=drag_to)
                            {
                    
                                mouse_released=true

                                b.rep(drag_from)=NO_PIECE
                                b.rep(drag_to)=drag_piece

                                draw_board
                            
                            }
                            
                        }
                        else
                        {
                            
                            var m=move(from=drag_from,to=drag_to)

                            def is_legal(m:move):Boolean=
                            {

                            	b.initMoveGen

	                            var legal:Boolean=false

	                            while(b.nextLegalMove()&&(!legal))
	                            {
	                            	if(
	                            		(b.current_move.from==m.from)&&
	                            		(b.current_move.to==m.to)&&
	                            		(b.current_move.prom_piece==m.prom_piece)
	                            	)
	                            	{
	                            		legal=true
	                            	}
	                            }

	                            legal

                        	}
                        
                            if(is_legal(m))
                            {
                                manual_move_made(m)
                            }
                            else
                            {

                                m=move(from=drag_from,to=drag_to,prom_piece=fromFenChar('q'))

                                if(is_legal(m))
                                {
                                	def handler(ev:MyEvent)
                                	{
                                		if(ev.Id=="{promqueen}")
                                		{
                                			m.prom_piece=fromFenChar('q')
                                			Builder.CloseStage("{promdialog}")
                                			manual_move_made(m)
                                		}

                                		if(ev.Id=="{promrook}")
                                		{
                                			m.prom_piece=fromFenChar('r')
                                			Builder.CloseStage("{promdialog}")
                                			manual_move_made(m)
                                		}

                                		if(ev.Id=="{prombishop}")
                                		{
                                			m.prom_piece=fromFenChar('b')
                                			Builder.CloseStage("{promdialog}")
                                			manual_move_made(m)
                                		}

                                		if(ev.Id=="{promknight}")
                                		{
                                			m.prom_piece=fromFenChar('n')
                                			Builder.CloseStage("{promdialog}")
                                			manual_move_made(m)
                                		}

                                		if(ev.Id=="{promking}")
                                		{
                                			m.prom_piece=fromFenChar('k')
                                			Builder.CloseStage("{promdialog}")
                                			manual_move_made(m)
                                		}
                                	}

                                	do_draw=false

                                	put_piece_xy("board",'?',translit_light,
                                		piece_cx(fileOf(drag_to))+piece_size/5,piece_cy(rankOf(drag_to))-piece_size/10,
                             			PIECE_COLORS(b.turn),new Font(piece_size),force_text="?")

                                	val promking=if(IS_ANTICHESS)
                                	"""<button width="250.0" style="-fx-font-size: 24px;" id="{promking}" text="King"/>""" else ""

                                	val blob=s"""
                                		|<vbox>
                                		|$promking
                                		|<button width="250.0" style="-fx-font-size: 24px;" id="{promqueen}" text="Queen"/>
                                		|<button width="250.0" style="-fx-font-size: 24px;" id="{promrook}" text="Rook"/>
                                		|<button width="250.0" style="-fx-font-size: 24px;" id="{prombishop}" text="Bishop"/>
                                		|<button width="250.0" style="-fx-font-size: 24px;" id="{promknight}" text="Knight"/>
                                		|</vbox>
                                	""".stripMargin
                                    Builder.MyStage("{promdialog}","Promotion",blob,handler=handler,
                                    	modal=true,unclosable=true,usewidth=false,useheight=false)

                                }

                            }

                        }
                        
                    }
                    else
                    {
                        if(setup)
                        {
                            mouse_released=true
                            
                            b.rep(drag_from)=NO_PIECE
                        }
                        else
                        {
                            println("out of board")
                        }
                    }

                    layers("drag").gc.clearRect(0, 0, canvas_size, canvas_size)
                    
                    if(do_draw) draw_board
                    
                }
            }
            
            if(etype=="MOUSE_DRAGGED")
            {
                
                if(!is_drag_going)
                {
                	val xi=px_to_index(x)
                	val yi=px_to_index(y)

                	if(fileRankOk(xi,yi))
                	{

	                    is_drag_going=true
	                    
	                    drag_from=fromFileRank(xi,yi)

	                    drag_piece=b.rep(drag_from)
	                    
	                    b.rep(drag_from)=NO_PIECE

	                    draw_board
	                    
	                    drag_dx=piece_cx(fileOf(drag_from))-x
	                    drag_dy=piece_cy(rankOf(drag_from))-y

                	}
                    
                }
                else
                {
                    
                    layers("drag").gc.clearRect(0, 0, canvas_size, canvas_size)
         
                    put_piece_xy("drag",toBlack(drag_piece),translit_light,x+drag_dx,y+drag_dy,PIECE_COLORS(colorOf(drag_piece)))
                    
                }
                
            }
            
            if(etype=="MOUSE_CLICKED")
            {

                if(mouse_released)
                {
                    mouse_released=false
                }
                else if(setup)
                {

                	val xi=px_to_index(x)
                	val yi=px_to_index(y)

                	if(fileRankOk(xi,yi))
                	{
                    
	                    val clicked_sq=fromFileRank(xi,yi)
	                    
	                    if((b.rep(clicked_sq)==setup_piece)||(mouseEvent.getButton()==MouseButton.SECONDARY))
	                    {
	                        b.rep(clicked_sq)=NO_PIECE
	                    }
	                    else
	                    {
	                        b.rep(clicked_sq)=setup_piece
	                    }

                	}
                    
                    draw_board
                }
                else if(!dragreleased)
                {                	
                	Fire("board clicked",""+((x-margin)/piece_size))
                }

                dragreleased=false

            }


		}
	}

	def put_piece_xy(l:String,p:TPiece,m:Map[Char,Char],x:Int,y:Int,c:Color,set_font:Font=chess_font,force_text:String="")
	{
		layers(l).gc.setFont(set_font)
		layers(l).gc.setFill(c)
		val text=if(force_text!="") force_text else ""+m(toFenChar(p))
		layers(l).gc.fillText(text,x,y)
	}

	def piece_cx(f:TFile):Int=true_index(f)*piece_size+piece_padding+margin

	def piece_cy(r:TRank):Int=true_index(r)*piece_size+piece_size-piece_padding+margin

	def set_from_fen(fen:String)
	{
		b.set_from_fen(fen)
		draw_board
	}

	def highlight_square(sq:TSquare,layer:String="highlight",col:Color=HIGHLIGHT_COLOR)
	{
		val f=true_index(fileOf(sq))
		val r=true_index(rankOf(sq))

		layers(layer).gc.setFill(col)
		layers(layer).gc.fillRoundRect(
			f*piece_size+piece_padding+margin,
			r*piece_size+piece_padding+margin,
			piece_size-2*piece_padding,
			piece_size-2*piece_padding,
			piece_size/2,
			piece_size/2
			)
	}

	def highlight_move(m:move)
	{
		highlight_square(m.from)
		highlight_square(m.to)
	}

	def clear_engine
	{
		layers("engine").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def highlight_engine_move(pv:String,score:Int=0)
	{

		clear_engine

		val parts=pv.split(" ")
		val algeb=parts(0)
		if(algeb.length< 4) return

		val algebparts=algeb.grouped(2).toArray

		val from=fromAlgeb(algebparts(0))
		val to=fromAlgeb(algebparts(1))

		var prom_piece=NO_PIECE

		if(algeb.length>4) prom_piece=fromFenChar(algeb(4))

		val m=move(from=from,to=to,prom_piece=prom_piece)

		highlight_square(m.from,"engine",ENGINE_HIGHLIGHT_COLOR)
		highlight_square(m.to,"engine",ENGINE_HIGHLIGHT_COLOR)

		val eagc=layers("enginearrow").gc

		val strokecol=scoreColorOf(score)
		val fillcol=scoreColorOf(score)

		eagc.setStroke(strokecol)
		eagc.setFill(fillcol)
		eagc.setLineWidth(6)

		val fromx=true_index(fileOf(from))*piece_size+piece_size/2+margin
		val fromy=true_index(rankOf(from))*piece_size+piece_size/2+margin

		val tox=true_index(fileOf(to))*piece_size+piece_size/2+margin
		val toy=true_index(rankOf(to))*piece_size+piece_size/2+margin

		eagc.strokeLine(
			fromx,
			fromy,
			tox,
			toy
			)

		eagc.fillOval(tox-9,toy-9,18,18)

	}

	def clear_score
	{
		layers("enginescore").gc.clearRect(0,0,canvas_size,canvas_size)
		layers("enginearrow").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def scoreColorOf(score:Int):Color=
	{
		if(score>=0) ENGINE_SCORE_POS_COLOR else ENGINE_SCORE_NEG_COLOR
	}

	def print_score(score:Int)
	{
		clear_score
		val esgc=layers("enginescore").gc
		esgc.setFill(scoreColorOf(score))
		esgc.setStroke(scoreColorOf(score))
		esgc.setLineWidth(4)
		esgc.setFont(new Font("Courier New",120))
		esgc.strokeText((if(score>0) "+" else "")+score,100,220)
	}

	def clear_highlight
	{
		layers("highlight").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def draw_board
	{

		layers("boardsq").gc.clearRect(0,0,canvas_size,canvas_size)
		if(!demo) layers("boardcoord").gc.clearRect(0,0,canvas_size,canvas_size)
		layers("board").gc.clearRect(0,0,canvas_size,canvas_size)

		if((showcoords)&&(!demo)) for(i<-0 to BOARD_SIZE-1)
		{
			layers("boardcoord").gc.setFont(new Font(margin/2))
			layers("boardcoord").gc.setFill(MyComponent.HexToColor("#000000"))

			val cx=true_index(i)*piece_size+margin+piece_size/2-margin/4
			val cy=true_index_inv(i)*piece_size+margin+piece_size/2+margin/4

			layers("boardcoord").gc.fillText(""+('a'+i).toChar,cx+margin/8,canvas_size-margin/3)
			layers("boardcoord").gc.fillText(""+('1'+i).toChar,margin/3,cy)
		}

		for(sq<-0 to BOARD_AREA-1)
		{
			val f=fileOf(sq)
			val r=rankOf(sq)

			val p=b.rep(sq)

			val ci=colorIndexOf(sq)

			val t=translits(ci)

			layers("boardsq").gc.setFill(SQUARE_COLORS(ci))
			layers("boardsq").gc.fillRect(f*piece_size+margin,r*piece_size+margin,piece_size,piece_size)

			put_piece_xy("board",toBlack(p),translit_light,piece_cx(f),piece_cy(r),PIECE_COLORS(colorOf(p)))
		}
	}

	def clear_board
	{
		set_from_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1")
	}

}

////////////////////////////////////////////////////////////////////
// object GuiBoard
////////////////////////////////////////////////////////////////////

// object GuiBoard holds default GuiBoard settings

object GuiBoard
{
	val PIECESIZE=55.0
	val BOARDOPACITY=0.2
	val PIECEFACTOR=0.8
	val WHITEPIECECOLOR="#FFFFFF"
	val BLACKPIECECOLOR="#000000"
	val LIGHTSQUARECOLOR="#CCCC33"
	val DARKSQUARECOLOR="#666600"
	val FONT="MERIFONTNEW"
	val MATERIAL="wood"

	var translit_light:Map[Char,Char]=Map[Char,Char](' '->' ',
		'P'->'p','N'->'n','B'->'b','R'->'r','Q'->'q','K'->'k',
		'p'->'o','n'->'m','b'->'v','r'->'t','q'->'w','k'->'l')

	var translit_dark:Map[Char,Char]=Map[Char,Char](' '->'+',
		'P'->'P','N'->'N','B'->'B','R'->'R','Q'->'Q','K'->'K',
		'p'->'O','n'->'M','b'->'V','r'->'T','q'->'W','k'->'L'
		)
}

