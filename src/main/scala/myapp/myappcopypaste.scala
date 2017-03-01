package smartchess

////////////////////////////////////////////////////////////////////

import javafx.stage._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import Builder._

////////////////////////////////////////////////////////////////////

object MyAppCopyPaste
{
	import MyApp._

	def GuiCopyFen
	{
		val fen=g.report_fen
		ClipboardSimple.clipset(fen)
		MyActor.Log("copied to clipboard: "+fen)
	}

	def GuiCopyPgn
	{
		val pgn=g.report_pgn
		ClipboardSimple.clipset(pgn)
		MyActor.Log("copied to clipboard: "+pgn)
	}

	def GuiCopyCurrentLine
	{
		val currentline=g.current_line_pgn
		ClipboardSimple.clipset(currentline)
		MyActor.Log("copied to clipboard: "+currentline)
	}

	def GuiCopyCurrentLineAlgeb
	{
		val currentlinealgeb=g.current_line_algeb
		ClipboardSimple.clipset(currentlinealgeb)
		MyActor.Log("copied to clipboard: "+currentlinealgeb)
	}

	def GuiPasteFen
	{
		val fen=ClipboardSimple.clipget
		g.set_from_fen(fen)
		MyActor.Log("pasted from clipboard: "+fen)

		Update
	}

	def PastePgn
	{
		val pgn=ClipboardSimple.clipget

		g.set_from_pgn(pgn)

		MyActor.Log("pasted from clipboard: "+pgn)

		CheckProfile

		Update
	}

	
}