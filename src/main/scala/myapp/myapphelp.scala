package smartchess

object Help
{
	val game_browser_navigation=List(
		TextItem(s"""
			|You have navigation buttons
			|(to begin, fast back, back, forward, fast forward, to end) to navigate between pages.
			|You can sort the games by a given criterion by selecting it from the "Sort by" combo then
			|pressing the "Sort" button. You can also search for a term in among the headers of the PGNs by typing 
			|it int the "Search for" textfield and then clicking on the "Search" button. To open a game simply
			|click on it.
		""".stripMargin)
	)

	def helptopics=Document(
		"helptopics",
		title="Help topics",		
		chapters=List(
			Chapter(
				title="Home",
				paragraphs=List(
					Paragraph(
						title="Introduction",
						ditems=List(
							TextItem(s"""
								|Smartchess is a chess graphical user interface written in the Scala language 
								|as an sbt project. It supports a wide range of variants and has automatic
								|book building capability.
							""".stripMargin),
							ImageItem("mainwindow.PNG",width=400)
						)
					),
					Paragraph(
						title="Download",
						ditems=List(
							TextItem(s"""
								|To download the program on the <a href="https://github.com/smartchessguiapp/smartchess">
								|GitHub repository's main page ( https://github.com/smartchessguiapp/smartchess )</a> 
								|click on "Clone or download", then on "Download ZIP" :
							""".stripMargin),
							ImageItem("download.PNG")
						)
					),
					Paragraph(
						title="Getting started",
						ditems=List(
							TextItem(s"""
								|Unzip the downloaded zip file. The application lives in the directory "smartchess-master".
							""".stripMargin)
						)
					),
					Paragraph(
						title="Running the program",
						ditems=List(
							TextItem(s"""
								|Make sure that the <b>latest version of Java</b> ( to be precise: JRE - Java Runtime Environment )
								|<b>is installed on your computer</b>.
							""".stripMargin),
							TextItem(s"""
								|In the applications directory <b>double click</b> on <b>smartchess.jar</b>.
							""".stripMargin),
							TextItem(s"""
								|Alternatively you can open a system console window ( command prompt window ), navigate ( cd )
								|to the application's directory and issue the command : <br>"java -jar smartchess.jar".
							""".stripMargin)
						)
					),
					Paragraph(
						title="Platform",
						ditems=List(
							TextItem(s"""
								|The program has been developed under, optimized for and tested on the Windows 64 bit platform,
								|however it should run on any platform that has the Java Runtime Environment.
							""".stripMargin)
						)
					)
				)
			),			
			Chapter(
				title="File menu",
				paragraphs=List(
					Paragraph(
						title="Open PGN",
						ditems=List(
							TextItem(s"""
								|Opens a PGN file.
							""".stripMargin),
							CTextItem(
								items=List(
									RawText(s"""
										|If you want the application to automatically switch to the PGN tab upon opening a PGN, 
										|from the "Settings" menu choose
									""".stripMargin
									),
									AppLink(
										"Profile",
										evkind="menuitem clicked",
										evid="{profilesettingsmenu}",
										evvalue="Profile"
									),
									RawText(s"""
										|and check "Auto select PGN tab".
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Open multiple game PGN",
						ditems=List(
							TextItem(s"""
								|Opens a multiple game PGN.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Save PGN as",
						ditems=List(
							TextItem(s"""
								|Saves the PGN file with a given name.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Open PGN list",
						ditems=List(
							TextItem(s"""
								|Opens a PGN list. PGN list is smartchess's answer to store multiple PGNs in one file.
								|For every game also the current line and the board flip information is saved in order 
								|that you can identify the game just by looking at the position and start analyzing the game
								|from the position of interest and from the viewpoint you have chosen.
							""".stripMargin)
						)
					),
					Paragraph(
						title="New PGN list",
						ditems=List(
							TextItem(s"""
								|Creates a new PGN list.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Save PGN list as",
						ditems=List(
							TextItem(s"""
								|Saves the PGN list with a given name. Note that PGN lists are always kept up to date on the disk
								|so this is only necessary if you want to save a PGN list with a different name.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Copy menu",
				paragraphs=List(
					Paragraph(
						title="Copy FEN to clipboard",
						ditems=List(
							TextItem(s"""
								|Copies the FEN of the current position to the clipboard.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Copy PGN to clipboard",
						ditems=List(
							TextItem(s"""
								|Copies the PGN of the current game to the clipboard.
								""".stripMargin)
						)
					),
					Paragraph(
						title="Copy current line",
						ditems=List(
							TextItem(s"""
								|Copies the the current line in SAN notation to the clipboard.
								""".stripMargin)
						)
					),
					Paragraph(
						title="Copy current line algeb",
						ditems=List(
							TextItem(s"""
								|Copies the the current line in algebraic notation to the clipboard.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Paste menu",
				paragraphs=List(					
					Paragraph(
						title="Paste - Paste FEN from clipboard",
						ditems=List(
							TextItem(s"""
								|Sets up the game from the FEN on the clipboard. All move information will be lost.
							""".stripMargin
							)
						)
					),					
					Paragraph(
						title="Paste - Paste PGN from clipboard",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
										|Sets up the game from the PGN on the clipboard. See also :
									""".stripMargin
									),
									DocLink(
										"Open PGN",
										chaptertitle="File menu",
										paragraphtitle="Open PGN"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					)
				)
			),
			Chapter(
				title="Tools menu",
				paragraphs=List(
					Paragraph(
						title="Sync lichess book",
						ditems=List(
							TextItem(s"""
								|If you have synchronized a lichess PGN, then you can use this tool to create a white and a black book from it.
								|The games will be filtered using the Filter PGN settings.
								|The generated books will be called [handle]_white and [handle]_black, where handle is the lichess handle set in Profile.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Rating charts",
						ditems=List(
							TextItem(s"""
								|Compiles rating histories and draws candlestick charts using a bulk PGN
								|containing a player's games.
								|The games in the PGN are supposed to be ordered by date, particulary the 
								|most recent game should come first.
							""".stripMargin),
							TextItem(s"""
								|<b>PGN file</b> lets you select the absolute path of the bulk PGN.
							""".stripMargin),
							TextItem(s"""
								|<b>Create charts</b> lets you create the charts. This operation may take time
								|if the PGN file is large. You can abort this operation.
							""".stripMargin),
							TextItem(s"""
								|<b>Rating category combo</b> is created based on the rating categories ( which can be
								|variants or time controls ) and is sorted according to the number of games in each category.
							""".stripMargin),
							TextItem(s"""
								|<b>Number of playing days combo</b> lets you select how many of the latest playing days is
								|presented in the chart. Setting it to ALL will include all playing days.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Show auxiliary books",
						ditems=List(
							TextItem(s"""
								|In addition to the currently selected book shown in the Book tab, show additional books
								|in separate windows. Auxiliary books are view only, the usual editing operations don't 
								|work for them.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Close auxiliary books",
						ditems=List(
							TextItem(s"""
								|Closes any open auxiliary book.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Add PGN games to book",
						ditems=List(
							TextItem(s"""
								|Adds all games that are listed in PGN games tab to the book.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Filter PGN",
						ditems=List(
							TextItem(s"""
								|Filters a PGN files based on various criteria. A mutliple game PGN will be created 
								|containing only those games that satisfy the filtering conditions.
							""".stripMargin),
							TextItem(s"""
								|<b>Inpute file name</b> specifies the multiple game PGN that is to be filtered.
							""".stripMargin),
							TextItem(s"""
								|<b>Output file name</b> specifies the name of the file the will be created. This file
								|will always be located in the "stuff/[Current Variant]/filtered" directory. It will have
								|an ".xml" extension which is added automatically, so you only need to type in the name.
							""".stripMargin),
							TextItem(s"""
								|<b>Player white</b> if specified searches for games with that white player.
							""".stripMargin),
							TextItem(s"""
								|<b>Player black</b> if specified searches for games with that black player.
							""".stripMargin),
							TextItem(s"""
								|<b>Search</b> performs the search.
							""".stripMargin),
							TextItem(s"""
								|<b>Handle white</b> will set up a search in which player white is the handle set in Settings - Profile.
							""".stripMargin),
							TextItem(s"""
								|<b>Handle black</b> will set up a search in which player black is the handle set in Settings - Profile.
							""".stripMargin),
							TextItem(s"""
								|<b>Ignore won</b> ignores games won by handle.
							""".stripMargin),
							TextItem(s"""
								|<b>Ignore lost</b> ignores games lost by handle.
							""".stripMargin),
							TextItem(s"""
								|<b>Ignore draw</b> ignores games drawn by handle.
							""".stripMargin),
							TextItem(s"""
								|<b></b>
							""".stripMargin),
							TextItem(s"""
								|<b>For all</b> if set the above ignore conditions apply to all games. In this case "won" refers to
								|result "1-0" ( white won ), lost to result "0-1" ( black won ) irrespective of who played the game.
							""".stripMargin),
							TextItem(s"""
								|<b>Rating</b> is the minimum average rating of the game.
							""".stripMargin),
							TextItem(s"""
								|<b>Max games</b> sets a limit to maximally how many games the filtered file may contain.
							""".stripMargin),
							TextItem(s"""
								|<b>Min plies</b> requires the game to have at least that many plies
								|( useful for ignoring aborted games for example ).
							""".stripMargin),
							TextItem(s"""
								|<b>Min time</b> refers to the minimum time control of the game is seconds ( increment is ignored ).
							""".stripMargin),
							TextItem(s"""
								|<b>Max time</b> refers to the maximum time control of the game is seconds ( increment is ignored ).
							""".stripMargin),
							TextItem(s"""
								|<b>Date from</b> specifies the starting date.
							""".stripMargin),
							TextItem(s"""
								|<b>Date to</b> specifies the finishing date.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Setup board manually",
						ditems=List(
							TextItem(s"""
								|Set up the board position manually. All move information will be lost.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Browse all games",
						ditems=List(
							TextItem(s"""
								|List in the PGN games tab all the games that has ever been opened by the program
								|in the current variant.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Add all games to PGN list",
						ditems=List(
							TextItem(s"""
								|Add all games listed in the PGN games tab to the currently open PGN list.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Delete current book",
						ditems=List(
							TextItem(s"""
								|Deletes the current book. This operation cannot be undone, so be careful.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Settings menu",
				paragraphs=List(
					Paragraph(
						title="Eval",
						ditems=List(
							TextItem(s"""
								|<b>Depth</b> sets the shallow search depth of the engine.
							""".stripMargin),
							TextItem(s"""
								|<b>Bonus</b> sets the bonus that is added to depth when performing a deep search.
							""".stripMargin),
							TextItem(s"""
								|<b>Color moves by score</b> when checked move evaluation by the engine or minimax will
								|annotate the move with the color coded annotation.
							""".stripMargin),
							TextItem(s"""
								|<b>Minimax depth</b> is the depth to which minimax should be performed.
							""".stripMargin),
							TextItem(s"""
								|<b>Play move factor</b> determines the bias toward the best move when in Train-Build mode.
								|100 means always the best move is played, 0 means all moves are played with equal chance.
							""".stripMargin),
							TextItem(s"""
								|<b>Play move limit</b> - moves valued less then this will not be played in Train-Build mode.
							""".stripMargin),
							TextItem(s"""
								|<b>Hint factor</b> determines the bias toward the best move giving a hint.
								|100 means always the best move is played, 0 means all moves are played with equal chance.
							""".stripMargin),
							TextItem(s"""
								|<b>Hint limit</b> - moves valued less then this will not be hinted.
							""".stripMargin),
							TextItem(s"""
								|<b>Search factor</b> determines the bias toward the best move when auto building book ( search ).
								|100 means always the best move is selected, 0 means all moves are selected with equal chance.
							""".stripMargin),
							TextItem(s"""
								|<b>Search depth bonus</b> is always added to search factor when going to the next depth.
							""".stripMargin),
							TextItem(s"""
								|<b>Minimax after</b> determines after adding how many nodes minimax will take place.
							""".stripMargin),
							TextItem(s"""
								|<b>Create minimax log</b> creates a log when checked.
							""".stripMargin),
							TextItem(s"""
								|<b>Use eval timeout</b> when checked eval timeout is applied.
							""".stripMargin),
							TextItem(s"""
								|<b>Eval timeout</b> when applied means that if going to the next depth at any depth requires 
								|more time than this, the evaluation of the move will be finished.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Book",
						ditems=List(
							TextItem(s"""
								|<b>Auto add move</b> when checked adds every move made on the board to the book.
							""".stripMargin),
							TextItem(s"""
								|<b>Auto inc plays</b> when checked increases play count for every move made on the board.
							""".stripMargin),
							TextItem(s"""
								|<b>Antichess dir</b> selects the directory which contains the Antichess solution.
							""".stripMargin),
							TextItem(s"""
								|<b>Max book depth</b> is the max depth to which games are added to the book.
							""".stripMargin),
							TextItem(s"""
								|<b>Check replica</b> if checked prevents a game already added to the book to be added again.
							""".stripMargin),
							TextItem(s"""
								|<b>Hide result stats</b> if checked hides result statistics.
							""".stripMargin),
							TextItem(s"""
								|<b>Show met by move</b> if checked shows the best response to a given move.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Board",
						ditems=List(
							TextItem(s"""
								|Board settings lets you control the piece size, chess font, piece and square color, material, opacity
								|and the relative size of pieces compared to a square ( piece factor ). For changes to take effect press
								|the "Apply changes" button. When applying changes the GUI will be restarted. Upon restart
								|some of the GUI's state, like that of the game browsers', may be forgotten 
								|( the current game however is always remembered ).
							""".stripMargin)
						)
					),
					Paragraph(
						title="Profile",
						ditems=List(
							TextItem(s"""
								|<b>My handle</b> lets you specify a handle ( or name ) which if appears in the PGN as player black
								|or player white the board will be flipped accordingly upon opening the PGN. This handle can also 
								|be used for PGN filtering purposes.
							""".stripMargin),
							TextItem(s"""
								|<b>Auto select PGN tab</b> when checked will auto switch to the PGN tab whenever a PGN is opened.
							""".stripMargin),
							TextItem(s"""
								|<b>PGN list page size</b> spcecifies how many games will be shown in the PGN list per page.
							""".stripMargin),
							TextItem(s"""
								|<b>Disable board click navigation</b> if checked disables back and forward navigation by
								|clicking on the left and right side of the board.
							""".stripMargin),
							TextItem(s"""
								|<b>Ok</b> for changes to take effect and close the window press Ok.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Reset all",
						ditems=List(
							TextItem(s"""
								|Resets all settings. All settings information will be lost, apply with care.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Help menu",
				paragraphs=List(
					Paragraph(
						title="Help topics",
						ditems=List(
							TextItem(s"""
								|Displays this help.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Create Wiki",
						ditems=List(
							TextItem(s"""
								|Creates Wiki files for the GitHub repository in MD format. This is just a developer's function.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Variant menu",
				paragraphs=List(
					Paragraph(
						title="List of variants",
						ditems=List(
							TextItem(s"""
								|Select the current variant of the application. Note that all variants have their own settings. 
								|All engines and windows of the current variant will be closed and those of the newly selected 
								|variant will be opened.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Book tab",
				paragraphs=List(					
					Paragraph(
						title="Paste PGN button",
						ditems=List(
							ImageItem("paste.png",path="icon"),
							CTextItem(
								items=List(
									RawText(s"""
										|Sets up the game from the PGN on the clipboard. See also :
									""".stripMargin
									),
									DocLink(
										"Open PGN",
										chaptertitle="File menu",
										paragraphtitle="Open PGN"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Book combo",
						ditems=List(
							ImageItem("bookcombo.PNG",path="web"),
							TextItem(s"""
								|Select current book.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Add book button",
						ditems=List(
							ImageItem("add.png",path="icon"),
							TextItem(s"""
								|Creates a new book and adds it to the book combo. The newly created book becomes the 
								|current one.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Save book button",
						ditems=List(
							ImageItem("savet.png",path="icon"),
							TextItem(s"""
								|Save the book to disk. Normally the book is only saved when switching to an other book or 
								|closing the program. Next to the Book "Save" button normally the number of positions in the 
								|book is shown. Upon saving book positions with no moves will be deleted. Right after saving 
								|instead of the number of positions the file size of the book will be shown here.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Look up solution button",
						ditems=List(
							ImageItem("lookupsolution.PNG",path="web"),
							CTextItem(
								items=List(
									RawText(s"""
										|Looks up the Antichess solution for the given position. For white the move will be annotated 
										|"!" for black the moves will be annoted "!?". To select the directory which contains the long  
										|lines of Antichess solution. See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Book",
										chaptertitle="Settings menu",
										paragraphtitle="Book"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Look up solution tree button",
						ditems=List(
							ImageItem("lookupsolutiontree.PNG",path="web"),
							TextItem(s"""
								|Looks up the Antichess solution tree for the given position. This means the all lines starting
								|from the given position present in the solution will become part of the book. Don't use this 
								|if the solution tree is very large ( when the position is close to the starting position ).
							""".stripMargin)
						)
					),
					Paragraph(
						title="Add move to book",
						ditems=List(
							ImageItem("addto.png",path="icon"),
							TextItem(s"""
								|Adds the current move to the book without annotation. The game will go back to the position 
								|in which the move was made and the book page of that position will be shown with this move 
								|added.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Annotate move buttons",
						ditems=List(
							ImageItem("annotmovebuttons.PNG",path="web"),
							TextItem(s"""
								|These buttons annotate the current move and don't go back to the previos position.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Delete all moves button",
						ditems=List(
							ImageItem("caution.png",path="icon"),
							TextItem(s"""
								|Deletes all moves from the book belonging to this position. Use it with care.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Search button",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
										|Starts automatic book building from the given position. See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Book board control buttons",
						ditems=List(
							ImageItem("bookboardcontrol.PNG",path="web"),
							CTextItem(
								items=List(
									RawText(s"""
										|These buttons behave the same way as the control buttons under the board 
										|( see also : 
									""".stripMargin),
									DocLink(
										"Board controls",
										chaptertitle="Board controls",
										paragraphtitle=""
									),
									RawText(s"""
										| ) except that the make analyzed move button also stores 
										|the move with its evaluation in the book.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Book store analyzed move button",
						ditems=List(
							ImageItem("goback.png",path="icon"),
							TextItem(s"""
								|This button stores the current best move of the engine, and stops the engine. The move 
								|will be shown among the book moves.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Add one deep move button",
						ditems=List(
							ImageItem("addonedeep.PNG",path="web"),
							CTextItem(
								items=List(
									RawText(s"""
										|Adds to the existing book the move considered best by the engine apart from the 
										|existing ones. The depth is Eval depth + Bonus depth. See also :
									""".stripMargin),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Add six shallow moves button",
						ditems=List(
							ImageItem("addsixshallow.PNG",path="web"),
							CTextItem(
								items=List(
									RawText(s"""
										|Adds to the existing book six moves considered best by the engine apart from the 
										|existing ones. The depth is Eval depth. See also : 
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Eval this button",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
											|Evaluates the current move with the engine. The depth is Eval depth + Bonus.
											|See also : 
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Reeval one and three buttons",
						ditems=List(
							ImageItem("reevalbuttons.PNG",path="web"),
							CTextItem(
								items=List(
									RawText(s"""
											|Reevaluate one or three moves respectively at Eval depth + Bonus depth.
											|Always the top move(s) in the book are reevaluated. See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Eval all button",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
											|Evaluates all the moves in the current position at Eval depth. 
											|Before evaluation all moves will be deleted.
											|See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Minimax button",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
											|Minimaxes the book to the current position.
											|See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Remove button",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
											|Removes all the positions from the book that a minimax operation would reach.
											|That is the subtree to minimax depth. See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Book moves",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
										|Below the book control panel the book moves belonging to the position are shown.
										|You can annotate, delete and comment each move separately. To annotate click on the 
										|annotation marks. To delete click on the X. To comment click on the comment. Antichess 
										|solution moves appear with the comment "solution" and with a different background color.
										|You can also set a priority value to moves from 0 to 10. Increase this with "up" and 
										|decrease it with "dn" or set it directly by clicking on the priority value. Priorities 
										|from 1 to 5 will remain in their place but marked. Priorities 6 to 10 will enjoy precedence 
										|above other move sorting criteria and will jump to the top. You can auto add moves made 
										|on the board and auto increase the number of plays for the given move. See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Book",
										chaptertitle="Settings menu",
										paragraphtitle="Book"
									),
									RawText(s"""
										| - Auto add move and Inc. plays checkboxes.
									""".stripMargin
									)
								)
							)
						)
					)
				)
			),
			Chapter(
				title="Board controls",
				paragraphs=List(
					Paragraph(
						title="Move input",
						ditems=List(
							CTextItem(
								items=List(
									RawText(s"""
										|You can use drag and drop style manual move input. 
										|Clicking on the right side of the board takes you one move forward in the game. 
										|Clicking on the left side takes you one move back. 
										|To disable click navigation use :
									""".stripMargin
									),
									DocLink(
										"Settings - Profile - Disable board click navigation",
										chaptertitle="Settings menu",
										paragraphtitle="Profile"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Flip board",
						ditems=List(
							ImageItem("flip.png",path="icon"),
							TextItem(s"""
								|Flips the board.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Reset",
						ditems=List(
							ImageItem("resett.png",path="icon"),
							TextItem(s"""
								|Resets the game to the starting position of the variant ( or a random starting position 
								|in the case of Chess960 ). All move information will be lost.
							""".stripMargin)
						)
					),
					Paragraph(
						title="To begin",
						ditems=List(
							ImageItem("begint.png",path="icon"),
							TextItem(s"""
								|Jumps to the beginning of the game.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Back",
						ditems=List(
							ImageItem("backt.png",path="icon"),
							TextItem(s"""
								|Goes back one move. The move won't be deleted.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Forward",
						ditems=List(
							ImageItem("forwardt.png",path="icon"),
							TextItem(s"""
								|Goes forward one move ( in the main variation ).
							""".stripMargin)
						)
					),
					Paragraph(
						title="To end",
						ditems=List(
							ImageItem("endt.png",path="icon"),
							TextItem(s"""
								|Jumps to the end of the game.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Delete",
						ditems=List(
							ImageItem("delt.png",path="icon"),
							TextItem(s"""
								|Deletes the current move. All lines following this move will be deleted as well.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Start engine",
						ditems=List(
							ImageItem("startt.png",path="icon"),
							TextItem(s"""
								|Starts the engine(s).
							""".stripMargin)
						)
					),
					Paragraph(
						title="Stop",
						ditems=List(
							ImageItem("stopt.png",path="icon"),
							TextItem(s"""
								|Stops the engine(s).
							""".stripMargin)
						)
					),
					Paragraph(
						title="Make analyzed move",
						ditems=List(
							ImageItem("maket.png",path="icon"),
							CTextItem(
								items=List(
									RawText(s"""
										|Make the current best move of the highest ranked running engine. See also:
									""".stripMargin
									),
									DocLink(
										"Engines tab - Engine rank",
										chaptertitle="Engines tab",
										paragraphtitle="Engine rank"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Train panel",
						ditems=List(
							ImageItem("trainpanel.PNG"),
							CTextItem(
								items=List(
									RawText(s"""
										|Selects training mode. To exit training uncheck the checkboxes or click on anywhere else 
										|on the train panel. Checking "Train" mode will let you play against the book. Antichess solution 
										|moves will be looked up. Checking "Build" mode will let you build a known mate tree. This means 
										|that the machine will try all moves of the mated side against you. If there is no book move for the
										|mating side, it searches for one and shows it to you for the first time. For the next time it
										|will be hidden. You can always look it up by switching to the "Book" tab. If both "Train" and "Build"
										|are checked, you can play against the machine. The extent to which the machine makes optimal 
										|moves can be set in "Settings - Eval - Play move factor" and "Settings - Eval - Play move limit".
										|See also :
									""".stripMargin
									),
									DocLink(
										"Settings - Eval",
										chaptertitle="Settings menu",
										paragraphtitle="Eval"
									),
									RawText(s"""
										|.
									""".stripMargin
									)
								)
							)
						)
					),
					Paragraph(
						title="Add to training positions",
						ditems=List(
							ImageItem("addto.png",path="icon"),
							TextItem(s"""
								|Adds position to training positions.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Delete from training positions",
						ditems=List(
							ImageItem("remove.png",path="icon"),
							TextItem(s"""
								|Deletes position from training positions.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Back to last training position",
						ditems=List(
							ImageItem("goback.png",path="icon"),
							TextItem(s"""
								|Goes back to the latest training position.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Random training position",
						ditems=List(
							ImageItem("roulette.png",path="icon"),
							TextItem(s"""
								|Sets up a random training position.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Anchor",
						ditems=List(
							ImageItem("anchor.png",path="icon"),
							TextItem(s"""
								|Creates an anchor to the current game position.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Jump to anchor",
						ditems=List(
							ImageItem("favourite.png",path="icon"),
							TextItem(s"""
								|Jumps to anchor position.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Hint",
						ditems=List(
							ImageItem("hint.png",path="icon"),
							TextItem(s"""
								|Suggest an engine move, makes it on the board and stores it in the book.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="PGN tab",
				paragraphs=List(
					Paragraph(
						title="Content",
						ditems=List(
							TextItem(s"""
								|The PGN tab shows the current game in PGN format. The main line is shown in black, the 
								|subvariations are shown in red.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Navigation",
						ditems=List(
							TextItem(s"""
								|To jump to a move within the PGN click on it, this move becomes the selected move
								|and is presented with a different background.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Comments",
						ditems=List(
							TextItem(s"""
								|To edit / delete the comment belonging to the selected move click on it. To edit / delete
								|the comment of an unselected move first click on it to select it then click on it again.
								|To edit / delete a header field click on the header field. To add a new header click 
								|on any of the headers and type in a new header field name and value.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Moves tab",
				paragraphs=List(					
					Paragraph(
						title="Content",
						ditems=List(
							TextItem(s"""
								|The moves tab lists all the legal moves in the position in both SAN and algebraic format.								
								|If the position has no legal moves, it will be caracterized as either mate or stalemate.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Navigation",
						ditems=List(
							TextItem(s"""
								|To make any of the moves on the board click on the SAN notation of the move.								
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="PGN games tab",
				paragraphs=List(
					Paragraph(
						title="Content",
						ditems=List(
							TextItem(s"""
								|Shows the games belonging to the multiple game PGN that was recently opened.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Navigation",
						ditems=game_browser_navigation
					)
				)
			),
			Chapter(
				title="Book games tab",
				paragraphs=List(
					Paragraph(
						title="Content",
						ditems=List(
							TextItem(s"""
								|Shows the games that are part of the current book and this position occurs in them.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Navigation",
						ditems=game_browser_navigation
					)
				)
			),
			Chapter(
				title="Engines tab",
				paragraphs=List(
					Paragraph(
						title="Add new engine",
						ditems=List(
							TextItem(s"""
								|To add a new engine click on "Add UCI engine" or "Add XBOARD engine" depending on the protocol
								|of the engine.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Edit engine",
						ditems=List(
							TextItem(s"""
								|You can edit an engine's rank among engines by clicking on 
								|the arrow symbols ( to top, up, down, to bottom ). You can also delete it by clicking on 
								|the X sign. To edit the engine's settings click on the settings symbol or the engine's name.
								|To have the engine auto loaded on later startups ( and load it for now as well ) click on the 
								|checkbox next to the engine.
							""".stripMargin)
						)
					),
					Paragraph(
						title="Engine rank",
						ditems=List(
							TextItem(s"""
								|The ranking ( order ) of the engines matters.
								|The top ranked running engine enjoys precedence over other running engines and the output of 
								|this top ranked running engine will be used to show analysis results on the board and to evaluate 
								|book moves.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="Systemlog tab",
				paragraphs=List(
					Paragraph(
						title="System log",
						ditems=List(
							TextItem(s"""
								|System log logs all events in the GUI. When performing long operations you are redirected 
								|to the "System log" tab so that you can follow the progress of the operation. The "Execqueue" 
								|and "Queuelog" tabs provide developer's system information not of interest to the general user.
							""".stripMargin)
						)
					)
				)
			),
			Chapter(
				title="PGN sync",
				paragraphs=List(
					Paragraph(
						title="About PGN sync",
						ditems=List(
							TextItem(s"""
								|PGN sync lets you synchronize lichess games of the handle set in Profile to a local PGN.
							""".stripMargin)
						)
					),
					Paragraph(
						title="System requirements",
						ditems=List(
							TextItem(s"""
								|You have to install Node.js on your system to be able to use this feature.
							""".stripMargin)
						)
					)
				)
			)
		)
	)
}