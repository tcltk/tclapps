#!/bin/sh
#
# H E X P L O D E -- a game of strategy
#
# Neil Winton (ndwinton@geocities.com or neil@winton.freeserve.co.uk)
#
# @(#)hexplode.tcl	1.1 1998/10/18
#
# Copyright 1997, 1998 by Neil Winton. All rights reserved.
#
# RCS: @(#) $Id: hexplode.tcl,v 1.2 2003/10/24 22:42:09 andreas_kupries Exp $
#
# You may copy, modify and distribute this program freely -- but please
# retain the above copyright statement and don't pretend that you wrote it!
#
# Restart with wish. Do not remove this --> \
exec wish $0 ${1-"$@"}

###
### Global Data
###

# globalDef
#
# Define/import global variables.
#
# If called with one argument add the named variable to the list of known
# globals. If called with two add it and set its value to the second
# argument. If called with no arguments import all globalDef'd variables
# into the current procedure.

proc globalDef {args} {
    global GLOBALS

    switch -exact -- [llength $args] {
	2   {uplevel #0 set $args}
	1   {}
	0   {
	    foreach var [array names GLOBALS] {
		uplevel 1 "global $var"
	    }
	    return
	}
	default {error "wrong number of args"}
    }
    set GLOBALS([lindex $args 0]) 1
}

globalDef Player 0	    ;# The index of the current player

globalDef MaxPlayers 6	    ;# The maximum number of possible players

globalDef PlayerColour	    ;# Array of player-colour mappings
array set PlayerColour {0 red 1 green 2 yellow 3 blue 4 cyan 5 magenta}

globalDef PlayerStrategy    ;# What strategy routine is used by the player
array set PlayerStrategy {0 human 1 chooser 2 gobbler 3 avoider 4 cornered 5 loader}

globalDef PlayerCount	    ;# The number of cells occupied by the player
array set PlayerCount {0 -1 1 -1 2 -1 3 -1 4 -1 5 -1}

globalDef PlayerState	    ;# Boolean indicating whether the player is active
array set PlayerState {0 1 1 1 2 0 3 0 4 0 5 0}	;# Only players 0 and 1

globalDef DefBorder {#202020}	;# Default cell border color
globalDef GameOver 0		;# Flag set to true when a winner is found
globalDef Cell			;# Array of information about cells
globalDef Waiter		;# Variable used for flash animation
globalDef Rows 5		;# Number of rows on the board
globalDef Cols 5		;# Number of columns on the board
globalDef Canvas		;# Board canvas widget
globalDef IsHuman 0		;# Is the current player a human?
globalDef SaveConfig		;# Array used to save config temporarily
globalDef LastRow {}		;# Row index of last cell entered
globalDef LastCol {}		;# Column index of last cell entered
globalDef IsTclet 0		;# Is this running as an embedded Tclet?

###
### Main Game Routines
###

# newBoard
#
# Generate a new board canvas large enough to hold rows x cols cells with
# a margin around them.
#
# Each cell is a hexagon of 1cm each side "standing" on a point.
# The horizontal width of each cell is sqrt(3) = 1.732cm and the vertical
# height is 2cm. The centre of cell (0,0) is placed 2cm in and 2cm down
# from the top left corner of the board. This means that the left and right
# margins are 1.134cm (= 2 - 1.732/2) and the top and bottom margins are 1cm.

proc newBoard {rows cols} {
    globalDef

    set Rows $rows
    set Cols $cols

    catch {destroy .board}
    set Canvas [canvas .board \
	-width [expr 1.732 * ($cols + (($rows - 1.0) / 2.0)) + 2.268]c \
	-height [expr $rows * 1.5 + 2.5]c \
	-xscrollincrement 1c -yscrollincrement 1c -relief sunken -bd 2]

    # Move the origin to 2cm in and 2cm down.
    # This will be the center of cell (0,0) and makes cell positioning
    # calculations much easier!

    $Canvas xview scroll -2 units
    $Canvas yview scroll -2 units
    pack $Canvas -side top

    # Now create all of the cells. We also work out how many neighbours
    # each cell will have at this point.

    for {set r 0} {$r < $rows} {incr r} {
	for {set c 0} {$c < $cols} {incr c} {

	    # Cells in the body of the board have six neigbours

	    set neighbours 6

	    # If the cell is on the top or bottom edge it loses two

	    if {$r == 0 || $r == [expr $rows - 1]} {
		incr neighbours -2
	    }

	    # If is is on the left or right edge it also loses two

	    if {$c == 0 || $c == [expr $cols - 1]} {
		incr neighbours -2
	    }

	    # Correct for cells at the top right and bottom left corners

	    if {($c == 0 && $r == [expr $rows - 1]) ||
		($c == [expr $cols - 1] && $r == 0)} {
		incr neighbours 1
	    }

	    # Create the cell!

	    newCell $r $c $neighbours
	}
    }
}

# newCell
#
# Draw a new cell on Canvas at specified row and column. Each cell
# consists of a polygon item and a text field centred within it (which
# will initially contain an empty string).
#
# This procedure also initializes the Cell() array to contain all of
# the critical information about the cell. This array has multiple
# types of keys all of the form <type>:<row>:<col>. The types and
# their meanings are as follows:
#
#   id:	    The canvas id of the cell polygon
#   numid:  The canvas id of the number text within the cell
#   owner:  The the index of the player that has "claimed" the cell
#   count:  The current value of the cell counter
#   nbrs:   The number of neighbours of the cell (from the function argument)

proc newCell {row col neighbours} {
    globalDef

    # Calculate the offset from x origin of the centre of the start of
    # the specified row

    set offset [expr $row * 0.866]

    # Create the cell itself

    set Cell(id:$row:$col) [$Canvas create polygon \
	    [expr $offset + $col * 1.732]c [expr $row * 1.5 - 1]c \
	    [expr $offset + $col * 1.732 + 0.866]c [expr $row * 1.5 - 0.5]c \
	    [expr $offset + $col * 1.732 + 0.866]c [expr $row * 1.5 + 0.5]c \
	    [expr $offset + $col * 1.732]c [expr $row * 1.5 + 1]c \
	    [expr $offset + $col * 1.732 - 0.866]c [expr $row * 1.5 + 0.5]c \
	    [expr $offset + $col * 1.732 - 0.866]c [expr $row * 1.5 - 0.5]c \
	    -fill black -outline $DefBorder -width 2]

    # Create the cell text

    set Cell(numid:$row:$col) [$Canvas create text \
	[expr $offset + $col * 1.732]c [expr $row * 1.5]c \
	-fill black -justify center -anchor center -text {} \
	-font {Helvetica 12 bold}]

    # Initialize the other cell data

    set Cell(owner:$row:$col) {}
    set Cell(count:$row:$col) 0
    set Cell(nbrs:$row:$col) $neighbours

    # Set up bindings to highlight and activate the cell for interactive
    # players. Note that we need bindings on both the cell and its
    # text to ensure that the cell remains highlighted wherever the
    # cursor is within it.
    #
    # Note that there is no binding on leaving the text because otherwise
    # the cell can become unhighlighted unnecessarily.

    $Canvas bind $Cell(id:$row:$col) <Enter> "enterCell $row $col"
    $Canvas bind $Cell(numid:$row:$col) <Enter> "enterCell $row $col"

    $Canvas bind $Cell(id:$row:$col) <Leave> "leaveLastCell"

    $Canvas bind $Cell(id:$row:$col) <ButtonPress> "if {\$IsHuman} {activateCell $row $col; enterCell $row $col}"
    $Canvas bind $Cell(numid:$row:$col) <ButtonPress> "if {\$IsHuman} {activateCell $row $col; enterCell $row $col}"
}

# clearBoard
#
# Reset the cells of an existing board to be empty

proc clearBoard {} {
    globalDef

    # Remove any message displayed on the canvas

    $Canvas delete msg

    # Reset other variables

    set IsHuman 0

    foreach p [array names PlayerCount] {
	set PlayerCount($p) -1
    }

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    $Canvas itemconfigure $Cell(id:$r:$c) -fill black -outline $DefBorder
	    $Canvas itemconfigure $Cell(numid:$r:$c) -text {}
	    set Cell(owner:$r:$c) {}
	    set Cell(count:$r:$c) 0
	}
    }

    leaveLastCell

    update idletasks
}

# enterCell
#
# This procedure highlights the border of the cell in the colour of the
# current player if either the cell is empty and available to be claimed
# or already owned by the player.

proc enterCell {row col} {
    globalDef

    # Remove any other "left-over" highlights

    leaveLastCell

    if {$GameOver || !$IsHuman} return

    # Raise the cell and its text so that its border will show correctly

    $Canvas raise $Cell(id:$row:$col);
    $Canvas raise $Cell(numid:$row:$col);

    # Highlight it if possible. If the cell is vacant use the colour
    # associated with the player, if already owned by the player use
    # white and if owned by someone else leave it alone.

    if {$Cell(owner:$row:$col) == {}} {
	$Canvas itemconfigure $Cell(id:$row:$col) -outline $PlayerColour($Player)
    } elseif {$Cell(owner:$row:$col) == $Player} {
	$Canvas itemconfigure $Cell(id:$row:$col) -outline white
    }

    # Record this cell as (potentially) highlighted

    set LastRow $row
    set LastCol $col

    update idletasks
}

# leaveLastCell
#
# Unhighlight the cell specified in LastRow and LastCol, if any

proc leaveLastCell {} {
    globalDef

    if {$LastRow != {}} {
	$Canvas itemconfigure $Cell(id:$LastRow:$LastCol) -outline $DefBorder
	set LastRow {}
	set LastCol {}
    }

    update idletasks
}

# activateCell
#
# If a cell is either unoccupied or already owned by the current player
# this claims the cell and increments the counter. We then test to see if
# the cell can be "exploded".

proc activateCell {row col} {
    globalDef

    if {$GameOver} return

    if {$Cell(owner:$row:$col) == $Player ||
	$Cell(owner:$row:$col) == {}} {

	# Claim the cell and update its contents

	set Cell(owner:$row:$col) $Player
	$Canvas itemconfigure $Cell(id:$row:$col) -fill $PlayerColour($Player)
	incr Cell(count:$row:$col)
	$Canvas itemconfigure $Cell(numid:$row:$col) \
	    -text $Cell(count:$row:$col)

	update idletasks

	# Try to explode it

	explodeCell $row $col

	# If the play has won then we put up the end-game message and
	# then reset

	if {$GameOver} {
	    gameOverMessage $PlayerColour($Player)
	} {
	    # The game is not yet over, so yield to the next player

	    nextTurn
	}
    }

    update idletasks
}

# explodeCell
#
# This is the real heart of the game. If the cell has reached "criticality"
# -- its count is the same as the number of its neighbours -- it will
# "explode" its contents on to the surrounding cells, adding one to the
# counter of each cell. If it explodes on to an opponent's cell then it
# captures that cell. If any of the neighbouring cells reach criticality
# as a result of the explosion they will also explode, and so on in a
# chain reaction. This is a great demonstration of recusion :-)

proc explodeCell {row col {take 0}} {
    globalDef


    # Check whether the game has already been won -- there is no point
    # in exploding any more cells!

    if {$GameOver} return

    # Check whether we are processing a cell off the edge of the
    # board and return quietly -- this makes the logic handling
    # neighbouring cells easier below.

    if {$row < 0 || $row >= $Rows || $col < 0 || $col >= $Cols} {
	return
    }

    # If the "take" flag is true the we are capturing a neighbour of an
    # already exploded cell.

    if {$take} {
	incr Cell(count:$row:$col)
	set Cell(owner:$row:$col) $Player
	$Canvas itemconfigure $Cell(id:$row:$col) -fill $PlayerColour($Player)
	$Canvas itemconfigure $Cell(numid:$row:$col) -text $Cell(count:$row:$col)
    }

    update idletasks

    # Test for criticality

    if {$Cell(count:$row:$col) < $Cell(nbrs:$row:$col)} {
	return
    }

    # EXPLODE!
    #
    # Flash the cell to show that it is being exploded and reset its
    # contents to leave it unoccupied

    set Cell(count:$row:$col) 0
    set Cell(owner:$row:$col) {}
    $Canvas itemconfigure $Cell(id:$row:$col) -fill white
    $Canvas itemconfigure $Cell(numid:$row:$col) -text {}
    after 200 "set Waiter 1"
    update idletasks
    tkwait variable Waiter
    $Canvas itemconfigure $Cell(id:$row:$col) -fill black
    update idletasks

    # Check for game over -- this may avoid unnecessary work below

    checkWin

    # Now take each of the neighbouring cells and explode if possible.
    # Note that the order of exploding is deterministic but I do not
    # know whether different orders would produce different end results.
    # I suspect not, but I have never had the time or inclination to
    # try to prove it!

    explodeCell $row [expr $col - 1] 1
    explodeCell $row [expr $col + 1] 1
    explodeCell [expr $row - 1] $col 1
    explodeCell [expr $row - 1] [expr $col + 1] 1
    explodeCell [expr $row + 1] [expr $col - 1] 1
    explodeCell [expr $row + 1] $col 1

    # Check again

    checkWin
}

# checkWin
#
# Check whether the game is over an set the GameOver flag if so. The game
# is over when one player owns all of the occupied cells on the board.
#
# Note that this does not get called until at least one cell has been
# exploded -- so that takes care of the initial move when the first player
# owns all occupied cells.

proc checkWin {} {
    globalDef

    if {$GameOver} return

    foreach p [array names PlayerCount] {
	set PlayerCount($p) 0
    }

    set totalCount 0
    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    if {$Cell(owner:$r:$c) != {}} {
		incr totalCount
		incr PlayerCount($Cell(owner:$r:$c))
	    }
	}
    }

    # If the current player now owns all the set cells then this is
    # the end!

    if {$PlayerCount($Player) == $totalCount} {
	set GameOver 1
    }
}

# gameOverMessage
#
# Display the end-game message (shadowed white text centred on the canvas)
# and wait for a button press before returning.

proc gameOverMessage {winner} {
    globalDef

    # Construct the message text

    set msg "Game Over: $winner wins!"

    # The shadow text

    set below [$Canvas create text \
	[expr [$Canvas cget -width]/2] [expr [$Canvas cget -height]/2] \
	-fill "#a0a0a0" -justify center -anchor center -text $msg \
	-font {helvetica 14 bold} -tags msg]

    # The message text

    set above [$Canvas create text \
	[expr [$Canvas cget -width]/2] [expr [$Canvas cget -height]/2] \
	-fill white -justify center -anchor center -text $msg \
	-font {helvetica 14 bold} -tags msg]

    # Adjust to allow for the canvas offset then shift the shadow one
    # pixel further.

    $Canvas move $below -2c -2c
    $Canvas move $below 1 1
    $Canvas move $above -2c -2c
}

# nextTurn
#
# Find out whose turn it is next and then invoke the appropriate
# strategy routine.

proc nextTurn {} {
    globalDef

    set IsHuman 0

    # Next player takes a turn if he has not been wiped out.
    # Player is the global index into to the Player* arrays.

    while 1 {
	set Player [expr {($Player + 1) % $MaxPlayers}]

	# Next statement must eventually be true (unless there's a bug :-)

	if {$PlayerState($Player) && $PlayerCount($Player) != 0} break
    }

    # Invoke the matching strategy routine

    eval $PlayerStrategy($Player)
}

###
### Game Configuration Dialogue
###

# configGame
#
# Build and display the game configuration panel

proc configGame {} {
    globalDef

    # Save the current values of variables. They will be restored if
    # the Cancel button is pressed.

    saveConfig

    # Build the top-level window the first time through

    if {![winfo exists .config]} {
	toplevel .config 
	wm title .config "Game Settings"

	# Board size entry boxes

	set c [frame .config.size -relief raised -bd 1]
	label $c.l1 -text "Board size: "
	entry $c.rows -width 2 -textvariable Rows
	label $c.l2 -text " rows x "
	entry $c.cols -width 2 -textvariable Cols
	label $c.l3 -text " cols"
	pack $c.l1 -side left
	pack $c.rows -side left
	pack $c.l2 -side left
	pack $c.cols -side left
	pack $c.l3 -side left
	pack $c -side top -expand yes -fill x -ipadx 2 -ipady 2

	# Player selection

	set p [frame .config.players -relief raised -bd 1]
	foreach player [lsort -integer [array names PlayerState]] {
	    # Appropriately coloured label

	    label .config.players.lb$player -text "$PlayerColour($player)" \
		-bg $PlayerColour($player)
	    grid .config.players.lb$player -column 0 -row $player -sticky ew

	    # Checkbox to enable/disable player

	    checkbutton .config.players.cb$player -variable PlayerState($player)
	    grid .config.players.cb$player -column 1 -row $player

	    # Option menu to select strategy
	    tk_optionMenu .config.players.om$player PlayerStrategy($player) human loader exploder avoider cornered gobbler chooser
	    grid .config.players.om$player -column 2 -row $player -sticky ew
	}
	grid columnconfigure .config.players 2 -minsize 2.5c
	pack $p -side top -expand yes -fill both -ipadx 2 -ipady 2

	# OK/Cancel buttons

	set b [frame .config.buttons -relief raised -bd 1]
	button $b.ok -text Ok -command "setConfig; wm withdraw .config" -width 6
	bind $b.ok <Return> "$b.ok invoke"
	button $b.cancel -text Cancel -command "restoreConfig; wm withdraw .config"
	bind $b.cancel <Return> "$b.cancel invoke"
	pack $b.ok -side left -padx 5 -pady 2
	pack $b.cancel -side left -padx 5 -pady 2
	pack $b -ipadx 5 -ipady 2 -fill x
    } 

    wm deiconify .config
    focus .config.buttons.ok
}

# saveConfig
#
# Save critical configuration information in the SaveConfig array

proc saveConfig {} {
    globalDef

    set SaveConfig(strategy) [array get PlayerStrategy]
    set SaveConfig(state) [array get PlayerState]
    set SaveConfig(rows) $Rows
    set SaveConfig(cols) $Cols
}

# restoreConfig
#
# Restore previously saved configuration

proc restoreConfig {} {
    globalDef

    array set PlayerState $SaveConfig(state)
    array set PlayerStrategy $SaveConfig(strategy)
    set Rows $SaveConfig(rows)
    set Cols $SaveConfig(cols)
}

# setConfig
#
# Apply the new configuration for those things that do not happen
# automatically as a result of setting variables -- basically rebuilding
# the board.

proc setConfig {} {
    globalDef

    newBoard $Rows $Cols
    clearBoard
}

###
### Game Strategies
###

# randNum
#
# Not a strategy but a pseudo-random number generator taken from TkTetris.
# It generates an integer n such that 0 <= n < range.

set _ran [expr [clock seconds] % 1000]
proc randNum {range} {
  global _ran
  set _ran [expr ($_ran * 9301 + 49297) % 233280]
  return [expr int($range * ($_ran / double(233280)))]
}

# human
#
# The "human" strategy -- wait for input!

proc human {} {
    globalDef

    set IsHuman 1
}

# criticality
#
# Calculate the "criticality" of a cell for the specified player. This is
# the difference between to current contents of the cell and its number
# of neighbours. It ranges from 0, for a cell that will explode to 6 for
# an empty cell in the centre of the board.
#
# If the cell indices are out of range it returns 999 and if the cell is
# not owned by the specified player it returns the criticality negated.

proc criticality {row col {player {}}} {
    globalDef

    if {$player == {}} {
	set player $Player
    }

    if {$row < 0 || $row >= $Rows || $col < 0 || $col >= $Cols} {
	return 999
    }

    set diff [expr $Cell(nbrs:$row:$col) - $Cell(count:$row:$col)]

    if {$Cell(owner:$row:$col) != $player && $Cell(owner:$row:$col) != {}} {
	return [expr {0 - $diff}]
    }

    return $diff
}

# vulnerability
#
# The "vulnerability" is a measure of the likelihood that a cell will be
# captured by an opponent before it can be exploded by the current player.
# It returns from 0 (most vulnerable) to 999 (not at all vulnerable).

proc vulnerability {row col} {
    globalDef

    set minVal -999

    set val [criticality $row [expr $col - 1] $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality $row [expr $col - 1] $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality $row [expr $col + 1] $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality [expr $row - 1] $col $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality [expr $row - 1] [expr $col + 1] $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality [expr $row + 1] [expr $col - 1] $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}
    set val [criticality [expr $row + 1] $col $Player]
    if {$val <= 0 && $val > $minVal} {set minVal $val}

    return [expr {0 - $minVal}]
}

# exploder -- STRATEGY
#
# Find a cell which can be exploded. Of preference chose one which causes
# the largest explosion. If no cell can be exploded then load up the cell
# nearest explosion.

proc exploder {} {
    globalDef

    set mostCrit 999
    set bestNbrs 0
    set bestCell {}

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    if {$Cell(owner:$r:$c) == $Player} {
		# The player owns the cell so now find out how close
		# the cell is to explosion.

		set crit [criticality $r $c]

		if {$crit < $mostCrit} {
		    # This is the nearest so far so we record how close it
		    # is and how many neighbours it has. And store the cell
		    # coordinates, of course!

		    set mostCrit $crit
		    set bestNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$crit == $mostCrit} {
		    # This is as close to criticality as a previously
		    # found cell, but it might have more neighbours. If
		    # so, choose it in preference.

		    if {$Cell(nbrs:$r:$c) > $bestNbrs} {
			set bestNbrs $Cell(nbrs:$r:$c)
			set bestCell [list [list $r $c]]
		    } elseif {$Cell(nbrs:$r:$c) == $bestNbrs} {
			# This is as good as we have found before, but no
			# better so add the coordinates to the bestCell
			# list.

			lappend bestCell [list $r $c]
		    }
		}
	    } elseif {$Cell(owner:$r:$c) == {}} {
		# Nobody yet owns this cell, but if it is close to an
		# edge then it may be close to explosion by virtue of
		# having few neighbours.

		if {$Cell(nbrs:$r:$c) < $mostCrit} {
		    set mostCrit $Cell(nbrs:$r:$c)
		    set bestNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$Cell(nbrs:$r:$c) == $mostCrit} {
		    lappend bestCell [list $r $c]
		}
	    }
	}
    }

    if {$bestCell == {}} {
	# This should never happen ...

	error "exploder: no cell found!"
    }

    # Now choose one of our potential best cells at random.

    set cell [lindex $bestCell [randNum [llength $bestCell]]]

    # Flash it to give a human user a chance to see what is going on
    # and then activate the cell.

    flashCell [lindex $cell 0] [lindex $cell 1]
    activateCell [lindex $cell 0] [lindex $cell 1]
}

# loader -- STRATEGY
#
# Load up cells to the point of explosion and then find a cell which causes
# the largest explosion.

proc loader {} {
    globalDef

    set mostCrit 100
    set bestNbrs 0
    set bestCell {}

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    if {$Cell(owner:$r:$c) == $Player} {
		# The player owns the cell so now find out how close
		# the cell is to explosion.

		set crit [criticality $r $c]

		if {$crit < $mostCrit && $crit > 1} {
		    # The cell is close to explosion but still at least
		    # two steps away from criticality.

		    set mostCrit $crit
		    set bestNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$crit == $mostCrit} {
		    # This is as close to criticality as a previously
		    # found cell, but it might have more neighbours. If
		    # so, choose it in preference.

		    if {$Cell(nbrs:$r:$c) > $bestNbrs} {
			set bestNbrs $Cell(nbrs:$r:$c)
			set bestCell [list [list $r $c]]
		    } elseif {$Cell(nbrs:$r:$c) == $bestNbrs} {
			lappend bestCell [list $r $c]
		    }
		}
	    } elseif {$Cell(owner:$r:$c) == {}} {
		# Nobody yet owns this cell, but if it is close to an
		# edge then it may be close to explosion by virtue of
		# having few neighbours.

		if {$Cell(nbrs:$r:$c) < $mostCrit} {
		    set mostCrit $Cell(nbrs:$r:$c)
		    set bestNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$Cell(nbrs:$r:$c) == $mostCrit} {
		    lappend bestCell [list $r $c]
		}
	    }
	}
    }

    if {$bestCell == {}} {
	# If this strategy did not find a good target cell -- which can
	# happen if all potential cells have reached criticality --
	# then we will apply the "exploder" strategy to detonate one!

	exploder
    } else {
	# Select a "best cell" at random then flash and activate it

	set cell [lindex $bestCell [randNum [llength $bestCell]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
    }
}

# cornered -- STRATEGY
#
# Chooses corner cells first, with "acute" corners in preference to
# "obtuse" corners. If they are not available it falls back to the
# loader strategy.

proc cornered {} {
    globalDef

    set maxCol [expr $Cols - 1]
    set maxRow [expr $Rows - 1]

    # Try acute corners

    set acute {}

    if {$Cell(owner:0:0) == {}} {
	lappend acute [list 0 0]
    }
    if {$Cell(owner:$maxRow:$maxCol) == {}} {
	lappend acute [list $maxRow $maxCol]
    }

    if {$acute != {}} {
	set cell [lindex $acute [randNum [llength $acute]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
	return
    }

    # No luck, try obtuse corners

    set obtuse {}

    if {$Cell(owner:0:$maxCol) == {}} {
	lappend obtuse [list 0 $maxCol]
    }
    if {$Cell(owner:$maxRow:0) == {}} {
	lappend obtuse [list $maxRow 0]
    }

    if {$obtuse != {}} {
	set cell [lindex $obtuse [randNum [llength $obtuse]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
	return
    }

    # No corners available, fall back to loader

    loader
}

# avoider -- STRATEGY
#
# Like loader but avoids cells where the vulnerability score is less than
# the criticality score.

proc avoider {} {
    globalDef

    set mostCrit 100
    set mostNbrs 0
    set bestCell {}

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    set crit [criticality $r $c]
	    set vul [vulnerability $r $c]

	    if {$crit > $vul} {
		# It is likely to be captured before we can explode
		# it so avoid it!

		continue
	    }

	    if {$Cell(owner:$r:$c) == $Player} {
		# The player owns the cell so now find out how close
		# the cell is to explosion and how vulnerable to being
		# captured by an opponent.

		if {$crit < $mostCrit && $crit > 1} {
		    # The cell is close to explosion but still at least
		    # two steps away from criticality.

		    set mostCrit $crit
		    set mostNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$crit == $mostCrit} {
		    # This is as close to criticality as a previously
		    # found cell, but it might have more neighbours. If
		    # so, choose it in preference.

		    if {$Cell(nbrs:$r:$c) > $mostNbrs} {
			set mostNbrs $Cell(nbrs:$r:$c)
			set bestCell [list [list $r $c]]
		    } elseif {$Cell(nbrs:$r:$c) == $mostNbrs} {
			lappend bestCell [list $r $c]
		    }
		}
	    } elseif {$Cell(owner:$r:$c) == {}} {
		# Nobody yet owns this cell, but if it is close to an
		# edge then it may be close to explosion by virtue of
		# having few neighbours.

		if {$Cell(nbrs:$r:$c) < $mostCrit} {
		    set mostCrit $Cell(nbrs:$r:$c)
		    set mostNbrs $Cell(nbrs:$r:$c)
		    set bestCell [list [list $r $c]]
		} elseif {$Cell(nbrs:$r:$c) == $mostCrit} {
		    lappend bestCell [list $r $c]
		}
	    }
	}
    }

    if {$bestCell == {}} {
	# If this strategy did not find a good target cell -- which can
	# happen if all potential cells have reached criticality --
	# then we will apply the "exploder" strategy to detonate one!

	exploder
    } else {
	# Select a "best cell" at randon then flash and activate it

	set cell [lindex $bestCell [randNum [llength $bestCell]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
    }
}

# gobbler -- STRATEGY
#
# This is a more sophisticated strategy. It tries first to capture the
# maximum number of pieces from opponents,if it can. Failing that it drops
# back to the "cornered" strategy.
# 
# This will give you a pretty good game (and is the default in the Tclet
# version).

proc gobbler {} {
    globalDef

    # First see if we can capture any pieces from opponents by exploding

    set bestWin 0
    set bestCell {}

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {
	    set win [winCount $r $c]
	    if {$win > $bestWin} {
		set bestWin $win
		set bestCell [list [list $r $c]]
	    } elseif {$win == $bestWin} {
		lappend bestCell [list $r $c]
	    }
	}
    }

    # If we can win some then do so (choose one at random if there
    # is a choice)

    if {$bestWin > 0} {
	set cell [lindex $bestCell [randNum [llength $bestCell]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
	return
    }

    # There were none to explode, so try the cornered strategy

    cornered
}

# winCount
#
# Figure out how many counters we would win if we explode a cell

proc winCount {row col} {
    globalDef

    # Check if the cell is owned by the current player and is critical

    if {[criticality $row $col] != 1} {
	return 0
    }

    # See how much we will win if we explode it

    set count 0

    incr count [winForCell $row [expr $col - 1]]
    incr count [winForCell $row [expr $col + 1]]
    incr count [winForCell [expr $row - 1] $col]
    incr count [winForCell [expr $row - 1] [expr $col + 1]]
    incr count [winForCell [expr $row + 1] [expr $col - 1]]
    incr count [winForCell [expr $row + 1] $col]

    return $count
}

proc winForCell {row col} {
    globalDef

    if {$row < 0 || $row >= $Rows || $col < 0 || $col >= $Cols} {
	return 0
    } elseif {$Cell(owner:$row:$col) != $Player} {
	return $Cell(count:$row:$col)
    }
    return 0
}

# chooser -- STRATEGY
#
# This is the most sophisticated (and CPU-intensive!) strategy.
#
# Like "gobbler", first see if we can capture any pieces from opponents
# by exploding but this time pick the cells that are most vulnerable to
# being captured themselves out of preference.
#
# This is quite hard to beat but can seem slow. It is the default in
# the non-Tclet version.

proc chooser {} {
    globalDef

    set mostVul 999
    set bestWin 0
    set bestCell {}

    for {set r 0} {$r < $Rows} {incr r} {
	for {set c 0} {$c < $Cols} {incr c} {

	    set vul [vulnerability $r $c]
	    set win [winCount $r $c]

	    if {$vul < $mostVul} {
		# This is the most vulnerable so far so "get them before
		# they get us". By definition, if it is vulnerable
		# to being captured then it can win some itself.

		set mostVul $vul
		set bestWin $win
		set bestCell [list [list $r $c]]
	    } elseif {$vul == $mostVul} {
		if {$win > $bestWin} {
		    set bestWin $win
		    set bestCell [list [list $r $c]]
		} elseif {$win == $bestWin} {
		    lappend bestCell [list $r $c]
		}
	    }
	}
    }

    # If we can win some then do so. If there is a choice, pick
    # one at random -- they should all be equally vulnerable and
    # win equal numbers of cells.

    if {$bestWin > 0} {
	set cell [lindex $bestCell [randNum [llength $bestCell]]]
	flashCell [lindex $cell 0] [lindex $cell 1]
	activateCell [lindex $cell 0] [lindex $cell 1]
	return
    }

    # There were none to explode, so try the avoider strategy

    avoider
}

# flashCell
#
# Flash a cell moved by the computer to give humans a chance to see
# what is going on!

proc flashCell {row col} {
    globalDef

    set oldFill [$Canvas itemcget $Cell(id:$row:$col) -fill]

    for {set i 0} {$i < 4} {incr i} {
	after 100 "set Waiter 1"
	$Canvas itemconfigure $Cell(id:$row:$col) -fill white
	update idletasks
	tkwait variable Waiter

	after 100 "set Waiter 1"
	$Canvas itemconfigure $Cell(id:$row:$col) -fill $oldFill
	update idletasks
	tkwait variable Waiter
    }
}

# newGame
#
# This is the procedure that actually starts play.

proc newGame {} {
    globalDef

    set GameOver 0
    clearBoard
    nextTurn
}

# aboutBox
#
# Info for Help -> About ...

proc aboutBox {} {
    tk_messageBox -type ok -title "About Hexplode" -message \
"H E X P L O D E

Copyright 1997, 1998 by Neil Winton

ndwinton@geocities.com or
neil@winton.freeserve.co.uk

http://www.winton.freeserve.co.uk"
}

proc displayHelp {} {
    toplevel .help
    wm title .help "Hexplode Help"

    text .help.text -font {Helvetica 12} -wrap word -width 60 \
	-yscrollcommand ".help.sb set"
    scrollbar .help.sb -orient vertical -command ".help.text yview"

    .help.text insert 0.0 \
"H E X P L O D E
A Game of Strategy\n" h1 \
"Playing the game\n" h2 \
"Hexplode is a game for two or more players. It is played on a grid\
of hexagonal cells, rather like a honeycomb. Players take it in turns to\
place \"counters\" on cells (shown as incrementing a number in the cell).\
You can either place a counter on an unoccupied cell or on a cell of your\
own colour.\n" p \
"When the number of counters on the cell reaches a certain threshold,\
it will \"explode\" on to the surrounding cells. This \"critical mass\" is\
achieved when the number of counters reaches the number of neighbours\
directly bordering the cell. The explosion deposits one counter on each of the\
surrounding cells. If these cells are currently owned by an opponent then\
all of the counters in the cell are claimed by the player causing the\
explosion. If any of these neighbouring cells itself reaches criticality\
as a result of this, it too will explode in a chain reaction.\n" p \
"The aim of the game is to be the last player with any cells left on\
the board. That's all there is to it!\n" p \
"Running the program\n" h2 \
"If you are reading this you have probably figured out how to invoke\
the program already. To start playing (with you against the computer) use\
the Play option on the Game menu.\n" p \
"To configure other players (up to six in total) use the Configure option\
on the Game menu. You can select the strategy for each player. The \"human\"\
strategy just lets you make your move. If you wish, you can also watch the\
computer play against itself. Setting six computer-operated players against\
each other can be interesting to watch. You can also change the size of\
the board, but be aware that the computer may take a long time to make a move\
with some of the more sophisticated strategies on a large board. Tcl is\
great, but it wasn't built for computationally intensive tasks!\n" p \
"Credits\n" h2 \
"I'd love to say that I invented the game but in truth I first saw\
a simple version of it coded for the \"BBC Micro\" in the magazine\
\"Personal Computer World\" about 15 years ago! If the inventor ever sees\
this I'd like him to get in touch so that I can give him (or her) the\
credit due. Having said that, I coded this entirely from memory of that venerable\
program. All of the strategies are of my own devising, but I'm sure\
there must be much better ones.\n" p \
"I must give a special word of thanks to Libby for her dedicated testing\
(and lots of other things too ...).\n" p \
"I hope you enjoy this game. Feel free to copy, modify or distribute\
it, but give credit where it is due and don't pretend you wrote it! If you\
have any feedback please send it to be at the address below.\n" p \
"Finally, if you really like Hexplode please consider making a donation to\
an organization involved in land-mine clearance, for example\
UNICEF (http://www.unicef.org).\
It would be great to think that \"virtual explosions\" could make a\
difference to those whose lives have been devastated by the real thing.\n" p \
"Happy Hexploding!\n" h1 \
"
Neil Winton

ndwinton@geocities.com
or
neil@winton.freeserve.co.uk

http://www.winton.freeserve.co.uk\n" center

    .help.text tag configure h1 -font {Helvetica 14 bold} \
	-foreground red -justify center -spacing1 0.08i -spacing3 0.08i
    .help.text tag configure h2 -font {Helvetica 12 bold} \
	-lmargin1 0.25i -spacing1 0.08i -spacing3 0.08i
    .help.text tag configure p -lmargin1 0.5i -lmargin2 0.5i \
	-spacing3 0.08i
    .help.text tag configure bold -font {Helvetica 12 bold}
    .help.text tag configure center -justify center

    .help.text configure -state disabled

    button .help.close -text "Close" -command {destroy .help}

    pack .help.close -side bottom -pady 5 -padx 5 -expand no
    pack .help.sb -side right -expand yes -fill y
    pack .help.text -side left -expand yes -fill both
}

###
### Main Routine
###

newBoard $Rows $Cols

# Check whether the script is being invoked standalone or as a Tclet

if {![info exists embed_args]} {
    menu .mb -tearoff 0 -type menubar
    . configure -menu .mb

    menu .mb.game -tearoff 0
    .mb.game add command -label Play -command newGame -underline 0
    .mb.game add command -label Configure -command configGame -underline 0
    .mb.game add separator
    .mb.game add command -label Quit -command exit -underline 0

    menu .mb.help -tearoff 0
    .mb.help add command -label "How to play" -command displayHelp -underline 0
    .mb.help add separator
    .mb.help add command -label "About Hexplode" -command aboutBox -underline 0

    .mb add cascade -menu .mb.game -label Game -underline 0
    .mb add cascade -menu .mb.help -label Help -underline 0

    wm title . "Hexplode"

    tkwait visibility .
} {
    set IsTclet 1

    # Change the default computer strategy to "gobbler" and put up a single
    # "New Game" button.

    set PlayerStrategy(1) gobbler

    frame .control
    pack .control -side bottom -expand yes -fill both
    button .control.restart -text "New Game" -command newGame
    pack .control.restart -side top -expand yes -fill both

    newGame
}

focus .
