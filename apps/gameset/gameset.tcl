#!/bin/sh
# \
exec wish "$0" ${1+"$@"}

# A card game inspired by "set" (S Uhler 7/96) stephen.uhler@sun.com
# Copyright (c) 1996, Sun Microsystems Laboratories
# Should run either stand-alone, or as a plug-in.
#
# RCS: @(#) $Id: gameset.tcl,v 1.1 2001/11/07 22:30:33 hobbs Exp $

# Global variables
#  CardMap():	Determines the "look" of a card
#  Deck:	The remaining deck of cards (in a list).  Each card
#		is a list of 4 elements, each of which is 1, 2, or 3.
#		Each element represents one of the card's properties.
#  Match():	Maps a property of 2 cards to the matching 3rd card
#  Selected:	List of selected card id's
#  Where():	Maps a card id to a board position and card name
#  RNseed:	The randomish number seed
#  Message:	The textvarialbe on the message line
#  Cancel:	To after id to erase messages

array set CardMap {
    color,1   red
    color,2   magenta
    color,3   blue
    style,1   "rectangle 0 0 100 100"
    style,2   "oval 0 0 100 100"
    style,3   "polygon 50 0 100 50 50 100 0 50"
    pattern,1 "-fill {}"
    pattern,2 "-stipple error"
    pattern,3 ""
    number,1  "175"
    number,2  "125 225"
    number,3  "75 175 275"
    sizex 450 sizey 350
    scale 0.8
}

array set Match {
    11 1   12 3   13 2
    21 3   22 2   23 1
    31 2   32 1   33 3
}

# Construct a new card deck.

proc Make_deck {} {
    global Deck
    catch {unset Deck}
    foreach style {1 2 3} {
    	foreach pattern {1 2 3} {
	    foreach color {1 2 3} {
		foreach number {1 2 3} {				    
		    lappend Deck "$style $pattern $color $number"
		}
	    }
	}
    }
}

# Snagged from the net - a simple randomish number generator.

set RNseed [clock clicks]
proc random {} {
    global RNseed
    set RNseed [expr 30903*($RNseed&65535)+($RNseed>>16)]
    return [expr ($RNseed & 65535)/65535.0]
}

# Pick a randomish card from the deck.

proc pick {} {
    global Deck    
    set len [llength $Deck]
    if {$len == 0} {
    	return {}
    }
    set card [expr int([random] * $len)]
    set result [lindex $Deck $card]
    set Deck [lreplace $Deck $card $card]
    return $result
}

# Render a card on the canvas.  The nominal size of a card is 100 x 400,
# and is scaled as needed.

proc draw_card {can card} {
    global CardMap
    foreach {style pattern color number} $card {}
    set cardId [$can create rectangle 0 0 100 400 -width 2 -fill #DDD \
    	-outline black]
    $can itemconfigure $cardId -tags [list B B$cardId C$cardId]
    foreach i $CardMap(number,$number) {
    	set id [eval $can create $CardMap(style,$style) -width 3 \
    		-fill $CardMap(color,$color) \
    		-outline $CardMap(color,$color) \
    		$CardMap(pattern,$pattern) \
		-tags {[list C$cardId]}]
	$can scale $id 0 0 $CardMap(scale) $CardMap(scale)
	$can move $id 10 $i
    }
    $can scale C$cardId 0 0 \
	    [expr $CardMap(sizex)/530.0] [expr $CardMap(sizey)/1250.0]
    $can bind C$cardId <1> [list hit $can $cardId]
    return $cardId
}

# Process mouse "hits" on a card, to toggle card selection.

proc hit {can id} {
    global Selected Where
    append Selected {}
    if {[set i [lsearch $Selected $id]] < 0} {
    	lappend Selected $id
    	$can itemconfigure B$id -fill white
    } else {
    	set Selected [lreplace $Selected $i $i]
    	$can itemconfigure B$id -fill #DDD
    }
    if {[llength $Selected] == 3} {
    	foreach {i j k} $Selected {}
    	if {[match $Where(card,$i) $Where(card,$j) $Where(card,$k)]} {
	    got_match $can
	} else {
	    got_nomatch $can
	}
	unset Selected
    }
}

# Process a match.  Remove the matched cards, and either replace them
# with new cards, move "extra" cards into their slots, or leave a blank
# space in the board.

proc got_match {can} {
    global Where Selected Deck
    catch {unset Where(after)}
    set Where(hint) 0

    # remove matched cards

    foreach i $Selected {
	$can delete C$i
    }
    update idletasks
    after 500

    # find cards on board to move into vacated slots

    set move [expr [llength [array names Where card,*]] > 12]
    if {$move} {
	set move_list {}
    	foreach {id pos} [array get Where pos,*] {
	    regsub pos, $id {} id
	    if {[lindex $pos 0] == 4 && [lsearch $Selected $id] < 0} {
		lappend move_list $id
	    }
	}
	.extra configure -state normal
    }

    # Replace each empty slot with a new or moved card.

    foreach i $Selected {
	lappend Where(done) $Where(card,$i)
	foreach {col row} $Where(pos,$i) {}
	unset Where(pos,$i)
	unset Where(card,$i)
	if {$move } {
	    if {$col < 4 && [set switch [lindex $move_list 0]] != {}} {
	    	update idletasks
	    	after 200
		position_card $can C$switch $row $col
		set Where(pos,$switch) [list $col $row]
		set move_list [lrange $move_list 1 end]
	    }
	} else {
	    set card [pick]
	    if {$card == ""} continue
	    set new [draw_card $can $card]
	    position_card $can C$new $row $col
	    set Where(pos,$new) [list $col $row]
	    set Where(card,$new) $card
	}
    }
    set left [llength $Deck]
    msg "$left cards left in the deck."
    if {$left == 0} {
	.extra configure -state disabled
    }
}

# Process a miss.  X-out the selected cards for a while.

proc got_nomatch {can} {
    global Selected
    update idletasks
    after 500
    Xit $can $Selected
    catch bell
    after 1000 [list Xit $can]
    $can itemconfigure B -fill #DDD
}

# Position a card on the board.

proc position_card {can tag row column {gap 3}} {
    foreach {x1 y1 x2 y2} [$can bbox $tag] {}
    $can move $tag [expr (($x2-$x1)+$gap)*$column + $gap - $x1] \
	    [expr (($y2-$y1)+$gap)*$row + $gap - $y1]
}

# Pick a card, render it, and place it on the board.

proc place {can row col} {
    global Where
    if {[set card [pick]] != ""} {
	set id [draw_card $can $card]
	position_card $can C$id $row $col
	set Where(pos,$id) [list $col $row]
	set Where(card,$id) $card
    }
}

# Deal the initial layout.

proc deal {can} {
    global Where Selected
    catch {unset Where}
    catch {unset Selected}
    set Where(hint) 0
    Make_deck
    foreach row {0 1 2} {
    	foreach col {0 1 2 3} {
	    place $can $row $col
	}
    }
}

# Deal 3 extra cards into column 4

proc extra {can} {
    global Where
    set Where(hint) 0
    catch {unset Where(after)}
    foreach row {0 1 2} {
	place $can $row 4
    }
}

# See if 3 cards match.

proc match {c1 c2 c3} {
    global Match
    foreach i $c1 j $c2 k $c3 {
    	if {$Match($i$j) != $k} {
	    return 0
	}
    }
    return 1
}

# Find all current matches, eliminating duplicates.

proc find_match {} {
    global Where Match
    foreach {id card}  [array get Where card*] {
    	regsub card, $id {} id
    	set have($card) $id
    }
    set cards [array names have]
    set i1 0
    foreach c1 $cards {
    	foreach c2 [lrange $cards [incr i1] end] {
	    set need ""
	    foreach x $c1 y $c2 {
		lappend need $Match($x$y)
	    }
	    if {[info exists have($need)]} {
	    	set found([lsort -integer "$have($c1) $have($c2) $have($need)"]) 1
	    }
    	}
    }
    return [array names found]
}

# Print a status message for a while

proc msg {string {time 5000}} {
    global Message Cancel
    catch {after cancel $Cancel}
    set Message $string
    set Cancel [after $time "set Message {}"]
}

# Dispense hints. [temporary until I decide what it should REALLY do.]

proc hint {can {time 30000}} {
    global Where Selected

    if {[info exists Where(after)]} {
    	msg "Try looking a little harder."
    	return
    }

    set match [find_match]
    set count [llength $match]
    set state $Where(hint),$count,[.extra cget -state]
    switch -glob -- $state {
    	*,0,disabled {
	    msg "Would you care for a new game?"
    	}
    	*,0,* {
	    msg "Try the \"extra cards\" button."
    	}
    	0,1,* {
	    msg "There is one match showing."
    	}
    	0,*,* {
	    msg "There are $count matches showing."
    	}
    	[1-3],*,* {
	    set all [eval concat $match]
	    set index [expr int([random] * [llength $all])]
	    set id [lindex $all $index]
	    $can itemconfigure B -fill #DDD
	    set Selected $id
	    $can itemconfigure B$id -fill white
	    msg "Try this..."
	}
    	default {
	    msg "No hints left."
    	}
    }
    incr Where(hint)
    set Where(after) [after $time { catch {unset Where(after)}}]
}

# Cheat - for debugging

bind all <Control-Alt-X> {
    puts "----"
    msg "Cheater!!"
    foreach match [find_match] {
    	foreach {c1 c2 c3} $match {}
    	puts "$Where(pos,$c1) - $Where(pos,$c2) - $Where(pos,$c3)"
    }
}

# X out or remove all X's from a card (or cards).

proc Xit {can {id remove}} {
    if {$id == "remove"} {
    	$can delete X
    } else {
    	foreach i $id {
	    foreach {x1 y1 x2 y2} [$can bbox C$i] {}
	    $can create line $x1 $y1 $x2 $y2 -tags X -width 2
	    $can create line $x1 $y2 $x2 $y1 -tags X -width 2
	}
    }
}

# Stipples are broken on the Mac and windows

if {$tcl_platform(platform) != "unix"} {
    set CardMap(pattern,2) "-fill yellow -width 6"
    set CardMap(scale) 0.7
}

# Build the user interface.

set Selected {}
catch {eval destroy [winfo children .]}
canvas .c -width $CardMap(sizex) -height $CardMap(sizey) -bg bisque
grid [frame .f]
grid .c
grid [label .message -textvariable Message]
button .quit -text Quit -command exit
button .hint -text Hint -command "hint .c"
button .new -text "New Game" -command {
    .c delete all
    .extra configure -state normal
    deal .c
}
button .extra -text "Extra Tiles" -command {
    if {[llength [find_match]] == 0} {
	extra .c
	.extra configure -state disabled
    } else {
    	msg "There are still matches."
    }
}
grid .new .extra .hint .quit -in .f -sticky ns
if {[catch {wm title . "match/TCL 1.0"}]} {
    destroy .quit
}	
deal .c
