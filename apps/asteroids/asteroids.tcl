#############################################################################
#
# Asteroids.tcl - Tcl remake of the Atari arcade game classic
# Jeff Godfrey, Feb-2005
#
#############################################################################

package require Tk 8.4

set ::DIR [file dirname [info script]]

proc main {} {
    initVars
    testForSounds
    calcMotionVectors
    buildUI
    bindGameKeys
    showMenu
    loadHighScores
    updateScore
    updateLives

    after 1000 updateFPS
    gameLoop [clock clicks -milliseconds]
}

proc initVars {} {
    # This adds a bit of color - all white is traditional
    array set ::CLR {
	dust        yellow
	ship        lightblue
	shiplife    white
	missile     green
	rock        white
	text        white
	flame       red
    }

    set sw [winfo screenwidth .]
    set sh [winfo screenheight .]
    set ::SMALL [expr {$sw < 800}]
    if {$::SMALL} {
	set ::SCALE [expr {$sw / 800.0}]
	set ::globals(screenWidth)  $sw
	set ::globals(screenHeight) $sh
    } else {
	set ::SCALE 1
	set ::globals(screenWidth)  800
	set ::globals(screenHeight) 600
    }

    set ::globals(gameOn)        1
    set ::globals(level)         0
    set ::globals(score)         0
    set ::globals(lives)         3
    set ::globals(timeStart)     [clock clicks -milliseconds]
    set ::globals(frameCount)    0
    set ::globals(highScoreFile) [file join $::DIR "asteroids_hs.txt"]

    set ::globals(lifeEvery)     10000  ;# new life every 10000 pts
    set ::globals(nextLife)      $::globals(lifeEvery)
    set ::globals(newMissileOK)  1
    set ::globals(shipExists)    0
    set ::globals(hyperOK)       1

    set ::globals(sndThrust)     0
    set ::globals(beatDelay)     750
    set ::globals(sndOK)         0

    set ::globals(shipCoords)   [list 0 -11 -7 11 -3 7 3 7 7 11 0 -11]
    set ::globals(flameCoords)  [list -2 7 0 12 2 7 -2 7]
    # --- Large rock coords
    set ::globals(rockCoords,1) {-39 -25 -33 -8 -38 21 -23 25 -13 39 24 \
				     34 38 7 33 -15 38 -31 16 -39 -4 -34 -16 -39}
    set ::globals(rockCoords,2) {-32 35 -4 32 24 38 38 23 31 -4 38 -25 14 \
				     -39 -28 -31 -39 -16 -31 4 -38 22}
    set ::globals(rockCoords,3) {12 -39 -2 -26 -28 -37 -38 -14 -21 9 -34 \
				     34 -6 38 35 23 21 -14 36 -25}
    # --- Medium rock coords
    set ::globals(rockCoords,4) {-7 -19 -19 -15 -12 -5 -19 0 -19 13 -9 19 \
				     12 16 18 11 13 6 19 -1 16 -17}
    set ::globals(rockCoords,5) {9 -19 18 -8 7 0 15 15 -7 13 -16 17 -18 3 \
				     -13 -6 -16 -17}
    set ::globals(rockCoords,6) {2 18 18 10 8 0 18 -13 6 -18 -17 -14 -10 \
				     -3 -13 15}
    # --- Small rock coords
    set ::globals(rockCoords,7) {-8 -8 -5 -1 -8 3 0 9 8 4 8 -5 1 -9}
    set ::globals(rockCoords,8) {-6 8 1 4 8 7 10 -1 4 -10 -8 -6 -4 0}
    set ::globals(rockCoords,9) {-8 -9 -5 -2 -8 5 6 8 9 6 7 -3 9 -9 0 -7}

    set ::numbers(0) [list -7 -10 -7 10 7 10 7 -10 -7 -10]
    set ::numbers(1) [list 0 -10 0 10]
    set ::numbers(2) [list -7 -10 7 -10 7 0 -7 0 -7 10 7 10]
    set ::numbers(3) [list -7 -10 7 -10 7 10 -7 10 7 10 7 0 -7 0]
    set ::numbers(4) [list -7 -10 -7 0 7 0 7 -10 7 10]
    set ::numbers(5) [list -7 10 7 10 7 0 -7 0 -7 -10 7 -10]
    set ::numbers(6) [list -7 -10 -7 10 7 10 7 0 -7 0]
    set ::numbers(7) [list -7 -10 7 -10 7 10]
    set ::numbers(8) [list -7 -10 7 -10 7 10 -7 10 -7 -10 -7 0 7 0]
    set ::numbers(9) [list 7 0 -7 0 -7 -10 7 -10 7 10]
}

proc buildUI {{root .}} {
    set base [expr {($root eq ".") ? "" : $root}]
    # --- menu
    option add *Menu.tearoff 0
    set m [menu $base.m]
    $root configure -menu $m
    menu $m.help
    $m add cascade -menu .m.help -label "Help" -underline 0
    $m.help add command -label About -under 0 -command About

    set w   $::globals(screenWidth)
    set h   $::globals(screenHeight)
    set off 10
    if {$::SMALL} {
	font create largeFont -family Arial -size 10
	font create smallFont -family Arial -size 7
	set y 20
    } else {
	font create largeFont -family Arial -size 20
	font create smallFont -family Arial -size 10
	set y 30
    }

    set c [canvas .c1 -width $w -height $h -bg black \
	       -highlightthickness 0 -bd 0]
    $c xview moveto 0 ; $c yview moveto 0
    $c create text $off $y -tag score -anchor w \
	-fill $::CLR(text) -font largeFont
    $c create text [expr {$w - $off}] $y -tag level -anchor e \
	-fill $::CLR(text) -font largeFont
    $c create text 2 [expr {$h - 2}] -fill $::CLR(text) -tag fps -anchor sw \
	-font smallFont

    wm title . "Asteroids"
    pack $c
    focus -force $c
    wm protocol . WM_DELETE_WINDOW appExit
    # Don't allow resizing until it doesn't something natural
    wm resizable . 0 0
}

proc appExit {} {
    set ::globals(gameOn) 0
    exit
}

proc gameOver {} {
    heartBeatOff
    checkHighScore
}

proc bindNavKeys {mode} {
    if {$mode eq "menu"} {
	bind . <KeyPress-Escape> {}
	bind . <KeyPress-N> newGame
	bind . <KeyPress-n> newGame
	bind . <KeyPress-E> appExit
	bind . <KeyPress-e> appExit
	bind . <KeyPress-H> displayHighScores
	bind . <KeyPress-h> displayHighScores
    } elseif {$mode eq "highScores"} {
	bind . <KeyPress-Escape> showMenu
	bind . <KeyPress-N> {}
	bind . <KeyPress-n> {}
	bind . <KeyPress-E> {}
	bind . <KeyPress-e> {}
	bind . <KeyPress-H> {}
	bind . <KeyPress-h> {}
    } elseif {$mode eq "game"} {
	bind . <KeyPress-Escape> {}
	bind . <KeyPress-N> {}
	bind . <KeyPress-n> {}
	bind . <KeyPress-E> {}
	bind . <KeyPress-e> {}
	bind . <KeyPress-H> {}
	bind . <KeyPress-h> {}
    }
}

proc checkHighScore {} {
    loadHighScores
    set lowScoreSet [lindex $::globals(highScores) end]
    set lowScore [lindex $lowScoreSet 1]
    if {$::globals(score) > $lowScore} {
	set w .hs
	toplevel $w -borderwidth 10
	wm title $w "New High Score!"
	wm resizable $w 0 0
	set l [label $w.label -text "Name:"]
	set e [entry  $w.entry -textvar ::_name -width 30]
	set b [button $w.btnOK -text OK -width 12 -command {set _res ""}]
	grid $l $e -sticky news -padx 4 -pady 2
	grid $b -column 1 -sticky e -padx 4 -pady 2
	bind $w <Return> [list $w.btnOK invoke]
	bind $w <Destroy> {set _res ""}
	$e selection range 0 end
	$e icursor 0
	focus -force $e
	raise $w
	grab set $w
	vwait _res
	destroy $w
	if {$::_name eq ""} {set ::_name "Unknown"}
	lappend ::globals(highScores) [list $::_name $::globals(score)]
	set ::_name ""
	cleanHighScores
	saveHighScores
	displayHighScores
    } else {
	showMenu
    }
}

proc displayHighScores {} {
    bindNavKeys highScores
    loadHighScores
    .c1 delete menu
    set scoreString ""
    set count 0
    set h  $::globals(screenHeight)
    set fh [expr {[font metrics largeFont -linespace] + 5}]
    set yLoc [expr {($h / 2) - (([llength $::globals(highScores)] / 2) * $fh)}]
    foreach scoreSet $::globals(highScores) {
	foreach {name score} $scoreSet {
	    incr count
	    set score [format %07d $score]
	    .c1 create text [expr {300*$::SCALE}] $yLoc \
		-text "${count}." -anchor e \
		-font largeFont -fill $::CLR(text) -tag highScore
	    .c1 create text [expr {320*$::SCALE}] $yLoc \
		-text $name -anchor w \
		-font largeFont -fill $::CLR(text) -tag highScore
	    .c1 create text [expr {460*$::SCALE}] $yLoc \
		-text $score -anchor w \
		-font largeFont -fill $::CLR(text) -tag highScore
	    incr yLoc $fh
	}
    }

    .c1 create text [expr {400*$::SCALE}] $yLoc \
	-text "Press \[Escape\] to return to Menu" \
	-anchor c -font smallFont -fill $::CLR(text) -tag highScore
}

proc loadHighScores {} {
    set ::globals(highScores) [list]
    if {[file readable $::globals(highScoreFile)]} {
	set fileID [open $::globals(highScoreFile) RDONLY]
	set ::globals(highScores) [read $fileID]
	close $fileID
    }
    cleanHighScores
}

proc cleanHighScores {} {
    set hs $::globals(highScores)
    for {set i [llength $hs]} {$i <= 10} {incr i} {
	lappend hs [list Blank 0]
    }
    set hs [lsort -index 1 -integer -decreasing $hs]
    set hs [lrange $hs 0 9]
    set ::globals(highScores) $hs
}

proc saveHighScores {} {
    set fileID [open $::globals(highScoreFile) {WRONLY CREAT TRUNC}]
    puts $fileID $::globals(highScores)
    close $fileID
}

proc showMenu {} {
    .c1 delete rock
    .c1 delete highScore
    addRock 1 4
    addRock 2 4
    addRock 3 4
    .c1 create text [expr {400*$::SCALE}] [expr {250*$::SCALE}] \
	-text "\[ N \] ew Game\n\n\[ H \] igh Scores\n\n\[ E \] xit" \
	-tags menu -anchor c -font largeFont -fill $::CLR(text)
    bindNavKeys menu
}

proc newGame {} {
    bindNavKeys game
    .c1 delete menu
    set ::globals(lives) 3
    set ::globals(score) 0
    set ::globals(level) 0
    updateScore
    updateLives
    nextLevel
}

proc updateScore {{incrScore 0}} {
    incr ::globals(score) $incrScore

    # --- did our hero earn another ship?
    if {$::globals(score) >= $::globals(nextLife)} {
	updateLives 1
	incr ::globals(nextLife) $::globals(lifeEvery)
    }

    .c1 delete score
    drawNumber 150 30 $::globals(score) score
}

proc updateLives {{incrLives 0}} {
    .c1 delete life
    incr ::globals(lives) $incrLives
    for {set i 1} {$i <= $::globals(lives)} {incr i} {
	set obj [.c1 create polygon $::globals(shipCoords) -tag life \
		     -outline $::CLR(shiplife) -fill ""]
	.c1 move $obj [expr {20 + ($i * 18)}] 65
	if {$::SMALL} {
	    .c1 scale $obj 0 0 $::SCALE $::SCALE
	}
    }
}

proc nextLevel {} {
    .c1 delete rock
    .c1 delete missile
    .c1 delete level
    incr ::globals(level)
    drawNumber 730 30 $::globals(level) level
    addRock 1 [expr {3 + $::globals(level)}]
    if {!$::globals(shipExists)} {after 1000 addShip}

    # --- speed up the heartbeat...
    if {$::globals(sndOK)} {
	if {$::globals(beatDelay) > 200} {
	    incr ::globals(beatDelay) -50
	}
	heartBeatOff
	heartBeat 0
    }
}

# --- draw integer value using vectors (very Asteroids like...)
proc drawNumber {xloc yloc val tag} {
    set digitList [split $val ""]
    set count 0
    for {set i [llength $digitList]; incr i -1} {$i >= 0} {incr i -1} {
	incr count
	set digit [lindex $digitList $i]
	set item [.c1 create line $::numbers($digit) -tags $tag \
		      -fill $::CLR(text)]
	.c1 move $item [expr {$xloc - ($count * 19)}] $yloc
	if {$::SMALL} {
	    .c1 scale $item 0 0 $::SCALE $::SCALE
	}
    }
}

proc calcMotionVectors {} {
    set PI 3.1415926
    for {set i 0} {$i <= 360} {incr i} {
	set ::vector(x,$i) [expr {cos($i * $PI / 180.0)}]
	set ::vector(y,$i) [expr {sin($i * $PI / 180.0) * -1}]
    }
}

proc updateFPS {} {
    set timeNow [clock clicks -milliseconds]
    set elapsedTime [expr {($timeNow - $::globals(timeStart)) / 1000.0}]
    set fps [expr {$::globals(frameCount) / $elapsedTime}]
    .c1 itemconfigure fps -text [format "%.2f" $fps]
    after 500 updateFPS
}

# --- original gameloop
#     This is not CPU friendly, as it just cranks the game as fast as it can
#     inside a while loop, though it seems to provide a good
#     "gaming experience"
proc gameLoop0 {time} {
    set ::TIGHTLOOP 1
    set timeBefore $::globals(timeStart)
    set timeAfter $::globals(timeStart)
    while {$::globals(gameOn)} {
	set timeDelta [expr {$timeAfter - $timeBefore}]
	set timeBefore $timeAfter
	if {$timeDelta} {
	    set timeSlice [expr {$timeDelta / 1000.0}]
	    nextFrame $timeSlice
	    incr ::globals(frameCount)
	}
	# --- make sure we've used at least 10 ms for the current frame.
	#     otherwise, the timeslice value is *so* small, that some of the
	#     animation scaling tends to go whacky...
	#     This has the effect of limiting the max FPS to 100
	while {$timeAfter - $timeBefore < 10} {
	    set timeAfter [clock clicks -milliseconds]
	}
    }
}

# --- modified game loop (provided by Jeff Hobbs)
#     This is a modification of the original gameloop code (above) that still
#     uses the event loop for processing (no while loop).  Because the event
#     loop is being used, it tends to be much more CPU friendly.  In my
#     initial testing, it seems to provide basically the same feel as the
#     original loop.  I don't know how it holds up under varying CPU loads,
#     slower systems, etc...
proc gameLoop1 {time} {
    set ::TIGHTLOOP 0
    if {$::globals(gameOn)} {
	set now [clock clicks -milliseconds]
	set delta [expr {$now - $time}]
	if {$delta} {
	    set timeSlice [expr {$delta / 1000.0}]
	    nextFrame $timeSlice
	    incr ::globals(frameCount)
	}
	after 5 [list gameLoop $now]
    }
}

# Rename either gameLoop0 or gameLoop1 to the *real* gameLoop
rename gameLoop1 gameLoop

proc nextFrame {timeSlice} {
    set screenHeight $::globals(screenHeight)
    set screenWidth  $::globals(screenWidth)

    # --- dust motion
    foreach item [.c1 find withtag dust] {
	.c1 move $item $::dust($item,xDelta) $::dust($item,yDelta)
	incr ::dust($item,life) -1
	if {$::dust($item,life) <= 0} {
	    .c1 delete $item
	    array unset ::dust "$item,*"
	}
    }

    # --- wreckage motion
    foreach item [.c1 find withtag wreckage] {
	.c1 move $item $::wreckage($item,xDelta) $::wreckage($item,yDelta)
	incr ::wreckage($item,life) -1
	if {$::wreckage($item,life) <= 0} {
	    .c1 delete $item
	    array unset ::wreckage "$item,*"
	}
    }

    # --- missile motion
    foreach shot [.c1 find withtag heroMissile] {
	set xCen $::missile($shot,xCen)
	set yCen $::missile($shot,yCen)
	set xDelta $::missile($shot,xDelta)
	set yDelta $::missile($shot,yDelta)
	.c1 move $shot $xDelta $yDelta
	set xCen [expr {$xCen + $xDelta}]
	set yCen [expr {$yCen + $yDelta}]

	# --- if off the screen, wrap it
	if {$xCen > $screenWidth} {
	    set xCen [expr {$xCen - $screenWidth}]
	    .c1 move $shot -$screenWidth 0
	} elseif {$xCen < 0} {
	    set xCen [expr {$xCen + $screenWidth}]
	    .c1 move $shot $screenWidth 0
	}
	if {$yCen > $screenHeight} {
	    set yCen [expr {$yCen - $screenHeight}]
	    .c1 move $shot 0 -$screenHeight
	} elseif {$yCen < 0} {
	    set yCen [expr {$yCen + $screenHeight}]
	    .c1 move $shot 0 $screenHeight
	}
	incr ::missile($shot,life) -1
	if {$::missile($shot,life) <= 0} {
	    .c1 delete $shot
	    array unset ::missile "$shot,*"
	    continue
	}

	set xPrev [expr {$xCen - $xDelta}]
	set yPrev [expr {$yCen - $yDelta}]
	set ::missile($shot,xCen) $xCen
	set ::missile($shot,yCen) $yCen

	# --- Since our animation is scaled by our timeSlice, it is possible
	#     that on a slow computer, a missile may be on one side of a
	#     target in one frame, and be on the other side in the next frame,
	#     without ever having triggered a collision.  To fix this, we'll
	#     create a line between the current missile position and the last
	#     position and see if the line intersects any asteroids...
	set ray [.c1 create line $xCen $yCen $xPrev $yPrev]
	foreach rock [.c1 find withtag "rock"] {
	    set overlapList [eval .c1 find overlapping [.c1 bbox $rock]]
	    if {[lsearch $overlapList $ray] >= 0} {
		# --- we've got a hit
		killRock $rock $timeSlice
		.c1 delete $shot
		array unset ::missile "$shot,*"
		break
	    }
	}
	.c1 delete $ray
    }

    # --- rock motion and rotation
    foreach obj [.c1 find withtag rock] {
	foreach {xmin ymin xmax ymax} [.c1 bbox $obj] {break}
	set xCen [expr {($xmax + $xmin) / 2}]
	set yCen [expr {($ymax + $ymin) / 2}]
	set xDim [expr {$xmax - $xmin}]
	set yDim [expr {$ymax - $ymin}]
	rotateItem .c1 $obj $xCen $yCen [expr {$::rock($obj,rot) * $timeSlice}]
	.c1 move $obj [expr {$::rock($obj,xVel) * $timeSlice}] \
	    [expr {$::rock($obj,yVel) * $timeSlice}]
	if {$xmin > $screenWidth} {
	    .c1 move $obj [expr {($screenWidth + $xDim) * -1}] 0
	} elseif {$xmax < 0} {
	    .c1 move $obj [expr {$screenWidth + $xDim}] 0
	}
	if {$ymin > $screenHeight} {
	    .c1 move $obj 0 [expr {($screenHeight + $yDim) * -1}]
	} elseif {$ymax < 0} {
	    .c1 move $obj 0 [expr {$screenHeight + $yDim}]
	}
    }

    # --- Ship ---
    if {$::globals(shipExists)} {
	set xDelta $::ship(xDelta)
	set yDelta $::ship(yDelta)
	set xCen   $::ship(xCen)
	set yCen   $::ship(yCen)
	set dir    $::ship(direction)

	# --- ship rotation
	if {$::keyStatus(LEFT) || $::keyStatus(RIGHT)} {
	    if {$::keyStatus(LEFT)} {
		set rotSpeed -$::ship(rotSpeed)
	    } else {
		set rotSpeed $::ship(rotSpeed)
	    }
	    set thisAngle [expr {$rotSpeed * $timeSlice}]
	    rotateItem .c1 ship $xCen $yCen $thisAngle
	    rotateItem .c1 flame $xCen $yCen $thisAngle
	    set dir [expr {$dir - $thisAngle}]
	    if {$dir > 360} {
		set dir [expr {$dir - 360}]
	    } elseif {$dir < 0} {
		set dir [expr {$dir + 360}]
	    }
	}

	# --- ship motion
	if {$::keyStatus(THRUST)} {
	    # --- don't overlap thrust sounds, as Snack sometimes crashes...
	    if {$::globals(sndThrust) == 0} {
		sndPlay sndThrust
		set ::globals(sndThrust) 1
		after 250 {set ::globals(sndThrust) 0}
	    }
	    incr ::ship(flameTimer)
	    if {$::ship(flameTimer) > 5} {
		set ::ship(flameTimer) 0
		if {$::ship(flameOn)} {
		    .c1 itemconfigure flame -state hidden
		    set ::ship(flameOn) 0
		} else {
		    .c1 itemconfigure flame -state normal
		    set ::ship(flameOn) 1
		}
	    }
	    set maxPerFrame [expr {$::ship(velocityMax) * $timeSlice}]
	    set newDelta [expr {$::ship(velocityMax) /
				($::ship(thrust) / $timeSlice / $timeSlice)}]
	    set intDir [expr {int($dir)}]
	    set xVector $::vector(x,$intDir)
	    set yVector $::vector(y,$intDir)
	    if {abs($xDelta) <= $maxPerFrame && abs($yDelta) <= $maxPerFrame} {
		set xDelta [expr {$xDelta + ($newDelta * $xVector)}]
		set yDelta [expr {$yDelta + ($newDelta * $yVector)}]
	    }
	} else {
	    if {$::ship(flameOn)} {
		set ::ship(flameOn) 0
		.c1 itemconfigure flame -state hidden
	    }
	}

	# --- decay the current speed, unless we're nearly stopped
	if {abs($xDelta) > .001} {
	    set xDelta [expr {$xDelta * .99}]
	} else {
	    set xDelta 0
	}
	if {abs($yDelta) > .001} {
	    set yDelta [expr {$yDelta * .99}]
	} else {
	    set yDelta 0
	}

	.c1 move ship $xDelta $yDelta
	.c1 move flame $xDelta $yDelta

	set xCen [expr {$xCen + $xDelta}]
	set yCen [expr {$yCen + $yDelta}]
	if {$xCen > $screenWidth} {
	    set xCen [expr {$xCen - $screenWidth}]
	    .c1 move ship -$screenWidth 0
	    .c1 move flame -$screenWidth 0
	} elseif {$xCen < 0} {
	    set xCen [expr {$xCen + $screenWidth}]
	    .c1 move ship $screenWidth 0
	    .c1 move flame $screenWidth 0
	}
	if {$yCen > $screenHeight} {
	    set yCen [expr {$yCen - $screenHeight}]
	    .c1 move ship 0 -$screenHeight
	    .c1 move flame 0 -$screenHeight
	} elseif {$yCen < 0} {
	    set yCen [expr {$yCen + $screenHeight}]
	    .c1 move ship 0 $screenHeight
	    .c1 move flame 0 $screenHeight
	}

	# --- see if we've been hit...
	foreach {xmin ymin xmax ymax} [.c1 bbox ship] {break}
	foreach item [.c1 find overlapping $xmin $ymin $xmax $ymax] {
	    set tagList [.c1 gettags $item]
	    if {[lsearch $tagList rock] >= 0} {
		killRock $item $timeSlice
		addWreckage $timeSlice
		.c1 delete ship
		.c1 delete flame
		set ::globals(shipExists) 0
		updateLives -1
		after 3000 addShip
		return
	    }
	}

	set ::ship(xCen)      $xCen
	set ::ship(yCen)      $yCen
	set ::ship(xDelta)    $xDelta
	set ::ship(yDelta)    $yDelta
	set ::ship(direction) $dir

	# --- handle FIRE!
	if {$::keyStatus(FIRE)} {
	    addMissile $timeSlice
	}

	# --- handle HYPERSPACE
	if {$::keyStatus(HYPER)} {
	    if {$::globals(hyperOK)} {
		set ::globals(hyperOK) 0
		set newX [random $screenWidth]
		set newY [random $screenHeight]
		set xDelta [expr {$newX - $::ship(xCen)}]
		set yDelta [expr {$newY - $::ship(yCen)}]
		.c1 move ship $xDelta $yDelta
		set ::ship(xCen) $newX
		set ::ship(yCen) $newY
		.c1 move flame $xDelta $yDelta
		# --- don't allow another hyper-jump for 1/2 seconds
		after 500 {set ::globals(hyperOK)} 1
	    }
	}
    }

    # --- draw the frame
    if {$::TIGHTLOOP} {
	update
    } else {
	update idle
    }
}

proc killRock {rock timeSlice} {
    sndPlay sndExplosion
    foreach {xmin ymin xmax ymax} [.c1 bbox $rock] {break}
    set xCen [expr {($xmax + $xmin) / 2}]
    set yCen [expr {($ymax + $ymin) / 2}]
    addDust $xCen $yCen $timeSlice
    set type $::rock($rock,type)
    if {$type == 1} {updateScore 20}
    if {$type == 2} {updateScore 50}
    if {$type == 3} {updateScore 100}
    incr type
    if {$type <= 3} {
	addRock $type 1 $xCen $yCen
	addRock $type 1 $xCen $yCen
    }
    .c1 delete $rock
    array unset ::rock "$rock,*"
    if {![llength [.c1 find withtag "rock"]]} {
	after 2500 nextLevel
    }
}

proc addDust {xLoc yLoc timeSlice} {
    for {set i 0} {$i <= 8} {incr i} {
	set speed [expr {30 * $::SCALE}]
	set dustSpeed [expr {$speed + [random $speed]}]
	set ang [random 360]
	set obj [.c1 create rectangle $xLoc $yLoc $xLoc $yLoc -tag dust \
		     -outline $::CLR(dust) -fill $::CLR(dust)]
	# no need to scale - this is added at correct location
	set ::dust($obj,xDelta) \
	    [expr {$::vector(x,$ang) * $dustSpeed * $timeSlice}]
	set ::dust($obj,yDelta) \
	    [expr {$::vector(y,$ang) * $dustSpeed * $timeSlice}]
	# --- calculate the approximate number of frames in 1/2 second
	#     this will be the life of our dust particle
	set ::dust($obj,life) [expr {int(1/$timeSlice)}]
    }
}

proc addWreckage {timeSlice} {
    set wreckageSpeed [expr {25 * $::SCALE}]
    set coordList [.c1 coords ship]
    for {set i 0} {$i < [llength $coordList] - 2} {incr i} {
	set ang [random 360]
	set xs [lindex $coordList $i]
	set ys [lindex $coordList [expr {$i + 1}]]
	set xe [lindex $coordList [expr {$i + 2}]]
	set ye [lindex $coordList [expr {$i + 3}]]
	set obj [.c1 create line $xs $ys $xe $ye -tag wreckage \
		     -fill $::CLR(ship)]
	# no need to scale - this is added at correct location
	set ::wreckage($obj,xDelta) \
	    [expr {$::vector(x,$ang) * $wreckageSpeed * $timeSlice}]
	set ::wreckage($obj,yDelta) \
	    [expr {$::vector(y,$ang) * $wreckageSpeed * $timeSlice}]
	set ::wreckage($obj,life) [expr {int(1.5/$timeSlice)}]
	incr i
    }

}

proc addMissile {timeSlice} {
    if {!$::globals(newMissileOK)} {return}
    if {[llength [.c1 find withtag heroMissile]] >= 4} {return}
    sndPlay sndShot
    set ::globals(newMissileOK) 0
    set missileSpeed [expr {600 * $::SCALE}] ; # pixels per second
    set ang [expr {int($::ship(direction))}]
    set xs  [expr {$::ship(xCen) + $::vector(x,$ang) * (16 * $::SCALE)}]
    set ys  [expr {$::ship(yCen) + $::vector(y,$ang) * (16 * $::SCALE)}]
    set obj [.c1 create rectangle $xs $ys $xs $ys -tag heroMissile \
		 -outline $::CLR(missile) -fill $::CLR(missile)]
    set ::missile($obj,xDelta) \
	[expr {$::vector(x,$ang) * $missileSpeed * $timeSlice}]
    set ::missile($obj,yDelta) \
	[expr {$::vector(y,$ang) * $missileSpeed * $timeSlice}]
    set ::missile($obj,xCen) $xs
    set ::missile($obj,yCen) $ys
    set ::missile($obj,life) [expr {int(1.2/$timeSlice)}]
    # --- limit firing of a new missile to every 100 ms
    after 100 {set ::globals(newMissileOK) 1}
}

proc addShip {} {
    if {$::globals(shipExists)} {return}
    if {!$::globals(lives)} {
	gameOver
	return
    }
    set sw $::globals(screenWidth)
    set sh $::globals(screenHeight)
    # --- Create a square safety zone around the ship.  If anything is
    #     intersecting the zone, don't place the ship yet.  We want to
    #     give our hero a fighting chance...
    set screenXCen [expr {$sw / 2}]
    set screenYCen [expr {$sh / 2}]
    set offset [expr {100 * $::SCALE}]
    set xMin [expr {($sw - $offset) / 2}]
    set xMax [expr {($sw + $offset) / 2}]
    set yMin [expr {($sh - $offset) / 2}]
    set yMax [expr {($sh + $offset) / 2}]
    if {[llength [.c1 find overlapping $xMin $yMin $xMax $yMax]] > 0} {
	# --- Something is *very* close to the ship - wait and try again.
	after 100 addShip
	return
    }

    set obj [.c1 create polygon $::globals(shipCoords) -tag ship \
		 -outline $::CLR(ship) -fill ""]
    if {$::SMALL} {
	.c1 scale $obj 0 0 $::SCALE $::SCALE
    }
    .c1 move $obj $screenXCen $screenYCen
    set obj [.c1 create polygon $::globals(flameCoords) -tag flame \
		 -outline $::CLR(flame) -fill "" -state hidden]
    if {$::SMALL} {
	.c1 scale $obj 0 0 $::SCALE $::SCALE
    }
    .c1 move $obj $screenXCen $screenYCen
    set ::ship(direction)     90
    set ::ship(flameOn)       0
    set ::ship(flameTimer)    0   ; # flicker the flame every 5 frames
    set ::ship(rotSpeed)      270 ; # degrees per second
    set ::ship(velocity)      0   ; # pixels per second
    set ::ship(xCen)          $screenXCen
    set ::ship(yCen)          $screenYCen
    set ::ship(xDelta)        0
    set ::ship(yDelta)        0
    set ::ship(velocityMax)   [expr {250 * $::SCALE}] ; # pixels per second
    set ::ship(velocityDecay) 3   ; # ship takes 3 secs to stop from full speed
    set ::ship(thrust)        .75 ; # ship takes .75 secs to reach full speed
    set ::globals(shipExists) 1
}

proc addRock {type {num 1} {xLoc ""} {yLoc ""}} {
    for {set i 1} {$i <= $num} {incr i} {
	set coordList \
            $::globals(rockCoords,[expr {[random 3] + 1 + (3 * ($type - 1))}])
	set speed [expr {40 * $::SCALE}]
	set xVel [expr {10 + [random $speed] + \
			    ($type * ([random $speed] + 1)) + \
			    ($::globals(level) * ([random 5] + 1))}]
	set yVel [expr {10 + [random $speed] + \
			    ($type * ([random $speed] + 1)) + \
			    ($::globals(level) * ([random 5] + 1))}]
	set rotation [expr {20 + [random $speed]}]
	if {[random 2]} {set xVel -$xVel}
	if {[random 2]} {set yVel -$yVel}
	if {[random 2]} {set rotation -$rotation}
	# --- don't set a rock on top of the ship
	while {1} {
	    set obj [.c1 create polygon $coordList -tag rock \
			 -outline $::CLR(rock) -fill ""]
	    if {$::SMALL} {
		.c1 scale $obj 0 0 $::SCALE $::SCALE
	    }
	    foreach {xmin ymin xmax ymax} [.c1 bbox $obj] {break}
	    set xCen [expr {($xmax + $xmin) / 2}]
	    set yCen [expr {($ymax + $ymin) / 2}]
	    rotateItem .c1 $obj $xCen $yCen [random 360]
	    if {$xLoc eq "" || $num > 1} {
		set xLoc [random [expr {600 * $::SCALE}]]
	    }
	    if {$yLoc eq "" || $num > 1} {
		set yLoc [random [expr {400 * $::SCALE}]]
	    }
	    .c1 move $obj $xLoc $yLoc
	    if {$type != 1} {break}
	    if {!$::globals(shipExists)} {break}
	    set xCen $::ship(xCen)
	    set yCen $::ship(yCen)
	    set offset [expr {75 * $::SCALE}]
	    set xMin [expr {$xCen - $offset}]
	    set xMax [expr {$xCen + $offset}]
	    set yMin [expr {$yCen - $offset}]
	    set yMax [expr {$yCen + $offset}]
	    set overlap [.c1 find overlapping $xMin $yMin $xMax $yMax]
	    if {[lsearch $overlap $obj] >= 0} {
		.c1 delete $obj
	    } else {
		break
	    }
	}
	set ::rock($obj,xVel) $xVel
	set ::rock($obj,yVel) $yVel
	set ::rock($obj,rot)  $rotation
	set ::rock($obj,xLoc) $xLoc
	set ::rock($obj,yLoc) $yLoc
	set ::rock($obj,type) $type
    }
}

proc random {{range 100}} {
    return [expr {int(rand()*$range)}]
}

proc rotateItem {w tagOrId Ox Oy angle} {
    set angle [expr {$angle * atan(1) * 4 / 180.0}]
    foreach id [$w find withtag $tagOrId] {
	set xy {}
	foreach {x y} [$w coords $id] {
	    # rotates vector (Ox,Oy)->(x,y) by angle clockwise
	    set x [expr {$x - $Ox}]             ;# Shift to origin
	    set y [expr {$y - $Oy}]
	    set xx [expr {$x * cos($angle) - $y * sin($angle)}] ;# Rotate
	    set yy [expr {$x * sin($angle) + $y * cos($angle)}]
	    set xx [expr {$xx + $Ox}]           ;# Shift back
	    set yy [expr {$yy + $Oy}]
	    lappend xy $xx $yy
	}
	$w coords $id $xy
    }
}

proc bindGameKeys {} {

    set LEFT_PRESS     "<KeyPress-Left>"
    set LEFT_RELEASE   "<KeyRelease-Left>"
    set RIGHT_PRESS    "<KeyPress-Right>"
    set RIGHT_RELEASE  "<KeyRelease-Right>"
    set THRUST_PRESS   "<KeyPress-Up>"
    set THRUST_RELEASE "<KeyRelease-Up>"
    set HYPER_PRESS    "<KeyPress-Down>"
    set HYPER_RELEASE  "<KeyRelease-Down>"
    set FIRE_PRESS     "<KeyPress-space>"
    set FIRE_RELEASE   "<KeyRelease-space>"

    bind . $LEFT_PRESS   {set ::keyStatus(LEFT) 1}
    bind . $RIGHT_PRESS  {set ::keyStatus(RIGHT) 1}
    bind . $THRUST_PRESS {set ::keyStatus(THRUST) 1}
    bind . $FIRE_PRESS   {set ::keyStatus(FIRE) 1}
    bind . $HYPER_PRESS  {set ::keyStatus(HYPER) 1}

    bind . $LEFT_RELEASE   {set ::keyStatus(LEFT) 0}
    bind . $RIGHT_RELEASE  {set ::keyStatus(RIGHT) 0}
    bind . $THRUST_RELEASE {set ::keyStatus(THRUST) 0}
    bind . $FIRE_RELEASE   {set ::keyStatus(FIRE) 0}
    bind . $HYPER_RELEASE  {set ::keyStatus(HYPER) 0}

    set ::keyStatus(LEFT) 0
    set ::keyStatus(RIGHT) 0
    set ::keyStatus(THRUST) 0
    set ::keyStatus(FIRE) 0
    set ::keyStatus(HYPER) 0
}

proc testForSounds {} {
    # --- define sndPlay as a no-op proc in case we don't have the required
    #     sound support

    proc sndPlay {snd} {}

    # --- if the Snack package can't be found, return...
    if {[catch {package require snack}]} return

    # --- load the sounds if available
    foreach {snd file} {
	sndShot shot.wav sndExplosion explosion.wav
	sndThrust thrust.wav sndBeat1 beat1.wav sndBeat2 beat2.wav
    } {
	if {[file readable $file]} {
	    sound $snd -file $file
	} else {
	    return
	}
    }

    # --- OK, here we have the necessary sound support, so redefine the sndPlay
    proc sndPlay {snd} {
	$snd play
    }
    set ::globals(sndOK) 1
}

# --- manage the heartbeat sound
proc heartBeat {count} {
    if {$count == 0} {
	set snd sndBeat1
	set count 1
    } else {
	set snd sndBeat2
	set count 0
    }
    sndPlay $snd
    after $::globals(beatDelay) heartBeat $count
}

# --- cancel the heartbeat sound
proc heartBeatOff {} {
    after cancel {heartBeat 0}
    after cancel {heartBeat 1}
}

proc About {} {
    set msg "Asteroids\n\nJeff Godfrey          \nFebruary, 2005"
    tk_messageBox -title About -message $msg
}

main
