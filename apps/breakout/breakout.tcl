#!/bin/sh
# \
exec wish "$0" ${1+"$@"}

#
# BREAKOUT
#
# A tclet/app variant of the classic game by Donal Fellows.
# http://www.cs.man.ac.uk/~fellowsd/tcl/breakout.html
#
# Copyright (c) 2000 Donal Fellows
#
# RCS: @(#) $Id: breakout.tcl,v 1.1 2001/11/07 22:30:33 hobbs Exp $

array set leveldata {
    1 {
	{}
	{offset none solid none count5 none solid none count5 none solid}
	{} {}
	{brk7 brk7 brk7 brk7 brk7 brk7 brk7 brk7 brk7 brk7}
	{offset brk6 brk6 brk6 brk6 brk6 brk6 brk6 brk6 brk6 brk6 brk6}
	{brk5 brk5 brk5 brk5 brk5 brk5 brk5 brk5 brk5 brk5}
	{offset brk4 brk4 brk4 brk4 brk4 brk4 brk4 brk4 brk4 brk4 brk4}
	{brk3 brk3 brk3 brk3 brk3 brk3 brk3 brk3 brk3 brk3}
	{offset brk2 brk2 brk2 brk2 brk2 brk2 brk2 brk2 brk2 brk2 brk2}
	{brk1 brk1 brk1 brk1 brk1 brk1 brk1 brk1 brk1 brk1}
	{}
	{}
	{offset count5 count4 count3 count2 count1 count0 count1 count2 count3 count4 count5}
    }
    2 {
	{} {}
	{count5 count5 count5 count5 count5 count5 count5 count5 count5 count5}
	{} {} {}
	{solid solid solid solid solid solid solid solid brk1 brk1}
	{offset solid solid solid solid solid solid solid solid brk1 brk1 brk1}
	{solid solid solid solid solid solid solid brk1 brk1 brk1}
	{offset solid solid solid solid solid solid solid brk1 brk1 brk1 brk1}
	{solid solid solid solid solid solid brk1 brk1 brk1 brk1}
	{offset solid solid solid solid solid solid brk1 brk1 brk1 brk1 brk1}
	{solid solid solid solid solid brk1 brk1 brk1 brk1 brk1}
    }
    3 {
	{} {}
	{none none solid count5 count5 count5 count5 solid}
	{offset none brk7 none offset solid brk4 brk4 brk4 brk4 solid none offset brk7}
	{none none solid brk3 brk3 brk3 brk3 solid}
	{offset none brk6 none offset solid brk2 brk2 brk2 brk2 solid none offset brk6}
	{none none solid brk1 brk1 brk1 brk1 solid}
	{offset none brk5 none offset solid none none none none solid none offset brk5}
	{none none none offset solid solid solid solid solid}
    }
}

set leveldata(0) $leveldata(2); # Ugh! This is a bit nasty...

array set brickinfo {
    brk7   {0 magenta   magenta 220 brick}
    brk6   {0 red       red     160 brick}
    brk5   {0 orange    #FF8000 110 brick}
    brk4   {0 yellow    yellow  70  brick}
    brk3   {0 green     green   40  brick}
    brk2   {0 cyan      cyan    20  brick}
    brk1   {0 blue      blue    10  brick}
    solid  {0 solidnorm grey50  0   solidbrick}
    count0 {0 count0    #555555 200 count0}
    count1 {0 count1    #777777 200 count1}
    count2 {0 count2    #999999 200 count2}
    count3 {0 count3    #bbbbbb 200 count3}
    count4 {0 count4    #dddddd 200 count4}
    count5 {0 count5    #ffffff 200 count5}
}

array set imagedata {
    baticon.gif {
	R0lGODdhMAAwAOMAAAAAAJRVJK6ebvgUQDz4NP//////ANF6N3NBG0MnDgAAAAAAAAAAAAAA
	AAAAAAAAACwAAAAAMAAwAAAE/hDISau9OOvNu/9gKI6kFpxoUIqn4L7CuXYtbMfqjAV37+Y6
	Cs/3GsCAwSFRMGgegwAl0el8IUlSX5VQ/c2yty2BafSWwDaxqzAoWM9LtaDAbptHaJecrudf
	P3lkLgRjc256bW5/NFplhI+QXAOEBYscaGKRkE2UlhtgmYZhZTF4aY6PdIcwXZ4mrKgClKKs
	TyxFsbKFtIJvt72Ea8EvfE2kpb9kw3TDpzaur4hrbs24z3C4fLrO11jc24jHvnhZxpNj5j3Q
	GgY1sOZdN+sYBgcH7jaSS/MX9fcoParJI+HvBAIE+AbtGjiiYICDCSQEYijCIUQhE3EQtGcQ
	QUQLNgl/8OvH8aFHEylklLB4EgoHlh9dsit5UebMfzVt0qPZUicGnjF97jAZVCiGnEY1FE3K
	tGmQCAA7
    }
    bricks.gif {
	R0lGODdhHgBpAPQAAAAAAAAA/wC//wD/ADg4VTg4qjhNVThVODiNqjiqOFU4OFU4VVVGOFVV
	OKo4OKo4qqpxOKqqOP8AAP8A//9/AP//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	ACwAAAAAHgBpAAAF/iAATGRpnugpjuTjvnAsu9PD1nOuL0ur/7MeDkh0CYvII5LYWxabTiA0
	uhtSg9ZrTKndAh6LLmyx4pnP6HR6BZC43/C4PL56O+74vH5/lzjabnyCgw4KCoGEiXqHfoqO
	hYiPiYySioeVlo2Yg5ebnJqee52hopGkeAoAhaeoK4avsLGysmwUtre4ubq5K7cQv8DBwsO/
	FBAAvsTKywwMtsvQys7G0dXA09bZ2NnVztzW3t/R4eLM1OXS5+jD2+vCDAAQDO7B8CLN+Pn6
	+/tsFf8AAwocKHAFwAgIEypcyBBhhQgADjacSLFBg38UM068+FCjx4QcP4oMKdLjxZIf/k+i
	1KhyZcWOLjfCjMmQJM2FDQBEaHBTYU4RFoMKHUqUKJsBSJMqXcp06YqkCaJKnUq1atQBCQBA
	tcq164EDSLuK5QoW69izUsuiXat27VmwbtHCjTt2Ll2vZu+Szau3atu+VA8ASHAA8FTBIr4q
	Xsy4cWM2AiJLnky5MuUVkhFo3sy5s2fNAhAAyPy5tGkDBiKbXl06dWjWsDe7jk17Nm3YqW/H
	zq2bNe/ep18Dby18uGfbxjsbAIDAQHLOy0Wgnk69unXrbAJo3869u/fuK7YXGE++vPnz4wMU
	ACAevfv3BAhof0/fvXz19fOTv6+/P//++ckHoH4CDlhfgQbCIIdfgvYtyOB5/z1oHgEAFECA
	hOVRKEJ8HHbo4YcfihACADs=
    }
    backdrops.gif {
	R0lGODdhIABgAPcAAD8/P39/f7+/v8HBwQCzAIAbARIDAAAAAACIE8CKABISALwMjIj08hL/
	/wC/vwCNqIA/QhIhIQBgYASIAACKgAASEi6MAInzwBL/EgC/ALQEAPQAgP8AEr8AAFQACpSA
	ACISAGAAAC4A1omAiBISEqYAwA3A8gMS/wAAvwI9VAAAlAAAIgAAYAAE1oAAiBIAEiMAsAGA
	qAASDqnsLw2JAAMSAPAEMogAABIAAAAAuQCAqAQAAKy08FeK8g4S/6wEI1cAMg4AA9QAAPSA
	gP8SEs4Eg+AAAAMAAGAAAwAAMPWAAP8SAKzIMleIAA4SAKxcuVf0qA7/DgS/AACN8AA/8gAh
	/wBgv6zIVleIMg4SAwbcAADzgAD/EgAEgwAAA+QAAPSAwBsAMkKAACQSAAMAEADAAAASAAA9
	MEwEBAIAAPgABPSA8/8S/78Avzr8aU2IzSQSA2AAYACMBKD08wCNq4g/zRIhA6z8APWIxzwM
	APX0x///Er+/AD8E0SoARAQAA9oAlCmArgQSDgCA8wAS/7AAj/XAKv8SAb8AYAABAEAAxxMA
	EgColABCrgAhDgBgAGwA0fWARP8SA5AAAE7AxwMSEgB8AACKxwEAAACAACAMrAAAVwAADmD2
	WACK9wBkzQD0mwD/DgBUiAGU9QAi/7T23fWKRbDUlPX0rv//DmwK0fUARP8AA8wAAE6AAADU
	AED0ABP/ACDfCAD0AAD/AGCoAABCAAAhAAAKAFh0APf0AP//AACoJAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAIABgAAAI/gADCBwoYGAAAQUNGkwoEKFC
	gwAANGT4MEDEiRUFXjxI8eFGhxk/dlQoMqNFiRxNnsQYEiXIliwrljQ5U6bLkRBvqtRJk2fI
	mDaBekRpsSdEoyCJ5mSpdODHgxpJ6kzY9GnDqBp9UnXqE2rRmle/dg0LlmBWoWYJ4gyLlmtb
	tylhxpX7cujbs3Pt5tVbV+rdlXuXBhbcl/BavIWPDoab2Onfsn6xBnUMM+lky3y9Vp0qGXFa
	ojW3Iqa4tWzpsZpRp2Y5YEDWiE17whbYurbr1xuHzqZdm3fvk7txo7R92yBxrrCDH1e5PLnv
	1iofLn9ePHrF1sOhW98+fLv3ogGq/n+nSXu89e7md5ZPTz48+/bi3wOnnTt9cqX3x+cP/3u/
	7NzE/QZcfcJRZ5xtBQ440HQKTXdfdgJG1xyE2o03XYTvYbeefCRtyCFl8XGI3ocgkqhYiPKN
	aOJwBLIXnH/f+Rebftm9JtWDweGl3UWg4QjjRjuOOKNigAXJ24ISgngbkNUF6CSSRvLXoJMY
	9lejfMUxSdl7Ws6Xk4+bHblSj2DW1yWQW/6kpI7FoYgklEteSR2VB8Yp5oJUVvhmkW6a1yV4
	5xF4pqBl4nclj8g9SOSgGw5p3Jp87onnhVJGOmlvDPJnZ6WX9tmonB/+GVuhkQW5WZmCGdlj
	dKtWiqakFNdB+qemebbpKqi0PlknmxxmeWVAADs=
    }
    count.gif {
	R0lGODdhHgBaAMIAAAAAAPgUQDz4NFBQ+P///wAAAAAAAAAAACwAAAAAHgBaAAAD/ggQ3P7w
	qRWEvTjry6gdYCiO5GA13xCtD3hWILPNXODKaiWUvCkwJpwuxnK4fkEYLEer3ZK7Ya4I9Nl+
	Smlv9IISm7MqNkVlHVG7rfrlAbs7irI8MKHMixOZG9zRbwmAPS9SK4AABCtnS3oYhocbYkJa
	IASHhyRdYypkDYCViEZWXos6F58YkaOcDp8topp+JJ+YWF5MjZ6HgBmRhA+VdYcQipMhnscE
	JWyMe2F0cXd4edEs081NfcXGyDyDq53ACoDDVqQZn7kEvDajk56UlyGZQt/gwmJIsGnMAoa7
	tUFU7WOFLtSJdim26aIFRNMtU+g8rbvhi1WwewaxaBuA+ewdQzr8rqF6ZocahDomV1gTCQnk
	xo6BPlbsdBFURhwPLXR8NJJilJfoRMxTUs+fTXYH9X0xNeFfKn0DwflrcAZhGkoxZ8mr5ZBf
	xFM9oUS11/QoPjQ8OnbD0oblumkpJcCN22Cl25Ftenhci3bKL4zkXgzU4M+Zz1sjgn4USIRB
	q8D56C01qs5JQKiN0emyWRWWu5pCueL0mu6f5TFj/5qlWm7jtmR8Xd59C41u3bm27c5mG0uW
	Wpn1HAdbrWgwrs2mu1gVoXgx5ilfj3aWfCucuMpP6UXdyfmVpIQcs8ZTZMsrcp6nY0FwdB1y
	X9/HYrfdDadkbgAJAAA7
    }
    solid.gif {
	R0lGODdhHgAeAMIAAAAAAH9/f7+/vz8/P4aGAP//AAAAAAAAACwAAAAAHgAeAAADsAgQ0v4w
	uqDADDjrzRllQieOwQBmQ6qu7LoNIZaStBzPAUGQO9ajN59Q9OvdgjnhrqfLNY022XCZLCYz
	R07Tx6RSG9EaCRzGSs6XkikmJs0WprZIVVG37nhTZYHuQygKaXIbIXxsgxxrbH5nL4uHiGFk
	BQUklRiXQBgxl5kcmZdxMJuYpZWdp5QBoZKlq6+nGbEoZbKgqLCvOKORGjglkHnCKGu9iXol
	xsQDdcLOKoAJADs=
    }
}

# ----------------------------------------------------------------------

proc makeimageset {sourcename w h args} {
    set y 0
    set killme 0
    incr w -1
    if {[catch {image height $sourcename}]} {
	global imagedata embed_args argv0
	if {[array exist embed_args] || \
		![file exists [file dirname $argv0]/$sourcename]} {
	    set sourcename [image create photo -format GIF \
		    -data $imagedata($sourcename)]
	} else {
	    set sourcename [image create photo -file \
		    [file dirname $argv0]/$sourcename]
	}
	set killme 1
    }
    foreach n $args {
	image create photo $n
	$n copy $sourcename -from 0 $y $w [expr {$y+$h-1}]
	incr y $h
    }
    if $killme {rename $sourcename {}}
    return [expr {$y/$h}]
}

set i 0
incr i [makeimageset bricks.gif     30 15  magenta red orange yellow green cyan blue]
incr i [makeimageset solid.gif      30 15  solidnorm solidflash]
incr i [makeimageset count.gif      30 15  count0 count1 count2 count3 count4 count5]
incr i [makeimageset backdrops.gif  32 32  backdrop0 backdrop1 backdrop2]
incr i [makeimageset baticon.gif    48 48  baticon]

proc makegraphics {w} {
    catch {destroy $w}
    frame $w -class BounceFrame;# -colormap new -visual truecolor
    canvas $w.lives -bg black -highlightthick 0 -borderwidth 0 \
	    -height 64 -width 64
    label $w.score -bg black -highlightthick 0 -borderwidth 0 \
	    -fg white -textvar score -font -*-helvetica-bold-o-*-*-*-240-*
    frame $w.sep -bg red -highlightthick 0 -borderwidth 0 -height 2
    canvas $w.c -bg black -highlightthick 0 -borderwidth 0 \
	    -width 300 -height 400

    pack $w.c -side bottom
    pack $w.sep -side bottom -fill x
    pack $w.lives -side left -fill both -expand 1
    pack $w.score -side left -anchor e -fill y -ipadx 10

    image create photo backdrop$w -width 300 -height 400
    $w.c create image 0 0 -image backdrop$w -tags {backdrop} -anchor nw

    return $w
}

proc makeborder {w} {
    global backid
    $w.c create rectangle -400 -400 700   0 -fill black -tags {bordertop    drop}
    $w.c create rectangle -400 -400   0 700 -fill black -tags {borderside   drop}
    $w.c create rectangle  300 -400 700 700 -fill black -tags {borderside   drop}
    $w.c create rectangle -400  400 300 700 -fill black -tags {borderbottom drop}
    backdrop$w copy backdrop$backid -to 0 0 300 400
    $w.c raise backdrop
    incr backid
    if [catch {image height backdrop$backid}] {set backid 0}
}

proc makebrick {w x y type} {
    global scores brickids brickcount brickinfo
    if ![info exist brickinfo($type)] {
	return -code error "Unknown block type: $type"
    }
    foreach {border Image color score optag} $brickinfo($type) {}
    if $border {
	lappend ids [$w.c create rectangle $x $y [expr {$x+28}] [expr {$y+13}]\
		-fill $color -tags "$optag visible drop" -outline white] \
		[$w.c create image [expr {$x+1}] [expr {$y+1}] \
		-anchor nw -tags drop -image $Image]
    } else {
	lappend ids [$w.c create image $x $y -anchor nw \
		-tags "$optag drop" -image $Image]
    }
    foreach id $ids {
	set brickids($id) $ids
	set scores($id) $score
    }
    if $score {incr brickcount}
}

proc makebricks {w} {
    global leveldata backid

    # We use the backdrop counter as the level number indicator...

    set y 0
    foreach row $leveldata($backid) {
	set x 0
	foreach brick $row {
	    switch $brick {
		offset {incr x -15}
		none {incr x 30}
		default {
		    makebrick $w $x $y $brick
		    incr x 30
		}
	    }
	}
	incr y 15
    }
    return
}

proc makebat {w} {
    $w.c create rectangle 120 350 180 365 -fill brown -tags {bat visible drop}
    global batpos
    set batpos 150
}

proc makeball {w} {
    $w.c create oval 145 295 155 305 -fill grey85 -tags {ball drop}
    global ballx bally balldx balldy
    set ballx 150
    set bally 300
    set balldx 5
    set balldy -5
}

proc makescreen {w} {
    global scores brickids brickcount
    catch {unset scores}
    catch {unset brickids}
    $w.c delete drop
    makeborder $w
    set brickcount 0
    makebricks $w
    makebat $w
    $w.c raise brickcorner
    makeball $w
}

proc makepartscreen {w} {
    makebat $w
    $w.c raise brickcorner
    $w.c delete ball
    makeball $w
}

proc printlives {w l} {
    $w.lives delete all
    for {set i 0;set x 5} {$i<$l} {incr i} {
	$w.lives create image $x 5 -image baticon -anchor nw
	incr x 48
    }
}

# ----------------------------------------------------------------------

proc movebat {w} {
    global b1 b3 batpos motioncmds
    if {$b1 && $b3} {
	# Do nothing if both buttons pressed
    } elseif {$b1 && $batpos>=37} {
	incr batpos -7
	lappend motioncmds [list $w.c move bat -7 0]
    } elseif {$b3 && $batpos<=263} {
	incr batpos 7
	lappend motioncmds [list $w.c move bat 7 0]
    }
}

proc moveball {w} {
    global ballx bally balldx balldy motioncmds

    set nballdx $balldx
    set nballdy $balldy
    #set hitb 0
    foreach id [$w.c find overlapping \
	    [expr {$ballx-6}].5 [expr {$bally-6}].5 \
	    [expr {$ballx+4}].5 [expr {$bally+5}].5] {
	set t [lindex [$w.c gettags $id] 0]
	switch -glob $t {
	    borderside   {set nballdx [expr {-$balldx}]}
	    bordertop    {set nballdy [expr {-$balldy}]}
	    borderbottom {return 1}

	    count* -
	    solidbrick -
	    bat -
	    brick {
		set xx 0; set yy 0
		foreach {x1 y1 x2 y2} [$w.c bbox $id] {}
		incr x1; incr y1; # bbox brokenness!
		switch -glob $t {
		    count0 - brick {hitbrick $w $id}
		    count* {hitcount $w $id $t}
		    solidbrick {
			hitrubber $w $id
			set xx [expr {int(rand()*3)-1}]
			set yy [expr {int(rand()*3)-1}]
		    }
		}
		if {$ballx<=$x1} {
		    set nballdx [incr xx -5]
		} elseif {$ballx>=$x2} {
		    set nballdx [incr xx +5]
		}
		if {$bally<=$y1} {
		    set nballdy [incr yy -5]
		} elseif {$bally>=$y2} {
		    set nballdy [incr yy +5]
		}
	    }
	}
    }
    set balldx $nballdx
    set balldy $nballdy
    incr ballx $balldx
    incr bally $balldy
    lappend motioncmds [list $w.c move ball $balldx $balldy]
    return 0
}

proc hitrubber {w id} {
    global brickids cancel balldx balldy
    set imgid [lindex $brickids($id) 0]
    $w.c itemconf $imgid -image solidflash
    if {[info exist cancel(rubber,$imgid)]} {
	after cancel $cancel(rubber,$imgid)
    }
    set cancel(rubber,$imgid) [after 600 endrubber $w $imgid]
}

proc endrubber {w imgid} {
    global cancel
    unset cancel(rubber,$imgid)
    $w.c itemconf $imgid -image solidnorm
}

proc hitcount {w id t} {
    global score scores brickids
    array set next {
	count5 count4
	count4 count3
	count3 count2
	count2 count1
	count1 count0
    }
    set n $next($t)
    $w.c itemconf [lindex $brickids($id) 0] -tags "$n drop" -image $n
    incr score $scores($id)
}

proc hitbrick {w id} {
    global score scores brickids brickcount cancel
    eval $w.c delete $brickids($id)
    incr score $scores($id)
    foreach i $brickids($id) {
	unset scores($i) brickids($i)
    }
    if {![incr brickcount -1]} {
	set cancel(newscreen) [after 1000 "unset cancel(newscreen); makescreen $w"]
    }
}

proc domotion {w} {
    global stopped cancel motioncmds curvis
    if {!$curvis} return
    if {!$stopped} {
	set motioncmds {}
	movebat $w
	if {[moveball $w]} {
	    set stopped 1
	    set cancel(newgame) [after 500 death $w]
	}
	# Put off all motion until all detection has been done
	foreach c $motioncmds {eval $c}
    }
    set cancel(motion) [after 32 domotion $w]
}

proc dying {w vel} {
    global cancel restart
    if {[lindex [$w.c bbox drop] 1]<0} {
	incr vel 1
	$w.c move drop 0 $vel
	set cancel(death) [after 30 dying $w $vel]
    } else {
	set cancel(death) [after 100 {unset cancel(death);set restart 1}]
    }
}
proc killbat {w c} {
    global cancel batpos
    if {$c>15} {
	$w.c delete bat
	set cancel(death) [after 100 {unset cancel(death);set restart 1}]
    } else {
	$w.c scale bat $batpos 357.5 1.5 0.8
	$w.c move ball 0 5
	incr c 1
	set cancel(death) [after 80 killbat $w $c]
    }
}   

proc death {w} {
    global cancel score stopped restart backid lives
    unset cancel(newgame)
    incr lives -1
    printlives $w $lives
    if {$lives<1} {
	dying $w 0
	vwait restart
	set backid 0
	set score 0
	set lives 3
	printlives $w $lives
	makescreen $w
    } else {
	killbat $w 0
	vwait restart
	makepartscreen $w
    }
    set stopped 0
}

proc cycle {w c} {
    global cancel curvis
    if {!$curvis} return
    if {$c<=20} {
	$w.c itemconf visible -outline grey[expr $c*5]
    } else {
	$w.c itemconf visible -outline grey[expr 200-$c*5]
    }
    incr c
    if {$c>=40} {set c 0}
    set cancel(cycle) [after 40 cycle $w $c]
}

proc vischange {w d} {
    global curvis cancel
    switch $d {
	VisibilityUnobscured {
	    if {$curvis} {return}
	    set curvis 1
	    domotion $w
	    cycle $w 0
	}
	default {
	    if {!$curvis} {return}
	    catch {
		after cancel $cancel(motion)
		after cancel $cancel(cycle)
		unset cancel(motion) cancel(cycle)
	    }
	    set curvis 0
	}
    }
}

set curvis 1
set stopped 0
set score 0
set b1 0
set b3 0
set backid 0

bind . <1> {set b1 1}
bind . <ButtonRelease-1> {set b1 0}
bind . <2> {set b3 1}
bind . <ButtonRelease-2> {set b3 0}
bind . <3> {set b3 1}
bind . <ButtonRelease-3> {set b3 0}
bind . <Left> {set b1 1}
bind . <KeyRelease-Left> {set b1 0}
bind . <Right> {set b3 1}
bind . <KeyRelease-Right> {set b3 0}
bind . <Destroy> {
    foreach n [array names cancel] {
	after cancel $cancel($n)
    }
    exit
}

bind . <Visibility> {vischange .f %s}

. conf -bg black
if {[array exist embed_args]} {
    pack [makegraphics .f] -padx 20
} else {
    pack [makegraphics .f]
}
focus .f.c
printlives .f [set lives 3]

makescreen .f

domotion .f
cycle .f 0
