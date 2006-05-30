#!/bin/sh
# The next line is executed by /bin/sh, but not tcl \
exec wish $0 ${1+"$@"}

#
# TIMER
#
# A simple timer application.  This was originally coded up quickly
# for use at a Tcl conference to time talks.
#
# Copyright (c) 1998 Jeffrey hobbs
#
# RCS: @(#) $Id: timer.tcl,v 1.2 2006/05/30 14:08:49 mic42 Exp $

package require Tk

wm title . "Timer 00:00"

array set G {
    warnsecs	120
    continue	0
    final	0
    start	0
    longsecs	300
    talktime    10
}

proc start {} {
    global G
    set t $G(talktime)
    if {![regexp {^[0-9]+$} $t] || $t < 1 || $t >= 120} {
	error "Talk time must be number in minutes between 1 and 120"
    }
    .count config -fg black
    set G(continue) 1
    set G(start) [ctime]
    set G(final) [expr {$t*60 + $G(start)}]
    countdown
}

proc stop {} {
    global G
    set G(continue) 0
    wm title . "Timer"
    after cancel $G(after)
}

proc ctime {{delay 59000}} {
    global G
    set s [clock seconds]
    set G(time) [clock format $s -format %R]
    after $delay [info level 0]
    return $s
}

proc countdown {{delay 990}} {
    global G
    set rem [expr {$G(final)-[clock seconds]}]
    if {$rem <= 0} {
	set G(count) "OUT OF TIME"
	stop
	return
    }
    if {$rem < $G(warnsecs)} {
	.count config -fg red
	set G(count) [format %02d:%02d [expr {$rem/60}] [expr {$rem%60}]]
    } elseif {$rem < $G(longsecs)} {
	set G(count) [format %02d:%02d [expr {$rem/60}] [expr {$rem%60}]]
    } else {
	set G(count) [format "%02d min" [expr {$rem/60}]]
    }
    wm title . "$G(count)"
    if {$G(continue)} { set G(after) [after $delay [info level 0]] }
}

ctime

option add *font {Courier 36 bold}
label .count -textvariable G(count) -font {Courier 108 bold} -bg white
label .ttime -text "Talk Time: "
entry .tent -textvariable G(talktime) -width 4
label .ctime -text "Current Time: "
label .cent -textvariable G(time)
frame .bs
button .start -text "START" -command {start}
button .stop -text "STOP" -command {stop}
button .exit -text "EXIT" -command {exit}

grid .count - - - -sticky news
grid .ttime .tent .ctime .cent -sticky ew
grid .bs - - - -sticky ew -padx 6 -pady 6
grid .start .stop .exit -in .bs -sticky ew -padx 4
