#!/bin/sh
# The next line is executed by /bin/sh, but not tcl \
exec wish "$0" ${1+"$@"}

# -*- tcl -*-
#
# TCL - The Audience Is Programming
#
# Author: Karl Lehenbauer, ProcPlace.com
#         First presented at Tcl'2001 (San Diego, CA)
#
# Updates by Jeff Hobbs, ActiveState Corp.
#

package require Tk 8.2
package require sound 2.0 ; # part of snack

proc play_tcl {c} {
    $c create image $::DIM(tclX) $::DIM(tclY) -image tclPhoto -anchor c
    fade_up tclPhoto 0.2
}

proc play_taip {c} {
    $c create image $::DIM(taipX) $::DIM(taipY) -image taipPhoto -anchor n
    fade_up taipPhoto 0.2
}

proc sequencer {canv} {
    after 8000  [list play_tcl $canv]
    after 15000 [list play_taip $canv]
}

proc fade_up {photo gamma} {
    $photo configure -gamma $gamma
    update idletasks
    if {$gamma < 1.0} {
        after 30 [list fade_up $photo [expr {$gamma + 0.025}]]
    }
}

proc start_sound {} {
    sound::sound s -file $::DIR/taip.mp3 -debug 0
    s play -block 0 -command done
}

proc done {} {
    after 1000 exit
}

proc doit {{root .} {constrained 1}} {
    image create photo tclPhoto -file $::DIR/taip-tcl.gif
    set tclImageHeight [image height tclPhoto]
    set tclImageWidth  [image width tclPhoto]

    image create photo taipPhoto -file $::DIR/taip-taip.gif
    set taipImageHeight [image height taipPhoto]

    # arbitrary vertical spacing between images
    set vSpace 20

    if {![winfo exists $root]} {
	toplevel $root
    }
    wm overrideredirect $root 1

    set swidth  [winfo screenwidth $root]
    set sheight [winfo screenheight $root]
    if {$constrained} {
	set width  [expr {$tclImageWidth + 160}]
	set height [expr {$tclImageHeight + $taipImageHeight + $vSpace + 60}]
	wm geometry $root ${width}x${height}+[expr \
		{($swidth-$width)/2}]+[expr {($sheight-$height)/2}]
    } else {
	if {[string equal "windows" $::tcl_platform(platform)]} {
	    wm state $root zoomed
	} else {
	    wm geometry $root ${swidth}x${sheight}+0+0
	}
    }
    raise $root

    set c [canvas $root.c -background black]
    pack $c -fill both -expand 1
    update idletasks

    set width  [winfo width $c]
    set height [winfo height $c]

    # Anchored at the center
    set ::DIM(tclX)  [expr {$width / 2}]
    set ::DIM(tclY)  [expr {($height / 2) - $vSpace}]

    # Anchored to the north
    set ::DIM(taipX) [expr {$width / 2}]
    set ::DIM(taipY) [expr {$::DIM(tclY) + $vSpace + ($tclImageHeight / 2)}]

    start_sound
    sequencer $c

    bind $root <Triple-1> { exit }
}

set f [info script]
while {[file type $f] eq "link"} {
    set f [file join [file dirname $f] [file readlink $f]]
}
set ::DIR [file dirname $f]
unset f

if {!$tcl_interactive} {
    doit .
}
