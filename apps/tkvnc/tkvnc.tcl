#!/bin/sh
# \
exec wish "$0" ${1+"$@"}

# Greg Baker's VNC viewer written in pure Tcl (using Tk for a GUI).
# (c)  Greg Baker,  Ifost  <gregb@ifost.org.au>
# Changes 2003 Copyright (c) Jeff Hobbs <jeff@hobbs.org>
# Changes 2003 Copyright (c) Miguel Sofer <msofer@users.sf.net>
#
# Status (9 Jan 2003).  
#  - DES encryption (i.e. authentication) is not implemented.  Therefore,
#    you can't connect to something that was started with Unix vncserver,
#    only Unix Xvnc -rfbport ... directly would work
#  - Tested against TightVNC's Xvnc and OSXVnc's OSXvnc-server
#  - raw encoding works
#  - I haven't seen a server using copyrect,  so I don't know if that works
#  - hextile is still a little confused and very slow (often kills Xvnc?)
#    There seems to be a CPU leak in the Tk image command.
#  - RRE is implemented,  but I think Xvnc doesn't give sensible results 
#  - CoRRE is implemented,  also gets strange results from tightvnc's server
#  - tight and zlib encodings are not implemented
#  - no support for SetColourMapEntries
#  - no support for ClientCutText (but ServerCutText is OK)
#  - Big-endian-ness is hard coded.  This is obviously a bit of a problem
#    on x86 platforms!
#
# Status (16 Jan 2003)
#  - DES still not implemented
#  - all standard encodings seem to work properly
#  - still works only at 8-bit color depth
#
# The easiest way to test this is on your unix server do:
#  Xvnc :1 -geometry 800x600 -depth 8 &
#  twm -display :1 &
#  xterm -display :1 &
# And on the client (which is any platform with Tk):
#  wish tkvnc.tcl ?server:1?
#

package require Tk 8.2

# should use ::vnc namespace
namespace eval ::vnc {
    variable VERSION 0.9.1

    variable PRIV
    array set PRIV {}

    #################################################################
    # You could try changing this, but I don't know if it will work.
    variable BITS_PER_PIXEL 8
    variable BINARY_FORMAT_FOR_PIXELS c
    variable RGB_MAX   {7 7 3}
    variable RGB_SHIFT {0 3 6}
    variable BYTES_PER_PIXEL [expr {$BITS_PER_PIXEL / 8}]

    # These are set correctly later
    variable FB; # current framebuffer information
    array set FB {
	width  0
	height 0
    }

    # Specify default encoding order
    set RAW      0
    set COPYRECT 1
    set RRE      2
    set CoRRE    4
    set HEXTILE  5
    variable ENC
    set ENC(order)     "1 5 4 2 0"
    set ENC($RAW)      ::vnc::RawEncoding
    set ENC($COPYRECT) ::vnc::CopyRectEncoding
    set ENC($RRE)      ::vnc::RRERectEncoding
    set ENC($CoRRE)    ::vnc::CoRRERectEncoding
    set ENC($HEXTILE)  ::vnc::HextileRectEncoding

    variable VNC; # vnc connection info
    array set VNC {}

    variable CurrentButtonMask 0
    variable fg_colour ""
    variable bg_colour ""
}

# EXCLUSIVE_SESSION = 1 doesn't seem to work

proc read_binary {chan format bytes} {
    set x [read $chan $bytes]
    set y ""
    binary scan $x $format y
    return $y
}

proc read_sized_string {chan} {
    set x   [read_binary $chan I 4]
    set str [read $chan $x]
    return $str
}

proc ::vnc::attach {server {port {}}} {
    foreach {host scrn} [split $server :] { break }
    if {$host == ""} {
	return -code error "No server specified"
    }
    if {$port == ""} {
	if {$scrn == ""} {
	    set port 5900
	} else {
	    # Port default to Screen# + 5900
	    set port [expr {$scrn + 5900}]
	}
    }
    if {[catch {set chan [socket $host $port]} err]} {
	return -code error "Unable to connect to '$host:$port':\n$err"
    }
    fconfigure $chan -translation binary

    GetConnectionDetails $chan
    UpdateUI $chan
    fileevent $chan readable [list ::vnc::ChannelReader $chan]
    SetPixelFormat $chan
    SetEncodings $chan
    variable FB
    FBUpdateRequest $chan 0 0 0 $FB(width) $FB(height)
}

proc ::vnc::GetConnectionDetails {chan {exclusive 0}} {
    variable VNC
    set VNC(chan)    $chan
    set VNC(version) [read $chan 12]
    puts -nonewline $chan "RFB 003.003\n"
    flush $chan

    #puts "Waiting for auth details"
    set VNC(auth) [read_binary $chan I 4]

    switch {$VNC(auth)} {
	0 {
	    set VNC(error) [read_sized_string $chan]
	    return -code error $VNC(error)
	}
	1 {
	    # cool!
	}
	2 {
	    set VNC(challenge) [read $chan 16]
	    return -code error "DES implementation in Tcl required to handle authenticated connections.  Do you have one?  Tell gregb@ifost.org.au"
	}
    }

    puts -nonewline $chan [binary format c $exclusive]
    flush $chan

    variable FB
    set FB(width)            [read_binary $chan S 2]
    set FB(height)           [read_binary $chan S 2]
    set FB(bpp)              [read_binary $chan c 1]
    set FB(depth)            [read_binary $chan c 1]
    set FB(big_endian_flag)  [read_binary $chan c 1]
    set FB(true_colour_flag) [read_binary $chan c 1]
    set FB(red_max)          [read_binary $chan S 2]
    set FB(green_max)        [read_binary $chan S 2]
    set FB(blue_max)         [read_binary $chan S 2]
    set FB(red_shift)        [read_binary $chan c 1]
    set FB(green_shift)      [read_binary $chan c 1]
    set FB(blue_shift)       [read_binary $chan c 1]
    set FB(padding)          [read $chan 3]
    set FB(title)            [read_sized_string $chan]
}


######################################################################
#  Client -> Server messages.
# There is one function for each of the standard messages that the
# RFB protocol describes, except for ClientCutText which isn't implemented.

proc ::vnc::FBUpdateRequest {chan incremental x y width height} {
    puts -nonewline $chan \
	[binary format ccSSSS 3 $incremental $x $y $width $height]
    flush $chan
}

proc ::vnc::SetPixelFormat {chan} {
    variable BITS_PER_PIXEL
    variable RGB_MAX
    variable RGB_SHIFT
    puts -nonewline $chan [binary format c 0]
    # Padding
    puts -nonewline $chan [binary format c3 {0 0 0}]
    # bits per pixel, depth, big-ending (1) or little-ending (0), true-colour
    puts -nonewline $chan [binary format cccc $BITS_PER_PIXEL $BITS_PER_PIXEL 1 1]
    # R/G/B max  and then R/G/B/shift
    puts -nonewline $chan [binary format S3c3 $RGB_MAX $RGB_SHIFT]
    # Padding
    puts -nonewline $chan [binary format c3 {0 0 0}]
    flush $chan
}

proc ::vnc::SetEncodings {chan} {
    variable ENC
    set num_prefs [llength $ENC(order)]
    puts -nonewline $chan [binary format cc 2 0]; # add padding
    puts -nonewline $chan [binary format S $num_prefs]
    puts -nonewline $chan [binary format I$num_prefs $ENC(order)]
    flush $chan
}

proc ::vnc::PointerEvent {chan state x y {button 0} {press 1}} {
    variable CurrentButtonMask
    puts -nonewline $chan [binary format c 5]
    # button mask  -- eight bit field for each of 8 buttons.  But
    # Tk can't give us that, I think.  It only seems to like individual
    # button down events.
    # hobbs is adding comments to test cvs
    if {[string equal $state move]} {
	set buttonmask $CurrentButtonMask
    } elseif {[string equal $state up]} {
	set buttonmask 0 ; # button release means no buttons
	set CurrentButtonMask $buttonmask
    } else {
	set buttonmask [expr {1 << ($button - 1)}]
	set CurrentButtonMask $buttonmask
    }

    puts -nonewline $chan [binary format cSS $buttonmask $x $y]
    flush $chan
}

proc ::vnc::KeyEvent {chan keycode {press 1}} {
    puts -nonewline $chan [binary format cc 4 $press]
    # padding
    puts -nonewline $chan xx
    puts -nonewline $chan [binary format I $keycode]
    flush $chan
}


######################################################################
# Server -> Client Messages
#
# Firstly a listener for filevent to use.    
# Only FbUpdate is complicated enough to warrant a separate
# function, really.
######################################################################


proc ::vnc::ChannelReader {chan} {
    set message_type [read_binary $chan c 1]
    if {[eof $chan]} { 
	fileevent $chan readable ""
	tk_messageBox -icon error -type ok -title "Lost Connection" \
	    -message "Lost connection to server"
	# actually, I should do something better here
	variable VNC
	unset VNC
	UpdateUI
	return
    }
    switch -- $message_type {
	0 { FBUpdate $chan }
	1 { error "Don't know how to handle SetColourMapEntries" }
	2 { bell }
	3 { ServerCutText $chan }
	default {error "Don't know what to do with message type $message_type"}
    }
}

proc ::vnc::ServerCutText {chan} {
    clipboard clear
    set padding [read $chan 3]
    set contents [read_sized_string $chan]
    clipboard append $contents
}


proc ::vnc::FBUpdate {chan} {
    variable SCREEN
    variable ENC
    variable FB
    set padding  [read $chan 1]
    set numrects [read_binary $chan S 2]
    for {set n 0} {$n < $numrects} {incr n} {
	set x_start  [read_binary $chan S 2]
	set y_start  [read_binary $chan S 2]
	set width    [read_binary $chan S 2]
	set height   [read_binary $chan S 2]
	set encoding [read_binary $chan I 4]
	#puts "Encoding type $encoding"
	if {[info exists ENC($encoding)]} {
	    $ENC($encoding) $chan $x_start $y_start $width $height
	} else {
	    puts stderr "Cannot handle encoding type $encoding"
	    exit
	}
    }
    FBUpdateRequest $chan 1 0 0 $FB(width) $FB(height)
    update idletasks
}

######################################################################
#   Now, each of the different encodings called in the previous function

# This function is extremely slow -- a 640x480 rectangle takes 4.1 seconds
# on my G3 PowerBook @ 400MhZ. 
# 
# Per 640 pixel scan-line,  this is:
#  - 8.7 microseconds total
#  - binary scan [read $chan ... ]  takes 330 microseconds
#  - $SCREEN put [...] takes 2746 microseconds
#  - update idletasks takes 600 microseconds,  which is why we don't do it 
#    every time
#  - The foreach loop takes around 6.2-7.3 miliseconds (6200 microseconds)
#  - If the body of the foreach loop is just lappend image_info_data "#ff00ff"
#    then each iteration is 3800-4000 microseconds 

#  - 
# It is around 35 microseconds per pixel.
# The lappend doesn't seem to take any appreciable time, and we are 
# very likely to succeed in finding rectpixel in the PIXELS cache.
# The binary scan takes around 240 microseconds for a 480 pixel scan line.
# The $vncscreen put takes around 3 seconds for a 640x480 rectangle.  The
# update idletasks takes 0.2 seconds.

proc ::vnc::RawEncoding {chan x_start y_start width height} {
    variable SCREEN
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BYTES_PER_PIXEL

    set bytes [expr {$width*$BYTES_PER_PIXEL}]
    for {set y 0} {$y < $height} {incr y} {
	set rectwords [split [read $chan $bytes] {}]
	set image_info_data {}
	foreach pixel $rectwords {
	    lappend image_info_data $PIXELS($pixel)
	}
  	$SCREEN put [list $image_info_data] -to $x_start [expr {$y_start + $y}]
	if {($y % 8) == 0} { update idletasks } 
    }
}

# or maybe this?
proc ::vnc::RawEncoding {chan x y width height} {
    variable SCREEN
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BYTES_PER_PIXEL

    set bytes [expr {$width*$height*$BYTES_PER_PIXEL}]
    if {!$bytes} { return }
    set rectwords [split [read $chan $bytes] {}]
    set img_data {}
    set w 0
    foreach pixel $rectwords {
	lappend line_data $PIXELS($pixel)
	if {([incr w] % $width) == 0} {
	    lappend img_data $line_data
	    set line_data {}
	}
    }
    $SCREEN put $img_data -to $x $y
    update idletasks
}

# or maybe this?
proc ::vnc::RawEncoding {chan x y width height} {
    variable SCREEN
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BYTES_PER_PIXEL

    set bytes [expr {$width*$height*$BYTES_PER_PIXEL}]
    if {!$bytes} { return }
    set rectwords [split [read $chan $bytes] {}]
    set w 0
    foreach pixel $rectwords {
	lappend line_data $PIXELS($pixel)
	if {([incr w] % $width) == 0} {
	    $SCREEN put [list $line_data] -to $x $y
	    set line_data {}
	    incr y
	    if {($y % 8) == 0} { update idletasks } 
	}
    }
}

# or maybe this?
proc ::vnc::RawEncoding {chan x y width height} {
    variable SCREEN
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BYTES_PER_PIXEL

    set bytes [expr {$width*$height*$BYTES_PER_PIXEL}]
    if {!$bytes} { return }
    set rectwords [split [read $chan $bytes] {}]

    set idx 0
    set endIdx $width
    set y_max [expr {$y + $height}]

    while {$y < $y_max} {
	set line_data {}
	while {$idx < $endIdx} {
	    lappend line_data $PIXELS([lindex $rectwords $idx])
	    incr idx
	}
	$SCREEN put [list $line_data] -to $x $y
	set idx $endIdx
	incr endIdx $width
	incr y
	if {!($y%8)} { update idletasks } 
    }
}


proc ::vnc::CopyRectEncoding  {chan x_start y_start width height} {
    variable SCREEN
    variable SCREENBUF
    set src_x [read_binary $chan S 2]
    set src_y [read_binary $chan S 2]

    #
    # As the copying is done line-by-line, we need to take care not
    # to overwrite the still-uncopied data when copying downwards
    #

    if {$src_y > $y_start} {
	$SCREEN copy $SCREEN \
		-from $src_x $src_y [expr {$src_x + $width}] [expr {$src_y + $height}]\
		-to $x_start $y_start
    } else {
	# Copy to an intermediate buffer to avoid overwriting
	$SCREENBUF copy $SCREEN \
		-from $src_x $src_y [expr {$src_x + $width}] [expr {$src_y + $height}] \
		-to 0 0
	$SCREEN copy $SCREENBUF \
		-from 0 0 $width $height \
		-to $x_start $y_start
    }    
}


proc ::vnc::RRERectEncoding {chan x_start y_start width height} {
    variable SCREEN
    set num_subrects [read_binary $chan I 4]
    set bg [ReadSinglePixelColour $chan]
    $SCREEN put $bg -to $x_start $y_start \
	    [expr {$x_start + $width}] [expr {$y_start + $height}] 
    for {set i 0} {$i < $num_subrects} {incr i} {
	set colour [ReadSinglePixelColour $chan]
	set x [expr {$x_start + ([read_binary $chan S 2] & 0xFFFF)}]
	set y [expr {$y_start + ([read_binary $chan S 2] & 0xFFFF)}]
	set w [expr {[read_binary $chan S 2] & 0xFFFF}]
	set h [expr {[read_binary $chan S 2] & 0xFFFF}]
	$SCREEN put $colour -to $x $y [expr {$x + $w}] [expr {$y + $h}] 
    }
}

proc ::vnc::CoRRERectEncoding {chan x_start y_start width height} {
    variable SCREEN
    set num_subrects [read_binary $chan I 4]
    set bg [ReadSinglePixelColour $chan]
    $SCREEN put $bg -to $x_start $y_start \
	    [expr {$x_start + $width}] [expr {$y_start + $height}] 
    for {set i 0} {$i < $num_subrects} {incr i} {
	set colour [ReadSinglePixelColour $chan]
	set x [expr {$x_start + ([read_binary $chan c 1] & 0xFF)}]
	set y [expr {$y_start + ([read_binary $chan c 1] & 0xFF)}]
	set w [expr {[read_binary $chan c 1] & 0xFF}]
	set h [expr {[read_binary $chan c 1] & 0xFF}]
	$SCREEN put $colour -to $x $y [expr {$x + $w}] [expr {$y + $h}] 
    }
}

proc ::vnc::HextileRectEncoding {chan x_start y_start width height} {
    variable fg_colour
    variable bg_colour
    variable SCREEN

    set x_end  [expr {$x_start + 16*(($width-1)/16)}]
    set w_last [expr {($width%16)? ($width%16) : 16}]

    set y_end  [expr {$y_start + 16*(($height-1)/16)}]
    set h_last [expr {($height%16)? ($height%16) : 16}]

    set h 16
    for {set y $y_start} {$y <= $y_end} {incr y 16} {
	if {$y == $y_end} {
	    set h $h_last
	}
	set w 16
	for {set x $x_start} {$x <= $x_end} {incr x 16} {
	    if {$x == $x_end} {
		set w $w_last
	    }
	    set subencoding [read_binary $chan c 1]	    
	    if {$subencoding & 1} {
		# Raw hextile
		RawEncoding $chan $x $y $w $h
		continue
	    }
	    if {$subencoding & 2} {
		# background color given
		set bg_colour [ReadSinglePixelColour $chan]
	    }
	    $SCREEN put $bg_colour -to $x $y [expr {$x + $w}] [expr {$y + $h}] 

	    if {$subencoding & 4} {
		# foreground color given
		set fg_colour [ReadSinglePixelColour $chan]
		set subrects_coloured 0
	    } else {
		set subrects_coloured [expr {$subencoding & 16}]
	    }

	    if {$subencoding & 8} {
		# subrects given
		set subrects_count [read_binary $chan c 1]
	    } else {
		continue
	    }

	    set colour $fg_colour
	    for {set s 0} {$s < $subrects_count} {incr s} {
		if {$subrects_coloured} {
		    set colour [ReadSinglePixelColour $chan]
		}
		#
		# Read both x_y and w_h bytes in one go
		#

		set coords [read_binary $chan S 2]
		set subrect_x0 [expr {$x + (($coords >> 12) & 15)}]
		set subrect_y0 [expr {$y + (($coords >>  8) & 15)}]
		set subrect_x1 [expr {$subrect_x0 + 1 + (($coords >>  4) & 15)}]
		set subrect_y1 [expr {$subrect_y0 + 1 + (($coords      ) & 15)}]

		$SCREEN put $colour -to $subrect_x0 $subrect_y0 $subrect_x1 $subrect_y1
	    }
	}
	update idletasks
    }
}


######################################################################
# Utility functions used by other procs
######################################################################

proc ::vnc::ReadSinglePixelColour {chan} {
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BYTES_PER_PIXEL
    return $PIXELS([read $chan $BYTES_PER_PIXEL])
}

# 
proc ::vnc::FillOutPixelCache {} {
    variable PIXELS
    variable BINARY_FORMAT_FOR_PIXELS
    variable BITS_PER_PIXEL
    variable RGB_MAX
    variable RGB_SHIFT

    foreach {rmax gmax bmax} $RGB_MAX {rshift gshift bshift} $RGB_SHIFT break

    if {$BITS_PER_PIXEL != 8} {
	return -code error "Have to change the loop parameters here" 
    } 
    for {set pixel 0} {$pixel < 256} {incr pixel} { 
	set rcomponent   [expr {($pixel >> $rshift) & $rmax}]
	set red_to_255   [expr {(255 * $rcomponent) / $rmax}]
	set gcomponent   [expr {($pixel >> $gshift) & $gmax}]
	set green_to_255 [expr {(255 * $gcomponent) / $gmax}]
	set bcomponent   [expr {($pixel >> $bshift) & $bmax}]
	set blue_to_255  [expr {(255 * $bcomponent) / $bmax}]
	set index [binary format c $pixel]; # 8-bit specific
	set PIXELS($index) \
	    [format "#%02X%02X%02X" $red_to_255 $green_to_255 $blue_to_255]
    }
}

proc ::vnc::disconnect {root} {
    variable VNC

    if {[info exists VNC(chan)]} {
	close $VNC(chan)
	UpdateUI
    }

    variable MENU
    $MENU.file entryconfigure "Connect"    -state normal
    $MENU.file entryconfigure "Disconnect" -state disabled
}

# ::vnc::connection --
#
#   ADD COMMENTS HERE
#
# Arguments:
#   None
# Results:
#   Will attach to user provided server.
#
proc ::vnc::connect {root} {
    set base [expr {($root == ".") ? "" : $root}]
    set t $base.__conn
    if {![winfo exists $t]} {
	toplevel $t
	wm withdraw $t
	wm title $t "Create Connection"
	label $t.lhost -text "Server: "
	entry $t.host -width 15
	label $t.lport -text "Port: "
	entry $t.port -width 3 -validate key -invcmd bell \
		-vcmd {string is integer %P}
	button $t.ok -text "Connect" -width 8 \
		-command {set ::vnc::PRIV(grab) 1}
	bind $t.host <Return> [list focus $t.port]
	bind $t.port <Return> [list focus $t.ok]
	bind $t.ok   <Return> [list $t.ok invoke]
	grid $t.lhost $t.host $t.lport $t.port $t.ok -sticky ew -pady 2
	grid configure $t.ok -sticky ew -padx 4
	grid columnconfig $t 1 -weight 1
	grid rowconfigure $t 1 -weight 1
	wm transient $t $root
	wm geometry $t +[expr {([winfo screenwidth $t]-[winfo \
		reqwidth $t]) / 2}]+[expr {([winfo \
		screenheight $t]-[winfo reqheight $t]) / 2}]
    }
    wm deiconify $t
    raise $t
    grab $t
    focus $t.host
    vwait ::vnc::PRIV(grab)
    grab release $t
    wm withdraw $t
    set host [$t.host get]
    set port [$t.port get]
    if {[catch {
	attach $host $port 
    } err]} {
	tk_messageBox -title "Socket Connection Error" \
		-message "Unable to connect to \"$host\" port $port:\n$err" \
		-icon error -type ok
	return
    }

    variable MENU
    $MENU.file entryconfigure "Connect"    -state disabled
    $MENU.file entryconfigure "Disconnect" -state normal
}

proc ::vnc::UpdateUI {{chan {}}} {
    variable CANVAS
    variable SCREEN
    variable ROOT
    variable FB

    if {$chan == ""} {
	$SCREEN blank
	wm geometry $ROOT ""
	wm title $ROOT "Unconnected - TkVNC"

	bind $CANVAS <Motion>        {}
	bind $CANVAS <ButtonPress>   {}
	bind $CANVAS <ButtonRelease> {}
	bind $ROOT   <KeyPress>      {}
	bind $ROOT   <KeyRelease>    {}
	return
    }

    $CANVAS configure -width $FB(width) -height $FB(height)
    $SCREEN configure -width $FB(width) -height $FB(height)
    $SCREEN blank

    wm geometry $ROOT ""
    wm title $ROOT "$FB(title) - TkVNC"
    #wm resizable $ROOT 0 0
    #wm maxsize $ROOT $FB(width) $FB(height)

    bind $CANVAS <Motion>        [list ::vnc::PointerEvent $chan move %x %y]
    bind $CANVAS <ButtonPress>   [list ::vnc::PointerEvent $chan down %x %y %b]
    bind $CANVAS <ButtonRelease> [list ::vnc::PointerEvent $chan up %x %y %b]
    bind $ROOT   <KeyPress>      [list ::vnc::KeyEvent $chan %N]
    bind $ROOT   <KeyRelease>    [list ::vnc::KeyEvent $chan %N 0]
}

proc ::vnc::InitUI {root} {
    set base [expr {($root == ".") ? "" : $root}]
    if {![winfo exists $root]} {
	toplevel $root
    }
    wm title $root "TkVNC"

    variable ROOT   $root
    variable CANVAS $base.screen
    variable MENU   $base.menu
    variable SCREEN [image create photo]
    variable SCREENBUF [image create photo]

    $SCREEN blank

    canvas $CANVAS -highlightthickness 0 -background white
    $CANVAS create image 0 0 -anchor nw -image $SCREEN -tags screen
    pack $CANVAS -fill both -expand 1

    set menu [menu $MENU]
    $root configure -menu $menu

    # File
    set m [menu $menu.file -tearoff 0]
    $menu add cascade -label "File" -menu $m
    $m add command -label "Connect" -command [list ::vnc::connect $root]
    $m add command -label "Disconnect" -state disabled \
	    -command [list ::vnc::disconnect $root]
    if {[llength [info commands console]]} {
	$m add command -label "Show Console" -command { console show }
    }
    if {[llength [info commands tkcon]]} {
	$m add command -label "Show TkCon" -command { tkcon show }
    }
    $m add separator
    $m add command -label Exit -command exit

    # Prefs
    set m [menu $menu.prefs -tearoff 0]
    $menu add cascade -label "Prefs" -menu $m
    $m add cascade -label "Encoding Order" -menu $m.order

    # Prefs -> Order
    set m [menu $menu.prefs.order -tearoff 0]
    foreach {lbl order} {
	"CopyRect Hextile CoRRE RRE Raw" "1 5 4 2 0"
	"Hextile CopyRect CoRRE RRE Raw" "5 1 4 2 0"
	"Raw CopyRect Hextile CoRRE RRE" "0 1 5 4 2"
	"Raw Hextile CopyRect CoRRE RRE" "0 5 1 4 2"
    } {
	$m add radio -variable ::vnc::ENC(order) -value $order -label $lbl \
		-command "catch {::vnc::SetEncodings \$::vnc::VNC(chan)}"
    }

    UpdateUI
}


#######################################################################
# Main program.  Quite simple really.
proc ::vnc::Init {argc argv} {
    FillOutPixelCache
    InitUI .

    if {$argc} {
	attach [lindex $argv 0]
    }
}

if {![llength [winfo children .]]} {
    ::vnc::Init $argc $argv
}
