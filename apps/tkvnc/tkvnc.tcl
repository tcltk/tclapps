#!/bin/sh
# \
exec wish "$0" ${1+"$@"}

# Greg Baker's VNC viewer written in pure Tcl (using Tk for a GUI).
# (c)  Greg Baker,  Ifost  <gregb@ifost.org.au>
#
# Current status (9 Jan 2003).  
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



######################################################################
# You could try changing this, but I don't know if it will work.

set VNC_BITS_PER_PIXEL 8
set VNC_BINARY_FORMAT_FOR_PIXELS c
set VNC_RGB_MAX {7 7 3}
set VNC_RGB_SHIFT {0 3 6}

######################################################################



set VNC_BYTES_PER_PIXEL [expr $VNC_BITS_PER_PIXEL / 8]

# EXCLUSIVE_SESSION = 1 doesn't seem to work

proc read_binary {channel format bytes} {
    set x [read $channel $bytes]
    set y ""
    binary scan $x $format y
    return $y
}

proc read_sized_string {channel} {
    set x [read_binary $channel I 4]
    set str [read $channel $x]
    return $str
}

proc vncGetConnectionDetails {connection {windowpath .} {exclusive 0}} {
    global vnc SERVER VNCSCREEN
    set colon [string first : $connection]
    set SERVER [string range $connection 0 [expr $colon-1]]
    set VNCSCREEN [string range $connection [expr $colon+1] end]
    
    set portnum [expr $VNCSCREEN+5900]
    set vnc [socket $SERVER $portnum]

    
    fconfigure $vnc -translation {binary binary}
    set vncversion [read $vnc 12]
    #puts "VNC version string: $vncversion"
    puts -nonewline $vnc "RFB 003.003\n"
    flush $vnc

    #puts "Waiting for auth details"
    set vncauth [read_binary $vnc I 4]
    #puts "VNC auth number = $vncauth"

    switch {$vncauth} {
	0 {
	    set vncproblem [read_sized_string $vnc]
	    error "$vncproblem"
	}
	1 {
	    # cool!
	}
	2 {
	    set vncchallenge [read $vnc 16]
	    error "Don't know how to handle authenticated connections.  I need a DES implementation in Tcl.  Do you have one?  Tell gregb@ifost.org.au"
	}
    }


    set exclusive [binary format c $exclusive]
    puts -nonewline $vnc $exclusive
    flush $vnc


    global vncframebufferwidth vncframebufferheight 
    set vncframebufferwidth [read_binary $vnc S 2]
    set vncframebufferheight [read_binary $vnc S 2]
    #puts "$vncframebufferwidth x $vncframebufferwidth "
    set vncbitsperpixel [read_binary $vnc c 1]
    set vncdepth [read_binary $vnc c 1]
    set vnc_big_endian_flag [read_binary $vnc c 1]
    set vnc_true_colour_flag [read_binary $vnc c 1]
    set vnc_red_max [read_binary $vnc S 2]
    set vnc_green_max [read_binary $vnc S 2]
    set vnc_blue_max [read_binary $vnc S 2]
    set vnc_red_shift [read_binary $vnc c 1]
    set vnc_green_shift [read_binary $vnc c 1]
    set vnc_blue_shift [read_binary $vnc c 1]
    set vnc_padding [read $vnc 3]
    set vnc_title [read_sized_string $vnc]
    
    #puts "$vncbitsperpixel bits per pixel"
    #puts "Depth $vncdepth"
    #puts "Big-Endian?  $vnc_big_endian_flag"
    #puts "True-Colour? $vnc_true_colour_flag"
    #puts "R/G/B mask $vnc_red_max/$vnc_green_max/$vnc_blue_max"
    #puts "R/G/B shift $vnc_red_shift/$vnc_green_shift/$vnc_blue_shift"


    if {[string compare $windowpath .] == 0} {
	set vnctoplevel ""
	wm title . $vnc_title
    } else {
	set vnctoplevel $windowpath
	toplevel $vnctoplevel -width $vncframebufferwidth -height $vncframebufferheight
	wm title $vnctoplevel $vnc_title
    }

    global vnccanvas
    set vnccanvas ${vnctoplevel}.screen
    canvas $vnccanvas  -width $vncframebufferwidth -height $vncframebufferheight
    pack $vnccanvas

    global vncscreen

    set vncscreen [image create photo -height $vncframebufferheight -width $vncframebufferwidth ]
    $vncscreen blank

    global vncscreentag
    set vncscreentag [$vnccanvas create image 0 0 -anchor nw -image $vncscreen]
}


######################################################################
#  Client -> Server messages.
# There is one function for each of the standard messages that the
# RFB protocol describes, except for ClientCutText which isn't implemented.

proc vncFramebufferUpdateRequest {channel incremental x y width height} {
    puts -nonewline $channel [binary format c 3]
    puts -nonewline $channel [binary format c $incremental]
    puts -nonewline $channel [binary format S $x]
    puts -nonewline $channel [binary format S $y]
    puts -nonewline $channel [binary format S $width]
    puts -nonewline $channel [binary format S $height]
    flush $channel
}

proc vncSetPixelFormat {channel} {
    global VNC_BITS_PER_PIXEL VNC_RGB_MAX VNC_RGB_SHIFT
    puts -nonewline $channel [binary format c 0]
    # Padding
    puts -nonewline $channel [binary format c3 {0 0 0}]
    # bits per pixel, depth, big-ending (1) or little-ending (0), true-colour
    puts -nonewline $channel [binary format cccc $VNC_BITS_PER_PIXEL $VNC_BITS_PER_PIXEL 1 1]
    # R/G/B max  and then R/G/B/shift
    puts -nonewline $channel [binary format S3c3 $VNC_RGB_MAX $VNC_RGB_SHIFT]
    # Padding
    puts -nonewline $channel [binary format c3 {0 0 0}]
    flush $channel
}

proc vncSetEncodings {channel} {
    set RAW 0
    set COPYRECT 1
    set RRE 2
    set CoRRE 4
    set HEXTILE 5
    set encodings_preferences "$RAW $COPYRECT $HEXTILE $CoRRE $RRE"
    set num_preferences [llength $encodings_preferences]
    puts -nonewline $channel [binary format c 2]
    puts -nonewline $channel [binary format c 0] ; # padding
    puts -nonewline $channel [binary format S $num_preferences]
    puts -nonewline $channel [binary format I$num_preferences $encodings_preferences]
    flush $channel
}

set vncCurrentButtonMask 0
proc vncPointerEvent {channel state x y {button 0} {press 1}} {
    global vncCurrentButtonMask
    puts -nonewline $channel [binary format c 5]
    # button mask  -- eight bit field for each of 8 buttons.  But
    # Tk can't give us that, I think.  It only seems to like individual
    # button down events.
    if {[string compare $state move] == 0} {
	set buttonmask $vncCurrentButtonMask
    } elseif {[string compare $state up] == 0} {
	set buttonmask 0 ; # button release means no buttons
	set vncCurrentButtonMask $buttonmask
    } else {
	set buttonmask [expr 1 << ($button - 1) ] 
	set vncCurrentButtonMask $buttonmask
    }

    puts -nonewline $channel [binary format c $buttonmask]
    puts -nonewline $channel [binary format S $x]
    puts -nonewline $channel [binary format S $y]
    flush $channel
}

proc vncKeyEvent {channel keycode {press 1}} {
    puts -nonewline $channel [binary format c 4]
    puts -nonewline $channel [binary format c $press]
    # padding
    puts -nonewline $channel xx
    puts -nonewline $channel [binary format I $keycode]
    flush $channel
}


######################################################################
# Server -> Client Messages
#
# Firstly a listener for filevent to use.    
# Only FramebufferUpdate is complicated enough to warrant a separate
# function, really.
######################################################################


proc vncChannelReader {channel} {
    set message_type [read_binary $channel c 1]
    if {[eof $channel]} { 
	fileevent $channel readable ""
	exit
	# actually, I should do something better here
    }
    switch -- "$message_type" {
	0 { vncFramebufferUpdate $channel }
	1 { error "Don't know how to handle SetColourMapEntries" }
	2 { bell }
	3 { vncServerCutText $channel }
	default {error "Don't know what to do with message type $message_type"}
    }
}

proc vncServerCutText {channel} {
    clipboard clear
    set padding [read $channel 3]
    set contents [read_sized_string $channel]
    clipboard append $contents
}


proc vncFramebufferUpdate {channel} {
    global vncscreen vnc_pixel_cache
    set padding [read $channel 1]
    set numrects [read_binary $channel S 2]
    for {set n 0} {$n < $numrects} {incr n} {
	set x_start [read_binary $channel S 2]
	set y_start [read_binary $channel S 2]
	set width [read_binary $channel S 2]
	set height [read_binary $channel S 2]
	set encodingtype [read_binary $channel I 4]
	#puts "Encoding type $encodingtype"
	switch -- "$encodingtype" {
	    0 { vncRawEncoding $channel $x_start $y_start $width $height }
	    1 { vncCopyRectEncoding $channel $x_start $y_start $width $height }
	    2 { vncRRERectEncoding $channel $x_start $y_start $width $height }
	    4 { vncCoRRERectEncoding $channel $x_start $y_start $width $height}
	    5 { vncHextileRectEncoding $channel $x_start $y_start $width $height }
	    default {
		puts "Don't know how to handle encoding type $encodingtype"
		exit
	    }
	}
	
    }
    global vncframebufferwidth vncframebufferheight
    vncFramebufferUpdateRequest $channel 1 0 0 $vncframebufferwidth $vncframebufferheight
    update idletasks
}

######################################################################
#   Now, each of the different encodings called in the previous function

# This function is extremely slow -- a 640x480 rectangle takes 4.1 seconds
# on my G3 PowerBook @ 400MhZ. 
# 
# Per 640 pixel scan-line,  this is:
#  - 8.7 microseconds total
#  - binary scan [read $channel ... ]  takes 330 microseconds
#  - $vncscreen put [...] takes 2746 microseconds
#  - update idletasks takes 600 microseconds,  which is why we don't do it 
#    every time
#  - The foreach loop takes around 6.2-7.3 miliseconds (6200 microseconds)
#  - If the body of the foreach loop is just lappend image_info_data "#ff00ff"
#    then each iteration is 3800-4000 microseconds 

#  - 
# It is around 35 microseconds per pixel.
# The lappend doesn't seem to take any appreciable time, and we are 
# very likely to succeed in finding rectpixel in the vnc_pixel_cache.
# The binary scan takes around 240 microseconds for a 480 pixel scan line.
# The $vncscreen put takes around 3 seconds for a 640x480 rectangle.  The
# update idletasks takes 0.2 seconds.

proc vncRawEncoding {channel x_start y_start width height} {
    global vncscreen vnc_pixel_cache VNC_BINARY_FORMAT_FOR_PIXELS VNC_BYTES_PER_PIXEL
    set bytes [expr $width*$VNC_BYTES_PER_PIXEL]
    for {set y 0} {$y < $height} {incr y} {
	binary scan [read $channel $bytes] $VNC_BINARY_FORMAT_FOR_PIXELS$bytes rectwords
	set image_info_data {}
	foreach rectpixel $rectwords {
	    lappend image_info_data $vnc_pixel_cache($rectpixel)
	}
  	$vncscreen put [list $image_info_data] -to $x_start [expr $y_start + $y]
	if {[expr $y % 8] == 0} { update idletasks } 
    }
}



# This alternative implementation is just a teensy bit faster, but
# not enough to make it worthwhile for the extra logic complexity.
# Here we update the image in decent sized rectangles.  Change
# the if {$p == ... }   value towards the end to change the rectangle
# size.  In practice,  the bulk of the time is still in looking up
# the vnc_pixel_cache array, and appending it on to image_info_data.

# proc vncRawEncoding {channel x_start y_start width height} {
#     set bytes [expr $width]
#     global vncscreen vnc_pixel_cache
#     set p 0
#     for {set y 0} {$y < $height} {incr y} {
# 	if {$p == 0} { set image_rect {} }
# 	binary scan [read $channel $bytes] c$bytes rectwords
# 	set image_info_data {}
# 	foreach rectpixel $rectwords {
# 	    lappend image_info_data $vnc_pixel_cache($rectpixel)
# 	}
# 	lappend image_rect $image_info_data
# 	if {$p == 4} { 
# 	    $vncscreen put $image_rect -to $x_start [expr $y_start + $y - $p]
# 	    set p 0 
# 	    update idletasks
# 	} else { 
# 	    incr p
# 	}
#     }
#     $vncscreen put $image_rect -to $x_start [expr $y_start + $y - $p]
# }


proc vncCopyRectEncoding  {channel x_start y_start width height} {
    puts "CopyRect"
    set src_x [read_binary $channel S 2]
    set src_y [read_binary $channel S 2]
    global vncscreen
    $vncscreen copy $vncscreen -from $src_x $src_y [expr $src_x + $width] [expr $src_y + $height] -to $x_start $y_start
}


proc vncRRERectEncoding {channel x_start y_start width height} {
    set num_subrects [read_binary $channel I 4]
    puts "$num_subrects subrectangles"
    set background [vncReadSinglePixelColour $channel]
    vncSolidBlockFill $background $x_start $y_start $width $height
    for {set i 0} {$i < $num_subrects} {incr i} {
	set colour [vncReadSinglePixelColour $channel]
	set x [read_binary $channel S 2] ; set x [expr $x & 0xFFFF]
	set y [read_binary $channel S 2] ; set y [expr $y & 0xFFFF]
	set w [read_binary $channel S 2] ; set w [expr $w & 0xFFFF]
	set h [read_binary $channel S 2] ; set h [expr $h & 0xFFFF]
	vncSolidBlockFill $colour $x $y $w $h
    }
}

proc vncCoRRERectEncoding {channel x_start y_start width height} {
    set num_subrects [read_binary $channel I 4]
    puts "$num_subrects subrectangles"
    set background [vncReadSinglePixelColour $channel]
    vncSolidBlockFill $background $x_start $y_start $width $height
    for {set i 0} {$i < $num_subrects} {incr i} {
	set colour [vncReadSinglePixelColour $channel]
	set x [read_binary $channel c 1] ; set x [expr $x & 0xFF]
	set y [read_binary $channel c 1] ; set y [expr $y & 0xFF]
	set w [read_binary $channel c 1] ; set w [expr $w & 0xFF]
	set h [read_binary $channel c 1] ; set h [expr $h & 0xFF]
	vncSolidBlockFill $colour [expr $x_start + $x] [expr $y_start + $y] $w $h
    }

}



set vnc_foreground_colour ""
proc vncHextileRectEncoding {channel x_start y_start width height} {
    global vnc_foreground_colour vnc_background_colour
    for {set tile_y 0} {$tile_y < $height} {incr tile_y 16} {
	for {set tile_x 0} {$tile_x < $width} {incr tile_x 16} {
	    set this_tile_start_x [expr $x_start + $tile_x]
	    set this_tile_start_y [expr $y_start + $tile_y]
	    set this_tile_width [expr $tile_x + 16 > $width ? ($width - $tile_x + 16) : 16 ]
	    set this_tile_height [expr $tile_y + 16 > $height ? ($height - $tile_y + 16) : 16 ]
	    set subencoding [read_binary $channel c 1]	    
	    set raw [expr $subencoding & 1]
	    if {$raw == 1} {
		puts "Raw hextile"
		vncRawEncoding $channel $this_tile_start_x $this_tile_start_y $this_tile_width $this_tile_height
		continue
	    } 
	    set background_specified [expr $subencoding & 2]
	    if {$background_specified == 2} {
		set vnc_background_colour [vncReadSinglePixelColour $channel]
	    }
	    set foreground_specified [expr $subencoding & 4]
	    if {$foreground_specified == 4} {
		set vnc_foreground_colour [vncReadSinglePixelColour $channel]
	    }
	    set any_sub_rects [expr $subencoding & 8]
	    if {$any_sub_rects == 8} {
		set subrects_count [read_binary $channel c 1]
	    } else {
		set subrects_count 0
		#puts "16x16 fill"
	    }
	    vncSolidBlockFill $vnc_background_colour  $this_tile_start_x $this_tile_start_y $this_tile_width $this_tile_height

	    set subrects_coloured [expr $subencoding & 16]
	    
	    set colour $vnc_foreground_colour
#  puts [time {
	    for {set s 0} {$s < $subrects_count} {incr s} {
		if {$subrects_coloured == 16} {
		    set colour [vncReadSinglePixelColour $channel]
		} 
		set x_and_y [expr ([read_binary $channel c 1] + 0x100) % 0x100]
		set width_and_height [expr ([read_binary $channel c 1] + 0x100) % 0x100]
		set subrect_x [expr $this_tile_start_x + ($x_and_y >> 4)]
		set subrect_y [expr $this_tile_start_y + ($x_and_y & 15)]
		set subrect_width [expr 1 + ($width_and_height >> 4)]
		set subrect_height [expr 1 + ($width_and_height & 15)]
		#puts "Subrect hextile from ($this_tile_start_x,$this_tile_start_y) (Encoding = $subencoding) $x_and_y ($subrect_x,$subrect_y) $width_and_height ($subrect_width x $subrect_height)"
		vncSolidBlockFill $colour $subrect_x $subrect_y $subrect_width $subrect_height
	    }
#  }]
	}
	update idletasks
    }    
}




######################################################################
# Utility functions used by other procs
######################################################################


proc vncSolidBlockFill {colour x y width height} {
    global vncscreen
    if {$width == 1 && $height == 1} {
	puts [time {$vncscreen put [list $colour] -to $x $y}]
	return
    }
    puts "vncSolidBlockFill $colour $x $y $width $height"
    set line [string repeat "$colour " $width]
    set rowdata [list $line]
    #set rowdata [list [string repeat "$colour " $width]]
    #set rowdata ""
    for {set i 0} {$i < $height} {incr i} {
	$vncscreen put $rowdata -to $x [expr $y + $i]
    }
    #set data [string repeat "$rowdata " $height]
    #$vncscreen put  $data -to $x $y
    set rowdata ""
    set line ""
}

proc vncReadSinglePixelColour {channel} {
    global vnc_pixel_cache VNC_BINARY_FORMAT_FOR_PIXELS VNC_BYTES_PER_PIXEL
    set x [read_binary $channel  $VNC_BINARY_FORMAT_FOR_PIXELS $VNC_BYTES_PER_PIXEL]
    return $vnc_pixel_cache($x)
}




# 
proc vncFillOutPixelCache {} {
    global vnc_pixel_cache VNC_RGB_MAX VNC_RGB_SHIFT VNC_BITS_PER_PIXEL
    
    set red_max [lindex $VNC_RGB_MAX 0]
    set red_shift [lindex $VNC_RGB_SHIFT 0]
    set green_max [lindex $VNC_RGB_MAX 1]
    set green_shift [lindex $VNC_RGB_SHIFT 1]
    set blue_max [lindex $VNC_RGB_MAX 2]
    set blue_shift [lindex $VNC_RGB_SHIFT 2]


    if {$VNC_BITS_PER_PIXEL != 8} {
	error "Have to change the loop parameters here" 
    } 
    for {set pixel -255} {$pixel < 256} {incr pixel} { 

	set red_component [expr ($pixel >> $red_shift) & $red_max]
	set red_to_255 [expr int (255.0 * $red_component / (1.0 * $red_max+1))]
	
	set green_component [expr ($pixel >> $green_shift) & $green_max]
	set green_to_255 [expr int (255.0 * $green_component / (1.0 * $green_max+1))]
	
	set blue_component [expr ($pixel >> $blue_shift) & $blue_max]
	set blue_to_255 [expr int (255.0 * $blue_component / (1.0 * $blue_max+1))]
	
	set hexstring [format %02X%02X%02X $red_to_255 $green_to_255 $blue_to_255]
	set vnc_pixel_cache($pixel) "\#$hexstring"
    }
}
vncFillOutPixelCache



#######################################################################
# Main program.  Quite simple really.

vncGetConnectionDetails [lindex $argv 0]
vncSetPixelFormat $vnc
vncSetEncodings $vnc
vncFramebufferUpdateRequest $vnc 0 0 0 $vncframebufferwidth $vncframebufferheight

fileevent $vnc readable "vncChannelReader $vnc"
bind $vnccanvas <Motion> "vncPointerEvent $vnc move %x %y"
bind $vnccanvas <ButtonPress> "vncPointerEvent $vnc down %x %y %b"
bind $vnccanvas <ButtonRelease> "vncPointerEvent $vnc up %x %y %b"
bind all <KeyPress> "vncKeyEvent $vnc %N"
bind all <KeyRelease> "vncKeyEvent $vnc %N 0"




