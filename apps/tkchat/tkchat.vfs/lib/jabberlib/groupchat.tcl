# groupchat.tcl--
# 
#       Support for the old gc-1.0 groupchat protocol.
#       
#  Copyright (c) 2002-2005  Mats Bengtsson
#  
# $Id: groupchat.tcl,v 1.3 2005/02/21 07:59:08 matben Exp $
# 
############################# USAGE ############################################
#
#   INSTANCE COMMANDS
#      jlibName groupchat enter room nick
#      jlibName groupchat exit room
#      jlibName groupchat mynick room ?nick?
#      jlibName groupchat status room
#      jlibName groupchat participants room
#      jlibName groupchat allroomsin
#
################################################################################

package provide groupchat 1.0

namespace eval jlib { }

namespace eval jlib::groupchat { }

# jlib::groupchat --
#
#       Provides API's for the old-style groupchat protocol, 'groupchat 1.0'.

proc jlib::groupchat {jlibname cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    return [eval {[namespace current]::groupchat::${cmd} $jlibname} $args]
}

proc jlib::groupchat::init {jlibname} {

    upvar ${jlibname}::gchat gchat
    
    namespace eval ${jlibname}::groupchat {
	variable rooms
    }
    set gchat(allroomsin) {}
}

# jlib::groupchat::enter --
#
#       Enter room using the 'gc-1.0' protocol by sending <presence>.
#
#       args:  -command callback

proc jlib::groupchat::enter {jlibname room nick args} {

    upvar ${jlibname}::gchat gchat
    upvar ${jlibname}::groupchat::rooms rooms
    
    set room [string tolower $room]
    set jid ${room}/${nick}
    eval {[namespace parent]::send_presence $jlibname -to $jid} $args
    set gchat($room,mynick) $nick
    
    # This is not foolproof since it may not always success.
    lappend gchat(allroomsin) $room
    set rooms($room) 1
    [namespace parent]::service::setroomprotocol $jlibname $room "gc-1.0"
    set gchat(allroomsin) [lsort -unique $gchat(allroomsin)]
    return ""
}

proc jlib::groupchat::exit {jlibname room} {

    upvar ${jlibname}::gchat gchat
    upvar ${jlibname}::lib lib
    
    set room [string tolower $room]
    if {[info exists gchat($room,mynick)]} {
	set nick $gchat($room,mynick)
    } else {
	return -code error "Unknown nick name for room \"$room\""
    }
    set jid ${room}/${nick}
    [namespace parent]::send_presence $jlibname -to $jid -type "unavailable"
    unset gchat($room,mynick)
    set ind [lsearch -exact $gchat(allroomsin) $room]
    if {$ind >= 0} {
	set gchat(allroomsin) [lreplace $gchat(allroomsin) $ind $ind]
    }
    $lib(rostername) clearpresence "${room}*"
    return ""
}

proc jlib::groupchat::mynick {jlibname room args} {

    upvar ${jlibname}::gchat gchat

    set room [string tolower $room]
    if {[llength $args] == 0} {
	if {[info exists gchat($room,mynick)]} {
	    return $gchat($room,mynick)
	} else {
	    return -code error "Unknown nick name for room \"$room\""
	}
    } elseif {[llength $args] == 1} {
	
	# This should work automatically.
	enter $jlibname $room $args
    } else {
	return -code error "Wrong number of arguments"
    }
}

proc jlib::groupchat::status {jlibname room args} {

    upvar ${jlibname}::gchat gchat

    set room [string tolower $room]
    if {[info exists gchat($room,mynick)]} {
	set nick $gchat($room,mynick)
    } else {
	return -code error "Unknown nick name for room \"$room\""
    }
    set jid ${room}/${nick}
    eval {[namespace parent]::send_presence $jlibname -to $jid} $args
}

proc jlib::groupchat::participants {jlibname room} {

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::gchat gchat
    upvar ${jlibname}::lib lib

    set room [string tolower $room]
    set isroom 0
    if {[regexp {^[^@]+@([^@ ]+)$} $room match domain]} {
	if {[info exists agent($domain,groupchat)]} {
	    set isroom 1
	}
    }    
    if {!$isroom} {
	return -code error "Not recognized \"$room\" as a groupchat room"
    }
    
    # The rosters presence elements should give us all info we need.
    set everyone {}
    foreach userAttr [$lib(rostername) getpresence $room -type available] {
	unset -nocomplain attrArr
	array set attrArr $userAttr
	lappend everyone ${room}/$attrArr(-resource)
    }
    return $everyone
}

proc jlib::groupchat::isroom {jlibname jid} {

    upvar ${jlibname}::groupchat::rooms rooms
    
    if {[info exists rooms($jid)]} {
	return 1
    } else {
	return 0
    }
}

proc jlib::groupchat::allroomsin {jlibname} {

    upvar ${jlibname}::gchat gchat

    set gchat(allroomsin) [lsort -unique $gchat(allroomsin)]
    return $gchat(allroomsin)
}

#-------------------------------------------------------------------------------
