#  muc.tcl --
#  
#      This file is part of the whiteboard application and jabberlib.
#      It implements the Multi User Chat (MUC) protocol part of the XMPP
#      protocol as defined by the 'http://jabber.org/protocol/muc*'
#      namespace.
#      
#  Copyright (c) 2003-2005  Mats Bengtsson
#  
#  See the README file for license, bugs etc.
#
# $Id: muc.tcl,v 1.23 2005/04/25 11:55:00 matben Exp $
# 
############################# USAGE ############################################
#
#   NAME
#      muc - convenience command library for MUC
#      
#   OPTIONS
#	see below for instance command options
#	
#   INSTANCE COMMANDS
#      mucName allroomsin
#      mucName create roomjid nick callback ?-extras?
#      mucName destroy roomjid ?-command, -reason, alternativejid?
#      mucName enter roomjid nick ?-command, -extras, -password?
#      mucName exit roomjid
#      mucName getaffiliation roomjid affiliation callback
#      mucName getrole roomjid role callback
#      mucName getroom roomjid callback
#      mucName invite roomjid jid ?-reason?
#      mucName isroom jid
#      mucName mynick roomjid
#      mucName participants roomjid
#      mucName setaffiliation roomjid nick affiliation ?-command, -reason?
#      mucName setnick roomjid nick ?-command?
#      mucName setrole roomjid nick role ?-command, -reason?
#      mucName setroom roomjid type ?-command, -form?
#      
############################# CHANGES ##########################################
#
#       0.1         first version
#       0.2         rewritten as a standalone component

package provide muc 0.2

namespace eval jlib::muc {
    
    # Globals same for all instances of this jlib.
    variable debug 0

    # Running number.
    variable uid 0
    
    variable xmlnsmuc 
    array set xmlnsmuc {
	""              "http://jabber.org/protocol/muc"
	"admin"         "http://jabber.org/protocol/muc#admin"
	"owner"         "http://jabber.org/protocol/muc#owner"
	"user"          "http://jabber.org/protocol/muc#user"
    }
    
    variable muc
    set muc(affiliationExp) {(owner|admin|member|outcast|none)}
    set muc(roleExp) {(moderator|participant|visitor|none)}
}

# jlib::muc::new --
# 
#       Creates a new instance of a muc object.
#       
# Arguments:
#       jlibname:     name of existing jabberlib instance; fully qualified!
#       args:        
# 
# Results:
#       namespaced instance command

proc jlib::muc::new {jlibname args} {
    
    variable uid
    variable muc2jlib

    # Generate unique command token for this browse instance.
    # Fully qualified!
    set mucname [namespace current]::[incr uid]

    Debug 2 "muc::new jlibname=$jlibname, mucname=$mucname"
      
    # Instance specific namespace.
    namespace eval $mucname {
	variable cache
	variable rooms
    }
    upvar ${mucname}::cache cache
    upvar ${mucname}::rooms rooms

    foreach {key value} $args {
	switch -- $key {
	    default {
		return -code error "unrecognized option \"$key\" for muc::new"
	    }
	}
    }
    set muc2jlib($mucname) $jlibname

    # Register service.
    $jlibname service register muc $mucname
        
    # Create the actual muc instance procedure.
    proc $mucname {cmd args}   \
      "eval jlib::muc::CommandProc {$mucname} \$cmd \$args"
    
    return $mucname
}


# jlib::muc::CommandProc --
#
#       Just dispatches the command to the right procedure.
#
# Arguments:
#       jlibname    name of jabberlib instance.
#       cmd         the method.
#       args        all args to the cmd method.
#       
# Results:
#       from the individual command if any.

proc jlib::muc::CommandProc {mucname cmd args} {
    
    # Which sub command? Just dispatch the command to the right procedure.
    return [eval {$cmd $mucname} $args]
}

# jlib::muc::invoke_callback --
# 
# 

proc jlib::muc::invoke_callback {mucname cmd type subiq} {

    uplevel #0 $cmd [list $mucname $type $subiq]
}

# jlib::muc::enter --
# 
#       Enter room.
#       
# Arguments:
#       mucname    name of jabberlib instance.
#       roomjiid
#       nick        nick name
#       args        ?-command callbackProc?
#                   ?-extras list of xmllist?
#                   ?-password str?
#       
# Results:
#       none.

proc jlib::muc::enter {mucname roomjid nick args} {

    variable muc2jlib
    variable xmlnsmuc
    upvar ${mucname}::cache cache
    upvar ${mucname}::rooms rooms
    
    set jlibname $muc2jlib($mucname)
    set xsub {}
    set extras {}
    foreach {name value} $args {
	
	switch -- $name {
	    -command {
		set cache($roomjid,entercb) $value
	    }
	    -extras {
		set extras $value
	    }
	    -password {
		set xsub [list [wrapper::createtag "password" \
		  -chdata $value]]
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
    set jid ${roomjid}/${nick}
    set xelem [wrapper::createtag "x" -subtags $xsub \
      -attrlist [list xmlns $xmlnsmuc()]]
    $jlibname send_presence -to $jid -xlist [list $xelem] -extras $extras \
      -command [list [namespace current]::parse_enter $mucname $roomjid]
    set cache($roomjid,mynick) $nick
    set rooms($roomjid) 1
    $jlibname service setroomprotocol $roomjid "muc"
}

# jlib::muc::parse_enter --
# 
#       Callback when entering room to make sure there are no error.
# 
# Arguments:
#       mucname
#       
#       jlibname 
#       type    presence typ attribute, 'available', 'error', etc.
#       args    -from, -id, -to, -x ...

proc jlib::muc::parse_enter {mucname roomjid jlibname type args} {

    upvar ${mucname}::cache cache

    if {[string equal $type "error"]} {
	unset -nocomplain cache($roomjid,mynick)
    } else {
	set cache($roomjid,inside) 1
    }
    if {[info exists cache($roomjid,entercb)]} {
	set cbproc $cache($roomjid,entercb)
	unset -nocomplain cache($roomjid,entercb)
	uplevel #0 $cbproc $mucname $type $args
    }
}

# jlib::muc::exit --
# 
#       Exit room.

proc jlib::muc::exit {mucname roomjid} {

    variable muc2jlib
    upvar ${mucname}::cache cache

    set jlibname $muc2jlib($mucname)
    set rostername [$jlibname getrostername]
    if {[info exists cache($roomjid,mynick)]} {
	set jid ${roomjid}/$cache($roomjid,mynick)
	$jlibname send_presence -to $jid -type "unavailable"
	unset -nocomplain cache($roomjid,mynick)
    }
    unset -nocomplain cache($roomjid,inside)
    $rostername clearpresence "${roomjid}*"
}

# jlib::muc::setnick --
# 
#       Set new nick name for room.

proc jlib::muc::setnick {mucname roomjid nick args} {

    variable muc2jlib
    upvar ${mucname}::cache cache
    
    set jlibname $muc2jlib($mucname)
    set opts {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts $name $value
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
    set jid ${roomjid}/${nick}
    eval {$jlibname send_presence -to $jid} $opts
    set cache($roomjid,mynick) $nick
}

# jlib::muc::invite --
# 
# 

proc jlib::muc::invite {mucname roomjid jid args} {
    
    variable muc2jlib
    variable xmlnsmuc

    set jlibname $muc2jlib($mucname)
    set opts {}
    set children {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts $name $value
	    }
	    -reason {
		lappend children [wrapper::createtag  \
		  [string trimleft $name "-"] -chdata $value]
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }    
    set invite [list [wrapper::createtag "invite"  \
      -attrlist [list to $jid] -subtags $children]]
    
    set xelem [wrapper::createtag "x" -subtags $invite  \
      -attrlist [list xmlns $xmlnsmuc(user)]]
    eval {$jlibname send_message $roomjid -xlist [list $xelem]} $opts
}

# jlib::muc::setrole --
# 
# 

proc jlib::muc::setrole {mucname roomjid nick role args} {

    variable muc2jlib
    variable muc
    variable xmlnsmuc
    
    if {![regexp $muc(roleExp) $role]} {
	return -code error "Unrecognized role \"$role\""
    }
    set jlibname $muc2jlib($mucname)
    set opts {}
    set subitem {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_callback $mucname $value]
	    }
	    -reason {
		set subitem [list [wrapper::createtag "reason" -chdata $value]]
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
    
    set subelements [list [wrapper::createtag "item" -subtags $subitem \
      -attrlist [list nick $nick role $role]]]
    
    set xmllist [wrapper::createtag "query" \
      -attrlist [list xmlns $xmlnsmuc(admin)] -subtags $subelements]
    eval {$jlibname send_iq "set" [list $xmllist] -to $roomjid} $opts
}

# jlib::muc::setaffiliation --
# 
# 

proc jlib::muc::setaffiliation {mucname roomjid nick affiliation args} {

    variable muc2jlib
    variable muc
    variable xmlnsmuc
    
    if {![regexp $muc(affiliationExp) $affiliation]} {
	return -code error "Unrecognized affiliation \"$affiliation\""
    }
    set jlibname $muc2jlib($mucname)
    set opts {}
    set subitem {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_callback $mucname $value]
	    }
	    -reason {
		set subitem [list [wrapper::createtag "reason" -chdata $value]]
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }

    switch -- $affiliation {
    	owner {
    	    set xmlns $xmlnsmuc(owner)
    	}
    	default {
    	    set xmlns $xmlnsmuc(admin)
    	}
    }
    
    set subelements [list [wrapper::createtag "item" -subtags $subitem \
      -attrlist [list nick $nick affiliation $affiliation]]]
    
    set xmllist [wrapper::createtag "query" \
      -attrlist [list xmlns $xmlns] -subtags $subelements]
    eval {$jlibname send_iq "set" [list $xmllist] -to $roomjid} $opts
}

# jlib::muc::getrole --
# 
# 

proc jlib::muc::getrole {mucname roomjid role callback} {

    variable muc2jlib
    variable muc
    variable xmlnsmuc
    
    if {![regexp $muc(roleExp) $role]} {
	return -code error "Unrecognized role \"$role\""
    }
    set jlibname $muc2jlib($mucname)
    set subelements [list [wrapper::createtag "item" \
      -attrlist [list role $role]]]
    
    set xmllist [wrapper::createtag "query" -subtags $subelements \
      -attrlist [list xmlns $xmlnsmuc(admin)]]
    $jlibname send_iq "get" [list $xmllist] -to $roomjid \
      -command [list [namespace current]::invoke_callback $mucname $callback]
}

# jlib::muc::getaffiliation --
# 
# 

proc jlib::muc::getaffiliation {mucname roomjid affiliation callback} {

    variable muc2jlib
    variable muc
    variable xmlnsmuc
    
    if {![regexp $muc(affiliationExp) $affiliation]} {
	return -code error "Unrecognized role \"$affiliation\""
    }
    set jlibname $muc2jlib($mucname)
    set subelements [list [wrapper::createtag "item" \
      -attrlist [list affiliation $affiliation]]]

    switch -- $affiliation {
    	owner - admin {
    	    set xmlns $xmlnsmuc(owner)
    	}
    	default {
    	    set xmlns $xmlnsmuc(admin)
    	}
    }
    
    set xmllist [wrapper::createtag "query" -subtags $subelements \
      -attrlist [list xmlns $xmlns]]
    $jlibname send_iq "get" [list $xmllist] -to $roomjid \
      -command [list [namespace current]::invoke_callback $mucname $callback]
}

# jlib::muc::create --
# 
#       The first thing to do when creating a room.
#       
# Arguments:
#       mucname    name of jabberlib instance.
#       roomjiid
#       nick        nick name
#       callback    callbackProc
#       args        ?-extras list of xmllist?
#       
# Results:
#       none.

proc jlib::muc::create {mucname roomjid nick callback args} {

    variable muc2jlib
    variable xmlnsmuc
    upvar ${mucname}::cache cache
    upvar ${mucname}::rooms rooms

    set jlibname $muc2jlib($mucname)
    set extras {}
    foreach {name value} $args {
	
	switch -- $name {
	    -extras {
		set extras $value
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
    set jid ${roomjid}/${nick}
    set cache($roomjid,createcb) $callback
    set xelem [wrapper::createtag "x" -attrlist [list xmlns $xmlnsmuc()]]
    $jlibname send_presence  \
      -to $jid  \
      -command [list [namespace current]::parse_create $mucname $roomjid]  \
      -xlist [list $xelem]  \
      -extras $extras
    set cache($roomjid,mynick) $nick
    set rooms($roomjid) 1
    $jlibname service setroomprotocol $roomjid "muc"
}

proc jlib::muc::parse_create {mucname roomjid jlibname type args} {

    variable muc2jlib
    upvar ${mucname}::cache cache

    if {[string equal $type "error"]} {
	unset -nocomplain cache($roomjid,mynick)
    } else {
	set cache($roomjid,inside) 1
    }
    if {[info exists cache($roomjid,createcb)]} {
	set cbproc $cache($roomjid,createcb)
	unset -nocomplain cache($roomjid,createcb)
	uplevel #0 $cbproc $mucname $type $args
    }
}

# jlib::muc::setroom --
# 
#       Sends an iq set element to room. If -form the 'type' argument is
#       omitted.
#       
# Arguments:
#       mucname     name of muc instance.
#       roomjid     the rooms jid.
#       type        typically 'submit' or 'cancel'.
#       args:        
#           -command 
#           -form   xmllist starting with the x-element
#       
# Results:
#       None.

proc jlib::muc::setroom {mucname roomjid type args} {

    variable muc2jlib
    variable xmlnsmuc

    set jlibname $muc2jlib($mucname)
    set opts {}
    set subelements {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_callback $mucname $value]
	    }
	    -form {
		set xelem $value
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
    if {[llength $xelem] == 0} {
	set xelem [list [wrapper::createtag "x"  \
	  -attrlist [list xmlns "jabber:x:data" type $type]]]
    }
    set xmllist [wrapper::createtag "query" -subtags $xelem \
      -attrlist [list xmlns $xmlnsmuc(owner)]]
    eval {$jlibname send_iq "set" [list $xmllist] -to $roomjid} $opts
}

# jlib::muc::destroy --
# 
# 
# Arguments:
#       mucname     name of muc instance.
#       roomjid     the rooms jid.
#       args        -command, -reason, alternativejid.
#       
# Results:
#       None.

proc jlib::muc::destroy {mucname roomjid args} {

    variable muc2jlib
    variable xmlnsmuc

    set jlibname $muc2jlib($mucname)
    set opts {}
    set subelements {}
    foreach {name value} $args {
	switch -- $name {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_callback $mucname $value]
	    }
	    -reason {
		lappend subelements [wrapper::createtag "reason" \
		  -chdata $value]
	    }
	    -alternativejid {
		lappend subelements [wrapper::createtag "alt" \
		  -attrlist [list jid $value]]
	    }
	    default {
		return -code error "Unrecognized option \"$name\""
	    }
	}
    }
      
    set destroyelem [wrapper::createtag "destroy" -subtags $subelements \
      -attrlist [list jid $roomjid]]

    set xmllist [wrapper::createtag "query" -subtags [list $destroyelem] \
      -attrlist [list xmlns $xmlnsmuc(owner)]]
    eval {$jlibname send_iq "set" [list $xmllist] -to $roomjid} $opts
}

# jlib::muc::getroom --
# 
# 

proc jlib::muc::getroom {mucname roomjid callback} {

    variable muc2jlib
    variable xmlnsmuc

    set jlibname $muc2jlib($mucname)
    set xmllist [wrapper::createtag "query" \
      -attrlist [list xmlns $xmlnsmuc(owner)]]
    $jlibname send_iq "get" [list $xmllist] -to $roomjid  \
      -command [list [namespace current]::invoke_callback $mucname $callback]
}

# jlib::muc::mynick --
# 
#       Returns own nick name for room, or empty if not there.

proc jlib::muc::mynick {mucname roomjid} {

    upvar ${mucname}::cache cache
    
    if {[info exists cache($roomjid,mynick)]} {
	return $cache($roomjid,mynick)
    } else {
	return ""
    }
}

# jlib::muc::allroomsin --
# 
#       Returns a list of all room jid's we are inside.

proc jlib::muc::allroomsin {mucname} {

    upvar ${mucname}::cache cache
    
    set roomList {}
    foreach key [array names cache "*,inside"] {
	regexp {(.+),inside} $key match room
	lappend roomList $room
    }
    return $roomList
}

proc jlib::muc::isroom {mucname jid} {
    
    upvar ${mucname}::rooms rooms
    
    if {[info exists rooms($jid)]} {
	return 1
    } else {
	return 0
    }
}

# jlib::muc::participants --
#
#

proc jlib::muc::participants {mucname roomjid} {

    variable muc2jlib
    upvar ${mucname}::cache cache
    
    set jlibname $muc2jlib($mucname)
    set rostername [[namespace parent]::getrostername $jlibname]
    set everyone {}

    # The rosters presence elements should give us all info we need.
    foreach userAttr [$rostername getpresence $roomjid -type available] {
	unset -nocomplain attrArr
	array set attrArr $userAttr
	lappend everyone ${roomjid}/$attrArr(-resource)
    }
    return $everyone
}

proc jlib::muc::Debug {num str} {
    variable debug
    if {$num <= $debug} {
	puts $str
    }
}

#-------------------------------------------------------------------------------

