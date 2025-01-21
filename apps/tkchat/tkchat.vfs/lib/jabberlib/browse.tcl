#  browse.tcl ---
#  
#      This file is part of The Coccinella application. 
#      It maintains the current state of all 'jid-types' for each server.
#      In other words, it manages the client's internal state corresponding
#      to 'iq' elements with namespace 'jabber:iq:browse'.
#      
#  Copyright (c) 2001-2004  Mats Bengtsson
#  
#  See the README file for license, bugs etc.
#  
# $Id: browse.tcl,v 1.32 2005/06/16 07:10:39 matben Exp $
# 
#  locals($jid,parent):       the parent of $jid.
#  locals($jid,parents):      list of all parent jid's,
#                             {server conference.server myroom@conference.server}
#  locals($jid,childs):       list of all childs of this jid if any.                             
#  locals($jid,xmllist):      the hierarchical xml list of this $jid.
#  locals($jid,type):         the type/subtype (category/type) of this $jid.
#  locals($type,typelist):    a list of jid's for this type/subtype.
#  locals(alltypes):          a list of all jid types.
#  locals($jid,allusers):     list of all users in room $jid.
#  locals($jid,isbrowsed):    1 if $jid browsed, 0 if not.
#
############################# USAGE ############################################
#
#       Changes to the state of this object should only be made from jabberlib,
#       and never directly by the client!
#       All jid's must be browsed hierarchically, that is, when browsing
#       'myroom@conference.server', 'conference.server' must have already been 
#       browsed, and browsing 'conference.server' requires 'server' to have
#       been browsed.
#
#   NAME
#      browse - an object for the ...
#      
#   SYNOPSIS
#      browse::new jlibname ?-command clientCommand?
#      
#   OPTIONS
#      none
#      
#   INSTANCE COMMANDS
#      browseName clear ?jid?
#      browseName delete
#      browseName get jid
#      browseName getchilds jid
#      browseName getconferenceservers
#      browseName getname jid
#      browseName getnamespaces jid
#      browseName getservicesforns ns
#      browseName getparentjid jid
#      browseName getparents jid
#      browseName gettype jid
#      browseName getalltypes globpattern
#      browseName getalljidfortypes globpattern
#      browseName getusers jid
#      browseName hasnamespace jid namespace
#      browseName isbrowsed jid
#      browseName isroom jid
#      browseName remove jid
#      browseName send_get jid ?-command procName?
#      browseName setjid jid xmllist       (only from jabberlib)
#
#   The 'clientCommand' procedure must have the following form:
#   
#      clientCommand {browseName type jid xmllist args}
#      
#   which is supposed to handle all get/set events which happen async.
#      
#   where 'type' can be 'set' or 'remove'.
#      
############################# CHANGES ##########################################
#
#       030703   removed browseName from browse::browse
#       040408   version 2.0
#       040408   browsename now fully qualified, added browse::new, send_get,
#                removed browse::browse,

package require wrapper

package provide browse 2.0

namespace eval browse {
    
    # Running number.
    variable uid 0
   
    # Globals same for all instances of all rooms.
    variable debug 0
    
    # Options only for internal use. EXPERIMENTAL!
    #     -setbrowsedjid:  default=1, store the browsed jid even if cached already
    variable options
    array set options {
	-setbrowsedjid 1
    }
}	

# browse::new --
# 
#       Creates a new instance of a browse object.
#       
# Arguments:
#       jlibname:     name of existing jabberlib instance
#       args:         -command procName {browsename type jid subiq args}
#                     the command is supposed to handle the async get/set 
#                     events only.
# 
# Results:
#       namespaced instance command

proc browse::new {jlibname args} {
    
    variable uid
    variable browse2jlib

    # Generate unique command token for this browse instance.
    # Fully qualified!
    set browsename [namespace current]::[incr uid]

    Debug 2 "browse::new jlibname=$jlibname, browsename=$browsename"
      
    # Instance specific namespace.
    namespace eval $browsename {
	variable locals
    }
    upvar ${browsename}::locals locals

    foreach {key value} $args {
	switch -- $key {
	    -command {
		set locals(cmd) $value
	    }
	    default {
		return -code error "unrecognized option \"$key\" for browse::new"
	    }
	}
    }
    set locals(confservers) {}
    set browse2jlib($browsename) $jlibname
    
    # Register service.
    $jlibname service register browse $browsename
    
    # Register some standard iq handlers that is handled internally.
    # The get/set events are async events and need to be handled by a -command.
    $jlibname iq_register get jabber:iq:browse  \
      [list [namespace current]::handle_get $browsename]
    $jlibname iq_register set jabber:iq:browse  \
      [list [namespace current]::handle_set $browsename]

    # Make sure we clean up any state if user logouts.
    $jlibname presence_register unavailable  \
      [list [namespace current]::handle_unavailable $browsename]
    
    # Create the actual browser instance procedure.
    proc $browsename {cmd args}   \
      "eval browse::CommandProc {$browsename} \$cmd \$args"
    
    return $browsename
}

# browse::CommandProc --
#
#       Just dispatches the command to the right procedure.
#
# Arguments:
#       browsename:   the instance of this conference browse.
#       cmd:        the method.
#       args:       all args to the cmd method.
#       
# Results:
#       none.

proc browse::CommandProc {browsename cmd args} {
        
    # Which command? Just dispatch the command to the right procedure.
    return [eval {$cmd $browsename} $args]
}

# browse::send_get --
#
#       Sends a get request within the browse namespace.
#
# Arguments:
#       browsename: the instance of this browse.
#       jid:        to jid
#       cmd:        procName {browsename type jid subiq args}      
#       
# Results:
#       none.

proc browse::send_get {browsename jid cmd} {
    
    variable browse2jlib
        
    Debug 2 "browse::send_get jid=$jid, cmd=$cmd"
    
    $browse2jlib($browsename) iq_get "jabber:iq:browse" -to $jid  \
      -command [list [namespace current]::parse_get $browsename $jid $cmd]
}

proc browse::parse_get {browsename jid cmd jlibname type subiq args} {

    upvar ${browsename}::locals locals

    Debug 2 "browse::parse_get jid=$jid, type=$type"

    switch -- $type {
	error {
	    uplevel #0 $cmd [list $browsename $type $jid $subiq] $args
	}
	default {
	    
	    # Set internal state first, then handle callback.
	    setjid $browsename $jid $subiq
	    uplevel #0 $cmd [list $browsename $type $jid $subiq] $args
	}
    }
}

# browse::handle_get --
# 
#       Hook for iq_register get.

proc browse::handle_get {browsename jlibname from subiq args} {
    
    upvar ${browsename}::locals locals

    Debug 2 "browse::handle_get from=$from"
    
    set ishandled 0
    if {[info exists locals(cmd)]} {
	set ishandled \
	  [uplevel #0 $locals(cmd) [list $browsename get $from $subiq] $args]
    }
    return $ishandled
}

# browse::handle_set --
# 
#       Hook for iq_register set.

proc browse::handle_set {browsename jlibname from subiq args} {
    
    upvar ${browsename}::locals locals

    Debug 2 "browse::handle_set from=$from, subiq='$subiq'"

    # Set internal state first, then handle any callback.
    setjid $browsename $from $subiq
    
    set ishandled 0
    if {[info exists locals(cmd)]} {
	set ishandled \
	  [uplevel #0 $locals(cmd) [list $browsename set $from $subiq] $args]
    }
    return $ishandled
}


proc browse::handle_unavailable {browsename jlibname jid type args} {
    
    Debug 2 "browse::handle_unavailable jid=$jid, type=$type"
    
    clear $browsename $jid
}

# browse::getparentjid --
#
#       Returns the logical parent of a jid. 
#       'matben@ayhkdws.se/home' => 'matben@ayhkdws.se' etc.
#       
# Arguments:
#       jid     the three-tier jid
#       
# Results:
#       another jid or empty if failed

proc browse::getparentjid {browsename jid} {
    
    upvar ${browsename}::locals locals

    set jid [jlib::jidmap $jid]
    if {[info exists locals($jid,parent)]} {
	set parJid $locals($jid,parent)
    } else {
	
	# Only to make it failsafe. DANGER!!!
	set parJid [GetParentJidFromJid $browsename $jid]
    }
    return $parJid
}

# This is not good...   DANGER!!!

proc browse::GetParentJidFromJid {browseName jid} {
        
    Debug 3 "GetParentJidFromJid BADBADBADBADBADBAD!!!  jid=$jid"

    set c {[^@/.<>:]+}
    if {[regexp "(${c}@(${c}\.)+${c})/${c}" $jid match parJid junk]} {	
    } elseif {[regexp "${c}@((${c}\.)+${c})" $jid match parJid junk]} {
    } elseif {[regexp "${c}\.((${c}\.)*${c})" $jid match parJid junk]} {
    } else {
	set parJid {}
    }
    return $parJid
}

# browse::get --
#
# Arguments:
#       browsename:   the instance of this conference browse.
#
# Results:
#       Hierarchical xmllist if already browsed or empty if not browsed.

proc browse::get {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::get  jid=$jid"
    
    set jid [jlib::jidmap $jid]
    if {[info exists locals($jid,xmllist)]} {
	return $locals($jid,xmllist)
    } else {
	return ""
    }
}

proc browse::isbrowsed {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::isbrowsed  jid=$jid"
    
    set jid [jlib::jidmap $jid]
    if {[info exists locals($jid,isbrowsed)] && ($locals($jid,isbrowsed) == 1)} {
	return 1
    } else {
	return 0
    }
}
    
# browse::remove --
#
#
# Arguments:
#       browsename:   the instance of this browse.
#       jid:          jid to remove.
#
# Results:
#       none.

proc browse::remove {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::remove  jid=$jid"
    
    set mjid [jlib::jidmap $jid]
    
    unset -nocomplain locals($mjid,parents) locals($mjid,xmllist) \
      locals($mjid,isbrowsed)

    # Evaluate the client callback.
    if {[info exists locals(cmd)]} {
	uplevel #0 $locals(cmd) [list $browsename remove $jid {}]
    }
}
    
# browse::getparents --
#
#
# Arguments:
#       browsename:   the instance of this browse.
#
# Results:
#       List of all parent jid's.

proc browse::getparents {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::getparents  jid=$jid"

    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,parents)]} {
	return $locals($jid,parents)
    } else {
	return ""
    }
}
    
# browse::getchilds --
#
#
# Arguments:
#       browsename:   the instance of this browse.
#
# Results:
#       List of all parent jid's.

proc browse::getchilds {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::getchilds  jid=$jid"
    
    set jid [jlib::jidmap $jid]
    if {[info exists locals($jid,childs)]} {
	return $locals($jid,childs)
    } else {
	return ""
    }
}
    
# browse::getname --
#
#       Returns the nickname of a jid in conferencing, or the rooms name
#       if jid is a room.
#
# Arguments:
#       browsename:   the instance of this conference browse.
#       
# Results:
#       The nick, room name or empty if undefined.

proc browse::getname {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::getname  jid=$jid"
    
    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,name)]} {
	return $locals($jid,name)
    } else {
	return ""
    }
}
    
# browse::getusers --
#
#       Returns all users of a room jid in conferencing.
#
# Arguments:
#       browsename:   the instance of this conference browse.
#       jid:          must be a room jid: 'roomname@server'.
#       
# Results:
#       The nick name or empty if undefined.

proc browse::getusers {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::getusers  jid=$jid"
    
    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,allusers)]} {
	return $locals($jid,allusers)
    } else {
	return ""
    }
}
    
# browse::getconferenceservers --
#
#

proc browse::getconferenceservers {browsename} {    
    
    upvar ${browsename}::locals locals
    
    return $locals(confservers)
}    
    
# browse::getservicesforns --
#
#       Gets all jid's that support a certain namespace.
#       Only for the browsed services.

proc browse::getservicesforns {browsename ns} {    
    
    upvar ${browsename}::locals locals
    
    if {[info exists locals(ns,$ns)]} {
	return $locals(ns,$ns)
    } else {
	return ""
    }
}

# browse::isroom --
#
#       If 'jid' is a child of a conference server, that is, a room.

proc browse::isroom {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    set jid [jlib::jidmap $jid]    
    set parentJid [getparentjid $browsename $jid]

    # Check if this is in our list of conference servers.
    set ind [lsearch -exact $locals(confservers) $parentJid]
    return [expr ($ind < 0) ? 0 : 1]
}    
    
# browse::gettype --
#
#       Returns the jidType/subType if found.

proc browse::gettype {browsename jid} {    
    
    upvar ${browsename}::locals locals
    
    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,type)]} {
	return $locals($jid,type)
    } else {
	return ""
    }
}    

# browse::getalljidfortypes --
#
#       Returns all jids that match the glob pattern typepattern.
#       
# Arguments:
#       browsename:   the instance of this conference browse.
#       typepattern:  a global pattern of jid type/subtype (service/*).
#
# Results:
#       List of jid's matching the type pattern.

proc browse::getalljidfortypes {browsename typepattern} {
    
    upvar ${browsename}::locals locals
    
    set allkeys [array names locals "${typepattern},typelist"]
    set jidlist {}
    
    foreach key $allkeys {
	set locals($key) [lsort -unique $locals($key)]
	set jidlist [concat $jidlist $locals($key)]
    }
    return $jidlist
}    

# browse::getalltypes --
#
#       Returns all types that match the glob pattern typepattern.
#       
# Arguments:
#       browsename:   the instance of this conference browse.
#       typepattern:  a glob pattern of jid type/subtype (service/*).
#
# Results:
#       List of types matching the type pattern.

proc browse::getalltypes {browsename typepattern} {    
    
    upvar ${browsename}::locals locals
    
    set ans {}
    if {[info exists locals(alltypes)]} {
	set locals(alltypes) [lsort -unique $locals(alltypes)]
	foreach type $locals(alltypes) {
	    if {[string match $typepattern $type]} {
		lappend ans $type
	    }
	}
    }
    return $ans
}    

# browse::getnamespaces --
#
#       Returns all namespaces for this jid describing the services available.
#
# Arguments:
#       browsename:   the instance of this conference browse.
#       jid:          .
#       
# Results:
#       List of namespaces or empty if none.

proc browse::getnamespaces {browsename jid} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::getnamespaces  jid=$jid"

    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,ns)]} {
	return $locals($jid,ns)
    } else {
	return ""
    }
}

# browse::hasnamespace --
#
#       Returns 0/1 if jid supports this namespace.
#
# Arguments:
#       browsename:   the instance of this conference browse.
#       jid:          .
#       ns            namespace.
#       
# Results:
#       0/1

proc browse::hasnamespace {browsename jid ns} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::hasnamespace  jid=$jid, ns=$ns"

    set jid [jlib::jidmap $jid]    
    if {[info exists locals($jid,ns)]} {
	return [expr [lsearch $locals($jid,ns) $ns] < 0 ? 0 : 1]
    } else {
	return 0
    }
}

# browse::setjid --
#
#       Called when receiving a 'set' or 'result' iq element in jabber:iq:browse
#       Shall only be called from jabberlib
#       Sets internal state, and makes callback to client proc. 
#       Could be called with type='remove' attribute.
#       For 'user' elements we need to build a table that maps the
#       'roomname@server/hexname' with the nick name.
#       It also keeps a list of all 'user's in a room.
#       
# Arguments:
#       browsename:   the instance of this conference browse.
#       fromJid:      the 'from' attribute which is also the parent of any
#                     childs.
#       subiq:      hierarchical xml list starting with element containing
#                     the xmlns='jabber:iq:browse' attribute.
#                     Any children defines a parent-child relation.
#                     
#                   ???????????
#       args:       -command cmdProc:   replaces the client callback command
#                        in the browse object
#       
# Results:
#       none.

proc browse::setjid {browsename fromJid subiq args} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::setjid browsename=$browsename, fromJid=$fromJid\n\t\
      subiq='[string range $subiq 0 80]...', args='$args'"

    set theTag [wrapper::gettag $subiq]
    array set attrArr [wrapper::getattrlist $subiq]
    array set argsArr $args
    
    # Seems that the browse component doesn't do STRINGPREP.
    set fromJid [jlib::jidmap $fromJid]
    
    # Root parent empty. A bit unclear what to do with it.
    if {![info exists locals($fromJid,parent)]} {
	
	# This can be a completely new room not seen before.
	# Workoround for bug in 'conference 0.4.1' component. No parent!
        # <iq type='set' to='matben@localhost/xx' 
        #         from='junk@conference.localhost'>
        #     <conference xmlns='jabber:iq:browse' name='Junk' type='public'/>
        # </iq>
	if {[string match *@* $fromJid]} {

	    # Ugly!!!
	    set parentJid [getparentjid $browsename $fromJid]
	    set locals($fromJid,parent) $parentJid
	    if {[info exists locals($parentJid,parents)]} {
		set locals($fromJid,parents)  \
		  [concat $locals($parentJid,parents) $parentJid]
	    } else {
		set locals($fromJid,parents) $parentJid
		set locals($parentJid,parents) {}
	    }
	} else {
	
	    # Else we assume it is a root. Not correct!
	    set locals($fromJid,parent) {}
	    set locals($fromJid,parents) {}
	    set parentJid {}
	}
    }
    
    # Docs say that jid is required attribute but... 
    # <conference> and <service> seem to lack jid.
    # If no jid attribute it is probably(?) assumed to be 'fromJid.
    if {![info exists attrArr(jid)]} {
	set jid $fromJid
	set parentJid $locals($jid,parent)
    } else {
	
	# Must do STRINGPREP when comparing two jids!
	set jid [jlib::jidmap $attrArr(jid)]
	if {$fromJid != $jid} {
	    set parentJid $fromJid
	} else {
	    set parentJid $locals($jid,parent)
	}
    }
    set locals($fromJid,isbrowsed) 1
    set locals($jid,isbrowsed) 1
    
    # Handle the top jid, and follow recursively for any childs.
    setsinglejid $browsename $parentJid $jid $subiq 1
}

# browse::setsinglejid --
#
#       Gets called for each jid in the jabber:iq:browse callback.
#       The recursive helper proc for 'setjid'.
#       
# Arguments:
#       browsename:   the instance of this conference browse.
#       parentJid:    the logical parent of 'jid'
#       jid:          the 'jid' we are setting; if empty it is in attribute list.
#       xmllist:      hierarchical xml list.
#                     Any children defines a parent-child relation.
#       
# Results:
#       none.

proc browse::setsinglejid {browsename parentJid jid xmllist {browsedjid 0}} {

    variable options
    variable browse2jlib
    
    upvar ${browsename}::locals locals
    
    set category [wrapper::gettag $xmllist]
    array set attrArr [wrapper::getattrlist $xmllist]

    # Check for any 'category' attribute introduced in the 1.2 rev. of JEP-0011.
    if {[info exists attrArr(category)]} {
    	set category $attrArr(category)
    }
    
    # If the 'jid' is empty we get it from our attributes!
    if {[string length $jid] == 0} {
	set jid $attrArr(jid)
    }
    
    Debug 3 "browse::setsinglejid parentJid=$parentJid, jid=$jid, category=$category"
    
    # First, is this a "set" or a "remove" type?
    if {[info exists attrArr(type)] && [string equal $attrArr(type) "remove"]} {
	if {[string equal $category "user"]} {
	    
	    # Be sure to update the room's list of participants.
	    if {[info exists locals($parentJid,allusers)]} {
		set ind [lsearch $locals($parentJid,allusers) $jid]
		if {$ind >= 0} {
		    set locals($parentJid,allusers)   \
		      [lreplace $locals($parentJid,allusers) $ind $ind]
		}
	    }
	}
    } elseif {$options(-setbrowsedjid) || !$browsedjid} {
	
	# Set type.
	set locals($jid,xmllist) $xmllist
	
	# Set up parents for this jid.
	# Root's parent is empty. When not root, store parent(s).
	if {[string length $parentJid] > 0} {
	    set locals($jid,parent) $parentJid
	    set locals($jid,parents)   \
	      [concat $locals($parentJid,parents) $parentJid]
	}
	
	# Add us to parentJid's child list if not there already.
	if {![info exists locals($parentJid,childs)]} {
	    set locals($parentJid,childs) {}
	}
	if {[lsearch -exact $locals($parentJid,childs) $jid] < 0} {
	    lappend locals($parentJid,childs) $jid
	}
	
	if {[info exists attrArr(type)]} {
	    set jidtype $category/$attrArr(type)
	    set locals($jid,type) $jidtype
	    lappend locals($jidtype,typelist) $jid
	    lappend locals(alltypes) $jidtype
	    set locals($jidtype,typelist) \
	    	[lsort -unique $locals($jidtype,typelist)]
	    set locals(alltypes) [lsort -unique $locals(alltypes)]
	}
	
	# Cache additional info depending on the tag.
	switch -exact -- $category {
	    conference {
	    
		# This is either a conference server or one of its rooms.
		if {[string match *@* $jid]} {
		    
		    # This must be a room. Cache its name.
		    if {[info exists attrArr(name)]} {
			set locals($jid,name) $attrArr(name)
		    }
		} else {
		
		    # Cache all conference servers. Don't count the rooms.
		    if {[lsearch -exact $locals(confservers) $jid] < 0} {		    
			lappend locals(confservers) $jid
		    }
		}
	    }
	    user {
	    
		# If with 'user' tag in conferencing, keep internal table that
		# maps the 'room@server/hexname' to nickname.
		if {[info exists attrArr(name)]} {
		    set locals($jid,name) $attrArr(name)
		}
		
		# Keep list of all 'user's in a room. The 'parentJid' must
		# be the room's jid here.
		lappend locals($parentJid,allusers) $jid
		set locals($parentJid,allusers)  \
		  [lsort -unique $locals($parentJid,allusers)]
	    }	    
	}
    }
    # End set type.
    
    # Loop through the children if any. Defines a parentship.
    # Only exception is a namespace definition <ns/>.
    foreach child [wrapper::getchildren $xmllist] {
	if {[string equal [wrapper::gettag $child] "ns"]} {
	    
	    # Cache any namespace declarations.
	    set ns [wrapper::getcdata $child]
	    lappend locals($jid,ns) $ns
	    set locals($jid,ns) [lsort -unique $locals($jid,ns)]
	    lappend locals(ns,$ns) $jid
	    set locals(ns,$ns) [lsort -unique $locals(ns,$ns)]
	    
	    # Register any groupchat protocol.
	    # There seems to be a problem here since not all conference
	    # components list <ns>jabber:iq:conference</ns> in their browse
	    # section.
	    if {[string equal $category "conference"]} {
	    
		switch -- $ns {
		    "http://jabber.org/protocol/muc" {
			$browse2jlib($browsename) service registergcprotocol $jid "muc"
		    }
		    "jabber:iq:conference" {
			$browse2jlib($browsename) service registergcprotocol $jid "conference"
		    }
		    "gc-1.0" {
			$browse2jlib($browsename) service registergcprotocol $jid "gc-1.0"
		    }
		}
	    }
	} else {
	    
	    # Now jid is the parent, and the jid to set is an attribute.
	    setsinglejid $browsename $jid {} $child
	}
    }
}

# browse::clear --
#
#       Empties everything cached internally for the specified jid (and all
#       its children ?).
#       Problem since icq.jabber.se child of icq.jabber.se/registered (?!)
#       It must be failsafe in case of missing browse elements.

proc browse::clear {browsename {jid {}}} {
    
    upvar ${browsename}::locals locals
    
    Debug 3 "browse::clear browse::clear $jid"
    if {[string length $jid]} {

	# testing...
	set jid [jlib::jidmap $jid]    
	ClearJidIsbrowsed $browsename $jid
	ClearJid $browsename $jid
    } else {
	ClearAll $browsename
    }
}

proc browse::ClearJid {browsename jid} {
    
    upvar ${browsename}::locals locals

    # Can be problems with this (ICQ)
    if {0 && [info exists locals($jid,childs)]} {
	foreach child $locals($jid,childs) {
	    ClearJid $browsename $child
	}
    }
    
    # Guard against the case where no parent registered.    
    # Keep parents!
    if {[info exists locals($jid,parent)]} {
    	set parent $locals($jid,parent)
    }
    if {[info exists locals($jid,parents)]} {
    	set parents $locals($jid,parents)
    }

    # Remove this specific jid from our internal state.
    array unset locals [jlib::ESC $jid],*
    if {[info exists parent]} {
    	set locals($jid,parent) $parent
    }
    if {[info exists parents]} {
    	set locals($jid,parents) $parents
    }
}

proc browse::ClearJidIsbrowsed {browsename jid} {
    
    upvar ${browsename}::locals locals

    if {[info exists locals($jid,childs)]} {
	foreach child $locals($jid,childs) {
	    ClearJidIsbrowsed $browsename $child
	}
    }
    unset -nocomplain locals($jid,isbrowsed)
}
    
# browse::ClearAll --
#
#       Empties everything cached internally.

proc browse::ClearAll {browsename} {
    
    upvar ${browsename}::locals locals

    # Be sure to keep some entries! Separate array for these?
    if {[info exists locals(cmd)]} {
	set clientCmd $locals(cmd)
    }
    unset locals
    if {[info exists clientCmd]} {
	set locals(cmd) $clientCmd
    }
    set locals(confservers) {}
}

# browse::delete --
#
#       Deletes the complete object.

proc browse::delete {browsename} {
    
    namespace delete $browsename
}

proc browse::Debug {num str} {
    variable debug

    if {$num <= $debug} {
	puts $str
    }
}

#-------------------------------------------------------------------------------

