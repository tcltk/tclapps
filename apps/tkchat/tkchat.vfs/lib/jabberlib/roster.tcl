# roster.tcl --
#
#       An object for storing the roster and presence information for a 
#       jabber client. Is used together with jabberlib.
#
# Copyright (c) 2001-2005  Mats Bengtsson
#  
# $Id: roster.tcl,v 1.34 2005/06/16 07:10:40 matben Exp $
# 
# Note that every jid in the rostArr is usually (always) without any resource,
# but the jid's in the presArr are identical to the 'from' attribute, except
# the presArr($jid-2,res) which have any resource stripped off. The 'from' 
# attribute are (always) with /resource.
# 
# All jid's in internal arrays are STRINGPREPed!
# 
# Variables used in roster:
# 
#       rostArr(groups)             : List of all groups the exist in roster.
#
#	rostArr($jid,item)          : $jid.
#	
#	rostArr($jid,name)          : Name of $jid.
#	
#	rostArr($jid,groups)        : Groups $jid is in. Note: PLURAL!
#
#	rostArr($jid,subscription)  : Subscription of $jid (to|from|both|"")
#
#	rostArr($jid,ask)           : "Ask" of $jid 
#                                     (subscribe|unsubscribe|"")
#                                       
#	presArr($jid-2,res)         : List of resources for this $jid.
#
#       presArr($from,type)         : One of 'available' or 'unavailable.
#
#       presArr($from,status)       : The presence status element.
#
#       presArr($from,priority)     : The presence priority element.
#
#       presArr($from,show)         : The presence show element.
#
#       presArr($from,x,xmlns)      : Storage for x elements.
#                                     xmlns is a namespace but where any
#                                     http://jabber.org/protocol/ stripped off
#                  
#       oldpresArr                  : As presArr but any previous state.
#                                      
############################# USAGE ############################################
#
#       Changes to the state of this object should only be made from jabberlib,
#       and never directly by the client!
#
#   NAME
#      roster - an object for roster and presence information.
#      
#   SYNOPSIS
#      roster::roster clientCommand
#      
#   OPTIONS
#      none
#      
#   INSTANCE COMMANDS
#      rostName clearpresence ?jidpattern?
#      rostName enterroster
#      rostName exitroster
#      rostName getgroups ?jid?
#      rostName getask jid
#      rostName getcapsattr jid name
#      rostName getname jid
#      rostName getpresence jid ?-resource, -type?
#      rostName getresources jid
#      rostName gethighestresource jid
#      rostName getrosteritem jid
#      rostName getsubscription jid
#      rostName getusers ?-type available|unavailable?
#      rostName getx jid xmlns
#      rostName getextras jid xmlns
#      rostName isavailable jid
#      rostName isitem jid
#      rostName removeitem jid
#      rostName reset
#      rostName setpresence jid type ?-option value -option ...?
#      rostName setrosteritem jid ?-option value -option ...?
#      rostName wasavailable jid
#      
#   The 'clientCommand' procedure must have the following form:
#   
#      clientCommand {rostName what {jid {}} args}
#      
#   where 'what' can be any of: enterroster, exitroster, presence, remove, set.
#   The args is a list of '-key value' pairs with the following keys for each
#   'what':
#       enterroster:   no keys
#       exitroster:    no keys
#       presence:    -resource      (required)
#                    -type          (required)
#                    -status        (optional)
#                    -priority      (optional)
#                    -show          (optional)
#                    -x             (optional)
#                    -extras        (optional)
#       remove:      no keys
#       set:         -name          (optional)
#                    -subscription  (optional)
#                    -groups        (optional)
#                    -ask           (optional)
#      
############################# CHANGES ##########################################
#
#       1.0a1    first release by Mats Bengtsson
#       1.0a2    clear roster and presence array before receiving such elements
#       1.0a3    added reset, isavailable, getresources, and getsubscription 
#       1.0b1    added gethighestresource command
#                changed setpresence arguments
#       1.0b2    changed storage of x elements, added getx command.
#       030602   added clearpresence command.
#       030702   added -type option to getusers command.
#       030703   removed rostName from roster::roster
#       040514   does STRINGPREP on all jids

package provide roster 1.0

namespace eval roster {
    
    variable rostGlobals
    
    # Globals same for all instances of this roster.
    set rostGlobals(debug) 0

    # Running number.
    variable uid 0
    
    # List of all rostArr element sub entries. First the actual roster,
    # with 'rostArr($jid,...)'
    set rostGlobals(tags) {name groups ask subscription} 
    
    # ...and the presence arrays: 'presArr($jid/$resource,...)'
    # The list of resources is treated separately (presArr($jid,res))
    set rostGlobals(presTags) {type status priority show x}
}

# roster::roster --
#
#       This creates a new instance of a roster.
#       
# Arguments:
#       clientCmd:  callback procedure when internals of roster or
#                   presence changes.
#       args:            
#       
# Results:
#       rostName which is the command for this instance of the roster
  
proc roster::roster {clientCmd args} {

    variable uid

    # Generate unique command token for this roster instance.
    # Fully qualified!
    set rostName [namespace current]::[incr uid]
      
    # Instance specific namespace.
    namespace eval $rostName {
	variable rostArr
	variable presArr
	variable options
	
	array set rostArr {}
	array set presArr {}
    }
    
    # Set simpler variable names.
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::options options
        
    set rostArr(groups) {}
    set options(cmd) $clientCmd
    
    # Create the actual roster instance procedure.
    proc $rostName {cmd args}   \
      "eval roster::CommandProc {$rostName} \$cmd \$args"

    return $rostName
}

# roster::CommandProc --
#
#       Just dispatches the command to the right procedure.
#
# Arguments:
#       rostName:   the instance of this roster.
#       cmd:        .
#       args:       all args to the cmd procedure.
#       
# Results:
#       none.

proc roster::CommandProc {rostName cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    return [eval {$cmd $rostName} $args]
}

# roster::setrosteritem --
#
#       Adds or modifies an existing roster item.
#       Features not set are left as they are; features not set will give
#       nonexisting array entries, just to differentiate between an empty
#       element and a nonexisting one.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        2-tier jid, with no /resource, usually.
#                   Some transports keep a resource part in jid.
#       args:       a list of '-key value' pairs, where '-key' is any of:
#                       -name value
#                       -subscription value
#                       -groups list        Note: GROUPS in plural!
#                       -ask value
#       
# Results:
#       none.

proc roster::setrosteritem {rostName jid args} {        
    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::setrosteritem rostName=$rostName, jid='$jid', args='$args'"
        
    set mjid [jlib::jidmap $jid]
    
    # Clear out the old state since an 'ask' element may still be lurking.
    foreach key $rostGlobals(tags) {
	unset -nocomplain rostArr($mjid,$key)
    }
    
    # This array is better than list to keep track of users.
    set rostArr($mjid,item) $mjid
    
    # Old values will be overwritten, nonexisting options will result in
    # nonexisting array entries.
    foreach {name value} $args {
	set par [string trimleft $name "-"]
	set rostArr($mjid,$par) $value
	if {[string equal $par "groups"]} {
	    foreach gr $value {
		if {[lsearch $rostArr(groups) $gr] < 0} {
		    lappend rostArr(groups) $gr
		}
	    }
	}
    }
    
    # Be sure to evaluate the registered command procedure.
    if {[string length $options(cmd)]} {
	uplevel #0 $options(cmd) [list $rostName set $jid] $args
    }
    return {}
}

# roster::removeitem --
#
#       Removes an existing roster item and all its presence info.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        2-tier jid with no /resource.
#       
# Results:
#       none.

proc roster::removeitem {rostName jid} {       
    variable rostGlobals

    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr presArr
    upvar ${rostName}::oldpresArr oldpresArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::removeitem rostName=$rostName, jid='$jid'"
    
    set mjid [jlib::jidmap $jid]
    
    # Be sure to evaluate the registered command procedure.
    # Do this before unsetting the internal state!
    if {[string length $options(cmd)]} {
	uplevel #0 $options(cmd) [list $rostName remove $jid]
    }
    
    # First the roster, then presence...
    foreach name $rostGlobals(tags) {
	unset -nocomplain rostArr($mjid,$name)
    }
    unset -nocomplain rostArr($mjid,item)
    
    # Be sure to unset all, also jid3 entries!
    array unset presArr [jlib::ESC $mjid]*
    array unset oldpresArr [jlib::ESC $mjid]*
    return {}
}

# roster::ClearRoster --
#
#       Removes all existing roster items but keeps all presence info.(?)
#       and list of resources.
#
# Arguments:
#       rostName:   the instance of this roster.
#       
# Results:
#       none. Callback evaluated.

proc roster::ClearRoster {rostName} {    

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::ClearRoster rostName=$rostName"
        
    # Remove the roster.
    foreach {x mjid} [array get rostArr *,item] {
	foreach key $rostGlobals(tags) {
	    unset -nocomplain rostArr($mjid,$key)
	}
    }
    array unset rostArr *,item
    
    # Be sure to evaluate the registered command procedure.
    if {[string length $options(cmd)]} {
	uplevel #0 $options(cmd) [list $rostName enterroster]
    }
    return {}
}

# roster::enterroster --
#
#       Is called when new roster coming.
#
# Arguments:
#       rostName:   the instance of this roster.
#       
# Results:
#       none.

proc roster::enterroster {rostName} {

    ClearRoster $rostName
}

# roster::exitroster --
#
#       Is called when finished receiving a roster get command.
#
# Arguments:
#       rostName:   the instance of this roster.
#       
# Results:
#       none. Callback evaluated.

proc roster::exitroster {rostName} {    

    upvar ${rostName}::options options

    # Be sure to evaluate the registered command procedure.
    if {[string length $options(cmd)]} {
	uplevel #0 $options(cmd) [list $rostName exitroster]
    }
}

# roster::reset --
#
#       Removes everything stored in the roster object, including all roster
#       items and any presence information.

proc roster::reset {rostName} {

    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr presArr
    
    unset -nocomplain rostArr presArr
    set rostArr(groups) {}
}

# roster::clearpresence --
# 
#       Removes all presence cached internally for jid glob pattern.
#       Helpful when exiting a room.
#       
# Arguments:
#       rostName:   the instance of this roster.
#       jidpattern: glob pattern for items to remove.
#       
# Results:
#       none.

proc roster::clearpresence {rostName {jidpattern ""}} {

    upvar ${rostName}::presArr presArr
    upvar ${rostName}::oldpresArr oldpresArr

    Debug 2 "roster::clearpresence $rostName '$jidpattern'"

    if {$jidpattern == ""} {
	unset -nocomplain presArr
    } else {
	array unset presArr $jidpattern
	array unset oldpresArr $jidpattern
    }
}

# roster::setpresence --
#
#       Sets the presence of a roster item. Adds the corresponding resource
#       to the list of resources for this jid.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        the from attribute. Usually 3-tier jid with /resource part.
#       type:       one of 'available', 'unavailable', or 'unsubscribed'.
#       args:       a list of '-key value' pairs, where '-key' is any of:
#                     -status value
#                     -priority value
#                     -show value
#                     -x list of xml lists
#       
# Results:
#       none.

proc roster::setpresence {rostName jid type args} { 

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr presArr
    upvar ${rostName}::oldpresArr oldpresArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::setpresence rostName=$rostName, jid='$jid', \
      type='$type', args='$args'"
    
    set mjid [jlib::jidmap $jid]
    jlib::splitjid $mjid mjid2 resource
    jlib::splitjid $jid jid2 x
    
    # XMPP specifies that an 'unavailable' element is sent *after* we've got
    # an subscription='remove' element. Store?
    
    if {[string equal $type "unsubscribed"]} {
	set argList [list -type $type]
    } else {
	
	# Keep cache of any old state.
        # Note special handling of * for array unset - prefix with \\ to quote.
	array unset oldpresArr [jlib::ESC $mjid],*
	array set oldpresArr [array get presArr [jlib::ESC $mjid],*]
	
	# Clear out the old presence state since elements may still be lurking.
	array unset presArr [jlib::ESC $mjid],*
	
	# Should we add something more to our roster, such as subscription,
	# if we haven't got our roster before this?
	
	# Add to list of resources.
	set presArr($mjid2,res) [lsort -unique [lappend presArr($mjid2,res) \
	  $resource]]
	
	set presArr($mjid,type) $type
	
	foreach {name value} $args {
	    set par [string trimleft $name "-"]
	    
	    switch -- $par {
		x {
		    
		    # This is a list of <x> lists.
		    foreach xelem $value {
			set ns [wrapper::getattribute $xelem xmlns]
			regexp {http://jabber.org/protocol/(.*)$} $ns \
			  match ns
			set presArr($mjid,x,$ns) $xelem
		    }
		}
		extras {

		    # This can be anything properly namespaced.
		    foreach xelem $value {
			set ns [wrapper::getattribute $xelem xmlns]
			set presArr($mjid,extras,$ns) $xelem
		    }
		}
		default {
		    set presArr($mjid,$par) $value
		}
	    }
	}
    }
    return {}
}

# roster::invokecommand --
# 
#       Evaluates the registered command procedure if any.

proc roster::invokecommand {rostName jid type args} { 

    upvar ${rostName}::options options
        
    if {[string length $options(cmd)]} {
	jlib::splitjid $jid jid2 resource
	set argList $args
	lappend argList -type $type -resource $resource
	uplevel #0 $options(cmd) [list $rostName presence $jid2] $argList
    }
}


# Firts attempt to keep the jid's as they are reported, with no separate
# resource part.

proc roster::setpresence2 {rostName jid type args} { 

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr2 presArr2
    upvar ${rostName}::oldpresArr2 oldpresArr2
    upvar ${rostName}::options options

    Debug 2 "roster::setpresence2 rostName=$rostName, jid='$jid', \
      type='$type', args='$args'"
    
    set mjid [jlib::jidmap $jid]
    
    set argList $args
    lappend argList -type $type

    if {[string equal $type "unsubscribed"]} {
	# empty
    } else {
	
	# Keep cache of any old state.
	array unset oldpresArr2 [jlib::ESC $mjid],*
	array set oldpresArr2 [array get presArr2 [jlib::ESC $mjid],*]
	
	# Clear out the old presence state since elements may still be lurking.
	array unset presArr2 [jlib::ESC $mjid],*
    
	set presArr2($mjid,type) $type
	set presArr2($mjid,jid)  $mjid
		
	foreach {name value} $args {
	    set par [string trimleft $name "-"]
	    
	    switch -- $par {
		x {
		    
		    # This is a list of <x> lists.
		    foreach xelem $value {
			set ns [wrapper::getattribute $xelem xmlns]
			regexp {http://jabber.org/protocol/(.*)$} $ns \
			  match ns
			set presArr2($mjid,x,$ns) $xelem
		    }
		}
		extras {

		    # This can be anything properly namespaced.
		    foreach xelem $value {
			set ns [wrapper::getattribute $xelem xmlns]
			set presArr2($mjid,extras,$ns) $xelem
		    }
		}
		default {
		    set presArr2($mjid,$par) $value
		}
	    }
	}
    }
    return {}
}

# roster::getrosteritem --
#
#       Returns the state of an existing roster item.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        .
#       
# Results:
#       a list of '-key value' pairs where key is any of: 
#       name, groups, subscription, ask. Note GROUPS in plural!

proc roster::getrosteritem {rostName jid} {    

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::getrosteritem rostName=$rostName, jid='$jid'"
    
    set mjid [jlib::jidmap $jid]
    if {![info exists rostArr($mjid,item)]} {
	return {}
    }
    set result {}
    foreach key $rostGlobals(tags) {
	if {[info exists rostArr($mjid,$key)]} {
	    lappend result -$key $rostArr($mjid,$key)
	}
    }
    return $result
}

# roster::isitem --
# 
#       Does the jid exist in the roster?

proc roster::isitem {rostName jid} {
    
    upvar ${rostName}::rostArr rostArr
    
    set mjid [jlib::jidmap $jid]
    if {[info exists rostArr($mjid,item)]} {
	return 1
    } else {
	return 0
    }
}

# roster::getusers --
#
#       Returns a list of jid's of all existing roster items.
#
# Arguments:
#       rostName:   the instance of this roster.
#       args:       -type available|unavailable
#       
# Results:
#       list of all 2-tier jid's in roster

proc roster::getusers {rostName args} {

    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr presArr    
    
    set all {}
    foreach {x jid} [array get rostArr *,item] {
	lappend all $jid
    }
    array set argsArr $args
    set jidlist {}
    if {$args == {}} {
	set jidlist $all
    } elseif {[info exists argsArr(-type)]} {
	set type $argsArr(-type)
	set jidlist {}
	foreach jid2 $all {
	    set isavailable 0

	    # Be sure to handle empty resources as well: '1234@icq.host'
	    foreach key [array names presArr "[jlib::ESC $jid2]*,type"] {
		if {[string equal $presArr($key) "available"]} {
		    set isavailable 1
		    break
		}
	    }
	    if {$isavailable && [string equal $type "available"]} {
		lappend jidlist $jid2
	    } elseif {!$isavailable && [string equal $type "unavailable"]} {
		lappend jidlist $jid2
	    }
	}	
    }
    return $jidlist
}

# roster::getpresence --
#
#       Returns the presence state of an existing roster item.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        username@server, without /resource.
#       args        ?-resource, -type?
#                   -resource: return presence for this alone,
#                       else a list for each resource.
#                       Allow empty resources!!??
#                   -type: return presence for (un)available only.
#       
# Results:
#       a list of '-key value' pairs where key is any of: 
#       resource, type, status, priority, show, x.
#       If the 'resource' in argument is not given,
#       the result contains a sublist for each resource. IMPORTANT! Bad?
#       BAD!!!!!!!!!!!!!!!!!!!!!!!!

proc roster::getpresence {rostName jid args} {    

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr presArr
    upvar ${rostName}::options options
    
    Debug 2 "roster::getpresence rostName=$rostName, jid=$jid, args='$args'"
    
    set jid [jlib::jidmap $jid]
    array set argsArr $args
    set haveRes 0
    if {[info exists argsArr(-resource)]} {
	set haveRes 1
	set resource $argsArr(-resource)
    }
    
    # It may happen that there is no roster item for this jid (groupchat).
    if {![info exists presArr($jid,res)] || ($presArr($jid,res) == "")} {
	if {[info exists argsArr(-type)] &&  \
	  [string equal $argsArr(-type) "available"]} {
	    return {}
	} else {
	    if {$haveRes} {
		return [list -resource $resource -type unavailable]
	    } else {      
		return [list [list -resource "" -type unavailable]]
	    }
	}
    }
    
    set result {}
    if {$haveRes} {

	# Return presence only from the specified resource.
	# Be sure to handle empty resources as well: '1234@icq.host'
	if {[lsearch -exact $presArr($jid,res) $resource] < 0} {
	    return [list -resource $resource -type unavailable]
	}
	set result [list -resource $resource]
	if {$resource == ""} {
	    set jid3 $jid
	} else {
	    set jid3 $jid/$resource
	}
	if {[info exists argsArr(-type)] &&  \
	  ![string equal $argsArr(-type) $presArr($jid3,type)]} {
	    return {}
	}
	foreach key $rostGlobals(presTags) {
	    if {[info exists presArr($jid3,$key)]} {
		lappend result -$key $presArr($jid3,$key)
	    }
	}
    } else {
	
	# Get presence for all resources.
	# Be sure to handle empty resources as well: '1234@icq.host'
	foreach res $presArr($jid,res) {
	    set thisRes [list -resource $res]
	    if {$res == ""} {
		set jid3 $jid
	    } else {
		set jid3 $jid/$res
	    }
	    if {[info exists argsArr(-type)] &&  \
	      ![string equal $argsArr(-type) $presArr($jid3,type)]} {
		# Empty.
	    } else {
		foreach key $rostGlobals(presTags) {
		    if {[info exists presArr($jid3,$key)]} {
			lappend thisRes -$key $presArr($jid3,$key)
		    }
		}
		lappend result $thisRes
	    }
	}
    }
    return $result
}

# UNFINISHED!!!!!!!!!!
# Return empty list or -type unavailable ???
# '-key value' or 'key value' ???
# Returns a list of flat arrays

proc roster::getpresence2 {rostName jid args} {    

    variable rostGlobals
    upvar ${rostName}::rostArr rostArr
    upvar ${rostName}::presArr2 presArr2
    upvar ${rostName}::options options
    
    Debug 2 "roster::getpresence2 rostName=$rostName, jid=$jid, args='$args'"
    
    array set argsArr {
	-type *
    }
    array set argsArr $args

    set mjid [jlib::jidmap $jid]
    jlib::splitjid $mjid jid2 resource
    set result {}
    
    if {$resource == ""} {
	
	# 2-tier jid. Match any resource.
	set arrlist [concat [array get presArr2 [jlib::ESC $mjid],jid] \
                         [array get presArr2 [jlib::ESC $mjid]/*,jid]]
	foreach {key value} $arrlist {
	    set thejid $value
	    set jidresult {}
	    foreach {akey avalue} [array get presArr2 [jlib::ESC $thejid],*] {
		set thekey [string map [list $thejid, ""] $akey]
		lappend jidresult -$thekey $avalue
	    }
	    if {[llength $jidresult]} {
		lappend result $jidresult
	    }
	}
    } else {
	
	# 3-tier jid. Only exact match.
	if {[info exists presArr2($mjid,type)]} {
	    if {[string match $argsArr(-type) $presArr2($mjid,type)]} {
		set result [list [list -jid $jid -type $presArr2($mjid,type)]]
	    }
	} else {
	    set result [list [list -jid $jid -type unavailable]]
	}
    }
    return $result
}

# roster::getgroups --
#
#       Returns the list of groups for this jid, or an empty list if not 
#       exists. If no jid, return a list of all groups existing in this roster.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        (optional).
#       
# Results:
#       a list of groups or empty.

proc roster::getgroups {rostName {jid {}}} {    

    upvar ${rostName}::rostArr rostArr
   
    Debug 2 "roster::getgroups rostName=$rostName, jid='$jid'"
    
    set jid [jlib::jidmap $jid]
    if {[string length $jid]} {
	if {[info exists rostArr($jid,groups)]} {
	    return $rostArr($jid,groups)
	} else {
	    return {}
	}
    } else {
	set rostArr(groups) [lsort -unique $rostArr(groups)]
	return $rostArr(groups)
    }
}

# roster::getname --
#
#       Returns the roster name of this jid.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        
#       
# Results:
#       the roster name or empty.

proc roster::getname {rostName jid} {

    upvar ${rostName}::rostArr rostArr
       
    set jid [jlib::jidmap $jid]
    if {[info exists rostArr($jid,name)]} {
	return $rostArr($jid,name)
    } else {
	return {}
    }
}

# roster::getsubscription --
#
#       Returns the 'subscription' state of this jid.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        
#       
# Results:
#       the 'subscription' state or "none" if no 'subscription' state.

proc roster::getsubscription {rostName jid} {

    upvar ${rostName}::rostArr rostArr
       
    set jid [jlib::jidmap $jid]
    if {[info exists rostArr($jid,subscription)]} {
	return $rostArr($jid,subscription)
    } else {
	return none
    }
}

# roster::getask --
#
#       Returns the 'ask' state of this jid.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        
#       
# Results:
#       the 'ask' state or empty if no 'ask' state.

proc roster::getask {rostName jid} {

    upvar ${rostName}::rostArr rostArr
   
    Debug 2 "roster::getask rostName=$rostName, jid='$jid'"
    
    if {[info exists rostArr($jid,ask)]} {
	return $rostArr($jid,ask)
    } else {
	return {}
    }
}

# roster::getresources --
#
#       Returns a list of all resources for this jid or empty.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        a jid without any resource (jid2).
#       args        ?-type?
#                   -type: return presence for (un)available only.
#       
# Results:
#       a list of all resources for this jid or empty.

proc roster::getresources {rostName jid args} {

    upvar ${rostName}::presArr presArr
   
    Debug 2 "roster::getresources rostName=$rostName, jid='$jid'"
    array set argsArr $args
    
    if {[info exists presArr($jid,res)]} {
	if {[info exists argsArr(-type)]} {
	    
	    # Need to loop through all resources for this jid.
	    set resList {}
	    set type $argsArr(-type)
	    foreach res $presArr($jid,res) {

		# Be sure to handle empty resources as well: '1234@icq.host'
		if {$res== ""} {
		    set jid3 $jid
		} else {
		    set jid3 $jid/$res
		}
		if {[string equal $argsArr(-type) $presArr($jid3,type)]} {
		    lappend resList $res
		}
	    }
	    return $resList
	} else {
	    return $presArr($jid,res)
	}
    } else {
	return {}
    }
}

proc roster::getmatchingjids2 {rostName jid args} {
    
    upvar ${rostName}::presArr2 presArr2
    
    set jidlist {}
    set arrlist [concat [array get presArr2 [jlib::ESC $mjid],jid] \
                     [array get presArr2 [jlib::ESC $mjid]/*,jid]]
    foreach {key value} $arrlist {
	lappend jidlist $value
    }
    return $jidlist
}

# roster::gethighestresource --
#
#       Returns the resource with highest priority for this jid or empty.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        a jid without any resource (jid2).
#       
# Results:
#       a resource for this jid or empty.

proc roster::gethighestresource {rostName jid} {

    upvar ${rostName}::presArr presArr
   
    Debug 2 "roster::gethighestresource rostName=$rostName, jid='$jid'"
    
    set maxres ""
    if {[info exists presArr($jid,res)]} {
	
	# Find the resource corresponding to the highest priority (D=0).
	set maxpri 0
	set maxres [lindex $presArr($jid,res) 0]
	foreach res $presArr($jid,res) {

	    # Be sure to handle empty resources as well: '1234@icq.host'
	    if {$res== ""} {
		set jid3 $jid
	    } else {
		set jid3 $jid/$res
	    }
	    if {[info exists presArr($jid3,priority)]} {
		if {$presArr($jid3,priority) > $maxpri} {
		    set maxres $res
		    set maxpri $presArr($jid3,priority)
		}
	    }
	}
    }
    return $maxres
}

proc roster::getmaxpriorityjid2 {rostName jid} {

    upvar ${rostName}::presArr2 presArr2
   
    Debug 2 "roster::getmaxpriorityjid2 jid='$jid'"
    
    # Find the resource corresponding to the highest priority (D=0).
    set maxjid ""
    set maxpri 0
    foreach jid3 [getmatchingjids2 $rostName $jid] {
	if {[info exists presArr2($jid3,priority)]} {
	    if {$presArr2($jid3,priority) > $maxpri} {
		set maxjid $jid3
		set maxpri $presArr2($jid3,priority)
	    }
	}
    }
    return $jid3
}

# roster::isavailable --
#
#       Returns boolean 0/1. Returns 1 only if presence is equal to available.
#       If 'jid' without resource, return 1 if any is available.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        either 'username$hostname', or 'username$hostname/resource'.
#       
# Results:
#       0/1.

proc roster::isavailable {rostName jid} {

    upvar ${rostName}::presArr presArr
   
    Debug 2 "roster::isavailable rostName=$rostName, jid='$jid'"
        
    set jid [jlib::jidmap $jid]

    # If any resource in jid, we get it here.
    jlib::splitjid $jid jid2 resource

    if {[string length $resource] > 0} {
	if {[info exists presArr($jid2/$resource,type)]} {
	    if {[string equal $presArr($jid2/$resource,type) "available"]} {
		return 1
	    } else {
		return 0
	    }
	} else {
	    return 0
	}
    } else {
	
	# Be sure to allow for 'user@domain' with empty resource.
	foreach key [array names presArr "[jlib::ESC $jid2]*,type"] {
	    if {[string equal $presArr($key) "available"]} {
		return 1
	    }
	}
	return 0
    }
}

proc roster::isavailable2 {rostName jid} {

    upvar ${rostName}::presArr2 presArr2
   
    Debug 2 "roster::isavailable rostName=$rostName, jid='$jid'"
	
    set jid [jlib::jidmap $jid]

    # If any resource in jid, we get it here.
    jlib::splitjid $jid jid2 resource

    if {[string length $resource] > 0} {
	if {[info exists presArr($jid2/$resource,type)]} {
	    if {[string equal $presArr($jid2/$resource,type) "available"]} {
		return 1
	    } else {
		return 0
	    }
	} else {
	    return 0
	}
    } else {
	
	# Be sure to allow for 'user@domain' with empty resource.
	foreach key [array names presArr "[jlib::ESC $jid2]*,type"] {
	    if {[string equal $presArr($key) "available"]} {
		return 1
	    }
	}
	return 0
    }
}

# roster::wasavailable --
#
#       As 'isavailable' but for any "old" former presence state.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        either 'username$hostname', or 'username$hostname/resource'.
#       
# Results:
#       0/1.

proc roster::wasavailable {rostName jid} {

    upvar ${rostName}::oldpresArr oldpresArr
   
    Debug 2 "roster::wasavailable rostName=$rostName, jid='$jid'"
	
    set jid [jlib::jidmap $jid]

    # If any resource in jid, we get it here.
    jlib::splitjid $jid jid2 resource

    if {[string length $resource] > 0} {
	if {[info exists oldpresArr($jid2/$resource,type)]} {
	    if {[string equal $oldpresArr($jid2/$resource,type) "available"]} {
		return 1
	    } else {
		return 0
	    }
	} else {
	    return 0
	}
    } else {
	
	# Be sure to allow for 'user@domain' with empty resource.
	foreach key [array names oldpresArr "[jlib::ESC $jid2]*,type"] {
	    if {[string equal $oldpresArr($key) "available"]} {
		return 1
	    }
	}
	return 0
    }
}

# roster::getx --
#
#       Returns the xml list for this jid's x element with given xml namespace.
#       Returns empty if no matching info.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        any jid
#       xmlns:      the (mandatory) xmlns specifier. Any prefix
#                   http://jabber.org/protocol/ must be stripped off.
#       
# Results:
#       xml list or empty.

proc roster::getx {rostName jid xmlns} {

    upvar ${rostName}::presArr presArr
   
    Debug 2 "roster::getx rostName=$rostName, jid='$jid', xmlns=$xmlns"

    set jid [jlib::jidmap $jid]
    if {[info exists presArr($jid,x,$xmlns)]} {
	return $presArr($jid,x,$xmlns)
    } else {
	return ""
    }
}

# roster::getextras --
#
#       Returns the xml list for this jid's extras element with given xml namespace.
#       Returns empty if no matching info.
#
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        any jid
#       xmlns:      the (mandatory) full xmlns specifier.
#       
# Results:
#       xml list or empty.

proc roster::getextras {rostName jid xmlns} {

    upvar ${rostName}::presArr presArr
   
    set jid [jlib::jidmap $jid]
    if {[info exists presArr($jid,extras,$xmlns)]} {
	return $presArr($jid,extras,$xmlns)
    } else {
	return ""
    }
}

# roster::getcapsattr --
# 
#       Access function for the <c/> caps elements attributes:
#       
#       <presence>
#           <c 
#               xmlns='http://jabber.org/protocol/caps' 
#               node='http://coccinella.sourceforge.net/protocol/caps'
#               ver='0.95.2'
#               ext='ftrans voip_h323 voip_sip'/>
#       </presence>
#       
# Arguments:
#       rostName:   the instance of this roster.
#       jid:        any jid
#       attrname:   
#       
# Results:
#       the value of the attribute or empty

proc roster::getcapsattr {rostName jid attrname} {
    
    upvar jlib::jxmlns jxmlns
    upvar ${rostName}::presArr presArr

    set attr ""
    set jid [jlib::jidmap $jid]
    set xmlnscaps $jxmlns(caps)
    if {[info exists presArr($jid,extras,$xmlnscaps)]} {
	set cElem $presArr($jid,extras,$xmlnscaps)
	set attr [wrapper::getattribute $cElem $attrname]
    }
    return $attr
}

proc roster::Debug {num str} {
    variable rostGlobals
    if {$num <= $rostGlobals(debug)} {
	puts $str
    }
}

#-------------------------------------------------------------------------------
