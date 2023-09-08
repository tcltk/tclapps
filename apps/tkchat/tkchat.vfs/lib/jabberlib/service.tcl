# service.tcl --
#
#       This is an abstraction layer for two things; the agent/browse/disco
#       protocols, and for the groupchat protocols gc-1.0/conference/muc.
#       All except disco/muc are EOL!
#       
#  Copyright (c) 2004-2005  Mats Bengtsson
#  
# $Id: service.tcl,v 1.17 2005/02/21 07:59:08 matben Exp $
# 
############################# USAGE ############################################
#
#   NAME
#      service - protocol independent methods for groupchats/conference/muc,
#                agents/browse/disco
#                
#   SYNOPSIS
#      jlib::service::init jlibName
#      
#   INSTANCE COMMANDS
#      jlibName service allroomsin
#      jlibName service childs jid
#      jlibName service exitroom room
#      jlibName service isroom jid
#      jlibName service getjidsfor aservice
#      jlibName service gettransportjids aservice
#      jlibName service gettype jid
#      jlibName service hashandnick jid
#      jlibName service hasfeature jid feature (xmlns)
#      jlibName service nick jid
#      jlibName service parent jid
#      jlibName service register type name
#      jlibName service roomparticipants room
#      jlibName service send_getchildren jid cmd
#      jlibName service setgroupchatpriority priorityList
#      jlibName service setgroupchatprotocol jid protocol
#      jlibName service setroomprotocol jid protocol
#      jlibName service unregister type name
# 
# 
#   VARIABLES
#
# serv:	                             
#	serv(gcProtoPriority)      : The groupchat protocol priority list.                             
#	                             
#       serv(gcprot,$jid)          : Map a groupchat service jid to protocol:
#       	                     (gc-1.0|conference|muc)
#
#       serv(prefgcprot,$jid)      : Stores preferred groupchat protocol that
#                                    overrides the priority list.
#     
############################# CHANGES ##########################################
#
#       0.1         first version

package provide service 1.0

namespace eval jlib { }

namespace eval jlib::service {
    
    # This is an abstraction layer for two things; the agent/browse/(disco?)
    # protocols, and for the groupchat protocols gc-1.0/conference/muc.
    
    # Cache the following services in particular.
    variable services {search register groupchat conference muc}    

    # Maintain a priority list of groupchat protocols in decreasing priority.
    # Entries must match: ( gc-1.0 | conference | muc )
    variable groupchatTypeExp {(gc-1.0|conference|muc)}
}

proc jlib::service {jlibname cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    set ans [eval {[namespace current]::service::${cmd} $jlibname} $args]
    return $ans
}

proc jlib::service::init {jlibname} {
    
    upvar ${jlibname}::serv serv

    # Init defaults.
    array set serv {
	agent    1
	browse   0
	disco    0
	muc      0
    }
	    
    # Maintain a priority list of groupchat protocols in decreasing priority.
    # Entries must match: ( gc-1.0 | conference | muc )
    set serv(gcProtoPriority) {muc conference gc-1.0}
}

# jlib::service::register --
# 
#       Let components (browse/disco/muc etc.) register that their services
#       are available.

proc jlib::service::register {jlibname type name} {
    
    upvar ${jlibname}::serv serv

    set serv($type) 1
    set serv($type,name) $name
}

proc jlib::service::unregister {jlibname type} {
    
    upvar ${jlibname}::serv serv

    set serv($type) 0
    array unset serv $type,*
}

proc jlib::service::get {jlibname type} {
    
    upvar ${jlibname}::serv serv
    
    if {$serv($type)} {
	return $serv($type,name)
    } else {
	return ""
    }
}

proc jlib::service::send_getchildren {jlibname jid cmd} {
    
    upvar ${jlibname}::serv serv
    upvar ${jlibname}::locals locals

    # We must have a way to figure out which method to use!!!
    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
	$serv(browse,name) send_get $jid $cmd
    } elseif {$serv(disco) && [$serv(disco,name) isdiscoed items $locals(server)]} {
	$serv(disco,name) send_get items $jid $cmd
    }
}

#-------------------------------------------------------------------------------
#
# A couple of routines that handle the selection of groupchat protocol for
# each groupchat service.
# A groupchat service may support more than a single protocol. For instance,
# the MUC component supports both gc-1.0 and MUC.

# Needs some more verification before using it for a dispatcher.

# jlib::service::setgroupchatpriority --
# 
#       Sets the list if groupchat protocols in decreasing priority.
#       The list contains elements 'gc-1.0', 'conference', 'muc',
#       describing which to pick if multiple options.

proc jlib::service::setgroupchatpriority {jlibname priorityList} {

    variable groupchatTypeExp
    upvar ${jlibname}::serv serv

    foreach prot $priorityList {
	if {![regexp $groupchatTypeExp $prot]} {
	    return -code error "Unrecognized groupchat type \"$prot\""
	}
    }
    set serv(gcProtoPriority) $priorityList
}

# jlib::service::setgroupchatprotocol --
# 
#       Explicitly picks a groupchat protocol to use for a groupchat service.
#       
# Arguments:
#       jlibname
#       jid
#       prot        any of 'gc-1.0', 'conference', 'muc'.
#
# Results:
#       None.

proc jlib::service::setgroupchatprotocol {jlibname jid prot} {

    variable groupchatTypeExp
    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv

    set jid [jlib::jidmap $jid]
    if {![regexp $groupchatTypeExp $prot]} {
	return -code error "Unrecognized groupchat type \"$prot\""
    }
    switch -- $prot {
	gc-1.0 {
	    if {![info exists agent($jid,groupchat)]} {
		return -code error  \
		  "No groupchat agent registered for \"$jid\""
	    }
	}
	conference {
	    if {!$serv(browse)} {
		return -code error \
		  "there is no browse object associated with this jlib"
	    }    
	    set confServicesJids [$serv(browse,name) getconferenceservers]
	    if {[lsearch -exact $confServicesJids $jid] < 0} {
		return -code error \
		  "The jid $jid does not know of any \"conference\" service"
	    }
	}
	muc {
	    if {!$serv(browse)} {
		# This must be changed when disco is coming...
		return -code error \
		  "there is no browse object associated with this jlib"
	    }    
	    if {![$serv(browse,name) hasnamespace $jid  \
	      "http://jabber.org/protocol/muc"]} {
		return -code error \
		  "The jid \"$jid\" does not know of any \"muc\" service"
	    }
	}
    }
    set serv(prefgcprot,$jid) $prot
}

# jlib::service::registergcprotocol --
# 
#       Register (sets) a groupchat service jid according to the priorities
#       presently set. Only called internally!

proc jlib::service::registergcprotocol {jlibname jid gcprot} {

    upvar ${jlibname}::serv serv
    
    Debug 2 "jlib::registergcprotocol jid=$jid, gcprot=$gcprot"
    set jid [jlib::jidmap $jid]
    
    # If we already told jlib to use a groupchat protocol then...
    if {[info exist serv(prefgcprot,$jid)]} {
	return
    }
    
    # Set 'serv(gcprot,$jid)' according to the priority list.
    foreach prot $serv(gcProtoPriority) {
	
	# Do we have registered a groupchat protocol with higher priority?
	if {[info exists serv(gcprot,$jid)] && \
	  [string equal $serv(gcprot,$jid) $prot]} {
	    return
	}
	if {[string equal $prot $gcprot]} {
	    set serv(gcprot,$jid) $prot
	    return
	}	
    }
}

# jlib::service::setroomprotocol --
# 
#       Set the groupchat protocol in use for room. This acts only as a
#       dispatcher for 'service' commands.  
#       Only called internally when entering a room!

proc jlib::service::setroomprotocol {jlibname roomjid protocol} {

    variable groupchatTypeExp
    upvar ${jlibname}::serv serv
    
    set roomjid [jlib::jidmap $roomjid]
    if {![regexp $groupchatTypeExp $protocol]} {
	return -code error "Unrecognized groupchat protocol \"$protocol\""
    }
    set serv(roomprot,$roomjid) $protocol
}

proc jlib::service::isinvestigated {jlibname jid} {
    
    upvar ${jlibname}::serv serv

    # Try to gather only positive results!
    set ans 0
    if {$serv(browse) && [$serv(browse,name) isbrowsed $jid]} {
	set ans 1
    } elseif {$serv(disco) && [$serv(disco,name) isdiscoed items $jid]} {
	set ans 1
    }
    return $ans
}
   
proc jlib::service::parent {jlibname jid} {    

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv

    # ???
    if {$serv(browse) && [$serv(browse,name) isbrowsed $jid]} {
	return [$serv(browse,name) getparentjid $jid]
    } elseif {$serv(disco) && [$serv(disco,name) isdiscoed items $jid]} {
	return [$serv(disco,name) parent $jid]
    } else {
	set jid [jlib::jidmap $jid]
	if {[info exists agent($jid,parent)]} {
	    return $agent($jid,parent)
	} else {
	    return -code error "Parent of \"$jid\" cannot be found"
	}
    }
}

proc jlib::service::childs {jlibname jid} {    

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv

    # ???
    if {$serv(browse) && [$serv(browse,name) isbrowsed $jid]} {
	return [$serv(browse,name) getchilds $jid]
    } elseif {$serv(disco) && [$serv(disco,name) isdiscoed items $jid]} {
	return [$serv(disco,name) children $jid]
    } else {
	set jid [jlib::jidmap $jid]
	if {[info exists agent($jid,childs)]} {
	    set agent($jid,childs) [lsort -unique $agent($jid,childs)]
	    return $agent($jid,childs)
	} else {
	    return -code error "Childs of \"$jid\" cannot be found"
	}
    }
}

# jlib::service::getjidsfor --
#
#       Return a list of jid's that support any of "search", "register",
#       "groupchat". Queries sent to both browser and agent.
#       
#       Problems with groupchat <--> conference Howto?
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       what:       "groupchat", "conference", "muc", "register", "search".
#       
# Results:
#       list of jids supporting this service, possibly empty.

proc jlib::service::getjidsfor {jlibname what} {

    variable services
    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv
    
    if {[lsearch $services $what] < 0} {
	return -code error "\"$what\" is not a recognized service"
    }
    set jids {}
    
    # Browse service if any.
    if {$serv(browse)} {
	set browseNS [$serv(browse,name) getservicesforns jabber:iq:${what}]
	if {[llength $browseNS]} {
	    set jids $browseNS
	}
	
	switch -- $what {
	    groupchat {
		
		# These server components support 'groupchat 1.0' as well.
		# The 'jabber:iq:conference' seems to be lacking in many jabber.xml.
		# Use 'getconferenceservers' as fallback.
		set jids [concat $jids \
		  [$serv(browse,name) getservicesforns jabber:iq:conference]]	    
		set jids [concat $jids [$serv(browse,name) getconferenceservers]]
		set jids [concat $jids [$serv(browse,name) getservicesforns  \
		  "http://jabber.org/protocol/muc"]]
	    }
	    muc {
		set jids [concat $jids [$serv(browse,name) getservicesforns  \
		  "http://jabber.org/protocol/muc"]]
	    }
	}
    }
    
    # Disco
    if {$serv(disco)} {
	set jidsdi [$serv(disco,name) getjidsforfeature jabber:iq:${what}]
	
	switch -- $what {
	    groupchat - muc {
		
		# Rooms also return muc as feature; skip these!
		#set jidsdi [concat $jidsdi [$serv(disco,name) getjidsforfeature \
		#  "http://jabber.org/protocol/muc"]]
		
		set jidsdi [concat $jidsdi [$serv(disco,name) getconferences]]
	    }
	}
	set jids [concat $jids $jidsdi]
    }       
    
    # Agent service if any.
    if {[info exists agent($what)] && [llength $agent($what)]} {
	set agent($what) [lsort -unique $agent($what)]
	set jids [concat $agent($what) $jids]
    }
    return [lsort -unique $jids]
}

proc jlib::service::getconferences {jlibname} {
    
    upvar ${jlibname}::serv serv

    # Try to gather only positive results!
    set jids {}
    if {$serv(browse)} {
	set jids [$serv(browse,name) getconferenceservers]
    }
    if {$serv(disco)} {
	set jids [concat $jids [$serv(disco,name) getconferences]]
    }
    return [lsort -unique $jids]
}

proc jlib::service::hasfeature {jlibname jid xmlns} {

    upvar ${jlibname}::serv serv

    # Try to gather only positive results!
    set ans 0
    if {$serv(browse)} {
	set ans [$serv(browse,name) hasnamespace $jid $xmlns]
    } 
    if {!$ans && $serv(disco) && [$serv(disco,name) isdiscoed info $jid]} {
	set ans [$serv(disco,name) hasfeature $xmlns $jid]
    }
    return $ans
}

# jlib::service::gettransportjids --
#
#       Return a list of jid's that support a specific transport.
#       Queries sent to both browser and agent.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       what:       "*", "jabber", "icq", "msn", "yahoo", "aim",...
#       
# Results:
#       list of jids supporting this service, possibly empty.

proc jlib::service::gettransportjids {jlibname what} {

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv

    set jids {}
    
    # Browse service if any.
    if {$serv(browse)} {
	set jids [concat $jids \
	  [$serv(browse,name) getalljidfortypes "service/$what"]]
    }
    if {$serv(disco)} {
	
	# The Jabber registrar defines the type/subtype for all
	# categories. The actual server is "server/im".
	set jids [concat $jids \
	  [$serv(disco,name) getjidsforcategory "gateway/$what"]]
    }

    # Agent service if any.
    foreach key [array names agent "*,service"] {
	if {[string equal $agent($key) $what] || ($what == "*")} {
	    lappend jids [string map {,service ""} $key]
	}
    }
    return [lsort -unique $jids]
}

# jlib::service::gettype --
# 
#       Returns the 'type/subtype' for this jid if any.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:
#       
# Results:
#       type/subtype, possibly empty.

proc jlib::service::gettype {jlibname jid} {

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv

    set type ""
    
    # Browse service if any. Returns 'service/icq' etc.
    if {$serv(browse)} {
	set type [$serv(browse,name) gettype $jid]
    }
    if {$serv(disco) && [$serv(disco,name) isdiscoed info $jid]} {
	set type [lindex [$serv(disco,name) types $jid] 0]
    }
    set jid [jlib::jidmap $jid]
    if {[info exists agent($jid,service)]} {
	set type "service/$agent($jid,service)"
    }
    return $type
}

# jlib::service::name --
# 
#       Return any name attribute for jid.

proc jlib::service::name {jlibname jid} {    

    upvar ${jlibname}::serv serv
    upvar ${jlibname}::lib lib
    
    # Check if domain name supports the 'groupchat' service.
    set name ""
    # ????????
    if {$serv(browse) && [$serv(browse,name) isbrowsed $jid]} {
	set name [$serv(browse,name) getname $jid]
    }
    if {$serv(disco) && [$serv(disco,name) isdiscoed info $jid]} {
	set name [$serv(disco,name) name $jid]
    }
    return $name
}

# jlib::service::isroom --
# 
#       Try to figure out if the jid is a room.
#       If we've browsed it it's been registered in our browse object.
#       If using agent(s) method, check the agent for this jid

proc jlib::service::isroom {jlibname jid} {    

    upvar ${jlibname}::agent agent
    upvar ${jlibname}::serv serv
    upvar ${jlibname}::locals locals
    
    # Check if domain name supports the 'groupchat' service.
    # disco uses explicit children of conference, and muc cache
    set isroom 0
    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
	set isroom [$serv(browse,name) isroom $jid]
    }
    if {!$isroom && $serv(disco) && [$serv(disco,name) isdiscoed info $locals(server)]} {
	set isroom [$serv(disco,name) isroom $jid]
    }
    if {!$isroom && $serv(muc)} {
	set isroom [$serv(muc,name) isroom $jid]
    }
    if {!$isroom} {
	set isroom [jlib::groupchat::isroom $jlibname $jid]
    }
    if {!$isroom && [regexp {^[^@]+@([^@ ]+)$} $jid match domain]} {
	if {[info exists agent($domain,groupchat)]} {
	    set isroom 1
	}
    }
    return $isroom
}

# jlib::service::nick --
#
#       Return nick name for ANY room participant, or the rooms name
#       if jid is a room.
#       For the browser we return the <name> chdata, but for the
#       groupchat-1.0 protocol we use a scheme to find nick.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        'roomname@conference.jabber.org/nickOrHex' typically,
#                   or just room jid.

proc jlib::service::nick {jlibname jid} {   

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::serv serv

    # All kind of conference components seem to support the old 'gc-1.0'
    # protocol, and we therefore must query our method for entering the room.
    jlib::splitjid $jid room res
        
    # Use fallback here???
    if {![info exists serv(roomprot,$room)]} {
	return $res
	#return -code error "Does not know which protocol to use in $room"
    }
    set nick $res
    if {$res == ""} {
	set nick $jid
    }
    
    switch -- $serv(roomprot,$room) {
	gc-1.0 {
	    
	    # Old-style groupchat just has /nick.
	    if {[regexp {^[^@]+@[^@/]+/(.+)$} $jid match nick]} {
		
		# Else we just use the username. (If room for instance)
	    } elseif {![regexp {^([^@]+)@[^@/]+$} $jid match nick]} {
		set nick $jid
	    }
	}
	muc {
	    
	    # The MUC conference method: nick is always the resource part. 
	    # Rooms lack the */res.
	    if {![regexp {^[^@]+@[^@/]+/(.+)$} $jid match nick]} {
		if {![regexp {^([^@]+)@.+} $jid match nick]} {
		    set nick $jid
		}
	    }
	}	
	conference {
	    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
		
		# Assume that if the login server is browsed we also should query
		# the browse object.
		set nick [$serv(browse,name) getname $jid]
	    }
	}
    }
    return $nick
}

# jlib::service::hashandnick --
#
#       A way to get our OWN three-tier jid and nickname for a given room
#       independent on if 'conference' or 'groupchat' is used.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       room:       'roomname@conference.jabber.org' typically.
#       
# Results:
#       list {kitchen@conf.athlon.se/63264ba6724.. mynickname}

proc jlib::service::hashandnick {jlibname room} {    

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::serv serv

    set room [jlib::jidmap $room]

    # All kind of conference components seem to support the old 'gc-1.0'
    # protocol, and we therefore must query our method for entering the room.
    if {![info exists serv(roomprot,$room)]} {
	return -code error "Does not know which protocol to use in $room"
    }
    set hashandnick [list ${room}/ ""]
    
    switch -- $serv(roomprot,$room) {
	gc-1.0 {
	
	    # Old-style groupchat just has /nick.
	    set nick [[namespace parent]::groupchat::mynick $jlibname $room]
	    set hashandnick [list ${room}/${nick} $nick]   
	} 
	muc {
	    if {$serv(muc)} {
		set nick [$serv(muc,name) mynick $room]
		set hashandnick [list ${room}/${nick} $nick]   
	    }
	} 
	conference {
	    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
		set hashandnick  \
		  [[namespace parent]::conference::hashandnick $jlibname $room]
	    }
	}
    }
    
    return $hashandnick
}

# jlib::service::allroomsin --
# 
# 

proc jlib::service::allroomsin {jlibname} {    

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::gchat gchat
    upvar ${jlibname}::serv serv

    set roomList [concat $gchat(allroomsin) \
      [[namespace parent]::muc::allroomsin $jlibname] \
      [[namespace parent]::conference::allroomsin $jlibname]]
    if {$serv(muc)} {
	set roomList [concat $roomList [$serv(muc,name) allroomsin]]
    }
    return [lsort -unique $roomList]
}

proc jlib::service::roomparticipants {jlibname room} {

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::serv serv
    
    set room [jlib::jidmap $room]
    if {![info exists serv(roomprot,$room)]} {
	return -code error "Does not know which protocol to use in $room"
    }

    set everyone {}
    if {![[namespace current]::isroom $jlibname $room]} {
	return -code error "The jid \"$room\" is not a room"
    }

    switch -- $serv(roomprot,$room) {
	gc-1.0 {
	    set everyone [[namespace parent]::groupchat::participants $jlibname $room]
	} 
	muc {
	    set everyone [$serv(muc,name) participants $room]
	}
	conference {
	    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
		set everyone [$serv(browse,name) getchilds $room]
	    }
	}
    }
    return $everyone
}

proc jlib::service::exitroom {jlibname room} {    

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::serv serv

    set room [jlib::jidmap $room]
    if {![info exists serv(roomprot,$room)]} {
	#return -code error "Does not know which protocol to use in $room"
	# Not sure here???
	set serv(roomprot,$room) "gc-1.0"
    }

    switch -- $serv(roomprot,$room) {
	gc-1.0 {
	    [namespace parent]::groupchat::exit $jlibname $room
	}
	muc {
	    $serv(muc,name) exit $room
	}
	conference {
	    if {$serv(browse) && [$serv(browse,name) isbrowsed $locals(server)]} {
		[namespace parent]::conference::exit $jlibname $room
	    }
	}
    }
}

#-------------------------------------------------------------------------------
