#  caps.tcl --
#  
#      This file is part of the jabberlib. It handles the internal cache
#      for caps (xmlns='http://jabber.org/protocol/caps') JEP-0115.
#      
#      It maps nodes as 'http://exodus.jabberstudio.org/caps#0.9' etc.
#      to their disco results.
#      
#      It makes also sure to disco only a single unique client of all
#      clients we have obtained presence from according to JEP-0115.
#      
#  Copyright (c) 2005  Mats Bengtsson
#  
# $Id: caps.tcl,v 1.4 2005/06/16 07:10:40 matben Exp $

# UNFINISHED!!!
#      
# EXPERIMENTAL!!!

package provide caps 1.0

namespace eval jlib::caps {}

proc jlib::caps::init {jlibname} {
    
    jlib::presence_register $jlibname available   [namespace current]::presence_cb
    jlib::presence_register $jlibname unavailable [namespace current]::unavail_cb
    
    namespace eval ${jlibname}::caps {

	# The internal cache keps record if the particular client and version
	# discoed.
	variable discoed
	
	variable cache
    }
}

proc jlib::caps::presence_cb {jlibname jid type args} {

    upvar jlib::jxmlns jxmlns
    upvar ${jlibname}::caps::discoed  discoed
    upvar ${jlibname}::caps::cache    cache
    upvar ${jlibname}::caps::capsjids capsjids
    
    set disco [jlib::service::get $jlibname disco]
    if {$disco == ""} {
	return
    }
    #puts "==================jlib::caps::presence_cb jid=$jid, type=$type, $args"
    
    # Find any c element from:
    # <presence>
    #     <c xmlns='http://jabber.org/protocol/caps'
    #         node='http://exodus.jabberstudio.org/caps'
    #         ver='0.9'
    #         ext='tins ftrans xhtml'/>
    # </presence> 
    array set argsArr $args
    if {[info exists argsArr(-extras)]} {
	set cElemList [wrapper::getnamespacefromchilds $argsArr(-extras) "c" \
	  $jxmlns(caps)]
	if {$cElemList != {}} {
	    set cElem [lindex $cElemList 0]
	    set node [wrapper::getattribute $cElem node]
	    set ver  [wrapper::getattribute $cElem ver]
	    set ext  [wrapper::getattribute $cElem ext]
	    
	    set cache(jid,$jid,node) $node
	    set cache(jid,$jid,ver)  $ver
	    set cache(jid,$jid,ext)  $ext
	    
	    # Keep track of all jid <-> node+ver combinations.
	    # The exts may be different for identical node+ver and must be
	    # obtained for individual jids using 'roster getcapsattr'.
	    
	    if {[lsearch $capsjids($node,$ver) $jid] < 0} {
		lappend capsjids($node,$ver) $jid
	    }
	    
	    if {![info exists discoed(ver,$node,$ver)]} {
		array set argsArr $args
		
		$disco send_get info $jid  \
		  [list [namespace current]::disco_cb $jlibname] \
		  -node ${node}#${ver}
		set discoed(ver,$node,$ver) 1
	    }
	}
    }
}

proc jlib::caps::disco_cb {jlibname disconame type from subiq args} {
    
    upvar ${jlibname}::caps::discoed  discoed
    upvar ${jlibname}::caps::cache    cache
    upvar ${jlibname}::caps::capsjids capsjids

    set node [wrapper::getattribute $subiq node]
    #puts "++++++++++++++++++++++++type=$type, from=$from, node=$node, $subiq, $args"
    
    if {$type == "error"} {
	foreach {capsxmlns ver} [split $node #] {break}
	array unset discoed *,$capsxmlns,$ver
    } else {
	set cache(subiq,$from) $subiq
	
	
    }
}

proc jlib::caps::unavail_cb {jlibname jid type args} {

    upvar ${jlibname}::caps::discoed  discoed
    upvar ${jlibname}::caps::cache    cache
    upvar ${jlibname}::caps::capsjids capsjids

    set roster [jlib::getrostername $jlibname]
    set node [$roster getcapsattr $jid node]
    set ver  [$roster getcapsattr $jid ver]
    
    if {[info exists capsjids($node,$ver)]} {
	if {[set ind [lsearch $capsjids($node,$ver) $jid]] >= 0} {
	    set capsjids($node,$ver) [lreplace $capsjids($node,$ver) $ind $ind]
	}
    }
    array unset cache jid,[jlib::ESC $jid],*
}

proc jlib::caps::free {jlibname} {

    upvar ${jlibname}::caps::discoed  discoed
    upvar ${jlibname}::caps::cache    cache
    upvar ${jlibname}::caps::capsjids capsjids

    array unset discoed
    array unset cache
    array unset capsjids

    jlib::presence_deregister $jlibname available   [namespace current]::presence_cb
    jlib::presence_deregister $jlibname unavailable [namespace current]::unavail_cb
}


