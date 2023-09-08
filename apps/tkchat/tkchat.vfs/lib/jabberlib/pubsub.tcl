#  pubsub.tcl --
#  
#      This file is part of the jabberlib. It contains support code
#      for the pub-sub (xmlns='http://jabber.org/protocol/pubsub') JEP-0060.
#      
#  Copyright (c) 2005  Mats Bengtsson
#  
# $Id: pubsub.tcl,v 1.3 2005/02/21 07:59:08 matben Exp $
# 
############################# USAGE ############################################
#
#   INSTANCE COMMANDS
#      jlibName pubsub affiliations
#      jlibName pubsub create ?-node name ?
#      jlibName pubsub delete
#      jlibName pubsub entities
#      jlibName pubsub entity
#      jlibName pubsub items
#      jlibName pubsub options
#      jlibName pubsub publish
#      jlibName pubsub purge
#      jlibName pubsub retract
#      jlibName pubsub subscribe
#      jlibName pubsub unsubscribe
#
################################################################################

#      UNFINISHED!!!
#      
# EXPERIMENTAL!!!

package provide pubsub 1.0

namespace eval jlib::pubsub {
    
    
}

proc jlib::pubsub {jlibname cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    return [eval {[namespace current]::pubsub::${cmd} $jlibname} $args]
}

proc jlib::pubsub::affiliations {jlibname jid node args} {
    
    upvar jlib::jxmlns jxmlns
    
    set opts [list -to $jid]
    set subtags [list [wrapper::createtag affiliations]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname get $xmllist} $opts $args
}

# jlib::pubsub::create --
# 
#       Create a new pubsub node.

proc jlib::pubsub::create {jlibname jid args} {
    
    upvar jlib::jxmlns jxmlns
    
    set attrlist {}
    set opts [list -to $jid]
    set configure 0
    foreach {key value} $args {
	set name [string trimleft $key -]
	
	switch -- $key {
	    -configure {
		set configure $value
	    }
	    -node {
		lappend attrlist $name $value
	    }
	    default {
		lappend opts $name $value
	    }
	}
    }
    set subtags [list [wrapper::createtag create -attrlist $attrlist]]
    if {$configure} {
	lappend subtags [wrapper::createtag configure]
    }
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts
}

proc jlib::pubsub::delete {jlibname jid node args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set subtags [list [wrapper::createtag delete -attrlist [list node $node]]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts $args
}

proc jlib::pubsub::entities {jlibname type jid node args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set entities {}
    foreach {key value} $args {
	set name [string trimleft $key -]
	
	switch -- $key {
	    -entities {
		set entities $value
	    }
	    default {
		lappend opts $name $value
	    }
	}
    }
    set subtags [list [wrapper::createtag entities \
      -attrlist [list node $node] -subtags $entities]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname $type $xmllist} $opts $args
}

proc jlib::pubsub::entity {jlibname jid node args} {
    
    upvar jlib::jxmlns jxmlns

    

}

proc jlib::pubsub::items {jlibname jid node subid args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set attrlist [list node $node subid $subid]
    set itemids {}
    foreach {key value} $args {
	set name [string trimleft $key -]
	
	switch -- $key {
	    -max_items {
		lappend attrlist $name $value
	    }
	    -itemids {
		set itemids $value
	    }
	    default {
		lappend opts $name $value
	    }
	}
    }
    set items {}
    foreach id $itemids {
	lappend items [wrapper::createtag item -attrlist [list id $id]]
    }
    set subtags [list [wrapper::createtag items  \
      -attrlist $attrlist -subtags $items]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname get $xmllist} $opts
}

proc jlib::pubsub::options {jlibname jid node subscribejid subid args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set xdata {}
    foreach {key value} $args {
	set name [string trimleft $key -]
	
	switch -- $key {
	    -xdata {
		set xdata $value
	    }
	    default {
		lappend opts $name $value
	    }
	}
    }
    set subtags [list [wrapper::createtag options \
      -attrlist [list node $node jid $subscribejid subid $subid]]]
    if {$xdata != {}} {
	lappend subtags $xdata
    }
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname get $xmllist} $opts
}

# jlib::pubsub::publish --
# 
#       Publish an item to a node.

proc jlib::pubsub::publish {jlibname jid node args} {

    upvar jlib::jxmlns jxmlns
    
    set opts [list -to $jid]
    set items {}
    foreach {key value} $args {
	set name [string trimleft $key -]
	
	switch -- $key {
	    -items {
		set items $value
	    }
	    default {
		lappend opts $name $value
	    }
	}
    }
    set subtags [list [wrapper::createtag create \
      -attrlist [list node $node] -subtags $items]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts
}

proc jlib::pubsub::purge {jlibname jid node args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set subtags [list [wrapper::createtag purge -attrlist [list node $node]]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts $args
}

# jlib::pubsub::retract --
# 
#       Delete an item from a node.

proc jlib::pubsub::retract {jlibname jid node itemids args} {
    
    upvar jlib::jxmlns jxmlns

    set opts [list -to $jid]
    set items {}
    foreach id $itemids {
	lappend items [wrapper::createtag item -attrlist [list id $id]]
    }
    set subtags [list [wrapper::createtag retract \
      -attrlist [list node $node] -subtags $items]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts $args
}

# jlib::pubsub::subscribe --
# 
#       Subscribe to a specific node with the 'subscribejid' as the delivery
#       address.

proc jlib::pubsub::subscribe {jlibname jid node subscribejid args} {
    
    upvar jlib::jxmlns jxmlns
    
    set opts [list -to $jid]
    set subtags [list [wrapper::createtag subscribe \
      -attrlist [list node $node jid $subscribejid]]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts $args
}

proc jlib::pubsub::unsubscribe {jlibname jid node subscribejid subid args} {
    
    upvar jlib::jxmlns jxmlns
    
    set opts [list -to $jid]
    set subtags [list [wrapper::createtag unsubscribe \
      -attrlist [list node $node jid $subscribejid subid $subid]]]
    set xmllist [list [wrapper::createtag pubsub \
      -attrlist [list xmlns $jxmlns(pubsub)] -subtags $subtags]]
    eval {jlib::send_iq $jlibname set $xmllist} $opts $args
}



