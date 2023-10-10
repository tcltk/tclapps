#  disco.tcl --
#  
#      This file is part of the jabberlib.
#      
#  Copyright (c) 2004-2005  Mats Bengtsson
#  
# $Id: disco.tcl,v 1.22 2005/06/16 07:10:40 matben Exp $
# 
############################# USAGE ############################################
#
#   NAME
#      disco - convenience command library for the disco part of XMPP.
#      
#   SYNOPSIS
#      disco::new jlibName ?-opt value ...?
#
#   OPTIONS
#	-command tclProc
#	
#   INSTANCE COMMANDS
#      discoName children jid
#      discoName send_get discotype jid callbackProc ?-opt value ...?
#      discoName isdiscoed discotype jid ?node?
#      discoName get discotype key jid ?node?
#      discoName getallcategories pattern
#      discoName getconferences
#      discoName getflatlist jid ?node?
#      discoName getjidsforcategory pattern
#      discoName getjidsforfeature feature
#      discoName features jid ?node?
#      discoName hasfeature feature jid ?node?
#      discoName isroom jid
#      discoName iscategorytype category/type jid ?node?
#      discoName name jid ?node?
#      discoName nodes jid ?node?
#      discoName parent jid
#      discoName parentitemslist jid ?node?
#      discoName parentnode jid node
#      discoName parentnodes jid node
#      discoName parents jid
#      discoName splititemlist list
#      discoName types jid ?node?
#      discoName reset ?jid ?node??
#      
#      where discotype = (items|info)
#      
############################# CHANGES ##########################################
#
#       0.1         first version
#       050208      major rewrite with api changes

# Structures:
#       items(jid,parent)           the parent jid
#       items(jid,parents)          list of parents jid
#       items(jid,node,children)    list of any children jids
#       
#       items(jid,node,pnode)       the parent node; empty if top node
#       items(jid,node,pnodes)      list of parent nodes
#       items(jid,node,nodes)       list of any sub nodes
#       
#       jid must always be nonempty while node may be empty.
#       
#       rooms(jid,node)             exists if children of 'conference'

# NEW: In order to manage the complex jid/node structure it is best to
#      keep an internal structure always using a pair jid&node. 
#      As array index: ($jid,$node,..) or list of childs:
#      {{$jid1 $node1} {$jid2 $node2} ..} where any of jid or node can be
#      empty but not both.
#       
#      This reflects the disco xml structure (node can be empty):
#      
#      jid node
#            jid node
#            jid node
#            ...

package require jlib

package provide disco 0.1

namespace eval disco {
    
    # Globals same for all instances of this jlib.
    variable debug 0
    if {[info exists ::debugLevel] && ($::debugLevel > 1) && ($debug == 0)} {
	set debug 2
    }
        
    variable version 0.1
    
    # Running number.
    variable uid 0
    
    # Common xml namespaces.
    variable xmlns
    array set xmlns {
	disco           http://jabber.org/protocol/disco 
	items           http://jabber.org/protocol/disco#items 
	info            http://jabber.org/protocol/disco#info
    }
}

# disco::new --
# 
#       Creates a new instance of the disco object.
#       
# Arguments:
#       jlibname:     name of existing jabberlib instance
#       args:         -command procName
# 
# Results:
#       namespaced instance command

proc disco::new {jlibname args} {
    
    variable uid
    variable xmlns
    variable disco2jlib
    
    # Generate unique command token for this disco instance.
    # Fully qualified!
    set disconame [namespace current]::[incr uid]
    
    # Instance specific arrays.
    namespace eval $disconame {
	variable items
	variable info
	variable rooms
	variable priv
    }
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    upvar ${disconame}::rooms rooms
    upvar ${disconame}::priv  priv

    foreach {key value} $args {
	switch -- $key {
	    -command {
		set priv(cmd) $value
	    }
	    default {
		return -code error "unrecognized option \"$key\" for disco::new"
	    }
	}
    }
    set disco2jlib($disconame) $jlibname
    
    # Register service.
    $jlibname service register disco $disconame
    
    # Register some standard iq handlers that is handled internally.
    $jlibname iq_register get $xmlns(items)  \
      [list [namespace current]::handle_get $disconame items]
    $jlibname iq_register get $xmlns(info)   \
      [list [namespace current]::handle_get $disconame info]
    
    # Create the actual disco instance procedure.
    proc $disconame {cmd args}  \
      "eval disco::cmdproc {$disconame} \$cmd \$args"
    
    set info(conferences) {}
    
    return $disconame
}

# disco::cmdproc --
#
#       Just dispatches the command to the right procedure.
#
# Arguments:
#       disconame:  the instance of this disco.
#       cmd:        
#       args:       all args to the cmd procedure.
#       
# Results:
#       none.

proc disco::cmdproc {disconame cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    return [eval $cmd $disconame $args]
}

# disco::send_get --
#
#       Sends a get request within the disco namespace.
#
# Arguments:
#       disconame:  the instance of this disco.
#       type:       items|info
#       jid:        to jid
#       cmd:        callback tcl proc        
#       args:       -node chdata
#       
# Results:
#       none.

proc disco::send_get {disconame type jid cmd args} {
    
    variable xmlns
    variable disco2jlib
    
    array set argsArr $args
    set opts {}
    if {[info exists argsArr(-node)]} {
	lappend opts -node $argsArr(-node)
    }
    
    eval {$disco2jlib($disconame) iq_get $xmlns($type) -to $jid  \
      -command [list [namespace current]::parse_get $disconame $type $jid $cmd]} \
      $opts
}

# disco::parse_get --
# 
#       Fills in the internal state arrays, and invokes any callback.

proc disco::parse_get {disconame discotype from cmd jlibname type subiq args} {
    
    variable disco2jlib
    upvar ${disconame}::items items
    upvar ${disconame}::info  info

    # We need to use both jid and any node for addressing since
    # each item may have identical jid's but different node's.

    # Do STRINGPREP.
    set from [jlib::jidmap $from]
    
    if {[string equal $type "error"]} {
	# Empty.
    } else {
	switch -- $discotype {
	    items {
		parse_get_items $disconame $from $subiq
	    }
	    info {
		parse_get_info $disconame $from $subiq
	    }
	}
    }
    
    # Invoke callback for this get.
    uplevel #0 $cmd [list $disconame $type $from $subiq] $args
}

# disco::parse_get_items --
# 
#       Fills the internal records with this disco items query result.
#       There are four parent-childs combinations:
#       
#         (0)   jid1
#                   jid         jid1 != jid
#               
#         (1)   jid1
#                   jid1+node   jid equal
#               
#         (2)   jid1+node1
#                   jid         jid1 != jid
#               
#         (3)   jid1+node1
#                   jid+node    jid1 != jid
#        
#        Typical xml:
#        <iq type='result' ...>
#             <query xmlns='http://jabber.org/protocol/disco#items' 
#                    node='music'>
#                 <item jid='catalog.shakespeare.lit'
#                       node='music/A'/> 
#                 ...
#
#   Any of the following scenarios is perfectly acceptable: 
#
#   (0) Upon querying an entity (JID1) for items, one receives a list of items 
#       that can be addressed as JIDs; each associated item has its own JID, 
#       but no such JID equals JID1. 
#
#   (1) Upon querying an entity (JID1) for items, one receives a list of items 
#       that cannot be addressed as JIDs; each associated item has its own 
#       JID+node, where each JID equals JID1 and each NodeID is unique. 
#
#   (2) Upon querying an entity (JID1+NodeID1) for items, one receives a list 
#       of items that can be addressed as JIDs; each associated item has its 
#       own JID, but no such JID equals JID1. 
#
#   (3) Upon querying an entity (JID1+NodeID1) for items, one receives a list 
#       of items that cannot be addressed as JIDs; each associated item has 
#       its own JID+node, but no such JID equals JID1 and each NodeID is
#       unique in the context of the associated JID. 
#       
#   In addition, the results MAY also be mixed, so that a query to a JID or a 
#   JID+node could yield both (1) items that are addressed as JIDs and (2) 
#   items that are addressed as JID+node combinations. 

proc disco::parse_get_items {disconame from subiq} {
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    upvar ${disconame}::rooms rooms

    # Parents node if any.
    set pnode [wrapper::getattribute $subiq "node"]
    set pitem [list $from $pnode]
    
    set items($from,$pnode,xml) $subiq
    unset -nocomplain items($from,$pnode,children) items($from,$pnode,nodes)
    unset -nocomplain items($from,$pnode,childs2)
    
    # This is perhaps not a robust way.
    if {![info exists items($from,parent)]} {
	set items($from,parent)  {}
	set items($from,parents) {}
    }
    if {![info exists items($from,$pnode,parent2)]} {
	set items($from,$pnode,parent2)  {}
	set items($from,$pnode,parents2) {}
    }
    
    # Cache children of category='conference' as rooms.
    if {[lsearch $info(conferences) $from] >= 0} {
	set isrooms 1
    } else {
	set isrooms 0
    }
    
    foreach c [wrapper::getchildren $subiq] {
	if {![string equal [wrapper::gettag $c] "item"]} {
	    continue
	}
	unset -nocomplain attr
	array set attr [wrapper::getattrlist $c]
	
	# jid is a required attribute!
	set jid [jlib::jidmap $attr(jid)]
	set node ""
	
	# Children--->
	# Only 'childs2' gives the full picture.
	if {$jid ne $from} {
	    lappend items($from,$pnode,children) $jid
	}
	if {[info exists attr(node)]} {
	    
	    # Not two nodes of a jid may be identical. Beware for infinite loops!
	    # We only do some rudimentary check.
	    set node $attr(node)
	    if {[string equal $pnode $node]} {
		continue
	    }
	    lappend items($from,$pnode,nodes) $node	    
	}
	lappend items($from,$pnode,childs2) [list $jid $node]
	
	# Parents--->
	# Case (2) above is particularly problematic since an entity jid's
	# position in the disco tree is not unique.
	if {$node == ""} {
	    
	    # This is a jid.
	    if {$pnode == ""} {

		# case (0):
		set xcase 0
		set items($jid,parent) $from
		set items($jid,parents) [concat $items($from,parents) \
		  [list $from]]
	    } else {

		# case (2):
		# The owning entity is required to describe this item. BAD.
		set xcase 2
		set items(2,$from,$pnode,$jid,$node,parent) $from
		set items(2,$from,$pnode,$jid,$node,parents) $from
	    }
	} else {
	    
	    # This is a node. case (1) or (3):
	    # Init if the first one.
	    if {$pnode == ""} {
		set xcase 3
		set items($jid,$node,pnode) {}
		set items($jid,$node,pnodes) {}
	    } else {
		set xcase 1
		set items($jid,$node,pnode) $pnode
		set items($jid,$node,pnodes) [concat $items($jid,$pnode,pnodes) \
		  [list $pnode]]
	    }
	}
	if {$xcase == 2} {

	    # The owning entity is required to describe this item. BAD.
	    set items(2,$from,$pnode,$jid,$node,parent2) $pitem
	    set items(2,$from,$pnode,$jid,$node,parents2) 7
	    [concat $items($from,$pnode,parents2) [list $pitem]]
	} else {
	    set items($jid,$node,parent2) $pitem
	    set items($jid,$node,parents2) [concat $items($from,$pnode,parents2) \
	      [list $pitem]]
	}
	
	# Cache the optional attributes.
	# Any {jid node} must have identical attributes and childrens.
	foreach key {name action} {
	    if {[info exists attr($key)]} {
		set items($jid,$node,$key) $attr($key)
	    }
	}
	if {$isrooms} {
	    set rooms($jid,$node) 1
	}
    }	
}

# disco::parse_get_info --
# 
#       Fills the internal records with this disco info query result.

proc disco::parse_get_info {disconame from subiq} {
    variable disco2jlib
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    upvar ${disconame}::rooms rooms

    set node [wrapper::getattribute $subiq "node"]

    array unset info [jlib::ESC $from],[jlib::ESC $node],*
    set info($from,$node,xml) $subiq
    set isconference 0
    
    foreach c [wrapper::getchildren $subiq] {
	unset -nocomplain attr
	array set attr [wrapper::getattrlist $c]
	
	# There can be one or many of each 'identity' and 'feature'.
	switch -- [wrapper::gettag $c] {
	    identity {
		
		# Each <identity/> element MUST possess 'category' and 
		# 'type' attributes. (category/type)
		# Each identity element SHOULD have the same name value.
		# 
		# JEP 0030:
		# If the hierarchy category is used, every node in the 
		# hierarchy MUST be identified as either a branch or a leaf; 
		# however, since a node MAY have multiple identities, any given 
		# node MAY also possess an identity other than 
		# "hierarchy/branch" or "hierarchy/leaf". 

		set category $attr(category)
		set ctype    $attr(type)
		set name     ""
		if {[info exists attr(name)]} {
		    set name $attr(name)
		}			
		set info($from,$node,name) $name
		set cattype $category/$ctype
		lappend info($from,$node,cattypes) $cattype
		lappend info($cattype,typelist) $from
		set info($cattype,typelist) \
		  [lsort -unique $info($cattype,typelist)]
		
		if {![string match *@* $from]} {
		    
		    switch -- $category {
			conference {
			    lappend info(conferences) $from
			    set isconference 1
			}
		    }
		}
	    }
	    feature {
		set feature $attr(var)
		lappend info($from,$node,features) $feature
		lappend info($feature,featurelist) $from
		
		# Register any groupchat protocol with jlib.
		# Note that each room also returns gc features; skip!
		if {![string match *@* $from]} {
		    
		    switch -- $feature {
			"http://jabber.org/protocol/muc" {
			    $disco2jlib($disconame) service \
			      registergcprotocol $from "muc"
			}
			"jabber:iq:conference" {
			    $disco2jlib($disconame) service \
			      registergcprotocol $from "conference"
			}
			"gc-1.0" {
			    $disco2jlib($disconame) service \
			      registergcprotocol $from "gc-1.0"
			}
		    }
		}
	    }
	}
    }
    
    # If this is a conference be sure to cache any children as rooms.
    if {$isconference && [info exists items($from,,children)]} {
	foreach c [$items($from,,children)] {
	    set rooms($c,) 1
	}
    }
}

proc disco::isdiscoed {disconame discotype jid {node ""}} {
    
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    
    set jid [jlib::jidmap $jid]

    switch -- $discotype {
	items {
	    return [info exists items($jid,$node,xml)]
	}
	info {
	    return [info exists info($jid,$node,xml)]
	}
    }
}

proc disco::get {disconame discotype key jid {node ""}} {
    
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    
    set jid [jlib::jidmap $jid]
 
    switch -- $discotype {
	items {
	    if {[info exists items($jid,$node,$key)]} {
		return $items($jid,$node,$key)
	    }
	}
	info {
	    if {[info exists info($jid,$node,$key)]} {
		return $info($jid,$node,$key)
	    }
	}
    }
    return ""
}

# Both the items and the info elements may have name attributes! Related???

#       The login servers jid name attribute is not returned via any items
#       element; only via info/identity element. 
#       

proc disco::name {disconame jid {node ""}} {
    
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    
    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,name)]} {
	return $items($jid,$node,name)
    } elseif {[info exists info($jid,$node,name)]} {
	return $info($jid,$node,name)
    } else {
	return {}
    }
}

# disco::features --
# 
#       Returns the var attributes of all feature elements for this jid/node.

proc disco::features {disconame jid {node ""}} {
    
    upvar ${disconame}::info info
    
    set jid [jlib::jidmap $jid]
    if {[info exists info($jid,$node,features)]} {
	return $info($jid,$node,features)
    } else {
	return {}
    }
}

# disco::hasfeature --
# 
#       Returns 1 if the jid/node has the specified feature var.

proc disco::hasfeature {disconame feature jid {node ""}} {
    
    upvar ${disconame}::info info

    set jid [jlib::jidmap $jid]
    if {[info exists info($jid,$node,features)]} {
	return [expr [lsearch $info($jid,$node,features) $feature] < 0 ? 0 : 1]
    } else {
	return 0
    }
}

# disco::types --
# 
#       Returns a list of all category/types of this jid/node.

proc disco::types {disconame jid {node ""}} {
    
    upvar ${disconame}::info info
    
    set jid [jlib::jidmap $jid]
    if {[info exists info($jid,$node,cattypes)]} {
	return $info($jid,$node,cattypes)
    } else {
	return {}
    }
}

# disco::iscategorytype --
# 
#       Search for any matching feature var glob pattern.

proc disco::iscategorytype {disconame cattype jid {node ""}} {
    
    upvar ${disconame}::info info
    
    set jid [jlib::jidmap $jid]
    if {[info exists info($jid,$node,cattypes)]} {
	if {[lsearch -glob $info($jid,$node,cattypes) $cattype] >= 0} {
	    return 1
	} else {
	    return 0
	}
    } else {
	return 0
    }
}

# disco::getjidsforfeature --
# 
#       Returns a list of all jids that support the specified feature.

proc disco::getjidsforfeature {disconame feature} {
    
    upvar ${disconame}::info info
    
    if {[info exists info($feature,featurelist)]} {
	set info($feature,featurelist) [lsort -unique $info($feature,featurelist)]
	return $info($feature,featurelist)
    } else {
	return {}
    }
}

# disco::getjidsforcategory --
#
#       Returns all jids that match the glob pattern category/type.
#       
# Arguments:
#       disconame:    the instance of this disco instance.
#       catpattern:   a global pattern of jid type/subtype (gateway/*).
#
# Results:
#       List of jid's matching the type pattern. nodes???

proc disco::getjidsforcategory {disconame catpattern} {
    
    upvar ${disconame}::info info
    
    set jidlist {}    
    foreach {key jids} [array get info "${catpattern},typelist"] {
	set jidlist [concat $jidlist $jids]
    }
    return $jidlist
}    

# disco::getallcategories --
#
#       Returns all categories that match the glob pattern catpattern.
#       
# Arguments:
#       disconame:    the instance of this disco instance.
#       catpattern:   a global pattern of jid type/subtype (gateway/*).
#
# Results:
#       List of types matching the category/type pattern.

proc disco::getallcategories {disconame catpattern} {    
    
    upvar ${disconame}::info info
    
    set ans {}
    foreach {key catlist} [array get info *,cattypes] {
	lappend ans $catlist
    }
    return [lsort -unique $ans]
}    

proc disco::getconferences {disconame} {
    
    upvar ${disconame}::info info

    return $info(conferences)
}

# disco::isroom --
# 
#       Room or not? The problem is that some components, notably some
#       msn gateways, have multiple categories, gateway and conference. BAD!
#       We therefore use a specific 'rooms' array.

proc disco::isroom {disconame jid} {

    upvar ${disconame}::rooms rooms
    
    if {[info exists rooms($jid,)]} {
	return 1
    } else {
	return 0
    }
}

proc disco::isroomOLD {disconame jid} {
    
    upvar ${disconame}::info  info
    
    set jid [jlib::jidmap $jid]

    # Use the form of the jid to get the service.
    jlib::splitjidex $jid node service res
    if {($node != "") && ($service != "") && ($res == "")} {
	if {[lsearch -exact $info(conferences) $service] >= 0} {
	    return 1
	} else {
	    return 0
	}
    } else {
	return 0
    }
}

# disco::children --
# 
#       Returns a list of all child jids of this jid.

proc disco::children {disconame jid} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,,children)]} {
	return $items($jid,,children)
    } else {
	return {}
    }
}

proc disco::childs2 {disconame jid {node ""}} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,childs2)]} {
	return $items($jid,$node,childs2)
    } else {
	return {}
    }
}

# Note: jid and node childs can be mixed!

proc disco::childrenlist {disconame jid {node ""}} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    set clist {}
    if {$node == ""} {
	if {[info exists items($jid,,children)]} {
	    set clist $items($jid,,children)
	}
	if {[info exists items($jid,$node,nodes)]} {
	    set clist [concat $clist $items($jid,$node,nodes)]
	}
	return $clist
    } else {
	if {[info exists items($jid,$node,nodes)]} {
	    set clist $items($jid,$node,nodes)
	}
	return $clist
    }
}

# disco::nodes --
# 
#       Returns a list of child nodes of this jid|node.

proc disco::nodes {disconame jid {node ""}} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,nodes)]} {
	return $items($jid,$node,nodes)
    } else {
	return {}
    }
}

# disco::parent --
# 
#       Returns the parent of the jid. Empty if no parent known.

proc disco::parent {disconame jid} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,parent)]} {
	return $items($jid,parent)
    } else {
	return {}
    }
}

proc disco::parent2 {disconame jid node} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,parent2)]} {
	return $items($jid,$node,parent2)
    } else {
	return {}
    }
}

# disco::parents --
# 
#       Returns a list of parents of the jid. Empty if no parent known.

proc disco::parents {disconame jid} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,parents)]} {
	return $items($jid,parents)
    } else {
	return {}
    }
}

# disco::parents2 --
# 
#       Returns the {{jid node} {jid node} ...} parents.

proc disco::parents2 {disconame jid {node ""}} {
    
    upvar ${disconame}::items items

    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,parents2)]} {
	return $items($jid,$node,parents2)
    } else {
	return {}
    }
}

# This wont distinguish between top node and nonexisting!!!

# disco::parentnode, parentnodes --
# 
#       Same as parent(s) but for nodes.

proc disco::parentnode {disconame jid node} {
    
    upvar ${disconame}::items items
    
    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,pnode)]} {
	return $items($jid,$node,pnode)
    } else {
	return {}
    }
}

proc disco::parentnodes {disconame jid node} {
    
    upvar ${disconame}::items items
    
    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,$node,pnodes)]} {
	return $items($jid,$node,pnodes)
    } else {
	return {}
    }
}

# disco::parentitemslist --
# 
#       Return a "flat list" of jids and nodes: {jid jid... ?node node...?}
#       of the parent of the jid/node combination.

proc disco::parentitemslist {disconame jid {node ""}} {
    
    upvar ${disconame}::items items
    
    set jid [jlib::jidmap $jid]
    if {[info exists items($jid,parents)]} {
	set plist $items($jid,parents)
	if {$node != ""} {
	    lappend plist $jid
	    
	    # The echo component messes up the jid's and therefore this check.
	    if {[info exists items($jid,$node,pnodes)]} {
		set plist [concat $plist $items($jid,$node,pnodes)]
	    }
	}
	return $plist
    } else {
	return {}
    }
}

# disco::getflatlist --
# 
#       Same as parentitemslist but includes the jid/node specified.

proc disco::getflatlist {disconame jid {node ""}} {
    
    set plist [parentitemslist $disconame $jid $node]
    if {$node == ""} {
	set v [concat $plist [list $jid]]
    } else {
	set v [concat $plist [list $node]]
    }
    return $v
}

# disco::flatentitylist --
# 
#       Flattens an entity list with each element {jid node} and picks
#       the most relevant of jid or node.
#       Not guarenteed to be unique!

proc disco::flatentitylist {disconame elist} {
    
    set flat {}
    set pjid ""
    foreach item $elist {
	set jid  [lindex $item 0]
	set node [lindex $item 1]
	if {$node != ""} {
	    lappend flat $node
	} else {
	    lappend flat $jid
	}
	set pjid $jid
    }
    return $flat
}

# disco::getstructfromflat --
# 
#       Inverse to 'flatentitylist'. Not unique result if any jid = node!

proc disco::getstructfromflat {disconame flat} {
    
    upvar ${disconame}::items items
       
    # Assume first is jid.
    set jid [lindex $flat 0]
    set node ""
    set elist [list [list $jid $node]]
    
    # First one already matched.
    foreach e [lrange $flat 1 end] {
	puts "e=$e"
	if {![info exists items($jid,$node,childs2)]} {
	    return -code error "inconsistent entity list \"$flat\""
	}
	set clist $items($jid,$node,childs2)
	puts "\t clist=$clist"
	
	# Search for any node first.
	if {[set ind [lsearch -regexp $clist " ${e}$"]] >= 0} {
	    puts "\t node: ind=$ind"
	    set item [lindex $clist $ind]
	} elseif {[set ind [lsearch -regexp $clist "^$e "]] >= 0} {
	    puts "\t jid: ind=$ind"
	    set item [lindex $clist $ind]
	} else {
	    return -code error "inconsistent entity list \"$flat\""
	}
	puts "\t item=$item"
	lappend elist $item
	set jid  [lindex $item 0]
	set node [lindex $item 1]
    }
    return $elist
}

# disco::splititemlist --
# 
#       Takes a flatlist of jids/nodes and returns {{jid jid...} {node...}}

proc disco::splititemlist {disconame plist} {
    
    upvar ${disconame}::items items
    
    set jidlist  {}
    set nodelist {}
    foreach a $plist {
	if {[info exists items($a,parent)]} {
	    lappend jidlist $a
	} else {
	    lappend nodelist $a
	}
    }    
    return [list $jidlist $nodelist]
}

proc disco::handle_get {disconame discotype jlibname from subiq args} {
    
    upvar ${disconame}::priv priv

    set ishandled 0
    if {[info exists priv(cmd)]} {
	set ishandled [uplevel #0 $priv(cmd)  \
	  [list $disconame $discotype $from $subiq] $args]
    }
    return $ishandled
}

# disco::reset --
# 
#       Clear this particular jid and all its children.

proc disco::reset {disconame {jid ""} {node ""}} {

    upvar ${disconame}::items items

    if {($jid == "") && ($node == "")} {
	array unset items
	array unset info
	array unset rooms
    } else {
	set jid [jlib::jidmap $jid]	
	
	# Can be problems with this (ICQ) ???
	if {[info exists items($jid,,children)]} {
	    foreach child $items($jid,,children) {
		ResetJid $disconame $child
	    }
	}
	ResetJid $disconame $jid
    }
}

# disco::ResetJid --
# 
#       Clear only this particular jid.

proc disco::ResetJid {disconame jid} {
    
    upvar ${disconame}::items items
    upvar ${disconame}::info  info
    upvar ${disconame}::rooms rooms

    if {$jid == {}} {
	unset -nocomplain items info rooms
	set info(conferences) {}
    } else {
	
	# Keep parents!
	if {[info exists items($jid,parent)]} {
	    set parent $items($jid,parent)
	}
	if {[info exists items($jid,parents)]} {
	    set parents $items($jid,parents)
	}
	
	array unset items [jlib::ESC $jid],*
	array unset info  [jlib::ESC $jid],*
	array unset rooms [jlib::ESC $jid],*
	
	# Add back parent(s).
	if {[info exists parent]} {
	    set items($jid,parent) $parent
	}
	if {[info exists parents]} {
	    set items($jid,parents) $parents
	}
	
	# Rest.
	foreach {key value} [array get info "*,typelist"] {
	    set info($key) [lsearch -all -not -inline -exact $value $jid]
	}
	foreach {key value} [array get info "*,featurelist"] {
	    set info($key) [lsearch -all -not -inline -exact $value $jid]
	}
    }
}

proc disco::Debug {num str} {
    variable debug
    if {$num <= $debug} {
	puts $str
    }
}

#-------------------------------------------------------------------------------
