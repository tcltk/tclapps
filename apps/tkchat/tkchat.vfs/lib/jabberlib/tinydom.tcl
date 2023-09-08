#  tinydom.tcl ---
#  
#      This file is part of The Coccinella application. It implements 
#      a tiny DOM model which wraps xml int tcl lists.
#
#  Copyright (c) 2003  Mats Bengtsson
#  
#  See the README file for license, bugs etc.
#  
# $Id: tinydom.tcl,v 1.7 2005/02/18 08:32:44 matben Exp $

package require xml

package provide tinydom 0.1

# This is an attempt to make a minimal DOM thing to store xml data as
# an hierarchical list which is better suited to Tcl.

namespace eval tinydom {

    variable xmlobj
    variable level 0
    variable uid 0
    variable cache
}

proc tinydom::parse {xml} {
    variable xmlobj
    variable uid
    variable cache

    set xmlparser [xml::parser]
    $xmlparser configure -reportempty 1   \
      -elementstartcommand  [namespace current]::XmlElementStart     \
      -elementendcommand    [namespace current]::XmlElementEnd       \
      -characterdatacommand [namespace current]::XmlCHdata           \
      -ignorewhitespace     1
    $xmlparser parse $xml
    
    # Store in internal array and return token which is the array index.
    set token tinydom[incr uid]
    set cache($token) $xmlobj(1)
    unset xmlobj
    return $token
}

proc tinydom::XmlElementStart {tagname attrlist args} {
    variable xmlobj
    variable level

    array set argsarr $args
    if {[info exists argsarr(-namespacedecls)]} {
	lappend attrlist xmlns [lindex $argsarr(-namespacedecls) 0]
    }
    set xmlobj([incr level]) [list $tagname $attrlist 0 {} {}]
}

proc tinydom::XmlElementEnd {tagname args} {
    variable xmlobj
    variable level

    if {$level > 1} {
    
	# Insert the child tree in the parent tree.
	XmlAppend [expr $level-1] $xmlobj($level)
    }
    incr level -1
}

proc tinydom::XmlCHdata {chdata} {
    variable xmlobj
    variable level

    set cdata [lindex $xmlobj($level) 3]
    append cdata [xmldecrypt $chdata]
    lset xmlobj($level) 3 $cdata
}

proc tinydom::XmlAppend {plevel childtree} {
    variable xmlobj
    
    # Get child list at parent level (level).
    set childlist [lindex $xmlobj($plevel) 4]
    lappend childlist $childtree
    
    # Build the new parent tree.
    lset xmlobj($plevel) 4 $childlist
}

proc tinydom::xmldecrypt {chdata} {

    foreach from {{\&amp;} {\&lt;} {\&gt;} {\&quot;} {\&apos;}}   \
      to {{\&} < > {"} {'}} {
	regsub -all $from $chdata $to chdata
    }	
    return $chdata
}

proc tinydom::documentElement {token} {
    variable cache
    
    return $cache($token)
}

proc tinydom::tagname {xmllist} {
    
    return [lindex $xmllist 0]
}

proc tinydom::attrlist {xmllist} {
    
    return [lindex $xmllist 1]
}

proc tinydom::chdata {xmllist} {
    
    return [lindex $xmllist 3]
}

proc tinydom::children {xmllist} {
    
    return [lindex $xmllist 4]
}

proc tinydom::cleanup {token} {
    variable cache
    
    unset -nocomplain cache($token)
}

#-------------------------------------------------------------------------------
