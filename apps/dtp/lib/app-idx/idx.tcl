# -*- tcl -*-
# Generation - Indices
# ------------------------------------------------------

package require tools
package require optchecker
package require meta

namespace eval ::app-idx {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-idx::help(cmdline) {

[call [cmd {@appname@}] [method idx] [opt [arg options...]] [arg meta]]

This method converts the information given to us via file [arg meta]
into an index. The output of this subcommand is written to
[const stdout] and will be in the [syscmd docidx] format.

[nl]

It is expected that the contents of [arg meta] are in the format
returned by the subcommand [method meta], modulo formatting which does
not change the semantics of the output.

[nl]

The generation of the index can be influenced by the options listed
below.

[list_begin definitions]
[lst_item "[option -title] [arg text]"]

Provides the [arg text] used as the label of [cmd index_begin].
Defaults to the empty string.

[lst_item "[option -desc] [arg text]"]

Provides the [arg text] used as the descriptive title of the index in
[cmd index_begin].  Defaults to [const "Keyword index"].

[list_end]
}

proc ::app-idx::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}


# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-idx::run {argv} {
    set errstring "wrong#args: idx ?-title text? ?-desc text? metafile"

    if {[llength $argv] < 1} {tools::usage $errstring}

    set outfile ""
    set title   ""
    set desc    "Keyword index"

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -title]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}
	    set title [lindex $argv 1]
	    set argv  [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -desc]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}
	    set desc [lindex $argv 1]
	    set argv [lrange $argv 2 end]
	    continue
	}
	tools::usage $errstring
    }

    if {[llength $argv] < 1} {tools::usage $errstring}

    optcheck::infile [set metafile [lindex $argv 0]] "Meta information file"

    puts stdout [::meta::2docidx [::meta::read $metafile] $title $desc]
    return    
}

# ------------------------------------------------------
package provide app-idx 0.1
