# -*- tcl -*-
# Generation - Table Of Contents
# ------------------------------------------------------

package require tools
package require optchecker
package require meta

namespace eval ::app-toc {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-toc::help(cmdline) {

[call [cmd {@appname@}] [method toc] [opt [arg options...]] [arg meta]]

This method converts the information given to us via file [arg meta]
into a table of contents. The output of this subcommand is written to
[const stdout],, or to the file specified with [option -out], if that
option is present, and will be in the [syscmd doctoc] format.

[nl]

It is expected that the contents of [arg meta] are in the format
returned by the subcommand [method meta], modulo formatting which does
not change the semantics of the output.

[nl]

The generation of the toc can be influenced by the options listed
below.

[list_begin definitions]

[lst_item "[option -out] [arg outputfile]"]

If present the generated output is diverted into the specified file
instead of written to [const stdout].

[lst_item "[option -title] [arg text]"]

Provides the [arg text] used as the label of [cmd toc_begin].
Defaults to [const "Table Of Contents"].

[lst_item "[option -desc] [arg text]"]

Provides the [arg text] used as the descriptive title of the toc in
[cmd toc_begin].  Defaults to [const "Manual"].

[list_end]
[nl]
}

proc ::app-toc::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-toc::run {argv} {
    set errstring "wrong#args: toc ?-out outputfile? ?-title text? ?-desc text? metafile"

    if {[llength $argv] < 1} {tools::usage $errstring}

    set outfile ""
    set title   "Table Of Contents"
    set desc    "Manual"

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
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

    if {[llength $argv] != 1} {tools::usage $errstring}

    optcheck::infile [set metafile [lindex $argv 0]] "Meta information file"

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
	set outfile [open $outfile w]
    } else {
	set outfile stdout
    }


    puts $outfile [::meta::2doctoc [::meta::read $metafile] $title $desc]
    return    
}

# ------------------------------------------------------
package provide app-toc 0.1
