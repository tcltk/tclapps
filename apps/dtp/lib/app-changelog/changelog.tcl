# -*- tcl -*-
# Generation (Conversion subsystem)
# ------------------------------------------------------

package require tools
package require optchecker
package require doctools::changelog

namespace eval app-changelog {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-changelog::help(cmdline) {
[call [cmd {@appname@}] [method changelog] [arg label] [arg module] [arg logfile]...]

This command takes a number of ChangeLog files as they are generated
and handled by [syscmd emacs], parses them, and converts the
result into a document in [term doctools] format. This document is
written to [const stdout].

[nl]

The arguments [arg label] and [arg module] are specifying texts for
use in the header of the created document which cannot be supplied by
the changelogs themselves.  }

proc ::app-changelog::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-changelog::run {argv} {
    set errstring "wrong#args: changelog label module logfile..."

    if {[llength $argv] < 3} {tools::usage $errstring}

    set label  [lindex $argv 0]
    set module [lindex $argv 1]
    set files  [lrange $argv 2 end]

    foreach f $files {
	optcheck::infile $f "ChangeLog file"
    }

    set data [list]
    foreach f $files {
	lappend data [doctools::changelog::scan [tools::getfile $f]]
    }

    set data [eval [linsert $data 0 doctools::changelog::merge]]
    puts [doctools::changelog::toDoctools $label $module "--" $data]
    return
}

# ------------------------------------------------------
package provide app-changelog 0.1
