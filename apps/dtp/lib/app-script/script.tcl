# -*- tcl -*-
# Generation (Conversion subsystem - Table of Contents)
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-script {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-script::help(cmdline) {

[call [cmd {@appname@}] [method script] [opt "[option -out] [arg outputfile]"]]

Returns a shell script stored in the application on [const stdout], or
to the file specified with [option -out], if that option is present.

}

proc ::app-script::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-script::run {argv} {
    set errstring "wrong#args: getscript ?-out output?"

    set outfile ""
    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 2} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
    }

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
	set outfile [open $outfile w]
    } else {
	set outfile stdout
    }

    if {[llength $argv] != 0} {tools::usage $errstring}
    tools::copyout [file join [tools::topdir] data doc_a_package.sh] $outfile
    return
}

# ------------------------------------------------------
package provide app-script 0.1
