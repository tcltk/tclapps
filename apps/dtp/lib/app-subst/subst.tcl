# -*- tcl -*-
# Template substitution (@...@ -> string)
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-subst {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-subst::help(cmdline) {
[call [cmd {@appname@}] [method subst] [opt "[option -out] [arg outputfile]"] [arg inputfile] [arg key] [arg value]...]

Reads the contents of the [arg inputfile] and replaces all occurences
of [const @][arg key][const @] with the the associated [arg value],
for all keys and values. The result of this substitution is written to
[const stdout], or to the file specified with [option -out], if that
option is present. The special value [const -] for [arg inputfile]
instructs the application to read the data process from [const stdin].

[nl]

The conversion can be influenced by the options listed below.

[list_begin definitions]

[lst_item "[option -out] [arg outputfile]"]

If present the generated output is diverted into the specified file
instead of written to [const stdout].

[list_end]
}

proc ::app-subst::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-subst::run {argv} {
    set errstring "wrong#args: subst ?-out outputfile? inputfile key value..."

    set outfile ""
    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
    }

    if {[llength $argv] < 3}      {tools::usage $errstring}
    if {[llength $argv] % 2 == 0} {tools::usage $errstring}

    set infile [lindex $argv 0]
    if {![string equal $infile -]} {optcheck::infile $infile "Input file"}

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
    }

    set map [list]
    foreach {in out} [lrange $argv 1 end] {
	lappend map @${in}@ $out
    }

    if {[string equal $infile -]} {
	set input [read stdin]
    } else {
	set input [tools::getfile $infile]
    }

    if {$outfile == {}} {
	set outfile stdout
    } else {
	set outfile [open $outfile w]
    }

    puts $outfile [string map $map $input]
    return
}

# ------------------------------------------------------
package provide app-subst 0.1
