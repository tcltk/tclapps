# -*- tcl -*-
# Template substitution (@...@ -> string)
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-subst {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-subst::help(cmdline) {
[call [cmd {@appname@}] [method subst] [arg inputfile] [arg key] [arg value]...]

Reads the contents of the [arg inputfile] and replaces all occurences
of [const @][arg key][const @] with the the associated [arg value],
for all keys and values. The result of this substitution is written to
[const stdout]. The special value [const -] for [arg inputfile]
instructs the application to read the data process from [const stdin].
}

proc ::app-subst::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-subst::run {argv} {
    set errstring "wrong#args: subst inputfile key value..."

    if {[llength $argv] < 3}      {tools::usage $errstring}
    if {[llength $argv] % 2 == 0} {tools::usage $errstring}

    set infile [lindex $argv 0]
    if {![string equal $infile -]} {optcheck::infile $infile "Input file"}

    set map [list]
    foreach {in out} [lrange $argv 1 end] {
	lappend map @${in}@ $out
    }

    if {[string equal $infile -]} {
	set input [read stdin]
    } else {
	set input [tools::getfile $infile]
    }

    puts stdout [string map $map $input]
    return
}

# ------------------------------------------------------
package provide app-subst 0.1
