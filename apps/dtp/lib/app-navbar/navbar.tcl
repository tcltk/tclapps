# -*- tcl -*-
# Generation of a simple HTML navigation bar (toc, keywords)
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-navbar {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-navbar::help(cmdline) {

[call [cmd {@appname@}] [method navbar] [arg iomap] [arg base] ([arg infile] [arg label] [const /on]|[const /off]|[const /pass])...]

Creates a piece of HTML containing a navigation bar. All

[arg infile]'s are mapped to their proper output file via the contents
of the mapping file [arg iomap]. It is assumed that the navigation bar
will be inserted into the output file for the input file [arg base],
this guides the generation of proper relative links.

[nl]

For files which are marked with [const /off] only a label is
generated. A link is generated if and only if the [arg infile] is
marked with [const /on], or [const /pass]. When an [arg infile] is
marked with [const /pass] the system will assume that the string

[arg infile] is a fixed url and inserts it unchanged into the
generated output.

}

proc ::app-navbar::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-navbar::run {argv} {
    set errstring "wrong#args: navbar iomap base (in label /on|/off|/pass)..."

    if {[llength $argv] % 3 != 2} {tools::usage $errstring}

    optcheck::infile [set mapfile [lindex $argv 0]] "Mapping file"
    set basefile     [lindex $argv 1]

    array set iomap  [tools::readmap $mapfile]
    set basefile     $iomap($basefile)

    puts stdout [generate [array get iomap] $basefile [lrange $argv 2 end]]
    return
}

proc ::app-navbar::generate {iomap_ basefile nav} {
    array set iomap $iomap_

    set text "<hr> \[\n"
    set first 1

    foreach {file label state} $nav {
	if {!$first} {append text "| "} else {append text "  "}
	set first 0

	switch -exact -- $state {
	    /on     {append text "<a href=\"[::tools::link $basefile $iomap($file)]\"> $label </a>\n"}
	    /off    {append text "$label\n"}
	    /pass   {append text "<a href=\"$file\"> $label </a>\n"}
	    default {tools::usage $errstring}
	}
    }

    append text "\] <hr>\n"
    return $text
}

# ------------------------------------------------------
package provide app-navbar 0.1
