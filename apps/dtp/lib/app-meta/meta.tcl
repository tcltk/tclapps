# -*- tcl -*-
# Extract meta information out of doctools manpage files.
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-meta {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-meta::help(cmdline) {

[call [cmd {@appname@}] [method meta] [opt "[option -out] [arg outputfile]"] [arg iomap]]

Extracts the meta information from all input files provided through
the mapping file [arg iomap] and returns it on [const stdout], or to
the file specified with [option -out], if that option is present. The
input files have to be in [syscmd doctools] format.

[nl]

The output is a tcl script containing a series of [cmd manpage]
commands. The first and only argument will be a key/value-list
acceptable to [cmd {array set}] containing information about the
manpage, like name of the file, keywords, cross references, title,
etc.

[nl]

The output of the subcommand [method meta] is an acceptable input for
the option [option -meta] of the subcommands 

[method doc], [method idx], and [method toc],

if stored in a file.

[nl]

The conversion can be influenced by the options listed below.

[list_begin definitions]

[lst_item "[option -out] [arg outputfile]"]

If present the generated output is diverted into the specified file
instead of written to [const stdout].

[list_end]


}

proc ::app-meta::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-meta::run {argv} {
    set errstring "wrong#args: meta ?-out outputfile? iomap"

    set outfile ""
    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
    }

    if {[llength $argv] < 1} {tools::usage $errstring}

    set               mapfile [lindex $argv 0]
    optcheck::infile $mapfile "Mapping file"

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
    }

    set files [list]
    foreach {in out} [tools::readmap $mapfile] {lappend files $in}

    package require dtglue
    package require meta

    if {$outfile == {}} {
	set outfile stdout
    } else {
	set outfile [open $outfile w]
    }

    puts $outfile [::meta::pretty [dtglue::getmeta $files]]
    return
}

# ------------------------------------------------------
package provide app-meta 0.1
