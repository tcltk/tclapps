# -*- tcl -*-
# Extract meta information out of doctools manpage files.
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-meta {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-meta::help(cmdline) {

[call [cmd {@appname@}] [method meta] [arg iomap]]

Extracts the meta information from all input files provided through
the mapping file [arg iomap] and returns it on [const stdout]. The
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
}

proc ::app-meta::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-meta::run {argv} {
    set errstring "wrong#args: meta iomap"

    if {[llength $argv] < 1} {tools::usage $errstring}

    set               mapfile [lindex $argv 0]
    optcheck::infile $mapfile "Mapping file"

    set files [list]
    foreach {in out} [tools::readmap $mapfile] {lappend files $in}

    package require dtglue
    package require meta

    puts stdout [::meta::pretty [dtglue::getmeta $files]]
    return
}

# ------------------------------------------------------
package provide app-meta 0.1
