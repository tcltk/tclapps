#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}
# Main file for multi-purpose DTP application

# We cannot assume the existence of package starkit. This implies that
# we have to create our own topdir information.

if {[catch {
    package require starkit
    starkit::startup
    set top $starkit::topdir
}]} {
    # Generate our own topdir information and do the same setup with
    # regard to auto_path as is done by the starkit support.

    set top [file dirname [file normalize [file join [pwd] [info script]]]]
    lappend auto_path [file join $top lib]
}

# setup the tools, especially our own portable topdir reference
package require tools
tools::topdir= $top

# Debugging as subdir of doctools - Need access to tcllib modules.

lappend auto_path [set _ [file dirname [file dirname $top]]]
#lappend auto_path [file dirname [file dirname [file dirname $_]]]


if 0 {
    puts "=== $top"
    puts "~~~ [file dirname [file normalize [file join [pwd] [info script]]]]"
    puts \t[join $auto_path \n\t]
}


if {[llength $argv] < 1} {
    puts stderr "usage: $argv0 subcommand arguments..."
    exit 1
}

set command [lindex $argv 0]

if {![file exists [file join $top lib app-$command]]} {
    cd [file join $top lib]
    set cmdlist [list]
    foreach a [glob -nocomplain -type d app-*] {
	lappend cmdlist [regsub -- {^app-} $a {}]
    }
    if {[llength $cmdlist] == 0} {
	set cmds nothing
    } elseif {[llength $cmdlist] == 1} {
	set cmds \"[join $cmdlist]\"
    } else {
	set cmds [linsert [join $cmdlist ", "] end-1 or]
    }
    tools::usage "unknown command \"$command\", expected $cmds"
    # Don't come here
}

package require app-$command
app-${command}::run [lrange $argv 1 end]
exit 0
