# -*- tcl -*-
# Generation (Conversion subsystem - Table of Contents)
# ------------------------------------------------------

package require tools

namespace eval app-script {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-script::help(cmdline) {

[call [cmd {@appname@}] [method getscript]]

Returns a shell script stored in the application on [const stdout].
}

proc ::app-script::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-script::run {argv} {
    set errstring "wrong#args: getscript"
    if {[llength $argv] != 0} {tools::usage $errstring}
    tools::copyout [file join [tools::topdir] data doc_a_package.sh]
    return
}

# ------------------------------------------------------
package provide app-script 0.1
