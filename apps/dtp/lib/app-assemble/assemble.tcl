# -*- tcl -*-
# Assemble all parts of dtp in one directory hierarchy.
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-assemble {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-assemble::help(cmdline) {
[call [cmd {@appname@}] [method assemble] [arg tcllibdir]]

Takes the relevant files from the textutil and doctools modules found
in the directory [file tcllibdir] (sources, or installation) and
copies them into the appropiate places of the directory hierarchy this
application is under.
}

proc ::app-assemble::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-assemble::run {argv} {
    set errstring "wrong#args: assemble tcllibdir"
    if {[llength $argv] != 1} {tools::usage $errstring}

    set tcllibdir [lindex $argv 0]

    if {![file exists      $tcllibdir]} {tools::usage "Tcllib directory does not exist: \"$tcllibdir\""}
    if {![file isdirectory $tcllibdir]} {tools::usage "Tcllib directory is a file: \"$tcllibdir\""}
    if {![file readable    $tcllibdir]} {tools::usage "Tcllib directory not accessible: \"$tcllibdir\""}

    set textutil [Locate $tcllibdir textutil]
    set doctools [Locate $tcllibdir doctools]

    set ourlibdir  [file join [::tools::topdir] lib]
    set ourhelpdir [file join [::tools::topdir] data help]

    GetTextutil $textutil $ourlibdir
    GetDoctools $doctools $ourlibdir $ourhelpdir
    return
}

proc ::app-assemble::Locate {tcllibdir module} {
    set res ""
    foreach p {{} modules} {
	set tmp [file join $tcllibdir $p $module]
	if {![file exists      $tmp]} {continue}
	if {![file isdirectory $tmp]} {continue}
	if {![file readable    $tmp]} {continue}
	set res $tmp
	break
    }

    if {$res == {}} {tools::usage "Tcllib directory incomplete, module $module missing"}
    return $res
}

proc ::app-assemble::GetTextutil {srcdir libdir} {

    set new [file join $libdir __textutil]
    set old [file join $libdir textutil__]
    set dst [file join $libdir textutil]

    file mkdir $new
    foreach src [glob -directory $srcdir *.tcl] {
	file copy -force $src $new
    }

    catch {file rename $dst $old}
    file rename $new $dst
    catch {file delete -force $old}
    return
}

proc ::app-assemble::GetDoctools {srcdir libdir helpdir} {
    set new [file join $libdir __doctools]
    set old [file join $libdir doctools__]
    set dst [file join $libdir doctools]

    file mkdir $new
    foreach src [glob -directory $srcdir *.tcl] {
	file copy -force $src $new
    }

    file copy -force [file join $srcdir mpformats] $new

    foreach src [glob -directory $srcdir doc*_*.man] {
	file copy -force $src $helpdir
    }

    catch {file rename $dst $old}
    file rename $new $dst
    catch {file delete -force $old}
    return
}

# ------------------------------------------------------
package provide app-assemble 0.1
