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
    SetupModule $libdir textutil

    set srcfiles [glob -nocomplain -directory $srcdir *.tcl]

    if {[llength $srcfiles] == 0} {tools::usage "Textutil incomplete: No tcl scripts found"}

    foreach {file label} {
	pkgIndex.tcl {Package index is missing}
	expander.tcl {Expander functionality is missing}
    } {
	if {[lsearch -glob $srcfiles *$file] < 0} {
	    tools::usage "Textutil incomplete: $label"
	}
    }

    variable new
    set tocopy [list]
    foreach src $srcfiles {lappend tocopy [list $src [file join $new [file tail $src]]]}

    CopyFiles $tocopy
    FinishModule
    return
}

proc ::app-assemble::GetDoctools {srcdir libdir helpdir} {
    SetupModule $libdir doctools
    variable new

    set srcfiles    [glob -nocomplain -directory $srcdir *.tcl]
    set fmttclfiles [glob -nocomplain -directory [file join $srcdir mpformats] *.tcl]
    set fmtmsgfiles [glob -nocomplain -directory [file join $srcdir mpformats] *.msg]
    set fmtfmtfiles [glob -nocomplain -directory [file join $srcdir mpformats] fmt.*]
    set fmtidxfiles [glob -nocomplain -directory [file join $srcdir mpformats] idx.*]
    set fmttocfiles [glob -nocomplain -directory [file join $srcdir mpformats] toc.*]
    set docfiles    [glob -nocomplain -directory $srcdir doc*_*.man]

    if {[llength $srcfiles]    == 0} {tools::usage "Tcllib/DocTools incomplete: No tcl scripts found"}
    if {[llength $fmtidxfiles] == 0} {
	tools::usage "DocTools: Indexer engines missing. This revision of the module is unuseable for dtp"
    }
    if {[llength $fmttocfiles] == 0} {
	tools::usage "DocTools: TOC engines missing. This revision of the module is unuseable for dtp"
    }
    if {[llength $docfiles] == 0} {
	tools::usage "DocTools: New 'format manpages' missing. This revision of the module is unuseable for dtp"
    }

    variable new
    set tocopy [list]
    foreach src $srcfiles    {lappend tocopy [list $src [file join $new [file tail $src]]]}
    foreach src $docfiles    {lappend tocopy [list $src [file join $helpdir [file tail $src]]]}
    foreach src $fmttclfiles {lappend tocopy [list $src [file join $new mpformats [file tail $src]]]}
    foreach src $fmtmsgfiles {lappend tocopy [list $src [file join $new mpformats [file tail $src]]]}
    foreach src $fmtfmtfiles {lappend tocopy [list $src [file join $new mpformats [file tail $src]]]}
    foreach src $fmtidxfiles {lappend tocopy [list $src [file join $new mpformats [file tail $src]]]}
    foreach src $fmttocfiles {lappend tocopy [list $src [file join $new mpformats [file tail $src]]]}

    CopyFiles $tocopy
    FinishModule
    return
}

proc ::app-assemble::SetupModule {libdir module} {
    variable new ; set new [file join $libdir __${module}]
    variable old ; set old [file join $libdir ${module}__]
    variable dst ; set dst [file join $libdir ${module}]

    catch {file delete -force $new}
    file mkdir $new

    return
}

proc ::app-assemble::FinishModule {} {
    variable new
    variable old
    variable dst

    catch {file rename        $dst $old}
    file        rename        $new $dst
    catch {file delete -force $old}
    return
}

proc ::app-assemble::CopyFiles {srcdstlist} {
    foreach {item} $srcdstlist {
	foreach {src dst} $item break
	file mkdir [file dirname $dst]

	puts "\tAssemble $src" ; # \t $dst""

	file copy -force $src $dst
    }
    return
}

# ------------------------------------------------------
package provide app-assemble 0.1
