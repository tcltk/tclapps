# -*- tcl -*-
# Make CSV data of the specified column unique
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-uniq {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-uniq::help(cmdline) {
[call [cmd {@appname@}] [method uniq] [opt "[option -sep] [arg sepChar]"] [opt [option -alternate]] [arg uniqcolumn] [arg file]...]

Reads the specified [arg file]s, eliminates all records which have
duplicate values in the column with index [arg uniqcolumn], except for the first record
in each equivalence set, and writes the result to [const stdout], in
regular csv syntax.

[nl]

Only the general options are available, they are described by the
topic [term genopt].
}

proc ::app-uniq::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-uniq::run {argv} {
    set errstring "wrong#args: uniq ?-sep char? ?-alternate? uniqcol file"

    # ==================================================
    # Process arguments

    if {[llength $argv] < 2} {tools::usage $errstring}

    set sepChar ,
    set alt     0
    while {[llength $argv]} {
        set err [cmdline::getopt argv {sep.arg alternate} opt arg]
	if {$err == 1} {
	    switch -exact -- $opt {
		sep       {set sepChar $arg}
		alternate {set alt     1}
	    }
	} elseif {$err < 0} {
	    tools::usage $errstring
	} else {
	    # Non argument found, stop processing.
	    break
	}
    }

    if {[llength $argv] < 2} {tools::usage $errstring}

    ::optcsv::colidx [set uniqcol [lindex $argv 0]]     $errstring
    ::optcsv::files  [set files   [lrange $argv 1 end]] $errstring

    # ==================================================
    # Process csv input

    array set keys {}
    ::optcsv::foreach_record $sepChar $alt data $files {
	set key [lindex $data $uniqcol]
	if {[info exists keys($key)]} {continue}
	set keys($key) .

	puts stdout [::csv::join $data $sepChar]
    }
    return
}

# ------------------------------------------------------
package provide app-uniq 0.1
