# -*- tcl -*-
# Sorting of CSV files
# ------------------------------------------------------

package require tools
package require optcsv
package require csv
package require cmdline

namespace eval app-sort {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-sort::help(cmdline) {
[call [cmd {@appname@}] [method sort] [opt "[option -sep] [arg sepChar]"] [opt [option -alternate]] [opt [option -n]] [opt [option -f]] [opt [option -r]] [opt "[option -skip] [arg n]"] [arg sortcol] [arg file]...]

This command sorts the specified [arg file]s by the given column

[arg sortcol] and writes the result to [const stdout], in regular csv
syntax.

[nl]

By default sorting is string-based, but it can be switched to integer
and floating point by specifying either the option [option -n]
(integer), or [option -f] respectively. If both options are specified
the latest occurrence has precedence.


[nl]

Specification of option [option -r] causes a reverse sort.

[nl]

Specification of the option [option -skip] causes the command to print
the first [arg n] records without change. I.e. it will start sorting
the records coming after a fixed length header.
}

proc ::app-sort::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-sort::run {argv} {
    set errstring "wrong#args: sort ?-sep sepchar? ?-f? ?-n? ?-r? ?-skip cnt? column file..."

    # ==================================================
    # Process arguments

    if {[llength $argv] < 2} {tools::usage $errstring}

    set sepChar   ,
    set alt       0
    set sortmode  ascii
    set order     increasing
    set reverse   0
    set skip      0
    set fill      {}

    while {[llength $argv]} {
        set err [cmdline::getopt argv {sep.arg alternate f n r skip.arg} opt arg]
	if {$err == 1} {
	    switch -exact -- $opt {
		sep       {set sepChar  $arg}
		alternate {set alt      1}
		f         {set sortmode real    ; set fill 0.0}
		n         {set sortmode integer ; set fill 0}
		r         {set order    decreasing}
		skip      {set skip     $arg}
	    }
	} elseif {$err < 0} {
	    tools::usage $errstring
	} else {
	    # Non argument found, stop processing.
	    break
	}
    }

    if {[llength $argv] < 2} {tools::usage $errstring}

    ::optcsv::colidx [set sortcol [lindex $argv 0]]     $errstring
    ::optcsv::files  [set files   [lrange $argv 1 end]] $errstring

    if {![string is integer -strict $skip] || ($skip < 0)} {
	tools::usage $errstring
    }

    # ==================================================
    # Process csv input

    set records [list]
    ::optcsv::foreach_record $sepChar $alt data $files {
	if {$skip > 0} {
	    puts stdout [::csv::join $data $sepChar]
	    incr skip -1
	    continue
	}
	# Fill to sort column.
	while {[llength $data] <= $sortcol} {lappend data $fill}
	lappend records $data
    }

    # ==================================================
    # Write result

    foreach data [lsort -index $sortcol -$order -$sortmode $records] {
	puts stdout [::csv::join $data $sepChar]
    }
    return
}

# ------------------------------------------------------
package provide app-sort 0.1
