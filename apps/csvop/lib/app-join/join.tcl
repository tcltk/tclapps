# -*- tcl -*-
# Joining CSV files through key column.
# ------------------------------------------------------

package require tools
package require struct
package require optcsv
package require csv

namespace eval app-join {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-join::help(cmdline) {
[call [cmd {@appname@}] [method join] [opt "[option -sep] [arg sepChar]"] [opt [option -alternate]] [opt [option -inner]|[option -left]|[option -right]|[option -full]] [arg keycolumn] [arg file]...]

The contents of all the specified [arg file]s are merged into one
table, by associating all records with the same key with each
other. The keys are drawn from the column with index [arg keycolumn],
which is the same for all input files. The result of the merger is
written to [const stdout], in regular csv syntax. The key column from
the first table is retained in the result, in its orginal
location. All other key columns are considered redundant and removed
from the output.

[nl]

The command recognizes one option beyond the standard options (which
are described in topic [term genopt].):

[list_begin definitions]
[lst_item [option -inner]]
[lst_item [option -left]]
[lst_item [option -right]]
[lst_item [option -full]]

The operation will perfom an inner join by default

([option -inner]). To request one of the three possible outer joins
use one of the other three options.

[list_end]
[comment {
    Limitation regarding the handling of key columns: While the output
    key column is correct for -inner in the outer join modes the
    actual list of keys may contain more keys than we have in the key
    column we retain. In other words, the current implementation
    looses information.
}]
}

proc ::app-join::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-join::run {argv} {
    set errstring "wrong#args: join ?-sep char? ?-alternate? ?-left|-right|-full|-inner? keycol file..."

    # ==================================================
    # Process arguments

    if {[llength $argv] < 2} {tools::usage $errstring}

    set sepChar ,
    set alt     0
    set joinmode -inner
    while {[llength $argv]} {
        set err [cmdline::getopt argv {sep.arg alternate inner left right full} opt arg]
	if {$err == 1} {
	    switch -exact -- $opt {
		sep       {set sepChar $arg}
		alternate {set alt     1}
		inner - left - right - full  {set joinmode -$opt}
	    }
	} elseif {$err < 0} {
	    tools::usage $errstring
	} else {
	    # Non argument found, stop processing.
	    break
	}
    }

    if {[llength $argv] < 2} {tools::usage $errstring}

    set keycol                 [lindex $argv 0]
    ::optcsv::files [set files [lrange $argv 1 end]] $errstring
    ::optcsv::colidx $keycol $errstring

    # ==================================================
    # Process csv input

    # ==================================================
    # Special case: Only one file, this is identity. They key is not
    # relevant.

    if {[llength $files] == 1} {
	::optcsv::foreach_record $sepChar $alt data $files {
	    puts stdout [::csv::join $data $sepChar]
	}
	return
    }

    # ==================================================
    # Multiple files ... Read them all, then invoke the
    # appropriate list operation. We are using keyed tables.

    set rkeys  {}
    set reqlen [expr {1+$keycol}]
    set cmd    [list ::struct::list dbJoinKeyed $joinmode -keys rkeys]

    foreach f $files {
	set ktable [list]
	set bwidth 0
	::optcsv::foreach_record $sepChar $alt data [list $f] {
	    if {[llength $data] < $reqlen} {
		tools::usage "\"$base\": \n\tNeed at least $reqlen columns in all records,\
			got a record with only [llength $data] columns"
	    }
	    #lappend ktable [list [lindex $data $keycol] $data]

	    # Remove keys from data.
	    lappend ktable [list [lindex $data $keycol] [lreplace $data $keycol $keycol]]

	    if {$bwidth < [set bw [llength $data]]} {set bwidth $bw}
	}
	incr bwidth -1; # No key column
	lappend cmd [::optcsv::fill $bwidth $ktable]
	set first 0
    }

    set joinedtable [eval $cmd]

    # ==================================================
    # iii. Write result, insert the true key information as we go.

    #incr keycol -1
    foreach row $joinedtable key $rkeys {
	puts stdout [::csv::join [linsert $row $keycol $key] $sepChar]
    }
    return
}

# ------------------------------------------------------
package provide app-join 0.1
