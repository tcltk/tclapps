# -*- tcl -*-
# Cut(ting) columns out of CSV files. IOW "Projection".
# This includes the ability to reorder and replicate columns.
# ------------------------------------------------------

package require tools
package require optcsv
package require csv
package require cmdline

namespace eval app-cut {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-cut::help(cmdline) {
[call [cmd {@appname@}] [method cut] [opt "[option -sep] [arg sepChar]"] [opt [option -alternate]] [arg colspec] [arg file]...]

This command first concatenates the input files, and then uses the
provided column specification ([arg colspec]) to determine which of
the columns in the input have to appear in the output, and in which
order. The result of the operation is written to [const stdout], in
regular csv syntax.

[nl]

Only the general options are available, they are described by the
topic [term genopt].

[nl]

The [arg colspec] is a list of column indices and column ranges,
separated by commas. Column indices are counted from 0 (for the first,
i.e. left-most column). A column range can have any of the following
forms, with both [var n] and [var m] legal column indices.

[list_begin definitions]
[lst_item "[var n]-[var m]"]

The range of columns beginning at column [var n], inclusive and ending
at column [var m], inclusive.

[lst_item "[var n]-"]

The range of columns beginning at column [var n], inclusive and ending
with the last column. In other words, this is a shortcut for "[var n]-end"

[lst_item "-[var m]"]

The range of column beginning at column 0 and ending at column [var m],
inclusive. In other words, this is a shortcut for "0-[var m]"

[list_end]

[nl]

Note that it is not only possible to remove, i.e. cut, columns from
the input, but also to simply reorder column, and to replicate
them. The last will happen if a column index is used more than once in
the column specification.

[nl]

At least one [arg file] has to be specified. Use the string [const -]
to cause the command to read its input from [const stdin]. This
specification can be mixed with an arbitrary number of true file
names, but itself used only once.

[nl]
[emph Example] for [arg colspec]:
[example {
    0,2-5,7-,6
}]
[nl]

This removes column 1 from the input, and moves column 6 to the last
position.
    }

proc ::app-cut::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-cut::run {argv} {
    set errstring "wrong#args: cut ?-sep char? ?-alternate? colspec file..."

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

    set colspec           [split [lindex $argv 0] ,]
    ::optcsv::files   [set files [lrange $argv 1 end]] $errstring
    ::optcsv::colspec colspec                          $errstring

    # ==================================================
    # Process csv input

    ::optcsv::foreach_record $sepChar $alt data $files {
	set dataOut [list]
	foreach i $colspec {
	    foreach {f t} $i break
	    eval [list lappend dataOut] [lrange $data $f $t]
	}
	puts stdout [::csv::join $dataOut $sepChar]
    }
    return
}

# ------------------------------------------------------
package provide app-cut 0.1
