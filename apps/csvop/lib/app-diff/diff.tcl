# -*- tcl -*-
# Differencing CSV files.
# ------------------------------------------------------

package require tools
package require optcsv
package require csv

namespace eval app-diff {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-diff::help(cmdline) {
[call [cmd {@appname@}] [method diff] \
 [opt "[option -sep] [arg sepChar]"] \
 [opt [option -alternate]] \
 [opt [option -n]] \
 [opt "[option -key] [arg spec]"] \
 [arg file1] [arg file2]]

Reads the two files and compares their records. The result of the
comparison is written to [const stdout], in regular csv syntax.  The
first column of the output indicates if the record was added or
removed in [arg file2] compared to [arg file1].

The command recognizes two options beyond the standard options (which
are described in topic [term genopt].):

[list_begin definitions]
[lst_item [option -n]]

If this option is specified the second column will contain the number
of the line in the input the record came from.

[lst_item "[option -key] [arg spec]"]

If specified it determines which parts of a record are used to
compare, i.e. the key columns. The argument is in the same format as
the column specification used by [cmd cut]. By default the entire
record is used in the comparison.

[list_end]
}

proc ::app-diff::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-diff::run {argv} {
    set errstring "wrong#args: diff ?-sep char? ?-alternate? ?-n? ?-key spec? file1 file2"

    # ==================================================
    # Process arguments

    if {[llength $argv] < 2} {tools::usage $errstring}

    set lineout   0
    set keySpec   0-
    set sepChar ,
    set alt     0
    while {[llength $argv]} {
        set err [cmdline::getopt argv {sep.arg alternate key.arg n} opt arg]
	if {$err == 1} {
	    switch -exact -- $opt {
		sep       {set sepChar $arg}
		alternate {set alt     1}
		n         {set lineout 1}
		key       {set keySpec $arg}
	    }
	} elseif {$err < 0} {
	    tools::usage $errstring
	} else {
	    # Non argument found, stop processing.
	    break
	}
    }

    if {[llength $argv] != 2} {tools::usage $errstring}

    ::optcsv::files   $argv
    ::optcsv::colspec keySpec $errstring

    # ----------------------------------------------------
    # Actual processing, uses the following information from the
    # commandline:
    #
    # inA     - channel for input A
    # inB     - channel for input B
    # sepChar - separator character
    
    # We read file2 completely and then go through the records of
    # file1. For any record we don't find we write a "deleted" record. If
    # we find the matching record we remove it from the internal
    # storage. In a second sweep through the internal array we write
    # "added" records for the remaining data as that was not in file1 but
    # is in file2.

    foreach {fileA fileB} $argv break
    set inA [open $fileA r]
    set inB [open $fileB r]

    set order [list]
    array set map {}
    set linenum 0
    while {![eof $inB]} {
	if {[gets $inB line] < 0} {
	    continue
	}
	incr linenum
	if {$alternate} {
	    set data [::csv::split -alternate $line $sepChar]
	} else {
	    set data [::csv::split $line $sepChar]
	}
	set  key  [keyof $data]

	if {[info exist map($key)]} {
	    puts stderr "warning: $key occurs multiple times in $fileB (lines $linenum and $map($key))"
	}
	set map($key) $linenum
	lappend order $data
    }
    close $inB

    set linenum 0

    if {$lineout} {
	array set lmap {}
    }

    while {![eof $inA]} {
	if {[gets $inA line] < 0} {
	    continue
	}
	incr linenum
	if {$alternate} {
	    set data [::csv::split -alternate $line $sepChar]
	} else {
	    set data [::csv::split $line $sepChar]
	}
	set key [keyof $data]

	if {$lineout} {set lmap($key) $linenum}

	if {[info exists map($key)]} {
	    if {$map($key) < 0} {
		puts stderr "warning: $key occurs multiple times\
			in $fileA (lines $linenum and [expr {-$map($key)}]"
	    } else {
		set map($key) [expr {-$linenum}]
	    }
	    continue
	}

	if {$lineout} {
	    puts stdout [::csv::join [linsert $data 0 - $linenum] $sepChar]
	} else {
	    puts stdout [::csv::join [linsert $data 0 -] $sepChar]
	}
    }
    close $inA

    foreach data $order {
	set key [keyof $data]
	if {$map($key) > 0} {
	    if {$lineout} {
		puts stdout [::csv::join [linsert $data 0 + $lmap($key)] $sepChar]
	    } else {
		puts stdout [::csv::join [linsert $data 0 +] $sepChar]
	    }
	}
    }
    return
}

proc ::app-diff::keyof {data} {
    upvar keySpec keySpec
    set key [list]
    foreach i $keySpec {
	foreach {f t} $i break
	eval lappend key [lrange $data $f $t]
    }
    return $key
}

# ------------------------------------------------------
package provide app-diff 0.1
