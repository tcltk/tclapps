# -*- tcl -*-
# CSV specific option processing.
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval ::optcsv {}

# ------------------------------------------------------


proc ::optcsv::files {files errstring} {
    if {[llength $files] == 0} {tools::usage $errstring}
    set stdin 1
    foreach f $files {
	if {[string equal $f -]} {
	    if {!$stdin} {
		tool::usage "Cannot use - (stdin) more than once"
	    }
	    set stdin 0
	} else {
	    optcheck::infile $f "CSV input file"
	}
    }
    return
}


proc ::optcsv::colspec {colspecvar errstring} {
    upvar $colspecvar colspec

    if {[llength $colspec] == 0} {
	tools::usage $errstring
    }
    set idx [list]
    foreach i $colspec {
	if {[regexp -- {[0-9]+-[0-9]+} $i]} {
	    foreach {f t} [split $i -] break
	    colidx $f $errstring
	    colidx $t $errstring
	    lappend idx [list $f $t]
	} elseif {[regexp -- {[0-9]+-} $i]} {
	    foreach {f t} [split $i -] break
	    colidx $f $errstring
	    lappend idx [list $f end]
	} elseif {[regexp -- {-[0-9]+} $i]} {
	    foreach {f t} [split $i -] break
	    colidx $t $errstring
	    lappend idx [list 0 $t]
	} elseif {[regexp -- {[0-9]+} $i]} {
	    colidx $i $errstring
	    lappend idx [list $i $i]
	} else {
	    tools::usage $errstring
	}
    }
    set colspec $idx
    return
}

proc ::optcsv::colidx {col errstring} {
    if {
	([string length $col] == 0) ||
	![string is integer $col]   ||
	$col < 0
    } {
	tools::usage $errstring
    }
    return
}


proc ::optcsv::foreach_record {sepChar alt recvar files script} {
    upvar $recvar data

    foreach f $files {
	if {[string equal $f -]} {
	    set in stdin
	} else {
	    set in [open $f r]
	}
	while {![eof $in]} {
	    if {[gets $in line] < 0} {continue}
	    if {[string length $line] == 0} {continue}
	    if {$alt} {
		set data [::csv::split -alternate $line $sepChar]
	    } else {
		set data [::csv::split            $line $sepChar]
	    }

	    uplevel 1 $script
	}
	if {![string equal $f -]} {
	    close $in
	}
    }
    return
}

proc ::optcsv::fill {maxlen ktable} {

    set nktable [list]
    foreach row $ktable {
	foreach {key row} $row break
	while {[llength $row] < $maxlen} {lappend row {}}
	lappend nktable [list $key $row]
    }
    return $nktable
}

# ------------------------------------------------------
package provide optcsv 0.1
