# -*- tcl -*-
# Format CSV input into a readable matrix.
# ------------------------------------------------------

package require tools
package require optcsv
package require csv
package require report
package require struct
package require cmdline


namespace eval app-format {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-format::help(cmdline) {
[call [cmd {@appname@}] [method format] [opt "[option -sep] [arg sepChar]"] [opt [option -alternate]] [arg outputformat] [arg file]...]

This command first concatenates the input files, then formats the
resulting list of csv records according the chosen [arg outputformat],
and at last writes the result of the formatting to [const stdout].

[nl]

Only the general options are available, they are described by the
topic [term genopt].

[nl]

The format codes acceptable for [arg outputformat] are

[list_begin definitions]
[lst_item html]

The result is a html table embedded into a paragraph, and as such
embeddable into other html documents.

[lst_item text]

A simple ascii formatted table. The whole table has a border, and all
columns are separated by "|". Inside the column all data is
left-justified and at least one space away from the column borders.

[lst_item text-headcap]

Like [const text], except that the first line is visually
separated from the remainder of the table. I.e. it is treated as
heading / header caption.

[lst_item text-botcap]

Like [const text], except that the last line is visually
separated from the remainder of the table. I.e. it is treated as
bottom caption.

[lst_item text-headbotcap]

Like [const text], except that both the first and last lines
are visually separated from the remainder of the table. I.e. they are
treated as header and bottom captions.

[list_end]
}

proc ::app-format::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-format::run {argv} {
    set errstring "wrong#args: format ?-sep char? ?-alternate? format file..."

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
	    tools::usage "Unknown option \"-$opt\""
	} else {
	    # Non argument found, stop processing.
	    break
	}
    }

    if {[llength $argv] < 2} {tools::usage $errstring}

    ::optcsv::files [set files [lrange $argv 1 end]] "Bad input file"
    set format                 [lindex $argv 0]
    switch -exact -- $format {
	html - text - text-headcap - text-botcap - text-headbotcap {}
	default {
	    tools::usage "Unknown format, use one of html, text, text-headcap, text-botcap, or text-headbotcap"
	}
    }

    # ==================================================
    # Process csv input

    struct::matrix::matrix m

    ::optcsv::foreach_record $sepChar $alt data $files {
	if {[m columns] < [llength $data]} {
	    m add columns [expr {[llength $data] - [m columns]}]
	}
	m add row $data
    }

    # ==================================================
    # Write results, special casing for HTML.

    if {[string equal $format html]} {
	puts stdout "<p><table border=1>"
    }

    ::report::report r [m columns] style $format

    r printmatrix2channel m stdout
    r destroy
    m destroy

    if {[string equal $format html]} {
	puts stdout "</table></p>"
    }
    return
}

# ------------------------------------------------------
# Report styles known here.

::report::defstyle html {} {
    set c  [columns]
    set cl $c ; incr cl -1
    data set "<tr> [split [string repeat " " $cl] ""] </tr>"
    for {set col 0} {$col < $c} {incr col} {
	pad $col left  "<td>"
	pad $col right "</td>"
    }
    return
}

::report::defstyle text {} {
    data   set [split "[string repeat "| "   [columns]]|"]
    top    set [split "[string repeat "+ - " [columns]]+"]
    bottom set [top get]
    top	   enable
    bottom enable

    for {set i 0} {$i < [columns]} {incr i} {
	pad $i both
    }
    return
}
::report::defstyle text-headcap {{n 1}} {
    text
    topdata   set [data get]
    topcapsep set [top  get]
    topcapsep enable
    tcaption $n
}
::report::defstyle text-botcap {{n 1}} {
    text
    topdata   set [data get]
    topcapsep set [top  get]
    topcapsep enable
    tcaption $n
    botdata   set [data   get]
    botcapsep set [bottom get]
    botcapsep enable
    bcaption $n
}
::report::defstyle text-headbotcap {{n 1}} {
    text
    topdata    set [data get]
    topcapsep  set [top  get]
    topdatasep set [top  get]
    topcapsep  enable
    topdatasep enable
    tcaption $n
    botdata    set [data   get]
    botcapsep  set [bottom get]
    botdatasep set [top  get]
    botcapsep  enable
    botdatasep enable
    bcaption $n
}



# ------------------------------------------------------
package provide app-format 0.1
