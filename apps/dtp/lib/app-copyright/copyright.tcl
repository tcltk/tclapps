# -*- tcl -*-
# Generation (Conversion subsystem)
# ------------------------------------------------------

package require tools
package require optchecker

namespace eval app-copyright {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-copyright::help(cmdline) {
[call [cmd {@appname@}] [method copyright] [opt "[option -out] [arg outputfile]"] [opt "[option -label] [arg text]"] [opt "[option -logo] [arg path]"] [opt "[option -link] [arg url]"]]

Creates a piece of HTML containing a copyright clause. Note that while
any two of the label, logo, and link options are allowed to be missing
at least one option from that set has to be present. The result is
written to [const stdout], or to the file specified with [option -out],
if that option is present.

[nl]

If the [option -label] is specified its argument will be used as the
name of the entity having the copyright.

[nl]

If [option -logo] is specified its argument is the name of an image
file to insert.

[nl]

If a [option -link] is present both label and logo will be hyperlinks
to its argument.

[nl]

If option [option -out] is present the generated output is diverted
into the specified file instead of written to [const stdout].

}

proc ::app-copyright::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-copyright::run {argv} {
    set errstring "wrong#args: copyright ?-out outputfile? ?-label text? ?-logo path? ?-link url?"
    if {[llength $argv] < 2} {tools::usage $errstring}

    set label "" ; set haslabel 0
    set logo  "" ; set haslogo  0
    set link  "" ; set haslink  0
    set outfile ""

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -label]} {
	    set label [lindex $argv 1]
	    set argv  [lrange $argv 2 end]
	    set haslabel 1
	    continue
	}
	if {[string equal [lindex $argv 0] -logo]} {
	    set logo  [lindex $argv 1]
	    set argv  [lrange $argv 2 end]
	    set haslogo 1
	    continue
	}
	if {[string equal [lindex $argv 0] -link]} {
	    set link  [lindex $argv 1]
	    set argv  [lrange $argv 2 end]
	    set haslink 1
	    continue
	}
	tools::usage $errstring
    }

    if {([llength $argv] != 0) || ($haslabel + $haslogo + $haslink) < 1} {
	tools::usage $errstring
    }

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
	set outfile [open $outfile w]
    } else {
	set outfile stdout
    }

    puts $outfile [generate $label $logo $link]
    return
}

proc ::app-copyright::generate {label logo link} {
    set text ""
    if {$label != {}} {append text $label}
    if {$logo  != {}} {append text "<img src=\"$logo\">"}
    if {$link != {}} {
	set text "<a href=\"$link\">$text</a>"
    }
    set     text "<h4>Copyright &copy; [clock format [clock seconds] -format %Y] $text</h4>"
    return $text
}

# ------------------------------------------------------
package provide app-copyright 0.1
