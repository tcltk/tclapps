# -*- tcl -*-
# Generation (Conversion subsystem)
# ------------------------------------------------------

package require tools
package require optchecker
package require dtglue

namespace eval app-doc {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-doc::help(cmdline) {
[call [cmd {@appname@}] [method doc] [opt [arg options...]] [arg format] [arg docfile] [opt [arg metafile]]]


This command was added to aid in the quick conversion of a single file
without causing the user to have to jump through all the hoops
required by [method gen-doc] to make the batch processing of large
sets efficient.

[nl]

It processes one file ([arg docfile]) and converts it into the
requested output [arg format]. The result of the conversion is written
to [const stdout], or to the file specified with [option -out], if
that option is present. If the [arg metafile] is present it is used
for the creation of cross-references, assuming that this is supported
by the chosen format. Providing a [arg metafile] to a format not
supporting cross-references is an error.

[nl]

The conversion can be influenced by the options listed below.

[list_begin definitions]

[lst_item "[option -out] [arg outputfile]"]

If present the generated output is diverted into the specified file
instead of written to [const stdout].


[lst_item "[option -varstring] [arg varname] [arg string]"]

This option is used to set format/engine specific parameters to some
value. The parameter is specified by [arg varname], the new value by
[arg string].

[nl]

Currently only the HTML engine supports engine parameters.

[lst_item "[option -varfile] [arg varname] [arg file]"]

This option is used to set format specific/engine parameters to some
value. The parameter is specified by [arg varname], the new value by
the contents of the file [arg file].

[nl]

Currently only the HTML engine supports engine parameters.

[list_end]
}

proc ::app-doc::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-doc::run {argv} {
    set errstring "wrong#args: doc ?-out outputfile? ?-varstring var string? ?-varfile var file? format docfile ?metafile?"

    if {[llength $argv] < 2} {tools::usage $errstring}

    set docfile ""
    set mapfile ""
    set outfile ""
    array set para {}

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 4} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -varstring]} {
	    if {[llength $argv] < 5} {tools::usage $errstring}

	    set varname  [lindex $argv 1]
	    set varvalue [lindex $argv 2]
	    set argv     [lrange $argv 3 end]

	    set para($varname) $varvalue
	    continue
	}
	if {[string equal [lindex $argv 0] -varfile]} {
	    if {[llength $argv] < 5} {tools::usage $errstring}

	    set varname  [lindex $argv 1]
	    ::optcheck::infile [lindex $argv 2] "Parameter content file"
	    set varvalue [tools::getfile [lindex $argv 2]]
	    set argv     [lrange $argv 3 end]

	    set para($varname) $varvalue
	    continue
	}
	tools::usage $errstring
    }

    if {[llength $argv] < 2} {tools::usage $errstring}
    if {[llength $argv] > 3} {tools::usage $errstring}

    set                   format  [lindex $argv 0]
    optcheck::infile [set docfile [lindex $argv 1]] "Document file"

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
    }

    set iomap {}

    if {[llength $argv] == 3} {
	# Check if the engine supports the xref parameter.
	# If yes, use meta data for xref
	# If no, error out

	if {![dtglue::hasXref $format]} {
	    tools::usage "Cross-references (engine parameter 'xref') not supported by $format, meta file not allowed"
	}

	optcheck::infile [set metafile [lindex $argv 2]] "Meta information file"
	package require meta

	set para(xref) [::meta::2xref [::meta::read $metafile] {}]
    }

    if {$outfile == {}} {
	set outfile stdout
    } else {
	set outfile [open $outfile w]
    }

    puts $outfile [dtglue::cvtonefile $format $docfile [array get para]]
    return
}

# ------------------------------------------------------
package provide app-doc 0.1
