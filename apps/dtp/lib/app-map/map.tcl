# -*- tcl -*-
# Create mapping from input to output files.
# ------------------------------------------------------

package require optchecker
package require tools

namespace eval app-map {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-map::help(cmdline) {

[call [cmd {@appname@}] [method map] [opt [arg options...]] [arg file]...]

This command returns on [const stdout], or to the file specified with
[option -out], if that option is present, an array mapping the paths
of the given input files to the names and locations of the output
files as specified and modifed per the accepted options, listed below.

[nl]

Each line of the output will be a valid tcl list containining two
elements. First the path to the input file, followed by the path to
the output file.

[nl]

The name of an output file is derived from the name of the input file
by the following algorithm:

[list_begin enum]
[enum]
Remove an existing extension from the name of the input file and add
the extension specified via option [option -ext].

[enum]
If option [option -outdir] is specified, then make the path coming out of
step 1 a relative path and then prepend the output directory provided
by the option.

[enum]
If the path coming out of the steps above is identical to the path of
the input file, then add the new extension at the end of the path to
ensure that the output file is different from the input file.

[list_end]
[nl]

[list_begin definitions]

[lst_item "[option -out] [arg outputfile]"]

If present the generated output is diverted into the specified file
instead of written to [const stdout].

[lst_item "[option -ext] [arg newextension]"]

This option has to be specified; and provides the new extension to
give to the output files.

[lst_item "[option -outdir] [arg directory]"]

If this option is specified all output files will be placed into the
given [arg directory].

[lst_item "[option -trail] [arg n]"]

The argument [arg n] has to be an integer number greater than

[const 0]. The value defaults to 0.  If specified the last [arg n]
elements of an input path are taken as the initial value for the
output path.

[list_end]
}


proc ::app-map::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-map::run {argv} {
    set errstring "wrong#args: map -ext newextension ?-out outputfile? ?-outdir directory? ?-trail n? file..."

    if {[llength $argv] < 3} {tools::usage $errstring}

    set outfile ""
    set outdir ""
    set newext ""
    set hasext 0
    set trail 0

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -out]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set outfile [lindex $argv 1]
	    set argv    [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -outdir]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    ::optcheck::outdir [set outdir [lindex $argv 1]] "Output directory"
	    set argv                       [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -ext]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set hasext 1
	    set newext [lindex $argv 1]
	    set argv   [lrange $argv 2 end]
	    continue
	}
	if {[string equal [lindex $argv 0] -trail]} {
	    if {[llength $argv] < 3} {tools::usage $errstring}

	    set trail [lindex $argv 1]
	    set argv  [lrange $argv 2 end]

	    if {![string is integer -strict $trail] || ($trail <= 0)} {tools::usage $errstring}
	    continue
	}
	tools::usage $errstring
    }

    if {([llength $argv] < 1) || !$hasext} {tools::usage $errstring}

    if {$outfile != {}} {
	optcheck::outfile $outfile "Output file"
	set outfile [open $outfile w]
    } else {
	set outfile stdout
    }

    # Perform the requested operation.

    foreach file $argv {
	puts $outfile [list $file [MapAFile $outdir $newext $file $trail]]
    }
    return
}

# ------------------------------------------------------

proc ::app-map::MapAFile {outdir newext file trail} {

    set outfile $file
    if {$trail > 0} {
	incr trail -1
	set outfile [eval file join [lrange [file split $outfile] end-$trail end]]
    }

    if {$outdir == {}} {
	set outfile [file rootname $outfile].$newext
    } else {
	if {![string equal relative [file pathtype $outfile]]} {
	    set outfile [eval file join [lrange [file split $outfile] 1 end]]
	}
	set outfile [file join $outdir [file rootname $outfile].$newext]
    }
    if {[string equal $outfile $file]} {
	append outfile .$newext
    }
    return $outfile
}

# ------------------------------------------------------
package provide app-map 0.1
