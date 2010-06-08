# -*- tcl -*-
# Generation (Conversion subsystem)
# ------------------------------------------------------

package require tools
package require optchecker
package require dtglue

namespace eval app-gen-doc {}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-gen-doc::help(cmdline) {
[call [cmd {@appname@}] [method gen-doc] [opt [arg options...]] [arg format] [arg iomap] [opt [arg meta]]]

Processes all input files listed in [arg iomap] and converts into the
requested output [arg format]. The results are written to the output
files listed in [arg iomap]. If [arg meta] is present the meta
information containied in it is read and used to create
cross-references between the manpages, if supported by the engine for
the chosed output [arg format].

[nl]

The conversion can be influenced by the options listed below.

[list_begin definitions]

[lst_item "[option -imgmap] [arg file]"]

This option is used to set the mapping of image input files
contained in the [arg file].

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

[lst_item "[option -subst] [arg varname] [arg marker] [arg path]"]

When used multiple times the data accumulates, in contrast to the
other options, where the last occurence is dominant.

[nl]

It declares that the data for the engine parameter [arg varname]
contains a variable link, that all locations for this link are marked
by the string [arg marker], and that the link has to refer to the
output file [arg path]. Based upon this information the formatter
will, for each output file in the [arg iomap], replace the marker with
a proper link to the [arg path], based upon the relative locations of
the current file and the file linked to, i.e. [arg path].

[nl]

An occurence of this option for a variable [arg varname] is ignored if
no engine parameters for [arg varname] were specified via either
[option -varfile] or [option -varstring].

[list_end]
}

proc ::app-gen-doc::help {topic} {
    variable help
    if {[info exists help($topic)]} {return $help($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-gen-doc::run {argv} {
    set errstring "wrong#args: gen-doc ?-imgmap imapfile? ?-varstring var string? ?-varfile var file? ?-subst var mark link? format iomap ?meta?"

    if {[llength $argv] < 2} {tools::usage $errstring}

    set imapfile ""
    set mapfile ""
    array set para {}
    set subst {}

    while {[string match -* [lindex $argv 0]]} {
	if {[string equal [lindex $argv 0] -imgmap]} {
	    if {[llength $argv] < 4} {tools::usage $errstring}

	    set imapfile [lindex $argv 1]
	    set argv     [lrange $argv 2 end]
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
	if {[string equal [lindex $argv 0] -subst]} {
	    if {[llength $argv] < 6} {tools::usage $errstring}

	    set var  [lindex $argv 1]
	    set mark [lindex $argv 2]
	    set link [lindex $argv 3]
	    set argv [lrange $argv 4 end]

	    lappend subst $var [list $mark $link]
	    continue
	}
	tools::usage $errstring
    }

    if {[llength $argv] < 2} {tools::usage $errstring}
    if {[llength $argv] > 3} {tools::usage $errstring}

    set                   format  [lindex $argv 0]
    optcheck::infile [set mapfile [lindex $argv 1]] "Mapping file"

    set imap {}
    if {$imapfile != {}} {
	optcheck::infile $imapfile "Image Mapping file"
	set imap [tools::readmap $imapfile]
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

	set iomap [tools::readmap $mapfile]

	set para(xref) [::meta::2xref [::meta::read $metafile] $iomap]
    }

    if {$iomap == {}} {set iomap [tools::readmap $mapfile]}

    if {[llength $subst] > 0} {
	# Build the per-page substitution information, skip
	# substitutions for undefined files and parameters.

	array set _ {}
	foreach {var val} $subst {
	    foreach {mark link} $val break

	    array set __ $iomap
	    foreach k [array names __] {
		if {![file exists $k]} {continue}
		if {![info exists para($var)]} {continue}
		lappend _($var,$k) $mark [set ref [::tools::link $__($k) $link]]
	    }
	}
	set subst [array get _]
	unset __ _
    }

    dtglue::cvtfiles $format $iomap $imap [array get para] $subst
    return
}

# ------------------------------------------------------
package provide app-gen-doc 0.1
