# -*- tcl -*-
# Help subsystem / generic
# ------------------------------------------------------

package require tools

namespace eval app-help {
    variable help [file join [tools::topdir] data help]
    variable  topics
    array set topics {}
}

# ------------------------------------------------------
# Acceptable syntax for cmdline functionality

set ::app-help::dyntopic(cmdline) {
[call [cmd {@appname@}] [method help]]

Returns a description of the help subcommand, especially the available
topics. This description will be in plain text.

[call [cmd {@appname@}] [method help] [arg topic] [arg format]]

The help for the [arg topic] is written to [const stdout], in the
specified [arg format].
}

proc ::app-help::help {topic} {
    variable dyntopic
    if {[info exists dyntopic($topic)]} {return $dyntopic($topic)}
    return {}
}

# ------------------------------------------------------
# Implementation of cmdline functionality.

proc ::app-help::run {argv} {
    if {([llength $argv] != 2) && ([llength $argv] != 0)} {
	tools::usage "wrong#args: help ?topic format?"
    }
    LoadTopics
    if {[llength $argv] == 0} {
	# Simple help for the 'help subcommand'
	tools::usage "
	[tools::appname] help topic format

	    where _topic_ is one of

[ListTopics]

	    and _format_ one of

	        nroff, html, tmml, latex, wiki, text, or list
	"
    }

    foreach {topic format} $argv break

    # Is the topic valid ? Error out if not.

    variable topics
    if {![info exists topics($topic)]} {
	tools::usage "Unknown help topic \"$topic\""
    }


    # Use cache first before trying to actually convert
    # stuff.

    variable help

    set cached [file join $help cache $format $topic]
    if {[file exists $cached]} {
	tools::copyout $cached
	return
    }

    # Check for an input file. If none is present
    # generate one by asking all application packages
    # in the kit for their help.

    set in [file join $help ${topic}.man]
    if {[file exists $in]} {
	set data [tools::getfile $in]
    } else {
	set data [get_doc $topic]
        catch {tools::putfile $in $data}
    }

    # Convert the input into the requested format,
    # return it to stdout, and cache it too. Catch
    # any errors happening in the caching operation,
    # we may run in a read-only filesystem (not even
    # transparent).

    package require dtglue
    set data [dtglue::cvtstring $format $data "$topic Documentation"]
	
    catch {
	file mkdir [file dirname $cached]
	tools::putfile $cached $data
    }

    puts stdout $data
    return
}

set ::app-help::header {
[comment {-*- tcl -*- doctools manpage}]
[manpage_begin {@appname@} n 1.0]
[moddesc {@appname@}]
[titledesc {@appname@}]
[description]
[list_begin definitions]
}
set ::app-help::trailer {
[list_end]
[manpage_end]
}

proc ::app-help::get_doc {topic} {
    variable header
    variable trailer

    set     data [list]
    lappend data $header

    cd [file join [tools::topdir] lib]
    set cmdlist [list]

    foreach a [lsort [glob -nocomplain -type d app-*]] {
	package require $a
	lappend data [${a}::help $topic]
    }

    lappend data $trailer

    return [string map [list @appname@ [tools::appname]] [join $data \n]]
}

proc ::app-help::LoadTopics {} {
    variable topics
    variable help
    foreach line [split [tools::getfile [file join $help topics]] \n] {
	foreach {topic label} $line break
	set topics($topic) $label
    }
    return
}

proc ::app-help::ListTopics {} {
    variable topics
    set text [list]

    set ml 0
    foreach t [array names topics] {if {[set _ [string length $t]] > $ml} {set ml $_}}

    foreach t [lsort [array names topics]] {
	lappend text "	        $t[string repeat " " [expr {$ml - [string length $t]}]] - $topics($t)"
    }
    return [join $text \n]
}

# ------------------------------------------------------
package provide app-help 0.1
