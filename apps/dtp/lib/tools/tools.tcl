# -*- tcl -*-
# Misc. tools used throughout the code.
# ------------------------------------------------------

namespace eval ::tools {
    variable appname ""
    variable topdir  ""
}

# ------------------------------------------------------

proc ::tools::topdir= {path} {
    variable  topdir
    set       topdir    $path
    appname= [file tail $path]
    return
}

proc ::tools::appname= {name} {
    variable  appname
    set       appname $name
    return
}

proc ::tools::appname {} {
    variable  appname
    return   $appname
}

proc ::tools::topdir {} {
    variable  topdir
    return   $topdir
}

# ------------------------------------------------------

proc ::tools::usage {text} {
    global        argv0
    puts stderr "[appname]: $text"
    exit 1
}

proc ::tools::internalerror {} {
    global       errorInfo
    puts stderr "[appname] internal error"
    puts stderr $errorInfo
    exit 1
}

# ------------------------------------------------------

proc ::tools::getfile {path} {
    set fh   [open $path r]
    set data [read $fh]
    close $fh
    return $data
}

proc ::tools::putfile {path data} {
    set              fh [open $path w]
    puts -nonewline $fh $data
    close           $fh
    return
}

proc ::tools::copyout {path {chan stdout}} {
    set    fh [open $path r]
    fcopy $fh $chan
    close $fh
    return
}

# ------------------------------------------------------

proc ::tools::outputfile {outdir format file} {
    if {$outdir == {}} {
	set outfile [file rootname $file].$format
    } else {
	set outfile [file join $outdir [file rootname $file].$format]
    }
    if {[string equal $outfile $file]} {
	if {$outdir == {}} {
	    set outfile $file.$format
	} else {
	    set outfile [file join $outdir $file.$format]
	}
    }
    return $outfile
}

proc ::tools::outputmap {outdir format files} {
    set iomap [list]
    foreach file $files {
	lappend iomap $file [outputfile $outdir $format $file]
    }
    return $iomap
}


proc ::tools::readmap {mapfile} {
    set result [list]
    foreach line [split [tools::getfile $mapfile] \n] {
	set line [string trim $line]
	if {$line == {}} continue
	if {[catch {foreach {in out} $line break}]} continue
	lappend result $in $out
    }
    return $result
}

proc ::tools::link {here dest} {
    # Ensure that the link is properly done relative to this file!

    set save $dest

    set here [file split $here]
    set dest [file split $dest]
    while {[string equal [lindex $dest 0] [lindex $here 0]]} {
	set dest [lrange $dest  1 end]
	set here [lrange $here 1 end]
	if {[llength $dest] == 0} {break}
    }
    set ul [llength $dest]
    set hl [llength $here]

    if {$ul == 0} {
	set dest [lindex [file split $save] end]
    } else {
	while {$hl > 1} {
	    set dest [linsert $dest 0 ..]
	    incr hl -1
	}
	set dest [eval file join $dest]
    }

    return $dest
}

# ------------------------------------------------------
package provide tools 0.1
