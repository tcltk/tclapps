# -*- tcl -*-
# Misc. tools used throughout the code.
# ------------------------------------------------------

package require tools

if {[catch {
    package require doctools
    package require doctools::toc
    package require doctools::idx
}]} {
    tools::internalerror
}

namespace eval ::dtglue {}

# ------------------------------------------------------

proc ::dtglue::hasXref {format} {
    if {[catch {
	::doctools::new dtest -format $format
	dtest configure -deprecated 1
    } msg]} {
	tools::usage "Unknown doctools format $format"
    }

    foreach k [dtest parameters] {set known($k) .}
    set res [info exists known(xref)]
    dtest destroy
    return $res
}


proc ::dtglue::Setup {object format {para {}}} {
    if {[catch {
	::doctools::new $object -format $format
	$object configure -deprecated 1
    } msg]} {
	tools::usage "Unknown doctools format $format"
    }
    if {$para != {}} {
	foreach k [$object parameters] {set known($k) .}
	foreach {k v} $para {
	    if {![info exists known($k)]} {
		tools::usage "Unknown format parameter \"$k\""
	    }
	    $object setparam $k $v
	}
    }
    return
}

proc ::dtglue::cvtstring {format data {fileinfo {}}} {
    Setup dt $format
    if {[catch {
	dt configure -file $fileinfo
	set result [dt format $data]
    }]} {
	tools::internalerror
    }

    dt destroy
    return $result
}

# ------------------------------------------------------

proc ::dtglue::cvtfiles {format iomap para subst} {
    Setup dt $format $para
    foreach {sym actual} $iomap {dt map $sym $actual}

    #array set ns $navspec
    array set pa $para
    array set su $subst

    foreach {in out} $iomap {
	if {![file exists $in]} {continue}

	puts stderr "    Processing $in ..."

	dt configure -file $in

	if {0} {
	    set hdr ""
	    if {[info exists pa(header)]} {append hdr $pa(header)}
	    if {[info exists ns($in)]}    {append hdr $ns($in)}
	    dt setparam header $hdr
	}

	if {[array size su] > 0} {
	    # Clean starting point
	    foreach p [array names pa] {
		dt setparam $p $pa($p)
	    }
	    # Templating of variables.
	    if {[llength [array names su *,$in]] > 0} {
		foreach k [array names su *,$in] {
		    foreach {para __} [split $k ,] break
		    dt setparam $para [string map $su($k) $pa($para)]
		}
	    }
	}

	if {[catch {
	    set data   [dt format [tools::getfile $in]]
	    file mkdir [file dirname $out]
	    tools::putfile $out $data
	}]} {
	    tools::internalerror
	}
    }

    dt destroy
    return
}

# ------------------------------------------------------

proc ::dtglue::getmeta {files} {
    if {[catch {
	::doctools::new dt -format list -deprecated 1
    }]} {
	tools::internalerror
    }

    set meta ""
    foreach f $files {
	if {![file exists $f]} {continue}

	if {[catch {
	    dt configure -file $f

	    # Extract basic meta information form manpage.
	    # Also add full path of input file as meta
	    # information before collecting it as part of
	    # the final result.

	    foreach {__ fmeta} [string trim [dt format [tools::getfile $f]]] break
	    lappend fmeta path $f

	    append meta [list manpage $fmeta]\n
	}]} {
	    tools::internalerror
	}
    }

    dt destroy
    return $meta
}

# ------------------------------------------------------

proc ::dtglue::TocSetup {object format para} {
    if {[catch {
	::doctools::toc::new $object -format $format
    } msg]} {
	tools::usage "Unknown doctoc format $format"
    }
    if {$para != {}} {
	foreach k [$object parameters] {set known($k) .}
	foreach {k v} $para {
	    if {![info exists known($k)]} {
		tools::usage "Unknown format parameter \"$k\""
	    }
	    $object setparam $k $v
	}
    }
    return
}

proc ::dtglue::cvttoc {format data map para infile} {
    TocSetup dtoc $format $para
    dtoc configure -file $infile

    foreach {sym actual} $map {dtoc map $sym $actual}

    if {[catch {set data [dtoc format $data]}]} {
	tools::internalerror
    }

    dtoc destroy
    return $data
}

proc ::dtglue::cvttocfiles {format outdir iomap fmap para} {
    TocSetup dt $format $para

    foreach {sym actual} $fmap {dt map $sym $actual}

    if {$outdir != {}} {
	file mkdir $outdir
    }
    foreach {in out} $iomap {
	if {[catch {
	    set data [tools::getfile $in]
	    set data [dt format $data]
	    file mkdir [file dirname $out]
	    tools::putfile $out $data
	}]} {
	    tools::internalerror
	}
    }

    dt destroy
    return
}

# ------------------------------------------------------

proc ::dtglue::IdxSetup {object format para} {
    if {[catch {
	::doctools::idx::new $object -format $format
    } msg]} {
	tools::usage "$msg\nUnknown docidx format $format"
    }
    if {$para != {}} {
	foreach k [$object parameters] {set known($k) .}
	foreach {k v} $para {
	    if {![info exists known($k)]} {
		tools::usage "Unknown format parameter \"$k\""
	    }
	    $object setparam $k $v
	}
    }
    return
}

proc ::dtglue::cvtidx {format data map para infile} {
    IdxSetup didx $format $para
    didx configure -file $infile

    foreach {sym actual} $map {didx map $sym $actual}

    if {[catch {set data [didx format $data]}]} {
	tools::internalerror
    }

    didx destroy
    return $data
}

proc ::dtglue::cvtidxfiles {format outdir iomap fmap para} {
    IdxSetup dt $format $para

    foreach {sym actual} $fmap {dt map $sym $actual}

    if {$outdir != {}} {
	file mkdir $outdir
    }
    foreach {in out} $iomap {
	if {[catch {
	    set data [tools::getfile $in]
	    set data [dt format $data]
	    file mkdir [file dirname $out]
	    tools::putfile $out $data
	}]} {
	    tools::internalerror
	}
    }

    dt destroy
    return
}

# ------------------------------------------------------
package provide dtglue 0.1
