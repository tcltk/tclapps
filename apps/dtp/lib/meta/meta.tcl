# -*- tcl -*-
# Handling of meta data (pretty printing, conversion)
# ------------------------------------------------------

package require tools

namespace eval ::meta {}

# ------------------------------------------------------
namespace eval ::meta {
    variable mpmeta ""
}

proc ::meta::read {file} {
    variable mpmeta ""
    set ip [interp create -safe]
    interp alias $ip manpage {} ::meta::MP
    $ip eval [tools::getfile $file]
    interp delete $ip
    return $mpmeta
}

proc ::meta::MP {data} {
    variable mpmeta
    lappend mpmeta $data
    return
}

# ------------------------------------------------------

namespace eval ::meta {
    variable off
    foreach {k n} {
	shortdesc 1
	keywords  2
	seealso   3
	section   3
	version   3
	module    4
	title     5
	path      6
	file      6
	desc      6
	fid       7
    } {
	set off([list $k]) $n
    }
}

proc ::meta::pretty {data} {
    variable off
    set res [list]

    foreach line [split $data \n] {
	if {$line == {}} continue
	foreach {__ kv} $line break
	array set t $kv
	lappend res "manpage \{"
	foreach k [lsort [array names t]] {
	    set kl [list $k]
	    lappend res "    $kl[string repeat " " $off($kl)] [list $t($k)]"
	}
	lappend res "\}"
	unset t
    }
    return [join $res \n]
}

# ------------------------------------------------------

proc ::meta::2xref {data iomap} {
    set res [list]

    array set _ $iomap
    set hasindex [info exists _(_index_)]
    unset _

    array set keys {}

    foreach item $data {
	if {$item == {}} continue
	array set __ $item

	# Cross-references ... File based, see-also

	foreach {symfile ___ ___} [SymFile $item] break

	lappend res [list sa,${__(title)}               $symfile]
	lappend res [list sa,${__(title)}($__(section)) $symfile]
	lappend res [list ${__(title)}                  $symfile]
	lappend res [list ${__(title)}($__(section))    $symfile]

	if {$hasindex} {
	    # Store an inverted file - keyword relationship
	    foreach k $__(keywords) {
		lappend keys($k) $symfile
	    }
	}
    }

    if {$hasindex} {
	set i 0
	foreach k [lsort [array names keys]] {
	    lappend res [list kw,$k _index_ key$i]
	    lappend res [list $k    _index_ key$i]
	    incr i
	}
    }

    return $res
}

proc ::meta::2docidx {data title desc} {

    array set keys {}
    foreach item $data {
	if {$item == {}} continue
	foreach {symfile ___ ___} [SymFile $item] break
	array set __ $item

	# Store inverted file - keyword relationship
	foreach k $__(keywords) {
	    lappend keys($k) [list $symfile $__(title)]
	}
    }

    # Generate index from collected information

    set     res [list]
    lappend res "\[index_begin [list $title $desc]\]"

    foreach k [lsort [array names keys]] {
	lappend res "  \[key [list $k]\]"
	foreach v [lsort -index 0 $keys($k)] {
	    foreach {file label} $v break
	    lappend res "    \[manpage [list $file $label]\]"
	}
    }

    lappend res "\[index_end\]"
    return [join $res \n]
}

# ------------------------------------------------------

proc ::meta::2doctoc {data title desc} {
    set res [list]

    lappend res "\[toc_begin [list $title $desc]\]"

    foreach item $data {
	if {$item == {}} continue
	foreach {symfile label desc} [SymFile $item] break
	lappend res "\[item [list $symfile $label $desc]\]"
    }

    lappend res "\[toc_end\]"
    return [join $res \n]
}

# ------------------------------------------------------

proc ::meta::SymFile {kv} {
    array set _ $kv
    if {[info exists _(path)]} {
	set symfile $_(path)
    } elseif {[info exists _(file)]} {
	set symfile $_(file)
    } else {
	set symfile $_(fid)
    }
    set desc  $_(desc)
    set label $_(title)
    return [list $symfile $label $desc]
}


# ------------------------------------------------------

proc ::meta::map {fmap mapfile} {
    package require tools

    array set tmp $fmap

    foreach {in out} [tools::readmap $mapfile] {
	if {[info exists tmp($in)]} {
	    set tmp($tmp($in)) $out
	}
	set tmp($in)  $out
    }

    return [array get tmp]
}

# ------------------------------------------------------
package provide meta 0.1
