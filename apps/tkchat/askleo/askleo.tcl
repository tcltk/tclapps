#############################################################
#
# askLEO
#
# This is a web scraper for English/German
# translation via http://dict.leo.org
#
# Translation is invoked on the current X selection
# (needn't be inside of tkChat) via <Shift-Button3>.
#
# Authors:
#
# 2002 - Reinhard Max	  <Reinhard.Max@gmx.de>
#	 Martin Scherbaum <maddin@scherbaum.org>
#
#############################################################

package require Tk
package require http
package require htmlparse

namespace eval ::dict.leo.org {

    namespace export query askLEO askLEOforSelection

    variable table ""
    variable last  ""
    variable Query ""
    variable td
    variable tdcounter 0
    variable leoURL http://pda.leo.org
}

proc ::dict.leo.org::parse {tag close options body} {
    variable td
    variable table
    variable tdcounter

    switch -- $close$tag {

	/TR - /tr {
	    if {[info exists td(2)] && [info exists td(3)]} {
		lappend table [string trim $td(2)] [string trim $td(3)]
	    }
	    set tdcounter 0
	    array unset td
	}

	td - td { incr tdcounter }

	default {
	    set item [htmlparse::mapEscapes $body]
	    if {[string length $item]} {
		append td($tdcounter) $item
	    }
	}
    }
}

proc ::dict.leo.org::query {query} {
    variable table
    variable leoURL
    set enc [::http::config -urlencoding]
    ::http::config -urlencoding iso8859-15
    set query [::http::formatQuery search $query]
    ::http::config -urlencoding $enc
    set tok [::http::geturl $leoURL -query $query]
    foreach line [split [::http::data $tok] "\n"] {
	if {[string match "*ENGLISCH*DEUTSCH*" $line]} break
    }
    ::http::cleanup $tok
    set table ""
    ::htmlparse::parse -cmd ::dict.leo.org::parse $line
    return $table
}

proc ::dict.leo.org::max {a b} {expr {$a > $b ? $a : $b}}

proc ::dict.leo.org::askLEOforSelection {} {
    if {![catch {selection get} query]} {
	askLEO $query
    }
}

proc ::dict.leo.org::askLEO {{query {}}} {

    variable w
    variable last
    variable Query
    set query [string trim $query]
    if { $query ne "" } {
	set Query $query
    }
    if { $Query ne $last } {
	$w.bot.text configure -state normal
	$w.bot.text delete 1.0 end
	$w.bot.text configure -state disabled
	if {$Query != ""} {
	    $w.bot.text configure -cursor watch
	    update
	    set table [dict.leo.org::query $Query]
	    set max 0
	    foreach c $table {set max [max $max [string length $c]]}
	    $w.bot.text configure -state normal
	    if {$max} {
		set sep [string repeat = $max]
		set table [linsert $table 0 " English" " Deutsch" $sep $sep]
		foreach {c1 c2} $table {
		    $w.bot.text insert end \
			[format "%-*s  %-*s\n" $max $c1 $max $c2]
		}
	    } else {
		$w.bot.text insert end {No matches}
	    }
	    $w.bot.text configure -state disabled
	    if {![winfo ismapped $w]} {wm deiconify $w} else {raise $w}
	    $w.bot.text configure -cursor ""
	}
    }
    set last $Query
}

proc ::dict.leo.org::init {} {
    variable w .leo
    variable LEOlogo
    catch {destroy $w}
    toplevel $w
    wm withdraw $w
    bind $w <Double-Button-3> [list wm withdraw $w]
    wm protocol $w WM_DELETE_WINDOW [list wm withdraw $w]
    frame $w.main
    frame  $w.top
    entry  $w.top.ent -background white -textvariable [namespace current]::Query
    button $w.top.but -text "ask LEO" -command [namespace code askLEO]
    bind   $w.top.ent <Return> [list $w.top.but invoke]

    pack $w.top.ent -expand yes -fill x -side left
    pack $w.top.but -expand no -fill none -side left
    pack $w.top	    -fill x -in $w.main

    frame $w.bot
    scrollbar $w.bot.scry -command [list $w.bot.text yview]
    scrollbar $w.bot.scrx -orient horizontal -command [list $w.bot.text xview]
    text $w.bot.text -wrap no -font fixed -state disabled \
	-yscrollcommand [list $w.bot.scry set] -xscrollcommand [list $w.bot.scrx set]
    grid $w.bot.text -row 0 -column 0 -sticky nsew
    grid $w.bot.scry -row 0 -column 1 -sticky ns
    grid $w.bot.scrx -row 1 -column 0 -sticky ew
    grid rowconfigure $w.bot 0 -weight 1
    grid columnconfigure $w.bot 0 -weight 1
    bind $w.bot.text <Button-2> [namespace code askLEOforSelection]
    #pack $w.bot.text -expand yes -fill both -side right
    pack $w.bot -expand yes -fill both -in $w.main

    pack $w.main -expand yes -fill both
    focus $w.top.ent
    wm title $w "askLEO"

    if { $::tcl_platform(platform) ne "windows" } {
	image create photo LEOlogo -data $LEOlogo
	toplevel $w.icon -background ""
	pack [label $w.icon.l -image LEOlogo]
	wm iconwindow $w $w.icon
    }
}

set ::dict.leo.org::LEOlogo {
    R0lGODlhKAAoAPQUAAAAAAAAMwAzAAAzMzMAADMAM2YAADMz/8wAM/8AM8zMAMzMM8z/AMz/
    M//MAP/MM///AP//M///7v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEBABQALAAAAAAoACgAAAX+IEVRFEVRFEVR1AEAFEVRFEVRFEUZAEWBFEVRFEVRFEVR
    FEVRFEVR1DEhAEBRFAVSFEVRBgAAAEVRFEVRFEVRFEVRFEVRB3gcyHEAFEVRFEVRFAAAAAAA
    FEVRFEVRIEVRFEVR1HEcE3IcE0BRFEVRlAEAAACAAABQFEVRFEVRFEVRx3EcyIQcBwIAFAVS
    FEUZAAAAAAAAAEBRFEVRFEVRCHIcBzhNxzEhE0BRFEUZRgIAAAAAAAAAAEVRIEVRxzEhxzEh
    xzEhyAEAFEVRhpEkCQCAAAAAAAAAAEVRx3FME3JMCDJNx3EAAAVSlGEkSZIkCQAAAAAAAABQ
    x3Ecx4T+HBOITNNxHNMEAJRhJEmSJEmSJAAAAAAAAOBxHMdxTBMyAQUwIRMCFABQGEmSJEmS
    gEmSJAAAAACATNOEIMiETIXzAAiCFA4AFg4QPEkCFEmSJEmSAABAIQiCTNM0TRMIOAExBYTy
    OE7jOAVQAA6QJEmSJEkCUCB1HMd0IMiEBM5ABI4TAYHjBITzAAUUgEmSJEmSJAlFHdM0Icg0
    HUTgPEDkAI8DRs7jDE7gPMCTJEmSJAlFUcdxIMg0IQcYOIATOYETEM7jBIDjRMDzPEmSJElC
    USBFIQhyTAhyEE7kOM4DTFPgCNMUOMHzgM/zPEmSUBRFUQeCHBNyBM7jPMr+4wTUBAIPME2E
    4wTP8zzP8yQURVEUdRzThBAO+AiP4zgRUACFExBB4AzE8zzP8zwURVEgRVHUMSFH4ASO80CP
    4wCFAz0AAT3A84DP8zzPQ1EURVEURSHIARTQoziP4jxOBCqP4zwOQBXU8zzPQ1EURVEURVHI
    QVEA+ACR4zxA5DiP8zCP8wAPQFHPQ1EURVEgRVEURVEUVQCO80CPEziP4ziPEzmPE4AUAVAU
    RVEURVEURVEUFTiR8yjA4zgBADqN4yjO40BOQFEURVEURVEURVEURREA+DhO4DgP5DxO4DyR
    owRO4AQURVEURVEgRVEURVGBEwGPMzgPFAAB5AT+BFAAkeOAEUFRFEVRFEVRFEVRhPM4DvQ4
    QOA4jgM+jhMAheM8ABAQFEVRFEVRFEVRVAA4D+gEzuM4QVBAj/NAgfM40OMUFEVRFEVRIEVR
    FEVRgQMETuA4keNAgRM5TuBAi/OAAEVRFEVRFEVRFEVRhBNATuQEwOM8jgMGQAA4AREQAUVR
    FEVRFEVRFEVRVEA8Dhg4jhMBBfA4zuNEzuNAAUVRFEVRFEVRIEVRFEVRARQ4zuM4D+QETuQ4
    geM8DkCBFEVRFEVRFEVRFEVRBFA4zgM9gPMoTgACAeEEQEBQFEVRFEVRFEVRFEVRVOBAD+hE
    CvA4DxA5jvNAjwMFFEXiURRFURRFUSBFURRFEQ/wKM7jOJETOM4DPYDzOE5AgRRFURRFURRF
    URRFUUBAAU7gPJDyOM4DOovzAE7gRABFURRFURRFURRFUVQBUAXoRIDzAM4DBY4TOJHjBE5B
    URRFURRFUSBFURRFURRFBU7hBM4DBY4zEI7zAFQAgBRFURRFURRFURRFURRFUURAAYUDBcQD
    AhQVOBFAURRFURRFURRFURRFURRFURRIARQlPABFARRFUQFBURRFURRFURRFUSBFURRFURRF
    URRFUQVFURRFURRFURRFgRRFURRFURQVAgA7
}

::dict.leo.org::init

package provide askleo 1.0