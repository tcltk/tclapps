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
package require textutil

namespace eval ::dict.leo.org {

    namespace export query askLEO askLEOforSelection

    namespace import ::textutil::adjust

    variable dialog .leo
    variable table     ""
    variable Query     ""
    variable lastQuery ""
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
	    if {[info exists td(1)] && [info exists td(2)]} {
		lappend table [string trim $td(1)] [string trim $td(2)]
	    }
	    set tdcounter 0
	    array unset td
	}

	TD - td {
            incr tdcounter
	    set item [htmlparse::mapEscapes $body]
	    if {[string length $item]} {
		append td($tdcounter) $item
	    }
        }

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
    set tok [::http::geturl $leoURL/?$query]
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
    if {[catch {selection get} query]} {
        set query ""
    }
    askLEO $query
}

proc ::dict.leo.org::askLEO {{query {}}} {

    variable dialog
    variable textwidget
    variable lastQuery
    variable Query
    variable table

    set w $textwidget
    set query [string trim $query]

    if {$query ne ""} {
	set Query $query
    }
    if {$Query ne $lastQuery} {
        set lastQuery $Query
	$w configure -state normal
	$w delete 1.0 end
	$w configure -state disabled
        $w configure -cursor watch
        update
        set table [dict.leo.org::query $Query]
        set max 39
        $w configure -state normal
        set sep [string repeat = $max]
        if {![llength $table]} {
            $w insert end {No matches}
        } else {
            set table [linsert $table 0 " English" " Deutsch" $sep $sep]
        }
        foreach {c1 c2} $table {
            set a1 [split [adjust $c1 -length $max] "\n"]
            set a2 [split [adjust $c2 -length $max] "\n"]
            set indent ""
            foreach l1 $a1 l2 $a2 {
                set l1 $indent$l1
                set l2 $indent$l2
                $w insert end [format "%-*s  %-*s\n" $max $l1 $max $l2]
                set indent "  "
            }
        }
    }
    $w configure -state disabled
    if {![winfo ismapped $dialog]} {wm deiconify $dialog} else {raise $dialog}
    $w configure -cursor ""
}

proc ::dict.leo.org::init {{standalone 0}} {
    variable dialog
    variable textwidget
    variable LEOlogo
    set NS {}
    if {[package vsatisfies [package provide Tk] 8.5]} { set NS ::ttk }

    catch {destroy $dialog}
    image create photo LEOlogo -data $LEOlogo
    
    toplevel $dialog -class AskLEO
    wm title $dialog "askLEO"
    if {$standalone} {
	wm protocol $dialog WM_DELETE_WINDOW exit
    } else {
	wm withdraw $dialog
	wm protocol $dialog WM_DELETE_WINDOW [list wm withdraw $dialog]
    }

    set f [${NS}::frame $dialog.main]
    ${NS}::frame  $f.top
    ${NS}::entry  $f.top.ent -background white -font {Helvetica 12} \
        -textvariable [namespace current]::Query
    ${NS}::button $f.top.but -text "ask LEO" -command [namespace code askLEO]

    grid $f.top.ent $f.top.but -sticky news
    grid columnconfigure $f.top 0 -weight 1

    ${NS}::frame $f.bot
    ${NS}::scrollbar $f.bot.vs -command [list $f.bot.text yview]
    ${NS}::scrollbar $f.bot.hs -orient horizontal -command [list $f.bot.text xview]
    set textwidget [text $f.bot.text -wrap no -font {{Dejavu Sans Mono} -14} -state disabled \
                        -yscrollcommand [list $f.bot.vs set] \
                        -xscrollcommand [list $f.bot.hs set]]
    grid $f.bot.text $f.bot.vs -sticky news
    grid $f.bot.hs   -         -sticky ew
    grid rowconfigure $f.bot 0 -weight 1
    grid columnconfigure $f.bot 0 -weight 1

    grid $f.top -sticky ew -padx 1 -pady 1
    grid $f.bot -sticky news  -padx 1 -pady 1
    grid rowconfigure $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1

    grid $f -sticky news
    grid rowconfigure $dialog 0 -weight 1
    grid columnconfigure $dialog 0 -weight 1

    bind $f.top.ent <Return> [list $f.top.but invoke]
    bind $f.bot.text <Button-2> [namespace code askLEOforSelection]
    if {!$standalone} {
	bind $dialog <Double-Button-3> [list wm withdraw $dialog]
    }
    focus $f.top.ent
}

proc ::dict.leo.org::show {} {
    variable dialog
    if {![winfo exists $dialog]} {init}
    toplevel $dialog.icon -background ""
    pack [label $dialog.icon.l -image LEOlogo]
    wm iconwindow $dialog $dialog.icon
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

package provide askleo 1.1

if {$argv0 eq [info script]} {
    # We're running standalone
    package require askleo
    ::dict.leo.org::init 1
    wm withdraw .
}
