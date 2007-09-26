# Sample plugin.
#
# Add a clock to the statusbar displaying the time in the New Orleans
# for the Tcl 2007 conference

namespace eval ::tkchat::nola {
    variable TZ -5 ;# New Orleans Summer Time is GMT - 5
}

proc ::tkchat::nola::Init {} {
    variable ::tkchat::NS
    if {[winfo exists .status] && ![winfo exists .status.nola]} {
        ${NS}::label .status.nola -font FNT -pad 1
        ::tkchat::StatusbarAddWidget .status .status.nola 1
        if {[package provide tooltip] ne {}} {
            tooltip::tooltip .status.nola "Current time in New Orleans"
        }
        after idle [list [namespace origin Tick] 1000]
    }
}

proc ::tkchat::nola::Tick {interval} {
    variable TZ
    set t [expr {[clock seconds] + (3600 * $TZ)}]
    set txt [clock format $t -format "NOLA: %H:%M:%S" -gmt 1]
    .status.nola configure -text $txt
    variable afterid [after $interval [info level 0]]
}

::tkchat::Hook add init ::tkchat::nola::Init