# Windows taskbar support.
# At some point I want to support multiple icons for nochat/chat/alert.
#

# TODO: deal with IncrMessageCOunter and ResetMessageCounter  and detach bookmark

if {[tk windowingsystem] ne "win32"} { return }
if {[catch {
    package require Winico
}]} {
    return
}

namespace eval ::tkchat::winico {
    variable version 1.0.0
}

proc ::tkchat::winico::InitHook {} {
    variable TaskbarIcon
    variable WinicoWmState [wm state .]

    set icofile [file join [file dirname [info script]] tkchat.ico]
    if {[file exists $icofile]} {
        set TaskbarIcon [winico createfrom $icofile]
        winico taskbar add $TaskbarIcon \
            -pos 0 \
            -text [wm title .] \
            -callback [list [namespace origin Callback] %m %i]
        bind . <Destroy> [namespace origin Cleanup]
    }
}

proc ::tkchat::winico::Update {} {
    variable ::tkchat::MessageCounter
    variable TaskbarIcon

    if {[llength [info commands winico]] < 1} { return }
    if { $MessageCounter > 0 } {
	winico taskbar modify $TaskbarIcon \
		-pos 2 \
		-text "$MessageCounter - Tcl'ers chat"
    } else {
	winico taskbar modify $TaskbarIcon \
		-pos 0 \
		-text "Tcl'ers chat"
    }
}

proc ::tkchat::winico::Cleanup {} {
    variable TaskbarIcon
    winico taskbar delete $TaskbarIcon
}

proc ::tkchat::winico::Callback {msg icn} {
    variable WinicoWmState
    switch -exact -- $msg {
	WM_LBUTTONDOWN {
	    if { [wm state .] eq "withdrawn" } {
		wm state . $WinicoWmState
		wm deiconify .
		focus .eMsg
	    } else {
		set WinicoWmState [wm state .]
		wm withdraw .
	    }
	}
    }
}

# -------------------------------------------------------------------------
::tkchat::Hook add init ::tkchat::winico::InitHook
package provide tkchat::winico $::tkchat::winico::version
# -------------------------------------------------------------------------
