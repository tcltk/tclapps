# MacOS X support
#
# Use the Growl notification system on Mac's to alert the user of new
# messages via popup message and dock icon change.

#if {[tk windowingsystem] ne "aqua"} { return }

package require msgcat

namespace eval ::tkchat::aqua {
    variable version 1.0.0
    namespace import ::msgcat::mc
}

proc ::tkchat::aqua::InitHook {} {
    variable growl [expr {![catch {package require tclgrowl}]}]
    variable tkdock [expr {![catch {package require tkdock}]}]

    if {$tkdock && [info exists starkit::topdir]} {
        variable Resources
        set MacOS [file dirname $starkit::topdir]
        set Resources [file dirname $MacOS]/Resources
        # tkdock sites under lib beside tkchat.kit in the app bundle
        lappend auto_path $MacOS/lib
        bind all <FocusIn> +::tkdock::origIcon
    }
    InitAlertMenu
}

proc ::tkchat::aqua::AlertHook {w raise nick msg} {
    global Options
    variable growl
    variable tkdock
    variable Resources
    if {$raise && $growl && $Options(Alert,GROWL)} {
        growl::message TkChat "Message from $nick" $msg
    }
    if {$tkdock} {
        ::tkdock::switchIcon $Resources/tkchat_warn.icns
    }
}

# Hook the messages. If a message arrives and we are not focused, the
# message counter is incremented and we ammend the dock icon here.
# PT: is this correct, resetting the origIcon on missed messages?
proc ::tkchat::aqua::MessageHook {nick msg msgtype args} {
    variable tkdock
    if {$tkdock && [focus] eq {} && $msgtype ne "TRAFFIC"} {
        ::tkdock::origIcon
    }
}

# Update the Alert menu with MacOS specific entries
proc ::tkchat::aqua::InitAlertMenu {} {
    global Options
    variable growl
    variable menuDone
    if {[info exists menuDone]} { return }
    if {!$growl} { return }
    if {![winfo exists .mbar.alert]} { return }

    # Insert our items just before the final separator (or append)
    set end [.mbar.alert index end]
    for {set ndx $end} {$ndx >= 0} {incr ndx -1} {
        if {[.mbar.alert type $ndx] eq "separator"} { break }
    }
    if {$ndx < 0} {set ndx end}
    
    # If growl is unavailable then disable the menu item.
    set state "disabled"
    if {$growl && [growl::available]} {
        set state "normal"
    }

    foreach {tag text state} [list \
                            GROWL "&Use Growl for alerts" $state \
                            BOUNCE "Bou&nce icon on alert" normal] {
        foreach {label charindex} [tk::UnderlineAmpersand $text] break
	.mbar.alert insert $ndx checkbutton \
            -label [mc $label] -underline $charindex \
            -variable Options(Alert,$tag) \
            -onvalue 1 -offvalue 0 -state $state
        incr ndx
    }
    set menuDone 1
}

# -------------------------------------------------------------------------
::tkchat::Hook add init ::tkchat::aqua::InitHook
::tkchat::Hook add alert ::tkchat::aqua::AlertHook
::tkchat::Hook add message ::tkchat::aqua::MessageHook
package provide tkchat::aqua $::tkchat::aqua::version
# -------------------------------------------------------------------------
