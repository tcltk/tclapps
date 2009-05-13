# Tkchat plugin that listens for power announcements and suspend changes.
#
# We should record the suspension time so that on resume we can decide
# if we should override the history reload and send a presence notification
# that we are offline.
#
# Requires the winpm extension from http://tkwinpm.googlecode.com/

if {[catch {
    package require winpm
}]} {
    return
}
namespace eval ::tkchat::power {
    variable version 1.0.0
    variable suspended 0
}

proc ::tkchat::power::OnSuspend {} {
    variable suspended [clock seconds]
    ::tkchat::addSystem .txt "system suspended"
    catch {::tkjabber::away "system going to sleep" offline}
}

proc ::tkchat::power::OnResume {} {
    global Options
    variable suspended
    set msg "system resumed"
    if {$Options(HistoryLines) != 0 && ([clock seconds] - $suspended) > 300} {
        set ::tkjabber::HaveHistory 0
        append msg ", enabling history refetch on reconnection"
    }
    tkchat::addSystem .txt $msg
    catch {::tkjabber::back "system resumed" online}
}

proc ::tkchat::power::InitHook {} {
    winpm bind PBT_APMSUSPEND [list [namespace origin OnSuspend]]
    winpm bind PBT_APMRESUMESUSPEND [list [namespace origin OnResume]]
}

# -------------------------------------------------------------------------
::tkchat::Hook add init ::tkchat::power::InitHook
package provide tkchat::power $::tkchat::power::version
# -------------------------------------------------------------------------
