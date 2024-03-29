if {[package vsatisfies [package provide Tcl] 8.7-]} return

namespace eval compat86 {
    proc noop {args} {}
}

# provide fake [tk systray] and [tk sysnotify]
namespace ensemble configure ::tk -map [dict merge \
    [namespace ensemble configure ::tk -map] \
    [dict create systray ::compat86::noop sysnotify ::compat86::noop]]

# provide fake [wm iconbadge]
rename ::wm ::compat86::wm
proc ::wm {cmd args} {
    if {$cmd eq "iconbadge"} {
        return
    }
    tailcall ::compat86::wm $cmd {*}$args
}

# provide lpop; from the original ::tkchat::Pop, but the default index
# is "end" instead of 0.
#  Pop the nth element off a list. Used in options processing.
proc lpop {varname {nth end}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

# provide a version of [dict getwithdefault] on 8.6
proc ::tcl::dict::getwithdefault {dict args} {
    if {[llength $args] < 2} {
	return -code error "wrong # args:\
	    should be \"dict getwithdefault dictionary\
	    ?key ...? key default\""
    }
    ::set default [lindex $args end]
    ::set keys [lrange $args 0 end-1]
    if {[exists $dict {*}$keys]} {
	return [get $dict {*}$keys]
    } else {
	return $default
    }
}
namespace ensemble configure ::dict -map [dict merge \
    [namespace ensemble configure ::dict -map] \
    [dict create getwithdefault ::tcl::dict::getwithdefault\
	getdef ::tcl::dict::getwithdefault]]

# HTTP Stuff
namespace eval ::http {}
proc ::http::responseInfo {tok} {
    # fake only the values we are using
    set charset [set [set tok](charset)]
    set url     [set [set tok](url)]
    return [dict create charset $charset url $url]
}

# don't bother convincing 8.6 and tclxml to process emojis
namespace eval ::sgml {}
proc ::sgml::PCDATA {opts pcdata} {
    array set options $opts

    if {$options(-ignorewhitespace) &&
	![string length [string trim $pcdata]]
    } then {
	return {}
    }

    uplevel #0 [list {*}$options(-characterdatacommand) $pcdata]
}

# new roster stuff.
# this should be conditional on Tk version, not Tcl's, but up to
# 8.6 both versions match
namespace eval ::newRoster {
    variable cl ::compat86::noop
}
proc ::newRoster::PutIntoPane {args} {}
proc ::newRoster::updateOnlineNames {args} {}
proc ::newRoster::SetUserTooltip {args} {}
