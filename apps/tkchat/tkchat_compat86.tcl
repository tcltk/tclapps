if {[package vsatisfies [package require Tcl] 8.7-]} return

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
    [dict create getwithdefault ::tcl::dict::getwithdefault]]


# HTTP Stuff
proc http::requestHeaderValue {token header} {
    Meta $token request $header VALUE
}
proc http::responseHeaderValue {token header} {
    Meta $token response $header VALUE
}
proc http::Meta {token who args} {
    variable $token
    upvar 0 $token state

    if {$who eq {request}} {
        set whom -headers
    } elseif {$who eq {response}} {
        set whom meta
    } else {
        return -code error {usage: ::http::Meta token request|response ?headerName ?VALUE??}
    }

    set header [string tolower [lindex $args 0]]
    set how    [string tolower [lindex $args 1]]
    set lenny  [llength $args]
    if {$lenny == 0} {
        return $state($whom)
    } elseif {($lenny > 2) || (($lenny == 2) && ($how ne {value}))} {
        return -code error {usage: ::http::Meta token request|response ?headerName ?VALUE??}
    } else {
        set result {}
        set combined {}
        foreach {key value} $state($whom) {
            if {$key eq $header} {
                lappend result $key $value
                append combined $value {, }
            }
        }
        if {$lenny == 1} {
            return $result
        } else {
            return [string range $combined 0 end-2]
        }
    }
}
# ------------------------------------------------------------------------------
#  Proc http::responseInfo
# ------------------------------------------------------------------------------
# Command to return a dictionary of the most useful metadata of a HTTP
# response.
#
# Arguments:
# token       - connection token (name of an array)
#
# Return Value: a dict. See man page http(n) for a description of each item.
# ------------------------------------------------------------------------------
proc http::responseInfo {token} {
    variable $token
    upvar 0 $token state
    set result {}
    foreach {key origin name} {
        stage                 STATE  state
        status                STATE  status
        responseCode          STATE  responseCode
        reasonPhrase          STATE  reasonPhrase
        contentType           STATE  type
        binary                STATE  binary
        redirection           RESP   location
        upgrade               STATE  upgrade
        error                 ERROR  -
        postError             STATE  posterror
        method                STATE  method
        charset               STATE  charset
        compression           STATE  coding
        httpRequest           STATE  -protocol
        httpResponse          STATE  httpResponse
        url                   STATE  url
        connectionRequest     REQ    connection
        connectionResponse    RESP   connection
        connectionActual      STATE  connection
        transferEncoding      STATE  transfer
        totalPost             STATE  querylength
        currentPost           STATE  queryoffset
        totalSize             STATE  totalsize
        currentSize           STATE  currentsize
        proxyUsed             STATE  proxyUsed
    } {
        if {$origin eq {STATE}} {
            if {[info exists state($name)]} {
                dict set result $key $state($name)
            } else {
                # Should never come here
                dict set result $key {}
            }
        } elseif {$origin eq {REQ}} {
            dict set result $key [requestHeaderValue $token $name]
        } elseif {$origin eq {RESP}} {
            dict set result $key [responseHeaderValue $token $name]
        } elseif {$origin eq {ERROR}} {
            # Don't flood the dict with data.  The command ::http::error is
            # available.
            if {[info exists state(error)]} {
                set msg [lindex $state(error) 0]
            } else {
                set msg {}
            }
            dict set result $key $msg
        } else {
            # Should never come here
            dict set result $key {}
        }
    }
    return $result
}
