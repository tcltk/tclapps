#!/bin/sh
# the next line restarts using tclsh \
    exec tclsh "$0" "$@"

# bridge.tcl -- bridge between Tcl Chat and IRC.

# Copyright 2003 David N. Welton <davidw@dedasys.com>

# $Id: bridge.tcl,v 1.4 2003/06/26 09:20:33 davidw Exp $

# There's a lot that could be added here.

set auto_path "[file dirname [info script]] $auto_path"

# This requires the irc module from CVS.
package require irc 0.3
package require chat


# Called when someone does something like leave or join.

proc chat::addTraffic {who action} {
    Send "PRIVMSG $::client::channel :*** $who $action"
}

# Called when a new message from Tcler's Chat is to be handled.

proc chat::addMessage {nick str} {
    Send "PRIVMSG $::client::channel :<$nick> $str"
}

# Called when a new action (/me) from Tcler's Chat is to be handled. 

proc chat::addAction {nick str} {
    Send "PRIVMSG $::client::channel :* $nick $str"
}

proc chat::addHelp {name str} {
    #puts "HELP: $name $str"
}


proc chat::Send {str} {
    variable Options
    variable Limit
    # if we are over the sending limit just push line into the queue
    if { $Limit(queue) != "" } {
        ::log::log debug "appending to queue"
        lappend Limit(queue) $str
        return
    }
    # count the number of lines per second
    if { ([clock seconds] - $Limit(last)) < 1 } {
        incr Limit(lines)
        ::log::log debug "lines: $Limit(lines)"
    } else {
        # remember the last time we sent a line
        set Limit(last) [clock seconds]
        set Limit(lines) 0
    }
    # if we are over the line limit kick off the queued sends
    if { $Limit(lines) > 4 } {
        ::log::log info "flood started"
        lappend Limit(queue) $str
        after 1000 chat::SendFromQueue
        return
    }
    $client::cn send $str
}

# Processes the queue created when we try to addMessage too fast

proc chat::SendFromQueue {} {
    variable Options
    variable Limit
    # return if the queue is empty
    if { [set str [lindex $Limit(queue) 0]] == "" } {
        set Limit(last) 0
        ::log::log info "flood ended"
        return
    }
    set Limit(queue) [lreplace $Limit(queue) 0 0]
    ::log::log debug "sending from queue"
    $client::cn send $str
    # send next line
    after 1000 chat::SendFromQueue
}

namespace eval client {}

# create a server connection and set up associated events

proc client::create { server port nk chan } {
    variable cn
    variable channel $chan
    variable nick $nk
    set cn [::irc::connection $server $port]

    $cn registerevent 001 {
        set ::client::nick [who]
        cmd-join $::client::channel
    }

    $cn registerevent 433 {
        if { [lindex [additional] 0] == $::client::nick } {
            cmd-send "NICK [string trim $::client::nick 0123456789][string range [expr rand()] end-2 end]"
        }
    }

    $cn registerevent defaultcmd {
	::log::log debug "[action] [msg]"
    }

    $cn registerevent defaultnumeric {
	::log::log debug "[action] XXX [target] XXX [msg]"
    }

    $cn registerevent defaultevent {
	::log::log debug "[action] XXX [who] XXX [target] XXX [msg]"
    }

    $cn registerevent PART {
	if { [target] == $client::channel && [who] != $client::nick } {
	    chat::msgSend "*** [who] leaves"
	}
    }

    $cn registerevent JOIN {
	if { [who] != $client::nick } {
	    chat::msgSend "*** [who] joins"
	}
    }

    $cn registerevent QUIT {
	if { [who] != $client::nick } {
	    chat::msgSend "*** [who] leaves"
	}
    }

    $cn registerevent PRIVMSG {
	if { [target] == $client::channel } {
	    if { [string match "\001*\001" [msg]] } {
	        if { [regexp {^\001ACTION (.+)\001$} [msg] -> msg] } {
	            chat::msgSend "* [who] $msg"
	        }
	    } else {
	        chat::msgSend "[who] says: [msg]"
	    }
	}
    }
    
    $cn registerevent EOF {
        ::client::connect $::client::cn
    }

    return $cn
}


# connect to the server and register

proc client::connect {cn} {
    variable ::chat::Limit
    variable nick
    # set up variable for rate limiting
    array set Limit [list last [clock seconds] queue {} lines 0]
    $cn connect
    $cn user $nick localhost domain "Tcl'ers Chat connector - See: http://mini.net/tcl/6248"
    $cn nick $nick
    ping
}

proc client::ping {} {
    variable cn
    $cn serverping
    after cancel [namespace current]::ping
    after 20000 [namespace current]::ping
}

proc bgerror {args} {
    global errorInfo
    ::log::log error "BGERROR: $args"
    ::log::log error "ERRORINFO: $errorInfo"
}

client::create irc.debian.org 6667 ircbridge \#tcl
client::connect $client::cn
::chat::Init

vwait forever