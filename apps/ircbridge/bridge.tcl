#!/bin/sh
# the next line restarts using tclsh \
    exec tclsh "$0" "$@"

# bridge.tcl -- bridge between Tcl Chat and IRC.

# Copyright 2003 David N. Welton <davidw@dedasys.com>

# $Id: bridge.tcl,v 1.10 2004/03/26 16:06:30 pascalscheffers Exp $

# There's a lot that could be added here.


set auto_path "[file dirname [info script]] $auto_path"

# This requires the irc module from CVS.
package require irc 0.4
package require chat

# Some config defaults:

set ::irc_server irc.debian.org
set ::irc_port 6667
set ::irc_user ircbridge
set ::irc_channel \#tcl

# Source the rc file if it exists. Use this file to set ::chatPassword
# and ::KillPassword to the correct values.
if { [file exists ~/.ircbridge] } {
    source ~/.ircbridge
}

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

# Whispers from tkchat chatusers:
proc chat::addWhisper {nick str} {

    if { $nick eq "WELCOME" } {
	::log::log debug "Welcome message: $str"
	return
    }

    set str [string range $str 0 end-1]
    set cmd [split [string trim $str] " "]

    if { [string match "->*" $nick] } {
	::log::log debug "Whispered to [string range $nick 2 end]"
	return
    }

    ::log::log debug "$nick whispered '$str'"

    switch -glob -- [lindex $cmd 0] {
	onlineusers {
	    chat::msgSend "/msg $nick onlineusers: $::onlineircusers"
	}
	default {
	    chat::msgSend "/msg $nick I don't understand '$str'"
	}
    }
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
    set cn [::irc::connection]

    $cn registerevent 001 {
        set ::client::nick [target]
        cmd-join $::client::channel
    }

    $cn registerevent 433 {
        if { [lindex [additional] 0] == $::client::nick } {
            cmd-send "NICK [string trimright $::client::nick 0123456789][string range [expr rand()] end-2 end]"
        }
    }

    $cn registerevent 353 {
	#List of online users sent on channel join
	::log::log debug "UsersOnline [msg]"
	set ::onlineircusers [split [string map {@ "" % "" + ""} [string trim [msg]]] " "]
	set item [lsearch $::onlineircusers ircbridge]
	if { $item > -1 } {
	    set ::onlineircusers [lreplace $::onlineircusers $item $item]
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
	    set item [lsearch $::onlineircusers [who]]
	    if { $item > -1 } {
		set ::onlineircusers [lreplace $::onlineircusers $item $item]
	    }
	}
	
    }

    $cn registerevent JOIN {
	if { [who] != $client::nick } {
	    chat::msgSend "*** [who] joins"

	    if { [lsearch $::onlineircusers [who]] == -1 } {
		lappend ::onlineircusers [who]
	    }

	}

    }

    $cn registerevent QUIT {
	if { [who] != $::client::nick } {
	    chat::msgSend "*** [who] leaves"
	    if { [who] == [string trimright $::client::nick 0123456789] } {
	        cmd-send "NICK [who]"
	    }
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
    
    $cn registerevent NICK {
        if { [who] == $::client::nick } {
            set ::client::nick [msg]
        } else {
            chat::msgSend "*** [who] is now known as [msg]"
        }
    }
    
    $cn registerevent EOF "
        ::log::log notice \"Disconnected from IRC\"
        ::client::connect \$::client::cn $server $port
    "

    connect $cn $server $port
}

# connect to the server and register

proc client::connect {cn server port} {
    variable ::chat::Limit
    variable nick
    # set up variable for rate limiting
    array set Limit [list last [clock seconds] queue {} lines 0]
    ::log::log notice "Connecting to $server on port $port"
    if {[catch {$cn connect $server $port} err]} {
        ::log::log notice "Could not connect: $err"
        after 10000 [list [namespace current]::connect $cn $server $port]
        return
    }
    $cn user $nick localhost domain "Tcl'ers Chat connector - See: http://mini.net/tcl/6248"
    $cn nick $nick
    ping
    ::log::log notice "Connected to $server"
}

proc client::ping {} {
    variable cn
    $cn serverping
    after cancel [namespace current]::ping
    after 20000 [namespace current]::ping
}

proc bgerror {args} {
    global errorInfo
    ::log::log error "BGERROR: [join $args]"
    ::log::log error "ERRORINFO: $errorInfo"
}

::irc::config debug 0

#Stuff for daemon[ize] see http://wiki.tcl.tk/deamon

proc shutdown {} {
    # no cleanup needed.
    exit
}

proc daemonize {} {
    close stdin
    close stdout
    close stderr
    if {[fork]} {exit 0}
    id process group set
    if {[fork]} {exit 0}
    set fd [open /dev/null r]
    set fd [open /dev/null w]
    set fd [open /dev/null w]
    cd /
    umask 022
    return [id process]
}


#Daemonize?
if { [lindex $argv 0] eq "-d" } {
    package require Tclx

    daemonize

    signal ignore  SIGHUP
    signal unblock {QUIT TERM}
    signal trap    {QUIT TERM} shutdown    
}

set onlineircusers [list]
client::create $::irc_server $::irc_port $::irc_user $::irc_channel
::chat::Init

vwait forever
