#!/bin/sh
# the next line restarts using tclsh \
    exec tclsh "$0" "$@"

# bridge.tcl -- bridge between Tcl Chat and IRC.

# Copyright 2003 David N. Welton <davidw@dedasys.com>

# $Id: bridge.tcl,v 1.3 2003/02/28 23:59:48 davidw Exp $

# There's a lot that could be added here.

set auto_path "[file dirname [info script]] $auto_path"

# This requires the irc module from CVS.
package require irc 0.3
package require chat

proc chat::addHelp {clr name str} {
    puts "HELP: $clr $name $str"
}


# Called when someone does something like leave or join.

proc chat::addTraffic {who action} {
    variable Options
    if { $who != $Options(Username) } {
	$client::cn privmsg "#tcl" "*** $who [::htmlparse::mapEscapes $action]"
    }
}


# Called when a new message from Tcler's Chat is to be handled. 

proc chat::addMessage {nick str} {
    variable Options
    if { ($nick != $Options(Username)) && ($nick != "tick") } {
	foreach line [split [::htmlparse::mapEscapes $str] "\n"] {
	    $client::cn privmsg \#tcl "<$nick> $line"
	}
    }
}

namespace eval client {}

proc client::connect { nk } {
    variable cn
    variable channel
    variable nick $nk
    set channel \#tcl
    set cn [::irc::connection irc.debian.org 6667]
    set ns [namespace qualifiers $cn]

    $cn registerevent PING {
	network send "PONG [msg]"
    }

    $cn registerevent 376 {
    }

    $cn registerevent defaultcmd {
	puts "[action] [msg]"
    }

    $cn registerevent defaultnumeric {
	puts "[action] XXX [target] XXX [msg]"
    }

    $cn registerevent defaultevent {
	puts "[action] XXX [who] XXX [target] XXX [msg]"
    }

    $cn registerevent PART {
	if { [target] == $client::channel && [who] != $client::nick } {
	    chat::msgSend "*** [who] leaves"
	}
    }

    $cn registerevent JOIN {
	puts "JOIN [who] [target]"
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
#	puts "PRIVMSG [who] [action] [target] [msg]"
	if { [target] == $client::channel && [who] != $client::nick} {
	    chat::msgSend "[who] says: [msg]"
	}
    }

    $cn registerevent KICK {
	puts "[who] KICKed [target 1] from [target] : [msg]"
    }

    $cn connect

    $cn user $nick localhost "IRC/Tcl'ers Chat connector - See: http://mini.net/tcl/6248"
    $cn nick $nick
    $cn join $channel
}

client::connect ircbridgetest
::chat::Init

vwait forever
