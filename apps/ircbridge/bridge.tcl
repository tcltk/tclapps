# bridge.tcl -- bridge between Tcl Chat and IRC.

# Copyright 2003 David N. Welton <davidw@dedasys.com>

# $Id: bridge.tcl,v 1.1 2003/01/30 06:46:43 davidw Exp $

# There's a lot that could be added here.

set auto_path "[file dirname [info script]] $auto_path"

# This requires the irc module from CVS.
package require irc 0.3
package require chat

proc chat::addHelp {clr name str} {
    puts "HELP: $clr $name $str"
}

proc chat::addTraffic {who action} {
    variable Options
    if { $who != $Options(Username) } {
	$client::cn privmsg \#tcl "<$who [::htmlparse::mapEscapes $action]>"
    }
}

proc chat::addMessage {nick str} {
    variable Options
    if { $nick != $Options(Username) } {
	$client::cn privmsg \#tcl "<$nick> [::htmlparse::mapEscapes $str]"
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

    $cn registerevent PRIVMSG {
	if { [target] == $client::channel && [who] != $client::nick } {
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
