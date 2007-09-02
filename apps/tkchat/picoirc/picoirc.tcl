# Based upon the picoirc code by Salvatore Sanfillipo and Richard Suchenwirth
# See http://wiki.tcl.tk/13134 for the original standalone version.
# 
# This package provides a general purpose minimal IRC client suitable for 
# embedding in other applications. All communication with the parent
# application is done via an application provided callback procedure.
#
# Each connection has its own state so you can hook up multiple channels or
# various servers.
#
# To initiate an IRC connection you must call picoirc::connect with a callback
# procedure, a nick to use on irc and the irc url to connect to.
# This will return a variable name that is the irc connection context.
# The callback must look like:
#  proc Callback {context state args} {
#  }
# where context is the irc context variable name (in case you need to pass it back
# to a picoirc procedure). state is one of a number of states:
#
#   init     - called just before the socket is created
#   connect  - called once we have connected, before we join any channels
#   close    - called when the socket gets closed, before the context is deleted
#   userlist - first argument is a list of all irc nicks in the channel
#   chat     - called when a message arrives. args are nick message type. type may
#              be "" or "ACTION"
#   system   - first argument is irc server message
#   topic    - first arg is the channel topic string
#   traffic  - called when users join, leave or change names. The args are action,
#              nick ?newnick?. Action is entered, left or nickchange and nick is the
#              user doing the action. newnick is the new name if action is nickchange.
#
# Copyright (c) 2004 Salvatore Sanfillipo
# Copyright (c) 2004 Richard Suchenwirth
# Copyright (c) 2007 Patrick Thoyts

namespace eval ::picoirc {
    variable uid; if {![info exists uid]} { set uid 0 }
    variable defaults {
        server   "irc.freenode.net"
        port     6667
        channel  "#tcl"
        callback ""
        motd     {}
        users    {}
    }
}

proc ::picoirc::connect {callback nick url} {
    global Options
    variable defaults
    variable uid
    set context [namespace current]::irc[incr uid]
    upvar #0 $context irc
    array set irc $defaults
    if {![regexp {^irc://([^:/]+)(?::([^/]+))?/([^,]+)} $url -> server port channel]} {
        regexp {^([^@]+)@([^:]+)(?::(\d+))?} $url -> channel server port
    }
    if {[info exists channel] && $channel ne ""} {set irc(channel) $channel}
    if {[info exists server] && $server ne ""} {set irc(server) $server}
    if {[info exists port] && $port ne ""} {set irc(port) $port}
    set irc(callback) $callback
    set irc(nick) $nick
    callback $context system "Attempting to connect to $irc(channel)@$irc(server)"
    callback $context init
    set irc(socket) [socket -async $irc(server) $irc(port)]
    fileevent $irc(socket) readable [list [namespace origin Read] $context]
    fileevent $irc(socket) writable [list [namespace origin Write] $context]
    return $context
}

proc ::picoirc::callback {context state args} {
    upvar #0 $context irc
    if {[string length $irc(callback)] > 0 \
            && [llength [info commands $irc(callback)]] == 1} {
        if {[catch [linsert $args 0 $irc(callback) $context $state] err]} {
            puts stderr $err
        }
    }
}

proc ::picoirc::Version {} {
    set ver "PicoIRC:[package provide picoirc]:Tcl [info patchlevel]"
    if {[info exists ::tkchat::rcsid]} {
        regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $::tkchat::rcsid -> rcsVersion
        set ver "Tkchat:$rcsVersion:Tcl [info patchlevel]"
    }
    return $ver
}

proc ::picoirc::Write {context} {
    upvar #0 $context irc
    fileevent $irc(socket) writable {}
    if {[set err [fconfigure $irc(socket) -error]] ne ""} {
        callback $context system $err
    }
    callback $context connect
    fconfigure $irc(socket) -buffering line -translation crlf -encoding utf-8
    callback $context system "Logging into $irc(server)"
    puts $irc(socket) "NICK $irc(nick)"
    set ver [join [lrange [split [Version] :] 0 1] " "]
    puts $irc(socket) "USER $irc(nick) 0 * :$ver user"
    puts $irc(socket) "JOIN $irc(channel)"
    return
}

proc ::picoirc::in {list element} {expr {[lsearch -exact $list $element]>=0}}
proc ::picoirc::splitirc {s} {
    foreach v {nick flags user host} {set $v {}}
    regexp {^([^!]*)!([^=]*)=([^@]+)@(.*)} $s -> nick flags user host
    return [list $nick $flags $user $host]
}
proc ::picoirc::Read {context} {
    upvar #0 $context irc
    if {[eof $irc(socket)]} {
        callback $context system "IRC socket closed"
        fileevent $irc(socket) readable {}
        callback $context close
        catch {close $irc(socket)}
        catch {unset $context}
        return
    }
    if {[gets $irc(socket) line] != -1} {
        if {[lindex [split $line] 0] eq "PING"} {
            puts $irc(socket) "PONG [info hostname] [lindex [split $line] 1]"
            return
        }
        puts stderr $line
        if {[regexp {:([^!]*)![^ ].* +PRIVMSG ([^ :]+) +:(.*)} $line -> \
                 nick target msg]} {
            set type ""
            if {[regexp {\001(\S+)(.*)?\001} $msg -> ctcp data]} {
                switch -- $ctcp {
                    ACTION { set type ACTION ; set msg $data }
                    VERSION {
                        puts $irc(socket) "PRIVMSG $nick :\001VERSION [Version]\001"
                        return 
                    }
                    default { puts $irc(socket) \
                                  "PRIVMSG $nick :\001ERRMSG $msg : unknown query" }
                }
            }
            if [in {azbridge ijchain} $nick] {regexp {<([^>]+)>(.+)} $msg -> nick msg}
            callback $context chat $nick $msg $type
        } elseif {[regexp {^:([^ ]+) +([^ ]+) +([^ ]+) +(.*)} $line -> \
                       server code target rest]} then {
            switch -- $code {
                001 - 002 - 003 - 004 - 005 - 250 - 251 - 252 - 
                254 - 255 - 265 - 266 { return }
                433 {
                    variable nickid ; if {![info exists nickid]} {set nickid 0}
                    set seqlen [string length [incr nickid]]
                    set irc(nick) [string range $irc(nick) 0 [expr 8-$seqlen]]$nickid
                    puts $irc(socket) "NICK $irc(nick)"
                }
                353 {
                    if {[regexp {[^:]*:(.*)} $rest -> names]} {
                        set irc(users) [concat $irc(users) $names]
                    }
                    return
                }
                366 {
                    callback $context userlist $irc(users)
                    set irc(users) {}
                    return
                }
                332 { callback $context topic $rest; return }
                333 { return }
                375 { set irc(motd) {} ; return }
                372 { append irc(motd) $rest ; return}
                376 { return }
            }
            callback $context system "$code $target $rest"
        } elseif {[regexp {^:([^ ]+) +([^ ]+) +(.*)} $line -> \
                       userid code rest]} then {
            switch -- $code {
                JOIN {
                    foreach {n f u h} [splitirc $userid] break
                    callback $context traffic entered $n
                    return
                }
                NICK {
                    foreach {n f u h} [splitirc $userid] break
                    callback $context traffic nickchange \
                        $n [string range $rest 1 end]
                    return
                }
                QUIT - PART {
                    foreach {n f u h} [splitirc $userid] break
                    callback $context traffic left $n
                    return 
                }
            }
        } else {
            callback $context system $line
        }
    }
}

proc ::picoirc::Post {context msg} {
    upvar #0 $context irc
    if [regexp {^/([^ ]+) *(.*)} $msg -> cmd msg] {
 	switch -- $cmd {
 	    me {set msg "\001ACTION $msg\001"}
 	    nick {puts $irc(socket) "NICK $msg"; set $irc(nick) $msg}
 	    quit {puts $irc(socket) "QUIT $msg" }
 	    names {puts $irc(socket) "NAMES $irc(channel)"}
            whois {puts $irc(socket) "WHOIS $msg"}
 	    quote {puts $irc(socket) $msg}
 	    join {
 		puts $irc(socket) "PART $irc(channel)"
 		puts $irc(socket) "JOIN $msg"
 		set irc(channel) $msg
 	    }
 	    msg {
 		if {[regexp {([^ ]+) +(.*)} $msg -> target querymsg]} {
 		    puts $irc(socket) "PRIVMSG $target :$msg"
 		    callback $context chat $target $querymsg ""
 		}
 	    }
 	    default {callback $context system "unknown command /$cmd"}
 	}
 	if {$cmd ne {me} || $cmd eq {msg}} return
    }
    foreach line [split $msg \n] {puts $irc(socket) "PRIVMSG $irc(channel) :$line"}
    set type ""; if [regexp {\001ACTION(.+)\001} $msg -> msg] {set type ACTION}
    callback $context chat $irc(nick) $msg $type
}

# -------------------------------------------------------------------------

package provide picoirc 0.4

# -------------------------------------------------------------------------
