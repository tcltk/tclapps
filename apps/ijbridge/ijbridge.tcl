#!/usr/bin/env tclsh
# ijbridge.tcl - Copyright (C) 2004 Pat Thoyts <patthoyts@users.sf.net>
#
# This implements a bridge between a Jabber Multi-User Chat and an IRC
# channel.
#
# The IRC portions of this have come from ircbridge.
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

package require jlib;                   # jabberlib
package require browse;                 # jabberlib
package require muc;                    # jabberlib
package require wrapper;                # jabberlib
package require sha1;                   # tcllib
package require log;                    # tcllib
package require irc 0.4;                # tcllib

namespace eval client {}

namespace eval ::ijbridge {

    variable version 1.0.0
    variable rcsid {$Id: ijbridge.tcl,v 1.1 2004/11/15 15:44:28 patthoyts Exp $}

    variable Options
    if {![info exists Options]} {
        array set Options {
            JabberServer   {}
            JabberPort     5222
            JabberUser     {}
            JabberPassword {}
            JabberResource ijbridge
            Conference     {}
            ConferenceNick {}
            
            IrcServer      irc.freenode.net
            IrcPort        6667
            IrcUser        {}
            IrcChannel     {}
        }
    }

    variable Limit
    if {![info exists Limit]} {
        array set Limit {}
    }

    variable IrcUserList
    if {![info exists IrcUserList]} {
        set IrcUserList [list]
    }
}

proc ::ijbridge::connect {{server {}} {port {}}} {
    variable Options
    variable conn
    
    catch {unset conn}

    # Now connect the IRC side of things.
    client::create $Options(IrcServer) $Options(IrcPort) \
        $Options(IrcUser) $Options(IrcChannel)


    set conn(roster) [roster::roster [namespace origin OnRoster]]
    set conn(jabber) [jlib::new $conn(roster) [namespace origin OnClient] \
                          -iqcommand          [namespace origin OnIq] \
                          -messagecommand     [namespace origin OnMessage] \
                          -presencecommand    [namespace origin OnPresence]]

    if {$server == {}} {
        set server $Options(JabberServer)
    }
    if {$port == {}} {
        set port $Options(JabberPort)
    }
    
    set conn(sock) [socket $server $port]
    $conn(jabber) setsockettransport $conn(sock)    
    $conn(jabber) openstream $Options(JabberServer) \
        -socket $conn(sock) -cmd [namespace origin OnConnect]

    return conn
}

proc ::ijbridge::disconnect {} {
    variable conn
    $conn(jabber) closestream
    unset conn(sock)
}

proc ::ijbridge::OnConnect {token args} {
    variable Options
    variable conn
    
    log::log debug "OnConnect $token $args"
    array set info $args
    set conn(info) $args
    set conn(id) [$token send_auth \
                      $Options(JabberUser) $Options(JabberResource) \
                      [namespace origin OnLogin] \
                      -password $Options(JabberPassword)]
    #-digest [sha1::sha1 $info(id)$JabberPassword]
}

proc ijbridge::OnLogin {token type query} {
    variable Options
    variable conn
    switch -exact -- $type {
        result {
            $token send_presence -type invisible
            set conn(muc) [jlib::muc::new $token]
            $conn(muc) enter $Options(Conference) $Options(ConferenceNick) \
                -command [namespace origin OnMucEnter]
        }
        error -
        default {
            log::log debug "OnLogin: $token $type $query"
        }
    }
}

proc ijbridge::OnMucEnter {muc type args} {
    variable Options
    log::log debug "OnMucEnter $muc $type $args"
}

proc ::ijbridge::OnClient {token cmd args} {
    log::log debug "OnClient $token $cmd $args"
    switch -exact -- $cmd {
        connect {
        }
        disconnect {
            log::log warning "disconnect!"
        }
        networkerror {
            log::log error "error $args"
        }
        streamerror {
            if {[catch {
                array set a $args
                log::log error $a(-errormsg)
            }]} {
                log::log error $args 
            }
            log::log warning disconnected
        }
        default {
        }
    }
}

proc ::ijbridge::OnRoster {roster type {jid {}} args} {
    variable conn
    log::log debug "OnRoster: $roster $type $jid $args"
    switch -exact -- $type {
        presence {
        }
        default {
        }
    }
}

proc ::ijbridge::OnIq {token type args} {
    log::log debug "iq: $token $type $args"
    switch -exact -- $type {
        get {
        }
    }
}

proc ::ijbridge::OnMessage {token type args} {
    variable Options
    variable conn
    switch -exact -- $type {
        groupchat {
            # xmit to irc
            log::log debug "---> $args"
            array set a $args
            set mynick [$conn(muc) mynick $Options(Conference)]
            set myid $Options(Conference)/$mynick
            if {[info exists a(-from)] && [string equal $a(-from) $myid]} {
                log::log debug "avoid resend"
            } else {

                if {[info exists a(-x)]} {
                    foreach chunk $a(-x) {
                        if {[lsearch -exact [wrapper::getattrlist $chunk] \
                                 jabber:x:delay] != -1} {
                            # We don't xmit history items.
                            log::log debug "avoid xmit history item"
                            return
                        }
                    }
                }

                set nickndx [string first / $a(-from)]
                set nick [string range $a(-from) [incr nickndx] end]
                set emote [string match /me* $a(-body)]
                if {$emote} {
                    set a(-body) [string range $a(-body) 4 end]
                }
                foreach line [split $a(-body) \n] {
                    if {$emote} {
                        xmit "PRIVMSG $::client::channel :$nick $line"
                    } else {
                        xmit "PRIVMSG $::client::channel :<$nick> $line"
                    }
                }
            }
        }
        chat -
        normal -
        error -
        default {
            log::log debug "msg: $token $type $args"
        }
    }
}
proc ::ijbridge::OnPresence {token type args} {
    log::log debug "prs: $token $type $args"
}

# send from irc -> jabber
#
# If who eq azbridge then body is "<azbridge> *** arjen left"
# or                              "<azbridge> <arjen> blha blah"
#
# Trim the azbridge's and replace with the user.
#
proc ::ijbridge::IrcToJabber {who msg emote} {
    variable Options
    if {[string equal $who "azbridge"]} {
        regexp {^<(.*?)> (.*)$}  $msg -> who msg
        set emote [regexp {^\* (\w+) (.*)$} $msg -> who msg]
    }
            
    if {$emote} {
        ijbridge::send -id "$Options(Conference)/$who" "* $who $msg"
    } else {
        ijbridge::send -id "$Options(Conference)/$who" "<$who> $msg"
    }
}

proc ::ijbridge::send {args} {
    variable Options
    variable conn
    array set opts { user {} -xlist {} -type chat -id {} -from {}}
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -user { set opts(user) [Pop args 1] }
            -id   { set opts(-id) [Pop args 1] }
            -from { set opts(-from) [Pop args 1] }
            --    { Pop args ; break }
        }
        Pop args
    }
    if {[llength $args] != 1} {
        return -code error "wrong \# args:\
            should be \"send ?option value? message\""
    }
    if {[string length $opts(-id)] < 1} {
        set opts(-id) $conn(id)
    }
    if {[string length $opts(user)] < 1} {
        set opts(user) $Options(Conference)
        set opts(-type) groupchat
    } else {
        return -code error "Not done"
    }

    eval [linsert [array get opts -*] 0 $conn(jabber) \
              send_message $opts(user) -body [lindex $args 0]]
    
    #$conn(jabber) send_message $opts(-user) -id $opts(-id) \
    #    -body [lindex $args 0] -type $opts(-type)
}

proc ::ijbridge::xmit {str} {
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
        after 1000 [namespace origin XmitFromQueue]
        return
    }
    $client::cn send $str
}

# Processes the queue created when we try to addMessage too fast

proc ::ijbridge::XmitFromQueue {} {
    variable Options
    variable Limit
    # return if the queue is empty
    if { [string length [set str [lindex $Limit(queue) 0]]] < 1 } {
        set Limit(last) 0
        ::log::log info "flood ended"
        return
    }
    set Limit(queue) [lreplace $Limit(queue) 0 0]
    ::log::log debug "sending from queue"
    $client::cn send $str
    # send next line
    after 1000 [namespace origin XmitFromQueue]
}


proc ::ijbridge::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

proc ::ijbridge::LoadConfig {} {
    variable Options
    set conf [file normalize [info script]]
    set base [file rootname [file tail $conf]].conf
    set conf [file join [file dirname $conf] $base]
    if {[file exists $conf]} {
        set f [open $conf r]
        set n 0
        while {![eof $f]} {
            gets $f line
            string trim $line
            if {[string match "#*" $line]} continue
            if {[string length $line] < 1} continue
            if {[llength $line] != 2} {
                return -code error "invalid config line $n: \"$line\""
            }
            if {![info exists Options([lindex $line 0])]} {
                return -code error "invalid config option\
                \"[lindex $line 0]\" at line $n"
            }
            set Options([lindex $line 0]) [lindex $line 1]
            incr n
        }
        close $f
    } else {
        log::log warning "no configuration file found!"
    }
    return
}

# -------------------------------------------------------------------------

# create a server connection and set up associated events

proc client::create { server port nk chan } {
    variable ::ijbridge::Options
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
	set ::ijbridge::IrcUserList \
            [split [string map {@ "" % "" + ""} [string trim [msg]]] " "]
        foreach bridge {ircbridge azbridge ijbridge} {
            set item [lsearch $::ijbridge::IrcUserList $bridge]
            if { $item > -1 } {
                set ::ijbridge::IrcUserList \
                    [lreplace $::ijbridge::IrcUserList $item $item]
            }
	}
    }

    $cn registerevent defaultcmd {
	::log::log debug "[action] [msg]"
    }

    $cn registerevent defaultnumeric {
	::log::log debug "[action] X-X [target] XXX [msg]"
    }

    $cn registerevent defaultevent {
	::log::log debug "[action] X|X [who] XXX [target] XXX [msg]"
    }

    $cn registerevent PONG {}

    $cn registerevent PART {
	if { [target] == $client::channel && [who] != $client::nick } {
	    ijbridge::send "*** [who] leaves"
	    set item [lsearch $::ijbridge::IrcUserList [who]]
	    if { $item > -1 } {
		set ::ijbridge::IrcUserList \
                    [lreplace $::ijbridge::IrcUserList $item $item]
	    }
	}
	
    }

    $cn registerevent JOIN {
	if { [who] != $client::nick } {
	    ijbridge::send "*** [who] joins"

	    if { [lsearch $::ijbridge::IrcUserList [who]] == -1 } {
		lappend ::ijbridge::IrcUserList [who]
	    }

	}

    }

    $cn registerevent QUIT {
	if { [who] != $::client::nick } {
	    ijbridge::send "*** [who] leaves"
	    if { [who] == [string trimright $::client::nick 0123456789] } {
	        cmd-send "NICK [who]"
	    }
	}
	
    }

    $cn registerevent PRIVMSG {
	if { [target] == $client::channel } {
            set emote 0
            set msg [msg]
	    if { [string match "\001*\001" [msg]] } {
	        if { [regexp {^\001ACTION (.+)\001$} [msg] -> msg] } {
	            set emote 1
	        }
	    }
            ijbridge::IrcToJabber [who] $msg $emote
	}
    }
    
    $cn registerevent NICK {
        if { [who] == $::client::nick } {
            set ::client::nick [msg]
        } else {
            ijbridge::send "*** [who] is now known as [msg]"
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
    variable ::ijbridge::Limit
    variable nick
    # set up variable for rate limiting
    array set Limit [list last [clock seconds] queue {} lines 0]
    ::log::log notice "Connecting to $server on port $port"
    if {[catch {$cn connect $server $port} err]} {
        ::log::log notice "Could not connect: $err"
        after 10000 [list [namespace current]::connect $cn $server $port]
        return
    }
    $cn user $nick localhost domain "Tcl Jabber-IRC bridge"
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

#::irc::config debug 0

# -------------------------------------------------------------------------


log::lvSuppressLE emerg 0
::ijbridge::LoadConfig

if {!$tcl_interactive} {
    if {[info exists tk_version]} {
        if {[tk windowingsystem] eq "win32"} { console show }
        wm withdraw .
        tkwait window .
    } else {
        eval [linsert $argv 0 ::ijbridge::connect]
        vwait ::forever
    }
}
