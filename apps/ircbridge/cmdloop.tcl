# cmdloop.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# $Id: cmdloop.tcl,v 1.1 2005/04/25 20:29:37 patthoyts Exp $

namespace eval ::cmdloop {
    variable hosts_allow
    if {![info exists hosts_allow]} {
        set hosts_allow {127.0.0.1 ::1 82.33.96.128}
    }
    
    variable welcome
    if {![info exists welcome]} {
        set welcome "Hello %client %port"
    }

    variable cmds_deny
    if {![info exists cmds_deny]} {
        set cmds_deny {exit denied}
    }
}

# cmdloop::Read --
#
#	Reads commands from stdin and evaluates them. This permits
#	us to issue commands to the server while it is still 
#	running. Suitable commands are ijbridge::presence and
#	ijbridge::say or ijbridge::xmit.
#
proc ::cmdloop::Read {chan ochan state} {
    variable cmds_deny
    upvar #0 $state input
    if {![info exists input]} {set input {}}
    if {[eof $chan]} {
        puts $ochan "!! EOF $chan"
    }
    if {[gets $chan line] != -1} {
        append input $line
        if {[string length $input] > 0 && [info complete $input]} {
            set cmd [lindex $input 0]
            if {[lsearch -exact $cmds_deny $cmd] != -1} {
                set res "$cmd command disabled"
            } elseif {$cmd eq "puts" && [string match "sock*" $chan] \
                          && [llength $input] == 2} {
                set res [lindex $input 1]
            } else {
                set code [catch {uplevel \#0 $input} res]
            }
            unset input
            puts $ochan $res
        }
    }
}

# cmdloop::Accept --
#
#	Setup the client channel for reading commands as we do 
#	for stdin. Useful with tkcon's socket connection feature.
#
proc ::cmdloop::Accept {chan client port} {
    # we could validate the client here.
    if {[lsearch $::cmdloop::hosts_allow $client] == -1} {
        puts $chan "Access denied"
        close $chan
        return
    }
    fconfigure $chan -blocking 0 -buffering line
    puts $chan [welcome $client $port]
    fileevent $chan readable \
        [list ::cmdloop::Read $chan $chan ::cmdloop::state_$chan]
}

proc ::cmdloop::welcome {{client {}} {port {}}} {
    variable welcome
    return [string map [list %client $client %port $port] $welcome]
}
    
proc ::cmdloop::cmdloop {} {
    variable welcome
    puts [welcome]
    puts -nonewline "> "
    fconfigure stdin -blocking 0 -buffering line
    fileevent stdin readable \
        [list ::cmdloop::Read stdin stdout ::cmdloop::state_stdin]
}

proc ::cmdloop::listen {{myaddr 0.0.0.0} {port 5441}} {
    if {$port ne {}} {
        socket -server ::cmdloop::Accept -myaddr $myaddr $port
    }
}

# Local variables:
# mode: tcl
# End:
