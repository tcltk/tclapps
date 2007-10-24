# picoirc.tcl -
#
#	This is a Tk GUI for the picoirc package that provides a simple
#	IRC client. It makes use of the chatwidget from tklib
#
# Copyright (C) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution of
# this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: picoirc.tcl,v 1.2 2007/10/24 10:35:25 patthoyts Exp $

package require Tk 8.5
package require chatwidget 1.0;  # tklib 
package require picoirc 0.5;     # tcllib

variable ircuid
if {![info exists ircuid]} { set ircuid -1 }

# -------------------------------------------------------------------------

proc Main {args} {
    global env
    array set opts {-debug 0 -nick "" -name ""}
    if {[info exists env(IRCNICK)]} {set opts(-nick) $env(IRCNICK)}
    if {[info exists env(IRCNAME)]} {set opts(-nick) $env(IRCNAME)}
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -nick   { set opts(-nick) [Pop args 1] }
            -name   { set opts(-name) [Pop args 1] }
            -debug  { set opts(-debug) 1 }
            --      { Pop args ; break }
            default { break }
        }
        Pop args
    }
    
    set app [toplevel .chat -class PicoIRC]
    wm withdraw $app
    wm title $app "picoIRC"
    
    set menu [menu $app.menu -tearoff 0]
    $menu add cascade -label Network -menu [menu $menu.file -tearoff 0]
    $menu.file add command -label "Login..." -underline 0 \
        -command [list [namespace origin LoginIRC] $app]
    $menu.file add separator
    $menu.file add command -label Exit \
        -command [list [namespace origin Exit] $app]
    $app configure -menu $menu

    ttk::notebook $app.nb

    if {$opts(-debug)} {
        set debug [ttk::frame $app.nb.debug -style ChatwidgetFrame]
        text $debug.text -relief flat -borderwidth 0 -wrap word \
            -yscrollcommand [list $debug.vs set] 
        $debug.text tag configure read -foreground blue3
        $debug.text tag configure write -foreground red3
        ttk::scrollbar $debug.vs -command [list $debug.text yview]
        grid $debug.text $debug.vs -sticky news -padx 1 -pady 1
        grid rowconfigure $debug 0 -weight 1
        grid columnconfigure $debug 0 -weight 1
        $app.nb add $debug -text Debug
    }

    set status [ttk::frame $app.status]
    ttk::label $status.pane0 -anchor w
    ttk::separator $status.sep0 -orient vertical
    ttk::label $status.pane1 -anchor w
    ttk::separator $status.sep1 -orient vertical
    ttk::sizegrip $status.sizegrip
    grid $status.pane0 $status.sep0 $status.pane1\
        $status.sep1 $status.sizegrip -sticky news
    grid columnconfigure $status 0 -weight 1
    grid rowconfigure $status 0 -weight 1

    grid $app.nb -sticky news
    grid $status -sticky sew
    grid rowconfigure $app 0 -weight 1
    grid columnconfigure $app 0 -weight 1

    bind $app <Control-F2> {console show}

    wm geometry .chat 600x400
    wm deiconify $app

    set uri [lindex $args 0]
    if {$opts(-nick) ne "" && $uri ne ""} {
        foreach {server port channel} [picoirc::splituri $uri] break
        after idle [list [namespace origin IrcConnect] $app \
                        -server $server -port $port \
                        -channel $channel -nick $opts(-nick)]
    }

    tkwait window $app
    return
}

proc LoginIRC {app} {
    set dlg $app.irclogin
    variable $dlg {}
    variable irc
    if {![info exists irc]} {
        array set irc {server irc.freenode.net port 6667 channel "" passwd ""}
    }
    if {![winfo exists $dlg]} {
        set dlg [toplevel $dlg -class Dialog]
        wm withdraw $dlg
        wm transient $dlg $app
        wm title $dlg "IRC Login"
        
        set f [ttk::frame $dlg.f]
        set g [ttk::frame $f.g]
        ttk::label $f.sl -text Server -anchor w
        ttk::entry $f.se -textvariable [namespace which -variable irc](server)
        ttk::entry $f.sp -textvariable \
            [namespace which -variable irc](port) -width 5
        ttk::label $f.nl -text Username -anchor w
        ttk::entry $f.nn -textvariable [namespace which -variable irc](nick)
        ttk::label $f.pl -text Password -anchor w
        ttk::entry $f.pw -show * -textvariable [namespace which -variable irc](passwd)
        ttk::label $f.cl -text Channel -anchor w
        ttk::entry $f.cn -textvariable [namespace which -variable irc](channel)
        ttk::button $f.ok -text Login -default active \
            -command [list set [namespace which -variable $dlg] "ok"]
        ttk::button $f.cancel -text Cancel \
            -command [list set [namespace which -variable $dlg] "cancel"]

        bind $dlg <Return> [list $f.ok invoke]
        bind $dlg <Escape> [list $f.cancel invoke]
        wm protocol $dlg WM_DELETE_WINDOW [list $f.cancel invoke]
        
        grid $f.sl $f.se $f.sp -in $g -sticky new -padx 1 -pady 1
        grid $f.nl $f.nn -     -in $g -sticky new -padx 1 -pady 1
        grid $f.pl $f.pw -     -in $g -sticky new -padx 1 -pady 1
        grid $f.cl $f.cn -     -in $g -sticky new -padx 1 -pady 1
        grid columnconfigure $g 1 -weight 1

        grid $g    -         -sticky news
        grid $f.ok $f.cancel -sticky e -padx 1 -pady 1
        grid rowconfigure    $f 0 -weight 1
        grid columnconfigure $f 0 -weight 1
        
        grid $f -sticky news
        grid rowconfigure $dlg 0 -weight 1
        grid columnconfigure $dlg 0 -weight 1

	wm resizable $dlg 0 0
        raise $dlg
    }     

    catch {::tk::PlaceWindow $dlg widget $app}
    wm deiconify $dlg
    tkwait visibility $dlg
    focus -force $dlg.f.ok
    grab $dlg
    vwait [namespace which -variable $dlg]
    grab release $dlg
    wm withdraw $dlg

    if {[set $dlg] eq "ok"} {
        after idle [list [namespace origin IrcConnect] $app \
                        -server $irc(server) -port $irc(port) \
                        -channel $irc(channel) \
                        -nick $irc(nick) -passwd $irc(passwd)]
    }
}

proc Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

proc Exit {app} {
    destroy $app
    exit
}

proc Status {Chat message} {
    upvar #0 $Chat chat
    $chat(app).status.pane0 configure -text $message
}

proc State {Chat message} {
    upvar #0 $Chat chat
    $chat(app).status.pane1 configure -text $message
}

proc bgerror {args} {
    tk_messageBox -icon error -title "Error" -message $::errorInfo
}

proc Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

proc ChannelNickMenu {Channel w x y} {
    set nick [string trim [$w get "@$x,$y linestart" "@$x,$y lineend"]]
    if {$nick eq ""} { return }
    destroy $w.popup
    set m [menu $w.popup -tearoff 0]
    $m add command -label "$nick" -state disabled
    $m add separator
    $m add command -label "Whois" -underline 0 \
        -command [list [namespace origin ChannelNickCommand] $Channel whois $nick]
    $m add command -label "Version" \
        -command [list [namespace origin ChannelNickCommand] $Channel version $nick]
    tk_popup $m [winfo pointerx $w] [winfo pointery $w]
}

proc ChannelNickCommand {Channel cmd nick} {
    upvar #0 $Channel chan
    switch -exact -- $cmd {
        whois { picoirc::send $chan(irc) "WHOIS $nick" }
        version { picoirc::send $chan(irc) "PRIVMSG $nick :\001VERSION\001" }
        default {}
    }
}

# -------------------------------------------------------------------------
# Handle the IRC transport (using picoirc)

proc IrcConnect {app args} {
    variable ircuid
    set id irc[incr ircuid]
    set Chat [namespace current]::$id
    upvar #0 $Chat chat
    array set chat [list app $app type irc passwd "" nick ""]
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -server   { set chat(server)   [Pop args 1] }
            -port     { set chat(port)     [Pop args 1] }
            -channel  { set chat(channel)  [Pop args 1] }
            -nick     { set chat(nick)     [Pop args 1] }
            -passwd   { set chat(passwd)   [Pop args 1] }
            default {
                return -code error "invalid option \"$option\""
            }
        }
        Pop args
    }
    set chat(window) [chatwidget::chatwidget $app.$id]
    $chat(window) names hide
    set chat(targets) [list]
    $app.nb add $chat(window) -text $chat(server)
    $app.nb select $chat(window)
    set url irc://$chat(server):$chat(port)
    if {[info exists chat(channel)] && $chat(channel) ne ""} {
        append url /$chat(channel)
    }
    set chat(irc) [picoirc::connect \
                       [list [namespace origin IrcCallback] $Chat] \
                       $chat(nick) $chat(passwd) $url]
    $chat(window) hook add post [list ::picoirc::post $chat(irc) ""]
    bind $chat(window) <Destroy> "+unset -nocomplain $Chat"
    return $Chat
}

proc IrcJoinChannel {Chat args} {
    variable ircuid
}

proc IrcAddChannel {Chat channel} {
    upvar #0 $Chat chat
    set Channel "${Chat}/$channel"
    upvar #0 $Channel chan
    array set chan [array get chat]
    set chan(channel) $channel
    set chan(window) [chatwidget::chatwidget $chat(window)$channel]
    lappend chat(targets) [list $channel $chan(window)]
    $chat(app).nb add $chan(window) -text $channel
    $chat(app).nb select $chan(window)
    set m0 [font measure ChatwidgetFont {[00:00]m}]
    set m1 [font measure ChatwidgetFont [string repeat m 10]]
    $chan(window) chat configure -tabs [list $m0 [expr {$m0 + $m1}]]
    $chan(window) chat tag configure MSG -lmargin2 [expr {$m0 + $m1}]
    $chan(window) chat tag configure NICK -font ChatwidgetBoldFont
    $chan(window) chat tag configure TYPE-system -font ChatwidgetItalicFont
    $chan(window) names tag bind NICK <Button-3> \
        [list [namespace origin ChannelNickMenu] $Channel %W %x %y]
    $chan(window) names tag bind NICK <Enter> \
        [list [namespace origin IrcNickTooltip] $Chat enter %W %x %y]
    $chan(window) names tag bind NICK <Leave> \
        [list [namespace origin IrcNickTooltip] $Chat leave %W %x %y]
    $chan(window) hook add post [list ::picoirc::post $chan(irc) $channel]
    bind $chan(window) <Destroy> "+unset -nocomplain $Channel"
    return
}

proc IrcRemoveChannel {Chat target} {
    upvar #0 $Chat chat
    Status $Chat "Left channel $target"
    set w [IrcFindWindow $Chat $target]
    if {[winfo exists $w]} { destroy $w }
    if {[set ndx [lsearch -index 0 $chat(targets) $target]] != -1} {
        set chat(targets) [lreplace $chat(targets) $ndx $ndx]
    }
}

proc IrcNickTooltip {Chat type w x y} {
    if {[package provide tooltip] eq {}} { return }
    set nick [string trim [$w get "@$x,$y linestart" "@$x,$y lineend"]]
    if {$nick eq ""} { return }
    puts stderr "Tooltip $type $nick"
    return
}

proc IrcFindWindow {Chat target} {
    upvar #0 $Chat chat
    set w $chat(window)
    if {[set ndx [lsearch -nocase -index 0 $chat(targets) $target]] != -1} {
        set w [lindex [lindex $chat(targets) $ndx] 1]
    }
    return $w
}

proc IrcCallback {Chat context state args} {
    upvar #0 $Chat chat
    upvar #0 $context irc
    switch -exact -- $state {
        init {
            Status $Chat "Attempting to connect to $irc(server)"
        }
        connect {
            $chat(window) message "Logging into $irc(server) as $irc(nick)" -type system
            Status $Chat "Connection to IRC server established."
            State $Chat connected
        }
        close {
            if {[llength $args] != 0} {
                $chat(window) message "Failed to connect: [lindex $args 0]" -type system
                Status $Chat [lindex $args 0]
            } else {
                $chat(window) message "Disconnected from server" -type system
                Status $Chat "Disconnected."
            }
            State $Chat !connected
        }
        userlist {
            foreach {target users} $args break
            set colors {black SteelBlue4 tomato chocolate SeaGreen4 red4
                green4 blue4 pink4}
            set w [IrcFindWindow $Chat $target]
            set current [$w name list -full]
            foreach nick $users {
                set opts [list -status online]
                if {[string match @* $nick]} {
                    set nick [string range $nick 1 end]
                    lappend opts -group operators 
                } else { lappend opts -group users }
                if {[lsearch -index 0 $current $nick] == -1} {
                    lappend opts -color \
                        [lindex $colors [expr {int(rand() * [llength $colors])}]]
                }
                eval [list $w name add $nick] $opts
            }
        }
        userinfo {
            foreach {nick userinfo} $args break
            array set info {name {} host {} channels {} userinfo {}}
            array set info $userinfo
            set chat(userinfo,$nick) [array get info]
        }
        chat {
            foreach {target nick msg type} $args break
            if {$type eq ""} {set type normal}
            set w [IrcFindWindow $Chat $target]
            if {$nick eq "tcl@tach.tclers.tk"} {
                set action ""; set jnick "" ; set jnew ""
                if {[regexp {^\s*([^ ]+) is now known as (.*)} $msg -> jnick jnew]} {
                    set action nickchange
                } elseif {[regexp {^\s*([^ ]+) has left} $msg -> jnick]} {
                    set action left
                } elseif {[regexp {^\s*([^ ]+) has become available} $msg -> jnick]} {
                    set action entered
                }
                if {$action ne ""} {
                    IrcCallbackNick $w $action $target $jnick $jnew jabber
                    return
                }
            }
            $w message $msg -nick $nick -type $type
        }
        system {
            foreach {target msg} $args break
            [IrcFindWindow $Chat $target] message $msg -type system
        }
        topic {
            foreach {target topic} $args break
            set w [IrcFindWindow $Chat $target]
            $w topic show
            $w topic set $topic
        }
        traffic {
            foreach {action target nick new} $args break
            if {$nick eq $irc(nick)} {
                switch -exact -- $action {
                    left    { IrcRemoveChannel $Chat $target }
                    entered { IrcAddChannel $Chat $target}
                    nickchange { set irc(nick) $new }
                }
            }
            if {$target ne {}} {
                set w [IrcFindWindow $Chat $target]
                IrcCallbackNick $w $action $target $nick $new
            } else {
                foreach window_target $chat(targets) {
                    foreach {window_channel w} $window_target break
                    set current [$w name list -full]
                    if {[lsearch -index 0 $current $nick] != -1} {
                        IrcCallbackNick $w $action $target $nick $new
                    }
                }
            }
        }
        debug {
            foreach {type line} $args break
            if {[winfo exists $chat(app).nb.debug.text]} {
                $chat(app).nb.debug.text insert end "$line\n" $type
            }
            # You can log raw IRC to file by uncommenting the following lines:
            #if {![info exists chat(log)]} {set chat(log) [open irc.log a]}
            #puts $chat(log) "[string toupper [string range $type 0 0]] $line"
        }
        version { return "" }
        default {
            $chat(window) message "unknown irc callback \"$state\": $args" -type error
        }
    }
}

proc IrcCallbackNick {w action target nick new {group users}} {
    #puts stderr "process traffic $w $nick $action $new $target"
    if {$action eq "nickchange"} {
        $w name delete $nick
        $w name add $new -group $group
        $w message "$nick changed to $new" -type system
    } else {
        switch -exact -- $action {
            left    { $w name delete $nick }
            entered { $w name add $nick -group $group }
        }
        $w message "$nick $action" -type system
    }
}

# -------------------------------------------------------------------------

if {![info exists initialized] && !$tcl_interactive} {
    set initialized 1
    wm withdraw .
    set r [catch [linsert $argv 0 Main] err]
    if {$r} {tk_messageBox -icon error -type ok -message $::errorInfo}
    exit $r
}

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:
