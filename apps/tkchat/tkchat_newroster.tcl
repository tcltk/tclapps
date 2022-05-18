package require Tk

namespace eval ::newRoster {
    variable cl
}

proc ::newRoster::gui {{parent ""}} {
    variable cl
    set t [toplevel $parent.contactList]
    set cl [ttk::treeview $t.tv -show tree -yscrollcommand [list $t.sy set] \
            -height 20 -selectmode none -class Roster]
    ttk::scrollbar $t.sy -command [list $cl yview]

    pack $t.sy -side right -fill y
    pack $cl -expand 1 -fill both

    $cl tag configure TITLE -font TkHeadingFont
    $cl tag configure SUBTITLE -font TkHeadingFont
    # steal bindings from Treeview
    foreach event {<Button-1> <Up> <Down> <Right> <Left> <Prior> <Next>} {
        bind Roster $event  [bind Treeview $event]
    }
    bind Roster <Motion>   { newRoster::TrackMotion %W %x %y }
    return $t
}

proc ::newRoster::TrackMotion {w x y} {
    set item [$w identify item $x $y]
    if {$item eq {}} {
        return
    }
    if {[$w tag has Jabber $item]} {
        $w configure -cursor hand2
    } else {
        $w configure -cursor {}
    }
}

proc ::newRoster::updateOnlineNames {} {
    variable cl
    global Options
    variable ::tkchat::OnlineUsers

    if {![winfo exists $cl]} {gui}

    set scrollview [$cl yview]

    $cl delete [$cl children {}]
    # Delete all URL-* tags to prevent a huge memory leak
    foreach tag [lsearch -all -inline [$cl tag names] URL-*] {
        $cl tag delete $tag
    }
    if {$Options(Visibility,ROSTER)} {
        updateRosterDisplay
    }
    set total 0
    foreach network $OnlineUsers(networks) {
        set userCnt [llength $OnlineUsers($network)]
        if { !$userCnt } {
            continue
        }
        incr total $userCnt
        $cl insert {} end -id $network -open true \
                -text "$userCnt $network Users" \
                -tags SUBTITLE
        if {$network eq "Jabber"} {
            $cl insert $network end -id Moderator -open true \
                    -text "Moderators" -tags SUBTITLE
            $cl insert $network end -id Participant -open true \
                    -text "Participants" -tags SUBTITLE
        }
        foreach nick $OnlineUsers($network) {
            set status [lindex $OnlineUsers($network-$nick,status) 0]
            set role participant
            if {$network eq "Jabber"} {
                set role [::tkchat::get_role $nick]
            }
            if {[info exists Options(Visibility,NICK-$nick)] &&
                    $Options(Visibility,NICK-$nick)} {
                set status disabled
            }
            if {$role eq "visitor"} {
                set status disabled
            }
            set where [expr {$role eq "moderator"?"Moderator":"Participant"}]

            if { ![info exists Options(Color,NICK-$nick)] } {
                set Options(Color,NICK-$nick) $Options(Color,MainFG)
            }
            $cl tag configure NICK-$nick -foreground \
                    #[lindex $Options(Color,NICK-$nick) 1]

            switch -exact -- $status {
                online - chat - dnd - away - xa {
                    set image ::tkchat::roster::$status
                }
                disabled - offline {
                    set image ::tkchat::roster::disabled
                }
            }
            if { [info exists OnlineUsers($network-$nick,jid)] } {
                set tags [list NICK NICK-$nick URL URL-[incr ::URLID] $network]
                $cl insert $where end -text "$nick" -tags $tags -image $image
                $cl tag bind URL-$::URLID <Button-1> [list \
                        ::tkjabber::getChatWidget \
                        $::tkjabber::conference/$nick $nick]
            } else {
                $cl insert $network end -text "$nick" -image $image \
                        -tags [list NICK NICK-$nick URL-[incr ::URLID]]
            }
        }
    }
    $cl insert {} 0 -text "$total Users Online" -tags TITLE
    $cl yview moveto [lindex $scrollview 0]
}

proc ::newRoster::updateRosterDisplay {} {
    variable cl
    variable ::tkchat::OnlineUsers
    variable ::tkjabber::jabber
    set roster [$jabber getrostername]
    set users [$roster getusers]

    $cl insert {} end -id Roster -text "Your contacts" \
            -tags SUBTITLE -open true

    if {([llength $users] == 0) || $OnlineUsers(Roster,hideMenu)} {
        return
    }
    foreach user $users {
        set name [$roster getname $user]
        if {$name eq ""} {
            set name [tkjabber::jid node $user]
        }
        set resource [$roster gethighestresource $user]
        foreach pres [$roster getpresence $user] {
            array set a [linsert $pres 0 -show online -type unavailable]
            if {$resource eq $a(-resource)} {
                if {$a(-type) eq ""} {
                    set img online
                }
                if {$a(-show) ne ""} {
                    set img $a(-show)
                }
                if {$a(-type) eq "unavailable"} {
                    set img "disabled"
                }
                if {$img eq "offline"} {
                    set img "disabled"
                }
                set image ::tkchat::roster::$img
            }
        }

        set link URL-[incr ::URLID]
        set tags [list ROSTER ROSTER-$user URL $link Jabber]

        $cl insert Roster end -text $name -tags $tags -image $image
        $cl tag bind $link <Button-1> \
            [list ::tkjabber::getChatWidget $user/$resource $name]
    }
}

proc ::newRoster::PutIntoPane {} {
    global Options
    set w [gui .pane]
    wm forget $w
    set Options(NamesWin) $w
}
