package require Tk

# new contact list implementation using [ttk::treeview]
# mostly stolen and adapted from tkchat original contact list
#

namespace eval ::newRoster {
    variable cl

    namespace import ::tooltip::tooltip
}

proc ::newRoster::gui {f} {
    variable cl

    ttk::frame $f
    set cl [ttk::treeview $f.tv \
        -yscrollcommand [list $f.sy set] \
        -height 20 \
        -selectmode none \
        -style Roster.Treeview]
    ttk::scrollbar $f.sy -command [list $cl yview]

    # create fixed items
    $cl insert {} end -id Roster \
        -text [mc "Your contacts"] \
        -open true \
        -tags TITLE
    $cl insert {} end -id Jabber \
        -open true \
        -tags TITLE
    $cl insert Jabber end -id Moderator \
        -open true \
        -text [mc "Moderators"] \
        -tags SUBTITLE
    $cl insert Jabber end -id Participant \
        -open true \
        -text [mc "Participants"] \
        -tags SUBTITLE
    $cl detach Roster
    $cl detach Jabber

    pack $f.sy -side right -fill y
    pack $cl -expand 1 -fill both

    $cl tag configure TITLE \
        -font TkHeadingFont \
        -background gray70
    $cl tag configure SUBTITLE \
        -font TkHeadingFont \
        -background gray85

    set indent    0
    # determine the row height
    set height [expr { int( 1.2 * [font metrics TkDefaultFont -linespace]) }]
    set rowheight [tk::ScaleNum $height]
    foreach theme [ttk::style theme names] {
        ttk::style theme settings $theme {
            ttk::style configure Roster.Treeview \
                -indent $indent \
                -rowheight $rowheight
        }
    }

    # New themes can be added at runtime.
    # Adjust the indent and row height accordingly upon selection.
    # This will cause a visual glitch the first time the theme is selected.
    bind $cl <<ThemeChanged>> [list apply {{indent rowheight} {
        if {[ttk::style configure Roster.Treeview -indent] != $indent ||
            [ttk::style configure Roster.Treeview -rowheight] != $rowheight
        } then {
            after idle [list ttk::style configure Roster.Treeview \
                    -indent $indent \
                    -rowheight $rowheight]
        }
    }} $indent $rowheight]

    bind $cl <Motion> [namespace code {TrackMotion %W %x %y}]
    return $f
}

proc ::newRoster::updateOnlineNames {} {
    global Options URLID
    variable cl
    variable ::tkchat::OnlineUsers
    variable ::tkjabber::conference

    set scrollview [$cl yview]
    # Delete all URL-* tags to prevent a huge memory leak
    foreach tag [lsearch -all -inline [$cl tag names] URL-*] {
	$cl tag delete $tag
    }
    # clean up the tooltip info
    tooltip clear $cl*

    if {$Options(Visibility,ROSTER)} {
	$cl move Roster {} end
	updateRosterDisplay
    } else {
	$cl detach Roster
    }
    set total 0
    foreach network $OnlineUsers(networks) {
	if {![$cl exists $network]} {
	    $cl insert {} end -id $network -open true -tags TITLE
	}

	set userCnt [llength $OnlineUsers($network)]
	if { !$userCnt } {
	    $cl detach $network
	    continue
	}
	incr total $userCnt
	$cl move $network {} end
	if {$network eq "Jabber"} {
	    $cl delete [$cl children Moderator]
	    $cl delete [$cl children Participant]
	} else {
	    $cl delete [$cl children $network]
	}
	$cl item $network \
	    -text [format [mc "%d %s Users"] $userCnt $network]

	foreach nick $OnlineUsers($network) {
	    set status [lindex $OnlineUsers($network-$nick,status) 0]
	    set role participant
	    if {$network eq "Jabber"} {
		set role [::tkchat::get_role $nick]
		set where [expr {
				 $role eq "moderator" ?
				 "Moderator" :
				 "Participant"
			     }]
	    } else {
		set where $network
	    }
	    if {[info exists Options(Visibility,NICK-$nick)] &&
		$Options(Visibility,NICK-$nick)
	    } {
		set status disabled
	    }
	    if {$role eq "visitor"} {
		set status disabled
	    }
	    
	    #Custom colors do not work well on Aqua because
	    #of Dark Mode, use defaults instead.
	    if {[tk windowingsystem] ne "aqua"} {
	    if { ![info exists Options(Color,NICK-$nick)] } {
		set Options(Color,NICK-$nick) $Options(Color,MainFG)
	    }
	    $cl tag configure NICK-$nick -foreground \
		#[lindex $Options(Color,NICK-$nick) 1]
	    }

	    switch -exact -- $status {
		online - chat - dnd - away - xa {
		    set image ::tkchat::roster::$status
		}
		disabled - offline {
		    set image ::tkchat::roster::disabled
		}
	    }

	    set id URL-[incr URLID]
	    set tags [list NICK NICK-$nick URL $id $network]
	    $cl insert $where end -text $nick -tags $tags -image $image

	    if { [info exists OnlineUsers($network-$nick,jid)] } {
		$cl tag bind $id <Button-1> \
		    [list ::tkjabber::getChatWidget $conference/$nick $nick]
		after idle [namespace code [list SetUserTooltip $nick]]
	    }

	    set script [list ::tkchat::OnNamePopup $nick $network %X %Y]
	    $cl tag bind $id <Button-3> $script
	    $cl tag bind $id <Control-Button-1> $script
	}
    }

    $cl heading #0 -text [format [mc "%d Users Online"] $total]
    $cl yview moveto [lindex $scrollview 0]
}

proc ::newRoster::updateRosterDisplay {} {
    global URLID
    variable cl
    variable ::tkchat::OnlineUsers
    variable ::tkjabber::jabber

    $cl delete [$cl children Roster]

    set roster [$jabber getrostername]
    set users [$roster getusers]
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

	set id URL-[incr URLID]
	set tags [list ROSTER ROSTER-$user URL $id Jabber]
	set item [$cl insert Roster end -text $name -tags $tags -image $image]

	$cl tag bind $id <Button-1> [list ::tkjabber::getChatWidget \
					 $user/$resource $name]

	set tip $user
	if {$resource ne {}} {append tip /$resource}
	foreach res [$roster getresources $user] {
	    append tip "\n  $res"
	}
	tooltip $cl -item $item $tip
    }
}

proc ::newRoster::PutIntoPane {} {
    global Options

    set w .pane.contactlist
    gui $w
    set Options(NamesWin) $w
}

# use a widget level binding because [treeview] lacks <Enter>/<Leave>
# events on tags
proc ::newRoster::TrackMotion {w x y} {
    set item [$w identify item $x $y]
    if {[$w tag has Jabber $item]} {
        $w configure -cursor hand2
    } else {
        $w configure -cursor {}
    }
}

proc ::newRoster::SetUserTooltip {nick} {
    variable cl
    variable ::tkchat::OnlineUsers

    if {![info exists OnlineUsers(Jabber-$nick,jid)]} { return }
    set tip [string trim $OnlineUsers(Jabber-$nick,jid)]
    if {$tip eq ""} { append tip $nick }
    if {[info exists OnlineUsers(Jabber-$nick,version)]} {
        append tip "\n$OnlineUsers(Jabber-$nick,version)"
    }
    append tip "\nrole: $OnlineUsers(Jabber-$nick,role)"
    set status [lindex $OnlineUsers(Jabber-$nick,status) 1]
    if {$status ne {}} {
        append tip "\nstatus: $status"
    }
    set tip [string trim $tip "\n"]
    tooltip $cl -item [$cl tag has NICK-$nick] $tip
}
