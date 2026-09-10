# new contact list implementation using [ttk::treeview]
# mostly stolen and adapted from tkchat original contact list
# requires Tk 8.7-
if {![package vsatisfies [package provide Tk] 8.7-]} return

namespace eval ::newRoster {
    variable cl
    variable versions {} ; # cache for jabber:iq:version for user's roster

    namespace import ::msgcat::mc
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
    $cl tag configure MULTIPLE \
        -font TkHeadingFont \
        -background gray92

    set indent    0
    # determine the row height
    set height [expr { int( 1.2 * [font metrics TkDefaultFont -linespace]) }]
    set rowheight [tk::ScaleNum $height]
    foreach theme [ttk::style theme names] {
        ttk::style theme settings $theme {
            ttk::style configure Roster.Treeview \
                -indent $indent \
                -rowheight $rowheight
            # remove focus indicator in the roster
            ttk::style map Roster.Treeview.Row -focusthickness {}
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
        if {[ttk::style map Roster.Treeview.Row -focusthickness] ne ""} {
            ttk::style map Roster.Treeview.Row -focusthickness {}
        }
    }} $indent $rowheight]

    bind $cl <Motion> [namespace code {TrackMotion %W %x %y}]
    return $f
}

proc ::newRoster::updateOnlineNames {} {
    global Options
    variable URLID 0
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
		set role [tkchat::get_role $nick]
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
	    set tags [list NICK-$nick $id $network]
	    $cl insert $where end -text $nick -tags $tags -image $image

	    if { [info exists OnlineUsers($network-$nick,jid)] } {
		$cl tag bind $id <Button-1> \
		    [list tkjabber::getChatWidget $conference/$nick $nick]
		after idle [namespace code [list SetUserTooltip $nick]]
	    }

	    set script [list tkchat::OnNamePopup $nick $network %X %Y]
	    $cl tag bind $id <Button-3> $script
	    $cl tag bind $id <Control-Button-1> $script
	}
    }

    $cl heading #0 -text [format [mc "%d Users Online"] $total]
    $cl yview moveto [lindex $scrollview 0]
}

proc ::newRoster::updateRosterDisplay {} {
    variable cl
    variable versions
    variable ::tkchat::OnlineUsers
    variable ::tkjabber::jabber

    $cl delete [$cl tag has ROSTER]
    foreach tag [lsearch -all -inline [$cl tag names] ROSTER-*] {
	$cl tag delete $tag
    }

    set roster [$jabber getrostername]
    set users [$roster getusers]
    if {([llength $users] == 0) || $OnlineUsers(Roster,hideMenu)} {
	return
    }

    set online {}; # list of online users, with resource
    foreach user [lsort $users] {
	set name [$roster getname $user]
	if {$name eq ""} {
	    set name [tkjabber::jid node $user]
	}
	set allpres [$roster getpresence $user -type available]
	set len [llength $allpres]

	switch -- $len {
	    0 - 1 {
		# unavailable or only one resource online
		set pres [lindex $allpres 0]
		if {$len == 1} {
		    set user $user/[dict get $pres -resource]
		    lappend online $user
		}
		InsertRosterItem $user $name $pres Roster
	    }
	    default {
		# more than one resource available
		set parent [$cl tag has MULTIPLE-$user]
		if {$parent eq ""} {
		    set parent [$cl insert Roster end \
			-text "$name ($len)" \
			-tags [list MULTIPLE MULTIPLE-$user] \
			-image ::tkchat::roster::online]
		} else {
		    $cl move $parent Roster end
		    $cl item $parent -text "$name ($len)"
		}
		foreach pres $allpres {
		    set resource [dict get $pres -resource]
		    set userres $user/$resource
		    InsertRosterItem $userres $resource $pres $parent
		    lappend online $userres
		}
	    }
	}
    }
    # remove empty items with tag MULTIPLE
    foreach item [$cl tag has MULTIPLE] {
	if {[$cl children $item] eq ""} {
	    set tag [lindex [$cl item $item -tags] 1]
	    $cl tag delete $tag
	    $cl delete $item
	}
    }
    # remove offline cached versions
    set versions [dict filter $versions script {k v} {
	expr {$k in $online}
    }]
}

proc ::newRoster::InsertRosterItem {user name pres parent} {
    variable cl
    variable versions

    set img "disabled"
    if {[dict size $pres] != 0} {
	# item is online
	if {[dict exists $pres -show]} {
	    set img [dict get $pres -show]
	} else {
	    set img "online"
	}
    }
    set id ROSTER-$user
    set tags [list ROSTER $id Jabber]
    set item [$cl insert $parent end \
	-text $name \
	-tags $tags \
	-image ::tkchat::roster::$img]
    # set up tip
    set tip $user
    if {[dict exists $versions $user]} {
	append tip "\n" [dict get $versions $user]
    }
    tooltip $cl -item $item $tip
    # item bindings
    $cl tag bind $id <Button-1> [list tkjabber::getChatWidget \
	$user $name]
    set script [list newRoster::RosterPopup $user $name %X %Y]
    $cl tag bind $id <Button-3> $script
    $cl tag bind $id <Control-Button-1> $script
}

proc ::newRoster::RosterPopup {user name x y} {
    variable cl

    set m [winfo parent $cl].rostermenu
    destroy $m
    menu $m
    $m add command \
	-label [mc "Send message"] \
	-command [list tkchat::SendMemo $user]
    $m add command \
	-label [mc "Private chat"] \
	-command [list tkjabber::getChatWidget $user $name]
    $m add command \
	-label [mc "User info"] \
	-command [list tkchat::UserInfoDialog $user]
    if {[string match "*/*" $user]} {
	$m add command \
	    -label [mc "Version info"] \
	    -command [list newRoster::queryVersion $user]
    }
    tk_popup $m $x $y
}

proc ::newRoster::queryVersion {jid} {
    variable ::tkjabber::jabber

    set xmllist [wrapper::createtag query -attrlist {xmlns jabber:iq:version}]
    $jabber send_iq get [list $xmllist] \
	-to $jid \
	-command [list newRoster::gotVersion $jid]
}

proc ::newRoster::gotVersion {jid type xmllist} {
    variable cl
    variable versions

    if {$type ne "result"} {
	tkchat::addStatus 0 [mc "error getting version for %s" $jid]
	return
    }
    set data {}
    foreach sub [wrapper::getchildren $xmllist] {
	dict set data [wrapper::gettag $sub] [wrapper::getcdata $sub]
    }
    set ver ""
    if {[dict exists $data name]} {
	append ver [dict get $data name]
    }
    if {[dict exists $data version]} {
	append ver " " [dict get $data version]
    }
    if {[dict exists $data os]} {
	append ver " : [dict get $data os]"
    }
    dict set versions $jid $ver
    tkchat::addStatus 0 "$jid is using $ver"
    tooltip $cl -item [$cl tag has ROSTER-$jid] $jid\n$ver
    return 1
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

    if {![info exists OnlineUsers(Jabber-$nick,jid)]} {
	return
    }
    set tip [string trim $OnlineUsers(Jabber-$nick,jid)]
    if {$tip eq ""} {
	append tip $nick
    }
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
