# -------------------------------------------------------------------------
# This file is part of the tkchat application
#
# Copyright (c) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# $Id: tkchat_rss.tcl,v 1.11 2007/10/11 22:53:45 patthoyts Exp $
# -------------------------------------------------------------------------

if {[catch {package require rssrdr_oo}]} { return }

namespace eval ::tkchat::rss {
    variable version 1.0.0

    namespace import ::tooltip::tooltip
}

proc ::tkchat::rss::Init {} {
    variable Init
    if {![info exists Init]} {
        set Init 1
    } else {
        return
    }

    global Options
    variable RSStip     {}
    variable Rss        {}
    variable RssUrlId   0

    if {[llength [array names Options RSS,watch,*]] > 0} {

        # One time status bar init
        image create photo ::tkchat::img::feedLo -data {
            R0lGODlhDgAOAOeIANt+M9yHM+iDM+qDM+eHM+mHM+iIM+qHM+eKM+iKM+iL
            M+iNM+iPM/GLM+iQM/KLM/OLM++PM+iTM+iUNeyUM/ePM/mPM/GUM+6WM+eZ
            O/aTM+iZO+yVTuqaOfmUM/2SM+yXTv6TM++WTv+TM+udPP2WM/CcN+ibU+ya
            T/mZM+qfP+ufP/edM9Sjc/CgPfKdVNqlcvehOfygNfWfUPqjOfqjO/KnQvyk
            O++pRf6lO/mnP++uSPerQ++uSvCuSNqxe++pbv6rP/+rP+awbduze/OwS/Sw
            S/yuRPmvR/WvXv+vRe+0YfGxc/C3W/m2S/61SP+1SPy3TPK4Z/mzcf24TO62
            h/K6afu7T/+6TP69UPW8b/++UPG9hN/Cnf3BU+fDi/3CVf7CU//DVfzGWOPE
            ru7Dmv7HWf/HWevFnO3HlPfJb/bIee7Fo/DKhPXHievKn+/IpPXJm/POl/LP
            qe7WrvHZr/nbp/Pg0/Pj1PHl3fDn3/bn3PPo3/no1fPq3/fq3/vp3frs1/vs
            3/nu3/3t3/n28/r28/r39P//////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            /////////////////////yH5BAEKAP8ALAAAAAAOAA4AAAjOAP9ZUZEhw4aD
            CFdY+SelSBs6deyoGWPmjEUjSzp4+YKoI6JAa8CIEROmw4QsTYj86OKno5wr
            W7ZMkIAlyo4eTny8OYQoDRUsEhxAwdFxkBskQwwh0vLEAQMlNvDo6TiHBxpE
            fY4wWCAkiA4SLQoh4uKCD6IkCxTkqGHiBg0YiP7EKIMojoIELAQhIiQDQx5E
            FJgggoPAwAmPF1LcQRRgCiI2BA5E2IMIkIcSKIBomEGmSoEXEBoAqBBiRIjS
            Hyw8APFPxAABsGPDHsDhX0AAOw==
        }
        image create photo ::tkchat::img::feedHi -data {
            R0lGODlhDgAOAOeIAMUiAMY0ANorAN0tANk0ANw0AN00ANo2ANk4ANo4ANo8
            ANo/ANpBAOs5AOg8ANpFAO08AOZBANpIANpMAPJBAOJNAPdDAOhMAPFIAORP
            ANlWANpWALlmCN9QH91XAP1HANpZAP5IAOFRH/dMAP9IAOJXAN9TH95cAPxP
            AN1fAOdaAMRrBt5fAPdWAOdhAOZbJPJcAO5gAPNjAPphAPhlAPhmAOVwAOtt
            AOZwAMJ+G/poANd9AMWBHfduAP5rAOV5AOZ5APJ0AOd6AO56AP50AOh+CO19
            AO59AP91APV7APp6AOKDH/97AOeJAPWBBfeFAOSFNP6EAP+EAOaKJOaMJPqK
            AO6RAP2LAPmQAP+NAOiTLsuaXP6TANmdPP+VAP2ZAP2aAP6aAP+dANKeefqh
            AOSdV/OmAN6gWvGlF+enLv6kAOOkTP+kAOSgZvCkN96nX+alaPCmWOyuUeux
            cOS8eujBevXEbezNt+3TuejVyOfXy+zay/HZxvfauuzdzPLdy/jcyPjiv/jg
            y/fkzPzjy/fx7fjx7fjy7v//////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////
            /////////////////////yH5BAEKAP8ALAAAAAAOAA4AAAjOAP9RSaFBw4aD
            CFlQ+TfFSBo6deyYIaOGjcUjSzx86YKoI6JAaMCIERPGwwQuTXjk2OKnoxws
            XrxMkJClyg8gT4S8OYRozZUsEh5IwdFxkJskOwwhshLlAQMmN/Do6TgnyBlE
            fZQwWICESI8THAoh0uJiD6IhCxT4qKFCB40ViP7IKIMojoIEMAQhIjQjQx5E
            FYoggoPgAAiPF1rcQRTACaI2BAxE4IMI0AgUJWxgiDEGSoEXEBwAoBCCRIjS
            Hyw0MPFPxAABsGPDHtDhX0AAOw==
        }

        # At this time, the only interface is via the statusbar
        if {[winfo exists .status] && ![winfo exists .status.rss]} {
            ttk::label .status.rss -image ::tkchat::img::feedLo
            bind .status.rss <Button-1> \
                [list [namespace origin ShowRssInfo]]
            bind .status.rss <Button-3> \
                [list [namespace origin ShowRssInfo2]]

            set var [namespace which -variable RSStip]
            trace add variable $var write \
                [list after 0 [namespace origin RssUpdateTip]]

            after idle [namespace origin CheckRSSFeeds]
        }
    }
}

proc ::tkchat::rss::RssUpdateTip {varname op} {
    variable RSStip
    tooltip .status.rss [string trim $RSStip \n]
}

proc ::tkchat::rss::ShowRssInfo {} {
    variable Rss
    variable RssUrlId
    if {[winfo exists .status.rss]} {
        .status.rss configure -image ::tkchat::img::feedLo
    }
    set dlg .rssinfo
    variable $dlg

    if {[winfo exists $dlg]} {
        wm deiconify $dlg
        focus -force $dlg.bf.ok
        return
    }

    set dlg [::tkchat::Dialog $dlg]
    wm withdraw $dlg
    wm title $dlg "RSS Feeds"
    wm transient $dlg .

    set nb [ttk::notebook $dlg.nb]
    set page 0

    array set channel {}
    array set a {}
    foreach {url token} $Rss {
        if {[$token status] ne "ok"} { continue }
        set f [ttk::frame $nb.page$page]
        set txt [text $f.txt -borderwidth 0 -font FNT]
        set sb [ttk::scrollbar $f.vs -command [list $txt yview]]
        $txt configure -yscrollcommand [list $sb set]
        
        $txt tag bind URL <Enter> [list $txt configure -cursor hand2]
        $txt tag bind URL <Leave> [list $txt configure -cursor {}]
        $txt tag configure TITLE -font NAME -spacing3 2
        $txt tag configure ITEM -lmargin1 6
        $txt tag configure URL -underline 1
        $txt tag bind URL <Enter> [list $txt configure -cursor hand2]
        $txt tag bind URL <Leave> [list $txt configure -cursor {}]
        
        grid $txt $sb -sticky news
        grid rowconfigure $f 0 -weight 1
        grid columnconfigure $f 0 -weight 1

        array unset channel *
        array set channel [$token channel]
        set title $url
        if {[string length channel(title)]} { set title $channel(title) }
        foreach item [$token data] {
            array set a {title "(no title)" description "(no description)"}
            array set a $item
            set tag URL-[incr RssUrlId]
            $txt insert end $a(title) [list $url URL ITEM $tag] \
                "\n$a(description)\n\n" [list $url ITEM]
            $txt tag bind $tag <Button-1> \
                [list ::tkchat::gotoURL [string map {% %%} $a(link)]]
            tooltip $txt -tag $tag $a(link)
        }
        
        $txt configure -state disabled
        $nb add $f -text $title

        incr page
    }
    ttk::frame $dlg.bf
    ttk::button $dlg.bf.ok -default active -text "OK" -width -12 \
        -command [list set [namespace which -variable $dlg] ok]
    pack $dlg.bf.ok

    bind $dlg <Return> [list $dlg.bf.ok invoke]
    bind $dlg <Escape> [list $dlg.bf.ok invoke]
    
    grid $nb     -sticky news -padx {2 1} -pady 2
    grid $dlg.bf -sticky e
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    wm deiconify $dlg
    focus $dlg.bf.ok
    catch {tk::PlaceWindow $dlg widget .}
    tkwait variable [namespace which -variable $dlg]
    destroy $dlg
}

proc ::tkchat::rss::ShowRssInfo2 {} {
    global Options
    variable updated
    variable Rss
    variable RssData {}

    if {[winfo exists .status.rss]} {
        .status.rss configure -image ::tkchat::img::feedLo
    }
    set t .rssinfo

    if {[winfo exists $t]} {
        wm deiconify $t
        focus -force $t.bf.ok
        return
    }

    ::tkchat::Dialog $t
    wm withdraw $t
    wm title $t "RSS Feeds"
    wm transient $t .

    set colspec {
        #0          {"Title"        0 w 300}
        date        {"Date"         0 e 100}
    }
    set tv [ttk::treeview $t.tv]
    ttk::scrollbar $t.sy1 -command [list $tv yview]
    ttk::scrollbar $t.sx1 -orient horizontal -command [list $tv xview]
    $tv configure \
        -yscrollcommand [list $t.sy1 set] \
        -xscrollcommand [list $t.sx1 set] \
        -columns [dict keys [dict remove $colspec #0]] \
        -selectmode browse
    dict for {col spec} $colspec {
        lassign $spec ht st an wi
        $tv heading $col -text [mc $ht]
        $tv column  $col -anchor $an -stretch $st -width $wi
    }
    $tv tag configure Channel -font TkHeadingFont -foreground red4
    set txt [text $t.txt]
    ttk::scrollbar $t.sy2 -command [list $txt yview]
    ttk::scrollbar $t.sx2 -orient horizontal -command [list $txt xview]
    $txt configure \
        -yscrollcommand [list $t.sy2 set] \
        -xscrollcommand [list $t.sx2 set] \
        -width 60  \
        -height 20 \
        -wrap word
    $txt tag configure TITLE -font TkHeadingFont
    $txt tag configure LINK -foreground blue3 -underline 1
    $txt tag configure TEXT -lmargin1 5m -lmargin2 5m
    $txt tag configure LAST -wrap none
    $txt tag bind LINK <Enter> {%W configure -cursor hand2}
    $txt tag bind LINK <Leave> {%W configure -cursor {}}

    set f [ttk::frame $t.bf]
    ttk::button $f.ok \
        -default active \
        -text "Ok"\
        -command [list destroy $t]
    ttk::button $f.reload \
        -text "\u27f3 Reload" \
        -command [list after 10 [namespace which CheckRSSFeeds]]
    pack $f.ok $f.reload -padx 3 -pady 3 -side right

    grid $tv    $t.sy1 $txt   $t.sy2 -sticky news
    grid $t.sx1 x      $t.sx2 x      -sticky news
    grid $f - - - -sticky e 
    grid columnconfigure $t 2 -weight 1
    grid rowconfigure    $t 0 -weight 1

    # the trace keeps the tree updated while the window is open
    array set updated {}
    trace add variable [namespace which -variable updated] write \
        [list [namespace which TraceProc] $tv $txt]
    bind $f <Destroy> [list apply [list {t}  {
        # clean up after ourselves
        variable updated
        variable RssData
        unset updated
        unset RssData
        tooltip clear $t*
    } [namespace current]] $t]
    # load feeds
    foreach url [lreverse [dict keys $Rss]] {
        if {$Options(RSS,watch,$url)} {
            FillRssData $tv $txt $url
        }
    }

    bind $tv <<TreeviewSelect>> \
        [list [namespace which OnTreeviewSelect] $tv $txt]

    bind $t <Return> [list $f.ok invoke]
    bind $t <Escape> [list $f.ok invoke]
    wm deiconify $t
    focus $f.ok
    catch {tk::PlaceWindow $t widget .}

    return
}

proc ::tkchat::rss::TraceProc {tv txt var url op} {
    FillRssData $tv $txt $url
}

proc ::tkchat::rss::FillRssData {tv txt url} {
    variable Rss
    variable RssData

    set parser [dict get $Rss $url]
    if {[$parser status] ne "ok"} {
        return
    }

    if {[$tv exists $url]} {
        foreach child [$tv children $url] {
            dict unset RssData $child
            $tv delete $child
            tooltip clear $tv*$child*
        }
        dict unset RssData $url
        $tv delete $url
        tooltip clear $tv*$url*
    }

    # channel info
    set channel [$parser channel]
    set date [clock format [dict getwithdefault $channel mtime 0]]
    set title [dict getwithdefault $channel title $url]
    $tv insert {} 0 -id $url \
        -text $title \
        -values [list [lrange $date 0 2]] \
        -tags Channel
    set text {}
    lappend text "Channel title: " TITLE $title\n\n {}
    lappend text "Date: "          TITLE $date\n\n  {}
    dict set RssData $url text $text
    tooltip $tv -item $url $url

    # iterate over channel items
    foreach item [$parser data] {
        set date [clock format [dict getwithdefault $item mtime 0]]
        set link [dict getwithdefault $item link "(no link)"]
        set desc [dict getwithdefault $item description "(no description)"]
        set title [dict getwithdefault $item title "(no title)"]
        # feeds could contain either "author" or "creator" tag.
        if {[dict exists $item creator]} {
            set atag "Creator"
            set auth [dict get $item creator]
        } else {
            set atag "Author"
            set auth [dict getwithdefault $item author "(no author)"]
        }
        set id [$tv insert $url end \
            -text $title \
            -values [list [lrange $date 0 2]]]

        # perform a very simple substitution of possible markup
        foreach var {title desc} {
            upvar 0 $var v
            set v [string trim [string map {
                    <p>  "\n"
                    </p> ""
                    <br> "\n"
                } $v] \n]
        }
        set text {}
        lappend text "Title: " TITLE $title\n\n TEXT
        lappend text "Date: "  TITLE $date\n\n  TEXT
        lappend text "$atag: " TITLE $auth\n\n  TEXT
        # some feeds provides the same text for both title and description
        if {$desc ne $title} {
            lappend text "Description: " TITLE $desc\n\n TEXT
        }
        dict set RssData $id link $link
        dict set RssData $id text $text
        tooltip $tv -item $id $link
    }

}

proc ::tkchat::rss::OnTreeviewSelect {tv txt} {
    variable RssData

    set id   [$tv selection]
    set text [dict get $RssData $id text]
    if {[string match "http*" $id]} {
        # this is a channel entry
        set link $id
        set last [list "URL: "]
    } else {
        set link [dict get $RssData $id link]
        set last [list "Link: "]
    }
    lappend last {LAST TITLE} "$link\n\n" {LAST LINK}
    $txt configure -state normal
    $txt delete 1.0 end
    $txt insert end {*}$text
    $txt insert end {*}$last
    $txt tag bind LINK <Button-1> \
        [list ::tkchat::gotoURL [string map {% %%} $link]]
    tooltip $txt -tag LINK $link
    $txt configure -state disabled
}

proc ::tkchat::rss::CheckRSSFeeds {} {
    global Options
    variable RSStimer
    variable RSStip {}

    if {![winfo exists .status.rss]} {
        return
    }

    log::log info "checking rss feeds"
    catch {after cancel $RSStimer}
    .status.rss configure -image ::tkchat::img::feedLo
    set active 0
    foreach feed [array names Options RSS,watch,*] {
        if {$Options($feed)} {
            set active 1
            break
        }
    }

    if {$active} {
        ::tkchat::StatusbarAddWidget .status .status.rss 1
        foreach feed [array names Options RSS,watch,*] {
            if {$Options($feed)} {
                set url [lindex [split $feed ,] 2]
                CheckRSS $url
            }
        }
        set RSStimer [after [expr {int(rand() * 10 + 15) * 60 * 1000}] [info level 0]]
    }
    return
}

proc ::tkchat::rss::CheckRSS {url} {
    try {
        set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8"]
        ::http::geturl $url -headers $hdrs -timeout 1000000 \
            -command [list ::tkchat::fetchurldone [namespace origin CheckRSS_Done]]
    } on error {} {
        ::tkchat::addStatus 0 "Unable to obtain RSS feed from $url" end ERROR
    }
}

proc ::tkchat::rss::CheckRSS_Done {tok} {
    if {[catch {CheckRSS_Inner $tok}]} {
        puts stderr $::errorInfo
    }
    return
}

proc ::tkchat::rss::CheckRSS_Inner {tok} {
    global Options
    variable Rss
    variable RSStip
    variable updated

    set count 0
    set feed [dict get [http::responseInfo $tok] url]
    if {![dict exists $Rss $feed]} {
        dict set Rss $feed [set parser [::rss::Rssparser new]]
    } else {
        set parser [dict get $Rss $feed]
    }

    # As rss is transmitted as application/* the http package doesn't handle
    # the encoding for us and we must do it here.
    # Default for xml files is utf-8.
    set encodings [string tolower [encoding names]]
    set encoding [string tolower [dict get [http::responseInfo $tok] charset]]
    if {$encoding in $encodings} {
        set enc $encoding
    } else {
        set enc utf-8
    }

    # this doesn't seem to work with some feeds
    # set data [encoding convertfrom $enc [http::data $tok]]
    set data [http::data $tok]
    $parser parse $data
    if {[$parser status] eq "ok"} {
        try {
            set last 0
            if {[info exists Options(RSS,last,$feed)]} {
                set last $Options(RSS,last,$feed)
            }
            set item [lindex [$parser data] 0]
            set title [dict get [$parser channel] title]
            set Options(RSS,last,$feed) [dict getwithdefault $item mtime 0]
            set Options(RSS,title,$feed) $title
            if {$Options(RSS,last,$feed) > $last} {
                foreach rss [$parser data] {
                    set t [dict get $rss mtime]
                    if {$t <= $last} { break }
                    incr count
                }
            }
            append RSStip "$count new items from $title\n"
            if {$count > 0} {
                ::tkchat::addStatus 0 "$count new items from $title"
                .status.rss configure -image ::tkchat::img::feedHi
                # when the window exists, there's a trace on this variable
                # which updates the tree
                set updated($feed) 1
            }
        } on error {err} {
            ::tkchat::addStatus 0 "RSS Error [list $err $feed]" end ERROR
        }
    } else {
        ::tkchat::addStatus 0 "Failed to parse RSS data from $feed:\
            [$parser error]" end ERROR
    }
    return
}

proc ::tkchat::rss::OptionsHook {parent} {
    global Options
    variable EditOptions
    array set EditOptions [array get Options RSS,watch,*]

    set page [ttk::frame $parent.rssOptions -borderwidth 0]
    set n 0
    foreach feed [array names EditOptions RSS,watch,*] {
        set url [lindex [split $feed ,] 2]
        set text [dict get [uri::split $url] host]
        if {[info exists Options(RSS,title,$url)]} {
            if {[string length $Options(RSS,title,$url)] > 0} {
                set text $Options(RSS,title,$url)
            }
        }
        ttk::checkbutton [set w $page.wf[incr n]] \
            -text $text -underline 0 \
            -variable [namespace current]::EditOptions($feed)
        grid $w -sticky new -padx 2 -pady 2
    }
    grid columnconfigure $page 0 -weight 1
    grid rowconfigure    $page $n -weight 1

    bind $page <<TkchatOptionsAccept>> [namespace code {
        variable EditOptions; global Options
        set feed_refresh 0
        foreach feed [array names EditOptions RSS,watch,*] {
            if {$Options($feed) != $EditOptions($feed)} {
                set feed_refresh 1
                set Options($feed) $EditOptions($feed)
            }
        }
        if {$feed_refresh} { after idle [namespace origin CheckRSSFeeds] }
        unset EditOptions
    }]
    bind $page <<TkchatOptionsCancel>> [namespace code {
        variable EditOptions; unset EditOptions
    }]
    return [list "RSS Feeds" $page]
}

# -------------------------------------------------------------------------
::tkchat::Hook add login ::tkchat::rss::Init
::tkchat::Hook add options ::tkchat::rss::OptionsHook
package provide tkchat::rss $::tkchat::rss::version
# -------------------------------------------------------------------------
