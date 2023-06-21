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
    global Options
    global tkchat_dir
    variable RSStip {}
    variable Rss    {}

    if {![catch {package require rssrdr_oo}]} {
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

                set var [namespace which -variable RSStip]
                trace add variable $var write \
                    [list after 0 [namespace origin RssUpdateTip]]
                
                after idle [namespace origin CheckRSSFeeds]
            }
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
    if {![info exists RssUrlId]} {
        set RssUrlId 0
    }
    if {[winfo exists .status.rss]} {
        .status.rss configure -image ::tkchat::img::feedLo
    }
    set dlg .rssinfo
    variable $dlg

    if {[winfo exists $dlg]} {
        wm deiconify $dlg
        focus -force $dlg.ok
        return
    }

    set dlg [::tkchat::Dialog $dlg]
    wm withdraw $dlg
    wm title $dlg "RSS Feeds"
    wm transient $dlg .

    set nb [ttk::notebook $dlg.nb]

    tooltip::clear $dlg.nb*

    set page 0
    dict for {url parser} $Rss {
        if {[$parser status] ne "ok"} {
            continue
        }
        set f [ttk::frame $nb.page[incr page]]

        set tv [ttk::treeview $f.tv]
        set sb [ttk::scrollbar $f.vs -command [list $tv yview]]
        $tv configure \
            -yscrollcommand [list $sb set] \
            -columns [list date title description link] \
            -show headings \
            -selectmode none
        $tv heading date        -text [mc "Date"]
        $tv heading title       -text [mc "Title"]
        $tv heading description -text [mc "Description"]
        $tv heading link        -text [mc "Link"]

        pack $sb -side right -fill y
        pack $tv -side right -fill both -expand 1

        set channel [$parser channel]
        set title $url
        if {[dict exists $channel title]} {
            set title [dict get $channel title]
        }
        foreach item [$parser data] {
            set da [lrange [clock format [dict get $item mtime]] 0 3]
            set li [dict get $item link]
            set de [dict get $item description]
            set ti [expr {
                [dict exists $item title] ?
                [dict get $item title] :
                "(no title)"}]
            set tag  URL-[incr RssUrlId]
            set tags [list $url URL ITEM $tag]
            $tv insert {} end -values [list $da $ti $de $li] -tags $tags
            $tv tag bind $tag <Button-1> [list ::tkchat::gotoURL $li]
            tooltip $tv -item [$tv tag has $tag] $li
        }

        $nb add $f -text $title
    }

    ttk::button $dlg.ok -default active -text "OK" -width -12 \
        -command [list set [namespace which -variable $dlg] ok]

    bind $dlg <Return> [list $dlg.ok invoke]
    bind $dlg <Escape> [list $dlg.ok invoke]

    grid $nb     -sticky news -padx {2 1} -pady 2
    grid $dlg.ok -sticky e
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    wm deiconify $dlg
    focus $dlg.ok
    catch {tk::PlaceWindow $dlg widget .}
    tkwait variable [namespace which -variable $dlg]
    destroy $dlg
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
        if {$Options($feed)} { set active 1 ; break }
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
}

proc ::tkchat::rss::CheckRSS {url} {
    if {[package provide rssrdr_oo] ne {}} {
        if {[catch {
            set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8"]
            ::http::geturl $url -headers $hdrs -timeout 1000000 \
                -command [list ::tkchat::fetchurldone [namespace origin CheckRSS_Done]]
        } msg]} then {
            ::tkchat::addStatus 0 "Unable to obtain RSS feed from $url" end ERROR
        }
    }
}

proc ::tkchat::rss::CheckRSS_Done {tok} {
    if {[catch {[namespace origin CheckRSS_Inner] $tok} err]} {
        puts stderr $::errorInfo
    }
    return
}

proc ::tkchat::rss::CheckRSS_Inner {tok} {
    global Options
    variable Rss
    variable RSStip
    set count 0
    set feed [dict get [http::responseInfo $tok] url]
    if {![dict exists $Rss $feed]} {
        dict set Rss $feed [set parser [::rss::Rss new]]
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

    set data [encoding convertfrom $enc [http::data $tok]]
    $parser parse $data
    if {[$parser status] eq "ok"} {
        if {[catch {
            set last 0
            if {[info exists Options(RSS,last,$feed)]} {
                set last $Options(RSS,last,$feed)
            }
            set item [lindex [$parser data] 0]
            set title [dict get [$parser channel] title]
            set Options(RSS,last,$feed) [dict get $item mtime]
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
            }
        } err]} then {
            ::tkchat::addStatus 0 "RSS Error $err" end ERROR
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
        array set U [uri::split $url]
        set text $U(host)
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
