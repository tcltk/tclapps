# -------------------------------------------------------------------------
# This file is part of the tkchat application
#
# Copyright (c) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# $Id: tkchat_rss.tcl,v 1.7 2007/09/24 22:24:15 patthoyts Exp $
# -------------------------------------------------------------------------

if {[catch {package require rssrdr}]} { return }

proc ::tkchat::RSSInit {} {
    global Options
    global tkchat_dir
    variable RSStip {}
    if {![catch {package require rssrdr}]} {
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
                if {[llength [package provide tooltip]] > 0} {
                    set var [namespace which -variable RSStip]
                    trace add variable $var write \
                        [list after 0 [namespace origin RssUpdateTip]]
                }
                
                after idle [namespace origin CheckRSSFeeds]
            }
        }
    }
}

proc ::tkchat::RssUpdateTip {varname op} {
    variable RSStip
    tooltip::tooltip .status.rss [string trim $RSStip \n]
}

proc ::tkchat::ShowRssInfo {} {
    variable NS
    variable Rss
    variable RssUrlId ; if {![info exists RssUrlId]} {set RssUrlId 0}
    .status.rss configure -image ::tkchat::img::feedLo
    set dlg .rssinfo
    variable $dlg

    if {[winfo exists $dlg]} {
        wm deiconify $dlg
        focus -force $dlg.ok
        return
    }

    set dlg [Dialog $dlg]
    wm withdraw $dlg
    wm title $dlg "RSS Feeds"
    wm transient $dlg .

    set use_notebook [llength [info commands ${NS}::notebook]]
    if {$use_notebook} {
        set nb [${NS}::notebook $dlg.nb]
    } else {
        set nb [${NS}::frame $dlg.nb]
    }
    
    set page 0
    foreach {url token} [array get Rss] {
        if {[rss::status $token] ne "ok"} { continue }
        set f [${NS}::frame $nb.page$page]
        set txt [text $f.txt -borderwidth 0 -font FNT]
        set sb [${NS}::scrollbar $f.vs -command [list $txt yview]]
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

        array set channel [rss::channel $token]
        set title $url
        if {[info exists channel(title)]} { set title $channel(title) }
        foreach item [rss::data $token] {
            array set a $item
            if {![info exists a(title)] || [string length $a(title)] < 1} {
                set a(title) "(no title)"
            }
            set tag URL-[incr RssUrlId]
            $txt insert end $a(title) [list $url URL ITEM $tag] \
                "\n$a(description)\n\n" [list $url ITEM]
            $txt tag bind $tag <Button-1> [list ::tkchat::gotoURL $a(link)]
            if {[llength [package provide tooltip]] > 0} {
                tooltip::tooltip $txt -tag $tag $a(link)
            }
        }
        
        $txt configure -state disabled
        if {$use_notebook} {
            $nb add $f -text $title
        } else {
            ${NS}::button $nb.b$page -text $title\
                -command [list raise $f]
            grid $nb.b$page -row 0 -column $page -sticky w
            grid $f  -row 1 -column 0 -sticky news -columnspan 100
            grid columnconfigure $nb 0 -weight 1
            grid rowconfigure $nb 1 -weight 1
        }

        incr page
    }
    
    ${NS}::button $dlg.ok -default active -text "OK" \
        -command [list set [namespace which -variable $dlg] ok]

    bind $dlg <Return> [list $dlg.ok invoke]
    bind $dlg <Escape> [list $dlg.ok invoke]
    
    grid $nb     -sticky news -padx {2 1} -pady 2
    grid $dlg.ok -sticky e
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    if {!$use_notebook} {catch {$nb.b0 invoke}}
    wm deiconify $dlg
    focus $dlg.ok
    catch {tk::PlaceWindow $dlg widget .}
    tkwait variable [namespace which -variable $dlg]
    destroy $dlg
}

proc ::tkchat::CheckRSSFeeds {} {
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
        StatusbarAddWidget .status .status.rss 1
        foreach feed [array names Options RSS,watch,*] {
            if {$Options($feed)} {
                set url [lindex [split $feed ,] 2]
                CheckRSS $url
            }
        }
        set RSStimer [after [expr {int(rand() * 10 + 15) * 60 * 1000}] [info level 0]]
    }
}

proc ::tkchat::CheckRSS {url} {
    if {[package provide rssrdr] ne {}} {
        if {[catch {
            set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8"]
            ::http::geturl $url -headers $hdrs -timeout 10000 \
                -command [list ::tkchat::fetchurldone ::tkchat::CheckRSS_Done]
        } msg]} then {
            addStatus 0 "Unable to obtain RSS feed from $url" end ERROR
        }
    }
}

proc ::tkchat::CheckRSS_Done {tok} {
    if {[catch {::tkchat::CheckRSS_Inner $tok} err]} {
        puts stderr $::errorInfo
    }
    return
}

proc ::tkchat::CheckRSS_Inner {tok} {
    global Options
    variable Rss
    variable RSStip
    set count 0
    set feed [set [set tok](url)]
    if {![info exists Rss($feed)]} { set Rss($feed) [::rss::create] }

    # As rss is transmitted as application/* the http package doesn't handle
    # the encoding for us and we must do it here. Default for xml files is utf-8
    set encodings [string tolower [encoding names]]
    set encoding [string tolower [set [set tok](charset)]]
    set idx [lsearch -exact $encodings $encoding]
    set enc utf-8
    if {$idx >= 0} { set enc [lindex $encodings $idx] }

    set data [encoding convertfrom $enc [http::data $tok]]
    ::rss::parse $Rss($feed) $data
    if {[::rss::status $Rss($feed)] eq "ok"} {
        if {[catch {
            set last 0
            if {[info exists Options(RSS,last,$feed)]} {
                set last $Options(RSS,last,$feed) 
            }
            array set item [lindex [::rss::data $Rss($feed)] 0]
            array set hdrs [rss::channel $Rss($feed)]
            set Options(RSS,last,$feed) $item(mtime)
            set Options(RSS,title,$feed) $hdrs(title)
            if {$Options(RSS,last,$feed) > $last} {
                foreach rss [::rss::data $Rss($feed)] {
                    unset -nocomplain item
                    array set item $rss
                    set t $item(mtime)
                    if {$t <= $last} { break }
                    incr count
                }
            }
            append RSStip "$count new items from $hdrs(title)\n" 
            if {$count > 0} {
                addStatus 0 "$count new items from $hdrs(title)"
                .status.rss configure -image ::tkchat::img::feedHi
            }
        } err]} then {
            addStatus 0 "RSS Error $err" end ERROR
        }
    } else {
        addStatus 0 "Failed to parse RSS data from $feed:\
            [rss::error $Rss($feed)]" end ERROR
    }

    return
}
