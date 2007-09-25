# Tkchat multimedia stream reader
#
# This is a simple Ogg or MP3 stream player derived from original code by
# David Zolli (Kroc) and Reinhard Max (rmax) (http://wiki.tcl.tk/12619)
# Hacked extensively by Pat Thoyts (patthoyts)

if {[catch {
    package require snack 2.2
    package require snackogg
    package require http
}]} {
    return
}

namespace eval ::tkchat::mms {
    variable streams
    if {![info exists streams]} {
        set streams {
            "Tcl conference (EU server)" "http://eu.tclers.tk/conference.ogg"
            "Tcl conference (US server)" "http://us.tclers.tk/conference.ogg"
            "-"            {}
            "Trance"       "http://scfire-ntc-aa04.stream.aol.com:80/stream/1003"
            "Top 40 Hits"  "http://scfire-nyk-aa01.stream.aol.com:80/stream/1014"
            "Classical"    "http://scfire-dll-aa02.stream.aol.com:80/stream/1006"
        }
    }
}

proc ::tkchat::mms::Play {url} {
    if {[catch {http::geturl $url -handler [namespace origin Stream]} err]} {
        tkchat::addStatus 0 "Unable to open stream at \"$url\": $err" end ERROR
    }
}

proc ::tkchat::mms::Stream {sock tok} {
    fileevent $sock readable {}
    Init
    Status Buffering
    ::snack::sound snd -channel $sock -buffersize 163840
    Wait 3000
    Status Playing
    after idle [list snd play -blocking 0 \
                    -command [list [namespace origin CloseStream] $sock $tok]]
    return 0
}

proc ::tkchat::mms::CloseStream {sock tok} {
    puts stderr [info level 0]
    catch {close $sock}
    ::http::cleanup $tok
}

proc ::tkchat::mms::Wait {total {done 0}} {
    ::tkchat::Progress {} $total $done
    while {$done < $total} {
        variable wait 0
        after 100 [list set [namespace which -variable wait] 1]
        tkwait variable [namespace which -variable wait]
        ::tkchat::Progress {} $total [incr done 100]
    }
    return
}

proc ::tkchat::mms::Pause {} {
    snd pause
    if {[.status.mms itemcget pause -image] eq "::tkchat::mms::imgPause"} {
        .status.mms itemconfigure pause -image ::tkchat::mms::imgPlay
    } else {
        .status.mms itemconfigure pause -image ::tkchat::mms::imgPause
    }
}

proc ::tkchat::mms::Stop {} {
    if {[catch {snd stop} err]} {
        Status $err
        ::snack::audio stop
    } else {
        Status Stopped
    }
    after 1000 {destroy .status.mms}
}

proc ::tkchat::mms::Status {message} {
    if {[winfo exists .status.mms]} {
        .status.mms itemconfigure label -text $message
    } else {
        ::tkchat::addStatus 0 $message
    }
}

proc ::tkchat::mms::Init {} {
    if {[lsearch -exact [font names] MMS] == -1} {
        font create MMS -family {Small Fonts} -size 6 -weight normal
    }
    image create bitmap ::tkchat::mms::imgPause -foreground green -data {
        #define pause_width 7
        #define pause_height 6
        static unsigned char pause_bits[] = {
            0x77, 0x77, 0x77, 0x77, 0x77, 0x77};
    }
    image create bitmap ::tkchat::mms::imgPlay -foreground green -data {
        #define play_width 5
        #define play_height 7
        static unsigned char play_bits[] = {
            0x03, 0x07, 0x0f, 0x1f, 0x0f, 0x07, 0x03};
    }
    if {[winfo exists .status] && ![winfo exists .status.mms]} {
        canvas .status.mms -width 96 -height 18 -background black
        .status.mms create image 80 4 -tags pause -anchor nw -image ::tkchat::mms::imgPause
        .status.mms create rectangle 88 3 95 10 -tags stop -fill green
        .status.mms bind pause <Button-1> [list [namespace origin Pause]]
        .status.mms bind stop <Button-1> [list [namespace origin Stop]]
        .status.mms create text 2 2 -tags label -fill green -anchor nw -font MMS
        # These are canvases, could just use variable height rects myself ...
        snack::levelMeter .status.mms.left -width 8 -length 16 -orient vertical
        snack::levelMeter .status.mms.right -width 8 -length 16 -orient vertical
        .status.mms create window 62 2 -tags left -anchor nw -window .status.mms.left
        .status.mms create window 70 2 -tags right -anchor nw -window .status.mms.right
        ::tkchat::StatusbarAddWidget .status .status.mms 1
    }
}

proc ::tkchat::mms::ChooseStream {} {
    variable ::tkchat::NS
    set dlg [::tkchat::Dialog .choosestream]
    variable $dlg {}
    wm withdraw $dlg
    wm title $dlg "Select stream URL"
    set f [${NS}::frame $dlg.f -padding 2]

    ${NS}::label $f.tl -text "Title: "
    ${NS}::entry $f.te -width 24
    ${NS}::label $f.l -text "Stream URL: "
    ${NS}::entry $f.e -width 24
    ${NS}::button $f.ok -text OK -command [list set [namespace which -variable $dlg] ok]
    ${NS}::button $f.cn -text Cancel -command [list set [namespace which -variable $dlg] cancel]
    grid $f.tl $f.te - -sticky new
    grid $f.l $f.e - -sticky new
    grid x $f.ok $f.cn -sticky se
    grid rowconfigure $f 2 -weight 1
    grid columnconfigure $f 1 -weight 1

    grid $f -sticky news
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    bind $dlg <Return> [list $f.ok invoke]
    bind $dlg <Escape> [list $f.cn invoke]
    
    catch {::tk::PlaceWindow $dlg widget [winfo toplevel $dlg]}
    wm deiconify $dlg
    tkwait visibility $dlg
    focus $f.e
    grab $dlg
    tkwait variable [namespace which -variable $dlg]
    grab release $dlg
    if {[set $dlg] eq "ok"} {
        set title [$f.te get]
        set url [$f.e get]
        lappend streams $title $url
        after idle [list [namespace origin Play] $url]
    }
    unset -nocomplain $dlg 
    destroy $dlg
}

proc ::tkchat::mms::ChooseFile {} {
    variable updateid
    set file [snack::getOpenFile -title "Select audio file"]
    if {[string length $file] > 0 && [file exists $file]} {
        Init
        Status Playing
        ::snack::sound snd -file $file -guessproperties true
        after idle [list snd play -blocking 0 \
                        -command [list after cancel [namespace which -variable updateid]]]
        after idle [list [namespace origin Update] 100]
    }
}

proc ::tkchat::mms::Update {{interval 100}} {
    if {[catch {
        foreach {l r} [snd sample [snd current_position]] break
        .status.mms.left configure -level $l
        .status.mms.right configure -level $r
    } err]} { puts stderr $err }
    variable updateid [after $interval [info level 0]]
}

proc ::tkchat::mms::FillMenu {m} {
    variable streams
    $m delete 0 end
    $m add command -label "Open stream..." -underline 0 -command [namespace origin ChooseStream]
    $m add command -label "Play file..." -underline 0 -command [namespace origin ChooseFile]
    $m add separator
    foreach {name url} $streams {
        if {$name eq "-"} {
            $m add separator
        } else {
            $m add command -label $name -command [list [namespace origin Play] $url]
        }
    }
}

# Inject a menu item into the tkchat menu.
proc ::tkchat::mms::InitHook {} {
    if {[winfo exists .mbar.file]} {
        set str "Play audio"
        if {[catch {.mbar.file index $str}]} {
            if {![catch {set ndx [.mbar.file index "Exit"]}]} {
                .mbar.file insert [incr ndx -1] cascade -label $str \
                    -menu [menu .mbar.file.stream -tearoff 0 \
                               -postcommand [list ::tkchat::mms::FillMenu .mbar.file.stream]]
            }
        }
    }
}

::tkchat::Hook add init ::tkchat::mms::InitHook
