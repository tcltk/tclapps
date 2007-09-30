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
            "Trance"       "http://scfire-ntc-aa04.stream.aol.com:80/stream/1003"
            "Top 40 Hits"  "http://scfire-nyk-aa01.stream.aol.com:80/stream/1014"
            "Classical"    "http://scfire-dll-aa02.stream.aol.com:80/stream/1006"
        }
    }
    variable gain_changing 0
}

proc ::tkchat::mms::Play {url} {
    if {[catch {http::geturl $url \
                    -handler [namespace origin Stream] \
                    -command [namespace origin StreamFinished]
    } err]} then {
        ::tkchat::addStatus 0 "error fetching \"$url\": $err"
    }
}

proc ::tkchat::mms::Stream {sock tok} {
    if {[::http::ncode $tok] >= 300} {
        ::tkchat::addStatus 0 "Failed to open stream: [::http::code $tok]"
        ::http::Eof $tok
        return 0
    }
    if {[catch {
        fileevent $sock readable {}
        Init
        Status Buffering
        ::snack::sound snd -channel $sock -buffersize 163840
        Wait 3000
        Status Playing
        after idle [list snd play -blocking 0 \
                        -command [list [namespace origin CloseStream] $sock $tok]]
    } err]} { puts stderr "Stream: $err" }
    return 0
}

# called when the http request gets closed.
proc ::tkchat::mms::StreamFinished {tok} {
    if {[catch {::http::cleanup $tok} err]} { puts stderr "StreamFinished: $err" }
}

# Called when snack finds the end of the sound stream
# We must call Eof on the token because we took over the fileevents
proc ::tkchat::mms::CloseStream {sock tok} {
    Status Stopped
    if {[catch {::http::Eof $tok} err]} { puts stderr "CloseStream: $err" }
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
        .status.mms itemconfigure pause -image ::tkchat::mms::imgPlay \
            -activeimage ::tkchat::mms::actPlay
    } else {
        .status.mms itemconfigure pause -image ::tkchat::mms::imgPause \
            -activeimage ::tkchat::mms::actPause
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
    image create bitmap ::tkchat::mms::actPause -foreground yellow \
        -data [::tkchat::mms::imgPause cget -data]
    image create bitmap ::tkchat::mms::actPlay -foreground yellow \
        -data [::tkchat::mms::imgPlay cget -data]
    if {[winfo exists .status] && ![winfo exists .status.mms]} {
        canvas .status.mms -width 96 -height 18 -background black \
            -borderwidth 0 -highlightthickness 0
        .status.mms create image 80 4 -tags pause -anchor nw \
            -image ::tkchat::mms::imgPause -activeimage ::tkchat::mms::actPause
        .status.mms create rectangle 88 3 95 10 -tags stop -fill green -activefill yellow
        .status.mms bind pause <Button-1> [list [namespace origin Pause]]
        .status.mms bind stop <Button-1> [list [namespace origin Stop]]
        .status.mms create text 2 2 -tags label -fill green -anchor nw -font MMS
        # These are canvases, could just use variable height rects myself...
        snack::levelMeter .status.mms.left -width 8 -length 16 -orient vertical
        snack::levelMeter .status.mms.right -width 8 -length 16 -orient vertical
        .status.mms create window 62 2 -tags left -anchor nw -window .status.mms.left
        .status.mms create window 70 2 -tags right -anchor nw -window .status.mms.right
        # A small gain slider...
        .status.mms create line 56 2 56 18 -tags vline -fill green
        .status.mms create rectangle 53 14 59 18 -tags gain -fill green -activefill yellow
        .status.mms bind gain <ButtonPress-1> [list [namespace origin GainStart] %x %y]
        .status.mms bind gain <Motion> [list [namespace origin GainMotion] %x %y]
        .status.mms bind gain <ButtonRelease-1> [list [namespace origin GainEnd] %x %y]
        GainInit [snack::audio play_gain]
        ::tkchat::StatusbarAddWidget .status .status.mms 1
    }
}

proc ::tkchat::mms::GainStart {x y} {
    variable gain_changing 1
}
proc ::tkchat::mms::GainEnd {x y} {
    variable gain_changing 0
}
proc ::tkchat::mms::GainMotion {x y} {
    variable gain_changing
    if {$gain_changing} {
        foreach {x1 y1 x2 y2} [.status.mms coords gain] break
        set cy [expr {$y1 + (($y2 - $y1)/2)}]
        set dy [expr {$cy - $y}]
        set y1 [expr {$y1 - $dy}]
        if {$y1 > 14} {set y1 14} ; if {$y1 < 0} {set y1 0}
        set y2 [expr {$y2 - $dy}]
        if {$y2 > 18} {set y2 18} ; if {$y2 < 4} {set y2 4}
        .status.mms coords gain $x1 $y1 $x2 $y2
        set gain [expr {int((double(16 - $y) / 16) * 100)}]
        snack::audio play_gain $gain
    }
}
proc ::tkchat::mms::GainInit {level} {
    foreach {x1 y1 x2 y2} [.status.mms coords gain] break
    set center [expr {16 - int(16 * ($level / 100.0))}]
    set dy [expr {abs($y2 - $y1)/2}]
    .status.mms coords gain $x1 [expr {$center - $dy}] $x2 [expr {$center + $dy}]
}

proc ::tkchat::mms::ChooseStream {} {
    variable ::tkchat::NS
    set dlg [::tkchat::Dialog .choosestream]
    variable $dlg {}
    wm withdraw $dlg
    wm title $dlg "Select stream"
    set f [${NS}::frame $dlg.f -padding 2]

    ${NS}::label $f.info -text "Enter a URL to open.\
         The title is optional and will be displayed in the\
         \nmenu for this stream." -anchor nw
    ${NS}::label $f.tl -anchor nw -text "Title: "
    ${NS}::entry $f.te -width 24
    ${NS}::label $f.l -anchor nw -text "Stream URL: "
    ${NS}::entry $f.e -width 24
    ${NS}::button $f.ok -text OK -command [list set [namespace which -variable $dlg] ok]
    ${NS}::button $f.cn -text Cancel -command [list set [namespace which -variable $dlg] cancel]
    grid $f.info -   - -sticky new -pady 1
    grid $f.tl $f.te - -sticky new -pady 1
    grid $f.l $f.e - -sticky new -pady 1
    grid x $f.ok $f.cn -sticky se -pady 1
    grid rowconfigure $f 3 -weight 1
    grid columnconfigure $f 1 -weight 1

    grid $f -sticky news
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    bind $dlg <Return> [list $f.ok invoke]
    bind $dlg <Escape> [list $f.cn invoke]
    
    catch {::tk::PlaceWindow $dlg widget [winfo toplevel $dlg]}
    wm deiconify $dlg
    tkwait visibility $dlg
    focus $f.te
    grab $dlg
    tkwait variable [namespace which -variable $dlg]
    grab release $dlg
    if {[set $dlg] eq "ok"} {
        set title [$f.te get]
        set url [$f.e get]
        variable streams
        lappend streams $title $url
        after idle [list [namespace origin Play] $url]
    }
    unset -nocomplain $dlg 
    destroy $dlg
}

proc ::tkchat::mms::ChooseFile {} {
    variable updateid
    set types {{{Sound files} {.mp3 .ogg .wav}} {{All Files} {*.*}}}
    set file [tk_getOpenFile -title "Select audio file" \
                 -filetypes $types -defaultextension .mp3]
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
    # If we cannot get the sample (ie: streaming channel) give up
    if {[catch {snd sample [snd current_position]} sample]} { return }
    foreach {l r} $sample break
    .status.mms.left configure -level $l
    .status.mms.right configure -level $r
    if {[winfo exists .status.mms] && [::snack::audio currentSound] ne ""} {
        variable updateid [after $interval [info level 0]]
    }
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

proc ::tkchat::mms::Save {} {
    variable streams
    return [list namespace eval [namespace current] [list variable streams $streams]]
}

::tkchat::Hook add save ::tkchat::mms::Save
::tkchat::Hook add init ::tkchat::mms::InitHook
