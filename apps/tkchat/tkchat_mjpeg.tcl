# Tkchat plugin to display an MJPEG stream in a Tk window.
#
# Copyright (c) 2007 Reinhard Max
# Copyright (c) 2007 Pat Thoyts
#
# Original code: http://wiki.tcl.tk/19957
#

if {[catch {
    package require Tk
    package require http
    package require img::jpeg
}]} {
    return
}

namespace eval ::tkchat::mjpeg {}

proc ::tkchat::mjpeg::Read {dlg fd tok} {
    variable state
    variable toread
    variable frame
    variable expected
    variable token $tok

    switch -- $state {
	boundary {
            Status $dlg "Waiting for image"
            Progress $dlg $tok 100 0
            fconfigure $fd -buffering line -translation crlf
	    gets $fd line
	    if {$line eq "--myboundary"} {set state mime}
            return [string length $line]
	}
	mime {
            Status $dlg "Reading image"
            fconfigure $fd -buffering line -translation crlf
	    gets $fd line
	    regexp {Content-Length: ([[:digit:]]+)} $line -> toread
	    if {$line eq ""} {
                variable expected $toread
		fconfigure $fd -translation binary
		set state data
	    }
            return [string length $line]
	}
	data {
	    set n [expr { $toread > 1000 ? 1000 : $toread }]
	    set data [read $fd $n]
	    incr toread -[string length $data]
	    append frame $data
            Progress $dlg $tok $expected [string length $frame]
	    if {$toread == 0} {
                Status $dlg "Ready."
                if {[catch {
                    foo configure -data $frame
                    foo2 copy foo -subsample 2 2
                    $dlg.image configure -image foo2
                } err]} { Status $dlg "error: $err" }
		set frame ""
		set state boundary
		fconfigure $fd -buffering line -translation crlf
	    }
            return [string length $data]
	}
    }
    return 0
}

proc ::tkchat::mjpeg::Status {dlg msg} {
    if {[winfo exists $dlg.status.pane0]} {
        $dlg.status.pane0 configure -text $msg
    }
}

proc ::tkchat::mjpeg::Progress {dlg tok total current} {
    if {[winfo exists $dlg.status.progress]} {
        if {$total == -1} {
            $dlg.status.progress configure -mode indeterminate
        } else {
            set value [expr {int(double($current)/double($total) * 100)}]
            #puts stderr "progress $dlg $tok $total $current => $value"
            $dlg.status.progress configure -mode determinate -value $value
        }
    }
}

proc ::tkchat::mjpeg::Cleanup {dlg} {
    variable token
    catch {close [set [set $token](socket)]}
    catch {::http::cleanup $token}
    unset -nocomplain token
}

proc ::tkchat::mjpeg::Open {url {title "Conference video stream"}} {
    variable ::tkchat::NS
    set dlg .videofeed
    if {[winfo exists $dlg]} {
        wm deiconify $dlg
        return
    }
    set dlg [toplevel $dlg -class Dialog]
    wm withdraw $dlg
    wm title $dlg $title
    wm protocol $dlg WM_DELETE_WINDOW [list [namespace origin Cleanup] $dlg]

    image create photo foo -width 800 -height 600
    image create photo foo2
    foo2 copy foo -subsample 2 2
    ${NS}::label $dlg.image -image foo2
    ${NS}::frame $dlg.status

    ${NS}::label $dlg.status.pane0 -anchor w
    grid $dlg.status.pane0 -row 0 -column 0 -sticky news
    if {[llength [info commands ${NS}::progressbar]] > 0} {
        ${NS}::progressbar $dlg.status.progress
        grid $dlg.status.progress -row 0 -column 1 -sticky w
    }
    grid columnconfigure $dlg.status 0 -weight 1
    grid rowconfigure $dlg.status 0 -weight 1

    grid $dlg.image -sticky news
    grid $dlg.status -sticky ew
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    bind $dlg <Alt-F4> [list destroy $dlg]
    wm deiconify $dlg

    OpenStream $url $dlg
    return
}

proc ::tkchat::mjpeg::OpenStream {url dlg} {
    if {[catch {
        variable toread 0
        variable state boundary
        variable frame ""
        variable token; if {[info exists token]} { catch {http::cleanup $token} }
        puts stderr "Open $url"
        Status $dlg "Opening $url"
        http::geturl $url \
            -handler [list [namespace origin Read] $dlg] \
            -command [list [namespace origin OpenStream] $url $dlg]
    } err]} {
        ::tkchat::addStatus 0 $err end ERROR
    }
}

proc ::tkchat::mjpeg::InitHook {} {
    set str "Video feeds"
    if {[catch {.mbar.file index $str}]} {
        if {![catch {set ndx [.mbar.file index "Exit"]}]} {
            .mbar.file insert [incr ndx -1] cascade -label $str \
                -menu [set m [menu .mbar.file.video -tearoff 0]]
            $m add command -label "Tcl conference (EU)"\
                -command [list [namespace origin Open] http://eu.tclers.tk/video.mjpg]
            $m add command -label "Tcl conference (US)"\
                -command [list [namespace origin Open] http://us.tclers.tk/video.mjpg]
        }
    }
}

::tkchat::Hook add init ::tkchat::mjpeg::InitHook
