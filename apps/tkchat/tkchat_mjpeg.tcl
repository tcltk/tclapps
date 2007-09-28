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

namespace eval ::tkchat::mjpeg {
    variable subsample
    if {![info exists subsample]} { set subsample 2 }
    variable retrycount
    if {![info exists retrycount]} { set retrycount 0 }
}

proc ::tkchat::mjpeg::Read {dlg fd tok} {
    variable state
    variable toread
    variable frame
    variable expected
    variable subsample
    variable hdrs
    variable token $tok    

    variable boundary
    if {![info exists boundary]} {
        after idle [list [namespace origin Watchdog] $dlg $fd $tok]
        set boundary "--myboundary"
        foreach {key val} [set [set tok](meta)] {
            if {[string match -nocase content-type $key]} {
                set r [regexp {boundary=([^\s;]+)} $val -> boundary]
                break
            }
        }
    }
    
    switch -- $state {
	boundary {
            Status $dlg "Waiting for image"
            Progress $dlg $tok 100 0
            fconfigure $fd -buffering line -translation crlf
	    gets $fd line
	    if {$line eq $boundary} {set state mime; set hdrs {}}
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
	    } else {
                if {[regexp {^([^:]+):\s*(.+)$} $line -> key value]} {
                    lappend hdrs $key $value
                }
            }
            return [string length $line]
	}
	data {
	    set data [read $fd $toread]
	    incr toread -[string length $data]
	    append frame $data
            Progress $dlg $tok $expected [string length $frame]
	    if {$toread == 0} {
                Status $dlg "Ready."
                if {[catch {
                    foo configure -data $frame
                    foo2 copy foo -shrink -subsample $subsample $subsample
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

proc ::tkchat::mjpeg::Watchdog {dlg sock tok} {
    variable retrycount
    if {[catch {fconfigure $sock}]} {
        Status $dlg "MJPEG stream has died, retrying ($retrycount)"
        if {[catch {::http::Eof $tok} err]} {Status $dlg $err}
        if {[catch {Cleanup $dlg 1} err]} { puts stderr $err }
    } else {
        set retrycount 0
        variable watchdog [after 1000 [info level 0]]
    }
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

proc ::tkchat::mjpeg::Cleanup {dlg {retry 0}} {
    variable token
    variable watchdog
    variable retrycount
    upvar #0 $token state
    set url ""
    if {[info exists state]} {
        catch {set url $state(url)}
        if {[catch {::http::Eof $token} err]} { puts stderr $err }
        if {[catch {::http::cleanup $token} err]} { puts stderr $err }
    }
    after cancel watchdog
    #unset -nocomplain token
    if {$retry && $retrycount < 10 && $url ne ""} {
        incr retrycount
        OpenStream $url $dlg
    } else {
        if {$retrycount > 2} {
            ::tkchat::addStatus 0 "Too many failed attempts\
               accessing MJPEG stream. Giving up." end ERROR
        } elseif {$url eq ""} {
            ::tkchat::addStatus 0 "Lost the video stream. Giving up." end ERROR
        }
        destroy $dlg
    }
}

proc ::tkchat::mjpeg::Scaling {dlg factor} {
    variable subsample
    $dlg.status.pane$subsample configure -relief flat
    set subsample $factor
    foo2 copy foo -shrink -subsample $subsample $subsample
    $dlg.image configure -image foo2
    $dlg.status.pane$factor configure -relief sunken
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

    ${NS}::label $dlg.image
    ${NS}::frame $dlg.status

    ${NS}::label $dlg.status.pane0 -anchor w
    ${NS}::label $dlg.status.pane1 -text 1
    ${NS}::label $dlg.status.pane2 -text \u00bd
    ${NS}::button $dlg.status.snap -text Snap \
        -command [list [namespace origin Snapshot] $dlg]
    bind $dlg.status.pane1 <Button-1> [list [namespace origin Scaling] $dlg 1]
    bind $dlg.status.pane2 <Button-1> [list [namespace origin Scaling] $dlg 2]
    set column 0
    grid $dlg.status.pane0 -row 0 -column $column -sticky news
    grid $dlg.status.pane1 -row 0 -column [incr column] -sticky e
    grid $dlg.status.pane2 -row 0 -column [incr column] -sticky e
    if {[llength [info commands ${NS}::progressbar]] > 0} {
        ${NS}::progressbar $dlg.status.progress
        grid $dlg.status.progress -row 0 -column [incr column] -sticky e
    }
    grid $dlg.status.snap  -row 0 -column [incr column] -sticky e
    grid columnconfigure $dlg.status 0 -weight 1
    grid rowconfigure $dlg.status 0 -weight 1

    if {[package provide tooltip] ne {}} {
        tooltip::tooltip $dlg.status.pane1 "Full size"
        tooltip::tooltip $dlg.status.pane2 "Half size"
    }

    image create photo foo -width 800 -height 600
    image create photo foo2
    variable subsample
    Scaling $dlg $subsample

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
        variable boundary ; unset -nocomplain boundary
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
            $m add command -label "Tcl conference (localhost)"\
                -command [list [namespace origin Open] http://localhost:3128/video.mjpg]
        }
    }
}

# -------------------------------------------------------------------------
#
# Snapshots are displayed in their own dialog window
#
proc ::tkchat::mjpeg::Snapshot {main} {
    variable ::tkchat::NS
    set img [image create photo]
    if {$img ne {}} {
        variable uid; if {![info exists uid]} {set uid 0}
        set dlg [toplevel $main.snap[incr uid] -class SnapshotDialog]
        wm withdraw $dlg
        wm geometry $dlg +0+0
        $img copy foo;                 # copy the full size image  
        #wm maxsize $dlg [$img cget -width] [$img cget -height]
        ${NS}::label $dlg.im -image $img
        ${NS}::button $dlg.bx -text "Close" \
            -command [list [namespace origin SnapClose] $dlg $img]
        ${NS}::button $dlg.bs -text "Save as" \
            -command [list [namespace origin SnapSaveAs] $img]
        bind $dlg <Escape> [list $dlg.bx invoke]
        bind $dlg <Return> [list $dlg.bs invoke]
        grid $dlg.im - -sticky news
        grid $dlg.bs $dlg.bx -sticky nse
        grid rowconfigure $dlg 0 -weight 1
        grid columnconfigure $dlg 0 -weight 1
        wm deiconify $dlg
        tkwait visibility $dlg.im
        focus $dlg.bs
    }
}

proc ::tkchat::mjpeg::SnapClose {dlg img} {
    if {[winfo exists $dlg]} {destroy $dlg}
    catch {image delete $img}
}

proc ::tkchat::mjpeg::SnapSaveAs {img} {
    variable lastdir ; if {![info exists lastdir]} {set lastdir {}}
    set file [tk_getSaveFile \
                  -initialfile [clock format [clock seconds] \
                                    -format "Tcl2007-%a-%H%M%S.jpg"] \
                  -initialdir $lastdir \
                  -defaultextension .jpg -filetypes {
                      {"JPEG files"   .jpg {}}
                      {"GIF files"    .gif {}}
                      {"PNG files"    .png {}}
                      {"All image files" {.jpg .gif .png} {}}
                      {"All fles"    {.*} {}}}]
    if {$file != {}} {
        set lastdir [file dirname $file]
        switch -exact -- [set fmt [string tolower [file extension $file]]] {
            .gif { set fmt gif }
            .jpg { set fmt jpeg }
            .png { set fmt png }
            default {
                tk_messageBox -icon error -title "Bad image format" \
                    -message "Unrecognised image format \"$fmt\". The file\
                       type must be one of .jpg, .gif or .png"
                return
            }
        }
        $img write $file -format $fmt
    }
}

# -------------------------------------------------------------------------

::tkchat::Hook add init ::tkchat::mjpeg::InitHook
