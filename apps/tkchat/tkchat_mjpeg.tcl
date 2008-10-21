# Tkchat plugin to display an MJPEG stream in a Tk window.
#
# Copyright (c) 2007 Reinhard Max
# Copyright (c) 2007 Pat Thoyts
#
# Original stream reading code: http://wiki.tcl.tk/19957
#

if {[catch {
    package require Tk
    package require http
    package require img::jpeg
}]} {
    return
}

namespace eval ::tkchat::mjpeg {
    variable version 1.1.0
    variable subsample
    if {![info exists subsample]} { set subsample 1 }
    variable retrycount
    if {![info exists retrycount]} { set retrycount 0 }
    variable webstreams {}
    variable streams
    if {![info exists streams]} {
        # list of: title url title url ...
        set streams {}
    }
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
        # Work around the broken timeout in the http package
        upvar #0 $tok http
        if {[info exists http(after)]} { after cancel $http(after) }

        after idle [list [namespace origin Watchdog] $dlg $fd $tok]
        set boundary "myboundary"
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
	    if {[string match --$boundary* $line]} {set state mime; set hdrs {}}
            return [string length $line]
	}
	mime {
            variable retrycount 0
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
                    set img [namespace current]::rawImg
                    if {[info commands $img] eq {}} {
                        image create photo $img -data $frame
                        image create photo [namespace current]::displayImg
                    } else {
                        $img configure -data $frame
                    }
                    [namespace current]::displayImg copy $img \
                        -shrink -subsample $subsample $subsample
                    $dlg.image configure -image [namespace current]::displayImg
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
            $dlg.status.progress configure -mode determinate -value $value
        }
    }
}

proc ::tkchat::mjpeg::Cleanup {dlg {retry 0}} {
    variable token
    variable watchdog
    variable retrycount
    if {[info exists watchdog]} { after cancel $watchdog }
    if {[info exists token]} {
        upvar #0 $token state
        set url ""
        if {[info exists state]} {
            catch {set url $state(url)}
            if {[catch {::http::Eof $token} err]} { puts stderr $err }
            if {[catch {::http::cleanup $token} err]} { puts stderr $err }
        }
        catch {image delete [namespace current]::rawImg}
        catch {image delete [namespace current]::displayImg}
        if {$retry && $retrycount < 10 && $url ne ""} {
            incr retrycount
            OpenStream $url $dlg
        } else {
            if {$retrycount > 2} {
                ::tkchat::addStatus 0 "Too many failed attempts\
                    accessing MJPEG stream. Giving up." end ERROR
            } elseif {$retry && $url eq ""} {
                ::tkchat::addStatus 0 "Lost the video stream. Giving up." end ERROR
            }
            destroy $dlg
        }
    } else {
        destroy $dlg
    }        
}

proc ::tkchat::mjpeg::Scaling {dlg factor} {
    variable subsample
    $dlg.status.pane$subsample configure -relief flat
    set subsample $factor
    set img [namespace current]::rawImg
    if {[info commands $img] ne {}} {
        [namespace current]::displayImg copy $img \
            -shrink -subsample $subsample $subsample
        $dlg.image configure -image [namespace current]::displayImg
    }
    $dlg.status.pane$factor configure -relief sunken
}

proc ::tkchat::mjpeg::Open {url {title "Video stream"}} {
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

    #image create photo [namespace current]::rawImg -width 800 -height 600
    #image create photo foo2
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

proc ::tkchat::mjpeg::OpenStream {url dlg args} {
    if {[catch {
        variable toread 0
        variable state boundary
        variable boundary ; unset -nocomplain boundary
        variable frame ""
        variable token; if {[info exists token]} { catch {http::cleanup $token} }
        puts stderr "Open $url (args:$args)"
        Status $dlg "Opening $url"
        http::geturl $url \
            -timeout 10000 \
            -handler [list [namespace origin Read] $dlg] \
            -command [list [namespace origin OpenStream] $url $dlg]
    } err]} {
        ::tkchat::addStatus 0 $err end ERROR
    }
}

proc ::tkchat::mjpeg::ChooseStream {} {
    variable ::tkchat::NS
    set dlg [::tkchat::Dialog .mjpeg_choosestream]
    variable $dlg {}
    wm withdraw $dlg
    wm title $dlg "Select stream"
    set f [${NS}::frame $dlg.f -padding 2]

    ${NS}::label $f.info -text "Enter a URL to open.\
         The title is optional and will be displayed in the\
         \nmenu for this stream." -anchor nw
    ${NS}::label $f.tl -anchor nw -text "Title: "
    ${NS}::entry $f.te -width 24
    ${NS}::label $f.l -anchor nw -text "URL: "
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
        after idle [list [namespace origin Open] $url $title]
    }
    unset -nocomplain $dlg 
    destroy $dlg
}

proc ::tkchat::mjpeg::FillMenu {m} {
    variable streams
    variable webstreams
    $m delete 0 end
    $m add command -label "Open stream..." -underline 0 \
        -command [namespace origin ChooseStream]
    if {[llength $webstreams] > 0} {
        $m add separator
        foreach {name url} $webstreams {
            if {$name eq "-"} {
                $m add separator
            } else {
                $m add command -label $name -command [list [namespace origin Open] $url $name]
            }
        }
    }
    $m add separator
    foreach {name url} $streams {
        if {$name eq "-"} {
            $m add separator
        } else {
            $m add command -label $name -command [list [namespace origin Open] $url $name]
        }
    }
}

# Inject our menu into the tkchat File menu.
proc ::tkchat::mjpeg::InitHook {} {
    set str "Video feeds"
    if {[catch {.mbar.file index $str}]} {
        if {![catch {set ndx [.mbar.file index end]}]} {
            .mbar.file insert [incr ndx -1] cascade -label $str \
                -menu [menu .mbar.file.video -tearoff 0 \
                           -postcommand [list [namespace origin FillMenu] .mbar.file.video]]
        }
    }
}

# Process the version hook - this lets the website push additional
# streaming channels to the user via headers (ie: for the conference).
proc ::tkchat::mjpeg::VersionHook {meta url} {
    variable streams
    variable webstreams
    if {[set ndx [lsearch -exact $meta X-TkChat-MJPEG]] != -1} {
        set data [lindex $meta [incr ndx]]
        catch {set data [base64::decode $data]}
        foreach pair $data {
            foreach {title url} $pair break
            if {[lsearch -exact $streams $url] == -1} {
                lappend webstreams $title $url
            }
        }
    }
    return
}

# Hook the tkchat save to record our new streams
proc ::tkchat::mjpeg::SaveHook {} {
    variable streams
    return [list namespace eval [namespace current] [list variable streams $streams]]
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
        $img copy [namespace current]::rawImg; # copy the full size image  
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
                                    -format "%a-%H%M%S.jpg"] \
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
::tkchat::Hook add save ::tkchat::mjpeg::SaveHook
::tkchat::Hook add version ::tkchat::mjpeg::VersionHook
package provide tkchat::mjpeg $::tkchat::mjpeg::version
# -------------------------------------------------------------------------
