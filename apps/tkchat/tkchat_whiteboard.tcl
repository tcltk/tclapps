#
# Safe Whiteboard
#
# $Id: tkchat_whiteboard.tcl,v 1.1 2007/09/13 14:29:38 patthoyts Exp $

namespace eval ::tkchat::Whiteboard {
    variable version 1.0
}

proc ::tkchat::Whiteboard::Init {} {
    if { [winfo exists .whiteboard] } {

        wm deiconify .whiteboard
        focus .whiteboard

    } else {
        set dlg [::tkchat::Dialog .whiteboard -container 1]
        wm title $dlg "Whiteboard"
        wm transient $dlg {}
        set slave [::safe::interpCreate whiteboard]
        ::safe::loadTk $slave -use $dlg
        bind $dlg <Destroy> [list interp delete $slave]
        interp alias $slave ::WhiteboardClear {} [namespace origin Clear]
        interp alias $slave ::WhiteboardLine {} [namespace origin Line]
        interp alias $slave ::WhiteboardScript {} [namespace origin Script]
        whiteboard eval {
            if {[llength [info commands ::ttk::entry]] > 0} {
                ttk::frame .wb
                ttk::entry .wb.e -textvariable wbentry
                ttk::button .wb.bclear -text Clear -command WhiteboardClear
            } else {
                frame .wb
                entry .wb.e -background white -textvariable wbentry
                button .wb.bclear -text Clear -command WhiteboardClear
            }
            canvas .wb.c -background white -width 350 -height 300
            bind .wb.c <Button-1> {
                set id [%W create line %x %y %x %y]
            }
            bind .wb.c <Button1-Motion> {
                %W coords $id [concat [%W coords $id] %x %y]
            }
            bind .wb.c <ButtonRelease-1> {
                WhiteboardLine %W $id
            }
            bind .wb.e <Return> {WhiteboardScript $::wbentry}

            grid .wb.e .wb.bclear -sticky new
            grid .wb.c -          -sticky news
            grid rowconfigure    .wb 1 -weight 1
            grid columnconfigure .wb 0 -weight 1

            grid .wb -sticky news
            grid columnconfigure . 0 -weight 1
            grid rowconfigure    . 0 -weight 1
        }
    }
}

# -------------------------------------------------------------------------
# The following commands are aliased in the safe slave

# called from the safe interp, transmit new line
proc ::tkchat::Whiteboard::Line {w id} {
    set cmd [list .wb.c create line]
    foreach c [whiteboard eval [list $w coords $id]] {
	lappend cmd [expr { int(round($c)) }]
    }
    Transmit $cmd
}

# called from the safe interp, clean the whiteboard and tell everyone
proc ::tkchat::Whiteboard::Clear {} {
    set cmd [list .wb.c delete all]
    foreach c [whiteboard eval [list $w coords $id]] {
	lappend cmd [expr { int(round($c)) }]
    }
    Transmit $cmd
}
    
# called from safe interp - eval script and transmit script
proc ::tkchat::Whiteboard::Script {script} {
    if {[catch { whiteboard eval $script }]} {
        puts $err
        return
    }
    Transmit $script
}

# -------------------------------------------------------------------------

# Evaluate transmitted script inside the safe interp
proc ::tkchat::Whiteboard::Eval {script {color {}}} {
    if {![interp exists whiteboard]} {
        if {!$::tkchat::Options(EnableWhiteboard)} {
            return
        }
        Init
    }

    if {[catch {whiteboard eval $script} err]} {
        puts $::errorInfo
    }
}

# Transmit some tcl script to all listeners
proc ::tkchat::Whiteboard::Transmit {script} {
    if {[catch {
        variable ::tkjabber::jabber
        variable ::tkjabber::conference
        set attrs [list xmlns urn:tkchat:whiteboard color $::Options(MyColor)]
        set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $script]]
        $jabber send_message $conference -type groupchat -xlist $xlist
    } err]} {
        puts $::errorInfo
    }
}

# -------------------------------------------------------------------------
package provide tkchat::whiteboard $::tkchat::Whiteboard::version
# -------------------------------------------------------------------------
