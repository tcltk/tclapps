#
# Safe Whiteboard
#
# $Id: tkchat_whiteboard.tcl,v 1.4 2008/11/09 16:28:09 patthoyts Exp $

namespace eval ::tkchat::Whiteboard {
    variable version 1.0
    variable History
    if {![info exists History]} { set History [list] }
}

proc ::tkchat::Whiteboard::Init {} {
    global Options
    if { [winfo exists .whiteboard] } {

        wm deiconify .whiteboard
        focus .whiteboard

    } else {
        set dlg [::tkchat::Dialog .whiteboard -container 0]
        wm title $dlg "Whiteboard"
        wm transient $dlg {}
        wm withdraw $dlg
        frame $dlg.frame -container 1
        pack $dlg.frame -fill both -expand 1
        set slave [::safe::interpCreate whiteboard]
        ::safe::loadTk $slave -use $dlg.frame
        bind $dlg <Destroy> [list catch [list interp delete $slave]]
        interp alias $slave ::WhiteboardClear {} [namespace origin Clear]
        interp alias $slave ::WhiteboardLine {} [namespace origin Line]
        interp alias $slave ::WhiteboardScript {} [namespace origin Script]
        whiteboard eval {
            ttk::frame .wb
            ttk::entry .wb.e -textvariable wbentry
            ttk::button .wb.bclear -text Clear -command WhiteboardClear
            ttk::label .wb.status -anchor w
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

            grid .wb.e .wb.bclear -sticky news
            grid .wb.c -          -sticky news
            grid .wb.status -     -sticky sew
            grid rowconfigure    .wb 1 -weight 1
            grid columnconfigure .wb 0 -weight 1

            grid .wb -sticky news
            grid columnconfigure . 0 -weight 1
            grid rowconfigure    . 0 -weight 1
            proc Status {s} {.wb.status configure -text $s}
        }
        wm deiconify $dlg
        #if {$Options(UseTkOnly)} { wm geometry $dlg 350x300 }
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
    Script $cmd
}
    
# called from safe interp - eval script and transmit script
proc ::tkchat::Whiteboard::Script {script} {
    if {[catch { whiteboard eval $script }]} {
        whiteboard eval [list Status $err]
        return
    }
    Transmit $script
}

# -------------------------------------------------------------------------

# Evaluate transmitted script inside the safe interp
proc ::tkchat::Whiteboard::Eval {who script {color {}}} {
    global Options
    variable History
    if {![interp exists whiteboard]} {
        if {!$Options(EnableWhiteboard)} {
            return
        }
        Init
    }

    if {[catch {whiteboard eval $script} err]} {
        log::log info $::errorInfo
        whiteboard eval [list Status $err]
    } else {
        lappend History [list $who $script]
        whiteboard eval [list Status "$who is drawing"]
    }
}

# Transmit some tcl script to all listeners
proc ::tkchat::Whiteboard::Transmit {script} {
    global Options
    if {[catch {
        variable ::tkjabber::jabber
        variable ::tkjabber::conference
        set attrs [list xmlns urn:tkchat:whiteboard color $Options(MyColor)]
        set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $script]]
        $jabber send_message $conference -type groupchat -xlist $xlist
    } err]} {
        whiteboard eval [list Status $err]
    }
}

# -------------------------------------------------------------------------
package provide tkchat::whiteboard $::tkchat::Whiteboard::version
# -------------------------------------------------------------------------
