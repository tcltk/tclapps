# choosefont -- Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This package provides a font selection dialog. Where possible it will 
# use compiled code to launch a platform-specific dialog. In Windows this
# will be the font common dialog and the intention is to call the Gtk+
# font dialog where the Window manager suggests a GNOME system and a KDE
# dialog for a KDE system.
# We will drop down to DKFs font chooser if we have no other implementation.
#
# $Id: choosefont.tcl,v 1.2 2007/04/27 00:03:48 patthoyts Exp $

package require critcl

namespace eval ::choosefont {
}

# Use the Extended Window Manager Hints properties to find out the
# name of the current window manager.
proc get_wm_name {} {
    set result {}
    set atom _NET_SUPPORTING_WM_CHECK
    if {![catch {exec xprop -root -f $atom 32x " \$0+" $atom} check]} {
        set wid [lindex $check 1]
        set atom _NET_WM_NAME
        if {![catch {exec xprop -id $wid -f $atom 8t " \$0+" $atom} name]} {
            set result [lindex $name 1]
        }
    }
    return $result
}

proc ::choosefont::select_system {} {
    package require Tk
    wm with .
    switch -exact -- [tk windowingsystem] {
        win32 {
            critcl::tsources choose_w32.tcl 
        }
        x11 {
            switch -exact -- [lindex [set name [get_wm_name]] 0] {
                Metacity -
                IceWM - {
                    critcl::tsources choose_gtk.tcl
                }
                KWin {
                    return -code error "KDE version not implemented yet"
                }
                default {
                    return -code error "window manager \"$name\" not supported"
                }
            }
        }
        aqua -
        default {
            return -code error "platform not supported"
        }
    }
}

#::choosefont::select_system
