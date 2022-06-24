# choosefont -- Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This package provides a font selection dialog. Where possible it will 
# use compiled code to launch a platform-specific dialog. In Windows this
# will be the font common dialog and the intention is to call the Gtk+
# font dialog where the Window manager suggests a GNOME system and a KDE
# dialog for a KDE system.
# We will drop down to DKF's font chooser if we have no other implementation.
#
# $Id: choosefont.tcl,v 1.1 2006/11/09 13:58:01 patthoyts Exp $

namespace eval ::choosefont {
    namespace export choosefont
}

proc ::choosefont::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

# ::choosefont::choosefont --
#	Default implementation is to use a pure Tcl implementation by DKF.
#	This is overridden if we successfuly load a compiled version.
#
proc ::choosefont::choosefont {args} {
    if {[llength [info command ::choosefont::ChooseFont]] != 0} {
        array set opts {-parent . -title "Select a font" \
                            -initialfont {} -apply {}}
        while {[string match -* [set option [lindex $args 0]]]} {
            switch -exact -- $option {
                -parent { set opts(-parent) [Pop args 1] }
                -title  { set opts(-title) [Pop args 1] }
                -apply  { set opts(-apply) [Pop args 1] }
                -initialfont { set opts(-initialfont) [Pop args 1] }
                --      { Pop args; break }
                default {
                    set err [join [lsort [array names opts]] ", "]
                    return -code error "bad option \"$option\":\
                        must be one of $err"
                }
            }
            Pop args
        }

        return [::choosefont::ChooseFont $opts(-parent) $opts(-title) \
                    $opts(-initialfont) $opts(-apply)]
    } else {
        return [eval [linsert $args 0 ::dkfFontSel::dkf_chooseFont]]
    }
}

