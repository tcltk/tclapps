# -------------------------------------------------------------------------
# This file is part of the tkchat application
#
# Copyright (c) 2008 Pat Thoyts <patthoyts@users.sourceforge.net>
#
#	tkchat plugin interface for the askLEO package. This
#	binds Shift-Button-3 and a menu item to pass the selection
#	to the translation site at leo.org and displays the results
#	in a dialog.
#	Optionally display an icon in the statusbar too.
#
# -------------------------------------------------------------------------

if {[catch {package require askleo}]} { return }

namespace eval ::tkchat::askleo {
    variable version 1.0.0
    variable enabled;   if {![info exists enabled]} { set enabled 0 }
    variable statusbar; if {![info exists statusbar]} { set statusbar 0 }
}

# Show the LEO dialog even when no selection is ready.
proc ::tkchat::askleo::Show {} {
    if {[catch {selection get} query]} {
        wm deiconify .leo
    } else {
        ::dict.leo.org::askLEO $query
    }
}

proc ::tkchat::askleo::Init {} {
    variable statusbar
    variable enabled
    if {!$enabled} { return }

    # Initialize the askleo dialog (stays withdrawn)
    ::dict.leo.org::init

    # Insert menu items
    if {[winfo exists .mbar.help]} {
        set str "Ask LEO ..."
        if {[catch {.mbar.help index $str}]} {
            if {![catch {set ndx [.mbar.help index "Translate Selection"]}]} {
                .mbar.help insert [incr ndx] command -label $str \
                    -command [namespace code [list Show]]
            }
        }
    }

    # add statusbar icon
    if {$statusbar && [winfo exists .status]} {
        variable ::tkchat::NS
        image create photo ::tkchat::img::askleo -data {
            R0lGODdhEAAQAOMQABcEATsBA6IuTsktHos8f84iVY9PEIJIXHBGo1pM2ucp
            Q4Ry7OqMsf/mRfnenv/k9iwAAAAAEAAQAAAEhhBIEIpVo+lm3gzClW2dRwlS
            YGzcgyyUBTgAWyZJVSgBTbq4xIXnayGCCQVm0JsNHlAG9DGoOpqGgSJksVQ1
            sw9opxw4wI5H2iPgFjbXgFyCmEI1DreAcFwg/gsODhYPDAVQBwB8LwkPewSO
            AnYHfDh7fwwEkn92BJ4ECwsPBAcPCwmAUg8RADs=
        }
        ${NS}::label .status.leo -image ::tkchat::img::askleo
        bind .status.leo <Button-1> [namespace code [list Show]]
        if {[llength [package provide tooltip]] > 0} {
            tooltip::tooltip .status.leo "Translate selection using leo.org"
        }
        ::tkchat::StatusbarAddWidget .status .status.leo 1
    }

    # bind to main window and the history window (created in CreateGUI)
    bind .txt <Shift-Button-3> { ::dict.leo.org::askLEOforSelection }
    bind .clone <Shift-Button-3> { ::dict.leo.org::askLEOforSelection }
}

proc ::tkchat::askleo::Save {} {
    variable statusbar; variable enabled
    return [list namespace eval [namespace current] [subst {
        variable enabled $enabled
        variable statusbar $statusbar
    }]]
}

proc ::tkchat::askleo::OptionsHook {parent} {
    variable ::tkchat::NS; variable ::tkchat::useTile
    variable EditOptions ; variable statusbar
    variable enabled
    array set EditOptions [list statusbar $statusbar enabled $enabled]

    set page [${NS}::frame $parent.askleoOptions -borderwidth 0]
    ${NS}::checkbutton $page.en -text "Enable plugin" \
        -variable [namespace which -variable EditOptions](enabled)
    ${NS}::labelframe $page.lf -labelwidget $page.en
    ${NS}::checkbutton $page.lf.sb -text "Show icon in statusbar (needs restart)" \
        -variable [namespace which -variable EditOptions](statusbar)
    grid $page.lf.sb -sticky new -padx 2 -pady 2
    grid columnconfigure $page.lf 0 -weight 1
    grid $page.lf -sticky news -padx 2 -pady 2
    grid columnconfigure $page 0 -weight 1
    grid rowconfigure $page 2 -weight 1

    bind $page <<TkchatOptionsAccept>> [namespace code {
        variable EditOptions
        variable statusbar $EditOptions(statusbar)
        variable enabled   $EditOptions(enabled)
        unset EditOptions
    }]
    bind $page <<TkchatOptionsCancel>> [namespace code {
        variable EditOptions; unset EditOptions
    }]
    return [list "askLEO" $page]
}

# -------------------------------------------------------------------------
::tkchat::Hook add login ::tkchat::askleo::Init
::tkchat::Hook add options ::tkchat::askleo::OptionsHook
::tkchat::Hook add save ::tkchat::askleo::Save
package provide tkchat::askleo $::tkchat::askleo::version
# -------------------------------------------------------------------------
