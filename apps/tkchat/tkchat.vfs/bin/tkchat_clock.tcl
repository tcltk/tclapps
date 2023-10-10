# tkchat plugin - Copyright (C) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# Add a clock to the statusbar displaying the time in the New Orleans
# for the Tcl 2007 conference

namespace eval ::tkchat::clock {
    variable version 1.0.0
    variable Options
    if {![info exists Options]} {
        array set Options {
            Enabled 0
            Timezone  :localtime
            Format  "%H:%M:%S"
            Tooltip ""
        }
    }

    namespace import ::tooltip::tooltip
}

proc ::tkchat::clock::Init {} {
    variable Options
    if {$Options(Enabled)} {
        if {[winfo exists .status] && ![winfo exists .status.clock]} {
            ttk::label .status.clock
            ::tkchat::StatusbarAddWidget .status .status.clock 1
            tooltip .status.clock $Options(Tooltip)
            variable timer [after idle [list [namespace origin Tick] 1000]]
        }
    }
}

proc ::tkchat::clock::Stop {} {
    variable timer
    if {[info exists timer]} { after cancel $timer }
    if {[winfo exists .status.clock]} { destroy .status.clock }
}
    
proc ::tkchat::clock::Tick {interval} {
    variable Options
    set txt [clock format [clock seconds] \
		 -format $Options(Format) \
		 -timezone $Options(Timezone)]
    .status.clock configure -text $txt
    variable timer [after $interval [info level 0]]
}

proc ::tkchat::clock::SaveHook {} {
    variable Options
    lappend data {} [list variable Options]
    lappend data [list array set Options [array get Options]] {}
    return [list namespace eval [namespace current] [join $data \n]]
}

proc ::tkchat::clock::ClockEnable {widgets varname key op} {
    variable EditOptions
    set state [expr {$EditOptions(Enabled) ? "normal" : "disabled"}]
    foreach w $widgets {
        if {[winfo exists $w]} {$w configure -state $state}
    }
    return
}

proc ::tkchat::clock::OptionsAccept {} {
    variable Options
    variable EditOptions
    array set Options [array get EditOptions]
    unset EditOptions
    Stop
    Init
}

proc ::tkchat::clock::OptionsCancel {} {
    variable EditOptions
    unset EditOptions
}

proc ::tkchat::clock::OptionsHook {parent} {
    variable Options
    variable EditOptions
    array set EditOptions [array get Options]
    set f [ttk::frame $parent.clock]
    set eb [ttk::checkbutton $f.eb -text "Enable clock" \
                -variable [namespace current]::EditOptions(Enabled)]
    set lf [ttk::labelframe $f.lf -labelwidget $eb]
    ttk::label $lf.ltz -anchor w -text Timezone
    ttk::entry $lf.etz -textvariable [namespace current]::EditOptions(Timezone)
    ttk::label $lf.lft -anchor w -text Format
    ttk::entry $lf.eft -textvariable [namespace current]::EditOptions(Format)
    ttk::label $lf.ltt -anchor w -text Tooltip
    ttk::entry $lf.ett -textvariable [namespace current]::EditOptions(Tooltip)
    trace add variable [namespace current]::EditOptions(Enabled) write \
        [list [namespace origin ClockEnable] [list $lf.etz $lf.eft $lf.ett]]
    grid $lf.ltz $lf.etz -sticky news -padx 1 -pady 1
    grid $lf.lft $lf.eft -sticky news -padx 1 -pady 1
    grid $lf.ltt $lf.ett -sticky news -padx 1 -pady 1
    grid rowconfigure $lf 10 -weight 1
    grid columnconfigure $lf 1 -weight 1
    grid $lf -sticky new -padx 2 -pady 2
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 0 -weight 1
    bind $f <<TkchatOptionsAccept>> [namespace which OptionsAccept]
    bind $f <<TkchatOptionsCancel>> [namespace which OptionsCancel]
    return [list Clock $f]
}
   
# -------------------------------------------------------------------------
::tkchat::Hook add init ::tkchat::clock::Init
::tkchat::Hook add save ::tkchat::clock::SaveHook
::tkchat::Hook add options ::tkchat::clock::OptionsHook
package provide tkchat::clock $::tkchat::clock::version
# -------------------------------------------------------------------------
