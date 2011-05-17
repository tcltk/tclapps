# Support the system tray icon on X11.
#
# This needs tktray and optionally dbus-tcl for notifications.
#
# Copyright (C) 2011 Pat Thoyts <patthoyts@users.sourceforge.net>

if {[tk windowingsystem] ne "x11"} { return }
if {[catch {package require tktray}]} { return }
package require msgcat

namespace eval ::tkchat::x11tray {
    variable version 1.0.0
    namespace import ::msgcat::mc
}

proc ::tkchat::x11tray::InitHook {} {
    global tkchat_dir
    set icofile [file join $tkchat_dir tkchat24.png]
    set warnfile [file join $tkchat_dir images dialog-warning.png]
    set img [image create photo ::tkchat::img::tray -file $icofile]
    set warn [image create photo ::tkchat::img::warn -file $warnfile]
    image create photo ::tkchat::img::tray_warn
    ::tkchat::img::tray_warn copy $img
    ::tkchat::img::tray_warn copy $warn -to 0 0

    bind TrayIcon <Configure> [list [namespace origin OnTrayConfigure] %W %w %h]
    variable tray [tktray::icon .tray -image $img -visible 1]
    bind . <FocusIn> +[list [namespace origin OnFocusIn] %W]

    variable dbus 0
    catch {
        package require dbus-tcl
        dbus connect session
        set dbus 1
    }

    set menu [menu $tray.popup]
    $menu add command -label "Available" -command [list ::tkjabber::back ""]
    $menu add command -label "Away" -command [list tkjabber::away "" away]
    $menu add command -label "Do not disturb" \
        -command [list tkjabber::away "" dnd]
    $menu add separator
    $menu add command -label [mc "Quit"] -command ::tkchat::quit
    
    bind $tray <Button-1> [list [namespace origin OnButton1] %W]
    bind $tray <Button-3> [list ::tk_popup $menu %X %Y]
}

proc ::tkchat::x11tray::OnTrayConfigure {w width height} {
    puts "tray configure $w $width $height"
}

proc ::tkchat::x11tray::OnButton1 {w} {
    variable last
    if {[wm state .] eq "withdrawn"} {
        wm deiconify .
        wm state . $last(state)
        wm geometry . $last(geom)
        focus .eMsg
    } else {
        set last(geom) [wm geometry .]
        set last(state) [wm state .]
        wm withdraw .
    }
}

proc ::tkchat::x11tray::AlertHook {w nick msg} {
    variable dbus
    variable last_message
    upvar #0 "::tkchat::ui_${nick}@all.tclers.tk" UI

    if {[focus -displayof [winfo toplevel $w]] eq {}} {
        if {$dbus} {
            set title "$nick"
            if {[info exists UI(FN)]} { set title $UI(FN) }
            if {[info exists last_message] \
                    && [lindex $last_message 0] eq $nick} {
                set msg [lindex $last_message 1]
            }
            alert_dbus $title $msg
        } else {
            alert_balloon $msg
        }
    }
}

proc ::tkchat::x11tray::alert_balloon {title body {timeout -1} {icon {}}} {
    variable msgid
    variable tray
    if {[info exists msgid]} {$tray cancel $msgid}
    $tray balloon $body 10
}

# Provided the user has installed the XDG desktop and icon files,
# we can have the notification pick those up. Otherwise we could provide
# the path to the svg file (if we ensure its copied to native fs).
proc ::tkchat::x11tray::alert_dbus {title body {timeout -1} {icon tkchat48}} {
    set iface org.freedesktop.Notifications
    set obj /org/freedesktop/Notifications
    dbus call -dest $iface -signature susssasa{sv}i $obj $iface Notify \
        "TkChat" 0 $icon $title $body {} \
        {category im desktop-entry tkchat} $timeout
}

proc ::tkchat::x11tray::MessageHook {nick msg msgtype args} {
    variable tray
    if {$msgtype ne "TRAFFIC"} {
        variable last_message [list $nick $msg]
        if {[focus] eq {}} {
            $tray configure -image ::tkchat::img::tray_warn
        }
    }
}

proc ::tkchat::x11tray::OnFocusIn {w} {
    variable tray
    $tray configure -image ::tkchat::img::tray
}

# -------------------------------------------------------------------------
::tkchat::Hook add init ::tkchat::x11tray::InitHook
::tkchat::Hook add alert ::tkchat::x11tray::AlertHook
::tkchat::Hook add message ::tkchat::x11tray::MessageHook
package provide tkchat::x11tray $::tkchat::x11tray::version
# -------------------------------------------------------------------------
