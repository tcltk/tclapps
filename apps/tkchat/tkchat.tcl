#!/bin/sh
#
# Tk front end to the Tcl'ers chat
#
###########################################################
#
# author: Bruce B Hartweg brhartweg@bigfoot.com
# updates: Jeff Hobbs, et al
#
# This program is free to use, modify, extend at will,
# the author(s) provides no warantees, guarantees
# or any responsibility for the use, re-use, abuse
# that may or may not happen. If you somehow sell
# this and make a ton of money - good for you, how
# about sending me some?
############################################################
# \
      exec wish "$0" ${1+"$@"}


if {![info exists env(PATH)]} {
    set env(PATH) .
}

# For development, it is very convenient to be able to drop the extra
# packages into the CVS tree. Make sure we have the real location of 
# the script and not a link.
set script [file normalize [info script]]
while {[file type $script] eq "link"} {
    set script [file join [file dirname $script] [file readlink $script]]
}
set dir [file dirname [file normalize $script]]
set auto_path [linsert $::auto_path 0 $dir [file join $dir lib]]

package require Tcl 8.4		; # core Tcl
package require Tk  8.4		; # core Tk
package require http 2		; # core Tcl
package require msgcat		; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib
package require base64		; # tcllib

catch {package require tls}	; # tls (optional)
catch {package require choosefont};# font selection (optional) 
catch {package require askleo};# german translations (options)

package require sha1		; # tcllib
package require jlib		; # jlib
package require browse		; # jlib
package require muc		; # jlib

catch {package require khim}    ; # khim (optional)
catch {package require tooltip 1.2};# tooltips (optional)  

if { ![catch { tk inactive }] } {
    # Idle detection built into tk8.5a3
    namespace eval ::idle {
	proc ::idle::supported {} { return 1 }
	proc ::idle::idletime {} { return [expr { [tk inactive] / 1000 }] }
    }
} elseif { [catch {
    # Optional idle detection
    package require idle
}] } then {
    # Not supported / available...
    namespace eval ::idle {}
    proc ::idle::supported {} {return 0}
}

# Ensure that Tk widgets are available in the tk namespace. This is useful
# if we are using Ttk widgets as sometimes we need the originals.
#
if {[llength [info commands ::tk::label]] < 1} {
    foreach cmd { label entry text canvas menubutton button frame labelframe \
	    radiobutton checkbutton scale scrollbar} {
	rename ::$cmd ::tk::$cmd
	interp alias {} ::$cmd {} ::tk::$cmd
    }
}

# In Tk 8.5a6 the tile widgets have been merged into the Tk code in the
# ttk namespace. This provides detects the presence of themed widgets
# and provides compatability with tile 0.7
#
namespace eval ::tkchat {
    variable useTile 1
    variable useClosebutton 0
    variable NS "::ttk"
    if {[llength [info commands ::ttk::*]] == 0} {
        if {![catch {package require tile 0.8}]} {
            # we're all good
        } elseif {![catch {package require tile 0.7}]} {
            # tile to ttk compatability
            interp alias {} ::ttk::style {} ::style
            interp alias {} ::ttk::setTheme {} ::tile::setTheme
            interp alias {} ::ttk::themes {} ::tile::availableThemes
            interp alias {} ::ttk::LoadImages {} ::tile::LoadImages
        } else {
            set useTile 0
            set NS "::tk"
        }
    } else {
        # [PT]: experimental ttk styled pane closebutton.
        if {[lsearch [ttk::style element names] "smallclosebutton"] != -1} {
            ttk::style layout TClosebutton {
                Closebutton.button -sticky nswe -children {
                    Closebutton.padding -sticky nswe -children {
                        Closebutton.closebutton -sticky {}
                    }
                }
            }
            set useClosebutton 1
        }
    }
}

# If we're using KHIM, make all entries and texts use it.

if {[package provide khim] ne {}} {
    # Avoid trying to do this when re-sourcing the file
    if {[llength [info command ::tk::khimWrappedEntry]] == 0} {
        rename ::tk::entry ::tk::khimWrappedEntry
        proc ::tk::entry {w args} {
            eval [linsert $args 0 ::tk::khimWrappedEntry $w]
            bindtags $w [linsert [bindtags $w] 1 KHIM]
            return $w
        }
        rename ::tk::text ::tk::khimWrappedText
        proc ::tk::text {w args} {
            eval [linsert $args 0 ::tk::khimWrappedText $w]
            bindtags $w [linsert [bindtags $w] 1 KHIM]
            return $w
        }
        if {[llength [info commands ::ttk::entry]] >= 1} {
            rename ::ttk::entry ::ttk::khimWrappedEntry
            proc ::ttk::entry {w args} {
                eval [linsert $args 0 ::ttk::khimWrappedEntry $w]
                bindtags $w [linsert [bindtags $w] 1 KHIM]
                return $w
            }
        }
    }
}

# Under windows, we can use DDE to open urls
if {$tcl_platform(platform) eq "windows"
	&& $tcl_platform(os) ne "Windows CE"} {
    package require dde

    # Iocpsock is a Windows sockets extension that supports IPv6 sockets.
    # This package also provides more efficient IP sockets on windows.
    catch {package require Iocpsock}
}

package forget app-tkchat	; # Workaround until I can convince people
				; # that apps are not packages. :)  DGP
package provide app-tkchat \
	[regexp -inline -- {\d+(?:\.\d+)?} {$Revision: 1.372 $}]

namespace eval ::tkchat {
    variable chatWindowTitle "The Tcler's Chat"

    # Everything will eventually be namespaced
    variable MessageHooks
    array set MessageHooks {}

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://mini.net

    variable HEADUrl {http://tcllib.cvs.sourceforge.net/*checkout*/tcllib/tclapps/apps/tkchat/tkchat.tcl?revision=HEAD}
    variable rcsid   {$Id: tkchat.tcl,v 1.372 2007/03/31 14:58:31 patthoyts Exp $}

    variable MSGS
    set MSGS(entered) [list \
	    "%user% has entered the chat!" \
	    "Out of a cloud of smoke, %user% appears!" \
	    "%user% saunters in." \
	    "%user% wanders in." \
	    "%user% checks into the chat." \
	    "%user% is feeling chatty!" \
	    "A limousine pulls up, and %user% steps out into the crowd of waiting paparazzi." \
	    "%user% valt door een gat in het plafond naar binnen." \
	    "%user% wandeld luid schreeuwend binnen." \
	    "%user% \u8FDB\u95E8" \
	    "%user% \u9032\u9580" \
	    ]
    set MSGS(left) [list \
	    "%user% has left the chat!" \
	    "In a cloud of smoke, %user% disappears!" \
	    "%user% exits, stage left!" \
	    "%user% doesn't want to talk anymore!" \
	    "%user% looks at the clock and dashes out the door" \
	    "%user% macht wie eine Banane ..." \
	    "Ladies and Gentlemen, %user% has left the building!" \
	    "%user% opens a hidden trap door and escapes through it." \
	    "%user% zakt door de vloer en is weg." \
	    "%user% vertrekt stilletjes." \
	    ]
    set MSGS(nickchange) [list \
	    "In a fit of schizophrenia, %user% would like to be known as %newuser%." \
	    "%user% replaces their old hat with a new one called %newuser%." \
	    "The Amazing %user% switches to their secret identity, mild mannered %newuser%." \
	    "Admist a burst of smoke, %user% disappears and %newuser% materializes."
	    ]

    # Variables to control the search function.
    variable searchString ""
    variable searchOffset end

    # a variable to support nickname completion
    variable lastCompletion ""

    variable ircOnlineUsers [list]

    # used for dynamically created command aliases (added by JJM 25/Sep/2003)
    variable commandAliases
    array set commandAliases [list names [list] types [list] bodies [list]]

    #NoisyUsers: temporarily hide users who are blabbering
    variable noisyUsers
    variable MessageCounter 0
}

image create photo ::tkchat::img::link_secure -data {
    R0lGODlhEwAQAPQWAAAAAG5ICH1WDkhISFRUVGZmZnh4eIdeEptxHrSJMcid
    RNWpUeG1YPfKfIaGhpiYmKampre3t//nqMjIyNra2ubm5v///wAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAABYALAAAAAATABAAAAXtoGVZlmVZ
    lgUAAAAAlmVZlmVZlmVZgNSAS4IcgGVZlmVZlmUBUgMAACAcgGVZlgValmUB
    DWBZlgUkgGVZlmVZlgUsgGVZFggogGVZlmVZlgUkAAAAAMAAlmVZlmVZIAAg
    QEVNEYAAgGVZlmUBFXAAURQ9QACABWBZlmUB1QREkGM8DmAUgGVZlgVQEwRC
    BgBAjmEQgGVZlgVQEQQZABAZhkEAFmhZlgVEEWQAAABARkEAlmVZFhBBjwEA
    IABERTEAlmVZFhA9jwMBQFQUwwBYlmWBlgUYRVEYBkEMA2BZlmVZlgUAAAAA
    AAACgGWFADs=
}
image create photo ::tkchat::img::link_insecure -data {
    R0lGODlhEwAQAPQWAAAAAG5ICH1WDkZGRlRUVGZmZnh4eIdeEptxHrSJMcid
    RNWpUeG1YPfKfIaGhpiYmKampre3t//nqMjIyNra2ubm5v///wAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAABYALAAAAAATABAAAAXtoGUBAAAA
    gGVZlmVZlmVZFiA1S4IcgGWBlmVZlmVZgNQAAAAIB2BZlmVZlmUBDQBalmUB
    CWBZlmVZlmUBC2BZlgUogGVZFmhZlmUBCWBZlgUwAAAAAAAAlgUggGVZIAAg
    QEVN0QAMgAUcgGUBFRAAURQ9AzCABWABlmUB1QREkGM8zmAUgGVZlgVQEwRC
    BgBAjmEUgGVZlgVQEQQZABAZRkEAFmhZlgVEEWQAAABARkEAlmVZFhBBjwEA
    IABERTEAlmVZFhA9jwMBQFQUwwBYlmWBlgUYRVEYBkEMA2BZlmVZlgUAAAAA
    AAACgGWFADs=
}
image create photo ::tkchat::img::link_connected -data {
    R0lGODlhEAAQAMIGAAAAADs6OoZwV9zCmf/hyP/79////////yH5BAEKAAcA
    LAAAAAAQABAAAAM7eLrc/vAMIeIahYgx4yBEUGSVQ4EBqJUHJRQCqsbAqd3h
    jLuTDG6uhS3AmbAYsQGRc2wwgxGoZUqtQhIAOw==
}
image create photo ::tkchat::img::link_disconnected -data {
    R0lGODlhEAAQAMIGAAAAADs6OoZwV9zCmf/hyP/79////////yH5BAEKAAcA
    LAAAAAAQABAAAANBeLrc/nAIAdkohKoh3SBEsBSYpkygSHzgdEyFkL5ySwBZ
    Hi5gPgmS2aoH1KB2E47E9FqJNhwm1PR7VCvYrHZ7SAAAOw==
}

# -------------------------------------------------------------------------

msgcat::mcmset en_gb { Login "Connect" Logout "Disconnect" }
msgcat::mcmset de    {
    Login "Login" Logout "Ausloggen" Yes "Ja" No "Nein"
    "Subscribe request from %s" "Subskriptionsanfrage von %s"
}
msgcat::mcmset fr {
    Login "Se connecter" Logout "Se d\u00e9connecter" Yes "Oui" No "Non"
    "Subscribe request from %s" "Requ\u00eate d'enregistrement de %s"
}

# -------------------------------------------------------------------------

# Maybe exec a user defined preload script at startup (to set Tk options,
# for example.
# just before showing the logon screen (or not), call '::tkchat::rcPostload' so
# you can also tinker with settings when the UI has been built.
proc ::tkchat::rcPostload {} {}
if {[info exists ::env(HOME)] \
	&& [file readable [set rctclfile \
		[file join $::env(HOME) .tkchatrc.tcl]]]} {
    if { [catch { uplevel #0 source $rctclfile } err] } {
	tk_messageBox \
		-type ok \
		-icon error \
		-title "Error while loading \"$rctclfile\"" \
		-message $err
	::log::log error $err
	exit
    }
}

# trace handler to set the log level whenever Options(LogLevel) is changed
# enable the selected level and above
proc ::tkchat::LogLevelSet { args } {
    global Options

    ::log::lvSuppressLE emergency 0		; # unsuppress all
    ::log::lvSuppressLE $Options(LogLevel)	; # suppress all below selected
    ::log::lvSuppress $Options(LogLevel) 0	; # unsuppress selected
}

#  Pop the nth element off a list. Used in options processing.
proc ::tkchat::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

if {[llength [info commands ::lreverse]] == 0} {
    proc ::lreverse {list} {
        set res {}
        set i [llength $list]
        while {$i > 0} {lappend res [lindex $list [incr i -1]]}
        set res
    }
}

# If Proxy Authentication was specified then each HTTP request
# must have an authentication header. This procedure supports
# proxys accepting Basic authentication by builing the header
# required from the users login and password.
#  - PT
proc ::tkchat::buildProxyHeaders {} {
    global Options
    set auth {}
    if { $Options(UseProxy)
	&& [info exists Options(ProxyUsername)]
	&& $Options(ProxyUsername) != {}
    } then {
	if {![info exists Options(ProxyAuth)]} {
	    set Options(ProxyAuth) [list "Proxy-Authorization" \
		    [concat "Basic" \
		    [::base64::encode \
		    $Options(ProxyUsername):$Options(ProxyPassword)]]]
	}
	set auth $Options(ProxyAuth)
    }
    return $auth
}

# Automatically inject any proxy headers into the http request.
proc ::tkchat::proxyfilter {host} {
    global Options
    set r {}
    if {$Options(UseProxy)} {
        if {[string length $Options(ProxyHost)] != 0} {

            # Add authorisation header to the request (by Anders Ramdahl)
            catch {
                upvar state State
                
                if {[llength [set auth [buildProxyHeaders]]] != 0} {
                    set State(-headers) [concat $auth $State(-headers)]
                }
            }

            set r [list $Options(ProxyHost) $Options(ProxyPort)]
        }
    }
    return $r
}

# Retrieve the lastest version of tkchat from the SourceForge CVS.
# This code is (almost) entirely ripped from TkCon. - PT.
proc ::tkchat::Retrieve {} {
    variable HEADUrl
    set rcsVersion {}

    set defExt ""
    if {[string match "windows" $::tcl_platform(platform)]} {
	set defExt ".tcl"
    }

    set file [tk_getSaveFile \
	    -title "Save Latest TkChat to ..." \
	    -defaultextension $defExt \
	    -initialdir [file dirname $::argv0] \
	    -initialfile [file tail $::argv0] \
	    -parent . \
	    -filetypes {{"Tcl Files" {.tcl .tk}} {"All Files" {*.*}}}]
    if {[string compare $file ""]} {
	set token [::http::geturl $HEADUrl -timeout 30000]
	::http::wait $token
	if {[::http::status $token] eq "ok" && [::http::ncode $token] == 200} {
	    set code [catch {
		set data [::http::data $token]
		if {[string length $data] < 1} {
		    return -code error "Document was empty"
		}
		set fid [open $file w]
		fconfigure $fid -translation binary
		puts -nonewline $fid $data
		close $fid
		regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $data -> rcsVersion
	    } err]
	} else {
	    set code 1
	    set err [::http::error $token]
	    if {[string length $err] < 1} {
		# limit this to 30 lines
		set err [join [lrange [split \
			[http::data $token] "\n"] 0 30] "\n"]
	    }
	}

	::http::cleanup $token

	if {$code} {
	    tk_messageBox \
		    -type ok \
		    -icon error \
		    -title "Error retrieving tkchat from CVS" \
		    -message $err
	    ::log::log error $err
	} else {
	    set resource? [tk_messageBox \
		    -type yesno \
		    -icon info \
		    -title "Retrieved tkchat $rcsVersion" \
		    -message "Successfully retrieved v$rcsVersion.\
		    Do you want to reload from the new version?"]
	    if { ${resource?} eq "yes" } {
		Debug reload
	    }
	}
    }
}

# FIXME: no status page is available yet
proc ::tkchat::ShowStatusPage {} {
    set url http://mini.net/tcl/status_tclers.tk
    if { [catch {
	::http::geturl $url -command [list ::tkchat::fetchurldone \
                                          ::tkchat::ShowStatusPage2]
    } msg] } then {
	addStatus 0 "Unable to obtain status page from $url" end ERROR
    }
}

# FIXME: no status page is available yet
proc ::tkchat::ShowStatusPage2 {} {
    # parse and display
}

proc ::tkchat::GetHistLogIdx {url} {
    if { [catch {
        set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8" \
                      "Cache-Control" "no-cache" "Pragma" "no-cache"]
	::http::geturl $url -headers $hdrs \
            -progress [namespace origin Progress] \
            -command [list ::tkchat::fetchurldone ::tkchat::GotHistLogIdx]
    } msg] } then {
	addStatus 0 "Unable to obtain history from $url: \"$msg\"" end ERROR
    }
}

proc ::tkchat::GotHistLogIdx {tok} {
    set loglist {}
    #array set meta [set [set tok](meta)]
    log::log debug "history meta: [set [set tok](meta)]"

    set RE {<A HREF="([0-9\-%d]+\.tcl)">.*\s([0-9]+) bytes}
    foreach line [split [::http::data $tok] \n] {
	if { [regexp  -- $RE $line -> logname size] } {
	    set logname [string map {"%2d" -} $logname]
	    set size [expr { $size / 1024 }]k
	    lappend loglist $logname $size
	}
    }

    # Only show 7 days worth.
    set loglist [list [lrange $loglist end-13 end]]
    ::log::log debug "Logs: $loglist"
    after idle [list after 0 ::tkchat::LoadHistoryFromIndex $loglist]
    return
}

proc ::tkchat::ParseHistLog {log {reverse 0}} {
    global Options

    set url "$Options(JabberLogs)/$log"
    addStatus 0 "Loading chat history"

    # fetch log
    ::log::log info "History: Fetch log \"$url\""
    set hdrs [list "Accept-Charset" "utf-8" "Cache-Control" "no-cache" "Pragma" "no-cache"]
    set tok [::http::geturl $url -headers $hdrs -timeout 60000 \
                 -progress [namespace origin Progress]]

    ::log::log info \
	    "History: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
	    # Jabber logs
	    set I [interp create -safe]
	    interp alias $I m {} ::tkjabber::ParseLogMsg
	    if { $reverse } {
		set histTmp $::tkjabber::HistoryLines
		set ::tkjabber::HistoryLines {}
	    }
            # At the moment, the logs are stored in utf-8 format on the 
            # server but get issued as iso-8859-1 due to an error in the 
            # tclhttpd configuration.
            if {[string equal iso8859-1 [set [set tok](charset)]]} {
                $I eval [encoding convertfrom utf-8 [http::data $tok]]
            } else {
                $I eval [http::data $tok]
            }
	    if { $reverse } {
		set ::tkjabber::HistoryLines \
			[concat $::tkjabber::HistoryLines $histTmp]
	    }
	}
	reset {
	    ::log::log info "History fetch was reset."
	}
	timeout {
	    ::log::log info "History fetch timed out"
	}
	error {
	    tk_messageBox -message "History fetch error: [::http::error $tok]"
	}
	default {
	    ::log::log warning "::tkchat::ParseHistLog: Unknown switch option"
	}
    }
    ::http::cleanup $tok
}

# this called on first logon and after a purge
# so not bothering to background it
proc ::tkchat::LoadHistory {} {
    global Options

    # hook in the translation menu initialization (background function)
    if {$Options(UseBabelfish)} {
	babelfishMenu
    }

    if {$Options(HistoryLines) != 0} {
	set url "$Options(JabberLogs)/?pattern=*.tcl"
	GetHistLogIdx $url
    }
    
    GetTipIndex
}

proc ::tkchat::InsertHistoryMark {} {
    # Set a mark for the history insertion point.
    .txt configure -state normal
    .txt insert 0.0 \
	    "+++++++++++++++++++++ Loading History +++++++++++++++++++++\n"
    .txt mark set HISTORY 0.0
    .txt configure -state disabled
    .txt see end
}

# Called once we have acquired the log file index.
# logindex is a list of "filename sizeK filename ...."
proc ::tkchat::LoadHistoryFromIndex {logindex} {
    global Options
    variable NS

    set loglist {}
    array set logsize {}

    foreach {name size} $logindex {
	lappend loglist $name
	set logsize($name) $size
    }

    if {$Options(HistoryLines) < 0} {
	if {[llength $loglist] > 0} {
	    # ask user
	    set t .histQ
	    catch {destroy $t}
	    toplevel $t -class Dialog
	    wm withdraw $t
	    wm transient $t
	    wm protocol $t WM_DELETE_WINDOW {}
	    wm title $t "Load History From Logs"

            set f [${NS}::frame $t.f -borderwidth 0]

	    ${NS}::label $f.lbl \
		    -text "Please select how far back you want to load:"
	    grid $f.lbl -sticky ew -padx 5 -pady 5 -columnspan 3

	    set i 0
	    variable HistQueryNum [llength $loglist]
	    foreach l $loglist {
		${NS}::radiobutton $f.rb$i \
			-text "$l ($logsize($l))" \
			-value $i \
			-variable ::tkchat::HistQueryNum
		grid $f.rb$i -sticky w -padx 15 -pady 0 -column 1
		incr i
	    }

	    ${NS}::radiobutton $f.rb$i \
		    -text "None" \
		    -value $i \
		    -variable ::tkchat::HistQueryNum
	    ${NS}::button $f.ok \
		    -text Ok \
		    -width 8 \
		    -command [list destroy $t] \
		    -default active

	    grid $f.rb$i -sticky w -padx 15 -pady 0 -column 1
	    grid $f.ok -padx 5 -pady 10 -column 1

            pack $f -side top -fill both -expand 1
	    bind $t <Return> [list $f.ok invoke]
	    catch {::tk::PlaceWindow $t widget .}
	    wm deiconify $t
	    tkwait visibility $t
	    focus $f.ok
	    grab $t
	    tkwait window $t
	    InsertHistoryMark
	    foreach log [lrange $loglist $HistQueryNum end] {
		if {[catch {ParseHistLog $log} new]} {
		    ::log::log error "error parsing history: \"$new\""
		}
	    }
	}
    } else {
	# go thru logs in reverse until N lines loaded
	set idx [llength $loglist]
	InsertHistoryMark
	for { incr idx -1 } { $idx >= 0 } { incr idx -1 } {
	    # fetch log
	    set log [lindex $loglist $idx]
	    if {[catch {ParseHistLog $log 1} new]} {
		::log::log error "error parsing history: \"$new\""
	    }
	    if { [llength $::tkjabber::HistoryLines] \
		    >= $Options(HistoryLines) } {
		break
	    }
	}
    }
    .txt configure -state normal
    .txt delete "HISTORY + 1 char" "HISTORY + 1 line"
    .txt insert "HISTORY + 1 char" \
	    "+++++++++++++++++++++ Parsing History +++++++++++++++++++++\n"
    .txt configure -state disabled
    after idle [list after 0 ::tkjabber::LoadHistoryLines]
}


proc ::tkchat::HistoryPaneToggle {} {
    #
    # toggles the visibility of the separate (cloned) chat window
    # containing the history
    #
    # Either loads the current contents of the chat window into the
    # separate window and displays it ...
    #
    # ... Or make the window invisible clearing it from all content
    
    variable useTile
    # remember current position in window:
    set fraction [lindex [.txt yview] 1]
    if {[winfo ismapped .cframe]} {
	# remove cloned window:
	.pane2 forget .cframe
	update idletasks
	.clone configure -state normal
	.clone delete 1.0 end
	.mbar.vis entryconfigure "*current history*" -state normal
    } else {
	# fill clone and display it:
	if {$useTile} {
	    .pane2 insert 0 .cframe
	} else {
	    .pane2 add .cframe -before .txtframe
	}
	::tkchat::textClone .txt .clone
	.clone configure -state disabled
	.mbar.vis entryconfigure "*current history*" -state disabled
    }
    # restore current position in window:
    update idletasks
    .txt yview moveto $fraction
}

proc ::tkchat::logonChat {} {
    global Options

    if {[info exists Options(JabberDebug)] && $Options(JabberDebug)} {
	set jlib::debug 2
    }

    # Logon to the jabber server.
    tkjabber::connect
    if { ! $::tkchat::LoggedIn } {
	after 1000 {::tkchat::logonScreen}
    }
}

# -------------------------------------------------------------------------
# Error handling for http requests (history etc)

# Display the error message returned when an HTTP request results
# in an authentication error.
# Do NOT clean up this token - that's the callers job.
#
proc ::tkchat::AuthenticationError {token {prefix ""}} {
    variable msgtext ""

    ::log::log error "$prefix error: [http::code $token]"
    htmlparse::parse \
	    -cmd [list ::tkchat::ErrorMessageParse ::tkchat::msgtext] \
	    [http::data $token]
    regsub -all -line -- "\n{1,}" $msgtext "\n" msgtext
    tk_messageBox \
	    -title [http::code $token] \
	    -icon warning \
	    -message $msgtext
    unset msgtext
}

proc ::tkchat::ErrorMessageParse {varname tag end attr txt} {
    upvar #0 $varname v

    set tag [string tolower $tag]
    set end [string length $end]

    if { $end == 0 } {
	if { $tag eq "hmstart" } {
	    set v ""
	} elseif { [string match "h*" $tag] } {
	    append v "\n$txt"
	} elseif { $tag eq "p" } {
	    append v "\n$txt"
	} elseif { $tag eq "pre" } {
	    append v "\n$txt"
	}
    } elseif { $tag eq "a" } {
	append v "$txt"
    }
}

proc ::tkchat::HttpServerError {token {prefix ""}} {
    set msg "$prefix error: [::http::code $token]"

    ::log::log error $msg
    tk_messageBox -message $msg
}

# -------------------------------------------------------------------------
# Translate the selection using Babelfish.
# -------------------------------------------------------------------------

proc ::tkchat::fetchurldone {cmd tok} {
    ::log::log info \
	    "fetchurl: status was [::http::status $tok] [::http::code $tok]"
    if {[winfo exists .status.progress]} {
        grid forget .status.progress
    }

    switch -- [::http::status $tok] {
	ok - OK - Ok {
	    if {[::http::ncode $tok] >= 500} {
		HttpServerError $tok
	    } elseif {[::http::ncode $tok] >= 400} {
		AuthenticationError $tok
	    } else {
		$cmd $tok
	    }
	}
	reset - Reset - RESET {
	    ::log::log info "Reset called during fetch of URL"
	}
	timeout - Timeout - TIMEOUT {
	    ::log::log info "Timeout occurred during fetch of URL"
	}
	error - Error - ERROR {
	    tk_messageBox -message "Fetch URL error: [::http::error $tok]"
	}
	default {
	    ::log::log warning "::tkchat::fetchurldone: Unknown switch option"
	}
    }
    ::http::cleanup $tok
}

proc ::tkchat::translateSel {from to} {
    if {![catch {selection get} msg]} {
	::log::log debug "translate: $from $to \"$msg\""
	translate $from $to $msg
    }
}

proc ::tkchat::translate {from to txt} {
    set url {http://babelfish.altavista.com/babelfish/tr}
    append op $from _ $to
    set query [http::formatQuery tt urltext urltext $txt lp $op]
    set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8"]
    set tok [http::geturl $url -query $query -headers $hdrs \
	    -command [list ::tkchat::fetchurldone ::tkchat::translateDone]]
}

proc ::tkchat::translateDone {tok} {
    set ::tkchat::translate [http::data $tok]
    set r [regexp -- {<td.*?class=s><div.*?>(.*)</div>} \
	    [::http::data $tok] -> text]
    if {$r} {
	showInfo Translation [string trim $text]
    } else {
	::log::log info "Translation returned no matching data."
    }
}

proc ::tkchat::babelfishInit {
	{url http://babelfish.altavista.com/babelfish/} } {
    set hdrs [list "Accept-Charset" "ISO-8859-1,utf-8"]
    set tok [http::geturl $url -headers $hdrs \
        -command [list ::tkchat::fetchurldone ::tkchat::babelfishInitDone]]
}

proc ::tkchat::babelfishInitDone {tok} {
    ::log::log debug "Babelfish init done."
    set ::tkchat::babelfish [http::data $tok]
    if { [regexp -- {<select name="lp"[^>]*?>(.*?)</select>} \
	    [::http::data $tok] -> r] } {
	.mbar.help.tr delete 0 end
	set lst [split [string trim $r] \n]
	foreach option $lst {
	    regexp -- {<option value="(.*?)"[^>]*>(.*?)</option>} \
		    $option -> value label
	    set value [split $value _]
	    .mbar.help.tr add command \
		    -label $label \
		    -command [concat [namespace current]::translateSel $value]
	    variable babelfishinit
	    set babelfishinit 1
	}
    } else {
	::log::log debug "babelfish received no data"
    }
}

proc ::tkchat::babelfishMenu {} {
    set menu .mbar.help
    if {![winfo exists ${menu}.tr]} {
	::log::log debug "Initializing babelfish translation"
	set tr [menu ${menu}.tr -tearoff 0]

	# Add to the Help->Translate menu
	catch {
	    set ndx [$menu index "Translate Selection"]
	    $menu entryconfigure $ndx -menu $tr
	}
	::tkchat::babelfishInit
    }
}

# -------------------------------------------------------------------------

proc ::tkchat::MsgTo {{user "All Users"}} {
    global Options
    variable MsgToColors
    variable useTile
    set tile_version 0.7.8 ;#[package provide tile]
    set do_bg 1

    # There is a bug in 0.6 that messes up all this stuff.
    if {$useTile && [package vsatisfies $tile_version 0.6] \
	    && ![package vsatisfies $tile_version 0.7]} {
	set do_bg 0
    }

    set windows [list .eMsg .tMsg]
    if {$do_bg && ![info exists MsgToColors]} {
	foreach w $windows {
	    catch {
		set MsgToColors($w,normal) [$w cget -background]
		set MsgToColors($w,whisper) $Options(WhisperIndicatorColor)
	    }
	}
    }

    if { $user eq "All Users" } {
	set type normal
    } else {
	set type whisper
    }

    if {$do_bg} {
	foreach w $windows {
	    catch {$w configure -background $MsgToColors($w,$type)}
	}
    }

    set Options(MsgTo) $user
}

proc ::tkchat::invClr {clr {grays 0}} {
    # generally this is used to get a color that shows
    # up on a dark BG if it was originally a white BG
    # so even the color is grey & the inv color is also
    # grey that is OK
    set r 0; set g 0; set b 0 ;# default to black
    scan $clr %2x%2x%2x r g b
    set R [expr {(~$r)%256}]
    set G [expr {(~$g)%256}]
    set B [expr {(~$b)%256}]
    # A little extra magic to avoid near shades of grey
    if {$grays && abs($R-$r) < 32 && abs($G-$g) < 32 && abs($B-$b) < 32} {
	set R [expr {($r+128)%256}]
	set G [expr {($g+128)%256}]
	set B [expr {($b+128)%256}]
    }
    return [format "%02x%02x%02x" $R $G $B]
}

proc ::tkchat::getColor { nick } {
    global Options

    if { [info exists Options(Color,$nick)] } {
	set w [lindex $Options(Color,$nick) 0]
	set clr [lindex $Options(Color,$nick) $w]
    } else {
	set w [lindex $Options(Color,NICK-$nick) 0]
	set clr [lindex $Options(Color,NICK-$nick) $w]
    }
    return $clr
}

proc ::tkchat::fadeColor {color} {
    if {[scan $color "%2x%2x%2x" r g b] == 3} {
	foreach c {r g b} {
	    set $c [expr {255 - int((255-[set $c]) * .5)}]
	}
	set color [format "%02x%02x%02x" $r $g $b]
    }
    return $color
}

proc ::tkchat::parseStr {str} {
    # get href info return list of str link pairs
    set sList {}
    set HTTPRE {(?x)(https?|ftp)://
	[[:alnum:]]+[^[:space:]]*[^[:space:].,!;&?()\[\]{}<>:'\"]+
    }
    while {[regexp -nocase -- $HTTPRE $str url]} {
	set pre ""
	set post ""
	set pos [string first $url $str]
	if { $pos > 0 } {
	    set pre [string range $str 0 [expr {$pos-1}]]
	}
	set post [string range $str [expr {$pos+[string length $url]}] end]

	if {[string length $pre]} {
	    lappend sList $pre ""
	}
	lappend sList $url [string map {% %%} $url]
	set str $post
    }

    if {[string length $str]} {
	lappend sList $str ""
    }
    set out {}
    variable TipIndex
    # Assume any 6 or 7-digit sequence is a SF bug id and make URLs for them
    foreach {str url} $sList {
	if {[string length $url]} {
	    lappend out $str $url {}
	    continue
	}
	while {[regexp -- {^(.*?)(\m[0-9]{6,7}\M)(.*?)$} \
		    $str -> pre id post]} {
	    if {[string length $pre]} {
		lappend out $pre {} {}
	    }
	    set url "http://sourceforge.net/support/tracker.php?aid=$id"
	    lappend out $id $url $url
	    set str $post
	}
        while {[regexp -- {^(.*?)[Tt][Ii][Pp]\s?\#?(\d+)(.*?)$} $str -> pre id post]} {
            if {[string length $pre]} { lappend out $pre {} {} }
            set tt ""
            if {[info exists TipIndex] && $id < [llength $TipIndex] && $id >= 0} {
                catch {
                    array set tip [lindex $TipIndex $id]
                    set tt $tip(Title)
                }
            }
            lappend out "tip $id" "http://tip.tcl.tk/$id" $tt
            set str $post
        }
	if {[string length $str]} {
	    lappend out $str {} {}
	}
    }
    return $out
}

proc ::tkchat::checkNick { w nick clr timestamp } {
    global Options

    if { $timestamp == 0 } {
	set timestamp [clock seconds]
    }
    set match 0
    foreach nk $Options(NickList) {
	if { [lindex $nk 0] eq $nick } {
	    if { $timestamp > [lindex $nk 1] } {
		lset Options(NickList) $match [lset nk 1 $timestamp]
	    }
	    break
	} else {
	    incr match
	}
    }
    if { $match == [llength $Options(NickList)] } {
	# Set text indent
	checkNickWidth $nick
	# Set tabs appropriate for STAMP visibility
	StampVis
	lappend Options(NickList) [list $nick $timestamp]
	set Options(NickList) [lsort -dictionary -index 0 $Options(NickList)]
	set Options(Color,NICK-$nick) $Options(Color,MainFG)
	NickVisMenu
	if { $clr eq "" } {
	    set clr [getColor MainFG]
	}
    }
    if { $clr ne "" && [lindex $Options(Color,NICK-$nick) 1] ne $clr } {
	# new color
	lset Options(Color,NICK-$nick) 1 $clr
	lset Options(Color,NICK-$nick) 2 [invClr $clr]
	set clr [getColor $nick]
	$w tag configure NICK-$nick -foreground "#$clr"
	.pane.names tag configure NICK-$nick -foreground "#$clr"
	$w tag configure NOLOG-$nick -foreground "#[fadeColor $clr]"
	$w tag lower NICK-$nick STAMP
    }
}

proc ::tkchat::checkNickWidth { nick } {
    set nickWidth [expr { [font measure NAME <$nick>] + 10 }]
    if { $nickWidth > $::Options(Offset) } {
	# Maybe limit the nick column width a bit...
	set max [expr { [font measure NAME [string repeat X 12]] + 10 }]
	if { $nickWidth <= $max } {
	    set ::Options(Offset) $nickWidth
	}
    }
}

# Beep and/or deiconify and raise the main window as an idle callback.
# This is done as an idle callback because there might be many requests
# to alert in a row and we want to batch them all together into one
# action.
#
proc ::tkchat::alertWhenIdle { w } {
    variable alert_pending

    if { ![info exists alert_pending] } {
	set alert_pending 1
	if { $::Options(AutoBookmark) && $w eq ".txt" && [focus] == {} } {
	    .txt mark set AddBookmark "end - 1 line linestart"
	    BookmarkToggle auto
	}
	after idle [namespace origin alertCallback]
    }
}

proc ::tkchat::alertCallback {} {
    global Options
    variable alert_pending

    unset -nocomplain alert_pending
    if {$Options(Alert,RAISE) && [llength [focus -displayof .]]==0} {
	# Only call this if the window doesn't already have focus
	wm deiconify .
	raise .
    }
    if {$Options(Alert,SOUND)} bell
}

# Check to see if an alert is desired for the given message.  Issue
# the alert if so.
#
# As a side effect, record the time of last post for user $nick in
# the global LastPost() array.
#
proc ::tkchat::checkAlert { w msgtype nick msg } {
    global Options
    variable LastPost

    set now [clock seconds]
    set alert 0

    if { $Options(Alert,$msgtype) && $nick ne $Options(Nickname) } {
	if { $Options(Alert,ALL) } {
	    set alert 1
	} else {
	    if { $Options(Alert,ME) } {
		set myname [string tolower $Options(Username)]
		set mynick [string tolower $Options(Nickname)]
		set txt [string tolower $msg]
		if { ($w eq ".txt" && [string match " whispers*" $txt]) \
			|| [string first $myname $txt] >=0 \
			|| [string first $mynick $txt] >=0 } {
		    set alert 1
		}
	    }
	    if { !$alert && $Options(Alert,TOPIC) } {
		if { ![info exists LastPost($nick)] \
			|| $LastPost($nick) < $now - 300 } {
		    set alert 1
		}
	    }
	}
    }
    if { $alert } {
	alertWhenIdle $w
    }
    set LastPost($nick) $now
}

proc ::tkchat::setAlert { tag } {
    global Options

    if { $Options(Alert,$tag) } {
	if { !$Options(Alert,RAISE) } {
	    set Options(Alert,SOUND) 1
	}
	if { !$Options(Alert,ACTION) } {
	    set Options(Alert,NORMAL) 1
	}
    }
}

proc ::tkchat::addMessage \
	{ w clr nick msg msgtype mark timestamp {extraOpts ""} } {
    variable MessageHooks

    array set opts $extraOpts
            
    #for colors, it is better to extract the displayed nick from the one used
    #for tags.
    set displayNick $nick
    regexp -- {^<{0,2}(.+?)>{0,2}$} $nick displayNick nick

    if { [nickIsNoisy $nick] } {
	return
    }
    checkNick $w $nick $clr $timestamp

    # Special handling for single dot action message
    set tags [list NICK-$nick]
    if { [string trim $msg] eq "." && $::Options(Username) ne $nick } {
	lappend tags SINGLEDOT
    }

    $w configure -state normal
    InsertTimestamp $w $nick $mark $timestamp $tags

    # Call message activity hooks
    if { $mark ne "HISTORY" } {
	checkAlert $w $msgtype $nick $msg
	if { $w eq ".txt" } {
	    foreach cmd [array names MessageHooks] {
		eval $cmd [list $nick $msg $msgtype $mark $timestamp]
	    }
	}
    }

    if { $msgtype eq "ACTION" } {
	$w insert $mark "   * $displayNick " [concat BOOKMARK NICK $tags]
	lappend tags ACTION
    } else {
	$w insert $mark "$displayNick\t" [concat BOOKMARK NICK $tags]
    }
    lappend tags MSG [list NICK-$nick]
    if { [info exists opts(nolog)] } {
	lappend tags [list NOLOG-$nick NOLOG]
    }
    set usett [llength [package provide tooltip]]
    foreach { str url tt } [parseStr $msg] {
	if { $url ne "" } {
	    set urltag [concat $tags URL URL-[incr ::URLID]]
	    $w tag bind URL-$::URLID <Button-1> [list ::tkchat::gotoURL $url]
            if {$usett && [string length $tt] > 0} {
                tooltip::tooltip $w -tag URL-$::URLID $tt
            }
	} else {
	    set urltag $tags
	}

	# Split into lines, so we can insert the proper tabs for
	# timestamps:
	set i 0
	foreach line [split $str \n] {
	    if { $i } {
		# The first line has the timestamp, only
		# subsequent lines need an extra tab char
		#::log::log debug "More than one line, add tabs"
		$w insert $mark \n $tags \t [list STAMP NICK-$nick] \t $tags
	    }
	    Insert $w $line $urltag $url $mark
	    set i 1
	}
    }
    $w insert $mark "\n" $tags
    $w configure -state disabled
    if { $::Options(AutoScroll) } {
	$w see end
    }
}

# Provide an indication of the number of messages since the window was last
# in focus.
proc ::tkchat::IncrMessageCounter { nick msg msgtype args } {
    if { [focus] == {} && $msgtype ne "TRAFFIC"} {
	variable chatWindowTitle
	variable MessageCounter

	if { !$MessageCounter & $::Options(AutoBookmark) } {
	    .txt mark set AddBookmark "end - 1 line linestart"
	    BookmarkToggle auto
	}
	incr MessageCounter
	set title "$MessageCounter - $chatWindowTitle"
	wm title . $title
	wm iconname . $title
	WinicoUpdate
    }
}

proc ::tkchat::ResetMessageCounter {} {
    if { [focus] != {} } {
	variable MessageCounter
	variable chatWindowTitle

	set MessageCounter 0
	set title $chatWindowTitle
	wm title . $title
	wm iconname . $title
	WinicoUpdate
    }
}

proc ::tkchat::InsertTimestamp { w nick mark timestamp {tags {}} } {
    # The nick argument is here, so we can display the local time for
    # each nick.
    if { $timestamp == 0 } {
	set timestamp [clock seconds]
    }
    $w insert $mark "\[[clock format $timestamp -format %H:%M]\]\t" \
	[concat BOOKMARK STAMP $tags]
}

proc ::tkchat::Insert { w str tags url mark } {
    global Options

    # Don't do emoticons on URLs
    if { ($url eq "") && $Options(Emoticons) } {
	variable IMG
	variable IMGre
	set i 0
	foreach match [regexp -inline -all -indices -- $IMGre $str] {
	    foreach { start end } $match break
	    set emot [string range $str $start $end]
	    $w insert $mark [string range $str $i [expr { $start - 1 }]] $tags
	    if { $mark eq "end" } {
		set idx [$w index "$mark -1 char"]
	    } else {
		set idx [$w index $mark]
	    }
	    $w image create $mark -image ::tkchat::img::$IMG($emot)
	    foreach tg $tags {
		$w tag add $tg $idx
	    }
	    set i [expr { $end + 1 }]
	}
	if { $i <= [string length $str] } {
	    $w insert $mark [string range $str $i end] $tags
	}
    } else {
	# no emoticons?  perish the thought ...
	$w insert $mark $str $tags
    }
}

proc ::tkchat::Hook {do type cmd} {
    switch -glob -- $type {
	msg -
	mes* {
	    set var [namespace current]::MessageHooks
	}
	default {
	    return -code error \
		    "unknown hook type \"$type\": must be message"
	}
    }
    switch -exact -- $do {
	add {
	    set ${var}($cmd) {}
	}
	remove {
	    unset -nocomplain -- ${var}($cmd)
	}
	default {
	    return -code error \
		    "unknown hook action \"$type\": must be add or remove"
	}
    }
}

proc ::tkchat::say { nick msg args } {
    # I've added a few lines to make this speak new messages via the
    # festival synthesiser. It doesn't do it robustly as yet (you'll need
    # festival installed) but as a quick (1min) hack it's got heaps of
    # cool points...  -- Steve Cassidy
    variable festival
    if {![info exists festival]} {
	set festival [open "|festival --pipe" w]
    }

    ::log::log debug [string map [list "\"" ""] $msg]
    puts $festival "(SayText \"$msg\")"
    flush $festival
}

if {0 && [string length [auto_execok festival]]} {
    ## Don't add this by default ...
    ::tkchat::Hook add message ::tkchat::say
}

proc ::tkchat::findExecutable {progname varname} {
    upvar 1 $varname result
    set progs [auto_execok $progname]
    if {[llength $progs]} {
	set result [lindex $progs 0]
    }
    return [llength $progs]
}

proc ::tkchat::gotoURL {url} {
    # this can take a bit
    . configure -cursor watch
    .txt configure -cursor watch
    update
    if {[regexp -nocase -- {&url=(.*)} $url -> trueUrl]} {
	# this was a redirect - just get final destination
	set url $trueUrl
    } elseif {[regexp -nocase -- {^chat} $url]} {
	# this is a relative url
	set url "$::tkchat::HOST/cgi-bin/$url"
    } else {
	# assume a raw url
    }

    # Set the clipboard value to this url in-case the user needs to paste the
    # url in (some windows systems).
    clipboard clear
    clipboard append $url

    global tcl_platform Options

    # this code from  http://purl.org/mini/tcl/557.html
    switch -- $tcl_platform(platform) {
	"unix" {
	    # special case for MacOS X:
	    if {$tcl_platform(os) == "Darwin"} {
		# assume all goes well:
		set notOK 0
		if { $Options(Browser) ne "" } {
		    set notOK \
			    [catch {exec open -a $Options(Browser) $url} emsg]
		}
		if {$notOK} {
		    # Safari should always be there:
		    set notOK [catch {exec open -a Safari $url} emsg]
		    if {$notOK} {
			tk_messageBox -message \
				"Error displaying $url in browser\n$emsg"
		    }
		}
	    } else {
		expr {
		    $Options(Browser) ne ""
		    || [findExecutable mozilla		Options(Browser)]
		    || [findExecutable mozilla-firefox	Options(Browser)]
		    || [findExecutable mozilla-firebird	Options(Browser)]
		    || [findExecutable konqueror	Options(Browser)]
		    || [findExecutable netscape		Options(Browser)]
		    || [findExecutable iexplorer	Options(Browser)]
		    || [findExecutable lynx		Options(Browser)]
		}

		# lynx can also output formatted text to a variable
		# with the -dump option, as a last resort:
		# set formatted_text [ exec lynx -dump $url ] - PSE
		#
		# -remote argument might need formatting as a command
		# 		Try that first
		if { [catch {
		    if {$Options(BrowserTab)} {
			exec $Options(Browser) -remote "openURL($url,new-page)" 2> /dev/null
		    } else {
		    	exec $Options(Browser) -remote openURL($url) 2> /dev/null
		    }
		}] } then {
		    # Try -remote with raw URL argument
		    if { [catch {
			exec $Options(Browser) -remote $url 2> /dev/null
		    }]} then {
			# perhaps browser doesn't understand -remote flag
			if { [catch { exec $Options(Browser) $url & } emsg] } {
			    tk_messageBox -message \
				    "Error displaying $url in browser\n$emsg"
			}
		    }
		}
	    }
	}
	"windows" {
	    # DDE uses commas to separate command parts
	    set url [string map {, %2c} $url]

	    # See if we can use dde and an existing browser.
	    set handled 0
	    foreach app \
		    {Firefox {Mozilla Firebird} Mozilla Netscape IExplore} {
		if {[set srv [dde services $app WWW_OpenURL]] != {}} {
		    if {[catch {dde execute $app WWW_OpenURL $url} msg]} {
			::log::log debug "dde exec $app failed: \"$msg\""
		    } else {
			set handled 1
			break
		    }
		}
	    }

	    # The windows NT shell treats '&' as a special character. Using
	    # a '^' will escape it. See http://wiki.tcl.tk/557 for more info.
	    if {! $handled} {
		if { $tcl_platform(os) eq "Windows NT" } {
		    set url [string map {& ^&} $url]
		}
		if { [catch {
		    eval exec [auto_execok start] [list $url] &
		} emsg]} then {
		    tk_messageBox -message \
			    "Error displaying $url in browser\n$emsg"
		}
	    }
	}
	"macintosh" {
	    if {![info exists env(BROWSER)]} {
		set env(BROWSER) "Browse the Internet"
	    }
	    if {[catch {
		AppleScript execute\
		      "tell application \"$env(BROWSER)\"
		open url \"$url\"
		end tell
		"} emsg]
	    } then {
		tk_messageBox -message \
		      "Error displaying $url in browser\n$emsg"
	    }
	}
	default {
	    ::log::log warning \
		"::tkchat::gotoURL: Unknown platform '$tcl_platform(platform)'"
	}
    }
    . configure -cursor {}
    .txt configure -cursor left_ptr
}

proc ::tkchat::addSystem { w msg {mark end} {tags SYSTEM} {timestamp 0} } {
    $w configure -state normal
    InsertTimestamp $w "" $mark $timestamp $tags
    $w insert $mark "\t$msg\n" [concat MSG $tags]
    $w configure -state disabled
    if { $::Options(AutoScroll) } {
	$w see end
    }
}

proc ::tkchat::addStatus {pane msg {mark end} {tags SYSTEM} {timestamp 0}} {
    if {[winfo exists .status] && [winfo ismapped .status]} {
        variable StatusAfter
        if {$pane == 0} {
            catch {after cancel $StatusAfter}
            after 6000 [list set [namespace which -variable \
                                      Status]($pane) $::tkjabber::conference]
        }
        variable StatusHistory
        lappend StatusHistory [clock seconds] $msg
        variable Status
        set Status($pane) $msg
        if {[string equal [lindex $tags 0] ERROR]} {
            addSystem .txt $msg $mark $tags $timestamp
        }
        if {$pane == 1} {
            if {[string equal $msg "connected"]} {
                .status.pane1 configure -image ::tkchat::img::link_connected
                catch {
                    set tip [fconfigure $::tkjabber::socket -peername]
                    set tip [lindex $tip 0]:[lindex $tip 2]
                    set tip "Connected to $tip"
                    tooltip::tooltip .status.pane1 $tip
                }
            } else {
                .status.pane1 configure -image ::tkchat::img::link_disconnected
                tooltip::tooltip .status.pane1 ""
            }
        }
    } else {
	addSystem .txt $msg $mark $tags $timestamp
    }
}

proc ::tkchat::Progress {tok total current} {
    if {![winfo exists .status.progress]} { return }
    log::log debug "Progress $total $current"
    set w .status.progress
    if {![winfo ismapped $w]} {
        if {$total == $current} { return }
        set slaves [lreverse [grid slaves .status]]
        eval [linsert $slaves 0 grid forget]
        eval grid [linsert $slaves 1 $w] -sticky news
    }
    if {$total == $current} {
        if {[string equal [$w cget -mode] "determinate"]} {
            $w stop
        }
        grid forget .status.progress
    }
    if {$total == 0 && [string equal [$w cget -mode] "determinate"]} {
        $w configure -mode indeterminate
        $w start
    } elseif {$total != 0} {
        $w configure -mode determinate \
            -value [expr {int(double($current)/double($total) * 100)}]
    }
}

# Add notification of user entering or leaving.
# Always add these to text - just tag them so we can elide them at will
# this way, the hide option can affect the past as well as the future
proc ::tkchat::addTraffic { w nick action mark timestamp } {
    # Action should be entered, left, nickchange or availability
    variable MSGS
    variable MessageHooks
    variable OnlineUsers

    set newnick ""
    set network ""
    if { [llength $action] != 1 } {
	set network [lindex $action 1]
	set action [lindex $action 0]
    }
    if { $action eq "nickchange" } {
	set newnick [lindex $nick 1]
	set nick [lindex $nick 0]
    }

    # Call message activity hooks
    if { $mark ne "HISTORY" } {
	foreach cmd [array names MessageHooks] {
	    eval $cmd [list $nick $action TRAFFIC $mark $timestamp]
	}
	if { $action eq "entered" } {
	    if { $network ne "Jabber" } {
		set OnlineUsers($network-$nick,status) [list online]
	    }
	    lappend OnlineUsers($network) $nick
	    set OnlineUsers($network) \
		    [lsort -dictionary -unique $OnlineUsers($network)]
	} elseif { $action ne "availability" } {
	    unset -nocomplain OnlineUsers($network-$nick,status)
	    set OnlineUsers($network) [lsearch -exact -sorted -dictionary \
		    -all -inline -not $OnlineUsers($network) $nick]
	    if { $action eq "nickchange" } {
		set OnlineUsers($network-$newnick,status) [list online]
		lappend OnlineUsers($network) $newnick
		set OnlineUsers($network) \
			[lsort -dictionary -unique $OnlineUsers($network)]
	    }
	}
	updateOnlineNames
    }
    if { $network eq "IRC" } {
	# Single <> to show IRC users.
	set nick <$nick>
	if { $newnick ne "" } {
	    set newnick <$newnick>
	}
    } elseif { $network eq "WebChat" } {
	# Double <> to show WebChat users.
	set nick <<$nick>>
    }

    $w configure -state normal
    set tags [list TRAFFIC [string toupper $action]]
    InsertTimestamp $w "" $mark $timestamp $tags
    set tags [concat MSG $tags]
    if { $action eq "availability" } {
	set msg [lindex $nick 1 0]
	$w insert $mark "\t[lindex $nick 0] is $msg" $tags
	if { [lindex $nick 1 1] ne "" } {
	    set msg [lindex $nick 1 1]
	    $w insert $mark " (" $tags
	    Insert $w $msg $tags {} $mark
	    $w insert $mark ")" $tags
	}
	$w insert $mark "\n" $tags
    } else {
	set msg [string map -nocase [list %user% $nick %newuser% $newnick] \
		[lindex $MSGS($action) \
		[expr { int(rand() * [llength $MSGS($action)]) }]]]
	$w insert $mark "\t$msg\n" $tags
    }
    $w configure -state disabled
    if { $::Options(AutoScroll) } {
	$w see end
    }
}

proc ::tkchat::showInfo {title str} {
    variable NS
    set t .infobox
    set i 0
    while {[winfo exists $t]} {
	set t .infobox[incr i]
    }
    set dlg [toplevel $t -class Dialog]
    wm title $t $title
    set t [${NS}::frame $dlg.f -borderwidth 0]
    pack $t -side top -fill both -expand 1

    set height [expr {[string length $str] / 75 + 1}]
    if {[set lines [regexp -all -- "\n" $str]] > $height} {
	set height $lines
    }
    text $t.txt -cursor left_ptr -wrap word -height $height -font NAME
    pack $t.txt -expand 1 -fill both
    bind $t.txt <Button-1> { focus %W }
    $t.txt tag configure URL -underline 1
    $t.txt tag bind URL <Enter> [list $t.txt configure -cursor hand2]
    $t.txt tag bind URL <Leave> [list $t.txt configure -cursor left_ptr]
    foreach {str url tt} [parseStr $str] {
	if { $url eq "" } {
	    $t.txt insert end "$str " INFO
	} else {
	    $t.txt insert end "$str " [list INFO URL URL-[incr ::URLID]]
	    $t.txt tag bind URL-$::URLID <Button-1> \
		    [list ::tkchat::gotoURL $url]
	}
    }
    $t.txt insert end "\n"
    $t.txt configure -state disabled
    ${NS}::button $t.close -text Close -command [list destroy $dlg]
    focus $t.close
    pack $t.close -side right
}

proc ::tkchat::createFonts {} {
    font create FNT   -family helvetica -size -12 -weight normal -slant roman
    font create ACT   -family helvetica -size -12 -weight normal -slant italic
    font create NOLOG -family helvetica -size -12 -weight normal -slant roman
    font create NAME  -family helvetica -size -12 -weight bold   -slant roman
    font create SYS   -family helvetica -size -12 -weight bold   -slant italic
    font create STAMP -family helvetica -size -12 -weight bold   -slant roman
}

proc ::tkchat::displayUsers {} {
    global Options
    variable useTile
    if {[winfo exists .pane]} {
	if {$Options(DisplayUsers)} {
	    if {$useTile} {
		catch {.pane add $Options(NamesWin)}
	    } else {
		.pane add $Options(NamesWin) -sticky news
	    }
	} else {
	    .pane forget $Options(NamesWin)
	}
    } else {
	if {$Options(DisplayUsers)} {
	    grid $Options(NamesWin)
	} else {
	    grid remove $Options(NamesWin)
	}
    }
}

proc ::tkchat::findCommonRoot { words } {
    #takes a list of words/nicks and returns the longest string
    #that matches the beginning of all of them.

    set count [llength $words]
    if { $count <= 1 } {
	return $words
    }
    set word [lindex $words 0]
    for { set c 0 } { $c < [string length $word] } {incr c} {
	set partial [string range $word 0 $c]
	if { [lsearch -not -glob $words "$partial*"] > -1 } {
	    return [string range $partial 0 end-1]
	}
    }
    return $word
}

proc ::tkchat::deleteCompletions {} {
    .txt configure -state normal
    set range [.txt tag nextrange NICKCOMPLETE 0.0]
    while { [llength $range] > 0 } {
	.txt delete [lindex $range 0] [lindex $range 1]
	set range [.txt tag nextrange NICKCOMPLETE [lindex $range 0]]
    }
    .txt configure -state disabled
}

proc ::tkchat::nickComplete {} {
    #Bound to <Key-Tab> in the message entry widgets .eMsg and .tMsg
    #It will do nickname completion a'la bash command completion
    #nicknames are taken from the complete, stored nick list
    #not the users' online one. Which is too unreliable IMO.
    global Options
    variable lastCompletion
    variable OnlineUsers

    set nicks [list]
    foreach network $OnlineUsers(networks) {
	set nicks [concat $nicks $OnlineUsers($network)]
    }
    set nicks [lsort -dictionary -unique $nicks]

    if {[winfo ismapped .eMsg]} {
	#the entry is on screen
	#This fails to find the correct word when the $cursor != end
	set str [.eMsg get]
	set cursor [.eMsg index insert]
	set partial [string range $str [string wordstart $str $cursor] \
		[string wordend $str $cursor]]
    } else {
	set partial [.tMsg get "insert-1c wordstart" "insert-1c wordend"]
    }

    set matches [lsearch -all -inline -glob $nicks "$partial*"]

    switch -- [llength $matches] {
	0 {
	    bell
	    set lastCompletion ""
	    return
	}
	1 {
	    set match "$matches "
	    set lastCompletion ""
	}
	default {
	    set match [findCommonRoot $matches]
	    deleteCompletions
	    if { [llength $lastCompletion] > 0 } {
		if { [clock seconds]-2 > [lindex $lastCompletion 0] } {
		    set lastCompletion ""
		}
		if { [string length $match] > 0
			&& [lindex $lastCompletion 1] eq $match } {
		    .txt configure -state normal
		    .txt insert end "Completions: $matches\n" \
			    [list MSG NICKCOMPLETE]
		    .txt configure -state disabled
		    if { $Options(AutoScroll) } {
			.txt see end
		    }
		    after 5500 {
			if { [llength $::tkchat::lastCompletion] > 0 \
				&& [clock seconds] - 4 \
				    < [lindex $::tkchat::lastCompletion 0] } {
			    return
			}
			::tkchat::deleteCompletions
		    }
		}
	    }
	    set lastCompletion [list [clock seconds] $match]
	    bell
	}
    }

    if {[winfo ismapped .eMsg]} {
	.eMsg delete [string wordstart $str $cursor] \
	    [string wordend $str $cursor]
	.eMsg insert [string wordstart $str $cursor] $match
    } else {
	.tMsg delete "insert-1c wordstart" "insert-1c wordend"
	.tMsg insert insert $match
    }
}

proc ::tkchat::CreateGUI {} {
    global Options
    variable chatWindowTitle
    variable useTile
    variable NS

    # Pick an enhanced Tk style.
    set done 0
    if { $Options(Style) eq "any" || [string match "as*" $Options(Style)] } {
	if { ![catch { package require as::style }] } {
	    as::style::init
	    set done 1
	} elseif { ![catch { package require style::as }] } {
	    style::as::init
	    set done 1
	}
    }
    if { !$done
	    && ($Options(Style) eq "any"
		|| [string match "g*" $Options(Style)])
	    && [tk windowingsystem] eq "x11" } {
	::tkchat::GtkLookStyleInit
    }

    wm title . $chatWindowTitle
    wm withdraw .
    wm protocol . WM_DELETE_WINDOW [namespace origin quit]

    catch { createFonts }

    menu .mbar
    . configure -menu .mbar

    menu .mbar.file  -tearoff 0
    menu .mbar.edit  -tearoff 0
    menu .mbar.emot  -tearoff 0
    menu .mbar.vis   -tearoff 0
    menu .mbar.alert -tearoff 0
    menu .mbar.dbg   -tearoff 0
    menu .mbar.help  -tearoff 0
    .mbar add cascade -label "File"	   -underline 0 -menu .mbar.file
    .mbar add cascade -label "Preferences" -underline 0 -menu .mbar.edit
    .mbar add cascade -label "Emoticons"   -underline 0 -menu .mbar.emot
    .mbar add cascade -label "Visibility"  -underline 0 -menu .mbar.vis
    .mbar add cascade -label "Alerts"	   -underline 0 -menu .mbar.alert
    .mbar add cascade -label "Debug"	   -underline 0 -menu .mbar.dbg
    .mbar add cascade -label "Help"	   -underline 0 -menu .mbar.help

    ## File Menu
    ##
    set m .mbar.file
    $m add command \
	    -label [msgcat::mc Login] \
	    -underline 0 \
	    -command ::tkchat::logonScreen
    $m add command \
	    -label "Save Options" \
	    -underline 0 \
	    -command ::tkchat::saveRC
    $m add separator
    $m add command \
	    -label "Open Whiteboard" \
	    -underline 5 \
	    -command ::tkchat::whiteboard_open
    $m add command \
            -label "Open Paste Dialog" \
            -underline 5 \
            -command ::tkchat::PasteDlg
    $m add separator
    $m add command \
	    -label "Exit" \
	    -underline 1 \
	    -command ::tkchat::quit

    ## Preferences/Edit Menu
    ##
    set m .mbar.edit
    $m add checkbutton \
	    -label "Display Online Users" \
	    -underline 0 \
	    -variable Options(DisplayUsers) \
	    -command ::tkchat::displayUsers
    $m add checkbutton \
	    -label "Enable Whiteboard" \
	    -underline 0 \
	    -variable Options(EnableWhiteboard)
    $m add checkbutton \
	    -label "Auto Bookmark" \
	    -underline 5 \
	    -variable Options(AutoBookmark)
    $m add checkbutton \
	    -label "Auto-Init Babelfish" \
	    -variable Options(UseBabelfish)

    $m add separator

    $m add command \
	    -label "Colors ..." \
	    -underline 0 \
	    -command ::tkchat::ChangeColors
    $m add command \
	    -label "Macros ..." \
	    -underline 0 \
	    -command ::tkchat::EditMacros
    if {[llength [package provide choosefont]] != 0} {
        $m add command \
	    -label "Font ..." \
	    -underline 0 \
	    -command ::tkchat::ChooseFont
    }
    $m add command \
	    -label "User details ..." \
	    -underline 0 \
	    -command ::tkchat::UserInfoDialog
    if {[package provide khim] ne {}} {
	$m add command \
	    -label "Input method..." \
	    -underline 0 \
	    -command {::khim::getOptions .khim}
    }
    $m add command \
	    -label "Options ..." \
	    -underline 0 \
	    -command ::tkchat::EditOptions

    $m add separator

    # Tile Themes Cascade Menu
    if { $useTile } {
        set themes [lsort [ttk::themes]]

	menu $m.themes -tearoff 0
	$m add cascade \
		-label "Tk themes" \
		-menu $m.themes
	foreach theme $themes {
	    $m.themes add radiobutton \
		    -label [string totitle $theme] \
		    -variable Options(Theme) \
		    -value $theme \
		    -command [list ::tkchat::SetTheme $theme]
	}
	$m add separator
    }

    # Local Chat Logging Cascade Menu
    menu $m.chatLog -tearoff 0
    $m add cascade \
	    -label "Local Chat Logging" \
	    -underline 0 \
	    -menu $m.chatLog
    $m.chatLog add radiobutton \
	    -label Disabled \
	    -underline 0 \
	    -variable Options(ChatLogOff) \
	    -value 1 \
	    -command { ::tkchat::OpenChatLog close }
    $m.chatLog add command \
	    -label "To File..." \
	    -underline 0 \
	    -command { ::tkchat::OpenChatLog open }
    $m.chatLog add command \
	    -label "Load File..." \
	    -underline 0 \
	    -command { ::tkchat::OpenChatLog load }

    # Server Chat Logging Cascade Menu
    menu $m.chatServLog -tearoff 0
    $m add cascade \
	    -label "Server Chat Logging" \
	    -underline 0 \
	    -menu $m.chatServLog
    $m.chatServLog add radiobutton \
	    -label "Log my messages, do not log my actions (old style)" \
	    -underline 1 \
	    -variable Options(ServerLogging) \
	    -value oldStyle
    $m.chatServLog add radiobutton \
	    -label "Log my messages and actions" \
	    -underline 0 \
	    -variable Options(ServerLogging) \
	    -value all
    $m.chatServLog add radiobutton \
	    -label "Do not log my messages and actions" \
	    -underline 3 \
	    -variable Options(ServerLogging) \
	    -value none

    # Loading Server History Cascade Menu
    menu $m.hist -tearoff 0
    $m add cascade \
	    -label "Loading Server History" \
	    -underline 15 \
	    -menu $m.hist
    $m.hist add radiobutton \
	    -label "Do NOT load any history" \
	    -underline 3 \
	    -variable Options(HistoryLines) \
	    -value 0
    $m.hist add radiobutton \
	    -label "Ask me which logs to load" \
	    -underline 0 \
	    -variable Options(HistoryLines) \
	    -value -1
    foreach l { 50 100 200 500 1000 2500 10000 } {
	$m.hist add radiobutton \
		-label "Load at least $l lines" \
		-variable Options(HistoryLines) \
		-value $l
    }

    # One to One chats Cascade Menu
    menu $m.chat1to1 -tearoff 0
    $m add cascade \
	    -label "One to One chats" \
	    -menu $m.chat1to1
    $m.chat1to1 add radiobutton \
	    -label "Keep all chat in one window" \
	    -underline 0 \
	    -variable Options(OneToOne) \
	    -value inline
    $m.chat1to1 add radiobutton \
	    -label "Popup a new window" \
	    -underline 0 \
	    -variable Options(OneToOne) \
	    -value popup
    $m.chat1to1 add radiobutton \
	    -label "Open in new tab" \
	    -underline 12 \
	    -variable Options(OneToOne) \
	    -value tabbed

    # Auto Away Cascade Menu
    menu $m.aa -tearoff 0
    $m add cascade \
	    -label "Auto Away" \
	    -underline 0 \
	    -menu $m.aa \
	    -state [expr {[idle::supported] ? "normal" : "disabled"}]
    $m.aa add radiobutton \
	    -label "Disabled" \
	    -variable Options(AutoAway) \
	    -value -1
    foreach minutes { 5 10 15 20 30 45 60 90 } {
	$m.aa add radiobutton \
		-label "After $minutes minutes" \
		-variable Options(AutoAway) \
		-value $minutes \
		-command ::tkjabber::autoStatus
    }

    ## Emoticon Menu
    ##
    set m .mbar.emot
    $m add command \
	    -label "Show Emoticons" \
	    -underline 0 \
	    -command ::tkchat::ShowSmiles
    $m add checkbutton \
	    -label "Use Emoticons" \
	    -underline 0 \
	    -variable Options(Emoticons) \
	    -onvalue 1 \
	    -offvalue 0
    $m add checkbutton \
	    -label "Animate Emoticons" \
	    -underline 0 \
	    -variable Options(AnimEmoticons) \
	    -onvalue 1 \
	    -offvalue 0 \
	    -command ::tkchat::DoAnim

    # Insert Cascade Menu
    menu $m.mnu -tearoff 0 -title Insert
    $m add cascade \
	    -label Insert \
	    -underline 0 \
	    -menu $m.mnu
    variable IMG
    foreach { img txt } [array get IMG] {
	set tmp($txt) $img
    }
    foreach { img txt } [array get tmp] {
	$m.mnu add command \
		-image ::tkchat::img::$img \
		-command [string map [list %txt% $txt] {
		    .eMsg insert insert "%txt% "
		    .tMsg insert insert "%txt% "
		}]
    }

    ## Visibility Menu
    ##
    set m .mbar.vis
    foreach tag $Options(ElideTags) text {
	"Single Dot"
	"Online/Away Status"
	"Logon/Logoff"
	"All System"
	"Error"
    } {
	$m add checkbutton \
		-label "Hide $text Messages" \
		-underline 5 \
		-variable Options(Visibility,$tag) \
		-onvalue 1 \
		-offvalue 0 \
		-command [list ::tkchat::DoVis $tag]
    }
    $m add checkbutton \
	    -label "Hide Timestamps" \
	    -underline 5 \
	    -variable Options(Visibility,STAMP) \
	    -onvalue 1 \
	    -offvalue 0 \
	    -command ::tkchat::StampVis

    $m add checkbutton \
        -label "Hide Statusbar" \
        -underline 11 \
        -variable Options(Visibility,STATUSBAR) \
        -onvalue 0 \
        -offvalue 1 \
        -command [namespace origin ToggleStatusbar]

    $m add separator
    $m add command \
	    -label "Hide All Users" \
	    -command { ::tkchat::NickVis 1 }
    $m add command \
	    -label "Show All Users" \
	    -command { ::tkchat::NickVis 0 }

    # Hide Users Cascade Menu
    menu $m.nicks -tearoff 0
    $m add cascade \
	    -label "Hide Users" \
	    -menu $m.nicks
    NickVisMenu
    $m add separator
    $m add command \
	    -label "Show current history in new pane" \
	    -underline 5 \
	    -command {::tkchat::HistoryPaneToggle}

    ## Alert Menu
    ##
    set m .mbar.alert
    foreach { tag text } {
	ALL	"Alert when any message received"
	ME	"Alert when username mentioned"
	TOPIC	"Alert when someone speaks who was quiet"
    } {
	$m add checkbutton \
		-label "$text" \
		-variable Options(Alert,$tag) \
		-onvalue 1 \
		-offvalue 0 \
		-command [list ::tkchat::setAlert $tag]
    }
    $m add separator
    foreach { tag text } {
	SOUND	"Beep on alert"
	RAISE	"Raise to top on alert"
    } {
	$m add checkbutton \
		-label $text \
		-variable Options(Alert,$tag) \
		-onvalue 1 \
		-offvalue 0
    }
    $m add separator
    foreach {tag text} {
	NORMAL	"Alert on regular posts"
	ACTION	"Alert on whispers and \"/me\" posts"
    } {
	$m add checkbutton \
		-label "$text" \
		-variable Options(Alert,$tag) \
		-onvalue 1 \
		-offvalue 0
    }

    ## Debug Menu
    ##
    set m .mbar.dbg
    $m add command \
	    -label "Reload script" \
	    -underline 0 \
	    -command { ::tkchat::Debug reload }
    $m add command \
	    -label "Restart script" \
	    -underline 2 \
	    -command { ::tkchat::Debug restart }
    $m add command \
	    -label "Retrieve script" \
	    -underline 2 \
	    -command { ::tkchat::Debug retrieve }
    $m add command \
	    -label "Evaluate selection" \
	    -underline 1 \
	    -command { ::tkchat::Debug evalSel }
    $m add command \
	    -label "Allow remote control" \
	    -underline 0 \
	    -command { ::tkchat::Debug server }
    $m add command \
	    -label "Reload history" \
	    -underline 7 \
	    -command { ::tkchat::Debug purge }

    $m add separator

    # Error Logging Cascade Menu
    menu $m.err -tearoff 0
    menu $m.err.lvl -tearoff 0
    $m add cascade \
	    -label "Error Logging" \
	    -underline 0 \
	    -menu $m.err
    $m.err add cascade \
	    -label "Log Level" \
	    -underline 0 \
	    -menu $m.err.lvl
    $m.err add radiobutton \
	    -label "To Stderr" \
	    -underline 3 \
	    -variable Options(LogStderr) \
	    -value 1 \
	    -command { tkchat::OpenErrorLog stderr }
    $m.err add command \
	    -label "To File..." \
	    -underline 3 \
	    -command { tkchat::OpenErrorLog pick }

    # Error Logging:Log Level Cascade Menu
    foreach lvl [lsort -command ::log::lvCompare $::log::levels] {
	$m.err.lvl add radiobutton \
		-label $lvl \
		-variable Options(LogLevel) \
		-value $lvl
    }

    $m add separator
    $m add checkbutton \
	    -label "Console" \
	    -underline 0 \
	    -variable ::tkchat::_console \
	    -command { ::tkchat::Debug console } \
	    -state disabled
    set ::tkchat::_console 0
    if { [llength [info commands ::tkcon]] } {
	$m entryconfigure "Console" \
		-state normal \
		-command {
		    if { $::tkchat::_console } {
			tkcon show
		    } else {
			tkcon hide
		    }
		}
    } elseif { $::tcl_platform(platform) ne "unix" \
	    && [llength [info commands ::console]] > 0 } {
	$m entryconfigure "Console" -state normal
	console eval {
	    bind .console <Map> {
		consoleinterp eval {
		    set ::tkchat::_console 1
		}
	    }
	    bind .console <Unmap> {
		consoleinterp eval {
		    set ::tkchat::_console 0
		}
	    }
	}
    } elseif {[llength [info commands ::console]] == 0} {
	     ::tkchat::ConsoleInit
		  $m entryconfigure "Console" -state normal
		  console eval {
	    bind .console <Map> {
		consoleinterp eval {
		    set ::tkchat::_console 1
		}
	    }
	    bind .console <Unmap> {
		consoleinterp eval {
		    set ::tkchat::_console 0
		}
	    }
	}
    }

    ## Help Menu
    ##
    set m .mbar.help
    $m add command \
	    -label "Quick Help..." \
	    -underline 0 \
	    -command ::tkchat::Help
    $m add command \
	    -label "Help (wiki)..." \
	    -underline 0 \
	    -command { ::tkchat::gotoURL http://wiki.tcl.tk/tkchat }
    $m add separator
    $m add cascade \
	    -label "Translate Selection" \
	    -underline 0 \
	    -command ::tkchat::babelfishMenu
    $m add separator
    $m add command \
	    -label "About..." \
	    -underline 0 \
	    -command ::tkchat::About

    # a pane for the main display (chat window and users window):
    if {$useTile} {
        if {[llength [info commands ::ttk::panedwindow]] != 0} {
            ::ttk::panedwindow .pane -orient horizontal
        } else {
            ::ttk::paned .pane -orient horizontal
        }
    } else {
	panedwindow .pane -sashpad 4 -sashrelief ridge
    }
    # another pane dividing the chat window:
    if {$useTile} {
        if {[llength [info commands ::ttk::panedwindow]] != 0} {
            ::ttk::panedwindow .pane2 -orient vertical
        } else {
            ::ttk::paned .pane2 -orient vertical
        }
    } else {
	panedwindow .pane2 -sashpad 4 -sashrelief ridge -orient vertical
    }
    
    ${NS}::frame .txtframe

    CreateTxtAndSbar

    bind .txt <Button-3> { ::tkchat::OnTextPopup %W %x %y }
    if {[llength [package provide askleo]] > 0} {
        bind .txt <Shift-Button-3> { ::dict.leo.org::askLEOforSelection }
    }

    # user display
    ScrolledWidget text .pane.names 0 1\
	    -background "#[getColor MainBG]" \
	    -foreground "#[getColor MainFG]" \
	    -relief sunken \
	    -borderwidth 2 \
	    -width 8 \
	    -height 1 \
	    -font FNT \
	    -wrap word \
	    -cursor left_ptr \
	    -state disabled
    .pane.names tag configure STAMP

    applyColors .txt All

    # bottom frame for entry
    ${NS}::frame .btm
    ${NS}::button .ml
    ${NS}::entry .eMsg
    .eMsg configure -foreground black
    .ml configure -text ">>" -width 0 -command ::tkchat::showExtra

    bind .eMsg <Return>		::tkchat::userPost
    bind .eMsg <KP_Enter>	::tkchat::userPost
    bind .eMsg <Key-Up>		::tkchat::entryUp
    bind .eMsg <Key-Down>	::tkchat::entryDown
    bind .eMsg <Key-Tab>	{ ::tkchat::nickComplete ; break }
    bind .eMsg <Key-Prior>	{ .txt yview scroll -1 pages }
    bind .eMsg <Key-Next>	{ .txt yview scroll  1 pages }

    text .tMsg -height 6 -font FNT
    bind .tMsg <Key-Tab>	{ ::tkchat::nickComplete ; break }

    ${NS}::button .post  -text "Post"  -command ::tkchat::userPost

    if {$useTile} {
	ttk::menubutton .mb \
		-menu .mb.mnu \
		-textvariable Options(MsgTo) \
		-direction above
    } else {
	tk::menubutton .mb \
		-indicatoron 1 \
		-pady 4 \
		-menu .mb.mnu \
		-textvar Options(MsgTo) \
		-direction above
    }
    menu .mb.mnu -tearoff 0
    .mb.mnu add command \
	    -label "All Users" \
	    -command { ::tkchat::MsgTo "All Users" }

    .pane.names tag configure NICK -font NAME
    .pane.names tag configure TITLE -font NAME
    .pane.names tag configure SUBTITLE -font SYS
    .pane.names tag configure URL -underline 1
    .pane.names tag bind URL <Enter> { .pane.names configure -cursor hand2 }
    .pane.names tag bind URL <Leave> { .pane.names configure -cursor {} }

    bind .pane.names <Double-Button-1> break
    bind . <FocusIn> \
	[list after 500 [list after idle ::tkchat::ResetMessageCounter]]
    if { [lsearch [wm attributes .] -alpha] != -1 } {
	bind Tkchat <FocusIn>  { ::tkchat::FocusInHandler %W }
	bind Tkchat <FocusOut> { ::tkchat::FocusOutHandler %W }
    }

    # using explicit rows for restart
    set Options(NamesWin) .pane.names
    .txt configure -width 10
    .pane.names configure -width 10
    grid .txt .sbar -in .txtframe -sticky news
    grid columnconfigure .txtframe 0 -weight 1
    grid rowconfigure .txtframe 0 -weight 1
    
    .pane2 add .txtframe
    
    # text widget to view history:
    # FIX ME: be nice to have a little theme-specific tab close button here.
    # FIX ME: this should use the text widget clone feature if available.
    variable useClosebutton
    ${NS}::frame .cframe -relief groove
    if {$useClosebutton} {
        ::ttk::button .cbtn -style TClosebutton
    } else {
        ${NS}::button .cbtn -text "Close history pane"
    }
    .cbtn configure -command ::tkchat::HistoryPaneToggle
    ScrolledWidget text .clone 0 1 \
	-wrap word -background #f0f0f0 -relief sunken -borderwidth 2 \
	-font FNT -cursor left_ptr -height 1
    if {[llength [package provide askleo]] > 0} {
        bind .clone <Shift-Button-3> {::dict.leo.org::askLEOforSelection}
    }
    .clone tag bind URL <Enter> [list .clone configure -cursor hand2]
    .clone tag bind URL <Leave> [list .clone configure -cursor left_ptr]
    pack .clone -in .cframe -side bottom -expand 1 -fill both
    pack .cbtn -in .cframe -side top -anchor ne -padx 4 -pady 2
    
    .pane add .pane2
    if {$useTile} {
	.pane add $Options(NamesWin)
    } else {
	.pane add $Options(NamesWin) -sticky news
    }
    set lower_row [list .ml .eMsg .post .mb]
    if {!$useTile && [tk windowingsystem] eq "aqua"} {
        lappend lower_row [frame .spacer -width 16]
    }
    grid .pane - -sticky news -padx 1 -pady 2
    grid .btm  - -sticky news
    eval grid $lower_row [list -in .btm -sticky ews -padx 2 -pady 2]
    grid configure .eMsg -sticky ew

    if {$useTile} {
        variable Status
        array set Status {0 Ready. 1 "not connected" SSL "  "}
        set st [ttk::frame .status]
        for {set pn 0} {$pn < 2} {incr pn} {
            ttk::label $st.pane$pn -anchor w \
                -textvariable [namespace current]::Status($pn)
            ttk::separator $st.sep$pn -orient vertical
        }
        ttk::progressbar $st.progress
        $st.pane1 configure -image ::tkchat::img::link_disconnected
        ttk::label $st.ssl -anchor w -compound right \
            -image ::tkchat::img::link_insecure \
            -textvariable [namespace current]::Status(SSL)
        if {[llength [info commands ::ttk::sizegrip]] > 0} {
            ttk::sizegrip $st.sg
        } else {
            ttk::frame $st.sg -width 16
        }
        grid $st.pane0 $st.sep0 $st.pane1 $st.sep1 $st.ssl \
            $st.sg -sticky news
        grid columnconfigure $st 0 -weight 1
        grid .status -sticky ew
    }

    grid rowconfigure	 . 0 -weight 1
    grid columnconfigure . 0 -weight 1
    grid columnconfigure .btm 1 -weight 1
    
    if { $::tcl_platform(os) eq "Windows CE" } {
	wm geometry . 240x300+0+0
    } else {
	wm geometry . $Options(Geometry)
    }
    wm deiconify .

    update
    if {[info exists $Options(Pane)] && [llength $Options(Pane)] == 2 } {
	if {$useTile} {
		eval [linsert $Options(Pane) 0 .pane sashpos 0]
	} else {
		eval [linsert $Options(Pane) 0 .pane sash place 0]
	}
    } else {
	set w [expr { ([winfo width .pane] * 4) / 5 }]
	if {$useTile} {
		set coord [.pane sashpos 0]
		.pane sashpos 0 $w
	} else {
		set coord [.pane sash coord 0]
		.pane sash place 0 $w [lindex $coord 1]
	}
    }
    if {$useTile} {
	 	set Options(PaneUsersWidth) [expr { [winfo width .pane] \
	 	   - [.pane sashpos 0]}]
	 } else {
	 	set Options(PaneUsersWidth) [expr { [winfo width .pane] \
	 	   - [lindex [.pane sash coord 0] 0] }]
    }
	 bind .pane <Configure> { after idle [list ::tkchat::PaneConfigure %W %w] }
    bind .pane <Leave>     { ::tkchat::PaneLeave %W }

    # update the pane immediately.
    PaneConfigure .pane [winfo width .pane]

    # call this to activate the option on whether the users should be shown
    MsgTo "All Users"
    displayUsers
}

proc ::tkchat::ToggleStatusbar {} {
    global Options
    if {[winfo exists .status]} {
        if {$Options(Visibility,STATUSBAR) && ![winfo ismapped .status]} {
            grid .status -sticky ew
        }
        if {!$Options(Visibility,STATUSBAR) && [winfo ismapped .status]} {
            grid forget .status
        }
    }
}

proc ::tkchat::OnTextPopup { w x y } {
    $w mark set AddBookmark "@$x,$y linestart"

    set m .txt_popup
    catch { destroy $m }
    menu $m -tearoff 0

    if { $w eq ".txt" } {
	set nick [lsearch -inline [$w tag names @$x,$y] NICK-*]
	if { $nick ne "" } {
	    $m add command \
		    -label "Hide user" \
		    -command [list ::tkchat::OnNameToggleVis $nick]
	    $m add separator
	}
	$m add command \
		-label "Set/Unset Bookmark" \
		-accelerator Ctrl-F2 \
		-command ::tkchat::BookmarkToggle
	$m add command \
		-label "Prev Bookmark" \
		-accelerator Shift-F2 \
		-command ::tkchat::BookmarkPrev
	$m add command \
		-label "Next Bookmark" \
		-accelerator F2 \
		-command ::tkchat::BookmarkNext
	$m add command \
		-label "Clear Bookmarks" \
		-command ::tkchat::BookmarkClear
    }
    $m add command \
	    -label "Google Selection" \
	    -accelerator Ctrl-G \
	    -command [list ::tkchat::GoogleSelection $w]
    $m add command \
            -label "Open Paste Dialog" \
            -accelerator Ctrl-P \
            -command [list ::tkchat::PasteDlg]

    if { ![winfo exists .mbar.help.tr] } {
	$m add command -label "Translate Initialize" -command ::tkchat::babelfishMenu
    } else {
	.mbar.help.tr clone $m.tr
	$m add cascade -label "Translate Selection" -menu $m.tr
    }

    tk_popup $m [winfo pointerx $w] [winfo pointery $w]
}

proc ::tkchat::CreateTxtAndSbar { {parent ""} } {
    global Options
    variable NS

    set txt $parent.txt
    set sbar $parent.sbar

    text $txt \
	    -background "#[getColor MainBG]" \
	    -foreground "#[getColor MainFG]" \
	    -relief sunken \
	    -borderwidth 2 \
	    -width 8 \
	    -height 1 \
	    -font FNT \
	    -wrap word \
	    -cursor left_ptr \
	    -yscroll "::tkchat::scroll_set $sbar" \
	    -state disabled

    ${NS}::scrollbar $sbar -command "$txt yview"

    $txt tag configure MSG -lmargin2 50
    $txt tag configure INFO -lmargin2 50
    $txt tag configure NICK -font NAME
    $txt tag configure ACTION -font ACT
    $txt tag configure NOLOG -font NOLOG
    $txt tag configure AVAILABILITY -font SYS
    $txt tag configure SYSTEM -font SYS
    $txt tag configure TRAFFIC -font SYS
    $txt tag configure ERROR -background red
    $txt tag configure ENTERED -foreground $Options(EntryMessageColor)
    $txt tag configure LEFT -foreground $Options(ExitMessageColor)
    $txt tag configure NICKCHANGE
    $txt tag configure URL -underline 1
    $txt tag configure STAMP -font STAMP -foreground "#[getColor MainFG]"
    $txt tag configure SINGLEDOT
    $txt tag configure BOOKMARK
    $txt tag bind URL <Enter> [list [namespace origin onEnterURL] %W %x %y]
    $txt tag bind URL <Leave> [list [namespace origin onLeaveURL] %W %x %y]

    # Adjust tag ordering for hidden text
    foreach tag $Options(ElideTags) {
	if { $Options(Visibility,$tag) } {
	    $txt tag raise $tag STAMP
	}
    }

    # on windows, a disabled text widget can't get focus
    # but someone might want to copy/paste the text
    bind $txt <Button-1> { focus %W }
    bind $txt <Up>	 [list $txt yview scroll -1 units]
    bind $txt <Down>	 [list $txt yview scroll  1 units]
    bind $txt <Button-4> [list $txt yview scroll -1 units]
    bind $txt <Button-5> [list $txt yview scroll  1 units]
}

proc ::tkchat::onEnterURL {w x y} {
    if {[winfo exists .status]} {
        set tags [$w tag names @$x,$y]
        if {[set ndx [lsearch -glob $tags URL-*]] != -1} {
            set url ""
            foreach {b e} [$w tag ranges [lindex $tags $ndx]] {
                append url [$w get $b $e]
            }
            if {[string length $url] > 0} {
                addStatus 0 $url
            }
        }
    }
    $w configure -cursor hand2
}
proc ::tkchat::onLeaveURL {w x y} {
    if {[winfo exists .status]} {
        addStatus 0 ""
    }
    $w configure -cursor {}
}

proc ::tkchat::SetChatWindowBindings { parent jid } {

    set post [list ::tkchat::userPostOneToOne $parent $jid]

    bind $parent.txt  <Button-3>  { ::tkchat::OnTextPopup %W %x %y }
    bind $parent.eMsg <Return>	  $post
    bind $parent.eMsg <KP_Enter>  $post
    $parent.post configure -command $post
    wm title $parent $::tkjabber::ChatWindows(title.$jid)
    wm protocol $parent WM_DELETE_WINDOW \
	    [list ::tkchat::DeleteChatWindow $parent $jid]
    bind $parent <FocusIn> \
	    [list wm title $parent $::tkjabber::ChatWindows(title.$jid)]
    applyColors $parent.txt $jid
}

proc ::tkchat::CreateNewChatWindow { parent } {
    global Options
    variable useTile
    variable NS

    if {$useTile} {
        if {[llength [info commands ::ttk::panedwindow]] != 0} {
            ::ttk::panedwindow $parent.pane -orient vertical
        } else {
            ::ttk::paned $parent.pane -orient vertical
        }
    } else {
	panedwindow $parent.pane -sashpad 4 -sashrelief ridge
    }
    ${NS}::frame $parent.txtframe

    CreateTxtAndSbar $parent

    # bottom frame for entry
    ${NS}::frame $parent.btm
    ${NS}::button $parent.ml \
	    -text ">>" \
	    -width 0 \
	    -command [list ::tkchat::showExtra $parent]
    ${NS}::entry $parent.eMsg
    bind $parent.eMsg <Key-Prior> [list $parent.txt yview scroll -1 pages]
    bind $parent.eMsg <Key-Next>  [list $parent.txt yview scroll  1 pages]
    text $parent.tMsg -height 6 -font FNT
    ${NS}::button $parent.post -text "Post"

    $parent.txt configure -width 10
    grid $parent.txt $parent.sbar \
	    -in $parent.txtframe -sticky news -padx 1 -pady 2
    grid columnconfigure $parent.txtframe 0 -weight 1
    grid rowconfigure $parent.txtframe 0 -weight 1
    if {$useTile} {
	 	$parent.pane add $parent.txtframe
	 } else {
	 	$parent.pane add $parent.txtframe -sticky news
	 }
    grid $parent.pane -sticky news -padx 1 -pady 2
    grid $parent.btm  -sticky news
    grid $parent.ml $parent.eMsg $parent.post \
	    -in $parent.btm -sticky ews -padx 2 -pady 2
    grid configure $parent.eMsg -sticky ew

    grid rowconfigure	 $parent 0 -weight 1
    grid columnconfigure $parent 0 -weight 1
    grid columnconfigure $parent.btm 1 -weight 1
    wm geometry $parent 450x350
    return $parent.txt
}

proc ::tkchat::DeleteChatWindow { w jid } {
    ::tkjabber::deleteChatWidget $jid
    destroy $w
}

# FIXME: Work in progress for notebook style tabbed windows?
proc ::tkchat::CreateNewChatTab { parent title } {
    set w [CreateNewChatWindow $parent]
    SetChatWindowBindings $parent $tit
    return $w
}

proc ::tkchat::SetTheme {theme} {
    global Options
    variable useTile
    catch {
	if {$useTile} {
            ttk::setTheme $theme
	}
	set Options(Theme) $theme
    }
}

# On window resizing, we need to adjust the sash location to keep
# proportions the same for each pane.
proc ::tkchat::PaneConfigure {pane width} {
    global Options
    variable useTile
    if {$::Options(DisplayUsers)} {
	if {[info exists Options(PaneUsersWidth)]} {
	    set pos [expr {$width - $Options(PaneUsersWidth)}]
	    if {$useTile} {
		$pane sashpos 0 $pos
	    } else {
		$pane sash place 0 $pos 2
	    }
	}
    }
}

proc ::tkchat::PaneLeave {pane} {
    global Options
    variable useTile
    if {$::Options(DisplayUsers)} {
	if {$useTile} {
	    set Options(PaneUsersWidth) \
		[expr {[winfo width .pane] - [.pane sashpos 0]}]
	} else {
	    set Options(PaneUsersWidth) \
		[expr {[winfo width .pane] - [lindex [.pane sash coord 0] 0]}]
	}
    }
}

proc ::tkchat::DoVis { tag } {
    if { $::Options(Visibility,$tag) } {
	.txt tag raise $tag
    } else {
	.txt tag lower $tag STAMP
    }
    .txt tag configure $tag -elide $::Options(Visibility,$tag)
    if { $::Options(AutoScroll) } {
	.txt see end
    }
    if { $::tkchat::LoggedIn } {
	after 10 {::tkchat::updateOnlineNames}
    }
}

proc ::tkchat::NickVis { val } {
    global Options

    foreach nick [array names ::Options Visibility,NICK-*] {
	if { $Options($nick) != $val } {
	    set Options($nick) $val
	    DoVis [string range $nick 11 end]
	}
    }
}

proc ::tkchat::StampVis {} {
    global Options
    variable bookmark
    variable ::tkjabber::ChatWindows

    set textWindows .txt
    foreach w [array names ChatWindows txt.*] {
	lappend textWindows $ChatWindows($w)
    }
    foreach w $textWindows {
	$w tag configure STAMP -elide $Options(Visibility,STAMP)

	set width $Options(Offset)
	if { $bookmark(id) && $w eq ".txt" } {
	    incr width $bookmark(width)
	}
	if { $Options(Visibility,STAMP) } {
	    # Invisible
	    $w tag raise STAMP
	    set tabs $width
	} else {
	    # Stamps visible
	    foreach tag $Options(ElideTags) {
		if { $Options(Visibility,$tag) } {
		    $w tag raise $tag STAMP
		}
	    }
	    if { $w eq ".txt" } {
		foreach tag [array names Options Visibility,NICK-*] {
		    if { $Options($tag) } {
			$w tag raise [string range $tag 11 end] STAMP
		    }
		}
	    }
	    set width_tstamp [expr { [font measure NAME "\[88:88\]"] + 5 }]
	    incr width $width_tstamp
	    if { $bookmark(id) && $w eq ".txt" } {
		incr width_tstamp $bookmark(width)
	    }
	    set tabs [list $width_tstamp $width]
	}
	$w configure -tabs $tabs
	$w tag configure MSG -lmargin2 $width
	if { $Options(AutoScroll) } {
	    $w see end
	}
    }
}

proc ::tkchat::NickVisMenu {} {
    set m .mbar.vis.nicks
    $m delete 0 end
    set cnt 0
    foreach nick $::Options(NickList) {
	set nick [lindex $nick 0]
	set tag NICK-$nick
	$m add checkbutton \
		-label $nick \
		-variable Options(Visibility,$tag) \
		-onvalue 1 \
		-offvalue 0 \
		-command [list ::tkchat::DoVis $tag]
	if { $cnt > 0 && $cnt % 25 == 0 } {
	    $m entryconfigure end -columnbreak 1
	}
	incr cnt
    }
}

# create a standard widget with scrollbars around
# (uses tile if present)
#
# wigdet  -> name of the widget to be created
# parent  -> path to the frame, in which the widget and the scrollbars should
#            be created
# scrollx -> boolean; create horizontal scrollbar?
# scrolly -> boolean; create vertical scrollbar?
# args    -> additional arguments passed on the the widget
#
# returns: the path to the created widget (frame)
#
proc ::tkchat::ScrolledWidget {widget parent scrollx scrolly args} {
	variable useTile
	if {$useTile} {ttk::frame $parent} else {frame $parent}
	# Create widget attached to scrollbars, pass thru $args
	eval $widget $parent.list $args
	# Create scrollbars attached to the listbox
	if {$scrollx} {
		if {$useTile} {
			ttk::scrollbar $parent.sx -orient horizontal \
			-command [list $parent.list xview]
		} else {
			scrollbar $parent.sx -orient horizontal \
			-command [list $parent.list xview] -elementborderwidth 1
		}
		grid $parent.sx -column 0 -row 1 -sticky ew
		$parent.list configure -xscrollcommand [list $parent.sx set]
	}
	if {$scrolly} {
		if {$useTile} {
			ttk::scrollbar $parent.sy -orient vertical \
			-command [list $parent.list yview]
		} else {
			scrollbar $parent.sy -orient vertical \
			-command [list $parent.list yview] -elementborderwidth 1
		}
		grid $parent.sy 	-column 1 -row 0 -sticky ns
		$parent.list configure -yscrollcommand [list $parent.sy set]
	}
	# Arrange them in the parent frame
	grid $parent.list  -column 0 -row 0 -sticky ewsn
	grid columnconfigure $parent 0 -weight 1
	grid rowconfigure $parent 0 -weight 1
	# hide the original widget command from the interpreter:
	interp hide {} $parent
	# Install the alias:
	interp alias {} $parent {} ::tkchat::ScrolledWidgetCmd $parent.list
	# fix the bindings for the listbox:
	bindtags $parent.list [lreplace [bindtags $parent.list] 0 0 $parent]
	#set tags [lrange [bindtags $parent.list] 1 end]
	#bindtags $parent.list "$parent $tags"
	#
	return $parent
}
proc ::tkchat::ScrolledWidgetCmd {self cmd args} {
	switch -- $cmd {
		widgetPath {return "$self.list"}
		default {return [uplevel 1 [list $self $cmd] $args]}
	}
}

proc ::tkchat::About {} {
    global Options
    variable rcsid
    variable NS

    regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $rcsid -> rcsVersion

    # don't cache this window - if user reloads on the fly
    # we want to make sure it displays latest greatest info!
    catch {destroy .about}

    set dlg [toplevel .about -class Dialog]
    set w [${NS}::frame $dlg.f]
    wm withdraw $dlg
    wm transient $dlg .
    wm title $dlg "About TkChat $rcsVersion"
    set ver "Using Tcl/Tk [info patchlevel]"
    if {[llength [package provide tile]] != 0} { append ver ", tile [package provide tile]" }
    if {[llength [package provide tls]] != 0} { append ver ", tls [package provide tls]" }
    ${NS}::button $w.b -text Dismiss -command [list wm withdraw $dlg]
    ScrolledWidget text $w.text 0 1 -height 22 -width 80 \
        -borderwidth 0 -padx 2 -pady 2 -font FNT
    grid $w.text -sticky news
    grid $w.b -sticky se
    grid rowconfigure $w 0 -weight 1
    grid columnconfigure $w 0 -weight 1
    $w.text tag configure center -justify center
    $w.text tag configure title -justify center -font {Courier -18 bold}
    $w.text tag configure h1 -font {Sans -12 bold}
    $w.text insert 1.0 \
	"TkChat v$rcsVersion\n" title "$ver\n\n" {h1 center} \
	"Copyright (c) 2001-2005  Bruce B Hartweg <brhartweg@bigfoot.com>\n" \
		center \
	"$rcsid\n\n" center \
	"Additional contributions from:\n\n" {}

    lappend txt "Don Porter"		"<dgp@users.sourceforge.net>"
    lappend txt "Pat Thoyts"		"<patthoyts@users.sourceforge.net>"
    lappend txt "Jeff Hobbs"		"<jeffh@activestate.com>"
    lappend txt "Ryan Casey"		"<scfied@hotmail.com>"
    lappend txt "Reinhard Max"		"<max@tclers.tk>"
    lappend txt "D. Richard Hipp"	"<drh@hwaci.com>"
    lappend txt "Kevin Kenny"		"<kennykb@users.sourceforge.net>"
    lappend txt "Pascal Scheffers"	"<pascal@scheffers.net>"
    lappend txt "Joe English"		"<jenglish@users.sourceforge.net>"
    lappend txt "Joe Mistachkin"	"<joe@mistachkin.com>"
    lappend txt "Daniel South"		"<wildcard_25@users.sourceforge.net>"

    insertHelpText $w.text $txt

    grid $w -sticky news
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    $w.text configure -state disabled
    bind $dlg <Return> [list $w.b invoke]
    bind $dlg <Escape> [list $w.b invoke]
    catch {::tk::PlaceWindow $dlg widget .}
    wm deiconify $dlg
}

proc ::tkchat::Help {} {
    variable rcsid
    variable NS
    global Options
    regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $rcsid -> rcsVersion

    catch {destroy .qhelp}
    set w [toplevel .qhelp -class Dialog]
    wm withdraw $w
    wm transient $w .
    wm title $w "TkChat $rcsVersion Help"
    ${NS}::frame $w.f
    text $w.text -height 32 -bd 1 -width 100 -wrap word \
        -yscrollcommand [list $w.vs set]
    ${NS}::scrollbar $w.vs -command [list $w.text yview]
    ${NS}::button $w.b -text "Dismiss" -command [list wm withdraw $w]
    grid $w.text $w.vs -in $w.f -sticky news
    grid $w.b -        -in $w.f -sticky e
    grid rowconfigure $w.f 0 -weight 1
    grid columnconfigure $w.f 0 -weight 1
    grid $w.f -sticky news
    grid rowconfigure $w 0 -weight 1
    grid columnconfigure $w 0 -weight 1
    $w.text tag configure title -justify center -font {Courier -18 bold}
    $w.text tag configure h1 -justify left -font {Sans -12 bold}
    $w.text insert 1.0 "TkChat v$rcsVersion Help\n" title

    $w.text insert end "Commands\n" h1

    lappend txt "/msg <nick|JID> <text>"
    lappend txt [list \
	    "Send private message to a user identified by nickname or JID"]

    lappend txt "/chat <nick|JID> ?text?"
    lappend txt [list [concat \
	    "Open a separate window to privately chat with the user" \
	    "identified by nickname or JID, optionally posting an initial" \
	    "message"]]

    lappend txt "/userinfo <nick>"
    lappend txt [list "Display registered information for user <nick>"]

    lappend txt "/afk ?reason?"
    lappend txt [list "Set your status to away with an optional reason"]

    lappend txt "/back ?reason?"
    lappend txt [list "Indicate that you have returned"]

    lappend txt "/away ?reason?"
    lappend txt [list "Synonym for /afk"]

    lappend txt "/google <text>"
    lappend txt [list "Open a google query for <text> in web browser"]

    lappend txt "/googlefight <word> <word>"
    lappend txt [list \
	    "Perform a google fight between two words or phrases (in quotes)"]

    lappend txt "/tip:<NUM>"
    lappend txt [list "Open the specified TIP document in web browser"]

    lappend txt "/wiki <text>"
    lappend txt [list "Do a Tclers wiki query with the remainder of the line"]

    lappend txt "/wikipedia <text>"
    lappend txt [list "Send a query to wikipedia (abbr. /wikip <text>)"]
    
    lappend txt "/wiktionary <text>"
    lappend txt [list "Send a query to wikipedia dictionary (abbr. /wikid <text>)"]
    
    lappend txt "/bug ?group? ?tracker? id"
    lappend txt [list "Open a sourceforge tracker item in browser"]

    lappend txt "/noisy ?<nick>? ?<minutes>?"
    lappend txt [list [concat \
	    "Toggle <nick> noisy for x minutes (default 5). Messages from" \
	    "noisy users are not diplayed. Not specifying a nick will give" \
	    "you a list of noisy users."]]

    lappend txt "/see <mark>"
    lappend txt [list "Goto named mark or index (eg: bookmark1 end 0.0)"]

    lappend txt "/alias <name> <type> <body>"
    lappend txt [list [concat \
	    "<type> is 'proc' or 'script', type proc takes exactly one" \
	    "argument."] \
	    "e.g: /alias foo script addSystem .txt \"test!\"" \
	    "/alias foo proc thisProc" \
	    "proc thisProc { arguments } { addSystem .txt \$arguments }"]

    lappend txt "/unalias <pattern>"
    lappend txt [list \
	    "Removes one or more aliases." \
	    "e.g: /unalias f*"]

    insertHelpText $w.text $txt

    $w.text insert end "Administrative commands\n" h1
    set txt ""
    lappend txt "/kick nick ?reason?" [list "Remove an undesirable user"]
    lappend txt "/mute nick ?reason?" [list "Globally silence a user"]
    lappend txt "/unmute nick ?reason?" [list "Unmute a muted user"]
    lappend txt "/op nick ?reason?" [list "Make user an administrator"]
    lappend txt "/deop nick ?reason?" [list "Remove admin privileges from user"]
    insertHelpText $w.text $txt
    

    set txt ""
    $w.text insert end "Searching\n" h1

    lappend txt "/?<text>"
    lappend txt [list [concat \
	    "Search the chat buffer for matching text. Repeating the command" \
	    "will progress to the previous match"]]

    lappend txt "/!"
    lappend txt [list "Clear the previous search result"]

    insertHelpText $w.text $txt

    $w.text configure -state disabled
    catch {::tk::PlaceWindow $w widget .}
    wm deiconify $w
}

proc ::tkchat::insertHelpText { w txt } {
    set tabOffset [$w cget -tabs]
    foreach { cmd usage } $txt {
	set cmdWidth [expr { [font measure [$w cget -font] $cmd] + 10 }]
	if { $cmdWidth > $tabOffset } {
	    set tabOffset $cmdWidth
	    $w configure -tabs $cmdWidth
	    $w tag configure USAGE -lmargin2 $cmdWidth
	}
	$w insert end $cmd
	foreach line $usage {
	    $w insert end \t$line\n USAGE
	}
    }
}

proc ::tkchat::parseString { variable_name string separators maximum } {
    # added by JJM 25/Sep/2003
    #
    # this routine makes parsing easier WHILE preserving
    # the "exactness" of the string by NOT treating it as a list...
    # parse string without using list commands... for targeted eval, etc
    #
    # get ahold of an array to put results into
    upvar 1 $variable_name local_array

    # get a list of separators...
    set separator_list [split $separators ""]

    # get length in characters
    set count [string length $string]

    # start at first index (maybe make this variable later?)
    set index 0

    # always start counting in result array from 1 (should this really be
    # zero?)
    set found_index 1

    # how many "matches" did we find?
    # NOTE: this will NOT be more than the parameter maximum, if specified
    set found_count 0

    # current string that needs to be added when next separator is found...
    set found_string ""

    #
    # keep going until the end of the string is reached
    #
    while {$index < $count} {
	#
	# go through string on a character-by-character basis
	#
	set character [string index $string $index]
	#
	# if the character is in the separator list,
	# then we need to add to the array...
	#
	if {[lsearch -exact $separator_list $character] != -1} then {
	    if {$maximum > 0} then {
		# we are limiting the number of "matches" to a certain amount
		# to allow for rather flexible argument parsing for callers...
		# (they can treat the first X arguments as separate, and the
		# rest as one long argument) #
		if {$found_count == ($maximum - 1)} then {
		    # stop adding new after X matches... (last one is taken
		    # care of after loop)
		    set do_add 0
		} else {
		    # we haven't reached the maximum yet
		    set do_add 1
		}
	    } else {
		# there is no maximum
		set do_add 1
	    }
	} else {
	    # we didn't find a separator yet
	    set do_add 0
	}

	if {$do_add != 0} then {
	    #
	    # add string to found array...
	    #
	    set local_array($found_index) $found_string
	    # next index in result array
	    set found_index [expr {$found_index + 1}]
	    # increase count of found arguments
	    set found_count [expr {$found_count + 1}]
	    # reset current string
	    set found_string ""
	} else {
	    #
	    # otherwise, just keep appending to current string
	    #
	    if {$found_string != ""} then {
		# tack on the current character (this is not a separator)
		append found_string $character
	    } else {
		# since no other characters in the current string yet, just set
		# it
		set found_string $character
	    }
	}

	incr index
    }

    #
    # don't forget last one... in case there is one...
    # (this should always happen if the string doesn't end in space...)
    #
    if {$found_string != ""} then {
	# add FINAL string to found array...
	set local_array($found_index) $found_string
	# next index in result array
	set found_index [expr {$found_index + 1}]
	# increase count to FINAL count of found arguments
	set found_count [expr {$found_count + 1}]
	# reset current string
	set found_string ""
    }

    #
    # pass back count always, even if no matches...
    #
    set local_array(count) $found_count

    if {$found_count > 0} then {
	# if we found anything, return non-zero
	set result 1
    } else {
	# otherwise return zero
	set result 0
    }

    return $result
}

proc ::tkchat::processAliasCommand { msg } {
    # added by JJM 25/Sep/2003
    # quickly gimme a list of arguments...
    set msg_list [split $msg " "]

    # extract just the command name...
    set command_name [string range [lindex $msg_list 0] 1 end]

    # process the command...
    switch -exact $command_name {
	"alias" {
	    array set msg_array {}
	    # did we succeed in parsing into the array?
	    if {[parseString msg_array $msg " " 4]} then {
		# did we get exactly 4 arguments?
		if {$msg_array(count) == 4} then {
		    # skip over "/alias" in array...
		    set result [addAlias $msg_array(2) $msg_array(3) $msg_array(4)]
		} else {
		    if {$msg_array(count) == 1} then {
			set result [listAliases]
		    } else {
			addStatus 0 "wrong # args: must be /alias name type body" end ERROR
			set result 0
		    }
		}
	    }
	}
	"unalias" {
	    array set msg_array {}
	    # did we succeed in parsing into the array?
	    if {[parseString msg_array $msg " " 2]} then {
		# did we get exactly 2 arguments?
		if {$msg_array(count) == 2} then {
		    # skip over "/unalias" in array...
		    set result [removeAliases $msg_array(2)]
		} else {
		    addStatus 0 "wrong # args: must be /unalias name" end ERROR
		    set result 0
		}
	    }
	}
	default {
	    addStatus 0 "unknown alias processing directive" end ERROR
	    set result 0
	}
    }

    return $result
}

proc ::tkchat::addAlias {name type body } {
    # added by JJM 25/Sep/2003
    variable commandAliases

    set index [findAlias $name]

    if {$index != -1} then {
	# replace existing alias...
	set commandAliases(types) [lreplace $commandAliases(types) $index $index $type]
	set commandAliases(bodies) [lreplace $commandAliases(bodies) $index $index $body]

	# show that we modified it.
	addStatus 0 "alias \"$name\" modified"
    } else {
	# add new alias...
	lappend commandAliases(names) $name
	lappend commandAliases(types) $type
	lappend commandAliases(bodies) $body

	# show that we added it.
	addStatus 0 "alias \"$name\" added"
    }

    # we always either add or replace, so return success.
    return 1
}

proc ::tkchat::removeAliases { name } {
    # added by JJM 25/Sep/2003
    variable commandAliases

    set result 0; # we haven't removed any yet.
    for {set index [expr {[llength $commandAliases(names)] - 1}]} {$index >= 0} {incr index -1} {
	set alias [lindex $commandAliases(names) $index]

	if {[string match $name $alias]} then {
	    # remove matching command alias...
	    set commandAliases(names) [lreplace $commandAliases(names) $index $index]
	    set commandAliases(types) [lreplace $commandAliases(types) $index $index]
	    set commandAliases(bodies) [lreplace $commandAliases(bodies) $index $index]

	    # show that we removed it.
	    addStatus 0 "alias \"$alias\" matching \"$name\" removed"

	    set result 1; # yes, we matched at least one.
	}
    }

    return $result
}

proc ::tkchat::listAliases {} {
    # added by JJM 25/Sep/2003
    variable commandAliases

    addStatus 0 "there are [llength $commandAliases(names)] aliases defined"

    for {set index 0} {$index < [llength $commandAliases(names)]} {incr index} {
	set name [lindex $commandAliases(names) $index]
	set type [lindex $commandAliases(types) $index]
	set body [lindex $commandAliases(bodies) $index]

	if { $type eq "proc" } then {
	    # show the whole thing, it's just a proc name.
	    set str $body
	} else {
	    # only show first 80 characters of the script.
	    set str [string range $body 0 79]
	}

	addSystem .txt "alias $name ($type) = \{$str\}"
    }

    # we always list all aliases...
    return 1
}

proc ::tkchat::findAlias { name } {
    # added by JJM 25/Sep/2003
    variable commandAliases
    # find the alias by name...
    return [lsearch -exact $commandAliases(names) $name]
}

proc ::tkchat::checkAlias { msg } {
    # added by JJM 25/Sep/2003
    variable commandAliases

    set msg_list [split $msg " "]
    set command_name [string range [lindex $msg_list 0] 1 end]

    # try to find the command alias...
    set index [findAlias $command_name]

    if {$index != -1} then {
	# get alias type and body.
	set command_type [lindex $commandAliases(types) $index]
	set command_body [lindex $commandAliases(bodies) $index]

	# set initial error info (none).
	set error 0
	set alias_error ""

	switch -exact $command_type {
	    "proc"  {
		set result 0; # default to "not handled". this MAY be changed by the [catch] below.

		array set msg_array {}
		# did we succeed in parsing into the array?
		if {[parseString msg_array $msg " " 2]} then {
		    # are there no arguments?
		    if {$msg_array(count) == 1} then {
			set msg_array(2) ""
			incr msg_array(count)
		    }

		    # did we get exactly 2 arguments?
		    if {$msg_array(count) == 2} then {
			#
			# NOTE: This proc should return zero to indicate
			# "not handled" and non-zero to indicate "handled".
			#
			set error [catch {set result [expr {[namespace eval [namespace \
										 current] [list $command_body $msg_array(2)]] != 0}]} alias_error]
		    } else {
			addSystem .txt "did not get exactly 2 arguments for alias \"$command_name\" ($command_type)" end ERROR
		    }
		} else {
		    addSystem .txt "could not parse arguments for alias \"$command_name\" ($command_type)" end ERROR
		}
	    }
	    "script" -
	    default  {
		# attempt to eval the command body in this namespace...
		set error [catch {namespace eval [namespace current] $command_body} \
			       alias_error]
		#
		# NOTE: If there is an error, we consider that to be "not handled".
		#
		set result [expr {!$error}]
	    }
	}

	# check for and show errors...
	if {$error} then {
	    addStatus 0 "alias \"$command_name\" ($command_type) error: $alias_error"
	}
    } else {
	set result 0
    }

    return $result
}

proc ::tkchat::userPostOneToOne {p jid} {
    if {[winfo ismapped $p.eMsg]} {
	set str [$p.eMsg get]
    } else {
	set str [$p.tMsg get 1.0 end]
    }
    set msg [string trim $str]

    tkjabber::msgSend $msg -tojid $jid -type chat
    if { [string match "/me *" $msg] } {
	set msg [string range $msg 4 end]
	set msgtype ACTION
    } else {
	set msgtype NORMAL
    }
    addMessage $p.txt "" $::Options(Nickname) $msg $msgtype end 0
    $p.eMsg delete 0 end
    $p.tMsg delete 1.0 end
}

proc ::tkchat::userPost {{jid ""}} {
    global Options

    if {[winfo ismapped .eMsg]} {
	set str [.eMsg get]
    } else {
	set str [.tMsg get 1.0 end]
    }
    set msg [string trim $str]

    switch -glob -- $msg {
	"" {
	    # skip
	}
	"/*" {
	    # possible command
	    checkCommand $msg
	}
	default {
	    # check for user defined macro
	    set words [regexp -all -inline -- {\S+} $msg]
	    set macro [lindex $words 0]
	    if {[info exists Options(Macro,$macro)]} {
		# invoke macro instead of raw string
		# build subst map - build it from higher number
		# down so that %10 matches before %1
		set i [llength $words]
		set map [list %% %]
		while {$i >0} {
		    incr i -1
		    lappend map %$i@ [join [lrange $words $i end]]
		    lappend map %$i [lindex $words $i]
		}
		set msg [string map $map $Options(Macro,$macro)]
	    }
	    if { $Options(MsgTo) eq "All Users" } {
		switch $Options(ServerLogging) {
		    none {
			tkjabber::msgSend "/nolog $msg" -attrs [list nolog 1]
		    }
		    default {
			tkjabber::msgSend $msg
		    }
		}
	    } else {
		::tkjabber::msgSend $msg -user $Options(MsgTo)
	    }
	}
    }
    .eMsg delete 0 end
    .tMsg delete 1.0 end

    if { $msg ne "" } {
	# add it to a recent history list
	upvar #0 ::tkchat::eHIST hist ::tkchat::eCURR cur
	if {[info exists hist] && [string compare $msg [lindex $hist end]]} {
	    # append new different msg, but constrain to max of 50 last msgs
	    set hist [lrange [lappend hist $msg] end-50 end]
	    # set current event to last
	    set cur [llength $hist]
	} elseif { [info exists hist] } {
	    set cur [llength $hist]
	}
    }
}

proc ::tkchat::checkCommand { msg } {
    global Options

    # check against commands that can be used while logged off
    set moreToGo 0
    switch -re -- $msg {
	{^/smiley?s?$} {
	    ShowSmiles
	}
	{^/colou?rs?$} {
	    ChangeColors
	}
	{^/font } {
	    set name [string trim [string range $msg 5 end]]
	    catch {ChangeFont -family $name}
	}
	{^/(font)?size [0-9]+} {
	    regexp -- {[0-9]+} $msg size
	    catch {ChangeFont -size $size}
	}
	{^/macros?$} {
	    EditMacros
	}
	{^/\?} {
	    doSearch $msg
	}
	{^/!} {
	    resetSearch
	}
	{^/(urn:)?tip[: ]\d+} {
	    if {[regexp {(?:urn:)?tip[: ](\d+)} $msg -> tip]} {
		gotoURL http://tip.tcl.tk/$tip
	    }
	}
	{^/bug[: ]} {
	    doBug [split $msg ": "]
	}
	{^/wiki[:\s]} {
	    set q [http::formatQuery [string range $msg 6 end]]
	    gotoURL http://wiki.tcl.tk/$q
	}
	{^/help} {
	    gotoURL http://wiki.tcl.tk/tkchat
	}
	{^/google\s} {
	    set msg [string range $msg 8 end]
	    ::log::log debug "Google query \"$msg\""
	    if {[string length $msg] > 0} {
		set    q {http://www.google.com/search}
		append q {?hl=en&ie=UTF-8&oe=UTF-8&btnG=Google+Search}
		append q "&q=$msg"
		gotoURL $q
	    }
	}
	{^/see\s} {
	    .txt see [lindex $msg 1]
	}
	{^/alias\s?}   -
	{^/unalias\s?} {
	    processAliasCommand $msg
	}
	{^/noisy\s?} {
	    noisyUser $msg
	}
	{^/googlefight\s?} {
	    set q {http://www.googlefight.com/index.php?lang=}
	    if {[string match fr_* [msgcat::mclocale]]} {
		append q fr_FR
	    } else {
		append q en_GB
	    }
	    set n 0
	    foreach word [lrange $msg 1 end] {
		append q "&word[incr n]=$word"
	    }
	    gotoURL $q
	}
        {^/eval\s} {
            set script [string range $msg 6 end]
            set r [catch $script err]
            if {$r} {
                tk_messageBox -icon error -title "Error in eval'd script" \
                    -message $::errorInfo
            } elseif {[string length $err] > 0} {
                addSystem .txt $err
            }
        }
        {^/wik(?:id)|(?:tionary)[:\s]} {
            regexp {^/wik(?:id)|(?:tionary)[:\s](.*)} $msg -> query
	    set q [http::formatQuery search $query]
	    gotoURL http://en.wiktionary.org/wiki/Special:Search?$q&go=Go
	}
	{^/wikip(?:edia)?[:\s]} {
            regexp {^/wikip(?:edia)?[:\s](.*)} $msg -> query
	    set q [http::formatQuery search $query]
	    gotoURL http://en.wikipedia.org/wiki/Special:Search?$q&go=Go
	}
	default {
	    set moreToGo 1
	}
    }

    # do we need to keep checking?
    if { $moreToGo } {
	if { !$::tkchat::LoggedIn } {
	    addStatus 0 "Command unavailable when not logged in: $msg"
	    return
	}
    } else {
	return
    }

    # now check against logged in commands
    switch -re -- $msg {
	{^/userinfo} {
	    ::tkjabber::msgSend $msg
	}
        {^/last\s+\w+} {
            ::tkjabber::msgSend [string range $msg 1 end] \
                -type chat -user ijchain -echo 0
        }
	{^/log\s?} {
	    if { $msg eq "/log" } {
		# Set the global logging state
		set Options(ServerLogging) all
		addStatus 0 "Your messages will be logged by the server."
	    } else {
		# Send a single message with logging enabled:
		::tkjabber::msgSend [string trim [string range $msg 4 end]]
	    }
	}
	{^/nolog\s?} {
	    if { $msg eq "/nolog" } {
		# Set the global logging state
		set Options(ServerLogging) none
		addStatus 0 \
			"Your messages will not be logged by the server."
	    } else {
		# Send a single message without logging:
		tkjabber::msgSend $msg -attrs [list nolog 1]
	    }
	}
	{^/nick\s?} {
	    ::tkjabber::setNick [string range $msg 6 end]
	}
	{^/topic\s?} {
	    tkjabber::setTopic [string range $msg 7 end]
	}
	{^/memo\s?} {
	    if { [regexp {^/memo ([^ ]+) (.+)} $msg -> toNick privMsg] } {
		tkjabber::send_memo $toNick $privMsg
	    }
	}
	{^/me\s?} {
	    switch $Options(ServerLogging) {
		oldStyle -
		none {
		    tkjabber::msgSend "/nolog$msg" -attrs [list nolog 1]
		}
		default {
		    tkjabber::msgSend $msg
		}
	    }
	}
	{^/ot\s?} {
	    if { [regexp {^/ot/?me (.+)$} $msg -> action] } {
		tkjabber::msgSend "/nolog/me $action"  -attrs [list nolog 1]
	    } else {
		tkjabber::msgSend "/nolog [string range $msg 4 end]" \
			-attrs [list nolog 1]
	    }
	}
	{^/msg\s} {
	    if { [regexp {^/msg ([^ ]+) (.+)} $msg -> toNick privMsg] } {
		if {[regexp {@} $toNick]} {
		    tkjabber::msgSend $privMsg -tojid $toNick -type normal
		} else {
		    tkjabber::msgSend $privMsg -user $toNick -type normal
		}
	    }
	}
	{^/chat\s?} {
	    if {[regexp {^/chat ([^ ]+)(?:\ (.*))?} $msg -> toNick privMsg]} {
		# Are we talking to a nick in this MUC or to an arbitrary JID?
		if {![regexp {([^@]+)@.*} $toNick toJID toNick]} {
		    set toJID $::tkjabber::conference/$toNick
		}
		set w [tkjabber::getChatWidget $toJID $toNick]
		set privMsg [string trim $privMsg]
		if {$privMsg ne ""} {
		    if { $w ne ".txt" } {
			addMessage \
				$w "" $Options(Nickname) $privMsg NORMAL end 0
			tkjabber::msgSend $privMsg -tojid $toJID -type chat
		    } else {
			tkjabber::msgSend $privMsg -user $toNick -type chat
		    }
		}
	    }
	}
	{^/afk}  -
	{^/away} {
	    set status ""
	    regexp {^/(?:(?:afk)|(?:away))\s*(.*)$} $msg -> status
	    set ::tkjabber::Away 1
	    set ::tkjabber::AutoAway 1
	    ::tkjabber::away $status
	}
	{^/dnd}  -
	{^/busy} {
	    set status ""
	    regexp {^/(?:(?:dnd)|(?:busy))\s*(.*)$} $msg -> status
	    set ::tkjabber::Away 1
	    set ::tkjabber::AutoAway 1
	    ::tkjabber::away $status dnd
	}
	{^/back} {
	    set status [string range $msg 6 end]
	    ::tkjabber::back $status
	}
        {^/quit} {
            ::tkchat::quit
        }
        {^/(kick)|(mute)|(unmute)|(op)|(deop)} {
            if {[regexp {^/((?:kick)|(?:mute)|(?:unmute)|(?:op)|(?:deop))\s+(\S+)(?:\s+(.*))?} $msg -> op nick reason]} {
                switch -exact -- $op {
                    kick { set role none }
                    mute { set role visitor }
                    unmute { set role participant }
                    op { set role moderator }
                    deop { set role participant }
                    default { set role $op }
                }
                ::tkjabber::setrole $nick $role $reason
            } else {
                ::tkchat::addStatus 0 \
                    "error: must be /$op nick ?reason ...?"
            }
        }
        {^/ban} {
            if {[regexp {^/ban\s+(\S+)(?:\s+(.*))?} $msg -> nick reason]} {
                ::tkjabber::setaffiliation $nick outcast $reason
            } else {
                ::tkchat::addStatus 0 \
                    "error: must be /ban nick ?reason ...?"
            }
        }
	default {
	    if {![checkAlias $msg]} then {
		# might be server command - pass it on
		switch $Options(ServerLogging) {
		    none {
			tkjabber::msgSend "/nolog $msg" -attrs [list nolog 1]
		    }
		    default {
			tkjabber::msgSend $msg
		    }
		}
	    }
	}
    }
    return
}

proc ::tkchat::entryUp {} {
    # Up arrow event in the message entry
    set w .eMsg
    upvar #0 ::tkchat::eHIST hist ::tkchat::eCURR cur
    if {$cur == 0} return
    if {$cur == [llength $hist]} {
	# at the end of the history, save the current line
	set ::tkchat::curMsg [$w get]
    }
    if {$cur} { incr cur -1 }
    $w delete 0 end
    set str [$w insert 0 [lindex $hist $cur]]
}

proc ::tkchat::entryDown {} {
    # Down arrow event in the message entry
    set w .eMsg
    upvar #0 ::tkchat::eHIST hist ::tkchat::eCURR cur
    if {$cur == [llength $hist]} return
    if {[incr cur] == [llength $hist] && [info exists ::tkchat::curMsg]} {
	# at the end of the history, it is the saved current line
	set msg $::tkchat::curMsg
    } else {
	set msg [lindex $hist $cur]
    }
    $w delete 0 end
    set str [$w insert 0 $msg]
}

proc ::tkchat::hideExtra {{p ""}} {
    grid remove $p.tMsg
    grid configure $p.eMsg -in $p.btm -row 0 -column 1 -sticky ew
    $p.ml configure -text ">>" -command [list ::tkchat::showExtra $p]
    $p.eMsg delete 0 end
    $p.eMsg insert end [string trim [$p.tMsg get 1.0 end]]
    if { $::Options(AutoScroll) } {
	update
	$p.txt see end
    }
}

proc ::tkchat::showExtra {{p ""}} {
    global Options
    grid remove $p.eMsg
    grid configure $p.tMsg -in $p.btm -row 0 -column 1 -sticky ew
    $p.ml configure -text "<<" -command [list ::tkchat::hideExtra $p]
    $p.tMsg delete 1.0 end
    $p.tMsg insert end [$p.eMsg get]
    if { $::Options(AutoScroll) } {
	update
	$p.txt see end
    }
}

proc ::tkchat::logonScreen {} {
    global Options
    variable DlgDone
    variable NS

    tkjabber::disconnect
    if {![winfo exists .logon]} {
	toplevel .logon -class Dialog
	wm withdraw .logon
	wm protocol .logon WM_DELETE_WINDOW { set ::tkchat::DlgDone cancel }
	wm transient .logon .
	wm title .logon "Logon to the Tcl'ers Chat"

	set lf [${NS}::frame .logon.frame]
	${NS}::checkbutton .logon.prx \
		-text "Use Proxy" \
		-variable ::Options(UseProxy) \
		-underline 7
	${NS}::label .logon.lph -text "Proxy host:port" -underline 0
	${NS}::frame .logon.fpx
	${NS}::entry .logon.eph -textvariable Options(ProxyHost)
	${NS}::entry .logon.epp -textvariable Options(ProxyPort) -width 5
	${NS}::label .logon.lpan -text "Proxy Auth Username" -underline 11
	${NS}::label .logon.lpap -text "Proxy Auth Password" -underline 13
	${NS}::entry .logon.epan -textvariable Options(ProxyUsername)
	${NS}::entry .logon.epap -textvariable Options(ProxyPassword) -show {*}
	${NS}::label .logon.lnm -text "Chat Username" -underline 9
	${NS}::label .logon.lpw -text "Chat Password" -underline 6
	${NS}::entry .logon.enm -textvariable Options(Username)
	${NS}::entry .logon.epw -textvariable Options(Password) -show *
	${NS}::checkbutton .logon.rpw \
		-text "Remember Chat Password" \
		-variable Options(SavePW) \
		-underline 0
	${NS}::frame .logon.fjsrv
	${NS}::label .logon.ljsrv -text "Jabber server:port" -underline 0
	${NS}::entry .logon.ejsrv -textvariable Options(JabberServer)
	${NS}::entry .logon.ejprt -textvariable Options(JabberPort) -width 5
	${NS}::label .logon.ljres -text "Jabber resource" -underline 3
	${NS}::entry .logon.ejres -textvariable Options(JabberResource)
	${NS}::label .logon.lconf -text "Jabber conference" -underline 10
	${NS}::entry .logon.econf -textvariable Options(JabberConference)

	${NS}::frame .logon.sslopt -borderwidth 0
	${NS}::radiobutton .logon.nossl \
		-text "No SSL" \
		-variable Options(UseJabberSSL) \
		-value no \
		-underline 1 \
		-command ::tkjabber::TwiddlePort
	${NS}::radiobutton .logon.rjabberssl \
		-text "Jabber SSL" \
		-variable Options(UseJabberSSL) \
		-value ssl \
		-command ::tkjabber::TwiddlePort
	${NS}::radiobutton .logon.rstarttls \
		-text "STARTTLS" \
		-variable Options(UseJabberSSL) \
		-value starttls \
		-command ::tkjabber::TwiddlePort

	${NS}::checkbutton .logon.atc \
		-text "Auto-connect" \
		-variable Options(AutoConnect) \
		-underline 5
	${NS}::frame  .logon.f  -border 0
	${NS}::button .logon.ok \
		-text "Logon" \
		-command { set ::tkchat::DlgDone ok } \
		-width 8 \
		-underline 0
	${NS}::button .logon.cn \
		-text "Cancel" \
		-command { set ::tkchat::DlgDone cancel } \
		-width 8 \
		-underline 0
	${NS}::button .logon.qu \
		-text "Quit" \
		-width 8 \
		-underline 0 \
		-command [namespace origin quit]
	catch {.logon.ok configure -default active}
	pack .logon.qu .logon.cn .logon.ok -in .logon.f -side right

	bind .logon <Alt-x> {.logon.prx invoke}
	bind .logon <Alt-l> {.logon.ok invoke}
	bind .logon <Alt-q> {.logon.qu invoke}
	bind .logon <Alt-c> {.logon.cn invoke}
	bind .logon <Alt-p> {focus .logon.eph}
	bind .logon <Alt-u> {focus .logon.epan}
	bind .logon <Alt-s> {focus .logon.epap}
	bind .logon <Alt-n> {focus .logon.enm}
	bind .logon <Alt-a> {focus .logon.epw}
	bind .logon <Alt-r> {.logon.rpw invoke}
	bind .logon <Alt-t> {.logon.atc invoke}
	bind .logon <Alt-e> {.logon.rjabberssl invoke}
	bind .logon <Alt-j> {focus .logon.ejsrv}
	bind .logon <Alt-b> {focus .logon.ejres}
	bind .logon <Alt-o> {focus .logon.nossl}
	bind .logon <Alt-f> {focus .logon.econf}

	trace variable Options(UseProxy)  w [namespace origin optSet]
	trace variable Options(SavePW)    w [namespace origin optSet]

	pack .logon.ejprt -in .logon.fjsrv -side right -fill y
	pack .logon.ejsrv -in .logon.fjsrv -side right -fill both -expand 1

	pack .logon.epp -in .logon.fpx -side right -fill y
	pack .logon.eph -in .logon.fpx -side right -fill both -expand 1

	pack .logon.nossl .logon.rjabberssl .logon.rstarttls \
		-in .logon.sslopt \
		-side left

	grid .logon.prx	-	      -		   -in $lf -sticky w  -pady 3
	grid x		.logon.lph    .logon.fpx   -in $lf -sticky w  -pady 3
	grid x		.logon.lpan   .logon.epan  -in $lf -sticky w  -pady 3
	grid x		.logon.lpap   .logon.epap  -in $lf -sticky w  -pady 3
	grid .logon.lnm	.logon.enm    -		   -in $lf -sticky ew -pady 5
	grid .logon.lpw	.logon.epw    -		   -in $lf -sticky ew
	grid x		.logon.rpw    -		   -in $lf -sticky w  -pady 3
	grid x		.logon.ljsrv  .logon.fjsrv -in $lf -sticky w  -pady 3
	grid x		.logon.ljres  .logon.ejres -in $lf -sticky w  -pady 3
	grid x		.logon.lconf  .logon.econf -in $lf -sticky w  -pady 3
	grid x		.logon.sslopt -		   -in $lf -sticky w  -pady 3
	grid x		.logon.atc    -		   -in $lf -sticky w  -pady 3
	grid x		x	      .logon.f	   -in $lf -sticky e  -pady 4

	pack $lf -side top -fill both -expand 1
	wm resizable .logon 0 0
	raise .logon
	bind .logon <Return> [list .logon.ok invoke]
	bind .logon <Escape> [list .logon.cn invoke]
    }

    set have_tls [expr {[package provide tls] != {}}]
    if {! $have_tls} {
	.logon.nossl invoke
	foreach w {.logon.nossl .logon.rjabberssl .logon.rstarttls} {
	    $w configure -state disabled
	}
    }

    optSet
    catch {::tk::PlaceWindow .logon widget .}
    wm deiconify .logon
    tkwait visibility .logon
    focus -force .logon.ok
    grab .logon
    vwait ::tkchat::DlgDone
    grab release .logon
    wm withdraw .logon
    if { $DlgDone eq "ok" } {
        unset -nocomplain Options(ProxyAuth)

	# connect
	logonChat
    }
}

proc ::tkchat::optSet {args} {
    global Options

    if {$Options(UseProxy)} {
	set s normal
    } else {
	set s disabled
    }
    foreach w {lph eph epp lpan epan lpap epap} {
	.logon.$w configure -state $s
    }
    if {$Options(SavePW)} {
	.logon.atc configure -state normal
    } else {
	.logon.atc configure -state disabled
	set Options(AutoConnect) 0
    }
}

proc ::tkchat::registerScreen {} {
    global Options
    variable DlgDone
    variable PasswordCheck ""
    variable NS

    set r .register

    if {![winfo exists $r]} {
	toplevel $r -class Dialog
	wm withdraw $r
	wm transient $r .
	wm title $r "Register for the Tcler's Chat"

	${NS}::label $r.
	${NS}::label $r.lfn -text "Full name" -underline 9
	${NS}::label $r.lem -text "Email address" -underline 9
	${NS}::label $r.lnm -text "Chat Username" -underline 9
	${NS}::label $r.lpw -text "Chat Password" -underline 6
	${NS}::label $r.lpwc -text "Confirm Password" -underline 6
	${NS}::entry $r.efn -textvar Options(Fullname)
	${NS}::entry $r.eem -textvar Options(Email)
	${NS}::entry $r.enm -textvar Options(Username)
	${NS}::entry $r.epw -textvar Options(Password) -show *
	${NS}::entry $r.epwc -textvar ::tkchat::PasswordCheck -show *

	${NS}::button $r.ok \
		-text "Ok" \
		-command { set ::tkchat::DlgDone ok } \
		-width 8 \
		-underline 0
	${NS}::button $r.cn \
		-text "Cancel" \
		-command { set ::tkchat::DlgDone cancel } \
		-width 8 \
		-underline 0

	bind $r <Alt-k> {.logon.ok invoke}
	bind $r <Alt-q> {.logon.cn invoke}
	bind $r <Alt-n> {focus .logon.enm}
	bind $r <Alt-a> {focus .logon.epw}

	grid $r.lfn $r.efn - -sticky w -pady 3
	grid $r.lem $r.eem - -sticky w -pady 3
	grid $r.lnm $r.enm - -sticky w -pady 3
	grid $r.lpw $r.epw - -sticky w -pady 3
	grid $r.lpwc $r.epwc - -sticky w -pady 3
	grid $r.ok - $r.cn -pady 10
	wm resizable $r 0 0
	raise $r
	bind $r <Return> [list .logon.ok invoke]
	bind $r <Escape> [list .logon.cn invoke]
    }
    catch {::tk::PlaceWindow $r widget .}
    wm deiconify $r
    tkwait visibility $r
    focus -force $r.efn
    grab $r
    while { 1 } {
	vwait ::tkchat::DlgDone
	if { $DlgDone eq "cancel" } {
	    break
	}
	if { $Options(Password) ne $PasswordCheck } {
	    tk_messageBox -message "The passwords do not match." \
		    -title "Password mismatch" -type ok
	    continue
	}
	break
    }
    grab release $r
    wm withdraw $r
    return [expr { $DlgDone eq "ok" }]
}

proc ::tkchat::doBug {msg} {
    # msg should be off form: ^/bug[: ]id
    if {[llength $msg] != 2} {
	addStatus 0 "wrong # args: must be /bug id" end ERROR
	return
    }
    set id  [lindex $msg end]
    set url "http://sourceforge.net/support/tracker.php?aid=$id"
    gotoURL $url
}

## ::tkchat::Find - searches in text widget $w for $str and highlights it
## If $str is empty, it just deletes any highlighting
# ARGS: w	- text widget
#	str	- string to search for
#	-case	TCL_BOOLEAN	whether to be case sensitive	DEFAULT: 0
#	-regexp	TCL_BOOLEAN	whether to use $str as pattern	DEFAULT: 0
## Taken from tkcon
##
proc ::tkchat::Find {w str args} {
    $w tag remove found 1.0 end
    set opts  {}
    foreach {key val} $args {
	switch -glob -- $key {
	    -c* { if {[string is true -strict $val]} { set case 1 } }
	    -r* { if {[string is true -strict $val]} { lappend opts -regexp } }
	    default { return -code error "Unknown option $key" }
	}
    }
    if {![info exists case]} { lappend opts -nocase }
    if {[string match {} $str]} return
    $w mark set foundmark 1.0
    while {[string compare {} [set ix [eval $w search $opts -count numc -- \
	    [list $str] foundmark end]]]} {
	$w tag add found $ix ${ix}+${numc}c
	$w mark set foundmark ${ix}+1c
    }
    return
}

# Patch 627521 by Pascal Scheffers:
# Search the chat window. msg should be what the user entered including
# the /? prefix.
# Modified by JH to be less compute-intensive, tighter code
proc ::tkchat::doSearch {msg} {
    variable searchString
    variable searchOffset

    if {[regexp {^/\?(.+)} $msg -> newSearch]} {
	if { $newSearch ne "" && $newSearch ne $searchString } {
	    # new search string differs from the previous, new search!
	    set searchString $newSearch
	    Find .txt $searchString -regexp 1
	    set searchOffset 0
	}
    }

    # do we need to search at all?
    if { $searchString ne "" } {
	# we need to query each time since the ranges will change if
	# we are clipping output at the top
	set ranges [.txt tag ranges found]
	set len [llength $ranges]
	if {$len} {
	    if {$searchOffset <= 0 || $searchOffset > $len} {
		# wrap to last (this is also the first seen)
		set searchOffset [expr {$len - 2}]
	    } else {
		incr searchOffset -2
	    }
	    .txt see [lindex $ranges $searchOffset]
	} else {
	    addStatus 0 "Could not find '$searchString'"
	}
    }
}

# Clear the search state and move back to the end of input.
proc ::tkchat::resetSearch {} {
    variable searchString ""
    .txt tag remove found 1.0 end
    .txt see end
}

# a couple of little helper funcs
proc ::tkchat::newColor { w idx } {
    variable DlgData

    set init "#[lindex $DlgData(Color,$idx) 3]"
    set tmp [tk_chooseColor -title "Select Override Color" -initialcolor $init]
    if { $tmp ne "" } {
	lset DlgData(Color,$idx) 3 [string range $tmp 1 end]
	$w configure -foreground $tmp -selectcolor $tmp
    }
}

proc ::tkchat::buildRow { f idx disp } {
    variable DlgData
    variable buildRow_seq

    if { ![info exists buildRow_seq] } {
	set buildRow_seq 1
    } else {
	incr buildRow_seq
    }
    set seq $buildRow_seq
    ::tk::label $f.nm$seq -text "$disp" -anchor w -font NAME -padx 0 -pady 0
    ::tk::radiobutton $f.def$seq -padx 0 -pady 0 -font FNT -indicatoron 0 \
	    -text "default" \
	    -variable ::tkchat::DlgData($idx) \
	    -value 1 \
	    -foreground  "#[lindex $DlgData(Color,$idx) 1]" \
	    -selectcolor "#[lindex $DlgData(Color,$idx) 1]"
    ::tk::radiobutton $f.inv$seq -padx 0 -pady 0 -font FNT -indicatoron 0 \
	    -text "inverted" \
	    -variable ::tkchat::DlgData($idx) \
	    -value 2 \
	    -foreground "#[lindex $DlgData(Color,$idx) 2]" \
	    -selectcolor "#[lindex $DlgData(Color,$idx) 2]"
    ::tk::radiobutton $f.ovr$seq -padx 0 -pady 0 -font FNT -indicatoron 0 \
	    -text "custom" \
	    -variable ::tkchat::DlgData($idx) \
	    -value 3 \
	    -foreground "#[lindex $DlgData(Color,$idx) 3]" \
	    -selectcolor  "#[lindex $DlgData(Color,$idx) 3]"
    button $f.clr$seq -padx 0 -pady 0 -font FNT \
	    -text "..." \
	    -command [list ::tkchat::newColor $f.ovr$seq $idx]
    grid $f.nm$seq $f.def$seq $f.inv$seq $f.ovr$seq $f.clr$seq \
	    -padx 2 -pady 2 -sticky ew
}

proc ::tkchat::EditMacros {} {
    variable NS
    
    set t .macros
    catch {destroy $t}
    toplevel $t -class Dialog
    wm transient $t .
    wm withdraw $t
    wm title $t "Edit Macros"

    listbox $t.lst -yscroll "$t.scr set" -font FNT -selectmode extended
    ${NS}::scrollbar $t.scr -command "$t.lst yview"
    ${NS}::label $t.lbl1 -text "Macro:" -font NAME
    ${NS}::entry $t.mac -width 10 -font FNT -validate all \
	-validatecommand {regexp -- {^\S*$} %P}
    bind $t.mac <Return> "focus $t.txt"
    ${NS}::label $t.lbl2 -text "Text:" -font NAME
    ${NS}::entry $t.txt -width 40 -font FNT
    bind $t.txt <Return> "$t.sav invoke"
    bind $t.lst <Double-Button-1> "::tkchat::MacroSel %W @%x,%y"
    button $t.sav -text Save -command "::tkchat::MacroSave $t"
    button $t.del -text Delete -command "::tkchat::MacroKill $t.lst"
    set    help "Macros are invoked whenever the first word in the posted\n"
    append help "message matches a defined macro name. Instead of the\n"
    append help "original message being sent, the Text from the macro\n"
    append help "definition is sent instead. You can substitue words from\n"
    append help "the post into the replacement text by using placeholders\n"
    append help "like %N. where N is which word to be inserted, where 1 is\n"
    append help "the first word after the macro name (%0 is the macro name itself)\n"
    append help "%N@ will substitute the Nth word to end of all input words.\n"
    append help "To get a litereal % char (if followed by a number) use %%\n"
    append help "Extra words are ignored, and if too few words passed the escape\n"
    append help "sequence will be shown\n"
    append help "\n"
    append help "Example: Macro foo defined as \n"
    append help "             '/me needs to %1 his %2 at the %3 because %4@'\n"
    append help "         User enters \n"
    append help "              'foo wash monkey zoo he is so dirty'\n"
    append help "         Result is everyone else seeing:\n"
    append help "    *user needs to wash his monkey at the zoo because he is so dirty\n"
    append help "\n"
    ${NS}::label $t.hlp -text $help -font FNT -justify left

    grid $t.lst - $t.scr -sticky news
    grid $t.del - - -sticky {} -pady 3
    grid $t.lbl1 $t.mac - -sticky nws
    grid $t.lbl2 $t.txt - -sticky news
    grid $t.sav - - -sticky {} -pady 3
    grid $t.hlp - - -sticky news -padx 10

    grid rowconfigure $t 0 -weight 10
    grid columnconfigure $t 1 -weight 10

    tkchat::MacroList $t.lst
    catch {::tk::PlaceWindow $t widget .}
    wm deiconify $t
}
proc ::tkchat::MacroSave {t} {
    global Options
    set m [string trim [$t.mac get]]
    set s [string trim [$t.txt get]]
    if {[string length $m] > 0 &&
	[string length $s] > 0} {
	set Options(Macro,$m) $s
	::tkchat::MacroList $t.lst
    }
}
proc ::tkchat::MacroKill { w } {
    global Options
    foreach idx [$w curselection] {
	set m [lindex [split [$w get $idx]] 0]
	unset -nocomplain Options(Macro,$m)
    }
    tkchat::MacroList $w
}
proc ::tkchat::MacroSel { w idx} {
    global Options
    set m [lindex [split [$w get $idx]] 0]
    if {[info exists Options(Macro,$m)]} {
	[winfo parent $w].mac delete 0 end
	[winfo parent $w].txt delete 0 end
	[winfo parent $w].mac insert end $m
	[winfo parent $w].txt insert end $Options(Macro,$m)
    }
}
proc ::tkchat::MacroList {w} {
    global Options
    $w delete 0 end
    foreach idx [lsort [array names Options Macro,*]] {
	$w insert end [format "%-10s  %s" [string range $idx 6 end] $Options($idx)]
    }
}

proc ::tkchat::ChangeColors {} {
    global Options
    variable DlgData
    variable DlgDone
    variable OnlineUsers

    # clear old data
    unset -nocomplain DlgData
    # make copy of current settings
    array set DlgData [array get Options Color,*]
    set DlgData(MyColor) $Options(MyColor)
    foreach nk [array names DlgData Color,*] {
	set nk [string range $nk 6 end]
	set DlgData($nk) [lindex $DlgData(Color,$nk) 0]
    }

    #Build screen
    set t .opts
    catch {destroy $t}
    toplevel $t -class Dialog
    wm transient $t .
    wm protocol $t WM_DELETE_WINDOW {set ::tkchat::DlgDone cancel}
    wm withdraw $t
    wm title $t "Color Settings"

    label $t.l1 -text "Posting Color" -font NAME
    label $t.l2 -text "Example Text" -background white \
	-foreground \#$DlgData(MyColor) -font ACT
    button $t.myclr -text "Change..." -font FNT -command {
	set tmp [tk_chooseColor \
		       -title "Select Your User Color" \
		       -initialcolor \#$::tkchat::DlgData(MyColor)]
	if { $tmp ne "" } {
	    .opts.l2 configure -foreground $tmp
	    set ::tkchat::DlgData(MyColor) [string range $tmp 1 end]
	}
    }

    label $t.l3 -text "Display Color Overrides" -font NAME
    frame $t.f -relief sunken -bd 2 -height 300
    canvas $t.f.cvs -yscrollcommand [list $t.f.scr set] \
	  -width 10 -height 300 -highlightthickness 0 -bd 0
    bind $t <Button-4> [list $t.f.cvs yview scroll -1 units]
    bind $t <Button-5> [list $t.f.cvs yview scroll  1 units]
    scrollbar $t.f.scr -command [list $t.f.cvs yview]
    pack $t.f.cvs -side left -expand 1 -fill both
    pack $t.f.scr -side left -fill y
    set f [frame $t.f.cvs.frm]
    $t.f.cvs create window 0 0 -anchor nw -window $f
    bind $f <Configure> {
	[winfo parent %W] configure -width [expr {%w+5}] -scrollregion [list 0 0 %w %h]
    }
    foreach {key str} { 1 "All\nDefault" 2 "All\nInverted" 3 "All\nCustom"} {
	button $f.all$key -text $str -padx 0 -pady 0 -font SYS -command \
		[string map [list %val% $key] {
		    foreach idx [array names DlgData Color,*] {
			set idx [string range $idx 6 end]
			set DlgData($idx) %val%
		    }
		}]
    }
    grid x $f.all1 $f.all2 $f.all3 x -padx 1 -pady 1
    foreach {idx str} {MainBG Background MainFG Foreground SearchBG Searchbackgr} {
	buildRow $f $idx $str
    }
    grid [label $f.online -text "Online Users" -font SYS] - - -
    set UserList [list]
    foreach network $OnlineUsers(networks) {
	set UserList [concat $UserList $OnlineUsers($network)]
    }
    set UserList [lsort -dictionary -unique $UserList]
    foreach nick $UserList {
	if { [info exists DlgData(Color,NICK-$nick)] } {
	    buildRow $f NICK-$nick $nick
	}
    }
    grid [label $f.offline -text "Offline Users" -font SYS] - - -
    foreach nick $Options(NickList) {
	set nick [lindex $nick 0]
	if { [lsearch -exact $UserList $nick] == -1 } {
	    buildRow $f NICK-$nick $nick
	}
    }
    frame $t.f2
    button $t.f2.ok \
	    -width 8 \
	    -text "OK" \
	    -command { set ::tkchat::DlgDone ok } \
	    -font SYS
    button $t.f2.app \
	    -width 8 \
	    -text "Apply" \
	    -command { set ::tkchat::DlgDone apply } \
	    -font SYS
    button $t.f2.can \
	    -width 8 \
	    -text "Cancel" \
	    -command { set ::tkchat::DlgDone cancel} \
	    -font SYS
    pack $t.f2.ok $t.f2.app $t.f2.can -side left -expand 1 -fill none

    grid $t.l1  $t.l2 $t.myclr x -padx 1 -pady 3 -sticky {}
    grid $t.l3    -       -    - -padx 1 -pady 3 -sticky ew
    grid $t.f     -       -    - -padx 1 -pady 5 -sticky news
    grid $t.f2    -       -    - -padx 1 -pady 10 -sticky news
    grid rowconfigure $t 2 -weight 1
    grid columnconfigure $t 3 -weight 1
    wm resizable $t 0 1
    catch {::tk::PlaceWindow $t widget .}
    wm deiconify $t
    set working 1
    while {$working} {
	vwait ::tkchat::DlgDone
	switch -- $DlgDone {
	    ok {
		set working 0
		set change 1
	    }
	    apply {
		set working 1
		set change 1
	    }
	    cancel {
		set change 0
		set working 0
	    }
	}
	if { $change } {
	    # apply changes for which
	    foreach nk [array names DlgData Color,*] {
		set nk [string range $nk 6 end]
		lset DlgData(Color,$nk) 0 $DlgData($nk)
	    }
	    # propagate changes to main data
	    array set Options [array get DlgData Color,*]
	    set Options(MyColor) $DlgData(MyColor)
	    # update colors
	    applyColors .txt All
	}
    }
    destroy $t
}

proc ::tkchat::applyColors { txt jid } {
    global Options

    # update colors
    $txt configure \
	    -background "#[getColor MainBG]" \
	    -foreground "#[getColor MainFG]"
    .pane.names configure \
	    -background "#[getColor MainBG]" \
	    -foreground "#[getColor MainFG]"
    $txt tag configure found -background "#[getColor SearchBG]"
    if { $jid eq "All" } {
	set nicks $Options(NickList)
    } else {
	lappend nicks $Options(Nickname)
	# Is it a conference/nick JID or a user/ressource one?
	if {[regexp {([^/]+)/(.+)} $jid -> conf nick]
	    && $conf eq $Options(JabberConference)
	} then {
	    lappend nicks $nick
	}
    }
    foreach nk $nicks {
	set nk [lindex $nk 0]
	set clr [getColor $nk]
	$txt tag configure NICK-$nk -foreground "#$clr"
	$txt tag configure NOLOG-$nk -foreground "#[fadeColor $clr]"
	.pane.names tag configure NICK-$nk -foreground "#$clr"
	if { $Options(Visibility,STAMP) } {
	    $txt tag raise NICK-$nk STAMP
	    $txt tag raise NOLOG-$nk STAMP
	} else {
	    $txt tag lower NICK-$nk STAMP
	    $txt tag lower NOLOG-$nk STAMP
	}
    }
}

# Point the Chat log to a new file.
proc ::tkchat::OpenChatLog {opt} {
    global Options
    switch -exact -- $opt {
	close {
	    set Options(ChatLogFile) ""
	    set Options(ChatLogOff) 1
	    Hook remove message [namespace origin ChatLogHook]
	    if {[info exists Options(ChatLogChannel)]} {
		close $Options(ChatLogChannel)
		unset Options(ChatLogChannel)
	    }
	}
	open {
	    set newFileName [tk_getSaveFile -initialfile $Options(ChatLogFile)]
	    if {[string length $newFileName]>0} {
		if {[catch {
		    set f [open $newFileName a]
		    fconfigure $f -buffering line -encoding utf-8
		    set Options(ChatLogFile) $newFileName
		    if {[info exists Options(ChatLogChannel)]} {
			close $Options(ChatLogChannel)
		    }
		    set Options(ChatLogChannel) $f
		    set Options(ChatLogOff) 0
		    Hook add message [namespace origin ChatLogHook]
		} err]} {
		    # Handle file access problems.
		    ::log::log error $err
		    bgerror $err
		}
	    }
	}
	load {
	    set loadFileName [tk_getOpenFile]
	    if { [string length $loadFileName] > 0 } {
		.txt configure -state normal
		.txt delete 1.0 end
		.txt configure -state disabled
		BookmarkClear
		InitOffset
		InsertHistoryMark
		after idle [list after 0 ::tkchat::LoadChatLog $loadFileName]
	    }
	}
    }
}

proc ::tkchat::LoadChatLog { loadFileName } {
    if { [catch {
	set f [open $loadFileName r]
	fconfigure $f -encoding utf-8
	set I [interp create -safe]
	interp alias $I m {} ::tkjabber::ParseLogMsg
	$I eval [read $f]
	close $f
    } err] } then {
	# Handle file access problems.
	::log::log error $err
	bgerror $err
	return
    }
    .txt configure -state normal
    .txt delete "HISTORY + 1 char" "HISTORY + 1 line"
    .txt insert "HISTORY + 1 char" \
	    "+++++++++++++++++++++ Parsing History +++++++++++++++++++++\n"
    .txt configure -state disabled
    after idle [list after 0 ::tkjabber::LoadHistoryLines]
}

proc ::tkchat::ChatLogHook { nick msg msgtype mark timestamp } {
    global Options

    if { !$Options(ChatLogOff) } {
	set timestamp \
		[clock format $timestamp -format "%Y%m%dT%H:%M:%S" -gmt 1]
	if { $msgtype eq "TRAFFIC" } {
	    switch -- $msg {
		entered {
		    set msg "$nick has become available"
		}
		left {
		    set msg "$nick has left"
		}
		nickchange {
		    set msg "[lindex $nick 0] is now known as [lindex $nick 1]"
		}
	    }
	    set nick {}
	} elseif { $msgtype eq "ACTION" } {
	    set msg "/me $msg"
	}
	puts $Options(ChatLogChannel) [list m $timestamp $nick $msg]
    }
}

# Point the Error Log to a new file
proc ::tkchat::OpenErrorLog {opt} {
    global Options
    switch -exact -- $opt {
	stderr {
	    set Options(LogFile) {}
	    set Options(LogStderr) 1
	    if {![string match stderr $Options(errLog)]} {
		close $Options(errLog)
	    }
	    set Options(errLog) stderr
	    ::log::lvChannelForall $Options(errLog)
	}
	pick {
	    set newFileName [tk_getSaveFile -initialfile $Options(LogFile)]
	    if {[string length $newFileName]>0} {
		if {[catch {
		    set f [open $newFileName a]
		    fconfigure $f -buffering line
		    set Options(LogFile) $newFileName
		    set oldchannel $Options(errLog)
		    set Options(errLog) $f
		    if {![string match stderr $oldchannel]} {
			close $oldchannel
		    }
		    set Options(LogStderr) 0
		    ::log::lvChannelForall $Options(errLog)
		} err]} {
		    # Handle file access problems.
		    set Options(LogFile) {}
		    set Options(LogStderr) 1
		    set Options(errLog) stderr
		    ::log::lvChannelForall $Options(errLog)
		    ::log::log error $err
		    bgerror $err
		}
	    }
	}
    }
}

proc ::tkchat::quit {} {
    set q "Are you sure you want to quit?"
    set a [tk_messageBox -type yesno -message $q]
    if { $a eq "yes" } {
	::tkchat::saveRC
	exit
    }
}

proc ::tkchat::saveRC {} {
    global Options
    variable useTile
    # Exit early if there is no home directory to save to
    if { ![info exists ::env(HOME)] } {
	return
    }

    set rcfile [file join $::env(HOME) .tkchatrc]
    array set tmp [GetDefaultOptions]

    # Options that need to be computed at save time
    set Options(Geometry) [wm geometry .]
    if {[package provide khim] ne {}} {
	set Options(Khim) [::khim::getConfig]
    }
    if { [winfo exists .pane] && $Options(DisplayUsers) } {
	if {$useTile} {
	    # the second list element '1' is just for compatibility
	    # to the non-Tile version:
	    set Options(Pane) [list [.pane sashpos 0] 1]
	} else {
	    set Options(Pane) [.pane sash coord 0]
	}
    }

    # Save these options to resource file
    set keep {
	Alert,* AnimEmoticons AutoAway AutoBookmark AutoConnect AutoFade
	AutoFadeLimit Browser BrowserTab ChatLogFile 
	ChatLogOff Color,* DisplayUsers
	Emoticons EnableWhiteboard EntryMessageColor errLog ExitMessageColor
	Font,* Fullname Geometry HistoryLines JabberConference JabberPort
	JabberResource JabberServer Khim 
	LogFile LogLevel LogStderr MyColor Nickname
	OneToOne Pane Password ProxyHost ProxyPort ProxyUsername SavePW
	ServerLogging Style Theme Transparency UseBabelfish UseJabberSSL
	UseProxy Username Visibility,*
    }

    foreach key $keep {
	foreach option [array names Options $key] {
	    if { [info exists tmp($option)] \
		    && [string tolower $Options($option)] \
		    eq [string tolower $tmp($option)] } {
		unset -nocomplain tmp($option)
	    } else {
		set tmp($option) $Options($option)
	    }
	}
    }

    # Trim down NickList and Visibility List
    foreach key [array names Options Visibility,NICK-*] {
	unset -nocomplain tmp($key)
    }
    foreach key [array names Options Color,NICK-*] {
	unset -nocomplain tmp($key)
    }
    unset -nocomplain tmp(NickList)
    set MainFG [getColor MainFG]
    foreach nk $Options(NickList) {
	set nick [lindex $nk 0]
	set keepNick 0
	if { $Options(Visibility,NICK-$nick) } {
	    set tmp(Visibility,NICK-$nick) 1
	    set keepNick 1
	}
	if { [lindex $Options(Color,NICK-$nick) 0] != 1 \
		|| [lindex $Options(Color,NICK-$nick) 1] ne $MainFG } {
	    set tmp(Color,NICK-$nick) $Options(Color,NICK-$nick)
	    set keepNick 1
	}
	if { $keepNick } {
	    lappend tmp(NickList) $nk
	}
    }

    # Do we save password?
    if { !$Options(SavePW) } {
	unset -nocomplain tmp(Password)
    }

    # Save original Nickname
    if { [info exists ::tkjabber::baseNick] } {
	set tmp(Nickname) $::tkjabber::baseNick
    }

    foreach option [lsort -dictionary [array names tmp]] {
	lappend oplist [list $option $tmp($option)]
    }
    if { ![catch { open $rcfile [list WRONLY CREAT] 0600 } fd] } {
	fconfigure $fd -encoding utf-8
	puts $fd "# Auto-generated file: DO NOT MUCK WITH IT!"
	puts $fd "array set Options \{"
	puts $fd [join $oplist "\n"]
	puts $fd "\}"
	puts $fd "# Auto-generated file: DO NOT MUCK WITH IT!"
	close $fd
    } else {
        tk_messageBox -icon error -title "Failed save" \
            -message "Failed to save options to '$rcfile'\n$fd"
    }
}

proc ::tkchat::scroll_set {sbar f1 f2} {
    # scroll_set assumes sbar ends with .sbar !
    global Options
    $sbar set $f1 $f2
    if {($f1 == 0) && ($f2 == 1)} {
	grid remove $sbar
    } else {
	if {[winfo exists .pane]} {
	    set p [string range $sbar 0 end-5]
	    grid $sbar -in $p.txtframe
	} else {
	    grid $sbar
	}
    }
    set Options(AutoScroll) [expr {(1.0 - $f2) < 1.0e-6 }]
}

proc ::tkchat::Debug {cmd args } {
    switch -- $cmd {
	console {
	    if {$::tkchat::_console} {
		console show
	    } else {
		console hide
	    }
	}
	reload {
	    uplevel \#0 [list source $::argv0]
	    set msg  "Script has been reloaded!\nDo you want to restart?"
	    set a [tk_messageBox -type yesno -message $msg]
	    if { $a eq "yes" } {
		Debug restart
	    }
	}
	restart {
	    tkjabber::disconnect
	    saveRC
	    eval destroy [winfo children .]
	    eval font delete [font names]
	    while { [after info] ne "" } {
		    foreach id [after info] {
		    after cancel $id
		}
	    }
	    unset ::Options
	    after 2000 [linsert $::argv 0 ::tkchat::Init]
	}
	retrieve {
	    Retrieve
	}
	purge {
	    .txt configure -state normal
	    .txt delete 1.0 end
	    .txt configure -state disabled
	    BookmarkClear
	    InitOffset
	    catch {::tkchat::LoadHistory}
	}
	server {
	    # Permit remote control using either DDE or the tcllib comm package
	    # We'll fix the title bar so people know which instance we are.
	    #
	    variable ServerID
	    if {![info exists ServerID]} {
		if {![catch {package require dde}]} {
		    set ServerID [tk appname]
		    set count 0
		    while {[dde services TclEval $ServerID] != {}} {
			incr count
			set ServerID "[tk appname] #$count"
		    }
		    dde servername $ServerID
		    if {$count != 0} {
			wm title . "[wm title .] #$count"
		    }
		} elseif {![catch {package require comm}]} {
		    set ServerID [comm::comm self]
		    wm title . "[wm title .] $ServerID"
		}
	    }
	}
	evalSel {
	    if { [catch {selection get} script] } {
		tk_messageBox -message "Couldn't get selection\n$script"
	    } else {
		if {[info complete $script] } {
		    if { [catch {uplevel \#0 $script} msg] } {
			tk_messageBox -message "pasted script errored\n$msg"
		    }
		} else {
		    tk_messageBox -message "script was not complete"
		}
	    }
	}
    }
}

proc ::tkchat::ChooseFont {} {
    set font [choosefont::choosefont \
                  -initialfont [list $::Options(Font,-family) \
                                    $::Options(Font,-size) \
                                    {}] \
                  -apply ::tkchat::SetFont]
    if { [string compare {} $font] } {
	SetFont $font
    }
    return
}

proc ::tkchat::SetFont { fontString } {
    foreach { family size } $fontString break
    set ::Options(Font,-family) $family
    set ::Options(Font,-size) $size
    foreach font {FNT ACT NOLOG NAME SYS STAMP} {
	font configure $font -family $family -size $size
    }
    return
}

proc ::tkchat::ChangeFont {opt val} {
    set ::Options(Font,$opt) $val
    foreach font {FNT ACT NOLOG NAME SYS STAMP} {
	font configure $font $opt $val
    }
}

proc ::tkchat::DoAnim {} {
    if {$::Options(AnimEmoticons)} {
	foreach img [image names] {
	    set name [lindex [split $img :] end]
	    catch {after cancel $::tkchat::img::id($name)}
	    anim $img
	}
    } else {
	foreach {nm id} [array get ::tkchat::img::id] {
	    after cancel $id
	    $nm configure -format "GIF -index 0"
	}
    }
}

proc ::tkchat::anim {image {idx -1}} {
    namespace eval ::tkchat::img {} ; # create img namespace
    incr idx
    if {[catch {$image configure -format "GIF -index $idx"}]} {
	if {$idx == 1} {
	    # stop animating, only base image exists
	    return
	} else {
	    # restart the cycle
	    set idx 0
	    $image configure -format "GIF -index $idx"
	}
    }
    catch {after cancel $::tkchat::img::id($image)}
    set ::tkchat::img::id($image) \
	    [after $::tkchat::img::delay [list ::tkchat::anim $image $idx]]
}

proc ::tkchat::SmileId {{image {}} args} {
    # Here be magic
    variable IMG

    # Do some checking so that things like 'C:/temp/tcl98/blah' and
    # 'lollipop' don't get smileys inserted
    set ids ""
    foreach arg $args {
	set IMG($arg) $image
	if { [string is alnum -strict -failindex i $arg] } {
	    lappend ids "\1$arg\2"
	} elseif { [string is alnum -strict [string index $arg end]] } {
	    if {$i > 0} {
		lappend ids "\1$arg\2"
	    } else {
		lappend ids "\3$arg\2"
	    }
	} else {
	    if {$i > 0} {
		lappend ids "\1$arg"
	    } else {
		lappend ids "\3$arg"
	    }
	}
    }
    set ids [join $ids "\0"]
    # The double-back is needed because when map is converted to a list,
    # it will become a single-back.
    set map [list \
	|   \\|		(   \\(		)   \\)		\[   \\\[	\
	-   \\-		.   \\.		*   \\*		?    \\?	\
	\\  \\\\	^   \\^		$   \\$		\1   \\m	\
	\2  \\M		\3  \\Y		\0  |				\
    ]
    # If we ever change this to use () capturing, change tkchat::Insert too.
    if { [info exists ::tkchat::IMGre] } {
	append ::tkchat::IMGre |[string map $map $ids]
    } else {
	set ::tkchat::IMGre [string map $map $ids]
    }
}

proc ::tkchat::Smile {} {
    namespace eval ::tkchat::img {} ; # create img namespace
    set ::tkchat::img::delay 150
    SmileId cry ":-(" ":^(" ":("
    image create photo ::tkchat::img::cry -format GIF -data {
	R0lGODlhDwAPANUAAP8AzeEDueQFvcUFp8kKq1AqdFJDkCQhUiIvaQASIQUr
	SAAdMQEjOgBtqABqowJ5uj1ofwCf9QCI0QB/xAB9vwB7vQB3twB1swBzsQBw
	rABhlAGb7QKo/gKU4gKFywWO18DAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAACAALAAAAAAPAA8AAAaa
	QJBQiFAoGIehElRAeDqdTwVRWBqenGxkI7EYhgVPlgONSJ6YKgjRyXoSCQo8
	M0GADpNIpANPVPoVDQcMH3oRHxAfGxMVFh4PCwwSG5QdEJRnExMXkU9QHQ8S
	DxgZFRQYCwcYURIORkcKGhUaSQgUXbENDg4asXZMDWelFhcYF7xqIAYOF4zE
		xxrJQk0OGQ0NGlRLQwcL3klKQQA7
    }
    SmileId grrr "8-(" "8^(" "8(" "8-|" "8^|" "8|"
    image create photo ::tkchat::img::grrr -format GIF -data {
	R0lGODlhDwAPANX/AMDAwM/RAMXHALGzAJKUAP//AOvrAOfnAObmAN/fANzc
	ANfYANjYANXVAM7PAM7OAMrKAMDBAMHBAL29ALS1ALW1ALO0ALS0AK6vAK6u
	AKytAKusAKysAKurAKqqAKmpAKOkAKSkAKKjAKChAJiZAJmZAJGSAJKSAJGR
	AI2OAI6NAI6OAI2NAHd3AP///8PDwwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAPAA8AAAaB
	QIBQCCsWh0hAcfXxfFZHJCxlwjgCDgyKBRtON8VQAlTcpLpKUwV0JsBUIcqp
	a6zb6y37x56HeUpGEAUQRiUeRR8PEQ8TBwYFCg8SD3swKxALHUYTDQgMEFBK
	HBcwLy4vYQEaaFMCJEYiIzAboUQpHBkDFg8QKGdJS01PUUlKdUlBADs=
    }
    SmileId LOL-anim LOL lol
    image create photo ::tkchat::img::LOL-anim -format GIF -data {
	R0lGODlhFQAYAKIFAAAAABhr/+fn5///AP///wAAAAAAAAAAACH/C05FVFND
	QVBFMi4wAwEAAAAh+QQFCgAFACwAAAAAFQAYAAADS1i63P4wykmrvbiAve0e
	INgtgAOEqKicJZm+JcucwyfSce3eoV3jq5fQ1sENeSocrafsaZgcnzR44ziB
	VGFStRuuOA3fzRPNmM+WBAAh+QQFCgAFACwKAAkABwADAAADCAgwrMrjrTYS
	ACH5BAkKAAUALAoACAAHAAQAAAMIOADK/SPKORMAIfkECQoABQAsAAAGABIA
	DgAAAz1YUNwNEDIwllN06ArrzV1Xec82ZqelmBQKLqjJbrBsu+t4468caRYQ
	IQIZgkoVglLZiiA5v8HlaZs+YpwEACH5BAkKAAUALAAABAASABAAAANDWFDc
	/mosSOOEIGcGJO5DKGWjA5KkFG6R2IHqyom067Y1DS9wvsqtnus0G9U0miKA
	gFxeTCuCVNqh7JChipBWUQgzCQAh+QQJCgAFACwAAAIAEgASAAADRVhQ3P5q
	LEjjrA9IDDW4zicymgR+Q5qK6uWhbLlFrVSvpKrfk7zzuZ9O1FsJQUHUEEkD
	EIhOZpNApWooMmiq4tthlC1AAgAh+QQJCgAFACwAAAEAEwATAAADTVhQ3P4M
	jAVrnNZKmt/m3TWBEWBy0oA1puqaysuO3yfHatvm+ez+rx4OCDxRUsQfxEaE
	eXI72mCZIxBOVkklBbB6U9uX0aVJCrdIaCEBACH5BAkKAAUALAAAAQATABMA
	AANJWLrc/jDKBaYDo1qaIfgfpzHfYJohFoolmGGvAqPZWaOyrdP4vJ+ziuqH
	gmlUPmCpU1ARQNAnNPd6WksP4/VpyipdXS9xdEmGEgAh+QQJCgAFACwAAAEA
	EwATAAADTli63P5wgQjBmLRZnOWtQMhtnBIOKBoW1jW66Xmq5qXadvuxad/r
	E5JPNcNsCKIksmW8IJ8AaLImgi5L1KsSa4p6rVxeCGypEEU0kG+VAAAh+QQJ
	CgAFACwAAAEAEwATAAADTli63A0uMjCgjNTeV7fL2gSMGlWF46CqY2F2Crim
	KRtXLI6/1rv+ut4JyKoJAQSSMqm8IZ/JaMg5kjKnrpNyi3lun5iTNSUxbW1l
	36mQAAAh+QQJCgAFACwAAAAAFAAWAAADXlhQ3P4NjAVrnDZLmuHm3TWBoTRg
	D6Cqjoky6imzynzFZ7yY3BZMwMAnF/nJjgPhivJx5WKeCWG5mg6igKk2y72m
	Rtwt9Este1vg8jjFLU9JsNH2/fqmeR3cDF6hOhIAIfkECQoABQAsAAAAABQA
	GAAAA2tYutwOLjYwoJTU3ld3zJqnUFU4AmhIdgs6vC86wloGx91qZYHelzFF
	DxAIVI7FVLBAdN1izZSwR0hZqwGHEVAVALze6oCTqpqVZKuakl6jJyW3qy0f
	P7juqmmGOutZHHFOewxONIR3UgsJAAAh+QQJCgAFACwAAAAAFAAYAAADXli6
	3P4wSgfmA6NahvVWHQSMI5d5IDasJ7iW7sjKlZrFQaYHna0BOZZwwFOVerYX
	7RUjkJ5O3a3QcVoB1xMMeXXKKNrncwAWm8kUrFmNWvS6NJGSxIzIhLCJ+MPn
	JwAAIfkECQoABQAsAAAAABQAGAAAA2tYutwOLjYwoJTU3ld3zJqnUFU4AmhI
	dgs6vC86wloGx91qZYHelzFFDxAIVI7FVLBAdN1izZSwR0hZqwGHEVAVALze
	6oCTqpqVZKuakl6jJyW3qy0fP7juqmmGOutZHHFOewxONIR3UgsJAAAh+QQJ
	CgAFACwAAAAAFAAYAAADYlhQ3P4NjAVrnDZLmuHm3TWBoTRgD6Cqjoky6imz
	ynzFZ7yY3BZMwMAnF/nJjgPhivJx5WKeCWG5mg6igKk2y72mRtwt9Este1vg
	8jjFLU9JsNH2/fqmeR3cDF6hhv6AFgkAACH5BAkKAAUALAAAAAAUABgAAANr
	WLrcDi42MKCU1N5Xd8yap1BVOAJoSHYLOrwvOsJaBsfdamWB3pcxRQ8QCFSO
	xVSwQHTdYs2UsEdIWasBhxFQFQC83uqAk6qalWSrmpJeoyclt6stHz+47qpp
	hjrrWRxxTnsMTjSEd1ILCQAAIfkECQoABQAsAAAAABQAGAAAA15Yutz+MEoH
	5gOjWob1Vh0EjCOXeSA2rCe4lu7IypWaxUGmB52tATmWcMBTlXq2F+0VI5Ce
	Tt2t0HFaAdcTDHl1yija53MAFpvJFKxZjVr0ujSRksSMyISwifjD5ycAACH5
	BAkKAAUALAAAAAAUABgAAANrWLrcDi42MKCU1N5Xd8yap1BVOAJoSHYLOrwv
	OsJaBsfdamWB3pcxRQ8QCFSOxVSwQHTdYs2UsEdIWasBhxFQFQC83uqAk6qa
	lWSrmpJeoyclt6stHz+47qpphjrrWRxxTnsMTjSEd1ILCQAAIfkECQoABQAs
	AAAAABQAGAAAA2JYUNz+DYwFa5w2S5rh5t01gaE0YA+gqo6JMuops8p8xWe8
	mNwWTMDAJxf5yY4D4YryceVinglhuZoOooCpNsu9pkbcLfRLLXtb4PI4xS1P
	SbDR9v36pnkd3AxeoYb+gBYJAAAh+QQJCgAFACwAAAAAFAAYAAADa1i63A4u
	NjCglNTeV3fMmqdQVTgCaEh2Czq8LzrCWgbH3Wplgd6XMUUPEAhUjsVUsEB0
	3WLNlLBHSFmrAYcRUBUAvN7qgJOqmpVkq5qSXqMnJberLR8/uO6qaYY661kc
	cU57DE40hHdSCwkAACH5BAkKAAUALAAAAAAUABgAAANeWLrc/jBKB+YDo1qG
	9VYdBIwjl3kgNqwnuJbuyMqVmsVBpgedrQE5lnDAU5V6thftFSOQnk7drdBx
	WgHXEwx5dcoo2udzABabyRSsWY1a9Lo0kZLEjMiEsIn4w+cnAAAh+QQJCgAF
	ACwAAAAAFAAYAAADa1i63A4uNjCglNTeV3fMmqdQVTgCaEh2Czq8LzrCWgbH
	3Wplgd6XMUUPEAhUjsVUsEB03WLNlLBHSFmrAYcRUBUAvN7qgJOqmpVkq5qS
	XqMnJberLR8/uO6qaYY661kccU57DE40hHdSCwkAACH5BAkKAAUALAAAAAAU
	ABgAAANiWFDc/g2MBWucNkua4ebdNYGhNGAPoKqOiTLqKbPKfMVnvJjcFkzA
	wCcX+cmOA+GK8nHlYp4JYbmaDqKAqTbLvaZG3C30Sy17W+DyOMUtT0mw0fb9
	+qZ5HdwMXqGG/oAWCQAAIfkECQoABQAsAAAAABQAGAAAA2tYutwOLjYwoJTU
	3ld3zJqnUFU4AmhIdgs6vC86wloGx91qZYHelzFFDxAIVI7FVLBAdN1izZSw
	R0hZqwGHEVAVALze6oCTqpqVZKuakl6jJyW3qy0fP7juqmmGOutZHHFOewxO
	NIR3UgsJAAAh+QQJCgAFACwAAAAAFAAYAAADXli63P4wSgfmA6NahvVWHQSM
	I5d5IDasJ7iW7sjKlZrFQaYHna0BOZZwwFOVerYX7RUjkJ5O3a3QcVoB1xMM
	eXXKKNrncwAWm8kUrFmNWvS6NJGSxIzIhLCJ+MPnJwAAIfkECQoABQAsAAAA
	ABQAGAAAA2tYutwOLjYwoJTU3ld3zJqnUFU4AmhIdgs6vC86wloGx91qZYHe
	lzFFDxAIVI7FVLBAdN1izZSwR0hZqwGHEVAVALze6oCTqpqVZKuakl6jJyW3
	qy0fP7juqmmGOutZHHFOewxONIR3UgsJAAAh+QQJCgAFACwAAAAAFAAYAAAD
	YlhQ3P4NjAVrnDZLmuHm3TWBoTRgD6Cqjoky6imzynzFZ7yY3BZMwMAnF/nJ
	jgPhivJx5WKeCWG5mg6igKk2y72mRtwt9Este1vg8jjFLU9JsNH2/fqmeR3c
	DF6hhv6AFgkAACH5BAkKAAUALAAAAAAUABgAAANrWLrcDi42MKCU1N5Xd8ya
	p1BVOAJoSHYLOrwvOsJaBsfdamWB3pcxRQ8QCFSOxVSwQHTdYs2UsEdIWasB
	hxFQFQC83uqAk6qalWSrmpJeoyclt6stHz+47qpphjrrWRxxTnsMTjSEd1IL
	CQAAIfkECQoABQAsAAAAABQAGAAAA15Yutz+MEoH5gOjWob1Vh0EjCOXeSA2
	rCe4lu7IypWaxUGmB52tATmWcMBTlXq2F+0VI5CeTt2t0HFaAdcTDHl1yija
	53MAFpvJFKxZjVr0ujSRksSMyISwifjD5ycAACH5BAUKAAUALAAAAAAUABgA
	AANrWLrcDi42MKCU1N5Xd8yap1BVOAJoSHYLOrwvOsJaBsfdamWB3pcxRQ8Q
	CFSOxVSwQHTdYs2UsEdIWasBhxFQFQC83uqAk6qalWSrmpJeoyclt6stHz+4
	7qpphjrrWRxxTnsMTjSEd1ILCQAAIf4aQ29weXJpZ2h0IKkgMjAwMCBLbGFh
	cyBXaXQAOw==
    }
    SmileId mad ">:(" ">:-(" ">:^("
    image create photo ::tkchat::img::mad -format GIF -data {
	R0lGODlhDwAPALP/AMDAwEpKSjk5Kd7ehP//Y729QoSEKefnKa2tEEpKAISE
	AK2tAL29AO/vAAAAAAAAACH5BAEAAAAALAAAAAAPAA8AAARlEEhZ0EJlalAW
	+9+1IUqyNOiSKMtUMKzCNPAiI5LXKIfhw7TWCyUIHAqHgADFqMwaRYJUybQ8
	fVKCj3l5HrLSQ3WIkg6kKBpOh/IZxEEJ4tNYnBh3Bi4XSnvwI38gIhsUFgh7ExEAOw==
    }
    SmileId oh ":-o" ":^o" ":o" ":-O" ":^O" ":O"
    image create photo ::tkchat::img::oh -format GIF -data {
	R0lGODlhDwAPALMAAAAAABgYGGPG/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAEALAAAAAAPAA8AAAQyMEgJap04VMH5
	xUAnelM4jgDlmcLWap3bsvIp1vaao+z+9pab6gYsxWQpUG+WKVmSkwgAOw==
    }
    SmileId smile ":-)" ":^)" ":)"
    image create photo ::tkchat::img::smile -format GIF -data {
	R0lGODlhDwAPAJEBAAAAAL+/v///AAAAACH5BAEAAAEALAAAAAAPAA8AAAIu
	jA2Zx5EC4WIgWnnqvQBJLTyhE4khaG5Wqn4tp4ErFnMY+Sll9naUfGpkFL5DAQA7
    }
    SmileId smile-big ":-D" ":^D" ":D"
    image create photo ::tkchat::img::smile-big -format GIF -data {
	R0lGODlhEAAQALMAAAAAAKamAP//AP///wAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAEALAAAAAAQABAAAAQ/MEgJqp04VMF7
	zUAnelPocSZKiWYqANrYyu5Io1a+ve0wAD7gD4fTWYivoHL4iiWFwNaqeFRN
	bdZSjZfR5jIRADs=
    }
    SmileId smile-dork "<:-)" "<:^)" "<:)"
    image create photo ::tkchat::img::smile-dork -format GIF -data {
	R0lGODlhEQAfAKIEAP//AAAAAP///zMzM////wAAAAAAAAAAACH5BAEAAAQA
	LAAAAAARAB8AAANhSLrcPi6uIGMQtLKL9RSdR3ChRmYmCKLSoJbWu1akyja1
	LeVzLPc4WWB4U5wCgOTQQUQmn4CbE0plOYdPbHSSnWq3I6pYaRyLM9fxtaxs
	flFeDCa7ycq9uC6eOW2bmiIeCQA7
    }
    SmileId smile-glasses "8-)" "8^)" "8)"
    image create photo ::tkchat::img::smile-glasses -format GIF -data {
	R0lGODlhFAAQALMAAAAAAAD/ANbWAP//AP///wAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAEALAAAAAAUABAAAARUMMgZgL00Tzu6
	t9rmjV8ICGQKaOc1uivVEjTQATRhx9Wl17jfZUUUBHW33K4iORk5HyivIkCl
	SFPnwIYtyarbIXfLqhp1uPF0Yx56TU7zM5QRryURADs=
    }
    SmileId smile-tongue-anim ":-p" ":^p" ":p"
    image create photo ::tkchat::img::smile-tongue-anim -format GIF -data {
	R0lGODlhDwAPAMQTAAAAADExAGMxAGNjAGNjMZxjAJycAJycMc6cAM7OAM7O
	Mc7OY/8AMf/OAP//AP//Mf//Y///nP///wAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQFZAAT
	ACwAAAAADwAPAAAIjQAnCBw4gMCAgQgHHoAQIQIEBQcSClzQ0CGEiwoSKmh4
	8YFHjwgUXoQAAIDHkg8cFBCoYGRJBw5KQmgQkoBHBSlh6kzAk0CABwgCNNCp
	M8CABAEINGgg4OjOAgEQJCAwAUGDAVizZk1gQODRARLCipXwdaCBBGDHHu2K
	0IBWrFwlThhwlqdbuQMJ6JUYEAAh+QQFDAATACwDAAUACQAKAAAIRQAhQAAA
	wIEDgg4UPDDI0EGCAA0aGgwgYEAChgkKBEgwoCODjgEMJBiJgAEDBCNTqhwp
	cmUCAxMGtHw5YILNCQQCELgZEAAh+QQFDAATACwGAAkAAwAFAAAIDAAZCBxI
	UGACBgkCAgAh+QQFDAATACwEAAQACAALAAAIQgAhCHzwYCCAgw4cHATwIKFD
	BwkaPEwYYEAChwkKVBzAoOOAAAYSJOjIAIFIkSRPouwo0sAAAyRdTphJgAGB
	mRMCAgAh+QQFDAATACwGAA0AAwACAAAICQATMEhAoGBAAAAh+QQFDAATACwG
	AAsAAwADAAAICgATMEhAsGCCgAAAIfkEBQwAEwAsBQAJAAUAAwAACAwABwgc
	mKDggAQEAwIAOw==
    }
    SmileId smirk-glasses ";/" ";-/" ";^/" ":/" ":-/" ":^/" "8/" "8-/" "8^/"
    image create photo ::tkchat::img::smirk-glasses -format GIF -data {
	R0lGODlhDwAPANX/AMDAwM/RAKepAJmbAI2PAICCAHp7AGxtAGlqAP//APr7
	APX2APX1AOrqANzdANXVAM7OAMzNAMvMAMnKAMnJAMjIAMfHAMTFAMLDAMDB
	ALu7ALi5ALe4ALa3ALe3ALGxALCwAKurAKqqAKenAKWmAKOkAKSkAKGiAKCg
	AJ6fAJucAJqbAJubAJmaAJGSAI6PAI+PAIiIAIWGAIaGAIGCAIKCAICBAIGB
	AGlpAFNTAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAPAA8AAAaG
	QIBQqCsWh0hAkUZTrWpHpA4hi+lEmZMMpxvquEXUY2PMdZUzF8t0qVxKKsPR
	SK/TD/VRHa9TwYwUCRRGLypFAxkeIQ8JjQ8WHBktSxogOhAMCgsNDhEaNF06
	KQI6IxQSEw4BGClnOjYdBHQfGB0FZ0o3KSckIyQaKTe4RDo0LU6gw1J0SUEAOw==
    }
    SmileId tongue2 ":-P" ":^P" ":P"
    image create photo ::tkchat::img::tongue2 -format GIF -data {
	R0lGODlhDwAPAMT/AMDAwFoAAJQAAK0AAM4QADkQAKUxAFI5EL21ADExKVpa
	Ss7Oa///c/f3Y4SEKe/vSv//SrW1MXNzGK2tEPf3CBgYAGNjAIyMAKWlAM7O
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAPAA8AAAWHICCK0WRG
	Ywo4Vpsh7aFOF4YgVIbt2BghGYuGQsRoLoiJCEipVCIQiERjeQEimQwlkVhE
	HZVqpqSlOBQNRmORwGVMZciDQZcT35M4JN2I5t5YGRcZD4UPRBQWFygIGC0S
	Dg4HBwUBFxckTIgEAwIDSSMTQGUEAgIGPSmiWS8GAqkqVyYTKCkhADs=
    }
    SmileId updown "(:" "(^:" "(-:"
    image create photo ::tkchat::img::updown -format GIF -data {
	R0lGODlhDwAPALMAAAAAAFr/zv//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAEALAAAAAAPAA8AAAQwMEgJap04VMH5
	xUAnelPoWaYAUN5orunoxnK31TOen3YK88CVRkdi4YQl2iejQWUiADs=
    }
    SmileId wink-anim ";-)" ";^)" ";)"
    image create photo ::tkchat::img::wink-anim -format GIF -data {
	R0lGODlhDwAPAPcAAOD0AL/WAKu6AF9oAICMAERKALe3t11oAm57A6y7MaWz
	L+//Y/H/Wer/N+j9M+j4MsfZL7jEMpWiKvD/cfD/SdnoMOz8NODvM73OMBwd
	CfH/e8nbM+7/WyQnDTo9IwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAYALAAAAAAPAA8AAAinAA0IFNihYMGB
	CA0UvECBQgMIBxF2YKhBw4QFDB5g6DCwQ4WGDRcscMAwAkeFFT6o/LDhgcoK
	FTh22LCggQUMGzhYULmhQYKCDUSKrMigwgYHDyB2cJCRwYIJExg00HhhY4cH
	FzxEaMiApAStVrcm8KAAAoYIXyVskCCzggMFHuLGlQDhJ8EEJCMkSKCgLAS2
	HRVgMAqhcALAEjso0Hs4YkKFBk8ODAgAOw==
    }
    SmileId blush ":-\}" ":^\}" ":8\}" ":\}"
    image create photo ::tkchat::img::blush -format GIF -data {
	R0lGODlhEAAQAMIAAAAAAPiUGPisEPj8APj8+AAAAAAAAAAAACH5BAEAAAQA
	LAAAAAAQABAAAAM9SKrQvpC0QWuLoGq78v4AQ02WF3oDOaKTIHyUmwJCELxb
	fTc6rvUtX+510jhQKRHM2FmOMMhVpHMMTa/XBAA7
    }
    SmileId coffee LP
    image create photo ::tkchat::img::coffee -format GIF -data {
	R0lGODlhEAAQAIQVAAAAAFJSUlpaWmNjY2tra3Nzc3t7e4SEhIyMjJSUlJyc
	nKWlpa2trbW1tcbGxs7OztbW1t7e3ufn5+/v7/f39///////////////////
	/////////////////////////yH5BAEAAB8ALAAAAAAQABAAAAV64CeOZEkC
	aAqYH8q8MIOejGIYB4IkieKsIgCFEQgICAXEwkEBtiITinT6cChmT0m1wVgo
	CgbbNbvtKghhVEStdXAXCfQVsKa334nBYV6H3Ltxe2ooDn9wBXsGCSsADRBl
	cDiLQAAJDQ5ugAcHBE4tAAZKhwNYJSopJiEAOw==
    }
    SmileId lunch |O| |o| |0|
    image create photo ::tkchat::img::lunch -format GIF -data {
	R0lGODlhEAAQAPABAAAAAP///yH5BAEAAAEALAAAAAAQABAAAAKRTJgwYcKE
	CRMmRIgwYcKECRMCBIgwYcKECRMCBIgwIUCACBMCBIgQIECAABECBIgQYMKE
	ARECBAgQYcKECQECDAgQYcKECQECDAgQYcKECQECDAgQYcKECQECDAgQYcKE
	CQECDIgQYMKEARECDIgQIECAABECDIgwIUCACBMCDIgwYcKECRMGTJgwYcKE
	CRMmBQA7
    }
    SmileId snooze zz zzz zzZ zZZ ZZZ ZZ
    image create photo ::tkchat::img::snooze -format GIF -data {
	R0lGODlhEAALAPABAAAAAP///yH5BAEAAAEALAAAAAAQAAsAAAJkTJgwYcKE
	AAECTJgwYcKEAAECTJgwYcKECRMCTJgwYcKECQMiTJgQIECECQEmTJgwYUCE
	AREmTJgwIcCEABMmBIgwIMKEAAECTIgQYMKEAAECDJgQIECECRMmBIgwYcKE
	CRMmBQA7
    }
    SmileId beer "|_P"
    image create photo ::tkchat::img::beer -format GIF -data {
	R0lGODlhEAAQAKECAAAAAP//AP///////yH/C05FVFNDQVBFMi4wAwEAAAAh
	/hVDcmVhdGVkIHdpdGggVGhlIEdJTVAAIfkECQEAAgAsAAAAABAAEAAAAjWU
	jymgx/iAXPK5GnCeBoQPglX0eWHWMeZZXitraaLoZiyavnOk3mXVCb2AQZ+l
	eDtSRsxEAQAh+QQJAQADACwAAAAAEAAQAAACNZyPOaDH+IBc8rkacJ4GhPyF
	G+eJYsSU5mdpodpK4BqrZ3bZJrrcb9VZ/VpCVkJ3OlaWQEQBACH5BAkBAAMA
	LAAAAAAQABAAAAI1nI85oMf4gFzyuRpwngaED4JVFJYR44Vpdnmr2pafG8tz
	PcPUYs9V19tYXqZEisiCjJYWQwEAIfkECQEAAwAsAAAAABAAEAAAAjWcjzmg
	x/iAXPK5GnCeBoQPglUUZmLHeF6ZXWupuuyJviZN2TC6zN/Y8W0sK10LIrTU
	RkBEAQAh+QQJAQADACwAAAAAEAAQAAACNpyPOaDH+IBc8rkacJ4GhP95YdVl
	4Bkx4mlaGhteqwm6ElzLeEwt+8YRzUaun6W0O1IqTM6hAAAh+QQJAQADACwA
	AAAAEAAQAAACNZyPOaDH+IBc8rkacJ4G5ACGG+eJoBcxpXle6xdaGpu5dNtJ
	d00tJ1uJ7CqOYeIF47mCwUQBACH5BAkBAAMALAAAAAAQABAAAAI2nI85oMf4
	gFzyuRpwngaED2Yb54VhxJSfml2saWnm6s5gLK0zuthr1fGNHLBbwvOiQSpM
	IKIAACH5BAkBAAMALAAAAAAQABAAAAI0nI85oMf4gFzyuRpwngaED4JVFHpi
	x5jhiaprdrmrpclw+7KTLVbd3KNlcpbfy1OkjJaJAgAh+QQJAQADACwAAAAA
	EAAQAAACNpyPOaDH+IBc8rkacJ4GhJ+BG+eJYsSU5mdpa3aVKthKLxvfMLWY
	ahUJrSqOl8fS0SEpwGaiAAAh+QQJAQADACwAAAAAEAAQAAACNJyPOaDH+IBc
	8rkacJ4GhA+CVRSWEeOV4pWmoaWpGSt/sGSu1JLJY+fbwGozyOuVGCk5hwIA
	IfkECQEAAwAsAAAAABAAEAAAAjWcjzmgx/iAXPK5GnCeBoQPglUUlhHjZWV2
	pSvbSe9naaLYquu5vN7YmW1qIRfMIbTERsxEAQA7
    }

    SmileId cyclops "O-\]" "O-)" "0-\]" "0-)"
    image create photo ::tkchat::img::cyclops -format GIF -data {
	R0lGODlhDwAPAKEAANnZ2QAAAP//AP///yH5BAEAAAMALAAAAAAPAA8AAAIz
	nB2Zx5MC4WIhWnlqVDagDYSa4I2BKG4jGQLr08HqRUvaumH3dX82h/HVPBTc
	pOFQEA8FADs=
    }

    SmileId donuts "donuts"
    image create photo ::tkchat::img::donuts -format GIF -data {
	R0lGODlhKAAPALIBAAAAAP//AGNjY0JC/0JCQjExMQAAAAAAACH/C05FVFND
	QVBFMi4wAwEAAAAh+QQJCgAGACwAAAAAKAAPAAADfmiq0L0wyklbuPfRKboX
	E4CN2RYNAqGuICSSJGAu6Gq3DBxY+6zUNlZOx+vNgEFV56UbyRjPUyrJYjZL
	hqgESRVYMd+nQ/ubdr1Nq2xN4d4WX7AjS9dA3ErXNcM2fT4ScWAMPgseG0V8
	cwoFjY6NEEEmY2SMAgWXmQUKVCoLCQAh+QQJCgAGACwAAAAAKAAPAAADf2iq
	0L0wyklbuPfRKboXE4CN2RYNAqGuICSSJGAu6Gq3DBxY+6zUNlZOx+vNgEFV
	56UbyRjPUyrJYjZLhqgESRVYMd+nY8INLptWmVoDKQuHMIeaQXETcNlrRpzV
	Lj4fEl9OC34bHhtFe3IKBY6PjhBBJnKGBgUCmJqZClQqCwkAIfkECQoABgAs
	AAAAACgADwAAA3xoqtC9MMpJW7j30Sm6FxOAjdkWDQKhriAkkiRgLuhqtwwc
	WPus1DZWTsfrzYBBVeelG8kYz1MqyWI2S4aoBEkVWDHfp2PCDS6bVpl2O03i
	vuCxWAMpK13XzHzz+UjgYAtiMx4bRXoOTwWLjIsQQSaJawoFApWXlgpUKgsJ
	ACH5BAkKAAYALAAAAAAoAA8AAAN9aKrQvTDKSVu499EpuhcTgI3ZFg0Coa4g
	JJIkYC7oarcMHFj7rNQ2Vk7H682AQVXnpRvJjqkki9ksMZ6nqJQgoGK8T6wE
	mVw2qbK0mKYN4rxfhyG8/rWFC3gsbPp8JHpWc3N1ER4bRRkOGgWNjo0QQSaL
	hQUClpiXClsECwkAIfkECQoABgAsAAAAACgADwAAA35oqtC9MMpJW7j30Sm6
	FxOAjdkWDQKhriAkkiRgLuhqtwwcWPus1DZWTsfrzYBBVeelG8mOqSSL2Swx
	nqeolCCgYrxPrASZXDapYcdYG8R5v2qDTAwhCxfv2GNOV3w+EnlWcj5+HRtF
	GQ4aBY2OjRBBJot9BgUCl5mYClsECwkAIfkECQoABgAsAAAAACgADwAAA31o
	qtC9MMpJW7j30Sm6FxOAjdkWDQKhriAkkiRgLuhqtwwcWPus1DZWTsfrzYBB
	VeelG8lcT1oqyWI2S4soBEkVWDFfreM07Xqb1qh2Ww7ivuAxwyCXdl3XjKMe
	+XwkcGBzDBocHRtFenUFjI2MEEEme2sKBQKWmJcKVCoLCQAh+QQFCgAGACwA
	AAAAKAAPAAADfWiq0L0wyklbuPfRKboXE4CN2RYNAqGuICSSJGAu6Gq3DBxY
	+6zUNlZOx+vNgEFV56UbyRjPUyrJYjZLhqgESRVYMd+nY8INLptWmXY7TeK+
	4HFUAykrXdfMfPP5SOBgLllrER4bRXoODwWMjYwQQSaKhAYFApaYlwpUKgsJ
	ADs=
    }

    SmileId bug "bug #" "bug#"
    image create photo ::tkchat::img::bug -format GIF -data {
	R0lGODlhEAAQAKEAAAAAAP///////////yH5BAEAAAIALAAAAAAQABAAAAIr
	lA94y5kMhYsL2Psg3tGGAHxWg4EgZjwbKlXr6L6sKqfxSsqXneI8BQweCgA7
    }
}

proc ::tkchat::ShowSmiles {} {
    variable NS
    
    set t .smileys
    if {[winfo exists $t]} {
	wm deiconify $t
	raise $t
    } else {
	variable IMG
        set images {}
	foreach {i e} [array get IMG] {
	    lappend tmp($e) $i
            lappend images $e
	}
        set images [lsort -unique $images]
	toplevel $t -class Dialog
        set f [${NS}::frame $t.f]
	wm title $t "Available Emoticons"
	wm withdraw $t
	wm protocol $t WM_DELETE_WINDOW [list wm withdraw $t]
        wm transient $t .
	set txt [text $f.txt -font NAME -tabs {1.5i l 2.0i l} \
		       -height [expr {[llength $images] + 6}]]
	set sb [${NS}::scrollbar $f.sb -command [list $txt yview]]
	$txt configure -yscrollcommand [list $sb set]
        set b [${NS}::button $f.ok -default active -text OK \
                   -command [list wm withdraw $t]]
	bind $t <Escape> [list $b invoke]
        bind $t <Return> [list $b invoke]

	foreach name $images {
            set image "::tkchat::img::$name"
	    $txt insert end "$name\t"
	    $txt image create end -image $image
	    if {[info exists tmp($name)]} {
		$txt insert end "\t[join $tmp($name) "   "]"
	    }
	    $txt insert end \n
	}
	$txt configure -state disabled
	grid $txt $sb -sticky news
        grid $b    -  -sticky e
	grid rowconfigure $f 0 -weight 1
	grid columnconfigure $f 0 -weight 1
        pack $f -side top -fill both -expand 1
	wm deiconify $t
	if {[llength [info command ::tk::PlaceWindow]] > 0} {
	    tk::PlaceWindow $t widget .
	}
    }
}

proc ::tkchat::Init {args} {
    global Options env
    variable OnlineUsers

    # set intial defaults
    set ::URLID 0
    set ::tkchat::eCURR 0
    set ::tkchat::eHIST ""
    set ::tkchat::LoggedIn 0
    array set Options [GetDefaultOptions]

    # attach a trace function to the log level
    trace add variable Options(LogLevel) write [namespace origin LogLevelSet]
    LogLevelSet

    # load RC file if it exists
    if { [info exists ::env(HOME)] } {
	set rcfile [file join $::env(HOME) .tkchatrc]
	if { [file readable $rcfile] } {
	    catch {
		set f [open $rcfile r]
		fconfigure $f -encoding utf-8
		set d [read $f]
		close $f
		eval $d
	    }
	}
    }

    # Convert old color list (<1.295) to new
    foreach nk [array names Options Color,*,Web] {
	set nk [string range $nk 6 end-4]
	catch {
	    switch -- $nk {
		MainBG -
		MainFG -
		SearchBG {
		    set Options(Color,$nk) [string map {Web 1 Inv 2 Mine 3} \
			    $Options(Color,$nk,Which)]
		    lappend Options(Color,$nk) $Options(Color,$nk,Web) \
			    $Options(Color,$nk,Inv) $Options(Color,$nk,Mine)
		}
		default {
		    set Options(Color,NICK-$nk) [string map \
			    {Web 1 Inv 2 Mine 3} $Options(Color,$nk,Which)]
		    lappend Options(Color,NICK-$nk) $Options(Color,$nk,Web) \
			    $Options(Color,$nk,Inv) $Options(Color,$nk,Mine)
		}
	    }
	}
	unset -nocomplain "Options(Color,$nk,Inv)"
	unset -nocomplain "Options(Color,$nk,Mine)"
	unset -nocomplain "Options(Color,$nk,Web)"
	unset -nocomplain "Options(Color,$nk,Which)"
    }

    # Convert old nick list (<1.289) to new and remove expired nicks
    set newNicks [list]
    foreach nk [lsort -dictionary -unique -index 0 $Options(NickList)] {
	if { ![string is integer -strict [lindex $nk 1]] } {
	    set nk [list $nk]
	}
	if { [lindex $nk 1] eq "" } {
	    lappend nk [clock seconds]
	}
	if { [lindex $nk 1] > [clock scan "-30 day"] } {
	    lappend newNicks $nk
	} else {
	    unset -nocomplain "Options(Color,NICK-[lindex $nk 0])"
	    unset -nocomplain "Options(Visibility,NICK-[lindex $nk 0])"
	}
    }
    set Options(NickList) $newNicks

    # Build the complete color list
    foreach nk $Options(NickList) {
	set nk [lindex $nk 0]
	if { ![info exist Options(Color,NICK-$nk)] } {
	    set Options(Color,NICK-$nk) $Options(Color,MainFG)
	}
    }

    # Set initial tab offset
    InitOffset

    # Compatability issues...
    if { [string is integer $Options(UseJabberSSL)] } {
	set Options(UseJabberSSL) [lindex {no ssl} $Options(UseJabberSSL)]
    }
    if { $::tcl_platform(os) eq "Windows CE" } {
	# Disable history loading on wince
	set Options(HistoryLines) 0
    }

    # Set the 'Hardcoded' Options:
    set OnlineUsers(networks) [list Jabber WebChat IRC]
    foreach network $OnlineUsers(networks) {
	set OnlineUsers($network) {}
	set OnlineUsers($network,hideMenu) 0
    }

    # Process command line args
    set nologin 0
    while {[string match -* [set option [lindex $args 0]]]} {
	switch -exact -- $option {
	    -nologin   { set nologin 1 }
	    -style     { set Options(Style) [Pop args 1] }
	    -theme     { set Options(Theme) [Pop args 1] }
	    -loglevel  { set Options(LogLevel) [Pop args 1] }
	    -useragent { set Options(UserAgent) [Pop args 1] }
	    -debug     { set Options(JabberDebug) 1 }
	    -nick - -nickname { setNickname [Pop args 1] }
	    -conference { set Options(JabberConference) [Pop args 1] }
	    -connect   { set Options(JabberConnect) [Pop args 1] }
	    -jabberserver {
		set j [split [Pop args 1] :]
		if {[llength $j] > 0} {
		    set Options(JabberServer) [lindex $j 0]
		    if {[llength $j] > 1} {
			set Options(JabberPort) [lindex $j 1]
		    }
		}
	    }
	    -- { Pop args ; break }
	    default {
		return -code error "bad option \"$option\":\
		    must be one of -nologin, -style, -theme,\
		    -loglevel, -useragent or --."
	    }
	}
	Pop args
    }

    # Set the useragent string to something a bit more standard.
    if {[info exists Options(UserAgent)]} {
	http::config -useragent $Options(UserAgent)
    } else {
	http::config -useragent "Mozilla/4.0\
	    ([string totitle $::tcl_platform(platform)];\
	    $::tcl_platform(os)) http/[package provide http]\
	    Tcl/[package provide Tcl]"
    }
    http::config -proxyfilter ::tkchat::proxyfilter

    # Open the error log to file if specified. Default is stderr.
    if {[string length $Options(LogFile)] > 0} {
	set Options(errLog) [open $Options(LogFile) a]
	fconfigure $Options(errLog) -buffering line
	set Options(LogStderr) 0
    } else {
	set Options(LogStderr) 1
    }
    ::log::lvChannelForall $Options(errLog)

    # Open the ChatLog file for appending.
    if {[string length $Options(ChatLogFile)] > 0} {
	set Options(ChatLogChannel) [open $Options(ChatLogFile) a]
	fconfigure $Options(ChatLogChannel) -buffering line -encoding utf-8
	set Options(ChatLogOff) 0
	Hook add message [namespace origin ChatLogHook]
    } else {
	set Options(ChatLogOff) 1
    }

    SetTheme $Options(Theme)

    # do this first so we have images available
    Smile
    # build screen
    CreateGUI
    foreach idx [array names Options Visibility,*] {
	set tag [string range $idx 11 end]
	.txt tag configure $tag -elide $Options($idx)
    }

    Hook add message [namespace origin IncrMessageCounter]
    BookmarkInit
    WinicoInit

    if {$Options(UseProxy)} {
	if {$Options(ProxyHost) != "" && $Options(ProxyPort) != ""} {
            # nothing
	} elseif {[info exists ::env(http_proxy)]} {
	    if {[regexp {(?:http://)?([[:alnum:].-]+)(?::(\d+))?} \
		     $::env(http_proxy) -> \
		     Options(ProxyHost) \
		     Options(ProxyPort)]} {
	    }
	}
    }

    ChangeFont -family $Options(Font,-family)
    ChangeFont -size $Options(Font,-size)

    createRosterImages

    # handle input method

    if {([package provide khim] ne {})
	&& [info exists Options(Khim)]} {
	eval $Options(Khim)
    }

    #call the (possibly) user defined postload proc:
    rcPostload

    # connect
    if {! $nologin} {
	if {$Options(AutoConnect)} {
	    logonChat
	} else {
	    logonScreen
	}
    }
}

proc ::tkchat::InitOffset {} {
    set ::Options(Offset) 50
    foreach nk $::Options(NickList) {
	set nk [lindex $nk 0]
	checkNickWidth $nk
    }
}

proc ::tkchat::GetDefaultOptions {} {
    # set defaults for User Settable Options
    array set Defaults {
	Alert,ACTION		1
	Alert,ALL		0
	Alert,ME		1
	Alert,NORMAL		1
	Alert,RAISE		1
	Alert,SOUND		0
	Alert,TOPIC		1
	AnimEmoticons		0
	AutoAway		-1
	AutoBookmark		0
	AutoConnect		0
	AutoFade		0
	AutoFadeLimit		80
	AutoScroll		0
	Browser			""
	BrowserTab		0
	ChatLogFile		""
	ChatLogOff		1
	DisplayUsers		1
	ElideTags		{ SINGLEDOT AVAILABILITY TRAFFIC SYSTEM ERROR }
	Emoticons		1
	EnableWhiteboard	1
	EntryMessageColor	#002500
	errLog			stderr
	ExitMessageColor	#250000
	Font,-family		Helvetica
	Font,-size		-12
	Fullname		""
	Geometry		600x500
	HistoryLines		-1
	JabberConference	tcl@tach.tclers.tk
	JabberLogs		"http://tclers.tk/conferences/tcl"
	JabberPort		5222
	JabberResource		tkchat
	JabberServer		all.tclers.tk
	LogFile			""
	LogLevel		notice
	LogStderr		1
	MsgTo			"All Users"
	MyColor			000000
	NickList		{}
	Nickname		""
	OneToOne		tabbed
	Pane			{520 2}
	Password		""
	ProxyHost		""
	ProxyPort		""
	ProxyUsername		""
	SavePW			0
	ServerLogging		all
        StoreMessages           1
	Style			any
	Theme			""
	Transparency		100
	UseBabelfish		0
	UseJabberSSL		no
	UseProxy		0
	Username		""
	Visibility,AVAILABILITY	0
	Visibility,ERROR	0
	Visibility,SINGLEDOT	0
	Visibility,STAMP	1
        Visibility,STATUSBAR	1
	Visibility,SYSTEM	0
	Visibility,TRAFFIC	0
	WhisperIndicatorColor	#ffe0e0
    }
    if {[info exists env(BROWSER)]} { set Defaults(Browser) $env(BROWSER) }
    foreach { nick clr } { MainBG ffffff MainFG 000000 SearchBG ff8c44 } {
	set Defaults(Color,$nick) [list 1 $clr [invClr $clr] $clr]
    }

    return [array get Defaults]
}

proc ::tkchat::setNickname { nick } {
    global Options
    variable ::tkjabber::baseNick

    if { ![info exist Options(Color,NICK-$nick)] } {
	if { [info exists Options(Color,NICK-$baseNick)] } {
	    set Options(Color,NICK-$nick) $Options(Color,NICK-$baseNick)
	} else {
	    set Options(Color,NICK-$nick) $Options(Color,MainFG)
	}
    }
    set Options(Nickname) $nick
    return $Options(Nickname)
}


# -------------------------------------------------------------------------
#
# routines for cloning of chat window adapted from http://wiki.tcl.tk/9167
#

proc ::tkchat::textClone {text clone} {
    #
    # clone the contents of a text widget into another text widget
    #
    # text -> path to text widget containing clone source
    # clone -> path to text widget for cloned content
    #
    ::tkchat::textRestore $clone [::tkchat::textSave $text]
}


proc ::tkchat::textSave {w} {
    #
    # serialize a text widgets contents
    #
    # w -> path to a text widget to serialize
    #
    # Returns: serialized text widget content
    #

    # the resulting string:
    set save {}
    # get the state of the widget:
    set dump [$w dump -mark 1.0 end]
    append dump " "
    append dump [$w dump -all 1.0 {end -1 ch}]
    # add more details:
    foreach {key value index} $dump {
	switch $key {
	  image {
	    # add attributes of an image:
	    set exec "\$w image create $index"
	    foreach k {-align -image -name -padx -pady} {
              set v [$w image cget $index $k]
              if {$v != ""} {append exec " $k \{$v\}"}
	    }
	    lappend save exec $exec {}
	  }
	  mark {
	    # add attributes of a mark:
	    lappend save $key $value $index
	    set exec "$w mark gravity $value [$w mark gravity $value]"
	    lappend save exec $exec {}
	  }
	  tagoff {
	    # add attributes of a tag:
	    set exec "\$w tag configure {$value}"
	    set keys {}
	    lappend keys -background -bgstipple -borderwidth -elide -fgstipple
	    lappend keys -font -foreground -justify -lmargin1 -lmargin2 -offset
	    lappend keys -overstrike -relief -rmargin -spacing1 -spacing2
	    lappend keys -spacing3 -tabs -underline -wrap
	    foreach k $keys {
              set v [$w tag cget $value $k]
              if {$v != ""} {append exec " $k \{$v\}"}
	    }
	    lappend save exec $exec {}
	    lappend save $key $value $index
	  }
	  window {
	    # add attributes of a window:
	    lappend save $key $value $index
	    set exec "$w window configure $index"
	    foreach k {-align -create -padx -pady -stretch}  {
              set v [$w window cget $index $k]
              if {$v != ""} {append exec " $k \{$v\}"}
	    }
	    lappend save exec $exec {}
	  }
	  default {
	    lappend save $key $value $index
	  }
	}
    }
    # return the serialized widget:
    return $save
}


proc tkchat::textRestore {w save} {
    #
    # fill a text widget with content
    # from a serialization
    #
    # w -> path to a text widget
    # save -> content of a text widget as serialized string
    #
    
    # empty the text widget:
    $w delete 1.0 end
    # create items, restoring their attributes:
    foreach {key value index} $save {
	switch $key {
	    exec    {eval $value}
	    image   {$w image create $index -name $value}
	    text    {$w insert $index $value}
	    mark    {
	      if {$value == "current"} {set current $index}
	      $w mark set $value $index
	    }
	    tagon   {set tag($value) $index}
	    tagoff  {
	      $w tag add $value $tag($value) $index
	      # this line is special to tkchat:
	      if {[string match URL-* $value]} {
	          $w tag bind $value <Button-1> [list ::tkchat::gotoURL [$w get $value.first $value.last]]
	      }
	    }
	    window  {$w window create $index -window $value}
	}
    }
    # restore the "current" index:
    $w mark set current $current
}

# -------------------------------------------------------------------------
# Tracing variables
# -------------------------------------------------------------------------
#trace add variable ::tkchat::UserClicked write ::tkchat::traceVar

proc ::tkchat::traceVar {varname -> action} {
    if {[catch {
	if {[string compare $action write] == 0} {
	    upvar $varname v
	    if {[catch {lindex [info level -1] 0} proc]} {
		set proc <unknown>
	    }
	    ::log::log debug "TRACE: $varname set to $v in $proc"
	}
    } msg]} { ::log::log warning "TRACE ERROR: $msg" }
}

# -------------------------------------------------------------------------
proc ::tkchat::UserInfoFetch {jid} {
    if {[catch {
	$::tkjabber::jabber vcard_get $jid \
	    [list [namespace current]::UserInfoFetchDone $jid]
    } msg]} {
	::log::log notice "error in vcard_get: $msg"
    }
}

proc ::tkchat::UserInfoFetchDone {jid jlib type xmllist} {
    ::log::log debug "UserInfoFetchDone jid=$jid type=$type '$xmllist'"

    set uivar [namespace current]::ui_$jid
    upvar #0 $uivar UI
    #set ::xmllist $xmllist
    if {[catch {
	switch $type {
	    result {
		after cancel $UI(after)
		UserInfoParse $jid $xmllist
	    }
	    error {
		after cancel $UI(after)
		set errType [lindex $xmllist 0]
		set errMsg [lindex $xmllist 1]
		switch $errType {
		    item-not-found {
			# The UserInfoDialog will take care of displaying
			# the not found message.
		    }
		    default {
			addSystem .txt "error while getting userinfo: $errType '$errMsg'"
		    }
		}
		# Not really a timeout, but this makes the dialog code continue
		# and use the right if branches
		set UI(timeout) 1
	    }
	    default {
		after cancel $UI(after)
		::log::log debug "eek, unknown type $type!"
		# Not really a timeout, but this makes the dialog code continue
		# and use the right if branches
		set UI(timeout) 1
	    }
	}
    } err]} { ::log::log error "ERROR UserInfoFetchDone: $err" }
}

proc ::tkchat::UserInfoParse {jid xmllist {prefix {}}} {
    variable ui_$jid
    upvar #0 [namespace current]::ui_$jid ui
    foreach child [wrapper::getchildren $xmllist] {
	set tag $prefix
	append tag [wrapper::gettag $child]
	set data [wrapper::getcdata $child]
	set kids [wrapper::getchildren $child]
	if {[llength $kids] > 0} {
	    UserInfoParse $jid $child "${tag}_"
	} else {
	    set ui($tag) $data
	}
    }
}

proc ::tkchat::UserInfoSend {jid} {
    variable ui_$jid
    set xmllist [wrapper::createtag vCard -attrlist {xmlns vcard-temp}]
    foreach {tag value} [array get ui_$jid] {
	set tags [split $tag _]
	set tag [lindex $tags end]
	set item [wrapper::createtag $tag -chdata $value]
	set xmllist [UserInfoAppendChild $xmllist [lrange $tags 0 end-1] $item]
	set xmllist [lreplace $xmllist 2 2 0]
    }

    $tkjabber::jabber send_iq set [list $xmllist] \
	-command [namespace current]::UserInfoSent
}

proc ::tkchat::UserInfoSent {type args} {
    if { $type ne "result" } {
	tk_messageBox -icon error -title [string totitle $type] \
	    -message $args
    }
}

proc ::tkchat::UserInfoAppendChild {xmllist tags child} {
    if {[llength $tags] > 0} {
	set tag [lindex $tags 0]
	set tags [lrange $tags 1 end]
	set kids [wrapper::getchildren $xmllist]
	set new {}
	set found 0
	foreach kid $kids {
	    if { [wrapper::gettag $kid] eq $tag } {
		set found 1
		lappend new [UserInfoAppendChild $kid $tags $child]
	    } else {
		lappend new $kid
	    }
	}
	if {!$found} {
	    set kid [wrapper::createtag $tag -attrlist {xmlns vcard-temp}]
	    lappend new [UserInfoAppendChild $kid $tags $child]
	}
	set xmllist [wrapper::setchildlist $xmllist $new]
	set xmllist [lreplace $xmllist 2 2 0]
    } else {
	set kids [wrapper::getchildren $xmllist]
	lappend kids $child
	set xmllist [wrapper::setchildlist $xmllist $kids]
	set xmllist [lreplace $xmllist 2 2 0]
    }
    return $xmllist
}

proc ::tkchat::UserInfoDialog {{jid {}}} {
    variable UserInfo
    variable UserInfoBtn
    variable UserInfoWin
    variable useTile
    variable NS

    if {$jid == {}} {
	set jid [::tkjabber::jid !resource $::tkjabber::myId]
    }

    set uivar [namespace current]::ui_$jid
    variable $uivar
    upvar #0 $uivar UI
    if {![info exists UI]} {
	set UI(after) [after 5000 [list array set $uivar {timeout 1}]]
	addStatus 0 "Requesting user info for $jid..."
	UserInfoFetch $jid
	tkwait variable $uivar
	after cancel $UI(after)
	unset UI(after)
    } else {
	if { [info exists UI(id)] } {
	    if {![catch {raise .$UI(id)}]} {
		return
	    }
	} else {
	    addStatus 0 "Still waiting for a vcard from the server..."
	    # Reentry during timeout period.
	    return
	}
    }
    if { [info exists UI(timeout)] && [::tkjabber::jid !resource $jid] \
	     ne [::tkjabber::jid !resource $::tkjabber::myId] } {
	# Not available, and not the users own vcard.
	::log::log debug "cleanup as no UI"
	unset $uivar
	addStatus 0 "No info available for $jid"
	return
    }

    if {![info exists UserInfoWin]} {set UserInfoWin 0}

    set id userinfo[incr UserInfoWin]
    set UI(id) $id

    set [namespace current]::$id -1
    set dlg [toplevel .$id -class Dialog]
    wm title $dlg "User info for $jid"
    set f [${NS}::frame $dlg.f]
    if {!$useTile} { $dlg.f configure -bd 0 }

    # country Country city City age Age
    # photo_url "Picture URL" icq_uin "ICQ uin"
    foreach {key text} {FN "Real name" EMAIL_USERID Email URL "Homepage URL" \
			    ADR_LOCALITY "City"  ADR_CTRY "Country" \
			    PHOTO_EXTVAL "Photo URL" BDAY "Birthday"} {
	set l [${NS}::label $f.l$key -text $text -anchor nw]
	set e [${NS}::entry $f.e$key -textvariable [set uivar]($key)]
	if {!$useTile} { $f.e$key configure -bd 1 -background white }
	grid configure $l $e -sticky news -padx 1 -pady 1
    }
    set l [${NS}::label $f.lstuff -text "Anything else" -anchor nw]
    set e [${NS}::frame $f.estuff]
    set et [text $e.text -height 6 -bd 1 -background white]
    set es [${NS}::scrollbar $e.scroll -command [list $et yview]]
    if {!$useTile} {
	$f.estuff configure -bd 0
	$e.scroll configure -bd 1
    }
    $et configure -yscrollcommand [list $es set]
    catch {$et insert 0.0 $UI(DESC)}
    grid configure $et $es -sticky news
    grid rowconfigure $e 0 -weight 1
    grid columnconfigure $e 0 -weight 1

    grid configure $l $e -sticky news -padx 1 -pady 1
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 8 -weight 1

    set btns [${NS}::frame $dlg.buttons]
    if {!$useTile} { $dlg.buttons configure -bd 1 }
    ${NS}::button $btns.ok -text Save -width 10 -state disabled \
	-command [list set [namespace current]::$id 1]
    ${NS}::button $btns.cancel -text Close -width 10 \
	-command [list set [namespace current]::$id 0]
    pack $btns.cancel $btns.ok -side right

    pack $btns -fill x -side bottom
    pack $f -fill both -expand 1 -side top

    if { [::tkjabber::jid !resource $jid] \
	     eq [::tkjabber::jid !resource $::tkjabber::myId] } {
	$btns.ok configure -state normal
    }

    bind .$id <Key-Escape> [list set [namespace current]::$id 0]

    set UserInfoBtn -1

    tkwait variable [namespace current]::$id

    if {[set [namespace current]::$id] == 1} {
	set UI(DESC) [$et get 0.0 end]
	UserInfoSend $jid
    }
    destroy $dlg
    unset [namespace current]::$id
    unset UI
}

# -------------------------------------------------------------------------

# Windows taskbar support.
# At some point I want to support multiple icons for nochat/chat/alert.
#
proc ::tkchat::WinicoInit {} {
    ::log::log debug "tk windowingsystem [tk windowingsystem]"
    if {[tk windowingsystem] eq "win32" &&
	![catch {package require Winico}]} {
	variable TaskbarIcon
	variable WinicoWmState [wm state .]

	set icofile [file join [file dirname [info script]] tkchat.ico]
	if {[file exists $icofile]} {
	    set TaskbarIcon [winico createfrom $icofile]
	    winico taskbar add $TaskbarIcon \
		    -pos 0 \
		    -text [wm title .] \
		    -callback [list [namespace origin WinicoCallback] %m %i]
	    bind . <Destroy> [namespace origin WinicoCleanup]
	}
    } else {
	proc ::tkchat::WinicoUpdate {} {return}
    }
}

proc ::tkchat::WinicoUpdate {} {
    variable MessageCounter
    variable TaskbarIcon

    if {[llength [info commands winico]] < 1} { return }
    if { $MessageCounter > 0 } {
	winico taskbar modify $TaskbarIcon \
		-pos 2 \
		-text "$MessageCounter - Tcl'ers chat"
    } else {
	winico taskbar modify $TaskbarIcon \
		-pos 0 \
		-text "Tcl'ers chat"
    }
}

proc ::tkchat::WinicoCleanup {} {
    variable TaskbarIcon
    winico taskbar delete $TaskbarIcon
}

proc ::tkchat::WinicoCallback {msg icn} {
    variable WinicoWmState
    switch -exact -- $msg {
	WM_LBUTTONDOWN {
	    if { [wm state .] eq "withdrawn" } {
		wm state . $WinicoWmState
		wm deiconify .
		focus .eMsg
	    } else {
		set WinicoWmState [wm state .]
		wm withdraw .
	    }
	}
    }
}

# -------------------------------------------------------------------------

proc ::tkchat::BookmarkInit {} {
    variable bookmark

    set bookmark(id) 0
    set bookmark(removed) 0
    set bookmark(last) 0.0

    image create photo ::tkchat::img::bookmark -format GIF -data {
	R0lGODlhEAAMAJEAANnZ2QAAAAD//////yH5BAEAAAAALAAAAAAQAAwAAAI9
	hE3xCf4FyQ+CD0HyLQh2kHwMAAAK5WMAAFAoHwMAgEL5GAAAFMrHAACgUD4G
	wQ6Sb0HwIUh+EPyj+AS7AAA7
    }
    image create photo ::tkchat::img::bookmarkauto -format GIF -data {
	R0lGODlhEAAMAJEAANnZ2QAAAAD/AP///yH5BAEAAAAALAAAAAAQAAwAAAI9
	hE3xCf4FyQ+CD0HyLQh2kHwMAAAK5WMAAFAoHwMAgEL5GAAAFMrHAACgUD4G
	wQ6Sb0HwIUh+EPyj+AS7AAA7
    }
    set bookmark(width) [image width ::tkchat::img::bookmark]
    set tempWidth [image width ::tkchat::img::bookmarkauto]
    if { $tempWidth > $bookmark(width) } {
	set bookmark(width) $temp
    }
    bind . <Control-F2>	{
	set x [expr {[winfo pointerx .txt] - [winfo rootx .txt]}]
	set y [expr {[winfo pointery .txt] - [winfo rooty .txt]}]
	.txt mark set AddBookmark "@$x,$y linestart"
	::tkchat::BookmarkToggle
    }
    bind . <F2>		::tkchat::BookmarkNext
    bind . <Shift-F2>	::tkchat::BookmarkPrev
    bind . <Control-G>	::tkchat::GoogleSelection
    bind . <Control-g>	::tkchat::GoogleSelection
    bind . <Control-p>  ::tkchat::PasteDlg
}

proc ::tkchat::BookmarkToggle { {auto ""} } {
    variable bookmark

    set index1 [.txt index "AddBookmark linestart"]
    set index2 [.txt index "AddBookmark lineend"]
    set imagedump [.txt dump -image $index1 $index2]
    set index3 [lsearch $imagedump bookmark*]
    set state [.txt cget -state]
    .txt configure -state normal
    if { $index3 == -1 } {
	incr bookmark(id)
	.txt image create AddBookmark \
		-name bookmark$auto$bookmark(id) \
		-image ::tkchat::img::bookmark$auto
	if { $bookmark(id) == 1 } {
	    set tabs [.txt cget -tabs]
	    if { $tabs eq {} } {
		# Make sure tabs have been set
		StampVis
		set tabs [.txt cget -tabs]
	    }
	    foreach tab $tabs {
		incr tab $bookmark(width)
		lappend newtabs $tab
		set width $tab
	    }
	    .txt configure -tabs $newtabs
	    .txt tag configure BOOKMARK	-lmargin1 $bookmark(width)
	    .txt tag configure MSG	-lmargin2 $width
	}
    } else {
	if { $auto eq "" } {
	    .txt delete [lindex $imagedump $index3]
	    set bookmark(last) $index1
	    incr bookmark(removed)
	    if { $bookmark(removed) == $bookmark(id) } {
		BookmarkClear
	    }
	}
    }
    .txt configure -state $state
    if { $::Options(AutoScroll) } {
	.txt see end
    }
}

proc ::tkchat::BookmarkRemoveAuto { index1 } {
    variable bookmark

    set index1 [.txt index "$index1 linestart"]
    set index2 [.txt index "$index1 lineend"]
    set imagedump [.txt dump -image $index1 $index2]
    set index3 [lsearch $imagedump bookmarkauto*]
    if { $index3 != -1 } {
	set state [.txt cget -state]
	.txt configure -state normal
	.txt delete [lindex $imagedump $index3]
	set bookmark(last) $index1
	incr bookmark(removed)
	if { $bookmark(removed) == $bookmark(id) } {
	    BookmarkClear
	}
	.txt configure -state $state
    }
}

proc ::tkchat::BookmarkNext {} {
    variable bookmark

    if { $bookmark(last) eq "end" } {
	set index1 0.0
    } else {
	set index1 "[.txt index $bookmark(last)] +1 chars"
    }
    set imagedump [.txt dump -image $index1 end]
    set index2 [lsearch $imagedump bookmark*]

    if { $index2 == -1 } {
	set bookmark(last) end
    } else {
	set bookmark(last) [lindex $imagedump $index2]
    }
    if { [string match "bookmarkauto*" $bookmark(last)] } {
	set index1 [.txt index $bookmark(last)]
	after 5000 [list ::tkchat::BookmarkRemoveAuto $index1]
    }
    .txt see $bookmark(last)
    return $bookmark(last)
}

proc ::tkchat::BookmarkPrev {} {
    variable bookmark

    if { $bookmark(last) == 0.0 } {
	set index1 end
    } else {
	set index1 "[.txt index $bookmark(last)] -1 chars"
    }
    set imagedump [.txt dump -image 0.0 $index1]
    set imagedump [lsearch -all -inline $imagedump bookmark*]
    set index2 [lindex $imagedump end]

    if { $index2 eq "" } {
	set bookmark(last) 0.0
    } else {
	set bookmark(last) $index2
    }
    if { [string match "bookmarkauto*" $bookmark(last)] } {
	set index1 [.txt index $bookmark(last)]
	after 5000 [list ::tkchat::BookmarkRemoveAuto $index1]
    }
    .txt see $bookmark(last)
    return $bookmark(last)
}

proc ::tkchat::BookmarkClear {} {
    variable bookmark

    if { $bookmark(id) } {
	set state [.txt cget -state]
	.txt configure -state normal
	while { $bookmark(id) } {
	    catch { .txt delete bookmark$bookmark(id) }
	    catch { .txt delete bookmarkauto$bookmark(id) }
	    incr bookmark(id) -1
	}
	.txt configure -state $state
	set bookmark(removed) 0
	set tabs [.txt cget -tabs]
	foreach tab $tabs {
	    incr tab -$bookmark(width)
	    lappend newtabs $tab
	    set width $tab
	}
	.txt configure -tabs $newtabs
	.txt tag configure BOOKMARK	-lmargin1 0
	.txt tag configure MSG	-lmargin2 $width
	if { $::Options(AutoScroll) } {
	    .txt see end
	}
    }
}

proc ::tkchat::GoogleSelection { w } {
    set sel [$w tag ranges sel]
    set t [$w get [lindex $sel 0] [lindex $sel 1]]
    gotoURL http://www.google.com/search?ie=UTF-8&oe=UTF-8&[::http::formatQuery q $t]
}

# -------------------------------------------------------------------------

# NoisyUsers

proc ::tkchat::noisyUser { msg } {
    variable noisyUsers

    #Assign msg elements to nick and time:
    set nick [lrange $msg 1 end-1]
    set time [lrange $msg end end]

    if { $nick eq "" } {
	set cnt 0
	foreach { nick } [lsort -dictionary [array names noisyUsers]] {
	    set cnt 1
	    if { [nickIsNoisy $nick] } {
		set time [clock format $noisyUsers($nick) -format %H:%M:%S]
		addSystem .txt "$nick is noisy until $time"
	    }
	}
	if { !$cnt } {
	    addSystem .txt "You don't consider anyone noisy right now"
	}
    } else {
	if { ![string is integer -strict $time] } {
	    if { [info exists noisyUsers($nick)] } {
		set time 0
	    } else {
		set time 5
	    }
	}
	switch -- $time {
	    -1 -
	    0 {
		if { [info exists noisyUsers($nick)] } {
		    unset noisyUsers($nick)
		    addSystem .txt "$nick is no longer considered noisy."
		} else {
		    addSystem .txt "$nick not considered noisy at this time."
		}
	    }
	    default {
		set noisyUsers($nick) [expr { [clock seconds] + 60 * $time }]
		if { $time > 1 } {
		    addSystem .txt \
			    "$nick is considered noisy for $time minutes."
		} else {
		    addSystem .txt \
			    "$nick is considered noisy for $time minute."
		}
	    }
	}
    }
}

proc ::tkchat::nickIsNoisy { nick } {
    variable noisyUsers

    if { [info exists noisyUsers($nick)] } {
	if { [clock seconds] < $noisyUsers($nick) } {
	    return 1
	} else {
	    addSystem .txt \
		    "$nick is no longer considered noisy (timeout expired)."
	    unset noisyUsers($nick)
	    return 0
	}
    }
    return 0
}

# -------------------------------------------------------------------------

# Tk 8.5a2+ can now do a global transparency on supported platforms (Win2K
# and WinXP.
# n must be from 1 to 100.
#
proc ::tkchat::SetAlpha {n} {
    global Options
    if {[lsearch [wm attributes .] -alpha] != -1} {
	if {$n < 1} {set n 1}
	if {$n > 100} {set n 100}
	set Options(Transparency) $n
	wm attributes . -alpha [expr {$n / 100.0}]
	# Work around a bug when transitioning from opaque to
	# any transparent value the toplevel becomes topmost.
	#if {[winfo exists .options]} {raise .options}
    }
}

proc ::tkchat::FadeAlpha {} {
    global Options
    if {$Options(AutoFade)} {
	variable FadeId
	set alpha [wm attributes . -alpha]
	if {($alpha * 100) > $Options(AutoFadeLimit)} {
	    wm attributes . -alpha [expr {$alpha - 0.01}]
	    set FadeId [after 200 [namespace origin FadeAlpha]]
	}
    }
}

proc ::tkchat::FadeCancel {} {
    global Options
    if {$Options(AutoFade) == 0} {
	set n [expr {$Options(Transparency) / 100.0}]
	after idle [list wm attributes . -alpha $n]
    } else {
	variable FadeId
	catch {after cancel $FadeId}
	unset -nocomplain FadeId
	catch {wm attributes . -alpha 0.999}
    }
}

proc ::tkchat::FocusInHandler {w args} {
    FadeCancel
}
proc ::tkchat::FocusOutHandler {w args} {
    if {[string length [focus]] == 0} {
	after idle [namespace origin FadeAlpha]
    }
}

proc ::tkchat::EditOptions {} {
    global Options
    variable NS
    variable useTile

    variable EditOptions
    array set EditOptions {Result -1}
    set EditOptions(Browser) $Options(Browser)
    set EditOptions(BrowserTab) $Options(BrowserTab)

    set EditOptions(Style)         $Options(Style)
    set EditOptions(AutoFade)      $Options(AutoFade)
    set EditOptions(AutoFadeLimit) $Options(AutoFadeLimit)
    set EditOptions(Transparency)  $Options(Transparency)

    if {[winfo exists .options]} {destroy .options}
    set dlg [toplevel .options -class Dialog]
    wm withdraw $dlg
    wm title $dlg "Tkchat Options"

    set f [${NS}::frame $dlg.base -borderwidth 0]
    if {$useTile} {
        set af [${NS}::labelframe $f.af -text "General" -underline 1]
    } else { 
        set af [${NS}::labelframe $f.af -text "General"]
    }
    ${NS}::checkbutton $af.store -text "Store private messages" \
        -variable ::Options(StoreMessages) -onvalue 1 -offvalue 0 \
        -underline 0
    pack $af.store -side top -fill x -expand 1
    bind $dlg <Alt-e> [list focus $af]
    bind $dlg <Alt-s> [list $af.store invoke]

    set bf [${NS}::labelframe $f.bf -text "Preferred browser" ]

    ${NS}::label $bf.m -anchor nw -font FNT -wraplength 4i -justify left \
	-text "Provide the command used to launch your web browser. For\
	instance /opt/bin/mozilla or xterm -e links. The URL to\
	be opened will be appended to the command string and for\
	mozilla-type browsers we will call the -remote option to\
	try to use a previously existing browser."
    ${NS}::entry $bf.e -textvariable ::tkchat::EditOptions(Browser)
    ${NS}::button $bf.b -text "..."  -command {
	if {[set file [tk_getOpenFile]] != {}} {
	    set ::tkchat::EditOptions(Browser) $file
	}
    }
    ${NS}::checkbutton $bf.tab -text "Force new Tab, if possible (Unix only)" \
	-variable ::tkchat::EditOptions(BrowserTab) -underline 0

    grid $bf.m - -sticky news -padx 2
    grid $bf.e $bf.b -sticky ew -padx 2
    grid $bf.tab -sticky ew -padx 2 -columnspan 2
    grid rowconfigure $bf 0 -weight 1
    grid columnconfigure $bf 0 -weight 1

    set sf [${NS}::labelframe $f.sf -text "Tk style"] ;#-padx 1 -pady 1]
    set gf [${NS}::labelframe $f.gf -text "Gimmiks"] ;#  -padx 1 -pady 1]

    ${NS}::label $sf.m -anchor nw -font FNT -wraplength 4i -justify left \
	-text "The Tk style selection available here will apply when you \
	   next restart tkchat."
    ${NS}::radiobutton $sf.as -text "ActiveState" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value as_style
    ${NS}::radiobutton $sf.gtk -text "GTK look" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value gtklook
    ${NS}::radiobutton $sf.any -text "Any" -underline 1 \
	-variable ::tkchat::EditOptions(Style) -value any
    ${NS}::radiobutton $sf.def -text "Tk default" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value tk

    if {[catch {package require style::as}]
	&& [catch {package require as::style}]} {
	$sf.as configure -state disabled
    }

    bind $dlg <Alt-a> [list $sf.as invoke]
    bind $dlg <Alt-g> [list $sf.gtk invoke]
    bind $dlg <Alt-n> [list $sf.any invoke]
    bind $dlg <Alt-t> [list $sf.def invoke]

    grid $sf.m - - - -sticky news -padx 2
    grid $sf.as $sf.gtk $sf.any $sf.def -sticky ew -padx 2
    grid rowconfigure $bf 0 -weight 1
    grid columnconfigure $bf 0 -weight 1

    # Gimmicks section.
    set gimmicks 0
    if {[lsearch [wm attributes .] -alpha] != -1} {
	set gimmicks 1
	${NS}::checkbutton $gf.fade -text "When not active, fade to " -underline 2 \
	    -variable ::tkchat::EditOptions(AutoFade)
	spinbox $gf.fadelimit -from 1 -to 100 -width 4 \
	    -textvariable ::tkchat::EditOptions(AutoFadeLimit)
	${NS}::label $gf.pct -text "%"
	${NS}::label $gf.alabel -text Transparency
	${NS}::scale $gf.alpha -from 1 -to 100 -orient horizontal
	$gf.alpha set $EditOptions(Transparency)
	#[expr {int([wm attributes . -alpha] * 100)}]
	$gf.alpha configure -command [namespace origin SetAlpha]

	bind $dlg <Alt-h> [list $gf.face invoke]
	bind $dlg <Alt-r> [list focus $gf.alpha]

	grid $gf.fade   - $gf.fadelimit $gf.pct x -sticky w -padx 2
	grid $gf.alabel $gf.alpha - - - -sticky we -padx 2
	grid configure $gf.alabel -pady {20 0} -sticky w
	grid columnconfigure $gf 4 -weight 1
    }

    ${NS}::button $f.ok -text OK -underline 0 -default active \
	-command [list set ::tkchat::EditOptions(Result) 1]
    ${NS}::button $f.cancel -text Cancel -underline 0 \
	-command [list set ::tkchat::EditOptions(Result) 0]

    grid $af - -sticky news -padx 2 -pady 2
    grid $bf - -sticky news -padx 2 -pady 2
    grid $sf - -sticky news -padx 2 -pady 2
    if {$gimmicks} {
	grid $gf - -sticky news -padx 2 -pady 2
    }
    grid $f.ok $f.cancel -sticky e
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 0 -weight 1

    pack $f -side top -fill both -expand 1

    bind $dlg <Return> [list $f.ok invoke]
    bind $dlg <Escape> [list $f.cancel invoke]
    bind $dlg <Alt-o>  [list focus $f.ok]
    bind $dlg <Alt-c>  [list focus $f.cancel]
    focus $bf.e

    wm resizable $dlg 0 0
    catch {::tk::PlaceWindow $dlg widget .}
    wm deiconify $dlg

    tkwait variable ::tkchat::EditOptions(Result)

    if {$EditOptions(Result) == 1} {
	set Options(Browser) $EditOptions(Browser)
	set Options(BrowserTab) $EditOptions(BrowserTab)
	foreach property {Style AutoFade AutoFadeLimit} {
	    if { $Options($property) ne $EditOptions($property) } {
		set Options($property) $EditOptions($property)
	    }
	}
    } else {
	# This one is the reverse of the other dialog properties. In this case
	# the Options copy is the one always updated and the EditOptions value
	# is the backup.
	set Options(Transparency) $EditOptions(Transparency)
    }

    destroy $dlg
    unset EditOptions
}

# -------------------------------------------------------------------------

# Try and adjust the Tk style.
# If we can find the ActiveState look&feel package then lets use that
# otherwise we have something that was modified from the Gtklook page
# of the wiki: http://mini.net/tcl/gtklook
#
proc ::tkchat::GtkLookStyleInit {} {
    set defaultColor #dcdad5
    set activeFG     #ffffff
    set activeBG     #4a6984
    set troughColor  #bdb6ad

    set families [font families]
    set family ""
    foreach test [list "Bitstream Vera Sans" "FreeSans"] {
        set ndx [lsearch -exact $families $test]
        if {$ndx == -1} {
            set ndx [lsearch -exact $families [string tolower $test]]
        }
        if {$ndx != -1} {
            set family [lindex $families $ndx]
            break
        }
    }
    if {$family eq ""} {set family Helvetica}
    set size 12
    catch {
        if {[string equal [tk::pkgconfig get fontsystem] "xft"]} {
            incr size -4
        }
    }
    font create GtkLookFont \
	-family $family -size $size -weight normal
    font create GtkLookDialogFont \
	-family $family -size [incr size 4] -weight bold -slant italic

    option add *background $defaultColor widgetDefault
    option add *borderWidth 1 widgetDefault
    option add *highlightThickness 0 widgetDefault
    option add *troughColor $troughColor widgetDefault
    option add *activeBorderWidth 1 widgetDefault
    option add *selectBorderWidth 1 widgetDefault
    option add *font GtkLookFont widgetDefault

    option add *Button.highlightThickness 1 widgetDefault
    option add *Checkbutton.highlightThickness 1 widgetDefault
    option add *Radiobutton.highlightThickness 1 widgetDefault

    option add *Listbox.background white widgetDefault
    option add *Listbox.selectBorderWidth 0 widgetDefault
    option add *Listbox.selectForeground $activeFG widgetDefault
    option add *Listbox.selectBackground $activeBG widgetDefault

    option add *Entry.background white
    option add *Entry.foreground black
    option add *Entry.selectBorderWidth 0
    option add *Entry.selectForeground $activeFG
    option add *Entry.selectBackground $activeBG

    option add *Text.background white
    option add *Text.selectBorderWidth 0
    option add *Text.selectForeground $activeFG
    option add *Text.selectBackground $troughColor

    option add *Menu.activeBackground $activeBG
    option add *Menu.activeForeground $activeFG
    option add *Menu.activeBorderWidth 0
    option add *Menu.highlightThickness 1
    option add *Menu.borderWidth 2

    option add *Menubutton.activeBackground $activeBG
    option add *Menubutton.activeForeground $activeFG
    option add *Menubutton.activeBorderWidth 0
    option add *Menubutton.highlightThickness 0
    option add *Menubutton.borderWidth 0

    option add *Labelframe.borderWidth 2
    option add *Frame.borderWidth 2

    option add *Dialog.msg.font GtkLookDialogFont
}
# -------------------------------------------------------------------------
# Whiteboard

proc tkchat::whiteboard_eval { wbitem color } {

    if { ![winfo exists .wb]  } {
	if { !$::Options(EnableWhiteboard) } {
	    return
	}
	whiteboard_open
    }

    set ::wbentry $wbitem

    catch {
	interp eval .wbinterp $::wbentry
    }
}

proc tkchat::whiteboard_transmit {w id} {
    set attrs [list xmlns urn:tkchat:whiteboard color $::Options(MyColor)]

    set wbitem [list .wb.c create line]
    foreach c [$w coords $id] {
	lappend wbitem [expr { int(round($c)) }]
    }
    set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $wbitem]]

    $tkjabber::jabber send_message $tkjabber::conference -type groupchat -xlist $xlist

    .wb.e selection range 0 end
}

proc tkchat::whiteboard_clear {} {
    set attrs [list xmlns urn:tkchat:whiteboard color $::Options(MyColor)]

    set wbitem ".wb.c delete all"

    set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $wbitem]]

    $tkjabber::jabber send_message $tkjabber::conference -type groupchat -xlist $xlist

    .wb.e selection range 0 end

}

proc tkchat::whiteboard_open {} {
    variable useTile
    variable NS

    if { ![winfo exists .wb] } {
	set wb [toplevel .wb]

	${NS}::entry $wb.e -textvar ::wbentry -width 80
	if {$useTile == 0} { $wb.e configure -background white }
	bind $wb.e <Return> {interp eval .wbinterp $::wbentry}
	set white_board [canvas $wb.c -background white -width 350 -height 300]
	${NS}::button $wb.bclear -text "clear" -command ::tkchat::whiteboard_clear
	bind $wb.c <Button-1> {
	    set entry ""
	    set id [%W create line %x %y %x %y]
	}
	bind $wb.c <Button1-Motion> \
		{%W coords $id [concat [%W coords $id] %x %y]}
	bind $wb.c <ButtonRelease-1> {::tkchat::whiteboard_transmit %W $id}
	grid $wb.e $wb.bclear -sticky new
	grid $wb.c - -sticky new
	#pack $wb.e $wb.c -fill both -expand 1

	catch {
	    interp create -safe .wbinterp
	    interp alias .wbinterp .wb.c {} .wb.c
	}
    } else {
	focus .wb
    }
}

proc ::tkchat::ConsoleInit {} {
    #####
	 #
	 # "console for Unix"
	 # http://wiki.tcl.tk/786
	 #
	 # Tcl code harvested on:   2 Mai 2006, 20:16 GMT
	 #
	 #       Provides a console window.
	 #
	 # Last modified on: $Date: 2007/03/31 14:58:31 $
	 # Last modified by: $Author: patthoyts $
	 #
	 # This file is evaluated to provide a console window interface to the
	 # root Tcl interpreter of an OOMMF application.  It calls on a script
	 # included with the Tk script library to do most of the work, making use
	 # of Tk interface details which are only semi-public.  For this reason,
	 # there is some risk that future versions of Tk will no longer support
	 # this script.  That is why this script has been isolated in a file of
	 # its own.

	 set _ [file join $::tk_library console.tcl]
	 if {![file readable $_]} {
   	  return -code error "File not readable: $_"
	 }

	 ########################################################################
	 # Provide the support which the Tk library script console.tcl assumes
	 ########################################################################
	 # 1. Create an interpreter for the console window widget and load Tk
	 set consoleInterp [interp create]
	 $consoleInterp eval [list set ::tk_library $::tk_library]
	 $consoleInterp alias exit ::console hide
	 load "" Tk $consoleInterp

	 # 2. A command 'console' in the application interpreter
	 proc ::console {sub {optarg {}}} [subst -nocommands {
   	  switch -exact -- \$sub {
      	  title {
         		$consoleInterp eval wm title . [list \$optarg]
      	  }
      	  hide {
         		$consoleInterp eval wm withdraw .
      	  }
      	  show {
         		$consoleInterp eval wm deiconify .
      	  }
      	  eval {
         		$consoleInterp eval \$optarg
      	  }
      	  default {
         		error "bad option \\\"\$sub\\\": should be hide, show, or title"
      	  }
   	 }
	 }]

	 # 3. Alias a command 'consoleinterp' in the console window interpreter
	 #       to cause evaluation of the command 'consoleinterp' in the
	 #       application interpreter.
	 proc ::consoleinterp {sub cmd} {
   	 switch -exact -- $sub {
      	  eval {
         		uplevel #0 $cmd
      	  }
      	  record {
         		history add $cmd
         		catch {uplevel #0 $cmd} retval
         		return $retval
      	  }
      	  default {
         		error "bad option \"$sub\": should be eval or record"
      	  }
   	 }
	 }
	 $consoleInterp alias consoleinterp consoleinterp

	 # 4. Bind the <Destroy> event of the application interpreter's main
	 #    window to kill the console (via tkConsoleExit)
	 bind . <Destroy> [list +if {[string match . %W]} [list catch \
      	  [list $consoleInterp eval tkConsoleExit]]]

	 # 5. Redefine the Tcl command 'puts' in the application interpreter
	 #    so that messages to stdout and stderr appear in the console.
	 rename ::puts ::tcl_puts
	 proc ::puts {args} [subst -nocommands {
   	 switch -exact -- [llength \$args] {
      	  1 {
         		if {[string match -nonewline \$args]} {
               	 if {[catch {uplevel 1 [linsert \$args 0 tcl_puts]} msg]} {
                  	  regsub -all tcl_puts \$msg puts msg
                  	  return -code error \$msg
               	 }
         		} else {
               	 $consoleInterp eval [list tkConsoleOutput stdout \
                     		"[lindex \$args 0]\n"]
         		}
      	  }
      	  2 {
         		if {[string match -nonewline [lindex \$args 0]]} {
               	 $consoleInterp eval [list tkConsoleOutput stdout \
                     		[lindex \$args 1]]
         		} elseif {[string match stdout [lindex \$args 0]]} {
               	 $consoleInterp eval [list tkConsoleOutput stdout \
                     		"[lindex \$args 1]\n"]
         		} elseif {[string match stderr [lindex \$args 0]]} {
               	 $consoleInterp eval [list tkConsoleOutput stderr \
                     		"[lindex \$args 1]\n"]
         		} else {
               	 if {[catch {uplevel 1 [linsert \$args 0 tcl_puts]} msg]} {
                  	  regsub -all tcl_puts \$msg puts msg
                  	  return -code error \$msg
               	 }
         		}
      	  }
      	  3 {
         		if {![string match -nonewline [lindex \$args 0]]} {
               	 if {[catch {uplevel 1 [linsert \$args 0 tcl_puts]} msg]} {
                  	  regsub -all tcl_puts \$msg puts msg
                  	  return -code error \$msg
               	 }
         		} elseif {[string match stdout [lindex \$args 1]]} {
               	 $consoleInterp eval [list tkConsoleOutput stdout \
                     		[lindex \$args 2]]
         		} elseif {[string match stderr [lindex \$args 1]]} {
               	 $consoleInterp eval [list tkConsoleOutput stderr \
                     		[lindex \$args 2]]
         		} else {
               	 if {[catch {uplevel 1 [linsert \$args 0 tcl_puts]} msg]} {
                  	  regsub -all tcl_puts \$msg puts msg
                  	  return -code error \$msg
               	 }
         		}
      	  }
      	  default {
         		if {[catch {uplevel 1 [linsert \$args 0 tcl_puts]} msg]} {
               	 regsub -all tcl_puts \$msg puts msg
               	 return -code error \$msg
         		}
      	  }
   	 }
	 }]
	 $consoleInterp alias puts puts

	 # 6. No matter what Tk_Main says, insist that this is an interactive  shell
	 set ::tcl_interactive 1

	 ########################################################################
	 # Evaluate the Tk library script console.tcl in the console interpreter
	 ########################################################################
	 $consoleInterp eval source [list [file join $::tk_library console.tcl]]
	 $consoleInterp eval {
   	 if {![llength [info commands ::tkConsoleExit]]} {
      	  tk::unsupported::ExposePrivateCommand tkConsoleExit
   	 }
	 }
	 $consoleInterp eval {
   	 if {![llength [info commands ::tkConsoleOutput]]} {
      	  tk::unsupported::ExposePrivateCommand tkConsoleOutput
   	 }
	 }
	 if {[string match 8.3.4 $::tk_patchLevel]} {
   	 # Workaround bug in first draft of the tkcon enhancments
   	 $consoleInterp eval {
      	  bind Console <Control-Key-v> {}
   	 }
	 }
	 # Restore normal [puts] if console widget goes away...
	 proc ::Oc_RestorePuts {slave} {
   	  rename ::puts {}
   	  rename ::tcl_puts ::puts
   	  interp delete $slave
	 }
	 $consoleInterp alias Oc_RestorePuts Oc_RestorePuts $consoleInterp
	 $consoleInterp eval {
   	  bind Console <Destroy> +Oc_RestorePuts
	 }

	 unset consoleInterp
	 ::console title "[wm title .] Console"
	 ::console hide
}


# -------------------------------------------------------------------------
# Jabber handling

namespace eval tkjabber {
    proc Variable {args} {
	if {[llength $args] % 2} {
	    variable [lindex $args end]
	    set args [lrange $args 0 end-1]
	}
	foreach {var val} $args {
	    variable $var
	    if {![info exists $var]} {
		set $var $val
	    }
	}
    }
    Variable jabber ; if {![info exists jabber]} {set jabber ""}
    Variable topic
    Variable muc
    Variable nickTries 0 ;# The number of times I tried to solve a nick conflict
    Variable baseNick "" ;# used when trying to solve a nick conflict.
    Variable grabNick "" ;# grab this nick when it becomes available.

    Variable ignoreNextNick ""
    # If the next entry is by this nick, don't display it (for nick changes.)

    Variable roster ""
    Variable browser ""
    Variable socket ""
    Variable conn
    Variable myId ""
    Variable RunRegistration 0
    Variable reconnect 0 ;# set to 1 after a succesful connect.
    # retrytime in seconds, distributed so not everyone tries at the same time.
    Variable connectionRetryTime [expr {int(5+rand()*5.0)}]
    Variable reconnectTimer {}

    Variable HistoryLines {}
    Variable HaveHistory 0
    Variable LastMessage 0 ;# used for reconnects when asking for conference history.

    Variable conference

    Variable muc_jid_map ;# array with conference-id to user-jid map.
    Variable users ;#
    Variable user_alias
    Variable Away 0
    Variable AutoAway 0
    Variable AwayStatus ""

    # To provide a map between parents widgets and chats
    variable ChatWindows; if {![info exists ChatWindows]} {array set ChatWindows {counter 0}}
}

# Login:
proc ::tkjabber::connect {} {
    global Options
    variable jabber
    variable roster
    variable browser
    variable socket
    variable reconnect
    variable conference
    variable reconnectTimer
    variable have_tls

    if { $reconnectTimer ne "" } {
	after cancel $reconnectTimer
	set reconnectTimer ""
    }

    set conference $Options(JabberConference)

    if {$Options(UseProxy) && [string length $Options(ProxyHost)] > 0} {
	set keepalive_seconds 30
    } else {
	set keepalive_seconds 90
    }

    if { !$reconnect } {
	if { $roster eq "" } {
	    set roster [::roster::roster ::tkjabber::RosterCB]
	}
	set jabber [::jlib::new $roster ::tkjabber::ClientCB \
		-iqcommand ::tkjabber::IqCB \
		-messagecommand ::tkjabber::MsgCB \
		-presencecommand ::tkjabber::PresCB \
		-keepalivesecs $keepalive_seconds]

	set browser [::browse::new $jabber -command ::tkjabber::BrowseCB]

	# override the jabberlib version info query
	::jlib::iq_register $jabber get jabber:iq:version \
	    [namespace origin on_iq_version] 40
	::jlib::iq_register $jabber get jabber:iq:last \
	    [namespace origin on_iq_last] 40
	::jlib::iq_register $jabber result jabber:iq:version \
	    [namespace origin on_iq_version_result] 40
    }

    set have_tls [expr {[package provide tls] != {}}]
    set socketCmd [info command ::socket]
    if {[llength [package provide Iocpsock]] > 0} {
        set socketCmd ::socket2 
        if {$have_tls} {set ::tls::socketCmd [info command ::socket2]}
    }
    if { [catch {
	if { $Options(UseProxy) && [string length $Options(ProxyHost)] > 0 } {
	    set socket [ProxyConnect $Options(ProxyHost) $Options(ProxyPort) \
		    $Options(JabberServer) $Options(JabberPort)]
	} elseif { $have_tls && $Options(UseJabberSSL) eq "ssl" } {
	    set socket \
		    [tls::socket $Options(JabberServer) $Options(JabberPort)]
	} else {
	    if { $Options(JabberPort) == 5223 } {
		incr Options(JabberPort) -1
	    }
	    if { [info exists Options(JabberConnect)] \
		    && $Options(JabberConnect) ne ""} {
		foreach {srv prt} [split $Options(JabberConnect) :] break
		if { $prt eq "" } {
		    set prt $Options(JabberPort)
		}
		set socket [$socketCmd $srv $prt]
	    } else {
		set socket [$socketCmd $Options(JabberServer) $Options(JabberPort)]
	    }
	}
    } res] } {
	# Connection failed.
	::tkchat::addStatus 0 "Connecting failed: $res" end ERROR
	if { $reconnect } {
	    scheduleReconnect
	} else {
	tk_messageBox \
		-icon error \
		-title "Connection Failure" \
		-message "Connecting failed: $res"
	}
	return
    } else {
	$jabber setsockettransport $socket
	openStream
    }

    # The next thing which will/should happen is the a call to ConnectProc by
    # jabberlib.
    .mbar.file entryconfigure 0 -label [::msgcat::mc Logout]
    set ::tkchat::LoggedIn 1
}

proc tkjabber::disconnect {} {
    variable socket
    variable reconnect
    variable reconnectTimer

    set reconnect 0
    if { $reconnectTimer ne "" } {
	after cancel $reconnectTimer
	set reconnectTimer ""
    }

    if { $socket eq "" } {
	return
    }

    cleanup
    tkchat::addStatus 0 "Disconnected from jabber server."
    tkchat::addStatus 1 "not connected"
}

proc ::tkjabber::cleanup {} {
    variable jabber
    variable muc
    variable conference
    variable socket
    variable roster
    variable baseNick
    variable ::tkchat::OnlineUsers

    if {[info exists muc]} {
	if {[catch {$muc exit $conference} err]} {
	    ::log::log error "cleanup: $err"
	}
    }

    if {[info exists roster]} {
	if {[catch {$roster reset} err]} {
	    ::log::log error "cleanup: $err"
	}
    }
    foreach network $OnlineUsers(networks) {
	array unset OnlineUsers $network-*
	set OnlineUsers($network) {}
    }
    ::tkchat::updateOnlineNames

    if { [catch {$jabber closestream}] } {
	::log::log error "Closestream: $::errorInfo"
    }

    catch {jlib::resetsocket $jabber}
    set socket ""
    ::tkchat::setNickname $baseNick
    .mbar.file entryconfigure 0 -label [::msgcat::mc Login]
    set ::tkchat::LoggedIn 0
}

proc tkjabber::openStream {} {
    variable socket
    variable jabber
    global Options
    ::log::log debug "OPENSTREAM to $Options(JabberServer) on $socket"

    $jabber openstream $Options(JabberServer) \
	-cmd [namespace current]::ConnectProc \
	-socket $socket \
	-version 1.0
}

proc tkjabber::ConnectProc {jlibName args} {
    global Options
    variable conn
    variable jabber
    variable have_tls

    ::log::log debug "ConnectProc args '$args'"

    array set conn $args
    tkchat::addStatus 0 "Connected to $conn(from), sending credentials."
    update idletasks

    # Now send authentication details:
    if {$have_tls && $Options(UseJabberSSL) eq "starttls"} {
	jlib::starttls $jabber [namespace origin OnStartTlsFinish]
    } else {
	SendAuth
    }
}

proc tkjabber::OnStartTlsFinish {jlib type args} {
    ::log::log debug "starttls: $jlib $type $args"
    SendAuth
}

proc tkjabber::SendAuth {} {
    # This proc is called by ConnectProc after openstream succeeded.

    global Options
    variable jabber
    variable myId
    variable socket

    CheckCertificate

    fconfigure $socket -encoding utf-8; # this is quite important.

    set user $Options(Username)
    set pass $Options(Password)
    set ress $Options(JabberResource)

    if {[info command ::jlib::havesasl] ne "" && [::jlib::havesasl]} {
	jlib::auth_sasl $jabber $user $ress $pass \
	    [namespace origin OnSaslFinish]
    } else {
	SendAuthOld
    }
}

proc tkjabber::CheckCertificate {} {
    # Check SSL certificate information (may be none if not SSL socket)
    variable socket
    variable have_tls
    variable cert
    if {$have_tls} {
        if {[catch {
            set info [tls::status $socket]
            array set cert {notAfter 0 subject "" issuer ""}
            foreach {key val} $info {
                set cert($key) [encoding convertfrom utf-8 $val]
            }
            set self_signed [string equal $cert(subject) $cert(issuer)]
            set life [expr {[clock scan $cert(notAfter)] - [clock seconds]}]
            if {$self_signed} {
                tkchat::addSystem .txt \
                    "Self-signed certificate issued by $cert(issuer)"
            }
            if {$life < 1} {
                tkchat::addSystem .txt \
                    "SSL certificate expired on $cert(notAfter)" end ERROR
            }
            array set O [split [string trim $cert(subject) /] "/,="]
            array set I [split [string trim $cert(issuer) /] "/,="]
            if {[info exists O(CN)]} {
                tkchat::addStatus SSL $O(CN)
            }
            .status.ssl configure -image ::tkchat::img::link_secure
            if {[info exists I(O)] 
                && [llength [package provide tooltip]] > 0} {
                set tip "Authenticated by $I(O)"
                tooltip::tooltip .status.ssl $tip
                bind .status.ssl <Double-Button-1> \
                    [list tkchat::ShowCertificate [array get cert]]
            }
        } err]} {
            log::log notice "SSL Warning: $err"
        }
    }
}

proc ::tkjabber::OnSaslFinish {jlib type args} {
    ::log::log debug "OnSaslFinish $type $args"
    if {$type eq "error"} {
	# try using the non-sasl login
	SendAuthOld
    } else {
	update idletasks
	::log::log debug "Calling login callback..."
	LoginCB $jlib $type $args
    }
}

proc tkjabber::SendAuthOld {} {
    global Options
    variable conn
    variable jabber
    variable myId

    set user $Options(Username)
    set pass $Options(Password)
    set ress $Options(JabberResource)

    set myId [$jabber send_auth $user $ress \
		  [namespace origin LoginCB] \
		  -digest [sha1::sha1 $conn(id)$pass]]
    ::log::log debug "SendAuth: Logging in as $myId"

    update idletasks
    # The next callback is the LoginCB
}

# Jabber callback procs - this is where we get messages from.

# The roster stuff...
proc ::tkjabber::RosterCB { rostName what {jid {}} args } {
    global Options
    variable conference
    variable grabNick
    variable ignoreNextNick
    variable jabber
    variable ::tkchat::OnlineUsers

    ::log::log debug "--roster-> what=$what, jid=$jid, args='$args'"

    switch -- $what {
	presence {
	    set p(-x) {}
	    array set p $args
	    set action ""
	    set newnick ""
	    set nick $p(-resource)
	    # online/away/offline, etc.
	    set status [list online]
	    if { [info exists p(-show)] } {
		set status [list $p(-show)]
	    }
	    if { [info exists p(-status)] } {
		lappend status $p(-status)
	    }
	    switch -- $p(-type) {
		available {
		    set action entered

		    # Get IrcUserList from ijchain
		    if { $nick eq "ijchain" } {
			$jabber send_message $p(-from) -subject IrcUserList
		    }

		    # Add the user's nick into a nick/jid map
		    foreach child $p(-x) {
			set ns [::wrapper::getattribute $child xmlns]
			if { "http://jabber.org/protocol/muc#user" eq $ns } {
			    set item [::wrapper::getchildswithtag $child item]
			    if { [llength $item] > 0 } {
				if { [info exists \
					OnlineUsers(Jabber-$nick,jid)] } {
				    set action availability
				}
				set usrjid [::wrapper::getattribute \
					[lindex $item 0] jid]
				set OnlineUsers(Jabber-$nick,jid) $usrjid
				set OnlineUsers(Jabber-$nick,status) $status
                                set OnlineUsers(Jabber-$nick,role) \
                                    [::wrapper::getattribute [lindex $item 0] role]
			    }
			    break
			}
		    }
		}
		unavailable {
		    set action left
		    set status offline

		    # Check for nickname change
		    foreach child $p(-x) {
			set ns [::wrapper::getattribute $child xmlns]
			if { "http://jabber.org/protocol/muc#user" eq $ns } {
			    set status_elem \
				    [::wrapper::getchildswithtag $child status]
			    if { [llength $status_elem]==0 } {
				# Not a nickname change.
				continue
			    }
			    set status_code [::wrapper::getattribute \
				    [lindex $status_elem 0] code]
			    if { $status_code eq "303" } {
				# nickname change!
				set item [::wrapper::getchildswithtag \
					$child item]
				if { [llength $item] > 0 } {
				    set action nickchange
				    set newnick [::wrapper::getattribute \
					    [lindex $item 0] nick]
				    break
				}
			    }
			}
		    }
		    unset -nocomplain OnlineUsers(Jabber-$nick,jid)

		    # Check for chat windows for a departing user.
		    variable ChatWindows
		    array set a {-from {}}
		    array set a $args
		    if {[info exists ChatWindows(txt.$a(-from))]} {
			tkchat::addSystem $ChatWindows(txt.$a(-from)) \
				"The other user has disconnected."
		    }

		    # Do we want to be this nick?
		    if { $grabNick ne "" && $nick eq $grabNick } {
			after idle [list ::tkjabber::setNick $grabNick]
			set grabNick ""
		    }
		}
	    }

	    if { $jid ne $conference } {
		set tstatus [string map {
		    dnd "do not disturb"
		    xa "away (idle)"
		    chat "I want to chat"
		    away "away"
		} [lindex $status 0]]
		set msg "$jid has $action ($tstatus)"
		if { [llength $status] > 1 } {
		    append msg ": [lindex $status 1]"
		}
		::tkchat::addSystem .txt $msg end AVAILABILITY
		return
	    }

	    # Much more interesting info available in array ...

	    if { $action eq "nickchange" } {
		lappend action Jabber
		::tkchat::addTraffic .txt [list $nick $newnick] $action end 0
		set ignoreNextNick $newnick
	    } elseif { $action eq "availability" } {
		set msg [lindex $status 0]
		lset status 0 [string map {xa idle chat talking dnd busy} $msg]
		lappend action Jabber
		::tkchat::addTraffic .txt [list $nick $status] $action end 0
	    } else {
		if { !($action eq "entered" && $ignoreNextNick eq $nick) } {
		    # if not the ignore nick:
		    lappend action Jabber
		    ::tkchat::addTraffic .txt $nick $action end 0
		} else {
		    ::tkchat::updateOnlineNames
		}
		# Always reset ignoreNextNick!
		set ignoreNextNick ""
	    }
	}
	default {
	    ::tkchat::addSystem .txt \
		    "--roster-> what=$what, jid=$jid, args='$args'"
	}
    }
}

# Browse stuff...
proc tkjabber::BrowseCB {browseName type jid xmllist args} {
    tkchat::addSystem .txt "--browse-> browseName=$browseName type=$type, jid=$jid, xmllist='$xmllist', args='$args'"
}
proc tkjabber::BrowseErrorCB {browseName what jid errlist} {
    tkchat::addSystem .txt "--browse-(error)-> what=$what, jid=$jid, errlist='$errlist'"
}

# The jabberlib stuff...
proc tkjabber::ClientCB {jlibName cmd args} {

    ::log::log debug "ClientCB: jlibName=$jlibName, cmd=$cmd, args='$args'"
    switch -- $cmd {
	connect {
            # We must update the conn(id) item here with the new stream id
            variable conn
            set conn(from) [$jlibName getstreamattr from]
            set conn(id) [$jlibName getstreamattr id]
            set conn(version) [$jlibName getstreamattr version]
            set conn(xmlns) [$jlibName getstreamattr xmlns]
	    tkchat::addStatus 0 "Connection to Jabber Server Established"
            tkchat::addStatus 1 "connected"
	}
	disconnect {
            tkchat::addStatus 1 "not connected"
	    cleanup
	    scheduleReconnect
	}
	networkerror {
	    array set x {-body ""}
	    array set x $args
            tkchat::addStatus 1 "error"
	    tkchat::addSystem .txt "Network error $x(-body)"
	    cleanup
	    scheduleReconnect
	}
	streamerror {
	    array set x {-errormsg ""}
	    array set x $args
	    set type [lindex $x(-errormsg) 0]
	    set message [lindex $x(-errormsg) 1]
	    switch -- $type {
		conflict {
		    tkchat::addSystem .txt $message
		}
		default {
		    tkchat::addSystem .txt "ClientCB: $cmd ; args='$args'"
		}
	    }
            tkchat::addStatus 1 "error"
	    disconnect
	}
	default {
	    tkchat::addSystem .txt "ClientCB: jlibName=$jlibName, cmd=$cmd, args='$args'"
	}
    }
    update idletasks
}

proc tkjabber::IqCB {jlibName type args} {
    # These callbacks don't work. You should register an iq handler instead
    # - see the setup for on_iq_version.
    ::log::log debug "|| MyIqCB > type=$type, args=$args"
}

proc ::tkjabber::MsgCB {jlibName type args} {
    global Options
    variable conference
    variable muc
    variable topic
    variable LastMessage
    variable ::tkchat::OnlineUsers

    set LastMessage [clock seconds]

    ::log::log debug "|| MsgCB > type=$type, args=$args"

    set color ""
    set timestamp 0

    array set m $args
    if { [info exists m(-x)] } {
	foreach x $m(-x) {
	    switch [wrapper::getattribute $x xmlns] {
		"jabber:x:delay" {
		    set timestamp [clock scan \
			    [wrapper::getattribute $x stamp] -gmt 1]
		    if { $timestamp eq "" } {
			set timestamp 0
		    }
		}
		"urn:tkchat:chat" {
		    array set tkchatAttr [wrapper::getattrlist $x]
		    set color [wrapper::getattribute $x color]
		}
		"urn:tkchat:whiteboard" {
		    tkchat::whiteboard_eval [wrapper::getcdata $x] \
			    [wrapper::getattribute $x color]
		    return
		}
		"urn:tkchat:changenick" {
		    # Request for nick handover.
		    ::tkchat::addSystem .txt \
			    "$m(-from) has requested your nickname."
		    transferNick $m(-from)
		    return
		}
	    }
	}
    }

    switch -- $type {
	chat {
	    set from $m(-from)
	    set w .txt
	    if { [regexp {([^/]+)/(.+)} $m(-from) -> conf from] } {
		if { $conf eq $conference } {
		    set w [getChatWidget $m(-from) $from]
		} else {
		    regexp {^([^@]+)@} $m(-from) -> from
		    set w [getChatWidget $m(-from) $from]
		}
	    } else {
		regexp {^([^@]+)@} $m(-from) -> from
		set w [getChatWidget $m(-from) $from]
	    }
	    if {$w eq ".txt"} {
		::tkchat::addMessage $w $color $from " whispers: $m(-body)" \
			ACTION end $timestamp
	    } else {
		if { [string match -nocase "/me *" $m(-body)] } {
		    set m(-body) [string range $m(-body) 4 end]
		    set msgtype ACTION
		} else {
		    set msgtype NORMAL
                    if {[string match "Realname*" $m(-body)]} {
                        # We are handling IRC whois data - should do some 
                        # caching if we can get the nick (mod the bridge)
                    }
		}
		::tkchat::addMessage \
			$w $color $from $m(-body) $msgtype end $timestamp
	    }
	}
	groupchat {
	    set from $m(-from)
	    regexp {([^/]+)/(.+)} $m(-from) -> conf from
	    if { [info exists m(-subject)] } {
		# changing topic.
		variable ::tkchat::chatWindowTitle
		variable ::tkchat::MessageCounter

		set chatWindowTitle "The Tcler's Chat - $m(-subject)"
		if { $MessageCounter } {
		    wm title . "$MessageCounter - $chatWindowTitle"
		} else {
		    wm title . $chatWindowTitle
		}
		set msg " changed the topic to: $m(-subject)"
		if { [info exists m(-body)] } {
		    if { $from eq $conference } {
			::tkchat::addSystem .txt $m(-body)
		    } else {
			append msg "\n ... $m(-body)"
			::tkchat::addMessage \
				.txt $color $from $msg ACTION end $timestamp
		    }
		} else {
		    ::tkchat::addMessage .txt \
			    $color $from $msg ACTION end $timestamp
		}
	    } else {
		if { [info exists m(-body)] } {
		    parseMsg $from $m(-body) $color end $timestamp
		} else {
		    ::log::log notice "Unknown message from $from: '$args'"
		}
	    }
	}
	normal {
	    set from $m(-from)
            set conf [jid !resource $m(-from)]
            if {$conf eq $conference } {
		set from [jid resource $m(-from)]
	    }
	    if { $from eq "ijchain" && $m(-subject) eq "IrcUserList" } {
		foreach nick $m(-body) {
		    set OnlineUsers(IRC-$nick,status) [list online]
		    lappend OnlineUsers(IRC) $nick
		}
		set OnlineUsers(IRC) [lsort -dictionary -unique \
			$OnlineUsers(IRC)]
		::tkchat::updateOnlineNames
		return
	    }
            set msg {}
	    if { [info exists m(-subject)] } {
		lappend msg "Subject: $m(-subject)"
	    }
	    if { [info exists m(-body)] } {
		lappend msg "$m(-body)"
	    }
	    if { [llength $msg] > 0 } {
		set msg " whispers: [join $msg \n]"
		::tkchat::addMessage .txt "" $from $msg ACTION end 0
                StoreMessage $from \
                    [expr {[info exists m(-subject)] ? $m(-subject) : ""}] \
                    [expr {[info exists m(-body)] ? $m(-body) : ""}]
	    } else {
		::log::log notice "Unknown message from $from: '$args'"
	    }
	}
	error {
	    if { [info exists m(-error)] } {
		switch -- [lindex $m(-error) 0] {
		    404 {
			# This has been seen when sending a private message
			# to a disconnected user.
			::tkchat::addSystem .txt [concat \
				"Your message to $m(-from) could not be"
				"delivered. The recipient has disconnected"]
		    }
		    405 {
			if { [catch {
			    $muc exit $conference
			}] } {
			    ::log::log debug "MUC EXIT: $::errorInfo"
			}
			set msg [lindex $m(-error) 1]
			::tkchat::addSystem .txt \
				"$m(-from): $msg. Trying to get in again..."
			$muc enter $::tkjabber::conference \
				[xmlSafe $::Options(Nickname)] \
				-command ::tkjabber::MucEnterCB
		    }
		    default {
			::tkchat::addSystem .txt  "MsgCB (error) args='$args'"
		    }
		}
	    }
	}
	default {
	    ::tkchat::addSystem .txt "|| MsgCB > type=$type, args=$args"
	}
    }
}

proc ::tkjabber::parseMsg { nick msg color mark timestamp } {
    set msg [split $msg " "]
    set opts {}
    if { $nick eq "ijchain" } {
	set nick [lindex $msg 0]
	set msg [lrange $msg 1 end]
	if { $nick eq "***" } {
	    set nick [lindex $msg 0]
	    set action [lrange $msg 1 end]
	    if { $action eq "leaves" || $action eq "joins" } {
		set action [string map { joins entered leaves left } $action]
		lappend action IRC
	    } elseif { [lrange $action 0 end-1] eq "is now known as" } {
		lappend nick [lindex $action end]
		set action [list nickchange IRC]
	    } else {
		::log::log notice "Unknown IRC command '$msg'"
		return
	    }
	    ::tkchat::addTraffic .txt $nick $action $mark $timestamp
	    return
	} elseif { $nick eq "*" } {
	    set nick [lindex $msg 0]
	    set action [lrange $msg 1 end]
	    if { $action eq "entered" || $action eq "left" } {
		lappend action WebChat
		::tkchat::addTraffic .txt $nick $action $mark $timestamp
		return
	    } else {
		set msg [linsert $action 0 /me]
	    }
	}
    } elseif { $nick eq $::Options(JabberConference) } {
	::tkchat::addSystem .txt $msg
	return
    }
    if { [lindex $msg 0] eq "/nolog" } {
	set msg [lrange $msg 1 end]
	lappend opts nolog 1
    } elseif { [uplevel 1 { info exists tkchatAttr(nolog) }] \
	    && [uplevel 1 { set tkchatAttr(nolog) }] } {
	lappend opts nolog 1
    }
    if { $nick eq "" } {
	if { [lrange $msg 1 3] eq "has become available" } {
	    ::tkchat::addTraffic .txt [lindex $msg 0] entered $mark $timestamp
	} elseif { [string match "has left*" [lrange $msg 1 2]] } {
	    ::tkchat::addTraffic .txt [lindex $msg 0] left $mark $timestamp
	} elseif { [string match "is now known as" [lrange $msg 1 4]] } {
	    set nick [list [lindex $msg 0] [lindex $msg end]]
	    ::tkchat::addTraffic .txt $nick nickchange $mark $timestamp
	}
    } else {
	if { [lindex $msg 0] eq "/me" } {
	    set msg [join [lrange $msg 1 end]]
	    set msgtype ACTION
	} else {
	    set msg [join $msg]
	    set msgtype NORMAL
	}
	::tkchat::addMessage \
		.txt $color $nick $msg $msgtype $mark $timestamp $opts
    }
}

proc tkjabber::PresCB {jlibName type args} {
    ::log::log debug "|| PresCB > type=$type, args=$args"
    array set a {-from {} -to {} -status {}}
    array set a $args
    switch -exact -- $type {
	probe {
	    # We do not need to reply.
	}
	subscribe {
	    after idle [list [namespace origin SubscriptionRequest] \
			    $a(-from) $a(-status)]
	}
	default {
	    tkchat::addSystem .txt "Received $type presence message from $a(-from)."
	}
    }
}

proc tkjabber::httpCB { status message } {
    ::log::log debug "jabber-http $status : $message"
}

proc tkjabber::RegisterCB {jlibName type theQuery} {
    ::log::log debug "RegisterCB: type=$type, theQuery='$theQuery'"
    switch -- $type {
	result {
	    tkchat::addSystem .txt "Registered."
	    update idletasks
	    SendAuth
	}
	error {
	    set reason [lindex $theQuery 0]
	    if {$reason eq "internal-server-error"} {
		# We got here when the server was crashed but with the
		# jabber daemon still in memory. It would accept connections
		# but would not authenticate.
		#
		# FIX ME: We could go and lookup a wiki status page and
		# display that here.
	    }

	    set msg $theQuery
	    if {[llength $msg] > 1} {
		set msg [lindex $msg 1]
	    }
	    tkchat::addSystem .txt "Failed to register this account: $msg" end ERROR
	}
	default {
	    tkchat::addSystem .txt "MyRegisterProc: type=$type, theQuery='$theQuery'"
	}
    }
}

proc ::tkjabber::LoginCB { jlibname type theQuery } {
    # After SendAuth, this is the next Callback.
    global Options
    variable jabber
    variable roster
    variable conference
    variable muc
    variable baseNick
    variable nickTries
    variable myId

    ::log::log debug "LoginCB: type=$type, theQuery='$theQuery'"

    switch -- $type {
	error {
	    if { $theQuery eq "401 Unauthorized" } {
		if { ![tkchat::registerScreen] } {
		    return
		}

		set cmd [namespace current]::RegisterCB
		if {[info exists Options(Fullname)]} {
		    lappend cmd -name $Options(Fullname)
		}
		if {[info exists Options(Email)]} {
		    lappend cmd -email $Options(Email)
		}
		eval [linsert $cmd 0 $jabber \
			  register_set $Options(Username) $Options(Password)]

		tkchat::addSystem .txt "Registering username."
		update idletasks
		# the next step is in RegisterCB
	    } else {
		tkchat::addSystem .txt "LoginCB: type=$type, theQuery='$theQuery'"
	    }

	}
	result {
	    ::tkchat::addStatus 0 "Logged in."
	    if {$myId == {}} { set myId [$jabber myjid] }
	    set tkjabber::reconnect 1
	    set tkjabber::connectionRetryTime [expr {int(5+rand()*5.0)}]
	    $jabber send_presence
	    set muc [jlib::muc::new $jabber]
	    if { $::Options(Nickname) eq "" } {
		::tkchat::setNickname $::Options(Username)
	    }
	    set baseNick $::Options(Nickname)
	    set nickTries 0
	    $muc enter $conference [xmlSafe $::Options(Nickname)] \
		    -command ::tkjabber::MucEnterCB
	    # We are logged in. Now any of the callbacks can be called,
	    # Likely ones are MsgCB, MucEnterCB, RosterCB for normal traffic.
	}
	default {
	    tkchat::addSystem .txt "LoginCB: type=$type, theQuery='$theQuery'"
	}
    }
    return


    $jabber conference get_enter $conference [namespace current]::GenericIQProc
    set subelements [list [wrapper::createtag {nick} -chdata tkchat]]
    $jabber conference set_enter $conference $subelements [namespace current]::GenericIQProc

    tkchat::addSystem .txt "MyLoginProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::SearchGetProc {jlibName type theQuery} {
    tkchat::addSystem .txt "MySearchGetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::SearchSetProc {jlibName type theQuery} {
    tkchat::addSystem .txt "MySearchSetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::RosterResultProc {jlibName what} {
    tkchat::addSystem .txt  "MyRosterResultProc: what=$what"
}
proc tkjabber::VCardSetProc {jlibName type theQuery} {
    tkchat::addSystem .txt  "VCardSetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::VCardGetProc {jlibName type theQuery} {
    tkchat::addSystem .txt  "VCardGetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::GenericIQProc {jlibName type theQuery} {
    tkchat::addSystem .txt  "GenericIQProc: type=$type, theQuery='$theQuery'"
}
proc ::tkjabber::MucEnterCB { mucName type args } {
    variable jabber
    variable conference
    variable muc
    variable nickTries
    variable baseNick
    variable Away
    variable AwayStatus
    variable AutoAway

    ::log::log debug "MucEnter: type=$type, args='$args'"
    switch -- $type {
	error {
	    array set m $args
	    if { ![info exists m(-error)] } {
		::tkchat::addSystem .txt "MucEnter: type=$type, args='$args'"
		return
	    }
	    switch -- [lindex $m(-error) 0] {
		401 {
		    ::tkchat::addSystem .txt \
			    "This conference is password protected."
		}
		403 {
		    ::tkchat::addSystem .txt \
			    "You have been banned from this conference."
		}
		404 {
		    ::tkchat::addSystem .txt "This room does not exist."
		}
		405 {
		    ::tkchat::addSystem .txt [concat \
			    "The maximum number of users has been reached" \
			    "for this room."]
		}
		407 {
		    ::tkchat::addSystem .txt \
			    "You must be a member to enter this conference."
		}
		409 {
		    # Nick conflict. Try again?
		    incr nickTries
		    ::tkchat::addSystem .txt \
			    "The nick $::Options(Nickname) is in use."
		    if { $nickTries > 5 } {
			::tkchat::addSystem .txt [concat \
				"Unable to solve nick conflict, try setting" \
				"one with /nick <nickname> and then trying" \
				"again"]
		    } else {
			if { $nickTries < 3 } {
			    ::tkchat::setNickname "$::Options(Nickname)_"
			} else {
			    ::tkchat::setNickname "${baseNick}_$nickTries"
			}
			::tkchat::addSystem .txt \
			    "Trying to enter using $::Options(Nickname)."
			$muc enter $conference [xmlSafe $::Options(Nickname)] \
				-command ::tkjabber::MucEnterCB
		    }
		}
		default {
		    ::tkchat::addSystem .txt \
			    "MucEnter: type=$type, args='$args'"
		}
	    }
	}
	available {
	    #only load history for tclers chat when it is not loaded already.
	    if {$conference eq "tcl@tach.tclers.tk" \
		    &&  !$::tkjabber::HaveHistory } {
		# Force history loading to the background
		after idle [list after 0 ::tkchat::LoadHistory]
	    }
	    if { $Away > 0 } {
		set AutoAway 1
		set jid $conference/[$muc mynick $conference]
		$jabber send_presence -type available -from $jid \
			-to $conference -show away -status $AwayStatus
	    }
            tkchat::addStatus 0 "Joined chat at $conference"
	    autoStatus
            after 500 [namespace origin ParticipantVersions]
	}
	default {
	    ::tkchat::addSystem .txt "MucEnter: type=$type, args='$args'"
	}
    }
}

proc ::tkjabber::ParticipantVersions {} {
    variable muc
    variable conference
    foreach id [$muc participants $conference] {
        query_user $id version
    }
}

proc ::tkjabber::userinfo {nick} {
    variable jabber
    variable conference
    variable roster

    if {[string match "/userinfo *" $nick]} {
	set nick [string range $nick 10 end]
    }
    if { [string first @ $nick] == -1 } {
	# No @ in the nick, assume someone from the conference:

	# Try to get the real jid for the user from the conference roster.
	set x [$roster getx $conference/$nick muc#user]
	set item [wrapper::getchildswithtag $x item]
	if {[llength $item] > 0} {
	    set jid [wrapper::getattribute \
			    [lindex $item 0] jid]
	    # vcard requests must be made without the resource part:
	    regexp {([^/]+)/(.+)} $jid -> jid res
	} else {
	    # Not online, perhaps...
	    # Default to the current server
	    set jid $nick@$::Options(JabberServer)
	}
	::tkchat::UserInfoDialog $jid
    } else {
	# A full jid was specified. Use that.
	::tkchat::UserInfoDialog $nick
    }
}

proc tkjabber::msgSend { msg args } {
    variable jabber
    variable roster
    variable conference
    variable Away
    set users {}

    array set opts {
	-type normal
	-user {}
	-xlist {}
	-attrs {}
	-tojid {}
        -echo  1
    }

    if { [string match "/userinfo *" $msg] } {
	after idle [list [namespace current]::userinfo $msg]
	return
    }

    if {$Away} {back ""}

    # Trim the nolog prefix - it's already an extended attribute.
    regexp {^/nolog\s?(.*)$} $msg -> msg

    if { [llength $args] > 0 } {
	array set opts $args
    }

    set user $opts(-user)

    if { $user eq "" && $opts(-tojid) eq "" } {
	set user $conference
	set type groupchat
    } elseif { $opts(-tojid) ne "" } {
	set user $opts(-tojid)
	set type $opts(-type)
    } else {
	# lookup the real nick
	set found 0
	set type $opts(-type)
	foreach person [$::tkjabber::muc participants $::tkjabber::conference] {
	    regexp {([^/])/(.+)} $person -> conf nick
	    if { $nick eq $user } {
		set user $person
		set found 1
                if {$opts(-echo)} {
                    ::tkchat::addMessage .txt "" $::Options(Username) \
			" whispered to $nick: $msg" ACTION end 0
                }
		break
	    }
	}
	if {!$found } {
	    ::log::log debug "Seaching roster. '$roster' [$roster getname $user] / [$roster getrosteritem $user/tkabber]"

	    foreach presence [$roster getpresence $user] {
		array set pres $presence
		if { $pres(-resource) ne {} && $pres(-type) eq "available" } {
		    ::log::log debug "Roster user: $user/$pres(-resource)"
		    lappend users $user/$pres(-resource)
		    incr found
		    ::tkchat::addMessage .txt "" $::Options(Username) \
			    " whispered to $user/$pres(-resource): $msg" \
			    ACTION end 0
		}
		unset pres
	    }

	}
	if { !$found } {
	    ::tkchat::addStatus 0 "Unknown nick name '$user'"
	    return
	}
    }
    if { [llength $users] == 0 } {
	set users $user
    }

    # Example usage
    #set x [wrapper::createtag x -attrlist {xmlns urn:tkchat:chat} \
    #    -subtags [list [wrapper::createtag color \
    #                        -attrlist {attr1 val1 attr2 val2} \
    #                        -chdata $::Options(MyColor)]]]

    set attrs [concat $opts(-attrs) \
		   [list xmlns urn:tkchat:chat color $::Options(MyColor)]]

    set xlist [concat [list [wrapper::createtag x -attrlist $attrs]] \
		   $opts(-xlist)]
    ::log::log debug "send_message $msg $xlist"
    foreach user $users {
	$jabber send_message $user -body $msg -type $type -xlist $xlist
    }
    #-xlist [wrapper::createtag x -attrlist {xmlns http://tcl.tk/tkchat foo bar}]

}

proc ::tkjabber::get_participant_jid {user} {
    variable conference
    if {[string first @ $user] == -1} {
        return ${conference}/$user
    }
    return $user
}

proc ::tkjabber::query_user {user what} {
    array set q {
        version  "jabber:iq:version"
        last     "jabber:iq:last"
        time     "jabber:iq:time"
        discover "http://jabber.org/protocol/disco#info"
    }
    if {![info exists q($what)]} {
        return -code error "invalid query \"$what\": must be one of\
            [join [array names q] {, }]"
    }
        
    set jid [get_participant_jid $user]
    set xmllist [wrapper::createtag query -attrlist [list xmlns $q($what)]]
    $tkjabber::jabber send_iq get [list $xmllist] -to $jid
    return
}

# tkjabber::jid --
#
#       A helper function for splitting out parts of Jabber IDs.
#
proc ::tkjabber::jid {part jid} {
    ::log::log debug "jid $part '$jid'"
    set r {}
    if {[regexp {^(?:([^@]*)@)?([^/]+)(?:/(.+))?} $jid \
	     -> node domain resource]} {
	switch -exact -- $part {
	    node      { set r $node }
	    domain    { set r $domain }
	    resource  { set r $resource }
	    !resource { set r ${node}@${domain} }
	    jid       { set r $jid }
	    default {
		return -code error "invalid part \"$part\":\
		    must be one of node, domain, resource or jid."
	    }
	}
    }
    return $r
}

# Send a Jabber message to the full jid of a user. Accept either a full
# JID or lookup a chatroom nick in the OnlineUsers array. Such messages
# are held for the user if the user is not currently available.
proc ::tkjabber::send_memo {to msg {subject Memo}} {
    variable myId
    variable jabber
    variable ::tkchat::OnlineUsers

    if {[string first @ $to] == -1} {
	if {[info exists OnlineUsers(Jabber-$to,jid)]} {
	    set to $OnlineUsers(Jabber-$to,jid)
	} else {
	    tkchat::addStatus 0 "Cannot find a JID for '$to'."
	    return
	}
    }

    set k {}
    lappend k [list subject {} 0 $subject {}]
    lappend k [list body {} 0 $msg {}]
    set a [list xmlns jabber:client type normal from $myId to $to]
    set m [list message $a 0 "" $k]
    $jabber send $m
    tkchat::addStatus 0 "Memo send to $to."
}

proc ::tkchat::updateOnlineNames {} {
    global Options
    variable OnlineUsers

    set scrollview [.pane.names yview]

    # Delete all URL-* tags to prevent a huge memory leak
    foreach tagname [lsearch -all -inline [.pane.names tag names] URL-*] {
	.pane.names tag delete $tagname
    }

    .pane.names configure -state normal
    .pane.names delete 1.0 end
    .mb.mnu delete 0 end
    .mb.mnu add command \
	    -label "All Users" \
	    -command [list ::tkchat::MsgTo "All Users"]
    set total 0
    foreach network $OnlineUsers(networks) {
	set userCnt [llength $OnlineUsers($network)]
	if { !$userCnt } {
	    continue
	}
	incr total $userCnt
	.pane.names insert end "$userCnt $network Users\n" [list SUBTITLE $network]
        if {$network eq "Jabber"} {
            .pane.names insert end "  Moderators\n" SUBTITLE
            .pane.names insert end "  Participants\n" SUBTITLE
            .pane.names mark set admins [.pane.names index "end - 2 lines"]
        }
        
	.pane.names tag bind $network <Button-1> \
		[list ::tkchat::OnNetworkToggleShow $network]
	if { $OnlineUsers($network,hideMenu) } {
	    continue
	}
	foreach nick $OnlineUsers($network) {
	    set status [lindex $OnlineUsers($network-$nick,status) 0]
            set role participant
            if {$network eq "Jabber"} {set role [get_role $nick]}
	    if {[info exists Options(Visibility,NICK-$nick)] \
		    && $Options(Visibility,NICK-$nick)} {
		set status disabled
	    }
            if {$role eq "visitor"} { set status disabled }
            set mark [expr {$role eq "moderator" ? "admins" : "end"}]
	    if { ![info exists Options(Color,NICK-$nick)] } {
		set Options(Color,NICK-$nick) $Options(Color,MainFG)
	    }
	    switch -exact -- $status {
		online {
		    .pane.names image create $mark -image ::tkchat::roster::online
		}
		chat {
		    .pane.names image create $mark -image ::tkchat::roster::chat
		}
		dnd {
		    .pane.names image create $mark -image ::tkchat::roster::dnd
		}
		away {
		    .pane.names image create $mark -image ::tkchat::roster::away
		}
		xa {
		    .pane.names image create $mark -image ::tkchat::roster::xa
		}
		disabled {
		    .pane.names image create $mark -image ::tkchat::roster::disabled
		}
	    }
	    if { [info exists OnlineUsers($network-$nick,jid)] } {
                set tags [list NICK NICK-$nick URL URL-[incr ::URLID]]
		.pane.names insert $mark "$nick" $tags "\n" NICK
		.pane.names tag bind URL-$::URLID <Button-1> [list \
			::tkjabber::getChatWidget \
			$::tkjabber::conference/$nick $nick]
		.mb.mnu add command \
			-label $nick \
			-command [list ::tkchat::MsgTo $nick]
	    } else {
		.pane.names insert $mark "$nick\n" \
			[list NICK NICK-$nick URL-[incr ::URLID]]
	    }
	    .pane.names tag bind URL-$::URLID <Button-3> \
		    [list ::tkchat::OnNamePopup $nick $network %X %Y]
	    .pane.names tag bind URL-$::URLID <Control-Button-1> \
		    [list ::tkchat::OnNamePopup $nick $network %X %Y]
	}
	.pane.names insert end "\n"
    }
    .pane.names insert 1.0 "$total Users Online\n\n" TITLE
    .pane.names yview moveto [lindex $scrollview 0]
    .pane.names configure -state disabled
}

proc ::tkchat::OnNetworkToggleShow { network } {
    variable OnlineUsers

    set OnlineUsers($network,hideMenu) \
	    [expr {!$OnlineUsers($network,hideMenu)}]
    updateOnlineNames
}

proc ::tkchat::OnNameToggleVis { nick } {
    global Options

    set Options(Visibility,$nick) [expr {!$Options(Visibility,$nick)}]
    DoVis $nick
}

proc ::tkchat::get_role {nick} {
    upvar #0 ::tkchat::OnlineUsers(Jabber-$nick,role) role
    if {[info exists role]} {
        return $role
    }
    return participant
}

proc ::tkchat::OnNamePopup { nick network x y } {
    global Options

    set m .pane.names_popup
    catch { destroy $m }
    menu $m -tearoff 0
    switch -exact -- $network {
        "IRC" {
            $m add command -label "User info" \
                -command [list ::tkjabber::msgSend "whois $nick" \
                              -type chat \
                              -tojid ijchain@all.tclers.tk/ijbridge]
        }
        "Jabber" {
            $m add command \
		-label "Private Chat" \
		-command [list ::tkjabber::getChatWidget \
                              $::tkjabber::conference/$nick $nick]
            $m add command \
		-label "User info" \
		-command [list ::tkjabber::msgSend "/userinfo $nick"]

            $m add command \
                -label "Version info" \
                -command [list ::tkjabber::query_user $nick version]

            # FIX ME: if the user is an admin/moderator then we want to show
            # an admin list
            if {[get_role $Options(Nickname)] eq "moderator"} {
                $m add cascade -label "Admin" -underline 0 \
                    -menu [set ma [menu $m.admin -tearoff 0]]
                $ma add checkbutton -label "Mute" -underline 0 \
                    -onvalue visitor -offvalue participant \
                    -variable ::tkchat::OnlineUsers(Jabber-$nick,role) \
                    -command [list [namespace origin ToggleRole] voice $nick]
                $ma add checkbutton -label "Moderator" -underline 2 \
                    -onvalue moderator -offvalue participant \
                    -variable ::tkchat::OnlineUsers(Jabber-$nick,role) \
                    -command [list [namespace origin ToggleRole] admin $nick]
                $ma add command -label "Kick" -underline 0 \
                    -command [list [namespace origin Kick] $nick]
                $ma add command -label "Ban" -underline 0 \
                    -command [list [namespace origin Kick] $nick ban]
            }
        }
    }
    if { [info exists Options(Visibility,NICK-$nick)] } {
        if { $Options(Visibility,NICK-$nick) } {
	    $m add command -label "Show user"
	} else {
	    $m add command -label "Hide user"
	}
	$m entryconfigure last \
            -command [list ::tkchat::OnNameToggleVis NICK-$nick]
    }
    tk_popup $m $x $y
}

proc ::tkchat::Kick {nick {what kick}} {
    set action [tk_messageBox -type yesno -title "Are you sure?" -icon question \
                    -message "You are about to $what $nick. Are you certain\
                       you want to do this -- remember you could just mute this user"]
    if {$action eq "yes"} {
        if {$what eq "ban"} {
            ::tkjabber::setaffiliation $nick outcast \
                "You have been banned by $Options(Nickname)"
        } else {
            ::tkjabber::setrole $nick none \
                "You have been kicked by $Options(Nickname)"
        }
    }
}

proc ::tkchat::ToggleRole {type nick} {
    global Options
    upvar #0 ::tkchat::OnlineUsers(Jabber-$nick,role) role
    switch -exact -- $type {
        voice {
            if {$role eq "visitor"} {
                ::tkjabber::setrole $nick visitor \
                    "You have been unmuted by $Options(Nickname)"
            } else {
                ::tkjabber::setrole $nick participant \
                    "You have been unmuted by $Options(Nickname)"
            }
        }
        admin {
            if {$role eq "moderator"} {
                ::tkjabber::setrole $nick moderator \
                    "You have been made an admin by $Options(Nickname)"
            } else {
                ::tkjabber::setrole $nick participant \
                    "You have had your admin status removed by $Options(Nickname)"
            }
        }
        default {
            tk_messageBox -icon error -title error -message "we shouldn't be here"
        }
    }
}

proc ::tkchat::createRosterImages {} {
    image create photo ::tkchat::roster::chat -data {
	R0lGODlhDgAKAMIAAAAAAP//gICAgP///4CAQACA/wBAgH9/fyH5BAEKAAcA
	LAAAAAAOAAoAAAMseAesy22FKda4F0hq8dDARFSA9ymAoEIsWhSG4czL+8a0
	a+M5YMOy3o9HSwAAOw==
    }
    image create photo ::tkchat::roster::online -data {
	R0lGODlhDgAKAMIAAAAAAP//gICAgICAQACA/wBAgP///////yH5BAEKAAcA
	LAAAAAAOAAoAAAMkeAes/itIAR+QgVZ1w9DbIozhQhBFEQLnmW5s+1axq9It
	ekMJADs=
    }
    image create photo ::tkchat::roster::away -data {
	R0lGODlhDgAKAOMAAAAAAAAA////gICAgP///4CAQACA/wBAgP//////////
	/////////////////////yH5BAEKAAgALAAAAAAOAAoAAAQ4ECFAJQo4WCD6
	uERIaFMnDIFIAGMpFKjIttNgp+usAYZxHLgQK8Dr+YCqnfF4yUiKvZ9lCmVO
	JREAOw==
    }
    image create photo ::tkchat::roster::dnd -data {
	R0lGODlhDgAKAOMAAAAAAP//gICAgP8AAICAQP///wCA/wBAgP//////////
	/////////////////////yH5BAEKAAgALAAAAAAOAAoAAAQ5ECFA5aTAgsDF
	HOCQTVwgAGGYbQFxDkVciBIg3Kg8r4ZxHKiUCNDr/YIgXvF3qUyKvoNlSlxK
	p5IIADs=
    }
    image create photo ::tkchat::roster::xa -data {
	R0lGODlhDgAKAOMAAAAAAP8AAP//gICAgP///4CAQACA/wBAgP//////////
	/////////////////////yH5BAEKAAgALAAAAAAOAAoAAAQ4ECFAJQo4WCD6
	uERIaFMnDIFIAGMpFKjIttNgp+usAYZxHLgQK8Dr+YCqnfF4yUiKvZ9lCmVO
	JREAOw==
    }
    image create photo ::tkchat::roster::disabled -data {
	R0lGODlhDgAKAMIGAAAAAD09PW9vb4CAQICAgP//gP///////yH5BAEKAAcA
	LAAAAAAOAAoAAAMkeAes/qtIAh+QhVZ1y9DbQozhIghBEALnmW5s+1axq9It
	ekMJADs=
    }
}

proc ::tkjabber::xmlSafe { str } {
    return [string map \
	    {& {&amp;} < {&lt;} > {&gt;} \" {&quot;} ' {&apos;}} $str]
}

proc ::tkjabber::setNick { newnick } {
    variable muc
    variable conference
    variable roster
    variable jabber
    variable grabNick
    variable baseNick
    variable ::tkchat::OnlineUsers

    if { [lsearch -exact $OnlineUsers(Jabber) $newnick] > -1 } {
	# Perhaps it is my own nick, in another window?
	set x [$roster getx $conference/[xmlSafe $newnick] muc#user]
	set item [wrapper::getchildswithtag $x item]
	set otherjid ""
	if {[llength $item] > 0} {
	    set otherjid [wrapper::getattribute [lindex $item 0] jid]
	}
	regexp {([^/]+)/(.+)} [$jabber myjid] -> myjid myres

	if { [regexp {([^/]+)/(.+)} $otherjid -> ojid ores] } {
	    if { $ojid eq $myjid && $ores ne $myres } {
		# Yes, it is my JID, different resource.
		# Send a rename request:
		set attrs [list xmlns urn:tkchat:changenick]
		set xlist [list [wrapper::createtag x -attrlist $attrs]]

		$tkjabber::jabber send_message $otherjid -type chat \
			-xlist $xlist
		::tkchat::addStatus 0 [concat \
			"This nick is owned by another you, requested" \
			"transfer..."]
		set grabNick $newnick
		return
	    }
	}
	::tkchat::addStatus 0 "The nickname '$newnick' is already in use."
	return
    }

    # There is a race condition here. new nick could enter between the check
    # and the setnick call...
    ::tkchat::setNickname $newnick
    set baseNick $newnick
    $muc setnick $conference [xmlSafe $newnick]
}

proc ::tkjabber::transferNick { reqfrom } {
    variable muc
    variable conference
    variable roster
    variable jabber
    variable ::tkchat::OnlineUsers

    regexp {([^/]+)/(.+)} $reqfrom -> ojid ores

    regexp {([^/]+)/(.+)} [$jabber myjid] -> myjid myres

    if { $ojid ne $myjid } {
	# No, it is not a request from an alter ego.
	# Denied.
	::log::log debug "Denied nick transfer request from $reqfrom"
	return
    }

    # It is a valid request. Do the transfer.
    set postfix $::Options(JabberResource)
    if { [string match "tkchat*" $postfix] } {
	set postfix [string range $postfix 6 end]
	if { $postfix eq "" } {
	    set postfix "_Away"
	}
    }
    set newnick $::Options(Nickname)$postfix
    if { [lsearch -exact $OnlineUsers(Jabber) $newnick] != -1 } {
	::tkchat::addStatus 0 \
		"Got a nick transfer request, but $newnick is already in use."
	return
    }

    # Set my nick name to newnick.
    ::tkchat::setNickname $newnick
    $muc setnick $conference [xmlSafe $newnick]

    # The other party does not need to be notified
    # - it should be in nickgrab mode.
}

proc ::tkjabber::setTopic { newtopic } {

    variable conference
    variable jabber

    $jabber send_message $conference -subject $newtopic -type groupchat
}

proc ::tkjabber::ParseLogMsg { when nick msg {opts ""} args } {
    variable HistoryLines
    variable HaveHistory
    set HaveHistory 1
    set timestamp [clock scan ${when} -gmt 1]
    lappend HistoryLines [list $timestamp $nick $msg]
    if { [llength $args] > 0 } {
	::log::log warning "Log incorrect log format."
    }
    #::log::log debug "[clock format $timestamp] $nick :: $msg"
}

proc ::tkjabber::LoadHistoryLines {} {
    global Options
    variable HistoryLines

    set state [.txt cget -state]
    .txt configure -state normal

    set count 0
    foreach entry $HistoryLines {
	set timestamp [lindex $entry 0]
	set nick [lindex $entry 1]
	set msg [lindex $entry 2]

	parseMsg $nick $msg "" HISTORY $timestamp

	incr count
	if { $count > 35 } { break }
    }
    set HistoryLines [lrange $HistoryLines $count end]

    if {$HistoryLines == {}} {
	::log::log debug "History loading completed."
	.txt configure -state normal
	.txt delete "HISTORY + 1 char" "HISTORY + 1 line"
	.txt insert "HISTORY + 1 char" \
		"+++++++++++++++++++++ End Of History ++++++++++++++++++++++\n"
    } else {
	after idle [list after 0 ::tkjabber::LoadHistoryLines]
    }
    if { $Options(AutoScroll) } {
	.txt see end
    }
    .txt configure -state $state
}

proc ::tkjabber::TwiddlePort {} {
    global Options
    if {$Options(UseJabberSSL) eq "ssl" && $Options(JabberPort) == 5222} {
	incr Options(JabberPort)
    } elseif {$Options(UseJabberSSL) ne "ssl" && $Options(JabberPort) == 5223} {
	incr Options(JabberPort) -1
    }
}

proc ::tkjabber::scheduleReconnect {} {
    variable reconnectTimer
    variable connectionRetryTime

    if { $reconnectTimer ne "" } {
	::log::log debug "Already trying to reconnect..."
	return
    }

    tkchat::addStatus 0 "Will try to reconnect in $connectionRetryTime seconds."
    set reconnectTimer [after [expr {$connectionRetryTime*1000}] tkjabber::connect]

    set connectionRetryTime [expr { int ($connectionRetryTime * 1.5) } ]
    # Max out at 3 minutes
    if { $connectionRetryTime > 180 } {
	set connectionRetryTime 180
    }
}

# Respond to subscriptin requests
proc tkjabber::SubscriptionRequest {from status} {
    variable subs_uid
    if {![info exists subs_uid]} { set subs_uid 0 }
    set jid [jid !resource $from]
    set ttl [msgcat::mc "Subscribe request from %s" $jid]
    set msg [msgcat::mc "Do you want to let %s add you to their roster?" $jid]
    set status [string trim $status]
    set wid dlg[incr subs_uid]
    set dlg [toplevel .$wid -class Dialog]
    wm title $dlg $ttl
    set f [frame $dlg.f -borderwidth 0]
    set lt [label $f.lt -text "$ttl" -anchor w]
    set ls [label $f.ls -text " \"$status\"" -anchor w]
    set lm [label $f.lm -text "$msg" -anchor w]
    set fb [frame $f.fb -borderwidth 0]
    set yes [button $fb.yes -text [msgcat::mc "Yes"] -default active \
		 -command [list set [namespace current]::$wid subscribed]]
    set no  [button $fb.no -text [msgcat::mc "No"] -default normal \
		 -command [list set [namespace current]::$wid unsubscribed]]
    bind $dlg <Return>     [list $yes invoke]
    bind $dlg <Key-Escape> [list $no  invoke]
    pack $no $yes -side right
    pack $lt $ls $lm $fb -side top -fill x -expand 1
    pack $f -side top -fill both -expand 1
    set [namespace current]::$wid waiting
    tkwait variable [namespace current]::$wid
    destroy $dlg
    set response [set [namespace current]::$wid]
    $tkjabber::jabber send_presence -type $response \
	-from [$tkjabber::jabber myjid] \
	-to $jid
    unset [namespace current]::$wid
    return
}

proc ::tkjabber::away { status {show away} } {
    variable AwayStatus
    variable conference

    set AwayStatus $status
    set jid $conference/[$tkjabber::muc mynick $conference]
    $tkjabber::jabber send_presence -type available \
	    -from $jid -to $conference -show $show -status $status
    autoStatus
}

proc ::tkjabber::back { status } {
    variable Away 0
    variable AutoAway 0
    variable AwayStatus ""
    variable conference

    set jid $conference/[$::tkjabber::muc mynick $conference]
    $::tkjabber::jabber send_presence -type available \
	    -from $jid -to $conference -show online -status $status
    autoStatus
}

# -------------------------------------------------------------------------

proc tkjabber::on_iq_last {token from subiq args} {
    tkchat::addStatus 0 "Time query from $from"
    if {[idle::supported]} {
        set opts [list -to $from]
        array set a [concat -id {{}} $args]
        if {$a(-id) ne {}} { lappend opts -id $a(-id) }
        set xml [wrapper::createtag query \
                     -attrlist [list xmlns jabber:iq:last \
                                    seconds [idle::idletime]]]
        eval [linsert $opts 0 $token send_iq result [list $xml]]
        return 1 ;# handled
    }
    return 0 ;# report not handled
}

proc tkjabber::on_iq_version {token from subiq args} {
    global tcl_platform
    tkchat::addStatus 0 "Version query from $from"
    array set a [concat -id {{}} $args]
    set opts [list -to $from]
    if {$a(-id) ne {}} { lappend opts -id $a(-id) }
    set os $tcl_platform(os)
    if {[info exists tcl_platform(osVersion)]} {
	append os " $tcl_platform(osVersion)"
    }
    append os "/Tcl [info patchlevel]"
    set subtags [list  \
      [wrapper::createtag name    -chdata "Tkchat"]  \
      [wrapper::createtag version -chdata [package provide app-tkchat]]  \
      [wrapper::createtag os      -chdata $os] ]
    set xmllist [wrapper::createtag query -subtags $subtags  \
                     -attrlist {xmlns jabber:iq:version}]
    eval [linsert $opts 0 $token send_iq result [list $xmllist]]
    return 1 ;# handled
}

proc tkjabber::on_iq_version_result {token from xmllist args} {
    variable conference
    array set a [concat -id {{}} $args]
    if {[jid !resource $from] eq $conference} {
        if {[llength [package provide tooltip]] > 0} {
            set nick [jid resource $from]
            array set data {}
            foreach sub [wrapper::getchildren $xmllist] {
                set data([wrapper::gettag $sub]) [wrapper::getcdata $sub]
            }
            set ver ""
            if {[info exists data(name)]} { append ver $data(name) }
            if {[info exists data(version)]} { append ver " " $data(version) }
            if {[info exists data(os)]} { append ver " : $data(os)" }
            set tkchat::OnlineUsers(Jabber-$nick,version) $ver
            tooltip::tooltip .pane.names -tag NICK-$nick $ver
            tkchat::addStatus 0 "$nick is using $ver"
        }
    }
    return 1 ;# handled
}

# -------------------------------------------------------------------------

proc tkjabber::ProxyConnect {proxyserver proxyport jabberserver jabberport} {
    global Options
    variable have_tls

    ::tkchat::addStatus 0 "Connecting to proxy $proxyserver:$proxyport"
    set sock [socket $proxyserver $proxyport]
    fconfigure $sock -blocking 0 -buffering line -translation crlf

    set proxyauth [join [::tkchat::buildProxyHeaders] {: }]
    puts $sock "CONNECT $jabberserver:$jabberport HTTP/1.1"
    puts $sock "Host: $jabberserver"
    puts $sock "User-Agent: [http::config -useragent]"
    puts $sock "Proxy-Connection: keep-alive"
    puts $sock "Connection: keep-alive"
    if {[string length $proxyauth] > 0} {
	puts $sock "$proxyauth"
    }
    puts $sock ""

    fileevent $sock readable {set proxy_readable ""}
    global proxy_readable
    vwait proxy_readable
    fileevent $sock readable {}

    set block [read $sock]
    set result [lindex [split $block \n] 0]
    set code [lindex [split $result { }] 1]
    fconfigure $sock -blocking 1 -translation binary -buffering none

    if {$code >= 200 && $code < 300} {
	if {$have_tls && $Options(UseJabberSSL) eq "ssl"} {
            ::tkchat::addStatus 0 "Securing network link"
	    ::tls::import $sock
	} else {
            ::tkchat::addStatus 0 "Connected"
        }
    } else {
	error "proxy connect failed: $block"
    }
    return $sock
}

# -------------------------------------------------------------------------

proc ::tkjabber::getChatWidget { jid from } {

    variable ChatWindows
    global Options
    # Look in ChatWindows and maybe popup a new chat window

    set jwr [jid !resource $jid]
    if {![info exists ChatWindows(txt.$jid)] &&
	[info exists ChatWindows(txt.$jwr)]
    } then {
	# We have a window for that JID with no resource.
	# Let's personalise it.
	foreach v {toplevel title txt} {
	    if {[info exists ChatWindows($v.$jwr)]} {
		set ChatWindows($v.$jid) $ChatWindows($v.$jwr)
		unset ChatWindows($v.$jwr)
	    }
	}
	set ChatWindows(title.$jid) "$from <$jid>"
	::tkchat::SetChatWindowBindings $ChatWindows(toplevel.$jid) $jid
    }

    if { [info exists ChatWindows(toplevel.$jid)] } {
	if { ![string match "$ChatWindows(toplevel.$jid)*" [focus]] } {
	    wm title $ChatWindows(toplevel.$jid) "* $ChatWindows(title.$jid)"
	    ::tkchat::alertWhenIdle $ChatWindows(txt.$jid)
	}
    }

    if { [info exists ChatWindows(txt.$jid)] } {
	return $ChatWindows(txt.$jid)
    }

    switch $Options(OneToOne) {
	tabbed -
	popup {
	    set ChatWindows(toplevel.$jid) [toplevel .chat[incr ChatWindows(counter)]]
	    set ChatWindows(title.$jid) "$from <$jid>"
	    set ChatWindows(txt.$jid) \
		[tkchat::CreateNewChatWindow $ChatWindows(toplevel.$jid)]
	    ::tkchat::SetChatWindowBindings $ChatWindows(toplevel.$jid) $jid
	    ::tkchat::StampVis
	    focus $ChatWindows(toplevel.$jid).eMsg
	    return $ChatWindows(txt.$jid)
	}
	default {
	    return .txt
	}
    }
}

proc tkjabber::deleteChatWidget { jid } {
    variable ChatWindows

    foreach item {txt toplevel title} {
	unset -nocomplain ChatWindows($item.$jid)
    }
}

proc ::tkjabber::autoStatus {} {
    variable autoStatusAfterId
    variable Away
    variable AutoAway
    variable AwayStatus
    global Options

    if { [info exists autoStatusAfterId] } {
	after cancel $autoStatusAfterId
	unset autoStatusAfterId
    }

    if { ![idle::supported] } return

    if { $Away == 0 && $Options(AutoAway) == -1 } {
	# Auto Away disabled in configure menu
	return
    }

    if { $Options(AutoAway) == -1 } {
	set idle_time 0
    } else {
	set idle_time [expr {$Options(AutoAway) * 60}]
    }
    set xa_time [expr {$idle_time + 60 * 30}]

    if { $Away == 2 && $AutoAway > 0 && [idle::idletime] < $idle_time } {
	back ""
    } elseif { $AutoAway == 0 && [idle::idletime] > $idle_time } {
	set Away 2
	set AutoAway 1
	away "no activity" away
    } elseif { $AutoAway == 1 && [idle::idletime] > $xa_time } {
	set AutoAway 2
	away $AwayStatus xa
    } elseif { $AutoAway == 2 && $Away == 1 } {
	return
    }
    set autoStatusAfterId [after 1000 ::tkjabber::autoStatus]
}

# -------------------------------------------------------------------------

proc ::tkchat::GetTipIndex {} {
    http::geturl http://www.tcl.tk/cgi-bin/tct/tip/tclIndex.txt -timeout 15000 \
        -progress ::tkchat::Progress \
        -command [list [namespace origin fetchurldone] \
                      [namespace origin GetTipIndexDone]]
}

proc ::tkchat::GetTipIndexDone {tok} {
    variable TipIndex
    set TipIndex [http::data $tok]
}

# -------------------------------------------------------------------------

proc ::tkchat::SafeGet {arrayName key} {
    upvar $arrayName a
    if {[info exists a($key)]} {
        return $a($key)
    }
    return ""
}
proc ::tkchat::ShowCertificate {info} {
    if {[winfo exists .certificate]} {
        wm deiconify .certificate
        return
    }
    variable NS
    array set C $info
    if {[string match /* $C(subject)]} {
        # older tls used / as a record separator
        array set O [split [string trim $C(subject) /] "/,="]
        array set I [string map {\ufffe http://}\
                         [split [string map {http:// \ufffe}\
                                     [string trim $C(issuer) /]] "/,="]]
    } else {
        array set O [split $C(subject) ",="]
        array set I [split $C(issuer) ",="]
    }
    set top [toplevel .certificate -class Dialog]
    set dlg [${NS}::frame $top.f]
    wm withdraw $top
    wm title $top "Certificate Information"
    wm transient $top .
    set t [text $dlg.txt -wrap word -width 70 -height 28 \
               -borderwidth 0 -padx 2 -pady 2 -font FNT -tabs {140 280}]
    $t tag configure HEAD -font SYS
    $t insert end "Server Identify Verified" HEAD "\n" {} \
        "The server $O(CN) supports secure sockets. The identity of this\
         server has been verified by $I(O)\n" {}
    if {$C(sbits) < 40} {set strength Weak} else {set strength High-grade}
    $t insert end "\n" {} "Connection Encrypted:\
        $strength Encryption ($C(cipher)) $C(sbits) bit" HEAD "\n" {}
    $t insert end "\n" {} "Issued To" HEAD "\n" {}
    $t insert end "Common Name (CN)\t[SafeGet O CN]\n"
    $t insert end "Organsation (O)\t[SafeGet O O]\n"
    $t insert end "Organizational Unit (OU)\t[SafeGet O OU]\n"
    $t insert end "Serial Number\t[SafeGet C serial]\n"
    $t insert end "\n" {} "Issued By" HEAD "\n" {}
    $t insert end "Common Name (CN)\t[SafeGet I CN]\n"
    $t insert end "Organization (O)\t[SafeGet I O]\n"
    $t insert end "Organizational Unit (OU)\t[SafeGet I OU]\n"
    $t insert end "\n" {} "Validity" HEAD "\n" {}
    $t insert end "Issued On\t$C(notBefore)\n"
    $t insert end "Expires On\t$C(notAfter)\n"
    $t insert end "\n" {} "Fingerprints" HEAD "\n" {}
    $t insert end "SHA1 Fingerprint\t[SafeGet C sha1_hash]\n"
    $t insert end "MD5 Fingerprint\t[SafeGet C md5_hash]\n"
    $t configure -state disabled
    ${NS}::button $dlg.ok -text OK -command [list destroy $top]
    bind $top <Return> [list $dlg.ok invoke]
    bind $top <Escape> [list $dlg.ok invoke]
    grid $t -sticky news
    grid $dlg.ok -stick e
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1
    grid $dlg -sticky news
    grid rowconfigure $top 0 -weight 1
    grid columnconfigure $top 0 -weight 1
    ::tk::PlaceWindow $top widget .
    wm deiconify $top
}

# -------------------------------------------------------------------------
# Windows CE specific code.

if { $tcl_platform(os) eq "Windows CE" && ![info exists ::tkchat::wince_fixes]} {
    set ::tkchat::wince_fixes 1
    # Work around for socket problem with sockets. ("select 10022")
    # Not quite there yet...
    proc tkchat::WinCE_Accept {channel peer port} {
	::log::log debug "WinCE work around accepted connection $channel $peer $port"
    }
    if { [catch {
	socket -server ::tkchat::WinCE_Accept 12345
	set ::tkchat::wince_clientchan [socket 127.0.0.1 12345]
    }] } {
	::log::log debug "Error during WinCE fix init: $::errorInfo"
    }
}

# -------------------------------------------------------------------------

proc tkchat::PasteDlg {} {
    variable paste_uid
    variable NS
    if {![info exists paste_uid]} { set paste_uid 0 }
    set wid paste[incr paste_uid]
    set dlg [toplevel .$wid -class Dialog]
    wm title $dlg "Paste data to paste.tclers.tk"
    set f [${NS}::frame $dlg.f1 -borderwidth 0]
    set f2 [${NS}::frame $f.f2 -borderwidth 0]
    ${NS}::label $f2.lbl -text Subject
    set subject [${NS}::entry $f2.subject -font FNT] 
    text $f.txt -background white -font FNT -yscrollcommand [list $f.vs set]
    ${NS}::scrollbar $f.vs -command [list $f.txt yview]
    set f3 [${NS}::frame $f.f3 -borderwidth 0]
    set send [${NS}::button $f3.send -text [msgcat::mc Send] -default active \
                  -command [list set [namespace current]::$wid send]]
    set cancel [${NS}::button $f3.cancel -text [msgcat::mc Cancel] \
                    -default normal \
                    -command [list set [namespace current]::$wid cancel]]

    foreach s {PRIMARY CLIPBOARD} {
	set failed [catch {selection get -selection $s} string]
	if {!$failed && [string length $string] > 0} {
            $f.txt insert end $string {}
	    break
        }
    }
    bind $dlg <Key-Escape> [list $cancel invoke]
    pack $f2.lbl -side left
    pack $subject -side right -fill x -expand 1
    pack $cancel $send -side right
    grid $f2    -     -sticky ew -pady 2
    grid $f.txt $f.vs -sticky news
    grid $f3    -     -sticky se
    grid rowconfigure $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1
    pack $f -side top -fill both -expand 1
    catch {::tk::PlaceWindow $dlg widget .}
    focus $subject
    tkwait variable [namespace current]::$wid
    if {[string equal [set [namespace current]::$wid] "send"]} {
        set msg [string trim [$f.txt get 0.0 end]]
        if {[string length $msg] > 0} {
            set k {}
            lappend k [list subject {} 0 [$subject get] {}]
            lappend k [list body {} 0 $msg {}]
            set a [list xmlns jabber:client type normal \
                       from [tkjabber::jid !resource $::tkjabber::myId] \
                       to tcl@paste.tclers.tk]
            set m [list message $a 0 "" $k]
            $::tkjabber::jabber send $m
        }
    }
    destroy $dlg
    unset [namespace current]::$wid
    return
}

# Store personal incoming messages in mbox format (as per the qmail mbox 
# man page.
proc ::tkjabber::StoreMessage {from subject message} {
    global env Options
    if {$Options(StoreMessages)} {
        if { [info exists env(HOME)] } {
            set filename [file join $env(HOME) .tkchat_msgs]
            catch {
                set f [open $filename a+ 0600]
                fconfigure $f -encoding utf-8
                set ts [clock format [clock seconds] \
                            -format "%a, %d %b %Y %H:%M:%S GMT" -gmt true]
                set msg [regsub -all -line {^(>*From )} $message {>\1}]
                set date [clock format [clock seconds] \
                              -format {%a %b %d %H:%M:%S} -gmt true]
                puts $f "From $from $date\nDate: $ts\nSubject: $subject\n\n$msg\n"
                close $f
            }
        }
    }
    return
}

# A users role in a MUC can be one of:
#  moderator: the user is a moderator.
#  participant: the user is an active member of the channel
#  visitor: the user has no voice.
#  none: the user is kicked.
proc ::tkjabber::setrole {nick role reason} {
    variable muc
    variable conference
    if {[catch {
        $::tkjabber::muc setrole $conference $nick $role \
            -reason $reason -command [namespace origin onAdminComplete]
    } err]} {
        tk_messageBox -icon error -title "Error" \
            -message "An error occurred setting the role for\
            \"$nick\".\n\n$err"
    }
}

proc ::tkjabber::setaffiliation {nick affiliation reason} {
    variable muc
    variable conference
    if {[catch {
        $::tkjabber::muc setaffiliation $conference $nick $affiliation \
            -reason $reason -command [namespace origin onAdminComplete]
    } err]} {
        tk_messageBox -icon error -title "Error" \
            -message "An error occurred setting the affiliation for\
            \"$nick\".\n\n$err"
    }
}

proc ::tkjabber::onAdminComplete {muc what xml args} {
    log::log debug "SetRole: $muc : $what : $xml : $args"
    switch -exact -- $what {
        result {
            tkchat::addStatus 0 "Succeeded"
        }
        error {
            tkchat::addStatus 0 "Unable to complete operation"
        }
    }
}

if {![info exists ::URLID]} {
    eval [linsert $argv 0 ::tkchat::Init]
}
