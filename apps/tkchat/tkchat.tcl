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
# packages into the CVS tree:
lappend ::auto_path [file join [file dirname [info script]] lib]

package require Tcl 8.4		; # core Tcl
package require Tk  8.4		; # core Tk
package require http 2		; # core Tcl
package require msgcat		; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib
package require base64		; # tcllib

catch {
    package require tls		; # tls (optional)
}

package require sha1		; # tcllib
package require jlib		; # jlib
package require browse		; # jlib
package require muc		; # jlib

if { [catch {
    # Optional idle detection
    package require idle
}] } then {
    # Not supported / available...
    namespace eval ::idle {}
    proc ::idle::supported {} {return 0}
}

# Deal with 'tile' support.
# We sometimes need to _really_ use the Tk widgets at the moment...
if {[llength [info commands ::tk::label]] < 1} {
    foreach cmd {label radiobutton entry} {
	rename ::$cmd ::tk::$cmd
    }
    if {![catch {package require tile 0.5}]} {
	namespace import -force ttk::*
    }
    foreach cmd {label radiobutton entry} {
	if {[llength [info commands ::$cmd]] < 1} {
	    interp alias {} ::$cmd {} ::tk::$cmd
	}
    }
}

# Under windows, we can use DDE to open urls
if {$tcl_platform(platform) eq "windows"
	&& $tcl_platform(os) ne "Windows CE"} {
    package require dde

    # Iocpsock is a Windows sockets extension that supports IPv6 sockets.
    # This package also provides more efficient IP sockets on windows.
    if {0 && ![catch {package require Iocpsock}]} {
	rename ::socket ::socket1
	interp alias {} ::socket {} ::socket2
    }
}

package forget app-tkchat	; # Workaround until I can convince people
				; # that apps are not packages. :)  DGP
package provide app-tkchat \
	[regexp -inline -- {\d+(?:\.\d+)?} {$Revision: 1.296 $}]

namespace eval ::tkchat {
    variable chatWindowTitle "The Tcler's Chat"

    # Everything will eventually be namespaced
    variable MessageHooks
    array set MessageHooks {}
    variable ChatActivityHooks
    array set ChatActivityHooks {}
    variable UserClicked 0

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://mini.net

    variable HEADUrl {http://cvs.sourceforge.net/viewcvs.py/tcllib/tclapps/apps/tkchat/tkchat.tcl?rev=HEAD}
    variable rcsid   {$Id: tkchat.tcl,v 1.296 2005/06/14 12:24:19 wildcard_25 Exp $}

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
	    "%user% doesn't want to talk to you anymore!" \
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
    ::log::lvSuppress $Options(LogLevel) 0	; # unsupress selected
}

#  Pop the nth element off a list. Used in options processing.
proc ::tkchat::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
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
		    [base64::encode \
		    $Options(ProxyUsername):$Options(ProxyPassword)]]]
	}
	set auth $Options(ProxyAuth)
    }
    return $auth
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
	set token [::http::geturl $HEADUrl \
		-headers [buildProxyHeaders] -timeout 30000]
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
	::http::geturl $url -headers [buildProxyHeaders] \
		-command [list ::tkchat::fetchurldone \
			::tkchat::ShowStatusPage2]
    } msg] } then {
	addSystem .txt "Unable to obtain status page from $url" end ERROR
    }
}

# FIXME: no status page is available yet
proc ::tkchat::ShowStatusPage2 {} {
    # parse and display
}

proc ::tkchat::GetHistLogIdx {url} {
    if { [catch {
	::http::geturl $url -headers [buildProxyHeaders] \
		-command [list ::tkchat::fetchurldone ::tkchat::GotHistLogIdx]
    } msg] } then {
	addSystem .txt "Unable to obtain history from $url: \"$msg\"" end ERROR
    }
}

proc ::tkchat::GotHistLogIdx {tok} {
    set loglist {}

    set RE {<A HREF="([0-9\-%d]+\.tcl)">.*\s([0-9]+) bytes}
    foreach line [split [::http::data $tok] \n] {
	if { [regexp  -- $RE $line -> logname size] } {
	    set logname [string map {"%2d" -} $logname]
	    set size [expr { $size / 1024 }]k
	    lappend loglist $logname $size
	}
    }

    # Only show 7 days worth.
    set loglist [lrange $loglist end-13 end]
    ::log::log debug "Logs: $loglist"
    after idle [list [namespace origin LoadHistoryFromIndex] $loglist]
    return
}

proc ::tkchat::ParseHistLog {log {reverse 0}} {
    global Options

    set url "$Options(JabberLogs)/$log"
    set retList {}
    set logTime 0

    # fetch log
    ::log::log info "History: Fetch log \"$url\""
    set tok [::http::geturl $url -headers [buildProxyHeaders]]

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
	    $I eval [::http::data $tok]
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

    return $retList
}

# this called on first logon and after a purge
# so not bothering to backgound it
proc ::tkchat::LoadHistory {} {
    global Options

    # hook in the translation menu initialization (background function)
    if {$Options(UseBabelfish)} {
	babelfishMenu
    }

    set FinalList {}
    if {$Options(HistoryLines) != 0} {
	set url "$Options(JabberLogs)/?pattern=*.tcl"
	GetHistLogIdx $url
    }
}

# Called once we have acquired the log file index.
# logindex is a list of "filename sizeK filename ...."
proc ::tkchat::LoadHistoryFromIndex {logindex} {
    global Options

    set FinalList {}
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
	    toplevel $t -class dialog
	    wm withdraw $t
	    wm transient $t
	    wm protocol $t WM_DELETE_WINDOW {}
	    wm title $t "Load History From Logs"

	    label $t.lbl -text "Please select how far back you want to load:"
	    grid $t.lbl -sticky ew -padx 5 -pady 5 -columnspan 3

	    set i 0
	    variable HistQueryNum [llength $loglist]
	    foreach l $loglist {
		radiobutton $t.rb$i \
			-text "$l ($logsize($l))" \
			-value $i \
			-variable ::tkchat::HistQueryNum
		grid $t.rb$i -sticky w -padx 15 -pady 0 -column 1
		incr i
	    }

	    radiobutton $t.rb$i \
		    -text "None" \
		    -value $i \
		    -variable ::tkchat::HistQueryNum
	    button $t.ok \
		    -text Ok \
		    -width 8 \
		    -command [list destroy $t] \
		    -default active

	    grid $t.rb$i -sticky w -padx 15 -pady 0 -column 1
	    grid $t.ok -padx 5 -pady 10 -column 1

	    bind $t <Return> [list $t.ok invoke]
	    catch {::tk::PlaceWindow $t widget .}
	    wm deiconify $t
	    tkwait visibility $t
	    focus $t.ok
	    grab $t
	    tkwait window $t
	    foreach log [lrange $loglist $HistQueryNum end] {
		if {[catch {ParseHistLog $log} new]} {
		    ::log::log error "error parsing history: \"$new\""
		} else {
		    set FinalList [concat $FinalList $new]
		}
	    }
	}
    } else {
	# go thru logs in reverse until N lines loaded
	set idx [llength $loglist]
	for { incr idx -1 } { $idx >= 0 } { incr idx -1 } {
	    # fetch log
	    set log [lindex $loglist $idx]
	    if {[catch {ParseHistLog $log 1} new]} {
		::log::log error "error parsing history: \"$new\""
	    } else {
		set FinalList [concat $new $FinalList]
	    }
	    if { [::tkjabber::HistoryLines] >= $Options(HistoryLines) } {
		break
	    }
	}
    }

    # Set a mark for the history insertion point.
    #set pos "[.txt index end] - 1 line"
    .txt config -state normal
    if {[lsearch [.txt mark names] HISTORY] == -1} {
	.txt insert 0.0 \
		"+++++++++++++++++++++ Loading History +++++++++++++++++++++\n"
	.txt mark set HISTORY 0.0
    }

    set Options(FinalList) $FinalList

    .txt config -state disabled
    .txt see end

    ::tkjabber::LoadHistoryLines
}

proc ::tkchat::logonChat {} {
    global Options

    if {0} {
	# use when testing only
	# - allows restarts without actually logging in again
	catch {pause off}
	return
    }

    if {[info exists Options(JabberDebug)] && $Options(JabberDebug)} {
	set jlib::debug 2
    }

    # Logon to the jabber server.
    tkjabber::connect
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
    set hdrs [buildProxyHeaders]
    lappend hdrs "Accept-Charset" "ISO-8859-1,utf-8"
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

proc ::tkchat::babelfishInit { \
	{url http://babelfish.altavista.com/babelfish/} } {
    set tok [http::geturl $url \
	    -headers [buildProxyHeaders] \
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
	set tr [menu ${menu}.tr]

	# Add to the Help->Translate menu
	catch {
	    set ndx [$menu index "Translate Selection"]
	    $menu entryconfigure $ndx -menu $tr
	}

	# Add to the context menu
	catch {
	    set ndx [.mbar.mm index "Translate"]
	    .mbar.mm entryconfigure $ndx -menu $tr
	}
	::tkchat::babelfishInit
    }
}

proc ::tkchat::babelfishMenuPost {x y} {
    variable babelfishinit 0
    ::log::log debug "babelfishmenu post"
    if {![winfo exists .mbar.tr]} {
	babelfishMenu
	tkwait variable babelfishinit
    }
    .mbar.help.tr post $x $y
}

# -------------------------------------------------------------------------

proc ::tkchat::MsgTo {{user "All Users"}} {
    global Options
    variable MsgToColors

    set windows [list .eMsg .tMsg]
    if {![info exists MsgToColors]} {
	foreach w $windows {
	    set MsgToColors($w,normal) [$w cget -bg]
	    set MsgToColors($w,whisper) $Options(WhisperIndicatorColor)
	}
    }

    if { $user eq "All Users" } {
	set type normal
    } else {
	set type whisper
    }

    foreach w $windows {
	$w configure -bg $MsgToColors($w,$type)
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

proc ::tkchat::getColor { name } {
    global Options

    if { [info exists Options(Color,$name)] } {
	set w [lindex $Options(Color,$name) 0]
	set clr [lindex $Options(Color,$name) $w]
    } else {
	set w [lindex $Options(Color,NICK-$name) 0]
	set clr [lindex $Options(Color,NICK-$name) $w]
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
	lappend sList $url $url
	set str $post
    }

    if {[string length $str]} {
	lappend sList $str ""
    }
    set out {}
    # Assume any 6 or 7-digit sequence is a SF bug id and make URLs for them
    foreach {str url} $sList {
	if {[string length $url]} {
	    lappend out $str $url
	    continue
	}
	while {[regexp -- {^(.*?)(\m[0-9]{6,7}\M)(.*?)$} \
		    $str -> pre id post]} {
	    if {[string length $pre]} {
		lappend out $pre ""
	    }
	    set url "http://sourceforge.net/support/tracker.php?aid=$id"
	    lappend out $id $url
	    set str $post
	}
	if {[string length $str]} {
	    lappend out $str ""
	}
    }
    return $out
}

proc ::tkchat::checkNick { txt nick clr timestamp } {
    global Options

    set wid [expr { [font measure NAME <$nick>] + 10 }]
    if { $wid > $Options(Offset) } {
	set Options(Offset) $wid

	# Maybe limit the nick column width a bit...
	set max [expr { [font measure NAME [string repeat X 12]] + 10 }]
	if { $Options(Offset) > $max } {
	    set Options(Offset) $max
	}

	# Set tabs appropriate for STAMP visibility
	StampVis
    }
    if { $timestamp == 0 } {
	set timestamp [clock seconds]
    }
    set match 0
    foreach nk $Options(NickList) {
	if { [lindex $nk 0] eq $nick } {
	    lset Options(NickList) $match [lset nk 1 $timestamp]
	    break
	} else {
	    incr match
	}
    }
    if { $match == [llength $Options(NickList)] } {
	lappend Options(NickList) [list $nick $timestamp]
	set Options(Color,NICK-$nick) $Options(Color,MainFG)
	NickVisMenu
	set clr [getColor $nick]
    }
    if { $clr ne "" && [lindex $Options(Color,NICK-$nick) 1] ne $clr } {
	# new color
	lset Options(Color,NICK-$nick) 1 $clr
	lset Options(Color,NICK-$nick) 2 [invClr $clr]
	set clr [getColor $nick]
	$txt tag configure NICK-$nick -foreground "#$clr"
	$txt tag configure NOLOG-$nick -foreground "#[fadeColor $clr]"
	$txt tag lower NICK-$nick STAMP
    }
}

# Beep and/or deiconify and raise the main window as an idle callback.
# This is done as an idle callback because there might be many requests
# to alert in a row and we want to batch them all together into one
# action.
#
proc ::tkchat::alertWhenIdle {} {
    variable alert_pending
    if {![info exists alert_pending]} {
	set alert_pending 1
	after idle [namespace origin alertCallback]
    }
}
proc ::tkchat::alertCallback {} {
    variable alert_pending
    global Options
    catch {unset alert_pending}
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
proc ::tkchat::checkAlert { msgtype nick str } {
    global Options LastPost

    set now [clock seconds]
    set alert 0

    if { $Options(Alert,$msgtype) } {
	if { $Options(Alert,ALL) } {
	    set alert 1
	} else {
	    if { $Options(Alert,ME) } {
		set myname [string tolower $Options(Username)]
		set mynick [string tolower $Options(Nickname)]
		set txt [string tolower $str]
		if { [string first $myname $txt] >=0 \
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
    if {$alert} {
	alertWhenIdle
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

proc ::tkchat::addMessage { w clr nick str {mark end} {timestamp 0}
	{extraOpts ""} } {
    global Options
    variable map
    variable MessageHooks

    array set opts $extraOpts

    if { [nickIsNoisy $nick] } {
	return
    }

    #for colors, it is better to extract the displayed nick from the one used
    #for tags.
    set displayNick $nick
    regexp -- {^<{0,2}(.+?)>{0,2}$} $nick displayNick nick

    checkNick $w $nick $clr $timestamp
    checkAlert NORMAL $nick $str

    $w config -state normal
    InsertTimestamp $w $nick $mark $timestamp [list NICK-$nick]
    $w insert $mark "$displayNick\t" [list NICK NICK-$nick]
    foreach { str url } [parseStr $str] {
	foreach cmd [array names MessageHooks] {
	    eval $cmd [list $nick $str $url]
	}
	if { [info exists opts(nolog)] } {
	    set tags [list MSG NOLOG-$nick NOLOG NICK-$nick]
	} else {
	    set tags [list MSG NICK-$nick]
	}
	if { $url ne "" } {
	    lappend tags URL URL-[incr ::URLID]
	    $w tag bind URL-$::URLID <1> [list ::tkchat::gotoURL $url]
	}

	# Split into lines, so we can insert the proper tabs for
	# timestamps:
	set lines [split $str \n]
	for { set i 0 } { $i < [llength $lines] } { incr i } {
	    set line [lindex $lines $i]
	    if { $i > 0 } {
		# The first line has the timestamp, only
		# subsequent lines need an extra tab char
		::log::log debug "More than one line, add tabs"
		$w insert $mark "\n" $tags "\t" [list STAMP NICK-$nick]
		set line "\t$line"
	    }
	    Insert $w $line $tags $url $mark
	}
    }

    # Call chat activity hooks
    foreach cmd [array names ::tkchat::ChatActivityHooks] {
	eval $cmd
    }
    $w insert $mark "\n" [list NICK NICK-$nick]
    $w config -state disabled
    if { $Options(AutoScroll) } {
	$w see end
    }
}

# Provide an indication of the number of messages since the window was last
# in focus.
proc ::tkchat::IncrMessageCounter {} {
    variable chatWindowTitle
    variable MessageCounter
    if {[focus] != {} } {
	ResetMessageCounter
    } else {
	incr MessageCounter
	set title "$MessageCounter - $chatWindowTitle"
	wm title . $title
	wm iconname . $title
	WinicoUpdate
    }
}

proc ::tkchat::ResetMessageCounter {} {
    variable MessageCounter
    variable chatWindowTitle
    set MessageCounter 0
    set title $chatWindowTitle
    wm title . $title
    wm iconname . $title
    WinicoUpdate
}

proc ::tkchat::InsertTimestamp { w nick {mark end} {seconds 0} {tags {}} } {
    # The nick argument is here, so we can display the local time for
    # each nick.
    if { $seconds == 0 } {
	set seconds [clock seconds]
    }
    $w insert $mark "\[[clock format $seconds -format %H:%M]\]\t" \
	[concat STAMP $tags]
}

proc ::tkchat::Insert { w str tags {url ""} {mark end} } {
    global Options
    set str [string map {"\n" "\n\t"} $str]
    # Don't do emoticons on URLs
    if { ($url eq "") && $Options(emoticons) } {
	variable IMG
	variable IMGre
	set i 0
	foreach match [regexp -inline -all -indices -- $IMGre $str] {
	    foreach { start end } $match break
	    set emot [string range $str $start $end]
	    if { [info exists IMG($emot)] } {
		$w insert $mark \
			[string range $str $i [expr { $start - 1 }]] $tags
		if { $mark eq "end" } {
		    set idx [$w index "$mark -1 char"]
		} else {
		    set idx [$w index $mark]
		}
		$w image create $mark -image ::tkchat::img::$IMG($emot)
		foreach tg $tags {
		    $w tag add $tg $idx
		}
	    } else {
		$w insert $mark [string range $str $i $end] $tags
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
	msg - mes* { set var [namespace current]::MessageHooks }
	chat       { set var [namespace current]::ChatActivityHooks }
	default {
	    return -code error "unknown hook type \"$type\": must be\
		    message or chat"
	}
    }
    switch -exact -- $do {
	add {
	    set ${var}($cmd) {}
	}
	remove {
	    catch {unset -- ${var}($cmd)}
	}
	default {
	    return -code error "unknown hook action \"$type\": must be\
		    add or remove"
	}
    }
}

proc ::tkchat::say { who msg args } {
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
    . config -cursor watch
    .txt config -cursor watch
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
	    expr {
		[info exists Options(BROWSER)]
		|| [findExecutable mozilla		Options(BROWSER)]
		|| [findExecutable mozilla-firefox	Options(BROWSER)]
		|| [findExecutable mozilla-firebird	Options(BROWSER)]
		|| [findExecutable konqueror		Options(BROWSER)]
		|| [findExecutable netscape		Options(BROWSER)]
		|| [findExecutable iexplorer		Options(BROWSER)]
		|| [findExecutable lynx			Options(BROWSER)]
	    }

	    # lynx can also output formatted text to a variable
	    # with the -dump option, as a last resort:
	    # set formatted_text [ exec lynx -dump $url ] - PSE
	    #
	    # -remote argument might need formatting as a command
	    # 		Try that first
	    if { [catch {
		exec $Options(BROWSER) -remote openURL($url) 2> /dev/null
	    }] } then {
		# Try -remote with raw URL argument
		if { [catch {
		    exec $Options(BROWSER) -remote $url 2> /dev/null
		}]} then {
		    # perhaps browser doesn't understand -remote flag
		    if { [catch { exec $Options(BROWSER) $url & } emsg] } {
			tk_messageBox -message \
				"Error displaying $url in browser\n$emsg"
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
    . config -cursor {}
    .txt config -cursor left_ptr
}

proc ::tkchat::addAction { w clr nick str {mark end} {timestamp 0}
	{extraOpts ""} } {
    global Options

    array set opts $extraOpts
    set tags [list NICK-$nick]

    #for colors, it is better to extract the displayed nick from the one used
    #for tags.
    set displayNick $nick
    regexp -- {^<{0,2}(.+?)>{0,2}$} $nick displayNick nick

    checkNick $w $nick $clr $timestamp
    checkAlert ACTION $nick $str

    # Special handling for single dot action message
    if { [string trim $str] eq "." && $Options(Username) ne $nick } {
	lappend tags SINGLEDOT
    }

    $w config -state normal
    InsertTimestamp $w $nick $mark $timestamp $tags
    $w insert $mark "   * $displayNick " [concat NICK $tags]
    foreach { str url } [parseStr $str] {
	if { [info exists opts(nolog)] } {
	    lappend tags MSG ACTION NOLOG [list NOLOG-$nick]
	} else {
	    lappend tags MSG ACTION
	}
	if { $url ne "" } {
	    lappend tags URL URL-[incr ::URLID]
	    $w tag bind URL-$::URLID <1> [list ::tkchat::gotoURL $url]
	}
	Insert $w $str $tags $url $mark
    }
    $w insert $mark "\n" $tags
    $w config -state disabled
    if { $Options(AutoScroll) } {
	$w see $mark
    }
}

proc ::tkchat::addSystem { w str {mark end} {tags SYSTEM} {time 0} } {
    $w config -state normal
    InsertTimestamp $w "" $mark $time $tags
    $w insert $mark "\t$str\n" [concat MSG $tags]
    $w config -state disabled
    if { $::Options(AutoScroll) } {
	$w see $mark
    }
}

# Add notification of user entering or leaving.
# Always add these to text - just tag them so we can elide them at will
# this way, the hide option can affect the past as well as the future
proc ::tkchat::addTraffic { w who action {mark end} {timestamp 0}
	{newwho {}} } {
    # Action should be entered or left
    global Options
    variable MSGS

    $w config -state normal
    if { [info exists MSGS($action)] } {
	set msg [string map -nocase [list %user% $who %newuser% $newwho] \
		[lindex $MSGS($action) \
		[expr { int(rand() * [llength $MSGS($action)]) }]]]
    } elseif { $action eq "nickchange" } {
	set msg "$who is now known as $newwho"
    } else {
	set msg "$who has $action the chat!!"
    }
    InsertTimestamp $w "" $mark $timestamp TRAFFIC
    $w insert $mark "\t$msg\n" [list MSG TRAFFIC [string toupper $action]]
    $w config -state disabled
    if { $Options(AutoScroll) } {
	$w see $mark
    }
}

proc ::tkchat::showInfo {title str} {
    set t .infobox
    set i 0
    while {[winfo exists $t]} {
	set t .infobox[incr i]
    }
    toplevel $t
    wm title $t $title
    set height [expr {[string length $str] / 75 + 1}]
    if {[set lines [regexp -all -- "\n" $str]] > $height} {
	set height $lines
    }
    text $t.txt -cursor left_ptr -wrap word -height $height -font NAME
    pack $t.txt -expand 1 -fill both
    bind $t.txt <1> { focus %W }
    $t.txt tag configure URL -underline 1
    $t.txt tag bind URL <Enter> [list $t.txt config -cursor hand2]
    $t.txt tag bind URL <Leave> [list $t.txt config -cursor left_ptr]
    foreach {str url} [parseStr $str] {
	if { $url eq "" } {
	    $t.txt insert end "$str " INFO
	} else {
	    $t.txt insert end "$str " [list INFO URL URL-[incr ::URLID]]
	    $t.txt tag bind URL-$::URLID <1> [list ::tkchat::gotoURL $url]
	}
    }
    $t.txt insert end "\n"
    $t.txt config -state disabled
    button $t.close -text Close -command [list destroy $t]
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
    if {[winfo exists .pane]} {
	if {$Options(DisplayUsers)} {
	    .pane add $Options(NamesWin) -sticky news
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
    .txt config -state normal
    set range [.txt tag nextrange NICKCOMPLETE 0.0]
    while { [llength $range] > 0 } {
	.txt delete [lindex $range 0] [lindex $range 1]
	set range [.txt tag nextrange NICKCOMPLETE [lindex $range 0]]
    }
    .txt config -state disabled
}

proc ::tkchat::nickComplete {} {
    #Bound to <Key-Tab> in the message entry widgets .eMsg and .tMsg
    #It will do nickname completion a'la bash command completion
    #nicknames are taken from the complete, stored nick list
    #not the users' online one. Which is too unreliable IMO.
    global Options
    variable lastCompletion

    set nicks [list]
    foreach key [array names Options "Visibility,NICK-*"] {
	set nick [string range $key [string length "Visibility,NICK-"] end]
	set nick [string map {< "" > ""} $nick]
	lappend nicks $nick
    }
    set nicks [lsort -dictionary $nicks]

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
		    .txt config -state normal
		    .txt insert end "Completions: $matches\n" \
			    [list MSG NICKCOMPLETE]
		    .txt config -state disabled
		    if {$Options(AutoScroll)} { .txt see end }
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
    . config -menu .mbar

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
    $m add command \
	    -label "Colors ..." \
	    -underline 0 \
	    -command ::tkchat::ChangeColors
    $m add command \
	    -label "Macros ..." \
	    -underline 0 \
	    -command ::tkchat::EditMacros
    $m add command \
	    -label "Font" \
	    -underline 0 \
	    -command ::tkchat::ChooseFont
    $m add command \
	    -label "User details ..." \
	    -underline 0 \
	    -command ::tkchat::UserInfoDialog
    $m add command \
	    -label "Options ..." \
	    -underline 0 \
	    -command ::tkchat::EditOptions

    $m add separator

    # Tile Themes Cascade Menu
    if { [package provide tile] != {} } {
	set themes [lsort [tile::availableThemes]]

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

    # Window Buffer Cascade Menu
    menu $m.buffer -tearoff 0
    $m add cascade \
	    -label "Max Window Buffer" \
	    -underline 3 \
	    -menu $m.buffer
    foreach l { 500 1000 1500 2500 5000 10000 } {
	$m.buffer add radiobutton \
		-label "$l lines" \
		-underline 0 \
		-variable Options(MaxLines) \
		-value $l
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
	    -underline 0 \
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
		-value $minutes
    }

    $m add separator
    $m add checkbutton \
	    -label "Enable Whiteboard" \
	    -underline 0 \
	    -variable Options(EnableWhiteboard)

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
	    -variable Options(emoticons) \
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
    menu $m.mnu -title Insert
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
    foreach tag $Options(ElideTags) text { "Single Dot" "Online/Away Status" \
		"Logon/Logoff" "All System" "Error" } {
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
	$m entryconfig "Console" \
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
	$m entryconfig "Console" -state normal
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
	    -label "Help (wiki)..." \
	    -underline 0 \
	    -command { ::tkchat::gotoURL http://wiki.tcl.tk/tkchat }

    $m add separator
    $m add command \
	    -label About... \
	    -underline 0 \
	    -command ::tkchat::About
    $m add cascade \
	    -label "Translate Selection" \
	    -underline 0 \
	    -command ::tkchat::babelfishMenu

    # main display
    if { [info commands ::panedwindow] != {} && $Options(UsePane) } {
	set UsePane 1
	panedwindow .pane -sashpad 4 -sashrelief ridge
	frame .txtframe
    } else {
	set UsePane 0
    }

    CreateTxtAndSbar

    bind .txt <Button-3> {
	::tkchat::babelfishMenuPost [winfo pointerx %W] [winfo pointery %W]
    }
    bind .txt <Shift-Button-3> { ::dict.leo.org::askLEOforSelection }

    # user display
    text .names \
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

    # bottom frame for entry
    frame .btm
    button .ml -text ">>" -width 0 -command ::tkchat::showExtra

    entry .eMsg
    bind .eMsg <Return>		::tkchat::userPost
    bind .eMsg <KP_Enter>	::tkchat::userPost
    bind .eMsg <Key-Up>		::tkchat::entryUp
    bind .eMsg <Key-Down>	::tkchat::entryDown
    bind .eMsg <Key-Tab>	{ ::tkchat::nickComplete ; break }
    bind .eMsg <Key-Prior>	{ .txt yview scroll -1 pages }
    bind .eMsg <Key-Next>	{ .txt yview scroll  1 pages }

    text .tMsg -height 6 -font FNT
    bind .tMsg <Key-Tab>	{ ::tkchat::nickComplete ; break }

    button .post -text "Post" -command ::tkchat::userPost

    menubutton .mb \
	    -indicatoron 1 \
	    -pady 4 \
	    -menu .mb.mnu \
	    -textvar Options(MsgTo) \
	    -direction above
    menu .mb.mnu -tearoff 0
    .mb.mnu add command \
	    -label "All Users" \
	    -command { ::tkchat::MsgTo "All Users" }

    .names tag config NICK -font NAME
    .names tag config TITLE -font SYS -justify center
    .names tag config URL -underline 1
    .names tag bind URL <Enter> { .names config -cursor hand2 }
    .names tag bind URL <Leave> { .names config -cursor {} }

    bind . <FocusIn> ::tkchat::ResetMessageCounter
    if { [lsearch [wm attributes .] -alpha] != -1 } {
	bind Tkchat <FocusIn>  { ::tkchat::FocusInHandler %W }
	bind Tkchat <FocusOut> { ::tkchat::FocusOutHandler %W }
    }

    # using explicit rows for restart
    set Options(NamesWin) [MakeScrolledWidget .names]
    if {$UsePane} {
	.txt configure -width 10
	.names configure -width 10
	grid .txt .sbar -in .txtframe -sticky news -padx 1 -pady 2
	grid columnconfigure .txtframe 0 -weight 1
	grid rowconfigure .txtframe 0 -weight 1
	.pane add .txtframe -sticky news
	#.pane add .names -sticky news
	.pane add $Options(NamesWin) -sticky news
	grid .pane -sticky news -padx 1 -pady 2
	grid .btm  -sticky news
    } else {
	#grid .txt .sbar .names -sticky news -padx 1 -pady 2
	grid .txt .sbar $Options(NamesWin) -sticky news -padx 1 -pady 2
	grid configure .sbar -sticky ns
	grid .btm	     -sticky news -columnspan 3
    }
    grid .ml .eMsg .post .mb -in .btm -sticky ews -padx 2 -pady 2
    grid configure .eMsg -sticky ew

    grid rowconfigure	 . 0 -weight 1
    grid columnconfigure . 0 -weight 1
    grid columnconfigure .btm 1 -weight 1

    if { $::tcl_platform(os) eq "Windows CE" } {
	wm geometry . 240x300+0+0
    } else {
	wm geometry . $Options(Geometry)
    }
    wm deiconify .

    if {$UsePane} {
	update
	if {[info exists $Options(Pane)] && [llength $Options(Pane)] == 2} {
	    eval [linsert $Options(Pane) 0 .pane sash place 0]
	} else {
	    set w [expr {([winfo width .pane] * 4) / 5}]
	    set coord [.pane sash coord 0]
	    .pane sash place 0 $w [lindex $coord 1]
	}
	set Options(PaneUsersWidth) \
	    [expr {[winfo width .pane] - [lindex [.pane sash coord 0] 0]}]
	bind .pane <Configure> [list [namespace origin PaneConfigure] %W %w]
	bind .pane <Leave> [list [namespace origin PaneLeave] %W]
	PaneConfigure .pane [winfo width .pane];# update the pane immediately.
    }

    # call this to activate the option on whether the users should be shown
    MsgTo "All Users"
    displayUsers
}

proc ::tkchat::CreateTxtAndSbar { {parent ""} } {
    global Options

    set txt $parent.txt
    set sbar $parent.sbar

    text $txt \
	    -background "#[getColor MainBG]" \
	    -foreground "#[getColor MainFG]" \
	    -font FNT \
	    -relief sunken \
	    -borderwidth 2 \
	    -wrap word \
	    -yscroll "::tkchat::scroll_set $sbar" \
	    -state disabled \
	    -cursor left_ptr \
	    -height 1
    scrollbar $sbar -command "$txt yview"

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
    $txt tag bind URL <Enter> [list $txt config -cursor hand2]
    $txt tag bind URL <Leave> [list $txt config -cursor {}]

    # Adjust tag ordering for hidden text
    foreach tag $Options(ElideTags) {
	if { $Options(Visibility,$tag) } {
	    $txt tag raise $tag STAMP
	}
    }

    # on windows, a disabled text widget can't get focus
    # but someone might want to copy/paste the text
    bind $txt <1>	 { focus %W }
    bind $txt <Up>	 [list $txt yview scroll -1 units]
    bind $txt <Down>	 [list $txt yview scroll  1 units]
    bind $txt <Button-4> [list $txt yview scroll -1 units]
    bind $txt <Button-5> [list $txt yview scroll  1 units]
}

proc ::tkchat::SetChatWindowBindings { parent jid } {

    set post [list ::tkchat::userPostOneToOne $parent $jid]

    bind $parent.eMsg <Return>	  $post
    bind $parent.eMsg <KP_Enter>  $post
    $parent.post configure -command $post
    wm title $parent $::tkjabber::ChatWindows(title.$jid)
    wm protocol $parent WM_DELETE_WINDOW [list ::tkchat::DeleteChatWindow $parent $jid]
    bind $parent <FocusIn> [list wm title $parent $::tkjabber::ChatWindows(title.$jid)]
    applyColors $parent.txt
}

proc ::tkchat::CreateNewChatWindow { parent } {
    global Options

    if {[info command ::panedwindow] != {} && $Options(UsePane)} {
	set UsePane 1
	panedwindow $parent.pane -sashpad 4 -sashrelief ridge
	frame $parent.txtframe
    } else {
	set UsePane 0
    }

    CreateTxtAndSbar $parent

    # bottom frame for entry
    frame $parent.btm
    button $parent.ml -text ">>" -width 0 -command [list ::tkchat::showExtra $parent]
    entry $parent.eMsg
    #bind $parent.eMsg <Key-Up>	  ::tkchat::entryUp
    #bind $parent.eMsg <Key-Down> ::tkchat::entryDown
    #bind $parent.eMsg <Key-Tab>  {::tkchat::nickComplete ; break}
    bind $parent.eMsg <Key-Prior> [list $parent.txt yview scroll -1 pages]
    bind $parent.eMsg <Key-Next>  [list $parent.txt yview scroll  1 pages]
    text $parent.tMsg -height 6 -font FNT
    #bind $parent.tMsg <Key-Tab> {::tkchat::nickComplete ; break}
    button $parent.post -text "Post"

    if {$UsePane} {
	$parent.txt configure -width 10
	grid $parent.txt $parent.sbar -in $parent.txtframe -sticky news -padx 1 -pady 2
	grid columnconfigure $parent.txtframe 0 -weight 1
	grid rowconfigure $parent.txtframe 0 -weight 1
	$parent.pane add $parent.txtframe -sticky news
	#$parent.pane add .names -sticky news
	#$parent.pane add $Options(NamesWin) -sticky news
	grid $parent.pane -sticky news -padx 1 -pady 2
	grid $parent.btm  -sticky news
    } else {
	#grid .txt .sbar .names -sticky news -padx 1 -pady 2
	grid $parent.txt $parent.sbar -sticky news -padx 1 -pady 2
	grid configure $parent.sbar -sticky ns
	grid $parent.btm	     -sticky news -columnspan 3
    }
    grid $parent.ml $parent.eMsg $parent.post -in $parent.btm -sticky ews -padx 2 -pady 2
    grid configure $parent.eMsg -sticky ew

    grid rowconfigure	 $parent 0 -weight 1
    grid columnconfigure $parent 0 -weight 1
    grid columnconfigure $parent.btm 1 -weight 1
    wm geometry $parent 450x350
    return $parent.txt
}

proc ::tkchat::DeleteChatWindow { toplevel jid } {
    ::tkjabber::deleteChatWidget $jid
    destroy $toplevel
}

# FIXME: Work in progress for notebook style tabbed windows?
proc ::tkchat::CreateNewChatTab { parent title } {
    set w [CreateNewChatWindow $parent]
    SetChatWindowBindings $parent $tit
    return $w
}

proc ::tkchat::SetTheme {theme} {
    global Options
    catch {
	if { [package provide tile] != {} } {
	    tile::setTheme $theme
	}
	set Options(Theme) $theme
    }
}

# On window resizing, we need to adjust the sash location to keep
# proportions the same for each pane.
proc ::tkchat::PaneConfigure {pane width} {
    global Options

    if {$::Options(DisplayUsers)} {
	if {[info exists Options(PaneUsersWidth)]} {
	    set pos [expr {$width - $Options(PaneUsersWidth)}]
	    $pane sash place 0 $pos 2
	}
    }
}

proc ::tkchat::PaneLeave {pane} {
    global Options
    if {$::Options(DisplayUsers)} {
	set Options(PaneUsersWidth) \
	    [expr {[winfo width .pane] - [lindex [.pane sash coord 0] 0]}]
    }
}

proc ::tkchat::DoVis { tag } {
    if { $::Options(Visibility,$tag) } {
	.txt tag raise $tag
    } else {
	.txt tag lower $tag STAMP
    }
    .txt tag config $tag -elide $::Options(Visibility,$tag)
    if { $::Options(AutoScroll) } {
	.txt see end
    }
}

proc ::tkchat::NickVis { val } {
    global Options

    foreach t [array names ::Options Visibility,NICK-*] {
	if { $Options($t) != $val } {
	    set Options($t) $val
	    DoVis [lindex [split $t ,] end]
	}
    }
}

proc ::tkchat::StampVis {} {
    global Options

    .txt tag config STAMP -elide $Options(Visibility,STAMP)

    set wid $Options(Offset)

    if { $Options(Visibility,STAMP) } {
	# Invisible
	.txt tag raise STAMP
	.txt config -tabs [list $wid l]
	.txt tag configure MSG -lmargin2 $wid
    } else {
	# Stamps visible
	foreach tag $Options(ElideTags) {
	    if { $Options(Visibility,$tag) } {
		.txt tag raise $tag STAMP
	    }
	}
	foreach tag [array names ::Options Visibility,NICK-*] {
	    if { $Options($tag) } {
		.txt tag raise [lindex [split $tag ,] end] STAMP
	    }
	}
	set wid_tstamp [expr { [font measure NAME "\[88:88\]"] + 5 }]
	set wid [expr { $wid + $wid_tstamp }]
	.txt config -tabs [list $wid_tstamp l $wid l]
	.txt tag configure MSG -lmargin2 $wid
    }
    if { $Options(AutoScroll) } {
	.txt see end
    }
}

proc ::tkchat::NickVisMenu {} {
    set m .mbar.vis.nicks
    $m delete 0 end
    set cnt 0
    foreach n [lsort -dict -index 0 $::Options(NickList)] {
	set n [lindex $n 0]
	set tag NICK-$n
	$m add checkbutton -label $n \
	    -onval 1 -offval 0 \
	    -var Options(Visibility,$tag) \
	    -command "::tkchat::DoVis $tag"
	if {$cnt >0 && $cnt % 25 == 0} {
	    $m entryconfig end -columnbreak 1
	}
	incr cnt
    }
}

proc ::tkchat::ScrolledWidgetSet {sbar f1 f2} {
    $sbar set $f1 $f2
    if {($f1 == 0) && ($f2 == 1)} {
	grid remove $sbar
    } else {
	grid $sbar
    }
}

proc ::tkchat::MakeScrolledWidget {w args} {
    global Options

    set parent [winfo parent $w]
    for {set n 0} {[winfo exists $parent.f$n]} {incr n} {}
    set f [frame $parent.f$n]
    set vs [scrollbar $f.vs -orient vertical -command [list $w yview]]
    $w configure -yscrollcommand [list $vs set]
    raise $w $f

    grid configure $w $vs -in $f -sticky news
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 0 -weight 1

    return $f
}

proc ::tkchat::About {} {
    variable rcsid
    global Options

    regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $rcsid -> rcsVersion

    # don't cache this window - of user reloads on the fly
    # we want to make sure it displays latest greatest info!
    catch {destroy .about}

    set w [toplevel .about -class dialog]
    wm withdraw $w
    wm transient $w .
    wm title $w "About TkChat $rcsVersion"
    button $w.b -text Dismiss -command [list wm withdraw $w]
    text $w.text -height 34 -bd 1 -width 100
    pack $w.b -fill x -side bottom
    pack $w.text -fill both -side left -expand 1
    $w.text tag config center -justify center
    $w.text tag config title -justify center -font {Courier -18 bold}
    $w.text tag config h1 -justify left -font {Sans -12 bold}
    $w.text insert 1.0 "About TkChat v$rcsVersion" title \
	"\n\nCopyright (C) 2001 Bruce B Hartweg <brhartweg@bigfoot.com>" \
	center "\n$rcsid\n\n" center

    $w.text insert end "Commands\n" h1 \
	"/msg <nick> <text>\t\tsend private message to user <nick>\n" {} \
	"/userinfo <nick>\t\tdisplay registered information for user <nick>\n" {} \
	"/afk ?reason?\t\tset your status to away with an optional reason\n" {} \
	"/back ?reason?\t\tindicate that you have returned\n" {} \
	"/away ?reason?\t\tsynonym for /afk\n" {} \
	"/google <text>\t\topen a google query for <text> in web browser\n" {} \
	"/googlefight <word> <word>\tperform a google fight between two words or phrases (in quotes)\n" {} \
	"/tip:<NUM>\t\topen the specified TIP document in web browser\n" {} \
	"/wiki <text>\t\tdo a wiki query with the remainder of the line\n" {} \
	"/bug ?group? ?tracker? id\topen a sourceforge tracker item in browser\n" {} \
	"/noisy ?<nick>? ?<minutes>?\tToggle <nick> noisy for x minutes (default 5)\n" {} \
	"\t\t\tmessages from noisy users are not diplayed.\n" {} \
	"\t\t\tNot specifying a nick will give you a list of noisy users.\n" {} \
	"/see <mark>\t\tgoto named mark or index (eg: bookmark1 end 0.0)\n" {} \
	"/alias <name> <type> <body>\ttype is 'proc' or 'script',\
	  type proc takes exactly one argument.\n\
	  \t\t\te.g: /alias foo script addSystem .txt \"test!\"\n" {} \
	"\t\t\t/alias foo proc thisProc\n" {} \
	"\t\t\tproc thisProc { arguments } { addSystem .txt \$arguments }\n" {} \
	"/unalias <pattern>\t\tremoves one or more aliases.\n\
    \t\t\te.g: /unalias f*\n" {} \
	"Searching\n" h1 \
	"/?<text>\t\t\tsearch the chat buffer for matching text.\
	 Repeating the command will progress\n\t\t\tto the previous match\n" {} \
	"/!\t\t\tclear the previous search result\n" {} \


    $w.text config -state disabled
    catch {::tk::PlaceWindow $w widget .}
    wm deiconify $w
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
			addSystem .txt "wrong # args: must be /alias name type body" end ERROR
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
		    addSystem .txt "wrong # args: must be /unalias name" end ERROR
		    set result 0
		}
	    }
	}
	default {
	    addSystem .txt "unknown alias processing directive" end ERROR
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
	addSystem .txt "alias \"$name\" modified"
    } else {
	# add new alias...
	lappend commandAliases(names) $name
	lappend commandAliases(types) $type
	lappend commandAliases(bodies) $body

	# show that we added it.
	addSystem .txt "alias \"$name\" added"
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
	    addSystem .txt "alias \"$alias\" matching \"$name\" removed"

	    set result 1; # yes, we matched at least one.
	}
    }

    return $result
}

proc ::tkchat::listAliases {} {
    # added by JJM 25/Sep/2003
    variable commandAliases

    addSystem .txt "there are [llength $commandAliases(names)] aliases defined"

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
	    addSystem .txt "alias \"$command_name\" ($command_type) error: $alias_error"
	}
    } else {
	set result 0
    }

    return $result
}

proc ::tkchat::userPostOneToOne {p jid} {
    global Options

    if {[winfo ismapped $p.eMsg]} {
	set str [$p.eMsg get]
    } else {
	set str [$p.tMsg get 1.0 end]
    }
    set msg [string trim $str]

    tkjabber::msgSend $msg -tojid $jid -type chat
    if { [string match "/me *" $msg] } {
	addAction $p.txt "" $Options(Nickname) [string range $msg 4 end]
    } else {
	tkchat::addMessage $p.txt "" $Options(Nickname) $msg end 0
    }
    $p.eMsg delete 0 end
    $p.tMsg delete 1.0 end
}

proc ::tkchat::userPost {{jid ""}} {
    global Options
    variable UserClicked

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
		{^/userinfo} {
		    set UserClicked 1
		    ::tkjabber::msgSend $msg
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
		{^/wiki[: ]} {
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
		    set q {http://www.googlefight.com/cgi-bin/compare.pl}
		    set n 1
		    foreach word [lrange $msg 1 end] {
			append q [expr {($n == 1) ? "?" : "&"}]
			append q q$n=$word
			incr n
		    }
		    if {[string match fr_* [msgcat::mclocale]]} {
			append q &langue=fr
		    } else {
			append q &langue=us
		    }
		    gotoURL $q
		}
		{^/log\s?} {
		    if { $msg eq "/log" } {
			# Set the global logging state
			set Options(ServerLogging) all
			addSystem .txt "Your messages will be logged by the server."
		    } else {
			# Send a single message with logging enabled:
			::tkjabber::msgSend [string trim [string range $msg 4 end]]
		    }
		}
		{^/nolog\s?} {
		    if { $msg eq "/nolog" } {
			# Set the global logging state
			set Options(ServerLogging) none
			addSystem .txt "Your messages will not be logged by the server."
		    } else {
			# Send a single message without logging:
			tkjabber::msgSend $msg -attrs [list nolog 1]
		    }
		}
		{^/nick\s?} {
		    tkjabber::setNick [string range $msg 6 end]
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
			tkjabber::msgSend "/nolog [string range $msg 4 end]" -attrs [list nolog 1]
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
		    if { [regexp {^/chat ([^ ]+)(?:\ (.*))?} $msg -> toNick privMsg] } {
			# Are we talking to a nick in this MUC or to an arbitrary JID?
			if {![regexp {([^@]+)@.*} $toNick toJID toNick]} {
			    set toJID $::tkjabber::conference/$toNick
			}
			set w [tkjabber::getChatWidget $toJID $toNick]
			set privMsg [string trim $privMsg]
			if {$privMsg ne ""} {
			    if { $w ne ".txt" } {
				tkchat::addMessage $w "" $Options(Nickname) $privMsg end 0
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
		    set tkjabber::AutoAway -1
		    tkjabber::away $status
		}
		{^/dnd}  -
		{^/busy} {
		    set status ""
		    regexp {^/(?:(?:afk)|(?:away))\s*(.*)$} $msg -> status
		    set tkjabber::AutoAway -1
		    tkjabber::away $status dnd
		}
		{^/back} {
		    set status [string range $msg 5 end]
		    tkjabber::back $status
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
    grid config $p.eMsg -in $p.btm -row 0 -column 1 -sticky ew
    $p.ml config -text ">>" -command [list ::tkchat::showExtra $p]
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
    grid config $p.tMsg -in $p.btm -row 0 -column 1 -sticky ew
    $p.ml config -text "<<" -command [list ::tkchat::hideExtra $p]
    $p.tMsg delete 1.0 end
    $p.tMsg insert end [$p.eMsg get]
    if { $::Options(AutoScroll) } {
	update
	$p.txt see end
    }
}

proc ::tkchat::logonScreen {} {
    global Options LOGON
    tkjabber::disconnect
    if {![winfo exists .logon]} {
	toplevel .logon -class dialog
	wm withdraw .logon
	wm transient .logon .
	wm title .logon "Logon to the Tcl'ers Chat"

	set lf [frame .logon.frame]
	checkbutton .logon.prx -text "Use Proxy" -var Options(UseProxy) \
	    -underline 7
	label .logon.lph -text "Proxy host:port" -underline 0
	frame .logon.fpx
	entry .logon.eph -textvar Options(ProxyHost)
	entry .logon.epp -textvar Options(ProxyPort) -width 5
	label .logon.lpan -text "Proxy Auth Username" -underline 11
	label .logon.lpap -text "Proxy Auth Password" -underline 13
	entry .logon.epan -textvar Options(ProxyUsername)
	entry .logon.epap -textvar Options(ProxyPassword) -show {*}
	label .logon.lnm -text "Chat Username" -underline 9
	label .logon.lpw -text "Chat Password" -underline 6
	entry .logon.enm -textvar Options(Username)
	entry .logon.epw -textvar Options(Password) -show *
	checkbutton .logon.rpw -text "Remember Chat Password" \
	      -var Options(SavePW) -underline 0
	frame .logon.fjsrv
	label .logon.ljsrv -text "Jabber server:port" -underline 0
	entry .logon.ejsrv -textvar Options(JabberServer)
	entry .logon.ejprt -textvar Options(JabberPort) -width 5
	label .logon.ljres -text "Jabber resource" -underline 3
	entry .logon.ejres -textvar Options(JabberResource)
	label .logon.lconf -text "Jabber conference" -underline 10
	entry .logon.econf -textvar Options(JabberConference)
	#checkbutton .logon.rjabberpoll -text "Use Jabber HTTP Polling" \
	#      -var Options(UseJabberPoll)

	frame .logon.sslopt -borderwidth 0
	radiobutton .logon.nossl -text "No SSL" \
	    -var Options(UseJabberSSL) -value no -underline 1 \
	    -command ::tkjabber::TwiddlePort
	radiobutton .logon.rjabberssl -text "Jabber SSL" \
	    -var Options(UseJabberSSL) -value ssl \
	    -command ::tkjabber::TwiddlePort
	radiobutton .logon.rstarttls -text "STARTTLS" \
	    -var Options(UseJabberSSL) -value starttls \
	    -command ::tkjabber::TwiddlePort

	checkbutton .logon.atc -text "Auto-connect" -var Options(AutoConnect) \
	    -underline 5
	frame  .logon.f  -border 0
	button .logon.ok -text "Logon" -command "set LOGON 1" -width 8 -underline 0
	button .logon.cn -text "Cancel" -command "set LOGON 0" -width 8 -underline 0
	button .logon.qu -text "Quit" -width 8 -underline 0 \
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
	    -in .logon.sslopt -side left

	grid .logon.prx -           -           -in $lf -sticky w -pady 3
	grid  x         .logon.lph  .logon.fpx  -in $lf -sticky w -pady 3
	grid  x         .logon.lpan .logon.epan -in $lf -sticky w -pady 3
	grid  x         .logon.lpap .logon.epap -in $lf -sticky w -pady 3
	grid .logon.lnm .logon.enm  -           -in $lf -sticky ew -pady 5
	grid .logon.lpw .logon.epw  -           -in $lf -sticky ew
	grid x          .logon.rpw  -           -in $lf -sticky w -pady 3
	grid x        .logon.ljsrv .logon.fjsrv -in $lf -sticky w -pady 3
	grid x        .logon.ljres .logon.ejres -in $lf -sticky w -pady 3
	grid x        .logon.lconf .logon.econf -in $lf -sticky w -pady 3
	grid x        .logon.sslopt -           -in $lf -sticky w -pady 3
	grid x        .logon.atc    -           -in $lf -sticky w -pady 3
	grid x        x             .logon.f    -in $lf -sticky e -pady 4

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
    vwait LOGON
    grab release .logon
    wm withdraw .logon
    if {$LOGON} {
	if {$Options(UseProxy)} {
	    catch {unset Options(ProxyAuth)}
	    ::http::config -proxyhost $Options(ProxyHost) \
		-proxyport $Options(ProxyPort)
	}
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
	.logon.$w config -state $s
    }
    if {$Options(SavePW)} {
	.logon.atc config -state normal
    } else {
	.logon.atc config -state disabled
	set Options(AutoConnect) 0
    }
}

proc ::tkchat::registerScreen {} {
    global Options

    set ::PasswordCheck ""
    set ::REGISTER ""
    set r .register

    if {![winfo exists $r]} {
	toplevel $r -class dialog
	wm withdraw $r
	wm transient $r .
	wm title $r "Register for the Tcler's Chat"

	label $r.
	label $r.lfn -text "Full name" -underline 9
	label $r.lem -text "Email address" -underline 9
	label $r.lnm -text "Chat Username" -underline 9
	label $r.lpw -text "Chat Password" -underline 6
	label $r.lpwc -text "Confirm Password" -underline 6
	entry $r.efn -textvar Options(Fullname)
	entry $r.eem -textvar Options(Email)
	entry $r.enm -textvar Options(Username)
	entry $r.epw -textvar Options(Password) -show *
	entry $r.epwc -textvar ::PasswordCheck -show *

	button $r.ok -text "Ok" -command "set ::REGISTER ok" -width 8 -underline 0
	button $r.cn -text "Cancel" -command "set ::REGISTER cancel"  -width 8 -underline 0

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
	vwait ::REGISTER
	if { $::REGISTER eq "cancel" } {
	    break
	}
	if { $Options(Password) ne $::PasswordCheck } {
	    tk_messageBox -message "The passwords do not match." \
		    -title "Password mismatch" -type ok
	    continue
	}
	break
    }
    grab release $r
    wm withdraw $r
    return [expr { $::REGISTER eq "ok" }]
}


proc ::tkchat::doBug {msg} {
    # msg should be off form: ^/bug[: ]id
    if {[llength $msg] != 2} {
	addSystem .txt "wrong # args: must be /bug id" end ERROR
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
	    addSystem .txt "Bummer. Could not find '$searchString'"
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
    set init "#[lindex $::DlgData(Color,$idx) 3]"
    set tmp [tk_chooseColor -title "Select Override Color" -initialcolor $init]
    if { $tmp ne "" } {
	lset ::DlgData(Color,$idx) 3 [string range $tmp 1 end]
	$w config -foreground $tmp -selectcolor $tmp
    }
}

proc ::tkchat::buildRow { f idx disp } {
    global DlgData
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
	    -variable DlgData($idx) \
	    -value 1 \
	    -foreground  "#[lindex $DlgData(Color,$idx) 1]" \
	    -selectcolor "#[lindex $DlgData(Color,$idx) 1]"
    ::tk::radiobutton $f.inv$seq -padx 0 -pady 0 -font FNT -indicatoron 0 \
	    -text "inverted" \
	    -variable DlgData($idx) \
	    -value 2 \
	    -foreground "#[lindex $DlgData(Color,$idx) 2]" \
	    -selectcolor "#[lindex $DlgData(Color,$idx) 2]"
    ::tk::radiobutton $f.ovr$seq -padx 0 -pady 0 -font FNT -indicatoron 0 \
	    -text "custom" \
	    -variable DlgData($idx) \
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
    set t .macros
    catch {destroy $t}
    toplevel $t -class Dialog
    wm transient $t .
    wm withdraw $t
    wm title $t "Edit Macros"

    listbox $t.lst -yscroll "$t.scr set" -font FNT -selectmode extended
    scrollbar $t.scr -command "$t.lst yview"
    label $t.lbl1 -text "Macro:" -font NAME
    entry $t.mac -width 10 -font FNT -validate all -vcmd {regexp -- {^\S*$} %P}
    bind $t.mac <Return> "focus $t.txt"
    label $t.lbl2 -text "Text:" -font NAME
    entry $t.txt -width 40 -font FNT
    bind $t.txt <Return> "$t.sav invoke"
    bind $t.lst <Double-1> "::tkchat::MacroSel %W @%x,%y"
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
    label $t.hlp -text $help -font FNT -justify left

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
	catch {unset Options(Macro,$m)}
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
    global Options DlgData

    # clear old data
    catch {unset DlgData}
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
    wm protocol $t WM_DELETE_WINDOW {set ::DlgDone cancel}
    wm withdraw $t
    wm title $t "Color Settings"

    label $t.l1 -text "Posting Color" -font NAME
    label $t.l2 -text "Example Text" -background white \
	-foreground \#$DlgData(MyColor) -font ACT
    button $t.myclr -text "Change..." -font FNT -command {
	set tmp [tk_chooseColor \
		       -title "Select Your User Color" \
		       -initialcolor \#$::DlgData(MyColor)]
	if { $tmp ne "" } {
	    .opts.l2 config -foreground $tmp
	    set ::DlgData(MyColor) [string range $tmp 1 end]
	}
    }

    label $t.l3 -text "Display Color Overrides" -font NAME
    frame $t.f -relief sunken -bd 2 -height 300
    canvas $t.f.cvs -yscrollcommand [list $t.f.scr set] \
	  -width 10 -height 300 -highlightthickness 0 -bd 0
    scrollbar $t.f.scr -command [list $t.f.cvs yview]
    pack $t.f.cvs -side left -expand 1 -fill both
    pack $t.f.scr -side left -fill y
    set f [frame $t.f.cvs.frm]
    $t.f.cvs create window 0 0 -anchor nw -window $f
    bind $f <Configure> {
	[winfo parent %W] config -width [expr {%w+5}] -scrollregion [list 0 0 %w %h]
    }
    foreach {key str} { 1 "All\nDefault" 2 "All\nInverted" 3 "All\nCustom"} {
	button $f.all$key -text $str -padx 0 -pady 0 -font SYS -command \
		[string map [list %val% $key] {
		    foreach idx [array names DlgData Color,*] {
			set idx [lindex [split $idx ,] end]
			set DlgData($idx) %val%
		    }
		}]
    }
    grid x $f.all1 $f.all2 $f.all3 x -padx 1 -pady 1
    foreach {idx str} {MainBG Background MainFG Foreground SearchBG Searchbackgr} {
	buildRow $f $idx $str
    }
    grid [label $f.online -text "Online Users" -font SYS] - - -
    foreach nick [lsort -dict $Options(OnLineUsers)] {
	if { [info exists DlgData(Color,NICK-$nick)] } {
	    buildRow $f NICK-$nick $nick
	}
    }
    grid [label $f.offline -text "Offline Users" -font SYS] - - -
    foreach nick [lsort -dict -index 0 $Options(NickList)] {
	set nick [lindex $nick 0]
	if { [lsearch -exact $Options(OnLineUsers) $nick] < 0 } {
	    buildRow $f NICK-$nick $nick
	}
    }
    frame $t.f2
    button $t.f2.ok -width 8 -text "OK" -command {set DlgDone ok} -font SYS
    button $t.f2.app -width 8 -text "Apply" -command {set DlgDone apply} -font SYS
    button $t.f2.can -width 8 -text "Cancel" -command {set DlgDone cancel} -font SYS
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
	vwait ::DlgDone
	switch -- $::DlgDone {
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
	    applyColors
	}
    }
    destroy $t
}

proc ::tkchat::applyColors { {txt .txt} } {
    global Options

    # update colors
    $txt config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    .names config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    $txt tag configure found -background "#[getColor SearchBG]"
    foreach nk $Options(NickList) {
	set nk [lindex $nk 0]
	set clr [getColor $nk]
	$txt tag config NICK-$nk -foreground "#$clr"
	$txt tag config NOLOG-$nk -foreground "#[fadeColor $clr]"
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
    }
}

proc ::tkchat::ChatLogHook {who str url} {
    global Options
    if {! $Options(ChatLogOff)} {
	set T [clock format [clock seconds] -format "%Y%m%dT%H:%M:%S"]
	puts $Options(ChatLogChannel) "$T: $who\t$str"
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

    if { [info exists ::env(HOME)] } {
	set rcfile [file join $::env(HOME) .tkchatrc]
	set Options(Geometry) [wm geometry .]
	if { [winfo exists .pane] && $Options(DisplayUsers) } {
	    set Options(Pane) [.pane sash coord 0]
	}
	array set tmp [array get Options]

	# Don't save these options to resource file
	set ignore {
	    ChatLogChannel ElideTags FetchTimerID FinalList History
	    JabberConnect JabberDebug JabberLogs NamesWin Offset OnlineTimerID
	    OnLineUsers PaneUsersWidth ProxyAuth ProxyPassword URL URL2 URLchk
	    URLlogs errLog retryFailedCheckPage
	}

	# Deprecated Options
	append ignore {
	    FetchToken OnlineToken Refresh TimeFormat TimeGMT hideTraffic
	    Popup,USERINFO Popup,WELCOME Popup,MEMO Popup,HELP
	    Visibility,USERINFO Visibility,WELCOME Visibility,MEMO
	    Visibility,HELP
	}

	# Trim down NickList
	set tmp(NickList) {}
	set MainFG [getColor MainFG]
	foreach nk $Options(NickList) {
	    set nick [lindex $nk 0]
	    set keepNick 0
	    if { !$Options(Visibility,NICK-$nick) } {
		lappend ignore Visibility,NICK-$nick
	    } else {
		incr keepNick
	    }
	    if { [lindex $Options(Color,NICK-$nick) 0] == 1 \
		    && [lindex $Options(Color,NICK-$nick) 1] eq $MainFG } {
		lappend ignore Color,NICK-$nick
	    } else {
		incr keepNick
	    }

	    if { $keepNick } {
		lappend tmp(NickList) $nk
	    }
	}

	if { !$tmp(SavePW) } {
	    lappend ignore Password
	}
	foreach idx $ignore {
	    catch { unset tmp($idx) }
	}
	set oplist [list]
	foreach option [lsort [array names tmp]] {
	    lappend oplist [list $option $tmp($option)]
	}
	if { ![catch { open $rcfile w 0600 } fd] } {
	    fconfigure $fd -encoding utf-8
	    puts $fd "# Auto-generated file: DO NOT MUCK WITH IT!"
	    puts $fd "array set Options \{"
	    puts $fd [join $oplist "\n"]
	    puts $fd "\}"
	    puts $fd "# Auto-generated file: DO NOT MUCK WITH IT!"
	    close $fd
	}
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
	    unset ::Options
	    after 2000 [linsert $::argv 0 ::tkchat::Init]
	}
	retrieve {
	    Retrieve
	}
	purge {
	    .txt config -state normal
	    .txt delete 1.0 end
	    .txt mark unset HISTORY
	    set ::Options(History) {}
	    set ::Options(Offset) 50
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
    set font [::dkfFontSel::dkf_chooseFont \
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
	}
    }
}

proc ::tkchat::anim {image {idx 0}} {
    namespace eval ::tkchat::img {} ; # create img namespace
    set this [lindex [info level 0] 0]
    if {[catch {$image configure -format "GIF -index $idx"}]} {
	if {$idx == 1} {
	    # stop animating, only base image exists
	    return
	}
	set cmd [list ::tkchat::anim $image]
    } else {
	set cmd [list ::tkchat::anim $image [expr {$idx + 1}]]
    }
    catch {after cancel $::tkchat::img::id($image)}
    set ::tkchat::img::id($image) [after $::tkchat::img::delay $cmd]
}

proc ::tkchat::SmileId {{image {}} args} {
    # Here be magic
    variable IMG
    foreach arg $args {	set IMG($arg) $image }
    # Do some checking so that things like 'C:/temp/tcl98/blah' and
    # 'lollipop' don't get smileys inserted
    set ids ""
    foreach arg [array names IMG] {
	if {[string is alnum -strict -failindex i $arg]} {
	    lappend ids "\1$arg\1"
	} elseif {$i > 0} {
	    lappend ids "\2$arg"
	} else {
	    lappend ids "\3$arg"
	}
    }
    set ids [join $ids "\0"]
    # The double-back is needed because when map is converted to a list,
    # it will become a single-back.
    set map {
	| \\| ( \\( ) \\) [ \\[ - \\- . \\. * \\* ? \\?
				\\ \\\\ ^ \\^ $ \\$ \1 \\y \2 \\m \3 \\Y \0 |
    }
    # If we ever change this to use () capturing, change tkchat::Insert too.
    set ::tkchat::IMGre [string map $map $ids]
}

proc ::tkchat::Smile {} {
    namespace eval ::tkchat::img {} ; # create img namespace
    set ::tkchat::img::delay 400
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
	R0lGODlhEAAQAPECAAAAAH9/f////wAAACH5BAEAAAIALAAAAAAQABAAAAKR
	TJgwoUSJEiVKlKgwYUKJEiVKlChRYcKIEiVKlKgwYUSJEiVKlChRYcKIEiVK
	lChRosKEEiVKlChRokSJEiVKlCgRIECAACVKlCgRIECAAAFKlCgRIECAAAVK
	lCgRIECAAAVKlCgRIECAAAFKlCgRIECAACVKlChRIECAECVKFAgQIECAAAFC
	lCgQIECAACFKBQA7
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
    SmileId beer |_P
    image create photo ::tkchat::img::beer -format GIF -data {
	R0lGODlhEAAQAPECAAAAAP//AP///wAAACH5BAEAAAIALAAAAAAQABAAAAKR
	lChRokSJEiVKhChRokSJECVKhCgQoECAECVKBAgwIMCEACFKRJgwYcKEAAFK
	RBgwYcKEEAVCRJgwYcKEECUCRJgwYcKEECUCRJgwIcKEECUCRIgwYcKEECUC
	RJgwYUKEEAVCRJgwYcKEAAFKRJgwYMKEACFKRJgwYcKEECVKRJgwYcKEECVK
	BAgQIECAECVKBQA7
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
    set t .smileys
    if {[winfo exists $t]} {
	wm deiconify $t
	raise $t
    } else {
	variable IMG
	foreach {i e} [array get IMG] {
	    lappend tmp($e) $i
	}
	toplevel $t
	catch {::tk::PlaceWindow $t widget .}
	wm title $t "Available Emoticons"
	wm protocol $t WM_DELETE_WINDOW [list wm withdraw $t]
	set txt [text $t.txt -font NAME -tabs {1.5i l 2.0i l} \
		       -height [expr {[llength [image names]] + 5}]]
	if { [tk windowingsystem] eq "win32" } {
	    $txt configure -background SystemButtonFace -foreground SystemButtonText
	} else {
	    $txt configure -background "#c0c0c0" -foreground black
	}
	set sb [scrollbar $t.sb -command [list $txt yview]]
	$txt configure -yscrollcommand [list $sb set]

	foreach image [lsort [image names]] {
	    set name [lindex [split $image :] end]
	    $txt insert end "$name\t"
	    $txt image create end -image $image
	    if {[info exists tmp($name)]} {
		$txt insert end "\t[join $tmp($name) "   "]"
	    }
	    $txt insert end \n
	}
	$txt configure -state disabled
	grid $txt $sb -sticky news
	grid rowconfigure $t 0 -weight 1
	grid columnconfigure $t 0 -weight 1
	if {[llength [info command ::tk::PlaceWindow]] > 0} {
	    tk::PlaceWindow $t widget .
	}
    }
}
proc ::tkchat::Init {args} {
    global Options env

    # set intial defaults
    set ::URLID 0
    set ::tkchat::eCURR 0
    set ::tkchat::eHIST ""

    # set defaults for User Settable Options
    array set Options {
	UseProxy		0
	ProxyHost		""
	ProxyPort		""
	Username		""
	Password		""
	SavePW			0
	Nickname		""
	UseJabberPoll		0
	UseJabberSSL		no
	JabberServer		all.tclers.tk
	JabberPort		5222
	JabberConference	tcl@tach.tclers.tk
	ServerLogging		all
	MyColor			000000
	FetchTimerID		-1
	OnlineTimerID		-1
	AutoConnect		0
	DisplayUsers		1
	NickList		{{} {}}
	History			{}
	AutoScroll		0
	Geometry		600x500
	Pane			{520 2}
	UsePane			1
	Font,-family		Helvetica
	Font,-size		-12
	MaxLines		500
	ChatLogFile		""
	LogFile			""
	LogLevel		notice
	errLog			stderr
	emoticons		1
	HistoryLines		-1
	timeout			30000
	Emoticons		1
	AnimEmoticons		0
	Style			{any}
	Theme			{}
	Transparency		100
	AutoFade		0
	AutoFadeLimit		80
	AutoAway		-1
	EnableWhiteboard	1
	Visibility,SINGLEDOT	0
	Visibility,STAMP	1
	Visibility,ERROR	0
	Alert,SOUND		0
	Alert,RAISE		1
	Alert,ALL		0
	Alert,ME		1
	Alert,TOPIC		1
	Alert,NORMAL		1
	Alert,ACTION		1
	WhisperIndicatorColor	#ffe0e0
	EntryMessageColor	#002500
	ExitMessageColor	#250000
	UseBabelfish		0
	JabberResource		tkchat
	OneToOne		tabbed
    }
    catch { set Options(BROWSER) $env(BROWSER) }
    foreach { name clr } { MainBG FFFFFF MainFG 000000 SearchBG FF8C44 } {
	set Options(Color,$name) [list 1 $clr [invClr $clr] $clr]
    }

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
		set Options(Color,NICK-$nk) [string map {Web 1 Inv 2 Mine 3} \
			$Options(Color,$nk,Which)]
		lappend Options(Color,NICK-$nk) $Options(Color,$nk,Web) \
			$Options(Color,$nk,Inv) $Options(Color,$nk,Mine)
	    }
	}
	unset -nocomplain "Options(Color,$nk,Inv)"
	unset -nocomplain "Options(Color,$nk,Mine)"
	unset -nocomplain "Options(Color,$nk,Web)"
	unset -nocomplain "Options(Color,$nk,Which)"
    }

    # Convert old nick list (<1.289) to new and remove expired nicks
    set newNicks [list]
    foreach nk $Options(NickList) {
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

    # Compatability issues...
    if { [string is integer $Options(UseJabberSSL)] } {
	set Options(UseJabberSSL) [lindex {no ssl} $Options(UseJabberSSL)]
    }
    if { $::tcl_platform(os) eq "Windows CE" } {
	# Disable history loading on wince
	set Options(HistoryLines) 0
    }

    # Set the 'Hardcoded' Options:
    array set Options {
	JabberLogs	"http://tclers.tk/conferences/tcl"
	Offset		50
	ElideTags	{ SINGLEDOT AVAILABILITY TRAFFIC SYSTEM ERROR }
	History		{}
	OnLineUsers	{}
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
	    -nick - -nickname { set Options(Nickname) [Pop args 1] }
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
    } else {
	set Options(ChatLogOff) 1
    }

    SetTheme $Options(Theme)

    # do this first so we have images available
    Smile
    # build screen
    CreateGUI
    foreach idx [array names Options Visibility,*] {
	set tag [lindex [split $idx ,] end]
	.txt tag config $tag -elide $Options($idx)
    }

    Hook add chat [namespace origin IncrMessageCounter]
    BookmarkInit
    WinicoInit

    if {$Options(UseProxy)} {
	if {$Options(ProxyHost) != "" && $Options(ProxyPort) != ""} {
	    ::http::config -proxyhost $Options(ProxyHost) \
		-proxyport $Options(ProxyPort)
	} elseif {[info exists ::env(http_proxy)]} {
	    if {[regexp {(?:http://)?([[:alnum:].-]+)(?::(\d+))?} \
		     $::env(http_proxy) -> \
		     Options(ProxyHost) \
		     Options(ProxyPort)]} {
		http::config -proxyhost $Options(ProxyHost) \
		    -proxyport $Options(ProxyPort)
	    }
	}
    }

    ChangeFont -family $Options(Font,-family)
    ChangeFont -size $Options(Font,-size)

    applyColors
    createRosterImages

    #call the (possibly) user defined postload proc:
    ::tkchat::rcPostload

    # connect
    if {! $nologin} {
	if {$Options(AutoConnect)} {
	    logonChat
	} else {
	    logonScreen
	}
    }
}

#############################################################
#
# askLEO
#
# This is a web scraper for English/German
# translation via http://dict.leo.org
#
# Translation is invoked on the current X selection
# (needn't be inside of tkChat) via <Shift-Button3>.
#
# Authors:
#
# 2002 - Reinhard Max	  <Reinhard.Max@gmx.de>
#	 Martin Scherbaum <maddin@scherbaum.org>
#
#############################################################
namespace forget ::dict.leo.org

namespace eval ::dict.leo.org {

    namespace export query askLEO askLEOforSelection

    package require Tk
    package require http
    package require htmlparse

    variable table ""
    variable last  ""
    variable Query ""
    variable td
    variable tdcounter 0
    variable leoURL http://pda.leo.org
}

proc ::dict.leo.org::parse {tag close options body} {
    variable td
    variable table
    variable tdcounter

    switch -- $close$tag {

	/TR - /tr {
	    if {[info exists td(2)] && [info exists td(3)]} {
		lappend table [string trim $td(2)] [string trim $td(3)]
	    }
	    set tdcounter 0
	    array unset td
	}

	td - td { incr tdcounter }

	default {
	    set item [htmlparse::mapEscapes $body]
	    if {[string length $item]} {
		append td($tdcounter) $item
	    }
	}
    }
}

proc ::dict.leo.org::query {query} {
    variable table
    variable leoURL
    set query [::http::formatQuery search $query]
    set tok [::http::geturl $leoURL -query $query]
    foreach line [split [::http::data $tok] "\n"] {
	if {[string match "*ENGLISCH*DEUTSCH*" $line]} break
    }
    ::http::cleanup $tok
    set table ""
    ::htmlparse::parse -cmd ::dict.leo.org::parse $line
	return $table
}

proc ::dict.leo.org::max {a b} {expr {$a > $b ? $a : $b}}

proc ::dict.leo.org::askLEOforSelection {} {
    if {![catch {selection get} query]} {
	askLEO $query
    }
}

proc ::dict.leo.org::askLEO {{query {}}} {

    variable w
    variable last
    variable Query
    set query [string trim $query]
    if { $query ne "" } {
	set Query $query
    }
    if { $Query ne $last } {
	$w.bot.text configure -state normal
	$w.bot.text delete 1.0 end
	$w.bot.text configure -state disabled
	if {$Query != ""} {
	    $w.bot.text configure -cursor watch
	    update
	    set table [dict.leo.org::query $Query]
	    set max 0
	    foreach c $table {set max [max $max [string length $c]]}
	    $w.bot.text configure -state normal
	    if {$max} {
		set sep [string repeat = $max]
		set table [linsert $table 0 " English" " Deutsch" $sep $sep]
		foreach {c1 c2} $table {
		    $w.bot.text insert end \
			[format "%-*s  %-*s\n" $max $c1 $max $c2]
		}
	    } else {
		$w.bot.text insert end {No matches}
	    }
	    $w.bot.text configure -state disabled
	    if {![winfo ismapped $w]} {wm deiconify $w} else {raise $w}
	    $w.bot.text configure -cursor ""
	}
    }
    set last $Query
}

proc ::dict.leo.org::init {} {
    variable w .leo
    variable LEOlogo
    catch {destroy $w}
    toplevel $w
    wm withdraw $w
    bind $w <Double-Button-3> [list wm withdraw $w]
    wm protocol $w WM_DELETE_WINDOW [list wm withdraw $w]
    frame $w.main
    frame  $w.top
    entry  $w.top.ent -bg white -textvariable [namespace current]::Query
    button $w.top.but -text "ask LEO" -command [namespace code askLEO]
    bind   $w.top.ent <Return> [list $w.top.but invoke]

    pack $w.top.ent -expand yes -fill x -side left
    pack $w.top.but -expand no -fill none -side left
    pack $w.top	    -fill x -in $w.main

    frame $w.bot
    scrollbar $w.bot.scry -command [list $w.bot.text yview]
    scrollbar $w.bot.scrx -orient horizontal -command [list $w.bot.text xview]
    text $w.bot.text -wrap no -font fixed -state disabled \
	-yscrollcommand [list $w.bot.scry set] -xscrollcommand [list $w.bot.scrx set]
    grid $w.bot.text -row 0 -column 0 -sticky nsew
    grid $w.bot.scry -row 0 -column 1 -sticky ns
    grid $w.bot.scrx -row 1 -column 0 -sticky ew
    grid rowconfigure $w.bot 0 -weight 1
    grid columnconfigure $w.bot 0 -weight 1
    bind $w.bot.text <Button-2> [namespace code askLEOforSelection]
    #pack $w.bot.text -expand yes -fill both -side right
    pack $w.bot -expand yes -fill both -in $w.main

    pack $w.main -expand yes -fill both
    focus $w.top.ent
    wm title $w "askLEO"

    if { $::tcl_platform(platform) ne "windows" } {
	image create photo LEOlogo -data $LEOlogo
	toplevel $w.icon -bg ""
	pack [label $w.icon.l -image LEOlogo]
	wm iconwindow $w $w.icon
    }
}

set ::dict.leo.org::LEOlogo {
    R0lGODlhKAAoAPQUAAAAAAAAMwAzAAAzMzMAADMAM2YAADMz/8wAM/8AM8zMAMzMM8z/AMz/
    M//MAP/MM///AP//M///7v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEBABQALAAAAAAoACgAAAX+IEVRFEVRFEVR1AEAFEVRFEVRFEUZAEWBFEVRFEVRFEVR
    FEVRFEVR1DEhAEBRFAVSFEVRBgAAAEVRFEVRFEVRFEVRFEVRB3gcyHEAFEVRFEVRFAAAAAAA
    FEVRFEVRIEVRFEVR1HEcE3IcE0BRFEVRlAEAAACAAABQFEVRFEVRFEVRx3EcyIQcBwIAFAVS
    FEUZAAAAAAAAAEBRFEVRFEVRCHIcBzhNxzEhE0BRFEUZRgIAAAAAAAAAAEVRIEVRxzEhxzEh
    xzEhyAEAFEVRhpEkCQCAAAAAAAAAAEVRx3FME3JMCDJNx3EAAAVSlGEkSZIkCQAAAAAAAABQ
    x3Ecx4T+HBOITNNxHNMEAJRhJEmSJEmSJAAAAAAAAOBxHMdxTBMyAQUwIRMCFABQGEmSJEmS
    gEmSJAAAAACATNOEIMiETIXzAAiCFA4AFg4QPEkCFEmSJEmSAABAIQiCTNM0TRMIOAExBYTy
    OE7jOAVQAA6QJEmSJEkCUCB1HMd0IMiEBM5ABI4TAYHjBITzAAUUgEmSJEmSJAlFHdM0Icg0
    HUTgPEDkAI8DRs7jDE7gPMCTJEmSJAlFUcdxIMg0IQcYOIATOYETEM7jBIDjRMDzPEmSJElC
    USBFIQhyTAhyEE7kOM4DTFPgCNMUOMHzgM/zPEmSUBRFUQeCHBNyBM7jPMr+4wTUBAIPME2E
    4wTP8zzP8yQURVEUdRzThBAO+AiP4zgRUACFExBB4AzE8zzP8zwURVEgRVHUMSFH4ASO80CP
    4wCFAz0AAT3A84DP8zzPQ1EURVEURSHIARTQoziP4jxOBCqP4zwOQBXU8zzPQ1EURVEURVHI
    QVEA+ACR4zxA5DiP8zCP8wAPQFHPQ1EURVEgRVEURVEUVQCO80CPEziP4ziPEzmPE4AUAVAU
    RVEURVEURVEUFTiR8yjA4zgBADqN4yjO40BOQFEURVEURVEURVEURREA+DhO4DgP5DxO4DyR
    owRO4AQURVEURVEgRVEURVGBEwGPMzgPFAAB5AT+BFAAkeOAEUFRFEVRFEVRFEVRhPM4DvQ4
    QOA4jgM+jhMAheM8ABAQFEVRFEVRFEVRVAA4D+gEzuM4QVBAj/NAgfM40OMUFEVRFEVRIEVR
    FEVRgQMETuA4keNAgRM5TuBAi/OAAEVRFEVRFEVRFEVRhBNATuQEwOM8jgMGQAA4AREQAUVR
    FEVRFEVRFEVRVEA8Dhg4jhMBBfA4zuNEzuNAAUVRFEVRFEVRIEVRFEVRARQ4zuM4D+QETuQ4
    geM8DkCBFEVRFEVRFEVRFEVRBFA4zgM9gPMoTgACAeEEQEBQFEVRFEVRFEVRFEVRVOBAD+hE
    CvA4DxA5jvNAjwMFFEXiURRFURRFUSBFURRFEQ/wKM7jOJETOM4DPYDzOE5AgRRFURRFURRF
    URRFUUBAAU7gPJDyOM4DOovzAE7gRABFURRFURRFURRFUVQBUAXoRIDzAM4DBY4TOJHjBE5B
    URRFURRFUSBFURRFURRFBU7hBM4DBY4zEI7zAFQAgBRFURRFURRFURRFURRFUURAAYUDBcQD
    AhQVOBFAURRFURRFURRFURRFURRFURRIARQlPABFARRFUQFBURRFURRFURRFUSBFURRFURRF
    URRFUQVFURRFURRFURRFgRRFURRFURQVAgA7
}

::dict.leo.org::init

#######################################################################
#
# dkf's font selection dialog
#
#######################################################################

namespace eval ::dkfFontSel {

    # Local procedure names (ones that it is a bad idea to refer to
    # outside this namespace/file) are prepended with an apostrophe
    # character.  There are no externally useful variables.

    # First some library stuff that is normally in another namespace

    # Simple (nay, brain-dead) option parser.  Given the list of
    # arguments in arglist and the list of legal options in optlist,
    # parse the options to convert into array values (which are stored
    # in the caller's array named in optarray.  Does not handle errors
    # spectacularly well, and can be replaced by something that does a
    # better job without me feeling to fussed about it!
    proc 'parse_opts {arglist optlist optarray} {
	upvar $optarray options
	set options(foo) {}
	unset options(foo)
	set callername [lindex [info level -1] 0]
	if {[llength $arglist]&1} {
	    return -code error \
		    "Must be an even number of arguments to $callername"
	}
	array set options $arglist
	foreach key [array names options] {
	    if {![string match -?* $key]} {
		return -code error "All parameter keys must start\
			with \"-\", but \"$key\" doesn't"
	    }
	    if {[lsearch -exact $optlist $key] < 0} {
		return -code error "Bad parameter \"$key\""
	    }
	}
    }

    # Capitalise the given word.  Assumes the first capitalisable
    # letter is the first character in the argument.
    proc 'capitalise {word} {
	set cUpper [string toupper [string index $word 0]]
	set cLower [string tolower [string range $word 1 end]]
	return ${cUpper}${cLower}
    }

    # The classic functional operation.  Replaces each element of the
    # input list with the result of running the supplied script on
    # that element.
    proc 'map {script list} {
	set newlist {}
	foreach item $list {
	    lappend newlist [uplevel 1 $script [list $item]]
	}
	return $newlist
    }

    # ----------------------------------------------------------------------
    # Now we start in earnest
    namespace export dkf_chooseFont

    variable Family Helvetica
    variable Size   12
    variable Done   0
    variable Win    {}

    array set Style {
	bold 0
	italic 0
	underline 0
	overstrike 0
    }

    # Get the gap spacing for the frameboxes.  Use a user-specified
    # default if there is one (that is a valid integer) and fall back
    # to measuring/guessing otherwise.
    proc 'get_gap {w} {
	set gap [option get $w lineGap LineGap]
	if {[catch {incr gap 0}]} {
	    # Some cunning font measuring!
	    ::tk::label $w._testing
	    set font [$w._testing cget -font]
	    set gap [expr {[font metrics $font -linespace]/2+1}]
	    destroy $w._testing
	}
	return $gap
    }


    # Build the user interface (except for the apply button, which is
    # handled by the 'configure_apply procedure...
    proc 'make_UI {w} {
	# Framed regions.  Do this with grid and labels, as that seems
	# to be the most effective technique in practise!
	frame $w.border1 -class DKFChooseFontFrame
	frame $w.border2 -class DKFChooseFontFrame
	frame $w.border3 -class DKFChooseFontFrame
	frame $w.border4 -class DKFChooseFontFrame
	set gap ['get_gap $w]
	grid $w.border1 -row 0 -column 0 -rowspan 4 -columnspan 4 \
		-padx $gap -pady $gap -sticky nsew
	grid $w.border2 -row 0 -column 4 -rowspan 4 -columnspan 3 \
		-padx $gap -pady $gap -sticky nsew
	grid $w.border3 -row 4 -column 0 -rowspan 3 -columnspan 9 \
		-padx $gap -pady $gap -sticky nsew
	grid $w.border4 -row 7 -column 0 -rowspan 3 -columnspan 9 \
		-padx $gap -pady $gap -sticky nsew
	incr gap $gap
	foreach col {0 3 4 6 8} {
	    grid columnconfigure $w $col -minsize $gap
	}
	foreach row {0 3 4 6 7 9} {
	    grid rowconfigure    $w $row -minsize $gap
	}
	grid columnconfigure $w 1 -weight 1
	grid rowconfigure    $w 1 -weight 1
	grid rowconfigure    $w 8 -weight 1


	# Labels for the framed boxes & focus accelerators for their contents
	foreach {subname row col focusWin} {
	    Family 0 1 .family
	    Style  0 5 .style.sBold
	    Size   4 1 .size.b8
	    Sample 7 1 .sample.text
	} {
	    set l [label $w.lbl$subname]
	    grid $l -row $row -column $col -sticky w
	    set accel ['get_accel $l]
	    if {[string length $accel]} {
		bind $w <$accel> [list focus $w$focusWin]
	    }
	}


	# Font families
	frame $w.familyBox
	listbox $w.family -exportsel 0 -selectmode browse \
		-xscrollcommand [list $w.familyX set] \
		-yscrollcommand [list $w.familyY set]
	scrollbar $w.familyX -command [list $w.family xview]
	scrollbar $w.familyY -command [list $w.family yview]
	foreach family ['list_families] {
	    $w.family insert end ['map 'capitalise $family]
	}
	grid $w.familyBox -row 1 -column 1 -rowspan 1 -columnspan 2 \
		-sticky nsew
	grid columnconfigure $w.familyBox 0 -weight 1
	grid rowconfigure    $w.familyBox 0 -weight 1
	grid $w.family  $w.familyY -sticky nsew -in $w.familyBox
	grid $w.familyX            -sticky nsew -in $w.familyBox
	bind $w.family <1> [namespace code {'change_family %W [%W nearest %y]}]
	bindtags $w.family [concat [bindtags $w.family] key$w.family]
	bind key$w.family <Key> [namespace code {'change_family %W active %A}]


	# Font styles.
	frame $w.style
	grid $w.style -row 1 -column 5 -sticky news
	grid columnconfigure $w.style 0 -weight 1
	foreach {fontstyle lcstyle row next prev} {
	    Bold      bold       0 Italic    {}
	    Italic    italic     1 Underline Bold
	    Underline underline  2 Strikeout Italic
	    Strikeout overstrike 3 {}        Underline
	} {
	    set b $w.style.s$fontstyle
	    checkbutton $b -variable [namespace current]::Style($lcstyle) \
		    -command [namespace code 'set_font]
	    grid $b -sticky nsew -row $row
	    grid rowconfigure $w.style $row -weight 1
	    if {[string length $next]} {
		bind $b <Down> [list focus $w.style.s$next]
	    }
	    if {[string length $prev]} {
		bind $b <Up> [list focus $w.style.s$prev]
	    }
	    bind $b <Tab>       "[list focus $w.size.b8];break"
	    bind $b <Shift-Tab> "[list focus $w.family ];break"
	    set accel ['get_accel $b]
	    if {[string length $accel]} {
		bind $w <$accel> "focus $b; $b invoke"
	    }
	    bind $b <Return> "$b invoke; break"
	}


	# Size adjustment.  Common sizes with radio buttons, and an
	# entry for everything else.
	frame $w.size
	grid $w.size -row 5 -column 1 -rowspan 1 -columnspan 7 -sticky nsew
	foreach {size row col u d l r} {
	    8  0 0  {} 10 {} 12
	    10 1 0   8 {} {} 14
	    12 0 1  {} 14  8 18
	    14 1 1  12 {} 10 24
	    18 0 2  {} 24 12 {}
	    24 1 2  18 {} 14 {}
	} {
	    set b $w.size.b$size
	    radiobutton $b -variable [namespace current]::Size -value $size \
		    -command [namespace code 'set_font]
	    grid $b -row $row -column $col -sticky ew
	    #grid columnconfigure $w.size $col -weight 1
	    if {[string length $u]} {bind $b <Up>    [list focus $w.size.b$u]}
	    if {[string length $d]} {bind $b <Down>  [list focus $w.size.b$d]}
	    if {[string length $l]} {bind $b <Left>  [list focus $w.size.b$l]}
	    if {[string length $r]} {bind $b <Right> [list focus $w.size.b$r]}
	    bind $b <Tab>       "[list focus $w.size.entry ];break"
	    bind $b <Shift-Tab> "[list focus $w.style.sBold];break"
	    set accel ['get_accel $b]
	    if {[string length $accel]} {
		bind $w <$accel> "focus $b; $b invoke"
	    }
	    bind $b <Return> "$b invoke; break"
	}
	entry $w.size.entry -textvariable [namespace current]::Size
	grid $w.size.entry -row 0 -column 3 -rowspan 2 -sticky ew
	grid columnconfigure $w.size 3 -weight 1
	bind $w.size.entry <Return> \
		[namespace code {'set_font;break}]


	# Sample text.  Note that this is editable
	frame $w.sample
	grid $w.sample -row 8 -column 1 -columnspan 7 -sticky nsew
	grid propagate $w.sample 0
	::tk::entry $w.sample.text
	catch {
	    $w.sample.text configure -background [$w.sample cget -background]
	}
	$w.sample.text insert 0 [option get $w.sample.text text Text]
	grid $w.sample.text


	# OK, Cancel and (partially) Apply.  See also 'configure_apply
	frame $w.butnframe
	grid $w.butnframe -row 0 -column 7 -rowspan 4 -columnspan 2 \
		-sticky nsew -pady $gap
	foreach {but code} {
	    ok  0
	    can 1
	} {
	    button $w.butnframe.$but -command \
		    [namespace code [list set Done $code]]
	    pack $w.butnframe.$but -side top -fill x -padx [expr {$gap/2}] \
		    -pady [expr {$gap/2}]
	}
	button $w.butnframe.apl
	bind $w.butnframe.ok <Down> [list focus $w.butnframe.can]
	bind $w.butnframe.can <Up> [list focus $w.butnframe.ok]
    }


    # Convenience proc to get the accelerator for a particular window
    # if the user has given one.  Makes it simpler to get this right
    # everywhere it is needed...
    proc 'get_accel {w} {
	option get $w accelerator Accelerator
    }


    # Called when changing the family.  Sets the family to either be
    # the first family whose name starts with the given character (if
    # char is non-empty and not special) or to be the name of the
    # family at the given index of the listbox.
    proc 'change_family {w index {char {}}} {
	variable Family
	if {[string length $char] && ![regexp {[]*?\\[]} $char]} {
	    set idx [lsearch -glob ['list_families] $char*]
	    if {$idx >= 0} {
		set index $idx
		$w activate $idx
		$w selection clear 0 end
		$w selection anchor $idx
		$w selection set $idx
		$w see $idx
	    }
	}
	set Family [$w get $index]
	##DEBUG
	#wm title [winfo toplevel $w] $Family
	'set_font
    }


    # The apply button runs this script when pressed.
    proc 'do_apply {w script} {
	'set_font
	set font [$w.sample.text cget -font]
	uplevel #0 $script [list $font]
    }


    # Based on whether the supplied script is empty or not, install an
    # apply button into the dialog.  This is not part of 'make_UI
    # since it happens at a different stage of initialisation.
    proc 'configure_apply {w script} {
	set b $w.butnframe.apl
	set binding [list $b invoke]
	if {[string length $script]} {
	    # There is a script, so map the button
	    array set packinfo [pack info $w.butnframe.ok]
	    $b configure -command [namespace code [list 'do_apply $w $script]]
	    pack $b -side top -fill x -padx $packinfo(-padx) \
		    -pady $packinfo(-pady)

	    bind $w.butnframe.can <Down> [list focus $w.butnframe.apl]
	    bind $w.butnframe.apl <Up>   [list focus $w.butnframe.can]

	    # Set up accelerator.  Tricky since we want to force a
	    # systematic match with the underline
	    set uline [$b cget -underline]
	    if {$uline>=0} {
		set uchar [string index [$b cget -text] $uline]
		set uchar [string tolower $uchar]
		bind $w <Meta-$uchar> $binding
	    }

	} else {
	    # No script => no button
	    set manager [winfo manager $b]
	    if {[string length $manager]} {
		$manager forget $b

		# Now we must remove the accelerator!  This is tricky
		# since we don't actually know what it is officially
		# bound to...
		foreach bindseq [bind $w] {
		    if {![string compare [bind $w $bindseq] $binding]} {
			bind $w $bindseq {}
			break
		    }
		}

	    }
	}
    }


    # Set the font on the editor window based on the information in
    # the namespace variables.  Returns a 1 if the operation was a
    # failure and 0 if it iwas a success.
    proc 'set_font {} {
	variable Family
	variable Style
	variable Size
	variable Win

	set styles {}
	foreach style {italic bold underline overstrike} {
	    if {$Style($style)} {
		lappend styles $style
	    }
	}
	if {[catch {
	    expr {$Size+0}
	    if {[llength $styles]} {
		$Win configure -font [list $Family $Size $styles]
	    } else {
		$Win configure -font [list $Family $Size]
	    }
	} s]} {
	    bgerror $s
	    return 1
	}
	return 0
    }


    # Get a sorted lower-case list of all the font families defined on
    # the system.  A canonicalisation of [font families]
    proc 'list_families {} {
	lsort [string tolower [font families]]
    }

    # ----------------------------------------------------------------------

    proc dkf_chooseFont {args} {
	variable Family
	variable Style
	variable Size
	variable Done
	variable Win

	array set options {
	    -parent {}
	    -title {Select a font}
	    -initialfont {}
	    -apply {}
	}
	'parse_opts $args [array names options] options
	switch -exact -- $options(-parent) {
	    . - {} {
		set parent .
		set w .__dkf_chooseFont
	    }
	    default {
		set parent $options(-parent)
		set w $options(-parent).__dkf_chooseFont
	    }
	}
	catch {destroy $w}

	toplevel $w -class DKFChooseFont
	wm title $w $options(-title)
	wm transient $w $parent
	wm iconname $w ChooseFont
	wm group $w $parent
	wm protocol $w WM_DELETE_WINDOW {#}

	if {![string length $options(-initialfont)]} {
	    set options(-initialfont) [option get $w initialFont InitialFont]
	}

	set Win $w.sample.text
	'make_UI $w
	bind $w <Return>  [namespace code {set Done 0}]
	bind $w <Escape>  [namespace code {set Done 1}]
	bind $w <Destroy> [namespace code {set Done 1}]
	focus $w.butnframe.ok

	'configure_apply $w $options(-apply)

	foreach style {italic bold underline overstrike} {
	    set Style($style) 0
	}
	foreach {family size styles} $options(-initialfont) {break}
	set Family $family
	set familyIndex [lsearch -exact ['list_families] \
		[string tolower $family]]
	if {$familyIndex<0} {
	    wm withdraw $w
	    tk_messageBox -type ok -icon warning -title "Bad Font Family" \
		    -message "Font family \"$family\" is unknown.  Guessing..."
	    set family [font actual $options(-initialfont) -family]
	    set familyIndex [lsearch -exact ['list_families] \
		    [string tolower $family]]
	    if {$familyIndex<0} {
		return -code error "unknown font family fallback \"$family\""
	    }
	    wm deiconify $w
	}
	$w.family selection set $familyIndex
	$w.family see $familyIndex
	set Size $size
	foreach style $styles {set Style($style) 1}

	'set_font

	wm withdraw $w
	update idletasks
	if {$options(-parent)==""} {
	    set x [expr {([winfo screenwidth $w]-[winfo reqwidth $w])/2}]
	    set y [expr {([winfo screenheigh $w]-[winfo reqheigh $w])/2}]
	} else {
	    set pw $options(-parent)
	    set x [expr {[winfo x $pw]+
			 ([winfo width $pw]-[winfo reqwidth $w])/2}]
	    set y [expr {[winfo y $pw]+
			 ([winfo heigh $pw]-[winfo reqheigh $w])/2}]
	}
	wm geometry $w +$x+$y
	update idletasks
	wm deiconify $w
	tkwait visibility $w
	vwait [namespace current]::Done
	if {$Done} {
	    destroy $w
	    return ""
	}
	if {['set_font]} {
	    destroy $w
	    return ""
	}
	set font [$Win cget -font]
	destroy $w
	return $font
    }

    # ----------------------------------------------------------------------
    # I normally load these from a file, but I inline them here for portability
    foreach {pattern value} {
	*DKFChooseFont.DKFChooseFontFrame.borderWidth	2
	*DKFChooseFont.DKFChooseFontFrame.relief	ridge


	*DKFChooseFont.lblFamily.text	       Family
	*DKFChooseFont.lblFamily.underline     0
	*DKFChooseFont.lblFamily.accelerator   Control-f

	*DKFChooseFont.lblStyle.text	       Style
	*DKFChooseFont.lblStyle.underline      2
	*DKFChooseFont.lblStyle.accelerator    Control-y

	*DKFChooseFont.lblSize.text	       Size
	*DKFChooseFont.lblSize.underline       2
	*DKFChooseFont.lblSize.accelerator     Control-z

	*DKFChooseFont.lblSample.text	       Sample


	*DKFChooseFont.style.Checkbutton.anchor		w

	*DKFChooseFont.style.sBold.text	       Bold
	*DKFChooseFont.style.sBold.underline   0
	*DKFChooseFont.style.sBold.accelerator Control-b

	*DKFChooseFont.style.sItalic.text      Italic
	*DKFChooseFont.style.sItalic.underline   0
	*DKFChooseFont.style.sItalic.accelerator Control-i

	*DKFChooseFont.style.sUnderline.text   Underline
	*DKFChooseFont.style.sUnderline.underline   0
	*DKFChooseFont.style.sUnderline.accelerator Control-u

	*DKFChooseFont.style.sStrikeout.text   Overstrike
	*DKFChooseFont.style.sStrikeout.underline   0
	*DKFChooseFont.style.sStrikeout.accelerator Control-o


	*DKFChooseFont.Label.padX	       1m
	*DKFChooseFont.Label.padY	       1

	*DKFChooseFont.family.height	       1
	*DKFChooseFont.family.width	       24
	*DKFChooseFont.familyX.orient	       horizontal
	*DKFChooseFont.Scrollbar.takeFocus     0

	*DKFChooseFont.size.b8.text	       8
	*DKFChooseFont.size.b10.text	       10
	*DKFChooseFont.size.b12.text	       12
	*DKFChooseFont.size.b14.text	       14
	*DKFChooseFont.size.b18.text	       18
	*DKFChooseFont.size.b24.text	       24
	*DKFChooseFont.size.Radiobutton.anchor w
	*DKFChooseFont.size.Entry.background   white

	*DKFChooseFont.sample.text.text	       ABCabcXYZxyz123
	*DKFChooseFont.sample.text.takeFocus   0
	*DKFChooseFont.sample.text.highlightThickness 0
	*DKFChooseFont.sample.text.borderWidth 0
	*DKFChooseFont.sample.text.relief      flat
	*DKFChooseFont.sample.text.width       0
	*DKFChooseFont.sample.text.cursor      {}
	*DKFChooseFont.sample.height	       40
	*DKFChooseFont.sample.width	       40

	*DKFChooseFont.butnframe.ok.default    active
	*DKFChooseFont.butnframe.ok.text       OK
	*DKFChooseFont.butnframe.can.default   normal
	*DKFChooseFont.butnframe.can.text      Cancel
	*DKFChooseFont.butnframe.apl.default   normal
	*DKFChooseFont.butnframe.apl.text      Apply
	*DKFChooseFont.butnframe.apl.underline 0
    } {
	option add $pattern $value startupFile
    }
    switch $tcl_platform(platform) {
	windows {
	    option add *DKFChooseFont.initialFont {Arial 12 bold} startupFile
	}
	default {
	    foreach {pattern value} {
		*DKFChooseFont*Button.BorderWidth      1
		*DKFChooseFont*Checkbutton.BorderWidth 1
		*DKFChooseFont*Entry.BorderWidth       1
		*DKFChooseFont*Label.BorderWidth       1
		*DKFChooseFont*Listbox.BorderWidth     1
		*DKFChooseFont*Menu.BorderWidth	       1
		*DKFChooseFont*Menubutton.BorderWidth  1
		*DKFChooseFont*Message.BorderWidth     1
		*DKFChooseFont*Radiobutton.BorderWidth 1
		*DKFChooseFont*Scale.BorderWidth       1
		*DKFChooseFont*Scrollbar.BorderWidth   1
		*DKFChooseFont*Text.BorderWidth	       1
		*DKFChooseFont.Scrollbar.width         10
		*DKFChooseFont.initialFont             {Helvetica 12 bold}
	    } {
		option add $pattern $value startupFile
	    }
	}
    }

}
namespace import -force ::dkfFontSel::dkf_chooseFont

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

    if {$jid == {}} {
	set jid [::tkjabber::jid !resource $::tkjabber::myId]
    }

    set uivar [namespace current]::ui_$jid
    variable $uivar
    upvar #0 $uivar UI
    if {![info exists UI]} {
	set UI(after) [after 5000 [list array set $uivar {timeout 1}]]
	#addSystem .txt "Requesting user info for $jid..."
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
	    addSystem .txt "Still waiting for a vcard from the server..."
	    # Reentry during timeout period.
	    return
	}
    }
    if { [info exists UI(timeout)] && [::tkjabber::jid !resource $jid] \
	     ne [::tkjabber::jid !resource $::tkjabber::myId] } {
	# Not available, and not the users own vcard.
	::log::log debug "cleanup as no UI"
	unset $uivar
	addSystem .txt "No info available for $jid"
	return
    }

    if {![info exists UserInfoWin]} {set UserInfoWin 0}

    set id userinfo[incr UserInfoWin]
    set UI(id) $id

    set [namespace current]::$id -1
    set dlg [toplevel .$id -class Dialog]
    wm title $dlg "User info for $jid"
    set f [frame $dlg.f -bd 0]

    # country Country city City age Age
    # photo_url "Picture URL" icq_uin "ICQ uin"
    foreach {key text} {FN "Real name" EMAIL_USERID Email URL "Homepage URL" \
			    ADR_LOCALITY "City"  ADR_CTRY "Country" \
			    PHOTO_EXTVAL "Photo URL" BDAY "Birthday"} {
	set l [label $f.l$key -text $text -anchor nw]
	set e [entry $f.e$key \
		   -textvariable [set uivar]($key) \
		   -bd 1 -background white]
	grid configure $l $e -sticky news -padx 1 -pady 1
    }
    set l [label $f.lstuff -text "Anything else" -anchor nw]
    set e [frame $f.estuff -bd 0]
    set et [text $e.text -height 6 -bd 1 -background white]
    set es [scrollbar $e.scroll -bd 1 -command [list $et yview]]
    $et configure -yscrollcommand [list $es set]
    catch {$et insert 0.0 $UI(DESC)}
    grid configure $et $es -sticky news
    grid rowconfigure $e 0 -weight 1
    grid columnconfigure $e 0 -weight 1

    grid configure $l $e -sticky news -padx 1 -pady 1
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 8 -weight 1

    set btns [frame $dlg.buttons -bd 1]
    button $btns.ok -text Save -width 10 -state disabled \
	-command [list set [namespace current]::$id 1]
    button $btns.cancel -text Close -width 10 \
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
    if {$MessageCounter > 0} {
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
		ResetMessageCounter
	    } else {
		set WinicoWmState [wm state .]
		wm withdraw .
	    }
	}
    }
}

# -------------------------------------------------------------------------

proc ::tkchat::BookmarkInit {} {
    # FIX ME: need to make a better image :)
    image create photo ::tkchat::img::bookmark -format GIF \
	-data {R0lGODlhEAAQAMIAANnZ2QAAAAD//////wlnuglnuglnuglnuiH5BAEAAAMA
	    LAAAAAAQABAAAAMpOLrc/jDIKV8QOOPQrv6c4n2gSJLheG7mmqXu28bhoKLd
	    WjMUBf3AXwIAOw==}

    menu .mbar.mm -tearoff 0
    .mbar.mm add command -label "Set Bookmark" -accelerator Ctrl-F2 \
	-command ::tkchat::BookmarkAdd
    .mbar.mm add command -label "Prev Bookmark" -accelerator Shift-F2 \
	-command ::tkchat::BookmarkNext
    .mbar.mm add command -label "Next Bookmark" -accelerator F2 \
	-command ::tkchat::BookmarkPrev
    .mbar.mm add command -label "Clear Bookmarks" \
	-command ::tkchat::BookmarkClear
    .mbar.mm add command -label "Google Selection" -accelerator Ctrl-G \
	-command ::tkchat::GoogleSelection


    .mbar.mm add cascade -label "Translate" -command ::tkchat::babelfishMenu

    .mbar.mm add command -label "Cancel"

    bind .txt <Button-1> {focus %W ; %W mark set insert @%x,%y}
    bind .txt <Button-3> {
	%W mark set AddBookmark "@%x,%y linestart"
	.mbar.mm post %X %Y
	%W mark unset AddBookmark
    }
    bind . <F2> ::tkchat::BookmarkNext
    bind . <Shift-F2> ::tkchat::BookmarkPrev
    bind . <Control-F2> ::tkchat::BookmarkAdd
    bind . <Control-G> ::tkchat::GoogleSelection
    bind . <Control-g> ::tkchat::GoogleSelection

}

proc ::tkchat::BookmarkAdd {} {
    variable bookmark
    if {![info exists bookmark(id)]} {set bookmark(id) 0}
    if {[catch {.txt index AddBookmark}]} {
	set x [expr {[winfo pointerx .txt] - [winfo rootx .txt]}]
	set y [expr {[winfo pointery .txt] - [winfo rooty .txt]}]
	.txt mark set AddBookmark "@$x,$y linestart"
    }
    .txt configure -state normal
    .txt image create AddBookmark -image ::tkchat::img::bookmark
    .txt mark set bookmark[incr bookmark(id)] AddBookmark
    .txt mark unset AddBookmark
    .txt configure -state disabled
}

proc ::tkchat::BookmarkNext {} {
    variable bookmark
    if {![info exists bookmark(last)]} {set bookmark(last) 0.0}
    if { $bookmark(last) eq "end" || [catch { .txt index $bookmark(last) }] } {
	set bookmark(last) 0.0
    }
    while {$bookmark(last) != {}} {
	set bookmark(last) [.txt mark next $bookmark(last)]
	if {[string match "bookmark*" $bookmark(last)]} {
	    break
	}
    }
    if {$bookmark(last) == {}} {
	set bookmark(last) end
    }
    .txt see $bookmark(last)
    return $bookmark(last)
}

proc ::tkchat::BookmarkPrev {} {
    variable bookmark
    if {![info exists bookmark(last)]} {set bookmark(last) end}
    if {$bookmark(last) == "0.0" || [catch {.txt index $bookmark(last)}]} {
	set bookmark(last) end
    }
    while {$bookmark(last) != {}} {
	set bookmark(last) [.txt mark previous $bookmark(last)]
	if {[string match "bookmark*" $bookmark(last)]} {
	    break
	}
    }
    if {$bookmark(last) == {}} {
	set bookmark(last) 0.0
    }
    .txt see $bookmark(last)
    return $bookmark(last)
}

proc ::tkchat::BookmarkClear {} {
    set mark 0.0
    while {[set mark [.txt mark next $mark]] != {}} {
	if {[string match "bookmark*" $mark]} {
	    set remove $mark
	    set mark "[.txt index $mark] + 1 char"
	    BookmarkRemove $remove
	}
    }
}

proc ::tkchat::BookmarkRemove {mark} {
    if {[lsearch [.txt mark names] $mark] != -1} {
	.txt configure -state normal
	.txt delete "$mark - 1 char"
	.txt mark unset $mark
	.txt configure -state disabled
    }
}

proc ::tkchat::GoogleSelection {} {
    set sel [.txt tag ranges sel]
    set t [.txt get [lindex $sel 0] [lindex $sel 1]]
    gotoURL http://www.google.com/search?ie=UTF-8&oe=UTF-8&[::http::formatQuery q $t]
}

# -------------------------------------------------------------------------

# NoisyUsers

proc ::tkchat::noisyUser { msg } {
    variable noisyUsers

    #Assign msg elements to cmd, nick and time:
    foreach {cmd nick time} [lrange [split $msg " "] 0 2] {}

    if { $nick eq "" } {
	set cnt 0
	foreach {nick time} [array get noisyUsers] {
	    incr cnt
	    if { $time < [clock seconds] } {
		addSystem .txt "$nick is no longer noisy (timeout expired)"
		unset noisyUsers($nick)
	    } else {
		addSystem .txt "$nick is noisy until [clock format $time -format %H:%M:%S]"
	    }
	}
	if { $cnt == 0 } {
	    addSystem .txt "You don't consider anyone noisy right now"
	}
	return
    }

    if { [info exists noisyUsers($nick)] } {
	if { [string is integer -strict $time] } {
	    switch $time {
		-1 -
		0 {
		    unset noisyUsers($nick)
		    addSystem .txt "$nick is no longer considered noisy."
		}
		default {
		    set noisyUsers($nick) [expr {[clock seconds] + 60*$time}]
		    if { $time > 1 } {
			addSystem .txt "$nick is considered noisy for $time minutes."
		    } else {
			addSystem .txt "$nick is considered noisy for $time minute."
		    }
		}
	    }
	} else {
	    #No time given, remove from noisyUsers
	    unset noisyUsers($nick)
	    addSystem .txt "$nick is no longer considered noisy."
	}
    } else {
	if { ![string is integer -strict $time] } {
	    set time 5
	}
	switch $time {
	    -1 -
	    0 {
		addSystem .txt "$nick not considered noisy at this time."
	    }
	    default {
		set noisyUsers($nick) [expr {[clock seconds] + 60*$time}]
		if { $time > 1 } {
		    addSystem .txt "$nick is considered noisy for $time minutes."
		} else {
		    addSystem .txt "$nick is considered noisy for $time minute."
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
	    addSystem .txt "$nick is no longer considered noisy (timeout expired)."
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
	# Work around a but when transitioning from opaque to
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
	catch {unset FadeId}
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
    variable EditOptions
    array set EditOptions {Result -1}
    if {[info exists Options(BROWSER)]} {
	set EditOptions(BROWSER) $Options(BROWSER)
    } else {
	set EditOptions(BROWSER) {}
    }

    set EditOptions(Style)         $Options(Style)
    set EditOptions(AutoFade)      $Options(AutoFade)
    set EditOptions(AutoFadeLimit) $Options(AutoFadeLimit)
    set EditOptions(Transparency)  $Options(Transparency)

    if {[winfo exists .options]} {destroy .options}
    set dlg [toplevel .options -class dialog]
    wm withdraw $dlg
    wm title $dlg "Tkchat Options"

    set bf [labelframe $dlg.bf -text "Preferred browser" -padx 1 -pady 1]

    message $bf.m -justify left -width 320 \
	-text "Provide the command used to launch your web browser. For\
	instance /opt/bin/mozilla or xterm -e links. The URL to\
	be opened will be appended to the command string and for\
	mozilla-type browsers we will call the -remote option to\
	try to use a previously existing browser."
    entry $bf.e -textvariable ::tkchat::EditOptions(BROWSER)
    button $bf.b -text "..."  -command {
	if {[set file [tk_getOpenFile]] != {}} {
	    set ::tkchat::EditOptions(BROWSER) $file
	}
    }

    grid $bf.m - -sticky news
    grid $bf.e $bf.b -sticky news
    grid rowconfigure $bf 0 -weight 1
    grid columnconfigure $bf 0 -weight 1

    set sf [labelframe $dlg.sf -text "Tk style" -padx 1 -pady 1]
    set gf [labelframe $dlg.gf -text "Gimmiks"  -padx 1 -pady 1]

    message $sf.m -justify left -width 320 \
	-text "The Tk style selection available here will apply when you \
	   next restart tkchat."
    radiobutton $sf.as -text "ActiveState" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value as_style
    radiobutton $sf.gtk -text "GTK look" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value gtklook
    radiobutton $sf.any -text "Any" -underline 1 \
	-variable ::tkchat::EditOptions(Style) -value any
    radiobutton $sf.def -text "Tk default" -underline 0 \
	-variable ::tkchat::EditOptions(Style) -value tk

    if {[catch {package require style::as}]
	&& [catch {package require as::style}]} {
	$sf.as configure -state disabled
    }

    bind $dlg <Alt-a> [list $sf.as invoke]
    bind $dlg <Alt-g> [list $sf.gtk invoke]
    bind $dlg <Alt-n> [list $sf.any invoke]
    bind $dlg <Alt-t> [list $sf.def invoke]

    grid $sf.m - - - -sticky news
    grid $sf.as $sf.gtk $sf.any $sf.def -sticky news
    grid rowconfigure $bf 0 -weight 1
    grid columnconfigure $bf 0 -weight 1

    # Gimmicks section.
    set gimmicks 0
    if {[lsearch [wm attributes .] -alpha] != -1} {
	set gimmicks 1
	checkbutton $gf.fade -text "When not active, fade to " -underline 2 \
	    -variable ::tkchat::EditOptions(AutoFade)
	spinbox $gf.fadelimit -from 1 -to 100 -width 4 \
	    -textvariable ::tkchat::EditOptions(AutoFadeLimit)
	label $gf.pct -text "%"
	label $gf.alabel -text Transparency
	scale $gf.alpha -from 1 -to 100 -orient horizontal
	$gf.alpha set $EditOptions(Transparency)
	#[expr {int([wm attributes . -alpha] * 100)}]
	$gf.alpha configure -command [namespace origin SetAlpha]

	bind $dlg <Alt-h> [list $gf.face invoke]
	bind $dlg <Alt-r> [list focus $gf.alpha]

	grid $gf.fade   - $gf.fadelimit $gf.pct x -sticky w
	grid $gf.alabel $gf.alpha - - - -sticky we
	grid configure $gf.alabel -pady {20 0} -sticky w
	grid columnconfigure $gf 4 -weight 1
    }

    button $dlg.ok -text OK -underline 0 -default active \
	-command [list set ::tkchat::EditOptions(Result) 1]
    button $dlg.cancel -text Cancel -underline 0 \
	-command [list set ::tkchat::EditOptions(Result) 0]

    grid $bf - -sticky news -padx 2 -pady 2
    grid $sf - -sticky news -padx 2 -pady 2
    if {$gimmicks} {
	grid $gf - -sticky news -padx 2 -pady 2
    }
    grid $dlg.ok $dlg.cancel -sticky e
    grid rowconfigure $dlg 0 -weight 1
    grid columnconfigure $dlg 0 -weight 1

    bind $dlg <Return> [list $dlg.ok invoke]
    bind $dlg <Escape> [list $dlg.cancel invoke]
    bind $dlg <Alt-o>  [list focus $dlg.ok]
    bind $dlg <Alt-c>  [list focus $dlg.cancel]
    focus $bf.e

    wm resizable $dlg 0 0
    catch {::tk::PlaceWindow $dlg widget .}
    wm deiconify $dlg

    tkwait variable ::tkchat::EditOptions(Result)

    if {$EditOptions(Result) == 1} {
	set Options(BROWSER) $EditOptions(BROWSER)
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

    font create GtkLookFont \
	-family Helvetica -size 12 -weight normal
    font create GtkLookDialogFont \
	-family Helvetica -size 16 -weight bold -slant italic

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

    if { ![winfo exists .wb] } {
	set wb [toplevel .wb]

	entry $wb.e -textvar ::wbentry -bg white -width 80
	bind $wb.e <Return> {interp eval .wbinterp $::wbentry}
	set white_board [canvas $wb.c -bg white -width 350 -height 300]
	button $wb.bclear -text "clear" -command ::tkchat::whiteboard_clear
	bind $wb.c <1> {set entry ""; set id [%W create line %x %y %x %y]}
	bind $wb.c <B1-Motion> {%W coords $id [concat [%W coords $id] %x %y]}
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
    Variable jhttp ""
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
    Variable AwayShow ""

    # Provides a map of nicks to full jids (works because the chat is
    # not anonymous. Used for the /memo function.
    variable members; if {![info exists members]} {array set members {}}

    # To provide a map between parents widgets and chats
    variable ChatWindows; if {![info exists ChatWindows]} {array set ChatWindows {counter 0}}
}

# Login:
proc tkjabber::connect {} {
    variable jhttp
    variable jabber
    variable roster
    variable browser
    variable socket
    variable conn
    variable reconnect
    variable conference
    variable reconnectTimer
    variable have_tls

    global Options

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
	    set roster [roster::roster [namespace current]::RosterCB]
	}
	set jabber [jlib::new $roster [namespace current]::ClientCB  \
			-iqcommand          [namespace current]::IqCB  \
			-messagecommand     [namespace current]::MsgCB \
			-presencecommand    [namespace current]::PresCB \
			-keepalivesecs	    $keepalive_seconds]

	set browser [browse::new $jabber -command [namespace current]::BrowseCB]

	# override the jabberlib version info query
	jlib::iq_register $jabber get jabber:iq:version \
	    [namespace origin on_iq_version] 40
    }

    if { $Options(UseJabberPoll) } {
	set socket [jlib::http::new $jabber "http://scheffers.net:5280/http-poll" \
		    -usekeys 1 \
		    -command [namespace current]::httpCB]
	openStream

    } else {
	set have_tls [expr {[package provide tls] != {}}]
	if { [catch {
	    if {$Options(UseProxy) && [string length $Options(ProxyHost)] > 0} {
		set socket [ProxyConnect $Options(ProxyHost) $Options(ProxyPort) \
				$Options(JabberServer) $Options(JabberPort)]
	    } elseif {$have_tls && $Options(UseJabberSSL) eq "ssl"} {
		set socket [tls::socket $Options(JabberServer) $Options(JabberPort)]
	    } else {
		if {$Options(JabberPort) == 5223} {incr Options(JabberPort) -1}
		if {[info exists Options(JabberConnect)]
		    && $Options(JabberConnect) ne ""} {
		    foreach {srv prt} [split $Options(JabberConnect) :] break
		    if {$prt eq ""} {set prt $Options(JabberPort)}
		    set socket [socket $srv $prt]
		} else {
		    set socket [socket $Options(JabberServer) $Options(JabberPort)]
		}
	    }
	} res] } {
	    # Connection failed.
	    tkchat::addSystem .txt "Connecting failed: $res" end ERROR
	    if { $reconnect } {
		scheduleReconnect
	    }
	} else {
	    #fconfigure $socket -encoding utf-8
	    $jabber setsockettransport $socket
	    openStream
	}
    }

    # The next thing which will/should happen is the a call to ConnectProc by
    # jabberlib.
    if {[winfo exists .mbar.file]} {
	.mbar.file entryconfigure 0 -label [msgcat::mc Logout]
    }
}

proc tkjabber::disconnect {} {

    variable jhttp
    variable jabber
    variable roster
    variable browser
    variable socket
    variable conn
    variable reconnect
    variable reconnectTimer

    global Options

    set reconnect 0
    if { $reconnectTimer ne "" } {
	after cancel $reconnectTimer
	set reconnectTimer ""
    }

    if { $socket eq "" } {
	return
    }

    cleanup
    tkchat::addSystem .txt "Disconnected from jabber server."
}

proc tkjabber::cleanup {} {
    variable jabber
    variable muc
    variable conference
    variable socket

    if {[info exists muc]} {
	if {[catch {$muc exit $conference} err]} {
	    ::log::log error "cleanup: $err"
	}
    }

    if { [catch {$jabber closestream}] } {
	::log::log error "Closestream: $::errorInfo"
    }

    #catch {close $socket}
    catch {jlib::resetsocket $jabber}
    set socket ""
    if {[winfo exists .mbar.file]} {
	.mbar.file entryconfigure 0 -label [msgcat::mc Login]
    }
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
    tkchat::addSystem .txt "Connected to $conn(from), sending credentials."
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
proc tkjabber::RosterCB {rostName what {jid {}} args} {
    ::log::log debug "--roster-> what=$what, jid=$jid, args='$args'"
    variable conference
    variable members
    variable grabNick
    variable ignoreNextNick

    switch -- $what {
	presence {
	    array set p $args
	    set action ""
	    set newnick ""
	    # online/away/offline, etc.
	    set status [list online]
	    if { [info exists p(-show)] } { set status [list $p(-show)] }
	    if { [info exists p(-status)] } { lappend status $p(-status) }
	    switch -- $p(-type) {
		available {
		    set action entered

		    # Add the user's nick into a nick/jid map
		    if {[info exists p(-x)]} {
			foreach child $p(-x) {
			    set ns [wrapper::getattribute $child xmlns]
			    if { "http://jabber.org/protocol/muc#user" \
				    eq $ns } {
				set item [wrapper::getchildswithtag $child item]
				if {[llength $item] > 0} {
				    if {[info exists members($p(-resource),jid)]} {
					set action changedstatus
				    }
				    set usrjid [wrapper::getattribute \
						    [lindex $item 0] jid]
				    set members($p(-resource),jid) $usrjid
				    set members($p(-resource),status) $status
				}
				break
			    }
			}
		    }
		}
		unavailable {
		    set nickchange 0
		    set action left
		    set status offline

		    if {[info exists p(-x)]} {
			# Check for nickname change
			foreach child $p(-x) {
			    set ns [wrapper::getattribute $child xmlns]
			    if { "http://jabber.org/protocol/muc#user" \
				    eq $ns } {
				set status_elem [wrapper::getchildswithtag $child status]
				if { [llength $status_elem]==0 } {
				    # Not a nickname change.
				    continue
				}
				set status_code [wrapper::getattribute [lindex $status_elem 0] code]
				if { $status_code eq "303" } {
				    # nickname change!
				    set item [wrapper::getchildswithtag $child item]
				    if {[llength $item] > 0} {
					set nickchange 1
					set action nickchange
					set newnick [wrapper::getattribute \
							[lindex $item 0] nick]
					break
				    }
				}
			    }
			}
		    }
		    if {[info exists members($p(-resource),jid)]} {
			unset members($p(-resource),jid)
		    }
		    if {[info exists members($p(-resource),status)]} {
			unset members($p(-resource),status)
		    }
		    # Do we want to be this nick?
		    if { $grabNick ne "" && $p(-resource) eq $grabNick } {
			after idle [list tkjabber::setNick $grabNick]
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
		set m "$jid has $action ($tstatus)"
		if {[llength $status] > 1} {append m ": [lindex $status 1]"}
		tkchat::addSystem .txt $m end AVAILABILITY
		return
	    }

	    # Much more interesting info available in array ...

	    if { $action eq "nickchange" } {
		::tkchat::addTraffic .txt $p(-resource) $action end 0 $newnick
		set ignoreNextNick $newnick
	    } elseif {$action eq "changedstatus"} {
		set s [string map {xa idle chat talking dnd busy} [lindex $status 0]]
		if {[lindex $status 1] ne ""} {append s " ([lindex $status 1])"}
		tkchat::addSystem .txt "$p(-resource) is $s" end AVAILABILITY
	    } else {
		if { !($action eq "entered" && $ignoreNextNick eq $p(-resource)) } {
		    # if not the ignore nick:
		    tkchat::addTraffic .txt $p(-resource) $action
		}
		# Always reset ignoreNextNick!
		set ignoreNextNick ""
	    }
	    tkchat::updateJabberNames
	}
	default {
	    tkchat::addSystem .txt "--roster-> what=$what, jid=$jid, args='$args'"
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
	    tkchat::addSystem .txt "Connection to Jabber Server Established"
	}
	disconnect {
	    cleanup
	    scheduleReconnect
	}
	networkerror {
	    array set x {-body ""}
	    array set x $args
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

proc tkjabber::MsgCB {jlibName type args} {
    variable conference
    variable muc
    variable topic
    variable LastMessage

    set LastMessage [clock seconds]

    ::log::log debug "|| MsgCB > type=$type, args=$args"

    set color ""
    set ts 0

    array set m $args
    if { [info exists m(-x)] } {
	foreach x $m(-x) {
	    switch [wrapper::getattribute $x xmlns] {
		"jabber:x:delay" {
		    set ts [clock scan [wrapper::getattribute $x stamp] -gmt 1]
		    if { $ts eq "" } {
			set ts 0
		    }
		}
		"urn:tkchat:chat" {
		    array set tkchatAttr [wrapper::getattrlist $x]
		    set color [wrapper::getattribute $x color]
		}
		"urn:tkchat:whiteboard" {
		    tkchat::whiteboard_eval [wrapper::getcdata $x] [wrapper::getattribute $x color]
		    return
		}
		"urn:tkchat:changenick" {
		    # Request for nick handover.
		    tkchat::addSystem .txt "$m(-from) has requested your nickname."
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
	    set name $from
	    if { [regexp {([^/]+)/(.+)} $m(-from) -> conf name] } {
		if { $conf eq $conference } {
		    set w [getChatWidget $m(-from) $name]
		} else {
		    regexp {^([^@]+)@} $m(-from) -> name
		    set w [getChatWidget $m(-from) $name]
		}
	    } else {
		regexp {^([^@]+)@} $m(-from) -> name
		set w [getChatWidget $m(-from) $name]
	    }
	    if {$w eq ".txt"} {
		tkchat::addAction $w $color $name " whispers: $m(-body)" end $ts
	    } else {
		if { [string match -nocase "/me *" $m(-body)] } {
		    tkchat::addAction $w $color $name [string range $m(-body) 4 end] end $ts
		} else {
		    tkchat::addMessage $w $color $name $m(-body) end $ts
		}
	    }
	}
	groupchat {
	    set from $m(-from)
	    regexp {([^/]+)/(.+)} $m(-from) -> conf from
	    set msg ""
	    if { [info exists m(-subject)] } {
		# changing topic.
		set topic $m(-subject)
		set ::tkchat::chatWindowTitle "The Tcler's Chat - $topic"
		wm title . $::tkchat::chatWindowTitle
		if { [info exists m(-body)] } {
		    if { $from eq $conference } {
			tkchat::addSystem .txt $m(-body)
		    } else {
			tkchat::addAction .txt $color $from " changed the topic to: $m(-subject)\n ... $m(-body)" end $ts
		    }
		} else {
		    tkchat::addAction .txt $color $from " changed the topic to: $m(-subject)" end $ts
		}
	    } else {
		if { [info exists m(-body)] > 0 } {
		    ::tkjabber::parseMsg $from $m(-body) $color end $ts
		} else {
		    ::log::log notice "Unknown message from $from: '$args'"
		}
	    }
	}
	normal {
	    set conf ""
	    set from $m(-from)
	    regexp {([^/]+)/(.+)} $m(-from) -> conf from
	    if { $conf ne $conference } {
		set from $m(-from)
	    }
	    set msg ""
	    if { [info exists m(-subject)] } {
		lappend msg "Subject: $m(-subject)"
	    }
	    if { [info exists m(-body)] } {
		lappend msg "$m(-body)"
	    }
	    if { [llength $msg] > 0 } {
		tkchat::addAction .txt "" $from " whispers: [join $msg \n]"
	    } else {
		::log::log notice "Unknown message from $from: '$args'"
	    }
	}
	error {
	    if { [info exists m(-error)] } {
		switch -- [lindex $m(-error) 0] {
		    405 {
			if { [catch {
			    $muc exit $conference
			}] } {
			    ::log::log debug "MUC EXIT: $::errorInfo"
			}
			tkchat::addSystem .txt "$m(-from): [lindex $m(-error) 1]. Trying to get in again..."
			$muc enter $::tkjabber::conference $::Options(Nickname) -command [namespace current]::MucEnterCB
		    }
		    default {
			tkchat::addSystem .txt  "MsgCB (error) args='$args'"
		    }
		}
	    }
	}
	#get {
	    if { [info exists m(-query)] } {
		::log::log debug "Jabber query\n$args"
		array set iq $m(-query)
	    } else {
		tkchat::addSystem .txt "|| MsgCB > type=$type, args=$args"
	    }
	}
	default {
	    tkchat::addSystem .txt "|| MsgCB > type=$type, args=$args"
	}
    }
}

proc ::tkjabber::parseMsg { nick msg color tag time } {
    set msg [split $msg " "]
    set opts {}
    if { $nick eq "ijchain" } {
	set nick [lindex $msg 0]
	set msg [lrange $msg 1 end]
	if { $nick eq "***" } {
	    # Single <> to show IRC users.
	    set nick <[lindex $msg 0]>
	    set action [lrange $msg 1 end]
	    if { $action eq "leaves" || $action eq "joins" } {
		set action [string map { joins entered leaves left } $action]
		::tkchat::addTraffic .txt $nick $action $tag $time
	    } elseif { [lrange $action 0 end-1] eq "is now known as" } {
		set newnick <[lindex $action end]>
		::tkchat::addTraffic .txt $nick nickchange $tag $time $newnick
	    } else {
		::log::log notice "Unknown IRC command '$msg'"
	    }
	    return
	} elseif { $nick eq "*" } {
	    set nick [lindex $msg 0]
	    set action [lrange $msg 1 end]
	    if { $action eq "entered" || $action eq "left" } {
		# Double <> to show webchat users.
		::tkchat::addTraffic .txt <<$nick>> $action $tag $time
		return
	    } else {
		set msg [linsert $action 0 /me]
	    }
	}
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
	    ::tkchat::addTraffic .txt [lindex $msg 0] entered $tag $time
	} elseif { [string match "has left*" [lrange $msg 1 2]] } {
	    ::tkchat::addTraffic .txt [lindex $msg 0] left $tag $time
	}
    } elseif { [lindex $msg 0] eq "/me" } {
	set msg [join [lrange $msg 1 end]]
	::tkchat::addAction .txt $color $nick $msg $tag $time $opts
    } else {
	set msg [join $msg]
	::tkchat::addMessage .txt $color $nick $msg $tag $time $opts
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

proc tkjabber::LoginCB {jlibname type theQuery} {
    # After SendAuth, this is the next Callback.
    variable jabber
    variable roster
    variable conference
    variable muc
    variable baseNick
    variable nickTries

    global Options
    ::log::log debug "LoginCB: type=$type, theQuery='$theQuery'"

    switch -- $type {
	error {
	    if { [lindex $theQuery 0] eq "not-authorized" \
		     || $theQuery eq "{} {}" } {
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
	    tkchat::addSystem .txt "Logged in."
	    #after 20000 [list jlib::schedule_keepalive $jlibname]
	    set tkjabber::reconnect 1
	    set tkjabber::connectionRetryTime [expr {int(5+rand()*5.0)}]
	    $jabber send_presence -type available
	    set muc [jlib::muc::new $jabber]
	    if { $::Options(Nickname) eq "" } {
		set ::Options(Nickname) $::Options(Username)
	    }
	    set baseNick $::Options(Nickname)
	    set nickTries 0
	    $muc enter $conference $::Options(Nickname) -command [namespace current]::MucEnterCB
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
proc tkjabber::MucEnterCB {mucName type args} {
    variable conference
    variable muc
    variable nickTries
    variable baseNick

    ::log::log debug "MucEnter: type=$type, args='$args'"
    switch -- $type {
	error {
	    array set m $args
	    if { [info exists m(-error)] } {
		switch -- [lindex $m(-error) 0] {
		    401 {
			tkchat::addSystem .txt "This conference is password protected."
		    }
		    403 {
			tkchat::addSystem .txt "You have been banned from this conference."
		    }
		    404 {
			tkchat::addSystem .txt "This room does not exist."
		    }
		    405 {
			tkchat::addSystem .txt "The maximum number of users has been reached for this room."
		    }
		    407 {
			tkchat::addSystem .txt "You must be a member to enter this conference."
		    }
		    409 {
			# Nick conflict. Try again?
			if { $nickTries > 5 } {
			    tkchat::addSystem .txt  "Unable to solve nick conflict, try setting one with /nick <nickname> and then trying again"
			}
			if { $nickTries < 2 } {
			    append ::Options(Nickname) _
			} else {
			    set ::Options(Nickname) $baseNick$nickTries
			}
			$muc enter $conference $::Options(Nickname) -command [namespace current]::MucEnterCB
		    }
		    default {
			tkchat::addSystem .txt  "MucEnter: type=$type, args='$args'"
		    }
		}
	    }
	}
	available {
	    #tkchat::addSystem .txt  "MucEnter: type=$type, args='$args'"
	    #only load history for tclers chat when it is not loaded already.
	    if {$conference eq "tcl@tach.tclers.tk" \
		    &&  !$tkjabber::HaveHistory } {
		# Force history loading to the background
		after 10 [list tkchat::LoadHistory]
	    }
	    autoStatus
	}
	default {
	    tkchat::addSystem .txt  "MucEnter: type=$type, args='$args'"
	}
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
	    regexp {([^/])/(.+)} $person -> conf name
	    if { $name eq $user } {
		set user $person
		set found 1
		::tkchat::addAction .txt "" $::Options(Username) \
		    " whispered to $name: $msg"
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
		    ::tkchat::addAction .txt "" $::Options(Username) \
			" whispered to $user/$pres(-resource): $msg"
		}
		unset pres
	    }

	}
	if { !$found } {
	    ::tkchat::addSystem .txt "Unknown nick name '$user'"
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
# JID or lookup a chatroom nick in the members array. Such messages
# are held for the user if the user is not currently available.
proc ::tkjabber::send_memo {to msg {subject Memo}} {
    variable myId
    variable jabber
    variable members

    if {[string first @ $to] == -1} {
	if {[info exists members($to,jid)]} {
	    set to $members($to,jid)
	} else {
	    tkchat::addSystem .txt "Cannot find a JID for '$to'."
	    return
	}
    }

    set k {}
    lappend k [list subject {} 0 $subject {}]
    lappend k [list body {} 0 $msg {}]
    set a [list xmlns jabber:client type normal from $myId to $to]
    set m [list message $a 0 "" $k]
    $jabber send $m
    tkchat::addSystem .txt "Memo send to $to."
}

proc ::tkchat::updateJabberNames {} {
    global Options
    variable ::tkjabber::members

    set scrollcmd [.names cget -yscrollcommand]
    .names configure -yscrollcommand {}

    # Delete all URL-* tags to prevent a huge memory leak
    foreach tagname [.names tag names] {
	if {[string match URL-* $tagname]} {
	    .names tag delete $tagname
	}
    }

    set i 0
    .names config -state normal
    .names delete 1.0 end
    .mb.mnu delete 0 end
    .mb.mnu add command -label "All Users" \
	-command [list ::tkchat::MsgTo "All Users"]
    set Options(OnLineUsers) {}
    foreach person [lsort -dictionary [$::tkjabber::muc participants $::tkjabber::conference]] {

	if {![regexp {([^/])/(.+)} $person -> conf name]} {
	    ::log::log debug "updateJabberNames: regexp failed on '$person'"
	    continue
	}

	set status [list online];# FIX ME
	if {[info exists members($name,status)]} {
	    set status $members($name,status)
	}

	lappend Options(OnLineUsers) $name
	# NOTE : the URL's don't work because of the & in them
	# doesn't work well when we exec the call to browsers
	# and if we follow spec and escape them with %26 then
	# the cgi script on the other end pukes so we will
	# just do an inline /userinfo when they are clicked
	switch -exact -- [lindex $status 0] {
	    online {
		.names image create end -image ::tkchat::roster::online
	    }
	    chat {
		.names image create end -image ::tkchat::roster::chat
	    }
	    dnd {
		.names image create end -image ::tkchat::roster::dnd
	    }
	    away {
		.names image create end -image ::tkchat::roster::away
	    }
	    xa {
		.names image create end -image ::tkchat::roster::xa
	    }
	}
	.names insert end "$name" [list NICK URL URL-[incr ::URLID]] "\n"
	.names tag bind URL-$::URLID <Double-Button-1> \
	    "[list ::tkjabber::getChatWidget $::tkjabber::conference/$name $name]"
	.names tag bind URL-$::URLID <3> \
	    "set ::tkchat::UserClicked 1;\
	       [list ::tkjabber::msgSend "/userinfo $name"]"
	incr i
	.mb.mnu add command -label $name \
	    -command [list ::tkchat::MsgTo $name]
    }

    .names insert 1.0 "$i Users Online\n\n" TITLE
    .names configure -yscrollcommand $scrollcmd
    .names config -state disabled
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
}


proc ::tkjabber::setNick { newnick } {
    variable muc
    variable conference
    variable roster
    variable jabber
    variable grabNick

    if {[lsearch -exact $::Options(OnLineUsers) $newnick] > -1 } {
	# Perhaps it is my own nick, in another window?
	set x [$roster getx $conference/$newnick muc#user]
	set item [wrapper::getchildswithtag $x item]
	set otherjid ""
	if {[llength $item] > 0} {
	    set otherjid [wrapper::getattribute \
			    [lindex $item 0] jid]
	}
	regexp {([^/]+)/(.+)} [$jabber myjid] -> myjid myres

	if { [regexp {([^/]+)/(.+)} $otherjid -> ojid ores] } {
	    if { $ojid eq $myjid && $ores ne $myres } {
		# Yes, it is my JID, different resource.
		# Send a rename request:
		set attrs [list xmlns urn:tkchat:changenick]
		set xlist [list [wrapper::createtag x -attrlist $attrs]]

		$tkjabber::jabber send_message $otherjid -type chat -xlist $xlist
		tkchat::addSystem .txt "This nick is owned by another you, requested transfer..."
		set grabNick $newnick
		return
	    }
	}
	tkchat::addSystem .txt "The nickname '$newnick' is already in use."
	return
    }

    # There is a race condition here. new nick could enter between the check
    # and the setnick call...
    set ::Options(Nickname) $newnick
    $muc setnick $conference $newnick
}

proc ::tkjabber::transferNick { reqfrom } {
    variable muc
    variable conference
    variable roster
    variable jabber

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
	    set postfix "Away"
	}
    }
    set newnick $::Options(Nickname)$postfix
    if {[lsearch -exact $::Options(OnLineUsers) $newnick] > -1 } {
	tkchat::addSystem .txt "Got a nick transfer request, but $newnick is already in use."
	return
    }

    # Set my nick name to newnick.
    set ::Options(Nickname) $newnick
    $muc setnick $conference $newnick

    # The other party does not need to be notified - it should be in nickgrab mode.

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
    set time [clock scan ${when} -gmt 1]
    lappend HistoryLines [list $time $nick $msg]
    if { [llength $args] > 0 } {
	::log::log warning "Log incorrect log format."
    }
    ::log::log debug "[clock format $time] $nick :: $msg"
}

proc ::tkjabber::HistoryLines {} {
    variable HistoryLines
    return [llength $HistoryLines]
}

proc ::tkjabber::LoadHistoryLines {} {
    global Options
    variable HistoryLines

    set state [.txt cget -state]
    .txt configure -state normal

    ::log::log debug tkjabber-LoadHistoryLines

    # mask the alerts
    set alerts [array get Options Alert,*]
    foreach {alert value} $alerts { set Options($alert) 0 }

    if {![info exists Options(FinalList)]} {set Options(FinalList) {}}

    set count 0
    foreach entry $HistoryLines {
	set time [lindex $entry 0]
	set nick [lindex $entry 1]
	set msg [lindex $entry 2]

	::tkjabber::parseMsg $nick $msg "" HISTORY $time

	incr count
	if {$count > 35 } { break }
    }
    .txt see end
    set HistoryLines [lrange $HistoryLines $count end]

    # Restore the alerts
    array set Options $alerts

    if {$HistoryLines == {}} {
	::log::log debug "History loading completed."
	.txt configure -state normal
	.txt delete "HISTORY + 1 char" "HISTORY + 1 line"
	.txt insert "HISTORY + 1 char" \
	    "+++++++++++++++++++++ End Of History +++++++++++++++++++++\n"
    } else {
	after idle [list after 0 ::tkjabber::LoadHistoryLines]
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

    tkchat::addSystem .txt "Will try to reconnect in $connectionRetryTime seconds."
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

proc tkjabber::away {status {show away}} {
    variable Away
    variable conference
    set Away 1
    set jid $conference/[$tkjabber::muc mynick $conference]
    $tkjabber::jabber send_presence -type available \
	-from $jid -to $conference -show $show -status $status
}

proc tkjabber::back {status} {
    variable Away
    variable AutoAway
    variable conference
    set Away 0
    set AutoAway 0

    set jid $conference/[$::tkjabber::muc mynick $conference]
    $::tkjabber::jabber send_presence -type available \
	-from $jid -to $conference -show online -status $status
}

# -------------------------------------------------------------------------

proc tkjabber::on_iq_version {token from subiq args} {
    global tcl_platform

    array set a {-id {}}
    array set a $args
    set opts {}
    if {$a(-id) ne {}} { lappend opts -id $a(-id) }
    set os $tcl_platform(os)
    if {[info exists tcl_platform(osVersion)]} {
	append os " $tcl_platform(osVersion)"
    }
    lappend opts -to $from
    set subtags [list  \
      [wrapper::createtag name    -chdata "Tkchat"]  \
      [wrapper::createtag version -chdata [package provide app-tkchat]]  \
      [wrapper::createtag os      -chdata $os] ]
    set xmllist [wrapper::createtag query -subtags $subtags  \
      -attrlist {xmlns jabber:iq:version}]
    eval {jlib::send_iq $token "result" [list $xmllist]} $opts

    # Tell jlib's iq-handler that we handled the event.
    return 1
}

# -------------------------------------------------------------------------

proc tkjabber::ProxyConnect {proxyserver proxyport jabberserver jabberport} {
    global Options
    variable have_tls

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
	    tls::import $sock
	}
    } else {
	error "proxy connect failed: $block"
    }
    return $sock
}

# -------------------------------------------------------------------------

proc tkjabber::getChatWidget { jid from } {

    variable ChatWindows
    global Options
    # Look in ChatWindows and maybe popup a new chat window

    if {![info exists ChatWindows(txt.$jid)] &&
	[regexp {([^/]+)/} $jid -> jwr] &&
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
	if { [info exists ChatWindows($item.$jid)] } {
	    unset ChatWindows($item.$jid)
	}
    }
}

proc tkjabber::autoStatus {} {
    variable autoStatusAfterId
    variable Away
    variable AutoAway
    variable AwayShow
    global Options

    if { [info exists autoStatusAfterId] } {
	after cancel $autoStatusAfterId
	unset autoStatusAfterId
    }

    if { ![idle::supported] } return
    set autoStatusAfterId [after 1000 [namespace origin autoStatus]]

    if { $Options(AutoAway) == -1 } {
	# Auto Away disabled in config menu
	return
    }

    if { $AutoAway < 0 } {
	# Auto Away disabled because use set status manually.
	return
    }


    set idle_time [expr {$Options(AutoAway)*60}]
    set xa_time [expr {$idle_time + 60*30}]

    # for easy debugging:
    #set idle_time 5
    #set xa_time 10

    if { [idle::idletime] < $idle_time && $AutoAway > 0 } {
	set AutoAway 0
	back ""
    } elseif { [idle::idletime] > $idle_time && $AutoAway == 0  } {
	set AutoAway 1
	away "no activity" away
    } elseif { [idle::idletime] > $xa_time && $AutoAway == 1 } {
	set AutoAway 2
	away "no activity" xa
    }

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

if {![info exists ::URLID]} {
    eval [linsert $argv 0 ::tkchat::Init]


}
