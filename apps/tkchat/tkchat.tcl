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

package require Tcl 8.3         ; # core Tcl
package require Tk 8.3		; # core Tk
package require http 2		; # core Tcl
package require msgcat		; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib
package require base64		; # tcllib

catch {
    package require tls     ; # tls (optional)
}


# We need Tk 8.3.2 to get -state options for [label]s
if {![catch {package vcompare $tk_patchLevel $tk_patchLevel}]} {
    if {![package vsatisfies $tk_patchLevel 8.3.2]} {
	return -code error "Tk version 8.3.2 or better is required."
    }
}

# Deal with 'tile' support.
# We sometimes need to _really_ use the Tk widgets at the moment...
#
if {[llength [info command ::tk::label]] < 1} {
    foreach cmd {label radiobutton entry} {
        rename ::$cmd ::tk::$cmd
    }
    if {![catch {package require tile 0.4}]} {
        if {[namespace exists ::ttk]} {
            namespace import -force ttk::*
        } else {
            namespace import -force tile::*
        }
    }
    foreach cmd {label radiobutton entry} {
        if {[llength [info command ::$cmd]] < 1} {
            interp alias {} ::$cmd {} ::tk::$cmd
        }
    }
}

# Under windows, we can use DDE to open urls
if {$tcl_platform(platform) == "windows"} {
    package require dde
}

package forget app-tkchat	;# Workaround until I can convince people
;# that apps are not packages.	:)  DGP
package provide app-tkchat \
    [regexp -inline {\d+(?:\.\d+)?} {$Revision: 1.227 $}]

# Maybe exec a user defined preload script at startup (to set Tk options,
# for example.
# just before showing the logon screen (or not), call 'tkchatrcPostload' so
# you can also tinker with settings when the UI has been built.
proc tkchatrcPostload {} {}
if {[info exists ::env(HOME)] && \
	[file readable [set rctclfile \
			    [file join $::env(HOME) .tkchatrc.tcl]]]} {
    if {[catch {uplevel \#0 source $rctclfile} err]} {
        tk_messageBox -type ok -icon error \
            -title "Error while loading \"$rctclfile\"" \
            -message $err
        log::log error $err
	exit
    }
}

namespace eval ::tkchat {
    variable chatWindowTitle "The Tcler's Chat"
    
    # Everything will eventually be namespaced
    variable MessageHooks
    array set MessageHooks {}
    variable ChatActivityHooks
    array set ChatActivityHooks {}

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://mini.net

    variable HEADUrl {http://cvs.sourceforge.net/viewcvs.py/tcllib/tclapps/apps/tkchat/tkchat.tcl?rev=HEAD}
    variable rcsid   {$Id: tkchat.tcl,v 1.227 2004/11/18 11:35:11 pascalscheffers Exp $}

    variable MSGS
    set MSGS(entered) [list \
			   "%user% has entered the chat!" \
			   "Out of a cloud of smoke, %user% appears!" \
			   "%user% saunters in." \
			   "%user% wanders in." \
			   "%user% checks into the chat." \
			   "%user% is feeling chatty!" \
			  ]
    set MSGS(left) [list \
			"%user% has left the chat!" \
			"In a cloud of smoke, %user% disappears!" \
			"%user% exits, stage left!" \
			"%user% doesn't want to talk to you anymore!" \
			"%user% looks at the clock and dashes out the door" \
			"%user% macht wie eine Banane ..." \
			"%user% possibly hasn't really left." \
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

# Tcl8.3 compatibility procs
if {[package vcompare [package provide Tcl] 8.3] == 0} {
    proc ::tkchat::tk_windowingsystem {} {
        if {[string equal $::tcl_platform(platform) "windows"]} {
            return "win32"
        } elseif {[string equal $::tcl_platform(platform) "unix"]} {
            return "x11"
        } else {
            return "dontcare"
        }
    }
} else {
    interp alias {} ::tkchat::tk_windowingsystem {} tk windowingsystem
}

proc ::tkchat::errLog {args} {
    log::logMsg [join $args]
    update idletasks;			# why are we doing this??
}

# trace handler to set the log level whenever Options(LogLevel) is changed
# enable the selected level and above
proc ::tkchat::LogLevelSet {args} {
    global Options
    log::lvSuppressLE emergency 0	  ;# unsuppress all
    log::lvSuppressLE $Options(LogLevel)  ;# suppress all below selected
    log::lvSuppress $Options(LogLevel) 0  ;# unsupress selected
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
	    set Options(ProxyAuth) \
		[list "Proxy-Authorization" \
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

    set file [tk_getSaveFile -title "Save Latest TkChat to ..." \
		  -defaultextension $defExt \
		  -initialdir [file dirname $::argv0] \
		  -initialfile [file tail $::argv0] \
		  -parent . \
		  -filetypes {{"Tcl Files" {.tcl .tk}} {"All Files" {*.*}}}]
    if {[string compare $file ""]} {
	set token [::http::geturl $HEADUrl \
		       -headers [buildProxyHeaders] -timeout 30000]
	::http::wait $token
        if {[string equal [::http::status $token] "ok"] && \
                [::http::ncode $token] == 200} {
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
                set err [join [lrange [split [http::data $token] "\n"] 0 30] "\n"]
            }
        }

	::http::cleanup $token

	if {$code} {
	    tk_messageBox -type ok -icon error \
		-title "Error retrieving tkchat from CVS" \
		-message $err
	    log::log error $err
	} else {
	    set resource? [tk_messageBox -type yesno -icon info \
			       -title "Retrieved tkchat $rcsVersion" \
			       -message "Successfully retrieved v$rcsVersion.\
		    Do you want to reload from the new version?"]
	    if {${resource?} == "yes"} {
		Debug reload
	    }
	}
    }
}

# Check the HTTP response for redirecting URLs. - PT
proc ::tkchat::checkForRedirection {tok optionName} {
    global Options
    set ncode [::http::ncode $tok]
    if {[expr {$ncode == 302 || $ncode == 301 || $ncode == 307}]} {
	upvar \#0 $tok state
	array set meta $state(meta)
	if {[info exists meta(Location)]} {
	    set Options($optionName) $meta(Location)
	    return 1
	}
    }
    return 0
}

proc ::tkchat::GetHistLogIdx {szary} {
    global Options
    upvar $szary ary

    catch {unset ary}
    array set ary {}
    set loglist {}
    # get list of available logs
    if { $Options(UseJabber) } {
	# Jabber logs
	set url "$Options(JabberLogs)/?pattern=*.tcl"
    } else {
	# Webchat logs
	set url "$Options(URLlogs)/?M=D"
    }
    set tok [::http::geturl $url \
		 -headers [buildProxyHeaders]]
    errLog "History Fetch: status was\
	    [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
	    if {[::http::ncode $tok] >= 500} {
                HttpServerError $tok "History Fetch"
            } elseif {[::http::ncode $tok] >= 400} {
                AuthenticationError $tok
	    } else {
		if { [checkForRedirection $tok URLlogs] } {
		    # trim off the /?M=D part
		    regsub {/\?M=D} $Options(URLlogs) {} Options(URLlogs)
		    set loglist [::tkchat::GetHistLogIdx ary]
		} else {
		    if { $Options(UseJabber) } {
			set RE {<A HREF="([0-9\-%d]+\.tcl)">.*\s([0-9]+) bytes}
		    } else {
			set RE {<A HREF="([0-9-]+\.txt)">.*\s([0-9]+k)}
		    }
		    foreach line [split [::http::data $tok] \n] {
			puts "$RE $line"
			if { [regexp  -- $RE $line -> logname size] } {
			    if { $Options(UseJabber) } {
				set logname [string map {"%2d" -} $logname]
				set size [expr { $size / 1024 }]k
				lappend loglist $logname
			    } else {
				set loglist [linsert $loglist 0 $logname]
			    }
			    set ary($logname) $size
			}
		    }
		}
	    }
	}
	reset { errLog "User reset post operation" }
	timeout { errLog "History Fetch timed out." }
	error {
	    tk_messageBox -message "History Fetch error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
    # Show a max of 7 days worth of logs
    return [lrange $loglist end-6 end]
}
proc ::tkchat::ParseHistLog {log {reverse 0}} {
    global Options

    if { $Options(UseJabber) } {
	# Jabber logs
	set url "$Options(JabberLogs)/$log"
    } else {
        set url "$Options(URLlogs)/$log"
    }

    set retList {}    
    set MsgRE {^\s*(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun).+?\[([^\]]+)\]\s+([^:]+):?\s*(.*)$}
    set ircRE {ircbridge: \*\*\* (.+) (.+)$}
    set TimeRE {^(.+?)\s+(.+?)\s+(\d{1,2})\s+(\d\d:\d\d:\d\d)\s+(\d{4})}
    
    set logTime 0
    # fetch log
    log::log info "History: Fetch log \"$url\""
    set tok [::http::geturl $url \
                   -headers [buildProxyHeaders]]

    errLog "History: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
        ok {
	    if { $Options(UseJabber) } {
		# Jabber logs
		set I [interp create -safe]
		interp alias $I m {} ::tkjabber::ParseLogMsg
		if { $reverse } {
		    set histTmp $::tkjabber::HistoryLines
		    set ::tkjabber::HistoryLines {}
		}
		$I eval [::http::data $tok]
		if { $reverse } {		    
		    set ::tkjabber::HistoryLines [concat $::tkjabber::HistoryLines $histTmp]
		}
		#set retList [tkjabber::LogMsgLines]
	    } else {
		set logdata [split  [::http::data $tok] \n]
		set lastnick ""
		set lastdata ""
		foreach line $logdata {
		    #log::log debug "History Parse: $line"
		    if { [regexp -- $TimeRE $line -> weekday month day time year] } {
			# the regexp makes sure the format is correct and
			# lets us insert the server timezone (CEST)
			set logTime [clock scan "$weekday $month $day $time CEST $year"]
		    } else {		    
			set logTime 0
		    }
		    
		    if {[regexp -- $MsgRE $line -> type nick data]} {
			if {[string length $lastnick]>0} {
			    lappend retList $logTime $lastnick $lastdata
			}
			# only show messages - don't care about enter/exit/new user
			# crap
			if {[string equal $type MSG]} {
			    #but we _do_ care about ircbridge joins/leaves:
			    if {[regexp -nocase -- $ircRE $line -> inick iaction]} {
				updateIrcUsers $inick $iaction
				set lastnick ""
				set lastdata ""
			    } else {
				set lastnick $nick
				set lastdata $data
			    }
			} else {
			    set lastnick ""
			    set lastdata ""
			}
		    } else {
			append lastdata "\n$line"
		    }
		}
		if {[string length $lastnick]>0} {
		    lappend retList $logTime $lastnick $lastdata
		}
	    }
        }
        reset {
            errLog "History fetch was reset."
        }
        timeout {
            errLog "History fetch timed out"
        }
        error {
            tk_messageBox -message "History fetch error: [::http::error $tok]"
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
    if {$Options(HistoryLines) == 0} {
	# don't even bother
    } elseif {$Options(HistoryLines) < 0} {
	set loglist [GetHistLogIdx logsize]
	if {[llength $loglist]>0} {
	    # ask user
	    set t [toplevel .histQ -class dialog]
	    wm withdraw $t
	    wm transient $t
	    wm protocol $t WM_DELETE_WINDOW { }
	    wm title $t "Load History From Logs"
	    grid [label $t.lbl \
		    -text "Please select how far back you want to load:"] \
		    -sticky ew -pady 5
	    set i 0
	    variable HistQueryNum [llength $loglist]
	    foreach l $loglist {
		grid [radiobutton $t.rb$i -text "$l ($logsize($l))" \
			-val $i -var ::tkchat::HistQueryNum] \
			-sticky w -padx 15 -pady 0
		incr i
	    }
	    grid [radiobutton $t.rb$i -text "None" \
		    -val $i -var ::tkchat::HistQueryNum] \
		    -sticky w -padx 15 -pady 0
	    grid [button $t.ok -text Ok -width 8 -command [list destroy $t] \
		    -default active] \
		    -sticky e -padx 5 -pady 10
	    grid columnconfigure $t 0 -weight 1
	    bind $t <Return> [list $t.ok invoke]
	    catch {::tk::PlaceWindow $t widget .}
	    wm deiconify $t
	    tkwait visibility $t
            focus $t.ok
	    grab $t
	    tkwait window $t
	    foreach log [lrange $loglist $HistQueryNum end] {
		if {[catch {ParseHistLog $log} new]} {
		    log::log error "error parsing history: \"$new\""
		} else {
		    set FinalList [concat $FinalList $new]
		}
	    }
	}
    } else {
	set loglist [GetHistLogIdx logsize]
	# go thru logs in reverse until N lines loaded
	for {set idx [expr {[llength $loglist] - 1}]} {$idx >= 0} {incr idx -1} {
	    # fetch log
	    set log [lindex $loglist $idx]
	    if {[catch {ParseHistLog $log 1} new]} {
                log::log error "error parsing history: \"$new\""
	    } else {
		set FinalList [concat $new $FinalList]
	    }
	    if { $Options(UseJabber) } {
		if { [::tkjabber::HistoryLines] >= $Options(HistoryLines) } {
		    break
		}
	    } else {
		if {[expr {[llength $FinalList]/2}] >= $Options(HistoryLines)} {
		    break
		}
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
    
    if { $Options(UseJabber) } {
	::tkjabber::LoadHistoryLines
    } else {
	LoadHistoryLines
    }    
}


proc ::tkchat::LoadHistoryLines {} {
    global Options

    set state [.txt cget -state]
    .txt configure -state normal

    log::log debug LoadHistoryLines

    # mask the alerts
    set alerts [array get Options Alert,*]
    foreach {alert value} $alerts { set Options($alert) 0 }

    if {![info exists Options(FinalList)]} {set Options(FinalList) {}}

    set count 0
    foreach {time nick msg} $Options(FinalList) {
        addMessage "" $nick $msg HISTORY $time
        incr count 3
        if {$count > 100} { break }
    }
    .txt see end
    set Options(FinalList) [lrange $Options(FinalList) $count end]

    # Restore the alerts
    array set Options $alerts

    if {$Options(FinalList) == {}} {
        log::log debug "History loading completed."
        .txt configure -state normal
        .txt delete "HISTORY + 1 char" "HISTORY + 1 line"
        .txt insert "HISTORY + 1 char" \
            "+++++++++++++++++++++ End Of History +++++++++++++++++++++\n"
    } else {
        after idle [list after 0 ::tkchat::LoadHistoryLines]
    }

    .txt configure -state $state
}


proc ::tkchat::msgSend {str {user ""}} {
  
    
    global Options
    if { $::Options(UseJabber) } {
	::tkjabber::msgSend $str -user $user	
    } else {
	errLog "Send to $Options(URL)"
	if {![package vsatisfies [package provide http] 2.4.6]} {
	    # The w3c says that strings should be utf-8 encoded when passed in
	    # x-url-encoding: http://www.w3.org/International/O-URL-code.html
	    # The http package only does this in 2.4.6+.
	    # We only worry about the user input string.
	    set str [encoding convertto utf-8 $str]
	}
	set qry [::http::formatQuery \
		       action	postmsg \
		       name	$Options(Username) \
		       password	$Options(Password) \
		       color	$Options(MyColor) \
		       updatefrequency 600 \
		       new_msg_on_top 0 \
		       ls		"" \
		       msg_to	$user \
		       msg		$str \
		      ]
	if {[catch {
	    ::http::geturl $Options(URL) \
		  -query [string map {%5f _} $qry] \
		  -headers [buildProxyHeaders] \
		  -command ::tkchat::msgDone
	} msg]} {
	    set delay [expr {$Options(Refresh) * 1000 / 2}]
	    errLog "Retrying msgSend after $delay: \"$msg\""
	    after $delay [list ::tkchat::msgSend $str $user]
	}
    }
}

proc ::tkchat::msgDone {tok} {
    global Options
    errLog "Post: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[::http::ncode $tok] == 500} {
                if {[info exists Options(msgSend_Retry)]} {
                    set msg "Posting error: retry failed: [::http::code $tok]"
                    tk_messageBox -message $msg
                    log::log error $msg
                    unset Options(msgSend_Retry)
                } else {
                    log::log error "Post error: [::http::code $tok] - need to retry"
                    set Options(msgSend_Retry) 1
                    # FRINK: nocheck
                    after idle [list ::http::geturl $Options(URL) \
                                      -query [set ${tok}(-query)] \
                                      -headers [buildProxyHeaders] \
                                      -command ::tkchat::msgDone]
                }
            } else {
                checkForRedirection $tok URL
                if {[catch {fetchPage} err]} { errLog $err }
            }
        }
	reset { errLog "User reset post operation" }
	timeout { errLog "Message Post timed out" }
	error {
	    tk_messageBox -message \
                  "Message Post Errored: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc ::tkchat::logonChat {{retry 0}} {
    global Options
    if {0} {	
        # use when testing only - allows restarts without actually logging in again
        catch {pause off}
        return
    }
    
    if { $::Options(UseJabber) } {
	# These package requires should be moved to the top of the script
	# when jabber support matures.
	lappend ::auto_path [file join [file dirname [info script]] lib]
	package require sha1	        ; # tcllib
	package require jlib            ; # jlib
	package require browse          ; # jlib
	package require muc             ; # jlib	
	#package require jlibhttp        ; # jlib
	
	# Logon to the jabber server.
	tkjabber::connect	
    } else {
	# Logon to the CGI based chat.
    
	errLog "Logon to $Options(URL2)"
	set qry [::http::formatQuery \
		       action       login \
		       name         $Options(Username) \
		       password     $Options(Password) \
		      ]
	::http::geturl $Options(URL2) \
	      -query $qry \
	      -headers [buildProxyHeaders] \
	      -command [namespace origin logonDone]
    }
    
}

proc ::tkchat::logonDone {tok} {
    errLog "Logon: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
	    if {[checkForRedirection $tok URL2]} {
		::http::cleanup $tok
		logonChat 0
		return
	    }

	    if {[::http::ncode $tok] >= 500} {
		tk_messageBox -message "Logon failure: [::http::code $tok]"
		pause 1 0
            } elseif {[::http::ncode $tok] >= 400} {
                AuthenticationError $tok
	    } else {
		if {[catch {pause off} err]} { errLog $err }
		::tkchat::DoAnim
		::tkchat::msgSend "/msg ircbridge onlineusers"
                
                if {[catch {LoadHistory} err]} { errLog $err }
	    }
	}
	reset	{ errLog "User reset logon operation" }
	timeout	{ tk_messageBox -message "Logon timed out" ; pause 1 0}
	error	{ tk_messageBox -message "Logon Error: [::http::error $tok]" }
    }
    ::http::cleanup $tok
}
proc ::tkchat::logoffChat {} {
    global Options
    set qry [::http::formatQuery action gotourl url chat.cgi]
    ::http::geturl $Options(URL2) \
	-query $qry \
	-headers [buildProxyHeaders] \
	-command [namespace origin logoffDone]
    logonScreen
}

proc ::tkchat::logoffDone {tok} {
    errLog "Logoff: status was [::http::status $tok][::http::code $tok]"
    # don't really care if this works or not
    ::http::cleanup $tok
}

proc ::tkchat::pause {pause {notify 1}} {
    global Options
    set ::tkchat::pause [string is true -strict $pause]
    if {$pause} {
	after cancel $Options(FetchTimerID)
	after cancel $Options(OnlineTimerID)
	catch {::http::reset $Options(FetchToken)}
	catch {::http::reset $Options(OnlineToken)}
	if {$notify} {
	    if {![winfo exists .pause]} {
		toplevel .pause -class dialog
		wm withdraw .pause
		wm transient .pause .
		pack [label .pause.m -text \
                            "The session is paused,\nno updates will occur."]
		button .pause.r -text "Resume" \
                      -command { ::tkchat::pause off ; wm withdraw .pause }
		pack .pause.r -padx 5 -pady 10
		bind .pause <Destroy> [list ::tkchat::pause off]
	    }
	    catch {::tk::PlaceWindow .pause widget .}
	    wm deiconify .pause
            focus .pause.r
	    raise .pause
	}
    } else {
        # Always do a fetchPage here - not checkPage.
	::tkchat::fetchPage
	::tkchat::onlinePage
    }
}

proc ::tkchat::checkPage {} {
    global Options

    if { [catch {

	set tok [http::geturl $Options(URLchk) \
		     -headers [buildProxyHeaders] \
		     -command [list ::tkchat::checkDone [clock seconds]]]
	errLog "checkPage token $tok"
	set Options(retryFailedCheckPage) 5

    } ] } {

	if { ![info exists Options(retryFailedCheckPage)] } {
	    set Options(retryFailedCheckPage) 5
	}
	addSystem "checkPage failed: [lindex [split $::errorInfo "\n"] 0]"
	errLog "error ::tkchat::checkPage: $::errorInfo"
        if {!$::tkchat::pause} {
            after [expr {$Options(retryFailedCheckPage)*1000}] ::tkchat::checkPage
	    addSystem "Trying again in $Options(retryFailedCheckPage) seconds."        
	    set Options(retryFailedCheckPage) [expr {$Options(retryFailedCheckPage)*2}]
	    if { $Options(retryFailedCheckPage) > 15*60 } {
		# Max out at 15 minutes intervals:
		set Options(retryFailedCheckPage) [expr {15*60}]
	    }
	}

    }

}

proc ::tkchat::checkDone {start tok} {
    set elapsed [expr {[clock seconds] - $start}]
    errLog "checkDone in $elapsed secs status [::http::status $tok]\
            [string length [http::data $tok]] bytes."
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                checkPage
                return
            }

            if {[::http::ncode $tok] >= 500} {
                errLog "Error calling check script. [http::error $tok]"
            }
	}
	reset - Reset - RESET {
	    errLog "Reset called while updating the chat page."
	}
	timeout - Timeout - TIMEOUT {
	    errLog "Timeout occurred while updating the chat page."
	}
	error - Error - ERROR {
	    errLog "Error occurred while checking for udpdates: \
                [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
    fetchPage
    onlinePage
}

proc ::tkchat::fetchPage {} {
    global Options

    if {[info exists Options(FetchToken)]} {
	# already fetching page, don't start again
	return
    }

    errLog "fetchPage from $Options(URL)"

    after cancel $Options(FetchTimerID)
    set Options(FetchTimerID) -1
    set qry [::http::formatQuery \
                   action	chat \
                   name	$Options(Username) \
                   password	$Options(Password) \
                   color	$Options(MyColor) \
                   updatefrequency 600 \
                   new_msg_on_top 0 \
                   ls		"" \
                  ]
    if {[catch {
        set Options(FetchToken) [::http::geturl $Options(URL) \
                                       -query $qry \
                                       -headers [buildProxyHeaders] \
                                       -command ::tkchat::fetchDone]
    } msg]} {
        # If the http connection failed and we caught it then we probably
        # are not connected to the network. Keep trying - maybe we are moving
        # our laptop or something :)
        errLog "Fetch error: $msg"
        if {!$::tkchat::pause} {
            #set Options(FetchTimerID) \
            #  [after [expr {$Options(Refresh) * 1000}] ::tkchat::fetchPage]
            after 1000 ::tkchat::checkPage
        }
    }
}

proc ::tkchat::fetchDone {tok} {
    global Options
    # If we timed out while still tying to connect this variable may not
    # be set.
    if {[info exists Options(FetchToken)]} {
        if {[string equal $tok $Options(FetchToken)]} {
            unset Options(FetchToken)
        } else {
            errLog "Fetch Command finished with token $tok" \
                  "expected $Options(FetchToken)"
            unset Options(FetchToken)
        }
    }
    if {!$::tkchat::pause} {
	#set Options(FetchTimerID) \
        #      [after [expr {$Options(Refresh) * 1000}] ::tkchat::fetchPage]
        after 1000 ::tkchat::checkPage
    }
    errLog "Fetch: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                ::tkchat::fetchPage
                return
            }
            if {[::http::ncode $tok] >= 500} {
                # server failed: don't try to parse anything
                # we could toggle some connected icon here.
            } else {
                if {[catch {parseData [::http::data $tok]} err]} {
                    errLog $err
                }
            }
	}
	reset - Reset - RESET {
	    errLog "Reset called while updating the chat page."
	}
	timeout - Timeout - TIMEOUT {
	    errLog "Timeout occurred while updating the chat page."
	}
	error - Error - ERROR {
	    tk_messageBox -message "fetchPage error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc ::tkchat::onlinePage {} {
    global Options
    if {[info exists Options(OnlineToken)]} {
	# already fetching page, don't start again
	return
    }

    after cancel $Options(OnlineTimerID)
    set Option(OnlineTimerID) -1
    set qry [::http::formatQuery \
                   action	stillalive \
                   name	$Options(Username) \
                   password	$Options(Password) \
                   color	$Options(MyColor) \
                   updatefrequency 600 \
                   new_msg_on_top 0 \
                   ls		"" \
                  ]
    if {[catch {
        set Options(OnlineToken) [::http::geturl $Options(URL) \
                                        -query $qry \
                                        -headers [buildProxyHeaders] \
                                        -command ::tkchat::onlineDone]
    } msg]} {
        errLog "Fetch error: $msg"
    }
}

proc ::tkchat::onlineDone {tok} {
    global Options
    if {[info exists Options(OnlineToken)]} {
        if {[string equal $tok $Options(OnlineToken)]} {
            unset Options(OnlineToken)
        } else {
            errLog "Online Command finished with token $tok" \
                  "expected $Options(OnlineToken)"
            unset Options(OnlineToken)
        }
    }
    errLog "Online: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                ::tkchat::onlinePage
                return
            }
	    if {[catch {updateNames [::http::data $tok]} err]} { errLog $err }
	}
	reset {
	    errLog "Reset called while retrieving the online page."
	}
	timeout {
	    errLog "Retrieval of the online users information timed out."
	}
	error {
	    tk_messageBox -message "onlinePage error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

# Display the error message returned when an HTTP request results
# in an authentication error.
# Do NOT clean up this token - that's the callers job.
#
proc ::tkchat::AuthenticationError {token {prefix ""}} {
    log::log error "$prefix error: [http::code $token]"
    variable msgtext ""
    htmlparse::parse \
        -cmd [list ::tkchat::ErrorMessageParse ::tkchat::msgtext] \
        [http::data $token]
    set msgtext [regsub -all -line "\n{1,}" $msgtext "\n"]
    tk_messageBox \
        -title [http::code $token] \
        -icon warning \
        -message $msgtext
    unset msgtext
}

proc ::tkchat::ErrorMessageParse {varname tag end attr text} {
    upvar #0 $varname v
    set tag [string tolower $tag]
    set end [string length $end]
    if {[string equal $tag "hmstart"] && $end == 0} {
        set v ""
    } elseif {[string match "h*" $tag] && $end == 0} {
        append v "\n$text"
    } elseif {[string equal "p" $tag] && $end == 0} {
        append v "\n$text"
    } elseif {[string equal "pre" $tag] && $end == 0} {
        append v "\n$text"
    } elseif {[string equal "a" $tag]} {
        append v "$text"
    }
}

proc ::tkchat::HttpServerError {token {prefix ""}} {
    set msg "$prefix error: [::http::code $tok]"
    log::log error $msg
    tk_messageBox -message $msg
}

# -------------------------------------------------------------------------
# Translate the selection using Babelfish.
# -------------------------------------------------------------------------

proc ::tkchat::fetchurldone {cmd tok} {
    errLog "fetchurl: status was [::http::status $tok] [::http::code $tok]"
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
	    errLog "Reset called during fetch of URL"
	}
	timeout - Timeout - TIMEOUT {
	    errLog "Timeout occurred during fetch of URL"
	}
	error - Error - ERROR {
	    tk_messageBox -message "Fetch URL error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc ::tkchat::translateSel {from to} {
    if {![catch {selection get} msg]} {
        log::log debug "translate: $from $to \"$msg\""
        translate $from $to $msg
    }
}

proc ::tkchat::translate {from to text} {
    set url {http://babelfish.altavista.com/babelfish/tr}
    append op $from _ $to
    set query [http::formatQuery tt urltext urltext $text lp $op]
    set tok [http::geturl $url \
             -query $query \
             -headers [buildProxyHeaders] \
             -command [list ::tkchat::fetchurldone ::tkchat::translateDone]]
}

proc ::tkchat::translateDone {tok} {
    set ::tkchat::translate [http::data $tok]
    set r [regexp {<td.*?class=s><div.*?>(.*)</div>} \
            [::http::data $tok] -> text]
    set text [string trim $text]
    log::log debug "Translate: \"$text\""
    if {$r} {
        #addSystem "TR: $text"
        showInfo Translation $text
    } else {
        errLog "Translation returned no matching data."
    }
}

proc ::tkchat::babelfishInit {{url http://babelfish.altavista.com/babelfish/}} {
    set tok [http::geturl $url \
             -headers [buildProxyHeaders] \
             -command [list ::tkchat::fetchurldone \
                            ::tkchat::babelfishInitDone]]
}

proc ::tkchat::babelfishInitDone {tok} {
    log::log debug "Babelfish init done."
    set ::tkchat::babelfish [http::data $tok]
    if {[regexp {<select name="lp"[^>]*?>(.*?)</select>} [::http::data $tok] -> r]} {
        .mbar.help.tr delete 0 end
        set lst [split [string trim $r] \n]
        foreach option $lst {
            regexp {<option value="(.*?)"[^>]*>(.*?)</option>} \
                    $option -> value label
            set value [split $value _]
            #log::log debug "option: $label $value"
            .mbar.help.tr add command -label $label \
                    -command [concat [namespace current]::translateSel $value]
            variable babelfishinit
            set babelfishinit 1
        }
    } else {
        log::log debug "babelfish received no data"
    }
}

proc ::tkchat::babelfishMenu {} {
    set menu .mbar.help
    if {![winfo exists ${menu}.tr]} {
        log::log debug "Initializing babelfish translation"
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
    log::log debug "babelfishmenu post"
    if {![winfo exists ${mbar}.tr]} {
        babelfishMenu
    }
    tkwait variable babelfishinit
    .mbar.help.tr post $x $y
}

# -------------------------------------------------------------------------

proc tkchat::updateIrcUsers { who what } {
    global Options
    variable ircOnlineUsers
    set userNo [lsearch -exact $ircOnlineUsers $who]

    #Check for rename:
    if { [regexp {([^ ]+) is now known as} $who -> realwho] } {
	updateIrcUsers $realwho leaves
	updateIrcUsers $what joins
	return
    }

    switch $what {
	joins {
	    if { $userNo == -1 } {
		lappend ircOnlineUsers $who
	    }
	}
	leaves {
	    if { $userNo > -1 } {
		set ircOnlineUsers [lreplace $ircOnlineUsers $userNo $userNo]
	    }
	}
	default {
	    log::log debug "Unknown ircbridge user update action '$what'"
	}
    }

}

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

    if {$user == "All Users"} {
        set type normal
    } else {
        set type whisper
    }

    foreach w $windows {
        $w configure -bg $MsgToColors($w,$type)
    }

    set Options(MsgTo) $user
}

proc ::tkchat::updateNames {rawHTML} {
    global Options
    variable ircOnlineUsers

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
    set exp {<A HREF="(.+?)".*?>(.+?)</A>}
    .mb.mnu delete 0 end
    .mb.mnu add command -label "All Users" \
	-command [list ::tkchat::MsgTo "All Users"]
    set Options(OnLineUsers) {}
    foreach {full url name} [regexp -nocase -all -inline -- $exp $rawHTML] {
	lappend tmp [list $full $url $name]
    }
    foreach person [lsort -dictionary -index 2 $tmp] {
	foreach {full url name} $person break
	lappend Options(OnLineUsers) $name
	# NOTE : the URL's don't work because of the & in them
	# doesn't work well when we exec the call to browsers
	# and if we follow spec and escape them with %26 then
	# the cgi script on the other end pukes so we will
	# just do an inline /userinfo when they are clicked
	.names insert end "$name" [list NICK URL URL-[incr ::URLID]] "\n"
	.names tag bind URL-$::URLID <1> \
	    "set ::tkchat::UserClicked 1;\
               [list ::tkchat::msgSend "/userinfo $name"]"
	incr i
	.mb.mnu add command -label $name \
	    -command [list ::tkchat::MsgTo $name]
    }

    # If we ever fix ircbridge to be able to pass whispers to IRC users
    # from the web chat then we can uncomment this and the .mb.mnu add
    # below. [PT]
    #
    #.mb.mnu add separator

    foreach name [lsort -dictionary $ircOnlineUsers] {
        lappend Options(OnLineUsers) $name
	.names insert end "$name" [list NICK] "\n"
	incr i
	#.mb.mnu add command -label $name \
        #      -command [list set Options(MsgTo) $name]
    }

    .names insert 1.0 "$i Users Online\n\n" TITLE
    .names configure -yscrollcommand $scrollcmd
    .names config -state disabled
}

proc ::tkchat::invClr {clr {grays 0}} {
    # generally this is used to get a color that shows
    # up on a dark BG if it was originally a white BG
    # so even the color is grey & the inv color is also
    # grey that is OK
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

proc ::tkchat::getColor {name} {
    global Options
    if {[catch {
	set w $Options(Color,$name,Which)
	set clr $Options(Color,$name,$w)
    } err]} {
	set clr ""
	errLog "bad color name '$name'"
    }
    return $clr
}

proc ::tkchat::fadeColor {color} {
    if {[scan $color "%2x%2x%2x" r g b] == 3} {
	foreach c {r g b} {
	    set $c [expr {255 - int((255-[set $c])*.6)}]
	}
	set color [format "%02x%02x%02x" $r $g $b]
    }
    return $color
}

proc ::tkchat::parseData {rawHTML} {
    global Options
    # get body of data
    set clr ""
    if {[regexp -nocase -- \
               {<BODY.*?(?:BGColor=.([[:xdigit:]]{6}?))?>(.*?)<A\s+NAME="end">.*?</BODY>} \
               $rawHTML -> clr body]} {
	if {[string length $clr] && \
                  [string compare $Options(Color,MainBG,Web) $clr]} {
	    set iclr [::tkchat::invClr $clr]
	    set Options(Color,MainBG,Web) $clr
	    set Options(Color,MainFG,Web) $iclr
	    .txt config -background "#[getColor MainBG]" \
                  -foreground "#[getColor MainFG]"
	}
	# split into "lines"
	set dataList {}
	foreach item [::textutil::splitx [string trim $body] \
                            {[\s\n]*<BR>\n*}] {
	    set item [string trimright $item]
	    if {[string length $item]} {
		lappend dataList $item
	    }
	}
	set newList [getRecentLines $dataList]
	addNewLines $newList
    } else {
	errLog "No BODY found in HTML page"
    }
}

proc ::tkchat::getRecentLines {input} {
    global Options
    set Found 0
    set mark 0
    set end [lindex $Options(History) end]
    set len [llength $Options(History)]
    while {[set idx [lsearch -exact [lrange $input $mark end] $end]] >= 0} {
	set num [expr {$mark + $idx}]
	set back [expr {$len - $num - 1}]
	set l1 [join [lrange $input 0 $num] +]
	set l2 [join [lrange $Options(History) $back end] +]
	set mark [incr num]
	if {[string equal $l1 $l2]} {
	    set Found $mark
	}
	update idletasks
    }
    return [lrange $input $Found end]
}

namespace eval ::tkchat {
    variable UserClicked 0
    variable RE
    array set RE {
        HelpStart {^<FONT COLOR="(.+?)"><B>\[(.+?)\]</B>(.*)$}
        MultiStart {^<FONT COLOR="(.+?)"><B>(\S+?)</B>:(.*?)$}
        ActionStart {^<FONT COLOR="(.+?)"><B>\*\s+(\S+?)\s+(.+)$}
        SectEnd {^(.*)</FONT>$}
        Color {^<FONT COLOR="(.+?)">(.*?)</FONT>$}
        Message {^<B>(\S+?)</B>:(.+?)$}
        Help {^<B>\[(.+?)\]</B>(.*)$}
        Action {^<B>\*\s+(\S+)\s+(.+)</B>$}
        Traffic {^<B>\s*(\S+)\s+has (entered|left) the chat</B>$}
        System {^<B>(.*)</B>$}
	IrcUsers {^<B>ircbridge</B>: \*\*\* ([^ ]+) (joins|leaves)$}
	IrcUserRename {^<B>ircbridge</B>: \*\*\* ([^ ]+) is now known as ([^ ]+)$}
    }
}

proc ::tkchat::addNewLines {input} {
    global Options
    variable RE
    variable UserClicked

    # Add the input to the history.  It's OK to do this before processing.
    eval [list lappend Options(History)] $input

    # Restrict the History size as specified
    if {[llength $Options(History)] > 500} {
        # Unless someone does a HUGE amount
        # of /help & /userinfo stuff this should
        # be plenty long to match against an entire
        # page worth of data
        set Options(History) [lrange $Options(History) end-499 end]
    }

    set inHelp 0
    set inMsg 0
    set inAction 0
    set UserInfoCmd [list]

    foreach line $input {
	# see if color is defined & strip it off
	if {[regexp -nocase -- $RE(Color) $line -> clr text]} {
	    set line $text
	    set color $clr
	} else {
	    set color ""
	}
	# check what kind of line it is
	if {$inHelp} {
	    if {[regexp -nocase -- $RE(SectEnd) $line -> text]} {
		lappend helpLines $text
		set inHelp 0
		if {$helpName == "USERINFO"} {
                    if {$UserClicked} {
                        set UserInfoCmd \
                            [list addHelp $helpColor $helpName \
                                 [join $helpLines \n]]
                    }
		} else {
                    addHelp $helpColor $helpName [join $helpLines \n]
		}
	    } else {
		lappend helpLines [string trimright $line]
	    }
	} elseif {$inMsg} {
	    if {[regexp -nocase -- $RE(SectEnd) $line -> text]} {
		lappend msgLines [string trimright $text]
		set inMsg 0
		addMessage $nickColor $nick [join $msgLines \n]
	    } else {
		lappend msgLines [string trimright $line]
	    }
	} elseif {$inAction} {
	    if {[regexp -nocase -- $RE(SectEnd) $line -> text]} {
		lappend msgLines [string trimright $text]
		set inAction 0
		addAction $nickColor $nick [join $msgLines \n]
	    } else {
		lappend msgLines [string trimright $line]
	    }
	} else {
            # optionally log the chat.
            log::log debug $line
            if {[info exists Options(ChatLogChannel)]} {
                puts $Options(ChatLogChannel) $line
            }

	    if {[regexp -nocase -- $RE(HelpStart) $line -> clr name str]} {
		set inHelp 1
		set helpColor $clr
		set helpName $name
		set helpLines [list $str]
	    } elseif {[regexp -nocase -- $RE(MultiStart) $line \
                             -> clr name str]} {
		set inMsg 1
		set nickColor $clr
		set nick $name
		set msgLines [list [string trimright $str]]
	    } elseif {[regexp -nocase -- $RE(ActionStart) $line \
                             -> clr name str]} {
		set inAction 1
		set nickColor $clr
		set nick $name
		set msgLines [list [string trimright $str]]
	    } elseif {[regexp -nocase -- $RE(IrcUsers) $line -> nick str]} {
		log::log debug "IrcUsers RE"
		switch $str {
		    joins  { addTraffic <$nick> entered }
		    leaves { addTraffic <$nick> left    }
		}
		::tkchat::updateIrcUsers $nick $str
	    } elseif {[regexp -nocase -- $RE(IrcUserRename) $line -> \
			   nick newnick]} {
		log::log debug "IrcUserRename RE"
		::tkchat::updateIrcUsers $nick leaves
		::tkchat::updateIrcUsers $newnick joins

		addSystem "In a fit of schizophrenia, <$nick> would really like to be <$newnick>" 

	    } elseif {[regexp -nocase -- $RE(Message) $line -> nick str]} {
		log::log debug "Message RE"
		addMessage $color $nick [string trim $str]
	    } elseif {[regexp -nocase -- $RE(Help) $line -> name str]} {
		addHelp $color $name [string trim $str]
	    } elseif {[regexp -nocase -- $RE(Action) $line -> nick str]} {
		addAction $color $nick $str
	    } elseif {[regexp -nocase -- $RE(System) $line -> str]} {
                if {[regexp -nocase -- $RE(Traffic) $line -> who action]} {
                    addTraffic $who $action
                } else {
                    addSystem $str
                }
	    } else {
		errLog "Didn't recognize - '$line' - assume help"
		addHelp $color "" [string trim $line]
	    }
	}
    }

    eval $UserInfoCmd
    set UserClicked 0

    if { [.txt index end] > $Options(MaxLines) } {
	.txt config -state normal
	.txt delete 1.0 "end - $Options(MaxLines) lines"
	.txt config -state disabled
    }
}

proc ::tkchat::stripStr {str} {
    if { $::Options(UseJabber) } {
	return $str
    } else {
	# remove any remaining tags
        regsub -all -nocase "<.*?>" $str {} tmp
        # replace html escapes with real chars
        return [::htmlparse::mapEscapes $tmp]
    }
    
}

proc ::tkchat::parseStr {str} {
    global Options
    # get href info return list of str link pairs
    set sList {}
    if { $Options(UseJabber) } {
	set HTTPRE {https?://(((([A-Za-z0-9][A-Za-z0-9-]*[A-Za-z0-9]|[A-Za-z0-9])\.)*([a-zA-Z][A-Za-z0-9-]*[A-Za-z0-9]|[a-zA-Z]))|([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+))(:[0-9]+)?(/([a-zA-Z0-9$_.+!*'(,);:@&=~-]|%[0-9A-Fa-f][0-9A-Fa-f])*(/([a-zA-Z0-9$_.+!*'(,);:@&=~-]|%[0-9A-Fa-f][0-9A-Fa-f])*)*(\?([a-zA-Z0-9$_.+!*'(,);:@&=~-]|%[0-9A-Fa-f][0-9A-Fa-f])*)?)?}
	while {[regexp -nocase -- $HTTPRE $str url]} {
	    set pre ""
	    set post ""
	    set pos [string first $url $str]
	    if { $pos > 0 } {
		set pre [string range $str 0 [expr {$pos-1}]]			 
	    }
	    set post [string range $str [expr {$pos+[string length $url]}] end]
	    
	    if {[string length $pre]} {
		lappend sList [stripStr $pre] ""
	    }
	    lappend sList [stripStr $url] $url
	    set str $post
	}    	
    } else {
	while {[regexp -nocase -- {^(.*?)<A.*?HREF="(.+?)".*?>(.*?)</A>(.*?)$} \
		      $str -> pre url link post]} {
	    if {[string length $pre]} {
		lappend sList [stripStr $pre] ""
	    }
	    lappend sList [stripStr $link] $url
	    set str $post
	}    
    }
    if {[string length $str]} {
	lappend sList [stripStr $str] ""
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

proc ::tkchat::checkNick {nick clr} {
    global Options
        
    set wid [expr {[font measure NAME $nick] + 10}]
    if {$wid > $Options(Offset)} {	
        set Options(Offset) $wid
	
	# Maybe limit the nick column width a bit...
	set max [expr {[font measure NAME [string repeat X 12]]+10}]
	if { $Options(Offset) > $max } {
	    set Options(Offset) $max
	}
	
	# Set tabs appropriate for STAMP visibility
	StampVis
    }
    if {$clr == ""} {
        set clr [getColor $nick]
        if {$clr == ""} {
            set clr [getColor MainFG]
        }
    }
    if {[lsearch -exact $Options(NickList) $nick] < 0} {
        lappend Options(NickList) $nick
        set Options(Color,$nick,Web) $clr
        set Options(Color,$nick,Inv) [::tkchat::invClr $clr]
        set Options(Color,$nick,Mine) $clr
        set Options(Color,$nick,Which) Web
        ::tkchat::NickVisMenu
    }
    if {![info exists Options(Color,$nick,Web)] ||
        [string compare $Options(Color,$nick,Web) $clr]} {
        # new color
        set Options(Color,$nick,Web) $clr
        set Options(Color,$nick,Inv) [::tkchat::invClr $clr]
        if {![info exists Options(Color,$nick,Mine)]} {
            set Options(Color,$nick,Mine) [getColor MainFG]
        }
        set clr [getColor $nick]
        if {$clr == ""} { set clr [getColor MainFG] }
	set nclr [fadeColor $clr]
	.txt tag configure NICK-$nick -foreground "#$clr"
	.txt tag configure NOLOG-$nick -foreground "#$nclr"
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
proc ::tkchat::checkAlert {msgtype nick str} {
    global Options LastPost
    set now [clock seconds]
    set LastPost($nick) $now
    set x Alert,$msgtype
    if {![info exists Options($x)] || !$Options($x)} {
        return
    }
    set alert 0
    if {$Options(Alert,ALL)} {
        set alert 1
    }
    if {!$alert && $Options(Alert,ME)} {
        set myname [string tolower $Options(Username)]
        set txt [string tolower $str]
        if {[string first $myname $txt]>=0} {
            set alert 1
        }
    }
    if {!$alert && $Options(Alert,TOPIC)} {
        if {![info exists LastPost($nick)] || $LastPost($nick)<$now-300} {
            set alert 1
        }
    }
    if {$alert} {
        alertWhenIdle
    }
}

proc ::tkchat::addMessage {clr nick str {mark end} {timestamp 0} {extraOpts ""}} {
    global Options
    variable map
    set w .txt

    array set opts $extraOpts

    if {[string equal $nick "ircbridge"]} {
	if {[regexp {^([^ ]+) says: (.*)$} $str -> truenick msg]} {
	    # Use their true nick, but display bridge users as <$nick>
	    # This allows people registered in both systems to appear
	    # with the right color info.
	    set nick <$truenick>
	    set str  $msg

	    if { [nickIsNoisy $nick] } {
		return
	    }

	    if {[string equal $truenick "ijchain"]
                || [string equal $truenick "ijbridge"]} {
		# ijchain is a Jabber to IRC link.
		if {[regexp {&lt;(.*?)&gt; (.*)$} $str -> truenick msg]} {
		    set nick "<$truenick>"
		    set str $msg
		    if { [regexp {^/me (.+)$} $msg -> action] } {
			addAction $clr "$nick" $action $mark
			return
		    }
		}
	    }
	    #Probably obsolete regexp now ircbridge parses CTCPs:
	    if { [regexp {^ACTION (.+)} $str -> action] } {
		addAction $clr "<$nick>" [string range $action 0 end-1] $mark
		return
	    }
	} elseif {[regexp {^\* ([^ ]+) (.*)$} $str -> truenick msg] } {
	    addAction $clr "<$truenick>" $msg $mark
	    return
	}
    }

    if { [nickIsNoisy $nick] } {
	return
    }

    #for colors, it is better to extract the displayed nick from the one used for
    #tags.
    set displayNick $nick
    regexp {^<(.+)>$} $nick displayNick nick

    checkNick $nick $clr
    checkAlert NORMAL $nick $str
    $w config -state normal
    if {[string equal $nick "clock"] || [string equal $nick "tick"]} {
	$w insert $mark "\t" [list STAMP]
	$w insert $mark "$nick\t" [list NICK NICK-$nick]
        $w insert $mark "[formatClock $str] " [list NICK-$nick MSG]
    } else {
	::tkchat::InsertTimestamp $w $nick $mark $timestamp	
	$w insert $mark "$displayNick\t" [list NICK NICK-$nick]
        foreach {str url} [parseStr $str] {
            foreach cmd [array names ::tkchat::MessageHooks] {
                eval $cmd [list $str $url]
            }
	    if { [info exists opts(nolog)] } {
		set tags [list MSG NOLOG-$nick ACTION]
	    } else {
		set tags [list MSG NICK-$nick]
	    }
            if {$url != ""} {
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
		    log::log debug "More than one line, add tabs"
		    $w insert $mark "\n\t" [list STAMP]		    
		    set line "\t$line"
		}
		tkchat::Insert $w $line $tags $url $mark
	    }
        }
        # Call chat activity hooks
        foreach cmd [array names ::tkchat::ChatActivityHooks] {
            eval $cmd
        }
    }
    $w insert $mark "\n" [list NICK NICK-$nick]
    $w config -state disabled
    if {$Options(AutoScroll)} { $w see end }
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
    }
}
proc ::tkchat::ResetMessageCounter {} {
    variable MessageCounter
    variable chatWindowTitle
    set MessageCounter 0
    set title $chatWindowTitle
    wm title . $title
    wm iconname . $title
}

proc ::tkchat::InsertTimestamp {w nick {mark end} {seconds 0} } { 
    
    # The nick argument is here, so we can display the local time for
    # each nick.

    if { $seconds == 0 } { set seconds [clock seconds] }

    $w insert $mark "\[[clock format $seconds -format %H:%M]\]\t" [list STAMP]
}

proc ::tkchat::Insert {w str tags {url ""} {mark end}} {
    global Options
    set str [string map {"\n" "\n\t"} $str]
    # Don't do emoticons on URLs
    if {($url == "") && $Options(emoticons)} {
	variable IMG
	variable IMGre
	set i 0
	foreach match [regexp -inline -all -indices -- $IMGre $str] {
	    foreach {start end} $match {break}
	    set emot [string range $str $start $end]
	    if {[info exists IMG($emot)]} {
		$w insert $mark [string range $str $i [expr {$start-1}]] $tags
                set idx [$w index "$mark -1 char"]
		$w image create $mark -image ::tkchat::img::$IMG($emot)
                foreach tg $tags {
                    $w tag add $tg $idx
                }
	    } else {
		$w insert $mark [string range $str $i $end] $tags
	    }
	    set i [expr {$end+1}]
	}
	if {$i <= [string length $str]} {
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
	add	{
            # FRINK: nocheck
            set ${var}($cmd) {}
        }
	remove	{
            # FRINK: nocheck
            catch {unset -- ${var}($cmd)}
        }
	default	{
	    return -code error "unknown hook action \"$type\": must be\
		    add or remove"
	}
    }
}

proc ::tkchat::say { message args } {
    # I've added a few lines to make this speak new messages via the
    # festival synthesiser. It doesn't do it robustly as yet (you'll need
    # festival installed) but as a quick (1min) hack it's got heaps of
    # cool points...  -- Steve Cassidy
    variable festival
    if {![info exists festival]} {
	set festival [open "|festival --pipe" w]
    }

    log::log debug [string map [list "\"" ""] $message]
    puts $festival "(SayText \"$message\")"
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
                  || [findExecutable mozilla	Options(BROWSER)]
                  || [findExecutable mozilla-firefox	Options(BROWSER)]
                  || [findExecutable mozilla-firebird	Options(BROWSER)]
                  || [findExecutable konqueror	Options(BROWSER)]
                  || [findExecutable netscape	Options(BROWSER)]
                  || [findExecutable iexplorer	Options(BROWSER)]
                  || [findExecutable lynx		Options(BROWSER)]
              }
	    # lynx can also output formatted text to a variable
	    # with the -dump option, as a last resort:
	    # set formatted_text [ exec lynx -dump $url ] - PSE
	    #
	    # -remote argument might need formatting as a command
	    # 		Try that first
	    if {[catch {exec $Options(BROWSER) -remote openURL($url) 2> /dev/null}]} {
	        # Try -remote with raw URL argument
	        if {[catch {exec $Options(BROWSER) -remote $url 2> /dev/null}]} {
		    # perhaps browser doesn't understand -remote flag
		    if {[catch {exec $Options(BROWSER) $url &} emsg]} {
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
            foreach app {Firefox {Mozilla Firebird} Mozilla Netscape IExplore} {
                if {[set srv [dde services $app WWW_OpenURL]] != {}} {
                    if {[catch {dde execute $app WWW_OpenURL $url} msg]} {
                        log::log debug "dde exec $app failed: \"$msg\""
                    } else {
                        set handled 1
                        break
                    }
                }
            }

	    # The windows NT shell treats '&' as a special character. Using
	    # a '^' will escape it. See http://wiki.tcl.tk/557 for more info.
            if {! $handled} {
                if {[string equal $tcl_platform(os) "Windows NT"]} {
                    set url [string map {& ^&} $url]
                }
                if {[catch {eval exec [auto_execok start] [list $url] &} emsg]} {
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
    }
    . config -cursor {}
    .txt config -cursor left_ptr
}

proc ::tkchat::formatClock {str} {
    global Options
    set out [stripStr $str]
    if {[regexp -- {^[\s:]*(\d+)} $out -> ticks]} {
        set cmd [list clock format $ticks -gmt $Options(TimeGMT)]
        if {![string equal $Options(TimeFormat) ""]} {
            lappend cmd -format $Options(TimeFormat)
        }
        set out [eval $cmd]
    }
    return $out
}

proc ::tkchat::addAction {clr nick str {mark end} {timestamp 0} {extraOpts ""}} {
    global Options
    checkNick $nick $clr
    checkAlert ACTION $nick $str
    array set opts $extraOpts
    .txt config -state normal
    #for colors, it is better to extract the displayed nick from the one used for
    #tags.
    set displayNick $nick
    regexp {^<(.+)>$} $nick displayNick nick
    ::tkchat::InsertTimestamp .txt $nick $mark $timestamp
    .txt insert $mark "   * $displayNick " [list NICK NICK-$nick]
    if {[string equal $nick clock]} {
        .txt insert $mark "[formatClock $str] " [list NICK-$nick ACTION]
    } else {
	foreach {str url} [parseStr $str] {
	    if { [info exists opts(nolog)] } {
		set tags [list MSG NOLOG-$nick ACTION]
	    } else {
		set tags [list MSG NICK-$nick ACTION]	
	    }
	    if {$url != ""} {
		lappend tags URL URL-[incr ::URLID]
		.txt tag bind URL-$::URLID <1> [list ::tkchat::gotoURL $url]
	    }
	    tkchat::Insert .txt $str $tags $url $mark
	}
    }
    .txt insert $mark "\n" [list NICK-$nick ACTION]

    # Special handling for single dot action message
    if {[string trim $str] == "." && $Options(Username) != $nick} {
        set inspt [.txt index "$mark - 2 line"]
        set endpt [.txt index "$mark - 1 line"]
        .txt tag add SINGLEDOT $inspt $endpt
        .txt tag raise SINGLEDOT NICK-$nick
    }

    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see $mark }
}

proc ::tkchat::addSystem {str {mark end}} {
    global Options
    .txt config -state normal
    ::tkchat::InsertTimestamp .txt "" $mark
    .txt insert $mark "\t$str\n" [list MSG SYSTEM]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see $mark }
}

# Add notification of user entering or leaving. We can hide these notifications
# by setting Options(hideTraffic)
# Always add tehse to text - just tag them so we can elide them at will
# this way, the hide option can affect the past as well as the future
proc ::tkchat::addTraffic {who action {mark end} {timestamp 0} } {
    global Options

    variable ::tkchat::MSGS
    .txt config -state normal
    if {[string equal $who$action "ircbridgeentered"]} {
        set msg "$who was erected"
    } elseif {[string equal $who$action "ircbridgeleft"]} {
        set msg "$who fell down"
    } elseif {[info exists MSGS($action)]} {
        set msg [string map -nocase [list %user% $who] \
                       [lindex $MSGS($action) \
                              [expr {int(rand()*[llength $MSGS($action)])}]]]
    } else {
        set msg "$who has $action the chat!!"
    }
    ::tkchat::InsertTimestamp .txt "" $mark $timestamp
    .txt insert $mark "\t$msg\n" [list MSG SYSTEM TRAFFIC]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see $mark }
}

proc ::tkchat::addUnknown {str} {
    global Options
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
	if {$url == ""} {
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

proc ::tkchat::doIrcBridgeWhisper { clr name str } {
    variable ircOnlineUsers
    set str [string trim $str]
    set what [lindex [split $str ":"] 0]
    switch -- $what {
	onlineusers {
	    set tmp [string range $str [expr [string first : $str]+1] end]
	    set ircOnlineUsers [split [string trim $tmp] " "]
	    #addSystem "Users on IRC: $ircOnlineUsers"
	}
	default {
	    addAction $clr $name " whispers: $str"
	}
    }
}

proc ::tkchat::didIrcBridgeWhisper { clr name str } {
    variable ircOnlineUsers
    global Options

    set str [string trim $str]
    switch -glob -- $str {
	onlineusers* {
	    #addSystem "Asking ircbridge for online users..."
	}
	default {
	    addAction $clr $Options(Username) \
		" whispered to [string range $name 2 end]: $str"
	}
    }
}

proc ::tkchat::addHelp {clr name str} {
    global Options

    if {[lsearch -exact $Options(NickList) $name] >= 0} {
	if { [string equal $name "ircbridge"] } {
	    doIrcBridgeWhisper $clr $name $str
	} else {
	    # this is an incoming private message
	    addAction $clr $name " whispers: $str"
	}
	return
    }
    if {[string match "->*" $name]} {
	if { [string equal $name "->ircbridge"] } {
	    didIrcBridgeWhisper $clr $name $str
	} else {
	    # an outgoing private message
	    addAction $clr $Options(Username) \
		" whispered to [string range $name 2 end]: $str"
	}
	return
    }


    if {[string equal $name "USERINFO"]} {
        set tag USERINFO
    } elseif {[string equal $name "MEMO"]} {
        set tag MEMO
    } elseif {[string equal $name "WELCOME"]} {
        set tag WELCOME
    } elseif {[string equal $name "IP"]} {
        set tag SYSTEM
    } else {
	set tag HELP
    }

    if {![string equal $tag SYSTEM] && $Options(Popup,$tag)} {
	::tkchat::showInfo $tag $str
    }

    if {$clr != ""} {
	.txt tag configure $tag -foreground "#$clr"
    }
    .txt config -state normal
    .txt insert end "$name\t" [list $tag NICK]
    foreach {str url} [::tkchat::parseStr $str] {
	regsub -all "\n" $str "\n\t" str
	if {[string equal $url ""]} {
	    .txt insert end "$str " [list MSG $tag]
	} else {
	    .txt insert end "$str " [list MSG $tag URL URL-[incr ::URLID]]
	    .txt tag bind URL-$::URLID <1> [list ::tkchat::gotoURL $url]
	}
    }
    .txt insert end "\n" [list $tag NICK]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc ::tkchat::createFonts {} {
    font create FNT  -family helvetica -size -12 -weight normal -slant roman
    font create ACT  -family helvetica -size -12 -weight normal -slant italic
    font create NAME -family helvetica -size -12 -weight bold	-slant roman
    font create SYS  -family helvetica -size -12 -weight bold	-slant italic
    font create STAMP -family helvetica -size -12 -weight bold	-slant roman
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

proc ::tkchat::deleteCompletions { } {
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

    switch [llength $matches] {
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
		if { [string equal [lindex $lastCompletion 1] $match] && \
			 [string length $match] > 0 } {
		    .txt config -state normal
		    .txt insert end "Completions: $matches\n" [list MSG NICKCOMPLETE]
		    .txt config -state disabled
		    if {$Options(AutoScroll)} { .txt see end }
		    after 5500 {
			if { [llength $::tkchat::lastCompletion] > 0 } {
			    if { [clock seconds]-4 < [lindex $::tkchat::lastCompletion 0] } {
				return
			    }
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
    if {([string match "as*" $Options(Style)] 
         || [string equal $Options(Style) "any"])
	&& ![catch {package require as::style}]} {
	as::style::init
	set done 1
    }
    if {!$done
	&& ([string match "g*" $Options(Style)] 
            || [string equal $Options(Style) "any"])
	&& [tk_windowingsystem] == "x11"} {
	gtklook_style_init
    }

    wm title . $chatWindowTitle
    wm withdraw .
    wm protocol . WM_DELETE_WINDOW [namespace origin quit]

    catch {createFonts}

    menu .mbar
    . config -menu .mbar

    .mbar add cascade -label "File" -underline 0 \
	-menu [menu .mbar.file -tearoff 0]
    .mbar add cascade -label "Preferences" \
	-underline 0 -menu [menu .mbar.edit -tearoff 0]
    .mbar add cascade -label "Emoticons" \
	-underline 0 -menu [menu .mbar.emot -tearoff 0]
    .mbar add cascade -label "Visibility" \
	-underline 0 -menu [menu .mbar.vis -tearoff 0]
    .mbar add cascade -label "Alerts" \
	-underline 0 -menu [menu .mbar.alert -tearoff 0]
    .mbar add cascade -label "Debug" -underline 0 \
	-menu [menu .mbar.dbg -tearoff 0]
    .mbar add cascade -label "Help" -underline 0 \
	-menu [menu .mbar.help -tearoff 0]

    ## File Menu
    ##
    set m .mbar.file
    $m add checkbutton -label "Pause" \
	-variable ::tkchat::pause \
	-underline 0 \
	-command { ::tkchat::pause $::tkchat::pause }
    $m add command -label "Logout" -underline 0 \
	-command [namespace origin logonScreen]
    $m add command -label "Save Options" -underline 0 \
	-command [namespace origin saveRC]
    $m add separator
    $m add command -label "Open Whiteboard" -underline 5 \
	-command [namespace origin whiteboard_open]
    $m add separator
    $m add command -label "Exit" -underline 1 -command [namespace origin quit]

    ## Preferences/Edit Menu
    ##
    set m .mbar.edit
    $m add checkbutton -label "Display Online Users" -underline 0 \
	-variable Options(DisplayUsers) \
	-command ::tkchat::displayUsers
    $m add command -label "Colors ..." -underline 0 \
	-command tkchat::ChangeColors
    $m add command -label "Macros ..." -underline 0 \
	-command tkchat::EditMacros
    $m add command -label "Font" -underline 0 \
	-command "::tkchat::ChooseFont"
    $m add command -label "User details ..." -underline 0 \
	-command tkchat::UserInfoDialog
    $m add command -label "Options ..." -underline 0 \
	-command ::tkchat::EditOptions

    $m add separator

    if {[package provide tile] != {}} {
        if {[llength [info commands ::tile::availableThemes]] > 0} {
            set themes [lsort [tile::availableThemes]]
        } else {
            set themes [lsort [style theme names]]
        }
	$m add cascade -label "Tk themes" -menu [menu $m.themes -tearoff 0]
	foreach theme $themes {
	    $m.themes add radiobutton -label [string totitle $theme] \
		-variable ::Options(Theme) \
		-value $theme \
		-command [list [namespace origin SetTheme] $theme]
	}
	$m add separator
    }

    $m add cascade -label "Refresh Frequency" \
	-menu [menu $m.refresh -tearoff 0] \
	-underline 0
    foreach s {15 30 45 60} {
	$m.refresh add radiobutton -label "$s seconds" -val $s \
	    -var Options(Refresh)
    }
    $m add cascade -label "Max Window Buffer" \
	-menu [menu $m.buffer -tearoff 0] \
	-underline 3
    foreach l {500 1000 1500 2500 5000 10000} {
	$m.buffer add radiobutton -label "$l lines" -val $l \
	    -var Options(MaxLines) -underline 0
    }
    $m add separator
    $m add cascade -label "Local Chat Logging" \
	-menu [menu $m.chatLog -tearoff 0] \
	-underline 0
    $m.chatLog add radiobutton -label Disabled -var ::Options(ChatLogOff) \
	-val 1 -command {tkchat::OpenChatLog close} -underline 0
    $m.chatLog add command -label "To File..." \
	-command {tkchat::OpenChatLog open} -underline 0
    
    if { $Options(UseJabber) } {
	$m add cascade -label "Server Chat Logging" \
	    -menu [menu $m.chatServLog -tearoff 0] \
	    -underline 0
	$m.chatServLog add radiobutton -label "Log my messages, do not log my actions (old style)" -val oldStyle \
	    -var Options(ServerLogging) -underline 1
	$m.chatServLog add radiobutton -label "Log my messages and actions" -val all \
	    -var Options(ServerLogging) -underline 0
	$m.chatServLog add radiobutton -label "Do not log my messages and actions" -val none \
	    -var Options(ServerLogging) -underline 3	
    }
    
    $m add cascade -label "Loading Server History" \
	-menu [menu $m.hist -tearoff 0] \
	-underline 15
    $m.hist add radiobutton -label "Do NOT load any history" -val 0 \
	-var Options(HistoryLines) -underline 3
    $m.hist add radiobutton -label "Ask me which logs to load" -val -1 \
	-var Options(HistoryLines) -underline 0
    foreach l {50 100 200 500 1000 2500 10000} {
	$m.hist add radiobutton -label "Load at least $l lines" -val $l \
	    -var Options(HistoryLines)
    }
    
    $m add separator
    $m add checkbutton -label "Enable Whiteboard" -underline 0 \
	-variable Options(EnableWhiteboard)

    ## Emoticon Menu
    ##
    set m .mbar.emot
    $m add command -label "Show Emoticons" \
	-command ::tkchat::ShowSmiles -underline 0
    $m add checkbutton -label "Use Emoticons" \
	-onval 1 -offval 0 -var Options(emoticons) \
	-underline 0
    $m add checkbutton -label "Animate Emoticons" \
	-onval 1 -offval 0 -var Options(AnimEmoticons) \
	-command ::tkchat::DoAnim -underline 0
    $m add cascade -label Insert -underline 0 \
	-menu [menu $m.mnu -title Insert]
    variable IMG
    foreach {i e} [array get IMG] {
	set tmp($e) $i
    }
    foreach {img txt} [array get tmp] {
	$m.mnu add command -image ::tkchat::img::$img -command \
	    ".eMsg insert insert \"$txt \" ; .tMsg insert insert \"$txt \""
    }

    ## Visibility Menu
    ##
    set m .mbar.vis
    foreach {tag text} {
	SYSTEM	 "System"
	TRAFFIC	 "Entry/Exit"
	WELCOME	 "Welcome"
	HELP	 "Help"
	USERINFO "User Info"
	MEMO	 "Memo"	
    } {
	$m add checkbutton -label "Hide $text Messages" \
	    -onval 1 -offval 0 \
	    -var Options(Visibility,$tag) \
	    -command "::tkchat::DoVis $tag" \
	    -underline 5
    }
    $m add checkbutton -label "Hide single dot actions" \
	-onval 1 -offval 0 -var Options(Visibility,SINGLEDOT) \
	-command [list ::tkchat::DoVis SINGLEDOT] -underline 12
    $m add checkbutton -label "Hide Timestamps" \
	-onval 1 -offval 0 -var Options(Visibility,STAMP) \
	-command [list ::tkchat::StampVis] -underline 5
    $m add separator
    $m add command -label "Hide All Users" -command  "::tkchat::NickVis 1"
    $m add command -label "Show All Users" -command  "::tkchat::NickVis 0"
    $m add cascade -label "Hide Users" -menu [menu $m.nicks -tearoff 0]
    ::tkchat::NickVisMenu
    $m add separator
    foreach {tag text} {
	HELP	 "Help"
	USERINFO "User Info"
	WELCOME	 "Welcome"
	MEMO	 "Memo"
    } {
	$m add checkbutton -label "Pop-up $text Messages" \
	    -onval 1 -offval 0 \
	    -var Options(Popup,$tag) \
	    -underline 10
    }

    ## Alert Menu
    ##
    set m .mbar.alert
    foreach {tag text} {
	SOUND	  "Beep on alert"
	RAISE	  "Raise to top on alert"
    } {
	$m add checkbutton -label "$text" \
	    -onval 1 -offval 0 \
	    -var Options(Alert,$tag)
    }
    $m add separator
    foreach {tag text} {
	ALL	  "Alert when any message received"
	ME	  "Alert when username mentioned"
	TOPIC	  "Alert when someone speaks who was quiet"
    } {
	$m add checkbutton -label "$text" \
	    -onval 1 -offval 0 \
	    -var Options(Alert,$tag)
    }
    $m add separator
    foreach {tag text} {
	NORMAL	  "Alert on regular posts"
	ACTION	  "Alert on whispers and \"/me\" posts"
    } {
	$m add checkbutton -label "$text" \
	    -onval 1 -offval 0 \
	    -var Options(Alert,$tag)
    }

    ## Debug Menu
    ##
    set m .mbar.dbg
    $m add comman -label "Reload script" -underline 0 \
	-command [list ::tkchat::Debug reload]
    $m add comman -label "Restart script" -underline 2 \
	-command [list ::tkchat::Debug restart]
    $m add comman -label "Retrieve script" -underline 2 \
	-command [list ::tkchat::Debug retrieve]
    $m add comman -label "Evaluate selection" -underline 1 \
	-command [list ::tkchat::Debug evalSel]
    $m add comman -label "Allow remote control" -underline 0 \
	-command [list ::tkchat::Debug server]
    $m add comman -label "Reload history" -underline 7 \
	-command [list ::tkchat::Debug purge]
    $m add separator
    $m add cascade -label "Error Logging" -underline 0 \
	-menu [menu $m.err -tearoff 0]
    $m.err add cascade -label "Log Level" -underline 0 \
	-menu [menu $m.err.lvl -tearoff 0]
    $m.err add radiobutton -label "To Stderr" -underline 3 \
	-var ::Options(LogStderr) -val 1 \
	-command {tkchat::OpenErrorLog stderr}
    $m.err add command -label "To File..." -underline 3 \
	-command {tkchat::OpenErrorLog pick}
    foreach lvl [lsort -command ::log::lvCompare $::log::levels] {
	$m.err.lvl add radiobutton -label $lvl -val $lvl \
	    -var Options(LogLevel)
    }
    $m add separator
    $m add checkbutton -label "Console" -underline 0 \
	-variable ::tkchat::_console \
	-command [list ::tkchat::Debug console] \
	-state disabled
    set ::tkchat::_console 0
    if {[llength [info commands tkcon]]} {
	$m entryconfig "Console" -state normal \
	    -command {
		if {$::tkchat::_console} { tkcon show } else { tkcon hide }
	    }
    } elseif {[string compare "unix" $::tcl_platform(platform)]} {
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
    $m add command -label About... -underline 0 -command tkchat::About
    $m add cascade -label "Translate Selection" -underline 0 \
        -command [list [namespace current]::babelfishMenu]

    # main display
    if {[info command ::panedwindow] != {} && $Options(UsePane)} {
	set UsePane 1
	panedwindow .pane -sashpad 4 -sashrelief ridge
	frame .txtframe
    } else {
	set UsePane 0
    }

    text .txt -background "#[getColor MainBG]" \
	-foreground "#[getColor MainFG]" \
	-font FNT -relief sunken -bd 2 -wrap word \
	-yscroll "::tkchat::scroll_set .sbar" \
	-state disabled -cursor left_ptr -height 1
    scrollbar .sbar -command ".txt yview"
    # user display
    text .names -background "#[getColor MainBG]" \
	-foreground "#[getColor MainFG]" \
	-relief sunken -bd 2 -width 8 -font FNT -state disabled \
	-cursor left_ptr -height 1 -wrap word

    # bottom frame for entry
    frame .btm
    button .ml -text ">>" -width 0 -command ::tkchat::showExtra
    entry .eMsg
    bind .eMsg <Return>	  ::tkchat::userPost
    bind .eMsg <KP_Enter> ::tkchat::userPost
    bind .eMsg <Key-Up>	  ::tkchat::entryUp
    bind .eMsg <Key-Down> ::tkchat::entryDown
    bind .eMsg <Key-Tab>  {::tkchat::nickComplete ; break}
    bind .eMsg <Key-Prior> [list .txt yview scroll -1 pages]
    bind .eMsg <Key-Next>  [list .txt yview scroll  1 pages]
    text .tMsg -height 6 -font FNT
    bind .tMsg <Key-Tab> {::tkchat::nickComplete ; break}
    button .post -text "Post" -command ::tkchat::userPost
    #button .refresh -text "Refresh" -command {pause off}
    menubutton .mb -indicator on -pady 4 \
	-menu .mb.mnu -textvar Options(MsgTo) -direction above
    menu .mb.mnu -tearoff 0
    .mb.mnu add command -label "All Users" \
	-command [list ::tkchat::MsgTo "All Users"]
    .txt tag configure MSG -lmargin2 50
    .txt tag configure INFO -lmargin2 50
    .txt tag configure NICK -font NAME
    .txt tag configure ACTION -font ACT
    .txt tag configure NOLOG -font ACT 
    .txt tag configure SYSTEM -font SYS
    .txt tag configure STAMP -font STAMP
    .txt tag configure URL -underline 1
    .txt tag configure SINGLEDOT -font ACT
    .txt tag bind URL <Enter> [list .txt config -cursor hand2]
    .txt tag bind URL <Leave> [list .txt config -cursor {}]
    .names tag config NICK -font NAME
    .names tag config TITLE -font SYS -justify center
    .names tag config URL -underline 1
    .names tag bind URL <Enter> [list .names config -cursor hand2]
    .names tag bind URL <Leave> [list .names config -cursor {}]
    # on windows, a disabled text widget can't get focus
    # but someone might want to copy/paste the text
    bind .txt <1> { focus %W }
    bind .txt <Up>	   {.txt yview scroll  -1 units}
    bind .txt <Down>	   {.txt yview scroll	1 units}
    bind .txt <Button-4>   {.txt yview scroll  -1 units}
    bind .txt <Button-5>   {.txt yview scroll	1 units}
    bind .txt <Button-3>   {::tkchat::babelfishMenuPost
				[winfo pointerx %W] [winfo pointery %W]}
    bind .txt <Shift-Button-3> {::dict.leo.org::askLEOforSelection}

    bind . <FocusIn> [list [namespace origin ResetMessageCounter]]
    if {[lsearch [wm attributes .] -alpha] != -1} {
        bind Tkchat <FocusIn>  [list [namespace origin FocusInHandler] %W]
        bind Tkchat <FocusOut> [list [namespace origin FocusOutHandler] %W]
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

    wm geometry . $Options(Geometry)
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

proc ::tkchat::SetTheme {theme} {
    global Options
    catch {
        #was: package vsatisfies [package provide tile] 0.4
        if {[llength [info command ::tile::setTheme]] > 0} {
            tile::setTheme $theme
        } else {
            style theme use $theme
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

proc ::tkchat::DoVis {tag} {
    .txt tag config $tag -elide $::Options(Visibility,$tag)
}

proc ::tkchat::NickVis {val} {
    foreach t [array names ::Options Visibility,NICK-*] {
	if {$::Options($t) != $val} {
	    set ::Options($t) $val
	    DoVis [lindex [split $t ,] end]
	}
    }
}

proc ::tkchat::StampVis {} {
    global Options
    set tag STAMP

    .txt tag config $tag -elide $::Options(Visibility,$tag)

    set wid $Options(Offset) 

    if { $::Options(Visibility,$tag) } {
	# Invisible
	.txt config -tabs [list $wid l]
	.txt tag configure MSG -lmargin2 $wid
    } else {
	# Stamps visible
	set wid_tstamp [expr {[font measure NAME "\[88:88\]"] + 5}]
	.txt config -tabs [list $wid_tstamp l [expr {$wid+$wid_tstamp}] l]
	.txt tag configure MSG -lmargin2 [expr {$wid+$wid_tstamp}]
    }

}

proc ::tkchat::NickVisMenu {} {
    set m .mbar.vis.nicks
    $m delete 0 end
    set cnt 0
    foreach n [lsort -dict $::Options(NickList)] {
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
    if {[package provide tile] != {}} {
	set sbcmd tscrollbar
    } else {
	set sbcmd scrollbar
    }

    set parent [winfo parent $w]
    for {set n 0} {[winfo exists $parent.f$n]} {incr n} {}
    set f [frame $parent.f$n]
    set vs [$sbcmd $f.vs -orient vertical -command [list $w yview]]
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
    text $w.text -height 30 -bd 1 -width 100
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
	  \t\t\te.g: /alias foo script addSystem \"test!\"\n" {} \
	"\t\t\t/alias foo proc thisProc\n" {} \
	"\t\t\tproc thisProc { arguments } { addSystem \$arguments }\n" {} \
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
			addSystem "wrong # args: must be /alias name type body"
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
		    addSystem "wrong # args: must be /unalias name"
		    set result 0
		}
	    }
	}
	default {
	    addSystem "unknown alias processing directive"
	    set result 0
	}
    }

    return $result
}

proc ::tkchat::addAlias { name type body } {
    # added by JJM 25/Sep/2003
    variable commandAliases

    set index [findAlias $name]

    if {$index != -1} then {
	# replace existing alias...
	set commandAliases(types) [lreplace $commandAliases(types) $index $index $type]
	set commandAliases(bodies) [lreplace $commandAliases(bodies) $index $index $body]

	# show that we modified it.
	addSystem "alias \"$name\" modified"
    } else {
	# add new alias...
	lappend commandAliases(names) $name
	lappend commandAliases(types) $type
	lappend commandAliases(bodies) $body

	# show that we added it.
	addSystem "alias \"$name\" added"
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
	    addSystem "alias \"$alias\" matching \"$name\" removed"

	    set result 1; # yes, we matched at least one.
	}
    }

    return $result
}

proc ::tkchat::listAliases {} {
    # added by JJM 25/Sep/2003
    variable commandAliases

    addSystem "there are [llength $commandAliases(names)] aliases defined"

    for {set index 0} {$index < [llength $commandAliases(names)]} {incr index} {
	set name [lindex $commandAliases(names) $index]
	set type [lindex $commandAliases(types) $index]
	set body [lindex $commandAliases(bodies) $index]

	if {$type == "proc"} then {
	    # show the whole thing, it's just a proc name.
	    set str $body
	} else {
	    # only show first 80 characters of the script.
	    set str [string range $body 0 79]
	}

	addSystem "alias $name ($type) = \{$str\}"
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
			addSystem "did not get exactly 2 arguments for alias \"$command_name\" ($command_type)"
		    }
		} else {
		    addSystem "could not parse arguments for alias \"$command_name\" ($command_type)"
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
	    addSystem "alias \"$command_name\" ($command_type) error: $alias_error"
	}
    } else {
	set result 0
    }

    return $result
}

proc ::tkchat::userPost {} {
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
                    msgSend $msg
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
		    gotoURL http://purl.org/tcl/wiki/$q
		}
		{^/google\s} {
		    set msg [string range $msg 8 end]
		    log::log debug "Google query \"$msg\""
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
		{^/alias\s?}  -
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
		    if { [string equal $msg "/log"] } {
			# Set the global logging state
			set Options(ServerLogging) all
			addSystem "Your messages will be logged by the server."
		    } else {
			# Send a single message with logging enabled:
			msgSend [string trim [string range $msg 4 end]]
		    }
		}
		{^/nolog\s?} {
		    if { [string equal $msg "/nolog"] } {
			# Set the global logging state
			set Options(ServerLogging) none
			addSystem "Your messages will not be logged by the server."
		    } else {
			# Send a single message without logging:
			tkjabber::msgSend $msg -attrs [list nolog 1]
		    }
		}	
		{^/nick\s?} {
		    if { $Options(UseJabber) } {
			tkjabber::setNick [string range $msg 6 end]
		    }
		}
		{^/topic\s?} {
		    if { $Options(UseJabber) } {
			tkjabber::setTopic [string range $msg 7 end]
		    }
		}
		{^/me\s?} {
		    if { $Options(UseJabber) } {
			switch $Options(ServerLogging) {
			    oldStyle -
			    none {
				tkjabber::msgSend "/nolog$msg" -attrs [list nolog 1]
			    }
			    default {
				tkjabber::msgSend $msg
			    }			    
			}
		    } else {
			msgSend $msg
		    }
		}
		{^/msg\s} {
		    if { $Options(UseJabber) } {
			if { [regexp {^/msg ([^ ]+) (.+)} $msg -> toNick privMsg] } {
			    tkjabber::msgSend $privMsg -user $toNick
			}
		    } else {
			msgSend $msg
		    }
		}
		default	 {
		    if {![checkAlias $msg]} then {
			# might be server command - pass it on			
			if { $Options(UseJabber) } {
			    switch $Options(ServerLogging) {
				none {
				    tkjabber::msgSend "/nolog $msg" -attrs [list nolog 1]
				}				
				default {
				    tkjabber::msgSend $msg
				}			    
			    }
			} else {
			    msgSend $msg
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
	    if {[string equal $Options(MsgTo) "All Users"]} {
		if { $Options(UseJabber) } {
		    switch $Options(ServerLogging) {
			none {
			    tkjabber::msgSend "/nolog $msg" -attrs [list nolog 1]
			}				
			default {
			    tkjabber::msgSend $msg 
			}			    
		    }
		} else {
		    msgSend $msg
		}
	    } else {
		msgSend $msg $Options(MsgTo)
	    }
	}
    }
    .eMsg delete 0 end
    .tMsg delete 1.0 end

    if {$msg != ""} {
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

proc ::tkchat::hideExtra {} {
    grid remove .tMsg
    grid config .eMsg -in .btm -row 0 -column 1 -sticky ew
    .ml config -text ">>" -command ::tkchat::showExtra
    .eMsg delete 0 end
    .eMsg insert end [string trim [.tMsg get 1.0 end]]
}
proc ::tkchat::showExtra {} {
    global Options
    grid remove .eMsg
    grid config .tMsg -in .btm -row 0 -column 1 -sticky ew
    .ml config -text "<<" -command ::tkchat::hideExtra
    .tMsg delete 1.0 end
    .tMsg insert end [.eMsg get]
}
proc ::tkchat::logonScreen {} {
    global Options LOGON
    set have_tls [expr {[package provide tls] != {}}]
    pause on 0
    tkjabber::disconnect    
    if {![winfo exists .logon]} {
	toplevel .logon -class dialog
	wm withdraw .logon
	wm transient .logon .
	wm title .logon "Logon to the Tcl'ers Chat"

        set lf [frame .logon.frame]
	checkbutton .logon.prx -text "Use Proxy" -var Options(UseProxy) \
            -underline 7
	label .logon.lph -text "Proxy Host" -underline 6
	label .logon.lpp -text "Proxy Port" -underline 6
	entry .logon.eph -textvar Options(ProxyHost)
	entry .logon.epp -textvar Options(ProxyPort)
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
	checkbutton .logon.rjabber -text "Use Jabber Server (experimental)" \
              -var Options(UseJabber) 
        frame .logon.fjsrv
	label .logon.ljsrv -text "Jabber server:port" 
	entry .logon.ejsrv -textvar Options(JabberServer)
	entry .logon.ejprt -textvar Options(JabberPort) -width 5
	#checkbutton .logon.rjabberpoll -text "Use Jabber HTTP Polling" \
        #      -var Options(UseJabberPoll) 
        if {$have_tls} {
            checkbutton .logon.rjabberssl -text "Use Jabber SSL" \
                -var Options(UseJabberSSL) -underline 0 \
                -command ::tkjabber::TwiddlePort
        }
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
        bind .logon <Alt-h> {focus .logon.eph}
        bind .logon <Alt-p> {focus .logon.epp}
        bind .logon <Alt-u> {focus .logon.epan}
        bind .logon <Alt-s> {focus .logon.epap}
        bind .logon <Alt-n> {focus .logon.enm}
        bind .logon <Alt-a> {focus .logon.epw}
        bind .logon <Alt-r> {.logon.rpw invoke}
        bind .logon <Alt-c> {.logon.atc invoke}

	trace variable Options(UseProxy)  w [namespace origin optSet]
	trace variable Options(SavePW)    w [namespace origin optSet]
	trace variable Options(UseJabber) w [namespace origin joptSet]

        pack .logon.ejprt -in .logon.fjsrv -side right -fill y
        pack .logon.ejsrv -in .logon.fjsrv -side right -fill both -expand 1

	grid .logon.prx -           -           -in $lf -sticky w -pady 3
	grid  x         .logon.lph  .logon.eph  -in $lf -sticky w -pady 3
	grid  x         .logon.lpp  .logon.epp  -in $lf -sticky w -pady 3
	grid  x         .logon.lpan .logon.epan -in $lf -sticky w -pady 3
	grid  x         .logon.lpap .logon.epap -in $lf -sticky w -pady 3
	grid .logon.lnm .logon.enm  -           -in $lf -sticky ew -pady 5
	grid .logon.lpw .logon.epw  -           -in $lf -sticky ew
	grid x          .logon.rpw  -           -in $lf -sticky w -pady 3
	grid x          .logon.rjabber -        -in $lf -sticky w -pady 3
	grid x        .logon.ljsrv .logon.fjsrv -in $lf -sticky w -pady 3
        if {$have_tls} {
            grid x .logon.rjabberssl -          -in $lf -sticky w -pady 3
        }
	#grid x          .logon.rjabberpoll -    -in $lf -sticky w -pady 3
	grid x          .logon.atc         -    -in $lf -sticky w -pady 3
	grid x          x              .logon.f -in $lf -sticky e -pady 4

        pack $lf -side top -fill both -expand 1
	wm resizable .logon 0 0
        raise .logon
        bind .logon <Return> [list .logon.ok invoke]
        bind .logon <Escape> [list .logon.cn invoke]
    }
    optSet
    joptSet
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
    foreach w {lph lpp eph epp lpan epan lpap epap} {
	.logon.$w config -state $s
    }
    if {$Options(SavePW)} {
	.logon.atc config -state normal
    } else {
	.logon.atc config -state disabled
	set Options(AutoConnect) 0
    }
}

proc ::tkchat::joptSet {args} {
    global Options
    set state [expr {$Options(UseJabber) ? "normal" : "disabled"}]
    set jwidgets {
        .logon.rjabberssl .logon.rjabberpoll
        .logon.ljsrv .logon.ejsrv .logon.ejprt
    }
    foreach w $jwidgets {
        if {[winfo exists $w]} {$w configure -state $state}
    }
}

proc ::tkchat::registerScreen {} {
    global Options
    
    set ::PasswordCheck ""
    pause on 0
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
	addSystem "wrong # args: must be /bug id"
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
	if {$newSearch != "" && ![string equal $newSearch $searchString]} {
	    # new search string differs from the previous, new search!
	    set searchString $newSearch
	    Find .txt $searchString -regexp 1
	    set searchOffset 0
	}
    }

    # do we need to search at all?
    if {$searchString != ""} {
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
	    addSystem "Bummer. Could not find '$searchString'"
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
proc ::tkchat::newColor {w idx} {
    set init "#$::DlgData(Color,$idx,Mine)"
    set tmp [tk_chooseColor \
                   -title "Select Override Color" \
                   -initialcolor $init]
    if {$tmp != ""} {
	set ::DlgData(Color,$idx,Mine) [string range $tmp 1 end]
	$w config -fg $tmp -selectcolor $tmp
    }
}
proc ::tkchat::buildRow {f idx disp} {
    global DlgData
    variable buildRow_seq
    if { ![info exists buildRow_seq] } {
	set buildRow_seq 1
    } else {
	incr buildRow_seq
    }
    set seq $buildRow_seq
    ::tk::label $f.nm$seq -text "$disp" -anchor w -font NAME -padx 0 -pady 0
    ::tk::radiobutton $f.def$seq -text "default" \
          -var DlgData(Color,$idx,Which) \
          -val Web -fg "#$DlgData(Color,$idx,Web)" \
          -selectcolor "#$DlgData(Color,$idx,Web)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    ::tk::radiobutton $f.inv$seq -text "inverted" \
          -var DlgData(Color,$idx,Which) \
          -val Inv -fg "#$DlgData(Color,$idx,Inv)" \
          -selectcolor "#$DlgData(Color,$idx,Inv)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    ::tk::radiobutton $f.ovr$seq -text "custom" \
          -var DlgData(Color,$idx,Which) \
          -val Mine -fg "#$DlgData(Color,$idx,Mine)"\
          -selectcolor  "#$DlgData(Color,$idx,Mine)" \
          -indicatoron 0  -padx 0 -pady 0 -font FNT
    button $f.clr$seq -text "..." -padx 0 -pady 0  -font FNT \
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
        if {$tmp != ""} {
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
    foreach {key str} {Web "All\nDefault" Inv "All\nInverted" Mine "All\nCustom"} {
        button $f.all$key -text $str -padx 0 -pady 0 -font SYS -command \
              [string map [list %val% $key] {
                  foreach idx [array names DlgData *,Which] {
                      set DlgData($idx) %val%
                  }
              }]
    }
    grid x $f.allWeb $f.allInv $f.allMine x -padx 1 -pady 1
    foreach {idx str} {MainBG Background MainFG Foreground SearchBG Searchbackgr} {
        buildRow $f $idx $str
    }
    grid [label $f.online -text "Online Users" -font SYS] - - -
    foreach nick [lsort -dict $Options(OnLineUsers)] {
        if {[info exists DlgData(Color,$nick,Which)]} {
            buildRow $f $nick $nick
        }
    }
    grid [label $f.offline -text "Offline Users" -font SYS] - - -
    foreach nick [lsort -dict $Options(NickList)] {
        if {[lsearch -exact $Options(OnLineUsers) $nick] < 0} {
            buildRow $f $nick $nick
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
	if {$change} {
	    # propagate changes to main data
	    array set Options [array get DlgData]
	    # update colors
            applyColors
	}
    }
    destroy $t
}

proc ::tkchat::applyColors {} {
    global Options
    # update colors
    .txt config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    .names config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    .txt tag configure found -background "#[getColor SearchBG]"
    foreach nk $Options(NickList) {
        set clr [getColor $nk]
        if {$clr == ""} {
            set clr [getColor MainFG]
        }
        if {[catch {
            .txt tag config NICK-$nk -foreground "#$clr"
        } msg]} { log::log debug "applyColors: \"$msg\"" }
    }
}

# Point the Chat log to a new file.
proc ::tkchat::OpenChatLog {opt} {
    global Options
    switch -exact -- $opt {
        close {
            set Options(ChatLogFile) ""
            set Options(ChatLogOff) 1
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
                    fconfigure $f -buffering line
                    set Options(ChatLogFile) $newFileName
                    if {[info exists Options(ChatLogChannel)]} {
                        close $Options(ChatLogChannel)
                    }
                    set Options(ChatLogChannel) $f
                    set Options(ChatLogOff) 0
                } err]} {
                    # Handle file access problems.
                    log::log error $err
                    bgerror $err
                }
            }
        }
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
            log::lvChannelForall $Options(errLog)
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
                    log::lvChannelForall $Options(errLog)
                } err]} {
                    # Handle file access problems.
                    set Options(LogFile) {}
                    set Options(LogStderr) 1
                    set Options(errLog) stderr
                    log::lvChannelForall $Options(errLog)
                    log::log error $err
                    bgerror $err
                }
            }
        }
    }
}

proc ::tkchat::quit {} {
    set q "Are you sure you want to quit?"
    set a [tk_messageBox -type yesno -message $q]
    if {[string equal $a "yes"]} {
	::tkchat::saveRC
	exit
    }
}

proc ::tkchat::saveRC {} {
    global Options
    if {[info exists ::env(HOME)]} {
	set rcfile [file join $::env(HOME) .tkchatrc]
        set Options(Geometry) [wm geometry .]
        if {[winfo exists .pane] && $::Options(DisplayUsers)} {
            set Options(Pane) [.pane sash coord 0]
        }
	array set tmp [array get Options]
	set ignore {
            History FetchTimerID OnlineTimerID FinalList NamesWin
            FetchToken OnlineToken OnLineUsers ProxyPassword ProxyAuth
            URL URL2 URLchk URLlogs errLog ChatLogChannel PaneUsersWidth
	    retryFailedCheckPage
        }
	if {!$tmp(SavePW)} {
	    lappend ignore Password
	}
	foreach idx $ignore {
	    catch {unset tmp($idx)}
	}
	set oplist [list]
	foreach option [lsort [array names tmp]] {
	    lappend oplist [list $option $tmp($option)]
	}
	if {![catch {open $rcfile w 0600} fd]} {
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
    global Options
    $sbar set $f1 $f2
    if {($f1 == 0) && ($f2 == 1)} {
	grid remove $sbar
    } else {
        if {[winfo exists .pane]} {
            grid $sbar -in .txtframe
        } else {
            grid $sbar
        }
    }
    set Options(AutoScroll) [expr {(1.0 - $f2) < 1.0e-6 }]
    #log::log debug "scroll_set: $sbar set $f1 $f2 (AutoScroll:\
    #   $Options(AutoScroll))"
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
            if {[string equal $a yes]} {
                Debug restart
            }
	}
	restart {
	    pause on 0
	    saveRC
	    eval destroy [winfo children .]
	    eval font delete {FNT ACT NAME SYS STAMP}
	    unset ::Options
	    Init
	}
	retrieve {
	    Retrieve
	}
	purge {
	    pause on 0
	    .txt config -state normal
	    .txt delete 1.0 end
	    set ::Options(History) {}
            .txt config -state normal
            catch {::tkchat::LoadHistory}
	    pause off
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
    foreach font {FNT ACT NAME SYS STAMP} {
	font configure $font -family $family -size $size
    }
    return
}

proc ::tkchat::ChangeFont {opt val} {
    set ::Options(Font,$opt) $val
    foreach font {FNT ACT NAME SYS STAMP} {
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
        if {[string equal [tk windowingsystem] "win32"]} {
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
    set ::URLID 0
    # set intial defaults
    set ::tkchat::pause 0
    set ::tkchat::eCURR 0
    set ::tkchat::eHIST ""
    array set Options {
	UseProxy	0
	ProxyHost	""
	ProxyPort	""
	Username	""
	Password	""
	SavePW		0
	Nickname	""
	UseJabber	0
	UseJabberPoll	0
	UseJabberSSL	0
	ServerLogging all
	MyColor		000000
	FetchTimerID	-1
	OnlineTimerID	-1
	AutoConnect	0
	DisplayUsers	1
	Refresh		30
	NickList	{}
	History		{}
	AutoScroll	0
	Geometry	600x500+0+0
        Pane            {520 2}
        UsePane         1
	Font,-family	Helvetica
	Font,-size	-12
	MaxLines	500
	ChatLogFile	""
	LogFile		""
	LogLevel	notice
	errLog		stderr
	emoticons	1
	hideTraffic	0
	TimeFormat	"At the tone, the time is %H:%M on %A %d %b %Y"
	TimeGMT		0
	HistoryLines	-1
	timeout		30000
	Emoticons	1
	AnimEmoticons	0
        Style           {any}
        Theme           {}
        Transparency    100
        AutoFade        0
        AutoFadeLimit   80
	EnableWhiteboard 1
	Popup,USERINFO	1
	Popup,WELCOME	0
	Popup,MEMO	1
	Popup,HELP	1
	Visibility,USERINFO  1
	Visibility,WELCOME   1
	Visibility,MEMO	     1
	Visibility,HELP	     1
	Visibility,SINGLEDOT 0
	Visibility,STAMP     1
	Alert,SOUND	     0
	Alert,RAISE	     1
	Alert,ALL	     0
	Alert,ME	     1
	Alert,TOPIC	     1
	Alert,NORMAL	     1
	Alert,ACTION	     1
        WhisperIndicatorColor #ffe0e0
        UseBabelfish         0
    }
    catch {set Options(BROWSER) $env(BROWSER)}
    set Options(URL)	 $::tkchat::HOST/cgi-bin/chat.cgi
    set Options(URL2)	 $::tkchat::HOST/cgi-bin/chat2.cgi
    set Options(URLchk)	 $::tkchat::HOST/cgi-bin/chatter.cgi
    set Options(URLlogs) $::tkchat::HOST/tchat/logs
    
    set Options(JabberServer) all.tclers.tk	
    set Options(JabberPort) 5222

    foreach {name clr} { MainBG FFFFFF MainFG 000000 SearchBG FF8C44} {
	set Options(Color,$name,Web)   $clr
	set Options(Color,$name,Inv)   [invClr $clr]
	set Options(Color,$name,Mine)  $clr
	set Options(Color,$name,Which) Web
    }
    # attach a trace function to the log level
    trace variable Options(LogLevel) w [namespace origin LogLevelSet]
    LogLevelSet

    # load RC file if it exists
    if {[info exists ::env(HOME)] && \
	      [file readable [set rcfile [file join $::env(HOME) .tkchatrc]]]} {
	catch {source $rcfile}
    }

    # Set the 'Hardcoded' Options:
    set Options(JabberResource) tkchat
    set Options(JabberLogs) "http://tclers.tk/conferences/tcl"
    set Options(JabberConference) tcl@tach.tclers.tk    
    
    set Options(Offset) 50
    catch {unset Options(FetchToken)}
    catch {unset Options(OnlineToken)}
    set Options(History) {}
    set Options(OnLineUsers) {}
    
    # Process command line args
    set nologin 0
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -nologin   { set nologin 1 }
            -style     { set Options(Style) [Pop args 1] }
            -theme     { set Options(Theme) [Pop args 1] }
            -loglevel  { LogLevelSet [Pop args 1] }
            -useragent { set Options(UserAgent) [Pop args 1] }
	    -conference { set Options(JabberConference) [Pop args 1] }
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
    log::lvChannelForall $Options(errLog)

    # Open the ChatLog file for appending.
    if {[string length $Options(ChatLogFile)] > 0} {
	set Options(ChatLogChannel) [open $Options(ChatLogFile) a]
	fconfigure $Options(ChatLogChannel) -buffering line
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
    if {[tk_windowingsystem] == "win32"} {
        WinicoInit
    }
    
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

    #call the (possibly) user defined postload proc:
    tkchatrcPostload

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
    if {$query != ""} {set Query $query}
    if {$Query != $last} {
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
    
    if {$::tcl_platform(platform) != "windows"} {
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
    } msg]} { log::log warning "TRACE ERROR: $msg" }
}

# -------------------------------------------------------------------------

proc ::tkchat::UserInfoDialog {} {
    variable UserInfo
    variable UserInfoBtn

    if {![info exists UserInfo]} {
        UserInfoFetch
        tkwait variable [namespace current]::UserInfo
    }

    set dlg [toplevel .userinfo]
    set f [frame $dlg.f -bd 0]

    foreach {key text} {realname "Real name" email Email country Country \
                            city City age Age url "Homepage URL" \
                            photo_url "Picture URL" icq_uin "ICQ uin"} {
        set l [label $f.l$key -text $text -anchor nw]
        set e [entry $f.e$key \
                   -textvariable [namespace current]::UserInfo($key) \
                   -bd 1 -background white]
        grid configure $l $e -sticky news -padx 1 -pady 1
    }
    set l [label $f.lstuff -text "Anything else" -anchor nw]
    set e [frame $f.estuff -bd 0]
    set et [text $e.text -height 6 -bd 1 -background white]
    set es [scrollbar $e.scroll -bd 1 -command [list $et yview]]
    $et configure -yscrollcommand [list $es set]
    catch {$et insert 0.0 $UserInfo(stuff)}
    grid configure $et $es -sticky news
    grid rowconfigure $e 0 -weight 1
    grid columnconfigure $e 0 -weight 1

    grid configure $l $e -sticky news -padx 1 -pady 1
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 8 -weight 1

    set btns [frame $dlg.buttons -bd 1]
    button $btns.ok -text Save -width 10 \
        -command [list set [namespace current]::UserInfoBtn 1]
    button $btns.cancel -text Cancel -width 10 \
        -command [list set [namespace current]::UserInfoBtn 0]
    pack $btns.cancel $btns.ok -side right

    pack $btns -fill x -side bottom
    pack $f -fill both -expand 1 -side top

    set UserInfoBtn -1
    tkwait variable [namespace current]::UserInfoBtn

    if {$UserInfoBtn == 1} {
        set UserInfo(stuff) [$et get 0.0 end]
        UserInfoSend
    }
    destroy $dlg
    unset UserInfoBtn
    unset UserInfo
}

proc ::tkchat::UserInfoFetch {} {
    global Options
    variable UserInfo
    if {![info exists UserInfo]} {
        set qry [http::formatQuery \
                     action    changeuserinfo \
                     name      $Options(Username) \
                     password  $Options(Password) \
                     color     $Options(MyColor) \
                     updatefrequency 600 \
                     new_msg_on_top 0 \
                     ls        ""]
        set tok [::http::geturl $Options(URL2) \
                     -query [string map {%5f _} $qry] \
                     -headers [buildProxyHeaders] \
                     -command [namespace origin UserInfoDone]]
    }
}

proc ::tkchat::UserInfoDone {tok} {
    variable UserInfo
    log::log debug "UserInfoDone [http::status $tok] ($tok)"
    switch -exact -- [http::status $tok] {
        ok {
            htmlparse::parse -cmd [namespace origin UserInfoParseCallback] \
                [http::data $tok]
        }
        default { }
    }
    http::cleanup $tok
}

proc ::tkchat::UserInfoParseCallback {tag slash param text} {
    variable UserInfo
    set tag [string toupper $tag]
    switch -exact -- $tag {
        INPUT {
            array set params {}
	    foreach {- key value} [regexp -all -inline \
		    {([A-Z]+)="([^""]*)"} $param] {
		set params([string toupper $key]) $value
            }
            if {[info exists params(NAME)]} {
                set UserInfo($params(NAME)) \
			[::htmlparse::mapEscapes $params(VALUE)]
            }
        }
        TEXTAREA {
            if {$slash == {}} {
                set UserInfo(stuff) $text
            }
        }
    }
}

proc ::tkchat::UserInfoSend {} {
    global Options
    variable UserInfo
    set qry [eval [linsert [array get UserInfo] 0 http::formatQuery]]
    set tok [::http::geturl $Options(URL2) \
                 -query [string map {%5f _} $qry] \
                 -headers [buildProxyHeaders] \
                 -command [namespace origin UserInfoSendDone]]
}

proc ::tkchat::UserInfoSendDone {tok} {
    log::log debug "UserInfoSend [http::status $tok] ($tok)"
    if {[http::status $tok] == "ok" && [http::ncode $tok] == 200} {
        #
    } else {
        tk_messageBox -icon warning -title "Warning" \
            -message "Failed to update information on the server."
    }
    http::cleanup $tok
}
# -------------------------------------------------------------------------

# Windows taskbar support.
# At some point I want to support multiple icons for nochat/chat/alert.
#
proc ::tkchat::WinicoInit {} {
    if {![catch {
        package require Winico
    }]} {
        variable TaskbarIcon
        set icofile [file join [file dirname [info script]] tkchat.ico]
        if {[file exists $icofile]} {
            set TaskbarIcon [winico createfrom $icofile]
            winico taskbar add $TaskbarIcon \
                -pos 0 \
                -text [wm title .] \
                -callback [list [namespace origin WinicoCallback] %m %i]
            bind . <Destroy> [namespace origin WinicoCleanup]
            Hook add chat [namespace origin WinicoChatHook]
        }
    }
}

proc ::tkchat::WinicoCleanup {} {
    variable TaskbarIcon
    winico taskbar delete $TaskbarIcon
}

proc ::tkchat::WinicoCallback {msg icn} {
    switch -exact -- $msg {
        WM_LBUTTONDOWN {
            if {[wm state .] == "withdrawn"} {
                wm deiconify .
                ResetMessageCounter
                WinicoChatHook
            } else {
                wm withdraw .
            }
        }
    }
}

proc ::tkchat::WinicoChatHook {} {
    variable MessageCounter
    variable TaskbarIcon
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
    if {$bookmark(last) == "end" || [catch {.txt index $bookmark(last)}]} {
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

    if { [string equal $nick ""] } {
	set cnt 0
	foreach {nick time} [array get noisyUsers] {
	    incr cnt
	    if { $time < [clock seconds] } {
		addSystem "$nick is no longer noisy (timeout expired)"
		unset noisyUsers($nick)
	    } else {
		addSystem "$nick is noisy until [clock format $time -format %H:%M:%S]"
	    }
	}
	if { $cnt == 0 } {
	    addSystem "You don't consider anyone noisy right now"
	}
	return
    }

    if { [info exists noisyUsers($nick)] } {
	if { [string is integer -strict $time] } {
	    switch $time {
		-1 -
		0 {
		    unset noisyUsers($nick)
		    addSystem "$nick is no longer considered noisy."
		}
		default {
		    set noisyUsers($nick) [expr {[clock seconds] + 60*$time}]
		    if { $time > 1 } {
			addSystem "$nick is considered noisy for $time minutes."
		    } else {
			addSystem "$nick is considered noisy for $time minute."
		    }
		}
	    }
	} else {
	    #No time given, remove from noisyUsers
	    unset noisyUsers($nick)
	    addSystem "$nick is no longer considered noisy."
	}
    } else {
	if { ![string is integer -strict $time] } {
	    set time 5
	}
	switch $time {
	    -1 -
	    0 {
		addSystem "$nick not considered noisy at this time."
	    }
	    default {
		set noisyUsers($nick) [expr {[clock seconds] + 60*$time}]
		if { $time > 1 } {
		    addSystem "$nick is considered noisy for $time minutes."
		} else {
		    addSystem "$nick is considered noisy for $time minute."
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
	    addSystem "$nick is no longer considered noisy (timeout expired)."
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

    if {[package vcompare [package provide Tcl] 8.3] == 0} {
        set bf [frame $dlg.bf]
    } else {
        set bf [labelframe $dlg.bf -text "Preferred browser" -padx 1 -pady 1]
    }
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

    if {[package vcompare [package provide Tcl] 8.3] == 0} {
        set sf [frame $dlg.sf]
        set gf [frame $dlg.gf]
    } else {
        set sf [labelframe $dlg.sf -text "Tk style" -padx 1 -pady 1]
        set gf [labelframe $dlg.gf -text "Gimmiks"  -padx 1 -pady 1]
    }
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
    
    if {[catch {package require as::style}]} {
        $sf.as configure -state disabled
    }
    
    grid $sf.m - - - -sticky news
    grid $sf.as $sf.gtk $sf.any $sf.def -sticky news
    grid rowconfigure $bf 0 -weight 1
    grid columnconfigure $bf 0 -weight 1

    # Gimmicks section.
    set gimmicks 0
    if {[lsearch [wm attributes .] -alpha] != -1} {
        set gimmicks 1
        set scale scale
        if {[info command tscale] != {}} {
            set scale tscale
        }
        checkbutton $gf.fade -text "When not active, fade to " -underline 2 \
            -variable ::tkchat::EditOptions(AutoFade)
        spinbox $gf.fadelimit -from 1 -to 100 -width 4 \
            -textvariable ::tkchat::EditOptions(AutoFadeLimit)
        label $gf.pct -text "%"
        label $gf.alabel -text Transparency
        $scale $gf.alpha -from 1 -to 100 -orient horizontal
        $gf.alpha set $EditOptions(Transparency)
        #[expr {int([wm attributes . -alpha] * 100)}]
        $gf.alpha configure -command [namespace origin SetAlpha]

        grid $gf.fade   - $gf.fadelimit $gf.pct x -sticky w
        grid $gf.alabel $gf.alpha - - - -sticky we
        grid configure $gf.alabel -pady {20 0} -sticky w
        grid columnconfigure $gf 4 -weight 1
    }

    button $dlg.ok -text OK \
        -command [list set ::tkchat::EditOptions(Result) 1]
    button $dlg.cancel -text Cancel \
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
    focus $bf.e

    wm resizable $dlg 0 0
    catch {::tk::PlaceWindow $dlg widget .}
    wm deiconify $dlg

    tkwait variable ::tkchat::EditOptions(Result)

    if {$EditOptions(Result) == 1} {
        set Options(BROWSER) $EditOptions(BROWSER)
        foreach property {Style AutoFade AutoFadeLimit} {
            if {![string equal $Options($property) $EditOptions($property)]} {
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

proc gtklook_style_init {} {
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
    set attrs [list xmlns tcl.tk:whiteboard color $::Options(MyColor)]

    set wbitem ".wb.c create line [string map {.0 {}} [$w coords $id]]"

    set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $wbitem]]
    
    $tkjabber::jabber send_message $tkjabber::conference -type groupchat -xlist $xlist
    
    .wb.e selection range 0 end
}

proc tkchat::whiteboard_clear { } {
    set attrs [list xmlns tcl.tk:whiteboard color $::Options(MyColor)]

    set wbitem ".wb.c delete all"

    set xlist [list [wrapper::createtag x -attrlist $attrs -chdata $wbitem]]
    
    $tkjabber::jabber send_message $tkjabber::conference -type groupchat -xlist $xlist
    
    .wb.e selection range 0 end
         
}
 
proc tkchat::whiteboard_open {} {
    
    if { !$::Options(UseJabber) } {
	tk_messageBox -message "The whiteboard only works in jabber mode. Sorry"
	return
    }
    
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
    variable jabber ; if {![info exists jabber]} {set jabber ""}
    variable topic
    variable jhttp ""
    variable muc
    variable nickTries 0 ;# The number of times I tried to solve a nick conflict
    variable baseNick "" ;# used when trying to solve a nick conflict.
    variable roster ""
    variable browser ""
    variable socket ""
    variable conn
    variable myId ""
    variable RunRegistration 0
    variable reconnect 0 ;# set to 1 after a succesful connect.
    
    variable HistoryLines {}
    variable HaveHistory 0
    
    variable conference tcl@tach.tclers.tk

    variable muc_jid_map ;# array with conference-id to user-jid map.  
    variable users ;# 
    variable user_alias

}

# Login:
proc tkjabber::connect { } {
    variable jhttp
    variable jabber
    variable roster
    variable browser    
    variable socket 
    variable conn
    variable reconnect
    variable conference
    global Options
    
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
    }   
    
    if { $Options(UseJabberPoll) } {
	set socket [jlib::http::new $jabber "http://scheffers.net:5280/http-poll" \
		    -usekeys 1 \
		    -command [namespace current]::httpCB]
	openStream
	
    } else {
        if {$Options(UseProxy) && [string length $Options(ProxyHost)] > 0} {
            set socket [ProxyConnect $Options(ProxyHost) $Options(ProxyPort) \
                            $Options(JabberServer) $Options(JabberPort) \
                            $Options(UseJabberSSL)]
        } elseif $Options(UseJabberSSL) {
            set socket [tls::socket $Options(JabberServer) $Options(JabberPort)]
        } else {
            set socket [socket $Options(JabberServer) $Options(JabberPort)]
        }
        $jabber setsockettransport $socket
	openStream
    }
    
    # The next thing which will/should happen is the a call to ConnectProc by
    # jabberlib.        
}

proc tkjabber::disconnect { } {
    variable jhttp
    variable jabber
    variable roster
    variable browser    
    variable socket 
    variable conn
    variable reconnect 
    global Options
    
    set reconnect 0
    
    if { $socket eq "" } {
	return
    }
    
    catch {close $socket}    
    if { [catch {$jabber closestream}] } {
	log::log error "Closestream: $::errorInfo"
    }
    tkchat::addSystem "Disconnected from jabber server."    
    
}

proc tkjabber::openStream {} {
    variable socket
    variable jabber
    global Options
    log::log debug "OPENSTREAM"
    $jabber openstream $Options(JabberServer) -cmd [namespace current]::ConnectProc -socket $socket    
}

proc tkjabber::SendAuth { } {
    # This proc is called by ConnectProc after openstream succeeded.

    global Options    
    variable conn
    variable jabber
    variable myId
       
    set username $Options(Username)
    set password $Options(Password)
    
    #set username tkchat
    #set password tkchat
    set myId [$jabber send_auth $username $Options(JabberResource) [namespace current]::LoginCB \
	-digest [sha1::sha1 $conn(id)$password]]
        
    #tkchat::addSystem "Logged in as $myId"

    update idletasks
    
    # The jabber keep alive packets are a single newline character.    
    after 30000 [list jlib::schedule_keepalive $jabber]
}


# Jabber callback procs - this is where we get messages from.

# The roster stuff...
proc tkjabber::RosterCB {rostName what {jid {}} args} {
    log::log debug "--roster-> what=$what, jid=$jid, args='$args'"
    variable conference
    
    switch -- $what {
	presence {
	    array set p $args
	    set action ""
	    # online/away/offline, etc.
	    set status online
	    if { [info exists p(-show)] } {
		set status $p(-show)
	    }
	    switch -- $p(-type) {
		available {
		    set action entered
		}
		unavailable {
		    set action left
		    set status offline
		}		
	    }
	    if { $jid ne $conference } {
		set tstatus [string map {
		    dnd "do not disturb"
		    xa "away (idle)"
		    chat "I want to chat"
		    away "away"
		} $status]

		tkchat::addSystem "$jid has $action ($tstatus)"
		return
	    }	    
	    
	    # Much more interesting info available in array ...
	    
	    tkchat::addTraffic $p(-resource) $action
	    
	    tkchat::updateJabberNames    
	}
	default {
	    tkchat::addSystem "--roster-> what=$what, jid=$jid, args='$args'"
	}
    }
    
}

# Browse stuff...
proc tkjabber::BrowseCB {browseName type jid xmllist args} {
    tkchat::addSystem "--browse-> browseName=$browseName type=$type, jid=$jid, xmllist='$xmllist', args='$args'"
}
proc tkjabber::BrowseErrorCB {browseName what jid errlist} {
    tkchat::addSystem "--browse-(error)-> what=$what, jid=$jid, errlist='$errlist'"
}

# The jabberlib stuff...
proc tkjabber::ClientCB {jlibName cmd args} {
    log::log debug "ClientCB: jlibName=$jlibName, cmd=$cmd, args='$args'"
    switch -- $cmd {
	connect {
	    tkchat::addSystem "Connection to Jabber Server Established"	    
	}
	disconnect {
	    tkchat::addSystem "Disconnected from server, will try to reconnect in 5 seconds."
	    after 5000 tkjabber::connect
	}
	networkerror {
	    array set x $args
	    tkchat::addSystem "Network error $x(-body), will try to reconnect in 5 seconds."
	    after 5000 tkjabber::connect
	}
	streamerror {
	    array set x $args
	    set type [lindex $x(-errormsg) 0]
	    set message [lindex $x(-errormsg) 1]
	    switch -- $type {
		conflict {
		    tkchat::addSystem $message		    
		}
		default {
		    tkchat::addSystem "ClientCB: $cmd ; args='$args'"		    
		}
	    }
	    disconnect
	}
	default {
	    tkchat::addSystem "ClientCB: jlibName=$jlibName, cmd=$cmd, args='$args'"
	}
    }  
    update idletasks
}
proc tkjabber::IqCB {jlibName type args} {
    log::log debug "|| MyIqCB > type=$type, args=$args"
    tkchat::addSystem "|| MyIqCB > type=$type, args=$args"
}
proc tkjabber::MsgCB {jlibName type args} {    
    variable conference
    variable topic
    
    log::log debug "|| MsgCB > type=$type, args=$args"
    
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
		"tcl.tk:tkchat" {
		    array set tkchatAttr [wrapper::getattrlist $x]		    
		    set color [wrapper::getattribute $x color]
		}
		"tcl.tk:whiteboard" {		    
		    tkchat::whiteboard_eval [wrapper::getcdata $x] [wrapper::getattribute $x color]
		    return
		}
	    }		    
	}
    }	    

    switch -- $type {
	chat {
	    set from $m(-from)
            if { [regexp {([^/]+)/(.+)} $m(-from) -> conf name] } {
		if { $conf eq $conference } {
		    tkchat::addAction "" $name " whispers: $m(-body)" end $ts
		} else {
		    tkchat::addAction "" $from " whispers: $m(-body)" end $ts
		}
	    } else {
		tkchat::addAction "" $from " whispers: $m(-body)" end $ts
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
			tkchat::addSystem $m(-body)
		    } else {
			tkchat::addAction $color $from " changed the topic to: $m(-subject)\n ... $m(-body)" end $ts
		    }
	    	} else {
		    tkchat::addAction $color $from " changed the topic to: $m(-subject)" end $ts
		}		
	    } else {		
		if { [info exists m(-body)] > 0 } {
		    set opts {}
		    if { [string match "ijchain*" $from] } {
			set pos [string first " " $m(-body)]
			set from [string trim [string range $m(-body) 0 $pos]]
			incr pos
			set m(-body) [string range $m(-body) $pos end]
			if { $from eq "<azbridge>" } {
			    set pos [string first " " $m(-body)]
			    set from "[string trim [string range $m(-body) 0 $pos]]"
			    incr pos
			    set m(-body) [string range $m(-body) $pos end]
			}
			if { $from eq "*" } {
			    set pos [string first " " $m(-body)]
			    set from "<[string trim [string range $m(-body) 0 $pos]]>"
			    incr pos
			    set m(-body) "/me [string range $m(-body) $pos end]"				
			} 			
	    	    }
		    if { [string match "/nolog*" $m(-body)] } {
			set m(-body) [string trim [string range $m(-body) 6 end]]
			lappend opts nolog 1
		    } elseif { [info exists tkchatAttr(nolog)] && $tkchatAttr(nolog) } {
			lappend opts nolog 1
		    }
		    if { [string range $m(-body) 0 3] eq "/me " } {
			tkchat::addAction $color $from [string range $m(-body) 4 end] end $ts $opts
		    } else {		    
			tkchat::addMessage $color $from $m(-body) end $ts $opts
		    }
		} else {
		    #tkchat::addSystem "Got a message I do not understand from $from:\n$args"
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
		tkchat::addAction "" $from " whispers: [join $msg \n]" 		
	    } else {
		tkchat::addSystem "Got a message I do not understand from $from:\n$args"
	    }	    
	    #tkchat::addMessage ""  "Subject: $m(-subject)\n$m(-body)" end $ts
	}
	error {
	    if { [info exists m(-error)] } {
		switch -- [lindex $m(-error) 0] {
		    405 {
			tkchat::addSystem "$m(-from): [lindex $m(-error) 1]. Trying to get in again..."
			$::tkjabber::muc enter $::tkjabber::conference $::Options(Nickname) -command [namespace current]::MucEnterCB
		    }
		    default {
			tkchat::addSystem  "MsgCB (error) args='$args'"
		    }
		}
	    }	    
	}
	#get {
	    if { [info exists m(-query)] } {
		log::log debug "Jabber query\n$args"
		array set iq $m(-query)		
	    } else {
		tkchat::addSystem "|| MsgCB > type=$type, args=$args"
	    }
	}
	default {
	    tkchat::addSystem "|| MsgCB > type=$type, args=$args"
	}
    }   
}
proc tkjabber::PresCB {jlibName type args} {
    log::log debug "|| PresCB > type=$type, args=$args"
    tkchat::addSystem "|| MyPresCB > type=$type, args=$args"
}
proc tkjabber::ConnectProc {jlibName args} {
    variable conn
    log::log debug "ConnectProc args '$args'"

    array set conn $args
    tkchat::addSystem "Connected to $conn(from), sending credentials."
    update idletasks

    SendAuth
    
}

proc tkjabber::httpCB { status message } {
    log::log debug "jabber-http $status : $message"
}

proc tkjabber::RegisterCB {jlibName type theQuery} {
    log::log debug "RegisterCB: type=$type, theQuery='$theQuery'"
    
    switch -- $type {
	result {
	    tkchat::addSystem "Registered."
	    update idletasks
	    SendAuth
	}
	default {
	    tkchat::addSystem "MyRegisterProc: type=$type, theQuery='$theQuery'"
	}
    }
}

proc tkjabber::LoginCB {jlibname type theQuery} {
    variable jabber
    variable roster
    variable conference
    variable muc
    variable baseNick
    variable nickTries
    
    global Options
    log::log debug "LoginCB: type=$type, theQuery='$theQuery'"
    
    #set conference tcl@conference.kroc.tk
    switch -- $type {
	error {
	    if { [lindex $theQuery 0] eq "not-authorized" || \
		$theQuery eq "{} {}" } {
		if { ![tkchat::registerScreen] } {
		    return
		}
		
		$jabber register_set $Options(Username) $Options(Password) [namespace current]::RegisterCB  \
		  -name $Options(Fullname) -email $Options(Email)
		
		tkchat::addSystem "Registering username."
		update idletasks
	
	    } else {
		tkchat::addSystem "LoginCB: type=$type, theQuery='$theQuery'"
	    }
		
	}
	result {
	    tkchat::addSystem "Logged in."
	    set tkjabber::reconnect 1
	    $jabber send_presence -type available    
	    set muc [jlib::muc::new $jabber]
	    if { $::Options(Nickname) eq "" } {
		set ::Options(Nickname) $::Options(Username)
	    }
	    set baseNick $::Options(Nickname)
	    set nickTries 0
	    $muc enter $conference $::Options(Nickname) -command [namespace current]::MucEnterCB
	}
	default {
	    tkchat::addSystem "LoginCB: type=$type, theQuery='$theQuery'"
	}
    }
    return

    
    $jabber conference get_enter $conference [namespace current]::GenericIQProc
    set subelements [list [wrapper::createtag {nick} -chdata tkchat]]
    $jabber conference set_enter $conference $subelements [namespace current]::GenericIQProc
    
    tkchat::addSystem "MyLoginProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::SearchGetProc {jlibName type theQuery} {
    tkchat::addSystem "MySearchGetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::SearchSetProc {jlibName type theQuery} {
    tkchat::addSystem "MySearchSetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::RosterResultProc {jlibName what} {
    tkchat::addSystem  "MyRosterResultProc: what=$what"
}
proc tkjabber::VCardSetProc {jlibName type theQuery} {
    tkchat::addSystem  "VCardSetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::VCardGetProc {jlibName type theQuery} {
    tkchat::addSystem  "VCardGetProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::GenericIQProc {jlibName type theQuery} {
    tkchat::addSystem  "GenericIQProc: type=$type, theQuery='$theQuery'"
}
proc tkjabber::MucEnterCB {mucName type args} {
    variable conference
    variable muc
    variable nickTries
    variable baseNick
    
    log::log debug "MucEnter: type=$type, args='$args'"
    switch -- $type {
	error {
	    array set m $args
	    if { [info exists m(-error)] } {
		switch -- [lindex $m(-error) 0] {
		    401 {
			tkchat::addSystem "This conference is password protected."
		    }
		    403 {
			tkchat::addSystem "You have been banned from this conference."
		    }
		    404 {
			tkchat::addSystem "This room does not exist."
		    }
		    405 {
			tkchat::addSystem "The maximum number of users has been reached for this room."
		    }
		    407 {
			tkchat::addSystem "You must be a member to enter this conference."
		    }		    
		    409 {
			# Nick conflict. Try again?
			if { $nickTries > 5 } {
    			    tkchat::addSystem  "Unable to solve nick conflict, try setting one with /nick <nickname> and then trying again"
			}
			if { $nickTries < 2 } {
			    append ::Options(Nickname) _
			} else {
			    set ::Options(Nickname) $baseNick$nickTries			    
			}
			$muc enter $conference $::Options(Nickname) -command [namespace current]::MucEnterCB
		    }
		    default {
			tkchat::addSystem  "MucEnter: type=$type, args='$args'"
		    }
		}
	    }
	}
	available {
	    #tkchat::addSystem  "MucEnter: type=$type, args='$args'"
	    #only load history when it is not loaded already.
	    if { !$tkjabber::HaveHistory } {
	    # Force history loading to the background
		after 10 tkchat::LoadHistory
	    }
	}
	default {
	    tkchat::addSystem  "MucEnter: type=$type, args='$args'"
	}
    }
    
}

proc ::tkjabber::userinfo {nick} {
    variable jabber
    variable conference

    if {[string match "/userinfo *" $nick]} {
        set nick [string range $nick 10 end]
    }
    log::log debug "userinfo for \"$nick\""
    jlib::vcard_get $jabber $conference/$nick [namespace current]::VCardGetProc
}

proc tkjabber::msgSend { msg args } {
    variable jabber
    variable roster
    variable conference
    set users {}
    
    array set opts {
	-user {}
	-xlist {}
	-attrs {}
    }

    if { [string match "/userinfo *" $msg] } {
        userinfo $msg
	return
    }

    if { [llength $args] > 0 } {
	array set opts $args
    }
    
    set user $opts(-user)

    if { $user eq "" } {
	set user $conference
	set type groupchat
    } else {
	# lookup the real nick
	set found 0
	set type chat
        foreach person [$::tkjabber::muc participants $::tkjabber::conference] {
	    regexp {([^/])/(.+)} $person -> conf name
	    if { $name eq $user } {
		set user $person
		set found 1
		::tkchat::addAction "" $::Options(Username) \
		    " whispered to $name: $msg"		
		break
	    }
	}
	if {!$found } {	    
	    log::log debug "Seaching roster. '$roster' [$roster getname $user] / [$roster getrosteritem $user/tkabber]"
	    
	    foreach presence [$roster getpresence $user] {
		array set pres $presence
		if { $pres(-resource) ne {} && $pres(-type) eq "available" } {
		    log::log debug "Roster user: $user/$pres(-resource)"
	    	    lappend users $user/$pres(-resource)
    		    incr found 
		    ::tkchat::addAction "" $::Options(Username) \
		        " whispered to $user/$pres(-resource): $msg"		
		}
		unset pres
	    }
	    
	}
	if { !$found } {	    
	    ::tkchat::addSystem "Unknown nick name '$user'"
	    return
	}    
    }
    if { [llength $users] == 0 } {
	set users $user
    }
    
    # Example usage
    #set x [wrapper::createtag x -attrlist {xmlns tcl.tk:tkchat} \
	    -subtags [list [wrapper::createtag color -attrlist {attr1 val1 attr2 val2} -chdata $::Options(MyColor)]]]

    set attrs [concat $opts(-attrs) [list xmlns tcl.tk:tkchat color $::Options(MyColor)]]

    set xlist [concat [list [wrapper::createtag x -attrlist $attrs]] $opts(-xlist)]
    
    foreach user $users {
	$jabber send_message $user -body $msg -type $type -xlist $xlist
    }
    #-xlist [wrapper::createtag x -attrlist {xmlns http://tcl.tk/tkchat foo bar}]
    
}

proc ::tkchat::updateJabberNames { } {
    global Options

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
    foreach person [$::tkjabber::muc participants $::tkjabber::conference] {
	regexp {([^/])/(.+)} $person -> conf name 
	
	lappend Options(OnLineUsers) $name
	# NOTE : the URL's don't work because of the & in them
	# doesn't work well when we exec the call to browsers
	# and if we follow spec and escape them with %26 then
	# the cgi script on the other end pukes so we will
	# just do an inline /userinfo when they are clicked
	.names insert end "$name" [list NICK URL URL-[incr ::URLID]] "\n"
	.names tag bind URL-$::URLID <1> \
	    "set ::tkchat::UserClicked 1;\
               [list ::tkchat::msgSend "/userinfo $name"]"
	incr i
	.mb.mnu add command -label $name \
	    -command [list ::tkchat::MsgTo $name]
    }

    .names insert 1.0 "$i Users Online\n\n" TITLE
    .names configure -yscrollcommand $scrollcmd
    .names config -state disabled
}

proc ::tkjabber::setNick { newnick } {
    variable muc
    variable conference
    
    if {[lsearch -exact $::Options(OnLineUsers) $newnick] > -1 } {
	tkchat::addSystem "The nickname '$newnick' is already in use."
	return
    }
    
    # There is a race condition here. new nick could enter between the check
    # and the setnick call...
    set ::Options(Nickname) $newnick
    $muc setnick $conference $newnick
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
	log::log warning "Log incorrect log format."
    }   
    log::log debug "[clock format $time] $nick :: $msg"
}

proc ::tkjabber::HistoryLines { } {
    variable HistoryLines
    return [llength $HistoryLines]
}

proc ::tkjabber::LoadHistoryLines {} {
    global Options
    variable HistoryLines

    set state [.txt cget -state]
    .txt configure -state normal

    log::log debug tkjabber-LoadHistoryLines

    # mask the alerts
    set alerts [array get Options Alert,*]
    foreach {alert value} $alerts { set Options($alert) 0 }

    if {![info exists Options(FinalList)]} {set Options(FinalList) {}}

    set count 0
    foreach entry $HistoryLines {
	set time [lindex $entry 0]
	set nick [lindex $entry 1]
	set msg [lindex $entry 2]
	
	if { [string match "ijchain*" $nick] } {
	    set pos [string first " " $msg]
	    set nick [string trim [string range $msg 0 $pos]]
	    incr pos
	    set msg [string range $msg $pos end]
	    if { $nick eq "<azbridge>" } {
		set pos [string first " " $msg]
		set nick "[string trim [string range $msg 0 $pos]]"
		incr pos
		set msg [string range $msg $pos end]
	    }
	    if { $nick eq "*" } {
		set pos [string first " " $msg]
		set nick "<[string trim [string range $msg 0 $pos]]>"
		incr pos
		set msg "/me [string range $msg $pos end]"				
	    }
	}
	
	if { [string equal $nick ""] && [string match "* has left" $msg] } {
	    tkchat::addTraffic [lindex [split $msg] 0] left HISTORY $time
	} elseif {[string equal $nick ""] && [string match "* has become available" $msg] } {
	    tkchat::addTraffic [lindex [split $msg] 0] entered HISTORY $time
	} elseif { [string match "/me *" $msg] } {
	    tkchat::addAction "" $nick [string range $msg 4 end] HISTORY $time
	} else {
            tkchat::addMessage "" $nick $msg HISTORY $time
	}
        incr count 
        if {$count > 35 } { break }
    }
    .txt see end
    set HistoryLines [lrange $HistoryLines $count end]

    # Restore the alerts
    array set Options $alerts

    if {$HistoryLines == {}} {
        log::log debug "History loading completed."
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
    if {$Options(UseJabberSSL) && $Options(JabberPort) == 5222} {
        incr Options(JabberPort)
    } elseif {!$Options(UseJabberSSL) && $Options(JabberPort) == 5223} {
        incr Options(JabberPort) -1
    }
}

# -------------------------------------------------------------------------

proc tkjabber::ProxyConnect {proxyserver proxyport jabberserver jabberport ssl} {

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
        if {$ssl} { tls::import $sock }
    } else {
        error "proxy connect failed: $block"
    }
    return $sock
}

# -------------------------------------------------------------------------

if {![info exists ::URLID]} {
    eval [linsert $argv 0 ::tkchat::Init]
}
