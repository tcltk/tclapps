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

package require http 2          ; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib
package require base64		; # tcllib
package require Tk 8.3		; # core Tk
# We need Tk 8.3.2 to get -state options for [label]s
if {![catch {package vcompare $tk_patchLevel $tk_patchLevel}]} {
    if {![package vsatisfies $tk_patchLevel 8.3.2]} {
	return -code error "Tk version 8.3.2 or better is required."
    }
}

package forget app-tkchat	;# Workaround until I can convince people
				;# that apps are not packages.  :)  DGP
package provide app-tkchat [regexp -inline {\d+\.\d+} {$Revision: 1.100 $}]

namespace eval ::tkchat {
    # Everything will eventually be namespaced
    variable MessageHooks
    array set MessageHooks {}

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://mini.net

    variable HEADUrl {http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/tcllib/tclapps/apps/tkchat/tkchat.tcl?rev=HEAD}
    variable rcsid   {$Id: tkchat.tcl,v 1.100 2003/07/27 18:43:21 patthoyts Exp $}

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
                         ]

    # Variables to control the search function.
    variable searchString ""
    variable searchOffset end

    # a variable to support nickname completion
    variable lastCompletion "" 
}

set ::DEBUG 1
proc vputs {args} {
    if {$::DEBUG} {
	set name [lindex [info level -1] 0]
	if {[llength $args]} {
	    log::log debug "$name: $args"
	} else {
	    log::log debug "CALLED $name"
	}
    }
}

proc errLog {args} {
    log::logMsg [join $args]
    update idletasks
}

# trace handler to set the log level whenever Options(LogLevel) is changed
# enable the selected level and above
proc ::tkchat::LogLevelSet {args} {
    global Options
    log::lvSuppressLE emergency 0         ;# unsuppress all
    log::lvSuppressLE $Options(LogLevel)  ;# suppress all below selected
    log::lvSuppress $Options(LogLevel) 0  ;# unsupress selected
}

# If Proxy Authentication was specified then each HTTP request
# must have an authentication header. This procedure supports
# proxys accepting Basic authentication by building the header
# required from the users login and password.
#  - PT
proc buildProxyHeaders {} {
    global Options
    set auth {}
    if { $Options(UseProxy) \
               && [info exists Options(ProxyUsername)] \
               && $Options(ProxyUsername) != {}
     } then {
	set auth [list "Proxy-Authorization" \
                        [concat "Basic" \
                               [base64::encode \
                                      $Options(ProxyUsername):$Options(ProxyPassword)]]]
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
	set code [catch {
	    if {[::http::status $token] == "ok"} {
		set fid [open $file w]
		fconfigure $fid -translation binary
		set data [::http::data $token]
		puts -nonewline $fid $data
		close $fid
		regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $data -> rcsVersion
	    }
	} err]
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
proc checkForRedirection {tok optionName} {
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
    set url "$Options(URLlogs)/?M=D"
    set tok [::http::geturl $url \
                   -headers [buildProxyHeaders]]
    errLog "History Fetch: status was\
	    [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
        ok {
            if {[::http::ncode $tok] == 500} {
                set msg "History Fetch error: [::http::code $tok]"
                log::log error $msg
                tk_messageBox -message $msg
            } else {
                if { [checkForRedirection $tok URLlogs] } {
                    # trim off the /?M=D part
                    regsub {/\?M=D} $Options(URLlogs) {} Options(URLlogs)
                    set loglist [::tkchat::GetHistLogIdx ary]
                } else {
                    set RE {<A HREF="([0-9-]+\.txt)">.*\s([0-9]+k)}
                    foreach line [split [::http::data $tok] \n] {
                        if { [regexp  -- $RE $line -> logname size] } {
                            set loglist [linsert $loglist 0 $logname]
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
    return $loglist
}
proc ::tkchat::ParseHistLog {log} {
    global Options
    
    set retList {}
    set MsgRE {^\s*(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun).+?\[([^\]]+)\]\s+([^:]+):?\s*(.*)$}
    # fetch log
    set url "$Options(URLlogs)/$log"
    log::log info "History: Fetch log \"$url\""
    set tok [::http::geturl $url \
                   -headers [buildProxyHeaders]]
    
    errLog "History: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
        ok {
            set logdata [split  [::http::data $tok] \n]
            set lastnick ""
            set lastdata ""
            foreach line $logdata {
                log::log debug "History Parse: $line"
                if {[regexp -- $MsgRE $line -> type nick data]} {
                    if {[string length $lastnick]>0} {
                        lappend retList $lastnick $lastdata
                    }
                    # only show messages - don't care about enter/exit/new user crap
                    if {[string equal $type MSG]} {
                        set lastnick $nick
                        set lastdata $data
                    } else {
                        set lastnick ""
                        set lastdata ""
                    }
                } else {
                    append lastdata "\n$line"
                }
            }
            if {[string length $lastnick]>0} {
                lappend retList $lastnick $lastdata
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
    babelfishInit

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
	    grab $t
	    tkwait window $t
	    foreach log [lrange $loglist $HistQueryNum end] {
		if {[catch {ParseHistLog $log} new]} {
		    puts "ERROR: $new"
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
	    if {[catch {ParseHistLog $log} new]} {
		puts "ERROR: $new"
	    } else {
		set FinalList [concat $new $FinalList]
	    }
	    if {[expr {[llength $FinalList]/2}] >= $Options(HistoryLines)} {
		break
	    }
	}
    }

    foreach {nick msg} $FinalList {
        addMessage "" $nick $msg
    }
    .txt config -state normal
    .txt insert end "+++++++++++++++++++++ End Of History +++++++++++++++++++++\n"
    .txt config -state normal
    .txt see end
}

proc msgSend {str {user ""}} {
    global Options
    errLog "Send to $Options(URL)"
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
              -command msgDone
    } msg]} {
        set delay [expr {$Options(Refresh) * 1000 / 2}]
        errLog "Retrying msgSend after $delay: $msg"
        after $delay [list msgSend $str $user]
    }
}

proc msgDone {tok} {
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
                                      -command msgDone]
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
    if { ! $retry } {
        if {[catch {LoadHistory} err]} { errLog $err }
    }
    if {0} {
        # use when testing only - allows restarts without actually logging in again
        catch {pause off}
        return
    }
    
    global Options
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

proc ::tkchat::logonDone {tok} {
    errLog "Logon: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL2]} {
                ::http::cleanup $tok
                logonChat 1
                return
            }
            
            if {[::http::ncode $tok] >= 500} {
                tk_messageBox -message "Logon failure: [::http::code $tok]"
                pause 1 0
            } else {
                if {[catch {pause off} err]} { errLog $err }
                ::tkchat::DoAnim
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

proc pause {pause {notify 1}} {
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
                      -command { pause off ; wm withdraw .pause }
		pack .pause.r -padx 5 -pady 10
		bind .pause <Destroy> [list pause off]
	    }
	    catch {::tk::PlaceWindow .pause widget .}
	    wm deiconify .pause
	    raise .pause
	}
    } else {
	fetchPage
	onlinePage
    }
}

proc fetchPage {} {
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
                                       -command fetchDone]
    } msg]} {
        # If the http connection failed and we caught it then we probably
        # are not connected to the network. Keep trying - maybe we are moving
        # our laptop or something :)
        errLog "Fetch error: $msg"
        if {!$::tkchat::pause} {
            set Options(FetchTimerID) \
                  [after [expr {$Options(Refresh) * 1000}] fetchPage]
        }
    }
}

proc fetchDone {tok} {
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
	set Options(FetchTimerID) \
              [after [expr {$Options(Refresh) * 1000}] fetchPage]
    }
    errLog "Fetch: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                fetchPage
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

proc onlinePage {} {
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
                                        -command onlineDone]
    } msg]} {
        errLog "Fetch error: $msg"
        if {!$::tkchat::pause} {
            set Options(FetchTimerID) \
                  [after [expr {$Options(Refresh) * 1000}] onlinePage]
        }
    }
}

proc onlineDone {tok} {
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
    if {!$::tkchat::pause} {
	set Options(OnlineTimerID) \
              [after [expr {$Options(Refresh) * 1000}] onlinePage]
    }
    errLog "Online: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                onlinePage
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

# -------------------------------------------------------------------------
# Translate the selection using Babelfish.
# -------------------------------------------------------------------------

proc tkchat::fetchurldone {cmd tok} {
    errLog "fetchurl: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            $cmd $tok
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

proc tkchat::translateSel {from to} {
    if {![catch {selection get} msg]} {
        log::log debug "translate: $from $to \"$msg\""
        translate $from $to $msg
    }
}

proc tkchat::translate {from to text} {
    set url {http://babelfish.altavista.com/babelfish/tr}
    append op $from _ $to
    set query [http::formatQuery tt urltext urltext $text lp $op]
    set tok [http::geturl $url \
             -query $query \
             -headers [buildProxyHeaders] \
             -command [list ::tkchat::fetchurldone ::tkchat::translateDone]]
}

proc tkchat::translateDone {tok} {
    set ::tkchat::translate [http::data $tok]
    set r [regexp {<Div.*?>(.*)</div>} \
            [::http::data $tok] -> text]
    set text [string trim $text]
    log::log debug "Translate: \"$text\""
    if {$r} {
        addSystem "TR: $text"
    } else {
        errLog "Translation returned no matching data."
    }
}

proc tkchat::babelfishInit {{url http://babelfish.altavista.com/babelfish/}} {
    set tok [http::geturl $url \
             -headers [buildProxyHeaders] \
             -command [list ::tkchat::fetchurldone \
                            ::tkchat::babelfishInitDone]]
}

proc tkchat::babelfishInitDone {tok} {
    set ::tkchat::babelfish [http::data $tok]
    if {[regexp {<select name="lp">(.*?)</select>} [::http::data $tok] -> r]} {
        .mbar.help.tr delete 0 end
        set lst [split [string trim $r] \n]
        foreach option $lst {
            regexp {<option value="(.*?)"[^>]*>(.*?)</option>} \
                    $option -> value label
            set value [split $value _]
            log::log debug "option: $label $value"
            .mbar.help.tr add command -label $label \
                    -command [concat [namespace current]::translateSel $value]
        }
    } else {
        log::log debug "babelfish received no data"
    }
}

# -------------------------------------------------------------------------

proc updateNames {rawHTML} {
    global Options

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
          -command [list set Options(MsgTo) "All Users"]
    set Options(OnLineUsers) {}
    foreach {full url name} [regexp -nocase -all -inline -- $exp $rawHTML] {
        lappend Options(OnLineUsers) $name
	# NOTE : the URL's don't work because of the & in them
	# doesn't work well when we exec the call to browsers
	# and if we follow spec and escape them with %26 then
	# the cgi script on the other end pukes so we will
	# just do an inline /userinfo when they are clicked
	.names insert end "$name" [list NICK URL URL-[incr ::URLID]] "\n"
	.names tag bind URL-$::URLID <1> \
              "set ::UserClicked 1 ; [list msgSend "/userinfo $name"]"
	incr i
	.mb.mnu add command -label $name \
              -command [list set Options(MsgTo) $name]
    }
    .names insert 1.0 "$i Users Online\n\n" TITLE
    .names config -state disabled
}

proc invClr {clr {grays 0}} {
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

proc getColor {name} {
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

proc parseData {rawHTML} {
    global Options
    # get body of data
    set clr ""
    if {[regexp -nocase -- \
               {<BODY.*?(?:BGColor=.([[:xdigit:]]{6}?))?>(.*?)<A\s+NAME="end">.*?</BODY>} \
               $rawHTML -> clr body]} {
	if {[string length $clr] && \
                  [string compare $Options(Color,MainBG,Web) $clr]} {
	    set iclr [invClr $clr]
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

proc getRecentLines {input} {
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

set UserClicked 0
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
}
proc addNewLines {input} {
    global Options RE UserClicked

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
                        set UserInfoCmd [list addHelp $helpColor $helpName [join $helpLines \n]]
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
	    } elseif {[regexp -nocase -- $RE(Message) $line -> nick str]} {
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

proc stripStr {str} {
    # remove any remaining tags
    regsub -all -nocase "<.*?>" $str {} tmp
    # replace html escapes with real chars
    return [::htmlparse::mapEscapes $tmp]
}

proc parseStr {str} {
    # get href info return list of str link pairs
    set sList {}
    while {[regexp -nocase -- {^(.*?)<A.*?HREF="(.+?)".*?>(.*?)</A>(.*?)$} \
                  $str -> pre url link post]} {
	if {[string length $pre]} {
	    lappend sList [stripStr $pre] ""
	}
	lappend sList [stripStr $link] $url
	set str $post
    }
    if {[string length $str]} {
	lappend sList [stripStr $str] ""
    }
    return $sList
}

proc checkNick {nick clr} {
    global Options
    set wid [expr {[font measure NAME $nick] + 10}]
    if {$wid > $Options(Offset)} {
        set Options(Offset) $wid
        .txt config -tabs [list $wid l]
        .txt tag configure MSG -lmargin2 $wid
    }
    if {"$clr" == ""} {
        set clr [getColor $nick]
        if {"$clr" == ""} {
            set clr [getColor MainFG]
        }
    }
    if {[lsearch $Options(NickList) $nick] < 0} {
        lappend Options(NickList) $nick
        set Options(Color,$nick,Web) $clr
        set Options(Color,$nick,Inv) [invClr $clr]
        set Options(Color,$nick,Mine) $clr
        set Options(Color,$nick,Which) Web
        ::tkchat::NickVisMenu
    }
    if {[string compare $Options(Color,$nick,Web) $clr]} {
        # new color
        set Options(Color,$nick,Web) $clr
        set Options(Color,$nick,Inv) [invClr $clr]
        .txt tag configure NICK-$nick -foreground "#[getColor $nick]"
    }
}

# Beep and/or deiconify and raise the main window as an idle callback.
# This is done as an idle callback because there might be many requests
# to alert in a row and we want to batch them all together into one
# action.
#
proc alertWhenIdle {} {
    variable alert_pending
    if {![info exists alert_pending]} {
        set alert_pending 1
        after idle [namespace current]alertCallback
    }
}
proc alertCallback {} {
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
proc checkAlert {msgtype nick str} {
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

proc addMessage {clr nick str} {
    global Options
    variable map
    set w .txt
    checkNick $nick $clr
    checkAlert NORMAL $nick $str
    $w config -state normal
    if {[string equal $nick "clock"] || [string equal $nick "tick"]} {
	$w insert end "$nick\t" [list NICK NICK-$nick]
        .txt insert end "[formatClock $str] " [list NICK-$nick MSG]
    } else {
	if {[string equal $nick "ircbridge"]} {
	    if {[regexp {^([^ ]+) says: (.*)$} $str -> truenick msg]} {
		# Use their true nick, but display bridge users as <$nick>
		# This allows people registered in both systems to appear
		# with the right color info.
		set nick $truenick
		set str  $msg

                if {$nick == "ijchain"} {
                    # ijchain is a Jabber to IRC link.
                    if {[regexp {&lt;(.*?)&gt; (.*)$} $str -> truenick msg]} {
                        set nick $truenick
                        set str $msg
                    }
                }
		
		#Probably obsolete regexp now ircbridge parses CTCPs:
		if { [regexp {^ACTION (.+)} $str -> action] } {
		    addAction $clr "<$nick>" [string range $action 0 end-1]
		} else {
		    $w insert end "<$nick>\t" [list NICK NICK-$nick]
		}
	    } elseif {[regexp {^\* ([^ ]+) (.*)$} $str -> truenick msg] } {
		addAction $clr "<$truenick>" $msg		      
	    }
	} else {
	    $w insert end "$nick\t" [list NICK NICK-$nick]
	}
        foreach {str url} [parseStr $str] {
            foreach cmd [array names ::tkchat::MessageHooks] {
                eval $cmd [list $str $url]
            }
            set tags [list MSG NICK-$nick]
            if {$url != ""} {
                lappend tags URL URL-[incr ::URLID]
                $w tag bind URL-$::URLID <1> [list gotoURL $url]
            }
            tkchat::Insert $w $str $tags $url
        }
    }
    $w insert end "\n" [list NICK NICK-$nick]
    $w config -state disabled
    if {$Options(AutoScroll)} { $w see end }
}

proc ::tkchat::Insert {w str tags {url ""}} {
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
		$w insert end [string range $str $i [expr {$start-1}]] $tags
                set idx [$w index "end -1 char"]
		$w image create end -image ::tkchat::img::$IMG($emot)
                foreach tg $tags {
                    $w tag add $tg $idx
                }
	    } else {
		$w insert end [string range $str $i $end] $tags
	    }
	    set i [expr {$end+1}]
	}
	if {$i <= [string length $str]} {
	    $w insert end [string range $str $i end] $tags
	}
    } else {
	# no emoticons?  perish the thought ...
	$w insert end $str $tags
    }
}

proc ::tkchat::Hook {do type cmd} {
    switch -glob -- $type {
	msg - mes* { set var [namespace current]::MessageHooks }
	default {
	    return -code error "unknown hook type \"$type\": must be\
		    message"
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

proc say { message args } {
    # I've added a few lines to make this speak new messages via the
    # festival synthesiser. It doesn't do it robustly as yet (you'll need
    # festival installed) but as a quick (1min) hack it's got heaps of
    # cool points...  -- Steve Cassidy
    global festival
    if {![info exists festival]} {
	set festival [open "|festival --pipe" w]
    }

    puts [string map [list "\"" ""] $message]
    puts $festival "(SayText \"$message\")"
    flush $festival
}

if {0 && [string length [auto_execok festival]]} {
    ## Don't add this by default ...
    ::tkchat::Hook add message say
}

proc findExecutable {progname varname} {
    upvar 1 $varname result
    set progs [auto_execok $progname]
    if {[llength $progs]} {
	set result [lindex $progs 0]
    }
    return [llength $progs]
}

proc gotoURL {url} {
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
    global tcl_platform Options
    # this code from  http://purl.org/mini/tcl/557.html
    switch -- $tcl_platform(platform) {
	"unix" {
	    expr {
                  [info exists Options(BROWSER)]
                  || [findExecutable netscape	Options(BROWSER)]
                  || [findExecutable iexplorer	Options(BROWSER)]
                  || [findExecutable $Options(NETSCAPE)	Options(BROWSER)]
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
	    # The windows NT shell treats '&' as a special character. Using
	    # a '^' will escape it. See http://wiki.tcl.tk/557 for more info. 
	    if {[string compare $tcl_platform(os) "Windows NT"] == 0} { 
		set url [string map {& ^&} $url] 
	    } 
	    if {[catch {eval exec [auto_execok start] [list $url] &} emsg]} {
		tk_messageBox -message \
                      "Error displaying $url in browser\n$emsg"
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

proc formatClock {str} {
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

proc addAction {clr nick str} {
    global Options
    checkNick $nick $clr
    checkAlert ACTION $nick $str
    .txt config -state normal
    .txt insert end "    * $nick " [list NICK NICK-$nick]
    if {[string equal $nick clock]} {
        .txt insert end "[formatClock $str] " [list NICK-$nick ACTION]
    } else {
	foreach {str url} [parseStr $str] {
	    set tags [list MSG NICK-$nick ACTION]
	    if {$url != ""} {
		lappend tags URL URL-[incr ::URLID]
		.txt tag bind URL-$::URLID <1> [list gotoURL $url]
	    }
	    tkchat::Insert .txt $str $tags $url
	}
    }
    .txt insert end "\n" [list NICK-$nick ACTION]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc addSystem {str} {
    global Options
    .txt config -state normal
    .txt insert end "\t$str\n" [list MSG SYSTEM]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

# Add notification of user entering or leaving. We can hide these notifications
# by setting Options(hideTraffic)
# Always add tehse to text - just tag them so we can elide them at will
# this way, the hide option can affect the past as well as the future
proc addTraffic {who action} {
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
    .txt insert end "\t$msg\n" [list MSG SYSTEM TRAFFIC]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc addUnknown {str} {
    global Options
}

proc showInfo {title str} {
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
	    $t.txt tag bind URL-$::URLID <1> [list gotoURL $url]
	}
    }
    $t.txt insert end "\n"
    $t.txt config -state disabled
    button $t.close -text Close -command [list destroy $t]
    pack $t.close -side right
}

proc addHelp {clr name str} {
    global Options

    if {[lsearch -exact $Options(NickList) $name] >= 0} {
	# this is an incoming private message
	addAction $clr $name " whispers: $str"
	return
    }
    if {[string match "->*" $name]} {
	# an outgoing private message
	addAction $clr $Options(Username) \
              " whispered to [string range $name 2 end]: $str"
	return
    }

    if {[string equal $name "USERINFO"]} {
        set tag USERINFO
    } elseif {[string equal $name "MEMO"]} {
        set tag MEMO
    } elseif {[string equal $name "WELCOME"]} {
        set tag WELCOME
    } else {
        set tag HELP
    }
    if {$Options(Popup,$tag)} {
	showInfo $tag $str
    }

    if {$clr != ""} {
	.txt tag configure $tag -foreground "#$clr"
    }
    .txt config -state normal
    .txt insert end "$name\t" [list $tag NICK]
    foreach {str url} [parseStr $str] {
	regsub -all "\n" $str "\n\t" str
	if {[string equal $url ""]} {
	    .txt insert end "$str " [list MSG $tag]
	} else {
	    .txt insert end "$str " [list MSG $tag URL URL-[incr ::URLID]]
	    .txt tag bind URL-$::URLID <1> [list gotoURL $url]
	}
    }
    .txt insert end "\n" [list $tag NICK]
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc createFonts {} {
    font create FNT  -family helvetica -size -12 -weight normal -slant roman
    font create ACT  -family helvetica -size -12 -weight normal -slant italic
    font create NAME -family helvetica -size -12 -weight bold	-slant roman
    font create SYS  -family helvetica -size -12 -weight bold	-slant italic
}

proc ::tkchat::displayUsers {} {
    if {$::Options(DisplayUsers)} { grid .names } else { grid remove .names }
}

proc findCommonRoot { words } {
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
    
    wm title . "Tcl'ers Chat"
    wm withdraw .
    wm protocol . WM_DELETE_WINDOW quit

    createFonts

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
	    -command { pause $::tkchat::pause }
    $m add command -label "Logout" -underline 0 \
	    -command [namespace origin logonScreen]
    $m add command -label "Save Options" -underline 0 -command saveRC
    $m add separator
    $m add command -label "Exit" -underline 1 -command quit

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
    
    $m add separator

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
        SYSTEM   "System"
        TRAFFIC  "Entry/Exit"
        WELCOME  "Welcome"
        HELP     "Help"
        USERINFO "User Info"
        MEMO     "Memo"
    } {
        $m add checkbutton -label "Hide $text Messages" \
              -onval 1 -offval 0 \
              -var Options(Visibility,$tag) \
              -command "::tkchat::DoVis $tag" \
              -underline 5
    }
    $m add separator
    $m add command -label "Hide All Users" -command  "::tkchat::NickVis 1"
    $m add command -label "Show All Users" -command  "::tkchat::NickVis 0"
    $m add cascade -label "Hide Users" -menu [menu $m.nicks -tearoff 0]
    ::tkchat::NickVisMenu
    $m add separator
    foreach {tag text} {
        HELP     "Help"
        USERINFO "User Info"
        WELCOME  "Welcome"
        MEMO     "Memo"
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
        SOUND     "Beep on alert"
        RAISE     "Raise to top on alert"
    } {
        $m add checkbutton -label "$text" \
              -onval 1 -offval 0 \
              -var Options(Alert,$tag)
    }
    $m add separator
    foreach {tag text} {
        ALL       "Alert when any message received"
        ME        "Alert when username mentioned"
        TOPIC     "Alert when someone speaks who was quiet"
    } {
        $m add checkbutton -label "$text" \
              -onval 1 -offval 0 \
              -var Options(Alert,$tag)
    }
    $m add separator
    foreach {tag text} {
        NORMAL    "Alert on regular posts"
        ACTION    "Alert on whispers and \"/me\" posts"
    } {
        $m add checkbutton -label "$text" \
              -onval 1 -offval 0 \
              -var Options(Alert,$tag)
    }

    ## Debug Menu
    ##
    set m .mbar.dbg
    $m add comman -label "Reload Script" -underline 0 \
          -command [list ::tkchat::Debug reload]
    $m add comman -label "Restart Script" -underline 2 \
          -command [list ::tkchat::Debug restart]
    $m add comman -label "Retrieve Script" -underline 2 \
          -command [list ::tkchat::Debug retrieve]
    $m add comman -label "Evaluate Selection" -underline 1 \
          -command [list ::tkchat::Debug evalSel]
    $m add comman -label "Allow Remote Control" -underline 0 \
          -command [list ::tkchat::Debug server]
    $m add comman -label "Purge Chat Window" -underline 0 \
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
    $m add cascade -label "Translate Selection" -underline 0 -menu [menu $m.tr]

    # main display
    text .txt -background "#[getColor MainBG]" \
          -foreground "#[getColor MainFG]" \
          -font FNT -relief sunken -bd 2 -wrap word \
          -yscroll "scroll_set .sbar" \
          -state disabled -cursor left_ptr -height 1
    scrollbar .sbar -command ".txt yview"
    # user display
    text .names -background "#[getColor MainBG]" \
          -foreground "#[getColor MainFG]" \
          -relief sunken -bd 2 -width 8 -font FNT -state disabled \
          -cursor left_ptr -height 1

    # bottom frame for entry
    frame .btm
    button .ml -text ">>" -command showExtra 
    entry .eMsg
    bind .eMsg <Return>   ::tkchat::userPost
    bind .eMsg <KP_Enter> ::tkchat::userPost
    bind .eMsg <Key-Up>   ::tkchat::entryUp
    bind .eMsg <Key-Down> ::tkchat::entryDown
    bind .eMsg <Key-Tab>  {::tkchat::nickComplete ; break}
    text .tMsg -height 6 -font FNT
    bind .tMsg <Key-Tab> {::tkchat::nickComplete ; break}
    button .post -text "Post" -command ::tkchat::userPost
    #button .refresh -text "Refresh" -command {pause off}
    menubutton .mb -indicator on -relief raised -bd 2 -pady 4 \
          -menu .mb.mnu -textvar Options(MsgTo)
    set Options(MsgTo) "All Users"
    menu .mb.mnu -tearoff 0
    .mb.mnu add command -label "All Users" \
          -command [list set Options(MsgTo) "All Users"]
    .txt tag configure MSG -lmargin2 50
    .txt tag configure INFO -lmargin2 50
    .txt tag configure NICK -font NAME
    .txt tag configure ACTION -font ACT
    .txt tag configure SYSTEM -font SYS
    .txt tag configure URL -underline 1
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
    bind .txt <Up>         {.txt yview scroll  -1 units}
    bind .txt <Down>       {.txt yview scroll   1 units}
    bind .txt <Button-4>   {.txt yview scroll  -1 units}
    bind .txt <Button-5>   {.txt yview scroll   1 units}

    # using explicit rows for restart
    grid .txt .sbar .names -sticky news -padx 1 -pady 2
    grid configure .sbar -sticky ns
    grid .btm              -sticky news -columnspan 3
    grid .ml .eMsg .post .mb -in .btm -sticky ews -padx 2 -pady 2
    grid configure .eMsg .mb -sticky ew

    grid rowconfigure    . 0 -weight 1
    grid columnconfigure . 0 -weight 1
    grid columnconfigure .btm 1 -weight 1

    # call this to activate the option on whether the users should be shown
    displayUsers

    wm geometry . $Options(Geometry)
    wm deiconify .
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


proc ::tkchat::About {} {
    variable rcsid
    global Options
    
    regexp -- {Id: tkchat.tcl,v (\d+\.\d+)} $rcsid -> rcsVersion

    # don't cache this window - of user reloads on the fly
    # we want to make sure it displays latest greates info!
    catch {destroy .about}

    set w [toplevel .about -class dialog]
    wm withdraw $w
    wm transient $w .
    wm title $w "About TkChat $rcsVersion"
    button $w.b -text Dismiss -command [list wm withdraw $w]
    text $w.text -height 9 -bd 1 -width 60
    pack $w.b -fill x -side bottom
    pack $w.text -fill both -side left -expand 1
    $w.text tag config center -justify center
    $w.text tag config title -justify center -font {Courier -18 bold}
    $w.text insert 1.0 "About TkChat v$rcsVersion" title \
          "\n\nCopyright (C) 2001 Bruce B Hartweg <brhartweg@bigfoot.com>" \
          center \
          "\n$rcsid"
    $w.text config -state disabled
    wm deiconify $w
    
}

proc ::tkchat::userPost {} {
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
            switch -re -- $msg {
                {^/smiley?s?$} {
                    ::tkchat::ShowSmiles
                }
                {^/colou?rs?$} {
                    tkchat::ChangeColors
                }
                {^/font } {
                    set name [string trim [string range $msg 5 end]]
                    catch {::tkchat::ChangeFont -family $name}
                }
                {^/(font)?size [0-9]+} {
                    regexp -- {[0-9]+} $msg size
                    catch {::tkchat::ChangeFont -size $size}
                }
                {^/macros?$} {
                    tkchat::EditMacros
                }
                {^/userinfo} {
                    set ::UserClicked 1
                    msgSend $msg
                }
                {^/\?} {
                    doSearch $msg
                }
                {^/!} {
                    resetSearch
                }
                {^/(urn:)?tip:\d+} {
                    if {[regexp {(?:urn:)?tip:(\d+)} $msg -> tip]} {
                        gotoURL http://purl.org/tcl/tip/$tip
                    }
                }
                {^/bug } {
		    doBug $msg
                }
                default  {
                    # might be server command - pass it on
                    msgSend $msg
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
                msgSend $msg
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

proc hideExtra {} {
    grid remove .tMsg
    grid config .eMsg -in .btm -row 0 -column 1 -sticky ew
    .ml config -text ">>" -command showExtra
    .eMsg delete 0 end
    .eMsg insert end [string trim [.tMsg get 1.0 end]]
}
proc showExtra {} {
    global Options
    grid remove .eMsg
    grid config .tMsg -in .btm -row 0 -column 1 -sticky ew
    .ml config -text "<<" -command hideExtra
    .tMsg delete 1.0 end
    .tMsg insert end [.eMsg get]
}
proc ::tkchat::logonScreen {} {
    global Options LOGON
    pause on 0
    if {![winfo exists .logon]} {
	toplevel .logon -class dialog
	wm withdraw .logon
	wm transient .logon .
	wm title .logon "Logon to the Tcl'ers Chat"
	checkbutton .logon.prx -text "Use Proxy" -var Options(UseProxy) -underline 0
        bind .logon <Alt-u> {focus .logon.prx}
	label .logon.lph -text "Proxy Host"
	label .logon.lpp -text "Proxy Port"
	entry .logon.eph -textvar Options(ProxyHost)
	entry .logon.epp -textvar Options(ProxyPort)
	label .logon.lpan -text "Proxy Auth Username"
	label .logon.lpap -text "Proxy Auth Password"
	entry .logon.epan -textvar Options(ProxyUsername)
	entry .logon.epap -textvar Options(ProxyPassword) -show {*}
	label .logon.lnm -text "Chat Username"
	label .logon.lpw -text "Chat Password"
	entry .logon.enm -textvar Options(Username)
	entry .logon.epw -textvar Options(Password) -show *
	checkbutton .logon.rpw -text "Remember Chat Password" \
              -var Options(SavePW)
	checkbutton .logon.atc -text "Auto-connect" -var Options(AutoConnect)
	button .logon.ok -text "Logon" -command "set LOGON 1" -width 8 -underline 0
        bind .logon <Alt-l> {focus .logon.ok}
	button .logon.cn -text "Quit" -command quit -width 8 -underline 0
        bind .logon <Alt-q> {focus .logon.cn}
	trace variable Options(UseProxy) w [namespace origin optSet]
	trace variable Options(SavePW) w [namespace origin optSet]
	grid .logon.prx - - -sticky w -pady 3
	grid  x .logon.lph .logon.eph -sticky w -pady 3
	grid  x .logon.lpp .logon.epp -sticky w -pady 3
	grid  x .logon.lpan .logon.epan -sticky w -pady 3
	grid  x .logon.lpap .logon.epap -sticky w -pady 3
	grid .logon.lnm .logon.enm - -sticky ew -pady 5
	grid .logon.lpw .logon.epw - -sticky ew
	grid x .logon.rpw  - -sticky w -pady 3 -pady 3
	grid x .logon.atc  - -sticky w -pady 3
	grid .logon.ok - .logon.cn -pady 10
	wm resizable .logon 0 0
        bind .logon <Return> [list .logon.ok invoke]
        bind .logon <Escape> [list .logon.cn invoke]
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
    if {$Options(UseProxy)} {
	::http::config -proxyhost $Options(ProxyHost) \
              -proxyport $Options(ProxyPort)
    }
    # connect
    logonChat
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

proc ::tkchat::doBug {msg} {
    # msg should be off form: ^/bug ?group? ?tracker? id
    # category defaults to Tcl bugs
    set len [llength $msg]
    if {$len < 2 || $len > 4} {
	addSystem "wrong # args: must be /bug ?group? ?tracker? id"
	return
    }
    array set groups {
	tcl     10894	tk      12997	tcllib  12883
	incrtcl 13244	expect  13179	tclx    13247
    }
    array set trackers {
	bugs    1	patches 3	rfes    35	frs     35
    }
    # defaults
    set gid tcl
    set tid bugs
    set id [lindex $msg end]
    if {$len == 3} {
	set gid [string tolower [lindex $msg 1]]
    } elseif {$len == 4} {
	set gid [string tolower [lindex $msg 1]]
	set tid [string tolower [lindex $msg 2]]
    }

    set group [array names groups $gid]
    if {[llength $group] != 1} {
	set group [array names groups $gid*]
	if {[llength $group] != 1} {
	    addSystem "Invalid group '$gid', must be one of\
		    [join [array names groups {, }]]"
	    return
	}
    }
    set group   $groups($group)

    set tracker [array names trackers $tid]
    if {[llength $tracker] != 1} {
	set tracker [array names trackers $tid*]
	if {[llength $tracker] != 1} {
	    addSystem "Invalid tracker '$tid', must be one of\
		    [join [array names trackers {, }]]"
	    return
	}
    }
    set tracker $trackers($tracker)

    set url "http://sourceforge.net/tracker/index.php?func=detail"
    append url "&aid=$id&group_id=$group"
    if {$tracker < 10} {
	set atid $tracker$group
    } else {
	set atid [expr {$tracker*10000 + $group}]
    }
    append url "&atid=$atid"

    # Until exec works correctly with &, just show the URL for C&P
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
proc newColor {w idx} {
    set init "#$::DlgData(Color,$idx,Mine)"
    set tmp [tk_chooseColor \
                   -title "Select Override Color" \
                   -initialcolor $init]
    if {$tmp != ""} {
	set ::DlgData(Color,$idx,Mine) [string range $tmp 1 end]
	$w config -fg $tmp -selectcolor $tmp
    }
}
proc buildRow {f idx disp} {
    global DlgData
    variable buildRow_seq
    if { ![info exists buildRow_seq] } {
	set buildRow_seq 1
    } else {
	incr buildRow_seq
    }
    set seq $buildRow_seq
    label $f.nm$seq -text "$disp" -anchor w -font NAME -padx 0 -pady 0
    radiobutton $f.def$seq -text "default" \
          -var DlgData(Color,$idx,Which) \
          -val Web -fg "#$DlgData(Color,$idx,Web)" \
          -selectcolor "#$DlgData(Color,$idx,Web)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    radiobutton $f.inv$seq -text "inverted" \
          -var DlgData(Color,$idx,Which) \
          -val Inv -fg "#$DlgData(Color,$idx,Inv)" \
          -selectcolor "#$DlgData(Color,$idx,Inv)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    radiobutton $f.ovr$seq -text "custom" \
          -var DlgData(Color,$idx,Which) \
          -val Mine -fg "#$DlgData(Color,$idx,Mine)"\
          -selectcolor  "#$DlgData(Color,$idx,Mine)" \
          -indicatoron 0  -padx 0 -pady 0 -font FNT
    button $f.clr$seq -text "..." -padx 0 -pady 0  -font FNT \
          -command [list newColor $f.ovr$seq $idx]
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
    wm protocol $t WM_DELETE_WINDOW {set DlgDone cancel}
    wm withdraw $t
    wm title $t "Color Settings"

    label $t.l1 -text "Posting Color" -font NAME
    label $t.l2 -text "Example Text" -background white -foreground \#$DlgData(MyColor) -font ACT
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
        if {[lsearch $Options(OnLineUsers) $nick] < 0} {
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

proc applyColors {} {
    global Options
    # update colors
    .txt config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    .names config -bg "#[getColor MainBG]" -fg "#[getColor MainFG]"
    .txt tag configure found -background "#[getColor SearchBG]"
    foreach nk $Options(NickList) {
        .txt tag config NICK-$nk -foreground "#[getColor $nk]"
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

proc quit {} {
    set q "Are you sure you want to quit?"
    set a [tk_messageBox -type yesno -message $q]
    if {[string equal $a "yes"]} {
	saveRC
	exit
    }
}

proc saveRC {} {
    global Options
    if {[info exists ::env(HOME)]} {
	set rcfile [file join $::env(HOME) .tkchatrc]
        set Options(Geometry) [wm geometry .]
	array set tmp [array get Options]
	set ignore {
            History FetchTimerID OnlineTimerID
            FetchToken OnlineToken ProxyPassword
            URL URL2 URLlogs errLog ChatLogChannel 
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

proc scroll_set {sbar f1 f2} {
    global Options
    $sbar set $f1 $f2
    if {[string equal "$f1$f2" "01"]} {
	grid remove $sbar
    } else {
	grid $sbar
    }
    set Options(AutoScroll) [string equal $f2 1]
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
	    eval font delete [font names]
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
    foreach font [font names] {
	font configure $font -family $family -size $size
    }
    return
}

proc ::tkchat::ChangeFont {opt val} {
    set ::Options(Font,$opt) $val
    foreach font [font names] {
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
        wm title $t "Available Emoticons"
        wm protocol $t WM_DELETE_WINDOW [list wm withdraw $t]
        set txt [text $t.txt -background black -foreground white \
                       -font NAME -tabs {1.5i l 2.0i l} \
                       -height [expr [llength [image names]] + 5]]
        foreach image [lsort [image names]] {
            set name [lindex [split $image :] end]
            $txt insert end "$name\t"
            $txt image create end -image $image
            if {[info exists tmp($name)]} {
                $txt insert end "\t[join $tmp($name) "   "]"
            }
            $txt insert end \n
        }
        pack $txt -expand 1 -fill both
    }
}

proc ::tkchat::Init {} {
    global Options env
    catch {set Options(BROWSER) $env(BROWSER)}
    catch {set Options(NETSCAPE) $env(NETSCAPE)}
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
	Font,-family	Helvetica
	Font,-size	-12
	MaxLines	500
	ChatLogFile	""
	LogFile		""
	LogLevel	info
	errLog		stderr
	emoticons	1
	hideTraffic	0
	TimeFormat	"At the tone, the time is %H:%M on %A %d %b %Y"
	TimeGMT		0
	HistoryLines	-1
	timeout		30000
	Emoticons	1
	AnimEmoticons	0
	Popup,USERINFO	1
	Popup,WELCOME	0
	Popup,MEMO	1
	Popup,HELP	1
	Visibility,USERINFO  1
	Visibility,WELCOME   1
	Visibility,MEMO	     1
	Visibility,HELP	     1
	Alert,SOUND	     0
	Alert,RAISE	     1
	Alert,ALL	     0
	Alert,ME	     1
	Alert,TOPIC	     1
	Alert,NORMAL	     1
	Alert,ACTION	     1
    }
    set Options(URL)	$::tkchat::HOST/cgi-bin/chat.cgi
    set Options(URL2)	$::tkchat::HOST/cgi-bin/chat2.cgi
    set Options(URLlogs) $::tkchat::HOST/tchat/logs
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
    set Options(Offset) 50
    catch {unset Options(FetchToken)}
    catch {unset Options(OnlineToken)}
    set Options(History) {}
    set Options(OnLineUsers) {}
    
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

    # do this first so we have images available
    Smile
    # build screen
    CreateGUI
    foreach idx [array names Options Visibility,*] {
	set tag [lindex [split $idx ,] end]
	.txt tag config $tag -elide $Options($idx)
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
    # connect
    if {$Options(AutoConnect)} {
	logonChat
    } else {
	logonScreen
    }
}

if {![info exists ::URLID]} {
    ::tkchat::Init
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
package require Tk
package require http
package require htmlparse
namespace forget ::dict.leo.org

namespace eval ::dict.leo.org {
    namespace export query askLEO askLEOforSelection
    variable table ""
    variable last  ""
    variable Query ""
}

proc ::dict.leo.org::parse {tag close options body} {
    variable TD
    variable table
	switch -- $close$tag {
	    TD	   {set TD ""}
	    /TD	   {if {[llength $TD]} {lappend table [string trim $TD]}}
	    default {append TD [string map {&nbsp; { }} $body]}
	}
}

proc ::dict.leo.org::query {query} {
    variable table
    set url http://dict.leo.org/?[http::formatQuery search $query]
    set tok [::http::geturl $url]
    foreach line [split [::http::data $tok] "\n"] {
	if {[string match "*search results*" $line]} break
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

bind . <Shift-Button-3> {::dict.leo.org::askLEOforSelection}

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
	    label $w._testing
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
	entry $w.sample.text -background [$w.sample cget -background]
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
#trace add variable ::UserClicked write ::tkchat::traceVar

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
