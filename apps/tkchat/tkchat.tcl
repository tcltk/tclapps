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
if {[info exists scripdoc::self]} {
    # Enable functionality as a scripted document
    lappend auto_path [file join $scripdoc::self lib]
}

package require http		; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib
package require base64		; # tcllib
package require Tk 8.3		; # core Tk

namespace eval ::tkchat {
    # Everything will eventually be namespaced
    variable MessageHooks
    array set MessageHooks {}

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://purl.org/mini

    variable HEADUrl {http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/tcllib/tclapps/apps/tkchat/tkchat.tcl?rev=HEAD}
    variable rcsid   {$Id: tkchat.tcl,v 1.29 2002/01/11 11:15:24 hartweg Exp $}

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
proc tkchat::LogLevelSet {args} {
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
               && $Options(ProxyUsername) != {} } {
	set auth [list "Proxy-Authorization" \
                        [concat "Basic" \
                               [base64::encode \
                                      $Options(ProxyUsername):$Options(ProxyPassword)]]]
    }
    return $auth
}

# Retrieve the lastest version of tkchat from the SourceForge CVS.
# This code is (almost) entirely ripped from TkCon. - PT.
proc tkchat::Retrieve {} {
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
	set token [::http::geturl $HEADUrl -headers [buildProxyHeaders] -timeout 30000]
	::http::wait $token
	set code [catch {
	    if {[::http::status $token] == "ok"} {
		set fid [open $file w]
		fconfigure $fid -translation binary
		set data [::http::data $token]
		puts -nonewline $fid $data
		close $fid
		regexp {Id: tkchat.tcl,v (\d+\.\d+)} $data -> rcsVersion
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
		uplevel \#0 [list source $file]
		tk_messageBox -message "Script has been reloaded!"
	    }
	}
    }
}

# Check the HTTP response for redirecting URLs. - PT
proc checkForRedirection {tok optionName} {
    global Options
    set ncode [::http::ncode $tok]
    if {[expr {$ncode == 302 || $ncode == 301}]} {
        upvar \#0 $tok state
        array set meta $state(meta)
        if {[info exists meta(Location)]} {
            set Options($optionName) $meta(Location)
            return 1
        }
    }
    return 0
}

proc tkchat::GetHistLogIdx {szary} {
    global Options
    upvar $szary ary

    catch {unset ary}
    array set ary {}
    set loglist {}
    # get list of available logs
    set url "$Options(URLlogs)/?M=D"
    set tok [::http::geturl $url  \
                   -headers [buildProxyHeaders]]
    if { [checkForRedirection $tok URLlogs] } {
        ::http::cleanup $tok
        set loglist [::tkchat::GetHistLogIdx ary]
    } elseif {[string equal [::http::status $tok] "ok"]} {
        set RE {<A HREF="([0-9-]+\.txt)">.*\s([0-9]+k)}
        foreach line [split  [::http::data $tok] \n] {
            if { [regexp  -- $RE $line -> logname size] } {
                set loglist [linsert $loglist 0 $logname]
                set ary($logname) $size
            }
        }
        ::http::cleanup $tok
    }
    return $loglist
}
proc tkchat::ParseHistLog {log} {
    global Options
    
    set retList {}
    set MsgRE {^\s*(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun).+?\[(\S+)\]\s+([^:]+):?\s*(.*)$}
    # fetch log
    set url "$Options(URLlogs)/$log"
    set tok [::http::geturl  $url \
                   -headers [buildProxyHeaders]]
    if {[string equal [::http::status $tok] "ok"]} {
        set logdata [split  [::http::data $tok] \n]
        ::http::cleanup $tok
        set lastnick ""
        set lastdata ""
        foreach line $logdata {
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
                append lastdata $line 
            }
        }
        if {[string length $lastnick]>0} {
            lappend retList $lastnick $lastdata
        }
    }
    return $retList
}

# this called on first logon and after a purge
# so not bothering to backgound it
proc tkchat::LoadHistory {} {
    global Options

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
            pack [label $t.lbl -text "Please Select How far back you want to load"] \
                  -expand 1 -fill both -pady 5
            set i 0
            variable HistQueryNum [llength $loglist]
            foreach l $loglist {
                pack [radiobutton $t.rb$i -text "$l ($logsize($l))" \
                            -val $i -var ::tkchat::HistQueryNum] \
                      -anchor w -padx 15 -pady 0
                incr i
            }
            pack [radiobutton $t.rb$i -text "None" \
                        -val $i -var ::tkchat::HistQueryNum] \
                  -anchor w -padx 15 -pady 0
            pack [button $t.ok -text Ok -command [list destroy $t] ] \
                  -fill both -padx 5 -pady 10
            catch {::tk::PlaceWindow $t widget .}
            wm deiconify $t
            tkwait visibility $t
            grab $t
            tkwait window $t
            foreach log [lrange $loglist $HistQueryNum end] {
                if [catch {ParseHistLog $log} new] {
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
            if [catch {ParseHistLog $log} new] {
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
    ::http::geturl $Options(URL) \
          -query [string map {%5f _} $qry] \
          -headers [buildProxyHeaders] \
          -command msgDone
}

proc msgDone {tok} {
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
	timeout { tk_messageBox -message "Message Post timed out" }
	error {
	    tk_messageBox -message \
                  "Message Post Errored: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc logonChat {} {
    if 0 {
        # use when testing only - allows restarts without actually logging in again
        catch {pause off}
        return
    }
    
    global Options
    errLog "Logon to $Options(URL2)"
    set qry [::http::formatQuery \
                   action	login \
                   name	$Options(Username) \
                   password	$Options(Password) \
                  ]
    ::http::geturl $Options(URL2) \
          -query $qry \
          -headers [buildProxyHeaders] \
          -command logonDone
}

proc logonDone {tok} {
    errLog "Logon: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL2]} {
                ::http::cleanup $tok
                logonChat
                return
            }
            
            if {[catch {::tkchat::LoadHistory} err]} { errLog $err }
            if {[catch {pause off} err]} { errLog $err }
        }
	reset	{ errLog "User reset logon operation" }
	timeout	{ tk_messageBox -message "Logon timed out" }
	error	{ tk_messageBox -message "Logon Error: [::http::error $tok]" }
    }
    ::http::cleanup $tok
}
proc logoffChat {} {
    global Options
    set qry [::http::formatQuery action gotourl url chat.cgi]
    ::http::geturl $Options(URL2) \
          -query $qry \
          -headers [buildProxyHeaders] \
          -command logoffDone
    logonScreen
}

proc logoffDone {tok} {
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
    set Options(FetchToken) [::http::geturl $Options(URL) \
                                   -query $qry \
                                   -headers [buildProxyHeaders] \
                                   -command fetchDone]
}

proc fetchDone {tok} {
    global Options
    if {[string equal $tok $Options(FetchToken)]} {
	unset Options(FetchToken)
    } else {
	errLog "Fetch Command finished with token $tok" \
              "expected $Options(FetchToken)"
	unset Options(FetchToken)
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
	    if {[catch {parseData [::http::data $tok]} err]} { errLog $err }
	}
	reset - Reset - RESET {
	    errLog "User reset post operation"
	}
	timeout - Timeout - TIMEOUT {
	    tk_messageBox -message "Message Post timed out"
	}
	error - Error - ERROR {
	    tk_messageBox -message "Message Post Errored: [::http::error $tok]"
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
    set Options(OnlineToken) [::http::geturl $Options(URL) \
                                    -query $qry \
                                    -headers [buildProxyHeaders] \
                                    -command onlineDone]
}

proc onlineDone {tok} {
    global Options
    if {[string equal $tok $Options(OnlineToken)]} {
	unset Options(OnlineToken)
    } else {
	errLog "Online Command finished with token $tok" \
              "expected $Options(OnlineToken)"
	unset Options(OnlineToken)
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
	    errLog "User reset post operation"
	}
	timeout {
	    tk_messageBox -message "Message Post timed out"
	}
	error {
	    tk_messageBox -message "Message Post Errored: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc updateNames {rawHTML} {
    global Options
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
	.names tag bind URL-$::URLID <1> "set ::UserClicked 1 ; [list msgSend "/userinfo $name"]"
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

    set inHelp 0
    set inMsg 0
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
                    if ($UserClicked) {
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
        .txt tag config MSG -lmargin2 [incr wid 20]
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
    }
    if {[string compare $Options(Color,$nick,Web) $clr]} {
        # new color
        set Options(Color,$nick,Web) $clr
        set Options(Color,$nick,Inv) [invClr $clr]
        .txt tag config NICK-$nick -foreground "#[getColor $nick]"
    }
}

proc addMessage {clr nick str} {
    global Options
    variable map
    checkNick $nick $clr
    .txt config -state normal
    .txt insert end $nick [list NICK NICK-$nick] "\t"
    foreach {str url} [parseStr $str] {
	if {[string compare "unix" $::tcl_platform(platform)]} {
	    regsub -all {:[-^]?\)} $str \u263a str
	}
	set str [string map [list "\n" "\n\t"] $str]
	foreach cmd [array names ::tkchat::MessageHooks] {
	    eval $cmd [list $str $url]
	}
	if {$url == ""} {
	    .txt insert end "$str " [list MSG NICK-$nick]
	} else {
	    .txt insert end "$str " \
                  [list MSG NICK-$nick URL URL-[incr ::URLID]]
	    .txt tag bind URL-$::URLID <1> [list gotoURL $url]
	}
    }
    .txt insert end "\n"
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc tkchat::Hook {do type cmd} {
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

if {[string length [auto_execok festival]]} {
    ::tkchat::Hook add message say

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
	    if {[catch {exec $Options(BROWSER) -remote openURL($url)}]} {
	        # Try -remote with raw URL argument 
	        if {[catch {exec $Options(BROWSER) -remote $url}]} {
		    # perhaps browser doesn't understand -remote flag
		    if {[catch {exec $Options(BROWSER) $url &} emsg]} {
		        tk_messageBox -message \
                              "Error displaying $url in browser\n$emsg"
		    }
		}
	    }
	}
	"windows" {
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
    if {[regexp {^[\s:]*(\d+)} $out -> ticks]} {
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
    .txt config -state normal
    .txt insert end "\t* $nick " [list NICK NICK-$nick]
    if {[string equal $nick clock]} {
        .txt insert end "[formatClock $str] " [list NICK-$nick ACTION]
    } else {
	foreach {str url} [parseStr $str] {
	    regsub -all "\n" $str "\n\t" str
	    if {[string equal $url ""]} {
		.txt insert end "$str " [list MSG NICK-$nick ACTION]
	    } else {
		.txt insert end "$str " \
                      [list MSG NICK-$nick ACTION URL URL-[incr ::URLID]]
		.txt tag bind URL-$::URLID <1> [list gotoURL $url]
	    }
	}
    }
    .txt insert end "\n"
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
    if {[info exists MSGS($action)]} {
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
    $t.txt tag config URL -underline 1
    $t.txt tag bind URL <Enter> [list $t.txt config -cursor hand2]
    $t.txt tag bind URL <Leave> [list $t.txt config -cursor left_ptr]
    foreach {str url} [parseStr $str] {
	set str [string map [list "\n" "\n\t"] $str]
	if {$url == ""} {
	    $t.txt insert end "$str "
	} else {
	    $t.txt insert end "$str " [list URL URL-[incr ::URLID]]
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
    if {[string equal $name "USERINFO"]} {
	# don't clutter up chat screen with these
	showInfo "User Info" $str
	return
    } elseif {[string equal $name "MEMO"]} {
	showInfo "Memo" $str
	return
    }

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
    if {$clr != ""} {
	.txt tag config HELP -foreground "#$clr"
    }
    .txt config -state normal
    .txt insert end "$name\t" [list HELP NICK]
    foreach {str url} [parseStr $str] {
	regsub -all "\n" $str "\n\t" str
	if {[string equal $url ""]} {
	    .txt insert end "$str " [list MSG HELP]
	} else {
	    .txt insert end "$str " [list MSG HELP URL URL-[incr ::URLID]]
	    .txt tag bind URL-$::URLID <1> [list gotoURL $url]
	}
    }
    .txt insert end "\n"
    .txt config -state disabled
    if {$Options(AutoScroll)} { .txt see end }
}

proc createFonts {} {
    font create FNT  -family helvetica -size -12 -weight normal -slant roman
    font create ACT  -family helvetica -size -12 -weight normal -slant italic
    font create NAME -family helvetica -size -12 -weight bold	-slant roman
    font create SYS  -family helvetica -size -12 -weight bold	-slant italic
}

proc tkchat::CreateGUI {} {
    global Options

    wm title . "Tcl'ers Chat"
    wm withdraw .
    wm protocol . WM_DELETE_WINDOW quit

    createFonts

    menu .mbar -type menubar
    . config -menu .mbar

    .mbar add cascade -label File -menu [menu .mbar.file -tearoff 0]
    .mbar add cascade -label Preferences -menu [menu .mbar.edit -tearoff 0]
    .mbar add cascade -label Debug -menu [menu .mbar.dbg -tearoff 0]
    .mbar add cascade -label Help -menu [menu .mbar.help -tearoff 0]

    ## File Menu
    ##
    set m .mbar.file
    $m add checkbutton -label Pause \
          -variable ::tkchat::pause \
          -command { pause $::tkchat::pause }
    $m add command -label Logout -command logonScreen
    $m add command -label "Save Options" -command saveRC
    $m add separator
    $m add command -label Exit -command quit

    ## Edit Menu
    ##
    set m .mbar.edit
    $m add command -label Colors... \
          -command tkchat::ChangeColors
    $m add cascade -label "Font Name" -menu [menu $m.fontName -tearoff 0]
    set num 0
    foreach name [lsort [font families]] {
	$m.fontName add radiobutton -label $name \
              -var Options(Font,-family) \
              -val $name \
              -command [list ::tkchat::ChangeFont -family $name]
        incr num
        if {($num%30)==1 && $num>1} {
            $m.fontName entryconfig end -columnbreak 1
        }
    }
    $m add cascade -label "Font Size" -menu [ menu $m.fontSize -tearoff 0]
    foreach sz {8 10 12 14 16 18 24 28 36} {
	$m.fontSize add radiobutton -label $sz \
              -var Options(Font,-size) \
              -val $sz \
              -command [list ::tkchat::ChangeFont -size $sz]
    }
    $m add separator
    $m add checkbutton -label "Hide Entry/Exit Messages" \
          -onval 1 -offval 0 -var Options(hideTraffic) -command {
              .txt tag config TRAFFIC -elide $Options(hideTraffic)
          }
    $m add cascade -label "Refresh Frequency" -menu [menu $m.refresh -tearoff 0]
    foreach s {15 30 45 60} {
        $m.refresh add radiobutton -label "$s seconds" -val $s \
              -var Options(Refresh)
    }        
    $m add cascade -label "Max Window Buffer" -menu [menu $m.buffer -tearoff 0]
    foreach l {500 1000 1500 2500 5000 10000} {
        $m.buffer add radiobutton -label "$l lines" -val $l \
              -var Options(MaxLines)
    }
    $m add separator
    $m add cascade -label "Local Chat Logging" -menu [menu $m.chatLog -tearoff 0]
    $m.chatLog add radiobutton -label Disabled -var ::Options(ChatLogOff) \
          -val 1 -command {tkchat::OpenChatLog close}
    $m.chatLog add command -label "To File..." \
          -command {tkchat::OpenChatLog open}
    $m add cascade -label "Loading Server History" -menu [menu $m.hist -tearoff 0]
    $m.hist add radiobutton -label "Do NOT load any history" -val 0 \
          -var Options(HistoryLines)
    $m.hist add radiobutton -label "Ask me which logs to load" -val -1 \
          -var Options(HistoryLines)
    foreach l {50 100 200 500 1000 2500 10000} {
        $m.hist add radiobutton -label "Load at least $l lines" -val $l \
              -var Options(HistoryLines)
    }
    
    ## Debug Menu
    ##
    set m .mbar.dbg
    $m add comman -label "Reload Script" \
          -command [list ::tkchat::Debug reload]
    $m add comman -label "Restart Script" \
          -command [list ::tkchat::Debug restart]
    $m add comman -label "Retrieve Script" \
          -command [list ::tkchat::Debug retrieve]
    $m add comman -label "Evaluate Selection" \
          -command [list ::tkchat::Debug evalSel]
    $m add comman -label "Purge Chat Window" \
          -command [list ::tkchat::Debug purge]
    $m add separator
    $m add cascade -label "Error Logging" -menu [menu $m.err -tearoff 0]
    $m.err add cascade -label "Log Level" -menu [menu $m.err.lvl -tearoff 0]
    $m.err add radiobutton -label "To Stderr" -var ::Options(LogStderr) -val 1 \
          -command {tkchat::OpenErrorLog stderr}
    $m.err add command -label "To File..." \
          -command {tkchat::OpenErrorLog pick}
    foreach lvl [lsort -command ::log::lvCompare $::log::levels] {
        $m.err.lvl add radiobutton -label $lvl -val $lvl \
              -var Options(LogLevel)
    }
    $m add separator
    $m add checkbutton -label "Console" \
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
    $m add command -label About... -command tkchat::About


    text .txt -background "#[getColor MainBG]" \
          -foreground "#[getColor MainFG]" \
          -font FNT -relief sunken -bd 2 -wrap word \
          -yscroll "scroll_set .sbar" \
          -state disabled -cursor left_ptr -height 1
    scrollbar .sbar -command ".txt yview"
    text .names -background "#[getColor MainBG]" \
          -foreground "#[getColor MainFG]" \
          -relief sunken -bd 2 -width 8 -font FNT -state disabled \
          -cursor left_ptr -height 1
    button .ml -text "More >>>" -command showExtra
    entry .eMsg
    bind .eMsg <Return> userPost
    bind .eMsg <KP_Enter> userPost
    text .tMsg -height 6 -font FNT
    button .post -text Post -command userPost
    menubutton .mb -indicator on -relief raised -bd 2 \
          -menu .mb.mnu -textvar Options(MsgTo)
    set Options(MsgTo) "All Users"
    menu .mb.mnu -tearoff 0
    .mb.mnu add command -label "All Users" \
          -command [list set Options(MsgTo) "All Users"]
    .txt tag config MSG -lmargin2 50
    .txt tag config NICK -font NAME
    .txt tag config ACTION -font ACT
    .txt tag config SYSTEM -font SYS
    .txt tag config URL -underline 1
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

    # using explicit rows for restart
    grid .txt   -   .sbar .names  -  -padx 1 -pady 2 -sticky news -row 0
    grid .ml .eMsg    -   .post  .mb -padx 2 -pady 3 -sticky ew -row 1
    grid columnconfigure . 1 -weight 1
    grid rowconfigure . 0 -weight 1
    wm geometry . $Options(Geometry)
    wm deiconify .
}

proc tkchat::About {} {
    variable rcsid
    global Options
    
    regexp {Id: tkchat.tcl,v (\d+\.\d+)} $rcsid -> rcsVersion

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

proc userPost {} {
    global Options
    if {[winfo ismapped .eMsg]} {
	set str [.eMsg get]
    } else {
	set str [.tMsg get 1.0 end]
    }
    set msg [string trim $str]
    if {$msg == ""} {
	return
    }
    if {[string equal $Options(MsgTo) "All Users"]} {
	msgSend $msg
    } else {
	msgSend $msg $Options(MsgTo)
    }
    .eMsg delete 0 end
    .tMsg delete 1.0 end
}

proc hideExtra {} {
    grid remove .tMsg
    grid config .eMsg -row 1 -column 1 -columnspan 2 -sticky ew
    .ml config -text "More >>>" -command showExtra
}
proc showExtra {} {
    grid remove .eMsg
    grid config .tMsg -row 1 -column 1 -columnspan 2 -sticky ew
    .ml config -text "Less <<<" -command hideExtra
    .tMsg insert end [.eMsg get]
}
proc logonScreen {} {
    global Options LOGON
    pause on 0
    if {![winfo exists .logon]} {
	toplevel .logon -class dialog
	wm withdraw .logon
	wm transient .logon .
	wm title .logon "Logon to the Tcl'ers Chat"
	checkbutton .logon.prx -text "Use Proxy" -var Options(UseProxy)
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
	button .logon.ok -text "Logon" -command "set LOGON 1"
	button .logon.cn -text "Quit" -command quit
	trace variable Options(UseProxy) w optSet
	trace variable Options(SavePW) w optSet
	grid .logon.prx - - -sticky w -pady 3
	grid  x .logon.lph .logon.eph -sticky w -pady 3
	grid  x .logon.lpp .logon.epp -sticky w -pady 3
	grid  x .logon.lpan .logon.epan -sticky w -pady 3
	grid  x .logon.lpap .logon.epap -sticky w -pady 3
	grid .logon.lnm .logon.enm - -sticky ew -pady 5
	grid .logon.lpw .logon.epw - -sticky ew
	grid x .logon.rpw  - -sticky w -pady 3 -pady 3
	grid x .logon.atc  - -sticky w -pady 3
	grid .logon.ok - .logon.cn -sticky {} -pady 10
	wm resizable .logon 0 0
    }
    optSet
    catch {::tk::PlaceWindow .logon widget .}
    wm deiconify .logon
    tkwait visibility .logon
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

proc optSet {args} {
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
    label $f.nm$idx -text "$disp" -anchor w -font NAME -padx 0 -pady 0
    radiobutton $f.def$idx -text "default" \
          -var DlgData(Color,$idx,Which) \
          -val Web -fg "#$DlgData(Color,$idx,Web)" \
          -selectcolor "#$DlgData(Color,$idx,Web)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    radiobutton $f.inv$idx -text "inverted" \
          -var DlgData(Color,$idx,Which) \
          -val Inv -fg "#$DlgData(Color,$idx,Inv)" \
          -selectcolor "#$DlgData(Color,$idx,Inv)" \
          -indicatoron 0 -padx 0 -pady 0 -font FNT
    radiobutton $f.ovr$idx -text "custom" \
          -var DlgData(Color,$idx,Which) \
          -val Mine -fg "#$DlgData(Color,$idx,Mine)"\
          -selectcolor  "#$DlgData(Color,$idx,Mine)" \
          -indicatoron 0  -padx 0 -pady 0 -font FNT
    button $f.clr$idx -text "..." -padx 0 -pady 0  -font FNT \
          -command [list newColor $f.ovr$idx $idx]
    grid $f.nm$idx $f.def$idx $f.inv$idx $f.ovr$idx $f.clr$idx \
          -padx 2 -pady 2 -sticky ew
}

proc tkchat::ChangeColors {} {
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
    foreach {idx str} {MainBG Background MainFG Foreground} {
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
    foreach nk $Options(NickList) {
        .txt tag config NICK-$nk -foreground "#[getColor $nk]"
    }
}

# Point the Chat log to a new file.
proc tkchat::OpenChatLog {opt} {
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
proc tkchat::OpenErrorLog {opt} {
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
	set ignore {History FetchTimerID OnlineTimerID FetchToken OnlineToken\
                          ProxyPassword URL URL2 errLog ChatLogChannel}
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
	    puts $fd "array set Options {"
	    puts $fd [join $oplist "\n"]
	    puts $fd "}"
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


proc tkchat::Debug {cmd args } {
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
	    tk_messageBox -message "Script has been reloaded!"
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


proc tkchat::ChangeFont {opt val} {
    set ::Options(Font,$opt) $val
    foreach font [font names] {
	font configure $font $opt $val
    }
}

proc tkchat::Init {} {
    global Options env
    catch {set Options(BROWSER) $env(BROWSER)}
    catch {set Options(NETSCAPE) $env(NETSCAPE)}
    set ::URLID 0
    # set intial defaults
    set ::tkchat::pause 0
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
	Refresh		30
	NickList	{}
	History		{}
	AutoScroll	0
        Geometry        600x500+0+0
	Font,-family	Helvetica
	Font,-size	-12
	MaxLines	500
        ChatLogFile     ""
        LogFile		""
        LogLevel        info
        errLog		stderr
        hideTraffic     0
        TimeFormat      "At the tone, the time is %H:%M on %A %d %b %Y"
        TimeGMT         0
        HistoryLines    -1
    }
    set Options(URL)	$::tkchat::HOST/cgi-bin/chat.cgi
    set Options(URL2)	$::tkchat::HOST/cgi-bin/chat2.cgi
    set Options(URLlogs) $::tkchat::HOST/tchat/logs
    foreach {name clr} { MainBG FFFFFF MainFG 000000 } {
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

    # build screen
    CreateGUI
    .txt tag config TRAFFIC -elide $Options(hideTraffic)
    
    if {$Options(UseProxy)} {
	::http::config -proxyhost $Options(ProxyHost) \
              -proxyport $Options(ProxyPort)
    }
    ::tkchat::ChangeFont -family $Options(Font,-family)
    ::tkchat::ChangeFont -size $Options(Font,-size)

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

