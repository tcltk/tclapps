#!/bin/sh
# the next line restarts using tclsh \
    exec tclsh "$0" "$@"

###########################################################
#
# author: Bruce B Hartweg brhartweg@bigfoot.com
# updates: Jeff Hobbs, et al
#
# Modified heavily by David Welton <davidw@dedasys.com> for use as
# 'ircbridge'.
#
# This program is free to use, modify, extend at will,
# the author(s) provides no warantees, guarantees
# or any responsibility for the use, re-use, abuse
# that may or may not happen. If you somehow sell
# this and make a ton of money - good for you, how
# about sending me some?
############################################################

package require http		; # core Tcl
package require textutil	; # tcllib 1.0
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib

package provide chat

namespace eval ::chat {
    variable MessageHooks
    array set MessageHooks {}

    # this is http://mini.net - but that recently had a dns problem
    variable HOST http://purl.org/mini
    variable DEBUG 1
}

proc chat::vputs {args} {
    variable DEBUG
    if {$DEBUG} {
	set name [lindex [info level -1] 0]
	if {[llength $args]} {
	    log::log debug "$name: $args"
	} else {
	    log::log debug "CALLED $name"
	}
    }
}

proc chat::errLog {args} {
    log::logMsg [join $args]
    update idletasks
}

# Check the HTTP response for redirecting URLs. - PT
proc chat::checkForRedirection {tok optionName} {
    variable Options
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

proc chat::msgSend {str {user ""}} {
    variable Options
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
              -command chat::msgDone
    } msg]} {
        set delay [expr {$Options(Refresh) * 1000 / 2}]
        errLog "Retrying msgSend after $delay: $msg"
        after $delay [list chat::msgSend $str $user]
    }
}

proc chat::msgDone {tok} {
    variable Options
    errLog "Post: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[::http::ncode $tok] == 500} {
                if {[info exists Options(msgSend_Retry)]} {
                    set msg "Posting error: retry failed: [::http::code $tok]"
                    log::log error $msg
                    unset Options(msgSend_Retry)
                } else {
                    log::log error "Post error: [::http::code $tok] - need to retry"
                    set Options(msgSend_Retry) 1
                    # FRINK: nocheck
                    after idle [list ::http::geturl $Options(URL) \
                                      -query [set ${tok}(-query)] \
                                      -command chat::msgDone]
                }
            } else {
                checkForRedirection $tok URL
                if {[catch {fetchPage} err]} { errLog "fetchPage: $err" }
            }
        }
	reset { errLog "User reset post operation" }
	timeout { errLog "Message Post timed out" }
	error {
	    log::log error "Message Post Errored: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc chat::logonChat {{retry 0}} {
    variable Options
    errLog "Logon to $Options(URL2)"
    set qry [::http::formatQuery \
                   action       login \
                   name         $Options(Username) \
                   password     $Options(Password)]

    http::geturl $Options(URL2) \
	-query $qry \
	-command chat::logonDone
}

proc chat::logonDone {tok} {
    errLog "Logon: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL2]} {
                ::http::cleanup $tok
                logonChat 1
                return
            }

            if {[catch {pause off} err]} { errLog $err }
            ::chat::DoAnim
        }
	reset	{ errLog "User reset logon operation" }
	timeout	{ log::log error "Logon timed out" }
	error	{ log::log error "Logon Error: [::http::error $tok]" }
    }
    ::http::cleanup $tok
}

proc chat::logoffChat {} {
    variable Options
    set qry [::http::formatQuery action gotourl url chat.cgi]
    ::http::geturl $Options(URL2) \
          -query $qry \
          -command chat::logoffDone
#    logonScreen
}

proc chat::logoffDone {tok} {
    errLog "Logoff: status was [::http::status $tok][::http::code $tok]"
    # don't really care if this works or not
    ::http::cleanup $tok
}

proc chat::pause {pause {notify 1}} {
    variable Options
    set ::chat::pause [string is true -strict $pause]
    if {$pause} {
	after cancel $Options(FetchTimerID)
	after cancel $Options(OnlineTimerID)
	catch {::http::reset $Options(FetchToken)}
	catch {::http::reset $Options(OnlineToken)}
	if {$notify} {
	    log::log notice "The session is paused"
	}
    } else {
	fetchPage
	onlinePage
    }
}

proc chat::fetchPage {} {
    variable Options

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
                                       -command chat::fetchDone]
    } msg]} {
        # If the http connection failed and we caught it then we probably
        # are not connected to the network. Keep trying - maybe we are moving
        # our laptop or something :)
        errLog "Fetch error: $msg"
        if {!$::chat::pause} {
            set Options(FetchTimerID) \
                  [after [expr {$Options(Refresh) * 1000}] chat::fetchPage]
        }
    }
}

proc chat::fetchDone {tok} {
    variable Options
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
    if {!$::chat::pause} {
	set Options(FetchTimerID) \
              [after [expr {$Options(Refresh) * 1000}] chat::fetchPage]
    }
    errLog "Fetch: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                fetchPage
                return
            }
	    if {[catch {parseData [::http::data $tok]} err]} { errLog "parseData: $err" }
	}
	reset - Reset - RESET {
	    errLog "Reset called while updating the chat page."
	}
	timeout - Timeout - TIMEOUT {
	    errLog "Timeout occurred while updating the chat page."
	}
	error - Error - ERROR {
	    errLog "fetchPage error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc chat::onlinePage {} {
    variable Options
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
                                        -command chat::onlineDone]
    } msg]} {
        errLog "Fetch error: $msg"
        if {!$::chat::pause} {
            set Options(FetchTimerID) \
                  [after [expr {$Options(Refresh) * 1000}] chat::onlinePage]
        }
    }
}

proc chat::onlineDone {tok} {
    variable Options
    if {[info exists Options(OnlineToken)]} {
        if {[string equal $tok $Options(OnlineToken)]} {
            unset Options(OnlineToken)
        } else {
            errLog "Online Command finished with token $tok" \
                  "expected $Options(OnlineToken)"
            unset Options(OnlineToken)
        }
    }
    if {!$::chat::pause} {
	set Options(OnlineTimerID) \
              [after [expr {$Options(Refresh) * 1000}] chat::onlinePage]
    }
    errLog "Online: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                onlinePage
                return
            }
	}
	reset {
	    errLog "Reset called while retrieving the online page."
	}
	timeout {
	    errLog "Retrieval of the online users information timed out."
	}
	error {
	    errLog "onlinePage error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}


proc chat::parseData {rawHTML} {
    variable Options
    # get body of data
    set clr ""
    if {[regexp -nocase -- \
               {<BODY.*?(?:BGColor=.([[:xdigit:]]{6}?))?>(.*?)<A\s+NAME="end">.*?</BODY>} \
               $rawHTML -> clr body]} {
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

proc chat::getRecentLines {input} {
    variable Options
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

proc chat::addNewLines {input} {
    variable Options
    variable InitialDump
    global RE UserClicked

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
    set UserInfoCmd [list]

    # This is needed so that the initial dump of stuff doesn't get
    # output to IRC.

    if { $InitialDump == 0 } {
	set InitialDump 1
	return
    }

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
		if { $InitialDump == 1 } {
		    addMessage $nick [join $msgLines \n]
		}
	    } else {
		lappend msgLines [string trimright $line]
	    }
	} else {
            log::log debug $line

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
		set msgLines [string trimright $str]
	    } elseif {[regexp -nocase -- $RE(Message) $line -> nick str]} {
		if { $InitialDump == 1 } {
		    addMessage $nick [join [string trim $str]]
		}
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
}

proc chat::stripStr {str} {
    # remove any remaining tags
    regsub -all -nocase "<.*?>" $str {} tmp
    # replace html escapes with real chars
    return [::htmlparse::mapEscapes $tmp]
}

proc chat::parseStr {str} {
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

proc chat::Hook {do type cmd} {
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

proc chat::findExecutable {progname varname} {
    upvar 1 $varname result
    set progs [auto_execok $progname]
    if {[llength $progs]} {
	set result [lindex $progs 0]
    }
    return [llength $progs]
}

proc chat::formatClock {str} {
    variable Options
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

proc chat::addUnknown {str} {
    variable Options
}

proc chat::Init {} {
    variable Options
    variable InitialDump 0
    global env
    set ::URLID 0
    # set intial defaults
    set ::chat::pause 0
    set ::chat::eCURR 0
    set ::chat::eHIST ""
    array set Options {
	Username	"ircbridge"
	Password	"ponte"
	SavePW		0
	MyColor		000000
	FetchTimerID	-1
	OnlineTimerID	-1
	AutoConnect	0
	Refresh		20
	NickList	{}
	History		{}
	AutoScroll	0
	MaxLines	500
	ChatLogFile	""
	LogFile		""
	LogLevel	info
	errLog		stderr
	hideTraffic	0
	TimeFormat	"At the tone, the time is %H:%M on %A %d %b %Y"
	TimeGMT		0
	HistoryLines	-1
	timeout		30000
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
    set Options(URL)	$::chat::HOST/cgi-bin/chat.cgi
    set Options(URL2)	$::chat::HOST/cgi-bin/chat2.cgi
    set Options(URLlogs) $::chat::HOST/tchat/logs

    set Options(Offset) 50
    catch {unset Options(FetchToken)}
    catch {unset Options(OnlineToken)}
    set Options(History) {}
    set Options(OnLineUsers) {}

    logonChat
}
