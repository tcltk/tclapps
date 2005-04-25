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
package require htmlparse	; # tcllib 1.0
package require log		; # tcllib

package provide chat 0.2

namespace eval chat {
    # set the configuration options
    array set Options {
	Username	"ircbridge"
	Password	"YOU MUST SET THIS UP IN ~/.ircbridge"
	KillPassword    "shut up and die"
	URL		http://mini.net/cgi-bin/chat.cgi
        URL2		http://mini.net/cgi-bin/chat2.cgi
        URLchatter	http://mini.net/cgi-bin/chatter2.cgi
	MyColor		000000
	Refresh		20
	ChatLogFile	""
	LogLevel	debug
	errLog		stderr
	timeout		30000
    }

    variable chatter_cgi ""
    variable chatter_stamp 0
    
    variable IgnoreList
    if {![info exists IgnoreList]} { set IgnoreList [list] }
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
    ::log::log debug "Send to $Options(URL)"
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
        ::log::log notice "Retrying msgSend after $delay: $msg"
        after $delay [list chat::msgSend $str $user]
    }
}

proc chat::msgDone {tok} {
    variable Options
    ::log::log debug "Post: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[::http::ncode $tok] == 500} {
                if {[info exists Options(msgSend_Retry)]} {
                    set msg "Posting error: retry failed: [::http::code $tok]"
                    ::log::log error $msg
                    unset Options(msgSend_Retry)
                } else {
                    ::log::log error "Post error: [::http::code $tok] - need to retry"
                    set Options(msgSend_Retry) 1
                    # FRINK: nocheck
                    after idle [list ::http::geturl $Options(URL) \
                                      -query [set ${tok}(-query)] \
                                      -command chat::msgDone]
                }
            } else {
                checkForRedirection $tok URL
                if {[catch {fetchPage} err]} { ::log::log error "fetchPage: $err" }
            }
        }
	reset { ::log::log warning "User reset post operation" }
	timeout { ::log::log warning "Message Post timed out" }
	error {
	    ::log::log error "Message Post Errored: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc chat::logonChat {{retry 0}} {
    variable Options
    ::log::log notice "Logon to $Options(URL2) as $Options(Username)"
    set qry [::http::formatQuery \
                   action       login \
                   name         $Options(Username) \
                   password     $Options(Password)]

    ::http::geturl $Options(URL2) \
	-query $qry \
	-command chat::logonDone
}

proc chat::logonDone {tok} {
    ::log::log notice "Logon: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL2]} {
                ::http::cleanup $tok
                logonChat 1
                return
            }
	    startChatterCheck

            if {[catch {pause off} err]} { ::log::log error "pause off: $err" }
            ::chat::DoAnim
        }
	reset	{ ::log::log warning "User reset logon operation" }
	timeout	{ ::log::log error "Logon timed out" }
	error	{ ::log::log error "Logon Error: [::http::error $tok]" }
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
    ::log::log debug "Logoff: status was [::http::status $tok][::http::code $tok]"
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
	    ::log::log notice "The session is paused"
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

    ::log::log debug "fetchPage from $Options(URL)"

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
        ::log::log error "Fetch error: $msg"
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
            ::log::log info "Fetch Command finished with token $tok expected $Options(FetchToken)"
            unset Options(FetchToken)
        }
    }
    if {!$::chat::pause} {
	set Options(FetchTimerID) \
              [after [expr {$Options(Refresh) * 1000}] chat::fetchPage]
    }
    ::log::log debug "Fetch: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok - OK - Ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                fetchPage
                return
            }
	    if {[catch {parseData [::http::data $tok]} err]} { ::log::log error "parseData: $err" }
	}
	reset - Reset - RESET {
	    ::log::log warning "Reset called while updating the chat page."
	}
	timeout - Timeout - TIMEOUT {
	    ::log::log warning "Timeout occurred while updating the chat page."
	}
	error - Error - ERROR {
	    ::log::log error "fetchPage error: [::http::error $tok]"
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
        ::log::log error "Fetch error: $msg"
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
            ::log::log info "Online Command finished with token $tok expected $Options(OnlineToken)"
            unset Options(OnlineToken)
        }
    }
    if {!$::chat::pause} {
	set Options(OnlineTimerID) \
              [after [expr {$Options(Refresh) * 1000}] chat::onlinePage]
    }
    ::log::log debug "Online: status was [::http::status $tok] [::http::code $tok]"
    switch -- [::http::status $tok] {
	ok {
            if {[checkForRedirection $tok URL]} {
                ::http::cleanup $tok
                onlinePage
                return
            }
	}
	reset {
	    ::log::log warning "Reset called while retrieving the online page."
	}
	timeout {
	    ::log::log warning "Retrieval of the online users information timed out."
	}
	error {
	    ::log::log error "onlinePage error: [::http::error $tok]"
	}
    }
    ::http::cleanup $tok
}

proc chat::parseData {rawHTML} {
    if {[regexp -nocase -- {<BODY[^>]*>(.*)</BODY>} [string map {\n ""} $rawHTML] -> body]} {
        regsub -all -nocase {<B>}   $body "\000" body
        regsub -all -nocase {<BR>}  $body "\n"   body
        regsub -all -nocase {<.*?>} $body ""     body
	set dataList [split $body "\n"]
	# remove empty lines
	while {[set pos [lsearch $dataList {}]] > -1} {
            set dataList [lreplace $dataList $pos $pos]
        }
        #puts "[join $dataList \n]"
        #puts ""
        #puts ""
	set newList [getRecentLines $dataList]
	addNewLines $newList
    } else {
	::log::log error "No BODY found in HTML page"
    }
}

proc chat::compareList {a b} {
    foreach x $a y $b {
        if {![string equal $x $y]} {return 0}
    }
    return 1
}

proc chat::getRecentLines {input} {
    variable Options
    set a [expr {[llength $input] - 3}]
    set b [expr {$a + 2}]
    set end [lrange $Options(History) end-2 end]
    while {$a >= 0} {
        if {[compareList $end [lrange $input $a $b]]} {
            return [lrange $input [expr {$b + 1}] end]
        }
        incr a -1
        incr b -1
    }
    return $input
}

proc chat::addNewLines {input} {
    variable Options
    variable InitialDump
    variable RE

    # Add the input to the history.  It's OK to do this before processing.
    eval [list lappend Options(History)] $input

    # Restrict the History size
    # 200 lines should be plenty
    set Options(History) [lrange $Options(History) end-199 end]

    # This is needed so that the initial dump of stuff doesn't get
    # output to IRC.
    if { [info exists InitialDump] } {
	unset InitialDump
	return
    }

    set last {}
    foreach line $input {
        set line [::htmlparse::mapEscapes $line]
        ::log::log debug "new line: '$line'"
        if {[regexp -nocase -- $RE(Whisper) $line -> nick line]} {
	    if { [string match "*hey $Options(KillPassword)*" $line] } {
		exit
	    }
            if {$nick == $Options(Username)} continue
            set last {addWhisper $nick $line}
        } elseif {[regexp -nocase -- $RE(Message) $line -> nick line]} {
            if {$nick == "tick" || $nick == $Options(Username)} continue
            set last {addMessage $nick $line}
        } elseif {[regexp -nocase -- $RE(Help) $line -> nick line]} {
            set last {addHelp $nick $line}
        } elseif {[regexp -nocase -- $RE(Action) $line -> nick line]} {
            if {$nick == $Options(Username)} continue
            set last {addAction $nick $line}
        } elseif {[regexp -nocase -- $RE(Traffic) $line -> nick line]} {
            if {$nick == $Options(Username)} continue
            set last {addTraffic $nick $line}
        }
        eval $last
    }
}

proc chat::Init {} {
    variable Options
    variable InitialDump 0
    variable pause 0
    variable RE
    global env
    array set Options {
	FetchTimerID	-1
	OnlineTimerID	-1
	History		{}
    }
    catch {unset Options(FetchToken)}
    catch {unset Options(OnlineToken)}

    if { [info exists ::chatPassword] } {
	set Options(Password) $::chatPassword
    }
    if { [info exists ::KillPassword] } {
	set Options(KillPassword) $::KillPassword
    }

    array set RE {
        Message {^\000(\S+?): (.+)$}
        Whisper {^\000\[([^\]]+)\] (.+)$}
        Help    {^\000\[(.+?)\]\s+(.*)$}
        Action  {^\000\*\s+(\S+) (.+)$}
        Traffic {^\000(\S+)\s+has (entered|left) the chat$}
    }

    # set up logging
    set fh $Options(errLog)
    if {$fh != "stderr" && $fh != "stdout"} {
        if {[catch {open $Options(errLog) a} fh]} {
            puts stderr "Could not open log file $Options(errLog): $fh"
            exit
        }
    }
    ::log::lvChannelForall $fh
    ::log::lvSuppressLE emergency 0
    ::log::lvSuppressLE $Options(LogLevel)
    ::log::lvSuppress $Options(LogLevel) 0

    logonChat
}


proc chat::startChatterCheck {} {
    variable Options
    variable chatter_cgi
    if { $chatter_cgi ne "" } {
	fileevent $chatter_cgi readable {}
	fileevent $chatter_cgi writable {}
	catch {
	    close $chatter_cgi 
	}
	set chatter_cgi ""
    }

    regexp {(.+)://([^/]+)(.+)} $Options(URLchatter) -> proto host path

    set chatter_cgi [socket -async $host 80]

    fileevent $chatter_cgi writable [list ::chat::verifyChatter $chatter_cgi]

}

proc chat::verifyChatter {s} {
    variable chatter_cgi
    variable chatter_stamp
    variable Options

    set msg [fconfigure $s -error]
    if {$msg ne ""} {	
	close $s
	if { $chatter_cgi eq $s } {
	    set chatter_cgi ""
	}
	after 20000 ::chat::startChatterCheck
	::log::log debug "verifyChatter failed: $msg"

    }
    if { [catch {fconfigure $s -peername}] } {
	close $s
	if { $chatter_cgi eq $s } {
	    set chatter_cgi ""
	}

	::log::log debug "verifyChatter failed: $::errorInfo"
	# Wait a short while and retry
	after 20000 ::chat::startChatterCheck	
    }
    
    fconfigure $s -blocking 0 -buffering line

    regexp {(.+)://([^/]+)(.+)} $Options(URLchatter) -> proto host path
    
    ::log::log debug "verifyChatter: Asking for stamp $chatter_stamp"

    puts $s "GET $path?$chatter_stamp\nHost: $host\nUser-Agent: IrcBridge\n\n"

    fileevent $s writable {}
    fileevent $s readable ::chat::readChatter

    ::log::log debug "verifyChatter: waiting..."

}

proc chat::readChatter {} {
    variable chatter_cgi
    variable chatter_stamp
    variable Options
    
    set d $chatter_stamp
    while { ![eof $chatter_cgi] && $d ne "" } {
	set d [gets $chatter_cgi]
	if { $d ne "" } {
	    set chatter_stamp $d
	}
    }
    if { [eof $chatter_cgi] } {
	::log::log debug "readChatter: EOF!"
	close $chatter_cgi
	set chatter_cgi ""	
	after 1000 startChatterCheck
    }

    ::log::log debug "readChatter: '$chatter_stamp'"

    fetchPage
}
