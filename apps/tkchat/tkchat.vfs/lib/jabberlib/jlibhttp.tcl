#  jlibhttp.tcl ---
#  
#      Provides a http transport mechanism for jabberlib. 
#      
#  Copyright (c) 2002-2004  Mats Bengtsson
#
# $Id: jlibhttp.tcl,v 1.2 2004/10/18 14:01:04 matben Exp $
# 
# USAGE ########################################################################
#
# jlib::http::new jlibname url ?-key value ...?
#	url 	A valid url for the POST method of HTTP.
#	
#	-command callback	a tcl procedure of the form 'callback {status message}
#	                        only errors are reported
#       -keylength              sets the length of the key sequence
#	-maxpollms ms	        max interval in ms for post requests
#	-minpollms ms	        min interval in ms for post requests
#       -proxyhost domain	name of proxu host if any
#       -proxyport integer	and its port number
#	-proxyusername name	your username for the proxy
#	-proxypasswd secret	and your password
#	-resendinterval ms	if sending fails, try again after this interval
#	-timeout ms		timeout for connecting the server
#	-usekeys 0|1            if keys should be used
#
# Callbacks for the JabberLib:
#	jlib::http::transportinit, jlib::http::transportreset, jlib::http::send
#
# ALGORITHM ####################################################################
#
# There are two parts that interact with each other:
#   1) the post process for send/receive
#   2) the poll process for receiving; this is actually an empty http post call
#   
# o Post with 'minpollms' and poll with 'maxpollms'.  
# o Every post cancel any scheduled poll and reschedules it
# o Every poll reschedules itself
# o Every poll and post keep tracks of a common 'lastsecs'
# o The first post triggers everything
# o When send:
#     if time - last < minpoll then just append xml to cache
#     else do post

# We should make this multiinstance in the future.

package require jlib
package require http 2.3
package require base64
package require sha1pure

package provide jlibhttp 0.1

namespace eval jlib::http {

    # All non-options should be in this array.
    variable priv

    variable debug 4
    variable errcode
    array set errcode {
	0       "unknown error"
       -1       "server error"
       -2       "bad request"
       -3       "key sequence error"
    }
}

# jlib::http::new --
#
#	Configures the state of this thing.

proc jlib::http::new {jlibname url args} {

    variable opts
    variable priv
    
    Debug 2 "jlib::http::new url=$url, args=$args"

    array set opts {
	-keylength             255
	-maxpollms           10000
	-minpollms            4000
	-proxyhost              ""
	-proxyport            8080
	-proxyusername          ""
	-proxypasswd            ""
	-resendinterval      20000
	-timeout                 0
	-usekeys                 0
	header                  ""
	proxyheader             ""
	url                     ""
    }
    if {![regexp -nocase {^(([^:]*)://)?([^/:]+)(:([0-9]+))?(/.*)?$} $url \
      x prefix proto host y port filepath]} {
	return -code error "The url \"$url\" is not valid"
    }
    set opts(jlibname)        $jlibname
    set opts(url)             $url
    array set opts $args

    set priv(id)      0
    set priv(postid)  ""
    set priv(pollid)  ""

    # Perhaps the autoproxy package can be used here?
    if {[string length $opts(-proxyhost)] && [string length $opts(-proxyport)]} {
   	::http::config -proxyhost $opts(-proxyhost) -proxyport $opts(-proxyport)
    }
    if {[string length $opts(-proxyusername)] || \
	[string length $opts(-proxypasswd)]} {
	set opts(proxyheader) [BuildProxyHeader  \
	  $opts(-proxyusername) $opts(-proxypasswd)]
    }
    set opts(header) $opts(proxyheader)
    
    # Initialize.
    transportreset

    set seed [expr {abs([pid]+[clock clicks]%100000)}]
    expr {srand(int($seed))}

    $jlibname registertransport \
      [namespace current]::transportinit \
      [namespace current]::send \
      [namespace current]::transportreset

    return ""
}

# jlib::http::BuildProxyHeader --
#
#	Builds list for the "Proxy-Authorization" header line.

proc jlib::http::BuildProxyHeader {proxyusername proxypasswd} {
    
    set str $proxyusername:$proxypasswd
    set auth [list "Proxy-Authorization" \
      "Basic [base64::encode [encoding convertto $str]]"]
    return $auth
}

proc jlib::http::NewSeed { } {
    
    set num [expr int(10000000 * rand())]
    return [format %0x $num]
}

proc jlib::http::NewKeySequence {seed len} {

    set keys    $seed
    set prevkey $seed
    
    for {set i 1} {$i < $len} {incr i} {
	
	# It seems that it is expected to have sha1 in binary format;
	# get from hex
	set hex [::sha1pure::sha1 $prevkey]
	set key [::base64::encode [binary format H* $hex]]
	lappend keys $key
	set prevkey $key
    }
    return $keys
}

# jlib::http::transportinit --
#
#	For the -transportinit command.

proc jlib::http::transportinit { } {

    variable priv
    variable opts

    set priv(postid)    ""
    set priv(pollid)    ""
    set priv(xml)       ""
    set priv(id)        0
    set priv(lastsecs)  [clock scan "1 hour ago"]
    if {$opts(-usekeys)} {
	
	# Use keys from the end.
	set priv(keys) [NewKeySequence [NewSeed] $opts(-keylength)]
	set priv(keyind) [expr [llength $priv(keys)] - 1]
    }
    transportreset
}

# jlib::http::transportreset --
#
#	For the -transportreset command.

proc jlib::http::transportreset { } {

    variable priv

    # Stop polling and resends.
    catch {after cancel $priv(resendid)}
    if {[string length $priv(postid)]} {
	catch {after cancel $priv(postid)}
    }
    if {[string length $priv(pollid)]} {
	catch {after cancel $priv(pollid)}
    }
    
    # Cleanup the keys.
    
}

# jlib::http::send --
#
#	For the -transportsend command.

proc jlib::http::send {xml} {
    
    variable opts
    variable priv
    
    Debug 2 "jlib::http::send"
    
    append priv(xml) $xml
    
    # Cancel any scheduled poll.
    if {[string length $priv(pollid)]} {
	after cancel $priv(pollid)
    }

    # If we don't have a scheduled post,
    # and time to previous post is larger than minumum, do post now.
    if {($priv(postid) == "") && \
      [expr [clock seconds] - $priv(lastsecs) > $opts(-minpollms)/1000.0]} {
	PostScheduled
    }
}

# jlib::http::PostScheduled --
# 
#       Just a wrapper for Post when sending xml.
       
proc jlib::http::PostScheduled { } {
    
    variable priv

    Debug 2 "jlib::http::PostScheduled"
    
    Post $priv(xml)
    set priv(xml) ""
    set priv(postid) ""
}

# jlib::http::Post --
# 
#       Do actual posting with (any) xml to send.
       
proc jlib::http::Post {xml} {
    
    variable opts
    variable priv

    if {$opts(-usekeys)} {
	
	# Administrate the keys.
	set key [lindex $priv(keys) $priv(keyind)]
	incr priv(keyind) -1

	# Need new key sequence?
	if {$priv(keyind) <= 1} {
	    set priv(keys) [NewKeySequence [NewSeed] $opts(-keylength)]
	    set priv(keyind) [expr [llength $priv(keys)] - 1]
	    set newkey [lindex $priv(keys) end]
	    set qry "$priv(id);$key;$newkey,$xml"
	} else {
	    set qry "$priv(id);$key,$xml"
	}
    } else {
	set qry "$priv(id),$xml"
    }
    Debug 2 "POST: $qry"
    
    # -query forces a POST request.
    # Make sure we send it as text dispite the application/* type.???
    # Add extra path /jabber/http ?
    if {[catch {
	set token [::http::geturl $opts(url)  \
	  -timeout $opts(-timeout) -query $qry -headers $opts(header) \
	  -command [namespace current]::Response]
    } msg]} {
	Debug 2 "\t post failed: $msg"
	#set priv(resendid) [after $opts(-resendinterval) \
	#  [namespace current]::send $xml]]
    } else {
	set priv(lastsecs) [clock seconds]
	
	# Reschedule next poll.
	Debug 4 "after $opts(-maxpollms) Poll"
	
	set priv(pollid) [after $opts(-maxpollms) \
	  [namespace current]::Poll]
    }
}

# jlib::http::Poll --
#
#	We need to poll the server at (regular?) intervals to see if it has
#	got something for us.

proc jlib::http::Poll { } {
    
    variable priv
    variable opts
    
    # Send an empty POST request. Verify that we've sent our stream start.
    if {![string equal $priv(id) "0"]} {
	Post ""
    }
    
    # Reschedule next poll.
    Debug 4 "after $opts(-maxpollms) Poll"
    set priv(pollid) [after $opts(-maxpollms) [namespace current]::Poll]]
}

# jlib::http::Response --
#
#	The response to our POST request. Parse any indata that should
#	be of mime type text/xml

proc jlib::http::Response {token} {

    upvar #0 $token state
    variable priv
    variable opts
    variable errcode
    
    # Trap any errors first.
    set status [::http::status $token]
    
    Debug 2 "jlib::http::Response status=$status, [::http::ncode $token]"
    Debug 2 "data='[::http::data $token]'"

    switch -- $status {
	ok {	    
	    if {[::http::ncode $token] != 200} {
		Finish error [::http::ncode $token]
		return
	    }
	    Debug 4 "[parray state(meta)]"
	    
	    set haveCookie 0
	    set haveContentType 0
	    
	    foreach {key value} $state(meta) {
		set lowkey [string tolower $key]
		if {[string equal $lowkey "set-cookie"]} {
		    
		    # Extract the 'id' from the Set-Cookie key.
		    if {![regexp -nocase {ID=([0-9a-zA-Z:\-]+);} $value m id]} {
			Finish error \
			  "Set-Cookie in HTTP header \"$value\" invalid"
			return
		    }
		    
		    # Invesitigate the ID.
		    set id2 [lindex [split $id :] end]
		    if {[string equal $id2 "0"]} {
			
			# Server error
			set code [lindex [split $id :] 0]
			if {[info exists errcode($code)]} {
			    set errmsg $errcode($code)
			} else {
			    set errmsg "Server error $id"
			}
			Finish error $errmsg
			return
		    }
		    set haveCookie 1
		} elseif {[string equal $lowkey "content-type"]} {
		    if {![string equal $value "text/xml"]} {
			# This is an invalid response.
			set errmsg "Content-Type in HTTP header is "
			append "\"$value\" expected \"text/xml\""
			Finish error $errmsg
			return
		    }
		    set haveContentType 1
		}
	    }
	    if {!$haveCookie} {
		Finish error "missing Set-Cookie in HTTP header"
		return
	    }
	    if {!$haveContentType} {
		Finish error "missing Content-Type in HTTP header"
		return
	    }
	    set priv(id) $id
	    
	    set body [::http::data $token]
	    Debug 2 "POLL: $body"
	    
	    # Send away to jabberlib for parsing and processing.
	    if {$body != ""} {
		[namespace parent]::recv $opts(jlibname) $body
	    }
	}
	default {
	    Finish $status [::http::error $token]
	}
    }
    parray $token
    
    # And cleanup after each post.
    ::http::cleanup $token
}

proc jlib::http::Finish {status {errmsg ""}} {
    
    variable opts

    Debug 2 "jlib::http::Finish status=$status, errmsg=$errmsg"
    if {[info exists opts(-command)]} {
	uplevel #0 $opts(-command) [list $status $errmsg]
    }
}

proc jlib::http::Debug {num str} {
    variable debug
    if {$num <= $debug} {
	puts $str
    }
}

#-------------------------------------------------------------------------------

