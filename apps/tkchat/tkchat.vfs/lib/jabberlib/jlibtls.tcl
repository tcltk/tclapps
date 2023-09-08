#  jlibtls.tcl --
#  
#      This file is part of the jabberlib. It provides support for the
#      tls network socket security layer.
#      
#  Copyright (c) 2004  Mats Bengtsson
#  
# $Id: jlibtls.tcl,v 1.5 2005/02/16 14:26:46 matben Exp $

package require tls

package provide jlibtls 1.0


namespace eval jlib { }

proc jlib::starttls {jlibname cmd args} {
    
    upvar ${jlibname}::locals locals
    
    Debug 2 "jlib::starttls"

    set locals(tls,uargs) $args
    set locals(tls,cmd) $cmd
    
    # Set up callbacks for elements that are of interest to us.
    element_register $jlibname failure [namespace current]::tls_failure
    element_register $jlibname proceed [namespace current]::tls_proceed

    if {[info exists locals(features)]} {
	tls_continue $jlibname
    } else {
	
	# Must be careful so this is not triggered by a reset or something...
	trace add variable ${jlibname}::locals(features) write \
	  [list [namespace current]::tls_features_write $jlibname]
    }
}

proc jlib::tls_features_write {jlibname name1 name2 op} {
    
    Debug 2 "jlib::tls_features_write"
    
    trace remove variable ${jlibname}::locals(features) write \
      [list [namespace current]::tls_features_write $jlibname]
    tls_continue $jlibname
}

proc jlib::tls_continue {jlibname} {
    
    upvar ${jlibname}::locals locals
    variable xmppxmlns

    Debug 2 "jlib::tls_continue"
    
    # Must verify that the server provides a 'starttls' feature.
    if {![info exists locals(features,starttls)]} {
	tls_finish $jlibname starttls-nofeature
    }
    set xmllist [wrapper::createtag starttls -attrlist [list xmlns $xmppxmlns(tls)]]
    send $jlibname $xmllist
    
    # Wait for 'failure' or 'proceed' element.
}

proc jlib::tls_proceed {jlibname tag xmllist} {    

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    upvar ${jlibname}::lib lib
    variable xmppxmlns
    
    Debug 2 "jlib::tls_proceed"
    
    if {[wrapper::getattribute $xmllist xmlns] != $xmppxmlns(tls)} {
	tls_finish $jlibname starttls-protocolerror \
	  "received incorrectly namespaced proceed element"
    }

    set sock $lib(sock)

    # Make it a SSL connection.
    array set a [list -cafile "" -certfile "" -keyfile "" \
                     -request 1 -server 0 -require 0 -ssl2 no -ssl3 yes -tls1 yes]
    array set a $locals(tls,uargs)
    eval [linsert [array get a] 0 ::tls::import $sock]
    #tls::import $sock -cafile "" -certfile "" -keyfile "" \
    #  -request 1 -server 0 -require 0 -ssl2 no -ssl3 yes -tls1 yes
    set retry 0
    
    while {1} {
	if {$retry > 20} {
	    catch {close $sock}
	    set err "too long retry to setup SSL connection"
	    tls_finish $jlibname startls-failure $err
	}
	if {[catch {tls::handshake $sock} err]} {
	    if {[string match "*resource temporarily unavailable*" $err]} {
		after 50  
		incr retry
	    } else {
		catch {close $sock}
		tls_finish $jlibname startls-failure $err
	    }
	} else {
	    break
	}
    }
    
    wrapper::reset $lib(wrap)
    
    # We must clear out any server info we've received so far.
    stream_reset $jlibname
    
    set xml "<stream:stream\
      xmlns='$opts(-streamnamespace)' xmlns:stream='$xmppxmlns(stream)'\
      to='$locals(server)' xml:lang='[getlang]' version='1.0'>"

    # The tls package resets the encoding to: -encoding binary
    fconfigure $sock -encoding utf-8
    eval $lib(transportsend) {$xml}

    # Must be careful so this is not triggered by a reset or something...
    trace add variable ${jlibname}::locals(features) write \
      [list [namespace current]::tls_features_write_2nd $jlibname]
    
    return {}
}

proc jlib::tls_features_write_2nd {jlibname name1 name2 op} {
    
    Debug 2 "jlib::tls_features_write_2nd"
    
    trace remove variable ${jlibname}::locals(features) write \
      [list [namespace current]::tls_features_write_2nd $jlibname]
    
    tls_finish $jlibname
}

proc jlib::tls_failure {jlibname tag xmllist} {

    upvar ${jlibname}::locals locals
    variable xmppxmlns

    Debug 2 "jlib::tls_failure"
    
    if {[wrapper::getattribute $xmllist xmlns] == $xmppxmlns(tls)} {
	tls_finish $jlibname startls-failure "tls failed"
    } else {
	tls_finish $jlibname startls-failure "tls failed for an unknown reason"
    }
    return {}
}

proc jlib::tls_finish {jlibname {errcode ""} {msg ""}} {

    upvar ${jlibname}::locals locals
    
    Debug 2 "jlib::tls_finish errcode=$errcode, msg=$msg"

    element_deregister $jlibname failure [namespace current]::tls_failure
    element_deregister $jlibname proceed [namespace current]::tls_proceed
    
    if {$errcode != ""} {
	uplevel #0 $locals(tls,cmd) $jlibname [list error [list $errcode $msg]]
    } else {
	uplevel #0 $locals(tls,cmd) $jlibname [list result {}]
    }
}

# jlib::tls_reset --
# 
# 

proc jlib::tls_reset {jlibname} {
    
    upvar ${jlibname}::locals locals

    trace remove variable ${jlibname}::locals(features) write \
      [list [namespace current]::tls_features_write $jlibname]
}

#-------------------------------------------------------------------------------
