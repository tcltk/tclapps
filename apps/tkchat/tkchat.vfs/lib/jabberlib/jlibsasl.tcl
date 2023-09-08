#  jlibsasl.tcl --
#  
#      This file is part of the jabberlib. It provides support for the
#      sasl authentication layer via the tclsasl package or the saslmd5
#      pure tcl package.
#      
#  Copyright (c) 2004-2005  Mats Bengtsson
#  
# $Id: jlibsasl.tcl,v 1.14 2005/05/22 10:03:24 matben Exp $

# We need to be flexible here since can have cyrus based sasl or our 
# own special pure tcl saslmd5.

if {0 && ![catch {package require sasl 1.0}]} {
    set ::_saslpack cyrussasl
} elseif {0 && ![catch {package require saslclient 1.1}]} {
    set ::_saslpack cyrussasl
} elseif {![catch {package require saslmd5}]} {
    set ::_saslpack saslmd5
} else {
    return -code error "no sasl package found"
}

package provide jlibsasl 1.0


namespace eval jlib {
    variable cyrussasl 
    
    if {$::_saslpack == "cyrussasl"} {
	set cyrussasl 1
    } else {
	set cyrussasl 0
    }
    unset ::_saslpack
}

proc jlib::sasl_init {} {
    variable cyrussasl 
    
    if {$cyrussasl} {
	sasl::client_init -callbacks \
	  [list [list log [namespace current]::sasl_log]]
    } else {
	# empty
    }
}

proc jlib::decode64 {str} {
    variable cyrussasl 

    if {$cyrussasl} {
	return [sasl::decode64 $str]
    } else {
	return [saslmd5::decode64 $str]
    }
}

proc jlib::encode64 {str} {
    variable cyrussasl 

    if {$cyrussasl} {
	return [sasl::encode64 $str]
    } else {
	return [saslmd5::encode64 $str]
    }
}

# jlib::auth_sasl --
# 
# 

proc jlib::auth_sasl {jlibname username resource password cmd} {
    
    upvar ${jlibname}::locals locals
    
    Debug 2 "jlib::auth_sasl"
        
    # Cache our login jid.
    set locals(username) $username
    set locals(resource) $resource
    set locals(password) $password
    set locals(myjid2)   ${username}@$locals(server)
    set locals(myjid)    ${username}@$locals(server)/${resource}
    set locals(sasl,cmd) $cmd
    
    # Set up callbacks for elements that are of interest to us.
    element_register $jlibname challenge [namespace current]::sasl_challenge
    element_register $jlibname failure   [namespace current]::sasl_failure
    element_register $jlibname success   [namespace current]::sasl_success

    if {[info exists locals(features,mechanisms)]} {
	auth_sasl_continue $jlibname
    } else {
	element_register $jlibname features [namespace current]::sasl_features
    }
}

proc jlib::sasl_features {jlibname tag xmllist} {

    upvar ${jlibname}::locals locals

    Debug 2 "jlib::sasl_features"

    element_deregister $jlibname features [namespace current]::sasl_features
    
    # Verify that sasl is supported before going on.
    set features [get_features $jlibname "mechanisms"]
    if {$features == ""} {
	set msg "no sasl mechanisms announced by the server"
	sasl_final $jlibname error [list {} $msg]
    } else {
	auth_sasl_continue $jlibname
    }
}

# jlib::auth_sasl_continue --
# 
#       We respond to the 
#       <stream:features>
#           <mechanisms ...>
#               <mechanism>DIGEST-MD5</mechanism>
#               <mechanism>PLAIN</mechanism>
#            ...

proc jlib::auth_sasl_continue {jlibname} {
    
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals
    variable xmppxmlns
    variable cyrussasl 

    Debug 2 "jlib::auth_sasl_continue"
    
    if {$cyrussasl} {

	# TclSASL's callback id's seem to be a bit mixed up.
	foreach id {authname user pass getrealm} {
	    lappend callbacks [list $id [list [namespace current]::sasl_callback \
	      $jlibname]]
	}
	set sasltoken [sasl::client_new \
	  -service xmpp -serverFQDN $locals(server) -callbacks $callbacks \
	  -flags success_data]
    } else {
	
	# The saslmd5 package follow the naming convention in RFC 2831
	foreach id {username authzid pass realm} {
	    lappend callbacks [list $id [list [namespace current]::saslmd5_callback \
	      $jlibname]]
	}
	set sasltoken [saslmd5::client_new \
	  -service xmpp -serverFQDN $locals(server) -callbacks $callbacks \
	  -flags success_data]
    }
    set lib(sasl,token) $sasltoken
    
    if {$cyrussasl} {
	$sasltoken -operation setprop -property sec_props \
	  -value {min_ssf 0 max_ssf 0 flags {noplaintext}}
    } else {
	$sasltoken setprop sec_props {min_ssf 0 max_ssf 0 flags {noplaintext}}
    }
    
    # Returns a serialized array if succesful.
    if {$cyrussasl} {
	set code [catch {
	    $sasltoken -operation start -mechanisms $locals(features,mechanisms) \
	      -interact [list [namespace current]::sasl_interact $jlibname]
	} out]
    } else {
	set ans [$sasltoken start -mechanisms $locals(features,mechanisms)]
	set code [lindex $ans 0]
	set out  [lindex $ans 1]
    }
    Debug 2 "\t -operation start: code=$code, out=$out"
    
    switch -- $code {
	0 {	    
	    # ok
	    array set outArr $out
	    set xmllist [wrapper::createtag auth \
	      -attrlist [list xmlns $xmppxmlns(sasl) mechanism $outArr(mechanism)] \
	      -chdata [encode64 $outArr(output)]]
	    send $jlibname $xmllist
	}
	4 {	    
	    # continue
	    array set outArr $out
	    set xmllist [wrapper::createtag auth \
	      -attrlist [list xmlns $xmppxmlns(sasl) mechanism $outArr(mechanism)] \
	      -chdata [encode64 $outArr(output)]]
	    send $jlibname $xmllist
	}
	default {
	    # This is an error
	    # We should perhaps send an abort element here.
	    sasl_final $jlibname error [list {} $out]
	}
    }
}

proc jlib::sasl_interact {jlibname data} {
    
    # empty
}

# jlib::sasl_callback --
# 
#       TclSASL's callback id's seem to be a bit mixed up.

proc jlib::sasl_callback {jlibname data} {
    
    upvar ${jlibname}::locals locals

    array set arr $data
    
    switch -- $arr(id) {
	authname {
	    # username
	    set value [encoding convertto utf-8 $locals(username)]
	}
	user {
	    # authzid
	    set value [encoding convertto utf-8 $locals(myjid2)]
	}
	pass {
	    set value [encoding convertto utf-8 $locals(password)]
	}
	getrealm {
	    set value [encoding convertto utf-8 $locals(server)]
	}
	default {
	    set value ""
	}
    }
    return $value
}

# jlib::saslmd5_callback --
# 
#       The saslmd5 package follow the naming convention in RFC 2831.

proc jlib::saslmd5_callback {jlibname data} {
    
    upvar ${jlibname}::locals locals

    array set arr $data
    
    switch -- $arr(id) {
	username {
	    set value [encoding convertto utf-8 $locals(username)]
	}
	pass {
	    set value [encoding convertto utf-8 $locals(password)]
	}
	authzid {
	    
	    # xmpp-core sect. 6.1:
	    # As specified in [SASL], the initiating entity MUST NOT provide an
	    # authorization identity unless the authorization identity is
	    # different from the default authorization identity derived from
	    # the authentication identity as described in [SASL].
	    
	    #set value [encoding convertto utf-8 $locals(myjid2)]
	    set value ""
	}
	realm {
	    set value [encoding convertto utf-8 $locals(server)]
	}
	default {
	    set value ""
	}
    }
    Debug 2 "jlib::saslmd5_callback id=$arr(id), value=$value"
    
    return $value
}

proc jlib::sasl_challenge {jlibname tag xmllist} {
    variable xmppxmlns
    
    Debug 2 "jlib::sasl_challenge"
    
    if {[wrapper::getattribute $xmllist xmlns] == $xmppxmlns(sasl)} {
	sasl_step $jlibname [wrapper::getcdata $xmllist]
    }
    return {}
}

proc jlib::sasl_step {jlibname serverin64} {
    
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals
    variable xmppxmlns
    variable cyrussasl

    set serverin [decode64 $serverin64]
    Debug 2 "jlib::sasl_step, serverin=$serverin"
    
    # Note that 'step' returns the output if succesful, not a serialized array!
    if {$cyrussasl} {
	set code [catch {
	    $lib(sasl,token) -operation step -input $serverin \
	      -interact [list [namespace current]::sasl_interact $jlibname]
	} output]
    } else {
	foreach {code output} [$lib(sasl,token) step -input $serverin] {break}
    }
    Debug 2 "\t code=$code \n\t output=$output"
    
    switch -- $code {
	0 {	    
	    # ok
	    set xmllist [wrapper::createtag response \
	      -attrlist [list xmlns $xmppxmlns(sasl)] \
	      -chdata [encode64 $output]]
	    send $jlibname $xmllist
	}
	4 {	    
	    # continue
	    set xmllist [wrapper::createtag response \
	      -attrlist [list xmlns $xmppxmlns(sasl)] \
	      -chdata [encode64 $output]]
	    send $jlibname $xmllist
	}
	default {
	    #puts "\t errdetail: [$lib(sasl,token) -operation errdetail]"
	    sasl_final $jlibname error [list {} $output]
	}
    }
}

proc jlib::sasl_failure {jlibname tag xmllist} {
    
    upvar ${jlibname}::locals locals
    variable xmppxmlns

    Debug 2 "jlib::sasl_failure"
    
    if {[wrapper::getattribute $xmllist xmlns] == $xmppxmlns(sasl)} {
	set errelem [lindex [wrapper::getchildren $xmllist] 0]
	#puts "\t errelem=$errelem"
	if {$errelem == ""} {
	    set errmsg "not-authorized"
	} else {
	    set errtag [wrapper::gettag $errelem]
	    set errmsg [sasl_getmsg $errtag]
	}
	sasl_final $jlibname error [list {} $errmsg]
    }
    return {}
}

proc jlib::sasl_success {jlibname tag xmllist} {
    
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    variable xmppxmlns

    Debug 2 "jlib::sasl_success"
    if {[wrapper::getattribute $xmllist xmlns] != $xmppxmlns(sasl)} {
	return
    }
    
    # xmpp-core sect 6.2:
    # Upon receiving the <success/> element,
    # the initiating entity MUST initiate a new stream by sending an
    # opening XML stream header to the receiving entity (it is not
    # necessary to send a closing </stream> tag first...
    
    wrapper::reset $lib(wrap)
    
    # We must clear out any server info we've received so far.
    stream_reset $jlibname
    
    set xml "<stream:stream\
      xmlns='$opts(-streamnamespace)' xmlns:stream='$xmppxmlns(stream)'\
      to='$locals(server)' xml:lang='[getlang]' version='1.0'>"

    eval $lib(transportsend) {$xml}
    
    # Must be careful so this is not triggered by a reset or something...
    trace add variable ${jlibname}::locals(features) write \
      [list [namespace current]::auth_sasl_features_write $jlibname]
    
    return {}
}

proc jlib::auth_sasl_features_write {jlibname name1 name2 op} {
    
    upvar ${jlibname}::locals locals

    trace remove variable ${jlibname}::locals(features) write \
      [list [namespace current]::auth_sasl_features_write $jlibname]

    bind_resource $jlibname $locals(resource) \
      [namespace current]::resource_bind_cb
}

proc jlib::resource_bind_cb {jlibname type subiq} {
    
    upvar ${jlibname}::locals locals
    variable xmppxmlns
    
    switch -- $type {
	error {
	    sasl_final $jlibname error $subiq
	}
	default {
	    
	    # Establish the session.
	    set xmllist [wrapper::createtag session \
	      -attrlist [list xmlns $xmppxmlns(session)]]
	    send_iq $jlibname set [list $xmllist] -command \
	      [list [namespace current]::send_session_cb $jlibname]
	}
    }
}

proc jlib::send_session_cb {jlibname type subiq args} {

    upvar ${jlibname}::locals locals
    
    sasl_final $jlibname $type $subiq
}

proc jlib::sasl_final {jlibname type subiq} {
    
    upvar ${jlibname}::locals locals
    
    Debug 2 "jlib::sasl_final"

    # We are no longer interested in these.
    element_deregister $jlibname challenge [namespace current]::sasl_challenge
    element_deregister $jlibname failure   [namespace current]::sasl_failure
    element_deregister $jlibname success   [namespace current]::sasl_success
    element_deregister $jlibname features  [namespace current]::sasl_features

    uplevel #0 $locals(sasl,cmd) [list $jlibname $type $subiq]
}

proc jlib::sasl_log {args} {
    
    Debug 2 "SASL: $args"
}

proc jlib::sasl_reset {jlibname} {
    
    upvar ${jlibname}::locals locals

    foreach tspec [trace info variable ${jlibname}::locals(features,mechanisms)] {
	foreach {op cmd} $tspec {break}
	trace remove variable ${jlibname}::locals(features,mechanisms) $op $cmd
    }
    foreach tspec [trace info variable ${jlibname}::locals(features)] {
	foreach {op cmd} $tspec {break}
	trace remove variable ${jlibname}::locals(features) $op $cmd
    }
}

namespace eval jlib {
    
    # This maps Defined Conditions to clear text messages.
    # draft-ietf-xmpp-core23; 6.4 Defined Conditions
    
    variable saslmsg
    array set saslmsg {
    aborted             {The receiving entity acknowledges an abort
       element sent by the initiating entity.}
    incorrect-encoding  {The data provided by the initiating
       entity could not be processed because the [BASE64] encoding is
       incorrect.}
    invalid-authzid     {The authzid provided by the initiating
       entity is invalid, either because it is incorrectly formatted or
       because the initiating entity does not have permissions to
       authorize that ID.}
    invalid-mechanism   {The initiating entity did not provide a
       mechanism or requested a mechanism that is not supported by the
       receiving entity.}
    mechanism-too-weak  {The mechanism requested by the initiating
       entity is weaker than server policy permits for that initiating
       entity.}
    not-authorized      {The authentication failed because the
       initiating entity did not provide valid credentials (this includes
       but is not limited to the case of an unknown username).}
    temporary-auth-failure {The authentication failed because of
       a temporary error condition within the receiving entity.}
   }
}

proc jlib::sasl_getmsg {condition} {
    variable saslmsg
    
    if {[info exists saslmsg($condition)]} {
	return $saslmsg($condition)
    } else {
	return $condition
    }
}

#-------------------------------------------------------------------------------
