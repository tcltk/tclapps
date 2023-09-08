#  saslmd5.tcl --
#  
#       This package provides a rudimentary implementation of the client side
#       SASL authentication method using the DIGEST-MD5 mechanism.
#       SASL [RFC 2222]
#       DIGEST-MD5 [RFC 2831]
#       
#       It also includes the PLAIN mechanism, so saslmd5 is a misnomer.
#       
#  Copyright (c) 2004  Mats Bengtsson
#  BSD license
#  
# $Id: saslmd5.tcl,v 1.5 2005/04/03 10:42:04 matben Exp $

package require base64
package require md5 2.0

package provide saslmd5 1.0


namespace eval saslmd5 {
    
    # These are in order of preference.
    variable mechanisms [list "DIGEST-MD5" "PLAIN"]
    variable needed {username authzid pass realm}
    variable uid 0
    
    set seed [expr {abs([pid]+[clock clicks]%100000)}]
    expr {srand(int($seed))}
}

# "static" methods.

proc saslmd5::mechanisms {} {
    variable mechanisms   
    return $mechanisms
}

proc saslmd5::info {args} {
    
    # empty
    return {}
}

proc saslmd5::client_init {args} {
    
    # empty
}

proc saslmd5::decode64 {str} {
    return [::base64::decode $str]
}

proc saslmd5::encode64 {str} {
    
    # important! no whitespace allowed in response!
    return [string map [list "\n" ""] [::base64::encode $str]]
}

# saslmd5::client_new --
#
#       Create a new instance for a session.
#
# Arguments:
#	args    -callbacks    {{id proc} ...} with id any of 
#	                      {username authzid pass realm}
#	                      note that everyone must be utf-8 encoded!
#	        -service      name of service (xmpp)
#	        -serverFQDN   servers fully qualified domain name
#	        -flags        not used
#	
# Results:
#       token.

proc saslmd5::client_new {args} {    
    variable uid
    
    #puts "saslmd5::client_new"
    set token [namespace current]::[incr uid]
    variable $token
    upvar 0 $token state

    set state(step)       0
    set state(service)    ""
    set state(serverFQDN) ""
    set state(flags)      {}
    
    foreach {key value} $args {
	switch -- $key {
	    -callbacks {
		set_callbacks $token $value
	    }
	    -service - -serverFQDN - -flags {
		set state([string trimleft $key -]) $value
	    }
	    default {
		return -code error "unrocognized option \"$key\""
	    }
	}
    }
    
    proc $token {cmd args}   \
      "eval [namespace current]::cmdproc {$token} \$cmd \$args"

    return $token
}

proc saslmd5::cmdproc {token cmd args} {

    # Which command? Just dispatch the command to the right procedure.
    return [eval {method_$cmd $token} $args]
}

# class methods.

# saslmd5::method_start --
#
#       Starts negotiating.
#
# Arguments:
#       token       
#       args        -mechanisms {list of mechanisms}
#	
# Results:
#       {returnCode list-or-error}.

proc saslmd5::method_start {token args} {
    variable $token
    upvar 0 $token state
    variable mechanisms
    
    #puts "saslmd5::method_start $args"
    set state(step) 0
    
    foreach {key value} $args {
	switch -- $key {
	    -mechanisms {
		set state(inmechanisms) $value
	    }
	    default {
		# empty
	    }
	}
    }
    if {![::info exists state(inmechanisms)]} {
	return [list 1 "missing a \"-mechanisms\" option"]
    }

    # we must have at least on of the servers announced mechanisms
    set match 0
    foreach m $mechanisms {
	if {[set idx [lsearch -exact $state(inmechanisms) $m]] >= 0} {
	    set match 1
	    set mechanism [lindex $state(inmechanisms) $idx]
	    break
	}
    }
    if {!$match} {
	return [list 1 "the servers mechanisms \"$state(inmechanisms)\"\
	  do not match any of the supported mechanisms \"$mechanisms\""]
    }
    set state(step) 1
    
    switch -- $mechanism {
	PLAIN {
	    set output [get_plain_output $token]
	}
	DIGEST-MD5 {
	    set output ""
	}
    }
	    
    # continue
    return [list 4 [list mechanism $mechanism output $output]]
}

proc saslmd5::get_plain_output {token} {
    variable $token
    upvar 0 $token state
    
    # SENT: <auth 
    #           xmlns="urn:ietf:params:xml:ns:xmpp-sasl"
    #           mechanism="PLAIN">
    #               somelongstring
    #       </auth>
    # where somelongstring is (from Pandion's .js src):
    #   /* Plaintext algorithm:
    #    * Base64( UTF8( Addr ) + 0x00 + UTF8( User ) + 0x00 + UTF8( Pass ) )
    #	 */
    # User is the username, Addr is the full JID, and Pass is the password.

    request_userpars $token
    
    set username $state(upar,username)
    set pass     $state(upar,pass)
    set realm    $state(upar,realm)

    set user_lat1  [encoding convertto iso8859-1 $username]
    set pass_lat1  [encoding convertto iso8859-1 $pass]
    set realm_lat1 [encoding convertto iso8859-1 $realm]
    
    set jid [jlib::joinjid $user_lat1 $realm_lat1 ""]
    return [binary format a*xa*xa* $jid $user_lat1 $pass_lat1]
}

# saslmd5::method_step --
#
#       Takes one step when negotiating.
#
# Arguments:
#       token       
#       args        -input challenge
#	
# Results:
#       {returnCode list-or-error}.

proc saslmd5::method_step {token args} {
    variable $token
    upvar 0 $token state
    
    #puts "saslmd5::method_step $token, $args"
    foreach {key value} $args {
	switch -- $key {
	    -input {
		set challenge $value
	    }
	}
    }
    if {![::info exists challenge]} {
	return [list 1 "must have -input challenge string"]
    }
    
    if {$state(step) == 0} {
	return [list 1 "need to call the 'start' procedure first"]
    } elseif {$state(step) == 1} {
	if {![iscapable $token]} {	
	    return [list 1 "missing one or more callbacks"]
	}
	array set challarr [parse_challenge $challenge]
	if {![::info exists challarr(nonce)]} {
	    return [list 1 "challenge missing 'nonce' attribute"]
	}
	if {![::info exists challarr(algorithm)]} {
	    return [list 1 "challenge missing 'algorithm' attribute"]
	}
	request_userpars $token
	set output [process_challenge $token [array get challarr]]
	incr state(step)
	
	# continue
	set code 4
    } else {
	incr state(step)
	
	# success
	set output ""
	set code 0
    }    
    return [list $code $output]
}

proc saslmd5::method_setprop {token property value} {
    variable $token
    upvar 0 $token state

    # empty
}

proc saslmd5::method_getprop {token property} {
    variable $token
    upvar 0 $token state

    # empty
    return ""
}

proc saslmd5::method_info {args} {
    
    # empty
    return {}
}

proc saslmd5::set_callbacks {token cblist} {
    variable $token
    upvar 0 $token state
        
    # some of tclsasl's id's are different from the spec's!
    # note that everyone must be utf-8 encoded!
    foreach cbpair $cblist {
	foreach {id cbproc} $cbpair {
	    set state(cb,$id) $cbproc
	}
    }
}

proc saslmd5::iscapable {token} {
    variable $token
    upvar 0 $token state
    variable needed

    set capable 1
    foreach id $needed {
	if {[::info exists state(cb,$id)] && ($state(cb,$id) != {})} {
	    # empty
	} else {
	    set capable 0
	    break
	}
    }
    return $capable
}

# saslmd5::request_userpars --
# 
#       Invokes the needed callbacks to get user's parameters.

proc saslmd5::request_userpars {token} {
    variable $token
    upvar 0 $token state
    variable needed

    foreach id $needed {
	if {[::info exists state(cb,$id)] && ($state(cb,$id) != {})} {
	    set plist [list id $id]
	    set state(upar,$id) [uplevel #0 $state(cb,$id) [list $plist]]
	} else {
	    return -code error "missing one or more callbacks"
	}
    }
}

# saslmd5::process_challenge --
# 
#       Computes an output from a challenge using user's parameters.
#
# Arguments:
#       token       
#       challenge
#
# Results:
#       the output string as clear text.

proc saslmd5::process_challenge {token challenge} {
    variable $token
    upvar 0 $token state
    
    array set charr $challenge
    
    # users parameters
    set username $state(upar,username)
    set authzid  $state(upar,authzid)
    set pass     $state(upar,pass)
    set realm    $state(upar,realm)
    
    set host     $state(serverFQDN)
    set service  $state(service)
        
    # make a 'cnonce'
    set bytes ""
    for {set n 0} {$n < 32} {incr n} {
	set r [expr {int(256*rand())}]
	append bytes [binary format c $r]
    }
    set cnonce [encode64 $bytes]
    
    # other
    set realm   $host
    set nonce   $charr(nonce)
    set nc      "00000001"
    set diguri  $service/$host
    set qop     "auth"
        
    # build 'response' (2.1.2.1   Response-value in RFC 2831)
    # try to be a bit general here (from Cyrus SASL)
    # 
    # encoding is a bit unclear. 
    # from RFC 2831:
    #   If "charset=UTF-8" is present, and all the characters of either 
    #   "username-value" or "passwd" are in the ISO 8859-1 character set, 
    #   then it must be converted to ISO 8859-1 before being hashed.
    # 
    # from Cyrus SASL:
    #   if the string is entirely in the 8859-1 subset of UTF-8, then translate
    #   to 8859-1 prior to MD5

    set user_lat1  [encoding convertto iso8859-1 $username]
    set realm_lat1 [encoding convertto iso8859-1 $realm]
    set pass_lat1  [encoding convertto iso8859-1 $pass]
    set secret     ${user_lat1}:${realm_lat1}:${pass_lat1}
    set secretmd5  [::md5::md5 $secret]
    set A1         ${secretmd5}:${nonce}:${cnonce}
    if {$authzid != ""} {
	append A1 :${authzid}
    }
    set A2         AUTHENTICATE:${diguri}
    if {$qop != "auth"} {
	append A2 ":00000000000000000000000000000000"
    }
    set HA1        [string tolower [::md5::md5 -hex $A1]]
    set HA2        [string tolower [::md5::md5 -hex $A2]]
    set KD         ${HA1}:${nonce}
    if {$qop != ""} {
	append KD :${nc}:${cnonce}:${qop}:${HA2}
    }
    set response   [string tolower [::md5::md5 -hex $KD]]
    
    # build output
    set output ""
    append output "username=\"$username\""
    append output ",realm=\"$realm\""
    append output ",nonce=\"$nonce\""
    append output ",cnonce=\"$cnonce\""
    append output ",nc=\"$nc\""
    append output ",serv-type=\"$service\""
    append output ",host=\"$host\""
    append output ",digest-uri=\"$diguri\""
    append output ",qop=\"$qop\""
    append output ",response=\"$response\""
    append output ",charset=\"utf-8\""
    if {$authzid != ""} {
	append output ",authzid=\"$authzid\""
    }
    return $output
}

# saslmd5::parse_challenge --
# 
#       Parses a clear text challenge string into a challenge list.

proc saslmd5::parse_challenge {str} {
    if {0} {
        # PT: this stuff fails when there are spaces between the pairs.
    # this takes a bit of low level processing...
    # 'split' does not work here since = may be used inside quotes.
    set challenge {}
    set idx 0
    while {1} {
	set n [string first = $str $idx]
	if {$n == -1} break
	set key [string range $str $idx [expr $n-1]]
	set idx [expr $n+1]
	if {[string index $str $idx] == "\""} {
	    incr idx
	    set n [string first "\"" $str $idx]
	    if {$n == -1} break
	    set value [string range $str $idx [expr $n-1]]
	    set idx [incr n]
	} else {
	    set n [string first , $str $idx]
	    if {$n == -1} {
		set value [string range $str $idx end]
		set idx [expr [string length $str] - 1]
	    } else {
		set value [string range $str $idx [expr $n-1]]
		set idx $n
	    }
	}
	lappend challenge $key $value
	if {[string index $str $idx] != ","} break
	incr idx
    }
    return $challenge
}
    set challenge $str
    # This is from tcllib's SASL code:
    set sep "\\\]\\\[\\\\()<>@,;:\\\"\\\?=\\\{\\\} \t"
    set tok {0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\-\|\~\!\#\$\%\&\*\+\.\^\_\`}
    set sqot {(?:\'(?:\\.|[^\'\\])*\')}
    set dqot {(?:\"(?:\\.|[^\"\\])*\")}
    set parameters {}
    regsub -all "(\[${tok}\]+)=(${dqot}|(?:\[${tok}\]+))(?:\[${sep}\]+|$)" $challenge {\1 \2 } parameters
    return $parameters

}

proc saslmd5::free {token} {
    variable $token
    upvar 0 $token state
    
    unset -nocomplain state
}

