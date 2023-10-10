################################################################################
#
# This is JabberLib (abbreviated jlib), the Tcl library for 
# use in making Jabber clients.
# Is originally written by Kerem HADIMLI, with additions
# from Todd Bradley. 
# Completely rewritten from scratch by Mats Bengtsson.
# The algorithm for building parse trees has been completely redesigned.
# Only some structures and API names are kept essentially unchanged.
#
# $Id: jabberlib.tcl,v 1.99 2005/06/16 07:10:40 matben Exp $
# 
# Error checking is minimal, and we assume that all clients are to be trusted.
# 
# News: the transport mechanism shall be completely configurable, but where
#       the standard mechanism (put directly to socket) is included here.
#
# Variables used in JabberLib:
# 
# lib:
#	lib(wrap)                  : Wrap ID
#       lib(clientcmd)             : Callback proc up to the client
#       lib(rostername)            : the name of the roster object
#	lib(sock)                  : socket name
#	lib(streamcmd)             : Callback command to run when the <stream>
#	                             tag is received from the server.
#
# iqcmd:	                             
#	iqcmd(uid)                 : Next iq id-number. Sent in 
#                                    "id" attributes of <iq> packets.
#	iqcmd($id)                 : Callback command to run when iq result 
#	                             packet of $id is received.
#
# locals:
#       locals(server)             : The server domain name or ip number
#       locals(username)
#       locals(myjid)
#       locals(myjid2)
#	                               
############################# SCHEMA ###########################################
#
#                                         -> roster >-  
#                                        /            \ 
#                                       /              \
#   TclXML <---> wrapper <---> jabberlib <-----------> client
#                                       
#                                       
#   
#   Note the one-way communication with the 'roster' object since it may only
#   be set by the server, that is, from 'jabberlib'. 
#   The client only "gets" the roster.
#
############################# USAGE ############################################
#
#   NAME
#      jabberlib - an interface between Jabber clients and the wrapper
#      
#   SYNOPSIS
#      jlib::new rosterName clientCmd ?-opt value ...?
#      jlib::havesasl
#      jlib::havetls
#      
#   OPTIONS
#	-iqcommand            callback for <iq> elements not handled explicitly
#	-messagecommand       callback for <message> elements
#	-presencecommand      callback for <presence> elements
#	-streamnamespace      initialization namespace (D = "jabber:client")
#	-keepalivesecs        send a newline character with this interval
#	-autoawaymins         if > 0 send away message after this many minutes
#	-xautoawaymins        if > 0 send xaway message after this many minutes
#	-awaymsg              the away message 
#	-xawaymsg             the xaway message
#	-autodiscocaps        0|1 should presence caps elements be auto discoed
#	
#   INSTANCE COMMANDS
#      jlibName agent_get to cmd
#      jlibName agents_get to cmd
#      jlibName config ?args?
#      jlibName openstream server ?args?
#      jlibName closestream
#      jlibName element_deregister tag func
#      jlibName element_register tag func ?seq?
#      jlibName getstreamattr name
#      jlibName get_features name
#      jlibName get_last to cmd
#      jlibName get_time to cmd
#      jlibName get_version to cmd
#      jlibName getagent jid
#      jlibName getrecipientjid jid
#      jlibName haveagent jid
#      jlibName iq_get xmlns ?-to, -command, -sublists?
#      jlibName iq_set xmlns ?-to, -command, -sublists?
#      jlibName iq_register type xmlns cmd
#      jlibName message_register xmlns cmd
#      jlibName myjid
#      jlibName mystatus
#      jlibName oob_set to cmd url ?args?
#      jlibName presence_register type cmd
#      jlibName registertransport initProc sendProc resetProc
#      jlibName register_set username password cmd ?args?
#      jlibName register_get cmd ?args?
#      jlibName register_remove to cmd ?args?
#      jlibName resetstream
#      jlibName roster_get cmd
#      jlibName roster_set item cmd ?args?
#      jlibName roster_remove item cmd
#      jlibName schedule_auto_away
#      jlibName search_get to cmd
#      jlibName search_set to cmd ?args?
#      jlibName send_iq type xmldata ?args?
#      jlibName send_message to ?args?
#      jlibName send_presence ?args?
#      jlibName send_auth username resource ?args?
#      jlibName send xmllist
#      jlibName setsockettransport socket
#      jlibName vcard_get to cmd
#      jlibName vcard_set cmd ?args?
#      
#  o using the experimental 'conference' protocol:  OUTDATED!
#      jlibName conference get_enter room cmd
#      jlibName conference set_enter room subelements cmd
#      jlibName conference get_create server cmd
#      jlibName conference set_create room subelements cmd
#      jlibName conference delete room cmd
#      jlibName conference exit room
#      jlibName conference set_user room name jid cmd
#      jlibName conference hashandnick room
#      jlibName conference roomname room
#      jlibName conference allroomsin
#      
#      
#   The callbacks given for any of the '-iqcommand', '-messagecommand', 
#   or '-presencecommand' must have the following form:
#   
#      Callback {jlibName type args}
#      
#   where 'type' is the type attribute valid for each specific element, and
#   'args' is a list of '-key value' pairs. The '-iqcommand' returns a boolean
#   telling if any 'get' is handled or not. If not, then a "Not Implemented" is
#   returned automatically.
#                 
#   The clientCmd procedure must have the following form:
#   
#      clientCmd {jlibName what args}
#      
#   where 'what' can be any of: connect, disconnect, iqreply, message, xmlerror,
#   version, presence, networkerror, oob, away, xaway, .... Iq elements have
#   the what equal to the last namespace specifier.
#   'args' is a list of '-key value' pairs.
#      
############################# CHANGES ##########################################
#
#       0.*      by Kerem HADIMLI and Todd Bradley
#       1.0a1    complete rewrite, and first release by Mats Bengtsson
#       1.0a2    minor additions and fixes
#       1.0a3    added vCard, '-cmd' to 'connect', private_get/set
#       1.0b1    few bugfixes, added browse_get, agent_get
#       1.0b2    type attribute in send_message wrong
#       1.0b3    added support for conferencing, many rewrites
#       1.0b4    added time, last, version
#       1.0b5    added better error catching
#       1.0b6    added config and auto away support
#       1.0b7    fixed bug in send_message for x elements
#       1.0b8    fixed bug in send_iq if xmldata empty
#	1.0b9    added configurable transport layer, incompatible change
#		     of 'connect' command
#                placed debug printouts in one proc; access function for debug
#                added caching of agent(s) stuff
#                added a 'service' subcommand
#                added the old groupchat interface
#                added a 'conference' subcommand, removed conf_* methods
#                added -streamnamespace option
#                completely reworked client callback structure
#                'register_remove' is now an iq-set command, new 'to' argument
#       1.0b10   fixed a number of problems with groupchat-conference compatibility,
#                added presence callback
#       1.0b11   changed 'browse_get' command 
#                added 'mystatus' command, added 'setgroupchatpriority',
#                'setgroupchatprotocol' and reworked all groupchat protocol
#                dispatching.
#       030523   added 'getagent' and 'haveagent' commands.
#       030611   added 'setroomprotocol' command and modified service dispatching
#       030705   jlib::new generates self token
#       030726   made browse object optional, jlib::new api changed!
#       031022   added iq_get and iq_set methods
#       031101   added 'service gettransportjids' and 'gettype'
#       031107   added 'getrecipientjid' command
#       040111   new iq callback mechanism 'iq_register'
#       
#       04*      started with 2.0 version; 
#                removed all browse stuff, added presence_register, muc as a
#                standalone component, jlibname now always fully qualified,
#                connect -> openstream, disconnect -> closestream
#                
#       050201  all network errors handled via client command (clientcmd)
#               individual commands shall never throw network errors!
#       050215  reworked autoaway; api changes!

package require wrapper
package require roster
package require service
package require stanzaerror
package require streamerror
package require groupchat
package require caps
package require pubsub

package provide jlib 2.0

namespace eval jlib {
    
    # Globals same for all instances of this jlib.
    #    > 1 prints raw xml I/O
    #    > 2 prints a lot more
    variable debug 0
    if {[info exists ::debugLevel] && ($::debugLevel > 1) && ($debug == 0)} {
	set debug 2
    }
    
    variable statics
    set statics(inited) 0
    set statics(presenceTypeExp)  \
      {(available|unavailable|subscribe|unsubscribe|subscribed|unsubscribed|invisible)}
    
    variable version 1.0
    
    # Running number.
    variable uid 0
    
    # Some common xmpp xml namespaces.
    variable xmppxmlns
    array set xmppxmlns {
	stream      http://etherx.jabber.org/streams
	streams     urn:ietf:params:xml:ns:xmpp-streams
	tls         urn:ietf:params:xml:ns:xmpp-tls
	sasl        urn:ietf:params:xml:ns:xmpp-sasl
	bind        urn:ietf:params:xml:ns:xmpp-bind
	stanzas     urn:ietf:params:xml:ns:xmpp-stanzas
	session     urn:ietf:params:xml:ns:xmpp-session
    }
    
    variable jxmlns
    array set jxmlns {
	disco           http://jabber.org/protocol/disco 
	disco,items     http://jabber.org/protocol/disco#items 
	disco,info      http://jabber.org/protocol/disco#info
	caps            http://jabber.org/protocol/caps
	ibb             http://jabber.org/protocol/ibb
	amp             http://jabber.org/protocol/amp
	muc             http://jabber.org/protocol/muc
	muc,user        http://jabber.org/protocol/muc#user
	muc,admin       http://jabber.org/protocol/muc#admin
	muc,owner       http://jabber.org/protocol/muc#owner
	pubsub          http://jabber.org/protocol/pubsub
    }
    
    # Auto away and extended away are only set when the
    # current status has a lower priority than away or xa respectively.
    # After an idea by Zbigniew Baniewski.
    variable statusPriority
    array set statusPriority {
	chat            1
	available       2
	away            3
	xa              4
	dnd             5
	invisible       6
	unavailable     7
    }
}

# Collects the 'conference' subcommand.
namespace eval jlib::conference { }

# jlib::new --
#
#       This creates a new instance jlib interpreter.
#       
# Arguments:
#       rostername: the name of the roster object
#       clientcmd:  callback procedure for the client
#       args:       
#	-iqcommand            
#	-messagecommand       
#	-presencecommand      
#	-streamnamespace      
#	-keepalivesecs        
#	-autoawaymins              
#	-xautoawaymins             
#	-awaymsg              
#	-xawaymsg         
#	-autodiscocaps    
#       
# Results:
#       jlibname which is the namespaced instance command
  
proc jlib::new {rostername clientcmd args} {    

    variable statics
    variable objectmap
    variable uid
    
    # Generate unique command token for this jlib instance.
    # Fully qualified!
    set jlibname [namespace current]::jlib[incr uid]
      
    # Instance specific namespace.
    namespace eval $jlibname {
	variable lib
	variable locals
	variable iqcmd
	variable iqhook
	variable msghook
	variable preshook
	variable opts
	variable agent
	# Cache for the 'conference' subcommand.
	variable conf
    }
            
    # Set simpler variable names.
    upvar ${jlibname}::lib      lib
    upvar ${jlibname}::iqcmd    iqcmd
    upvar ${jlibname}::prescmd  prescmd
    upvar ${jlibname}::msgcmd   msgcmd
    upvar ${jlibname}::opts     opts
    upvar ${jlibname}::conf     conf
    upvar ${jlibname}::locals   locals
    
    array set opts {
	-iqcommand            ""
	-messagecommand       ""
	-presencecommand      ""
	-streamnamespace      "jabber:client"
	-keepalivesecs        60
	-autoawaymins         0
	-xautoawaymins        0
	-awaymsg              ""
	-xawaymsg             ""
	-autodiscocaps        0
    }
    
    # Verify options.
    if {[catch {eval jlib::verify_options $jlibname $args} msg]} {
	return -code error $msg
    }
    
    if {!$statics(inited)} {
	init
    }

    set wrapper [wrapper::new [list [namespace current]::got_stream $jlibname] \
      [list [namespace current]::end_of_parse $jlibname]  \
      [list [namespace current]::dispatcher $jlibname]    \
      [list [namespace current]::xmlerror $jlibname]]
    
    set iqcmd(uid)   1001
    set prescmd(uid) 1001
    set msgcmd(uid)  1001
    set lib(rostername)   $rostername
    set lib(clientcmd)    $clientcmd
    set lib(wrap)         $wrapper
    
    set lib(isinstream) 0
    
    init_inst $jlibname
            
    # Init conference and groupchat state.
    set conf(allroomsin) {}
    groupchat::init $jlibname
    if {$opts(-autodiscocaps)} {
	caps::init $jlibname
    }
    
    # Register some standard iq handlers that are handled internally.
    iq_register $jlibname get jabber:iq:last    \
      [namespace current]::handle_get_last
    iq_register $jlibname get jabber:iq:time    \
      [namespace current]::handle_get_time
    iq_register $jlibname get jabber:iq:version \
      [namespace current]::handle_get_version
    
    # Create the actual jlib instance procedure.
    proc $jlibname {cmd args}   \
      "eval jlib::cmdproc {$jlibname} \$cmd \$args"
    
    # Init the service layer for this jlib instance.
    service::init $jlibname
    
    return $jlibname
}

# jlib::init --
# 
#       Static initializations.

proc jlib::init {} {
    variable statics
    
    if {[catch {package require jlibsasl}]} {
	set statics(sasl) 0
    } else {
	set statics(sasl) 1
	sasl_init
    }
    if {[catch {package require jlibtls}]} {
	set statics(tls) 0
    } else {
	set statics(tls) 1
    }
    set statics(inited) 1
}

# jlib::init_inst --
# 
#       Instance specific initializations.

proc jlib::init_inst {jlibname} {

    upvar ${jlibname}::locals   locals
    
    # Any of {available chat away xa dnd invisible unavailable}
    set locals(status)        "unavailable"
    set locals(myjid)         ""
    set locals(trigAutoAway)  1
    set locals(server)        ""
}

# jlib::havesasl --
# 
#       Cache this info for effectiveness. It is needed at application level.

proc jlib::havesasl { } {
    variable statics
    
    if {![info exists statics(sasl)]} {
	if {[catch {package require jlibsasl}]} {
	    set statics(sasl) 0
	} else {
	    set statics(sasl) 1
	}
    }
    return $statics(sasl)
}

# jlib::havetls --
# 
#       Cache this info for effectiveness. It is needed at application level.

proc jlib::havetls { } {
    variable statics
    
    if {![info exists statics(tls)]} {
	if {[catch {package require jlibtls}]} {
	    set statics(tls) 0
	} else {
	    set statics(tls) 1
	}
    }
    return $statics(tls)
}

# jlib::cmdproc --
#
#       Just dispatches the command to the right procedure.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        openstream - closestream - send_iq - send_message ... etc.
#       args:       all args to the cmd procedure.
#       
# Results:
#       none.

proc jlib::cmdproc {jlibname cmd args} {
    
    Debug 5 "jlib::cmdproc: jlibname=$jlibname, cmd='$cmd', args='$args'"

    # Which command? Just dispatch the command to the right procedure.
    return [eval {$cmd $jlibname} $args]
}

# jlib::getrostername --
# 
#       Just returns the roster instance for this jlib instance.

proc jlib::getrostername {jlibname} {
    
    upvar ${jlibname}::lib lib
    
    return $lib(rostername)
}

# jlib::config --
#
#	See documentaion for details.
#
# Arguments:
#	args		Options parsed by the procedure.
#	
# Results:
#       depending on args.

proc jlib::config {jlibname args} {    

    upvar ${jlibname}::opts opts
    
    array set argsArr $args
    set options [lsort [array names opts -*]]
    set usage [join $options ", "]
    if {[llength $args] == 0} {
	set result {}
	foreach name $options {
	    lappend result $name $opts($name)
	}
	return $result
    }
    regsub -all -- - $options {} options
    set pat ^-([join $options |])$
    if {[llength $args] == 1} {
	set flag [lindex $args 0]
	if {[regexp -- $pat $flag]} {
	    return $opts($flag)
	} else {
	    return -code error "Unknown option $flag, must be: $usage"
	}
    } else {
	
	# Reschedule auto away only if changed. Before setting new opts!
	if {[info exists argsArr(-autoawaymins)] &&  \
	  ($argsArr(-autoawaymins) != $opts(-autoawaymins))} {
	    schedule_auto_away $jlibname
	}
	if {[info exists argsArr(-xautoawaymins)] &&  \
	  ($argsArr(-xautoawaymins) != $opts(-xautoawaymins))} {
	    schedule_auto_away $jlibname
	}
	foreach {flag value} $args {
	    if {[regexp -- $pat $flag]} {
		set opts($flag) $value		
	    } else {
		return -code error "Unknown option $flag, must be: $usage"
	    }
	}
    }
    if {[info exists argsArr(-autodiscocaps)]} {
	if {$opts(-autodiscocaps)} {
	    caps::init $jlibname
	} else {
	    caps::free $jlibname
	}
    }
    return ""
}

# jlib::verify_options
#
#	Check if valid options and set them.
#
# Arguments
# 
#	args    The argument list given on the call.
#
# Side Effects
#	Sets error

proc jlib::verify_options {jlibname args} {    

    upvar ${jlibname}::opts opts
    
    set validopts [array names opts]
    set usage [join $validopts ", "]
    regsub -all -- - $validopts {} theopts
    set pat ^-([join $theopts |])$
    foreach {flag value} $args {
	if {[regexp $pat $flag]} {
	    
	    # Validate numbers
	    if {[info exists opts($flag)] && \
	      [string is integer -strict $opts($flag)] && \
	      ![string is integer -strict $value]} {
		return -code error "Bad value for $flag ($value), must be integer"
	    }
	    set opts($flag) $value
	} else {
	    return -code error "Unknown option $flag, can be: $usage"
	}
    }
}

# jlib::registertransport --
# 
# 

proc jlib::registertransport {jlibname initProc sendProc resetProc} {
    
    upvar ${jlibname}::lib lib

    set lib(transportinit)  $initProc
    set lib(transportsend)  $sendProc
    set lib(transportreset) $resetProc
}

# jlib::setsockettransport --
# 
#       Sets the standard socket transport and the actual socket to use.

proc jlib::setsockettransport {jlibname sock} {
    
    upvar ${jlibname}::lib lib
    
    # Settings for the raw socket transport layer.
    set lib(sock) $sock
    set lib(transportinit)  [list [namespace current]::initsocket $jlibname]
    set lib(transportsend)  [list [namespace current]::putssocket $jlibname]
    set lib(transportreset) [list [namespace current]::resetsocket $jlibname]
}

# The procedures for the standard socket transport layer -----------------------

# jlib::initsocket
#
#	Default transport mechanism; init socket.
#
# Arguments:
# 
# Side Effects:
#	none

proc jlib::initsocket {jlibname} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::opts opts

    set sock $lib(sock)
    if {[catch {
	fconfigure $sock -blocking 0 -buffering none -encoding utf-8
    } err]} {
	return -code error "The connection failed or dropped later"
    }
     
    # Set up callback on incoming socket.
    fileevent $sock readable [list [namespace current]::recvsocket $jlibname]

    # Schedule keep-alives to keep socket open in case anyone want's to close it.
    # Be sure to not send any keep-alives before the stream is inited.
    if {$opts(-keepalivesecs)} {
	after [expr 1000 * $opts(-keepalivesecs)] \
	  [list [namespace current]::schedule_keepalive $jlibname]
    }
}

# jlib::putssocket
#
#	Default transport mechanism; put directly to socket.
#
# Arguments:
# 
#	xml    The xml that is to be written.
#
# Side Effects:
#	none

proc jlib::putssocket {jlibname xml} {

    upvar ${jlibname}::lib lib

    Debug 2 "SEND: $xml"
    if {[catch {puts -nonewline $lib(sock) $xml} err]} {
	# Error propagated to the caller that calls clientcmd.
	return -code error
    }
}

# jlib::resetsocket
#
#	Default transport mechanism; reset socket.
#
# Arguments:
# 
# Side Effects:
#	none

proc jlib::resetsocket {jlibname} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals

    catch {close $lib(sock)}
    catch {after cancel $locals(aliveid)}
}

# jlib::recvsocket --
#
#	Default transport mechanism; fileevent on socket socket.
#       Callback on incoming socket xml data. Feeds our wrapper and XML parser.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       
# Results:
#       none.

proc jlib::recvsocket {jlibname} {

    upvar ${jlibname}::lib lib

    if {[catch {eof $lib(sock)} iseof] || $iseof} {
	kill $jlibname
	uplevel #0 $lib(clientcmd) [list $jlibname networkerror]	  
	return
    }
    
    # Read what we've got.
    if {[catch {read $lib(sock)} temp]} {
	kill $jlibname

	# We need to call clientcmd here since async event.
	uplevel #0 $lib(clientcmd) [list $jlibname networkerror]	  
	return
    }
    Debug 2 "RECV: $temp"
    
    # Feed the XML parser. When the end of a command element tag is reached,
    # we get a callback to 'jlib::dispatcher'.
    wrapper::parse $lib(wrap) $temp
}

# standard socket transport layer end ------------------------------------------

# jlib::recv --
#
# 	Feed the XML parser. When the end of a command element tag is reached,
# 	we get a callback to 'jlib::dispatcher'.

proc jlib::recv {jlibname xml} {

    upvar ${jlibname}::lib lib

    wrapper::parse $lib(wrap) $xml
}

# jlib::openstream --
#
#       Initializes a stream to a jabber server. The socket must already 
#       be opened. Sets up fileevent on incoming xml stream.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       server:     the domain name or ip number of the server.
#       args:
#           -cmd    callback when we receive the <stream> tag from the server.
#           -to     the receipients jabber id.
#           -id
#           -version
#       
# Results:
#       none.

proc jlib::openstream {jlibname server args} {    

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    variable xmppxmlns

    array set argsArr $args
    set locals(server) $server
    set locals(last) [clock seconds]

    # Register a <stream> callback proc.
    if {[info exists argsArr(-cmd)] && [llength $argsArr(-cmd)]} {
	set lib(streamcmd) $argsArr(-cmd)
    }
    set optattr ""
    foreach {key value} $args {
	
	switch -- $key {
	    -cmd - -socket {
		# empty
	    }
	    default {
		set attr [string trimleft $key "-"]
		append optattr " $attr='$value'"
	    }
	}
    }

    if {[catch {

	# This call to the transport layer shall set up fileevent callbacks etc.
   	# to handle all incoming xml.
	eval $lib(transportinit)
        
    	# Network errors if failed to open connection properly are likely to show here.
	set xml "<?xml version='1.0' encoding='UTF-8'?><stream:stream\
	  xmlns='$opts(-streamnamespace)' xmlns:stream='$xmppxmlns(stream)'\
	  xml:lang='[getlang]' to='$server'$optattr>"

	eval $lib(transportsend) {$xml}
    } err]} {
	
	# The socket probably was never connected,
	# or the connection dropped later.
	closestream $jlibname
	return -code error "The connection failed or dropped later: $err"
    }
    return ""
}

# jlib::closestream --
#
#       Closes the stream down, closes socket, and resets internal variables.
#       There is a potential problem if called from within a xml parser 
#       callback which makes the subsequent parsing to fail. (after idle?)
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       
# Results:
#       none.

proc jlib::closestream {jlibname} {    

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::closestream"

    set xml "</stream:stream>"
    catch {eval $lib(transportsend) {$xml}}
    kill $jlibname
}

# jlib::kill --
# 
#       Like closestream but without any network transactions.

proc jlib::kill {jlibname} {
    
    upvar ${jlibname}::lib lib

    Debug 3 "jlib::kill"
    
    catch {eval $lib(transportreset)}
    reset $jlibname
    
    # Be sure to reset the wrapper, which implicitly resets the XML parser.
    wrapper::reset $lib(wrap)
}

# jlib::dispatcher --
#
#       Just dispatches the xml to any of the iq, message, or presence handlers,
#       which in turn dispatches further and/or handles internally.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       xmldata:    the complete xml as a hierarchical list.
#       
# Results:
#       none.

proc jlib::dispatcher {jlibname xmldata} {
    
    Debug 5 "jlib::dispatcher jlibname=$jlibname, xmldata=$xmldata"
    
    # Which method?
    set tag [wrapper::gettag $xmldata]
    
    switch -- $tag {
	iq {
	    iq_handler $jlibname $xmldata
	}
	message {
	    message_handler $jlibname $xmldata	    
	}
	presence {
	    presence_handler $jlibname $xmldata	
	}
	features {
	    features_handler $jlibname $xmldata
	    element_run_hook $jlibname $tag $xmldata
	}
	error {
	    error_handler $jlibname $xmldata
	}
	default {
	    element_run_hook $jlibname $tag $xmldata
	}
    }
}

# jlib::iq_handler --
#
#       Callback for incoming <iq> elements.
#       The handling sequence is the following:
#       1) handle all roster pushes (set) internally
#       2) handle all preregistered callbacks via id attributes
#       3) handle callbacks specific for 'type' and 'xmlns' that have been
#          registered with 'iq_register'
#       4) if unhandled by 3, use any -iqcommand callback
#       5) if still, use the client command callback
#       6) if type='get' and still unhandled, return an error element
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#	xmldata     the xml element as a list structure.
#	
# Results:
#       roster object set, callbacks invoked.

proc jlib::iq_handler {jlibname xmldata} {    

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::iqcmd iqcmd
    upvar ${jlibname}::opts opts    
    variable xmppxmlns

    Debug 5 "jlib::iq_handler: ------------"

    # Extract the command level XML data items.    
    set tag [wrapper::gettag $xmldata]
    array set attrArr [wrapper::getattrlist $xmldata]
    
    # Make an argument list ('-key value' pairs) suitable for callbacks.
    # Make variables of the attributes.
    set arglist {}
    foreach {key value} [array get attrArr] {
	set $key $value
	lappend arglist -$key $value
    }
    
    # The 'type' attribute must exist! Else we return silently.
    if {![info exists type]} {	
	return
    }
    if {![info exists from]} {
	set afrom ""
    } else {
	set afrom $from
    }
    
    # @@@ The child must be a single <query> element (or any namespaced element).
    # WRONG WRONG !!!!!!!!!!!!!!!
    set childlist [wrapper::getchildren $xmldata]
    set subiq [lindex $childlist 0]
    set xmlns [wrapper::getattribute $subiq xmlns]
    
    if {[string equal $type "error"]} {
	set callbackType "error"
    } elseif {[regexp {.*:([^ :]+)$} $xmlns match callbackType]} {
	# empty
    } else {
	set callbackType "iqreply"
    }
    set ishandled 0

    # (1) This is a server push! Handle internally.

    if {[string equal $type "set"]} {
	
	switch -- $xmlns {
	    jabber:iq:roster {
		
		# Found a roster-push, typically after a subscription event.
		# First, we reply to the server, saying that, we 
		# got the data, and accepted it. ???		    
		# We call the 'parse_roster_get', because this
		# data is the same as the one we get from a 'roster_get'.
		
		parse_roster_get $jlibname 1 {} ok $subiq
		#parse_roster_get $jlibname 1 {} set $subiq
		set ishandled 1
	    }
	}
    }
    
    # (2) Handle all preregistered callbacks via id attributes.
    #     Must be type 'result' or 'error'.
    #     Some components use type='set' instead of 'result'.

    switch -- $type {
	result - set {

	    # A request for the entire roster is coming this way, 
	    # and calls 'parse_roster_set'.
	    # $iqcmd($id) contains the 'parse_...' call as 1st element.
	    if {[info exists id] && [info exists iqcmd($id)]} {
		
		# TODO: add attrArr to callback.
		uplevel #0 $iqcmd($id) [list result $subiq]
		        
		#uplevel #0 $iqcmd($id) [list result $subiq] $arglist
		
		# The callback my in turn call 'closestream' which unsets 
		# all iq before returning.
		unset -nocomplain iqcmd($id)
		set ishandled 1
	    }
	}
	error {
	    set errspec [getstanzaerrorspec $xmldata]
	    if {[info exists id] && [info exists iqcmd($id)]} {
		uplevel #0 $iqcmd($id) [list error $errspec]
		
		#uplevel #0 $iqcmd($id) [list error $xmldata]
		
		unset -nocomplain iqcmd($id)
		set ishandled 1
	    }	    
	}
    }
	        
    # (3) Handle callbacks specific for 'type' and 'xmlns' that have been
    #     registered with 'iq_register'

    if {[string equal $ishandled "0"]} {
	set ishandled [eval {
	    iq_run_hook $jlibname $type $xmlns $afrom $subiq} $arglist]
    }
    
    # (4) If unhandled by 3, use any -iqcommand callback.

    if {[string equal $ishandled "0"]} {	
	if {[string length $opts(-iqcommand)]} {
	    set iqcallback [concat  \
	      [list $jlibname $type -query $subiq] $arglist]
	    set ishandled [uplevel #0 $opts(-iqcommand) $iqcallback]
	} 
	    
	# (5) If unhandled by 3 and 4, use the client command callback.

	if {[string equal $ishandled "0"]} {
	    set clientcallback [concat  \
	      [list $jlibname $callbackType -query $subiq] $arglist]
	    set ishandled [uplevel #0 $lib(clientcmd) $clientcallback]
	}

	# (6) If type='get' or 'set', and still unhandled, return an error element.

	if {[string equal $ishandled "0"] && \
	  ([string equal $type "get"] || [string equal $type "set"])} {
	    
	    # Return a "Not Implemented" to the sender. Just switch to/from,
	    # type='result', and add an <error> element.
	    if {[info exists attrArr(from)]} {
		set attrArr(to) $attrArr(from)
		unset attrArr(from)
		set attrArr(type) "error"
		set xmldata [wrapper::setattrlist $xmldata [array get attrArr]]
		
		set errstanza [wrapper::createtag "feature-not-implemented" \
		  -attrlist [list xmlns $xmppxmlns(stanzas)]]
		set errtag [wrapper::createtag "error" -subtags [list $errstanza] \
		  -attrlist {code 501 type cancel}]
		
		lappend childlist $errtag
		set xmldata [wrapper::setchildlist $xmldata $childlist]
		
		send $jlibname $xmldata
	    }
	}
    }
}
    
# jlib::message_handler --
#
#       Callback for incoming <message> elements. See 'jlib::dispatcher'.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#	xmldata     the xml element as a list structure.
#	
# Results:
#       callbacks invoked.

proc jlib::message_handler {jlibname xmldata} {    

    upvar ${jlibname}::opts opts    
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::msgcmd msgcmd
    
    # Extract the command level XML data items.
    set attrlist  [wrapper::getattrlist $xmldata]
    set childlist [wrapper::getchildren $xmldata]
    set attrArr(type) "normal"
    array set attrArr $attrlist
    set type $attrArr(type)
    
    # Make an argument list ('-key value' pairs) suitable for callbacks.
    # Make variables of the attributes.
    set arglist {}
    foreach {key value} [array get attrArr] {
	lappend arglist -$key $value
    }
    set ishandled 0
    
    switch -- $type {
	error {
	    set errspec [getstanzaerrorspec $xmldata]
	    lappend arglist -error $errspec
	}
    }
   
    # Extract the message sub-elements.
    set x {}
    set xxmlnsList {}
    foreach child $childlist {
	
	# Extract the message sub-elements XML data items.
	set ctag    [wrapper::gettag $child]
	set cchdata [wrapper::getcdata $child]
	
	switch -- $ctag {
	    body - subject - thread {
		lappend arglist -$ctag $cchdata
	    }
	    x {
		lappend x $child
		lappend xxmlnsList [wrapper::getattribute $child xmlns]
	    }
	}
    }
    
    # Invoke any registered handler for this message.
    set iscallback 0
    if {[info exists attrArr(id)]} {
	set id $attrArr(id)
	if {[info exists msgcmd($id)]} {
	    uplevel #0 $msgcmd($id) [list $jlibname $type] $arglist
	    unset -nocomplain msgcmd($id)
	    set iscallback 1
	}
    }	
    if {[llength $x]} {
	lappend arglist -x $x
	set xxmlnsList [lsort -unique $xxmlnsList]
	
	# Invoke any registered message handlers.
	foreach xxmlns $xxmlnsList {
	    set ishandled [eval {
		message_run_hook $jlibname $type $xxmlns} $arglist]
	    if {$ishandled} {
		break
	    }
	}
    }
    if {!$iscallback && [string equal $ishandled "0"]} {	
    
	# Invoke callback to client.
	if {[string length $opts(-messagecommand)]} {
	    uplevel #0 $opts(-messagecommand) [list $jlibname $type] $arglist
	} else {
	    uplevel #0 $lib(clientcmd) [list $jlibname message] $arglist
	}
    }
}
    
# jlib::presence_handler --
#
#       Callback for incoming <presence> elements. See 'jlib::dispatcher'.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#	xmldata     the xml element as a list structure.
#	
# Results:
#       roster object set, callbacks invoked.

proc jlib::presence_handler {jlibname xmldata} { 

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::prescmd prescmd
    upvar ${jlibname}::opts opts
    
    # Extract the command level XML data items.
    set attrlist  [wrapper::getattrlist $xmldata]
    set childlist [wrapper::getchildren $xmldata]
    array set attrArr $attrlist
    
    # Make an argument list ('-key value' pairs) suitable for callbacks.
    # Make variables of the attributes.
    set arglist {}
    set type "available"
    foreach {attrkey attrval} $attrlist {
	set $attrkey $attrval
	lappend arglist -$attrkey $attrval
    }
    
    # Check first if this is an error element (from conferencing?).
    if {[string equal $type "error"]} {
	set errspec [getstanzaerrorspec $xmldata]
	lappend arglist -error $errspec
    } else {
	
	# Extract the presence sub-elements. Separate the x element.
	set x {}
	set extras {}
	foreach child $childlist {
	    
	    # Extract the presence sub-elements XML data items.
	    set ctag [wrapper::gettag $child]
	    set cchdata [wrapper::getcdata $child]
	    
	    switch -- $ctag {
		status - priority - show {
		    lappend params $ctag $cchdata
		    lappend arglist -$ctag $cchdata
		}
		x {
		    lappend x $child
		}
		default {
		    
		    # This can be anything properly namespaced.
		    lappend extras $child
		}
	    }
	}	    
	if {[llength $x] > 0} {
	    lappend arglist -x $x
	}
	if {[llength $extras] > 0} {
	    lappend arglist -extras $extras
	}
	
	# Do different things depending on the 'type' attribute.
	if {[string equal $type "available"] ||  \
	  [string equal $type "unavailable"]} {
	    
	    # Not sure if we should exclude roster here since this
	    # is not pushed to us but requested.
	    # It must be set for presence sent to groupchat rooms!
	    
	    # Set presence in our roster object
	    eval {$lib(rostername) setpresence $from $type} $arglist
	} else {
	    
	    # We probably need to respond to the 'presence' element;
	    # 'subscribed'?. ????????????????? via lib(rostername)
	    # If we have 'unsubscribe'd another users presence it cannot be
	    # anything else than 'unavailable' anymore.
	    if {[string equal $type "unsubscribed"]} {
		$lib(rostername) setpresence $from "unsubscribed"
	    }
	    if {[string length $opts(-presencecommand)]} {
		uplevel #0 $opts(-presencecommand) [list $jlibname $type] $arglist
	    } else {
		uplevel #0 $lib(clientcmd) [list $jlibname presence] $arglist
	    }	
	}
    }
    
    # Invoke any callback before the rosters callback.
    if {[info exists id] && [info exists prescmd($id)]} {
	uplevel #0 $prescmd($id) [list $jlibname $type] $arglist
	unset -nocomplain prescmd($id)
    }	
    if {![string equal $type "error"]} {
	eval {$lib(rostername) invokecommand $from $type} $arglist
    }
    
    #     Handle callbacks specific for 'type' that have been
    #     registered with 'presence_register'

    eval {presence_run_hook $jlibname $from $type} $arglist
}

# jlib::features_handler --
# 
#       Callback for the <stream:features> element.

proc jlib::features_handler {jlibname xmllist} {

    upvar ${jlibname}::locals locals
    variable xmppxmlns
    
    Debug 4 "jlib::features_handler"
    
    foreach child [wrapper::getchildren $xmllist] {
	wrapper::splitxml $child tag attr chdata children
	
	switch -- $tag {
	    mechanisms {
		set mechanisms {}
		if {[wrapper::getattr $attr xmlns] eq $xmppxmlns(sasl)} {
		    foreach mechelem $children {
			wrapper::splitxml $mechelem mtag mattr mchdata mchild
			if {$mtag == "mechanism"} {
			    lappend mechanisms $mchdata
			}
		    }
		}

		# Variable that may trigger a trace event.
		set locals(features,mechanisms) $mechanisms
	    }
	    starttls {
		if {[wrapper::getattr $attr xmlns] eq $xmppxmlns(tls)} {
		    set locals(features,starttls) 1
		    set childs [wrapper::getchildswithtag $xmllist required]
		    if {$childs != ""} {
			set locals(features,starttls,required) 1
		    }
		}
	    }
	    default {
		set locals(features,$tag) 1
	    }
	}
    }
    
    # Variable that may trigger a trace event.
    set locals(features) 1
}

# jlib::get_features --
# 
#       Just to get access of the stream features.

proc jlib::get_features {jlibname name {name2 ""}} {
    
    upvar ${jlibname}::locals locals

    set ans ""
    if {$name2 != ""} {
	if {[info exists locals(features,$name,$name2)]} {
	    set ans $locals(features,$name,$name2)
	}
    } else {
	if {[info exists locals(features,$name)]} {
	    set ans $locals(features,$name)
	}
    }
    return $ans
}

# jlib::error_handler --
# 
#       Callback when receiving an stream:error element. According to xmpp-core
#       this is an unrecoverable error (4.7.1) and the stream MUST be closed
#       and the TCP connection also be closed.
#       
#       jabberd 1.4.3: <stream:error>Disconnected</stream:error>

proc jlib::error_handler {jlibname xmllist} {

    upvar ${jlibname}::lib lib
    variable xmppxmlns
    
    closestream $jlibname
    
    # Be sure to reset the wrapper, which implicitly resets the XML parser.
    wrapper::reset $lib(wrap)
    if {[llength [wrapper::getchildren $xmllist]]} {
	set errspec [getstreamerrorspec $xmllist]
    } else {
	set errspec [list unknown [wrapper::getcdata $xmllist]]
    }
    uplevel #0 $lib(clientcmd) [list $jlibname streamerror -errormsg $errspec]
}

# jlib::got_stream --
#
#       Callback when we have parsed the initial root element.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       args:       attributes
#       
# Results:
#       none.

proc jlib::got_stream {jlibname args} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals

    Debug 3 "jlib::got_stream jlibname=$jlibname, args='$args'"
    
    # Cache stream attributes.
    foreach {name value} $args {
	set locals(streamattr,$name) $value
    }
    uplevel #0 $lib(clientcmd) [list $jlibname connect]
    schedule_auto_away $jlibname
    set lib(isinstream) 1
    
    # If we use    we should have a callback command here.
    if {[info exists lib(streamcmd)] && [llength $lib(streamcmd)]} {
	uplevel #0 $lib(streamcmd) $jlibname $args
	unset lib(streamcmd)
    }
}

# jlib::getthis --
# 
#       Access function for: server, username, myjid, myjid2...

proc jlib::getthis {jlibname name} {
    
    upvar ${jlibname}::locals locals
    
    if {[info exists locals($name)]} {
	return $locals($name)
    } else {
	return ""
    }
}

# jlib::getstreamattr --
# 
#       Returns the value of any stream attribute, typically 'id'.

proc jlib::getstreamattr {jlibname name} {
    
    upvar ${jlibname}::locals locals
    
    if {[info exists locals(streamattr,$name)]} {
	return $locals(streamattr,$name)
    } else {
	return ""
    }
}

# jlib::end_of_parse --
#
#       Callback when the ending root element is parsed.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       
# Results:
#       none.

proc jlib::end_of_parse {jlibname} {

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::end_of_parse jlibname=$jlibname"
    
    catch {eval $lib(transportreset)}
    uplevel #0 $lib(clientcmd) [list $jlibname disconnect]
    reset $jlibname
}

# jlib::xmlerror --
#
#       Callback when we receive an XML error from the wrapper (parser).
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       
# Results:
#       none.

proc jlib::xmlerror {jlibname args} {

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::xmlerror jlibname=$jlibname, args='$args'"
    
    catch {eval $lib(transportreset)}
    uplevel #0 $lib(clientcmd) [list $jlibname xmlerror -errormsg $args]
    reset $jlibname
}

# jlib::reset --
#
#       Unsets all iqcmd($id) callback procedures.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       
# Results:
#       none.

proc jlib::reset {jlibname} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::iqcmd iqcmd
    upvar ${jlibname}::prescmd prescmd
    upvar ${jlibname}::agent agent
    upvar ${jlibname}::locals locals
    variable statics
    
    Debug 3 "jlib::reset"
    
    cancel_auto_away $jlibname
    
    set num $iqcmd(uid)
    unset -nocomplain iqcmd
    set iqcmd(uid) $num
    
    set num $prescmd(uid)
    unset -nocomplain prescmd
    set prescmd(uid) $num
    
    unset -nocomplain agent    
    unset -nocomplain locals
    
    init_inst $jlibname

    set lib(isinstream) 0
    
    stream_reset $jlibname
    if {[havesasl]} {
	sasl_reset $jlibname
    }
    if {[havetls]} {
	tls_reset $jlibname
    }
}

# jlib::stream_reset --
# 
#       Clears out all variables that are cached for this stream.
#       The xmpp specifies that any information obtained during tls,sasl
#       must be discarded before opening a new stream.
#       Call this before opening a new stream

proc jlib::stream_reset {jlibname} {
    
    upvar ${jlibname}::locals locals
    
    array unset locals features*
    array unset locals streamattr,*
}

# jlib::getstanzaerrorspec --
# 
#       Extracts the error code and an error message from an type='error'
#       element. We must handle both the original Jabber protocol and the
#       XMPP protocol:
#
#   The syntax for stanza-related errors is as follows (XMPP):
#
#   <stanza-kind to='sender' type='error'>
#     [RECOMMENDED to include sender XML here]
#     <error type='error-type'>
#       <defined-condition xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
#       <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
#         OPTIONAL descriptive text
#       </text>
#       [OPTIONAL application-specific condition element]
#     </error>
#   </stanza-kind>
#   
#   Jabber:
#   
#   <iq type='error'>
#     <query ...>
#       <error code='..'> ... </error>
#     </query>
#   </iq>
#   
#   or:
#   <iq type='error'>
#     <error code='401'/>
#     <query ...>...</query>
#   </iq>

proc jlib::getstanzaerrorspec {stanza} {
    
    variable xmppxmlns

    set errcode ""
    set errmsg  ""
        
    # First search children of stanza (<iq> element) for error element.
    foreach child [wrapper::getchildren $stanza] {
	set tag [wrapper::gettag $child]
	if {[string equal $tag "error"]} {
	    set errelem $child
	}
	if {[string equal $tag "query"]} {
	    set queryelem $child
	}
    }
    if {![info exists errelem] && [info exists queryelem]} {
	
	# Search children if <query> element (Jabber).
	set errlist [wrapper::getchildswithtag $queryelem "error"]
	if {[llength $errlist]} {
	    set errelem [lindex $errlist 0]
	}
    }
	
    # Found it! XMPP contains an error stanza and not pure text.
    if {[info exists errelem]} {
	foreach {errcode errmsg} [geterrspecfromerror $errelem stanzas] {break}
    }
    return [list $errcode $errmsg]
}

# jlib::getstreamerrorspec --
# 
#       Extracts the error code and an error message from a stream:error
#       element. We must handle both the original Jabber protocol and the
#       XMPP protocol:
#
#   The syntax for stream errors is as follows:
#
#   <stream:error>
#      <defined-condition xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
#      <text xmlns='urn:ietf:params:xml:ns:xmpp-streams'>
#        OPTIONAL descriptive text
#      </text>
#      [OPTIONAL application-specific condition element]
#   </stream:error>
#
#   Jabber:
#   

proc jlib::getstreamerrorspec {errelem} {
    
    return [geterrspecfromerror $errelem streams]
}

# jlib::geterrspecfromerror --
# 
#       Get an error specification from an stanza error element.
#       
# Arguments:
#       errelem:    the <error/> element
#       kind.       'stanzas' or 'streams'
#       
# Results:
#       none.

proc jlib::geterrspecfromerror {errelem kind} {
       
    variable xmppxmlns
    variable errCodeToText

    array set msgproc {
	stanzas  stanzaerror::getmsg
	streams  streamerror::getmsg
    }
    set cchdata [wrapper::getcdata $errelem]
    set errcode [wrapper::getattribute $errelem code]
    set errmsg  "Unknown"

    if {[string is integer -strict $errcode]} {
	if {$cchdata != ""} {
	    set errmsg $cchdata
	} elseif {[info exists errCodeToText($errcode)]} {
	    set errmsg $errCodeToText($errcode)
	}
    } elseif {$cchdata != ""} {
	
	# Old jabber way.
	set errmsg $cchdata
    }
	
    # xmpp way.
    foreach c [wrapper::getchildren $errelem] {
	set tag [wrapper::gettag $c]
	
	switch -- $tag {
	    text {
		# Use only as a complement iff our language. ???
		set xmlns [wrapper::getattribute $c xmlns]
		set lang  [wrapper::getattribute $c xml:lang]
		# [string equal $lang [getlang]]
		if {[string equal $xmlns $xmppxmlns($kind)]} {
		    set errstr [wrapper::getcdata $c]
		}
	    } 
	    default {
		set xmlns [wrapper::getattribute $c xmlns]
		if {[string equal $xmlns $xmppxmlns($kind)]} {
		    set errcode $tag
		    set errstr [$msgproc($kind) $tag]
		}
	    }
	}
    }
    if {[info exists errstr]} {
	if {$errmsg != ""} {
	    append errmsg ". "
	}
	append errmsg $errstr
    }
    if {$errmsg == ""} {
	set errmsg "Unknown"
    }
    return [list $errcode $errmsg]
}

# jlib::bind_resource --
# 
#       xmpp requires us to bind a resource to the stream.

proc jlib::bind_resource {jlibname resource cmd} {
    
    variable xmppxmlns

    set xmllist [wrapper::createtag bind \
      -attrlist [list xmlns $xmppxmlns(bind)] \
      -subtags [list [wrapper::createtag resource -chdata $resource]]]
    send_iq $jlibname set [list $xmllist] -command \
      [list [namespace current]::parse_bind_resource $jlibname $cmd]
}

proc jlib::parse_bind_resource {jlibname cmd type subiq args} {
    
    upvar ${jlibname}::locals locals
    variable xmppxmlns
    
    # The server MAY change the 'resource' why we need to check this here.
    if {[string equal [wrapper::gettag $subiq] bind] &&  \
      [string equal [wrapper::getattribute $subiq xmlns] $xmppxmlns(bind)]} {
	set jidelem [wrapper::getchildswithtag $subiq jid]
	if {$jidelem != {}} {
	    set sjid [wrapper::getcdata $jidelem]
	    splitjid $sjid sjid2 sresource
	    if {![string equal [resourcemap $locals(resource)] $sresource]} {
		set locals(resource) $sresource
		set locals(myjid) "$locals(myjid2)/$sresource"
	    }
	}
    }    
    uplevel #0 $cmd [list $jlibname $type $subiq]
}
   
# jlib::invoke_iq_callback --
#
#       Callback when we get server response on iq set/get.
#       This is a generic callback procedure.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        the 'cmd' argument in the calling procedure.
#       type:       "error" or "ok".
#       subiq:      if type="error", this is a list {errcode errmsg},
#                   else it is the query element as a xml list structure.
#       
# Results:
#       none.

proc jlib::invoke_iq_callback {jlibname cmd type subiq} {

    Debug 3 "jlib::invoke_iq_callback cmd=$cmd, type=$type, subiq=$subiq"
    
    uplevel #0 $cmd [list $jlibname $type $subiq]
}

# jlib::parse_roster_get --
#
#       Callback command from the 'roster_get' call.
#       Could also be a roster push from the server.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       ispush:     is this the result of a roster push or from our 
#                   'roster_set' call?
#       cmd:        callback command for an error element.
#       type:       "error" or "ok"
#       thequery:       
#       
# Results:
#       the roster object is populated.

proc jlib::parse_roster_get {jlibname ispush cmd type thequery} {

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::parse_roster_get ispush=$ispush, cmd=$cmd, type=$type,"
    Debug 3 "   thequery=$thequery"
    if {[string equal $type "error"]} {
	
	# We've got an error reply. Roster pushes should never be an error!
	if {[string length $cmd] > 0} {
	    uplevel #0 $cmd [list $jlibname error]
	}
	return
    }
    if {!$ispush} {
	
	# Clear the roster and presence.
	$lib(rostername) enterroster
    }
    
    # Extract the XML data items.
    if {![string equal [wrapper::getattribute $thequery xmlns] "jabber:iq:roster"]} {
    
	# Here we should issue a warning:
	# attribute of query tag doesn't match 'jabber:iq:roster'
	
    }    
    if {$ispush} {
	set what "roster_push"
    } else {
	set what "roster_item"
    }
    foreach child [wrapper::getchildren $thequery] {
	
	# Extract the message sub-elements XML data items.
	set ctag [wrapper::gettag $child]
	set cattrlist [wrapper::getattrlist $child]
	set cchdata [wrapper::getcdata $child]
	
	if {[string equal $ctag "item"]} {
	    
	    # Add each item to our roster object.
	    # Build the argument list of '-key value' pairs. Extract the jid.
	    set arglist {}
	    set subscription {}
	    foreach {key value} $cattrlist {
		if {[string equal $key "jid"]} {
		    set jid $value
		} else {
		    lappend arglist -$key $value
		    if {[string equal $key "subscription"]} {
			set subscription $value
		    }
		}
	    }
	    
	    # Check if item should be romoved (subscription='remove').
	    if {[string equal $subscription "remove"]} {
		$lib(rostername) removeitem $jid
	    } else {
	    
		# Collect the group elements.
		set groups {}
		foreach subchild [wrapper::getchildren $child] {
		    set subtag [wrapper::gettag $subchild]
		    if {[string equal $subtag "group"]} {
			lappend groups [wrapper::getcdata $subchild]
		    }
		}
		if {[string length $groups]} {
		    lappend arglist -groups $groups
		}
		
		# Fill in our roster with this.
		eval {$lib(rostername) setrosteritem $jid} $arglist
	    }
	}
    }
    
    # Tell our roster object that we leave...
    if {!$ispush} {
	$lib(rostername) exitroster
    }
}

# jlib::parse_roster_set --
#
#       Callback command from the 'roster_set' call.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        the jabber id (without resource).
#       cmd:        callback command for an error query element.
#       groups:     
#       name:       
#       type:       "error" or "ok"
#       thequery:
#       
# Results:
#       none.

proc jlib::parse_roster_set {jlibname jid cmd groups name type thequery} {

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::parse_roster_set jid=$jid"
    if {[string equal $type "error"]} {
	
	# We've got an error reply.
	uplevel #0 $cmd [list $jlibname error]
	return
    }
}

# jlib::parse_roster_remove --
#
#       Callback command from the 'roster_remove' command.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        the jabber id (without resource).
#       cmd:        callback command for an error query element.
#       type:
#       thequery:
#       
# Results:
#       none.

proc jlib::parse_roster_remove {jlibname jid cmd type thequery} {

    Debug 3 "jlib::parse_roster_remove jid=$jid, cmd=$cmd, type=$type,"
    Debug 3 "   thequery=$thequery"
    if {[string equal $type "error"]} {
	uplevel #0 $cmd [list $jlibname error]
    }
}

# jlib::parse_search_set --
#
#       Callback for 'jabber:iq:search' 'result' and 'set' elements.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        the callback to notify.
#       type:       "ok", "error", or "set"
#       subiq:

proc jlib::parse_search_set {jlibname cmd type subiq} {    

    upvar ${jlibname}::lib lib

    uplevel #0 $cmd [list $type $subiq]
}

# jlib::iq_register --
# 
#       Handler for registered iq callbacks.
#       
#       We could think of a more general mechanism here!!!!
#       1) Using -type, -xmlns, -from etc.

proc jlib::iq_register {jlibname type xmlns func {seq 50}} {
    
    upvar ${jlibname}::iqhook iqhook
    
    lappend iqhook($type,$xmlns) [list $func $seq]
    set iqhook($type,$xmlns) \
      [lsort -integer -index 1 [lsort -unique $iqhook($type,$xmlns)]]
}

proc jlib::iq_run_hook {jlibname type xmlns from subiq args} {
    
    upvar ${jlibname}::iqhook iqhook

    set ishandled 0    

    foreach key [list $type,$xmlns *,$xmlns $type,*] {
	if {[info exists iqhook($key)]} {
	    foreach spec $iqhook($key) {
		set func [lindex $spec 0]
		set code [catch {
		    uplevel #0 $func [list $jlibname $from $subiq] $args
		} ans]
		if {$code} {
		    bgerror "iqhook $func failed: $code\n$::errorInfo"
		}
		if {[string equal $ans "1"]} {
		    set ishandled 1
		    break
		}
	    }
	}	
	if {$ishandled} {
	    break
	}
    }
    return $ishandled
}

# jlib::message_register --
# 
#       Handler for registered message callbacks.
#       
#       We could think of a more general mechanism here!!!!

proc jlib::message_register {jlibname type xmlns func {seq 50}} {
    
    upvar ${jlibname}::msghook msghook
    
    lappend msghook($type,$xmlns) [list $func $seq]
    set msghook($type,$xmlns) \
      [lsort -integer -index 1 [lsort -unique $msghook($type,$xmlns)]]
}

proc jlib::message_run_hook {jlibname type xmlns args} {
    
    upvar ${jlibname}::msghook msghook

    set ishandled 0
    
    foreach key [list $type,$xmlns *,$xmlns $type,*] {
	if {[info exists msghook($key)]} {
	    foreach spec $msghook($key) {
		set func [lindex $spec 0]
		set code [catch {
		    uplevel #0 $func [list $jlibname $xmlns] $args
		} ans]
		if {$code} {
		    bgerror "msghook $func failed: $code\n$::errorInfo"
		}
		if {[string equal $ans "1"]} {
		    set ishandled 1
		    break
		}
	    }
	}	
	if {$ishandled} {
	    break
	}
    }
    return $ishandled
}

# jlib::presence_register --
# 
#       Handler for registered presence callbacks.

proc jlib::presence_register {jlibname type func {seq 50}} {
    
    upvar ${jlibname}::preshook preshook
    
    lappend preshook($type) [list $func $seq]
    set preshook($type)  \
      [lsort -integer -index 1 [lsort -unique $preshook($type)]]
}

proc jlib::presence_run_hook {jlibname from type args} {
    
    upvar ${jlibname}::preshook preshook

    set ishandled 0
    
    if {[info exists preshook($type)]} {
	foreach spec $preshook($type) {
	    set func [lindex $spec 0]
	    set code [catch {
		uplevel #0 $func [list $jlibname $from $type] $args
	    } ans]
	    if {$code} {
		bgerror "preshook $func failed: $code\n$::errorInfo"
	    }
	    if {[string equal $ans "1"]} {
		set ishandled 1
		break
	    }
	}
    }
    return $ishandled
}

proc jlib::presence_deregister {jlibname type func} {
    
    upvar ${jlibname}::preshook preshook
    
    set ind [lsearch -glob $preshook($type) "$func *"]
    if {$ind >= 0} {
	set preshook($type) [lreplace $preshook($type) $ind $ind]
    }
}

# jlib::element_register --
# 
#       Used to get callbacks from non stanza elements, like sasl etc.

proc jlib::element_register {jlibname tag func {seq 50}} {
    
    upvar ${jlibname}::elementhook elementhook
    
    lappend elementhook($tag) [list $func $seq]
    set elementhook($tag)  \
      [lsort -integer -index 1 [lsort -unique $elementhook($tag)]]
}

proc jlib::element_deregister {jlibname tag func} {
    
    upvar ${jlibname}::elementhook elementhook
    
    if {![info exists elementhook($tag)]} {
	return ""
    }
    set ind -1
    set found 0
    foreach spec $elementhook($tag) {
	incr ind
	if {[string equal $func [lindex $spec 0]]} {
	    set found 1
	    break
	}
    }
    if {$found} {
	set elementhook($tag) [lreplace $elementhook($tag) $ind $ind]
    }
}

proc jlib::element_run_hook {jlibname tag xmldata} {
    
    upvar ${jlibname}::elementhook elementhook

    set ishandled 0
    
    if {[info exists elementhook($tag)]} {
	foreach spec $elementhook($tag) {
	    set func [lindex $spec 0]
	    set code [catch {
		uplevel #0 $func [list $jlibname $tag $xmldata]
	    } ans]
	    if {$code} {
		bgerror "preshook $func failed: $code\n$::errorInfo"
	    }
	    if {[string equal $ans "1"]} {
		set ishandled 1
		break
	    }
	}
    }
    return $ishandled
}

# jlib::send_iq --
#
#       To send an iq (info/query) packet.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       type:       can be "get", "set", "result", or "error".
#                   "result" and "error" are used when replying an incoming iq.
#       xmldata:    list of elements as xmllists
#       args:
#                   -to $to       : Specify jid to send this packet to. If it 
#		    isn't specified, this part is set to sender's user-id by 
#		    the server.
#		    
#                   -id $id       : Specify an id to send with the <iq>. 
#                   If $type is "get", or "set", then the id will be generated 
#                   by jlib internally, and this switch will not work. 
#                   If $type is "result" or "error", then you may use this 
#                   switch.
#                   
#                   -command $cmd : Specify a callback to call when the 
#                   reply-packet is got. This switch will not work if $type 
#                   is "result" or "error".
#       
# Results:
#       none.

proc jlib::send_iq {jlibname type xmldata args} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::iqcmd iqcmd
        
    Debug 3 "jlib::send_iq type='$type', xmldata='$xmldata', args='$args'"
    
    array set argsArr $args
    set attrlist [list "type" $type]
    
    # Need to generate a unique identifier (id) for this packet.
    if {[string equal $type "get"] || [string equal $type "set"]} {
	lappend attrlist "id" $iqcmd(uid)
	
	# Record any callback procedure.
	if {[info exists argsArr(-command)]} {
	    set iqcmd($iqcmd(uid)) $argsArr(-command)
	}
	incr iqcmd(uid)
    } elseif {[info exists argsArr(-id)]} {
	lappend attrlist "id" $argsArr(-id)
    }
    if {[info exists argsArr(-to)]} {
	lappend attrlist "to" $argsArr(-to)
    }
    if {[llength $xmldata]} {
	set xmllist [wrapper::createtag "iq" -attrlist $attrlist \
	  -subtags $xmldata]
    } else {
	set xmllist [wrapper::createtag "iq" -attrlist $attrlist]
    }
    
    send $jlibname $xmllist
}

# jlib::iq_get, iq_set --
#
#       Wrapper for 'send_iq' for set/getting namespaced elements.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       xmlns:
#       args:     -to recepient jid
#                 -command procName
#                 -sublists
#                 else as attributes
#       
# Results:
#       none.

proc jlib::iq_get {jlibname xmlns args} {

    set opts {}
    set sublists {}
    set attrlist [list xmlns $xmlns]
    foreach {key value} $args {
	
	switch -- $key {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_iq_callback $jlibname $value]
	    }
	    -to {
		lappend opts -to $value
	    }
	    -sublists {
		set sublists $value
	    }
	    default {
		lappend attrlist [string trimleft $key "-"] $value
	    }
	}
    }
    set xmllist [wrapper::createtag "query" -attrlist $attrlist \
      -subtags $sublists]
    eval {send_iq $jlibname "get" [list $xmllist]} $opts
}

proc jlib::iq_set {jlibname xmlns args} {

    set opts {}
    set sublists {}
    foreach {key value} $args {
	
	switch -- $key {
	    -command {
		lappend opts -command  \
		  [list [namespace current]::invoke_iq_callback $jlibname $value]
	    }
	    -to {
		lappend opts -to $value
	    }
	    -sublists {
		set sublists $value
	    }
	    default {
		#lappend subelements [wrapper::createtag  \
		#  [string trimleft $key -] -chdata $value]		
	    }
	}
    }
    set xmllist [wrapper::createtag "query" -attrlist [list xmlns $xmlns] \
      -subtags $sublists]
    eval {send_iq $jlibname "set" [list $xmllist]} $opts
}

# jlib::send_auth --
#
#       Send simple client authentication.
#       It implements the 'jabber:iq:auth' set method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       username:
#       resource:
#       cmd:        client command to be executed at the iq "result" element.
#       args:       Any of "-password" or "-digest" must be given.
#           -password
#           -digest
#           -to
#       
# Results:
#       none.

proc jlib::send_auth {jlibname username resource cmd args} {

    upvar ${jlibname}::locals locals

    set subelements [list  \
      [wrapper::createtag "username" -chdata $username]  \
      [wrapper::createtag "resource" -chdata $resource]]
    set toopt {}

    foreach {key value} $args {
	switch -- $key {
	    -password - -digest {
		lappend subelements [wrapper::createtag  \
		  [string trimleft $key -] -chdata $value]
	    }
	    -to {
		set toopt [list -to $value]
	    }
	}
    }

    set xmllist [wrapper::createtag "query" -attrlist {xmlns jabber:iq:auth} \
      -subtags $subelements]
    eval {send_iq $jlibname "set" [list $xmllist] -command  \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]} $toopt
    
    # Cache our login jid.
    set locals(username) $username
    set locals(resource) $resource
    set locals(myjid2)   ${username}@$locals(server)
    set locals(myjid)    ${username}@$locals(server)/${resource}
}

# jlib::register_get --
#
#       Sent with a blank query to retrieve registration information.
#       Retrieves a key for use on future registration pushes.
#       It implements the 'jabber:iq:register' get method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        client command to be executed at the iq "result" element.
#       args:       -to     : the jid for the service
#       
# Results:
#       none.

proc jlib::register_get {jlibname cmd args} {

    array set argsArr $args
    set xmllist [wrapper::createtag "query" -attrlist {xmlns jabber:iq:register}]
    if {[info exists argsArr(-to)]} {
	set toopt [list -to $argsArr(-to)]
    } else {
	set toopt ""
    }
    eval {send_iq $jlibname "get" [list $xmllist] -command  \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]} $toopt
}

# jlib::register_set --
#
#       Create a new account with the server, or to update user information.
#       It implements the 'jabber:iq:register' set method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       username:
#       password:
#       cmd:        client command to be executed at the iq "result" element.
#       args:       -to       : the jid for the service
#                   -nick     :
#                   -name     :
#                   -first    :
#                   -last     :
#                   -email    :
#                   -address  :
#                   -city     :
#                   -state    :
#                   -zip      :
#                   -phone    :
#                   -url      :
#                   -date     :
#                   -misc     :
#                   -text     :
#                   -key      :
#       
# Results:
#       none.

proc jlib::register_set {jlibname username password cmd args} {
    
    set subelements [list  \
      [wrapper::createtag "username" -chdata $username]  \
      [wrapper::createtag "password" -chdata $password]]
    array set argsArr $args
    foreach argsswitch [array names argsArr] {
	if {[string equal $argsswitch "-to"]} {
	    continue
	}
	set par [string trimleft $argsswitch {-}]
	lappend subelements [wrapper::createtag $par  \
	  -chdata $argsArr($argsswitch)]
    }
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:register}   \
      -subtags $subelements]
    
    if {[info exists argsArr(-to)]} {
	set toopt [list -to $argsArr(-to)]
    } else {
	set toopt ""
    }
    eval {send_iq $jlibname "set" [list $xmllist] -command  \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]} $toopt
}

# jlib::register_remove --
#
#       It implements the 'jabber:iq:register' set method with a <remove/> tag.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:
#       cmd:        client command to be executed at the iq "result" element.
#       args    -key
#       
# Results:
#       none.

proc jlib::register_remove {jlibname to cmd args} {

    set subelements [list [wrapper::createtag "remove"]]
    array set argsArr $args
    if {[info exists argsArr(-key)]} {
	lappend subelements [wrapper::createtag "key" -chdata $argsArr(-key)]
    }
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:register} -subtags $subelements]

    eval {send_iq $jlibname "set" [list $xmllist] -command   \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]} -to $to
}

# jlib::search_get --
#
#       Sent with a blank query to retrieve search information.
#       Retrieves a key for use on future search pushes.
#       It implements the 'jabber:iq:search' get method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:         this must be a searchable jud service, typically 
#                   'jud.jabber.org'.
#       cmd:        client command to be executed at the iq "result" element.
#       
# Results:
#       none.

proc jlib::search_get {jlibname to cmd} {
    
    set xmllist [wrapper::createtag "query" -attrlist {xmlns jabber:iq:search}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command        \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::search_set --
#
#       Makes an actual search in our roster at the server.
#       It implements the 'jabber:iq:search' set method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        client command to be executed at the iq "result" element.
#       to:         this must be a searchable jud service, typically 
#                   'jud.jabber.org'.
#       args:    -subtags list
#       
# Results:
#       none.

proc jlib::search_set {jlibname to cmd args} {

    array set argsarr $args

    if {[info exists argsarr(-subtags)]} {
	set xmllist [wrapper::createtag "query"  \
	  -attrlist {xmlns jabber:iq:search}   \
	  -subtags $argsarr(-subtags)]
    } else {
	set xmllist [wrapper::createtag "query"  \
	  -attrlist {xmlns jabber:iq:search}]
    }
    send_iq $jlibname "set" [list $xmllist] -to $to -command  \
      [list [namespace current]::parse_search_set $jlibname $cmd]
}

# jlib::send_message --
#
#       Sends a message element.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:         the jabber id of the receiver.
#       args:
#                   -subject $subject     : Set subject of the message to 
#                   $subject.
#
#                   -thread $thread       : Set thread of the message to 
#                   $thread.
#                   
#                   -priority $priority   : Set priority of the message to 
#                   $priority.
#
#                   -body text            : 
#                   
#                   -type $type           : normal, chat or groupchat
#                   
#                   -id token
#
#                   -xlist $xlist         : A list containing *X* xml_data. 
#                   Anything can be put inside an *X*. Please make sure you 
#                   created it with "wrapper::createtag" procedure, 
#                   and also, it has a "xmlns" attribute in its root tag. 
#
#                   -command
#                   
# Results:
#       none.

proc jlib::send_message {jlibname to args} {

    upvar ${jlibname}::msgcmd msgcmd

    Debug 3 "jlib::send_message to=$to, args=$args"
    
    array set argsArr $args
    set attrlist [list to $to]
    set children {}
    
    foreach {name value} $args {
	set par [string trimleft $name "-"]
	
	switch -- $name {
	    -command {
		lappend attrlist "id" $msgcmd(uid)
		set msgcmd($msgcmd(uid)) $value
		incr msgcmd(uid)
	    }
	    -xlist {
		foreach xchild $value {
		    lappend children $xchild
		}
	    }
	    -type {
		if {![string equal $value "normal"]} {
		    lappend attrlist "type" $value
		}
	    }
	    -id {
		lappend attrlist $par $value
	    }
	    default {
		lappend children [wrapper::createtag $par -chdata $value]
	    }
	}
    }
    set xmllist [wrapper::createtag "message" -attrlist $attrlist  \
      -subtags $children]
    
    send $jlibname $xmllist
}

# jlib::send_presence --
#
#       To send your presence.
#
# Arguments:
# 
#       jlibname:   the instance of this jlib.
#       args:
#           -to     the jabber id of the recepient.
#           -from   should never be set by client!
#           -type   one of 'available', 'unavailable', 'subscribe', 
#                   'unsubscribe', 'subscribed', 'unsubscribed', 'invisible'.
#           -status
#           -priority
#           -show
#           -xlist
#           -extras
#           -command   Specify a callback to call if we may expect any reply
#                   package, as entering a room with 'gc-1.0'.
#     
# Results:
#       none.

proc jlib::send_presence {jlibname args} {

    variable statics
    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    upvar ${jlibname}::prescmd prescmd
    
    Debug 3 "jlib::send_presence args='$args'"
    
    set attrlist {}
    set children {}
    set type "available"
    array set argsArr $args
    
    foreach {key value} $args {
	set par [string trimleft $key -]
	
	switch -- $par {
	    type {
		set type $value
		if {[regexp $statics(presenceTypeExp) $type]} {
		    lappend attrlist $par $type
		} else {
		    return -code error "Is not valid presence type: \"$type\""
		}
	    }
	    from - to {
		lappend attrlist $par $value
	    }
	    xlist - extras {
		foreach xchild $value {
		    lappend children $xchild
		}
	    }
	    command {
		lappend attrlist "id" $prescmd(uid)
		set prescmd($prescmd(uid)) $value
		incr prescmd(uid)
	    }
	    default {
		lappend children [wrapper::createtag $par -chdata $value]
	    }
	}
    }
    set xmllist [wrapper::createtag "presence" -attrlist $attrlist  \
      -subtags $children]
    
    # Any of {available away dnd invisible unavailable}
    # Must be destined to login server (by default).
    if {![info exists argsArr(-to)] || ($argsArr(-to) eq $locals(server))} {
	set locals(status) $type
	if {[info exists argsArr(-show)]} {
	    set locals(status) $argsArr(-show)
	}
    }
    
    send $jlibname $xmllist
}

# jlib::send --
# 
#       Sends general xml using a xmllist.

proc jlib::send {jlibname xmllist} {
    
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::locals locals
	
    # For the auto away function.
    if {$locals(trigAutoAway)} {
	schedule_auto_away $jlibname
    }
    set locals(last) [clock seconds]
    set xml [wrapper::createxml $xmllist]

    # We fail only if already in stream.
    # The first failure reports the network error, closes the stream,
    # which stops multiple errors to be reported to the client.
    if {$lib(isinstream) && [catch {eval $lib(transportsend) {$xml}} err]} {
	kill $jlibname
	uplevel #0 $lib(clientcmd) [list $jlibname networkerror]
    }
}

# jlib::mystatus --
# 
#       Returns any of {available away xa chat dnd invisible unavailable}
#       for our status with the login server.

proc jlib::mystatus {jlibname} {

    upvar ${jlibname}::locals locals
    
    return $locals(status)
}

# jlib::myjid --
# 
#       Returns our 3-tier jid as authorized with the login server.

proc jlib::myjid {jlibname} {

    upvar ${jlibname}::locals locals
    
    return $locals(myjid)
}

# jlib::oob_set --
#
#       It implements the 'jabber:iq:oob' set method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:
#       cmd:        client command to be executed at the iq "result" element.
#       url:
#       args:
#                   -desc
#       
# Results:
#       none.

proc jlib::oob_set {jlibname to cmd url args} {

    set attrlist {xmlns jabber:iq:oob}
    set children [list [wrapper::createtag "url" -chdata $url]]
    array set argsarr $args
    if {[info exists argsarr(-desc)] && [string length $argsarr(-desc)]} {
	lappend children [wrapper::createtag {desc} -chdata $argsarr(-desc)]
    }
    set xmllist [wrapper::createtag query -attrlist $attrlist  \
      -subtags $children]
    send_iq $jlibname set [list $xmllist] -to $to -command  \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::agent_get --
#
#       It implements the 'jabber:iq:agent' get method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:         the *server's* name! (users.jabber.org, for instance)
#       cmd:        client command to be executed at the iq "result" element.
#       
# Results:
#       none.

proc jlib::agent_get {jlibname to cmd} {

    set xmllist [wrapper::createtag "query" -attrlist {xmlns jabber:iq:agent}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace current]::parse_agent_get $jlibname $to $cmd]
}

proc jlib::agents_get {jlibname to cmd} {

    set xmllist [wrapper::createtag "query" -attrlist {xmlns jabber:iq:agents}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace current]::parse_agents_get $jlibname $to $cmd]
}

# parse_agent_get, parse_agents_get --
#
#       Callbacks for the agent(s) methods. Caches agent information,
#       and makes registered client callback.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        the 'to' attribute of our agent(s) request.
#       cmd:        client command to be executed.
#       
# Results:
#       none.

proc jlib::parse_agent_get {jlibname jid cmd type subiq} {

    upvar ${jlibname}::lib lib
    upvar ${jlibname}::agent agent
    upvar [namespace current]::service::services services

    Debug 3 "jlib::parse_agent_get jid=$jid, cmd=$cmd, type=$type, subiq=$subiq"

    switch -- $type {
	error {
	    uplevel #0 $cmd [list $jlibname error $subiq]
	} 
	default {
     
	    # Loop through the subelement to see what we've got.
	    foreach elem [wrapper::getchildren $subiq] {
		set tag [wrapper::gettag $elem]
		set agent($jid,$tag) [wrapper::getcdata $elem]
		if {[lsearch $services $tag] >= 0} {
		    lappend agent($tag) $jid
		}
		if {[string equal $tag "groupchat"]} {
		    [namespace current]::service::registergcprotocol  \
		      $jlibname $jid "gc-1.0"
		}
	    }    
	    uplevel #0 $cmd [list $jlibname $type $subiq]
	}
    }
}

proc jlib::parse_agents_get {jlibname jid cmd type subiq} {

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::agent agent
    upvar [namespace current]::service::services services

    Debug 3 "jlib::parse_agents_get jid=$jid, cmd=$cmd, type=$type, subiq=$subiq"

    switch -- $type {
	error {
	    uplevel #0 $cmd [list $jlibname error $subiq]
	} 
	default {
	    
	    # Be sure that the login jabber server is the root.
	    if {[string equal $locals(server) $jid]} {
		set agent($jid,parent) {}
	    }
	    # ???
	    set agent($jid,parent) {}
	    
	    # Cache the agents info we've got.
	    foreach agentElem [wrapper::getchildren $subiq] {
		if {![string equal [wrapper::gettag $agentElem] "agent"]} {
		    continue
		}
		set jidAgent [wrapper::getattribute $agentElem jid]
		set subAgent [wrapper::getchildren $agentElem]
		
		# Loop through the subelement to see what we've got.
		foreach elem $subAgent {
		    set tag [wrapper::gettag $elem]
		    set agent($jidAgent,$tag) [wrapper::getcdata $elem]
		    if {[lsearch $services $tag] >= 0} {
			lappend agent($tag) $jidAgent
		    }
		    if {[string equal $tag "groupchat"]} {
			[namespace current]::service::registergcprotocol  \
			  $jlibname $jid "gc-1.0"
		    }
		}
		set agent($jidAgent,parent) $jid
		lappend agent($jid,childs) $jidAgent	
	    }
	    uplevel #0 $cmd [list $jlibname $type $subiq]
	}
    }
}

# jlib::getagent --
# 
#       Accessor function for the agent stuff.

proc jlib::getagent {jlibname jid} {

    upvar ${jlibname}::agent agent

    if {[info exists agent($jid,parent)]} {
	return [array get agent [jlib::ESC $jid],*]
    } else {
	return ""
    }
}

proc jlib::have_agent {jlibname jid} {

    upvar ${jlibname}::agent agent

    if {[info exists agent($jid,parent)]} {
	return 1
    } else {
	return 0
    }
}

# jlib::vcard_get --
#
#       It implements the 'jabber:iq:vcard-temp' get method.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:
#       cmd:        client command to be executed at the iq "result" element.
#       
# Results:
#       none.

proc jlib::vcard_get {jlibname to cmd} {

    set attrlist [list xmlns vcard-temp]    
    set xmllist [wrapper::createtag "vCard" -attrlist $attrlist]
    send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::vcard_set --
#
#       Sends our vCard to the server. Internally we use all lower case
#       but the spec (JEP-0054) says that all tags be all upper case.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       cmd:        client command to be executed at the iq "result" element.
#       args:       All keys are named so that the element hierarchy becomes
#                   vcardElement_subElement_subsubElement ... and so on;
#                   all lower case.
#                   
# Results:
#       none.

proc jlib::vcard_set {jlibname cmd args} {

    set attrlist [list xmlns vcard-temp]    
    
    # Form all the sub elements by inspecting the -key.
    array set arr $args
    set subelem {}
    set subsubelem {}
    
    # All "sub" elements with no children.
    foreach tag {fn nickname bday url title role desc} {
	if {[info exists arr(-$tag)]} {
	    lappend subelem [wrapper::createtag [string toupper $tag] \
	      -chdata $arr(-$tag)]
	}
    }
    if {[info exists arr(-email_internet_pref)]} {
	set elem {}
	lappend elem [wrapper::createtag "INTERNET"]
	lappend elem [wrapper::createtag "PREF"]
	lappend subelem [wrapper::createtag "EMAIL" \
	  -chdata $arr(-email_internet_pref) -subtags $elem]
    }
    if {[info exists arr(-email_internet)]} {
	foreach email $arr(-email_internet) {
	    set elem {}
	    lappend elem [wrapper::createtag "INTERNET"]
	    lappend subelem [wrapper::createtag "EMAIL" \
	      -chdata $email -subtags $elem]
	}
    }
    
    # All "subsub" elements.
    foreach tag {n org} {
	set elem {}
	foreach key [array names arr "-${tag}_*"] {
	    regexp -- "-${tag}_(.+)" $key match sub
	    lappend elem [wrapper::createtag [string toupper $sub] \
	      -chdata $arr($key)]
	}
    
	# Insert subsub elements where they belong.
	if {[llength $elem]} {
	    lappend subelem [wrapper::createtag [string toupper $tag] \
	      -subtags $elem]
	}
    }
    
    # The <adr><home/>, <adr><work/> sub elements.
    foreach tag {adr_home adr_work} {
	regexp -- {([^_]+)_(.+)} $tag match head sub
	set elem [list [wrapper::createtag [string toupper $sub]]]
	set haveThisTag 0
	foreach key [array names arr "-${tag}_*"] {
	    set haveThisTag 1
	    regexp -- "-${tag}_(.+)" $key match sub
	    lappend elem [wrapper::createtag [string toupper $sub] \
	      -chdata $arr($key)]
	}		
	if {$haveThisTag} {
	    lappend subelem [wrapper::createtag [string toupper $head] \
	      -subtags $elem]
	}
    }	
    
    # The <tel> sub elements.
    foreach tag [array names arr "-tel_*"] {
	if {[regexp -- {-tel_([^_]+)_([^_]+)} $tag match second third]} {
	    set elem {}
	    lappend elem [wrapper::createtag [string toupper $second]]
	    lappend elem [wrapper::createtag [string toupper $third]]
	    lappend subelem [wrapper::createtag "TEL" -chdata $arr($tag) \
	      -subtags $elem]
	}
    }

    set xmllist [wrapper::createtag vCard -attrlist $attrlist \
      -subtags $subelem]
    send_iq $jlibname "set" [list $xmllist] -command \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]    
}

# jlib::get_last --
#
#       Query the 'last' of 'to' using 'jabber:iq:last' get.

proc jlib::get_last {jlibname to cmd} {
    
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:last}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::handle_get_last --
#
#       Seconds since last activity. Response to 'jabber:iq:last' get.

proc jlib::handle_get_last {jlibname from subiq args} {    

    upvar ${jlibname}::locals locals
    
    array set argsarr $args

    set secs [expr [clock seconds] - $locals(last)]
    set xmllist [wrapper::createtag "query"  \
      -attrlist [list xmlns jabber:iq:last seconds $secs]]
    
    set opts {}
    if {[info exists argsarr(-from)]} {
	lappend opts -to $argsarr(-from)
    }
    if {[info exists argsarr(-id)]} {
	lappend opts -id $argsarr(-id)
    }
    eval {send_iq $jlibname "result" [list $xmllist]} $opts

    # Tell jlib's iq-handler that we handled the event.
    return 1
}

# jlib::get_time --
#
#       Query the 'time' of 'to' using 'jabber:iq:time' get.

proc jlib::get_time {jlibname to cmd} {
    
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:time}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command        \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::handle_get_time --
#
#       Send our time. Response to 'jabber:iq:time' get.

proc jlib::handle_get_time {jlibname from subiq args} {
    
    array set argsarr $args
    
    set secs [clock seconds]
    set utc [clock format $secs -format "%Y%m%dT%H:%M:%S" -gmt 1]
    set tz "GMT"
    set display [clock format $secs]
    set subtags [list  \
      [wrapper::createtag "utc" -chdata $utc]  \
      [wrapper::createtag "tz" -chdata $tz]  \
      [wrapper::createtag "display" -chdata $display] ]
    set xmllist [wrapper::createtag "query" -subtags $subtags  \
      -attrlist {xmlns jabber:iq:time}]

    set opts {}
    if {[info exists argsarr(-from)]} {
	lappend opts -to $argsarr(-from)
    }
    if {[info exists argsarr(-id)]} {
	lappend opts -id $argsarr(-id)
    }
    eval {send_iq $jlibname "result" [list $xmllist]} $opts

    # Tell jlib's iq-handler that we handled the event.
    return 1
}
    
# jlib::get_version --
#
#       Query the 'version' of 'to' using 'jabber:iq:version' get.

proc jlib::get_version {jlibname to cmd} {
        
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:version}]
    send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace current]::invoke_iq_callback $jlibname $cmd]
}

# jlib::handle_get_version --
#
#       Send our version. Response to 'jabber:iq:version' get.

proc jlib::handle_get_version {jlibname from subiq args} {
    global  prefs tcl_platform
    variable version
    
    array set argsArr $args
    
    # Return any id!
    set opts {}
    if {[info exists argsArr(-id)]} {
	set opts [list -id $argsArr(-id)]
    }
    set os $tcl_platform(os)
    if {[info exists tcl_platform(osVersion)]} {
	append os " " $tcl_platform(osVersion)
    }
    lappend opts -to $from
    set subtags [list  \
      [wrapper::createtag name    -chdata "JabberLib"]  \
      [wrapper::createtag version -chdata $version]  \
      [wrapper::createtag os      -chdata $os] ]
    set xmllist [wrapper::createtag query -subtags $subtags  \
      -attrlist {xmlns jabber:iq:version}]
    eval {send_iq $jlibname "result" [list $xmllist]} $opts

    # Tell jlib's iq-handler that we handled the event.
    return 1
}

# jlib::roster_get --
#
#       To get your roster from server.
#       All roster info is propagated to the client via the callback in the
#       roster object. The 'cmd' is only called as a response to an iq-result
#       element.
#
# Arguments:
#       
#       jlibname:   the instance of this jlib.
#       args:       ?
#       cmd:        callback command for an error query element.
#     
# Results:
#       none.
  
proc jlib::roster_get {jlibname cmd args} {

    array set argsArr $args  
    
    # Perhaps we should clear our roster object here?
    
    set xmllist [wrapper::createtag "query"  \
      -attrlist {xmlns jabber:iq:roster}]
    send_iq $jlibname "get" [list $xmllist] -command   \
      [list [namespace current]::parse_roster_get $jlibname 0 $cmd]
}

# jlib::roster_set --
#
#       To set/add an jid in/to your roster.
#       All roster info is propagated to the client via the callback in the
#       roster object. The 'cmd' is only called as a response to an iq-result
#       element.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        jabber user id to add/set.
#       cmd:        callback command for an error query element.
#       args:
#           -name $name:     A name to show the user-id as on roster to the user.
#           -groups $group_list: Groups of user. If you omit this, then the user's
#                            groups will be set according to the user's options
#                            stored in the roster object. If user doesn't exist,
#                            or you haven't got your roster, user's groups will be
#                            set to "", which means no groups.
#       
# Results:
#       none.
 
proc jlib::roster_set {jlibname jid cmd args} {

    upvar ${jlibname}::lib lib

    Debug 3 "jlib::roster_set jid=$jid, cmd=$cmd, args='$args'"
    array set argsArr $args  

    # Find group(s).
    if {![info exists argsArr(-groups)]} {
	set groups [$lib(rostername) getgroups $jid]
    } else {
	set groups $argsArr(-groups)
    }
    
    set attrlist [list {jid} $jid]
    set name {}
    if {[info exists argsArr(-name)]} {
	set name $argsArr(-name)
	lappend attrlist {name} $name
    }
    set subdata {}
    foreach group $groups {
    	if {$group != ""} {
	    lappend subdata [wrapper::createtag "group" -chdata $group]
	}
    }
    
    set xmllist [wrapper::createtag "query"   \
      -attrlist {xmlns jabber:iq:roster}      \
      -subtags [list [wrapper::createtag {item} -attrlist $attrlist  \
      -subtags $subdata]]]
    send_iq $jlibname "set" [list $xmllist] -command   \
      [list [namespace current]::parse_roster_set $jlibname $jid $cmd  \
      $groups $name]
}

# jlib::roster_remove --
#
#       To remove an item in your roster.
#       All roster info is propagated to the client via the callback in the
#       roster object. The 'cmd' is only called as a response to an iq-result
#       element.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        jabber user id.
#       cmd:        callback command for an error query element.
#       args:       ?
#       
# Results:
#       none.

proc jlib::roster_remove {jlibname jid cmd args} {

    Debug 3 "jlib::roster_remove jid=$jid, cmd=$cmd, args=$args"
    
    set xmllist [wrapper::createtag "query"   \
      -attrlist {xmlns jabber:iq:roster}      \
      -subtags [list  \
      [wrapper::createtag "item"  \
      -attrlist [list jid $jid subscription remove]]]]
    send_iq $jlibname "set" [list $xmllist] -command   \
      [list [namespace current]::parse_roster_remove $jlibname $jid $cmd]
}

# jlib::schedule_keepalive --
# 
#       Supposed to detect network failures but seems not to work like that.

proc jlib::schedule_keepalive {jlibname} {   

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    upvar ${jlibname}::lib lib

    if {$opts(-keepalivesecs) && $lib(isinstream)} {
	Debug 2 "SEND:"
	if {[catch {
	    puts -nonewline $lib(sock) "\n"
	    flush $lib(sock)
	} err]} {
	    closestream $jlibname
	    set errmsg "Network was disconnected"
	    uplevel #0 $lib(clientcmd) [list $jlibname networkerror -body $errmsg]   
	    return
	}
	set locals(aliveid) [after [expr 1000 * $opts(-keepalivesecs)] \
	  [list [namespace current]::schedule_keepalive $jlibname]]
    }
}

# jlib::schedule_auto_away, cancel_auto_away, auto_away_cmd
#
#       Procedures for auto away things.

proc jlib::schedule_auto_away {jlibname} {       

    upvar ${jlibname}::locals locals
    upvar ${jlibname}::opts opts
    
    cancel_auto_away $jlibname
    if {$opts(-autoawaymins) > 0} {
	set locals(afterawayid) [after [expr 60000 * $opts(-autoawaymins)] \
	  [list [namespace current]::auto_away_cmd $jlibname away]]
    }
    if {$opts(-xautoawaymins) > 0} {
	set locals(afterxawayid) [after [expr 60000 * $opts(-xautoawaymins)] \
	  [list [namespace current]::auto_away_cmd $jlibname xaway]]
    }    
}

proc jlib::cancel_auto_away {jlibname} {

    upvar ${jlibname}::locals locals

    if {[info exists locals(afterawayid)]} {
	after cancel $locals(afterawayid)
	unset locals(afterawayid)
    }
    if {[info exists locals(afterxawayid)]} {
	after cancel $locals(afterxawayid)
	unset locals(afterxawayid)
    }
}

# jlib::auto_away_cmd --
# 
#       what:       "away", or "xaway"

proc jlib::auto_away_cmd {jlibname what} {      

    variable statusPriority
    upvar ${jlibname}::locals locals
    upvar ${jlibname}::lib lib
    upvar ${jlibname}::opts opts

    Debug 3 "jlib::auto_away_cmd what=$what"
    
    if {$what eq "xaway"} {
	set status xa
    } else {
	set status $what
    }

    # Auto away and extended away are only set when the
    # current status has a lower priority than away or xa respectively.
    if {$statusPriority($locals(status)) >= $statusPriority($status)} {
	return
    }
    
    # Be sure not to trig ourselves.
    set locals(trigAutoAway) 0

    switch -- $what {
	away {
	    send_presence $jlibname -show "away" -status $opts(-awaymsg)
	}
	xaway {
	    send_presence $jlibname -show "xa" -status $opts(-xawaymsg)
	}
    }
    set locals(trigAutoAway) 1
    uplevel #0 $lib(clientcmd) [list $jlibname $status]
}

# jlib::getrecipientjid --
# 
#       Tries to obtain the correct form of jid to send message to.
#       Follows the XMPP spec, section 4.1.

proc jlib::getrecipientjid {jlibname jid} {

    upvar ${jlibname}::lib lib
    
    jlib::splitjid $jid jid2 resource 
    set isroom [[namespace current]::service::isroom $jlibname $jid2]
    if {$isroom} {
	return $jid
    } elseif {[$lib(rostername) isavailable $jid]} {
	return $jid
    } else {
	return $jid2
    }
}

proc jlib::getlang {} {
    
    if {[catch {package require msgcat}]} {
	return en
    } else {
        foreach lang [::msgcat::mcpreferences] {
            if {[string first _ $lang] == -1 } { break }
        }
    
	switch -- $lang {
	    "" - c - posix {
		return en
	    }
	    default {
		return $lang
	    }
	}
    }
}

namespace eval jlib {
    
    # We just the http error codes here since may be useful if we only
    # get the 'code' attribute in an error element.
    variable errCodeToText
    array set errCodeToText {
	100 "Continue"
	101 "Switching Protocols"
	200 "OK"
	201 "Created"
	202 "Accepted"
	203 "Non-Authoritative Information"
	204 "No Content"
	205 "Reset Content"
	206 "Partial Content"
	300 "Multiple Choices"
	301 "Moved Permanently"
	302 "Found"
	303 "See Other"
	304 "Not Modified"
	305 "Use Proxy"
	307 "Temporary Redirect"
	400 "Bad Request"
	401 "Unauthorized"
	402 "Payment Required"
	403 "Forbidden"
	404 "Not Found"
	405 "Method Not Allowed"
	406 "Not Acceptable"
	407 "Proxy Authentication Required"
	408 "Request Time-out"
	409 "Conflict"
	410 "Gone"
	411 "Length Required"
	412 "Precondition Failed"
	413 "Request Entity Too Large"
	414 "Request-URI Too Large"
	415 "Unsupported Media Type"
	416 "Requested Range Not Satisfiable"
	417 "Expectation Failed"
	500 "Internal Server Error"	
	501 "Not Implemented"
	502 "Bad Gateway"
	503 "Service Unavailable"
	504 "Gateway Time-out"
	505 "HTTP Version not supported"
    }
}

# Various utility procedures to handle jid's....................................

# jlib::ESC --
#
#	array get and array unset accepts glob characters. These need to be
#	escaped if they occur as part of a JID.
#

proc jlib::ESC {s} {
    return [string map {* \\* ? \\? [ \\[ ] \\] \\ \\\\} $s]
}

# STRINGPREPs for the differnt parts of jids.

proc jlib::UnicodeListToRE {ulist} {
    
    set str [string map {- -\\u} $ulist]
    set str "\\u[join $str \\u]"
    return [subst $str]
}

namespace eval jlib {
    
    # Characters that need to be escaped since non valid.
    #       JEP-0106 EXPERIMENTAL!  Think OUTDATED???
    variable jidesc { "#\&'/:<>@} ;#'"  This is just to fix emacs.
    
    # Prohibited ASCII characters.
    set asciiC12C22 {\x00-\x1f\x80-\x9f\x7f\xa0}
    set asciiC11 {\x20}
    
    # C.1.1 is actually allowed (RFC3491), weird!
    set    asciiProhibit(domain) $asciiC11
    append asciiProhibit(domain) $asciiC12C22
    append asciiProhibit(domain) /@   

    # The nodeprep prohibits these characters in addition.
    #x22 (") 
    #x26 (&) 
    #x27 (') 
    #x2F (/) 
    #x3A (:) 
    #x3C (<) 
    #x3E (>) 
    #x40 (@) 
    set    asciiProhibit(node) {"&'/:<>@} ;#'"  This is just to fix emacs.
    append asciiProhibit(node) $asciiC11 
    append asciiProhibit(node) $asciiC12C22
    
    set asciiProhibit(resource) $asciiC12C22
    
    # RFC 3454 (STRINGPREP); all unicode characters:
    # 
    # Maps to nothing (empty).
    set mapB1 {
	00ad	034f	1806	180b	180c	180d	200b	200c
	200d	2060	fe00	fe01	fe02	fe03	fe04	fe05
	fe06	fe07	fe08	fe09	fe0a	fe0b	fe0c	fe0d
	fe0e	fe0f	feff    
    }
    
    # ASCII space characters. Just a space.
    set prohibitC11 {0020}
    
    # Non-ASCII space characters
    set prohibitC12 {
	00a0	1680	2000	2001	2002	2003	2004	2005
	2006	2007	2008	2009	200a	200b	202f	205f
	3000
    }
    
    # C.2.1 ASCII control characters
    set prohibitC21 {
	0000-001F   007F
    }
    
    # C.2.2 Non-ASCII control characters
    set prohibitC22 {
	0080-009f	06dd	070f	180e	200c	200d	2028
	2029	2060	2061	2062	2063	206a-206f	feff
	fff9-fffc       1d173-1d17a
    }
    
    # C.3 Private use
    set prohibitC3 {
	e000-f8ff	f0000-ffffd	100000-10fffd
    }
    
    # C.4 Non-character code points
    set prohibitC4 {
	fdd0-fdef	fffe-ffff	1fffe-1ffff	2fffe-2ffff
	3fffe-3ffff	4fffe-4ffff	5fffe-5ffff	6fffe-6ffff
	7fffe-7ffff	8fffe-8ffff	9fffe-9ffff	afffe-affff
	bfffe-bffff	cfffe-cffff	dfffe-dffff	efffe-effff
	ffffe-fffff	10fffe-10ffff
    }
    
    # C.5 Surrogate codes
    set prohibitC5 {d800-dfff}
    
    # C.6 Inappropriate for plain text
    set prohibitC6 {
	fff9	fffa	fffb	fffc	fffd
    }
    
    # C.7 Inappropriate for canonical representation
    set prohibitC7 {2ff0-2ffb}
    
    # C.8 Change display properties or are deprecated
    set prohibitC8 {
	0340	0341	200e	200f	202a	202b	202c	202d
	202e	206a	206b	206c	206d	206e	206f
    }
    
    # Test: 0, 1, 2, A-Z
    set test {
	0030    0031   0032    0041-005a
    }
    
    # And many more...

    variable mapB1RE       [UnicodeListToRE $mapB1]
    variable prohibitC11RE [UnicodeListToRE $prohibitC11]
    variable prohibitC12RE [UnicodeListToRE $prohibitC12]

}

# jlib::splitjid --
# 
#       Splits a general jid into a jid-2-tier and resource

proc jlib::splitjid {jid jid2Var resourceVar} {
    
    set ind [string first / $jid]
    if {$ind == -1} {
	uplevel 1 [list set $jid2Var $jid]
	uplevel 1 [list set $resourceVar {}]
    } else {
	set jid2 [string range $jid 0 [expr $ind - 1]]
	set res [string range $jid [expr $ind + 1] end]
	uplevel 1 [list set $jid2Var $jid2]
	uplevel 1 [list set $resourceVar $res]
    }
}

# jlib::splitjidex --
# 
#       Split a jid into the parts: jid = [ node "@" ] domain [ "/" resource ]
#       Possibly empty. Doesn't check for valid content, only the form.

proc jlib::splitjidex {jid nodeVar domainVar resourceVar} {
    
    set node   ""
    set domain ""
    set res    ""
    if {[regexp {^(([^@]+)@)?([^ /@]+)(/(.*))?$} $jid m x node domain y res]} {
	uplevel 1 [list set $nodeVar $node]
	uplevel 1 [list set $domainVar $domain]
	uplevel 1 [list set $resourceVar $res]
    } elseif {$jid == ""} {
	uplevel 1 [list set $nodeVar $node]
	uplevel 1 [list set $domainVar $domain]
	uplevel 1 [list set $resourceVar $res]
    } else {
	return -code error "not valid jid form"
    }
}

# jlib::joinjid --
# 
#       Joins the, optionally empty, parts into a jid.
#       domain must be nonempty though.

proc jlib::joinjid {node domain resource} {
    
    set jid $domain
    if {$node != ""} {
	set jid ${node}@${jid}
    }
    if {$resource != ""} {
	set jid ${jid}/${resource}
    }
    return $jid
}

# jlib::jidequal --
# 
#       Checks if two jids are actually equal after mapped. Does not check
#       for prohibited characters.

proc jlib::jidequal {jid1 jid2} {
    
    return [string equal [jidmap $jid1] [jidmap $jid2]]
}

# jlib::jidvalidate --
# 
#       Checks if this is a valid jid interms of form and characters.

proc jlib::jidvalidate {jid} {
    
    if {$jid == ""} {
	return 0
    } elseif {[catch {splitjidex $jid node name resource} ans]} {
	return 0
    }
    foreach what {node name resource} {
	if {$what != ""} {
	    if {[catch {${what}prep [set $what]} ans]} {
		return 0
	    }
	}
    }
    return 1
}

# String preparation (STRINGPREP) RFC3454:
# 
#    The steps for preparing strings are:
#
#  1) Map -- For each character in the input, check if it has a mapping
#     and, if so, replace it with its mapping.  This is described in
#     section 3.
#
#  2) Normalize -- Possibly normalize the result of step 1 using Unicode
#     normalization.  This is described in section 4.
#
#  3) Prohibit -- Check for any characters that are not allowed in the
#     output.  If any are found, return an error.  This is described in
#     section 5.
#
#  4) Check bidi -- Possibly check for right-to-left characters, and if
#     any are found, make sure that the whole string satisfies the
#     requirements for bidirectional strings.  If the string does not
#     satisfy the requirements for bidirectional strings, return an
#     error.  This is described in section 6.

# jlib::*map --
# 
#       Does the mapping part.

proc jlib::nodemap {node} {

    return [string tolower $node]
}

proc jlib::namemap {domain} { 
 
    return [string tolower $domain]
}

proc jlib::resourcemap {resource} {
    
    # Note that resources are case sensitive!
    return $resource
}

# jlib::*prep --
# 
#       Does the complete stringprep.

proc jlib::nodeprep {node} {
    variable asciiProhibit
    
    set node [nodemap $node]
    if {[regexp ".*\[${asciiProhibit(node)}\].*" $node]} {
	return -code error "username contains illegal character(s)"
    }
    return $node
}

proc jlib::nameprep {domain} {   
    variable asciiProhibit
    
    set domain [namemap $domain]
    if {[regexp ".*\[${asciiProhibit(domain)}\].*" $domain]} {
	return -code error "domain contains illegal character(s)"
    }
    return $domain
}

proc jlib::resourceprep {resource} {
    variable asciiProhibit
    
    set resource [resourcemap $resource]
    
    # Orinary spaces are allowed!
    if {[regexp ".*\[${asciiProhibit(resource)}\].*" $resource]} {
	return -code error "resource contains illegal character(s)"
    }
    return $resource
}

# jlib::jidmap --
# 
#       Does the mapping part of STRINGPREP. Does not check for prohibited
#       characters.
#       
# Results:
#       throws an error if form unrecognized, else the mapped jid.

proc jlib::jidmap {jid} {
    
    if {$jid == ""} {
	return ""
    }
    # Guard against spurious spaces.
    set jid [string trim $jid]
    if {[catch {splitjidex $jid node domain resource} res]} {
	return -code error $res
    }
    return [joinjid [nodemap $node] [namemap $domain] [resourcemap $resource]]
}

# jlib::jidprep --
# 
#       Applies STRINGPREP to the individiual and specific parts of the jid.
#       
# Results:
#       throws an error if prohibited, else the prepared jid.

proc jlib::jidprep {jid} {
    
    if {$jid == ""} {
	return ""
    }
    if {[catch {splitjidex $jid node domain resource} res]} {
	return -code error $res
    }
    if {[catch {
	set node     [nodeprep $node]
	set domain   [nameprep $domain]
	set resource [resourceprep $resource]
    } err]} {
	return -code error $err
    }
    return [joinjid $node $domain $resource]
}

proc jlib::MapStr {str } {
    
    # TODO
}

# jlib::encodeusername, decodeusername, decodejid --
# 
#       Jid escaping.
#       JEP-0106 EXPERIMENTAL!

proc jlib::encodeusername {username} {    
    variable jidesc
    
    set str $username
    set ndx 0
    while {[regexp -start $ndx -indices -- "\[$jidesc\]" $str r]} {
	set ndx [lindex $r 0]
	scan [string index $str $ndx] %c chr
	set rep "#[format %.2x $chr];"
	set str [string replace $str $ndx $ndx $rep]
	incr ndx 3
    }
    return $str
}

proc jlib::decodeusername {username} {
    
    # Be sure that only the specific characters are being decoded.
    foreach sub {{#(20);} {#(22);} {#(23);} {#(26);} {#(27);} {#(2f);}  \
      {#(3a);} {#(3c);} {#(3e);} {#(40);}} {
	regsub -all $sub $username {[format %c 0x\1]} username
    }	
    return [subst $username]
}

proc jlib::decodejid {jid} {
    
    set jidlist [split $jid @]
    if {[llength $jidlist] == 2} {
	return "[decodeusername [lindex $jidlist 0]]@[lindex $jidlist 1]"
    } else {
	return $jid
    }
}

proc jlib::getdisplayusername {jid} {

    set jidlist [split $jid @]
    if {[llength $jidlist] == 2} {
	return [decodeusername [lindex $jidlist 0]]
    } else {
	return $jid
    }
}

proc jlib::setdebug {args} {
    variable debug
    
    if {[llength $args] == 0} {
	return $debug
    } elseif {[llength $args] == 1} {
	set debug $args
    } else {
	return -code error "Usage: jlib::setdebug ?integer?"
    }
}

proc jlib::Debug {num str} {
    global  fdDebug
    variable debug
    if {$num <= $debug} {
	if {[info exists fdDebug]} {
	    puts $fdDebug $str
	    flush $fdDebug
	}
	puts $str
    }
}

#--- namespace jlib::conference ------------------------------------------------

# jlib::conference --
#
#       Provides API's for the conference protocol using jabber:iq:conference.

proc jlib::conference {jlibname cmd args} {
    
    # Which command? Just dispatch the command to the right procedure.
    if {[catch {
	eval {[namespace current]::conference::$cmd $jlibname} $args
    } ans]} {
	return -code error $ans
    }
    return $ans
}

# jlib::conference::get_enter, set_enter --
#
#       Request conference enter room, and do enter room.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:         'roomname@conference.jabber.org' typically.
#       subelements xml list
#       cmd:        callback command for iq result element.
#       
# Results:
#       none.

proc jlib::conference::get_enter {jlibname room cmd} {

    [namespace parent]::Debug 3 "jlib::conference::get_enter room=$room, cmd=$cmd"
    
    set xmllist [wrapper::createtag "enter"  \
      -attrlist {xmlns jabber:iq:conference}]
    [namespace parent]::send_iq $jlibname "get" [list $xmllist] -to $room -command  \
      [list [namespace parent]::invoke_iq_callback $jlibname $cmd]
    [namespace parent]::service::setroomprotocol $jlibname $room "conference"
    return ""
}

proc jlib::conference::set_enter {jlibname room subelements cmd} {

    [namespace parent]::send_presence $jlibname -to $room
    [namespace parent]::send_iq $jlibname "set"  \
      [list [wrapper::createtag "enter" -attrlist {xmlns jabber:iq:conference} \
      -subtags $subelements]] -to $room -command  \
      [list [namespace current]::parse_set_enter $jlibname $room $cmd]
    return ""
}

# jlib::conference::parse_set_enter --
#
#       Callback for 'set_enter' and 'set_create'. 
#       Cache useful info to unburden client.
#       
# Arguments:
#       jlibname:   the instance of this jlib.
#       jid:        the jid we browsed.
#       cmd:        for callback to client.
#       type:       "ok" or "error"
#       subiq:

proc jlib::conference::parse_set_enter {jlibname room cmd type subiq} {    

    upvar ${jlibname}::conf conf

    [namespace parent]::Debug 3 "jlib::conference::parse_set_enter room=$room, cmd='$cmd', type=$type, subiq='$subiq'"
    
    if {[string equal $type "error"]} {
	uplevel #0 $cmd [list $jlibname error $subiq]
    } else {
	
	# Cache useful info:    
	# This should be something like:
	# <query><id>myroom@server/7y3jy7f03</id><nick/>snuffie<nick><query/>
	# Use it to cache own room jid.
	foreach child [wrapper::getchildren $subiq] {
	    set tagName [wrapper::gettag $child]
	    set value [wrapper::getcdata $child]
	    set $tagName $value
	}
	if {[info exists id] && [info exists nick]} {
	    set conf($room,hashandnick) [list $id $nick]
	}
	if {[info exists name]} {
	    set conf($room,roomname) $name
	}
	lappend conf(allroomsin) $room
	
	# And finally let client know.
	uplevel #0 $cmd [list $jlibname $type $subiq]
    }
}

# jlib::conference::get_create, set_create --
#
#       Request conference creation of room.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       to:        'conference.jabber.org' typically.
#       cmd:        callback command for iq result element.
#       
# Results:
#       none.

proc jlib::conference::get_create {jlibname to cmd} {

    [namespace parent]::Debug 3 "jlib::conference::get_create cmd=$cmd, to=$to"
    
    [namespace parent]::send_presence $jlibname -to $to
    set xmllist [wrapper::createtag "create"   \
      -attrlist {xmlns jabber:iq:conference}]
    [namespace parent]::send_iq $jlibname "get" [list $xmllist] -to $to -command   \
      [list [namespace parent]::invoke_iq_callback $jlibname $cmd]
}

proc jlib::conference::set_create {jlibname room subelements cmd} {

    # We use the same callback as 'set_enter'.
    [namespace parent]::send_presence $jlibname -to $room
    [namespace parent]::send_iq $jlibname "set"  \
      [list [wrapper::createtag "create" -attrlist {xmlns jabber:iq:conference} \
      -subtags $subelements]] -to $room -command  \
      [list [namespace current]::parse_set_enter $jlibname $room $cmd]
    [namespace parent]::service::setroomprotocol $jlibname $room "conference"
    return ""
}

# jlib::conference::delete --
#
#       Delete conference room.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       room:       'roomname@conference.jabber.org' typically.
#       cmd:        callback command for iq result element.
#       
# Results:
#       none.

proc jlib::conference::delete {jlibname room cmd} {

    set xmllist [wrapper::createtag {delete}  \
      -attrlist {xmlns jabber:iq:conference}]
    [namespace parent]::send_iq $jlibname "set" [list $xmllist] -to $room -command  \
      [list [namespace parent]::invoke_iq_callback $jlibname $cmd]
    return ""
}

proc jlib::conference::exit {jlibname room} {

    upvar ${jlibname}::conf conf
    upvar ${jlibname}::lib lib

    [namespace parent]::send_presence $jlibname -to $room -type unavailable
    set ind [lsearch -exact $conf(allroomsin) $room]
    if {$ind >= 0} {
	set conf(allroomsin) [lreplace $conf(allroomsin) $ind $ind]
    }
    $lib(rostername) clearpresence "${room}*"
    return ""
}

# jlib::conference::set_user --
#
#       Set user's nick name in conference room.
#
# Arguments:
#       jlibname:   the instance of this jlib.
#       room:       'roomname@conference.jabber.org' typically.
#       name:       nick name.
#       jid:        'roomname@conference.jabber.org/key' typically.
#       cmd:        callback command for iq result element.
#       
# Results:
#       none.

proc jlib::conference::set_user {jlibname room name jid cmd} {

    [namespace parent]::Debug 3 "jlib::conference::set_user cmd=$cmd, room=$room"
    
    set subelem [wrapper::createtag "user"  \
      -attrlist [list name $name jid $jid]]
    set xmllist [wrapper::createtag "conference"  \
      -attrlist {xmlns jabber:iq:browse} -subtags $subelem]
    [namespace parent]::send_iq $jlibname "set" [list $xmllist] -to $room -command  \
      [list [namespace parent]::invoke_iq_callback $jlibname $cmd]
}

# jlib::conference::hashandnick --
#
#       Returns list {kitchen@conf.athlon.se/63264ba6724.. mynickname}

proc jlib::conference::hashandnick {jlibname room} {

    upvar ${jlibname}::conf conf

    if {[info exists conf($room,hashandnick)]} {
	return $conf($room,hashandnick)
    } else {
	return -code error "Unknown room \"$room\""
    }
}

proc jlib::conference::roomname {jlibname room} {

    upvar ${jlibname}::conf conf

    if {[info exists conf($room,roomname)]} {
	return $conf($room,roomname)
    } else {
	return -code error "Unknown room \"$room\""
    }
}

proc jlib::conference::allroomsin {jlibname} {

    upvar ${jlibname}::conf conf
    
    set conf(allroomsin) [lsort -unique $conf(allroomsin)]
    return $conf(allroomsin)
}

#-------------------------------------------------------------------------------
