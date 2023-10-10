#  jlibdns.tcl --
#
#      This file is part of the jabberlib.
#      It provides support for XEP-0156:
#          A DNS TXT Resource Record Format for XMPP Connection Methods
#      and client DNS SRV records (RFC 6120 XMPP Core sect. 3.2)
#
#  Copyright (c) 2006-2008  Mats Bengtsson
#  Copyright (c) 2023       Emiliano Gavilán
#
# This file is distributed under BSD style license.
#
#
############################# USAGE ############################################
#
#   NAME
#      jlib::dns - library for DNS lookups
#
#   SYNOPSIS
#      jlib::dns::get_addr_port domain cmd
#      jlib::dns::get_http_bind_url domain cmd   OUTDATED
#      jlib::dns::get_http_poll_url domain cmd
#      jlib::dns::get_http_bosh_url domain cmd
#
############################# 7.1.2 Initial Registration #######################
#
# <method>
#   <name>_xmpp-client-httppoll</name>
#   <desc>HTTP Polling connection method</desc>
#   <syntax>
#     The http: or https: URL at which to contact the HTTP Polling connection manager or proxy
#   </syntax>
#   <doc>XEP-0025</doc>
# </method>
#
# <method>
#   <name>_xmpp-client-xbosh</name>
#   <desc>XMPP Over Bosh connection method</desc>
#   <syntax>
#     The http: or https: URL at which to contact the HTTP Binding connection manager or proxy
#   </syntax>
#   <doc>XEP-0206</doc>
# </method>

# package require dns 9.9    ;# Fake version to avoid loding buggy version.
package require dns

package provide jlib::dns 0.1

namespace eval jlib::dns {

    variable owner
    array set owner {
	client      _xmpp-client._tcp
	poll        _xmppconnect
    }

    variable nameA
    array set nameA {
	bind        _xmpp-client-httpbind
	bosh        _xmpp-client-xbosh
	poll        _xmpp-client-httppoll
    }
}

proc jlib::dns::get_addr_port {domain cmd args} {
    # dns::resolve may throw error!
    dns::resolve _xmpp-client._tcp.$domain \
	-type SRV \
	-command [list [namespace current]::addr_cb $cmd] \
	{*}$args
}

proc jlib::dns::addr_cb {cmd token} {

    set addrList {}
    if {[dns::status $token] eq "ok"} {
	# set result [dns::result $token]
	foreach reply [dns::result $token] {
	    if {![dict exists $reply rdata]} continue
	    set rdata [dict get $reply rdata]
	    if {[dict exists $rdata priority] &&
		[dict exists $rdata weight]   &&
		[dict exists $rdata port]     &&
		[dict exists $rdata target]   &&
		[isUInt16 [dict get $rdata priority]] &&
		[isUInt16 [dict get $rdata weight]]   &&
		[isUInt16 [dict get $rdata port]]     &&
		([dict get $rdata target] ne ".")
	    } then {
		if {[dict get $rdata weight] == 0} {
		    set n 0
		} else {
		    set n [expr { ([dict get $rdata weight] + 1) * rand() }]
		}
		set priority [expr { [dict get $rdata priority] * 65536 - $n }]
		lappend addrList [list $priority \
		    [dict get $rdata target] \
		    [dict get $rdata port]]
	    }
	}
	if {[llength $addrList]} {
	    set addrPort {}
	    foreach p [lsort -real -index 0 $addrList] {
		lappend addrPort [lrange $p 1 2]
	    }
	    uplevel #0 [list {*}$cmd $addrPort]
	} else {
	    uplevel #0 [list {*}$cmd {} dns-empty]
	}
    } else {
	uplevel #0 [list {*}$cmd {} [dns::error $token]]
    }

    dns::cleanup $token
}

proc jlib::dns::isUInt16 {n} {
    expr {[string is integer -strict $n] && $n >= 0 && $n < 65536}
}

proc jlib::dns::get_http_bind_url {domain cmd args} {
    dns::resolve _xmppconnect.$domain \
	-type TXT \
	-command [list [namespace current]::http_cb bind $cmd] \
	{*}$args
}

proc jlib::dns::get_http_bosh_url {domain cmd args} {
    dns::resolve _xmppconnect.$domain \
	-type TXT \
	-command [list [namespace current]::http_cb bosh $cmd] \
	{*}$args
}

proc jlib::dns::get_http_poll_url {domain cmd args} {
    dns::resolve _xmppconnect.$domain \
	-type TXT \
	-command [list [namespace current]::http_cb poll $cmd] \
	{*}$args
}

proc jlib::dns::http_cb {attr cmd token} {
    variable nameA

    set found 0
    if {[dns::status $token] eq "ok"} {
	foreach reply [dns::result $token] {
	    if {[dict exists $reply rdata] &&
		[regexp "$nameA($attr)=(.*)" [dict get $reply rdata] - url]
	    } then {
		set found 1
		uplevel #0 [list {*}$cmd $url]
	    }
	}
	if {!$found} {
	    uplevel #0 [list {*}$cmd {} dns-no-resource-record]
	}
    } else {
	uplevel #0 [list {*}$cmd {} [dns::error $token]]
    }

    dns::cleanup $token
}

# Test
proc jlib::dns::Test {} {
    set cb {apply {{domain res {err ""}} {
	puts -nonewline "$domain -> "
	if {$res ne ""} {
	    puts "lookup response: $res"
	} else {
	    puts "lookup error: $err"
	}
    }}}
    foreach domain {
	gmail.com jabber.ru jabber.com jabber.cz tigase.org all.tclers.tk
    } {
	get_addr_port $domain [list {*}$cb $domain]
    }

    # Missing
    foreach domain {gmail.com jabber.ru jabber.at} {
	get_http_bosh_url $domain [list {*}$cb $domain]
    }
}
