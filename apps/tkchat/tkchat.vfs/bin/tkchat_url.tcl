# Fetch a URL with added caching. Provided the request is a plain
# GET and doesnt use -handler we can try caching and avoid some traffic.
# This will help with the TIP index and RSS feeds.
# We should also check the domain and see what cookies might be set.

package require http
package require sha1

namespace eval tkchat {
    namespace eval webcache {
        variable cache
        variable cookiejar
    }
}

proc tkchat::webcache::spliturl {url} {
    set URLmatcher {(?x)		# this is _expanded_ syntax
	^
	(?: (\w+) : ) ?			# <protocol scheme>
	(?: //
	    (?:
		(
		    [^@/\#?]+		# <userinfo part of authority>
		) @
	    )?
	    ( [^/:\#?]+ )		# <host part of authority>
	    (?: : (\d+) )?		# <port part of authority>
	)?
	( / [^\#]*)?			# <path> (including query)
	(?: \# (.*) )?			# <fragment>
	$
    }
    if {![regexp -- $URLmatcher $url -> scheme user host port path]} {
	return -code error "unsupported url '$url'"
    }
    set query {}
    if {[set n [string first ? $path]] != -1} {
        set query [string range $path $n end]
        set path [string range $path 0 [incr n -1]]
    }
    return [list scheme $scheme user $user host $host port $port \
                path $path query $query]
}

proc tkchat::webcache::appendcookies {url argsvarname} {
    variable cookiejar
    upvar 1 $argsvarname args
    array set parts [spliturl $url]
    # look in jar for cookies by domain and path and expiry
    set cookies [array get cookiejar $parts(host),*]
    # TODO: split the path and query and match path
    # TODO: check for expired cookies and remove them.
    lappend args -headers [list "X-Tkchat-Webcache" "test;q=0.1"]
    return
}

proc tkchat::webcache::log {m} {puts stderr $m}

proc tkchat::webcache::geturl {url args} {
    variable cachedir

    # Switch the result to pass via our response handler
    set cmd {}
    set newargs {}
    foreach {opt val} $args {
        if {$opt eq "-command"} {
            set cmd $val
        } else {
            lappend newargs $opt $val
        }
    }
    lappend newargs -command [list [namespace origin on_response] $url $cmd]
    appendcookies $url newargs

    # TODO: avoid POST
    set id [sha1::sha1 -hex $url]
    if {[file exists [file join $cachedir $id]]} {
        log "HEAD $url"
        set tok [eval [linsert $newargs 0 http::geturl $url -validate 1]]
    } else {
        log "GET $url"
        set tok [eval [linsert $newargs 0 http::geturl $url]]
    }
    
    # was not an async request - better wait here.
    if {$cmd eq {}} {
        http::wait $tok
    }
    return $tok
}

proc tkchat::webcache::on_response {url cmd tok} {
    if {[http::status $tok] eq "ok" 
        && [http::ncode $tok] >= 200 
        && [http::ncode $tok] < 300
    } then {
        preservecookies $url $tok
        cacheresult $url $cmd $tok
    } else {
        uplevel #0 [linsert $cmd end $tok]
    }
}

proc tkchat::webcache::preservecookies {url tok} {
    variable cookiejar
    array set parts [spliturl $url]
    set domain $parts(host)
    foreach {name value} [http::meta $tok] {
        if {[string equal -nocase "set-cookie" $name]} {
            array set cookie [set crumbs [parsecookie $value]]
            set key $cookie(domain),$cookie($path),$cookie(expires)
            set cookiejar($key) $crumbs
        }
    }
}

proc tkchat::webcache::parsecookie {data} {
    return [list domain "" path "" expires 0]
}

# Process a web response.
# If this was a HEAD request, compare with our cache copy and
# either fixup the http token (HIT) or issue a GET request (MISS).
# On a GET request, call the MISS path and update the cache.
proc tkchat::webcache::cacheresult {url cmd tok} {
    
}

# unit test
if {[info exists argv0] && ([info script] eq $argv0)} {
    namespace eval tkchat::webcache {
        
        foreach {n url test} {
            1 http://www.example.com/a/b/c.html
              {scheme http user {} host www.example.com port {} path /a/b/c.html query {}}
            2 http://www.example.com/a/b?q=1
              {scheme http user {} host www.example.com port {} path /a/b query ?q=1}
            3 http://www.example.com/a/b?q=1&b=2/3/4
              {scheme http user {} host www.example.com port {} path /a/b query ?q=1&b=2/3/4}
            4 https://www.example.com/a/b/c
              {scheme https user {} host www.example.com port {} path /a/b/c query {}}
            5 http://www.example.com:8080/a/b/c?q=1&b=2
              {scheme http user {} host www.example.com port 8080 path /a/b/c query ?q=1&b=2}
            6 http://www.example.com/?q=1b=2/3
              {scheme http user {} host www.example.com port {} path / query ?q=1b=2/3}
            7 http://user@www.example.com/
              {scheme http user user host www.example.com port {} path / query {}}
            8 http://user:pass@www.example.com/
              {scheme http user user:pass host www.example.com port {} path / query {}}
            9 http://user:pass@www.example.com/?q=1@2
              {scheme http user user:pass host www.example.com port {} path / query ?q=1@2}
        } {
            if {[spliturl $url] ne $test} {
                puts "failed: spliturl-$n\n\t$test\n\t[spliturl $url]"
            }
        }

        # parsecookie
        foreach {n str test} {
            1.0 {Customer="WILE_E_COYOTE"; Version="1"; Path="/acme"}
            {}
            1.1 {Part_Number="Rocket_Launcher_0001"; Version="1"; Path="/acme"}
            {}
            
            3.0 {CommunityServer-LastVisitUpdated-1001=; path=/}
            {}
            3.1 {CommunityServer-UserCookie1001=lv=1/1/1999 12:00:00 AM&mra=1/27/2010 4:54:21 AM; expires=Thu, 27-Jan-2011 12:54:22 GMT; path=/}
            {}
        } {
            if {[parsecookie $str] ne $test} {
                puts "failed parsecookie-$n\n\t$test\n\t[parsecookie $str]"
            }
        }
    }
}

