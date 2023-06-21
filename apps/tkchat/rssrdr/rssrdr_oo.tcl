# rssrdr.tcl - Copyright (C) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# Simple parser for RSS XML files.
# If rss::data returns a list of lists containing the data for each item
# in the RSS file.

package require wrapper;          # jabberlib

namespace eval ::rss {
    variable version 1.0

    proc ParseAtomDate {date} {
	set date [string trim $date]
	if {[catch {clock scan $date -format {%Y-%m-%dT%T%Z}} time]} {
	    if {[catch {clock scan $date} time]} {
		set time 0
	    }
	}
	return $time
    }

    proc ParseRssDate {date} {
	set date [string trim $date]
	if {[catch {clock scan $date -format {%a, %d %b %Y %T %Z}} time]} {
	    if {[catch {clock scan $date} time]} {
		set time 0
	    }
	}
	return $time
    }

    namespace export Parse*
}

oo::class create ::rss::Rss {
    variable rss

    constructor {} {
	namespace import ::rss::Parse*
	array set rss {
	    status      ok
	    data        {}
	    type        rss
	}
	set my [namespace which my]
	set rss(parser) [wrapper::new \
	    [list $my StreamStart]    \
	    [list $my StreamEnd]      \
	    [list $my StreamParse]    \
	    [list $my StreamError]]
    }

    method parse {xml} {
	my Reset
	set rss(raw) $xml
	if {[catch {wrapper::parse $rss(parser) $xml} err]} {
	    my StreamError $err
	}
	return
    }

    method status {} {
	return $rss(status)
    }

    method data {} {
	if {[info exists rss(data)]} {
	    return $rss(data)
	}
	return
    }

    method channel {} {
	set channel [dict create title "" mtime 0]
	if {[info exists rss(channel)]} {
	    set channel [dict merge $channel $rss(channel)]
	}
	return $channel
    }

    method error {} {
	if {[info exists rss(error)]} {
	    return $rss(error)
	}
	return
    }

    # private methods

    method Reset {} {
	catch {wrapper::reset $rss(parser)}
	array set rss {
	    status      ok
	    data        ""
	    xlist       ""
	    type        rss
	    channel     {}
	}
	unset -nocomplain rss(error)
    }

    method StreamStart {args} {
	#puts "[self] start $args"
	if {[dict exists $args xmlns] &&
	    [dict get $args xmlns] eq "http://www.w3.org/2005/Atom"
	} then {
	    set rss(type) atom
	}
	return
    }

    method StreamEnd {} {
	#puts "[self] end"
	#wrapper::reset $rss(parser)
	return
    }

    method StreamError {args} {
	set rss(status) error
	set rss(error) $args
	#puts "[self] error $args"
	wrapper::reset $rss(parser)
	return
    }

    method StreamParse {xlist} {
	if {[catch {
	    switch -exact -- $rss(type) {
		atom {
		    my StreamParseAtom $xlist
		}
		rss  {
		    my StreamParseRss $xlist
		}
		default {
		    error "invalid feed type \"$rss(type)\""
		}
	    }
	} err]} {
	    set rss(status) error
	    set rss(error) $err
	    set rss(xlist) $xlist
	    return -code error $err
	}
	return
    }

    method StreamParseRss {xlist} {
	set r {}
	if {[set root [wrapper::gettag $xlist]] ne "channel"} {
	    return -code error "invalid RSS data: root element\
		\"$root\" must be \"channel\""
	}
	foreach item [wrapper::getchildren $xlist] {
	    switch -exact -- [set tag [wrapper::gettag $item]] {
		description     -
		link            -
		title {
		    dict set rss(channel) $tag [wrapper::getcdata $item]
		}
		pubDate {
		    dict set rss(channel) mtime [ParseRssDate [wrapper::getcdata $item]]
		}
		item {
		    set e {}
		    foreach node [wrapper::getchildren $item] {
			set ntag [wrapper::gettag $node]
			switch -exact -- $ntag {
			    pubDate {
				dict set e mtime [ParseRssDate [wrapper::getcdata $node]]}
			    default {
				dict set e $ntag [string trim [wrapper::getcdata $node]]
			    }
			}
		    }
		    if {[dict size $e]} {
			lappend r $e
		    }
		}
	    }
	}
	set rss(status) ok
	set rss(data) $r
	return
    }

    method StreamParseAtom {xlist} {
	switch -exact [set tag [wrapper::gettag $xlist]] {
	    title {
		dict set rss(channel) title [wrapper::getcdata $xlist]
	    }
	    updated {
		catch {
		    set date [wrapper::getcdata $xlist]
		    dict set rss(channel) mtime [ParseAtomDate $date]
		}
	    }
	    entry {
		set e {}
		set mtime 0
		foreach node [wrapper::getchildren $xlist] {
		    set ntag [wrapper::gettag $node]
		    switch -exact -- $ntag {
			title {
			    dict set e title [wrapper::getcdata $node]
			}
			link {
			    dict set e link [wrapper::getattribute $node href]
			}
			author {
			    set authors {}
			    foreach anode [wrapper::getchildren $node] {
				if {[wrapper::gettag $anode] eq "name"} {
				    lappend authors [wrapper::getcdata $anode]
				}
			    }
			    if {[llength $authors]} {
				dict set e author $authors
			    }
			}
			id {
			    dict set e id [wrapper::getcdata $node]
			}
			published -
			updated {
			    if {[set t [ParseAtomDate [wrapper::getcdata $node]]] > $mtime} {
				set mtime $t
			    }
			}
			summary -
			content {
			    dict set e description [wrapper::getcdata $node]
			}
			default {
			    puts "unhandle entry tag \"$ntag\""
			}
		    }
		}
		if {[dict size $e]} {
		    dict set e mtime $mtime
		    lappend rss(data) $e
		}
	    }
	}
	return
    }
}

package provide rssrdr_oo $::rss::version
