# rssrdr.tcl - Copyright (C) 2007 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# Simple parser for RSS XML files.
# If rss::data returns a list of lists containing the data for each item
# in the RSS file.

package require wrapper;          # jabberlib 

namespace eval ::rss {
    variable version 1.0
    variable uid ; if {![info exists uid]} { set uid 0 }
}

proc ::rss::create {} {
    variable uid
    set Rss [namespace current]::rss[incr uid]
    upvar #0 $Rss rss
    array set rss {status ok data {} type rss}
    set rss(parser) [wrapper::new \
                         [list [namespace origin StreamStart] $Rss] \
                         [list [namespace origin StreamEnd]   $Rss] \
                         [list [namespace origin StreamParse] $Rss] \
                         [list [namespace origin StreamError] $Rss]]
    return $Rss
}

proc ::rss::parse {Rss xml} {
    upvar #0 $Rss rss
    Reset $Rss
    if {[catch {wrapper::parse $rss(parser) $xml} err]} {
        StreamError $Rss $err
    }
    return
}

proc ::rss::status {Rss} {
    upvar #0 $Rss rss
    return $rss(status)
}

proc ::rss::data {Rss} {
    upvar #0 $Rss rss
    if {[info exists rss(data)]} {
        return $rss(data)
    }
    return {}
}

proc ::rss::channel {Rss} {
    upvar #0 $Rss rss
    array set channel {title "" mtime 0}
    if {[info exists rss(channel)]} {
        array set channel $rss(channel)
    }
    return [array get channel]
}

proc ::rss::error {Rss} {
    upvar #0 $Rss rss
    if {[info exists rss(error)]} {
        return $rss(error)
    }
    return {}
}

# -------------------------------------------------------------------------
# Internal methods

proc ::rss::Reset {Rss} {
    upvar #0 $Rss rss
    catch {wrapper::reset $rss(parser)}
    array set rss {status ok data "" xlist "" type rss channel {}}
    unset -nocomplain rss(error)
}

proc ::rss::StreamStart {Rss args} {
    upvar #0 $Rss rss
    #puts "$Rss start $args"
    array set a $args
    if {[info exists a(xmlns)]} {
        if {$a(xmlns) eq "http://www.w3.org/2005/Atom"} {
            set rss(type) atom
        }
    }
    return
}

proc ::rss::StreamEnd {Rss} {
    #upvar #0 $Rss rss
    #puts "$Rss end"
    #wrapper::reset $rss(parser)
    return
}

proc ::rss::StreamError {Rss args} {
    upvar #0 $Rss rss
    set rss(status) error
    set rss(error) $args
    puts "$Rss error $args"
    wrapper::reset $rss(parser)
    return
}

proc ::rss::StreamParse {Rss xlist} {
    upvar #0 $Rss rss
    if {[catch {
        switch -exact -- $rss(type) {
            atom { StreamParseAtom $Rss $xlist }
            rss  { StreamParseRss  $Rss $xlist }
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

proc ::rss::StreamParseRss {Rss xlist} {
    upvar #0 $Rss rss
    set r {}
    if {[set root [wrapper::gettag $xlist]] ne "channel"} {
        return -code error "invalid RSS data: root element\
            \"$root\" must be \"channel\""
    }
    foreach item [wrapper::getchildren $xlist] {
        switch -exact -- [set tag [wrapper::gettag $item]] {
            description -
            link -
            title { lappend rss(channel) $tag [wrapper::getcdata $item] }
            pubDate { lappend rss(channel) mtime [ParseRssDate [wrapper::getcdata $item]] }
            item {
                set e {}
                foreach node [wrapper::getchildren $item] {
                    set ntag [wrapper::gettag $node]
                    switch -exact -- $ntag {
                        pubDate {lappend e mtime [ParseRssDate [wrapper::getcdata $node]]}
                        default {
                            lappend e $ntag [string trim [wrapper::getcdata $node]]
                        }
                    }
                }
                if {[llength $e] > 0} {lappend r $e}
            }
        }
    }
    set rss(status) ok
    set rss(data) $r
    return
}

proc ::rss::StreamParseAtom {Rss xlist} {
    upvar #0 $Rss rss
    set tag [wrapper::gettag $xlist]
    switch -exact [set tag [wrapper::gettag $xlist]] {
        title { lappend rss(channel) title [wrapper::getcdata $xlist] }
        updated {
            catch {
                set date [wrapper::getcdata $xlist]
                lappend rss(channel) mtime [ParseAtomDate $date]
            }
        }
        entry {
            set e {}
            set mtime 0
            foreach node [wrapper::getchildren $xlist] {
                set ntag [wrapper::gettag $node]
                switch -exact -- $ntag {
                    title {lappend e title [wrapper::getcdata $node]}
                    link {lappend e link [wrapper::getattribute $node href]}
                    author {
                        set authors {}
                        foreach anode [wrapper::getchildren $node] {
                            if {[wrapper::gettag $anode] eq "name"} {
                                lappend authors [wrapper::getcdata $anode]
                            }
                        }
                        if {[llength $authors]>0} {lappend e author $authors}
                    }
                    id {lappend e id [wrapper::getcdata $node]}
                    published - updated {
                        if {[set t [ParseAtomDate [wrapper::getcdata $node]]] > $mtime} {
                            set mtime $t
                        }
                    }
                    summary - content {lappend e description [wrapper::getcdata $node]}
                    default { puts "unhandle entry tag \"$ntag\""}
                }
            }
            if {[llength $e] > 0} {lappend e mtime $mtime ; lappend rss(data) $e}
        }
    }
    return
}

# Try various ways to read an RSS timestamp.
# RSS specifies pubDate as RFC822 time format
# ie: Sat, 07 Sep 2002 00:00:01 GMT
# Atom uses RFC3339 (ISO8601)
if {[package vsatisfies [package provide Tcl] 8.5]} {

    proc ::rss::ParseAtomDate {date} {
        set date [string trim $date]
        if {[catch {clock scan $date -format {%Y-%m-%dT%T%Z}} time]} {
            if {[catch {clock scan $date} time]} {
                set time 0
            }
        }
        return $time
    }
    proc ::rss::ParseRssDate {date} {
        set date [string trim $date]
        if {[catch {clock scan $date -format {%a, %d %b %Y %T %Z}} time]} {
            if {[catch {clock scan $date} time]} {
                set time 0
            }
        }
        return $time
    }

} else {

    proc ::rss::ParseAtomDate {date} {
        set date [string trim $date]
        if {[catch {clock scan [string map {- ""} $date} time]} {
            if {[catch {clock scan $date} time]} {
                set time 0
            }
        }
        return $time
    }
    proc ::rss::ParseRssDate {date} {
        set date [string trim $date]
        if {[catch {clock scan $data} time]} {
            set time 0
        }
        return $time
    }
}

package provide rssrdr $::rss::version
