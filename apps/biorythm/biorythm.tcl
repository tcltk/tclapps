#!/bin/sh
# The next line is executed by /bin/sh, but not tcl \
exec wish $0 ${1+"$@"}

#
# This was a tclet/app example of showing biorythms originally
# written by Ken Corey while at SunLabs.
#
# http://www.tcl-tk.net/software/plugin/bio.html
#
# RCS: @(#) $Id: biorythm.tcl,v 1.1 2001/11/07 22:30:33 hobbs Exp $

# Set up global vars.
set width 400
set height 200
set yaw [expr $height/2]
set emotional 0.22439947525641
set physical  0.27318196987737
set intellectual 0.19039955476302
set newtodaydate {}
set bogusdays 10
set boguslength 0
set lastdays 7
set lmargin -100
set state 0
set dateitem {}

# Make first line of widgets
frame .db
button .db.today -text "Today!" -command {set todaysdate $origtoday;adjustdate;calcbios $lastdays;.c itemconfig $dateitem -text $todaysdate}
pack .db.today -side left
label .db.t2 -text "Starting Date:"
entry .db.t -textvariable todaysdate
label .db.t1 -text "Birthdate:"
entry .db.b -textvariable birthdate
pack .db.t2 -side left
pack .db.t -side left
pack .db.t1 -side left
pack .db.b -side left
pack .db

#Make second line
frame .td
label .td.td -text {}
pack .td.td
pack .td

set origtoday [clock format [clock seconds] -format {%d %b %Y}]
.db.t insert end $origtoday
set newtodaydate [clock scan $origtoday]
set origbirthdate "06 Nov 1973"
.db.b insert end $origbirthdate

#Make third line
frame .dd
label .dd.lb -text "\# days to graph:"
pack .dd.lb -side left -anchor sw
scale .dd.days -from 14 -to 1 -command {calcbios} -orient horiz -length $width \
    -font fixed -variable bogusdays
pack .dd.days -side right -padx 6
pack .dd -fill x
frame .bb
label .bb.lb -text "Graph Date Adj:"
pack .bb.lb -side left -anchor sw
scale .bb.days -from -120 -to 120 -command {adjustdate} -orient horiz -length $width \
    -font fixed -variable boguslength
pack .bb.days -side right -padx 6
pack .bb -fill x

# When the user releases button 1, do this:
bind .bb.days  {
    set todaysdate [.c itemcget $dateitem -text]
    calcbios $lastdays
    set boguslength 0
}

# Where to draw the canvas.
canvas .c -relief sunken -bd 2 -height [expr $height+32] -width [expr $width-($lmargin-2)] \
    -scrollregion [list $lmargin 0 $width $height]
pack .c

# Put the decorations:
.c create line 0 0 0 $height -fill black
.c create line 0 $yaw $width $yaw -fill black
.c create text $lmargin 0 -fill blue -text HIGH -anchor nw
.c create text $lmargin $height -fill red -text LOW -anchor sw
.c create text -10 [expr $yaw-20] -fill red -font fixed -text Emotional -anchor e
.c create text -10 $yaw           -fill green -font fixed -text Physical -anchor e
.c create text -10 [expr $yaw+20] -fill blue -font fixed -text Intellectual -anchor e
set dateitem [.c create text 0 [expr $height+4]  -fill black -font fixed -text $todaysdate -anchor n]

# Set up some eventhandling
bind .db.b  {calcbios $lastdays}
bind .db.t  {set newtodaydate $todaysdate;adjustdate;calcbios $lastdays;.c itemconfig $dateitem -text $todaysdate}
bind .db.b  {calcbios $lastdays}
bind .db.t  {set newtodaydate $todaysdate;adjustdate;calcbios $lastdays;.c itemconfig $dateitem -text $todaysdate}

# Called when moving the bottom slider.  Adjusts the date that will be shown.
proc adjustdate {{adjustvalue 0}} {
    global todaysdate newtodaydate lastdays dateitem

    if {[catch {clock scan $todaysdate} t] < 1} {
        set newtodaydate [expr $t + $adjustvalue*86400]
        calcbios $lastdays
        .c itemconfig $dateitem -text [clock format $newtodaydate -format {%d %b %Y}]
    }
}

# When the dates to display have changed, redraw them.
proc calcbios { numdays } {
    global birthdate todaysdate line width height yaw emotional physical
    global origtoday intellectual state origbirthdate origtoday lastdays
    global newtodaydate

    if {$state < 1} {
        set state 1
        set thisdate [clock format $newtodaydate -format {%d %b %Y}]
        if {[catch {clock scan $thisdate} t] < 1} {
            if {[catch {clock scan $birthdate} u] < 1} {
                set totaldays [expr ($t-$u)/86400]
                .td.td config -text "Total Days: $totaldays (~[format "%0.1f" [expr $totaldays/365.25]] years)"
                foreach {edat pdat idat} {{} {} {}} {}
                set space {}
                for {set i 0} {$i <= $numdays} {incr i} {
                    set lx($i) [expr $i*($width/$numdays)]
                    append edat "$space$lx($i) [expr $yaw - (sin($emotional*($totaldays+$i))*$yaw)]"
                    append pdat "$space$lx($i) [expr $yaw - (sin($physical*($totaldays+$i))*$yaw)]"
                    append idat "$space$lx($i) [expr $yaw - (sin($intellectual*($totaldays+$i))*$yaw)]"
                    set space { }
                }
                set limit [expr $numdays+4]
                for {set i 0} {$i <= $limit} {incr i} {
                    catch {.c delete $line($i)}
                }
                set rise [expr $yaw-($height/20)]
                set fall [expr $yaw+($height/20)]
                for {set i 0} {$i <= $numdays} {incr i} {
                    set line($i) [.c create line $lx($i) $rise $lx($i) $fall -fill black]
                }
                set line($i) [eval .c create line $edat -fill red -splinesteps 1]
                incr i
                set line($i) [eval .c create line $pdat -fill green -splinesteps 1]
                incr i
                set line($i) [eval .c create line $idat -fill blue -splinesteps 1]
            } else {
                Fakedialog "That's not a valid Birthdate... setting it back to $origbirthdate." Ok
                set birthdate $origbirthdate
            }
        } else {
            Fakedialog "That's not a valid starting date... setting it back to today's date." Ok
            set todaysdate $origtoday
        }
        set lastdays $numdays
        set state 0
    }
}

# A routine to put up a 'fake' dialog box in front of the program,
# since toplevels are a little rude from within Netscape.
proc Fakedialog {text button {button2 {}} {button3 {}}} {
    set buttonstatus {}
    frame .top -bd 2 -relief raised
    frame .top.r -bd 2 -relief sunken
    message .top.r.lb -text $text
    frame .top.r.bs
    frame .top.r.bs.f
    set buttonstatus
    button .top.r.bs.f.b1 -text $button -command {set buttonstatus 1}
    pack .top.r.bs.f.b1 -side left -padx 10 -pady 10
    if {$button2 != {}} {
        button .top.r.bs.f.b2 -text $button2 -command {set buttonstatus 2}
        pack .top.r.bs.f.b2 -side left -padx 10 -pady 10
    }
    if {$button3 != {}} {
        button .top.r.bs.f.b3 -text $button3 -command {set buttonstatus 3}
        pack .top.r.bs.f.b3 -side left -padx 10 -pady 10
    }
    pack .top.r.bs.f -side bottom
    pack .top.r.bs -side bottom -fill x
    pack .top.r.lb -expand 1 -pady 20 -padx 10
    pack .top.r -expand 1
    place .top -relx .5 -rely .5 -anchor c
    set oldfocus [focus -lastfor .]
    focus .top.r.bs.f.b1
    bind .top.r.bs.f.b1  {
        tkButtonInvoke %W
    }
    grab .
    tkwait variable buttonstatus
    grab release .
    destroy .top
    focus $oldfocus
    return $buttonstatus
}

calcbios $lastdays
