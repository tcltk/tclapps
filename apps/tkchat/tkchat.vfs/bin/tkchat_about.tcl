# overwrite the standard "about" dialog
# provide both credits and license information
proc ::tkchat::About {} {
    variable version

    # don't cache this window - if user reloads on the fly
    # we want to make sure it displays latest greatest info!
    set w .about
    destroy $w
    Dialog $w
    wm withdraw $w

    lassign [info commands ::tkchat::img::Tkchat] icon
    set ver [mc "Using Tcl %s, Tk %s" \
	[package provide Tcl] \
	[package provide Tk]]
    if {[package provide tls] ne {}} {
	append ver ", tls [package provide tls]"
    }

    set bf [ttk::frame $w.buttonframe]
    ttk::button $bf.credits \
	-text [mc "Credits"] \
	-command [list [namespace which About-Credits] $w.text]
    ttk::button $bf.license \
	-text [mc "License"] \
	-command [list [namespace which About-License] $w.text]
    ttk::button $bf.close \
	-text [mc "Close"] \
	-command [list destroy $w] \
	-default active
    ttk::label $w.title \
	-text [mc "TkChat v%s" $version] \
	-font {Courier -18 bold}
    ttk::label $w.ver \
	-text $ver \
	-font {Sans -12 bold}
    if {$icon ne ""} {
	$w.title configure \
	    -image $icon \
	    -compound left
    }
    ScrolledWidget text $w.text 0 1 \
	-height 20 \
	-width 80 \
	-padx 2 -pady 2
    $w.text tag configure title \
	-justify center \
	-font [dict merge [font actual TkTextFont] {-weight bold}]
    About-Credits $w.text

    pack $bf.close $bf.license $bf.credits -padx {6 0} -pady 6 -side right
    grid $w.title -pady 6
    grid $w.ver   -pady 6
    grid $w.text  -sticky news -padx 6
    grid $bf      -sticky e -padx 6
    grid rowconfigure $w $w.text    -weight 1
    grid columnconfigure $w $w.text -weight 1

    bind $w <Return> [list $bf.close invoke]
    bind $w <Escape> [list $bf.close invoke]

    wm title $w [mc "About TkChat %s" $version]
    if {$icon ne ""} {
	catch {wm iconphoto $w $icon}
    }
    tk::PlaceWindow $w widget .
    wm deiconify $w
}

proc ::tkchat::About-Credits {t} {
    $t configure -state normal
    $t delete 1.0 end
    $t insert end [mc "Copyright (c) %s by the following authors" "2001-2022"] \
	{title} "\n\n"
    lappend txt "Bruce B Hartweg"	"<brhartweg@bigfoot.com>"
    lappend txt "Don Porter"		"<dgp@users.sourceforge.net>"
    lappend txt "Pat Thoyts"		"<patthoyts@users.sourceforge.net>"
    lappend txt "Jeff Hobbs"		"<jeffh@activestate.com>"
    lappend txt "Ryan Casey"		"<scfied@hotmail.com>"
    lappend txt "Reinhard Max"		"<max@tclers.tk>"
    lappend txt "D. Richard Hipp"	"<drh@hwaci.com>"
    lappend txt "Kevin Kenny"		"<kennykb@users.sourceforge.net>"
    lappend txt "Pascal Scheffers"	"<pascal@scheffers.net>"
    lappend txt "Joe English"		"<jenglish@users.sourceforge.net>"
    lappend txt "Joe Mistachkin"	"<joe@mistachkin.com>"
    lappend txt "Donal K. Fellows"	"<dkf@users.sourceforge.net>"
    lappend txt "Daniel South"		"<wildcard_25@users.sourceforge.net>"
    lappend txt "Steve Landers"		"<steve@digitalsmarties.com>"
    lappend txt "Elchonon Edelson"	"<eee@users.sourceforge.net>"
    lappend txt "Kevin Walzer"		"<kw@codebykevin.com>"
    lappend txt "Emiliano Gavil\u00e1n" "<emilianogavilan@gmail.com>"
    insertHelpText $t $txt
    $t delete "end -1 char" end
    $t configure -state disabled
}

proc ::tkchat::About-License {t} {
    global tkchat_dir

    $t configure -state normal
    $t delete 1.0 end
    $t insert end [mc "License terms"] {title} "\n\n"
    set fd [open [file join $tkchat_dir license.terms]]
    $t insert end [chan read -nonewline $fd]
    chan close $fd
    $t configure -state disabled
}