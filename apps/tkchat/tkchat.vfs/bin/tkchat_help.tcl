proc ::tkchat::Help {} {
    variable version

    set w .qhelp
    destroy $w
    Dialog $w
    wm withdraw $w

    lassign [info commands ::tkchat::img::Tkchat] icon
    set title [mc "TkChat %s help" $version]

    ttk::label $w.title \
	-text $title \
	-font {Courier -18 bold}
    if {$icon ne ""} {
	$w.title configure \
	    -image $icon \
	    -compound left
    }
    ScrolledWidget text $w.text 0 1 \
	-height 24 \
	-width 90 \
	-wrap word
    ttk::button $w.b \
	-text [mc "Close"] \
	-command [list destroy $w] \
	-default active

    grid $w.title -pady 6
    grid $w.text  -sticky news -padx 6
    grid $w.b     -sticky e -pady 6 -padx 6
    grid rowconfigure $w $w.text -weight 1
    grid columnconfigure $w $w.text -weight 1

    $w.text tag configure h1 \
	-justify left \
	-font [dict merge [font actual TkTextFont] {-weight bold}]

    $w.text insert end "Commands\n" h1
    lappend txt "/msg <nick|JID> <text>"
    lappend txt [list "Send private message to a user identified by nickname\
	 or JID."]
    lappend txt "/whisper <nick|JID> <text>"
    lappend txt [list "Synonym for /msg ."]
    lappend txt "/chat <nick|JID> ?text?"
    lappend txt [list "Open a separate window to privately chat with the user\
	identified by nickname or JID, optionally posting an initial message."]
    lappend txt "/userinfo <nick>"
    lappend txt [list "Display registered information for user <nick>."]
    lappend txt "/afk ?reason?"
    lappend txt [list "Set your status to away with an optional reason."]
    lappend txt "/back ?reason?"
    lappend txt [list "Indicate that you have returned."]
    lappend txt "/away ?reason?"
    lappend txt [list "Synonym for /afk ."]
    lappend txt "/google <text>"
    lappend txt [list "Open a google query for <text> in web browser."]
    lappend txt "/googlefight <word> <word>"
    lappend txt [list "Perform a google fight between two words or phrases\
	(in quotes)."]
    lappend txt "/tip:<NUM>"
    lappend txt [list "Open the specified TIP document in web browser."]
    lappend txt "/wiki <text>"
    lappend txt [list "Do a Tclers wiki query with the remainder of the line."]
    lappend txt "/wikipedia <text>"
    lappend txt [list "Send a query to wikipedia (abbr. /wikip <text>)."]
    lappend txt "/wiktionary <text>"
    lappend txt [list "Send a query to wikipedia dictionary (abbr. /wikid\
	 <text>)."]
    lappend txt "/bug ?group? ?tracker? id"
    lappend txt [list "Open a sourceforge tracker item in browser."]
    lappend txt "/noisy ?<nick>? ?<minutes>?"
    lappend txt [list "Toggle <nick> noisy for x minutes (default 5).\
	Messages from noisy users are not diplayed. Not specifying a nick\
	will give you a list of noisy users."]
    lappend txt "/see <mark>"
    lappend txt [list "Goto named mark or index (eg: bookmark1 end 0.0)."]
    lappend txt "/alias <name> <type> <body>"
    lappend txt [list "<type> is 'proc' or 'script', type proc takes exactly\
	one argument." \
	"e.g: /alias foo script addSystem .txt \"test!\"" \
	"/alias foo proc thisProc" \
	"proc thisProc { arguments } { addSystem .txt \$arguments }"]
    lappend txt "/unalias <pattern>"
    lappend txt [list "Removes one or more aliases." "e.g: /unalias f*"]
    insertHelpText $w.text $txt

    $w.text insert end "\n" {} "Administrative commands\n" h1
    set txt ""
    lappend txt "/kick nick ?reason?"
    lappend txt [list "Remove an undesirable user."]
    lappend txt "/mute nick ?reason?"
    lappend txt [list "Globally silence a user."]
    lappend txt "/unmute nick ?reason?"
    lappend txt [list "Unmute a muted user."]
    lappend txt "/op nick ?reason?"
    lappend txt [list "Make user an administrator."]
    lappend txt "/deop nick ?reason?"
    lappend txt [list "Remove admin privileges from user."]
    insertHelpText $w.text $txt

    set txt ""
    $w.text insert end "\n" {} "Searching\n" h1
    lappend txt "/?<text>"
    lappend txt [list "Search the chat buffer for matching text.\
	Repeating the command will progress to the previous match."]
    lappend txt "/!"
    lappend txt [list "Clear the previous search result."]
    insertHelpText $w.text $txt
    $w.text configure -state disabled

    bind $w <Key-Escape> [list $w.b invoke]
    bind $w <Key-Return> [list $w.b invoke]

    wm title $w $title
    if {$icon ne ""} {
	catch {wm iconphoto $w $icon}
    }
    tk::PlaceWindow $w widget .
    wm deiconify $w
}
