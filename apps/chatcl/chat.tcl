#!/usr/bin/tclsh
# This Script and the readme may only be given away "as is", unmodified
# and complete. The Script is Freeware.
# Created and Copyright by Ralf Gueldemeister <ralfg@bigfoot.com>
# on 25.12.98

encoding system utf-8
package require Tclx
source config.tcl

# HTML Part
#
########################

# WILLKOMMEN.HTML - Printed before user logs in
proc willkommen_html {} {
    global html_title html_css html_bodytag script2_name

    P {
	<HTML>
	<HEAD><TITLE>$html_title - Login</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>$html_title</I></H1>
	<H3>Login</H3>
	<FORM METHOD=GET ACTION="$script2_name">
	<INPUT TYPE=hidden NAME="action" VALUE="login">
	<TABLE><TR><TD>
	<B>Nickname:</B> </TD><TD><INPUT NAME="name"><BR></TD></TR>
	<TR><TD><B>Password:</B> </TD><TD><INPUT TYPE=PASSWORD NAME="password"><BR>
	</TD></TR></TABLE>
	<INPUT TYPE=SUBMIT VALUE="enter">
	</FORM>
	<P>
	<B><A HREF="$script2_name?action=register">register new nick</A></B>
	</P>
	</CENTER>
	</BODY>
	</HTML>
    }
}

#CHATINPUT.HTML
proc chatinput_html {} {
    global html_css html_bodytag script_name input_field_size script2_name
    global query logout_url
    P {
	<HTML>
	<HEAD>
	$html_css
	</HEAD>
	$html_bodytag
	<FORM METHOD=GET ACTION="$script_name" accept-charset="UTF-8">
	<TABLE FRAMESPACING=0 FRAMEPADDING=0>
	<TR><TD>
	<INPUT TYPE=HIDDEN NAME="action" VALUE="postmsg">
	<INPUT TYPE=HIDDEN NAME="name" VALUE="$query(name)">
	<INPUT TYPE=HIDDEN NAME="password" VALUE="$query(password)">
	<INPUT TYPE=HIDDEN NAME="color" VALUE="$query(color)">
	<INPUT TYPE=HIDDEN NAME="new_msg_on_top" VALUE="$query(new_msg_on_top)">
	<INPUT TYPE=HIDDEN NAME="updatefrequency" VALUE="$query(updatefrequency)">
	<INPUT TYPE=HIDDEN NAME="ls" VALUE="$query(ls)">
	<INPUT SIZE="$input_field_size" NAME="msg" MAXLENGTH=400>
	<INPUT TYPE=SUBMIT VALUE="post">
    }
    select_user
    P {
	</TD><TD ALIGN="left">
	&nbsp;<A HREF="$script2_name?action=options_html&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)" TARGET="_parent">options</A>&nbsp;<A HREF="$script2_name?action=changeuserinfo&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)" TARGET="_parent">profile</A>
	<BR>
	&nbsp;<A HREF="$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)&pause=1#end" TARGET="main">pause</A>&nbsp;<A HREF="$script2_name?action=gotourl&url=$logout_url" TARGET="_parent"><B>logout</B></A>
	</TD><TD ALIGN="right">
	&nbsp;<SMALL><A HREF="http://www.ralfchat.de" TARGET="_blank">&copy; ralf 2000</A></SMALL>
	</TD></TR>
	</TABLE>
	</FORM>
	<SCRIPT LANGUAGE="javascript">
	<!--
    }
    if {!$query(norefresh)} {
	P {parent.main.location.reload();\n}
    }
    P {
	document.forms[0].msg.focus();
	//-->
	</SCRIPT>
	</BODY>
	</HTML>
    }
}


#STILLALIVE.HTML - the users online frame
proc stillalive_html_header {} {
    global script_name query html_css html_bodytag
    P {
	<HTML>
	<HEAD>
	<META HTTP-EQUIV="refresh" CONTENT="20; URL=$script_name?action=stillalive&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)">
	<META HTTP-EQUIV="expires" CONTENT="0">
	<META HTTP-EQUIV="pragma" CONTENT="no-cache">
	$html_css
	</HEAD>
	$html_bodytag
    }
}

proc stillalive_html_footer {} {
    P {
	</BODY></HTML>
    }
}

#CHAT.HTML
proc chat_html_header {} {
    global env chat_msg query html_css html_bodytag script_name

    P {<HTML>\n<HEAD>\n}
    if {!$query(pause)} {
	if {[string match *MSIE* $env(HTTP_USER_AGENT)]} {
	    P {
		<SCRIPT LANGUAGE=\"JavaScript\">
		<!--
		window.setTimeout("window.location.reload()",$query(updatefrequency)000);
		//-->
		</SCRIPT>
		<NOSCRIPT>
		<META HTTP-EQUIV=\"refresh\" CONTENT=\"$query(updatefrequency); URL=$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)\#end\">
		</NOSCRIPT>
	    }
	} else {
	    P {<META HTTP-EQUIV=\"refresh\" CONTENT=\"$query(updatefrequency); URL=$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)\#end\">
	    }
	}
    }
    P {
	<META HTTP-EQUIV=\"expires\" CONTENT=\"0\">
	<META HTTP-EQUIV=\"pragma\" CONTENT=\"no-cache\">
	$html_css
	</HEAD>
	$html_bodytag
    }
    if {$query(new_msg_on_top) == 1} {
	P {
	    <A NAME=\"end\"></A>
	}
	if {$query(pause)} {
	    P {<A HREF=\"$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)\#end\"><B>$chat_msg(26)</B></A><P>
	    }
	}
    }
}

proc chat_html_footer {} {

    global query chat_msg script_name

    if {$query(new_msg_on_top) == 0} {
	P {<A NAME=\"end\"></A>\n}
	if {$query(pause)} {
	    P {<P><A HREF=\"$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)\#end\"><B>$chat_msg(26)</B></A>
	    }
	}
    }
    P {
	</BODY>
	</HTML>
    }
}


# MESSAGES Part
#
########################

set chat_msg(0)  "wrong masterpassword"; # wrong masterpassword
set chat_msg(1)  "unknown command"; # unknown command
set chat_msg(2)  "no number of days specified"; # no number of days specified
set chat_msg(3)  "
<I><B>commands:</B></I><BR>
/msg <KBD>nickname</KBD> <KBD>message</KBD> - send private message to nickname<BR>
/help - this helptext<BR>
/about - about this program<BR>
/me - action (eg \"/me says hello\" will appear as \"* user says hello\")<BR>
/userinfo <KBD>nick</KBD> - userinfo about (offline) user<BR>
/memo <KBD>nick message</KBD> - leave message for offline user<BR><BR>
<I><B>master commands:</B></I><BR>    
/list_nicks <KBD>masterpassword</KBD> - list all users (online or not)<BR>
/clean_old_nicks <KBD>masterpassword x</KBD> - remove all users who haven\'t visited the chat for <KBD>x</KBD> days<BR>
/remove_nick <KBD>masterpassword nick</KBD> - remove nick<BR>
/ban <KBD>masterpassword nick</KBD> - ban user from chatroom<BR>
/ban <KBD>masterpassword</KBD> ip:<KBD>x.x.x.x</KBD> - ban ip from chatroom (works only when user with banned ip enters, you have to kick/ban the user manually, too)<BR>
/kick <KBD>masterpassword nick</KBD> - kick user out of the chat<BR>
/unban <KBD>masterpassword nick</KBD> - unban user<BR>
/unban <KBD>masterpassword</KBD> ip:<KBD>x.x.x.x</KBD> - unban ip<BR>
/banned_list <KBD>masterpassword</KBD> - show banned users and ips<BR>
/admin_memo <KBD>masterpassword message</KBD> - leave message for all users<BR>
/ip <KBD>masterpassword nick</KBD> - show ip of user<BR>
"
set chat_msg(4)  "has left the chat"; # [user] has left the chat
set chat_msg(5)  "users"; # users
set chat_msg(6)  "user"; # user
set chat_msg(7)  "online"; # online
set chat_msg(8)  "Nickname";# Nickname
set chat_msg(9)  "Created on"; # Created on
set chat_msg(10) "last visited"; # last visited
set chat_msg(11) "current/last ip"; # current/last ip
set chat_msg(12) "status"; # status
set chat_msg(13) "#memos"; # #memos
set chat_msg(14) "normal"; # normal
set chat_msg(15) "kicked"; # kicked
set chat_msg(16) "banned"; # banned
set chat_msg(17) "removed users"; # removed users
set chat_msg(18) "total"; # total
set chat_msg(19) "removed"; # removed
set chat_msg(20) "doesnot exist"; # doesnot exist
set chat_msg(21) "to all users"; # to all users
set chat_msg(22) "unbanned"; # unbanned
set chat_msg(23) "Memo for"; # Memo for
set chat_msg(24) "was posted successfully"; # was posted successfully
set chat_msg(25) "all users"; # all users
set chat_msg(26) "continue"; # continue

# Main Part - DON'T CHANGE ANYTHING HERE!
#
#######################

proc action {} {
    global qs query
    if {$qs eq ""} {	# Wenn query_string leer sprung zum
	welcome;	# welcome-teil, 1st visit seite
    } else {
	switch -- $query(action) {
	    chatinput_html {
		header
		chatinput_html
	    }
	    stillalive {
		stillalive
	    }
	    chat {
		chat
	    }
	    postmsg {
		postmsg
	    }
	    chatnoframes_html {
		header
		chatnoframes_html
	    }
	    default {
		error
	    }
	}
    }
}


proc welcome {} { # Aufruf beim ersten Programmstart
    header
    willkommen_html
}

proc stillalive {} { # stillalive/who prozedur

    global query chat_msg data_dir data_stillalive_file data_msg_file
    global script2_name

    checkpass
    array set sa [splitsemi [readfile "$data_dir/$data_stillalive_file"]]


    # &repair_safile if ($#sa % 2 == 0); # wenn safile fehlerhaft -> repair

    set time [clock seconds]
    set sa($query(name)) $time

    # Eintraege nach ueberfaelligen (aelter als 150s) durchsuchen und diese entfernen    
    set left ""
    foreach {k v} [array get sa] {
	# ist zeitstempel schon aelter als 150s?
	if {$v < ($time - 150)} {
	    #print logout msg
	    append left "<B>$k $chat_msg(4)</B>\n"
	    unset sa($k)
	}
    }
    appendfile "$data_dir/$data_msg_file" $left
    writefile "$data_dir/$data_stillalive_file" [joinsemi [array get sa]]

    set nrusers [array size sa]
    set users [expr {$nrusers > 1 ? $chat_msg(5) : $chat_msg(6)}]

    if {$query(noframes) == 1} {
	P {<I><B>$nrusers $users $chat_msg(7):</I></B>\n}
	foreach {k v} [array get sa] {
	    P {
		<A HREF=\"$script2_name?action=userinfo\&infoabout=$k\" TARGET=\"RC_INFO\">$k</A>
	    }
	}
    } else {
	header
	stillalive_html_header
	
	P {<B><BIG><I>$nrusers $users $chat_msg(7)</I></BIG></B><P>\n}
	foreach {k v} [array get sa] {
	    P {
		<B><A HREF=\"$script2_name?action=userinfo\&infoabout=$k\" TARGET=\"RC_INFO\">$k</A></B><BR>
	    }
	}
	stillalive_html_footer
    }
}

proc chat {} { # Chatprocedure: show Messages

    global query data_dir data_msg_file data_private_file

    checkpass

    if {$query(noframes) != 1} {
	header
	chat_html_header
    }

    set chatfile [readfile $data_dir/$data_msg_file]

    # Ausgabe Messages    
    set chatmsgs [list]
    set PRIV_RE {^<!--PrivateMsgHere;;([^;]+);;([^;]+);;-->(.*)}
    set time [clock seconds]
    foreach line [split $chatfile "\n"] {
	while {[regexp $PRIV_RE $line -> this_name this_time line]} {
	    # private msg?
	    if {$this_name eq $query(name)} { # private msg fuer user?
		set privatefile \
		    [readfile $data_dir/$data_private_file.$query(name)]
		foreach pline [split $privatefile "\n"] {
		    if {![regexp {^([0-9]*);;(.*)} $pline -> time message]} continue
		    if {$time == $this_time} {
			if {$query(new_msg_on_top) == 1} {
			    set chatmsgs [linsert $chatmsgs 0 $message]
			} else {
			    P {$message\n<BR>\n}
			}
		    }
		}
	    }
	}
	
	if {$line != ""} {
	    if {$query(new_msg_on_top) == 1} {
		set chatmsgs [linsert $chatmsgs 0 $line]
	    } else {
		P {$line\n<BR>\n}
	    }
	}
    }
    
    if {$query(new_msg_on_top) == 1} {
	foreach line $chatmsgs {
	    P {$line\n<BR>\n}
	}
    }
    if {$query(noframes) != 1} {
	chat_html_footer
    }
}

proc postmsg {} {

    global query data_dir data_msg_file query message_limit

    set chatfile [split [readfile $data_dir/$data_msg_file] "\n"]

    # alte Eintraege >$message_limit loeschen
    set chatfile [lrange $chatfile end-$message_limit end]
    
    writefile $data_dir/$data_msg_file [join $chatfile "\n"]\n

    # check name+password
    checkpass
	
    #puts stderr before:$query(msg)
    set query(msg)  [wash_msg $query(msg)]
    #puts stderr after:$query(msg)

    if {[string match "/*" $query(msg)]} {
	regsub -all { +} $query(msg) { } msg
	eval [linsert [split $msg] 0 command]
    } elseif {$query(msg_to) ne ""} {
	command /msg $query(msg_to) $query(msg)
    } elseif {$query(msg) ne ""} {

	# preserve blanks
	#regsub -all { } $query(msg) {\&nbsp;} query(msg)
	
	# poste msg nach $data_dir/message
	# set userspecified font color if not standard
	if {$query(color) eq "standard"} {
	    appendfile $data_dir/$data_msg_file \
		"<B>$query(name)</B>: $query(msg)\n"
	} else {
	    appendfile $data_dir/$data_msg_file \
		[cat "<FONT COLOR=\"$query(color)\"><B>" \
		     "$query(name)</B>: $query(msg)</FONT>\n"]
	}
	LOG 3 "\[MSG\] $query(name): $query(msg)"
    }
    header
    if {$query(noframes) == 1} {
	chatnoframes_html
    } else {
	chatinput_html
    }
}
    
proc command {cmd args} {

    global query chat_msg masterpassword userinfo_as_private_msg

    #  puts stderr "cmd:$cmd:$args"

    switch -- $cmd {
	/msg {
	    set msg_to [lindex $args 0]
	    set message [string trim [join [lrange $args 1 end]]]
	    if {$message != ""} {
		postprivatemsg $query(name) ->$msg_to $message
		postprivatemsg $msg_to $query(name) $message
	    }
	}
	/help help
	/about about
	/me {
	    me_action $query(name) [join $args]
	}
	/list_nicks {
	    set usermpass [lindex $args 0]
	    if {$usermpass eq $masterpassword} {
		list_nicks
	    } else {
		postprivatemsg $query(name) "LIST_NICKS" $chat_msg(0)
	    }
	}
	/clean_old_nicks {
	    set usermpass [lindex $args 0]
	    set cleandate [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		if {$cleandate ne ""} {
		    clean_old_nicks $cleandate
		} else {
		    postprivatemsg $query(name) "CLEAN_OLD_NICKS" $chat_msg(2)
		}
	    } else {
		postprivatemsg $query(name) "CLEAN_OLD_NICKS" $chat_msg(0)
	    }
	}
	/remove_nick {
	    set usermpass [lindex $args 0]
	    set nick2rm   [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		remove_nick $nick2rm
	    } else {
		postprivatemsg $query(name) "REMOVE_NICK" $chat_msg(0)
	    }
	}
	/userinfo {
	    foreach query(infoabout) $args break
	    set userinfo_as_private_msg 1;
	    userinfo
	}
	/ban {
	    set usermpass [lindex $args 0]
	    set nick2ban  [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		kick_or_ban_nick $nick2ban 2
	    } else {
		postprivatemsg $query(name) "BAN" $chat_msg(0)
	    }
	}
	/kick {
	    set usermpass [lindex $args 0]
	    set nick2kick [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		kick_or_ban_nick $nick2kick 1
	    } else {
		postprivatemsg $query(name) "KICK" $chat_msg(0)
	    }
	}
	/banned_list {
	    set usermpass [lindex $args 0]
	    if {$usermpass eq $masterpassword} {
		banned_list
	    } else {
		postprivatemsg $query(name) "BANNED_LIST" $chat_msg(0)
	    }
	}
	/unban {
	    set usermpass  [lindex $args 0]
	    set nick2unban [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		unban $nick2unban
	    } else {
		postprivatemsg $query(name) "UNBAN" $chat_msg(0)
	    }
	}
	/memo {
	    set memo_to [lindex $args 0]
	    set memo [join [lrange $args 1 end]]
	    memo $memo_to $query(name) $memo
	}
	/ip {
	    set usermpass [lindex $args 0]
	    set nick      [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		ip $nick
	    } else {
		postprivatemsg $query(name) "IP" $chat_msg(0)
	    }
	}
	/admin_memo {
	    set usermpass [lindex $args 0]
	    set nick      [lindex $args 1]
	    if {$usermpass eq $masterpassword} {
		admin_memo $query(name) [join [lrange $args 1 end]]
	    } else {
		postprivatemsg $query(name) "ADMIN_MEMO" $chat_msg(0)
	    }
	}
	default {
	    postprivatemsg $query(name) \
		"HELP" "$chat_msg(1): :$cmd:$args"
	}
    }
}

proc help {} {
    global query chat_msg
    postprivatemsg $query(name) "HELP" $chat_msg(3)
}

proc me_action {who what} {  # parameter 0: wer 1: was

    global query data_dir data_msg_file

    # set userspecified font color if not standard
    if {$query(color) eq "standard"} {
	appendfile $data_dir/$data_msg_file "<B>* $who $what</B>\n"
    } else {
	appendfile $data_dir/$data_msg_file \
	    "<FONT COLOR=\"$query(color)\"><B>* $who $what</B></FONT>\n"
    }        
}

proc about {} {
    global query about_msg
    postprivatemsg $query(name) "ABOUT" $about_msg
}

proc list_nicks {} {

    global query chat_msg config_msg data_dir data_nicks_file

    set nickfile [split [readfile $data_dir/$data_nicks_file] "\n"]
    
    set nicklist [cat "<TABLE><TR><TD><B>$chat_msg(8)</B></TD>" \
		      "<TD><B>$config_msg(12)</B></TD>" \
		      "<TD><B>$chat_msg(9)</B></TD>" \
		      "<TD><B>$chat_msg(10)</B></TD>" \
		      "<TD><B>$chat_msg(11)</B></TD>" \
		      "<TD><B>$chat_msg(12)</B></TD>" \
		      "<TD><B>$chat_msg(13)</B></TD></TR>"]
		  
    foreach line $nickfile {
	set i 0
	array unset e
	foreach e [splitsemi $line] {
	    set nickfileentry($i) $e
	    incr i
	}
	switch -- $nickfileentry(10) {
	    "" -
	    0 { set status $chat_msg(14) }
	    1 { set status $chat_msg(15) }
	    2 { set status $chat_msg(16) }
	    default {
		set status unknown:$nickfileentry(10)
	    }
	}
	append nicklist "<TR><TD>" \
	    $nickfileentry(0) "</TD><TD>" \
	    $nickfileentry(2) "</TD><TD>" \
	    $nickfileentry(3) "</TD><TD>" \
	    [clock format $nickfileentry(4)] "</TD><TD>" \
	    $nickfileentry(11) "</TD><TD>" \
	    $status "</TD><TD>" \
	    $nickfileentry(12) "</TD></TR>"
    }
    append nicklist "</TABLE>"
    postprivatemsg $query(name) "LIST_NICKS" $nicklist
}

# clear all old nicks older than nr_of_days
proc clean_old_nicks {nr_of_days} {

    global query chat_msg nickfile data_dir data_nicks_file

    set inicks [split [readfile $data_dir/$data_nicks_file] "\n"]
    set onicks [list]
    set removed_users_names [list]
    set time [expr {[clock seconds] - $nr_of_days*60*60*24}]

    foreach nick $inicks {
	set nick [splitsemi $nick]
	if {[lindex $nick 4] >= $time || [lindex $nick 0] eq $query(name)} {
	    lappend onicks [joinsemi $nick]
	} else {
	    lappend removed_users_names [lindex $nick 0]
	}
    }

    if {[llength $removed_users_names] > 0} {

	writefile $data_dir/$data_nicks_file [join $onicks "\n"]
	
	foreach name $removed_users_names {
	    rm_uientry $name; # delete user from uifile
	    rm_memoentry $name; # delete memos for user from memofile
	}

	postprivatemsg $query(name) "CLEAN_OLD_NICKS" \
	    "$chat_msg(17): $removed_users_names<BR>"

	LOG 1 "\[CLEAN_OLD_NICKS\] $chat_msg(17): $removed_users_names"
    }
}

proc remove_nick {nick} { # $_[0] = nick to remove
    global query chat_msg data_dir data_nicks_file data_private_file

    set nicks [readfile $data_dir/$data_nicks_file]

    set found [regsub "\n$nick;;[^\n]*" $nicks "" nicks]
    if {$found} {
	
	writefile $data_dir/$data_nicks_file $nicks\n
						
	file delete "$data_dir/$data_private_file.$nick"
	rm_uientry $nick
	rm_memoentry $nick
	postprivatemsg $query(name) "REMOVE_NICK" \
	    "$chat_msg(6) $nick $chat_msg(19)"
	LOG 1 "\[REMOVE_NICK\] $nick $chat_msg(19)"
    } else {
	postprivatemsg $query(name) "REMOVE_NICK" \
	    "$chat_msg(6) $nick $chat_msg(20)"
    }
}

proc select_user {} {

    global query data_dir chat_msg data_stillalive_file reset_private_msg_select

    set alive [splitsemi [readfile $data_dir/$data_stillalive_file]]
    
    P {<SELECT MAXLENGTH=2 NAME="msg_to">}
    
    set selected [expr {$query(msg_to) eq "" ? "SELECTED" : ""}]
    if {$reset_private_msg_select == 1}	{
	set selected ""
    }
    P {<OPTION $selected VALUE="">$chat_msg(21)</A>}

    foreach {nick time} $alive {

	if {[string length $nick] > 9} {
	    set shortnick [string range $nick 0 9]..
	} else {
	    set shortnick $nick
	}
	set selected [expr {($query(msg_to) eq $nick &&
			     $reset_private_msg_select == 0) ?
			    "SELECTED" : ""}]
	P {<OPTION $selected VALUE=\"$nick\">$shortnick</A>}
    }
    P {</SELECT>}
}

# remove user from userinfo database
proc rm_uientry {nick} {

    global data_dir data_userinfo_file

    if {![info exists $data_dir/$data_userinfo_file]} return
    set userinfo [readfile $data_dir/$data_userinfo_file]
    set found [regsub "\n$nick;;[^\n]*" $userinfo "" userinfo]
    if {$found} {
	writefile $data_dir/$data_userinfo_file $userinfo
    }
}

# remove memos for user from memo database
proc rm_memoentry {nick} {
    
    global data_dir data_memo_file
    if {![info exists $data_dir/$data_memo_file]} return

    set memos [readfile $data_dir/$data_memo_file]
    set found [regsub -all "\n$nick;;[^\n]*" $memos "" memos]
    if {$found} {
	writefile $data_dir/$data_memo_file $memos\n
    }

}

proc kick_or_ban_nick {nick level} {
    # level (1=kick, 2=ban)

    global chat_msg query data_dir data_banned_file data_nicks_file

    if {[string match "ip:*" $nick]} {
	appendfile $data_dir/$data_banned_file "$nick;;\n"
	set ip [string range $nick 3 end]
	postprivatemsg $query(name) BAN "IP $ip $chat_msg(16)"
	return;
    }

    set records [split [readfile $data_dir/$data_nicks_file] "\n"]
    
    set index [lsearch $records "$nick;;*"]

    if {$index == -1} return

    set record [splitsemi [lindex $records $index]]
    lset record 10 $level
    lset records $index [joinsemi $record]

    writefile $data_dir/$data_nicks_file [join $records "\n"]
    
    switch -- $level {
	1 {
	    postprivatemsg $query(name) "KICK" "User $nick $chat_msg(15)"
	}
	2 {
	    postprivatemsg $query(name) "BAN" "User $nick $chat_msg(16)"
	    appendfile $data_dir/$data_banned_file "$nick;;"
	}
    }
}    

proc banned_list {} {

    global query chat_msg data_dir data_banned_file
    
    set banned [string map {;; <BR>} [readfile $data_dir/$data_banned_file]]
    postprivatemsg $query(name) "BANNED_LIST" \
	"<B>$chat_msg(16):</B><BR>$banned"
}

proc unban {nick} {
    global query chat_msg data_dir data_banned_file data_nicks_file

    set banns [splitsemi [readfile $data_dir/$data_banned_file]]

    set indexes [lsort -integer -decreasing \
		     [lsearch -all -exact $banns $nick]]

    if {[llength $indexes] > 0} {
	foreach index $indexes {
	    set banns [lreplace $banns $index $index]
	}
	
	writefile $data_dir/$data_banned_file [joinsemi $banns]

	postprivatemsg $query(name) "UNBAN" \
	    "$chat_msg(6) $nick $chat_msg(22)"
    }

    set records [readfile $data_dir/$data_nicks_file]
    set index [lsearch $records "$nick;;*"]
    if {$index > -1} {
	set nick [splitsemi [lindex $records $index]]
	lset nick 10 0
	lset records $index [joinsemi $nick]
	writefile $data_dir/$data_nicks_file [join $records "\n"]
    }
}

proc memo {to from message} {
    
    global query chat_msg data_dir data_nicks_file data_memo_file

    set nicks [split [readfile $data_dir/$data_nicks_file] "\n"]
    set index [lsearch $nicks "$to;;*"]
    if {$index > -1} {
	set nick [splitsemi [lindex $nicks $index]]
	lset nick 12 [expr {[lindex $nick 12] + 1}]
	lset nicks $index [joinsemi $nick]
	writefile $data_dir/$data_nicks_file [join $nicks "\n"]
	appendfile $data_dir/$data_memo_file \
	    "$to;;$from;;$message;;[clock seconds]\n"
	postprivatemsg $query(name) "MEMO" \
	    "$chat_msg(23) $to $chat_msg(24)"
    } else {
	postprivatemsg $query(name) "MEMO" "$chat_msg(6) $to $chat_msg(20)"
    }
}

proc wash_msg {msg} {

    global script2_name

    set msg [string map {& &amp; < &lt; > &gt; \n <BR>} $msg]

    regsub -all {(\b(https?|ftp)://.[-A-Za-z0-9/_:.%\\\#+@?=&;~]+)} $msg \
	"<A HREF=\"$script2_name?action=gotourl\&url=[unkill_specials {\1}]\" TARGET=\"linkwindow\">\1<\/A>" msg
    regsub -all {(\bnews:[-A-Za-z0-9.]+\b)} $msg {<A HREF="\1">\1<\/A>} msg
    regsub -all {(\bmailto:.*\b)} $msg "<A HREF=\"[unkill_specials {$1}]\">\1<\/A>" msg
    return $msg
}

proc unkill_specials {s} {
    string map {&amp; &} $s
}

proc admin_memo {from message} {

    global query chat_msg data_dir data_nicks_file data_memo_file

    set inicks [split [readfile $data_dir/$data_nicks_file] "\n"]
    set onicks [list]
    set memos ""

    set time [clock seconds]
    foreach nick $inicks {
	set nick [splitsemi $nick]
	lset nick 12 [expr {[lindex $nick 12] + 1}]
	lappend onicks [joinsemi $nick]
	append memos "$nick;;$from;;$message;;$time\n";
    }
    writefile $data_dir/$data_nicks_file [join $onicks "\n"]
    appendfile $data_dir/$data_memo_file $memos
	
    postprivatemsg $query(name) "ADMIN_MEMO" \
	"$chat_msg(23) $chat_msg(25) $chat_msg(24)"
}

proc ip {nick} {

    global query data_dir data_nicks_file config_msg

    set records [split [readfile $data_dir/$data_nicks_file] "\n"]

    set index [lsearch -glob $records "$nick;;*"]

    if {$index > -1} {
	set record [splitsemi [lindex $records $index]]
	set ip [lindex $record 11]
	postprivatemsg $query(name) "IP" "$nick: $ip"
    } else {
	postprivatemsg $query(name) "IP" "$config_msg(1)"
    }
}

action;	# Fuehrt je nach action variable gegebene sub aus
        # action im FORM definieren!

exit;

# END Main Part
#
##############################
