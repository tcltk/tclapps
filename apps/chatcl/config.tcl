# Config Part
#
#######################

set data_dir "data"; # data directory like "data"
set welcome_msg "    Welcome to the Tcl'ers Chat!<BR>
    /help - helptext";  # msg printed when user logs in
set masterpassword "tralala"; # Masterpassword for special commands
set updatefrequency 20; # number of seconds the chat updates itself
set min_update_freq 20; # the minimum update frequency
set clear_old_msgs 0; # set to 1 if you want that old msgs are deleted when the chat is empty
set message_limit 20; # number of maximal (public) messages in the chat window
set new_msg_on_top 0; # set to 1 if you want new messages printed on top, to work around the msie bug
set admin_email_addresse max@m4x.d; # Your email address
set mailprogramme /usr/sbin/sendmail; # Path to the mail-transport-agent
set mail_on_new_registration 3; # set to 1 if you want to be mailed when a new user registers
set logtype 3; # The type of log you wish: 0 - no log, 1 - registrations, 2 - +logins, 3 - +messages
set log_dir "logs"; # directory for log files
set script_name "chat.tcl"; # filename of the script like "chat.cgi"
set script2_name "chat2.tcl"; # filename of the second cript like "chat2.cgi"
set max_users -1; # the maximal number of users in the chat at the same time, set to "-1" if unlimited
set days_to_keep_memos 60; # Number of days to keep memos
set reset_private_msg_select 0; # set to 1 if you want the select field for private msgs to be reset to "to all users" after msg was sent
set flocking 1; # set to 1 if you want flocking, but flocking is not supported by Windows NT (set to 0)
set international_nicks 1; # 0 - nick may only contain letters and numbers, 1 - nick may contain any character except ";/ :\?*<>|

# Files - don't need to be changed, only if you want several script
# using eg the same nick file but different msg file. So you can use
# the script for multiple chat rooms.

set data_msg_file "messages";
set data_stillalive_file "stillalive";
set data_private_file "private"; # without .user
set data_nicks_file "nicks"; # you should leave this one, else the user has to register on every single script
set data_userinfo_file "userinfo"; # the user-info database
set data_banned_file "banned"; # banned user file
set data_memo_file "memo"; # memo file

# HTML-Stuff

set html_bodytag "<BODY BGColor=#ffffff>"; # The bodytag for html files
set html_css {
<STYLE TYPE="text/css">
</STYLE>
}; # The style sheet definition used for all html files
set html_title "The Tcl'ers Chat"; # The title of your chat
set banner_picture ""; # the banner picture, set to "" if you don´t want any banner
set banner_link "http://localhost/"; # The banner link
set logout_url "$script_name"; # the url for the logout-link
set input_field_size "30"; # the size of the msg input field

# END CONFIG PART - ALL DONE
#
########################

# HTML Part
#
########################


#USER_ERROR.HTML - view error msg
proc user_error_html {title message} {

    global html_title html_css html_bodytag

    P {
	<HTML>
	<HEAD><TITLE>$html_title - $title</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>$title</I></H1>
	<B>$message</B>
	</CENTER>
	</BODY>
	</HTML>
    }
}

# MESSAGES Part
#
########################

set config_msg(0) "unknown nickname"; # unknown nickname

# nonick_html
set config_msg(1) "Nick doesn't exist"; # Nick doesn't exist
set config_msg(2) "<A HREF=\"$script2_name?action=register\">register</A> or<BR><A HREF=\"$script_name\">try another nick</A>"; # <A HREF=\"$script2_name?action=register\">register</A> or<BR><A HREF=\"$script_name\">try another nick</A>

# wrongpass_html
set config_msg(3) "Wrong Password"; # Wrong Password
set config_msg(4) "<A HREF=\"$script2_name?action=send_pwd&name="; # <A HREF=\"$script2_name?action=send_pwd&name=[nickname]
set config_msg(19) "\">lost password</A><BR><A HREF=\"$script_name\">retry</A>"; # [config_msg[4]]\">lost password</A><BR><A HREF=\"$script_name\">retry</A>

# kicked_html
set config_msg(5) "You were kicked"; # You were kicked
set config_msg(6) "<A HREF=\"$script2_name?action=gotourl&url=$logout_url\" TARGET=\"_parent\">logout</A>"; # <A HREF=\"$script2_name?action=gotourl&url=$logout_url\" TARGET=\"_parent\">logout</A>

# banned_html
set config_msg(7) "You are banned"; # You are banned
set config_msg(8) "<A HREF=\"$script2_name?action=gotourl&url=$logout_url\" TARGET=\"_parent\">logout</A>"; # <A HREF=\"$script2_name?action=gotourl&url=$logout_url\" TARGET=\"_parent\">logout</A>

# user [user] not found
set config_msg(9) "user"; # user
set config_msg(10) "not found"; # not found

# <B>[nickname]</B><BR>realname: [realname]<BR>email: <A HREF=\"mailto:[email]\">[email]</A><BR>age: [age]<BR>city: [city]<BR>country: [country]<BR>homepage: <A HREF=\"[url]\">[url]</A><BR>anything else: [stuff]<BR>photo: <A HREF=\"[photo_url]\">$userinfo(photo_url)</A>
set config_msg(11) "realname"; # realname
set config_msg(12) "eMail"; # eMail
set config_msg(13) "age"; # age
set config_msg(14) "city"; # city
set config_msg(15) "country"; # country
set config_msg(16) "homepage"; # homepage
set config_msg(17) "anything else"; # anything else
set config_msg(18) "photo"; # photo

# Main Part - DON'T CHANGE ANYTHING HERE!
#
#######################

set version "0.12";     # don't change
set lastmodified "Mar 10th 2000"; # don't change
set about_msg "Ralfs Chat - <A HREF=\"$script2_name?action=gotourl\&url=http://www.ralfchat.de\" TARGET=\"ralfchat\">www.ralfchat.de</A> -  created and copyright by Ralf G&uuml;ldemeister \&lt;ralf\@ralfchat.de> on Dec 25th 1998; last modified on $lastmodified; Version $version"; # msg printed on /about

set timestamp [clock seconds];
set log_file [clock format $timestamp -format "%Y-%m-%e.txt"]


proc extract_qs {} { # extract variables from $qs --> hash $query

    global qs query

    set map [list + { } "\\" "\\\\" "%" "\\u00"]

    array set query {
	noframes 0
	norefresh 0
	pause 0
	msg_to ""
	name ""
	new_msg_on_top 0
    }
    
    foreach arg [split $qs &] {
	foreach {key value} [split $arg =] break
	set query($key) [encoding convertfrom utf-8 \
			     [subst -nocommands -novariables \
				  [string map $map $value]]]
    }

    if {$query(pause) == ""} {set query(pause) 0}
}

proc getquery {} { # get query string
    global env qs
    switch -- $env(REQUEST_METHOD) {
	GET {
	    set qs $env(QUERY_STRING)
	}
	POST {
	    set qs [read stdin $env(CONTENT_LENGTH)]
	}
	default {
	    set qs ""
	}
    }
    extract_qs
}

proc header {} { # standard html header
    P {Content-Type: text/html; charset=UTF-8\n\n}
}

proc ERROR {message} {

    global html_title html_css html_bodytag qs

    header
    P {
	<HTML>
	<HEAD><TITLE>$html_title - An Error Occured</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<H1>An Error Occured</H1>
	<B>Error Message: $message<BR>Query_String: $qs</B>
	</BODY>
	</HTML>
    }
    exit
}


proc repair_safile {} {
    my $i=0;
    my @satemp;
    my @satemp2;
    for (@sa) {        
	       if (/[A-Za-z]/) {
		   $satemp[$i]=$_;
		   $i++;
	       }
	   }    
    $i=0;
    @sa = "";
    for {@satemp} {} {} {
	$sa[$i*2] = $satemp[$i];
	$sa[$i*2+1] = time;
	$i++; 
    }
    
    @sa = "" if ($\#sa % 2 == 0);
}
    
proc checkpass {} {

    global query config_msg data_dir data_nicks_file

    set nicks [readfile $data_dir/$data_nicks_file]

    set index [lsearch $nicks "$query(name);;*"]

    if {$index > -1} {
	set nick [splitsemi [lindex $nicks $index]]
	set nickok 1
	if {[lindex $nick 1] ne $query(password)} {
	    header
	    user_error_html $config_msg(3) \
		"$config_msg(4) $query(name) $config_msg[19]"
	    exit
	}
	switch -- [lindex $nick 10] {
	    1 {
		header
		user_error_html $config_msg(5) $config_msg(6)
		exit
	    }
	    2 {
		header
		user_error_html $config_msg(7) $config_msg(8)
		exit
	    }
	}
    } else {
	header
	user_error_html $config_msg(1)  $config_msg(2)
	exit
    }
}

proc postprivatemsg {to from msg} {

    global query config_msg data_dir data_private_file data_msg_file
    checkpass;  # Passwort checken

    # kill linebreaks
    set msg [string map {\n " "} $msg]

    if {![file exists $data_dir/$data_private_file.$to]} {
	postprivatemsg $query(name) "MSG" "$config_msg(0): $to"
	return;
    }

    # alte Eintraege >35 loeschen
    set priv [split [readfile $data_dir/$data_private_file.$to] "\n"]

    set priv [lrange $priv end-35 end]

    set last_priv_msg_id [lindex [splitsemi [lindex $priv end]] 0]

    while {[clock seconds] == $last_priv_msg_id} {after 100}
    set priv_msg_id [clock seconds];

    if {$query(color) eq "standard"} {
	lappend priv "$priv_msg_id;;<B>\[$from\]</B> $msg";
    } else {
	lappend priv \
	    [cat "$priv_msg_id;;<FONT COLOR=\"$query(color)\">" \
		 "<B>\[$from]</B> $msg</FONT>\n"]
    }

    writefile $data_dir/$data_private_file.$to [join $priv "\n"]

    appendfile $data_dir/$data_msg_file \
	"<!--PrivateMsgHere;;$to;;$priv_msg_id;;-->\n"
}

proc userinfo {} {

    global query config_msg data_dir data_userinfo_file
    global userinfo_as_private_msg userinfo
    
    set ui [split [readfile $data_dir/$data_userinfo_file] "\n"]

    set index [lsearch $ui "$query(infoabout);;*"]
    if {$index > -1} {
	set values [splitsemi [lindex $ui $index]]
    } else {
	set values [list]
    }

    set keys {_ realname email age city country url stuff photo_url icq_uin}

    foreach key $keys value $values {
	set userinfo($key) $value
    }
    if {[info exists userinfo_as_private_msg] && $userinfo_as_private_msg == 1} {
	if {$index > -1} {
	    postprivatemsg $query(name) "USERINFO" \
		[cat "<B>$query(infoabout)</B><BR>" \
		     "$config_msg(11): $userinfo(realname)<BR>" \
		     "$config_msg(12): <A HREF=\"mailto:$userinfo(email)\">" \
		     "$userinfo(email)</A><BR>" \
		     "$config_msg(13): $userinfo(age)<BR>" \
		     "$config_msg(14): $userinfo(city)<BR>" \
		     "$config_msg(15): $userinfo(country)<BR>" \
		     "$config_msg(16): <A HREF=\"$userinfo(url)\">" \
		     "$userinfo(url)</A><BR>" \
		     "$config_msg(17): $userinfo(stuff)<BR>" \
		     "$config_msg(18): <A HREF=\"$userinfo(photo_url)\">" \
		     "$userinfo(photo_url)</A>"]
	} else {
	    postprivatemsg $query(name) "USERINFO" \
		"$config_msg(9) $query(infoabout) $config_msg(10)"
	}
    } else {
	header
	userinfo_html
    }
}

proc cat {args} {join $args ""}

proc P {args} {
    set text [uplevel 1 [list subst -nocommands [join $args ""]]]
    puts -nonewline [regsub -all -line {\n^[ \t]+} $text {}]
}

proc LOG {level message} {
    global env logtype log_dir log_file
    if {$logtype >= $level} {
	set LOGFILE [open $log_dir/$log_file a]
	flock -write $LOGFILE
	puts $LOGFILE [cat [clock format [clock seconds]] " " \
			   $env(REMOTE_ADDR) " " $message]
	close $LOGFILE;
    }
}

proc readfile {filename} {
    if {![file exists $filename]} {
	writefile $filename ""
    }	
    set fd [open $filename]
    flock -read $fd
    set content [read -nonewline $fd]
    close $fd
    return $content
}

proc writefile {filename content} {
    set fd [open $filename w]
    flock -write $fd
    puts -nonewline $fd $content
    close $fd
}

proc appendfile {filename content} {
    set fd [open $filename a]
    flock -write $fd
    puts -nonewline $fd $content
    close $fd
}

proc srevert s {
    set l [string length $s]
    set res ""
    while {$l} {append res [string index $s [incr l -1]]}
    set res
}

proc splitsemi {string} {
    split [srevert [string map {;; "\0"} [srevert $string]]] "\0"
}

proc joinsemi {string} {
    join $string ";;"
}

getquery

# 1;
