#!/usr/bin/tclsh
# This Script and the readme may only be given away "as is", unmodified
# and complete. The Script is Freeware.
# Created and Copyright by Ralf Gueldemeister <ralfg@bigfoot.com>
# on 25.12.98

encoding system utf-8
package require Tclx
source common.tcl
getquery
source config.tcl

# HTML Part
#
########################


#REGISTER.HTML - Printed when user registers new nick
proc register_html {} {
    global html_css script2_name html_title html_bodytag
    P {
	<HTML>
	<HEAD><TITLE>$html_title - register new nick</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>Register New Nick</I></H1>
	<FORM METHOD=POST ACTION="$script2_name">
	<INPUT TYPE=HIDDEN NAME="action" VALUE="create_nick">
	<TABLE><TR><TD>
	<B>new Nickname: </B></TD><TD><INPUT NAME="name"><BR></TD></TR>
	<TR><TD><B>Password:</B> </TD><TD><INPUT TYPE=PASSWORD NAME="password"><BR></TD></TR>
	<TR><TD><B>Password again:</B> </TD><TD><INPUT TYPE=PASSWORD NAME="password2"><BR></TD></TR>
	<TR><TD><B>eMail:<BR> (only for administration)</B> </TD><TD><INPUT NAME="email"><BR>
	</TD></TR></TABLE>
	<INPUT TYPE=SUBMIT VALUE="register">
	</FORM>
	</CENTER>
	</BODY>
	</HTML>
    }
}

#BANNER.HTML - the banner on the top - You may change or remove it!
proc banner_html {} {
    global html_css html_bodytag banner_link banner_picture
    P {
	<HTML>
	<HEAD>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<A HREF="$banner_link" TARGET="_blank"><IMG SRC="$banner_picture" BORDER=0></A>
	</CENTER>
	</BODY></HTML>
    }
}

#OPTIONS.HTML - the options page
proc options_html {} {
    global query script2_name html_title html_css html_bodytag
    P {
	<HTML><HEAD><TITLE>$html_title - Options</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>Options</I></H1>
	<FORM METHOD=POST ACTION="$script2_name">
	<INPUT TYPE=HIDDEN NAME="action" VALUE="setoptions">
	<INPUT TYPE=HIDDEN NAME="name" VALUE="$query(name)">
	<INPUT TYPE=HIDDEN NAME="password" VALUE="$query(password)">
	<TABLE>
	<TR><TD>Update Frequency (secs): </TD><TD><INPUT SIZE="10" NAME="updatefrequency" VALUE=$query(updatefrequency)></TD></TR>
	<TR><TD>Font Color:</TD><TD><SELECT NAME="color">
	<OPTION SELECTED VALUE="$query(color)">current</OPTION>
	<OPTION VALUE="standard">standard</OPTION>
	<OPTION VALUE="standard">--------</OPTION>
	<OPTION VALUE="0000FF">blue</OPTION>
	<OPTION VALUE="008000">green</OPTION>
	<OPTION VALUE="FF0000">red</OPTION>
	<OPTION VALUE="000000">black</OPTION>
	<OPTION VALUE="FFFFFF">white</OPTION>
	<OPTION VALUE="800000">maroon</OPTION>
	<OPTION VALUE="808000">olive</OPTION>
	<OPTION VALUE="000080">navy</OPTION>
	<OPTION VALUE="800080">purple</OPTION>
	<OPTION VALUE="808080">gray</OPTION>
	<OPTION VALUE="C0C0C0">silver</OPTION>
	<OPTION VALUE="00FF00">lime</OPTION>
	<OPTION VALUE="FFFF00">yellow</OPTION>
	<OPTION VALUE="FF00FF">fuchsia</OPTION>
	<OPTION VALUE="00FFFF">aqua</OPTION>
	<OPTION VALUE="008080">teal</OPTION>
	</SELECT></TD></TR>
	<TR><TD>Message order</TD><TD><SELECT NAME="new_msg_on_top">
	<OPTION VALUE="$query(new_msg_on_top)" SELECTED>current</OPTION>
	<OPTION VALUE="1">newest on top</OPTION>
	<OPTION VALUE="0">newest on bottom</OPTION>
	</SELECT></TD></TR>
	<TR><TD>Login Sound</TD><TD><INPUT TYPE=TEXT SIZE=20 NAME="ls" VALUE="$query(ls)"></TD></TR>
	<TR><TD><H3>Change password</H3></TR></TD>
	<TR><TD>old password: </TD><TD><INPUT TYPE=PASSWORD NAME="change_pwd_old"></TD></TR>
	<TR><TD>new password: </TD><TD><INPUT TYPE=PASSWORD NAME="change_pwd_new"></TD></TR>
	<TR><TD>new password again: </TD><TD><INPUT TYPE=PASSWORD NAME="change_pwd_new2"></TD></TR>
	</TABLE>
	<INPUT TYPE=SUBMIT VALUE="set options">
	</FORM>
	</CENTER></BODY></HTML>
    }
}

#USERINFO.HTML - information about the users, click on user in online list
proc userinfo_html {} {
    global query userinfo html_title html_css html_bodytag
    P {
	<HTML>
	<HEAD><TITLE>$html_title - User Information</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>User Information</I></H1>
    }

    if {$userinfo(photo_url) ne ""} {
	P {<TABLE><TR><TD VALIGN=top><IMG SRC="$userinfo(photo_url)"></TD><TD VALIGN=top>\n}
    }
    P {
	<TABLE>
	<TR><TD><B>nickname</B>:</TD><TD>$query(infoabout)</TD></TR>
	<TR><TD><B>real name</B>:</TD><TD>$userinfo(realname)</TD></TR>
	<TR><TD><B>email</B>:</TD><TD><A HREF="mailto:$userinfo(email)">$userinfo(email)</A></TD></TR>
	<TR><TD><B>age</B>:</TD><TD>$userinfo(age)</TD></TR>
	<TR><TD><B>city</B>:</TD><TD>$userinfo(city)</TD></TR>
	<TR><TD><B>country</B>:</TD><TD>$userinfo(country)</TD></TR>
	<TR><TD><B>homepage url</B>:</TD><TD><A HREF="$userinfo(url)">$userinfo(url)</A></TD></TR>
	<TR><TD><B>ICQ uin</B>:</TD><TD>$userinfo(icq_uin)</TD></TR>
	<TR><TD VALIGN=top><B>anything else</B>:</TD><TD>$userinfo(stuff)</TD></TR>
	</TABLE>
    }
    if {$userinfo(photo_url) ne ""} {
	P {</TD></TR></TABLE>\n}
    }
    P {
	</CENTER>
	</BODY>
	</HTML>
    }
}

#CHANGEUSERINFO.HTML - change user information
proc changeuserinfo_html {} {
    global query userinfo html_title html_css html_bodytag script2_name
    P {
	<HTML>
	<HEAD><TITLE>$html_title - Change User Information</TITLE>
	$html_css
	</HEAD>
	$html_bodytag
	<CENTER>
	<H1><I>Change User Information</I></H1>
	<TABLE>
	<FORM METHOD=POST ACTION="$script2_name">
	<INPUT TYPE=hidden NAME="action" VALUE="setuserinfo">
	<INPUT TYPE=HIDDEN NAME="name" VALUE="$query(name)">
	<INPUT TYPE=HIDDEN NAME="password" VALUE="$query(password)">
	<INPUT TYPE=HIDDEN NAME="color" VALUE="$query(color)">
	<INPUT TYPE=HIDDEN NAME="new_msg_on_top" VALUE="$query(new_msg_on_top)">
	<INPUT TYPE=HIDDEN NAME="updatefrequency" VALUE="$query(updatefrequency)">
	<INPUT TYPE=HIDDEN NAME="ls" VALUE="$query(ls)">
	<TR><TD><B>real name</B>:</TD><TD><INPUT NAME="realname" VALUE="$userinfo(realname)"></TD></TR>
	<TR><TD><B>email</B>:</TD><TD><INPUT NAME="email" VALUE="$userinfo(email)"></TD></TR>
	<TR><TD><B>country</B>:</TD><TD><INPUT NAME="country" VALUE="$userinfo(country)"></TD></TR>
	<TR><TD><B>city</B>:</TD><TD><INPUT NAME="city" VALUE="$userinfo(city)"></TD></TR>
	<TR><TD><B>age</B>:</TD><TD><INPUT NAME="age" VALUE="$userinfo(age)"></TD></TR>
	<TR><TD><B>homepage url</B>:</TD><TD><INPUT NAME="url" VALUE="$userinfo(url)"></TD></TR>
	<TR><TD><B>picture url</B>:</TD><TD><INPUT NAME="photo_url" VALUE="$userinfo(photo_url)"></TD></TR>
	<TR><TD><B>ICQ uin</B>:</TD><TD><INPUT NAME="icq_uin" VALUE="$userinfo(icq_uin)"></TD></TR>
	<TR><TD VALIGN=top><B>everything else</B>:</TD><TD><TEXTAREA ROWS=10 COLS=50 NAME="stuff" WRAP="physical">$userinfo(stuff)</TEXTAREA></A></TD></TR>
	</TABLE>
	<INPUT TYPE=SUBMIT VALUE="set user info"></FORM>
	</CENTER>
	</BODY>
	</HTML>
    }
}

#SCRIPTLINKS.HTML - Links to other chatscripts
proc scriptlinks_html {} {
    global query html_css html_bodytag script2_name
    P {
	<HTML>
	<HEAD>
	$html_css
	</HEAD>
	$html_bodytag
	<A HREF="$script2_name?action=login&name=$query(name)&password=$query(password)" TARGET="_parent">chat 2</A>
	</BODY></HTML>
    }
}

#CHATFRAMES.HTML
proc chatframes_html {} {
    global query html_title banner_picture script_name script2_name
    P {<HTML><HEAD><TITLE>$html_title</TITLE></HEAD>\n}
    if {$banner_picture != ""} {
	P { <FRAMESET ROWS="65,*,60" BORDER=0 FRAMEBORDER=0 FRAMESPACING=0>\n}
	P {  <FRAME SRC="$script2_name?action=banner" SCROLLING=NO>\n}
    } else {
	P { <FRAMESET ROWS="*,60" BORDER=0 FRAMEBORDER=0 FRAMESPACING=0>\n}
    }
    P {  <FRAMESET COLS="*,150" BORDER=0 FRAMEBORDER=0 FRAMESPACING=0>\n}
    P {   <FRAME NAME="main" SRC="$script_name?action=chat&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)&pause=$query(pause)#end">  \n}
    P {    <FRAME SRC="$script_name?action=stillalive&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)">    \n}
    P {   </FRAMESET>\n}
    P {  <FRAME SRC="$script_name?action=chatinput_html&name=$query(name)&password=$query(password)&updatefrequency=$query(updatefrequency)&color=$query(color)&new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)&norefresh=1" scrolling=no>\n}
    P {</FRAMESET>\n}
    P {</HTML>\n}
}


# MESSAGES Part
#
########################

set chat2_msg(0) "has entered the chat"; # [user] has entered the chat

# illegal_nick_html
set chat2_msg(1) "Illegal Nick"; # Illegal Nick
set chat2_msg(2) "Your Nick \"$query(name)\" contains illegal characters<BR>A Nick may contain only letters, numbers and the underdash \"_\".<BR><B><A HREF=\"$script2_name?action=register\">retry</A>"; # Your Nick \"$query(name)\" contains illegal characters<BR>A Nick may contain only letters, numbers and the underdash \"_\".<BR><B><A HREF=\"$script2_name?action=register\">retry</A>

# illegal_pass_html
set chat2_msg(3) "Illegal Password"; # Illegal Password
set chat2_msg(4) "A password may not contain \";\"<BR><A HREF=\"$script2_name?action=register\">retry</A>"; # A password may not contain \";\"<BR><A HREF=\"$script2_name?action=register\">retry</A>

# pass_check_failed_html
set chat2_msg(5) "Password Check Failed"; # Password Check Failed
set chat2_msg(6) "The passwords you entered are not equal<BR><A HREF=\"$script2_name?action=register\">retry</A>"; # The passwords you entered are not equal<BR><A HREF=\"$script2_name?action=register\">retry</A>

# existingnick_html
set chat2_msg(7) "Nickname already exists"; # Nickname already exists
set chat2_msg(8) "<A HREF=\"$script2_name?action=register\">try another nick</A>"; # <A HREF=\"$script2_name?action=register\">try another nick</A>

# nickcreated_html
set chat2_msg(9) "Your nick is now registered";# Your nick is now registered
set chat2_msg(10) "<A HREF=\"$script_name\">login</A>"; # <A HREF=\"$script_name\">login</A>

# chat_is_full_html
set chat2_msg(11) "Chat is full"; # Chat is full
set chat2_msg(12) "Sorry, the chat room is full, please try another room or come back another time.<BR>You can login anyway if you login with the password \"yourpassword masterpassword\".<BR><A HREF=\"$script_name\">back</A>"; # Sorry, the chat room is full, please try another room or come back another time.<BR>You can login anyway if you login with the password \"yourpassword masterpassword\".<BR><A HREF=\"$script_name\">back</A>

# send_pwd_html
set chat2_msg(13) "Lost Password"; # Lost Password
set chat2_msg(14) "The password was sent to you.<BR><A HREF=\"$script_name\">back</A>"; # The password was sent to you ($query(email)).<BR><A HREF=\"$script_name\">back</A>

# illegal_international_nick_html
set chat2_msg(15) "Illegal Nick"; # Illegal Nick
set chat2_msg(16) "Your Nick \"$query(name)\" contains illegal characters<BR>A Nick may not contain \\\";/ :?*<>|<BR><A HREF=\"$script2_name?action=register\">retry</A>"; # Your Nick \"$query(name)\" contains illegal characters<BR>A Nick may not contain \\\";/ :?*<>|<BR><A HREF=\"$script2_name?action=register\">retry</A>

set chat2_msg(17) "Password changed"; # Password changed
set chat2_msg(18) "The new password may not contain \";\""; # The new password may not contain \";\"
set chat2_msg(19) "New password check failed, probably mistyped"; # New password check failed, probably mistyped
set chat2_msg(20) "please wait"; # please wait
set chat2_msg(21) "Memo from"; # Memo from
set chat2_msg(22) "\[CHAT\] Your Password"; # [CHAT] Your Password
set chat2_msg(23) "Your password is"; # Your password is
set chat2_msg(24) "- the admin"; # - the admin
set chat2_msg(25) "Nick"; # Nick

# Main Part - DON'T CHANGE ANYTHING HERE!
#
#######################



proc action {} {
    global qs query
    
    if {$qs eq ""} { # Wenn query_string leer sprung zum
    	ERROR "dont't execute this file, run chat.tcl instead"
	return
    }
    switch -- $query(action) {
	"register" {
	    header
	    register_html
	}
	"login" {
	    login
	}
	"create_nick" {
	    create_nick
	}
	"banner" {
	    header
	    banner_html
	}
	"options_html" {
	    header
	    options_html
	}
	"setoptions" {
	    setoptions
	}
	"gotourl" {
	    gotourl
	}
	"userinfo" {
	    userinfo
	}
	"changeuserinfo" {
	    changeuserinfo
	}
	"setuserinfo" {
	    setuserinfo
	}
	"show_users" {
	    show_users
	}
	"scriptlinks" {
	    header
	    scriptlinks_html
	}
	"send_pwd" {
	    send_pwd
	}
	"show_usernames" {
	    show_usernames
	}
	default {
	    ERROR "no such action: \"$query(action)\""
	}
    }
}

proc login {} { # Loginprozedur 

    global env query data_dir data_nicks_file config_msg data_banned_file
    global clear_old_msgs new_msg_on_top updatefrequency welcome_msg
    global data_msg_file welcome_msg chat2_msg timestamp

    # check login name+password
    set nicks [split [readfile $data_dir/$data_nicks_file] "\n"]
    if {[llength $nicks] == 0} {  # sonst nonick_html
    	header
    	user_error_html $config_msg(1) $config_msg(2)
        exit
    }

    # check if ip is banned
    set bans [list]
    set bans [splitsemi [readfile $data_dir/$data_banned_file]]
    if {[lsearch $bans "ip:$env(REMOTE_ADDR)"] > -1} {
	header
	user_error_html $config_msg(7) $config_msg(8)
	exit
    }

    max_user_limit; # check if chat is full

    set index [lsearch -regexp $nicks "(?i)^$query(name);;"]

    if {$index >= 0} {
	set nick [splitsemi [lindex $nicks $index]]
	set query(name) [lindex $nick 0]
	if {[lindex $nick 1] eq $query(password)} {
	    for {set i [llength $nick]} {$i < 13} {incr i} {
		lappend nick {}
	    }
	    # update last visited var
	    lset nick 4 [clock seconds]
	    # set ip
	    lset nick 11 $env(REMOTE_ADDR)

	    #set unkicked if user was kicked
	    switch -- [lindex $nick 10] {
		1 {
		    lset nick 10 0
		}
		2 {
		    #user is banned
                    header
                    user_error_html $config_msg(7) $config_msg(8)
                    exit
                }
	    }
	    if {$clear_old_msgs == 1} {
		clear_old_msgs_sub
	    }

	    if {[lindex $nick 5] ne ""} {
                set query(updatefrequency) [lindex $nick 5]
		set query(color)           [lindex $nick 6]
		set query(new_msg_on_top)  [lindex $nick 9]
		set query(ls)              [lindex $nick 13]
	    } else {
		set query(updatefrequency) $updatefrequency
		set query(color)           "standard"
		set query(new_msg_on_top)  $new_msg_on_top
	    }

	    # check memos
	    set memos [lindex $nick 12]
	    if {$memos ne "" && $memos > 0} {
		set query(pause)
		check_for_memo $memo
		lset nick 12 0
	    }
	    lset nicks $index [joinsemi $nick]
	    writefile $data_dir/$data_nicks_file [join $nicks "\n"]\n
	    
	    if {$welcome_msg ne ""} {
		postprivatemsg $query(name) "WELCOME" $welcome_msg
		#print login msg
		set msg "<B>$query(name) $chat2_msg(0)</B>"
		if {$query(ls) ne ""} {
		    append msg "<!--PLAYLOGINSOUND=$timestamp-->\n";
		} else {
		    append msg "\n";
		}						
		appendfile $data_dir/$data_msg_file $msg


                enterchat;    # wenn beides richtig goto chat
                exit;         # danach prozedur verlassen
            } else {
                header
                user_error_html $config_msg(3) "$config_msg(4)$query(name)$config_msg(19)"
                exit
            }
        }
    }
    
    # falls nick falsch nonick_html
    header
    user_error_html $config_msg(1) $config_msg(2)
}

proc enterchat {} {
    global query script_name header chatframes_html
    # LOG ACTION
    LOG 2 LOGIN $query(name)
    if {$query(noframes) == 1} {
	P {Location: $script_name?action=chatnoframes_html&name=$query(name)&} \
	    {password=$query(password)&color=$query(color)&} \
	    {new_msg_on_top=$query(new_msg_on_top)&ls=$query(ls)&noframes=1\n\n}
    } else {
	header;
    	chatframes_html
    }
}

proc validate_string {string} {
    global international_nicks
    # alle strings die spaces, semikolons oder steuerzeichen enthalten
    if {$string eq ""} {return 1}
    if {[regexp {\W} $string] && $international_nicks == 0} {
        return 1; # illegal string
    } elseif {[regexp {[\";/:\\\?\*<>\| ]} $string]} {
	return 1;
    } else {
        return 0; # legal string
    }
}

proc create_nick {} { # neuen nick anlegen + weiterleiten zum chat
    global env query international_nicks data_dir data_private_file
    global data_nicks_file mail_on_new_registration chat2_msg updatefrequency
    global new_msg_on_top
    if {[validate_string $query(name)] == 1} { # ueberpruefen ob nick illegale Zeichen enthaelt
        header
        if {$international_nicks == 0} {
	    user_error_html $chat2_msg(1) $chat2_msg(2)
	} else {
	    user_error_html $chat2_msg(15) $chat2_msg(16)
	}	
        exit
    }
    if {[string match "*;*" $query(password)] || $query(password) eq ""} {
	# ueberpruefen ob password illegale Zeichen enthaelt
        header
        user_error_html $chat2_msg(3) $chat2_msg(4)
        exit
    }

    if {$query(password) ne $query(password2)} {
        header
        user_error_html $chat2_msg(5) $chat2_msg(6)
        exit
    }

    set nicks [readfile $data_dir/$data_nicks_file]
    
    if {[lsearch $nicks "$query(name);;*"] > -1} {
	header;		 # wenn ja dann ausgabe existingnick.htm
	user_error_html $chat2_msg(7) $chat2_msg(8)
	exit
    }
            
    set time [clock seconds]
    set localtime [clock format $time]
    set nickdata [list $query(name) \
		      $query(password) \
		      $query(email) \
		      $localtime \
		      $time \
		      $updatefrequency \
		      "standard" \
		      "" \
		      "" \
		      $new_msg_on_top]
    appendfile $data_dir/$data_nicks_file [joinsemi $nickdata]\n
    writefile $data_dir/$data_private_file.$query(name) ""
    
    #MAIL TO ADMIN
    if {$mail_on_new_registration == 1} {
        set MAIL [open "|$mailprogramme -t" w]
        puts $MAIL "To: $admin_email_addresse"
        puts $MAIL "From: $admin_email_addresse (Ralfs Chat Script)"
        puts $MAIL "Subject: [CHAT] New Nick Registration"
	puts $MAIL ""
        puts $MAIL "A new user was registered"
        puts $MAIL "Nickname: $query(name)"
        puts $MAIL "eMail: $query(email)"
        puts $MAIL "HTTP User Agent: $env(HTTP_USER_AGENT)"
        puts $MAIL "Remote Addresse: $env(REMOTE_ADDR)"
    }

    # LOG ACTION
    LOG 1 NEW USER "$query(name) <$query(email)>"
    
    header
    user_error_html $chat2_msg(9) $chat2_msg(10)
}

proc clear_old_msgs_sub {} {

    global data_dir data_stillalive_file data_msg_file

    # safile einlesen
    set sa [splitsemi [readfile "$data_dir/$data_stillalive_file"]]
    
    set noe [clock seconds]
    set empty 1
    foreach {_ time} $sa {
	if {$time > $now - 150} {
	    set empty 0
	    break
	}
    }
    if {$empty} {
	writefile "$data_dir/$data_msg_file" ""
    }
}

proc setoptions {} {

    global query min_update_freq data_dir data_nicks_file chat2_msg

    checkpass

    if {$query(updatefrequency) < $min_update_freq ||
	$query(updatefrequency) eq ""
    } then {
        set $query(updatefrequency) $min_update_freq
    }

    set cpw_msg ""
    set change_password_ok 0

    if {$query(change_pwd_old) ne "" && $query(change_pwd_new) ne ""} {
	if {$query(change_pwd_old) eq $query(password)} {
	    if {$query(change_pwd_new) eq $query(change_pwd_new2)} {
		if {![regexp  {[;\n]} $query(change_pwd_new)]} { 
		    set change_password_ok 1
		    set cpw_msg $chat2_msg(17)
		} else {
		    set cpw_msg $chat2_msg(18)
		}
	    } else {
		set cpw_msg $chat2_msg(19)
	    }
	} else {
	    set cpw_msg $config_msg(3)
	}
	if {$cpw_msg ne ""} {
	    postprivatemsg "$query(name)" "CHANGE_PWD" $cpw_msg
	}
	if {$change_password_ok == 1} {
	    set query(password) $query(change_pwd_new)
	}
    }

    set nicks [split [readfile "$data_dir/$data_nicks_file"] "\n"]

    set index [lsearch $nicks "$query(name);;*"]
    if {$index > -1} {
	set nick [splitsemi [lindex $nicks $index]]
	lset nick 1 $query(password)
	lset nick 5 $query(updatefrequency)
	lset nick 6 $query(color)
	lset nick 9 $query(new_msg_on_top)
	for {set i [llength $nick]} {$i <= 13} {incr i} {
	    lappend nick {}
	}
	if {$query(ls) ne "" &&
	    [regsub {^(file:)?(//)?(localhost)?/?(.*)} \
		 $query(ls) {file://localhost/\4} query(ls)]
	} then {
	    lset nick 13 $query(ls)
	} else {
	    lset nick 13 ""
	}
	lset nicks $index [joinsemi $nick]
	writefile $data_dir/$data_nicks_file [join $nicks "\n"]\n
    }
    
    header
    chatframes_html
}

proc setuserinfo {} {

    global query data_dir data_userinfo_file
    checkpass
    
    set map {; &\#59; & &amp; < &lt; \n <br> \r ""}

    foreach item [list $query(name) $query(realname) $query(email) \
		      $query(age) $query(city) $query(country) \
		      $query(url) $query(stuff) $query(photo_url) \
		      $query(icq_uin)] {
	lappend userinfo [string map $map $item]
    }
    
    set ui [split [readfile "$data_dir/$data_userinfo_file"] "\n"]
    set index [lsearch $ui "$query(name);;*"]

    if {$index > -1} {
	lset ui $index [joinsemi $userinfo]
    } else {
	lappend ui [joinsemi $userinfo]
    }
    # puts stderr [joinsemi $userinfo]
    writefile "$data_dir/$data_userinfo_file" [join $ui "\n"]

    header
    chatframes_html
}

proc changeuserinfo {} {
    global userinfo query data_dir data_userinfo_file

    checkpass

    set map {<br> \n &amp; & &\#59; ;}

    set ui [split [readfile "$data_dir/$data_userinfo_file"] "\n"]
    set index [lsearch $ui "$query(name);;*"]

    if {$index > -1} {
	foreach {_ userinfo(realname) userinfo(email) userinfo(age)
	    userinfo(city) userinfo(country) userinfo(url) userinfo(stuff)
	    userinfo(photo_url) userinfo(icq_uin)
	} [splitsemi [string map $map [lindex $ui $index]]] break
    } else {
	array set userinfo {
	    realname {} email {} age {} city {} country {} url {} stuff {}
	    photo_url {} icq_uin {}
	}
    }
    header
    changeuserinfo_html
}

proc show_users {} {
    header
    P [number_of_users]
}

proc number_of_users {} {
    global data_dir data_stillalive_file chat_msg

    array set sa [splitsemi [readfile "$data_dir/$data_stillalive_file"]]

    # &repair_safile if ($#sa % 2 == 0); # wenn safile fehlerhaft -> repair

    # Eintraege nach ueberfaelligen (aelter als 150s) durchsuchen und diese entfernen    

    set now [clock seconds]
    foreach {k v} [array get sa] {
	# ist zeitstempel schon aelter als 150s?
	if {$v < ($now - 150)} {
	    unset sa($k)
	}
    }
    writefile $data_dir/$data_stillalive_file [joinsemi [array get sa]]

    return [array size sa]
}

proc gotourl {} {

    global query chat2_msg

    header
    P {
	<HTML>
	<meta http-equiv="refresh" content="0; URL=$query(url)">
	<BODY>$chat2_msg(20)</BODY>
	</HTML>
    }
}

proc max_user_limit {} {

    global max_users query

    if {[number_of_users] >= $max_users && $max_users >= 0} {
        if {[string match "* $masterpassword" $query(password)]} {
	    regexp {[^ ]+} $query(password) query(password)
        } else {
            header
            user_error_html $chat2_msg(11) $chat2_msg(12)
            exit
        }
    }
}

proc check_for_memo {} {

    global data_dir data_memo_file days_to_keep_memos

    set memos [split [readfile "$data_dir/$data_memo_file"] "\n"]

    set time [expr {[clock seconds] - $days_to_keep_memos * 60 * 60 * 24}]
    set outmemos [list]
    foreach memo $memos {
	set memo [splitsemi $memo]
	foreach {to from text time} $memo break
	if {$to eq $query(name)} {
	    postprivatemsg $query(name) "MEMO" \
		"<B>$chat2_msg(21) $from ([clock format $time]): $test</B>"
	} elseif {[lindex $memo 3] > $time} {
	    lappend outmemos [joinsemi $memo]
	}
    }
    writefile $data_dir/$data_memo_file [join $outmemos "\n"]
}

proc send_pwd {} {

    global query chat_msg chat2_msg data_dir data_nicks_file mailprogramme
    
    set nicks [split [readfile $data_dir/$data_nicks_file] "\n"]

    set index [lsearch $nicks "$query(name);;*"]

    if {$index > -1} {

	set nick [splitsemi [lindex $nicks $index]]
	
	set query(email) [lindex $nick 2]
	set query(password) [lindex $nick 1]

	# mail to user
	set MAIL [open "|$mailprogramme -t" w]
	puts $MAIL "To: $query(email)"
	puts $MAIL "From: $admin_email_addresse ($html_title)"
	puts $MAIL "Subject: $chat2_msg(22)"
	puts $MAIL ""
	puts $MAIL "$chat2_msg(23) $query(password) ($chat2_msg(25): $query(name))"
	puts $MAIL ""
	puts $MAIL "$chat2_msg(24)"
	close $MAIL

	header
	user_error_html $chat2_msg(13) $chat2_msg(14)
    }
}

proc show_usernames {} {

    global data_dir data_stillalive_file

    header

    array set sa [splitsemi [readfile $data_dir/$data_stillalive_file]]
    # &repair_safile if ($#sa % 2 == 0); # wenn safile fehlerhaft -> repair

    # Eintraege nach ueberfaelligen (aelter als 150s) durchsuchen und diese entfernen    
    set now [clock seconds]
    foreach {k v} [array get sa] {
	# ist zeitstempel schon aelter als 150s?
	if {$v < ($now - 150)} {
	    unset sa($k)
	} else {
	    P {$k<br>\n}
	}
    }
    writefile $data_dir/$data_stillalive_file [joinsemi [array get sa]]
}

# Fuehrt je nach action variable gegebene sub aus
# action im FORM definieren!
action
