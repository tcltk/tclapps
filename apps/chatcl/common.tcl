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

proc LOG {level type message} {
    global env logtype log_dir log_file
    if {$logtype >= $level} {
	set LOGFILE [open $log_dir/$log_file a]
	flock -write $LOGFILE
	puts $LOGFILE \
	    [list [clock seconds] $env(REMOTE_ADDR) $type $message]
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
