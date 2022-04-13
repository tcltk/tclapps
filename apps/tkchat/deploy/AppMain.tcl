package require tk

if {[tk windowingsystem] eq "win32"} {
	after 500
	update idletasks
}

set dir  [file join [file dirname [info script]]]
lappend auto_path $dir/lib
lappend auto_path $dir/bin
source [file join $dir bin tkchat.tcl]
