set exe [zipfs mount //zipfs:/app]
if {[tk windowingsystem] eq "win32"} {
	set output "TkChat.exe"
} else {
	set output "tkchat"
}
puts "setting root\n"
file delete -force [file join tkchat.vfs tcl_library]
file delete -force [file join tkchat.vfs tk_library]
file copy -force $tcl_library [file join tkchat.vfs tcl_library]
file copy -force $tk_library [file join tkchat.vfs tk_library]
zipfs mkimg $output tkchat.vfs tkchat.vfs "" $exe
puts "done"
