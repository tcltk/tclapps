set exe [zipfs mount //zipfs:/app]
if {[tk windowingsystem] eq "win32"} {
	set output "TkChat.exe"
} else {
	set output "tkchat"
}
puts "setting root\n"
zipfs mkimg $output tkchat.vfs tkchat.vfs "" $exe
puts "done"
