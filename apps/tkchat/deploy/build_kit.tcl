set exe [zipfs mount //zipfs:/app]
puts "setting root\n"
puts "copying tcl library...\n"
file copy -force $tcl_library [file join tkchat.vfs tcl_library]
puts "copying tk library...\n"
file copy -force $tk_library [file join tkchat.vfs tk_library]
puts "making exe...\n"
zipfs mkimg TkChat.exe tkchat.vfs tkchat.vfs "" $exe
puts "done"
