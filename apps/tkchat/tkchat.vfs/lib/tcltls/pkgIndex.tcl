if {[package vsatisfies [package present Tcl] 8.5-]} {
    if {$::tcl_platform(os) eq "Darwin"} {
	package ifneeded tls 1.8.0
	load [file join $dir libtcl9tls1.8.0.dylib] Tls
	return
    }
	if {$::tcl_platform(os) eq "Windows NT"} {
	package ifneeded tls 1.7.23 
	load [file join $dir tcl9tls1723.dll] Tls
	return
    }
	if {$::tcl_platform(os) eq "Linux"} {
	package ifneeded tls 1.7.23 
	load [file join $dir tcltls.so] Tls
	return
    }
	set tlsTclInitScript [file join $dir tls.tcl]
	if {[file exists $tlsTclInitScript]} {
		source $tlsTclInitScript
	}
}
