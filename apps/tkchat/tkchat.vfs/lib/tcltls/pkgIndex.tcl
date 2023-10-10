if {[package vsatisfies [package present Tcl] 8.5-]} {
	package ifneeded tls 1.7.23 [list apply {{dir} {
		if {{shared} eq "static"} {
			load {} Tls
		} else {
		    load [file join $dir tcltls[info sharedlibextension]] Tls
		}

		set tlsTclInitScript [file join $dir tls.tcl]
		if {[file exists $tlsTclInitScript]} {
			source $tlsTclInitScript
		}
	}} $dir]
} elseif {[package vsatisfies [package present Tcl] 8.4]} {
	package ifneeded tls 1.7.23 [list load [file join $dir tcltls[info sharedlibextension]] Tls]
}
