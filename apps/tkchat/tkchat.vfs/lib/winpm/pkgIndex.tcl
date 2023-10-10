if {![string equal $::tcl_platform(platform) "windows"]} { return }
if {![string equal $::tcl_platform(machine) "intel"]} { return }
package ifneeded winpm 0.1 \
    [list load [file join $dir winpm01.dll] winpm]
