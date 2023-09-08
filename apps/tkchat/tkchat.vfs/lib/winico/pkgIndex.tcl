# Winico pkgIndex
# Only relevant on Windows-x86
if {[string compare $::tcl_platform(platform) "windows"]} { return }
if {[string compare $::tcl_platform(machine) "intel"]} { return }
package ifneeded Winico 0.6 [list load [file join $dir Winico06.dll]]
