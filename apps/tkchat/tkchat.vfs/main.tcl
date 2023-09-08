# Starkit setup.
#
package require Tk
package require starkit
starkit::startup

set kitdir [file dirname $starkit::topdir]

# Try and load a tls starkit.
#
switch -exact -- $tcl_platform(platform) {
    "windows" { lappend tlsnames [file join $kitdir tls-win.kit] }
    "unix"    { lappend tlsnames [file join $kitdir tls-lin.kit] }
}
lappend tlsnames [file join $kitdir tls.kit] [auto_execok tls.kit]
foreach tlsfile $tlsnames {
    if { [file exists $tlsfile] } {
        source $tlsfile
        break
    }
}

# Try and load the most recent tile starkit if using Tk 8.4
#
if {![package vsatisfies [package provide Tk] 8.5]} {
    foreach tile [lsort -decreasing \
                      [glob -nocomplain -directory $kitdir -tail tile*.kit]] {
        if {[file exists [file join $kitdir $tile]]} {
            source [file join $kitdir $tile]
            break
        }
    }
}

# Cleanup global variables.
#
unset -nocomplain kitdir tlsfile tlsnames tile

# Start the chat script.
#
source [file join $starkit::topdir bin tkchat.tcl]
