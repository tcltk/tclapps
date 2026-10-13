# -*- tcl -*-
# Tcl package index file, version 1.1
#
if {[package vsatisfies [package provide Tcl] 9.0-]} {
    package ifneeded tls 2.0.1 [list apply {{dir} {
        # Find and load the dynamic library
        set lib ""
        foreach file [glob -nocomplain -directory $dir *] {
            if {[regexp -nocase {^tcl9tls201\.(dll|dylib|so)(\..*)?$} \
                    [file tail $file]]} {
                set lib $file
                break
            }
        }

        if {$lib ne ""} {
            load $lib [string totitle tls]
        } else {
            load {} [string totitle tls]
        }

        # Source init file
        set initScript [file join $dir tls.tcl]
        if {[file exists $initScript]} {
            source -encoding utf-8 $initScript
        }
    }} $dir]
} else {
    if {![package vsatisfies [package provide Tcl] 8.5]} {
        return
    }
    package ifneeded tls 2.0.1 [list apply {{dir} {
        # Find and load the dynamic library
        set lib ""
        foreach file [glob -nocomplain -directory $dir *] {
            if {[regexp -nocase {^tls201t\.(dll|dylib|so)(\..*)?$} \
                    [file tail $file]]} {
                set lib $file
                break
            }
        }

        if {$lib ne ""} {
            load $lib [string totitle tls]
        } else {
            load {} [string totitle tls]
        }

        # Source init file
        set initScript [file join $dir tls.tcl]
        if {[file exists $initScript]} {
            source -encoding utf-8 $initScript
        }
    }} $dir]
}