if {![package vsatisfies [package provide Tcl] 8.2]} {return}
package ifneeded sha1   2.0.3 [list source [file join $dir sha1.tcl]]
