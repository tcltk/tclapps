# pkgIndex.tcl --
# Copyright (c) 2003 Andreas Kupries

package ifneeded tools      0.1 [list source [file join $dir tools.tcl]]
package ifneeded optchecker 0.1 [list source [file join $dir optcheck.tcl]]
