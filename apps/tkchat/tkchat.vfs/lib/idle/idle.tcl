# idle.tcl -Critcl based library to do user idle detection on various platforms.
#
# Copyright (C) 2005 Pascal Scheffers <pascal@scheffers.net> All Rights Reserved
#
# Licensed under the same License Terms as Tcl/Tk 8.4 or later.
# See http://www.tcl.tk/software/tcltk/license_terms.txt for the exact terms.
#
# The primary location for this package is http://svn.scheffers.net/misc/idle
# (a subversion repository).

# For developers:
# Usage:
#   package require idle 1.0
#   if { ![idle::supported] } {
#       puts "cannot do idle detection on your platform"
#   }
#   namespace import idle::*
#   puts "You have been idle for [idletime] seconds."
#
# Notes:
#   * If [idletime] returns -1, your platform is supported, but cannot determine
#   the idle time *at this time*
#   * If your platform is not supported, [idletime] always returns 0.

#
# Adding support for a new platform:
# * add a proc which:
#     - has the (critcl) code needed for your platform
#     - this proc overrides proc idle::idletime and returns the number of seconds
#       since the last user activity.
#     - Finaly, sets the variable idle::supported to 1.
# * Update proc idle::init to correctly detect your platform and calls
#   your platform init proc

package require critcl

namespace eval idle {
    variable dir [file dirname [info script]]
    variable supported 0
    namespace export idletime
}

proc idle::init {} {
    global tcl_platform
    if {[catch {tk inactive} err]} {
        switch -- $tcl_platform(os) {
            "Windows NT" {
                if { $tcl_platform(osVersion) >= 5.0 } {
                    win32init
                }
            }
        }
    } else {
        # Tk 8.5 has [tk inactive] which does the same job.
        # Round to nearest second.
        proc ::idle::idletime {} { return [expr {int(floor([tk inactive] + 500)/1000)}] }
        proc ::idle::supported {} { return 1 }
    }
}

proc idle::idletime { } {
    # This is the 'dummy' proc, for unsupported platforms.
    return 0
}

proc idle::supported { } {
    variable supported
    # This is the 'dummy' proc, for unsupported platforms.
    return $supported
}

proc idle::loadlib {} {
    # Try to load the shared object, if it exists
    variable dir
    
    set dll [file join $dir [critcl::platform] idle[info sharedlibextension]]
    
    if { [file exists $dll] } {
        load $dll
        return 1
    }
    
    return 0  
    
}

proc idle::win32init {} {
    variable supported
    variable dir
    
    if { ![loadlib] } {        
        critcl::clibraries -luser32
        critcl::ccode {
#define STRICT
#define WIN32_LEAN_AND_MEAN
#define WINVER 0x0410
#define _WIN32_WINNT 0x0500  /* win2k+ required */
#include <windows.h>
        }
        
        critcl::cproc GetLastInputInfo {} long {
            LASTINPUTINFO li;
            li.cbSize=sizeof(LASTINPUTINFO);
            
            if ( !GetLastInputInfo(&li) ) {
                return -1;
            }
            /* ticks are in milliseconds. We don't need that resolution. 
             * Round to the nearest second.
             */
            return (GetTickCount() - li.dwTime + 500)/1000;    
        }
    }
    
    set supported 1
    
    proc idletime {} {
        return [GetLastInputInfo]
    }
    
}

idle::init

package provide idle 1.0
