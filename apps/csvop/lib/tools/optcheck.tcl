# -*- tcl -*-
# Checking various types of things, mainly paths.
# ------------------------------------------------------

package require tools

namespace eval ::optcheck {}

# ------------------------------------------------------

proc ::optcheck::outdir {path label} {
    if {[file exists $path]} {
	# The output directory already exists. Make sure that it is
	# truly a directory we can write to.

	if {![file isdirectory $path]} {tools::usage "$label \"$path\" is a file"}
	if {![file writable    $path]} {tools::usage "$label \"$path\" is not writable"}
	return
    }

    # The path itself does not exist, so we look upward for the part
    # which does exist and make sure that this part is a writable
    # directory.
		
    set up [file dirname $path]
    while {![file exists $up]} {
	set last $up
	set up [file dirname $up]
	if {[string equal $last $up]} break
    }

    if {![file exists      $up]} {tools::usage "$label \"$path\" is so bogus that checks are impossible"}
    if {![file isdirectory $up]} {tools::usage "Parent \"$up\" of \"$path\" is a file"}
    if {![file writable    $up]} {tools::usage "Parent \"$up\" of \"$path\" is not writable"}
    return
}


proc ::optcheck::infile {path label} {
    if {[file exists $path]} {
	if {![file isfile   $path]} {tools::usage "$label \"$path\" is not a file"}
	if {![file readable $path]} {tools::usage "$label \"$path\" is not readable"}
	return
    }

    tools::usage "$label \"$path\" does not exist"
    return
}

proc ::optcheck::outfile {path label} {
    if {[file exists $path]} {
	# The output file already exists. Make sure that it is truly a
	# file we can write to.

	if {![file isfile   $path]} {tools::usage "$label \"$path\" is not a file"}
	if {![file writable $path]} {tools::usage "$label \"$path\" is not writable"}
	return
    }

    # The path itself does not exist, so we look upward for the part
    # which does exist and make sure that this part is a writable
    # directory. Yes, directory, the directory the new file will be
    # placed in.
		
    set up [file dirname $path]
    while {![file exists $up]} {
	set last $up
	set up [file dirname $up]
	if {[string equal $last $up]} break
    }

    if {![file exists      $up]} {tools::usage "$label \"$path\" is so bogus that checks are impossible"}
    if {![file isdirectory $up]} {tools::usage "Parent \"$up\" of \"$path\" is a file"}
    if {![file writable    $up]} {tools::usage "Parent \"$up\" of \"$path\" is not writable"}
    return
}

# ------------------------------------------------------
package provide optchecker 0.1
