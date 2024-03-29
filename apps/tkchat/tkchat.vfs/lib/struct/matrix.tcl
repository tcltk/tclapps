# matrix.tcl --
#
#	Implementation of a matrix data structure for Tcl.
#
# Copyright (c) 2001 by Andreas Kupries <a.kupries@westend.com>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: matrix.tcl,v 1.3 2001/07/10 20:39:47 andreas_kupries Exp $

namespace eval ::struct {}

namespace eval ::struct::matrix {
    # Data storage in the matrix module
    # -------------------------------
    #
    # One namespace per object, containing
    #
    # - Two scalar variables containing the current number of rows and columns.
    # - Four array variables containing the array data, the caches for
    #   rowheights and columnwidths and the information about linked arrays.
    #
    # The variables are
    # - columns #columns in data
    # - rows    #rows in data
    # - data    cell contents
    # - colw    cache of columnwidths
    # - rowh    cache of rowheights
    # - link    information about linked arrays
    
    # counter is used to give a unique name for unnamed matrixs
    variable counter 0

    # commands is the list of subcommands recognized by the matrix
    variable commands
    set      commands(.) [list	\
	    "add"		\
	    "cells"		\
	    "cellsize"		\
	    "columns"		\
	    "columnwidth"	\
	    "delete"		\
	    "destroy"		\
	    "format"		\
	    "get"		\
	    "insert"		\
	    "link"		\
	    "rowheight"		\
	    "rows"		\
	    "set"		\
	    "swap"		\
	    "unlink"
	    ]

    # Some subcommands have their own subcommands.
    set commands(add)    [list "column" "columns" "row" "rows"]
    set commands(delete) [list "column" "row"]
    set commands(format) [list "2chan" "2string"]
    set commands(get)    [list "cell" "column" "rect" "row"]
    set commands(insert) [list "column" "row"]
    set commands(set)    [list "cell" "column" "rect" "row"]
    set commands(swap)   [list "columns" "rows"]

    # Only export one command, the one used to instantiate a new matrix
    namespace export matrix
}

# ::struct::matrix::matrix --
#
#	Create a new matrix with a given name; if no name is given, use
#	matrixX, where X is a number.
#
# Arguments:
#	name	Optional name of the matrix; if null or not given, generate one.
#
# Results:
#	name	Name of the matrix created

proc ::struct::matrix::matrix {{name ""}} {
    variable counter
    
    if { [llength [info level 0]] == 1 } {
	incr counter
	set name "matrix${counter}"
    }

    if { [llength [info commands ::$name]] } {
	error "command \"$name\" already exists, unable to create matrix"
    }

    # Set up the namespace
    namespace eval ::struct::matrix::matrix$name {
	variable columns 0
	variable rows    0

	variable data
	variable colw
	variable rowh
	variable link

	array set data {}
	array set colw {}
	array set rowh {}
	array set link {}
    }

    # Create the command to manipulate the matrix
    interp alias {} ::$name {} ::struct::matrix::MatrixProc $name

    return $name
}

##########################
# Private functions follow

# ::struct::matrix::MatrixProc --
#
#	Command that processes all matrix object commands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand to invoke.
#	args	Arguments for subcommand.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::MatrixProc {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(.) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::_$cmd $name] $args
}

# ::struct::matrix::_add --
#
#	Command that processes all 'add' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'add' to invoke.
#	args	Arguments for subcommand of 'add'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_add {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name add option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__add_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(add) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__add_$cmd $name] $args
}

# ::struct::matrix::_delete --
#
#	Command that processes all 'delete' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'delete' to invoke.
#	args	Arguments for subcommand of 'delete'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_delete {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name delete option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__delete_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(delete) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__delete_$cmd $name] $args
}

# ::struct::matrix::_format --
#
#	Command that processes all 'format' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'format' to invoke.
#	args	Arguments for subcommand of 'format'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_format {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name format option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__format_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(format) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__format_$cmd $name] $args
}

# ::struct::matrix::_get --
#
#	Command that processes all 'get' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'get' to invoke.
#	args	Arguments for subcommand of 'get'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_get {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name get option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__get_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(get) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__get_$cmd $name] $args
}

# ::struct::matrix::_insert --
#
#	Command that processes all 'insert' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'insert' to invoke.
#	args	Arguments for subcommand of 'insert'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_insert {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name insert option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__insert_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(insert) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__insert_$cmd $name] $args
}

# ::struct::matrix::_set --
#
#	Command that processes all 'set' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'set' to invoke.
#	args	Arguments for subcommand of 'set'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_set {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name set option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__set_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(set) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__set_$cmd $name] $args
}

# ::struct::matrix::_swap --
#
#	Command that processes all 'swap' subcommands.
#
# Arguments:
#	name	Name of the matrix object to manipulate.
#	cmd	Subcommand of 'swap' to invoke.
#	args	Arguments for subcommand of 'swap'.
#
# Results:
#	Varies based on command to perform

proc ::struct::matrix::_swap {name {cmd ""} args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 2 } {
	error "wrong # args: should be \"$name swap option ?arg arg ...?\""
    }
    
    # Split the args into command and args components
    if { [llength [info commands ::struct::matrix::__swap_$cmd]] == 0 } {
	variable commands
	set optlist [join $commands(swap) ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$cmd\": must be $optlist"
    }
    eval [list ::struct::matrix::__swap_$cmd $name] $args
}

# ::struct::matrix::__add_column --
#
#	Extends the matrix by one column and then acts like
#	"setcolumn" (see below) on this new column if there were
#	"values" supplied. Without "values" the new cells will be set
#	to the empty string. The new column is appended immediately
#	behind the last existing column.
#
# Arguments:
#	name	Name of the matrix object.
#	values	Optional values to set into the new row.
#
# Results:
#	None.

proc ::struct::matrix::__add_column {name {values {}}} {
    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::rowh    rh

    if {[set l [llength $values]] < $rows} {
	# Missing values. Fill up with empty strings

	for {} {$l < $rows} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $rows} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$rows - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:

    # - The new column is not added to the width cache, the other
    #   columns are not touched, the cache therefore unchanged.
    # - The rows are either removed from the height cache or left
    #   unchanged, depending on the contents set into the cell.

    set r 0
    foreach v $values {
	if {$v != {}} {
	    # Data changed unpredictably, invalidate cache
	    catch {unset rh($r)}
	} ; # {else leave the row unchanged}
	set data($cols,$r) $v
	incr r
    }
    incr cols
    return
}

# ::struct::matrix::__add_row --
#
#	Extends the matrix by one row and then acts like "setrow" (see
#	below) on this new row if there were "values"
#	supplied. Without "values" the new cells will be set to the
#	empty string. The new row is appended immediately behind the
#	last existing row.
#
# Arguments:
#	name	Name of the matrix object.
#	values	Optional values to set into the new row.
#
# Results:
#	None.

proc ::struct::matrix::__add_row {name {values {}}} {
    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::colw    cw

    if {[set l [llength $values]] < $cols} {
	# Missing values. Fill up with empty strings

	for {} {$l < $cols} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $cols} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$cols - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:

    # - The new row is not added to the height cache, the other
    #   rows are not touched, the cache therefore unchanged.
    # - The columns are either removed from the width cache or left
    #   unchanged, depending on the contents set into the cell.

    set c 0
    foreach v $values {
	if {$v != {}} {
	    # Data changed unpredictably, invalidate cache
	    catch {unset cw($c)}
	} ; # {else leave the row unchanged}
	set data($c,$rows) $v
	incr c
    }
    incr rows
    return
}

# ::struct::matrix::__add_columns --
#
#	Extends the matrix by "n" columns. The new cells will be set
#	to the empty string. The new columns are appended immediately
#	behind the last existing column. A value of "n" equal to or
#	smaller than 0 is not allowed.
#
# Arguments:
#	name	Name of the matrix object.
#	n	The number of new columns to create.
#
# Results:
#	None.

proc ::struct::matrix::__add_columns {name n} {
    if {$n <= 0} {
	return -code error "A value of n <= 0 is not allowed"
    }

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows

    # The new values set into the cell is always the empty
    # string. These have a length and height of 0, i.e. the don't
    # influence cached widths and heights as they are at least that
    # big. IOW there is no need to touch and change the width and
    # height caches.

    while {$n > 0} {
	for {set r 0} {$r < $rows} {incr r} {
	    set data($cols,$r) ""
	}
	incr cols
	incr n -1
    }

    return
}

# ::struct::matrix::__add_rows --
#
#	Extends the matrix by "n" rows. The new cells will be set to
#	the empty string. The new rows are appended immediately behind
#	the last existing row. A value of "n" equal to or smaller than
#	0 is not allowed.
#
# Arguments:
#	name	Name of the matrix object.
#	n	The number of new rows to create.
#
# Results:
#	None.

proc ::struct::matrix::__add_rows {name n} {
    if {$n <= 0} {
	return -code error "A value of n <= 0 is not allowed"
    }

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows

    # The new values set into the cell is always the empty
    # string. These have a length and height of 0, i.e. the don't
    # influence cached widths and heights as they are at least that
    # big. IOW there is no need to touch and change the width and
    # height caches.

    while {$n > 0} {
	for {set c 0} {$c < $cols} {incr c} {
	    set data($rows,$c) ""
	}
	incr rows
	incr n -1
    }
    return
}

# ::struct::matrix::_cells --
#
#	Returns the number of cells currently managed by the
#	matrix. This is the product of "rows" and "columns".
#
# Arguments:
#	name	Name of the matrix object.
#
# Results:
#	The number of cells in the matrix.

proc ::struct::matrix::_cells {name} {
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::columns columns
    return [expr {$rows * $columns}]
}

# ::struct::matrix::_cellsize --
#
#	Returns the length of the string representation of the value
#	currently contained in the addressed cell.
#
# Arguments:
#	name	Name of the matrix object.
#	column	Column index of the cell to query
#	row	Row index of the cell to query
#
# Results:
#	The number of cells in the matrix.

proc ::struct::matrix::_cellsize {name column row} {
    set column [ChkColumnIndex $name $column]
    set row    [ChkRowIndex    $name $row]

    upvar ::struct::matrix::matrix${name}::data data
    return [string length $data($column,$row)]
}

# ::struct::matrix::_columns --
#
#	Returns the number of columns currently managed by the
#	matrix.
#
# Arguments:
#	name	Name of the matrix object.
#
# Results:
#	The number of columns in the matrix.

proc ::struct::matrix::_columns {name} {
    upvar ::struct::matrix::matrix${name}::columns columns
    return $columns
}

# ::struct::matrix::_columnwidth --
#
#	Returns the length of the longest string representation of all
#	the values currently contained in the cells of the addressed
#	column if these are all spanning only one line. For cell
#	values spanning multiple lines the length of their longest
#	line goes into the computation.
#
# Arguments:
#	name	Name of the matrix object.
#	column	The index of the column whose width is asked for.
#
# Results:
#	See description.

proc ::struct::matrix::_columnwidth {name column} {
    set column [ChkColumnIndex $name $column]

    upvar ::struct::matrix::matrix${name}::colw cw

    if {![info exists cw($column)]} {
	upvar ::struct::matrix::matrix${name}::rows rows
	upvar ::struct::matrix::matrix${name}::data data

	set width 0
	for {set r 0} {$r < $rows} {incr r} {
	    foreach line [split $data($column,$r) \n] {
		set len [string length $line]
		if {$len > $width} {
		    set width $len
		}
	    }
	}

	set cw($column) $width
    }

    return $cw($column)
}

# ::struct::matrix::__delete_column --
#
#	Deletes the specified column from the matrix and shifts all
#	columns with higher indices one index down.
#
# Arguments:
#	name	Name of the matrix.
#	column	The index of the column to delete.
#
# Results:
#	None.

proc ::struct::matrix::__delete_column {name column} {
    set column [ChkColumnIndex $name $column]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::colw    cw
    upvar ::struct::matrix::matrix${name}::rowh    rh

    # Move all data from the higher columns down and then delete the
    # superfluous data in the old last column. Move the data in the
    # width cache too, take partial fill into account there too.
    # Invalidate the height cache for all rows.

    for {set r 0} {$r < $rows} {incr r} {
	for {set c $column; set cn [expr {$c + 1}]} {$cn < $cols} {incr c ; incr cn} {
	    set data($c,$r) $data($cn,$r)
	    if {[info exists cw($cn)]} {
		set cw($c) $cw($cn)
		unset cw($cn)
	    }
	}
	unset data($c,$r)
	catch {unset rh($r)}
    }
    incr cols -1
    return
}

# ::struct::matrix::__delete_row --
#
#	Deletes the specified row from the matrix and shifts all
#	row with higher indices one index down.
#
# Arguments:
#	name	Name of the matrix.
#	row	The index of the row to delete.
#
# Results:
#	None.

proc ::struct::matrix::__delete_row {name row} {
    set row [ChkRowIndex $name $row]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::colw    cw
    upvar ::struct::matrix::matrix${name}::rowh    rh

    # Move all data from the higher rows down and then delete the
    # superfluous data in the old last row. Move the data in the
    # height cache too, take partial fill into account there too.
    # Invalidate the width cache for all columns.

    for {set c 0} {$c < $cols} {incr c} {
	for {set r $row; set rn [expr {$r + 1}]} {$rn < $rows} {incr r ; incr rn} {
	    set data($c,$r) $data($c,$rn)
	    if {[info exists rh($rn)]} {
		set rh($r) $rh($rn)
		unset rh($rn)
	    }
	}
	unset data($c,$r)
	catch {unset cw($c)}
    }
    incr rows -1
    return
}

# ::struct::matrix::_destroy --
#
#	Destroy a matrix, including its associated command and data storage.
#
# Arguments:
#	name	Name of the matrix to destroy.
#
# Results:
#	None.

proc ::struct::matrix::_destroy {name} {
    upvar ::struct::matrix::matrix${name}::link link

    # Unlink all existing arrays before destroying the object so that
    # we don't leave dangling references / traces.

    foreach avar [array names link] {
	_unlink $name $avar
    }

    namespace delete ::struct::matrix::matrix$name
    interp alias {} ::$name {}
}

# ::struct::matrix::__format_2string --
#
#	Formats the matrix using the specified report object and
#	returns the string containing the result of this
#	operation. The report has to support the "printmatrix" method.
#
# Arguments:
#	name	Name of the matrix.
#	report	Name of the report object specifying the formatting.
#
# Results:
#	A string containing the formatting result.

proc ::struct::matrix::__format_2string {name report} {
    return [$report printmatrix $name]
}

# ::struct::matrix::__format_2chan --
#
#	Formats the matrix using the specified report object and
#	writes the string containing the result of this operation into
#	the channel. The report has to support the
#	"printmatrix2channel" method.
#
# Arguments:
#	name	Name of the matrix.
#	report	Name of the report object specifying the formatting.
#	chan	Handle of the channel to write to.
#
# Results:
#	None.

proc ::struct::matrix::__format_2chan {name report chan} {
    $report printmatrix2channel $name $chan
    return
}

# ::struct::matrix::__get_dell --
#
#	Returns the value currently contained in the cell identified
#	by row and column index.
#
# Arguments:
#	name	Name of the matrix.
#	column	Column index of the addressed cell.
#	row	Row index of the addressed cell.
#
# Results:
#	value	Value currently stored in the addressed cell.

proc ::struct::matrix::__get_cell {name column row} {
    set column [ChkColumnIndex $name $column]
    set row    [ChkRowIndex    $name $row]

    upvar ::struct::matrix::matrix${name}::data data
    return $data($column,$row)
}

# ::struct::matrix::__get_column --
#
#	Returns a list containing the values from all cells in the
#	column identified by the index. The contents of the cell in
#	row 0 are stored as the first element of this list.
#
# Arguments:
#	name	Name of the matrix.
#	column	Column index of the addressed cell.
#
# Results:
#	List of values stored in the addressed row.

proc ::struct::matrix::__get_column {name column} {
    set column [ChkColumnIndex $name $column]

    upvar ::struct::matrix::matrix${name}::data data
    upvar ::struct::matrix::matrix${name}::rows rows

    set result [list]
    for {set r 0} {$r < $rows} {incr r} {
	lappend result $data($column,$r)
    }
    return $result
}

# ::struct::matrix::__get_rect --
#
#	Returns a list of lists of cell values. The values stored in
#	the result come from the submatrix whose top-left and
#	bottom-right cells are specified by "column_tl", "row_tl" and
#	"column_br", "row_br" resp. Note that the following equations
#	have to be true: column_tl <= column_br and row_tl <= row_br.
#	The result is organized as follows: The outer list is the list
#	of rows, its elements are lists representing a single row. The
#	row with the smallest index is the first element of the outer
#	list. The elements of the row lists represent the selected
#	cell values. The cell with the smallest index is the first
#	element in each row list.
#
# Arguments:
#	name		Name of the matrix.
#	column_tl	Column index of the top-left cell of the area.
#	row_tl		Row index of the top-left cell of the the area
#	column_br	Column index of the bottom-right cell of the area.
#	row_br		Row index of the bottom-right cell of the the area
#
# Results:
#	List of a list of values stored in the addressed area.

proc ::struct::matrix::__get_rect {name column_tl row_tl column_br row_br} {
    set column_tl [ChkColumnIndex $name $column_tl]
    set row_tl    [ChkRowIndex    $name $row_tl]
    set column_br [ChkColumnIndex $name $column_br]
    set row_br    [ChkRowIndex    $name $row_br]

    if {
	($column_tl > $column_br) ||
	($row_tl    > $row_br)
    } {
	return -code error "Invalid cell indices, wrong ordering"
    }

    upvar ::struct::matrix::matrix${name}::data data
    set result [list]

    for {set r $row_tl} {$r <= $row_br} {incr r} {
	set row [list]
	for {set c $column_tl} {$c <= $column_br} {incr c} {
	    lappend row $data($c,$r)
	}
	lappend result $row
    }

    return $result
}

# ::struct::matrix::__get_row --
#
#	Returns a list containing the values from all cells in the
#	row identified by the index. The contents of the cell in
#	column 0 are stored as the first element of this list.
#
# Arguments:
#	name	Name of the matrix.
#	row	Row index of the addressed cell.
#
# Results:
#	List of values stored in the addressed row.

proc ::struct::matrix::__get_row {name row} {
    set row [ChkRowIndex $name $row]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols

    set result [list]
    for {set c 0} {$c < $cols} {incr c} {
	lappend result $data($c,$row)
    }
    return $result
}

# ::struct::matrix::__insert_column --
#
#	Extends the matrix by one column and then acts like
#	"setcolumn" (see below) on this new column if there were
#	"values" supplied. Without "values" the new cells will be set
#	to the empty string. The new column is inserted just before
#	the column specified by the given index. This means, if
#	"column" is less than or equal to zero, then the new column is
#	inserted at the beginning of the matrix, before the first
#	column. If "column" has the value "Bend", or if it is greater
#	than or equal to the number of columns in the matrix, then the
#	new column is appended to the matrix, behind the last
#	column. The old column at the chosen index and all columns
#	with higher indices are shifted one index upward.
#
# Arguments:
#	name	Name of the matrix.
#	column	Index of the column where to insert.
#	values	Optional values to set the cells to.
#
# Results:
#	None.

proc ::struct::matrix::__insert_column {name column {values {}}} {
    # Allow both negative and too big indices.
    set column [ChkColumnIndexAll $name $column]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::rowh    rh

    if {$column > $cols} {
	# Same as 'addcolumn'
	__add_column $name $values
	return
    }

    set firstcol $column
    if {$firstcol < 0} {
	set firstcol 0
    }

    if {[set l [llength $values]] < $rows} {
	# Missing values. Fill up with empty strings

	for {} {$l < $rows} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $rows} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$rows - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:
    # Invalidate all rows, move all columns

    # Move all data from the higher columns one up and then insert the
    # new data into the freed space. Move the data in the
    # width cache too, take partial fill into account there too.
    # Invalidate the height cache for all rows.

    for {set r 0} {$r < $rows} {incr r} {
	for {set cn $cols ; set c [expr {$cn - 1}]} {$c >= $firstcol} {incr c -1 ; incr cn -1} {
	    set data($cn,$r) $data($c,$r)
	    if {[info exists cw($c)]} {
		set cw($cn) $cw($c)
		unset cw($c)
	    }
	}
	set data($firstcol,$r) [lindex $values $r]
	catch {unset rh($r)}
    }
    incr cols
    return
}

# ::struct::matrix::__insert_row --
#
#	Extends the matrix by one row and then acts like "setrow" (see
#	below) on this new row if there were "values"
#	supplied. Without "values" the new cells will be set to the
#	empty string. The new row is inserted just before the row
#	specified by the given index. This means, if "row" is less
#	than or equal to zero, then the new row is inserted at the
#	beginning of the matrix, before the first row. If "row" has
#	the value "end", or if it is greater than or equal to the
#	number of rows in the matrix, then the new row is appended to
#	the matrix, behind the last row. The old row at that index and
#	all rows with higher indices are shifted one index upward.
#
# Arguments:
#	name	Name of the matrix.
#	row	Index of the row where to insert.
#	values	Optional values to set the cells to.
#
# Results:
#	None.

proc ::struct::matrix::__insert_row {name row {values {}}} {
    # Allow both negative and too big indices.
    set row [ChkRowIndexAll $name $row]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::rowh    rh

    if {$row > $rows} {
	# Same as 'addrow'
	__add_row $name $values
	return
    }

    set firstrow $row
    if {$firstrow < 0} {
	set firstrow 0
    }

    if {[set l [llength $values]] < $cols} {
	# Missing values. Fill up with empty strings

	for {} {$l < $cols} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $cols} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$cols - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:
    # Invalidate all columns, move all rows

    # Move all data from the higher rows one up and then insert the
    # new data into the freed space. Move the data in the
    # height cache too, take partial fill into account there too.
    # Invalidate the width cache for all columns.

    for {set c 0} {$c < $cols} {incr c} {
	for {set rn $rows ; set r [expr {$rn - 1}]} {$r >= $firstrow} {incr r -1 ; incr rn -1} {
	    set data($c,$rn) $data($c,$r)
	    if {[info exists rh($r)]} {
		set rh($rn) $rh($r)
		unset rh($r)
	    }
	}
	set data($c,$firstrow) [lindex $values $c]
	catch {unset cw($c)}
    }
    incr rows
    return
}

# ::struct::matrix::_link --
#
#	Links the matrix to the specified array variable. This means
#	that the contents of all cells in the matrix is stored in the
#	array too, with all changes to the matrix propagated there
#	too. The contents of the cell "(column,row)" is stored in the
#	array using the key "column,row". If the option "-transpose"
#	is specified the key "row,column" will be used instead. It is
#	possible to link the matrix to more than one array. Note that
#	the link is bidirectional, i.e. changes to the array are
#	mirrored in the matrix too.
#
# Arguments:
#	name	Name of the matrix object.
#	option	Either empty of '-transpose'.
#	avar	Name of the variable to link to
#
# Results:
#	None

proc ::struct::matrix::_link {name args} {
    switch -exact -- [llength $args] {
	0 {
	    return -code error "wrong # args: link ?-transpose? arrayvariable"
	}
	1 {
	    set transpose 0
	    set variable  [lindex $args 0]
	}
	2 {
	    foreach {t variable} $args break
	    if {[string compare $t -transpose]} {
		return -code error "$name: illegal syntax: link ?-transpose? arrayvariable"
	    }
	    set transpose 1
	}
	default {
	    return -code error "$name: wrong # args: link ?-transpose? arrayvariable"
	}
    }

    upvar ::struct::matrix::matrix${name}::link link

    if {[info exists link($variable)]} {
	return -code error "$name link: Variable \"$variable\" already linked to matrix"
    }

    # Ok, a new variable we are linked to. Record this information,
    # dump our current contents into the array, at last generate the
    # traces actually performing the link.

    set link($variable) $transpose

    upvar #0 $variable array
    upvar ::struct::matrix::matrix${name}::data data

    foreach key [array names data] {
	foreach {c r} [split $key ,] break
	if {$transpose} {
	    set array($r,$c) $data($key)
	} else {
	    set array($c,$r) $data($key)
	}
    }

    trace variable array wu [list ::struct::matrix::MatTraceIn  $variable $name]
    trace variable date  w  [list ::struct::matrix::MatTraceOut $variable $name]
    return
}

# ::struct::matrix::_rowheight --
#
#	Returns the height of the specified row in lines. This is the
#	highest number of lines spanned by a cell over all cells in
#	the row.
#
# Arguments:
#	name	Name of the matrix
#	row	Index of the row queried for its height
#
# Results:
#	The height of the specified row in lines.

proc ::struct::matrix::_rowheight {name row} {
    set row [ChkRowIndex $name $row]

    upvar ::struct::matrix::matrix${name}::rowh rh

    if {![info exists rh($row)]} {
	upvar ::struct::matrix::matrix${name}::columns cols
	upvar ::struct::matrix::matrix${name}::data data

	set height 1
	for {set c 0} {$c < $cols} {incr c} {
	    set cheight [llength [split $data($c,$row) \n]]
	    if {$cheight > $height} {
		set height $cheight
	    }
	}

	set rh($row) $height
    }

    return $rh($row)
}

# ::struct::matrix::_rows --
#
#	Returns the number of rows currently managed by the matrix.
#
# Arguments:
#	name	Name of the matrix object.
#
# Results:
#	The number of rows in the matrix.

proc ::struct::matrix::_rows {name} {
    upvar ::struct::matrix::matrix${name}::rows rows
    return $rows
}

# ::struct::matrix::__set_cell --
#
#	Sets the value in the cell identified by row and column index
#	to the data in the third argument.
#
# Arguments:
#	name	Name of the matrix object.
#	column	Column index of the cell to set.
#	row	Row index of the cell to set.
#	value	THe new value of the cell.
#
# Results:
#	None.
 
proc ::struct::matrix::__set_cell {name column row value} {
    set column [ChkColumnIndex $name $column]
    set row    [ChkRowIndex    $name $row]

    upvar ::struct::matrix::matrix${name}::data data

    if {![string compare $value $data($column,$row)]} {
	# No change, ignore call!
	return
    }

    set data($column,$row) $value

    if {$value != {}} {
	upvar ::struct::matrix::matrix${name}::colw colw
	upvar ::struct::matrix::matrix${name}::rowh rowh
	catch {unset colw($column)}
	catch {unset rowh($row)}
    }
    return
}

# ::struct::matrix::__set_column --
#
#	Sets the values in the cells identified by the column index to
#	the elements of the list provided as the third argument. Each
#	element of the list is assigned to one cell, with the first
#	element going into the cell in row 0 and then upward. If there
#	are less values in the list than there are rows the remaining
#	rows are set to the empty string. If there are more values in
#	the list than there are rows the superfluous elements are
#	ignored. The matrix is not extended by this operation.
#
# Arguments:
#	name	Name of the matrix.
#	column	Index of the column to set.
#	values	Values to set into the column.
#
# Results:
#	None.

proc ::struct::matrix::__set_column {name column values} {
    set column [ChkColumnIndex $name $column]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::rowh    rh
    upvar ::struct::matrix::matrix${name}::colw    cw

    if {[set l [llength $values]] < $rows} {
	# Missing values. Fill up with empty strings

	for {} {$l < $rows} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $rows} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$rows - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:

    # - Invalidate the column in the width cache.
    # - The rows are either removed from the height cache or left
    #   unchanged, depending on the contents set into the cell.

    set r 0
    foreach v $values {
	if {$v != {}} {
	    # Data changed unpredictably, invalidate cache
	    catch {unset rh($r)}
	} ; # {else leave the row unchanged}
	set data($column,$r) $v
	incr r
    }
    catch {unset cw($column)}
    return
}

# ::struct::matrix::__set_rect --
#
#	Takes a list of lists of cell values and writes them into the
#	submatrix whose top-left cell is specified by the two
#	indices. If the sublists of the outerlist are not of equal
#	length the shorter sublists will be filled with empty strings
#	to the length of the longest sublist. If the submatrix
#	specified by the top-left cell and the number of rows and
#	columns in the "values" extends beyond the matrix we are
#	modifying the over-extending parts of the values are ignored,
#	i.e. essentially cut off. This subcommand expects its input in
#	the format as returned by "getrect".
#
# Arguments:
#	name	Name of the matrix object.
#	column	Column index of the topleft cell to set.
#	row	Row index of the topleft cell to set.
#	values	Values to set.
#
# Results:
#	None.

proc ::struct::matrix::__set_rect {name column row values} {
    # Allow negative indices!
    set column [ChkColumnIndexNeg $name $column]
    set row    [ChkRowIndexNeg    $name $row]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::colw    colw
    upvar ::struct::matrix::matrix${name}::rowh    rowh

    if {$row < 0} {
	# Remove rows from the head of values to restrict it to the
	# overlapping area.

	set values [lrange $values [expr {0 - $row}] end]
	set row 0
    }

    # Restrict it at the end too.
    if {($row + [llength $values]) > $rows} {
	set values [lrange $values 0 [expr {$rows - $row - 1}]]
    }

    # Same for columns, but store it in some vars as this is required
    # in a loop.
    set firstcol 0
    if {$column < 0} {
	set firstcol [expr {0 - $column}]
	set column 0
    }

    # Now pan through values and area and copy the external data into
    # the matrix.

    set r $row
    foreach line $values {
	set line [lrange $line $firstcol end]

	set l [expr {$column + [llength $line]}]
	if {$l > $cols} {
	    set line [lrange $line 0 [expr {$cols - $column - 1}]]
	} elseif {$l < [expr {$cols - $firstcol}]} {
	    # We have to take the offeset into the line intou account
	    # or we add fillers we don't need, overwriting part of the
	    # data array we shouldn't.

	    for {} {$l < [expr {$cols - $firstcol}]} {incr l} {
		lappend line {}
	    }
	}

	set c $column
	foreach cell $line {
	    if {$cell != {}} {
		catch {unset rh($r)}
		catch {unset cw($c)}
	    }
	    set data($c,$r) $cell
	    incr c
	}
	incr r
    }
    return
}

# ::struct::matrix::__set_row --
#
#	Sets the values in the cells identified by the row index to
#	the elements of the list provided as the third argument. Each
#	element of the list is assigned to one cell, with the first
#	element going into the cell in column 0 and then upward. If
#	there are less values in the list than there are columns the
#	remaining columns are set to the empty string. If there are
#	more values in the list than there are columns the superfluous
#	elements are ignored. The matrix is not extended by this
#	operation.
#
# Arguments:
#	name	Name of the matrix.
#	row	Index of the row to set.
#	values	Values to set into the row.
#
# Results:
#	None.

proc ::struct::matrix::__set_row {name row values} {
    set row [ChkRowIndex $name $row]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rows    rows
    upvar ::struct::matrix::matrix${name}::colw    cw
    upvar ::struct::matrix::matrix${name}::rowh    rh

    if {[set l [llength $values]] < $cols} {
	# Missing values. Fill up with empty strings

	for {} {$l < $cols} {incr l} {
	    lappend values {}
	}
    } elseif {[llength $values] > $cols} {
	# To many values. Remove the superfluous items
	set values [lrange $values 0 [expr {$cols - 1}]]
    }

    # "values" now contains the information to set into the array.
    # Regarding the width and height caches:

    # - Invalidate the row in the height cache.
    # - The columns are either removed from the width cache or left
    #   unchanged, depending on the contents set into the cell.

    set c 0
    foreach v $values {
	if {$v != {}} {
	    # Data changed unpredictably, invalidate cache
	    catch {unset cw($c)}
	} ; # {else leave the row unchanged}
	set data($c,$row) $v
	incr c
    }
    catch {unset rh($row)}
    return
}

# ::struct::matrix::__swap_columns --
#
#	Swaps the contents of the two specified columns.
#
# Arguments:
#	name		Name of the matrix.
#	column_a	Index of the first column to swap
#	column_b	Index of the second column to swap
#
# Results:
#	None.

proc ::struct::matrix::__swap_columns {name column_a column_b} {
    set column_a [ChkColumnIndex $name $column_a]
    set column_b [ChkColumnIndex $name $column_b]

    upvar ::struct::matrix::matrix${name}::data data
    upvar ::struct::matrix::matrix${name}::rows rows
    upvar ::struct::matrix::matrix${name}::colw colw

    # Note: This operation does not influence the height cache for all
    # rows and the width cache only insofar as its contents has to be
    # swapped too for the two columns we are touching. Note that the
    # cache might be partially filled or not at all, so we don't have
    # to "swap" in some situations.

    for {set r 0} {$r < $rows} {incr r} {
	set tmp                $data($column_a,$r)
	set data($column_a,$r) $data($column_b,$r)
	set data($column_b,$r) $tmp
    }

    set cwa [info exists colw($column_a)]
    set cwb [info exists colw($column_b)]

    if {$cwa && $cwb} {
	set tmp             $colw($column_a)
	set colw($column_a) $colw($column_b)
	set colw($column_b) $tmp
    } elseif {$cwa} {
	# Move contents, don't swap.
	set   colw($column_b) $colw($column_a)
	unset colw($column_a)
    } elseif {$cwb} {
	# Move contents, don't swap.
	set   colw($column_a) $colw($column_b)
	unset colw($column_b)
    } ; # else {nothing to do at all}
    return
}

# ::struct::matrix::__swap_rows --
#
#	Swaps the contents of the two specified rows.
#
# Arguments:
#	name	Name of the matrix.
#	row_a	Index of the first row to swap
#	row_b	Index of the second row to swap
#
# Results:
#	None.

proc ::struct::matrix::__swap_rows {name row_a row_b} {
    set row_a [ChkRowIndex $name $row_a]
    set row_b [ChkRowIndex $name $row_b]

    upvar ::struct::matrix::matrix${name}::data    data
    upvar ::struct::matrix::matrix${name}::columns cols
    upvar ::struct::matrix::matrix${name}::rowh    rowh

    # Note: This operation does not influence the width cache for all
    # columns and the height cache only insofar as its contents has to be
    # swapped too for the two rows we are touching. Note that the
    # cache might be partially filled or not at all, so we don't have
    # to "swap" in some situations.

    for {set c 0} {$c < $cols} {incr c} {
	set tmp             $data($c,$row_a)
	set data($c,$row_a) $data($c,$row_b)
	set data($c,$row_b) $tmp
    }

    set rha [info exists rowh($row_a)]
    set rhb [info exists rowh($row_b)]

    if {$rha && $rhb} {
	set tmp          $rowh($row_a)
	set rowh($row_a) $rowh($row_b)
	set rowh($row_b) $tmp
    } elseif {$rha} {
	# Move contents, don't swap.
	set   rowh($row_b) $rowh($row_a)
	unset rowh($row_a)
    } elseif {$rhb} {
	# Move contents, don't swap.
	set   rowh($row_a) $rowh($row_b)
	unset rowh($row_b)
    } ; # else {nothing to do at all}
    return
}

# ::struct::matrix::_unlink --
#
#	Removes the link between the matrix and the specified
#	arrayvariable, if there is one.
#
# Arguments:
#	name	Name of the matrix.
#	avar	Name of the linked array.
#
# Results:
#	None.

proc ::struct::matrix::_unlink {name avar} {

    upvar ::struct::matrix::matrix${name}::link link

    if {![info exists link($avar)]} {
	# Ignore unlinking of unkown variables.
	return
    }

    # Delete the traces first, then remove the link management
    # information from the object.

    upvar #0 $avar array
    upvar ::struct::matrix::matrix${name}::data data

    trace vdelete array wu [list ::struct::matrix::MatTraceIn  $avar $name]
    trace vdelete date  w  [list ::struct::matrix::MatTraceOut $avar $name]

    unset link($avar)
    return
}

# ::struct::matrix::ChkColumnIndex --
#
#	Helper to check and transform column indices. Returns the
#	absolute index number belonging to the specified
#	index. Rejects indices out of the valid range of columns.
#
# Arguments:
#	matrix	Matrix to look at
#	column	The incoming index to check and transform
#
# Results:
#	The absolute index to the column

proc ::struct::matrix::ChkColumnIndex {name column} {
    upvar ::struct::matrix::matrix${name}::columns c

    switch -regex -- $column {
	{end-[0-9]+} {
	    regsub -- {end-} $column {} column
	    set cc [expr {$c - 1 - $column}]
	    if {($cc < 0) || ($cc >= $c)} {
		return -code error "bad column index end-$column, column does not exist"
	    }
	    return $cc
	}
	end {
	    if {$c <= 0} {
		return -code error "bad column index $column, column does not exist"
	    }
	    return [expr {$c - 1}]
	}
	{[0-9]+} {
	    if {($column < 0) || ($column >= $c)} {
		return -code error "bad column index $column, column does not exist"
	    }
	    return $column
	}
	default {
	    return -code error "bad column index \"$column\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::ChkRowIndex --
#
#	Helper to check and transform row indices. Returns the
#	absolute index number belonging to the specified
#	index. Rejects indices out of the valid range of rows.
#
# Arguments:
#	matrix	Matrix to look at
#	row	The incoming index to check and transform
#
# Results:
#	The absolute index to the row

proc ::struct::matrix::ChkRowIndex {name row} {
    upvar ::struct::matrix::matrix${name}::rows r

    switch -regex -- $row {
	{end-[0-9]+} {
	    regsub -- {end-} $row {} row
	    set rr [expr {$r - 1 - $row}]
	    if {($rr < 0) || ($rr >= $r)} {
		return -code error "bad row index end-$row, row does not exist"
	    }
	    return $rr
	}
	end {
	    if {$r <= 0} {
		return -code error "bad row index $row, row does not exist"
	    }
	    return [expr {$r - 1}]
	}
	{[0-9]+} {
	    if {($row < 0) || ($row >= $r)} {
		return -code error "bad row index $row, row does not exist"
	    }
	    return $row
	}
	default {
	    return -code error "bad row index \"$row\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::ChkColumnIndexNeg --
#
#	Helper to check and transform column indices. Returns the
#	absolute index number belonging to the specified
#	index. Rejects indices out of the valid range of columns
#	(Accepts negative indices).
#
# Arguments:
#	matrix	Matrix to look at
#	column	The incoming index to check and transform
#
# Results:
#	The absolute index to the column

proc ::struct::matrix::ChkColumnIndexNeg {name column} {
    upvar ::struct::matrix::matrix${name}::columns c

    switch -regex -- $column {
	{end-[0-9]+} {
	    regsub -- {end-} $column {} column
	    set cc [expr {$c - 1 - $column}]
	    if {$cc >= $c} {
		return -code error "bad column index end-$column, column does not exist"
	    }
	    return $cc
	}
	end {
	    return [expr {$c - 1}]
	}
	{[0-9]+} {
	    if {$column >= $c} {
		return -code error "bad column index $column, column does not exist"
	    }
	    return $column
	}
	default {
	    return -code error "bad column index \"$column\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::ChkRowIndexNeg --
#
#	Helper to check and transform row indices. Returns the
#	absolute index number belonging to the specified
#	index. Rejects indices out of the valid range of rows
#	(Accepts negative indices).
#
# Arguments:
#	matrix	Matrix to look at
#	row	The incoming index to check and transform
#
# Results:
#	The absolute index to the row

proc ::struct::matrix::ChkRowIndexNeg {name row} {
    upvar ::struct::matrix::matrix${name}::rows r

    switch -regex -- $row {
	{end-[0-9]+} {
	    regsub -- {end-} $row {} row
	    set rr [expr {$r - 1 - $row}]
	    if {$rr >= $r} {
		return -code error "bad row index end-$row, row does not exist"
	    }
	    return $rr
	}
	end {
	    return [expr {$r - 1}]
	}
	{[0-9]+} {
	    if {$row >= $r} {
		return -code error "bad row index $row, row does not exist"
	    }
	    return $row
	}
	default {
	    return -code error "bad row index \"$row\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::ChkColumnIndexAll --
#
#	Helper to transform column indices. Returns the
#	absolute index number belonging to the specified
#	index.
#
# Arguments:
#	matrix	Matrix to look at
#	column	The incoming index to check and transform
#
# Results:
#	The absolute index to the column

proc ::struct::matrix::ChkColumnIndexAll {name column} {
    upvar ::struct::matrix::matrix${name}::columns c

    switch -regex -- $column {
	{end-[0-9]+} {
	    regsub -- {end-} $column {} column
	    set cc [expr {$c - 1 - $column}]
	    return $cc
	}
	end {
	    return $c
	}
	{[0-9]+} {
	    return $column
	}
	default {
	    return -code error "bad column index \"$column\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::ChkRowIndexAll --
#
#	Helper to transform row indices. Returns the
#	absolute index number belonging to the specified
#	index.
#
# Arguments:
#	matrix	Matrix to look at
#	row	The incoming index to check and transform
#
# Results:
#	The absolute index to the row

proc ::struct::matrix::ChkRowIndexAll {name row} {
    upvar ::struct::matrix::matrix${name}::rows r

    switch -regex -- $row {
	{end-[0-9]+} {
	    regsub -- {end-} $row {} row
	    set rr [expr {$r - 1 - $row}]
	    return $rr
	}
	end {
	    return $r
	}
	{[0-9]+} {
	    return $row
	}
	default {
	    return -code error "bad row index \"$row\", syntax error"
	}
    }
    # Will not come to this place
}

# ::struct::matrix::MatTraceIn --
#
#	Helper propagating changes made to an array
#	into the matrix the array is linked to.
#
# Arguments:
#	avar		Name of the array which was changed.
#	name		Matrix to write the changes to.
#	var,idx,op	Standard trace arguments
#
# Results:
#	None.

proc ::struct::matrix::MatTraceIn {avar name var idx op} {
    # Propagate changes in the linked array back into the matrix.

    if {![string compare $op u]} {
	# External array was destroyed, perform automatic unlink.
	$name unlink $avar
	return
    }

    upvar #0 $avar                              array
    upvar ::struct::matrix::matrix${name}::data data
    upvar ::struct::matrix::matrix${name}::link link

    set transpose $link($avar)
    if {$transpose} {
	foreach {r c} [split $idx ,] break
    } else {
	foreach {c r} [split $idx ,] break
    }

    # Use standard method to propagate the change.
    # => Get automatically index checks, cache updates, ...

    $name set cell $c $r $array($idx)
    return
}

# ::struct::matrix::MatTraceOut --
#
#	Helper propagating changes made to the matrix into the linked arrays.
#
# Arguments:
#	avar		Name of the array to write the changes to.
#	name		Matrix which was changed.
#	var,idx,op	Standard trace arguments
#
# Results:
#	None.

proc ::struct::matrix::MatTraceOut {avar name var idx op} {
    # Propagate changes in the matrix data array into the linked array.

    upvar #0 $avar                              array
    upvar ::struct::matrix::matrix${name}::data data
    upvar ::struct::matrix::matrix${name}::link link

    set transpose $link($avar)
    if {$transpose} {
	foreach {r c} [split $idx ,] break
    } else {
	foreach {c r} [split $idx ,] break
    }

    set array($c,$r) $data($idx)
    return
}
