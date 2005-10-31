#! /bin/sh
#
#  tclunit
#
#  Tclunit is a simple GUI wrapper around the tcltest
#  unit test framework.  It will give you the "green bar"
#  that makes so many developers happy.
#
#  Synopsis:
#     tclunit [testFile | testDirectory]
#
#  Tclunit will execute a single test file, or run
#  all tests in a directory using tcltest's runAllTests
#  procedure.  Test output is captured, parsed, and
#  presented in the GUI.
#
#  Each file is listed in a tree view, with a green
#  check if all tests passed, or a red "x" if any
#  test failed.  Opening the file in the tree view
#  lists all tests individually.  Selecting any test
#  shows its output in the text view.
#
#  This program was developed with an early release
#  of the "tile" themable widget package.  It may
#  work with pre-0.6.5 releases, but it hasn't been
#  tested.  The latest ActiveTcl releases should
#  work just fine.
#
#  This program was created en-route to the Tcl/Tk 2005
#  conference, as a quick demo to share with the
#  attendees.  It isn't exactly robust.  It was not
#  employed with any particulary saavy development
#  strategies, like Model-View-Controller.  There are
#  a bunch of marginally documented global variables.
#  But worst of all, there isn't a test suite.
#
#  But with those apologies, please enjoy.
#
#
#  Bob Techentin
#  October 24, 2005
#
#-----------------------------------------------------------


# restart using wish \
exec wish "$0" "$@"


package require tile


#-----------------------------------------------------------
#  init_for_tests
#
#  Description:
#    Initializes variables and GUI for running test suite.
#
#  Arguments
#    none
#
#  Side Effects
#    Modified global env() array.
#    Sets initial values for capturing test output
#    Cleans out GUI display
#
#-----------------------------------------------------------
proc init_for_tests {} {
    # Run tests with verbose options
    #   so we can parse the output
    set ::env(TCLTEST_OPTIONS) "-verbose {body pass skip start error}"

    #  Initialize Capturing Test Output or "cto" array
    array unset ::cto
    array set ::cto {
	capturing 0
	filename ""
      passed 0
      skipped 0
      failed 0
      totalpassed 0
      totalskipped 0
      totalfailed 0
	testName ""
	result ""
      statusLine ""
    }

    #  Initialize results array
    array unset ::testResults

    #  Initialize GUI, basically by deleting contents
    $::widget(txt) delete 1.0 end
    $::widget(tv) delete [$::widget(tv) children {}]
    $::widget(ind) configure -background green -text "So Far So Good..."
    update idletasks
}

#-----------------------------------------------------------
#  run_all_tests
#
#  Description:
#    Creates a test script for running
#    all tests in the current working 
#    directory.  Then runs the tests.
#
#  Arguments:
#    none
#  Side Effects:
#    runs all the tests in the directory
#-----------------------------------------------------------
proc run_all_tests {} {
    init_for_tests
    set testScript { 
	package require tcltest
	tcltest::runAllTests
	exit
    }
    run_tests $testScript
}

#-----------------------------------------------------------
#  run_test_file
#
#  Description:
#    Creates a test script for running
#    a single test file, then runs the tests.
#
#  Arguments:
#    testfile  - file to be tested
#  Side effects:
#    runs the test file
#-----------------------------------------------------------
proc run_test_file {testfile} {
    init_for_tests
    set testScript { 
	source $testfile
	exit
    }
    set testScript [subst $testScript]
    test_file_start $testfile
    run_tests $testScript
}

#-----------------------------------------------------------
#  run_tests
#
#  Description:
#    Run tclsh and feed it the test script written by
#    run_all_tests or run_test_file.  Set up a fileevent
#    reader to parse the output.  Then wait for it
#    to finish.
#
#    Just for fun, capture clock time before and after
#    running the tests, so we can compute a test
#    velocity.
#
#  Arguments:
#    testScript   - script to feed to tclsh
#  Side Effects:
#    execs tclsh process
#    defines fileevent to parse the output
#    and hangs until the parser notifies us.
#-----------------------------------------------------------
proc run_tests { testScript } {

    #  Set timers
    set ::started_tests [clock clicks -milliseconds]
    set ::finished_tests 0

    #  Exec a tcl shell to run the scripts
    set ::pipe [open "|tclsh" w+]
    fconfigure $::pipe -blocking 0 -buffering line
    fileevent $::pipe readable [list capture_test_output $::pipe]

    #  Tell the shell what to do
    puts $::pipe $testScript

    #  And wait for it to finish
    vwait ::finished_tests

    #  check the time
    set ::finished_tests [clock clicks -milliseconds]

}

#-----------------------------------------------------------
#  capture_test_output
#
#  Description:
#    Parses the tcltest output stream, and decides
#    if a line represents a pass, fail, skip, or 
#    failure.  In case of failures, we expect more
#    lines of test case results, so we capture those
#    until a global flag is reset.
#
#    When running all tests, file names are printed
#    so if the line is a file name, we save that.
#
#  Arguments:
#    chan - stdout channel of the tcltest process
#  Side Effects:
#    none
#-----------------------------------------------------------
proc capture_test_output {chan} {

    if {[eof $chan]} {
	# notify [run_tests] that we've completed
	set ::finished_tests 1
	close $chan
	return
    }

    # Read the line
    gets $chan line

    #  If we're saving up test results...
    if { $::cto(capturing) } {
	test_failed_continue $line
    }

    #  Check for start, pass and fail lines
    switch -glob -- $line {
	"++++ * PASSED"     {test_passed $line}
	"==== * FAILED"     {test_failed $line}
	"---- * start"      {test_started $line}
      "++++ * SKIPPED: *" {test_skipped $line}
    }

    #  If the line is a file name
    #   then save it
    if { [file exists $line] } {
	test_file_start $line
    }

}

#-----------------------------------------------------------
#  test_skipped
#
#  Description:
#    Count the test as skipped.
#    Parse the test name from the line, and
#    add the test to the GUI.
#
#  Arguments:
#    line  - text of line captured from tcltest output
#  Side Effects:
#    changes the test results variables and the GUI
#-----------------------------------------------------------
proc test_skipped {line} {

    incr_test_counter "skipped"

    # update the GUI
    scan $line "%s %s" junk testName
    set id [$::widget(tv) insert $::cto(filename) end \
            -text $testName -image skippedIcon]
    update idletasks

    #  Save a text string for display
    set ::testResults($id) $line
}

#-----------------------------------------------------------
#  test_started
#
#  Description:
#    Parse the test name from the line, and
#    reset the capture test ouput (cto) variables.
#
#  Arguments:
#    line  - text of line captured from tcltest output
#  Side Effects:
#    changes the capture test output (cto) variables
#-----------------------------------------------------------
proc test_started {line} {
    scan $line "---- %s start" testName
    set ::cto(testName) $testName
    set ::cto(result) ""
    set ::cto(capturing) 0
}

#-----------------------------------------------------------
#  test_passed
#
#  Description:
#    Count the test as passed.
#    Add the test to the GUI, and save the results.
#
#  Arguments:
#    line  - text of line captured from tcltest output
#  Side Effects:
#    changes the capture test output (cto) variables
#-----------------------------------------------------------
proc test_passed {line} {

    incr_test_counter "passed"

    #  update the GUI
    set id [$::widget(tv) insert $::cto(filename) end \
		-text $::cto(testName) -image passedIcon]
    update idletasks

    #  Save a text string for display
    set ::testResults($id) PASSED
}

#-----------------------------------------------------------
#  test_failed
#
#  Description:
#    Count the test as failed.
#    Start capturing tcltest output until we get
#    the "FAILED" line.
#
#  Arguments:
#    line  - text of line captured from tcltest output
#  Side Effects:
#    changes the capture test output (cto) variables
#-----------------------------------------------------------
proc test_failed {line} {

    incr_test_counter "failed"

    #  Start capturing failure results
    set ::cto(capturing) 1
    set ::cto(result) $line\n
}

#-----------------------------------------------------------
#  test_failed_continue
#
#  Description:
#    Continue capturing failed test output, appending
#    it to this tests' results variable.  If we detect
#    the final line in the failed test output (with "FAILED")
#    then we stop the capture process and add this test
#    to the GUI.
#
#    Note that the "Result should have been..." line
#    seems to come with an attached newline, while every
#    other line requires a newline.  Not sure why this
#    special case is required to get test results that
#    look just like regular tcltest output on a console.
#
#  Arguments:
#    line  - text of line captured from tcltest output
#  Side Effects:
#    changes the capture test output (cto) variables
#    and updates the GUI.
#-----------------------------------------------------------
proc test_failed_continue {line} {
    append ::cto(result) "$line"
    if { ! [string match "*Result should have been*" $line] } {
	append ::cto(result) "\n"
    }

    #  Is this the last line in the failure log?
    if { $line eq "==== $::cto(testName) FAILED" } {
	set ::cto(capturing) 0

	#  Add the test to the gui
	set id [$::widget(tv) insert $::cto(filename) end \
		    -text $::cto(testName) -image failedIcon]
      $::widget(tv) item $::cto(filename) -image failedIcon
      $::widget(ind) configure -background red -text "TEST FAILURES"
	update idletasks

	#  Save a text string for display
	set ::testResults($id) $::cto(result)
    }
}

#-----------------------------------------------------------
#  test_file_start
#
#  Description:
#    Initializes the capture test output (cto) variables
#    for a new test file, and adds the file to the GUI.
#
#  Arguments:
#    filename - name of test file (that is about to be run)
#  Side Effects:
#    changes the capture test output (cto) variables
#    and the GUI
#-----------------------------------------------------------
proc test_file_start {filename} {

    #  Save the filename (which is the only thing on the line)
    lappend ::cto(filenames) $filename
    set ::cto(filename) $filename

    #  Initialize the counters
    set ::cto(passed) 0
    set ::cto(skipped) 0
    set ::cto(failed) 0

    #  Initialize test results, so users see something when
    #  the filename is selected in the treeview
    set ::testResults($filename) ""

    #  Add this filename to the GUI
    $::widget(tv) insert {} end -id $filename \
	-text $filename -open false -image passedIcon
    update idletasks
}


#-----------------------------------------------------------
#  incr_test_counter
#
#  Description:
#    Counts the test by incrementing the appropriate
#    counters in the capture test output (cto) variables.
#    Updates the GUI by formatting the totals for both
#    the main window status line and for the test file
#    results.
#
#  Arguments:
#     resultType - one of "skipped", "passed", or "failed"
#  Side Effects:
#     changes the capture test output (cto) variables
#     and updates the GUI
#-----------------------------------------------------------
proc incr_test_counter {resultType} {

    incr ::cto($resultType)
    incr ::cto(total$resultType)

    #  Update the summary line
    set total [expr {$::cto(passed) + $::cto(skipped) + $::cto(failed)}]
    set ::cto(statusLine) \
            [format "%-20s:  Total %-5d    Passed %-5d    Skipped %-5d    Failed %d-5" \
              $::cto(filename) $total $::cto(passed) $::cto(skipped) $::cto(failed)]

    #  Copy summary to this test file's results
    set ::testResults($::cto(filename)) $::cto(statusLine)
}


#-----------------------------------------------------------
#  build_gui
#
#  Description:
#    Builds the GUI main window using tile widgets.
#    This was constructed and tested with tile 0.6.5,
#    but may work with other versions.
#
#    Note that the big green/red pass/fail indicator
#    is a regular Tk label, which allows background
#    color changes.
#
#  Arguments:
#    none
#  Side Effects:
#    creates a simple GUI and defines a global
#    variable, ::widgets(), with a few names of
#    widgets that are used throughout the application.
#-----------------------------------------------------------

proc build_gui {} {


    #  Select a test file or "run all tests"
    #  in a specific directory
    set ff [ttk::frame .fileframe]
    set frad [ttk::radiobutton $ff.filecheck -text "Test File" \
                    -variable ::runAllTests -value 0]
    set fent [ttk::entry $ff.filentry -textvariable ::testFile]
    set fbut [ttk::button $ff.filebtn -text "Browse..." -command browseFile]

    set arad [ttk::radiobutton $ff.allcheck -text "Run All Tests" \
                    -variable ::runAllTests -value 1]
    set aent [ttk::entry $ff.allentry -textvariable ::testDirectory]
    set abut [ttk::button $ff.allbtn -text "Choose Dir..." -command browseDir]

    grid $frad $fent $fbut -sticky ew -padx 4 -pady 4
    grid $arad $aent $abut -sticky ew -padx 4 -pady 4
    grid columnconfigure $ff 1 -weight 1


    # Paned window
    set pw [ttk::paned .pw -orient horizontal]

    # tree view of tests run
    set tvf [ttk::frame $pw.tvf]
    set tv [ttk::treeview $tvf.tv -yscrollcommand [list $tvf.vsb set]]
    set sb [ttk::scrollbar $tvf.vsb -orient vertical \
		-command [list $tvf.tv yview]]

    grid $tv $sb -sticky news -padx 4 -pady 4
    grid columnconfigure $tvf 0 -weight 1
    grid    rowconfigure $tvf 0 -weight 1

    #  set treeview selection action
    bind $tv <<TreeviewSelect>> {gui_treeview_select}

    $pw add $tvf


    #  frame to hold "run" button and test results text
    set bf [ttk::frame $pw.bf]

    #  buttons to run/stop tests, color indicator, and text
    set run  [ttk::button $bf.run  -text "Run"  -command gui_run_tests]
    set stop [ttk::button $bf.stop -text "Stop" -command gui_stop_tests -state disabled]
    set ind [label $bf.indicator -text "" -background green]
    set txt [text $bf.text]

    grid $run  $ind   -sticky news -padx 4 -pady 4
    grid $stop   ^    -sticky news -padx 4 -pady 4
    grid $txt    -    -sticky news -padx 4 -pady 4
    grid columnconfigure $bf 1 -weight 1
    grid    rowconfigure $bf 2 -weight 1

    $pw add $bf

    #  add a status line
    set statline [ttk::label .statusLine -textvariable ::cto(statusLine)] 


    #  Assemble the main window parts
    grid $ff -sticky ew
    grid $pw -sticky news
    grid $statline -sticky w -padx 4 -pady 4

    grid columnconfigure . 0 -weight 1
    grid    rowconfigure . 1 -weight 1


    # save widget names
    set ::widget(run)  $run
    set ::widget(stop) $stop
    set ::widget(tv)   $tv
    set ::widget(txt)  $txt
    set ::widget(ind)  $ind


    #  Define icons for the tree view

    #  actcheck16 from crystal icons
    image create photo passedIcon -data {
       R0lGODlhEAAQAIIAAPwCBMT+xATCBASCBARCBAQCBEQCBAAAACH5BAEAAAAA
       LAAAAAAQABAAAAM2CLrc/itAF8RkdVyVye4FpzUgJwijORCGUhDDOZbLG6Nd
      2xjwibIQ2y80sRGIl4IBuWk6Af4EACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYg
       UHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJp
       Z2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }

    #  actcross16 from crystal icons
    image create photo failedIcon -data {
       R0lGODlhEAAQAIIAAASC/PwCBMQCBEQCBIQCBAAAAAAAAAAAACH5BAEAAAAA
       LAAAAAAQABAAAAMuCLrc/hCGFyYLQjQsquLDQ2ScEEJjZkYfyQKlJa2j7AQn
       MM7NfucLze1FLD78CQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJz
       aW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVz
       ZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }

    #  services-16 from crystal icons
    image create photo skippedIcon -data {
       R0lGODlhEAAQAIUAAPwCBPy2BPSqBPSeBPS6HPSyDPSmBPzSXPzGNOyOBPSy
       FPzujPzaPOyWBPy+DPyyDPy+LPzKTPzmZPzeTPSaFOSGBNxuBNxmBPzWVPzq
       dPzmXPzePPzaRPzeRPS2FNxqBPzWNOyeBPTCLPzOJNxyBPzGHNReBOR6BOR2
       BNRmBMxOBMxWBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZz
       QIBwSCwahYHA8SgYLIuEguFJFBwQCSpAMVgwEo2iI/mAECKSCYFCqVguF0BA
       gFlkNBsGZ9PxXD5EAwQTEwwMICAhcUYJInkgIyEWSwMRh5ACJEsVE5EhJQQm
       S40nKCQWAilHCSelQhcqsUatRisqWkt+QQAh/mhDcmVhdGVkIGJ5IEJNUFRv
       R0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFs
       bCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
}

#-----------------------------------------------------------
#  gui_run_tests
#
#  Description:
#    Called by the "Run" button, this proc decides
#    whether to call run_all_tests or run_test_file,
#    then makes a nice summary of the tests and updates
#    the GUI.
#
#  Arguments:
#    none
#  Side Effects
#    Pretty much everything happens.
#-----------------------------------------------------------
proc gui_run_tests {} {

    #  enable/disable the buttons
    $::widget(run)  configure -state disabled
    $::widget(stop) configure -state normal

    #  run the tests
    if { $::runAllTests } {
      cd $::testDirectory
	run_all_tests
    } else {
      if { ! [file exists $::testFile] } {
          browseFile
      }
      cd [file dirname $::testFile]
	run_test_file $::testFile
    }

    #  look at final statistics
    set passed $::cto(totalpassed)
    set skipped $::cto(totalskipped)
    set failed $::cto(totalfailed)
    set total [expr {$passed + $skipped + $failed}]


    # Computing timing statistic
    set time_in_ms [expr {$::finished_tests - $::started_tests}]
    set velocity [expr {1000.0 * $total / $time_in_ms}]

    # update GUI indicator
    set ::cto(statusLine) \
        [format "Total %-5d Passed %-5d Skipped %-5d Failed %-5d    (%.1f tests/second)" \
                        $total $passed $skipped $failed $velocity]
    $::widget(ind) configure -text $::cto(statusLine)

    #  enable/disable the buttons
    $::widget(run)  configure -state normal
    $::widget(stop) configure -state disabled
}

#-----------------------------------------------------------
#  gui_stop_tests
#
#  Description:
#    Called by the "Stop" button, this procedure terminates
#    the running test process by closing the pipe and 
#    setting the global flag.  It also changes the enabled
#    states of the buttons.
#
#  Arguments:
#    none
#  Side Effects
#    Pretty much everything stops.
#-----------------------------------------------------------
proc gui_stop_tests {} {
    close $::pipe
    set ::finished_tests 1
}


#-----------------------------------------------------------
#  gui_treeview_select
#
#  Description:
#    Called by the <<TreeviewSelect>> event binding,
#    this procedure figures out the $id of the
#    selected entry and copies the proper text results
#    into the text widget.
#
#  Arguments:
#    none
#  Side Effects:
#    Changes text widget contents
#-----------------------------------------------------------
proc gui_treeview_select {} {

    # get selection from treeview
    set id [$::widget(tv) selection]

    # display text
    set txt $::widget(txt)
    $txt delete 1.0 end
    $txt insert end $::testResults($id)
}

#-----------------------------------------------------------
#  browseFile
#
#  Description:
#    Called by the file "Browse..." button, 
#    this procedure opens tk_getOpenFile
#    and save the selected test file name to
#    a global variable.
#
#  Arguments:
#    none
#  Side Effects
#    Sets testFile and runAllTests global variables
#-----------------------------------------------------------
proc browseFile {} {
    set dirname [file dirname $::testFile]
    if { $dirname eq "" } {
     
   set dirname [pwd]
    }
    set filetypes {
        {{Test Files} {.test .tcl}} 
        {{All Files}   *          }
    }

    set filename [tk_getOpenFile -initialdir $dirname -filetypes $filetypes]
    if { $filename ne "" } {
        cd [file dirname $filename]
        set ::testFile [file tail $filename]
        set ::runAllTests 0
    }
}

#-----------------------------------------------------------
#  browseDir
#
#  Description:
#    Called by the directory "Select Dir..." button,
#    this procedure opens tk_chooseDirectory to
#    select a new directory for running all tests.
#
#  Arguments:
#    none
#  Side Effects:
#    Sets the global variables testDirectory and runAllTests
#-----------------------------------------------------------
proc browseDir {} {
    set dirname [tk_chooseDirectory -initialdir $::testDirectory]

    if { $dirname ne "" } {
        set ::testDirectory $dirname
        set ::runAllTests 1
    }
}

#-----------------------------------------------------------
#  main
#
#  Description:
#    Main program, parses command line arguments to
#    figure out if the user specified either a directory
#    or a test file.  It then builds the gui.
#
#  Arguments:
#    args - command line arguments (argv) the first of
#           which might be a file name
#  Side Effects:
#    runs the program
#-----------------------------------------------------------
proc main {args} {

    #  process command line arguments
    set ::testFile ""
    set ::testDirectory [pwd]
    set ::runAllTests 1
    if { [llength $args] > 0 } {
	if { [file exists [lindex $args 0]] } {
	    set filename [lindex $args 0]

          if { ! [file isdirectory $filename] } {
              set ::runAllTests 0
              set ::testFile $filename
          } else {
              set ::testDirectory $filename
          }
	}
    }

    build_gui

}

main $argv

