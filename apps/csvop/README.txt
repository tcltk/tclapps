CSVop, a tool to manipulate CSV files
=====================================

Introduction
------------

CSVop is an easy to use application adding a nice interface around the
tcllib package handling csv files.


Dependencies
------------

The code is setup to use the 'starkit' package (*) to allow for easy
deployment as *kit/pack. This means that "csvop" depends on Tcl 8.4
and C code (starkit, tclvfs, metakit).

For proper operation we need the csv, matrix, and report modules of
tcllib. The embedded help requires the doctools and textutil packages.

Command line, help
------------------

	csvop help

		Simple help about the help sytem

	csvop help cmdline <format>

		csvop commandline help in a supported format, written to stdout.

		/Internal/ The generic framework asks each application
		package for a fragment of .man, assembles this into
		one page and converts that. Tries to cache the result
		(no caching if starkit is not writeable).
