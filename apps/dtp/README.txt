DTP, the DocTools Processor
===========================

The code is setup to use the 'starkit' package (*) to allow for easy
deployment as *kit/pack. This means that the dtp depends on Tcl 8.4
and C code (starkit, tclvfs, metakit).

For proper operation we need the textutil and doctools modules of
tcllib.

Command line, help
------------------

	dtp help

		Simple help about the help sytem

	dtp help cmdline <format>

		dtp commandline help in a supported format, written to stdout.

		/Internal/ The generic framework asks each application
		package for a fragment of .man, assembles this into
		one page and converts that. Tries to cache the result
		(no caching if starkit is not writeable).
