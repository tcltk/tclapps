DTP, the DocTools Processor
===========================

Introduction
------------

The DocTools Processor is an easy to use application adding a nice
interface around the tcllib package handling doctools formatted
texts. It makes it easy to process a large set of manpages in one go,
customizing the output, and more.

Note that the 'doctools' in the name of the application refers to

-   a tcl-based format for writing manpages
-   a module in the Tcllib package of the same name
-   a package in the module above to handle text written in the format.


Dependencies
------------

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
