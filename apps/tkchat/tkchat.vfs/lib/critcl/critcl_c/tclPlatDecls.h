/*
 * tclPlatDecls.h --
 *
 *	Declarations of platform specific Tcl APIs.
 *
 * Copyright (c) 1998-1999 by Scriptics Corporation.
 * All rights reserved.
 *
 * RCS: @(#) $Id: tclPlatDecls.h 1325 2007-03-29 07:27:33Z jcw $
 */

#ifndef _TCLPLATDECLS
#define _TCLPLATDECLS

#undef TCL_STORAGE_CLASS
#ifdef BUILD_tcl
#   define TCL_STORAGE_CLASS DLLEXPORT
#else
#   ifdef USE_TCL_STUBS
#      define TCL_STORAGE_CLASS
#   else
#      define TCL_STORAGE_CLASS DLLIMPORT
#   endif
#endif

/*
 *  Pull in the typedef of TCHAR for windows.
 */
#if defined(__CYGWIN__)
    typedef char TCHAR;
#elif defined(__WIN32__) && !defined(_TCHAR_DEFINED)
#   include <tchar.h>
#   ifndef _TCHAR_DEFINED
	/* Borland seems to forget to set this. */
        typedef _TCHAR TCHAR;
#	define _TCHAR_DEFINED
#   endif
#   if defined(_MSC_VER) && defined(__STDC__)
	/* MSVC++ misses this. */
	typedef _TCHAR TCHAR;
#   endif
#endif

/* !BEGIN!: Do not edit below this line. */

/*
 * Exported function declarations:
 */

#ifdef __WIN32__
#ifndef Tcl_WinUtfToTChar_TCL_DECLARED
#define Tcl_WinUtfToTChar_TCL_DECLARED
/* 0 */
EXTERN TCHAR *		Tcl_WinUtfToTChar _ANSI_ARGS_((CONST char * str, 
				int len, Tcl_DString * dsPtr));
#endif
#ifndef Tcl_WinTCharToUtf_TCL_DECLARED
#define Tcl_WinTCharToUtf_TCL_DECLARED
/* 1 */
EXTERN char *		Tcl_WinTCharToUtf _ANSI_ARGS_((CONST TCHAR * str, 
				int len, Tcl_DString * dsPtr));
#endif
#endif /* __WIN32__ */
#ifdef MAC_OSX_TCL
#ifndef Tcl_MacOSXOpenBundleResources_TCL_DECLARED
#define Tcl_MacOSXOpenBundleResources_TCL_DECLARED
/* 0 */
EXTERN int		Tcl_MacOSXOpenBundleResources _ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * bundleName, 
				int hasResourceFile, int maxPathLen, 
				char * libraryPath));
#endif
#ifndef Tcl_MacOSXOpenVersionedBundleResources_TCL_DECLARED
#define Tcl_MacOSXOpenVersionedBundleResources_TCL_DECLARED
/* 1 */
EXTERN int		Tcl_MacOSXOpenVersionedBundleResources _ANSI_ARGS_((
				Tcl_Interp * interp, CONST char * bundleName, 
				CONST char * bundleVersion, 
				int hasResourceFile, int maxPathLen, 
				char * libraryPath));
#endif
#endif /* MAC_OSX_TCL */

typedef struct TclPlatStubs {
    int magic;
    struct TclPlatStubHooks *hooks;

#ifdef __WIN32__
    TCHAR * (*tcl_WinUtfToTChar) _ANSI_ARGS_((CONST char * str, int len, Tcl_DString * dsPtr)); /* 0 */
    char * (*tcl_WinTCharToUtf) _ANSI_ARGS_((CONST TCHAR * str, int len, Tcl_DString * dsPtr)); /* 1 */
#endif /* __WIN32__ */
#ifdef MAC_OSX_TCL
    int (*tcl_MacOSXOpenBundleResources) _ANSI_ARGS_((Tcl_Interp * interp, CONST char * bundleName, int hasResourceFile, int maxPathLen, char * libraryPath)); /* 0 */
    int (*tcl_MacOSXOpenVersionedBundleResources) _ANSI_ARGS_((Tcl_Interp * interp, CONST char * bundleName, CONST char * bundleVersion, int hasResourceFile, int maxPathLen, char * libraryPath)); /* 1 */
#endif /* MAC_OSX_TCL */
} TclPlatStubs;

#ifdef __cplusplus
extern "C" {
#endif
extern TclPlatStubs *tclPlatStubsPtr;
#ifdef __cplusplus
}
#endif

#if defined(USE_TCL_STUBS) && !defined(USE_TCL_STUB_PROCS)

/*
 * Inline function declarations:
 */

#ifdef __WIN32__
#ifndef Tcl_WinUtfToTChar
#define Tcl_WinUtfToTChar \
	(tclPlatStubsPtr->tcl_WinUtfToTChar) /* 0 */
#endif
#ifndef Tcl_WinTCharToUtf
#define Tcl_WinTCharToUtf \
	(tclPlatStubsPtr->tcl_WinTCharToUtf) /* 1 */
#endif
#endif /* __WIN32__ */
#ifdef MAC_OSX_TCL
#ifndef Tcl_MacOSXOpenBundleResources
#define Tcl_MacOSXOpenBundleResources \
	(tclPlatStubsPtr->tcl_MacOSXOpenBundleResources) /* 0 */
#endif
#ifndef Tcl_MacOSXOpenVersionedBundleResources
#define Tcl_MacOSXOpenVersionedBundleResources \
	(tclPlatStubsPtr->tcl_MacOSXOpenVersionedBundleResources) /* 1 */
#endif
#endif /* MAC_OSX_TCL */

#endif /* defined(USE_TCL_STUBS) && !defined(USE_TCL_STUB_PROCS) */

/* !END!: Do not edit above this line. */

#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLIMPORT

#endif /* _TCLPLATDECLS */


