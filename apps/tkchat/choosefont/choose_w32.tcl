# choosefont -- Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This package provides a font selection dialog. Where possible it will 
# use compiled code to launch a platform-specific dialog. In Windows this
# will be the font common dialog and the intention is to call the Gtk+
# font dialog where the Window manager suggests a GNOME system and a KDE
# dialog for a KDE system.
# We will drop down to DKFs font chooser if we have no other implementation.
#
# $Id: choose_w32.tcl,v 1.2 2012/07/10 17:18:48 andreas_kupries Exp $

namespace eval ::choosefont {

    critcl::tk
    critcl::clibraries -lcomdlg32 -luser32 -lgdi32
    critcl::ccode {
#define STRICT
#define WIN32_LEAN_AND_MEAN
#define _WIN32_WINNT 0x0502
#include <windows.h>
#include <commdlg.h>
#include <tkPlatDecls.h> /* this brings in some internal stubs */

        static HWND 
        GetHWNDFromObj(Tcl_Interp *interp, Tcl_Obj *pathObj)
        {
            Tk_Window tkwin = (Tk_Window)NULL;
            Window window = None;
            HWND hwnd = (HWND)NULL;
            
            tkwin = Tk_NameToWindow(interp, Tcl_GetString(pathObj), 
                                    Tk_MainWindow(interp));
            if (tkwin != NULL)
                window = Tk_WindowId(tkwin);
            if (window != None)
                hwnd = Tk_GetHWND(window);
            if (hwnd != NULL && Tk_IsTopLevel(tkwin))
                hwnd = GetParent(hwnd); /* toplevels have a wrapper window */
            return hwnd;
        }

        static int
        BackgroundEvalObjv(Tcl_Interp *interp, 
            int objc, Tcl_Obj *const *objv, int flags)
        {
            Tcl_DString errorInfo, errorCode;
            Tcl_SavedResult state;
            int r = TCL_OK;
            
            Tcl_DStringInit(&errorInfo);
            Tcl_DStringInit(&errorCode);
            
            /*
             * Record the state of the interpreter
             */
            
            Tcl_SaveResult(interp, &state);
            Tcl_DStringAppend(&errorInfo, 
                Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY), -1);
            Tcl_DStringAppend(&errorCode, 
                Tcl_GetVar(interp, "errorCode", TCL_GLOBAL_ONLY), -1);
            
            /*
             * Evaluate the command and handle any error.
             */
            
            r = Tcl_EvalObjv(interp, objc, objv, flags);
            if (r == TCL_ERROR) {
                Tcl_AddErrorInfo(interp, "\n    (background event handler)");
                Tcl_BackgroundError(interp);
            }
            
            /*
             * Restore the state of the interpreter
             */
            
            Tcl_SetVar(interp, "errorInfo",
                Tcl_DStringValue(&errorInfo), TCL_GLOBAL_ONLY);
            Tcl_SetVar(interp, "errorCode",
                Tcl_DStringValue(&errorCode), TCL_GLOBAL_ONLY);
            Tcl_RestoreResult(interp, &state);
            
            /*
             * Clean up references.
             */
            
            Tcl_DStringFree(&errorInfo);
            Tcl_DStringFree(&errorCode);
            
            return r;
        }

        typedef struct HookData {
            Tcl_Interp *interp;
            Tcl_Obj *titleObj;
            Tcl_Obj *cmdObj;
        } HookData;

        static Tcl_Obj *
        GetFontObj(HDC hdc, LOGFONT *plf)
        {
            Tcl_Obj *resObj, *attrObj;
            int len = 0, pt = 0;

            resObj = Tcl_NewListObj(0, NULL);
            attrObj = Tcl_NewListObj(0, NULL);
            Tcl_ListObjAppendElement(NULL, resObj,
                                     Tcl_NewStringObj(plf->lfFaceName, -1));
            pt = -MulDiv(plf->lfHeight, 72, GetDeviceCaps(hdc, LOGPIXELSY));
            Tcl_ListObjAppendElement(NULL, resObj, Tcl_NewIntObj(pt));
            if (plf->lfWeight >= 700) {
                Tcl_ListObjAppendElement(NULL, attrObj,
                                         Tcl_NewStringObj("bold", -1));
            }
            if (plf->lfItalic) {
                Tcl_ListObjAppendElement(NULL, attrObj,
                                         Tcl_NewStringObj("italic", -1));
            }
            if (plf->lfUnderline) {
                Tcl_ListObjAppendElement(NULL, attrObj,
                                         Tcl_NewStringObj("underline", -1));
            }
            if (plf->lfStrikeOut) {
                Tcl_ListObjAppendElement(NULL, attrObj,
                                         Tcl_NewStringObj("overstrike", -1));
            }
            Tcl_ListObjLength(NULL, attrObj, &len);
            if (len > 0) {
                Tcl_ListObjAppendElement(NULL, resObj, attrObj);
            }
            return resObj;
        }

        static UINT_PTR CALLBACK
        HookProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam)
        {
            CHOOSEFONT *pcf = (CHOOSEFONT *)lParam;
            static HookData *phd = NULL;

            if (WM_INITDIALOG == msg && lParam != 0) {
                phd = (HookData *)pcf->lCustData;
                if (phd->titleObj != NULL) {
                    int len = 0;
                    Tcl_UniChar *wsz = 
                        Tcl_GetUnicodeFromObj(phd->titleObj, &len);
                    if (len > 0) {
                        SetWindowTextW(hwndDlg, wsz);
                        return 1;
                    }
                }
            }

            /* Handle apply button command */
            if (WM_COMMAND == msg && LOWORD(wParam) == 1026) {
                LOGFONT lf = {0};
                int iPt = 0;
                HDC hdc = GetDC(hwndDlg);
                SendMessage(hwndDlg, WM_CHOOSEFONT_GETLOGFONT, 0, (LPARAM)&lf);

                if (phd != NULL) {
                    int objc;
                    Tcl_Obj **objv, **tmpv, *fontObj;
                    Tcl_ListObjGetElements(NULL, phd->cmdObj, &objc, &objv);
                    tmpv = (Tcl_Obj **)ckalloc(sizeof(Tcl_Obj *) * (objc + 2));
                    memcpy(tmpv, objv, sizeof(Tcl_Obj *) * objc);
                    tmpv[objc] = GetFontObj(hdc, &lf);
                    BackgroundEvalObjv(phd->interp, objc+1, tmpv, 0);
                    ckfree((char *)tmpv);
                }
                return 1;
            }
            return 0; /* pass on for default processing */
        }
    }

    critcl::cproc ChooseFont {Tcl_Interp* interp Tcl_Obj* parent
        Tcl_Obj* titleObj Tcl_Obj* fontObj Tcl_Obj* applyObj} ok {
        CHOOSEFONT cf;
        LOGFONT lf;
        HookData hd;
        int r = TCL_OK, oldMode = 0, fontc = 0;
        Tcl_Obj **fontv;
        Tcl_Obj *resObj = NULL;

        ZeroMemory(&cf, sizeof(CHOOSEFONT));
        ZeroMemory(&lf, sizeof(LOGFONT));
        lf.lfCharSet = DEFAULT_CHARSET;
        cf.lStructSize = sizeof(CHOOSEFONT);
        cf.hwndOwner = GetHWNDFromObj(interp, parent);
        cf.lpLogFont = &lf;
        cf.nFontType = SCREEN_FONTTYPE;
        cf.Flags = CF_SCREENFONTS | CF_EFFECTS | CF_ENABLEHOOK;
        cf.rgbColors = RGB(0,0,0);
        cf.lpfnHook = HookProc;
        cf.lCustData = &hd;
        hd.interp = interp;
        hd.cmdObj = applyObj;
        hd.titleObj = titleObj;

        /* parse a font description */
        r = Tcl_ListObjGetElements(NULL, fontObj, &fontc, &fontv);
        if (TCL_OK == r && fontc > 0) {
            cf.Flags |= CF_INITTOLOGFONTSTRUCT;
            strncpy(lf.lfFaceName, Tcl_GetString(fontv[0]), LF_FACESIZE-1);
            lf.lfFaceName[LF_FACESIZE-1] = 0;
        }

        if (TCL_OK == r && fontc > 1) {
            int iPointSize = 0;
            r = Tcl_GetIntFromObj(interp, fontv[1], &iPointSize);
            if (TCL_OK == r) {
                HDC hdc = GetDC(cf.hwndOwner);
                lf.lfHeight = -MulDiv(iPointSize, 
                    GetDeviceCaps(hdc, LOGPIXELSY), 72);
            }
        }

        if (TCL_OK == r && fontc > 2) {
            int n;
            for (n = 2; TCL_OK == r && n < fontc; ++n) {
                enum {ATTR_BOLD, ATTR_ITALIC, ATTR_UNDERLINE, ATTR_OVERSTRIKE};
                static const char *attrStrings[] = {
                    "bold", "italic", "underline", "overstrike", NULL
                };
                int attrc, attr, attribute;
                Tcl_Obj **attrv;
                
                r = Tcl_ListObjGetElements(interp, fontv[n], &attrc, &attrv);
                for (attr = 0; TCL_OK == r && attr < attrc; ++attr) {
                    r = Tcl_GetIndexFromObj(interp, attrv[attr],
                                            attrStrings, "option", 0,
                                            &attribute);
                    if (TCL_OK == r) {
                        switch (attribute) {
                            case ATTR_BOLD: lf.lfWeight = FW_BOLD; break;
                            case ATTR_ITALIC: lf.lfItalic = TRUE; break;
                            case ATTR_UNDERLINE: lf.lfUnderline = TRUE; break;
                            case ATTR_OVERSTRIKE: lf.lfStrikeOut = TRUE; break;
                        }
                    }
                }
            }
        }
        
        if (TCL_OK == r) {
            int len = 0;
            r = Tcl_ListObjLength(interp, applyObj, &len);
            if (len > 0) cf.Flags |= CF_APPLY;
        }

        if (TCL_OK == r) {
            oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
            if (ChooseFont(&cf)) {
                resObj = GetFontObj(GetDC(cf.hwndOwner), &lf);
            } else {
                resObj = Tcl_NewListObj(0, NULL);
            }
            Tcl_SetServiceMode(oldMode);
            Tcl_SetObjResult(interp, resObj);
        }
        
        return r;
    }
}

# Local variables:
# mode: c
# indent-tabs-mode: nil
# End:
