# choosefont -- Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This package provides a font selection dialog. Where possible it will 
# use compiled code to launch a platform-specific dialog. In Windows this
# will be the font common dialog and the intention is to call the Gtk+
# font dialog where the Window manager suggests a GNOME system and a KDE
# dialog for a KDE system.
# We will drop down to DKFs font chooser if we have no other implementation.
#
# $Id: choose_gtk.tcl,v 1.3 2007/04/27 20:32:30 patthoyts Exp $

namespace eval ::choosefont {
    critcl::tk
    eval [linsert [exec pkg-config gtk+-2.0 --libs] 0 critcl::clibraries]
    eval [linsert [exec pkg-config gtk+-2.0 --cflags] 0 critcl::cflags]
    critcl::ccode {
#include <gtk/gtk.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkarrow.h>
#include <gdk/gdkx.h>

        /* Create a Glib event source to link the Tk and Glib event loops */
        static void SetupProc(ClientData clientData, int flags) {
            Tcl_Time block_time = {0, 0};
            if (!(flags & TCL_WINDOW_EVENTS)) {
                return;
            }
            if (!gtk_events_pending()) {
                block_time.usec = 10000;
            }
            Tcl_SetMaxBlockTime(&block_time);
            return;
        }

        static int EventProc(Tcl_Event *evPtr, int flags)
        {
            if (!(flags & TCL_WINDOW_EVENTS)) {
                return 0;
            }
            while (gtk_events_pending()) {
                gtk_main_iteration();
            }
            return 1;
        }
        static void CheckProc(ClientData clientData, int flags) {
            if (!(flags & TCL_WINDOW_EVENTS)) {
                return;
            }
            if (gtk_events_pending()) {
                Tcl_Event *event = (Tcl_Event *)ckalloc(sizeof(Tcl_Event));
                event->proc = EventProc;
                Tcl_QueueEvent(event, TCL_QUEUE_TAIL);
            }
            return;
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

	static Tcl_Obj *
	GetFontObjFromGtkWidget(GtkWidget *dlg)
	{
            PangoFontDescription *fd;
            const gchar *fontname;
            Tcl_Obj *resObj = Tcl_NewListObj(0, NULL);

            fontname = gtk_font_selection_dialog_get_font_name
                (GTK_FONT_SELECTION_DIALOG(dlg));
            fd = pango_font_description_from_string(fontname);
            
            if (fd != NULL) {
		int len = 0;
                Tcl_Obj *attrObj = Tcl_NewListObj(0, NULL);
                Tcl_ListObjAppendElement(NULL, resObj,
		    Tcl_NewStringObj(pango_font_description_get_family(fd), -1));
                Tcl_ListObjAppendElement(NULL, resObj,
		    Tcl_NewIntObj(pango_font_description_get_size(fd)/PANGO_SCALE));
		if (pango_font_description_get_weight(fd) >= PANGO_WEIGHT_BOLD) {
		    Tcl_ListObjAppendElement(NULL, attrObj, Tcl_NewStringObj("bold", -1));
		}
		if (pango_font_description_get_style(fd) == PANGO_STYLE_ITALIC
		    || pango_font_description_get_style(fd) == PANGO_STYLE_OBLIQUE) {
		    Tcl_ListObjAppendElement(NULL, attrObj, Tcl_NewStringObj("italic", -1));
		}
		Tcl_ListObjLength(NULL, attrObj, &len);
		if (len > 0) {
		    Tcl_ListObjAppendElement(NULL, resObj, attrObj);
		}
	    }
            return resObj;
	}

        typedef struct Stuff {
            Tcl_Interp *interp;
	    Tk_Window  tkwin;
            Tcl_Obj *applyObj;
	    GtkWidget *dlg;
	    gint response;
        } Stuff;

	static int
	on_fontdlg_realized(GtkWidget *widget, gpointer dataPtr)
	{
	    Stuff *stuffPtr = dataPtr;
	    GdkWindow *gwin = gtk_widget_get_parent_window(GTK_WIDGET(stuffPtr->dlg));
	    if (gwin) {
		Window w = GDK_WINDOW_XID(GDK_WINDOW(gwin));
		g_printf("Setting transient\n");
		XSetTransientForHint(Tk_Display(stuffPtr->tkwin), w,
				     Tk_WindowId(stuffPtr->tkwin));
	    }
	    return 0;
	}

        static int
        on_fontdlg_response(GtkDialog *dlg, gint response, gpointer dataPtr)
        {
            Stuff *stuffPtr = dataPtr;
	    Tcl_Obj *objv[2];

            switch (response) {
                case GTK_RESPONSE_APPLY:
		    stuffPtr->response = 0;
		    if (stuffPtr->applyObj) {
			objv[0] = stuffPtr->applyObj;
			objv[1] = GetFontObjFromGtkWidget(GTK_WIDGET(dlg));
			BackgroundEvalObjv(stuffPtr->interp, 2, objv, 0);
		    }
                    return 1;
                    
                case GTK_RESPONSE_ACCEPT:
                case GTK_RESPONSE_OK:
		    stuffPtr->response = 1;
		    return 1;
                default:
                    stuffPtr->response = 2;
		    return 0;
	    }
	}
    }

    critcl::cinit {
        gtk_init(0, NULL);
        Tcl_CreateEventSource(SetupProc, CheckProc, NULL);
    } {}

    critcl::cproc ChooseFont {Tcl_Interp* interp Tcl_Obj* parentObj
        Tcl_Obj* titleObj Tcl_Obj* fontObj Tcl_Obj* applyObj} ok {
        int r = TCL_OK, oldMode, fontc;
        Tcl_Obj *tmpObj = NULL, **fontv;
	Tk_Window tkwin;
	Stuff stuff;

	tkwin = Tk_NameToWindow(interp, Tcl_GetString(parentObj), 
	    Tk_MainWindow(interp));
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}

        /* parse a font description: pango has the size last */
        r = Tcl_ListObjGetElements(NULL, fontObj, &fontc, &fontv);
        if (TCL_OK == r && fontc > 0) {
	    if (fontc > 2) {
		int n;
		tmpObj = fontv[1];
		for (n = 1; n < fontc-1; ++n) {
		    fontv[n] = fontv[n+1];
		}
		fontv[fontc-1] = tmpObj;
	    }
	    tmpObj = Tcl_ConcatObj(fontc, fontv);
        }

	/* Create the Gtk font dialog and show modally */
	stuff.interp = interp;
	stuff.tkwin = tkwin;
	stuff.applyObj = applyObj;
	stuff.response = 0;
        stuff.dlg = gtk_font_selection_dialog_new(Tcl_GetString(titleObj));
	if (tmpObj != NULL) {
	    gtk_font_selection_dialog_set_font_name
		(GTK_FONT_SELECTION_DIALOG(stuff.dlg), Tcl_GetString(tmpObj));
	}
        g_signal_connect(G_OBJECT(GTK_FONT_SELECTION_DIALOG(stuff.dlg)),
                         "response",
                         G_CALLBACK(on_fontdlg_response), &stuff);
	g_signal_connect_after(G_OBJECT(GTK_FONT_SELECTION_DIALOG(stuff.dlg)),
			 "realize",
			 G_CALLBACK(on_fontdlg_realized), &stuff);
        gtk_widget_show(GTK_FONT_SELECTION_DIALOG(stuff.dlg)->apply_button);
        gtk_window_set_modal(GTK_WINDOW(stuff.dlg), TRUE);
	gtk_window_has_toplevel_focus(GTK_WINDOW(stuff.dlg));
        gtk_widget_show_all(GTK_WIDGET(stuff.dlg));

        /*
         * Run our modal loop
         */
        oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
        while (stuff.response == 0) {
            Tcl_DoOneEvent(TCL_ALL_EVENTS);
        }
        Tcl_SetServiceMode(oldMode);

        /*
         * Convert the Gtk+ font name into a Tcl font description
         */
        if (stuff.response == 1) {
	    Tcl_SetObjResult(interp, GetFontObjFromGtkWidget(stuff.dlg));
        } else {
	    Tcl_ResetResult(interp);
	}
	gtk_widget_destroy(stuff.dlg);
        return TCL_OK;
    }
}

#
# Local variables:
#   mode: c
# End:
#
