# choosefont -- Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This package provides a font selection dialog. Where possible it will 
# use compiled code to launch a platform-specific dialog. In Windows this
# will be the font common dialog and the intention is to call the Gtk+
# font dialog where the Window manager suggests a GNOME system and a KDE
# dialog for a KDE system.
# We will drop down to DKFs font chooser if we have no other implementation.
#
# $Id: choose_gtk.tcl,v 1.1 2006/11/10 00:51:05 patthoyts Exp $

namespace eval ::choosefont {
    critcl::tk
    eval [linsert [exec pkg-config gtk+-2.0 --libs] 0 critcl::clibraries]
    eval [linsert [exec pkg-config gtk+-2.0 --cflags] 0 critcl::cflags]
    critcl::ccode {
#include <gtk/gtk.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkarrow.h>

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
        
        typedef struct Stuff {
            Tcl_Interp *interp;
            Tcl_Obj *applyObj;
            int running;
            GtkFontSelectionDialog *fsd;
        } Stuff;

        static void
        on_fontdlg_apply(GtkWidget *widget, gpointer dataPtr)
        {
            Stuff *stuffPtr = dataPtr;
            g_printf("%s\n", 
                     gtk_font_selection_dialog_get_font_name(stuffPtr->fsd));

        }

        static int
        on_fontdlg_response(GtkDialog *dlg, gint response, gpointer dataPtr)
        {
            gint *responsePtr = dataPtr;

            switch (response) {
                case GTK_RESPONSE_APPLY:
                    return 1;
                    
                case GTK_RESPONSE_ACCEPT:
                case GTK_RESPONSE_OK:
		    *responsePtr = 1;
		    return 1;
                default:
                    *responsePtr = 2;
		    return 0;
	    }
	}
    }

    critcl::cinit {
        gtk_init(0, NULL);
        Tcl_CreateEventSource(SetupProc, CheckProc, NULL);
    } {}

    critcl::cproc ChooseFont {Tcl_Interp* interp Tcl_Obj* parent
        Tcl_Obj* titleObj Tcl_Obj* fontObj Tcl_Obj* applyObj} ok {
        int r = TCL_OK, oldMode;
        Tcl_Obj *resObj = NULL;
        GtkWidget *dlg;
        gint response = 0;

        /*
         * Create the Gtk font dialog and show modally
         */
        dlg = gtk_font_selection_dialog_new(Tcl_GetString(titleObj));
        g_signal_connect(G_OBJECT(GTK_FONT_SELECTION_DIALOG(dlg)),
                         "response",
                         G_CALLBACK(on_fontdlg_response), &response);
        gtk_widget_show(GTK_FONT_SELECTION_DIALOG(dlg)->apply_button);
        gtk_window_set_modal(GTK_WINDOW(dlg), TRUE);
        gtk_widget_show_all(dlg);

        /*
         * Run our modal loop
         */
        oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
        while (response == 0) {
            Tcl_DoOneEvent(TCL_ALL_EVENTS);
        }
        Tcl_SetServiceMode(oldMode);

        /*
         * Convert the Gtk+ font name into a Tcl font description
         */
        gtk_dialog_response(GTK_DIALOG(dlg), &response);
	g_printf("response %d\n", response);
        if (1) //response == GTK_RESPONSE_OK)
        {
            PangoFontDescription *fd;
            const gchar *fontname;
            Tcl_Obj *resObj = Tcl_NewListObj(0, NULL);

            fontname = gtk_font_selection_dialog_get_font_name
                (GTK_FONT_SELECTION_DIALOG(dlg));
            fd = pango_font_description_from_string(fontname);

            if (fd != NULL) {
		Tcl_Obj *attrObj = Tcl_NewListObj(0, NULL);
                Tcl_ListObjAppendElement(interp, resObj,
                  Tcl_NewStringObj(pango_font_description_get_family(fd), -1));
                Tcl_ListObjAppendElement(interp, resObj,
		    Tcl_NewIntObj(pango_font_description_get_size(fd)/PANGO_SCALE));
		if (pango_font_description_get_weight(fd) >= PANGO_WEIGHT_BOLD) {
		    Tcl_ListObjAppendElement(interp, attrObj, Tcl_NewStringObj("bold", -1));
		}
		if (pango_font_description_get_style(fd) == PANGO_STYLE_ITALIC) {
		    Tcl_ListObjAppendElement(interp, attrObj, Tcl_NewStringObj("italic", -1));
		}
		Tcl_ListObjAppendElement(interp, resObj, attrObj);
	    }
	    Tcl_SetObjResult(interp, resObj);
        }
	gtk_widget_destroy(dlg);
        return TCL_OK;
    }
}

#
# Local variables:
#   mode: c
# End:
#
