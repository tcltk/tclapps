# -*-tcl-*-
#
# On 8.5+ msgcat files are sourced using utf-8 encoding. On 8.4 they
# were read using utf-8 then evaluated. So use UTF-8
#
::msgcat::mcmset ru {
    "&File"                 "&Файл"
    "&Preferences"          "&Предпочтения"
    "&Emoticons"            "&Эмоциконки"
    "&Visibility"           "&Видимость"
    "&Alerts"               "&Alerts"
    "&Debug"                "&Отлаживайте"
    "&Help"                 "&Спрака"

    "&Login"                "&Подключение"
    "&Logout"               "&Отключение"
    "Login via &IRC..."     "Подключиться через &IRC..."
    "&Save options"         "&Save options"
    "Open &whiteboard"      "Open &whiteboard"
    "Open &paste dialog"    "Open &paste dialog"
    "&Install application"  "&Установите прогрвмму"
    "E&xit"                 "В&ыход"

    "&Display online users" "&Display online users"
    "&Enable whiteboard"    "&Enable whiteboard"
    "Auto &bookmark"        "Auto &bookmark"
    "Auto-i&nit Babelfish"  "Auto-i&nit Babelfish"

    "&Colors..."            "&Colors..."
    "&Font..."              "&Щрифт..."
    "&User details..."      "&User details..."
    "&Input method..."      "&Input method..."
    "&Options..."           "&Options..."
    "&Tk themes"            "&Tk themes"

    "&Local chat logging"   "&Local chat logging"
    "&Disabled"             "&Disabled"
    "&To file..."           "&To file..."
    "&Load file..."         "&Load file..."

    "&Server chat logging" "&Server chat logging"
    "L&og my messages, do not log my actions (old style)" \
        "L&og my messages, do not log my actions (old style)"
    "&Log my messages and actions" "&Log my messages and actions"
    "Do &not log my messages and actions" "Do &not log my messages and actions"

    "Loading server &history" "Loading server &history"
    "Do &NOT load any history" "Do &NOT load any history"
    "&Ask me which logs to load" "&Ask me which logs to load"
    "Load at least %d lines" "Load at least %d lines"

    "&One to One chats"     "&One to One chats"
    "&Keep all chat in one window" "&Keep all chat in one window"
    "&Popup a new window"   "&Popup a new window"
    "Open in new &tab"      "Open in new &tab"

    "&Auto away"            "&Auto away"
    "After %d minutes"      "After %d minutes"

    "&Show emoticons..."    "&Show emoticons..."
    "&Use emoticons"        "&Use emoticons"
    "&Animate emoticons"    "&Animate emoticons"
    "U&pdate emoticons"     "U&pdate emoticons"
    "&Insert"               "&Insert"

    "Hide &single dot messages" "Hide &single dot messages"
    "Hide &online/away status messages" "Hide &online/away status messages"
    "Hide &logon/logoff messages" "Hide &logon/logoff messages"
    "Hide &all system messages" "Hide &all system messages"
    "Hide &error messages"  "Hide &error messages"
    "Hide &timestamps"      "Hide &timestamps"
    "Hide status&bar"       "Hide status&bar"
    "Hide all &users"       "Hide all &users"
    "Sho&w all users"       "Sho&w all users"
    "&Hide users"           "&Hide users"
    "Show &statusbar history" "Show &statusbar history"
    "Show &current history in new pane" "Show &current history in new pane"

    "Alert when any &message received" "Alert when any &message received"
    "Alert when &username mentioned" "Alert when &username mentioned"
    "Alert when s&omeone speaks who was quiet" \
        "Alert when s&omeone speaks who was quiet"
    "Alert when &specified subject mentioned" \
        "Alert when &specified subject mentioned"
    "&Beep on alert"         "&Beep on alert"
    "&Raise to top on alert" "&Raise to top on alert"
    "Alert on regular &posts" "Alert on regular &posts"
    "Alert on &whispers and \"/me\" posts" "Alert on &whispers and \"/me\" posts"

    "&Reload script"        "&Reload script"
    "Re&start script"       "Re&start script"
    "E&valuate selection"   "E&valuate selection"
    "&Allow remote control" "&Allow remote control"
    "&Get user versions"    "&Get user versions"
    "Reload &history"       "Reload &history"
    "&Error logging"        "&Error logging"
    "&Log level"            "&Log level"
    "To &stderr"            "To &stderr"
    "To &file..."           "To &file..."
    "Console"               "Console"

    "&Quick help..."        "&Quick help..."
    "Help (&wiki)..."       "Help (&wiki)..."
    "&Check version"        "&Check version"
    "&View Change&Log..."   "&View ChangeLog..."
    "&About..."             "&О программе..."

    "Hide user"             "Hide user"
    "Set/Unset bookmark"    "Set/Unset bookmark"
    "Prev bookmark"         "Prev bookmark"
    "Next bookmark"         "Next bookmark"
    "Clear bookmarks"       "Clear bookmarks"
    "Google selection"      "Google selection"

    "Initialize translation" "Initialize translation"
    "Translate selection"    "Translate selection"

    OK     "OK"
    Cancel "Cancel"
    Yes    "Yes"
    No     "No"
    Close  "Close"
    Send   "Send"
    Reply  "Reply"
    Ready  "Ready"
    Clear  "Clear"
    Quit   "Quit"
    Login  "Login"
    Post   "Post"
    Error  "Error"

    "All users"              "All users"
    "Disconnected from jabber server." "Disconnected from jabber server."
    "Connected to %s, sending credentials." "Connected to %s, sending credentials."
    "Close history pane" "Close history pane"
    "Lurk"   "Lurk"
    "Unlurk" "Unlurk"
    "Lurking, select Unlurk from the context menu to re-enable" \
        "Lurking, select Unlurk from the context menu to re-enable"

    "Load History From Logs" "Load History From Logs"
    "Please select how far back you want to load:" \
        "Please select how far back you want to load:"

    "Subject" "Тема"
    "%s - tkchat message" "%s - сообщение tkchat"
    "Re: %s" "Re: %s"

    "Subscribe request from %s" "Subscribe request from %s"
    "Do you want to let %s add you to their roster?" \
        "Do you want to let %s add you to their roster?"

    "%s has become available" "%s has become available"
    "%s has left"             "%s вышел(а) из комнаты"
    "%s is now known as %s"   "%s is now known as %s"

    "Tkchat confirm quit"            "Tkchat confirm quit"
    "Are you sure you want to quit?" "Are you sure you want to quit?"

    "About TkChat %s" "О программе TkChat %s"
    "Using Tcl/Tk %s" "Using Tcl/Tk %s"
    "Copyright (c) %s by the following authors:" \
        "Copyright (c) %s by the following authors:"
    "Paste data to %s" "Paste data to %s"
    "Eval in whiteboard" "Eval in whiteboard"
    "Subject required" "Subject required"
    "You must provide a subject to be displayed as the title for this paste." \
        "You must provide a subject to be displayed as the title for this paste."
}

# Logon dialog
::msgcat::mcset ru "Use HTTP pro&xy"
::msgcat::mcset ru "&Proxy host:port"
::msgcat::mcset ru "Proxy auth &username"
::msgcat::mcset ru "Proxy auth pa&ssword"
::msgcat::mcset ru "J&ID | Nick"
::msgcat::mcset ru "Chat p&assword"
::msgcat::mcset ru "&Remember chat password"
::msgcat::mcset ru "&Jabber server:port"
::msgcat::mcset ru "Jab&ber resource"
::msgcat::mcset ru "Jabber con&ference"
::msgcat::mcset ru "N&o SSL"
::msgcat::mcset ru "Jabber SSL"
::msgcat::mcset ru "STARTTLS"
::msgcat::mcset ru "Auto-&connect"
::msgcat::mcset ru "&Validate SSL certificates"
::msgcat::mcset ru "&Quit"

# vim:ft=tcl:ts=8:sw=4:sts=4:noet
# Local variables:
# mode: tcl
# indent-tabs-mode: nil
# buffer-file-coding-system: utf-8-dos
# End:
