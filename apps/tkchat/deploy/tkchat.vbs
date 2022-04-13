Dim objShell

Set objShell = WScript.CreateObject("WScript.Shell")

strCommand = "tclsh87 AppMain.tcl"

objShell.Run(strCommand),0,True

Set objShell = Nothing
