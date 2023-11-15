Set oWS = WScript.CreateObject("WScript.Shell")
sLinkFile = "TkChat.lnk" 
Set oLink = oWS.CreateShortcut(sLinkFile) 
appDataLocation = oWS.ExpandEnvironmentStrings("%APPDATA%")
app_dir = appDataLocation & "\TkChat"
oLink.TargetPath = app_dir & "\bin\wish90.exe"
oLink.Arguments = "AppMain.tcl" 
oLink.WorkingDirectory = app_dir & "\bin"
oLink.IconLocation = app_dir & "\TkChat.ico"
oLink.Save 

