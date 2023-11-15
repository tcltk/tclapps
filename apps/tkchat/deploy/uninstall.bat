@echo off
 
call :confirm
if "%yesno%"=="6" (
call :remove
) else (
call :cancel
)
exit /b
 


:confirm
::returns 6 = Yes, 7 = No.
set yesno=
echo wscript.echo MsgBox("Do you want to uninstall TkChat?", vbYesNo+vbInformation, "Uninstall") > %temp%\confirm.vbs
for /f "tokens=* delims=" %%a in ('cscript //nologo "%temp%\confirm.vbs"') do set yesno=%%a
del %temp%\confirm.vbs /f /q
exit /b
 
 
:remove
reg delete "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TkChat.lnk" /f
reg delete  "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f
 
rmdir "%appdata%\Microsoft\Windows\Start Menu\TkChat" /s /q 
echo MsgBox "Uninstallation of TkChat is complete.", vbOKOnly+vbInformation, "Uninstall"> %temp%\remove.vbs
call %temp%\remove.vbs
del %temp%\remove.vbs /f /q
start /b "" cmd /c rmdir "%appdata%\TkChat\" /s /q && exit
 
 
:cancel
echo MsgBox "Uninstallation of TkChat cancelled.",vbOKOnly+vbInformation, "Uninstall TkChat"  > %temp%\cancel.vbs
call %temp%\cancel.vbs
del %temp%\cancel.vbs /f /q
exit /b
