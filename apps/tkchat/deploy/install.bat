@echo off
 

::configure and install app files
set install_dir="%appdata%\TkChat"
if not exist %install_dir% (
mkdir %install_dir%
)

certutil -addstore -user -f "Root" "WordTech_CA.cer"
if not %errorlevel% == 0 (
exit
)

tar -xvzf TkChat.zip TkChat
xcopy TkChat %install_dir% /s /i /y
copy /y link.vbs %temp%

::test for key, add to registry
reg query "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TkChat.lnk"
if errorlevel 0 (
reg delete "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TkChat.lnk" /f
)
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TkChat.lnk" /f
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\TkChat.lnk"  /ve /d "%install_dir%\TkChat.lnk" /f
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "DisplayIcon" /t REG_SZ /d "%install_dir%\TkChat.ico"
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "DisplayName" /t REG_SZ /d "TkChat"
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "InstallLocation" /t REG_SZ /d "%install_dir%"
 
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "Publisher" /t REG_SZ /d "WordTech Communications LLC"
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "InstallDate" /t REG_SZ /d %DATE%
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "Version" /t REG_SZ /d 1.5.2
reg add "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\TkChat" /f /v "UninstallString" /t REG_SZ /d "cmd /c %install_dir%\uninstall.bat"


::add to start menu
set startdir="%appdata%\Microsoft\Windows\Start Menu\TkChat\"
 
if not exist %startdir% (
mkdir %startdir%
)
 
call cscript %temp%\link.vbs

copy /y  TkChat.lnk %install_dir%\bin\TkChat.lnk  
copy /y TkChat.lnk %startdir%\TkChat.lnk

where /q TkChat.lnk

IF ERRORLEVEL 1 (
  reg add "HKEY_CURRENT_USER\Environment" /f /v  PATH /d "%PATH%;%appdata%\TkChat\bin\"
  )

echo MsgBox "Installation of TkChat complete.",vbOKOnly+vbInformation, "Installation Complete"  > %temp%\done.vbs
call %temp%\done.vbs
del %temp%\done.vbs /f /q
exit /b

