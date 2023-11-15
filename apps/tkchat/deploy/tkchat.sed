[Version]
Class=IEXPRESS
SEDVersion=3
[Options]
PackagePurpose=InstallApp
ShowInstallProgramWindow=0
HideExtractAnimation=0
UseLongFileName=1
InsideCompressed=0
CAB_FixedSize=0
CAB_ResvCodeSigning=0
RebootMode=N
InstallPrompt=%InstallPrompt%
DisplayLicense=%DisplayLicense%
FinishMessage=%FinishMessage%
TargetName=%TargetName%
FriendlyName=%FriendlyName%
AppLaunched=%AppLaunched%
PostInstallCmd=%PostInstallCmd%
AdminQuietInstCmd=%AdminQuietInstCmd%
UserQuietInstCmd=%UserQuietInstCmd%
SourceFiles=SourceFiles

[Strings]
InstallPrompt=Would you like to install TkChat?
DisplayLicense=D:\Desktop\tkchat\license.txt
FinishMessage=Thank you for installing TkChat. To run the program, please select it from the start menu.
TargetName=D:\Desktop\tkchat\TkChat_Setup.exe
FriendlyName=TkChat_Setup
AppLaunched=cmd /c install.bat
PostInstallCmd=<None>
AdminQuietInstCmd=
UserQuietInstCmd=
FILE0="WordTech_CA.cer"
FILE1="install.bat"
FILE2="TkChat.zip"
FILE3="link.vbs"
[SourceFiles]
SourceFiles0=D:\Desktop\tkchat\
[SourceFiles0]
%FILE0%=
%FILE1%=
%FILE2%=
%FILE3%=
