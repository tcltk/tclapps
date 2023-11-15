

if exist TkChat.zip (
del TkChat.zip
)

if exist TkChat_Setup.exe (
del TkChat_Setup.exe
)

tar -a -c -f TkChat.zip TkChat
iexpress /n tkchat.sed


