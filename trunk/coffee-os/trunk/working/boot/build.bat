@echo off
echo This is assuming you are using Windows to write the files out.
echo you need HxD or similar tools or 'dd' for windows.
echo sometimes 'dd' has issues so double check your sectors=file contents
echo with a HEX editor of choice, or write them directly with one.
echo Path specified is where I store FREEDOS and nasm. YMMV.
echo Any errors here means the files DID NOT assemble correctly.
echo --Jazz
echo
@echo on
path=c:\nasm
nasm boot.asm -f bin -o boot.bin
nasm usb2.asm -f bin -o usb2.bin
@echo off
echo If you are not in DOS, this lets us know what happened.
echo "Windows" closes the window on us otherwise.
pause