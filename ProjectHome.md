What is COFFEEOS?

A stable linux-based operating system built on FreePascal(TM) (and similar code).
This is the THIRD FULL complete rewrite so far. Flushing Subversion at the moment and uploading newer working code.

Licensed GPLv3 and Modified BSD (MIT) licensed.(DOUBLY FREE)


Name COPYWRIGHTED in 2009, YOU MAY NOT USE this name.
Sourceforge project is not THIS code, nor this project, a derivative of FPOS, also
on GOOGLE code, which I have contributed to.Project admin will be notified.


DOWNLOAD UPDATE

Some files marked DEPRECIATED do not appear. Choose 'ALL Downloads'
and hit search. DOS/FREEDOS downloads are depreciated, however development continues.

ARDUINO platform CAN support CP/M and/or DOS, at least in 8/16 bit modes.
MAPLE from LEAFLABS CAN support 32-bit modes. This is due to CPU
internal architecture and hardware build. HowTO is covered elsewhere on this.(TRY MAKE magazine or INSTRUCTABLE.)

I have both available for my use.



Programmer headache and assumption are as follows:

ASSUME NOTHING. ABSOLUTELY NOTHING.
REALLY...NOTHING.

(OSDEV forums are ran by a bunch of STUCK UP BASTARDS.)

Triple faulting has been fixed, it was AGAIN, a LINKER issue with the linking script.

Assembler is used in parts where required.

The goal is to limit Real-Mode, even VM86 interactions, not cut them off altogether.
BIOS CALLS have to be rewritten with VM86, if they are working at all.
Interrupts are throwing my code in loops in Real Mode.

Tossing REAL MODE CODE into wastebasket for now.
THERE ARE OTHER WAYS .....my friends.


GRUB faults have been fixed.

NOTE:
32bit and LONG MODE(64bit) do not have 'BIOS' interrupts of thier own pre-set.BIG BUMMER.

SEE INTEL 64bit bios specs for further details(PART D, I believe).

I have noticed a glitch in 32-bit DPMI modes with the READKEY function in PASCAL/FPC.
I have implemented a 32-bit workaround(doesnt check for mouse data yet..).

This BUG is under Freedos. I have also noticed a DPMI/fdxxms.sys glitch which hangs freedos on keyboard interrupts. This is most annoying as it locks the machine in a 32-bit state and hangs it randomly.The alternative is to use himemx.sys, commonly available.

Attempts have succeeded in replacing the out-of-date readkey function.Readportb($64) and Readportb($60) have done the job of a byte comparison apparently in the FPC internal code which has gone unchecked.

I will also be uploading a games unit for freedos shortly. (YEAH,YEAH....who runs DOS these days?..) More to come on the EMBEDDED front.

Memory Address $400 is tricky..we need it to CRTC HACK our way into other screen resolutions and apparently we can still peek and poke a tad bit..(readport 40?)

Dont always expect the Subversion builds to build ok. This is expected. If you want a demo, 'check out' the source (SLIK-SVN-1.7.8 is recommended under win32/64.) or download the ISO files offered.I have made available also a USB FREEDOS method for those not wanting to waste a CD(or for those without a drive).

I DO NOT have APM bios at my disposal anymore. **Things will get hairy.**

Ring3 mode jump has been fixed. I was not looking at the segments correctly.

Paging has been fixed.

The access permissions on USERMODE and KERNEL MODE were backwards. Fix is in ASSEMBLER for now.  DO NOT MODIFY return value, it will hang on Paging init otherwise as the EIP gets thrown.

TESTING:
Page Tables are DWORDS at the  BIT level manipulated, not at the BYTE level. This can be done in C, its tricky in PASCAL.

Now onto the memory manager...

Page tables have been moved. PT 768 is a waste of nearly 1GB for kernel's own use, whether we use it or not.This may be FINE in LONG MODE, but I am not writing a LONG MODE OS at this time. KERNEL will not utilize 1GB anyway.WE can better use that wasted RAM.

Memory should not be wasted in kernel land.Moved to PT 1020 or PT4 depending on where we put the PDirectory. The lower the better.Current ASM code points here, take notice of the multiplier, this is where we point.

**PageAlloc/FREE works as intended.(2 pages in use) AND kvm/qemu SHOWS IT.**

I have also updated GDT to FIX a 12 year old outstanding OS GLITCH allowing malicious code, and I have done this without use of NX bit.Be honest with you, M$ made it up to make up for years of BAD CODE. Linus never bothered to fix his kernel either apparently.The fix is simple enough. KEEP kernel DATA AWAY from kernel CODE. We can do this in USERLAND also, I havent gotten that far yet. FIX seems to run ok. IGNORED for now.

You can start learning to code from here, assuming some code will not port to FPC:
http://www.learn-programming.za.net/programming_pascal_learn01.html

If you can learn TP/BP7 and calculate LINEAR addresses correctly, Free Pascal and TP32 will work with you.

C is DIFFERENT, not BETTER. C is SLOPPY RUSHED, INSECURE by DEFAULT. The compiler has no default checks enabled.

Why is it so hard to typecheck and range check C?
APPARENTLY, C, according to ARDUINO DEVS is ALSO A JOKE!!! HA HA!

So what if PASCAL is a typed language and this makes development HARDER...we can do this the RIGHT way now. We can spend LESS time debugging our code and patch SECURITY holes FASTER.

The proof is in the code. Laugh at me all you want.

Some of the C DEVS on the net LIE, or provide you with INCORRECT information.OSdev forums are KNOWN for this. I have discovered multiple issues with thier unfixed code. GRUB MULTIBOOT SPECIFICATION NO EXCEPTION.

OSDEV BS LINE of the year seems to be: 'UH UH not showing you. ONLY saying how it COULD be done.NO examples here.WE ASSUME you are in CLASS for this.'

TRIPLE CHECK everything against a HARDWARE specification MANUAL if unsure.Then DOUBLE CHECK that manual is both up to date and accurate.

Most of Documentation used is INSIDE the Subversion tree.

OH, VBE3 PM Entry point should be working now(IFFFFF its available.).Please note its limitations.You can still do 64K bank switching in Protected Mode. Using this, (not common to find the EP..) is hell and a handbasket, it requires us to override the GDT yet again while we are using VRAM. NOT GONNA HAPPEN. REMOVED for now, as it complicates video buffer and screen access.(not using my ARDUINO for debugging..yet.)

B800(VRAM) on 16-bit access requires us to be IN 16-bit mode. This is not possible at the moment. DO NOT swet a 16-bit GDT entry here or things will get hairy.

COMM PORT sources are UNTESTED but should work ok.

TPC32:
http://turbo51.com/compiler-design/tpc16-turbo-pascal-compiler-written-in-turbo-pascal
NOTE: TPC32 requires HDLDR32 from HX DPMI to run in Freedos.

YES, I have working BP7 on modern hardware with Freedos available. I have personally patched the CRT unit to run > 3.6GHZ safely and smoothly.I dont have anything faster to test this patch on, and I doubt netbooks will be punching that much speed for ahwile. I also have the elusive newfrontier unit. Im using FPC still, even though I have alternative TPC32 at my disposal at this time.

GRUB .98 has VESA issues and needs to be patched.Version 1.99 not tested and ISOLINUX not fully tested, I have noted issues with the latter two.I have had issues patching GRUB .98, however, and there is a NOTED BUG.It seems to be fixed in .97 sources but in actuality I cant get VESA data yet.

Will post patched TP7 CRT UNIT shortly.

I also do some embedded programming unrelated to this OS.

--Jazz