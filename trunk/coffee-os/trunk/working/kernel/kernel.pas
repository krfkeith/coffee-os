
{ COFFEE OS Kernel }

Program Kernel;

{ NOTE: scroll screen unless otherwise disabled for menu or screen navigation }

uses 
  x86,
  multiboot,
  console,
  uconsole,
  idt,
  isr,
  HWprinters,
  irq,
  timer,
  keybrd,
  mouse,
  rtc,
  pmm,
  vmm,
  commands,
  heap,
  syscalls,
  cpuid,
  playmusic,
  HWcoms;

var


  MagicNumbers: LongWord; export name 'MagicNumbers';

 // ret:TTask;
  CurSorPosX,CurSorPosY:word;
  ismultithread,isUserMode,ispagingenabled :boolean;
  scrolldisabled:boolean;
  IsTaskingActive:boolean;

 // checkname:string32;
 // strdata:string;

//   boot,checkid:integer;
   floatused,currentpid,gotpid:integer;
//   initrd_location,initrd_end:longword;
//   fs_root:PtrUInt;
   BootTicks:longint;

      j,i:integer;
      dirent:pointer; //=nil
 //     fsnode:Tfs_node
      sz:longint;
      buf:array [1..256] of char;
	 

procedure switch_to_user_mode; external name 'switch_to_user_mode';
procedure EnablePaging; external name 'EnablePaging';
Procedure test_user_function; [public, alias: 'test_user_function'];
var
 strdata:string;

begin

  IsUserMode:=true;
  strdata:='User-Mode.';
  asm
    //writestring
	mov eax,01
	mov esi,[strdata]
	int 80
    //halt
    mov eax,06
    int 80
  end;
  //ideally the input and console loop would be here.
  //we dont exit. We are tripped into ring0, or we call ring0 functions.
  //Then we far return back as if we were here all along.
end;


begin
   EchoToConsole:=true;
   //floatused:=1;
   ismultithread:=false;
   scrolldisabled:=false;
   asm

      //Get stack pointers (64KB EACH)
      mov StartESP,edx
      mov UserESP,ecx
      mov VBIOSESP,eax
	  
   end;

  InstallConsole;

  if MagicNumbers <> MultiBootloaderMagic then begin
      writelongln(MagicNumbers);
	  writestrln('EXPECTED: $2BADB002');
    //  writelong(MultiBootloaderMagic);
      
      TextColor(Red);
      writestrln('Halting system, a multiboot-compliant boot loader needed!');
      writestrln('AKA: you didnt use grub.');

      asm
        cli
        hlt
      end;

  end;


{

Startup sequence:

@7C00:(BIOS Boot Sector)
Boot.bin locates stage 2 loader and jmp to it

Stage2:
16-bit RM
PM JUMP(32)[GRUB]

PASCAL:
IDT/GDT loaded
TSS Init/Load
(HERE)


Here is how to setup VBE PM access and EP:

   //To enable 32-bit VBIOS EP: (ASSUMING there is an EP...)
	asm
	//SETUP INITIAL PMID DATA, NASM wont let us do it another way..
	 mov [PMInfoBlock.Signature], 'PMID'
	 mov [PMInfoBlock.A000Sel], 0xA000
	 mov [PMInfoBlock.B000Sel], 0xB000
	 mov [PMInfoBlock.B800Sel], 0xB800
	 mov [PMInfoBlock.CodeSegSel], 0xC000
	end;
	
	var
	  scansegC000:logint;

    scanSegC000:=$C000;
	repeat
      if (PMInfo.Signature='PMID') then begin
			        writeln('A protected mode entry point has been found at ');
                                writeln(scansegC000);
			        break;
		        end;
      inc(scansegC000,4);
    until scansegC000>= $FFFF; //scan C000 thru adaptor ROM
	//we should have hit VBIOS @C000....
	
	writestrln('NO PROTECTED VBE3 MODE ENTRY POINT.');
	exit;

    asm	
       mov [PMInfoBlock.A000Sel],0xA000
       mov [PMInfoBlock.B000Sel],0xB000
       mov [PMInfoBlock.B800Sel],0xB800
       mov [PMInfoBlock.BIOSDataSel], 0x6000  //BIOS shadow copy area

       mov [PMInfoBlock.CodeSegSel],0xC000
       mov [PMInfoBlock.InProtectMode],1
       mov ss,VBIOS_ESP
       mov sp,0
    
       call [C000:PMInfoBlock.PMInititialize] //--> init our VBIOS..(A far pointer)
       call [C000:PMInfoBlock.EntryPoint] //--> now our new int10 equivalent(A far pointer)
  // now we can do int10 calls and whatever.
	end;

}

 
   //kernel start (1.20MB)
   writeline;
   textcolor(5);
   writestrln('Multiboot information');
   writestrln('---------------------');
   textcolor(8);
   writeline;
   if ((mbinfo^.flags and 1)=1) then begin 
      writestring('Boot Device = ');
      TextColor(13);
      writeintln(ord(mbinfo^.bootDevice[2])); //byte one is drive booted from.		
      TextColor(8);
   end;
{To get the full information use this:

     i:=1;
     textcolor(red);
     repeat
        if ((ord(mbinfo^.bootDevice[i]))<>255) and ((ord(mbinfo^.bootDevice[i]))>80) and ((ord(mbinfo^.bootDevice[i]))<90) then begin
//values set at 255 are useless.
//CD boot device should yield: 83 in field 3.
// we dont want to see partition type here, but they are passed to us.
          writeint(ord(mbinfo^.bootDevice[i]));
          writechar(#32);
        end;
        inc(i);
     until i=4;
}
//     textcolor(8);
//     writeline;

 if ((mbinfo^.flags and 2)=1) then begin 
//WE can do a VESA VERSION PROBE....
//Hackish until I parse strings and redo the strings units. But it works.

	writestring('VESA version ');
	writedword(hi(VESAInfo^.version));
	writestring('.');
	writedword(lo(VESAInfo^.version));
	writestrln(' detected.');

//Maybe this is ALLOCATED AREA? 
   writestring('VESA Memory size = ');
   TextColor(13);
   writedword(mbinfo^.vbe_interface_len);
   TextColor(8);
   writestrln(' KB');
 
   if (modeinfo^.PhysBasePtr<>nil) then begin //NON-ZERO if VESA/VBE Present.
        textcolor(2);
      	writestring('VESA Linear FrameBuffer @: ');   
        writedwordln(longword(modeinfo^.PhysBasePtr));
	    textcolor(8);
   end;
   
   if (PMInfo^.EntryPoint<>0) and (PMInfo^.Checksum<>0) then begin
      writestring('VESA PM Entry Point FOUND @:');
	  writedwordln(PMInfo^.EntryPoint);
   end else begin
        textcolor(4);
      	writestrln('NO PROTECTED VBE3 MODE ENTRY POINT.');   	  
	    textcolor(8);
	end;
//  reports 0 as it should, we are in TEXT mode.	
//	writestring('Current VESA MODE: ');
//	writedwordln(mbinfo^.vbe_mode);
   end;
   
//clear and (init) MAth CoProcessors
  writeportb($f0,$00); //clear busy signal
  writeportb($f1,$00); //reset

   writestring('Kernel 4KB Stack @: ');
   textcolor(4);
   writelongln(StartESP);
   textcolor(8);
    	
   writestring('User-Mode 4KB Stack @: ');
   textcolor(4);
   writelongln(UserESP);
   textcolor(8);
	
      writestring('Lower memory <1MB = ');
	
      TextColor(13);
      writedword((mbinfo^.LowerMemory)-1 );
      writestrln(' KB');

      TextColor(8);

      writestring('Maximum Available RAM: ');
      textcolor(13);
      writedword((((mbinfo^.Uppermemory div 1024)div 1024)div 1024)); 
      writestrln(' GB');
      textcolor(8);


   
  
   if ((mbinfo^.flags and 6)=1) then begin //we have a valid mem map from GRUB!
      FindUsableRAM(mbinfo);
   end;
  
 
  BootTicks:=0; // helps with UpTime command.
  InstallTimer;
  InstallIDT;
 // InstallRTC;
  InstallISR;
  InstallIRQ; //tell all HW to accept interrupts. 
  	
  //InstallPMM(StartESP); 

  //InstallkHeap; //Setup Heap(and Maps it in)
  //InstalluHeap; //--Userland

   // EnablePaging; //this is external in prt0 at the moment...(works)
   // IsPagingEnabled:=true; //mov IsPagingEnabled,1


//ideally we want PAE:
// kDetectCPUID; //QEMU does *NOT* have PAE.

// if CPUHasPAE then InstallPAEVMM else InstallVMM; //Setup paging 

//PAE version is coming in a little while..SIZE differences and multiple Directory entries abound...
//(due to >4GB memory addressing)


//Qemu sets up by default Com1 and LPT1.
 // InstallPorts; //Setup com and printer ports
//  if InitPrinter then writestrln('Printer Code init ok.'); //There really isnt much init code.
//  bochs_debugline('** COM/LPR and EMULATED COM/LPR ONLINE ** '); //only works if running the bochs.rc file...


//init_syscalls; //useless withuot ring3, which requires VMM WORKING correctly.
// InitTasking;


//switch_to_user_mode; 
//drops to kernel mode on int 80 or timer(HW triggered) event.

IsConsoleActive:=true;  //InActive for sleep/locked terminal,etc.
TextColor(Yellow);
  tabulate;
  writestrln('Type "help" for command list, or "license" for licence.');
  writeline;
  TextColor(7);
  writestring('Coffee > ');
 
// Initialise the initial ramdisk, and set it as the filesystem root.
//  fs_root := initialise_initrd(initrd_location); --The FAMOUS 'MOUNT' command...


//THIS WILL COME INTO PLAY LATER ON...
//useage: ATADetectPartition(0,[partnum]);
//if Minors[Minor+I].FsType:= 83 then writeln('found root ext FS v2 on HD.')
// fs_root:=mount_ext2(0,0,partitionNum); --we mounted root YAY!!!

//find a way to use and turn on swap.
//if Minors[Minor+I].FsType:= 82 then writeln('found swap space.')


//ENABLE (X11) Graphix display:

//InitGraph; --must store old video to restore it later.(one screen, multiple VTs)
//only ONE call to InitGraph is allowed.

//SetModeVESA($4112,true);
//autofill and check in mode list 112 (w/LFB), call clearGraph

//{$define USEGUI}
//RenderX11BackGround;
//Display PIXMAP(filename.png) --default background.
//RenderIcons;
//ShowMouse;

//SelectGUIModel; --provide a 'use last model' option.
//LoadGUIModel(default) //LCARS,etc...user selected.
//StartGUI(default) //the one we just selected.

//IT IS UP TO GUI MODEL to handle Rendering of buttons, etc.

  while true do;
//do NOT change this loop.It ensures the kernel stays running.

//if the kernel exits, note that QEMU generally complains with a "Memory execution outside of RAM/ROM error."
//Its safer to use ACPI poweroff.
end.

