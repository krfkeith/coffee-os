{
/////////////////////////////////////////////////////////
//                                                     //
//               Freepascal barebone OS                //
//                      kernel.pas                     //
//                                                     //
/////////////////////////////////////////////////////////
//
//      By:             De Deyn Kim <kimdedeyn@skynet.be>
//      License:        Public domain
//
}
 
unit kernel;
 
interface
 
uses
        multiboot,
        console,
	gdt,
        idt,
	isr,
	irq,
	heap,
	pmm,
	vmm;

procedure kmain(mbinfo: Pmultibootinfo; mbmagic: DWORD); stdcall;
 
implementation
 
procedure kmain(mbinfo: Pmultibootinfo; mbmagic: DWORD); stdcall; [public, alias: 'kmain'];
begin
        kclearscreen();
        kwritestr('Freepascal barebone OS booted!');
        xpos := 0;
        ypos += 1;
        installgdt;
	installidt;
	installisr;
        installirq;
        
        asm
		sti   //a good test     
	        @loop:
                jmp @loop
        end;
end;
 
end.
