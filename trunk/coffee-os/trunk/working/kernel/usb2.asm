use16
org 600h

;You may notice that some of this is Early ELF or Stage3 Kernel code.
;Due to unforeseen issues with FPC this code has been moved here.
; ALSO: Most PASCAL compilers ASSUME Ring3 execution. Not executing Ring3 code may cause issues.
; PASCAL Assumes all GDT entries are LDT entries normally.
; This would not cause problem, except we NEED Ring0 access to fix some issues.
; This STILL needs some work and I need another THUMB drive to test this with.
; Doing work on Pascal Pro Compiler in meantime.
; --Jazz

jmp stage2
nop

section .text ;NO-EXECUTE RW
; I know this code is repeated, we have unloaded the bootloader by now and need it back.

bpbOEM:			db "MSDOS5.0  "
bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 11
bpbNumberOfFATs: 	DB 2
bpbRootEntries: 	DW 224
bpbTotalSectors: 	DW 2880
bpbMediaId: 		DB 0xf0 ;f8 for HDD  
bpbSectorsPerFAT: 	DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0 
bsUnused: 		DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:	        DD 0xa0a1a2a3
bsVolumeLabel: 	        DB "COFFEEOSDISK"
bsFileSystem: 	        DB "FAT12   "

stage2got:
  db "Stage2 Loaded.",13,10,0
LoadingMsg:
	db "Searching for Kernel file...", 0
MsgFailure:
	db  " *** FATAL: Missing or corrupt COFFEE file.*** ",0
rebootwait:	
	db "Press Any Key to Reboot.",13, 10, 0
a20enabling:
	db  "Unlocking A20 gate.",13,10, 0
gdtinstall:
	db  "Installing GDT..",0
memgot:
	db  "Getting memory info...",0

jump31:
	db  "Jumping to Protected Mode", 13,10,0
novesa:
	db	"There is NO VESA. ABORT(1)",13,10,0
error1:
	db	"General VESA error.ABORT(3)",13,10,0
error2: 
    db	"VESA Function not supported.ABORT(2)",13,10,0
error3:
	db 	"VESA Function Call FAILED. ABORT(4)",13,10,0  
inPM:
		db "We are In Protected Mode.",0
inPM2:
		db"We are back from Unreal Mode.",0		
inPG:
		db "Paging Enabled @ FF00 0000 [Kernel PT=1020].",13,10,0
probing:
 	db "Probing VBE Video VESA(and Video BIOS)...",0
COFFEEBOOTstr:
	db "COFFEEBOOT",0 ; we can check for this later in MultiBoot Specs given to PASCALMAIN.

;32bit strings, 16bit ones have a colon..	
no32EP: 
   db "There was no 32-bit Entry Point for VBE functions found."
BadImage: 
   db "*** FATAL: Invalid or corrupt kernel image. Halting system.", 13,10,0

back16:
 db "In UnRealMode(4GB Limit) with Pseudo GPF Disabled...",13,10,0
    
donestr:
  db "...done. ",13,10,0	
      
bit32ok db "Copying kernel to 1MB mark..",0
CallKernel db "Launching Loaded Kernel...",0
  
section .code ;executing code area(RO area, no executing for non ring0 proceses)
  
stage2:
  mov si,stage2got
  call stage2_print
  call stage2b
  jmp $

stage2_print:
	mov ah, 0x0E	
	xor bh, bh	
.nextchar	
	lodsb		
	or al,al
	jz .return
	int 10h	
	jmp .nextchar
.return		
  ret

; where the kernel is to be loaded to in protected mode
%define IMAGE_PMODE_BASE 0x100000

RMIVTPTR:                                 ; IDT table pointer for 16bit access
    dw 0x03FF                              ; table limit (size)
    dd 0x00000000                          ; table base address

IDTPtr:
                                      ; IDT table pointer for 32bit access
    dw 0x0000                              ; table limit (NULL--prevent 16bit IVT from being called)
    dd 0x00000000                          ; table base address(YES, overrides RM IVT)

RMGDTR: ;RM has no GDT!
   dw 0x00
   dd 0x00
   ;4th word not set
   
gdtr:   
   dw gdt_end - gdt - 1   ; GDT limit
   dd gdt                  ; (GDT base gets set above)

; null descriptor
gdt:   ;4 words in size
   dw 0         ; limit 15:0
   dw 0         ; base 15:0

   db 0         ; base 23:16
   db 0         ; type

   db 0         ; limit 19:16, flags
   db 0         ; base 31:24

; 'Got to keep 'em separated...' (prevents malicious code)
; PERIOD. WE do what other OSes DO NOT. KEEP YOU SAFE!

; asumption is for pageTable 1020, adjust for pageTable 4 if we need to stay low
; 32MB RAM MINIMUM TO RUN, SO DONT FORGET TO CHECK FOR IT.
; Double this to 64MB when we setup and use usermode(Ring3).

;This assumes FULL 4GB VMM space is allocate-able, you CPUs PMU should give us hell on address 
;translation/access otherwise(via GPF).Enable SWAP on GPF if you have < 4GB RAM.

; The next few addresses get whacky and here is why:

;Base is a 32 bit value split between bits 16-39 and bits 56-63 of the descriptor.
;Limit is a 20 bit value split between bits 0-15 and bits 48-51 of the descriptor.
;Offset is a 32 bit values split between bits 0-15 and bits 48-63 of the descriptor.

;Selector is a 16 bit value in bits 16-31 of the descriptor.
;(386=>286 compatibility.Still think we can run 8088 code....)

;There is certain MATH to this:
; use mbasecalc.exe for windows(or under wine) to get these addresses

;  (From PASCAL sources)
;    LowLimit := (Limit and $FFFF);
;    LowBase := (Base and $FFFF);
;    MiddleBase := (Base shr 16) and $FF;
;    Access := Acc; //access type
;    Granularity := ((Limit shr 16) and $0F) or (Gran and $F0);
;    HighBase := (Base shr 24) and $FF;
;
; In assembler now:

; This is 32bit DWORDs where we want them(in comments), 
;we convert the addresses accordingly to the actual address using the calculator I mentioned.
    
;mov dword Limit,
;mov dword Base,

; Equivalent code to setup the GDT_Entry record/struc, we will just plug in this result
; as you can see below.

; This is a HEADACHE doing this from PASCAL as PASCAL is under LDT in RING3..
; so how do we otherwise setup a OS-level GDT? We CANNOT from within PASCAL.
; This code as well, HAS to be in assembler. Failure to do this
; will cause RANDOM UNEXPLAINABLE GPFs...just what we have been getting
; for a LOOONG TIME now. Still cannot explain how C is 16 AND 32 bit....
; PASCAL uses ONE or the OTHER. NOT both.

;mov word [gdt_entry.limit_low],[Limit and $FFFF] 
;mov word [gdt_entry.base_low], [Base and $FFFF]  

;mov byte [gdt_entry.base_middle],[Base >> 16]
;mov byte [gdt_entry.base_middle],[gdt_entry.base_middle and $FF]

;mov byte [gdt_entry.access],
;mov byte [gdt_entry.granularity],

;mov byte [gdt_entry.base_high],[(Base >> 24)]
;mov byte [gdt_entry.base_high],[gdt_entry.base_high and $FF]
;

; code segment descriptor
SYS_CODE_SEL   equ   $-gdt
gdt2:   

 ;  dd 0x1200000           ;12MB (WAY more than enough)    
 ;  dd 0xFF000000         ; base 
 
   dw 0
   dw 0
   db 0
   db 0x9A         ; present, ring 0, code, non-conforming, readable
   db 0xCF                 ; page-granular, 32-bit
   db 0


; data segment descriptor
SYS_DATA_SEL   equ   $-gdt
gdt3:   
 ;  dd 0x200000           ;2MB    (Im overly GENEROUS)
 ;  dd 0xFDE00000         ; base

   dw 0
   dw 0
   db 0xF7
   db 0x92         ; present, ring 0, code, non-conforming, readable
   db 0xCF                 ; page-granular, 32-bit
   db 0

USER_CODE_SEL   equ   $-gdt
gdt4:   
   ;dd 0xC1800000               ; limit 3GB(we still need room for PCI address space and VRAM space)
   ;dd 0xFDC00000         ; base
   
   dw 0
   dw 0
   db 0xF7
   db 0xFA         ; present, ring 0, code, non-conforming, readable
   db 0xCF                 ; page-granular, 32-bit
   db 0


; data segment descriptor
USER_DATA_SEL   equ   $-gdt
gdt5:   
  ; dd 0x20000000               ; limit 512MB
  ; dd 0x3C400000         ; base @ [3.5GB+32MB] 
   dw 0
   dw 0
   db 0xF1
   db 0xF2         ; present, ring 3, data, expand-up, writable
   db 0xCF                 ; page-granular(4K), 32-bit
   db 0
   
; a code segment descriptor that is 'appropriate' for real mode
; 16-bit data CORRECTED by NAPALM
; no protection for RM code
REAL_CODE_SEL   equ   $-gdt
gdt6:   
   dw 0xFFFF
   dw 0         ; (base gets set above)
   db 0
   db 0x9A         ; present, ring 0, code, non-conforming, readable
   db 0x0F         ; byte-granular, 16-bit
   db 0

; a data segment descriptor that is 'appropriate' for real mode
; (16-bit, byte-granular, limit=0xFFFF)
REAL_DATA_SEL   equ   $-gdt
gdt7:   
   dw 0xFFFF
   dw 0         ; (base gets set above)
   db 0
   db 0x92         ; present, ring 0, data, expand-up, writable
   db 0x0F         ; byte-granular, 16-bit
   db 0

;Needed for VBE 32PM EP
;FROM an OLD OLD DOS SPEC:
; A000 = GRAPH MODE MEMORY
; B000 = MONO TEXT MODE (USUALLY FREE for EMM386 USE in DOS)
; B800 = COLOE TEXT MODE
;NOTE: these three are for REAL MODE or bank switched PM

; PM uses VBEModeInfo.PhysFBPointer (LFB) instead(where supported)
;otherwise uses these and VBEModeInfo.BankSwitch to switch banks
 
VIDEO_SEL_B800	equ	$-gdt
gdt8:
   dw 0xffff ;32K Limit 
   dw 0x0b80
   db 0
   db 0x92 ;Ring3 RW
   db 0x0F ;Byte GRANULAR, 16bit Access
   db 0

VIDEO_SEL_A000	equ	$-gdt
gdt9:
   dw 0x7fff ;64K Limit 
   dw 0x0A00
   db 0
   db 0x92 ;Ring3 RW
   db 0x0F ;Byte Granular,16Bit Access
   db 0
   
;This CHUNK NOT USED (...WHO uses BW TEXT AREA THESE DAYS?)
;Maybe useful as debugging console? [CTRL-ALT-F12??]

VIDEO_SEL_B000	equ	$-gdt
gdt10:
   dw 0x7fff ;64K Limit 
   dw 0x0B00
   db 0
   db 0x92 ;Ring3 RW
   db 0x0F ;Byte Granular,16Bit Access
   db 0

VIDEO_SEL_BIOSINIT	equ	$-gdt
gdt11:
   dw 0x7fff ;64K Limit 
   dw VBIOS_BUFFER 
   db 0
   db 0x9A ;Ring3 RO CODE SEL
   db 0x0F ;Byte Granular,16Bit Access
   db 0

;For VBIOS 32-bit EP
 
VBIOS32	equ	$-gdt
gdt12:
   dw 0x7FFF ; Protected mode stack for 32-bit VBIOS calls (64K)
   dw VBIOS_STACK
   db 0
   db 0x92         ; present, ring 0, data, expand-up, writable
   db 0x0F         ; byte-granular, 16-bit
   db 0


VBIOSDATASEL	equ	$-gdt
gdt13:
   dw 0x7FFF ; VBIOS Data Selector (64K)
   dw VBIOS_DS
   db 0
   db 0x92         ; present, ring 0, data, expand-up, writable
   db 0x0F         ; byte-granular, 16-bit
   db 0

TSS_SEL		equ	$-gdt
gdt14:
	dw 0x103
	dw 0
	db 0
	db 0x89			; Ring 0 available 32-bit TSS
	db 0
	db 0
		   
gdt_end:

; Even if you don't use TSS-based task-switching, you need one
; TSS to hold the user stack pointer.

tss:
	dw 0, 0			; back link

	dd 0			; ESP0
	dw SYS_DATA_SEL, 0		; SS0, reserved

	dd 0			; ESP1
	dw 0, 0			; SS1, reserved

	dd 0			; ESP2
	dw 0, 0			; SS2, reserved

	dd 0			; CR3
	dd 0, 0			; EIP, EFLAGS
	dd 0, 0, 0, 0		; EAX, ECX, EDX, EBX
	dd 0, 0, 0, 0		; ESP, EBP, ESI, EDI
	dw 0, 0			; ES, reserved
	dw 0, 0			; CS, reserved
	dw 0, 0			; SS, reserved
	dw 0, 0			; DS, reserved
	dw 0, 0			; FS, reserved
	dw 0, 0			; GS, reserved
	dw 0, 0			; LDT, reserved
	dw 0, 0			; debug, IO perm. bitmap
tss_end:	
	
tssPtr:
	dw tss_end - tss - 1	; IDT limit
	dd tss			; linear, physical address of IDT

	
; macro for ISRs without error code
%macro ISRWithoutErrorCode 1
global isr%1
isr%1:
  cli
  push byte 0
  push byte %1
; mov ebx,[48+esp]
;  cmp word [ebx - 2],$80CD
  jmp ISRCommonStub   ; we got here on anoher interrupt, so handle other interrupts (jne)
 ; jmp  SysCallCommonStub 
%endmacro

; macro for ISRs with error code
%macro ISRWithErrorCode 1
global isr%1
isr%1:
  cli
  push byte %1
; mov ebx,[48+esp]
;  cmp word [ebx - 2],$80CD
  jmp ISRCommonStub   ; we got here on anoher interrupt, so handle other interrupts (jne)
;  jmp  SysCallCommonStub 
%endmacro

ISRWithoutErrorCode  0 ; Division By Zero Exception
ISRWithoutErrorCode  1 ; Debug Exception
ISRWithoutErrorCode  2 ; Non Maskable Interrupt Exception
ISRWithoutErrorCode  3 ; Breakpoint Exception
ISRWithoutErrorCode  4 ; Into Detected Overflow Exception
ISRWithoutErrorCode  5 ; Out of Bounds Exception/PrintScreen
ISRWithoutErrorCode  6 ; Invalid Opcode Exception
ISRWithoutErrorCode  7 ; Processor Extension Not Available
ISRWithErrorCode     8 ; DOUBLE FAULT-possibly TRIPLE FAULT
ISRWithoutErrorCode  9 ; Keyboard/ CoProcessor Code Error
ISRWithErrorCode    10 ; Video Mode/CoProcessor Error
ISRWithErrorCode    11 ; Segment Not Present /Alignment Error/NotImp Exception
ISRWithErrorCode    12 ; Machine Check/Segfault Exception
ISRWithErrorCode    13 ; General Protection Fault Exception
ISRWithErrorCode    14 ; Page Fault Exception
ISRWithoutErrorCode 15 ; Unknown Interrupt /Reserved Exception
ISRWithoutErrorCode 16 ; Extended Keyboard Exception
ISRWithoutErrorCode 17 ; Reserved
ISRWithoutErrorCode 18 ; Machine Check Exception
ISRWithoutErrorCode 19 ; Bootstrap Reserved.Trip to reboot machine.
ISRWithoutErrorCode 20 ; Reserved
ISRWithoutErrorCode 21 ; Unused- for DOS
ISRWithoutErrorCode 22 ;  DOS Terminate Address-UNUSED
ISRWithoutErrorCode 23 ;  CTRL-Break Handler
ISRWithoutErrorCode 24 ; Critical DiskIO Error Handler
ISRWithoutErrorCode 25 ; ABS Disk Read
ISRWithoutErrorCode 26 ;  ABS Disk Write
ISRWithoutErrorCode 27 ; Dos TSR-UNUSED
ISRWithoutErrorCode 28 ; DOS (Delay) routine.Reprogrammed to ours.
ISRWithoutErrorCode 29 ; Fast Console Write 
ISRWithoutErrorCode 30 ; Reserved
ISRWithoutErrorCode 31 ; DPMI memory routines
ISRWithoutErrorCode 80 ;(Syscalls) 80h=128

;These two should get replace later on by the PASCAL versions or reimplemented here.

ISRHandler:
    MOV             AL, 20H
    OUT             020H, AL
    ret

IRQHandler:
    MOV             AL, 20H
    OUT             20H, AL
	; if irq > 9
    ;MOV             AL, 20H
    ;OUT             0xA0, AL
    ret


global irq0
global irq1
global irq2
global irq3
global irq4
global irq5
global irq6
global irq7
global irq8
global irq9
global irq10
global irq11
global irq12
global irq13
global irq14
global irq15

ISRCommonStub:
  pusha
  push ds
  push es
  push fs
  push gs

  mov  ax,0x10
  mov  ds,ax
  mov  es,ax
  mov  fs,ax
  mov  gs,ax
  mov  eax,esp
  push eax

  mov  eax,ISRHandler
  call eax

  pop  eax
; TSS not saved upon return to ring3, so do it.

 lea eax,[esp + 76]	; 19 dwords == 76 bytes
 mov [ESP],eax      ; Ring 0 ESP value after IRET

  pop  gs
  pop  fs
  pop  es
  pop  ds

  popa
  add  esp,8 
  sti
  iret

; I had these in a macro but because of the Timer interrupt with tasking, it had to be changed.  
irq1:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 33
    jmp  IRQCommonStub

irq2:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 34
    jmp  IRQCommonStub

irq3:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 35
    jmp  IRQCommonStub

irq4:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 36
    jmp  IRQCommonStub

irq5:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 37
    jmp  IRQCommonStub

irq6:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 38
    jmp  IRQCommonStub

irq7:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 39
    jmp  IRQCommonStub

irq8:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 40
    jmp  IRQCommonStub

irq9:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 41
    jmp  IRQCommonStub

irq10:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 42
    jmp  IRQCommonStub

irq11:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 43
    jmp  IRQCommonStub

irq12:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 45
    jmp  IRQCommonStub

irq13:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 46
    jmp  IRQCommonStub

irq14:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 47
    jmp  IRQCommonStub

irq15:
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 48
    jmp  IRQCommonStub

; Enables non-maskable interrupts
global enable_nmi
enable_nmi:
        MOV             AL, 0DH
        OUT             070H, AL
        JMP             $+2

; Disables non-maskable interrupts
global disable_nmi
disable_nmi:
        MOV             AL, 8FH
        OUT             070H, AL
        JMP             $+2

;IRQ:
; [0] Timer (110ms intervals, 18.2 per second)
; [1] Keyboard (also int33, correct old dos MOUSE code to point to int44/IRQ12)
; [2] slave 8259 or EGA/VGA vertical retrace
; [3] COM2 or COM4
; [4] COM1 or COM3 or mouse
; [5] fixed disk or LPT2
; [6] floppy disk
; [7] LPT1
; [8] Real Time Clock (CMOS)
; [9] software redirected to IRQ2
; [10] RESERVED(INTEL)
; [11] RESERVED(INTEL)
; [12] PS/2 mouse
; [13] numeric coprocessor error 
; [14] fixed disk controller
; [15] second fixed disk controller

irq0: ;system Timer interrupt
; This is just easier to code here in ASSEMBLER than pass the buck back and forth with PASCAL
    cli
    push byte 0    ; Note that these don't push an error code on the stack:
                   ; We need to push a dummy error code
    push byte 32
;A (Context switch) is moving stack to the next PID and then back after task_switch, like so.
; There is a reason we dont do it here.

;  cmp eax,1000 --System Timer freq in hertz(ms)
;  jne exit

; Otherwise, if system timer elapsed, switch tasks
;  mov esp,eax
;  mov eax,esp
;  push eax
;  call task_switch
;  mov esp,eax
   jmp exit

exit:	
    jmp  IRQCommonStub

IRQCommonStub:
  pusha
  push ds
  push es
  push fs
  push gs

  mov  ax,0x10
  mov  ds,ax
  mov  es,ax
  mov  fs,ax
  mov  gs,ax
  mov  eax,esp
  push eax

  mov  eax,IRQHandler
  call eax

  pop eax               ; here recall the esp as it was before the call
                          ; irq_handler  

; TSS not saved upon return to ring3, so do it.
 lea eax,[esp + 76]	; 19 dwords == 76 bytes
 mov [ESP],eax      ; Ring 0 ESP value after IRET
  
  pop gs
  pop fs
  pop es
  pop ds
  popa
  add esp, 8
;  sti
  iret ; so we come back to ring3, finctions as ret in ring0

;extern kernel_directory

global copy_page_physical
copy_page_physical:
   push ebx              ; According to __cdecl, we must preserve the contents of EBX.
   pushf                 ; push EFLAGS, so we can pop it and reenable interrupts
                         ; later, if they were enabled anyway.
   cli                   ; Disable interrupts, so we aren't interrupted.
                         ; Load these in BEFORE we disable paging!
   mov ebx, [esp+12]     ; Source address
   mov ecx, [esp+16]     ; Destination address

   mov edx, cr0          ; Get the control register...
   and edx, 0x7fffffff   ; and...
   mov cr0, edx          ; Disable paging.

   mov edx, 1024         ; 1024*4bytes = 4096 bytes to copy

.loopPagePys:
   mov eax, [ebx]        ; Get the word at the source address
   mov [ecx], eax        ; Store it at the dest address
   add ebx, 4            ; Source address += sizeof(word)
   add ecx, 4            ; Dest address += sizeof(word)
   dec edx               ; One less word to do
   jnz .loopPagePys

   mov edx, cr0          ; Get the control register again
   or  edx, 0x80000000   ; and...
   mov cr0, edx          ; Re-Enable paging.

   popf                  ; Pop EFLAGS back.
   pop ebx               ; Get the original value of EBX back.
   sti
   ret

     
bits 16
InstallGDT:


    xor ebx,ebx
	mov bx,ds                       ; BX=segment
	
	shl ebx,4                       ; BX="linear" address of segment base
	mov eax,ebx
	
    mov [gdt2 + 2],ax               ; set base address of 32-bit segments
    mov [gdt3 + 2],ax
    mov [gdt4 + 2],ax               ; set base address of 16-bit segments
    mov [gdt5 + 2],ax
    mov [gdt6 + 2],ax               ; set base address of 16-bit segments
    mov [gdt7 + 2],ax
    mov [gdt8 + 2],ax
    mov [gdt9 + 2],ax
    mov [gdt10 + 2],ax
    mov [gdt11 + 2],ax
    mov [gdt12 + 2],ax
    mov [gdt13 + 2],ax
    mov [gdt14 + 2],ax
	
	shr eax,16
	
    mov [gdt2 + 4],al
    mov [gdt3 + 4],al
    mov [gdt4 + 4],al
    mov [gdt5 + 4],al
    mov [gdt6 + 4],al               ; set base address of 16-bit segments
    mov [gdt7 + 4],al
    mov [gdt8 + 4],al
    mov [gdt9 + 4],al
    mov [gdt10 + 4],al
    mov [gdt11 + 4],al
    mov [gdt12 + 4],al
    mov [gdt13 + 4],al
    mov [gdt14 + 4],al

    mov [gdt2 + 7],ah
    mov [gdt3 + 7],ah
    mov [gdt4 + 7],ah
    mov [gdt5 + 7],ah
    mov [gdt6 + 7],ah               ; set base address of 16-bit segments
    mov [gdt7 + 7],ah
    mov [gdt8 + 7],ah
    mov [gdt9 + 7],ah
    mov [gdt10 + 7],ah
    mov [gdt11 + 7],ah
    mov [gdt12 + 7],ah
    mov [gdt13 + 7],ah
    mov [gdt14 + 7],ah
   
   lea eax,[gdt + ebx]             ; EAX=PHYSICAL address of gdt
   mov [gdtr + 2],eax

   lgdt[gdtr]
   ret	                 ; Because we are called, we flush later on.

LoadTSS:	
  ; Load the index of our TSS structure - The index is
   ; 0x68, as it is the 15th selector 
   ; , but we set the bottom two bits (+3)
   ; so that it has an RPL of 3, not zero.

   mov ax,0x6B
   ltr ax            ; Load into the task state register.
   ret
	
; Nothing fancy here, just load up after being NULL terminated..
; ZERO, then overrite(and reset vectors)...
	
InstallIDT:
   pusha
   lidt[idt]
   popa
   ret

;MUST be Ring0 before activating Ring3.
; This is STRAIGHT from the C in JamesM's kernel demo
; NOTE: MUST be in ASSEMBLER, within PASCAL code this causes issues...
   
global switch_to_user_mode
switch_to_user_mode:
   cli ;there is especial way to turn interrupts on for user mode, sti will NOT do it.
   mov dword eax,$23 
   mov word ds,ax
   mov word es,ax
   mov word fs,ax
   mov word gs,ax
   mov eax,UserESP
   push dword $23      
   pushf
   push dword $1b
   push dword eax 
   push dword $23 
   iret
jmp  0x1b:user

user:
  
  ; usermode will return to us ONLY on HW interrupt.(or syscall int 80)
  
  ; Make LDT here
  
  jmp $
  ; call Ukernel
  

idt:
	%rep 256

	dw 0			; low 16 bits of ISR address
	dw SYS_CODE_SEL		; selector
	db 0			; word count
	db 8Eh			; access byte: Present, Ring 0, '386 intr gate
	dw 0			; high 16 bits of ISR

	%endrep

	; since all 32 ints (INTEL SPEC) are accounted for, we are good..
idt_end:

idt_ptr:
	dw idt_end - idt - 1	; IDT limit
	dd idt			; linear, physical address of IDT

  
bits 16
_EnableA20:
; this isnt perfect and does probe every possible mode, but it works. --Jazz
        cli
		
        call    a20wait
        mov     al,0xAD
        out     0x64,al

        call    a20wait
        mov     al,0xD0
        out     0x64,al

        call    a20wait2
        in      al,0x60
        push    eax

        call    a20wait
        mov     al,0xD1
        out     0x64,al

        call    a20wait
        pop     eax
        or      al,2
        out     0x60,al

        call    a20wait
        mov     al,0xAE
        out     0x64,al

        call    a20wait
        sti
        ret

a20wait:
        in      al,0x64
        test    al,2
        jnz     a20wait
        ret


a20wait2:
        in      al,0x64
        test    al,1
        jz      a20wait2
        ret

; where the kernel is to be loaded to in protected mode (1MB)
%define IMAGE_PMODE_BASE 0x00100000

;---------------------------------------------
;	Get memory map from bios
;	/in es:di->destination buffer for entries
;	/ret bp=entry count
;---------------------------------------------

BiosGetMemoryMap:

	xor		ebx, ebx
	xor		bp, bp
	mov		edx, 0x0534D4150		; Place "SMAP" into edx
	mov		eax, 0xe820
	mov		[es:di + 20], dword 1		; force a valid ACPI 3.X entry
	mov		ecx, 24
	int		0x15
	jc		short .failed			; carry set on first call means "unsupported function"
	mov		edx, 0x0534D4150		; Some BIOSes apparently trash this register?
	cmp		eax, edx			; on success, eax must have been reset to "SMAP"
	jne		short .failed
	test	ebx, ebx				; ebx = 0 implies list is only 1 entry long (worthless)
	je		short .failed
	jmp		short .jmpin
.e820lp:
	mov		eax, 0xe820			; eax, ecx get trashed on every int 0x15 call
	mov		[es:di + 20], dword 1		; force a valid ACPI 3.X entry
	mov		ecx, 24				; ask for 24 bytes again
	int		0x15
	jc		short .e820f			; carry set means "end of list already reached"
	mov		edx, 0x0534D4150		; repair potentially trashed register
.jmpin:
	jcxz	.skipent				; skip any 0 length entries
	cmp		cl, 20				; got a 24 byte ACPI 3.X response?
	jbe		short .notext
	test	byte [es:di + 20], 1			; if so: is the "ignore this data" bit clear?
	je		short .skipent
.notext:
	mov		ecx, [es:di + 8]		; get lower dword of memory region length
	test	ecx, ecx				; is the qword == 0?
	jne		short .goodent
	mov		ecx, [es:di + 12]		; get upper dword of memory region length
	jecxz	.skipent				; if length qword is 0, skip entry
.goodent:
	inc		bp				; got a good entry: ++count, move to next storage spot
	add		di, 24
.skipent:
	test	ebx, ebx				; if ebx resets to 0, list is complete
	jne		short .e820lp
.e820f:
	ret						; bp=entry count
.failed:
	stc						; "function unsupported" error exit
	ret

;---------------------------------------------
;	Get memory size for >64M configuations (32 bit)
;	ret\ ax=KB between 1MB and 16MB
;	ret\ bx=number of 64K blocks above 16MB
;	ret\ bx=0 and ax= -1 on error
;---------------------------------------------

BiosGetMemorySize64MB_32bit:
	push	ecx
	push	edx
	xor		ecx, ecx
	xor		edx, edx
	mov		eax, 0xe881
	int		0x15
	jc		.error
	jcxz	.use_ax				;bios may have stored it in ax,bx or cx,dx. test if cx is 0
	mov		eax, ecx			;its not, so it should contain mem size; store it
	mov		ebx, edx

.use_ax:
	pop		edx				;mem size is in ax and bx already, return it
	pop		ecx
	ret

.error:
	mov		ax, -1
	mov		bx, 0
	pop		edx
	pop		ecx
	ret

;---------------------------------------------
;	Get memory size for >64M configuations
;	ret\ ax=KB between 1MB and 16MB
;	ret\ bx=number of 64K blocks above 16MB
;	ret\ bx=0 and ax= -1 on error
;---------------------------------------------

BiosGetMemorySize64MB:
	push	ecx
	push	edx
	xor		ecx, ecx
	xor		edx, edx
	mov		ax, 0xe801
	int		0x15	
	jc		.error
	cmp		ah, 0x86		;unsupported function
	je		.error
	cmp		ah, 0x80		;invalid command
	je		.error
	jcxz	.use_ax			;bios may have stored it in ax,bx or cx,dx. test if cx is 0
	mov		ax, cx			;its not, so it should contain mem size; store it
	mov		bx, dx

.use_ax:
	pop		edx				;mem size is in ax and bx already, return it
	pop		ecx
	ret

.error:
	mov		ax, -1
	mov		bx, 0
	pop		edx
	pop		ecx
	ret

;---------------------------------------------
;	Get amount of contiguous KB from addr 0
;	ret\ ax=KB size from address 0
;---------------------------------------------

BiosGetMemorySize:
	int	0x12
	ret

;---------------------------------------------
;	Get contiguous exetended memory size
;	ret\ ax=KB size above 1MB; ax= -1 on error
;---------------------------------------------

BiosGetExtendedMemorySize:
	mov		ax, 0x88
	int		0x15
	jc		.error
	test	ax, ax		; if size=0
	je		.error
	cmp		ah, 0x86	;unsupported function
	je		.error
	cmp		ah, 0x80	;invalid command
	je		.error
	ret
.error:
	mov		ax, -1
	ret


abort:
	mov si,novesa
	call stage2_print
	jmp $

abort2:
	mov si,error2
	call stage2_print
	jmp $

abort3:
	mov si,error1
	call stage2_print
	jmp $


abort4:
	mov si,error3
	call stage2_print
	jmp $
	
ProbeVBE:
	MOV AX, 4F00h
	mov dword [vbe_info.VBESignature],0x32454256 ;VBE2 request 
;mov dword [vbe_info.VBESignature,"2EBV" (READ it backwards..)

	mov word [vbe_info.VBEVersion],0x0300	;try v3 info
	LES DI, [vbe_info]
	INT 10h
	cmp ax,004fh ; Do we have VESA3?
	jne .1 ;no?
	je  .2 ;YES!

.1:
	; Lets try VESA 2 info...v3 not avail...FAIL if not found. VESA v1.2 is useless for us.
	MOV AX, 4F00h
	mov dword [vbe_info.VBESignature],0x32454256 ;VBE2 request
;mov dword [vbe_info.VBESignature,"2EBV"

	mov word [vbe_info.VBEVersion],0x0200	;request v2 info 
	LES DI, [vbe_info]
	INT 10h
	cmp ax,004fh ; Do we have VESA2?
	jne  abort ; no? abort.
    je .2 ; Yes? continue

.2:
	;ok, we have VESA2...

	; Return me the PM Info block for VESA2(if available)
	    les di,[PMInfoBlock]
        mov bl,0
        mov ax,4f0ah
        int 10
	
	cmp ah,01h ; call failed
	je  abort4

	cmp ah,02h ; call not supported(HW) ;[probably called vbe 2 functions, which dont have it]
	je  abort2

	cmp ax,004fh ; GEN VESA error 
	je  abort3

    ;need to get current row and save for later. When we set mode 83(mode3,no clear) row,col get reset otherwise	
	
	;gotoxy(dh,dl)
	;mov AH,0x02
    ;mov BH,0
    ;mov DL, [pos+80]  
    ;int 10h
	
	;save current mode for later(this code was ineffective, we are STILL in TEXT mode...)
	mov ah,0x0F
	int     10h
	mov byte [multiboot_info.vbe_mode],al ;put mode info here (byte->word location)
	
	
	ret

bits	32

; page directory table
%define		PAGE_DIR			0x9C000

; 0th page table. Address must be 4KB aligned
%define		PAGE_TABLE_0		0x9D000

; nth page table. Address must be 4KB aligned
%define		PAGE_TABLE_1020		0x9E000

; each page table has 1024 entries
%define		PAGE_TABLE_ENTRIES	1024

; attributes (page is present;page is writable; supervisor mode)
%define			PRIV				7
%define			USERPRIV			3

EnablePaging:
	pusha										; save stack frame

	;------------------------------------------
	;	idenitity map 1st page table (4MB)
	;------------------------------------------

	mov		eax, PAGE_TABLE_0					; first page table
	mov		ebx, 0x0 | PRIV						; starting physical address of page
	mov		ecx, PAGE_TABLE_ENTRIES				; for every page in table...
.loop:
	mov		dword [eax], ebx					; write the entry
	add		eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096							; go to next page address (Each page is 4Kb)
	loop	.loop								; go to next entry

	;------------------------------------------
	;	set up the entries in the directory table
	;------------------------------------------

	mov		eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		dword [PAGE_DIR], eax

	mov		eax, PAGE_TABLE_1020 | PRIV			; nth entry in directory table
	mov		dword [PAGE_DIR+(1020*4)], eax

	;------------------------------------------
	;	install directory table
	;------------------------------------------

	mov		eax, PAGE_DIR
	mov		cr3, eax

	;------------------------------------------
	;	enable paging
	;------------------------------------------
	
	mov		eax, cr0
	or		eax, 0x80000000
	mov		cr0, eax
	
	mov		eax, PAGE_TABLE_1020				; first page table
	mov		ebx, 0x100000 | PRIV			; starting physical address of page
	mov		ecx, PAGE_TABLE_ENTRIES			; for every page in table...
.loop2:
	mov		dword [eax], ebx				; write the entry
	add		eax, 4							; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096						; go to next page address (Each page is 4Kb)
	loop	.loop2							; go to next entry
	popa
	ret
	

bits 16

; *************** Jumped HERE from STAGE1 BOOT LOADER (but still in REAL MODE)*************************

disk_error:      
        mov  si,MsgFailure     ; string to print
		call stage2_print
		mov si, rebootwait
		call stage2_print
		mov     ah, 0x00
        int     0x16                                ; await keypress
        int     0x19                                ; warm boot computer
     	
Load3:
  	mov si, LoadingMsg
  	call stage2_print

	mov bp,2
	mov bx, 0x4000 ; put stage3 here
	mov cx,0x10 ; cyl=0 sect=10
  	xor dh,dh  ; head=0
  	mov ax,0x0206 ; read 06 sectors, ah function 2
    jmp four

four:		
    
    int 13h
    jnc five ; if there was no error, exit
    jc disk_error2

disk_error2:		
	; if three retries fail then there was an error.STOP.
    sub bp,1
    cmp bp,0
    je disk_error
	push ax
	xor     ax, ax                              ; BIOS reset disk(to clear carry flag)
    int     13h   
	pop ax
	jmp four

five:
  jmp threedone

stage2b:
    
	;   Setup segments and stack     
   	cli                        ; clear interrupts
    xor             ax, ax             ; null segments
    mov             ds, ax
    mov             es, ax
    mov             ax, 0x9000         ; stack begins at 0x9000-0xffff
    mov             ss, ax
    mov             sp, 0xFFFF
    sti                        ; enable interrupts
    
    jmp  Load3  ;Stage3 Loader Disk->RAM [1MB]
	
threedone:
	mov 	si, donestr
	call 	stage2_print
 	
	mov     byte    [multiboot_info.bootDevice], dl
	mov   esi,COFFEEBOOTstr
	mov     dword	[multiboot_info.bootloader_name],esi

	mov 	si, probing
    call 	stage2_print
	call	ProbeVBE
	mov             si, donestr
	call            stage2_print
	
    mov             si, memgot
    call            stage2_print
  
    call            BiosGetMemorySize64MB
    push            eax
    mov             eax, 64
    mul             ebx
	mov             ecx, eax
	pop             eax
	add             eax, ecx
	add             eax, 1024               ; the routine doesnt add the KB between 0-1MB; add it
	mov             dword [multiboot_info.memoryHi], 0
	mov             dword [multiboot_info.memoryLo], eax
	; cmp dword [multiboot_info.memoryLo],>64000000(64MB)
	; jne noMEM	
	mov             eax, 0x0
	mov             ds, ax
	mov             di, 0x1000
	call            BiosGetMemoryMap
	
	mov             si, donestr
	call            stage2_print
	
	cli	
    
	mov             si, a20enabling
	call            stage2_print
    call            _EnableA20
	
	; disable non maskable interrupts(NMI)
   ;	call disable_nmi
	mov             si, gdtinstall
    call            stage2_print

    ; Save the RM IVT for later..
   ; sidt[RMIVTPTR]	
    
   ; push ds	;save RM --we need it later
	
	call		   InstallGDT
	mov             si, donestr
	call            stage2_print
	
    call			InstallIDT
	; Next thing we should see is 'in protected mode', assuming the GDT/TSS is setup correctly...

; Int10 is now overridden, use protected mode access to VRAM or drop to RM for access..

	;If we have PAE then use it, most modern CPUs have it.
	;mov eax,1
	;cpuid
	;bt eax,6
	;je PAE
	;jne continue
;PAE:	
;	mov     eax, cr4                   ; Enable PAE or GDT limit overflows beyond end of RAM (>16GB)
;	bts     eax, 5
;	mov     cr0, eax
;    jmp continue
	
;continue:	

	mov     eax, cr0                   ; set bit 0 in cr0--enter pmode
	or      eax, 1
	mov     cr0, eax
	
    ;HERE is your FLUSH code...	FLUSH!
	jmp     0x08:Stage3                ; far jump to fix CS. Remember that the code selector is 0x8!
		
	; Note: Do NOT re-enable interrupts! Doing so will triple fault!
	; We will fix this in Stage 3.
		
bits 32
   
; IDT entry 23? (DPMI) points here

return16: 

    mov bx,0x10         ; selector to data segment w/ 4G limit
    mov ds,bx  ;set DS @ 4GB limit and then turn off paging.
    
	and al,0xFE
    mov cr0,eax  ; PM is now OFF!
    pop ds ;Get RM back
    sti 

;demo prints a smiley do whatever here(may have some limitations but allows 32/48 bit addressing)
    mov bx,0x0201
    mov eax,0xb8000 ;32-bit reference
    mov word [ds:eax],bx ;16:32 or 48 but reference to move the smiley into VRAM
    
    cli   
    or      eax, 1
    mov     cr0, eax   
    ret 

	;(with HELP from NAPALM)
	
resetpic:                                  ; reset's 8259 master and slave pic vectors
    push ax                                ; expects bh = master vector, bl = slave vector
    mov  al, 0x11                          ; 0x11 = ICW1_INIT | ICW1_ICW4
    out  0x20, al                          ; send ICW1 to master pic
    out  0xA0, al                          ; send ICW1 to slave pic
    mov  al, bh                            ; get master pic vector param
    out  0x21, al                          ; send ICW2 aka vector to master pic
    mov  al, bl                            ; get slave pic vector param
    out  0xA1, al                          ; send ICW2 aka vector to slave pic
    mov  al, 0x04                          ; 0x04 = set slave to IRQ2
    out  0x21, al                          ; send ICW3 to master pic
    shr  al, 1                             ; 0x02 = tell slave its on IRQ2 of master
    out  0xA1, al                          ; send ICW3 to slave pic
    shr  al, 1                             ; 0x01 = ICW4_8086
    out  0x21, al                          ; send ICW4 to master pic
    out  0xA1, al                          ; send ICW4 to slave pic
    pop  ax                                ; restore ax from stack
    ret   
		
goback16:

    mov  [KERNEL_STACK], esp        ; save 32bit stack pointer
    sidt [IDTPtr]               ; save 32bit idt pointer
    sgdt [gdtr]               ; save 32bit gdt pointer	
	lgdt [RMGDTR]               ; load 16bit gdt pointer
    lea  esi, [esp+0x24]                   ; set position of intnum on 32bit stack
    lodsd                                  ; read intnum into eax
    mov  [0x00], al                  ; set intrrupt immediate byte from our arguments
    mov  esi, [esi]                        ; read regs pointer in esi as source
    mov  edi, RM_STACK                      ; set destination to 16bit stack
    mov  ecx, regs16_t_size                ; set copy size to our struct size
    mov  esp, edi                          ; save destination to as 16bit stack offset
    rep  movsb                             ; do the actual copy (32bit stack to 16bit stack)
    jmp  word REAL_CODE_SEL:gogo16      ; switch to 16bit selector (16bit protected mode)

gogo16:
    use16
    mov ax,REAL_DATA_SEL
	mov ds,ax
	mov es,ax
	mov fs,ax
	mov gs,ax
	mov ss,ax
	mov  eax, cr0                          ; get cr0 so we can modify it
    and  al,  ~0x01                        ; mask off PE bit to turn off protected mode
    mov  cr0, eax    
	jmp word 0x0000:realREALRM

realREALRM:
    use16
    xor ax,ax
    mov ds,ax
    mov ss,ax
    lidt[RMIVTPTR]	
	mov  bx, 0x0870                        ; master 8 and slave 112
    call resetpic                          ; set pic's the to real-mode settings
    popa                                   ; load general purpose registers from 16bit stack
    pop  gs                                ; load gs from 16bit stack
    pop  fs                                ; load fs from 16bit stack
    pop  es                                ; load es from 16bit stack
    pop  ds                                ; load ds from 16bit stack
    sti     
;do whatever --or int immead

	
    cli
	xor  sp, sp                            ; zero sp so we can reuse it
    mov  ss, sp                            ; set ss so the stack is valid
	mov  sp, 9000                   ; set correct stack position so we can copy back(current stack location:GET)
    pushf                                  ; save eflags to 16bit stack
    push ds                                ; save ds to 16bit stack
    push es                                ; save es to 16bit stack
    push fs                                ; save fs to 16bit stack
    push gs                                ; save gs to 16bit stack
    pusha                                  ; save general purpose registers to 16bit stack
    mov  bx, 0x2028                        ; master 32 and slave 40
    call resetpic                          ; restore the pic's to protected mode settings
    mov  eax, cr0                          ; get cr0 so we can modify it
    inc  eax                               ; set PE bit to turn on protected mode
    mov  cr0, eax                          ; set cr0 to result
    jmp dword SYS_CODE_SEL:back2PMode
	
back2PMode:
    use32	
	mov  ax, SYS_DATA_SEL                        ; get our 32bit data selector
    mov  ds, ax                            ; reset ds selector
    mov  es, ax                            ; reset es selector
    mov  fs, ax                            ; reset fs selector
    mov  gs, ax                            ; reset gs selector
    mov  ss, ax                            ; reset ss selector
    lgdt [gdtr]               ; restore 32bit gdt pointer
    lidt [IDTPtr]               ; restore 32bit idt pointer
	
	mov  esp, KERNEL_STACK        ; restore 32bit stack pointer
    mov  esi, RM_STACK                      ; set copy source to 16bit stack
    lea  edi, [esp+0x28]                   ; set position of regs pointer on 32bit stack
    mov  edi, [edi]                        ; use regs pointer in edi as copy destination
    mov  ecx, regs16_t_size                ; set copy size to our struct size
    cld                                    ; clear direction flag (so we copy forward)
    rep  movsb                             ; do the actual copy (16bit stack to 32bit stack)
    popa                                   ; restore registers
    ;sti                                     enable interrupts
	ret
	
stage3_print:
    mov bx,0x08 ; the size of a GDT descriptor is 8 bytes
    mov fs,bx   ; fs = the 2nd GDT descriptor, a 4 GB data seg
	mov ebx,0xB8000 ; address of first char for VGA mode 3
	mov si,inPM ; si = message text
	ForEachChar:
		
		lodsb		; get next char
		cmp al,0x00	; if it's null, break
		je EndForEachChar
		
		mov [fs:ebx],al	; write char to display memory
		inc ebx		; 2 bytes per char
		inc ebx		; so increment twice
		
	jmp ForEachChar
	EndForEachChar:
	ret
	
Stage3:
	mov     ax, 0x10                ; set data segments to data selector (0x10)
    mov     ds, ax
    mov     ss, ax
    mov 	es,ax
  
	;call enable_nmi

	;call LoadTSS ;(have to be INSIDE protected mode to call this..)

   mov esi,inPM
   mov edi,[B8000+(9*80*2)]
   call stage3_print
	
 ; We can NOW enable interrupts, but there is no point yet without a proper IDT and Handler code.
	
    jmp $
    ; To enable 32-bit VBIOS EP: (FINALLY...)
    ; we can move this to PASCAL code later...the scanning section is easier to implement there...
	
	;SETUP INITIAL PMID DATA, NASM wont let us do it another way..
	; mov [PMInfoBlock.Signature], 'PMID'
	; mov [PMInfoBlock.A000Sel], 0xA000
	; mov [PMInfoBlock.B000Sel], 0xB000
	; mov [PMInfoBlock.B800Sel], 0xB800
	; mov [PMInfoBlock.CodeSegSel], 0xC000
	
    ;mov 0xC000,VBIOS_BUFFER
	
	;STEP TWO:
	;scan VBIOS_BUFFER for PMINFOBLOCK
	;how do we do this in asm?
	
;	cmp PMInfoBlock.Checksum,0
;	jne no32EPfound
	
 ;   mov [PMInfoBlock.A000Sel],GDT8
 ;   mov [PMInfoBlock.B000Sel],GDT9
 ;   mov [PMInfoBlock.B800Sel],GDT10
 ;   mov [PMInfoBlock.BIOSDataSel],GDT13
 ;   mov [PMInfoBlock.CodeSegSel],GDT11
 ;   mov [PMInfoBlock.InProtectMode],1
 ;   mov ss,VBIOS32
 ;   mov sp,0

  ; call Gdt11:PMInfoBlock.PMInititialize --> init our VBIOS..(A far pointer)
  ; call Gdt12:PMInfoBlock.EntryPoint --> now our new int10 equivalent(A far pointer)
	
;no32EPfound:
	
;	mov esi,no32EP
;	call stage3_print
	
; ALL of the structs[records] previously will have to be imported somehow(one at a time?)
; and declared GLOBAL so that PASCALMAIN calls these and not its own.
   
  ; call return16 (UNREAL MODE)[works]
  
  call goback16 ;REAL REAL MODE
  
  ;back now, no issues
	mov     ax, 0x10                ; set data segments to data selector (0x10)
    push ds ;AGAIN, save RM for later
	mov     ds, ax
    mov     ss, ax
    mov     es, ax

; jump to our kernel! 

; Dont forget to setup IDT/ISRs before enabling interrupts...
; We will Triple-Fault otherwise..

      jmp   0x08:4000	;stage3 code(the kernel,should only override the ISR/IDTs and setup IRQs..)
      jmp $

; call EnablePaging when ready from inside the kernel file. 
	   
;kernel DATA area --non-instruction(RW area)	   
section .bss ; 'better save space' for these...
;takes up about 300K or so...(memory)

struc multiboot_info
	.flags			resd	1
	.memoryLo		resd	1
	.memoryHi		resd	1
	.bootDevice		resd	1
	.cmdLine		resd	1
	.mods_count		resd	1
	.mods_addr		resd	1
	.syms0			resd	1
	.syms1			resd	1
	.syms2			resd	1
	.mmap_length		resd	1
	.mmap_addr		resd	1
	.drives_length		resd	1
	.drives_addr		resd	1
	.config_table		resd	1
	.bootloader_name	resd	1
	.apm_table		resd	1
	.vbe_info		resd	1
	.vbe_mode_info		resd	1
	.vbe_mode		resw	1
	.vbe_interface_seg	resw	1
	.vbe_interface_off	resw	1
	.vbe_interface_len	resw	1
endstruc

struc vbe_info
	.VBESignature       resd 1 
	.VBEVersion         resd 1      
	.OEMStringPtr       resd 1 
	.Capabilities       resw 1
	.VideoModePtr       resd 1   
	.TotalMemory        resw 1
	.OemSoftwareRev     resw 1       
	.OemVendorNamePtr   resd 1
	.OemProductNamePtr  resd 1
	.OemProductRevPtr   resd 1
	.Paddington  resb 477 
endstruc

struc vbe_mode_info
    .ModeAttributes  resw 1
    .WindowAFlags    resb 1
    .WindowBFlags    resb 1
    .Granularity     resw 1
    .WindowSize      resw 1
    .WindowASeg      resw 1
    .WindowBSeg      resw 1
    .BankSwitch      resd 1
    .BytesPerLine    resw 1
    .XRes		resw 1
    .YRes           resw 1
    .CharWidth       resb 1
    .CharHeight      resb 1
    .NumBitplanes    resb 1
    .BitsPerPixel    resb 1
    .NumberOfBanks   resb 1
    .MemoryModel     resb 1
    .BankSize        resb 1
    .NumOfImagePages resb 1
    .Reserved        resb 1
    .RedMaskSize     resb 1   
    .RedFieldPosition  resb 1
    .GreenMaskSize     resb 1
    .GreenFieldPosition resb 1
    .BlueMaskSize       resb 1
    .BlueFieldPosition  resb 1
    .RsvdMaskSize       resb 1
    .RsvdFieldPosition  resb 1
    .DirectColourMode   resb 1
    .PhysBasePtr        resd 1
    .OffScreenMemOffset resd 1
    .OffScreenMemSize  resw 1
    .paddington2 resb 463 
endstruc

struc PMInfoBlock
    .Signature resd 1
	.EntryPoint resw 1
	.PMInitialize resw 1
	.BIOSDataSel  resw 1
	.A000Sel resd 1
	.B000Sel resd 1
	.B800Sel resd 1
	.CodeSegSel resd 1
	.InProtectMode resb 1
	.CheckSum resb 1
endstruc

struc	MemoryMapEntry
	.baseAddress			resq	1
	.length				resq	1
	.type				resd	1
	.acpi_null			resd	1
endstruc

struc regs16_t
	    .di resw 1
	    .si resw 1
	    .bp resw 1
	    .sp resw 1
	    .bx resw 1
	    .dx resw 1
	    .cx resw 1
	    .ax resw 1
	    .gs resw 1
	    .fs resw 1
	    .es resw 1
	    .ds resw 1
	    .ef resw 1
endstruc


;ALL stack size are same
RM_STACKSIZE			equ 0x7fff ;Stack for REAL MODE
KERNEL_STACKSIZE        equ	0x7fff ; 32K stack...does VMM needs this changed? lets see..
USER_STACKSIZE			equ	0x7fff ;user mode stack
VBIOS_BUFFER:
	resb 0x7fff ;VBIOS (C000) buffer

UserESP:
    resb  USER_STACKSIZE
	
VBIOS_STACK:
	resb 0x7FFF

VBIOS_DS:
	resb 1024
	
RM_STACK:
  resb RM_STACKSIZE	
  
KERNEL_STACK:
  resb KERNEL_STACKSIZE

USER_STACK:
  resb USER_STACKSIZE   
  
