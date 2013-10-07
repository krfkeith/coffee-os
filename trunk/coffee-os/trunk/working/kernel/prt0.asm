; COFFEE OS FreePascal loader for 32-bit OS modes tripped from GRUB.
; This is nearly complete. 
;
; -CopyLEFT and opensource based on existing OS loaders (CC) by Richard Jasmin AKA "Jazz"
; (frazzledjazz@gmail.com)
;
; We are in 32bits protected mode
; AND Loaded from GRUB.


bits 32

; THis is all we can PUSH, we have to check the rest AFTER.  
MULTIBOOT_MODULE_ALIGN		equ		1<<0
MULTIBOOT_INFO_BOOTDEV	equ		1<<1
MULTIBOOT_VIDEO		equ		1<<2

MULTIBOOT_UNSUPPORTED  equ 0x0000fffc

MULTIBOOT_HEADER_MAGIC          equ     0x1BADB002
MULTIBOOT_BOOTLOADER_MAGIC          equ     0x2BADB002

MULTIBOOT_HEADER_FLAGS          equ    MULTIBOOT_MODULE_ALIGN|MULTIBOOT_INFO_BOOTDEV|MULTIBOOT_VIDEO
MULTIBOOT_CHECKSUM       equ     -(MULTIBOOT_HEADER_MAGIC + MULTIBOOT_HEADER_FLAGS)
KERNEL_STACKSIZE                equ	0x32000 ; 32K stack...does VMM needs this changed? lets see..
USER_STACKSIZE			equ	0x32000 ;user mode stack
RM_STACKSIZE	equ	0x7fff ;VBIOS shadow copy stack

section .text
;
; Multiboot header
;
align 4
dd MULTIBOOT_HEADER_MAGIC
dd MULTIBOOT_HEADER_FLAGS
dd MULTIBOOT_CHECKSUM

global start
extern mbinfo
extern MagicNumbers
extern PASCALMAIN
	
global FlushGDT
jmp start
   
start:
		cli ; interrupts OFF until further notice!
       		push $2
		popf
	
    
	    	mov  [MagicNumbers],eax                ; Multiboot magic number
        	mov  [mbinfo],ebx               ; Multiboot info
		
		call disable_nmi
	
		mov edx, KERNEL_STACK+KERNEL_STACKSIZE  ;Create kernel stack 
		mov ecx,USER_STACK+USER_STACKSIZE ;Create User-mode stack
		call enable_nmi
       
		call		   InstallGDT
		;call FlushTSS
		
		
        call PASCALMAIN                        ; Call kernel entrypoint
	
        jmp $ ; go nowhere


IDTPtr:
                                      ; IDT table pointer for 32bit access
    dw 0x0000                              ; table limit (NULL--prevent 16bit IVT from being called)
    dd 0x00000000                          ; table base address(YES, overrides RM IVT)


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
   db 0   ;xF7
   db 0x92         ; present, ring 0, code, non-conforming, readable
   db 0xCF                 ; page-granular, 32-bit
   db 0

USER_CODE_SEL   equ   $-gdt
gdt4:   
   ;dd 0xC1800000               ; limit 3GB(we still need room for PCI address space and VRAM space)
   ;dd 0xFDC00000         ; base
   
   dw 0
   dw 0
   db 0  ;xF7
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
   db 0  ;xF1
   db 0xF2         ; present, ring 3, data, expand-up, writable
   db 0xCF                 ; page-granular(4K), 32-bit
   db 0
   

TSS_SEL		equ	$-gdt
gdt6:
	dw 0x102
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
   
	
	shr eax,16
	
    mov [gdt2 + 4],al
    mov [gdt3 + 4],al
    mov [gdt4 + 4],al
    mov [gdt5 + 4],al
    mov [gdt6 + 4],al               ; set base address of 16-bit segments
   

    mov [gdt2 + 7],ah
    mov [gdt3 + 7],ah
    mov [gdt4 + 7],ah
    mov [gdt5 + 7],ah
    mov [gdt6 + 7],ah               ; set base address of 16-bit segments
  
   lea eax,[gdt + ebx]             ; EAX=PHYSICAL address of gdt
   mov [gdtr + 2],eax

   lgdt[gdtr]
   ret	                 ; Because we are called, we flush later on.


GLOBAL FlushTSS   ; Allows our  code to call tss_flush().
FlushTSS:
   mov ax, 0x2B   
   ltr ax            
   ret

; macro for ISRs without error code
%macro ISRWithoutErrorCode 1
global isr%1
isr%1:
  cli
  push byte 0
  push byte %1
; mov ebx,[48+esp]
;  cmp word [ebx - 2],$80CD
 ; je  SysCallCommonStub 
  
  jmp ISRCommonStub   ; we got here on anoher interrupt

%endmacro

; macro for IRQs without error code
%macro IRQWithoutErrorCode 1
global irq%1
irq%1:
  cli
  push byte 0
  push byte %1+32
  
  
  jmp IRQCommonStub   ;interrupts reserved for hardware
%endmacro

; macro for ISRs with error code
%macro ISRWithErrorCode 1
global isr%1
isr%1:
  cli
  push byte %1
; mov ebx,[48+esp]
;  cmp word [ebx - 2],$80CD
;  je  SysCallCommonStub 
  
  jmp ISRCommonStub   ; we got here on anoher interrupt
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
ISRWithoutErrorCode 80
IRQWithoutErrorCode 0 ; Timer (110ms intervals, 18.2 per second)
IRQWithoutErrorCode 1 ; Keyboard
IRQWithoutErrorCode 2 ; slave 8259 or EGA/VGA vertical retrace
IRQWithoutErrorCode 3 ; COM2 or COM4/LAN
IRQWithoutErrorCode 4 ; COM1 or COM3 or mouse/LAN
IRQWithoutErrorCode 5 ; fixed disk or LPT2/sound card
IRQWithoutErrorCode 6 ; floppy disk
IRQWithoutErrorCode 7 ; LPT1
IRQWithoutErrorCode 8 ; Real Time Clock
IRQWithoutErrorCode 9 ;  software redirected to IRQ2
IRQWithoutErrorCode 10 ; ???
IRQWithoutErrorCode 11 ; ???
IRQWithoutErrorCode 12 ; PS/2 mouse
IRQWithoutErrorCode 13 ;  numeric coprocessor error 
IRQWithoutErrorCode 14 ; fixed disk controller(IDE)
IRQWithoutErrorCode 15 ;  second fixed disk controller(IDE)

;A (Context switch) is moving stack to the next PID and then back after task_switch, like so.
; There is a reason we dont do it here.(see timer unit)

;  mov esp,eax
;  mov eax,esp
;  push eax
;  call task_switch
;  mov esp,eax



extern ISRHandler
extern IRQHandler

; This is our common ISR stub. It saves the processor state, sets
; up for kernel mode segments, calls the Pascal-level ISR handler,
; and finally restores the stack frame.
ISRCommonStub:


   pusha                    ; Pushes edi,esi,ebp,esp,ebx,edx,ecx,eax

   mov ax, ds               ; Lower 16-bits of eax = ds.
   push ax                 ; save the data segment descriptor

   mov ax, 0x10  ; load the kernel data segment descriptor
   mov ds, ax
   mov es, ax
   mov fs, ax
   mov gs, ax
  
   cli

   
   call ISRHandler
   
   pop ax        ; reload the original data segment descriptor
   ; TSS not saved upon return to ring3, so do it.
; this will be required from here on out
  lea eax,[esp + 76]	; 19 dwords == 76 bytes
  mov [ESP],eax      ; Ring 0 ESP value after IRET
  
   mov ds, ax
   mov es, ax
   mov fs, ax
   mov gs, ax

   popa                     ; Pops edi,esi,ebp...
   add esp, 8     ; Cleans up the pushed error code and pushed ISR number
   sti
   iret           ; pops 5 things at once: CS, EIP, EFLAGS, SS, and ESP


IRQCommonStub:


   pusha                    ; Pushes edi,esi,ebp,esp,ebx,edx,ecx,eax
 
   
   mov ax, ds               ; Lower 16-bits of eax = ds.
   push ax                 ; save the data segment descriptor

   mov ax, 0x10  ; load the kernel data segment descriptor
   mov ds, ax
   mov es, ax
   mov fs, ax
   mov gs, ax

 
   
   call IRQHandler

   pop ax        ; reload the original data segment descriptor
   ; TSS not saved upon return to ring3, so do it.
; this will be required from here on out
  lea eax,[esp + 76]	; 19 dwords == 76 bytes
  mov [ESP],eax      ; Ring 0 ESP value after IRET
  
   mov ds, ax
   mov es, ax
   mov fs, ax
   mov gs, ax

   popa                     ; Pops edi,esi,ebp...
   add esp, 8     ; Cleans up the pushed error code and pushed ISR number
   sti
   iret           ; pops 5 things at once: CS, EIP, EFLAGS, SS, and ESP

; Enables A20 line
global enable_a20
ENABLE_A20: 
	MOV             AL, 0D1H
    OUT             064H, AL
 
	MOV             AL, 0DFH
    OUT             060H, AL
	Ret
	
; Disables A20 line
global disable_a20
DISABLE_A20: 
	MOV             AL, 0D1H
    OUT             064H, AL
 
	MOV             AL, 0DDH
    OUT             060H, AL
	ret
	
; Enables non-maskable interrupts
global enable_nmi
enable_nmi:
        MOV             AL, 0DH
        OUT             070H, AL
		NOP
		RET

; Disables non-maskable interrupts
global disable_nmi
disable_nmi:

        MOV             AL, 0DH
        OUT             070H, AL
        NOP
		RET



; This is STRAIGHT from the C in JamesM's kernel demo
;MUST be here, within PASCAL code, it causes issues for some reason...
;adjust pages to have USER bit set -OR- alloc User Heap (and stack) like we have done.
; or we will page fault on jump.

global switch_to_user_mode
EXTERN test_user_function
switch_to_user_mode:
     ;JamesM tutorial says you need to disable interrupts. But really I see no need for it
     mov ax,0x23
     mov ds,ax
     mov es,ax 
     mov fs,ax 
     mov gs,ax ;we don't need to worry about SS. it's handled by iret
 
     mov eax,esp
     push 0x23 ;user data segment with bottom 2 bits set for ring 3
     push eax ;push our current stack just for the heck of it
     pushf
     push 0x1B; ;user code segment with bottom 2 bits set for ring 3
     push test_user_function 
     iret
	 
extern kernel_directory

bits	32

; page directory table
%define		PAGE_DIR			0x9c000 ;9c000

; 0th page table. Address must be 4KB aligned
%define		PAGE_TABLE_0		0x9D000

; nth page table. Address must be 4KB aligned
%define		PAGE_TABLE_1019		0x9E000

; nth page table. Address must be 4KB aligned
%define		PAGE_TABLE_1	0x9F000

; each page table has 1024 entries
%define		PAGE_TABLE_ENTRIES	1024

; attributes (page is present;page is writable; supervisor mode)
;were backwards according to qemu(FIX: 02092013)
%define			PRIV				3
%define			USERPRIV			7

global EnablePaging
EnablePaging:
	pusha										; save stack frame
;	cli
   push ebx              ; According to __cdecl, we must preserve the contents of EBX.
   pushf 
	;------------------------------------------
	;	idenitity map 1st (and 2nd) page table (8MB)
	;------------------------------------------
;Move the kernel(1MB+) to high memory under paging structure
	
	mov		eax, PAGE_TABLE_0					; first page table
	mov		ebx, 0x000000 | PRIV						; starting physical address of page(if not 0, where is IDT?)
	mov		ecx, PAGE_TABLE_ENTRIES				; for every page in table...
loop1:
	mov		dword [eax], ebx					; write the entry
	add		eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096							; go to next page address (Each page is 4Kb)
	loop	loop1								; go to next entry

    mov		eax, PAGE_TABLE_1				; first page table
	mov		ebx, 0x400000 | PRIV						; starting physical address of page(4MB)
	mov		ecx, PAGE_TABLE_ENTRIES				; for every page in table...
loop2:
	mov		dword [eax], ebx					; write the entry
	add		eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096							; go to next page address (Each page is 4Kb)
	loop	loop2					
	
;setup a user mode page

	mov		eax, PAGE_TABLE_1019					; first page table
	mov		ebx, 0xFEC00000 | USERPRIV						; starting physical address of page
	mov		ecx, PAGE_TABLE_ENTRIES				; for every page in table...
loop3:
	mov		dword [eax], ebx					; write the entry
	add		eax, 4								; go to next page entry in table (Each entry is 4 bytes)
	add		ebx, 4096							; go to next page address (Each page is 4Kb)
	loop	loop3								; go to next entry

	;------------------------------------------
	;	set up the entries in the directory table
	;------------------------------------------

	mov		eax, PAGE_TABLE_0 | PRIV			; 1st table is directory entry 0
	mov		dword [PAGE_DIR], eax
	mov		eax, PAGE_TABLE_1 | PRIV			; 2nd entry in directory table
	mov		dword [PAGE_DIR+(1*4)], eax


	mov		eax, PAGE_TABLE_1019 | USERPRIV			; 1019nth entry in directory table
	mov		dword [PAGE_DIR+(1019*4)], eax


	;------------------------------------------
	;	install directory table
	;------------------------------------------


    
	;------------------------------------------
	;	enable paging
	;------------------------------------------
	; dont override the registers were using...
	mov		eax, cr0
	or		eax, 0x80000000
	mov		ecx, PAGE_DIR
	mov		cr3, ecx
	mov		cr0, eax

    popf                  ; Pop EFLAGS back.
    pop ebx               ; Get the original value of EBX back.
	
	
	;cli
	;hlt
	
	;sti
	popa
	ret 
	

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
   mov cr0, edx          ; Enable paging.

   popf                  ; Pop EFLAGS back.
   pop ebx               ; Get the original value of EBX back.
   sti
   ret

extern TssPtr

	   
section .data
section .bss ; 'better save space' for these...this is uninitialized data area.
;takes up about 300K or so...(memory)
; This space is ZEROED.
_bss:


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


;
; Kernel stack location
;
align 32

;ALL stack size are same


KERNEL_STACK:
  resb KERNEL_STACKSIZE

USER_STACK:
  resb USER_STACKSIZE
				
_ebss:
