;/////////////////////////////////////////////////////////
;//                                                     //
;//               Freepascal barebone OS                //
;//                      stub.asm                       //
;//                                                     //
;/////////////////////////////////////////////////////////
;//
;//     By:             De Deyn Kim <kimdedeyn@skynet.be>
;//     License:        Public domain
;//
 
;
; Kernel stub
;
 
;
; We are in 32bits protected mode
;
[bits 32]
 
;
; Export entrypoint
;
[global kstart]
 
;
; Import kernel entrypoint
;
[extern kmain]
 
;
; Posible multiboot header flags
;
MULTIBOOT_MODULE_ALIGN          equ     1<<0
MULTIBOOT_MEMORY_MAP            equ     1<<1
MULTIBOOT_GRAPHICS_FIELDS       equ     1<<2
MULTIBOOT_ADDRESS_FIELDS        equ     1<<16
 
;
; Multiboot header defines
;
MULTIBOOT_HEADER_MAGIC          equ     0x1BADB002
MULTIBOOT_HEADER_FLAGS          equ     MULTIBOOT_MODULE_ALIGN | MULTIBOOT_MEMORY_MAP
MULTIBOOT_HEADER_CHECKSUM       equ     -(MULTIBOOT_HEADER_MAGIC + MULTIBOOT_HEADER_FLAGS)
 
;
; Kernel stack size
;
KERNEL_STACKSIZE                equ     0x4000
 
section .text
 
;
; Multiboot header
;
align 4
dd MULTIBOOT_HEADER_MAGIC
dd MULTIBOOT_HEADER_FLAGS
dd MULTIBOOT_HEADER_CHECKSUM
 
;
; Entrypoint
;
kstart:
        mov esp, KERNEL_STACK+KERNEL_STACKSIZE  ;Create kernel stack
        push eax                                ;Multiboot magic number
        push ebx                                ;Multiboot info
        call kmain                              ;Call kernel entrypoint
        cli                                     ;Clear interrupts
        hlt                                     ;Halt machine
 

; macro for ISRs without error code
%macro ISRWithoutErrorCode 1
global isr%1
isr%1:
  cli
  push byte 0
  push byte %1
  jmp  ISRCommonStub
%endmacro

; macro for ISRs with error code
%macro ISRWithErrorCode 1
global isr%1
isr%1:
  cli
  push byte %1
  jmp  ISRCommonStub
%endmacro

ISRWithoutErrorCode  0 ; Division By Zero Exception
ISRWithoutErrorCode  1 ; Debug Exception
ISRWithoutErrorCode  2 ; Non Maskable Interrupt Exception
ISRWithoutErrorCode  3 ; Breakpoint Exception
ISRWithoutErrorCode  4 ; Into Detected Overflow Exception
ISRWithoutErrorCode  5 ; Out of Bounds Exception
ISRWithoutErrorCode  6 ; Invalid Opcode Exception
ISRWithoutErrorCode  7 ; No Coprocessor Exception
ISRWithErrorCode     8 ; Double Fault Exception
ISRWithoutErrorCode  9 ; Coprocessor Segment Overrun Exception
ISRWithErrorCode    10 ; Bad TSS Exception
ISRWithErrorCode    11 ; Segment Not Present Exception
ISRWithErrorCode    12 ; Stack Fault Exception
ISRWithErrorCode    13 ; General Protection Fault Exception
ISRWithErrorCode    14 ; Page Fault Exception
ISRWithoutErrorCode 15 ; Unknown Interrupt Exception
ISRWithoutErrorCode 16 ; Coprocessor Fault Exception
ISRWithoutErrorCode 17 ; Alignment Check Exception
ISRWithoutErrorCode 18 ; Machine Check Exception
ISRWithoutErrorCode 19 ; Reserved
ISRWithoutErrorCode 20 ; Reserved
ISRWithoutErrorCode 21 ; Reserved
ISRWithoutErrorCode 22 ; Reserved
ISRWithoutErrorCode 23 ; Reserved
ISRWithoutErrorCode 24 ; Reserved
ISRWithoutErrorCode 25 ; Reserved
ISRWithoutErrorCode 26 ; Reserved
ISRWithoutErrorCode 27 ; Reserved
ISRWithoutErrorCode 28 ; Reserved
ISRWithoutErrorCode 29 ; Reserved
ISRWithoutErrorCode 30 ; Reserved
ISRWithoutErrorCode 31 ; Reserved

; This is our common ISR stub. It saves the processor state, sets
; up for kernel mode segments, calls the Pascal-level ISR handler,
; and finally restores the stack frame.
extern ISRHandler
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
  pop  gs
  pop  fs
  pop  es
  pop  ds

  popa
  add  esp,8
  iret

; macro for IRQs, IRQ 0 corresponds to ISR 32, IRQ 1 to ISR 33, and so on
%macro IRQ 1
global irq%1
irq%1:
  cli
  push byte 0
  push byte %1+32
  jmp  IRQCommonStub
%endmacro

IRQ  0 ; Timer
IRQ  1 ; Keyboard
IRQ  2 ; ???
IRQ  3 ; ???
IRQ  4 ; ???
IRQ  5 ; ???
IRQ  6 ; ???
IRQ  7 ; ???
IRQ  8 ; ???
IRQ  9 ; ???
IRQ 10 ; ???
IRQ 11 ; ???
IRQ 12 ; ???
IRQ 13 ; ???
IRQ 14 ; ???
IRQ 15 ; ???

extern IRQHandler
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

  pop  eax
  pop  gs
  pop  fs
  pop  es
  pop  ds

  popa
  add  esp,8
  iret

extern GDTPtr
global FlushGDT
FlushGDT:
  push eax
  lgdt [GDTPtr]
  mov  ax,0x10
  mov  ds,ax
  mov  es,ax
  mov  fs,ax
  mov  gs,ax
  mov  ss,ax
  jmp  0x08:flush
flush:
  pop  eax
  ret


section .bss
 
;
; Kernel stack location
;
align 32
KERNEL_STACK:
        resb KERNEL_STACKSIZE
