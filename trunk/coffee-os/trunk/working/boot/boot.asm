; COFFEE OS Bootloader by Jazz
; Supports USB and (should also support) Floppy/CDROM [under emulation]
; "EVERYTHING BOOTS!"
use16
org 7C00h
start:  jmp main

bpbOEM:			db "MSDOS5.0" ; cannot exceed 8 bytes
bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 11 ;11
bpbNumberOfFATs: 	DB 2
bpbRootEntries: 	DW 224
bpbTotalSectors: 	DW 2880
bpbMediaId: 		DB 0xf0 ;f8 for HDD  
bpbSectorsPerFAT: 	DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0 ;80
bsUnused: 		DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:	        DD 0xa0a1a2a3
bsVolumeLabel: 	        DB "COFFEEOSDISK"
bsFileSystem: 	        DB "FAT12   "
 
main:
  cli
  xor ax, ax
  mov ds, ax          ; DS := 0000
  mov es, ax	      ; ES:=0000
  mov ss, ax          ; SS := 0000
  mov sp, 7C00h
  sti  

  mov bx, 600h ; put stage2 here

  mov cx,2 ; cyl=0 sect=1
  xor dh,dh  ; head=0
  mov ax,20Ah
  test dl,dl
  jne .1
  
;  mov ax,206h
;  call read
;  jc disk_error
;  add bx,512*17
;  mov cx,1
;  mov dh,1

.1:
  call read
  jc disk_error

  mov si, Load2
  call print ;how do we know if we've loaded it all? (or >1MB kernels...)
  jmp 0:600h
  jmp $   

read:
  mov bp,2
  
.1:
  push ax
  int 13h
  jnc .2
  sub bp,1
  jc .2
  xor ax,ax
  int 13h
  pop ax
  jmp .1

.2:
  pop bp 
  ret

disk_error:      mov     si, boot_drive_read_error     ; string to print
		call print
                jmp $
 
print:
  mov ah, 0Eh
  xor bh, bh
.1:
  mov al, [si]
  lea si, [si+1]
  test al, al
  je .2
  int 10h
  jmp .1
.2:
  ret
     
boot_drive_read_error:
  db 'Boot drive read error! Press CTRL-ALT-DEL when ready.', 13, 10, 0

Load2:
 db 'Loading Stage2...',13,10,0

  times 510-($-$$)                db 0
                                  dw 0AA55h