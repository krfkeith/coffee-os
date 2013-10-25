all:
	nasm -f elf stub.asm -o stub.o
	fpc -Aelf -n -O3 -Op3 -Si -Sc -Sg -Xd -CX -XXs -Rintel -Tlinux kernel.pas
	ld --gc-sections -s -Tlinker.script -o iso/boot/kernel stub.o kernel.o multiboot.o system.o console.o x86.o gdt.o idt.o isr.o irq.o heap.o pmm.o vmm.o
	mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot \
		-boot-load-size 4 -boot-info-table -o basic.iso iso
