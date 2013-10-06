Unit VM86;
//finally, some VM86, AKA ability to run 16bit code under PM.

// Original programmer: Tim Robinson
// Homepage: http://www.themoebius.org.uk/ 

// translated from C by Jazz.

{
With the Intel 80286 processor came protected mode. While this was a great improvement over the unprotected 8086, giving programmers the ability to separate code, data and stack, to limit the lengths of segments, and to move segments around in memory transparently, it was clear that Intel didn't have backwards compatibility in mind with the new mode. Firstly it was officially impossible to switch from protected back to real mode (although a clever IBM hack made this possible); secondly, protected mode was also incompatible with the vast amount of real-mode code around at the time.

Microsoft Windows/286 was one of the first programs to make use of protected mode on the 80286, providing some degree of hardware-assisted memory management and inter-application protection. However, Windows/286 still needed to run DOS programs, which, by their very nature, ran under real mode. So Windows/286 'faked' multitasking between DOS and Windows applications by suspending Windows whenever a DOS program was in the foreground; it would have been far too slow to pre-emptively multitask DOS and Windows because this would have involved switching repeatedly between real and protected modes, something that the 286 wasn't very good at.

By the time they came to design the i386, the Intel designers realised that the old real-mode DOS programs weren't going to go away soon, and that they needed to design a feature into their new CPU which would allow real-mode code to co-exist with a protected mode OS. Thus virtual 8086 mode was created.

"So what?" you say. "I'm not going to run any DOS programs inside my OS." OK, that's fair enough. But remember that DOS isn't the only real-mode code that you might want to run: more important to most people is the BIOS. Inside the BIOS is a set of custom device drivers for various custom hardware in your system: one example of this is the VESA BIOS, using which you can switch to high-resolution video modes and generally use graphics modes beyond 320x200x256 or 640x480x16. So V86 mode is very handy for this (and if you don't believe me, it's the same method used by SVGALib on Linux and various NT graphics drivers). 

As the name suggests, virtual 8086 mode (commonly shortened to VM86 or just V86 mode) lets you run one or more virtual 8086 chips on the single processor. More accurately, you can run one or more virtual PC systems on one processor using a combination of multitasking (to emulate several processors) and virtualization of hardware (to emulate several sets of hardware).

In basic terms, the CPU is in virtual 8086 mode when the VM flag (bit 17) is set in the EFLAGS register. However, V86 mode requires some help from your kernel to keep running; more importantly, you can't just toggle EFLAGS.VM in the middle of your code and assume you're running in V86 mode, as you can to enter protected mode.

Let's put the talk of flags aside for a moment. What makes V86 mode special is the following:

    * Addresses are formed in the real-mode way; that is, using a segment and an offset. The segment registers CS, DS, ES, FS, GS and SS work as they do in real mode. Segments are limited to 64KB and their base addresses are set with the segment registers. So...
    * ...segmentation is disabled once you're in V86 mode. That is, the CPU ignores whatever funky base addresses you might have specifed in your GDT or LDT and addresses memory as if it were in real mode. However, paging is still in effect: the segment:offset addresses get translated to linear (i.e. paged) and not physical (i.e. RAM) addresses. So...
    * ...the access rights that segments control are also discarded. V86 code runs permanently in ring 3. So...
    * ...the IOPL flags take effect. However...
    * ...IOPL takes on a different meaning in V86 mode. Instead of denying or allowing port I/O, it instead allows or disallows V86 tasks to make changes to the interrupt enable flag (EFLAGS.IF). This means that if IOPL < 3, attempts to execute sensitive instructions (CLI, STI, LOCK, PUSHF, POPF and IRET) will result in a general protection fault which will be handled in the kernel. This is a good thing, because a V86 task changing the interrupt flag could potentially disallow actual hardware interrupts from reaching the protected-mode device drivers.
    * Port I/O is not IOPL-sensitive in V86 mode, but is controlled by the I/O permission bitmap in the TSS. You can disable I/O bitmap checking by setting the I/O map base to point beyond the limit of your TSS; in this case, the CPU will generate a GPF for all ports. If you want to allow V86 code to access ports without a GPF, you will need to extend the TSS by 8192 bytes (enough for 65,536 ports with one bit per port), point the I/O map field to the start of the bitmap, and set all the bits to zeroes.
    * Note that the INT instruction is IOPL-sensitive in V86 mode, although if IOPL = 3, the CPU ignores the real-mode interrupt vector table and always uses the IDT instead. Again, if IOPL < 3, software interrupts trigger a GPF which can be handled by the kernel.

We can see from this that V86 mode is more similar in some ways (address formation) to real mode, and in others (protection) to protected mode. You can also guess that we'll need some kernel support to keep V86 mode going, although you don't really need to write too much code to get, say, the video BIOS running. A downside to using V86 mode is speed: every IOPL-sensitive instruction will cause the CPU to trap to kernel mode, as will I/O to ports which are masked out in the TSS. 

Let's see what's involved in getting some simple code to run. As I mentioned before, the CPU enters V86 mode when EFLAGS.VM  is set; however, you can't toggle this flag in the middle of an instruction stream. The best way I've found to do it is from a kernel-to-user interrupt return; in fact, you'll find that if you've already got some ring 0 interrupt handlers in place, the switches to and from V86 mode (remember the processor switches out of V86 mode for interrupts) fit in nicely. This is by design.

Recall that the IRET instruction pops EFLAGS off the stack, so the first thing you'll need to do is set the VM bit in the EFLAGS image you've got on the stack (i.e. the copy of EFLAGS that the CPU pushed there when it entered ring 0 in the first place). I'm assuming here that you've already got enough multitasking implemented that you can switch reliably between ring 3 and ring 0 (and back again) and keep track of the ring 3 stuff on the stack once you're in ring 0. Apart from the obvious flag difference, the other differences to a normal ring 3 stack image are:

    * CS and SS are 16-bit segment registers, although they're widened to 32 bits when the CPU pushes them onto the stack
    * EIP is actually IP; that is, only the bottom 16 bits are significant. Again, it's widened when it get pushed. The same applies to ESP.
    * The CPU pops DS, ES, FS and GS on an IRET to V86 mode; in memory, they're actually stored in the order ES, DS, FS, GS (with ES at the lower address and GS at the higher address). This is to allow kernel code to modify the values of the segment registers; of course, any pops to these registers while still in ring 0 will load them with protected mode values; any attempts to load them with real-mode values (which are needed by the real mode code) will probably result in a fault. These are also 16 bits widened to 32.

We can see from this that you don't shouldn't need to make any changes to the ring 0 interrupt entry/exit code; you still need to push and pop all the general-purpose registers (the CPU doesn't handle these specially in V86 mode), and you can carry on pushing and popping the segment registers, although the values you give them won't have any effect once you're in V86 mode.

In fact, setting up a V86 stack frame is the same as setting up a normal ring 3 stack frame, except for the extra 4 segment registers at the end and the VM bit set in EFLAGS. 

Although we know how to switch to V86 mode, we need somewhere for our code to run. Specifically we need somewhere for code and data to go, and somewhere to use as a stack. For reasons which are hopefully obvious (i.e. real mode code can't access beyond the first megabyte), code, data and stack all need to go in the first megabyte. You need to make sure that you've got some space free there; space, that is, in the first megabyte of your virtual address space -- by the wonder of paging, it can be located anywhere in physical memory. I'll leave that to you to sort out because your memory layout depends on the way you're managing it. Suffice to say that if you've chosen to put your kernel in the bottom half and your applications in the top half, you're stuck (unless you re-map the first megabyte so that it's accessible to ring 3 and make sure there's nothing important there). If you're planning on calling the BIOS you'll also need to make sure that the BIOS area A0000  to FFFFF is mapped correctly; for a simple V86 monitor you can map the whole area straight to physical memory and make it read-write. If you want to get clever you might think about making it copy-on-write, so that each task gets its own copy of the video buffer. If it uses the BIOS, your code will also need the interrupt vector table and BIOS data area from address 0 to 500. If your boot loader or kernel wrote over the IVT and BDA you are also stuck; if so, you will need to modify them so as to preseve that memory. Again, you could make the bottom 500 bytes copy-on-write so that each task got its own IVT and BDA; however, for now it should be enough to map the first page to address zero and make it read-write.

Once you've got some area clear in the first megabyte and mapped the BIOS correctly you need to decide on the correct values for CS:IP and SS:SP. Remember that every linear address can be described by many segment:offset pairs. Here's some code I use to go from a 32-bit linear address to a segment:offset pair (plus a few handy macros). Note that this code only works (reliably) for memory aligned to page boundaries; it could probably be improved.

Depending on what code you intend to run in your virtual 8086 machine, you will need up to three memory areas: code, data and stack. For now it should be enough to emulate simple .COM programs, where code and data are contained within one 64KB segment. Although for the moment you should write your own real-mode code to test this with (for tasks like using the BIOS to change video modes), there are a few things to consider if you want to emulate the real-mode environment fully. One consideration is that DOS programs often assume that they have full ownership of the bottom megabyte of memory, so you may have to be prepared to allow them to read and write anywhere there. Another is that .COM programs are linked to start at offset 100h  within their segment, so unless you want to write all your programs to run otherwise, you should try and emulate this. A simple way is to allocate 256 bytes more than you need to, put the contents of the program 256 bytes from the start of the buffer, and run it from there. You could even go so far as emulating a DOS PSP (Program Segment Prefix) and pass a command line there; real DOS puts an INT 20h instruction at the start of the PSP, and a zero word at the top of the initial stack, which means that when programs do a RET instruction they hit the interrupt and get terminated automatically. You don't have to emulate DOS to this extent, but it's a handy way of exiting V86 programs.

Now we're able to switch to protected mode, and have something there to run. The question now is: what can we run? The answer to this is, as much as you're prepared to write kernel support for. I'll go into this in more detail later on; at the moment it should be possible to run simple sequences of instructions (i.e. code that doesn't use any of the IOPL-sensitive instructions I mentioned earlier). For now, let's stick to some simple 16-bit assembly code; something like this NASM listing. 

This code will load a distinctive value into the bottom half of EAX, then trigger a breakpoint, which I'm assuming your kernel will handle and provide a register dump (where we'll see the 0x1234 value clearly). Once you've assembled this (as flat binary), you need to have your kernel load it into the memory block it allocated before, and set up the ring 3 stack frame to continue in Virtual 8086 mode at the CS:IP of the start of the code. If all is well, once the CPU returns to user mode, it will see the VM bit set and switch to V86 mode, then load AX and immediately switch back to kernel mode to handle the breakpoint (actually as a GPF, as we'll see below). The stack frame now will be the same as it was when it was initially, except that, with a bit of luck, EAX will hold the value 0x1234 in its bottom half.

Although the fact that your 32-bit protected mode kernel can run the 16-bit real mode code above is impressive, the code itself doesn't really do much. Let's make it more useful by having it call the BIOS and switch video modes. 

This program will switch to 320x200x8 graphics mode then exit; we don't necessarily need to use the INT 3 breakpoint instruction to exit, but it's as good a way as any. Note that it's more straightforward to program the video card directly if you want to switch to 320x200x8 mode, although once this code works it should be easy to extend it to VESA modes later.

If you try running this code in your V86 monitor you will find that it crashes immediately, with a GPF (interrupt 13h), on the INT 10h instruction. Your kernel needs to handle interrupts by:

    * Checking the byte at CS:IP (you'll need to convert CS:IP to a linear address for this, using the FP_TO_LINEAR macro above) to see whether a 0xCD (INT) opcode caused the fault.
    * Putting the new IP, CS and FLAGS registers on the user-mode stack. The new IP is the value of IP after the interrupt has returned, i.e. two bytes after the current value. Remember that IP is the bottom 16 bits of EIP, so you need to do some bit twiddling to make sure you're updating the right part (or make a union which allows you to manipulate the two halves of a 32-bit register automatically). You need to notionally clear the EFLAGS.IF bit, although if you clear the real IF bit, the machine will stop multitasking. I'll come onto the role of the virtual IF bit later.
    * Decrementing the user-mode value of SP by 6 bytes. Again, remember to only update the bottom 16 bits of ESP for this.
    * Redirecting CS:IP to the new interrupt handler. The new address can be obtained using the real-mode interrupt vector table at address 0 (zero). The IVT is, in effect, an array of 256 far pointers (segment:offset pairs); in each element, the offset occupies the first word, and the segment the second.

Here's some code to do all that. 

If you now try this handler in your kernel, and it succeeds in vectoring the code to the BIOS interrupt handler, you will find that it faults again, and not on an interrupt. This time it will be another IOPL-sensitive instruction, each of which must be handled by the kernel. It's fairly straightforward to handle each of these, and you can add them to the switch (ip[0]) along with the interrupt code. In short, here's what you need to do for each instruction (you'll need to look up the instruction encodings in the Intel manuals; the NASM documentation also provides a handy reference).

    * CLI: clear the task's virtual interrupt enable flag
    * STI: set the task's VIF
    * PUSHF: push the bottom 16 bits of EFLAGS onto the user stack. If the VIF is set, set the IF bit in the word you pushed; otherwise clear it.
    * POPF: pop a word off the stack, set the IF bit, and load it into the bottom 16 bits of the task's EFLAGS. Set the VIF according to the state of IF in the flags word on the stack.
    * IRET: do the reverse of the INT instruction: pop the flags off the stack in the same way as POPF does and branch to the CS:IP you popped off the stack.
    * various forms of IN: read a byte/word/dword from a port
    * various forms of OUT: write a byte/word/dword to a port
    * various forms of INS and OUTS: like IN and OUT but incrementing or decrementing [E]SI and [E]CX

You will need to handle the REP (repeat), A32 (32-bit address) and O32 (32-bit operand) prefixes too, where appropriate. The virtual interrupt enable flag which I just mentioned maintains the state of the interrupt flag as the V86 task sees it; of course the kernel should have full control over the real IF so as not to affect multitasking and hardware access. The VIF is more important if you start vectoring hardware interrupts to a V86 task (if it thinks interrupts are disabled then don't dispatch any). 

}


procedure detect_v86; boolean;
var

   eaxVAL:longint; //might get away with boolean here.

begin
asm
        smsw    ax
        and     eax,1           //CR0.PE bit. if =1, then VM86 (unreal mode) is set.
	mov eax,eaxVAL
        ret
end;
   detect_v86:=longInt2bool(eaxVAL);
end;


type
	FARPTR:uint32;

const

// Extract the segment part of a FARPTR 
	FP_SEG       = ( fp shl 16)

// Extract the offset part of a FARPTR 
	 FP_OFF       = ( fp & $ffff)

function makeFarPtr(seg,ofs:longint):pointer;

begin
makeFarPtr:=(( (seg) shl 16) or  (off)));
end;

// Convert a segment:offset pair to a linear address */
function FP_TO_LINEAR (seg,ofs:longint):pointer;
begin
	 FP_TO_LINEAR:= ((( (seg)) shl 4) + ( (off))))
end;

function i386LinearToFp:pointer; //PTRUINT??
var
  seg,off:longint;

begin
    off :=  ptr & $f;
    seg := ( ptr - ((addr_t) ptr & $f)) div 16;
    i386LinearToFp:= MK_FP(seg, off);
end;



var
ip:uint8ptr;
stack,ivt:ptrUInt16;

begin
ip = FP_TO_LINEAR(ctx.cs, ctx.eip);
ivt = 0;
stack =  FP_TO_LINEAR(ctx.ss, ctx.esp);

case (ip[0]) of
 $cd:  // INT n 
    stack -= 3;
    ctx.esp = ((ctx.esp & 0xffff) - 6) & 0xffff;

    stack[0] = (ctx.eip + 2);
    stack[1] = ctx.cs;
    stack[2] = ctx.eflags;

    if (current.v86_if)
        stack[2] := or stack[2] EFLAG_IF;
    else
        stack[2] := stack[2] &  not EFLAG_IF;

    current.v86_if = false;
    ctx.cs = ivt[ip[1] * 2 + 1];
    ctx.eip = ivt[ip[1] * 2];
    return true;  // continue execution 

else:  // something wrong 
    return false; // terminate the app 
end;

const
 VALID_FLAGS =        $DFF

function i386V86Gpf(ctx:^context_v86_t):boolean;
begin
    ip:ptruint8;
    stack,ivt:ptruint16;
    stack32:ptruint32;
    is_operand32, is_address32:boolean;

    ip = FP_TO_LINEAR(ctx.cs, ctx.eip);
    ivt = 0;
    stack =  FP_TO_LINEAR(ctx.ss, ctx.esp);
    stack32 = stack;
    is_operand32 = is_address32 = false;
   
// TRACE4("i386V86Gpf: cs:ip = 04:04 ss:sp = 04:04: ",  ctx.cs, ctx.eip,ctx.ss, ctx.esp);

    while (true)
    begin
        case (ip[0]) of
        begin
        case $66:            // O32 
           // TRACE0("o32 ");
            is_operand32 = true;
            inc(ip);
            ctx.eip = (ctx.eip + 1);
            break;

        case $67:            //A32 
           // TRACE0("a32 ");
            is_address32 = true;
            inc(ip);
            ctx.eip = (ctx.eip + 1);
            break;
            
        case $9c:            // PUSHF 
         //   TRACE0("pushf\n");

            if (is_operand32)
            begin
                ctx.esp = ((ctx.esp & $ffff) - 4) & $ffff;
                dec(stack32);
                stack32[0] = ctx.eflags & VALID_FLAGS;

                if (current.v86_if)
                    stack32[0] or = EFLAG_IF;
                else
                    stack32[0] := stack32[0] and not EFLAG_IF;
            end;
            else
            begin
                ctx.esp = ((ctx.esp & $ffff) - 2) & $ffff;
                dec(stack);
                stack[0] =  ctx.eflags;

                if (current.v86_if)
                    stack[0] :=stack[0] or EFLAG_IF;
                else
                    stack[0] :==stack[0] and not EFLAG_IF;
            end;

            ctx.eip = (ctx.eip + 1);
            return true;

        case $9d:            // POPF 
           // TRACE0("popf\n");

            if (is_operand32)
            begin
                ctx.eflags = EFLAG_IF or EFLAG_VM or (stack32[0] and VALID_FLAGS);
                current.v86_if = (stack32[0] and EFLAG_IF) <> 0;
                ctx.esp = ((ctx.esp and  $ffff) + 4) & $ffff;
            end;
            else
            begin
                ctx.eflags = EFLAG_IF or EFLAG_VM or stack[0];
                current.v86_if = (stack[0] and EFLAG_IF) <> 0;
                ctx.esp = ((ctx.esp and $ffff) + 2) and $ffff;
            end;
            
            ctx.eip = (ctx.eip + 1);
            return true;

        case $cd:            // INT n 
           // TRACE1("interrupt 0x%x => ", ip[1]);
            case (ip[1]) of
            begin
            case $30:
               // TRACE0("syscall\n");
                if (ctx.regs.eax = ExitThread)
                    ExitThread(0);
                return true;

            case $20:
            case $21:
                //i386V86EmulateInt21(ctx);
                if (current.v86_in_handler)
                    return false;

                //TRACE1("redirect to %x\n", current->v86_handler);
                current.v86_in_handler = true;
                current.v86_context = ^ctx;
                current.kernel_esp:=current.kernel_esp + (sizeof(context_v86_t) - sizeof(context_t));
                ctx.eflags = EFLAG_IF or 2;
                ctx.eip = current.v86_handler;
                ctx.cs = USER_FLAT_CODE or 3;
                ctx.ds =  USER_FLAT_DATA or 3;
		ctx.es =  USER_FLAT_DATA or 3;
		ctx.gs =  USER_FLAT_DATA or 3;
		ctx.ss =  USER_FLAT_DATA or 3;
                ctx.fs = USER_THREAD_INFO or 3;
                ctx.esp = current.user_stack_top;
                return true;

            default:
                stack -= 3;
                ctx.esp = ((ctx.esp and $ffff) - 6) and $ffff;

                stack[0] = (ctx.eip + 2);
                stack[1] = ctx.cs;
                stack[2] = ctx.eflags;
                
                if (current.v86_if)
                    stack[2] :=stack[2] or EFLAG_IF;
                else
                    stack[2] :=stack[2] and not EFLAG_IF;

                ctx.cs = ivt[ip[1] * 2 + 1];
                ctx.eip = ivt[ip[1] * 2];
               // TRACE2("%04x:%04x\n", ctx->cs, ctx->eip);
                return true;
            end;

            break;

        case $cf:            /* IRET */
          //  TRACE0("iret => ");
            ctx.eip = stack[0];
            ctx.cs = stack[1];
            ctx.eflags = EFLAG_IF or EFLAG_VM or stack[2];
            current.v86_if = (stack[2] and EFLAG_IF) != 0;

            ctx.esp = ((ctx.esp and $ffff) + 6) and $ffff;
           // TRACE2("%04x:%04x\n", ctx->cs, ctx->eip);
            return true;

        case $fa:            /* CLI */
           // TRACE0("cli\n");
            current.v86_if = false;
            ctx.eip = (ctx.eip + 1);
            return true;

        case $fb:            /* STI */
           // TRACE0("sti\n");
            current.v86_if = true;
            ctx.eip = (ctx.eip + 1);
            return true;

        default:
            wprintf("unhandled opcode $", ip[0]);
            return false;
        }
    }

    return false;
}

