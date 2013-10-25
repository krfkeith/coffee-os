{   X86 Intel CPU specific poke/peek functions. This has been modified from FPC RTL and no longer matches it.
    However, it still supports X86-64 instructions.
    Added Misc functions and defs. --Jazz

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit x86;

interface
{$packrecords C}

TYPE 
    //I noticed some issues with some C code being a little obscure. --Jazz
    // Until you port the code over, this allows it to run ok.
    uint=CARDINAL;
    uint32 = Longword;
    uint8= byte;
    uint16=word;
    cint8                  = shortint;
    cint32                 = longint;
    pbyte = ^byte;
    pword = ^word;
    PtrUInt8 =^UInt8;
    SInt32=cint32;
    Sint8=cint8;
    PtrUInt32=^Longword;


function ReadPortB (Port : Longint): Byte;
function ReadPortW (Port : Longint): Word;
function ReadPortL (Port : Longint): Longint;
function readportd (port : word) : dword;

Procedure WritePortB (Port : Longint; Value : Byte);
Procedure WritePortL (Port : Longint; Value : Longint);
Procedure WritePortW (Port : Longint; Value : Word);
procedure writeportd (port : word ; val : dword);


//extra functions
procedure enableInterrupts;assembler;
procedure disableInterrupts;assembler;
procedure flush_tlb;  assembler;
procedure popad; assembler;
procedure pushad; assembler;
procedure popfd; assembler;
procedure pushfd; assembler;
function btod (nb : byte) : dword; 
function wtod (nb : word) : dword; 


implementation
{$ASMMODE intel}


procedure writeportd (port : word ; val : dword); 

var
   p   : pointer;

begin

   p := @val;

   asm
      mov   dx , port
      mov   esi, p
      outsd
   end;
end;

function readportd (port : word) : dword; 

var
   tmp : dword;
   p : pointer;

begin

   p := @tmp;

   asm
      mov   dx , port
      mov   edi, p
      insd
   end;

   readportd := tmp;

end;

procedure pushfd; assembler;
asm
	pushfd
end;


procedure popfd; assembler;
asm
	popfd
end;


procedure pushad; assembler;
asm
	pushad
end;


procedure popad; assembler;
asm
	popad
end;

procedure flush_tlb;  assembler;
asm
	mov   eax, cr3
	mov   cr3, eax
end;

procedure disableInterrupts;assembler;

asm
   cli
end;

procedure enableInterrupts;assembler;

asm
   sti
end;

//DWORD OPS: come in handy with PCI and similar addressing
function btod (nb : byte) : dword; [public, alias : 'B2D'];

var
   tmp : dword;

begin
   asm
      xor   eax, eax
      mov   al , nb
      mov   tmp, eax
   end;

   btod := tmp;

end;

function wtod (nb : word) : dword; [public, alias : 'W2D'];

var
   tmp : dword;

begin
   asm
      xor   eax, eax
      mov   ax , nb
      mov   tmp, eax
   end;

   wtod := tmp;

end;

{$ASMMODE ATT}


Procedure WritePortB (Port : Longint; Value : Byte);
begin
  asm

        movl port,%edx
        movb value,%al
        outb %al,%dx
  end;
end;

Procedure WritePortW (Port : Longint; Value : Word);

begin
  asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
  end;
end;


Procedure WritePortL (Port : Longint; Value : Longint);

begin
  asm

        movl port,%edx
        movl value,%eax
        outl %eax,%dx
  end;
end;


function ReadPortB (Port : Longint): Byte; assembler;
asm

  movl port,%edx
  xorl %eax,%eax
  inb %dx,%al
end;

function ReadPortW (Port : Longint): Word; assembler;
asm
  movl port,%edx
  xorl %eax,%eax
  inw %dx,%ax
end;

function ReadPortL (Port : Longint): LongInt; assembler;
asm

  movl port,%edx
  inl %dx,%eax
end;


procedure kHalt; assembler; //USE with RING0 PROCESSES ONLY, Halt is for Ring3 code
asm
   cli
   hlt
end;

{
procedure Halt; assembler;  //ideally halt(longword), like bp7...
//will get into the internal error codes in a minute...
asm
  mov eax,900 //for example, dont know where we will end up.
  int 80
end;
}
end.
