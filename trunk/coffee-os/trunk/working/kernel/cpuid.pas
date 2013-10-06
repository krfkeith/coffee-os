{
  Unit implemented so the OS can get information about
  the processor ID and manufacturer for error reporting (if implemented)
  and for fixing certain bugs on different CPU's

  Matt.

--or for detecting certain feature sets for kernel internal use.

--Jazz

}
unit cpuid;

interface

const
  ID_BIT = $200000; // EFLAGS ID bit

var
  CPUHasPAE:boolean;
  CPUHasMSR,CPUHasTSE,CPUHasPSE, CPUHasDebug, CPUHasVME, CPUHasFPU:boolean;

type
  TCPUID = array [1..4] of Longword;
  TVendor = array [0..11] of Char;

function IsCPUAvailable: Boolean;
function GetCPUID: TCPUID; assembler;
function GetCPUVendor: TVendor; assembler;
procedure DetectCPUID;
procedure kDetectCPUID;

implementation

uses
  Console,UConsole;

function IsCPUAvailable: Boolean; assembler;

asm
  pushfd
  pop    eax
  mov    edx,eax
  xor    eax,id_bit
  push   eax
  popfd
  pushfd
  pop    eax
  xor    eax,edx
  mov    al,1
end ['eax','edx'];

function GetCPUID: TCPUID; assembler;
asm
  push    ebx         {save affected register}
  push    edi
  mov     edi,eax     {@result}
  mov     eax,1 //there are other options to get, we want basic info...
  cpuid
  stosd               {cpuid[1]}
  mov     eax,ebx
  stosd               {cpuid[2]}
  mov     eax,ecx
  stosd               {cpuid[3]}
  mov     eax,edx
  stosd               {cpuid[4]}
  pop     edi         {restore registers}
  pop     ebx
end ['eax','edx','ebx','ecx','edi'];

function GetCPUVendor: TVendor; assembler;
asm
  push    ebx         {save affected register}
  push    edi
  mov     edi,eax     {@result (tVendor)}
  mov     eax,0
  cpuid
  mov     eax,ebx
  xchg    ebx,ecx     {save ecx result}
  mov     ecx,4
@1:
  stosb
  shr     eax,8
  loop    @1
  mov     eax,edx
  mov     ecx,4
@2:
  stosb
  shr     eax,8
  loop    @2
  mov     eax,ebx
  mov     ecx,4
@3:
  stosb
  shr     eax,8
  loop    @3
  pop     edi         { restore the registers }
  pop     ebx
end ['eax','ebx','ecx','edi'];

procedure DetectCPUID;
var
  CPUID: TCPUID;
  Vendor: TVendor;

begin
  if IsCPUAvailable then begin
    CPUID := GetCPUID;
    Vendor := GetCPUVendor;
    writestring('Current CPU in Use: ');
    writelongln(CPUID[0] and 1); //warning: bit eval on longword.
    writestring('Vendor of CPU: ');
    writestrln(Vendor);
    writestring('CPU Supports the Following modes: ');
//these come in useful later.
  if (CPUID[4] and 0)=1 then begin
      writestring(' FPU ');
    end; 
  if (CPUID[4] and 1)=1 then begin
      writestring(' VME ');
    end;  
   if (CPUID[4] and 2)=1 then begin
      writestring(' Debug ');
    end; 
   if (CPUID[4] and 3)=1 then begin
      writestring(' PSE ');
    end;
    if (CPUID[4] and 4)=1 then begin
      writestring(' TSE ');
    end;
    if (CPUID[4] and 5)=1 then begin
      writestring(' MSR ');
    end;

    if (CPUID[4] and 6)=1 then begin
      CPUHasPAE:=true;
      writestring(' PAE ');
    end;
    writeline;
  end else
    writestrln('Could not get CPU ID Information.');
end;

//Do the above, but SILENTLY...
procedure kDetectCPUID;
var
  CPUID: TCPUID;
  Vendor: TVendor;

begin
  if IsCPUAvailable then begin
    CPUID := GetCPUID;
    Vendor := GetCPUVendor;
 //these come in useful later.
  if (CPUID[4] and 0)=1 then begin
      CPUHasFPU:=true;
    end; 
  if (CPUID[4] and 1)=1 then begin
      CPUHasVME:=true;
    end;  
   if (CPUID[4] and 2)=1 then begin
      CPUHasDebug:=true;
    end; 
   if (CPUID[4] and 3)=1 then begin
      CPUHasPSE:=true;
    end;
    if (CPUID[4] and 4)=1 then begin
      CPUHasTSE:=true;
    end;
    if (CPUID[4] and 5)=1 then begin
      CPUHasMSR:=true;
    end;
    if (CPUID[4] and 6)=1 then begin
      CPUHasPAE:=true;
      writestring(' PAE ');
    end;
end;
end;

end.

