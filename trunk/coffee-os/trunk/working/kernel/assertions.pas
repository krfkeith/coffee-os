{ Copyright (C) Chih-Cherng Chin  1999.       All rights reserved.    }
{                                                                     }
{ The author shall not be liable to the user for any direct, indirect }
{ or consequential loss arising from the use of, or inability to use, }
{ any unit, program or file howsoever caused. No warranty is given    }
{ that the units and programs will work under all circumstances.      }

//simple modifications made due to code differences on FPC.Original code is otherwise intact.

unit assertions;

interface
procedure assert(blAssertExpr : boolean);
procedure WriteHexWord(w : word);
function ReturnOffset : word; assembler;
function ReturnSegment : word; assembler;

implementation

const
   cHexChars : array [0..$F] of char = '0123456789ABCDEF';

procedure WriteHexWord(w : word);
begin
   write(cHexChars[Hi(w) shr 4], cHexChars[Hi(w) and $F],
      cHexChars[Lo(w) shr 4], cHexChars[Lo(w) and $F]);
end;

//cant use inline under FPC, and its cleaner anyway.
function ReturnOffset : word; assembler;
asm
   mov ax, [bp+2] 
end;

function ReturnSegment : word; assembler;
asm
  mov ax, [bp+4] 
end;

{$F+}
procedure assert(blAssertExpr : boolean);
{$F-}
begin
   if blAssertExpr = FALSE then begin
      write('Assertion failed at'); //'at' is more accurate then 'before'
      WriteHexWord(ReturnSegment);
      write(':');
      WriteHexWord(ReturnOffset);
      writeln; //this line changes if you dont use the stock crt unit.
      asm
        cli
        hlt
      end;
   end;
end;

end.
