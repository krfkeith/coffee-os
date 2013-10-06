{
 tasking.pas - Defines the structures and prototypes needed for Non-pre-emtive multitasking(like linux)
 Includes some old code from TP4 that was brought up to FPC specs.
 
Oldschool software threading routines were written by:

  Michael Warot - Blue Star Systems
  November 1987

First PC Computer was around 1984-85..somewhere around there..(ye ole x86 ;-) gotta luv it..)
We DO NOT Task Switch to HW. The TSS is TOO slow and difficult to code.

Threads and tasks are pretty much same thing, all routines to work with them one way or another are here.

--Jazz

}

unit Tasking;


interface

type
 string32=string[32];
 FlagPtr    = ^Boolean;

 UBit31  = 0..(1 shl 31)-1;

  PHeader = ^THeader;
  THeader = bitpacked record
    Prev,Next: PHeader;
    Magic:longint;   //needed for task-switching code to identify kernel.
    Allocated: Boolean; 
    Length: UBit31;
  end;

  TFooter=record
     magic:longint;     // Magic number, same as in THeader.
     Fheader:PHeader;   //points to THeader.  Pointer to the block header.
  end;

 TProcState = (Kill,	{Task to execute}
               Live,    {Running task}
               Slow,                    { Running, but in background }
               Pause,                   { Waiting for above          }
               Hiber);                  { Sleeping like a hedgie in winter.. }
  TProcStateSet= set of TProcState;

var
   ProcState:TProcStateSet;

type

 Task=packed record
   id:integer;                // Process ID.
   esp,ebp:longint;       // Stack and base pointers.
   eip:longint;            // Instruction pointer.
   page_directory:longword; // Page directory.(PPageDir)
   state:TProcStateSet; // Is task alive? 
   name:string[32];  // Task's name
   next:longint; // Next Task Pointer
   thread:pointer; //Threads within a task(user-level code)[should point to running thread #]
//Required for Yield and Sleep and such
   HiberPtr  : FlagPtr;     { Pointer to "WAKE" flag }
   Priority  : LongInt;     { priority (0=Real Time) }
   NextTime  : Longint;     { Next wake up call @    }
 end;

     // The next task in a linked list.
TTask=^Task;
 
var
  pid:integer;
  eaxVal:longint;

//Initial ESP was set in the assembler file.
//...as [StartESP]

// The next available process ID.
   next_pid:integer=0; 
   IsTaskingActive,IsPagingActive: Boolean;

 current_task:TTask;
 new_task,Next:TTask; 
 ready_queue:TTask;
 InitPriority:integer;

const
  MaxProc=32000; //Will run out of RAM long before this.

procedure InitTasking;
//switch_task is in IRQ.pas

// Forks the current process, spawning a new one with a different memory space.
function fork(name:string32):TTask;

//task_switch is called on irq0, timer interrupt and it changes context for us.

procedure move_stack(new_stack_start:PtrUInt;  size:longword);
function getpid:integer;
procedure switch_task;
procedure copy_page_physical; external name 'copy_page_physical'; //prt0.asm
Procedure Yield;     { Call this procedure often in your code. This is the
                         heart of the Multi-Tasking, it will return after all
                         of the other processes have a crack at it.        }

Procedure Sleep(Flag : FlagPtr);
                       { Call this procedure with an address of a flag which
                         when TRUE, will re-awaken the process. Upon entry
                         this procedure will test the value of this flag, and
                         if FALSE, will mark the process HIBER.
                         This procedure makes a call to YIELD in all cases.
                         Note : Don't let all of you processes Sleep, or
                         you could put things into a deadlock. }

Procedure Lock(Resource : Byte);
                       { This procedure allows the programmer to insure that
                         a procedure is not entered twice, it does this by
                         having the second call yield until the resource is
                         free, using Sleep }

Procedure UnLock(Resource : Byte);
                       { This procedure unlocks a resource, allowing it to be
                         used by other processes }

Procedure KillProc;    { This procedure is intended to be called by a process
                         that has done all of it's work. It marks the process
                         as one that is 'DEAD' and thus never re-awakens }

Function  Child_Process:Boolean;
                       { This function returns True if the calling procedure
                         is a child process. This test should be used to branch
                         into a specific procedure for a given task.       }

Procedure SetPriority(P : Integer);
Function ProcessCount:word;
procedure IdleTask;


implementation

uses
	console,UConsole,timer,isr,pmm,heap, vmm;
var
  MaxStack  : Word;
  SaveFrame    : longint; //The esp register 
  Procs     :  TTask; { Keeps the process pointers } 

  NextP,                              { Last live process number  }
  ThisP,                              { Current process           }
  LastP     : Word;                   { Last Process number       }

  LiveCount : Word;                   { How many thing happening? }


  Locks     : Array[0..255] of Boolean; { Resource locks }


{

  These procedures work on the following basis:
    1> For each process, there is an amount of memory reserved for
       a machine stack, this is called a Stack Frame(esp register). This holds
       the current state of a given process.This is stored in the TSS.

    2> The process table (Procs) contains pointers to all of the
       Stack Frames. When a task is to be swapped out, it's state
       is saved in it's own stack, then the frame pointer is placed
       in (Procs) until the process is to be swapped back in.

    3> Every one in a while, when a task has some time to share,
       it makes a call to Yield, which does all of the swapping.
}

//scheduler...
//     inc(pid);
//     if (pid > current_task^.next) then pid :=1; //kernel is always task 1   
//switch_task;

procedure IdleTask;
begin
writestrln('Idle Task');
while true do;	// Critical, will page fault if no loop
end;


procedure switch_task;
//keep the EIP and register dumping routine local. We dont want the data
// on the screen anyway.
label
  GetEIP;

var
   espVal, ebpVal, eipVal:longword;
   phys_dir:longint;
   Reg: array [0..7] of LongWord;
   SReg: array [0..5] of Word;
   EFLAGS,CurrentEIP: LongWord;
begin
   // If we haven't initialised tasking yet, just return.
   if IsTaskingActive=false then
       exit; 
   // Read esp, ebp now for saving later on.
	asm
		cli
		mov esp,[espVal]
		mov ebp,[ebpVal]
		sti
	end;
   // Read the instruction pointer. We do some cunning logic here:
   // One of two things could have happened when this function exits -
   // (a) We called the function and it returned the EIP as requested.
   // (b) We have just switched tasks, and because the saved EIP is essentially
   // the instruction after read_eip(), it will seem as if read_eip has just
   // returned.
   // In the second case we need to return immediately. To detect it we put a dummy
   // value in EAX further down at the end of this function. As C returns values in EAX,
   // it will look like the return value is this dummy value! (0x12345).
  
  //Get the current register state, will drop the EIP for us..  
  asm
    // save registers while they are not modified by another procedure call. note that
    // depending on your compiler settings, ebp may already be trashed (stack frame)

    mov dword ptr Reg[4*0],eax
    mov dword ptr Reg[4*1],ecx
    mov dword ptr Reg[4*2],edx
    mov dword ptr Reg[4*3],ebx
    mov dword ptr Reg[4*4],esp // esp is already incorrect since it was decreased by the amount of stack space the local variables require
    mov eax,16*4+6*2+4+4
    add dword ptr Reg[4*4],eax // correct esp
    mov dword ptr Reg[4*5],ebp
    mov dword ptr Reg[4*6],esi
    mov dword ptr Reg[4*7],edi
    // save segment registers
    mov word ptr SReg[2*0],ds
    mov word ptr SReg[2*1],es
    mov word ptr SReg[2*2],cs
    mov word ptr SReg[2*3],ss
    mov word ptr SReg[2*4],fs
    mov word ptr SReg[2*5],gs
    // save EFLAGS
    pushfd
    pop dword ptr EFLAGS
    // now get eip
    call GetEIP
  GetEIP:
    pop dword ptr CurrentEIP
end;

   eipVal:=CurrentEIP; //commands unit does this, this is same value.
  
   // Have we just switched tasks?
   if (eipVal = $12345) then
       exit;
    // No, we didn't switch tasks. Let's save some register values and switch.
   current_task^.eip := eipVal;
   current_task^.esp := espVal;
   current_task^.ebp := ebpVal;
    // Get the next task to run.
   current_task := next;
   // If we fell off the end of the linked list start again at the beginning.
   if (current_task^.next)<>0 then current_task := ready_queue; 
    espVal := current_task^.esp;
    ebpVal := current_task^.ebp; 

 // Here we:
   // * Stop interrupts so we don't get interrupted.
   // * Temporarily put the new EIP location in ECX.
   // * Load the stack and base pointers from the new task struct.
   // * Change page directory to the physical address (physicalAddr) of the new directory.
   // * Put a dummy value (0x12345) in EAX so that above we can recognise that we've just
   // switched task.
   // * Restart interrupts. The STI instruction has a delay - it doesn't take effect until after
   // the next instruction.
   // * Jump to the location in ECX (remember we put the new EIP in there).
   asm 
     cli                 
     mov ecx,[eipVal]       
     mov esp, [espVal]       
     mov ebp,[ebpVal]
   end;
   phys_dir:=current_directory; //virtToPhys(long(@current_directory)) 
   asm     
     mov eax,[phys_dir]         
     mov cr3,eax       
     mov eax,[$12345] 
     sti                 
     jmp ecx 
  end;

end;


Procedure Yield;                  { Let the other task's go at it }
var
   SaveFrame:longint;

Begin
   asm
		mov esp,SaveFrame
   end;
   Procs[ThisP].esp := SaveFrame;      

  If (Slow in Procs[ThisP].State) then
  With Procs[ThisP] do
  begin
    include( Procs[ThisP].State,pause);
    NextTime := TimerTicks+Priority;
    If NextTime > $001800ae then
      NextTime := NextTime - $001800ae;
  End; { with }

  If LiveCount >= 1 then              { If we actually have a task to }
  begin                               { swap to, then....             }
    repeat                            { keep looking until we hit a   }
      If ThisP < LastP then           { live one                      }
        Inc(ThisP)
      else
        ThisP := 0;

      With Procs[ThisP] do begin
      //What to do with processes....
        if  (Live in  Procs[ThisP].State) then exit; //Let it live.

        if  (Hiber in ProcState) then begin
                 If HiberPtr^ then   { Check to see if we should }
                     include(ProcState,live);    { wake a sleeping process   }
        end;
        if  (Pause in ProcState) then begin
	         If (Priority = 0) OR
                     (TimerTicks > NextTime) then
                  begin
		    include(ProcState,slow);            { handle slow task }
                  end;
        end;
        if  (Kill in ProcState) then begin
                 If ThisP <> 0 then                    { Kill Off a process }
                  Begin
                         MemFree(pointer(esp)); //clear stack
		   	 include(ProcState,Pause);                    
                  end;
       end;
      end;
      
    until (Live in Procs[ThisP].State); //Do until Live state found.
  end;
  SaveFrame := Procs[ThisP].esp;        { Load new stack frame }
   asm
		mov SaveFrame,esp
   end;
  
End; { Raw_Yield }


Procedure Sleep(Flag : FlagPtr);     { Put a process to sleep           }
//use delay for IOWait on HW..USE THIS to yield to other processes..
Begin
  If flag=nil then
  Begin
    Procs[ThisP].HiberPtr := Flag;   { Set wake up pointer }
    include(ProcState,Hiber);  { Mark this process as hibernating }
  End;
  Yield;                             { Do a yield, either way, to keep
                                       things going smoothly            }
End; { Sleep }

Procedure Lock(Resource : Byte);     { Lock a resource ID }
Begin
  If NOT Locks[Resource] Then        { If not open, then wait until }
    Sleep(@Locks[Resource]);         { the resource becomes available }

  { Resource MUST be available now! }

  Locks[Resource] := FALSE;          { Make it unavailable for use  }
End; { Lock }

Procedure UnLock(Resource : Byte);   { Unlock that resource }
Begin
  Locks[Resource] := True;           { Make the resource available }
End; { UnLock }

Procedure KillProc;                  { Stop a process in it's tracks    }
Begin
  If (LiveCount > 1) and (IsTaskingActive=true) then              { if we are actually swapping then }
  begin
    include(ProcState,Kill);      {   mark us as dead                }
    Dec(LiveCount);                  {   Bump process count             }
    Yield;                       {   and yield. (Never returns)     }
  end;
                                 
    exit;            
End; { KillProc }

Function Child_Process:boolean;              { Returns true if not root process }
Begin
  Child_Process := ThisP <> 0;
End;

Procedure SetPriority(P : Integer);               { Set number of ticks between runs }
//should be a function like: nice(2).
Begin
  With Procs[ThisP] do begin
    
    If P = 0 then begin //Real-Time
      include(ProcState,Live);
      exclude(ProcState,Slow);
    end
    else
      include(ProcState,Slow);
      exclude(ProcState,Live);
    end;
end;


Function ProcessCount:word;
Begin
  ProcessCount := LiveCount;
End;

procedure InitTasking;
var
	i:byte;
        eipVal,ebpval,espval,eaxVal:longword;
begin 
  writestring('Initializing Multi-Tasking Core    ');
  NextP := 0;                        { We are in the root process      }
  ThisP := 1;
  LastP := 1;                        { Last Active process             }
  FillChar(Procs,(SizeOf(Procs)),#0);
  include(ProcState,Live);
  LiveCount := 1;                    { And one task is running (this one) }
  For i := 0 to 255 do
    Locks[i] := True;                { All resources available }
  
   // Relocate the stack so we know where it is.
  //move to where?
    move_stack(09000000,32708); //32K stack size, remember?
writestrln('Stack moved.');
asm
    cli
    hlt
 end;
   // Initialise the first task (kernel task)
   ready_queue:= Memalloc((sizeof(ready_queue)));
   current_task := ready_queue;
   current_task^.id :=1; //ID number for the task, kernel is always = 1.

   asm
      mov ebpVal,ebp
      mov espVal,esp
      mov eaxVal,esp
   end;

   current_task^.ebp:= ebpVal;
   current_task^.esp := espVal;

   current_task^.eip := 0; 
   current_task^.page_directory := VirtToPhys(longword(@PageDir^[0])); 
//@pd^[0]
//otherwise this bombs looking for Virt Address. 
   current_task^.next := 0; //which task comes next, if 0, then scheduler does not switch_tasks.
   IsTaskingActive:=true;
 	textcolor(green);	
	writestrln('[ OK ]');
	textcolor(8);
 end;

function  getpid:integer;
begin
    getpid:= current_task^.id;
end;


function fork(name:string32):TTask;

var
 stack,TaskEIP:longint; //Task^.esp (CPU register)
 thread:pointer;
 tmp_task,parent_task:TTask;
 eaxVAL,espVal,ebpVal,eip,newAddr:longint;
 directory:longword;
 
begin
   // We are modifying kernel structures, and so cannot be interrupted.
   asm 
	cli
   end;
   // Take a pointer to this process' task struct for later reference.
   parent_task := current_task;
   // Clone the address space.

//fix this to point to a TPageDirEntry type and not a longword type
//  directory := clone_directory(Current_Directory);

 // Create a new process.
// Allocate 4 kilobytes of space on stack(per process)

   new_task^.esp:= longword(Memalloc($4000));
//   new_task^.esp:= new_task^.esp+ $1000; 
   new_task^.ebp:=0;
   new_task^.page_directory := longword(@Current_Directory);  	
   new_task^.next := 0;
   tmp_task:= ready_queue;
   while (tmp_task^.next)<>0 do 
       tmp_task := next;
   tmp_task^.next:=Next^.next;
 
// This will be the entry point for the new process.SHOULD be SPECIFIED:  new_task^.eip:=@IdleTask..
// This will yield DemoTask running as pid [1] on task_switch.
// IDEALLY you pass the TTask Variable INTO as well as OUT OF Fork.(IMPROVE THE DESIGN)
// for the DEMO and testing purposes, this works.
   asm 
     mov eaxVal,esp
   end;
   new_task^.eip := eaxVal; //@Routine to call(stack)

// STAGE 1 BETA CODE END. 

   new_task^.name:=name;
 //  TaskEIP:=new_task^.eip;
  
// We could be the parent or the child here - check.
   if (current_task = parent_task)  then begin
       // We are the parent, so set up the esp/ebp/eip for our child.
       //read in the values.
      asm 	
		mov espVal,esp
       		mov ebpVal,ebp 
      end;
       
       new_task^.esp := espVal;
       new_task^.ebp := ebpVal;
     
      //set 'LIVE' state: (FIXME) new_task^.state := Live;
        new_task^.thread:= thread;
        new_task^.id:= getpid+1;
        pid:= new_task^.id; // enable task switching
     
     // All finished: Reenable interrupts.
  
       asm  
	  sti
       end;
       fork:= new_task;
   end
   else
   begin
       // We are the child 
       fork:=new_task; //=0
   end;
end; 

procedure move_stack(new_stack_start:longword;  size:longword);
//starts in heap+4096 (4k)
var

    old_stack_pointer,old_base_pointer,i,offset,new_stack_pointer,new_base_pointer:PtrUInt;
    tmp,tmp2:PtrUInt;
    pd_addr:PtrUInt;
    kp1:longword;

begin
  // Allocate some space for the new stack.
   i:= new_stack_start; 
   writelong(i);
   repeat      
    // General-purpose stack is in user-mode.
    current_page:=(longword(PPageDir(AllocPage))); 
    inc(i,1024);
   until   ( i > longword(@new_stack_start-size));

  // Flush the TLB by reading and writing the page directory address again.
  asm
	mov eax,cr3
        mov cr3,eax
  end;   
 
  // Old ESP and EBP, read from registers.
  asm 
     mov old_stack_pointer,esp
     mov old_base_pointer,ebp
  end;
 asm
    cli
    hlt
 end;
  // Offset to add to old stack addresses to get a new stack address.
  offset:= longword(new_stack_start - placementAddress);
  // New ESP and EBP.
  new_stack_pointer := old_stack_pointer + offset;
  new_base_pointer  := old_base_pointer  + offset;

  // Copy the stack.
  move(new_stack_pointer, old_stack_pointer, (sizeof(placementAddress - old_stack_pointer)));

  // Backtrace through the original stack, copying new values into
  // the new stack.  

   i := longword(@new_stack_start);  
   repeat
    tmp := i;
    // If the value of tmp is inside the range of the old stack, assume it is a base pointer
    // and remap it. This will unfortunately remap ANY value in this range, whether they are
    // base pointers or not.
    if (( old_stack_pointer < tmp) and (tmp < placementAddress)) then
    begin
      tmp := tmp + offset;
      tmp2 := i;
      tmp2:= tmp;
    end;
    Dec(i,4);
  until i > longword(@new_stack_start - size);
//for debugging
//writelongln(old_stack_pointer);
//writelongln(old_base_pointer);

//writelongln(new_stack_pointer);
//writelongln(new_base_pointer);

  // Change stacks.
  asm 
    mov esp,new_stack_pointer
    mov ebp,new_base_pointer 
  end;

end;

begin
  InitPriority:=200;
  SetPriority(InitPriority);
end.
