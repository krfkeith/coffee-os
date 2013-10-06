unit signals;
//The FP functions are FPC internals and most of this unit will get rewritten anyway once I get this far.

//Anything requiring BaseUnix,Linux,sysutils will call HERE.

interface

    {*************************************************************************}
    {                               SIGNALS                                   }
    {*************************************************************************}


Const
  { For sending a signal }

  SA_NOCLDSTOP = 1;
  SA_NOCLDWAIT = 2;
  SA_SIGINFO   = 4;
  SA_RESTORER  = $04000000;
  SA_ONSTACK   = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NODEFER   = $40000000;
  SA_RESETHAND = $80000000;

  SA_NOMASK    = SA_NODEFER;
  SA_ONESHOT   = SA_RESETHAND;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;


  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;


  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGBUS     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGUSR1    = 10;
  SIGSEGV    = 11;
  SIGUSR2    = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTerm    = 15;
  SIGSTKFLT  = 16;
  SIGCHLD    = 17;
  SIGCONT    = 18;
  SIGSTOP    = 19;
  SIGTSTP    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGURG     = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGIO      = 29;
  SIGPOLL    = SIGIO;
  SIGPWR     = 30;
  SIGUNUSED  = 31;


{ si_code field values for tsiginfo.si_code when si_signo = SIGFPE }
const
  FPE_INTDIV = 1; { integer divide by zero }
  FPE_INTOVF = 2; { integer overflow }
  FPE_FLTDIV = 3; { floating point divide by zero }
  FPE_FLTOVF = 4; { floating point overflow }
  FPE_FLTUND = 5; { floating point underflow }
  FPE_FLTRES = 6; { floating point inexact result }
  FPE_FLTINV = 7; { floating point invalid operation }
  FPE_FLTSUB = 8; { floating point subscript out of range }

const
  SI_PAD_SIZE   = ((128 div sizeof(longint)) - 3);

type
  sigset_t = array[0..wordsinsigset-1] of cuLong;
  tsigset  = sigset_t;
  sigset   = sigset_t;
  psigset  = ^tsigset;

  psiginfo = ^tsiginfo;
  tsiginfo = record
       si_signo : longint;
       si_errno : longint;
       si_code : longint;
       _sifields : record
           case longint of
              0 : ( _pad : array[0..(SI_PAD_SIZE)-1] of longint );
              1 : ( _kill : record
                   _pid : pid_t;
                   _uid : uid_t;
                end );
              2 : ( _timer : record
                   _timer1 : dword;
                   _timer2 : dword;
                end );
              3 : ( _rt : record
                   _pid : pid_t;
                   _uid : uid_t;
                   _sigval : pointer;
                end );
              4 : ( _sigchld : record
                   _pid : pid_t;
                   _uid : uid_t;
                   _status : longint;
                   _utime : clock_t;
                   _stime : clock_t;
                end );
              5 : ( _sigfault : record
                   _addr : pointer;
                end );
              6 : ( _sigpoll : record
                   _band : longint;
                   _fd : longint;
                end );
           end;
    end;

{ CPU dependent TSigContext }

{$packrecords C}

type
  tfpreg = record
          significand: array[0..3] of word;
          exponent: word;
  end;

  pfpstate = ^tfpstate;
  tfpstate = record
           cw, sw, tag, ipoff, cssel, dataoff, datasel: cardinal;
           st: array[0..7] of tfpreg;
           status: cardinal;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    gs, __gsh: word;
    fs, __fsh: word;
    es, __esh: word;
    ds, __dsh: word;
    edi: cardinal;
    esi: cardinal;
    ebp: cardinal;
    esp: cardinal;
    ebx: cardinal;
    edx: cardinal;
    ecx: cardinal;
    eax: cardinal;
    trapno: cardinal;
    err: cardinal;
    eip: cardinal;
    cs, __csh: word;
    eflags: cardinal;
    esp_at_signal: cardinal;
    ss, __ssh: word;
    fpstate: pfpstate;
    oldmask: cardinal;
    cr2: cardinal;
  end;

  tsigaltstack=record
        ss_sp : pointer;
        ss_flags : longint;
        ss_size : longint;
  end;

  Pucontext=^Tucontext;
  TUcontext=record
    uc_flags : cardinal;
    uc_link  : Pucontext;
    uc_stack : tsigaltstack;
    uc_mcontext : tsigcontext;
    uc_sigmask : tsigset;
  end;


type
  signalhandler_t = procedure(signal: longint); cdecl;
  sigactionhandler_t = procedure(signal: longint; info: psiginfo; context: psigcontext); cdecl;
  sigrestorerhandler_t = procedure; cdecl;
      
  signalhandler = signalhandler_t;
  sigactionhandler = sigactionhandler_t;
  sigrestorerhandler = sigrestorerhandler_t;
  tsignalhandler = signalhandler_t;
  tsigactionhandler = sigactionhandler_t;
  tsigrestorerhandler = sigrestorerhandler_t;
  
  psigactionrec = ^sigactionrec;
  sigactionrec = record
    sa_handler: sigactionhandler_t;
    sa_flags: culong;
    sa_restorer: sigrestorerhandler_t;
    sa_mask: sigset_t;
  end;




    Function  FpSigProcMask(how : cInt; nset : pSigSet; oset : pSigSet): cInt; external name 'FPC_SYSC_SIGPROCMASK';
    Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt; external name 'FPC_SYSC_SIGPROCMASK';
    Function  FpSigPending (var nset : TSigSet): cInt;
    Function  FpSigSuspend (Const sigmask : TSigSet): cInt;
//  Is a depreciated POSIX function that can be considered alias to sigaction
//returns handler for number given.needed to do a syscall.
Function  FpSignal  (signum:longint;Handler:signalhandler):signalhandler;


Function FpsigEmptySet(var nset : TSigSet): cint;
Function FpSigFillSet (var nset : TSigSet): cInt;
Function FpSigAddSet  (var nset : TSigSet; signo : cInt): cInt;
Function FpSigDelSet  (var nset : TSigSet; signo : cInt): cInt;
Function FpSigIsMember(Const nset : TSigSet; signo : cInt): cInt;

interface

 // general sigset funcs implementation.


function fpsigaddset(var nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
   nset[(signo-1) shr ln2bitsinword]:=nset[(signo-1) shr ln2bitsinword] OR (1 shl ((signo-1) and ln2bitmask));
   fpsigaddset:=0;
End;

function fpsigdelset(var nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
   nset[(signo-1) shr ln2bitsinword]:=nset[(signo-1) shr ln2bitsinword] AND NOT (1 shl ((signo-1) and ln2bitmask));
   fpsigdelset:=0;
End;

function fpsigemptyset(var nset : tsigset):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO nset[i]:=0;
  fpsigemptyset:=0;
End;

function fpsigfillset(var nset : tsigset):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO nset[i]:=high(nset[i]);
  fpsigfillset:=0;
End;

function fpsigismember(const nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
    if ((nset[(signo-1) shr ln2bitsinword]) and (1 shl ((signo-1) and ln2bitmask)))>0 Then
     fpsigismember:=1
    else
     fpsigismember:=0;
End;

Function fpSigPending(var nset: TSigSet):cint;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
begin
  fpsigpending:=do_syscall(syscall_nr_rt_sigpending,TSysParam(@nset));
end;

function fpsigsuspend(const sigmask:TSigSet):cint;
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  fpsigsuspend:= do_syscall(syscall_nr_rt_sigsuspend,TSysParam(@sigmask),TSysParam(8));
end;

function sigblock(mask:cuint):cint;

var nset,oset: TSigSet;

begin
 fpsigemptyset(nset);
 // fpsigaddset(nset,mask);   needs _mask_
 nset[0]:=mask;
 sigblock:= fpsigprocmask(SIG_BLOCK,@nset,@oset);   // SIG_BLOCK=1
 if sigblock=0 Then
  sigblock:=oset[0];
end;

function sigpause(sigmask:cint):cint;

var nset: TSigSet;

begin
 fpsigemptyset(nset);
 nset[0]:=sigmask;
 sigpause:= fpsigsuspend(nset);
end;

function fppause:cint;

begin
  fppause:=sigpause(sigblock(cuint(0)));
end;

//wait if...
function wifexited(status : cint): boolean;
begin
 wifexited:=(status AND $7f) =0;
end;

function wexitstatus(status : cint): cint;
begin
 wexitstatus:=(status and $FF00) shr 8;
end;

function wstopsig(status : cint): cint;
begin
 wstopsig:=(status and $FF00) shr 8;
end;

const wstopped=127;

function wifsignaled(status : cint): boolean;
begin
 wifsignaled:=((status and $FF)<>wstopped) and ((status and 127)<>0);
end;

function wtermsig(status : cint):cint;

begin
 wtermsig:=cint(status and 127);
end;

Function FpSignal(signum:longint;Handler:signalhandler):signalhandler;
// should be moved out of generic files. Too specific.

var sa,osa : sigactionrec;

begin
     sa.sa_handler:=SigActionHandler(handler);
     FillChar(sa.sa_mask,sizeof(sa.sa_mask),#0);
     sa.sa_flags := 0;
{     if (sigintr and signum) =0 then
 {restart behaviour needs libc}
      sa.sa_flags :=sa.sa_flags or SA_RESTART;
}
     FPSigaction(signum,@sa,@osa);
     if fpgetErrNo<>0 then
       fpsignal:=NIL
     else
       fpsignal:=signalhandler(osa.sa_handler);
end;



end.
