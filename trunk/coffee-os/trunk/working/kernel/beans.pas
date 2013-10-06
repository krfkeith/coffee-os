{This file is being tweeded.

This unit will probably be removed at some point.
Most of the necessary low level calls are already implemented.

(GNU Linux includes all the units that USE the BaseUnix, in otherwords **THIS** Unit)

All the RTL comprises of is: THIS unit, the SYSTEM unit, and some, but not all, 'extra' units like classes and such.
There really aren't that many 'extra' units.

Be honest with you...a lot of these functions are *NIX specific and have no other use.
A basic implementation is really all you need, and THAT is not here.

All lot of these calls are wrappers to syscalls or the functions themselves, which point nowhere.
WONDERFUL, eh?

--Jazz


}

Unit Beans;

Interface

{$inline on}
Uses UnixType;

{$define oldreaddir}            // Keep using readdir system call instead
                                // of userland getdents stuff.

{$define usedomain}             // Allow uname with "domain" entry.
                                // (which is a GNU extension)


{$DEFINE has_ugetrlimit}
{$define OLDMMAP}

//YEAH...REAL EFFECTIVE....

type
  cint8                  = UnixType.cint8;       pcint8                 = UnixType.pcint8;
  cuint8                 = UnixType.cuint8;      pcuint8                = UnixType.pcuint8;
  cchar                  = UnixType.cchar;       pcchar                 = UnixType.pcchar;
  cschar                 = UnixType.cschar;      pcschar                = UnixType.pcschar;
  cuchar                 = UnixType.cuchar;      pcuchar                = UnixType.pcuchar;

  cint16                 = UnixType.cint16;      pcint16                = UnixType.pcint16;
  cuint16                = UnixType.cuint16;     pcuint16               = UnixType.pcuint16;
  cshort                 = UnixType.cshort;      pcshort                = UnixType.pcshort;
  csshort                = UnixType.csshort;     pcsshort               = UnixType.pcsshort;
  cushort                = UnixType.cushort;     pcushort               = UnixType.pcushort;

  cint32                 = UnixType.cint32;      pcint32                = UnixType.pcint32;
  cuint32                = UnixType.cuint32;     pcuint32               = UnixType.pcuint32;
  cint                   = UnixType.cint;        pcint                  = UnixType.pcint;
  csint                  = UnixType.csint;       pcsint                 = UnixType.pcsint;
  cuint                  = UnixType.cuint;       pcuint                 = UnixType.pcuint;
  csigned                = UnixType.csigned;     pcsigned               = UnixType.pcsigned;
  cunsigned              = UnixType.cunsigned;   pcunsigned             = UnixType.pcunsigned;

  cint64                 = UnixType.cint64;      pcint64                = UnixType.pcint64;
  cuint64                = UnixType.cuint64;     pcuint64               = UnixType.pcuint64;
  clonglong              = UnixType.clonglong;   pclonglong             = UnixType.pclonglong;
  cslonglong             = UnixType.cslonglong;  pcslonglong            = UnixType.pcslonglong;
  culonglong             = UnixType.culonglong;  pculonglong            = UnixType.pculonglong;

  cbool                  = UnixType.cbool;       pcbool                 = UnixType.pcbool;

  clong                  = UnixType.clong;       pclong                 = UnixType.pclong;
  cslong                 = UnixType.cslong;      pcslong                = UnixType.pcslong;
  culong                 = UnixType.culong;      pculong                = UnixType.pculong;

{$ifndef FPUNONE}
  cfloat                 = UnixType.cfloat;      pcfloat                = UnixType.pcfloat;
  cdouble                = UnixType.cdouble;     pcdouble               = UnixType.pcdouble;
  clongdouble            = UnixType.clongdouble; pclongdouble           = UnixType.pclongdouble;
{$endif}

  csize_t                = UnixType.size_t;      pcsize_t               = UnixType.psize_t;
  coff_t                 = UnixType.TOff;

type
    dev_t    = UnixType.dev_t;
    TDev     = UnixType.TDev;
    pDev     = UnixType.pDev;
    gid_t    = UnixType.gid_t;
    TGid     = UnixType.TGid;
    TIOCtlRequest = UnixType.TIOCtlRequest;
    pGid     = UnixType.pGid;
    ino_t    = UnixType.ino_t;
    TIno     = UnixType.TIno;
    pIno     = UnixType.pIno;
    mode_t   = UnixType.mode_t;
    TMode    = UnixType.TMode;
    pMode    = UnixType.pMode;
    nlink_t  = UnixType.nlink_t;
    TnLink   = UnixType.TnLink;
    pnLink   = UnixType.pnLink;
    off_t    = UnixType.off_t;
    TOff     = UnixType.TOff;
    pOff     = UnixType.pOff;
    pid_t    = UnixType.pid_t;
    TPid     = UnixType.TPid;
    pPid     = UnixType.pPid;
    size_t   = UnixType.size_t;
    TSize    = UnixType.TSize;
    pSize    = UnixType.pSize;
    pSize_t  = UnixType.pSize_t;
    ssize_t  = UnixType.ssize_t;
    TsSize   = UnixType.TsSize;
    psSize   = UnixType.psSize;
    uid_t    = UnixType.uid_t;
    TUid     = UnixType.TUid;
    pUid     = UnixType.pUid;
    clock_t  = UnixType.clock_t;
    TClock   = UnixType.TClock;
    pClock   = UnixType.pClock;
    time_t   = UnixType.time_t;
    TTime    = UnixType.TTime;
    pTime    = UnixType.pTime;
    ptime_t  = UnixType.ptime_t;

    socklen_t= UnixType.socklen_t;
    TSocklen = UnixType.TSocklen;
    pSocklen = UnixType.pSocklen;

    timeval  = UnixType.timeval;
    ptimeval = UnixType.ptimeval;
    TTimeVal = UnixType.TTimeVal;
    timespec = UnixType.timespec;
    ptimespec= UnixType.ptimespec;
    Ttimespec= UnixType.Ttimespec;
    
    pthread_mutex_t   = UnixType.pthread_mutex_t;
    pthread_cond_t    = UnixType.pthread_cond_t;
    pthread_t         = UnixType.pthread_t;

    tstatfs  = UnixType.TStatFs;

CONST
    ARG_MAX       = UnixType.ARG_MAX;
    NAME_MAX      = UnixType.NAME_MAX;
    PATH_MAX      = UnixType.PATH_MAX;
    SYS_NMLN      = UnixType.SYS_NMLN;
    SIG_MAXSIG    = UnixType.SIG_MAXSIG;
    wordsinsigset = UnixType.wordsinsigset;

    PRIO_PROCESS  = UnixType.PRIO_PROCESS;
    PRIO_PGRP	  = UnixType.PRIO_PGRP;
    PRIO_USER	  = UnixType.PRIO_USER;


{$packrecords C}

 { Error numbers }

const

{$ifndef FPC_HAS_ESYS}
  ESysEPERM       = 1;    { Operation not permitted }
  ESysENOENT      = 2;    { No such file or directory }
  ESysESRCH       = 3;    { No such process }
  ESysEINTR       = 4;    { Interrupted system call }
  ESysEIO = 5;    { I/O error }
  ESysENXIO       = 6;    { No such device or address }
  ESysE2BIG       = 7;    { Arg list too long }
  ESysENOEXEC     = 8;    { Exec format error }
  ESysEBADF       = 9;    { Bad file number }
  ESysECHILD      = 10;   { No child processes }
  ESysEAGAIN      = 11;   { Try again }
  ESysENOMEM      = 12;   { Out of memory }
  ESysEACCES      = 13;   { Permission denied }
  ESysEFAULT      = 14;   { Bad address }
  ESysENOTBLK     = 15;   { Block device required, NOT POSIX! }
  ESysEBUSY       = 16;   { Device or resource busy }
  ESysEEXIST      = 17;   { File exists }
  ESysEXDEV       = 18;   { Cross-device link }
  ESysENODEV      = 19;   { No such device }
  ESysENOTDIR     = 20;   { Not a directory }
  ESysEISDIR      = 21;   { Is a directory }
  ESysEINVAL      = 22;   { Invalid argument }
  ESysENFILE      = 23;   { File table overflow }
  ESysEMFILE      = 24;   { Too many open files }
  ESysENOTTY      = 25;   { Not a typewriter }
  ESysETXTBSY     = 26;   { Text file busy. The new process was
                            a pure procedure (shared text) file which was
                            open for writing by another process, or file
                            which was open for writing by another process,
                            or while the pure procedure file was being
                            executed an open(2) call requested write access
                            requested write access.}
  ESysEFBIG       = 27;   { File too large }
  ESysENOSPC      = 28;   { No space left on device }
  ESysESPIPE      = 29;   { Illegal seek }
  ESysEROFS       = 30;   { Read-only file system }
  ESysEMLINK      = 31;   { Too many links }
  ESysEPIPE       = 32;   { Broken pipe }
  ESysEDOM        = 33;   { Math argument out of domain of func }
  ESysERANGE      = 34;   { Math result not representable }


  ESysEDEADLK     = 35;   { Resource deadlock would occur }
  ESysENAMETOOLONG= 36;   { File name too long }
  ESysENOLCK      = 37;   { No record locks available }
  ESysENOSYS      = 38;   { Function not implemented }
  ESysENOTEMPTY= 39;      { Directory not empty }
  ESysELOOP       = 40;   { Too many symbolic links encountered }
  ESysEWOULDBLOCK = ESysEAGAIN;   { Operation would block }
  ESysENOMSG      = 42;   { No message of desired type }
  ESysEIDRM       = 43;   { Identifier removed }
  ESysECHRNG      = 44;   { Channel number out of range }
  ESysEL2NSYNC= 45;       { Level 2 not synchronized }
  ESysEL3HLT      = 46;   { Level 3 halted }
  ESysEL3RST      = 47;   { Level 3 reset }
  ESysELNRNG      = 48;   { Link number out of range }
  ESysEUNATCH     = 49;   { Protocol driver not attached }
  ESysENOCSI      = 50;   { No CSI structure available }
  ESysEL2HLT      = 51;   { Level 2 halted }
  ESysEBADE       = 52;   { Invalid exchange }
  ESysEBADR       = 53;   { Invalid request descriptor }
  ESysEXFULL      = 54;   { Exchange full }
  ESysENOANO      = 55;   { No anode }
  ESysEBADRQC     = 56;   { Invalid request code }
  ESysEBADSLT     = 57;   { Invalid slot }
  ESysEDEADLOCK= 58;      { File locking deadlock error }
  ESysEBFONT      = 59;   { Bad font file format }
  ESysENOSTR      = 60;   { Device not a stream }
  ESysENODATA     = 61;   { No data available }
  ESysETIME       = 62;   { Timer expired }
  ESysENOSR       = 63;   { Out of streams resources }
  ESysENONET      = 64;   { Machine is not on the network }
  ESysENOPKG      = 65;   { Package not installed }
  ESysEREMOTE     = 66;   { Object is remote }
  ESysENOLINK     = 67;   { Link has been severed }
  ESysEADV        = 68;   { Advertise error }
  ESysESRMNT      = 69;   { Srmount error }
  ESysECOMM       = 70;   { Communication error on send }
  ESysEPROTO      = 71;   { Protocol error }
  ESysEMULTIHOP= 72;      { Multihop attempted }
  ESysEDOTDOT     = 73;   { RFS specific error }
  ESysEBADMSG     = 74;   { Not a data message }
  ESysEOVERFLOW= 75;      { Value too large for defined data type }
  ESysENOTUNIQ= 76;       { Name not unique on network }
  ESysEBADFD      = 77;   { File descriptor in bad state }
  ESysEREMCHG     = 78;   { Remote address changed }
  ESysELIBACC     = 79;   { Can not access a needed shared library }
  ESysELIBBAD     = 80;   { Accessing a corrupted shared library }
  ESysELIBSCN     = 81;   { .lib section in a.out corrupted }
  ESysELIBMAX     = 82;   { Attempting to link in too many shared libraries }
  ESysELIBEXEC= 83;       { Cannot exec a shared library directly }
  ESysEILSEQ      = 84;   { Illegal byte sequence }
  ESysERESTART= 85;       { Interrupted system call should be restarted }
  ESysESTRPIPE= 86;       { Streams pipe error }
  ESysEUSERS      = 87;   { Too many users }
  ESysENOTSOCK= 88;       { Socket operation on non-socket }
  ESysEDESTADDRREQ= 89;   { Destination address required }
  ESysEMSGSIZE= 90;       { Message too long }
  ESysEPROTOTYPE= 91;     { Protocol wrong type for socket }
  ESysENOPROTOOPT= 92;    { Protocol not available }
  ESysEPROTONOSUPPORT= 93;        { Protocol not supported }
  ESysESOCKTNOSUPPORT= 94;        { Socket type not supported }
  ESysEOPNOTSUPP= 95;     { Operation not supported on transport endpoint }
  ESysEPFNOSUPPORT= 96;   { Protocol family not supported }
  ESysEAFNOSUPPORT= 97;   { Address family not supported by protocol }
  ESysEADDRINUSE= 98;     { Address already in use }
  ESysEADDRNOTAVAIL= 99;  { Cannot assign requested address }
  ESysENETDOWN= 100;      { Network is down }
  ESysENETUNREACH= 101;   { Network is unreachable }
  ESysENETRESET= 102;     { Network dropped connection because of reset }
  ESysECONNABORTED= 103;  { Software caused connection abort }
  ESysECONNRESET= 104;    { Connection reset by peer }
  ESysENOBUFS     = 105;  { No buffer space available }
  ESysEISCONN     = 106;  { Transport endpoint is already connected }
  ESysENOTCONN= 107;      { Transport endpoint is not connected }
  ESysESHUTDOWN= 108;     { Cannot send after transport endpoint shutdown }
  ESysETOOMANYREFS= 109;  { Too many references: cannot splice }
  ESysETIMEDOUT= 110;     { Connection timed out }
  ESysECONNREFUSED= 111;  { Connection refused }
  ESysEHOSTDOWN= 112;     { Host is down }
  ESysEHOSTUNREACH= 113;  { No route to host }
  ESysEALREADY= 114;      { Operation already in progress }
  ESysEINPROGRESS= 115;   { Operation now in progress }
  ESysESTALE      = 116;  { Stale NFS file handle }
  ESysEUCLEAN     = 117;  { Structure needs cleaning }
  ESysENOTNAM     = 118;  { Not a XENIX named type file }
  ESysENAVAIL     = 119;  { No XENIX semaphores available }
  ESysEISNAM      = 120;  { Is a named type file }
  ESysEREMOTEIO= 121;     { Remote I/O error }
  ESysEDQUOT      = 122;  { Quota exceeded }
{$endif FPC_HAS_ESYS}

{$IFDEF FPC_IS_SYSTEM}
  
{$packrecords c}

Type

    dev_t    = cuint64;         { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

    kDev_t   = cushort;         // Linux has two different device conventions
    TkDev    = KDev_t;          // kernel and glibc. This is kernel.
    pkDev    = ^kdev_t;

    ino_t    = clong;           { used for file serial numbers }
    TIno     = ino_t;
    pIno     = ^ino_t;

    ino64_t  = cuint64;
    TIno64   = ino64_t;
    pIno64   = ^ino64_t;


    mode_t   = cuint32;      { used for file attributes     }

    TMode    = mode_t;
    pMode    = ^mode_t;

    nlink_t  = cuint32;         { used for link counts         }
    TnLink   = nlink_t;
    pnLink   = ^nlink_t;

{$if not defined(fs32bit)}
    off_t    = cint64;          { used for file sizes          }
{$else}
    off_t    = cint;
{$endif}
    TOff     = off_t;
    pOff     = ^off_t;

    off64_t  = cint64;
    TOff64   = off64_t;
    pOff64   = ^off64_t;

    pid_t    = cint;          { used as process identifier   }
    TPid     = pid_t;
    pPid     = ^pid_t;


    size_t   = cuint32;         { as definied in the C standard}
    ssize_t  = cint32;          { used by function for returning number of bytes }
    clock_t  = culong;
    time_t   = clong;           { used for returning the time  }

    wint_t    = cint32;
    TSize     = size_t;
    pSize     = ^size_t;
    psize_t   = pSize;
    TSSize    = ssize_t;
    pSSize    = ^ssize_t;
    TClock    = clock_t;
    pClock    = ^clock_t;
    TTime     = time_t;
    pTime     = ^time_t;
    ptime_t   = ^time_t;

    wchar_t   = cint32;
    pwchar_t  = ^wchar_t;


    uid_t    = cuint32;         { used for user ID type        }
    gid_t    = cuint32;         { used for group IDs           }
    ipc_pid_t = cushort;      // still 16-bit,should be 32 now.....

    TUid     = uid_t;
    pUid     = ^uid_t;
    TGid     = gid_t;
    pGid     = ^gid_t;
    
    TIOCtlRequest = cInt;


    socklen_t= cuint32;
    TSockLen = socklen_t;
    pSockLen = ^socklen_t;

  timeval     = packed record
                 tv_sec:time_t;
                 tv_usec:clong;
                end;
  ptimeval    = ^timeval;
  TTimeVal    = timeval;

  timespec    = packed record
                 tv_sec   : time_t;
                 tv_nsec  : clong;
                end;
  ptimespec   = ^timespec;
  TTimeSpec   = timespec;


  TStatfs = packed record
    fstype,            { File system type }
    bsize   : cint;    { Optimal block trensfer size }
    blocks,            { Data blocks in system }
    bfree,             { free blocks in system }
    bavail,            { Available free blocks to non-root users }
    files,             { File nodes in system }
    ffree   : culong;  { Free file nodes in system }
    fsid    : array[0..1] of cint;          { File system ID }
    namelen,           { Maximum name length in system }
    frsize  : cint;
    spare   : array [0..4] of cint; { For later use }
  end;

  PStatFS=^TStatFS;

  mbstate_value_t = record
    case byte of
      0: (__wch: wint_t);
      1: (__wchb: array[0..3] of char);
  end;
  
  mbstate_t = record
    __count: cint;
    __value: mbstate_value_t;
  end;
  pmbstate_t = ^mbstate_t;

  pthread_t = culong;

  sched_param = record
    __sched_priority: cint;
  end;

  pthread_attr_t = record
    __detachstate: cint;
    __schedpolicy: cint;
    __schedparam: sched_param;
    __inheritsched: cint;
    __scope: cint;
    __guardsize: size_t;
    __stackaddr_set: cint;
    __stackaddr: pointer;
    __stacksize: size_t;
  end;

//use spin_lock instead...
  _pthread_fastlock = record
    __status: clong;
    __spinlock: cint; //should be boolean according to MY sources, and many other code.
//this is a C GPF Security Hole. 256 bits in a boolean area again.
  end;

  pthread_mutex_t = record
    __m_reserved: cint;
    __m_count: cint;
    __m_owner: pointer;
    __m_kind:  cint;
    __m_lock: _pthread_fastlock;
  end;

  pthread_mutexattr_t = record
    __mutexkind: cint;
  end;

  pthread_cond_t = record
    __c_lock: _pthread_fastlock;
    __c_waiting: pointer;
    __padding: array[0..48-1-sizeof(_pthread_fastlock)-sizeof(pointer)-sizeof(clonglong)] of byte;
    __align: clonglong;
  end;

  pthread_condattr_t = record
    __dummy: cint;
  end;

  pthread_key_t = cuint;

  pthread_rwlock_t = record
    __rw_readers: cint;
    __rw_writer: pointer;
    __rw_read_waiting: pointer;
    __rw_write_waiting: pointer;
    __rw_kind: cint;
    __rw_pshared: cint; //should be boolean
  end;

  pthread_rwlockattr_t = record
    __lockkind: cint;
    __pshared: cint; //should be boolean
  end;

  sem_t = record
     __sem_lock: _pthread_fastlock;
     __sem_value: cint;
     __sem_waiting: pointer; //should be boolean
  end;



CONST
    _PTHREAD_MUTEX_TIMED_NP      = 0;
    _PTHREAD_MUTEX_RECURSIVE_NP  = 1;
    _PTHREAD_MUTEX_ERRORCHECK_NP = 2;
    _PTHREAD_MUTEX_ADAPTIVE_NP   = 3;

    _PTHREAD_MUTEX_NORMAL     = _PTHREAD_MUTEX_TIMED_NP;
    _PTHREAD_MUTEX_RECURSIVE  = _PTHREAD_MUTEX_RECURSIVE_NP;
    _PTHREAD_MUTEX_ERRORCHECK = _PTHREAD_MUTEX_ERRORCHECK_NP;
    _PTHREAD_MUTEX_DEFAULT    = _PTHREAD_MUTEX_NORMAL;
    _PTHREAD_MUTEX_FAST_NP    = _PTHREAD_MUTEX_ADAPTIVE_NP;


   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
 

    ARG_MAX        = 131072;   {4096}  { Maximum number of argument size     }
    NAME_MAX       = 255;      {14}    { Maximum number of bytes in filename }
    PATH_MAX       = 4095;     {255}   { Maximum number of bytes in pathname }
    SYS_NMLN       = 65;

   SIG_MAXSIG      = 128;       // highest signal version


 { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;


{$ENDIF}


CONST
//  SYS_NMLM                 = 65;
    UTSNAME_LENGTH           = SYS_NMLN;
    UTSNAME_NODENAME_LENGTH  = UTSNAME_LENGTH;
    {$ifdef usedomain}
    UTSNAME_DOMAIN_LENGTH    = UTSNAME_LENGTH;
    {$endif}

   FD_MAXFDSET     = 1024;
   BITSINWORD      = 8*sizeof(cuLong);
   wordsinsigset   = SIG_MAXSIG DIV BITSINWORD;         // words in sigset_t
   wordsinfdset    = FD_MAXFDSET DIV BITSINWORD;        // words in fdset_t
   ln2bitsinword   = 5;                                 { 32bit : ln(32)/ln(2)=5 }
   ln2bitsinword   = 6;                                 { 64bit : ln(64)/ln(2)=6 }
   ln2bitmask      = 1 shl ln2bitsinword - 1;


TYPE
   Blksize_t  = cuint;
   Blkcnt_t   = cuint;
   Blkcnt64_t = cuint64;

   TBlkSize   = BlkSize_t;
   PBlkSize   = ^BlkSize_t;
   TBlkCnt    = Blkcnt_t;
   PBlkCnt    = ^Blkcnt_t;

   { system information services }
   UtsName   = Record
                Sysname : Array[0..UTSNAME_LENGTH -1] OF Char;   // Name of this OS
                Nodename: Array[0..UTSNAME_NODENAME_LENGTH-1] OF Char;   // Name of this network node.
                Release : Array[0..UTSNAME_LENGTH -1] OF Char;   // Release level.
                Version : Array[0..UTSNAME_LENGTH -1] OF Char;   // Version level.
                Machine : Array[0..UTSNAME_LENGTH -1] OF Char;   // Hardware type.
               {$ifdef usedomain}
                Domain  : array[0..UTSNAME_DOMAIN_LENGTH-1] of char {$ifndef ver2_2} platform; {$endif}  // Linux addition "Domain"
               {$endif}
               end;
  TUtsName   = UtsName;
  PUtsName   = ^TUtsName;


{ Definition of (kernel) stat type }
{ see kernel/include/asm-<cpu>/stat.h, include/linux/types.h and }
{ include /include/asm-<cpu>/posix-types.h                       }

const
  _STAT_VER_LINUX_OLD = 1;
  _STAT_VER_KERNEL = 1;
  _STAT_VER_SVR4 = 2;
  _STAT_VER_LINUX = 3;
  _STAT_VER = _STAT_VER_LINUX;

type


 // kernel record

  Stat = packed record  
    case byte of
      0:
        (dev        : qword    deprecated;
        __pad0      : array[0..3] of byte deprecated;
        __ino       : cardinal deprecated;
        mode        : cardinal deprecated;
        nlink       : cardinal deprecated;
        uid	    : cardinal deprecated;
        gid         : cardinal deprecated;
        rdev        : qword deprecated;
        __pad3      : array[0..3] of byte deprecated;
        size        : qword    deprecated;
        blksize     : cardinal deprecated;
        blocks      : qword    deprecated;
        atime	    : cardinal deprecated;
        atime_nsec  : cardinal deprecated;
        mtime	    : cardinal deprecated; 
        mtime_nsec  : cardinal deprecated;
        ctime	    : cardinal deprecated;
        ctime_nsec  : cardinal deprecated;
        ino         : qword    deprecated);
      1:		// Unix typing will be reintroduced.
        (st_dev   : qword;
        __pad0_   : array[0..3] of byte;
        __st_ino_,
        st_mode,
        st_nlink,
        st_uid,
        st_gid    : cardinal;
        st_rdev   : qword;
        __pad3_   : array[0..3] of byte;
        st_size   : qword;
        st_blksize: cardinal;
        st_blocks : qword;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : cardinal;
        st_ino    : qword);
  end;




  TStat      = Stat;
  PStat      = ^Stat;

  { directory services }

  Dirent   = packed record
                d_fileno      : ino64_t;                        // file number of entry
                d_off         : off_t;
                d_reclen      : cushort;                        // length of string in d_name
                d_type        : cuchar;                         // file type, see below
                d_name        : array[0..(255 + 1)-1] of char;  // name must be no longer than this
               end;
  TDirent  = Dirent;
  pDirent  = ^Dirent;

{$ifdef oldreaddir}
           { Still old one. This is a userland struct}

   Dir       = record
                dd_fd     : integer;
                dd_loc    : longint;
                dd_size   : integer;
                dd_buf    : pdirent;
                {The following are used in libc, but NOT in the linux kernel sources ??}
                dd_nextoff: cardinal;
                dd_max : integer; {size of buf. Irrelevant, as buf is of type dirent}
                dd_lock   : pointer;
               end;

   TDir      = Dir;
   pDir      = ^Dir;


   UTimBuf   = Record
                 actime  : time_t;
                 modtime : time_t;
                end;

   TUtimBuf  = UtimBuf;
   pUtimBuf  = ^UtimBuf;

   kernel_off_t = clong;
   kernel_loff_t = clonglong;

   FLock     = Record
                l_type  : cshort;       { lock type: read/write, etc. }
                l_whence: cshort;       { type of l_start }
                l_start : kernel_off_t; { starting offset }
                l_len   : kernel_off_t; { len = 0 means until end of file }
                l_pid   : pid_t;        { lock owner }

               End;

{$ifndef cpu64}
   FLock64   = Record
                l_type  : cshort;        { lock type: read/write, etc. }
                l_whence: cshort;        { type of l_start }
                l_start : kernel_loff_t; { starting offset }
                l_len   : kernel_loff_t; { len = 0 means until end of file }
                l_pid   : pid_t;         { lock owner }
               End;
{$endif}

   tms       = packed Record
                tms_utime  : clock_t;   { User CPU time }
                tms_stime  : clock_t;   { System CPU time }
                tms_cutime : clock_t;   { User CPU time of terminated child procs }
                tms_cstime : clock_t;   { System CPU time of terminated child procs }
               end;
   TTms      = tms;
   PTms      = ^tms;

 TFDSet    = ARRAY[0..(FD_MAXFDSET div BITSINWORD)-1] of cuLong;
 pFDSet    = ^TFDSet;

  timezone = packed record
    tz_minuteswest,tz_dsttime:cint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

const
  POLLIN      = $0001;
  POLLPRI     = $0002;
  POLLOUT     = $0004;
  POLLERR     = $0008;
  POLLHUP     = $0010;
  POLLNVAL    = $0020;

  { XOpen, XPG 4.2 }
  POLLRDNORM  = $0040;
  POLLRDBAND  = $0080;
  POLLWRNORM  = $0100;
  POLLWRBAND  = $0200;

type
  pollfd = record
    fd: cint;
    events: cshort;
    revents: cshort;
  end;
  tpollfd = pollfd;
  ppollfd = ^pollfd;

{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK      =          0;        { test for existence of file }
    R_OK      =          4;        { test for read permission on file }
    W_OK      =          2;        { test for write permission on file }
    X_OK      =          1;        { test for execute or search permission }
    { seek routine }
    SEEK_SET  =          0;        { seek from beginning of file }
    SEEK_CUR  =          1;        { seek from current position  }
    SEEK_END  =          2;        { seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY  =          0;        { Open read-only.  }
    O_WRONLY  =          1;        { Open write-only. }
    O_RDWR    =          2;        { Open read/write. }

    O_CREAT   =        $40;
    O_EXCL    =        $80;
    O_NOCTTY  =       $100;
    O_TRUNC   =       $200;
    O_APPEND  =       $400;
    O_NONBLOCK =      $800;
    O_NDELAY  =     O_NONBLOCK;
    O_SYNC    =      $1000;
    O_DIRECT  =      $4000;
    O_DIRECTORY =   $10000;
    O_NOFOLLOW =    $20000;
    O_LARGEFILE =    $8000;

//needed for chmod and chown and chgrp commands
//change 'mode' and 'owner' and 'group' respectively.

    { mode_t possible values.these are bits in a byte                     }
    S_IRUSR =  %0100000000;     { Read permission for owner   }
    S_IWUSR =  %0010000000;     { Write permission for owner  }
    S_IXUSR =  %0001000000;     { Exec  permission for owner  }
    S_IRGRP =  %0000100000;     { Read permission for group   }
    S_IWGRP =  %0000010000;     { Write permission for group  }
    S_IXGRP =  %0000001000;     { Exec permission for group   }
    S_IROTH =  %0000000100;     { Read permission for world   }
    S_IWOTH =  %0000000010;     { Write permission for world  }
    S_IXOTH =  %0000000001;     { Exec permission for world   }

    S_IRWXU =  S_IRUSR or S_IWUSR or S_IXUSR; //rwx User
    S_IRWXG =  S_IRGRP or S_IWGRP or S_IXGRP; //rwx group 
    S_IRWXO =  S_IROTH or S_IWOTH or S_IXOTH; //rwx owner

    { Used for waitpid }
    WNOHANG   =          1;     { don't block waiting               }
    WUNTRACED =          2;     { report status of stopped children }

  { File types }
  S_IFMT  = 61440; { type of file mask}
  S_IFIFO = 4096;  { named pipe (fifo)}
  S_IFCHR = 8192;  { character special}
  S_IFDIR = 16384; { directory }
  S_IFBLK = 24576; { block special}
  S_IFREG = 32768; { regular }
  S_IFLNK = 40960; { symbolic link }
  S_IFSOCK= 49152; { socket }

  { Constansts for MMAP }
 {$IFDEF FPC_IS_SYSTEM}
  MAP_PRIVATE   =2;
 {$ENDIF}
  MAP_ANONYMOUS =$20;


  { For File control mechanism }
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_SetOwn = 8;
  F_GetOwn = 9;

{ getrlimit/ugetrlimit resource parameter constants }
const
  RLIMIT_CPU = 0;       { CPU time in ms  }
  RLIMIT_FSIZE = 1;     { Maximum filesize  }
  RLIMIT_DATA = 2;      { max data size  }
  RLIMIT_STACK = 3;     { max stack size  }
  RLIMIT_CORE = 4;      { max core file size  }
  RLIMIT_RSS = 5;       { max resident set size  }
  RLIMIT_NPROC = 6;     { max number of processes  }
  RLIMIT_NOFILE = 7;    { max number of open files  }
  RLIMIT_MEMLOCK = 8;   { max locked-in-memory address space  }
  RLIMIT_AS = 9;        { address space limit(?)  }
  RLIMIT_LOCKS = 10;    { maximum file locks held  }

type
  rlim_t = cULong;
  PRLimit = ^TRLimit;
  TRLimit = record
    rlim_cur : rlim_t;
    rlim_max : rlim_t;
  end;

  iovec = record
            iov_base : pointer;
	    iov_len  : size_t;
	   end;
  tiovec=iovec;
  piovec=^tiovec;		


  { Functions}


Type TGrpArr = Array [0..0] of TGid;            { C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;


    Function  FpUmask      (cmask : TMode): TMode;
    Function  FpLink       (existing : pChar; newone : pChar): cInt;
    Function  FpMkfifo     (path : pChar; Mode : TMode): cInt;
    Function  FpChmod      (path : pChar; Mode : TMode): cInt;
    Function  FpChown      (path : pChar; owner : TUid; group : TGid): cInt;
    Function  FpUtime      (path : pChar; times : putimbuf): cInt;
    Function  FpPipe       (var fildes : tfildes):cInt;
    Function  FpDup        (fildes : cInt): cInt;  external name 'FPC_SYSC_DUP';
    Function  FpDup2       (fildes, fildes2 : cInt): cInt; external name 'FPC_SYSC_DUP2';
    Function  FpTimes      (var buffer : tms): TClock;

    Function  FpAlarm      (seconds : cuint): cuint;
    Function  FpPause : cInt;
    Function  FpSleep      (seconds : cuint): cuint;

    Function  FpGetpid  : TPid; external name  'FPC_SYSC_GETPID';
    Function  FpGetppid : TPid;
    Function  FpGetuid  : TUid;
    Function  FpGeteuid : TUid;
    Function  FpGetgid  : TGid;
    Function  FpGetegid : TGid;
    Function  FpSetuid     (uid : TUid): cInt;
    Function  FpSetgid     (gid : TGid): cInt;
    Function  FpGetgroups (gidsetsize : cInt; var grouplist : tgrparr): cInt;
    Function  FpGetpgrp : TPid;
    Function  FpSetsid  : TPid;
    Function  FpFcntl      (fildes : cInt; cmd : cInt): cInt;
    Function  FpFcntl      (fildes : cInt; cmd : cInt; arg : cInt): cInt;
    Function  FpFcntl      (fildes : cInt; cmd : cInt; var arg : flock): cInt;

    Function  FpGetcwd     (path:pChar; siz:TSize):pChar;  external name 'FPC_SYSC_GETCWD';
    Function  FpExecve     (path : pChar; argv : ppChar; envp: ppChar): cInt;
    Function  FpExecv      (path : pChar; argv : ppChar): cInt;
    Function  FpUname      (var name: utsname): cInt;
    Function  FpOpendir    (dirname : pChar): pDir;  external name 'FPC_SYSC_OPENDIR';
    Function  FpReaddir    (var dirp : Dir) : pDirent; external name 'FPC_SYSC_READDIR';
    Function  FpClosedir   (var dirp : Dir): cInt; external name 'FPC_SYSC_CLOSEDIR';
    Function  FpChdir      (path : pChar): cInt;  external name 'FPC_SYSC_CHDIR';
    Function  FpOpen       (path : pChar; flags : cInt; Mode: TMode):cInt; external name 'FPC_SYSC_OPEN';
    Function  FpMkdir      (path : pChar; Mode: TMode):cInt;  external name 'FPC_SYSC_MKDIR';
    Function  FpUnlink     (path : pChar): cInt;  external name 'FPC_SYSC_UNLINK';
    Function  FpRmdir      (path : pChar): cInt; external name 'FPC_SYSC_RMDIR';
    Function  FpRename     (old  : pChar; newpath: pChar): cInt;   external name 'FPC_SYSC_RENAME';
    Function  FpFStat      (fd : cInt; var sb : stat): cInt; external name 'FPC_SYSC_FSTAT';
    Function  FpStat       (path: pChar; var buf : stat): cInt;  external name 'FPC_SYSC_STAT';
    Function  FpAccess     (pathname : pChar; aMode : cInt): cInt; external name 'FPC_SYSC_ACCESS';
    Function  FpClose      (fd : cInt): cInt;  external name 'FPC_SYSC_CLOSE';

    Function  FpRead       (fd : cInt; buf: pChar; nbytes : TSize): TSsize; external name 'FPC_SYSC_READ';
    Function  FpPRead      (fd : cInt; buf: pChar; nbytes : TSize; offset:Toff): TSsize;
    function  FpReadV	   (fd: cint; const iov : piovec; iovcnt : cint):TSSize;
    Function  FpWrite      (fd : cInt; buf:pChar; nbytes : TSize): TSsize;  external name 'FPC_SYSC_WRITE';
    Function  FpPWrite     (fd : cInt; buf:pChar; nbytes : TSize; offset:Toff): TSSize;
    function  FpWriteV	   (fd: cint; const iov : piovec; iovcnt : cint):TSSize; 

    Function  FpLseek      (fd : cInt; offset : TOff; whence : cInt): TOff; external name 'FPC_SYSC_LSEEK';
    Function  FpTime       (var tloc : TTime): TTime; external name 'FPC_SYSC_TIME';
    Function  FpFtruncate  (fd : cInt; flength : TOff): cInt;  external name 'FPC_SYSC_FTRUNCATE';
    Function  FPSigaction  (sig: cInt; act : pSigActionRec; oact : pSigActionRec): cint;  external name 'FPC_SYSC_SIGACTION';
    Function  FPSelect     (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint;
    Function  FpPoll       (fds: ppollfd; nfds: cuint; timeout: clong): cint;
    Function  FpIOCtl      (Handle:cint;Ndx: TIOCtlRequest; Data: Pointer):cint; external name  'FPC_SYSC_IOCTL';
    Function  fpLstat(path:pchar;Info:pstat):cint;
    Function  fpSymlink(oldname,newname:pchar):cint;
    Function  fpReadLink(name,linkname:pchar;maxlen:size_t):cint; external name  'FPC_SYSC_READLINK';

    Function  FpGetEnv     (name : pChar): pChar;
    function  fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;

    function FpGetRLimit(resource:cint;rlim:PRLimit):cint; external name 'FPC_SYSC_GETRLIMIT';
    function FpSetRLimit(Resource:cint;rlim:PRLimit):cint; external name 'FPC_SYSC_SETRLIMIT';
           


  function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
  procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
  property errno : cint read fpgeterrno write fpseterrno;

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt; inline;
Function  FpMkfifo  (path : AnsiString; Mode : TMode): cInt; inline;
Function  FpChmod   (path : AnsiString; Mode : TMode): cInt; inline;
Function  FpChown   (path : AnsiString; owner : TUid; group : TGid): cInt; inline;
Function  FpGetcwd : AnsiString;
Function  FpExecve  (path : AnsiString; argv : ppchar; envp: ppchar): cInt; inline;
Function  FpExecv   (path : AnsiString; argv : ppchar): cInt; inline;
Function  FpOpendir (dirname : AnsiString): pDir; inline;
Function  FpOpendir (dirname : shortString): pDir; inline;
Function  FpOpen    (path : pChar; flags : cInt):cInt; inline;
Function  FpOpen    (path : AnsiString; flags : cInt):cInt; inline;
Function  FpOpen    (path : AnsiString; flags : cInt; Mode: TMode):cInt; inline;
Function  FpOpen    (path : String; flags : cInt):cInt;
Function  FpOpen    (path : String; flags : cInt; Mode: TMode):cInt;
Function  FpChdir   (path : AnsiString): cInt; inline;
Function  FpMkdir   (path : AnsiString; Mode: TMode):cInt; inline;
Function  FpUnlink  (path : AnsiString): cInt; inline;
Function  FpRmdir   (path : AnsiString): cInt; inline;
Function  FpRename  (old  : AnsiString;newpath: AnsiString): cInt; inline;
Function  FpStat    (path: AnsiString; var buf : stat): cInt; inline;
Function  FpStat    (path: String; var buf : stat): cInt;
Function  fpLstat   (path: Ansistring; Info: pstat):cint; inline;
Function  fpLstat   (path:pchar;var Info:stat):cint; inline;
Function  fpLstat   (Filename: ansistring;var Info:stat):cint; inline;
Function  FpAccess  (pathname : AnsiString; aMode : cInt): cInt; inline;
function  FpWaitPid (pid : TPid; Var Status : cInt; Options : cint) : TPid;

Function  FPFStat   (var F:Text;Var Info:stat):Boolean; inline;
Function  FPFStat   (var F:File;Var Info:stat):Boolean; inline;

Function  FpRead    (fd : cInt; var buf; nbytes : TSize): TSsize; inline;
Function  FpWrite   (fd : cInt; const buf; nbytes : TSize): TSsize; inline;

function  FppRead   (fd : cInt; var buf; nbytes : TSize; offset:Toff): TSsize; inline;
function  FppWrite  (fd : cInt; const buf; nbytes : TSize; offset:Toff): TSsize; inline;

Function  FpDup     (var oldfile,newfile:text):cint;
Function  FpDup     (var oldfile,newfile:file):cint;
Function  FpDup2    (var oldfile,newfile:text):cint;
Function  FpDup2    (var oldfile,newfile:file):cint;
function  fptime    :time_t; inline;


Function fpSelect   (N:cint;readfds,writefds,exceptfds:pfdset;TimeOut:cint):cint;
Function fpSelect   (var T:Text;TimeOut :PTimeval):cint;
Function fpSelect   (var T:Text;TimeOut :time_t):cint;

Function  fpS_ISDIR    (m : TMode): Boolean;
Function  fpS_ISCHR    (m : TMode): Boolean;
Function  fpS_ISBLK    (m : TMode): Boolean;
Function  fpS_ISREG    (m : TMode): Boolean;
Function  fpS_ISFIFO   (m : TMode): Boolean;

// The following two are very common, but not POSIX.
Function  fpS_ISLNK       (m:TMode) : Boolean;
Function  fpS_ISSOCK      (m:TMode) : Boolean;

Function fpReadLink(Name:ansistring):ansistring;

//update this to NOT need /bin/sh..... TYPICAL UNIX....
function CreateShellArgV(const prog:string):ppchar; deprecated;
function CreateShellArgV(const prog:Ansistring):ppchar; deprecated;
procedure FreeShellArgV(p:ppchar); deprecated;

{ Fairly portable constants. I'm not going to waste time to duplicate and alias
them anywhere}

Const
  MAP_FAILED    = pointer(-1);  { mmap() has failed }
  MAP_SHARED    =  $1;          { Share changes }
  MAP_PRIVATE   =  $2;          { Changes are private }
  MAP_TYPE      =  $f;          { Mask for type of mapping }
  MAP_FIXED     = $10;          { Interpret addr exactly }

// MAP_ANON(YMOUS) is OS dependant but used in the RTL and in ostypes.inc

  MAP_ANON	= MAP_ANONYMOUS;

  PROT_READ     =  $1;          { page can be read }
  PROT_WRITE    =  $2;          { page can be written }
  PROT_EXEC     =  $4;          { page can be executed }
  PROT_NONE     =  $0;          { page can not be accessed }

implementation

Uses Sysctl;

// generic calls. (like getenv)

function InternalCreateShellArgV(cmd:pChar; len:longint):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c

Why not instead, call the command.pas unit to launch one? 

}
const   Shell   = '/bin/sh'#0'-c'#0;
var
  pp,p : ppchar;
//  temp : string; !! Never pass a local var back!!
begin
  getmem(pp,4*sizeof(pointer));
  p:=pp;
  p^:=@Shell[1];
  inc(p);
  p^:=@Shell[9];
  inc(p);
  getmem(p^,len+1);
  move(cmd^,p^^,len);
  pchar(p^)[len]:=#0;
  inc(p);
  p^:=Nil;
  InternalCreateShellArgV:=pp;
end;

function CreateShellArgV(const prog:string):ppchar;
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog));
end;

function CreateShellArgV(const prog:Ansistring):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
  using a AnsiString;
}
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog)); // if ppc works like delphi this also work when @prog[1] is invalid (len=0)
end;


procedure FreeShellArgV(p:ppchar);
begin
  if (p<>nil) then begin
    freemem(p[2]);
    freemem(p);
   end;
end;

{$ifndef FPC_USE_LIBC}
Function fpgetenv(name:pchar):pchar;

var
  p     : ppchar;
  found : boolean;
  np,cp : pchar;
  len,i : longint;
Begin
  if (name=nil) or (envp=NIL) Then
   exit(NIL);
  np:=name;
  while (np^<>#0) and (np^<>'=') DO
   inc(np);
  len:=np-name;
  p:=envp;
  while (p^<>NIL) DO
   Begin
    cp:=p^;
    np:=name;
    i:=len;
    while (i<>0) and (cp^<>#0) DO
      Begin
        if cp^<>np^ Then
          Begin
           inc(cp); inc(np);
           break;
         End;
        inc(cp); inc(np);
        dec(i)
      End;
    if (i=0) and (cp^='=') Then
        exit(cp+1);
   inc(p);
  end;
 fpgetenv:=nil;
End;
{$ENDIF}

Function fpgetenv(name:string):Pchar; [public, alias : 'FPC_SYSC_FPGETENV'];
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}

Begin
  name:=name+#0;
  fpgetenv:=fpgetenv(@name[1]);
end;

// general draft  'set' funcs.

function fpFD_SET(fdno:cint;var nset : TFDSet): cint;

Begin
   if (fdno<0) or (fdno > FD_MAXFDSET) Then
       exit(-1);
   nset[fdno shr ln2bitsinword]:=nset[(fdno) shr ln2bitsinword] OR (culong(1) shl ((fdno) and ln2bitmask));
   fpFD_SET:=0;
End;

function fpFD_CLR(fdno:cint;var nset : TFDSet): cint;

Begin
   if (fdno<0) or (fdno >  FD_MAXFDSET) Then
       exit(-1);
   nset[(fdno) shr ln2bitsinword]:=nset[(fdno) shr ln2bitsinword] AND Cardinal(NOT (culong(1) shl ((fdno) and ln2bitmask)));
   fpFD_CLR:=0;
End;

function fpFD_ZERO(var nset : TFDSet):cint;

var i :longint;

Begin
  for i:=0 to wordsinfdset-1 DO nset[i]:=0;
  fpFD_ZERO:=0;
End;

function fpfdfillset(var nset : TFDSet):cint;

var i :longint;

Begin
  for i:=0 to wordsinfdset-1 DO nset[i]:=Cardinal(NOT 0);
  fpfdfillset:=0;
End;

function fpFD_ISSET(fdno:cint;const nset : TFDSet): cint;

Begin
   if (fdno<0) or (fdno >  FD_MAXFDSET) Then
       exit(-1);
    if ((nset[fdno shr ln2bitsinword]) and (culong(1) shl ((fdno) and ln2bitmask)))>0 Then
     fpFD_ISSET:=1
    else
     fpFD_ISSET:=0;
End;


 // cpu specific syscalls
  {$i bsyscall.inc}      //fucked it up.have to restore this file.

Type
  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

Const   ITimer_Real    =0;
        ITimer_Virtual =1;
        ITimer_Prof    =2;

Function SetITimer(Which : Longint;Const value : ItimerVal; var VarOValue:ItimerVal):Longint;

Begin
  SetItimer:=Do_Syscall(syscall_nr_setitimer,Which,TSysParam(@Value),TSysParam(@varovalue));
End;

Function GetITimer(Which : Longint;Var value : ItimerVal):Longint;

Begin
  GetItimer:=Do_Syscall(syscall_nr_getItimer,Which,TSysParam(@value));
End;

Function fpalarm(Seconds: cuint):cuint;

Var it,oitv : Itimerval;
    retval  : cuint;

Begin
//      register struct itimerval *itp = &it;

 it.it_interval.tv_sec:=0;
 it.it_interval.tv_usec:=0;
 it.it_value.tv_usec:=0;
 it.it_value.tv_sec:=seconds;
 If SetITimer(ITIMER_REAL,it,oitv)<0 Then
   Exit(0);                     // different from *BSD!

 retval:= oitv.it_value.tv_usec;
 if retval<>0 Then
   inc(retval);
 fpAlarm:=retval;
End;

// The following versions are for internal use _ONLY_
// This because it works for the first 32 signals _ONLY_, but that
// is enough since they are depreciated, and for legacy applications
// anyway.


function fpsleep(seconds:cuint):cuint;

var time_to_sleep,time_remaining : timespec;
    nset,oset  : TSigSet;
    oerrno     : cint;
    oact       : sigactionrec;

begin
        time_to_sleep.tv_sec := seconds;
        time_to_sleep.tv_nsec := 0;
         fpsigemptyset(nset);
         fpsigaddset  (nset,SIGCHLD);
         if fpsigprocmask(SIG_BLOCK,@nset,@oset)=-1 Then
          exit(cuint(-1));
        if fpsigismember(oset,SIGCHLD)<>0 Then
          Begin
            fpsigemptyset(nset);
            fpsigaddset  (nset,SIGCHLD);
            if fpsigaction(SIGCHLD,NIL,@oact)<0 Then
              begin
                oerrno:=fpgeterrno;
                fpsigprocmask(SIG_SETMASK,@oset,NIL);
                fpseterrno(oerrno);
                exit(cuint(-1));
              End;
            if oact.sa_handler=SigActionhandler(SIG_IGN) Then
             Begin
               fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining);
               oerrno:=fpgeterrno;
               fpsigprocmask(SIG_SETMASK,@oset,NIL);
               fpseterrno(oerrno);
             End
            Else
             Begin
               fpsigprocmask(SIG_SETMASK,@oset,NIL);
               fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining)
             End;
          end
        else
            fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining);
        if fpsleep<>0 Then
         if time_remaining.tv_nsec>=500000000 Then
          inc(fpsleep);
End;

function fpuname(var name:utsname):cint; [public,alias:'FPC_SYSC_UNAME'];

begin
  fpuname:=Do_Syscall(syscall_nr_uname,TSysParam(@name));
end;

Function fpGetDomainName(Name:PChar; NameLen:size_t):cint;

Var
        srec  : utsname;
        tsize : size_t;
Begin
        if fpuname(srec)<0 Then
          exit(-1);
        tsize:=strlen(@srec.domain[0]);
        if tsize>(namelen-1) Then
         tsize:=namelen-1;
        move(srec.domain[0],name[0],tsize);
        name[namelen-1]:=#0;
        fpgetDomainName:=0;
End;

function fpGetHostName(Name:PChar; NameLen:size_t):cint;

Var
        srec  : utsname;
        tsize : size_t;
begin
        if fpuname(srec)<0 Then
          exit(-1);
        tsize:=strlen(@srec.nodename[0]);
        if tsize>(namelen-1) Then
         tsize:=namelen-1;
        move(srec.nodename[0],name[0],tsize);
        name[namelen-1]:=#0;
        fpgethostName:=0;
End;

const WAIT_ANY = -1;

function fpwait(var stat_loc:cint): pid_t;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
 fpWait:=do_syscall(syscall_nr_Wait4,WAIT_ANY,TSysParam(@Stat_loc),0,0);
end;

function fpgetpid : pid_t;

 begin
  fpgetpid:=do_syscall(syscall_nr_getpid);
 end;

function fpgetppid : pid_t;

begin
 fpgetppid:=do_syscall(syscall_nr_getppid);
end;

function fpgetuid : uid_t;

begin
 fpgetuid:=do_syscall(syscall_nr_getuid);
end;

function fpgeteuid : uid_t;

begin
 fpgeteuid:=do_syscall(syscall_nr_geteuid);
end;

function fpgetgid : gid_t;

begin
 fpgetgid:=do_syscall(syscall_nr_getgid);
end;

function fpgetegid : gid_t;

begin
 fpgetegid:=do_syscall(syscall_nr_getegid);
end;

function fpsetuid(uid : uid_t): cint;

begin
 fpsetuid:=do_syscall(syscall_nr_setuid,uid);
end;

function fpsetgid(gid : gid_t): cint;

begin
 fpsetgid:=do_syscall(syscall_nr_setgid,gid);
end;

// type tgrparr=array[0..0] of gid_t;

function fpgetgroups(gidsetsize : cint; var grouplist:tgrparr): cint;

begin
 fpgetgroups:=do_syscall(syscall_nr_getgroups,gidsetsize,TSysParam(@grouplist));
end;

function fpgetpgrp : pid_t;

begin
 fpgetpgrp:=do_syscall(syscall_nr_getpgrp);
end;

function fpsetsid : pid_t;

begin
 fpsetsid:=do_syscall(syscall_nr_setsid);
end;

Function fpumask(cmask:mode_t):mode_t;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 fpumask:=Do_syscall(syscall_nr_umask,cmask);
end;

Function fplink(existing:pchar;newone:pchar):cint;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  fpLink:=Do_Syscall(syscall_nr_link,TSysParam(existing),TSysParam(newone));
end;

Function fpmkfifo(path:pchar;mode:mode_t):cint;

begin

fpmkfifo:=do_syscall(syscall_nr_mknod,TSysParam(path),TSysParam(mode or S_IFIFO),TSysParam(0));
end;

Function fpchmod(path:pchar;mode:mode_t):cint;

begin
  fpchmod:=do_syscall(syscall_nr_chmod,TSysParam(path),TSysParam(mode));
end;

Function fpchown(path:pchar;owner:uid_t;group:gid_t):cint;

begin
  fpChOwn:=do_syscall(syscall_nr_chown,TSysParam(path),TSysParam(owner),TSysParam(group));
end;

Function fppipe(var fildes : tfildes):cint;

begin
 fppipe:=do_syscall(syscall_nr_pipe,TSysParam(@fildes));
end;

function fpfcntl(fildes:cint;Cmd:cint;Arg:cint):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,arg);
end;

function fpfcntl(fildes:cint;Cmd:cint;var Arg:flock):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,TSysParam(@arg));
end;

function fpfcntl(fildes:cint;Cmd:cint):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd);
end;

function fpexecve(path:pchar;argv:ppchar;envp:ppchar):cint;

Begin
  fpexecve:=do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(argv),TSysParam(envp));
End;

function fpexecv(path:pchar;argv:ppchar):cint;

Begin
  fpexecv:=do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(argv),TSysParam(envp));
End;

function fptimes(var buffer : tms):clock_t;
begin
  fptimes:=Do_syscall(syscall_nr_times,TSysParam(@buffer));
end;

Function fpSelect(N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}

begin
{$ifdef cpux86_64}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr_select,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$else}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr__newselect,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$endif}
{$ifndef bunxfunc_fpselect_implemented}
  {$error Implement fpselect first}
{$endif bunxfunc_fpselect_implemented}
end;

function fpPoll(fds: ppollfd; nfds: cuint; timeout: clong): cint;
begin
  fpPoll:=do_syscall(syscall_nr_poll,tsysparam(fds),tsysparam(nfds),tsysparam(timeout));
end;

Function fpLstat(path:pchar;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 fpLStat:=do_syscall(
    syscall_nr_lstat64,
    TSysParam(path),TSysParam(info));
end;


function fpNice(N:cint):cint;
{
  Set process priority. A positive N means a lower priority.
  A negative N increases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

begin
  fpNice:=do_syscall(Syscall_nr_nice,N);
end;

Function fpSymlink(oldname,newname:pchar):cint;
{
  We need this for erase
}

begin
  fpsymlink:=do_syscall(syscall_nr_symlink,TSysParam(oldname),TSysParam(newname));
end;

function Fppread(fd: cint; buf: pchar; nbytes : size_t; offset:Toff): ssize_t; [public, alias : 'FPC_SYSC_PREAD'];

begin
{$ifdef CPU64}
  Fppread:=do_syscall(syscall_nr_pread64,Fd,TSysParam(buf),nbytes,TSysParam(OffSet));
{$else}
  Fppread:=do_syscall(syscall_nr_pread,Fd,TSysParam(buf),nbytes,
    {$ifdef FPC_ABI_EABI}      0,  { align parameters as required with dummy } {$endif FPC_ABI_EABI}
    {$ifdef FPC_BIG_ENDIAN}    hi(offset),lo(offset){$endif}
    {$ifdef FPC_LITTLE_ENDIAN} lo(offset),hi(offset){$endif}
   );
{$endif}
end;

function Fppwrite(fd: cint;buf:pchar; nbytes : size_t; offset:Toff): ssize_t; [public, alias : 'FPC_SYSC_PWRITE'];

begin
{$ifdef CPU64}
  Fppwrite:=do_syscall(syscall_nr_pwrite64,Fd,TSysParam(buf),nbytes,TSysParam(OffSet));
{$else}
  Fppwrite:=do_syscall(syscall_nr_pwrite,Fd,TSysParam(buf),nbytes,
    {$ifdef FPC_ABI_EABI}      0,  { align parameters as required with dummy } {$endif FPC_ABI_EABI}
    {$ifdef FPC_BIG_ENDIAN}    hi(offset),lo(offset){$endif}
    {$ifdef FPC_LITTLE_ENDIAN} lo(offset),hi(offset){$endif}
   );
{$endif}
end;

function Fpreadv(fd: cint; const iov : piovec; iovcnt : cint):ssize_t; [public, alias : 'FPC_SYSC_READV'];

begin
  Fpreadv:=do_syscall(syscall_nr_readv,Fd,TSysParam(iov),iovcnt);
end;

function Fpwritev(fd: cint; const iov : piovec; iovcnt : cint):ssize_t;  [public, alias : 'FPC_SYSC_WRITEV'];

begin
  Fpwritev:=do_syscall(syscall_nr_writev,Fd,TSysParam(iov),iovcnt);
end;
     

{$ifdef usestime} //as in DateTime unit?
//these Tvariables are longints. --Jazz


function fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;
begin
  fpsettimeofday:=do_SysCall(Syscall_nr_settimeofday,TSysParam(tp),TSysParam(tzp));
end;

{$endif}


{ macro implenenations }

function FpS_ISDIR(m : TMode): boolean;

begin
  FpS_ISDIR:=((m and S_IFMT) = S_IFDIR);
end;

function FpS_ISCHR(m : TMode): boolean;
begin
  FpS_ISCHR:=((m and S_IFMT) = S_IFCHR);
end;

function FpS_ISBLK(m : TMode): boolean;
begin
 FpS_ISBLK:=((m and S_IFMT) = S_IFBLK);
end;

function FpS_ISREG(m : TMode): boolean;
begin
 FpS_ISREG:=((m and S_IFMT) = S_IFREG);
end;

function FpS_ISFIFO(m : TMode): boolean;
begin
 FpS_ISFIFO:=((m and S_IFMT) = S_IFIFO);
end;

Function FPS_ISLNK(m:TMode):boolean;

begin
 FPS_ISLNK:=((m and S_IFMT) = S_IFLNK);
end;

Function FPS_ISSOCK(m:TMode):boolean;

begin
 FPS_ISSOCK:=((m and S_IFMT) = S_IFSOCK);
end;

{ redefs and overloads implementation }

const
  TextRecNameLength = 256;
  TextRecBufSize    = 256;
type
  TLineEndStr = string [3];
  TextBuf = array[0..TextRecBufSize-1] of char;
  TextRec = Packed Record
    Handle    : THandle;
    Mode      : longint;
    bufsize   : SizeInt;
    _private  : SizeInt;
    bufpos,
    bufend    : SizeInt;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..32] of byte;
    name      : array[0..textrecnamelength-1] of char;
    LineEnd   : TLineEndStr;
    buffer    : textbuf;
  End;


const
  filerecnamelength = 255;
type
  FileRec = Packed Record
    Handle    : THandle;
    Mode      : longint;
    RecSize   : SizeInt;
    _private  : array[1..3 * SizeOf(SizeInt) + 5 * SizeOf (pointer)] of byte;
    UserData  : array[1..32] of byte;
    name      : array[0..filerecnamelength] of char;
  End;


Function  FpLink (existing : AnsiString; newone : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpLink:=FpLink(pchar(existing),pchar(newone));
End;

Function  FpMkfifo (path : AnsiString; Mode : TMode): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpMkfifo:=FpMkfifo(pchar(path),mode);
End;

Function  FpChmod (path : AnsiString; Mode : TMode): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpChmod:=FpChmod(pchar(path),mode);
End;

Function  FpChown (path : AnsiString; owner : TUid; group : TGid): cInt;{$ifdef VER2_0}inline;{$endif}
Begin
  FpChown:=FpChown(pchar(path),owner,group);
End;

Function  FpUtime (path : AnsiString; times : putimbuf): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpUtime:=FpUtime(pchar(path),times);
End;

Function  FpGetcwd :AnsiString;

Var
  Buf : Array[0..PATH_MAX+1]  of char;
Begin
  Buf[PATH_MAX+1]:=#0;
  If FpGetcwd(@Buf[0],PATH_MAX)=Nil then
    FpGetcwd:=''
  else
    FpGetcwd:=Buf;
End;

Function  FpExecve (path : AnsiString; argv : ppchar; envp: ppchar): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpExecve:=FpExecve (pchar(path),argv,envp);
End;

Function  FpExecv (path : AnsiString; argv : ppchar): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpExecv:=FpExecve (pchar(path),argv,envp);
End;


Function  FpChdir (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
 FpChDir:=FpChdir(pchar(Path));
End;

Function  FpOpen (path : AnsiString; flags : cInt; Mode: TMode):cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpOpen:=FpOpen(pchar(Path),flags,mode);
End;


Function  FpMkdir (path : AnsiString; Mode: TMode):cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpMkdir:=FpMkdir(pchar(Path),mode);
End;

Function  FpUnlink (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpUnlink:=FpUnlink(pchar(path));
End;

Function  FpRmdir (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpRmdir:=FpRmdir(pchar(path));
End;

Function  FpRename (old  : AnsiString;newpath: AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpRename:=FpRename(pchar(old),pchar(newpath));
End;

Function  FpStat (path: AnsiString; var buf : stat): cInt; {$ifdef VER2_0}inline;{$endif}
begin
  FpStat:=FpStat(pchar(path),buf);
End;

Function  fpLstat   (path: Ansistring; Info: pstat):cint; inline;
begin
  fplstat:=fplstat(pchar(path), info);
end;

Function  fpLstat   (path:pchar;var Info:stat):cint; inline;

begin
  fpLstat:=fplstat(path,@info);
end;

Function  fpLstat   (Filename: ansistring;var Info:stat):cint; inline;

begin
  fpLstat:=fplstat(filename,@info);
end;

Function FpAccess (pathname : AnsiString; aMode : cInt): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpAccess:=FpAccess(pchar(pathname),amode);
End;

Function  FPFStat(var F:Text;Var Info:stat):Boolean; {$ifdef VER2_0}inline;{$endif}
{
  Get all information on a text file, and return it in info.
}
begin
  FPFStat:=FPFstat(TextRec(F).Handle,INfo)=0;
end;

Function  FPFStat(var F:File;Var Info:stat):Boolean; {$ifdef VER2_0}inline;{$endif}
{
  Get all information on a untyped file, and return it in info.
}
begin
  FPFStat:=FPFstat(FileRec(F).Handle,Info)=0;
end;

function xFpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; external name 'FPC_SYSC_READ';


Function  FpRead           (fd : cInt;var buf; nbytes : TSize): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FPRead:=xFpRead(fd,pchar(@buf),nbytes);
end;

Function  FpWrite          (fd : cInt;const buf; nbytes : TSize): TSsize; {$ifdef VER2_0}inline;{$endif}
begin
 FpWrite:=FpWrite(fd,pchar(@buf),nbytes);
end;

function  FppRead           (fd : cInt;var buf; nbytes : TSize; offset:Toff): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FppRead:=FppRead(fd,pchar(@buf),nbytes,offset);
end;

function  FppWrite          (fd : cInt;const buf; nbytes : TSize; offset:Toff): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FppWrite:=FppWrite(fd,pchar(@buf),nbytes,offset);
end;

Function  FpOpen    (path : pChar; flags : cInt):cInt; {$ifdef VER2_0}inline;{$endif}

begin
 FpOpen:=FpOpen(path,flags,438);
end;

Function  FpOpen    (path : AnsiString; flags : cInt):cInt; {$ifdef VER2_0}inline;{$endif}

begin
 FpOpen:=FpOpen(pchar(path),flags,438);
end;

Function  FpOpen    (path : String; flags : cInt):cInt;

begin
 path:=path+#0;
 FpOpen:=FpOpen(@path[1],flags,438);
end;

Function  FpOpen    (path : String; flags : cInt; Mode: TMode):cInt;

begin
 path:=path+#0;
 FpOpen:=FpOpen(@path[1],flags,Mode);
end;

Function  FpOpendir (dirname : AnsiString): pDir; {$ifdef VER2_0}inline;{$endif}
Begin
  FpOpenDir:=FpOpenDir(pchar(dirname));
End;


Function  FpOpendir (dirname : shortString): pDir; {$ifdef VER2_0}inline;{$endif}
Begin
  dirname:=dirname+#0;
  FpOpenDir:=FpOpenDir(pchar(@dirname[1]));
End;


Function  FpStat (path: String; var buf : stat): cInt;

begin
 path:=path+#0;
 FpStat:=FpStat(pchar(@path[1]),buf);
end;

Function fpDup(var oldfile,newfile:text):cint;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
begin
  flush(oldfile);{ We cannot share buffers, so we flush them. }
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  textrec(newfile).handle:=fpDup(textrec(oldfile).handle);
  fpdup:=textrec(newfile).handle;
end;

Function fpDup(var oldfile,newfile:file):cint;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  filerec(newfile).handle:=fpDup(filerec(oldfile).handle);
  fpdup:=  filerec(newfile).handle;
end;


Function FpDup2(var oldfile,newfile:text):cint;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile. It closes newfile if it was still open.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
var
  tmphandle : word;
begin
  case TextRec(oldfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(oldfile);{ We cannot share buffers, so we flush them. }
  end;
  case TextRec(newfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(newfile);
  end;
  tmphandle:=textrec(newfile).handle;
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).handle:=tmphandle;
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  fpDup2:=fpDup2(textrec(oldfile).handle,textrec(newfile).handle);
end;

Uses Syscall;

function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;

begin
  if (pointer(func)=nil) or (sp=nil) then
   exit(-1); // give an error result
{$ASMMODE ATT}
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        movl    flags,%ebx
        movl    SysCall_nr_clone,%eax
        int     $0x80
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
end;

// TODO: update also on non x86!

function vmsplice (fdout: cInt; iov: PIOVec; count: size_t; flags: cuInt): cInt;
begin
  vmsplice := do_syscall(syscall_nr_vmsplice, TSysParam(fdout), TSysParam(iov), 
    TSysParam(count), TSysParam(flags));
end;


{ FUTEX_OP is a macro, doesn't exist in libC as function}
function FUTEX_OP(op, oparg, cmp, cmparg: cint): cint; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  FUTEX_OP := ((op and $F) shl 28) or ((cmp and $F) shl 24) or ((oparg and $FFF) shl 12) or (cmparg and $FFF);
end;

end.
end.
