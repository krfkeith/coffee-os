{
 
Should take most of the useable (and non-wrapped) functions from the FPC RTL/ObjectPascal units therein.

}

Unit Math;
//All known math functions.

interface
{ **********************************************************************
  *                some parts derived from       Unit FMATH.PAS                          *
  *                             Version 2.7f                           *
  *                     (c) J. Debord, December 2002                   *

  Notes:

  1) The default real type is DOUBLE (8-byte real).
     Other types may be selected by defining the symbols:

       SINGLEREAL   (Single precision, 4 bytes)
       EXTENDEDREAL (Extended precision, 10 bytes)

  2) Error handling: The function MathError returns the error code from
     the last function evaluation. It must be checked immediately after
     a function call:

       Y := f(X);        (* f is one of the functions of the library *)
       if MathError = FN_OK then ...

     The possible error codes, and the default values attributed to the
     function, are the following:

     ------------------------------------------------------------------
     Error code   Value  Significance            Function default value
     ------------------------------------------------------------------
     FN_OK          0    No error
     FN_DOMAIN     -1    Argument domain error   0
     FN_SING       -2    Function singularity    +/- MAXNUM
     FN_OVERFLOW   -3    Overflow range error    MAXNUM
     FN_UNDERFLOW  -4    Underflow range error   0
     ------------------------------------------------------------------

     where MAXNUM is a constant defining the highest number which may be
     represented within the chosen floating point type.

     The standard functions Exp and Ln have been redefined according to
     the above conventions as Expo and Log.

     The assembler functions may be accessed in two ways:

     * Call the Pascal functions (e.g. Expo, ArcSin...). This will
       provide some acceleration while keeping the error handling.

    
  **********************************************************************
  References:

  Special functions: translated C code from Cephes math library
                     by S. Moshier (http://www.moshier.net)

  Complex functions: modified Pascal code from E. F. Glynn
                     (http://www.efg2.com/Lab/)

  Thanks to J. D. Gayrard and P. Nogaret for the assembler functions.
  ********************************************************************** }

type
  TFPURoundingMode = (rmNearest, rmDown, rmUp, rmTruncate);
  TFPUPrecisionMode = (pmSingle, pmReserved, pmDouble, pmExtended);
  TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
                   exOverflow, exUnderflow, exPrecision);
  TFPUExceptionMask = set of TFPUException;
  float=extended;
  TValueRelationship = -1..1;

  DegreeType =  record
                  Degrees, Minutes, Seconds : real;
                end;
const
  Infinity = 9.9999999999E+37;
  NANDoubleArray : ARRAY[0..7] OF BYTE = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
var
  nan : double absolute NANDoubleArray; 

 RaisePending:boolean;
 mxcsr:dword;

{$packrecords C}

const 
   EqualsValue = 0;
   GreaterThanValue = High ( TValueRelationship );
   LessThanValue = Low ( TValueRelationship );

function GetRoundMode: TFPURoundingMode;
function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
function GetPrecisionMode: TFPUPrecisionMode;
function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
function GetExceptionMask: TFPUExceptionMask;
function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
procedure ClearExceptions(RaisePending: Boolean);
 procedure Set8087CW(cw:word);
function Get8087CW:word;assembler;
procedure SetSSECSR(w : dword);
function GetSSECSR : dword;
function Isqr(x : integer) : longint ; assembler;
function Imul(x, y : integer) : longint ; assembler;

{  Radians  }
{ sin, cos, and arctan are predefined }

function Tan( Radians : real ) : real;
function ArcSin( InValue : real ) : real;
function ArcCos( InValue : real ) : real;

{  Degrees, expressed as a real number  }

function DegreesToRadians( Degrees : real ) : real;
function RadiansToDegrees( Radians : real ) : real;
function Sin_Degree( Degrees : real ) : real;
function Cos_Degree( Degrees : real ) : real;
function Tan_Degree( Degrees : real ) : real;
function ArcSin_Degree( Degrees : real ) : real;
function ArcCos_Degree( Degrees : real ) : real;
function ArcTan_Degree( Degrees : real ) : real;

{  Degrees, in Degrees, Minutes, and Seconds, as real numbers  }

function DegreePartsToDegrees( Degrees, Minutes, Seconds : real ) : real;
function DegreePartsToRadians( Degrees, Minutes, Seconds : real ) : real;
procedure DegreesToDegreeParts( DegreesIn : real;
                                var Degrees, Minutes, Seconds : real );
procedure RadiansToDegreeParts( Radians : real;
                                var Degrees, Minutes, Seconds : real );
function Sin_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
function Cos_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
function Tan_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
function ArcSin_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
function ArcCos_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
function ArcTan_DegreeParts( Degrees, Minutes, Seconds : real ) : real;
{  Degrees, expressed as DegreeType ( reals in record ) }

function DegreeTypeToDegrees( DegreeVar : DegreeType ) : real;
function DegreeTypeToRadians( DegreeVar : DegreeType ) : real;
procedure DegreeTypeToDegreeParts( DegreeVar : DegreeType;
                                   var Degrees, Minutes, Seconds : real );
procedure DegreesToDegreeType( Degrees : real; var DegreeVar : DegreeType );
procedure RadiansToDegreeType( Radians : real; var DegreeVar : DegreeType );
procedure DegreePartsToDegreeType( Degrees, Minutes, Seconds : real;
                                   var DegreeVar : DegreeType );
function Sin_DegreeType( DegreeVar : DegreeType ) : real;
function Cos_DegreeType( DegreeVar : DegreeType ) : real;
function Tan_DegreeType( DegreeVar : DegreeType ) : real;
function ArcSin_DegreeType( DegreeVar : DegreeType ) : real;
function ArcCos_DegreeType( DegreeVar : DegreeType ) : real;
function ArcTan_DegreeType( DegreeVar : DegreeType ) : real;

{  Hyperbolic functions  }

function Sinh( Invalue : real ) : real;
function Cosh( Invalue : real ) : real;
function Tanh( Invalue : real ) : real;
function Coth( Invalue : real ) : real;
function Sech( Invalue : real ) : real;
function Csch( Invalue : real ) : real;
function ArcSinh( Invalue : real ) : real;
function ArcCosh( Invalue : real ) : real;
function ArcTanh( Invalue : real ) : real;
function ArcCoth( Invalue : real ) : real;
function ArcSech( Invalue : real ) : real;
function ArcCsch( Invalue : real ) : real;

{  Logarithms, Powers, and Roots  }

{ e to the x  is  exp() }
{ natural log is  ln()  }
function Log10( InNumber : real ) : real;
function Log( Base, InNumber : real ) : real;  { log of any base }
function Power( InNumber, Exponent : real ) : real;
function Root( InNumber, TheRoot : real ) : real;


{
		sinh
		cosh
		complex sinh
		complex cosh
		complex addition
		complex subtraction
		rectangular-to-polar coordinate conversion
		polar-to-rectangular coordinate conversion
		complex multiplication
		complex division
		complex exponential

}


TYPE 
	CompStr = String[2*SizeOf(Real)];
	complex = RECORD
				CASE Byte OF
					0 : (s : CompStr);
					1 : (len  : Byte;
						 R, I : Real);
					2 : (leng : Byte;
						 Rad, Theta : Real);
			  END;
	FUNCTION C2S(C : Complex; w, d : Byte) : String;
	FUNCTION P2S(C : Complex; w, d : Byte) : String;
	FUNCTION sinh(x : Real) : Real; 
	FUNCTION cosh(x : Real) : Real; 
	FUNCTION CmpAdd(A, B : Complex) : CompStr;
	FUNCTION CmpSub(A, B : Complex) : CompStr;
	FUNCTION CmpMul(A, B : Complex) : CompStr;
	FUNCTION CmpDiv(A, B : Complex) : CompStr;
	FUNCTION CmpExp(X    : Complex) : CompStr;
	FUNCTION CmpSinh(X   : Complex) : CompStr;
	FUNCTION CmpCosh(X   : Complex) : CompStr;
	
	FUNCTION RecToPol(X  : Complex) : CompStr; {rectangular to polar coordinates }
	FUNCTION PolToRec(X  : Complex) : CompStr; {polar to rectangular coordinates }
	

{ ----------------------------------------------------------------------
  Floating point type (Default = Double)
  ---------------------------------------------------------------------- }

{$IFDEF SINGLEREAL}
  type Float = Single;
{$ELSE}
{$IFDEF EXTENDEDREAL}
  type Float = Extended;
{$ELSE}
  {$DEFINE DOUBLEREAL}
  type Float = Double;
{$ENDIF}
{$ENDIF}

{ ----------------------------------------------------------------------
  Mathematical constants
  ---------------------------------------------------------------------- }

const
  PI         = 3.14159265358979323846;  { Pi }
  LN2        = 0.69314718055994530942;  { Ln(2) }
  LN10       = 2.30258509299404568402;  { Ln(10) }
  LNPI       = 1.14472988584940017414;  { Ln(Pi) }
  INVLN2     = 1.44269504088896340736;  { 1/Ln(2) }
  INVLN10    = 0.43429448190325182765;  { 1/Ln(10) }
  TWOPI      = 6.28318530717958647693;  { 2*Pi }
  PIDIV2     = 1.57079632679489661923;  { Pi/2 }
  SQRTPI     = 1.77245385090551602730;  { Sqrt(Pi) }
  SQRT2PI    = 2.50662827463100050242;  { Sqrt(2*Pi) }
  INVSQRT2PI = 0.39894228040143267794;  { 1/Sqrt(2*Pi) }
  LNSQRT2PI  = 0.91893853320467274178;  { Ln(Sqrt(2*Pi)) }
  LN2PIDIV2  = 0.91893853320467274178;  { Ln(2*Pi)/2 }
  SQRT2      = 1.41421356237309504880;  { Sqrt(2) }
  SQRT2DIV2  = 0.70710678118654752440;  { Sqrt(2)/2 }
  GOLD       = 1.61803398874989484821;  { Golden Mean = (1 + Sqrt(5))/2 }
  CGOLD      = 0.38196601125010515179;  { 2 - GOLD }
//C(speed of light)....

{ ----------------------------------------------------------------------
  Machine-dependent constants
  ---------------------------------------------------------------------- }

{$IFDEF SINGLEREAL}
const
  MACHEP = 1.192093E-7;               { Floating point precision: 2^(-23) }
  MAXNUM = 3.402823E+38;              { Max. floating point number: 2^128 }
  MINNUM = 1.175495E-38;              { Min. floating point number: 2^(-126) }
  MAXLOG = 88.72283;                  { Max. argument for Exp = Ln(MAXNUM) }
  MINLOG = -87.33655;                 { Min. argument for Exp = Ln(MINNUM) }
  MAXFAC = 33;                        { Max. argument for Factorial }
  MAXGAM = 34.648;                    { Max. argument for Gamma }
  MAXLGM = 1.0383E+36;                { Max. argument for LnGamma }
{$ELSE}
{$IFDEF DOUBLEREAL}
const
  MACHEP = 2.220446049250313E-16;     { 2^(-52) }
  MAXNUM = 1.797693134862315E+308;    { 2^1024 }
  MINNUM = 2.225073858507202E-308;    { 2^(-1022) }
  MAXLOG = 709.7827128933840;
  MINLOG = -708.3964185322641;
  MAXFAC = 170;
  MAXGAM = 171.624376956302;
  MAXLGM = 2.556348E+305;
{$ELSE}
{$IFDEF EXTENDEDREAL}
const
  MACHEP = 1.08420217248550444E-19;   { 2^(-63) }
  MAXNUM = 1.18973149535723103E+4932; { 2^16384 }
  MINNUM = 3.36210314311209558E-4932; { 2^(-16382) }
  MAXLOG = 11356.5234062941439;
  MINLOG = - 11355.137111933024;
  MAXFAC = 1754;
  MAXGAM = 1755.455;
  MAXLGM = 1.04848146839019521E+4928;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ ----------------------------------------------------------------------
  Error codes for mathematical functions
  ---------------------------------------------------------------------- }

const
  FN_OK        =   0;  { No error }
  FN_DOMAIN    = - 1;  { Argument domain error }
  FN_SING      = - 2;  { Function singularity }
  FN_OVERFLOW  = - 3;  { Overflow range error }
  FN_UNDERFLOW = - 4;  { Underflow range error }
  FN_TLOSS     = - 5;  { Total loss of precision }
  FN_PLOSS     = - 6;  { Partial loss of precision }

{ ----------------------------------------------------------------------
  Global variables and constants
  ---------------------------------------------------------------------- }

const
  NFACT = 33;  { The factorials of the first NFACT integers are stored
                 in a table }
var
  MathErr : Integer;  { Error code from the latest function call }

  FactArray : array[0..NFACT] of Float;  { Table of factorials }

{ ----------------------------------------------------------------------
  Functional type
  ---------------------------------------------------------------------- }

type
  TFunc = function(X : Float) : Float;

{ ----------------------------------------------------------------------
  Complex numbers
  ---------------------------------------------------------------------- }

type
  Complex =
    record
      X, Y : Float;
    end;

{ ----------------------------------------------------------------------
  Complex constants
  ---------------------------------------------------------------------- }

const
  C_infinity : Complex = (X : MAXNUM; Y : 0.0);
  C_zero     : Complex = (X : 0.0;    Y : 0.0);
  C_one      : Complex = (X : 1.0;    Y : 0.0);
  C_i        : Complex = (X : 0.0;    Y : 1.0);
  C_pi       : Complex = (X : PI;     Y : 0.0);
  C_pi_div_2 : Complex = (X : PIDIV2; Y : 0.0);

{ ----------------------------------------------------------------------
  Error handling function
  ---------------------------------------------------------------------- }

function MathError : Integer;  { Error code from the last function call }

{ ----------------------------------------------------------------------
  Min, max, sign and exchange
  ---------------------------------------------------------------------- }

function Min(X, Y : Integer) : Integer;
function Max(X, Y : Integer) : Integer;

function Min(X, Y : Float) : Float;
function Max(X, Y : Float) : Float;

function Sgn0(X : Float)  : Integer;  { Sign (returns 0 if X = 0) }
function Sgn(X : Float)   : Integer;  { Sign (returns 1 if X = 0) }
function Sgn(Z : Complex) : Integer;  { Complex sign }

procedure Swap(var X, Y : Float);     { Exchange 2 reals }
procedure Swap(var X, Y : Integer);   { Exchange 2 integers }
procedure Swap(var W, Z : Complex);   { Exchange 2 complexes }

{ ----------------------------------------------------------------------
  Complex numbers
  ---------------------------------------------------------------------- }

operator := (X : Float)   Result : Complex;
operator -  (Z : Complex) Result : Complex;

operator = (Z : Complex; X : Float) Result : Boolean;

operator + (Z1, Z2 : Complex)       Result : Complex;
operator + (Z : Complex; X : Float) Result : Complex;
operator + (X : Float; Z : Complex) Result : Complex;

operator - (Z1, Z2 : Complex)       Result : Complex;
operator - (Z : Complex; X : Float) Result : Complex;
operator - (X : Float; Z : Complex) Result : Complex;

operator * (Z1, Z2 : Complex)       Result : Complex;
operator * (Z : Complex; X : Float) Result : Complex;
operator * (X : Float; Z : Complex) Result : Complex;

operator / (Z1, Z2 : Complex)       Result : Complex;
operator / (Z : Complex; X : Float) Result : Complex;
operator / (X : Float; Z : Complex) Result : Complex;

function Cmplx(X, Y : Float)     : Complex;  { X + i.Y }
function Polar(R, Theta : Float) : Complex;  { R.exp(i.Theta) }

function CAbs(Z : Complex)  : Float;         { |Z| }
function CArg(Z : Complex)  : Float;         { Arg(Z) }
function CConj(A : Complex) : Complex;       { A* }

function CSqrt(Z : Complex)                 : Complex;  { Sqrt(Z) }
function CRoot(Z : Complex; K, N : Integer) : Complex;  { Z^(1/N), K=0..N-1 }

function CSin(Z : Complex)    : Complex;     { Sin(Z) }
function CCos(Z : Complex)    : Complex;     { Cos(Z) }
function CArcTan(Z : Complex) : Complex;     { ArcTan(Z) }

{ ----------------------------------------------------------------------
  Logarithms, exponentials, and powers (real or complex argument)
  ---------------------------------------------------------------------- }

function Expo(X : Float)   : Float;     { Exponential }
function Expo(Z : Complex) : Complex;

function Log(X : Float)   : Float;      { Natural log }
function Log(Z : Complex) : Complex;

function Power(Z : Complex; N : Integer) : Complex;  { Z^N }
function Power(Z : Complex; X : Float)   : Complex;  { Z^X }
function Power(A, B : Complex)           : Complex;  { A^B }

{ ----------------------------------------------------------------------
  Other logarithms and exponentials (real argument only)
  ---------------------------------------------------------------------- }

function Exp2(X : Float)    : Float;  { 2^X }
function Exp10(X : Float)   : Float;  { 10^X }
function Log2(X : Float)    : Float;  { Log, base 2 }
function Log10(X : Float)   : Float;  { Decimal log }
function LogA(X, A : Float) : Float;  { Log, base A }

{ ----------------------------------------------------------------------
  Trigonometric functions (real or complex argument)
  ---------------------------------------------------------------------- }

function Tan(X : Float)   : Float;
function Tan(Z : Complex) : Complex;

function ArcSin(X : Float)   : Float;
function ArcSin(Z : Complex) : Complex;

function ArcCos(X : Float)   : Float;
function ArcCos(Z : Complex) : Complex;

{ ----------------------------------------------------------------------
  Other trigonometric functions (real argument only)
  ---------------------------------------------------------------------- }

function Pythag(X, Y : Float)    : Float;  { Sqrt(X^2 + Y^2) }
function ArcTan2(Y, X : Float)   : Float;  { Angle (Ox, OM) with M(X,Y) }
function FixAngle(Theta : Float) : Float;  { Set Theta in -Pi..Pi }

{ ----------------------------------------------------------------------
  Hyperbolic functions (real or complex argument)
  ---------------------------------------------------------------------- }

function Sinh(X : Float)   : Float;
function Sinh(Z : Complex) : Complex;

function Cosh(X : Float)   : Float;
function Cosh(Z : Complex) : Complex;

function Tanh(X : Float)   : Float;
function Tanh(Z : Complex) : Complex;

function ArcSinh(X : Float)   : Float;
function ArcSinh(Z : Complex) : Complex;

function ArcCosh(X : Float)   : Float;
function ArcCosh(Z : Complex) : Complex;

function ArcTanh(X : Float)   : Float;
function ArcTanh(Z : Complex) : Complex;

{ ----------------------------------------------------------------------
  Other hyperbolic function (real argument only)
  ---------------------------------------------------------------------- }

procedure SinhCosh(X : Float; var SinhX, CoshX : Float);  { Sinh & Cosh }


{$IFDEF CPUP2}
//these are assembler functions  
function fSin(X : Float): Float;
function fCos(X : Float): Float;
function fTan(X : Float): Float;

function fArcSin(X : Float): Float;
function fArcCos(X : Float): Float;
function fArcTan(X : Float): Float;
function fArcTan2(y, X : Float): Float;

function fLn(X : Float): Float;
function fLog2(X : Float): Float;
function fLog10(X : Float): Float;
function fExp(X : Float): Float;
function fExp2(X : Float): Float;
function fExp10(X : Float): Float;

function fSinh(X : Float): Float;
function fCosh(X : Float): Float;
function fTanh(X : Float): Float;

function fArcSinh(X : Float): Float;
function fArcCosh(X : Float): Float;
function fArcTanh(X : Float): Float;


{$ENDIF}

{ ----------------------------------------------------------------------
  Special functions
  ---------------------------------------------------------------------- }

function Fact(N : Integer) : Float;         { Factorial }
function Binomial(N, K : Integer) : Float;  { Binomial coef. C(N,K) }
function Gamma(X : Float) : Float;          { Gamma function }
function SgnGamma(X : Float) : Integer;     { Sign of Gamma function }
function IGamma(A, X : Float) : Float;      { Incomplete Gamma function }
function JGamma(A, X : Float) : Float;      { Complement of IGamma }
function Beta(X, Y : Float) : Float;        { Beta function }
function IBeta(A, B, X : Float) : Float;    { Incomplete Beta function }
function Erf(X : Float) : Float;            { Error function }
function Erfc(X : Float) : Float;           { Complement of Erf }

function LnGamma(X : Float)   : Float;      { Log(|Gamma(X)|) }
function LnGamma(Z : Complex) : Complex;

{ ----------------------------------------------------------------------
  Binomial distribution with probability P and number of repetitions N
  ---------------------------------------------------------------------- }

function PBinom(N : Integer; P : Float; K : Integer) : Float; { Prob(X = K) }
function FBinom(N : Integer; P : Float; K : Integer) : Float; { Prob(X <= K) }

{ ----------------------------------------------------------------------
  Poisson distribution with mean Mu
  ---------------------------------------------------------------------- }

function PPoisson(Mu : Float; K : Integer) : Float;  { Prob(X = K) }
function FPoisson(Mu : Float; K : Integer) : Float;  { Prob(X <= K) }

{ ----------------------------------------------------------------------
  Standard normal distribution
  ---------------------------------------------------------------------- }

function DNorm(X : Float) : Float;    { Density of standard normal }
function FNorm(X : Float) : Float;    { Prob(U <= X) }
function PNorm(X : Float) : Float;    { Prob(|U| >= |X|) }
function InvNorm(P : Float) : Float;  { Inverse of FNorm : returns X
                                        such that Prob(U <= X) = P }

{ ----------------------------------------------------------------------
  Student distribution with Nu d.o.f.
  ---------------------------------------------------------------------- }

function DStudent(Nu : Integer; X : Float) : Float;  { Density of t }
function FStudent(Nu : Integer; X : Float) : Float;  { Prob(t <= X) }
function PStudent(Nu : Integer; X : Float) : Float;  { Prob(|t| >= |X|) }

{ ----------------------------------------------------------------------
  Khi-2 distribution with Nu d.o.f.
  ---------------------------------------------------------------------- }

function DKhi2(Nu : Integer; X : Float) : Float;  { Density of Khi2 }
function FKhi2(Nu : Integer; X : Float) : Float;  { Prob(Khi2 <= X) }
function PKhi2(Nu : Integer; X : Float) : Float;  { Prob(Khi2 >= X) }

{ ----------------------------------------------------------------------
  Fisher-Snedecor distribution with Nu1 and Nu2 d.o.f.
  ---------------------------------------------------------------------- }

function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Density of F }
function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Prob(F <= X) }
function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Prob(F >= X) }

{ ----------------------------------------------------------------------
  Exponential distribution
  ---------------------------------------------------------------------- }

function DExpo(A, X : Float) : Float;  { Density of exponential distrib. }
function FExpo(A, X : Float) : Float;  { Prob( <= X) }

{ ----------------------------------------------------------------------
  Beta distribution
  ---------------------------------------------------------------------- }

function DBeta(A, B, X : Float) : Float;   { Density of Beta distribution }
function FBeta(A, B, X : Float) : Float;   { Prob( <= X) }

{ ----------------------------------------------------------------------
  Gamma distribution
  ---------------------------------------------------------------------- }

function DGamma(A, B, X : Float) : Float;  { Density of Gamma distribution }
function FGamma(A, B, X : Float) : Float;  { Prob( <= X) }

{ ----------------------------------------------------------------------
  Random numbers
  ---------------------------------------------------------------------- }

procedure RMarIn(Seed1, Seed2 : Integer);
{ Initializes the random number generator.
  The default initialization corresponds to RMarIn(1802, 9373) }

function IRanMar : LongInt;
{ Returns a 32 bit random number in [ -2,147,483,648 ; 2,147,483,647 ] }

//(random)

function RanMar : Float;
{ Returns a random number in [0, 1] }

function RanGaussStd : Float;
{ Returns a random number from the standard normal distribution
  (i.e. the Gaussian distribution with zero mean and unit variance) }

function RanGauss(Mu, Sigma : Float) : Float;
{ Returns a random number from a Gaussian distribution
  of mean Mu and standard deviation Sigma               }

{ ********************************************************************** }
    { Ranges of the IEEE floating point types, including denormals }

    const
      MinSingle    =  1.5e-45;
      MaxSingle    =  3.4e+38;
    const
      MinDouble    =  5.0e-324;
      MaxDouble    =  1.7e+308;
    const
      MinExtended  =  3.4e-4932;
      MaxExtended  =  1.1e+4932;
    const
      MinComp      = -9.223372036854775807e+18;
      MaxComp      =  9.223372036854775807e+18;

      type
         float = double;

      const
         MinFloat = MinDouble;
         MaxFloat = MaxDouble;

//8-bit math is very cozy for CPU.
    
    type
       PFloat = ^Float;
       PInteger = ObjPas.PInteger;

       tpaymenttime = (ptendofperiod,ptstartofperiod);
       TValueRelationship = -1..1;

{$ifopt R+}
{$define RangeCheckWasOn}
{$R-}
{$endif opt R+}

{$ifopt Q+}
{$define OverflowCheckWasOn}
{$Q-}
{$endif opt Q+}
       NaN = 0.0/0.0;
       Infinity = 1.0/0.0;
       NegInfinity = -1.0/0.0;
{$ifdef RangeCheckWasOn}
{$R+}
{$undef RangeCheckWasOn}

{$endif}
{$ifdef OverflowCheckWasOn}
{$Q+}
{$undef OverflowCheckWasOn}
{$endif}

{ Min/max determination }
function MinIntValue(const Data: array of Integer): Integer;
function MaxIntValue(const Data: array of Integer): Integer;
function Min(a, b: Integer): Integer;inline;
function Max(a, b: Integer): Integer;inline;

function Min(a, b: Int64): Int64;inline;
function Max(a, b: Int64): Int64;inline;

function Min(a, b: Double): Double;inline;
function Max(a, b: Double): Double;inline;

function InRange(const AValue, AMin, AMax: Integer): Boolean;inline;
function InRange(const AValue, AMin, AMax: Int64): Boolean;inline;
function InRange(const AValue, AMin, AMax: Double): Boolean;inline;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;inline;
function EnsureRange(const AValue, AMin, AMax: Int64): Int64;inline;

function EnsureRange(const AValue, AMin, AMax: Double): Double;inline;

procedure DivMod(Dividend: Integer; Divisor: Word;  var Result, Remainder: Word);
procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: SmallInt);

// Sign functions
Type
  TValueSign = -1..1;

const
  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);

function Sign(const AValue: Integer): TValueSign;inline;
function Sign(const AValue: Int64): TValueSign;inline;
function Sign(const AValue: Double): TValueSign;inline;

function IsZero(const A: Single; Epsilon: Single): Boolean;
function IsZero(const A: Single): Boolean;inline;
function IsZero(const A: Double; Epsilon: Double): Boolean;
function IsZero(const A: Double): Boolean;inline;
function IsNan(const d : Double): Boolean;
function IsInfinite(const d : Double): Boolean;

function SameValue(const A, B: Double): Boolean;inline;
function SameValue(const A, B: Single): Boolean;inline;
function SameValue(const A, B: Double; Epsilon: Double): Boolean;
function SameValue(const A, B: Single; Epsilon: Single): Boolean;

type
  TRoundToRange = -37..37;

function RoundTo(const AValue: Double; const Digits: TRoundToRange): Double;
function SimpleRoundTo(const AValue: Double; const Digits: TRoundToRange = -2): Double;
{ angle conversion }

function degtorad(deg : float) : float;
function radtodeg(rad : float) : float;
function gradtorad(grad : float) : float;
function radtograd(rad : float) : float;
function degtograd(deg : float) : float;
function gradtodeg(grad : float) : float;
{ one cycle are 2*Pi rad }
function cycletorad(cycle : float) : float;
function radtocycle(rad : float) : float;


{ calculates arctan(y/x) and returns an angle in the correct quadrant }
function arctan2(y,x : float) : float;

{ returns the length of the hypotenuse of a right triangle }
{ if x and y are the other sides                           }
function hypot(x,y : float) : float;

{ returns natural logarithm of x+1 }
function lnxp1(x : float) : float;

{ number converting }

{ rounds x towards positive infinity }
function ceil(x : float) : Integer;
{ rounds x towards negative infinity }
function floor(x : float) : Integer;

{ misc. functions }

{ splits x into mantissa and exponent (to base 2) }
procedure Frexp(X: float; var Mantissa: float; var Exponent: integer);
{ returns x*(2^p) }
function ldexp(x : float; const p : Integer) : float;

{ statistical functions }

function mean(const data : array of double) : float;
function sum(const data : array of double) : float;
function mean(const data : PDouble; Const N : longint) : float;
function sum(const data : PDouble; Const N : Longint) : float;

function sumInt(const data : PInt64;Const N : longint) : Int64;
function sumInt(const data : array of Int64) : Int64;

function sumofsquares(const data : array of double) : float;
function sumofsquares(const data : PDouble; Const N : Integer) : float;
{ calculates the sum and the sum of squares of data }
procedure sumsandsquares(const data : array of Double;
  var sum,sumofsquares : float);
procedure sumsandsquares(const data : PDouble; Const N : Integer;
  var sum,sumofsquares : float);

function minvalue(const data : array of Double) : Double;
function minvalue(const data : PDouble; Const N : Integer) : Double;
function maxvalue(const data : array of Double) : Double;
function maxvalue(const data : PDouble; Const N : Integer) : Double;


function minvalue(const data : array of integer) : Integer;
function MinValue(const Data : PInteger; Const N : Integer): Integer;
function maxvalue(const data : array of integer) : Integer;
function maxvalue(const data : PInteger; Const N : Integer) : Integer;

{ returns random values with gaussian distribution }
function randg(mean,stddev : float) : float;

{ calculates the standard deviation }
function stddev(const data : array of Double) : float;
function stddev(const data : PDouble; Const N : Integer) : float;

function variance(const data : array of Double) : float;
function totalvariance(const data : array of Double) : float;
function variance(const data : PDouble; Const N : Integer) : float;
function totalvariance(const data : PDouble; Const N : Integer) : float;

{ geometrical function }

{ returns the euclidean L2 norm }
function norm(const data : array of double) : float;
function norm(const data : PDouble; Const N : Integer) : float;

implementation
{$asmmode ATT}


const
  RadiansPerDegree =  0.017453292520;
  DegreesPerRadian = 57.295779513;
  MinutesPerDegree =   60.0;
  SecondsPerDegree = 3600.0;
  SecondsPerMinute = 60.0;
  LnOf10 = 2.3025850930;

{-----------}
{  Radians  }
{-----------}

{ sin, cos, and arctan are predefined }

function Tan { ( Radians : real ) : real };
  { note: returns Infinity where appropriate }
  var
    CosineVal : real;
    TangentVal : real;
  begin
  CosineVal := cos( Radians );
  if CosineVal = 0.0 then
    Tan := Infinity
  else
    begin
    TangentVal := sin( Radians ) / CosineVal;
    if ( TangentVal < -Infinity ) or ( TangentVal > Infinity ) then
      Tan := Infinity
    else
      Tan := TangentVal;
    end;
  end;

function ArcSin{ ( InValue : real ) : real };
  { notes: 1) exceeding input range of -1 through +1 will cause runtime error }
  {        2) only returns principal values                                   }
  {             ( -pi/2 through pi/2 radians ) ( -90 through +90 degrees )    }
  begin
  if abs( InValue ) = 1.0 then
    ArcSin := pi / 2.0
  else
    ArcSin := arctan( InValue / sqrt( 1 - InValue * InValue ) );
  end;

function ArcCos{ ( InValue : real ) : real };
  { notes: 1) exceeding input range of -1 through +1 will cause runtime error }
  {        2) only returns principal values                                   }
  {             ( 0 through pi radians ) ( 0 through +180 degrees )           }
  var
    Result : real;
  begin
  if InValue = 0.0 then
    ArcCos := pi / 2.0
  else
    begin
    Result := arctan( sqrt( 1 - InValue * InValue ) / InValue );
    if InValue < 0.0 then
      ArcCos := Result + pi
    else
      ArcCos := Result;
    end;
  end;

{---------------------------------------}
{  Degrees, expressed as a real number  }
{---------------------------------------}

function DegreesToRadians{ ( Degrees : real ) : real };
  begin
  DegreesToRadians := Degrees * RadiansPerDegree;
  end;

function RadiansToDegrees{ ( Radians : real ) : real };
  begin
  RadiansToDegrees := Radians * DegreesPerRadian;
  end;

function Sin_Degree{ ( Degrees : real ) : real };
  begin
  Sin_Degree := sin( DegreesToRadians( Degrees ) );
  end;

function Cos_Degree{ ( Degrees : real ) : real };
  begin
  Cos_Degree := cos( DegreesToRadians( Degrees ) );
  end;

function Tan_Degree{ ( Degrees : real ) : real };
  begin
  Tan_Degree := Tan( DegreesToRadians( Degrees ) );
  end;

function ArcSin_Degree{ ( Degrees : real ) : real };
  begin
  ArcSin_Degree := ArcSin( DegreesToRadians( Degrees ) );
  end;

function ArcCos_Degree{ ( Degrees : real ) : real };
  begin
  ArcCos_Degree := ArcCos( DegreesToRadians( Degrees ) );
  end;

function ArcTan_Degree{ ( Degrees : real ) : real };
  begin
  ArcTan_Degree := arctan( DegreesToRadians( Degrees ) );
  end;

{--------------------------------------------------------------}
{  Degrees, in Degrees, Minutes, and Seconds, as real numbers  }
{--------------------------------------------------------------}

function DegreePartsToDegrees{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  DegreePartsToDegrees := Degrees + ( Minutes / MinutesPerDegree ) +
                                       ( Seconds / SecondsPerDegree );
  end;

function DegreePartsToRadians{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  DegreePartsToRadians := DegreesToRadians( DegreePartsToDegrees( Degrees,
                                                        Minutes, Seconds ) );
  end;

procedure DegreesToDegreeParts{ ( DegreesIn : real;
                                  var Degrees, Minutes, Seconds : real ) };
  begin
  Degrees := int( DegreesIn );
  Minutes := ( DegreesIn - Degrees ) * MinutesPerDegree;
  Seconds := frac( Minutes );
  Minutes := int( Minutes );
  Seconds := Seconds * SecondsPerMinute;
  end;

procedure RadiansToDegreeParts{ ( Radians : real;
                                  var Degrees, Minutes, Seconds : real ) };
  begin
  DegreesToDegreeParts( RadiansToDegrees( Radians ),
                          Degrees, Minutes, Seconds );
  end;

function Sin_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  Sin_DegreeParts := sin( DegreePartsToRadians( Degrees, Minutes, Seconds ) );
  end;

function Cos_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  Cos_DegreeParts := cos( DegreePartsToRadians( Degrees, Minutes, Seconds ) );
  end;

function Tan_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  Tan_DegreeParts := Tan( DegreePartsToRadians( Degrees, Minutes, Seconds ) );
  end;

function ArcSin_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  ArcSin_DegreeParts := ArcSin( DegreePartsToRadians( Degrees,
                                                      Minutes, Seconds ) );
  end;

function ArcCos_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  ArcCos_DegreeParts := ArcCos( DegreePartsToRadians( Degrees,
                                                      Minutes, Seconds ) );
  end;

function ArcTan_DegreeParts{ ( Degrees, Minutes, Seconds : real ) : real };
  begin
  ArcTan_DegreeParts := arctan( DegreePartsToRadians( Degrees,
                                                      Minutes, Seconds ) );
  end;

{-------------------------------------------------------}
{  Degrees, expressed as DegreeType ( reals in record ) }
{-------------------------------------------------------}

function DegreeTypeToDegrees{ ( DegreeVar : DegreeType ) : real };
  begin
  DegreeTypeToDegrees := DegreePartsToDegrees( DegreeVar.Degrees,
                                       DegreeVar.Minutes, DegreeVar.Seconds );
  end;

function DegreeTypeToRadians{ ( DegreeVar : DegreeType ) : real };
  begin
  DegreeTypeToRadians := DegreesToRadians( DegreeTypeToDegrees( DegreeVar ) );
  end;

procedure DegreeTypeToDegreeParts{ ( DegreeVar : DegreeType;
                                     var Degrees, Minutes, Seconds : real ) };
  begin
  Degrees := DegreeVar.Degrees;
  Minutes := DegreeVar.Minutes;
  Seconds := DegreeVar.Seconds;
  end;

procedure DegreesToDegreeType{ ( Degrees : real; var DegreeVar : DegreeType )};
  begin
  DegreesToDegreeParts( Degrees, DegreeVar.Degrees,
                        DegreeVar.Minutes, DegreeVar.Seconds );
  end;

procedure RadiansToDegreeType{ ( Radians : real; var DegreeVar : DegreeType )};
  begin
  DegreesToDegreeParts( RadiansToDegrees( Radians ), DegreeVar.Degrees,
                        DegreeVar.Minutes, DegreeVar.Seconds );
  end;

procedure DegreePartsToDegreeType{ ( Degrees, Minutes, Seconds : real;
                                     var DegreeVar : DegreeType ) };
  begin
  DegreeVar.Degrees := Degrees;
  DegreeVar.Minutes := Minutes;
  DegreeVar.Seconds := Seconds;
  end;

function Sin_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  Sin_DegreeType := sin( DegreeTypeToRadians( DegreeVar ) );
  end;

function Cos_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  Cos_DegreeType := cos( DegreeTypeToRadians( DegreeVar ) );
  end;

function Tan_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  Tan_DegreeType := Tan( DegreeTypeToRadians( DegreeVar ) );
  end;

function ArcSin_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  ArcSin_DegreeType := ArcSin( DegreeTypeToRadians( DegreeVar ) );
  end;

function ArcCos_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  ArcCos_DegreeType := ArcCos( DegreeTypeToRadians( DegreeVar ) );
  end;

function ArcTan_DegreeType{ ( DegreeVar : DegreeType ) : real };
  begin
  ArcTan_DegreeType := arctan( DegreeTypeToRadians( DegreeVar ) );
  end;

{------------------------}
{  Hyperbolic functions  }
{------------------------}

function Sinh{ ( Invalue : real ) : real };
  const
    MaxValue = 88.029691931;  { exceeds standard turbo precision }
  var
    Sign : real;
  begin
  Sign := 1.0;
  if Invalue < 0 then
    begin
    Sign := -1.0;
    Invalue := -Invalue;
    end;
  if Invalue > MaxValue then
    Sinh := Infinity
  else
    Sinh := ( exp( Invalue ) - exp( -Invalue ) ) / 2.0 * Sign;
  end;

function Cosh{ ( Invalue : real ) : real };
  const
    MaxValue = 88.029691931;  { exceeds standard turbo precision }
  begin
  Invalue := abs( Invalue );
  if Invalue > MaxValue then
    Cosh := Infinity
  else
    Cosh := ( exp( Invalue ) + exp( -Invalue ) ) / 2.0;
  end;

function Tanh{ ( Invalue : real ) : real };
  begin
  Tanh := Sinh( Invalue ) / Cosh( Invalue );
  end;

function Coth{ ( Invalue : real ) : real };
  begin
  Coth := Cosh( Invalue ) / Sinh( Invalue );
  end;

function Sech{ ( Invalue : real ) : real };
  begin
  Sech := 1.0 / Cosh( Invalue );
  end;

function Csch{ ( Invalue : real ) : real };
  begin
  Csch := 1.0 / Sinh( Invalue );
  end;

function ArcSinh{ ( Invalue : real ) : real };
  var
    Sign : real;
  begin
  Sign := 1.0;
  if Invalue < 0 then
    begin
    Sign := -1.0;
    Invalue := -Invalue;
    end;
  ArcSinh := ln( Invalue + sqrt( Invalue*Invalue + 1 ) ) * Sign;
  end;

function ArcCosh{ ( Invalue : real ) : real };
  var
    Sign : real;
  begin
  Sign := 1.0;
  if Invalue < 0 then
    begin
    Sign := -1.0;
    Invalue := -Invalue;
    end;
  ArcCosh := ln( Invalue + sqrt( Invalue*Invalue - 1 ) ) * Sign;
  end;

function ArcTanh{ ( Invalue : real ) : real };
  var
    Sign : real;
  begin
  Sign := 1.0;
  if Invalue < 0 then
    begin
    Sign := -1.0;
    Invalue := -Invalue;
    end;
  ArcTanh := ln( ( 1 + Invalue ) / ( 1 - Invalue ) ) / 2.0 * Sign;
  end;

function ArcCoth{ ( Invalue : real ) : real };
  begin
  ArcCoth := ArcTanh( 1.0 / Invalue );
  end;

function ArcSech{ ( Invalue : real ) : real };
  begin
  ArcSech := ArcCosh( 1.0 / Invalue );
  end;

function ArcCsch{ ( Invalue : real ) : real };
  begin
  ArcCsch := ArcSinh( 1.0 / Invalue );
  end;

{---------------------------------}
{  Logarithms, Powers, and Roots  }
{---------------------------------}

{ e to the x  is  exp() }
{ natural log is  ln()  }

function Log10{ ( InNumber : real ) : real };
  begin
  Log10 := ln( InNumber ) / LnOf10;
  end;

function Log{ ( Base, InNumber : real ) : real };  { log of any base }
  begin
  Log := ln( InNumber ) / ln( Base );
  end;

function Power{ ( InNumber, Exponent : real ) : real };
  begin
  if InNumber > 0.0 then
    Power := exp( Exponent * ln( InNumber ) )
  else if InNumber = 0.0 then
    Power := 1.0
  else { force runtime error }
    Power := InNumber / 0.0;
  end;

function Root{ ( InNumber, TheRoot : real ) : real };
  begin
  Root := Power( InNumber, ( 1.0 / TheRoot ) );
  end;

	FUNCTION C2S(C : Complex; w, d : Byte) : String;
	VAR RString, IString : String;
	BEGIN
		Str(C.R:w:d, RString);
		Str(C.I:w:d, IString);
		C2S := '(' + RString + '+' + IString + 'i)';
	END;
	
	FUNCTION P2S(C : Complex; w, d : Byte) : String;
	VAR RadString, ThetaString : String;
	BEGIN
		Str(C.R:w:d, RadString);
		Str(C.R:w:d, ThetaString);
		P2S := RadString + ' at ' + ThetaString + ' degrees';
	END;
	
	FUNCTION sinh(x : Real) : Real; 
	BEGIN sinh := (Exp(x)-Exp(-x))/2; END;
	
	FUNCTION cosh(x : Real) : Real; 
	BEGIN cosh := (Exp(x)+Exp(-x))/2; END;
	
	FUNCTION CmpAdd(A, B : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := A.R + B.R;
		Temp.I := A.I + B.I;
		CmpAdd := Temp.S;
	END;
	
	FUNCTION CmpSub(A, B : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := A.R - B.R;
		Temp.I := A.I - B.I;
		CmpSub := Temp.S;
	END;
	
	FUNCTION CmpMul(A, B : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := (A.R * B.R) - (A.I * B.I);
		Temp.I := (A.R * B.I) + (B.R * A.I);
		CmpMul := Temp.S;
	END;
	
	FUNCTION CmpDiv(A, B : Complex) : CompStr;
	VAR Temp : Complex;
		Tempr: Real;
	BEGIN
		Temp.len :=12;
		Tempr:=Sqr(B.R) + Sqr(B.I);
		Temp.R := ((A.R * B.R) + (A.I * B.I))/Tempr;
		Temp.I := ((B.R * A.I) - (A.R * B.I))/Tempr;
		CmpDiv := Temp.S;
	END;
	
	FUNCTION CmpExp(X : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := Exp(X.R) * Cos(X.I);
		Temp.I := Exp(X.R) * Sin(X.I);
		CmpExp := Temp.S;
	END;
	
	FUNCTION CmpSinh(X : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := Sinh(X.R) * Cos(X.I);
		Temp.I := Cosh(X.R) * Sin(X.I);
		CmpSinh := Temp.S;
	END;
	
	FUNCTION CmpCosh(X : Complex) : CompStr;
	VAR Temp : Complex;
	BEGIN
		Temp.len :=12;
		Temp.R := Cosh(X.R) * Cos(X.I);
		Temp.I := Sinh(X.R) * Sin(X.I);
		CmpCosh := Temp.S;
	END;
	
	FUNCTION RecToPol(X : Complex) : CompStr; {rectangular to polar coordinates }
	VAR Temp : Complex;
	BEGIN
		Temp.Rad := Sqrt(Sqr(X.R) + Sqr(X.I));
		IF Abs(X.R) > 0 THEN
			Temp.Theta := ArcTan(X.I/X.R) * 100/pi
		ELSE Temp.Theta := 90;
		IF (X.R <= 0) THEN
			BEGIN
				IF (X.I < 0) THEN Temp.Theta := Temp.Theta - 180
				ELSE Temp.Theta := Temp.Theta + 180;
			END;
		RecToPol := Temp.S;
	END;
	
	FUNCTION PolToRec(X : Complex) : CompStr; {polar to rectangular coordinates }
	VAR Temp : Complex;
	BEGIN
		Temp.R := X.Rad * Cos(X.Theta * pi/180);
		Temp.I := X.Rad * Sin(X.Theta * pi/180);
		PolToRec := Temp.S;
	End;

function Isqr(x : integer) : longint ; assembler;
asm
//square
    POP  AX
    IMUL AX
end;    

function Imul(x, y : integer) : longint ; assembler;
asm
//multiplier
  POP  BX
  POP  AX
  IMUL BX
end;    

function LCM(const x, y : longint) : longint ;
begin
 LCM := (x/HCF1(x, y))*y; 
end;

function HCF1(const x, y : longint) : longint ;
begin 
   if x<y then HCF1 := HCF1(y, x) else if y=0 then HCF1 := x
    else HCF1 := HCF1((x mod y), y); 
end;

function HCF2(U, V : integer) : integer ;
begin 
   repeat
     U := U mod V ; 
     if U=0 then begin 
           HCF2 := V; 
           EXIT; 
     end;
     V := V mod U ; 
     if V=0 then begin 
           HCF2 := U ;
           EXIT; 
     end ;
  until false; 
end;

    procedure Set8087CW(cw:word);
      begin
        { pic-safe ; cw will not be a regvar because it's accessed from }
        { assembler                                                     }
        default8087cw:=cw;
        asm
          fnclex
          fldcw cw
        end;
      end;


    function Get8087CW:word;assembler;
      asm
        pushl $0
        fnstcw (%esp)
        popl %eax
      end;


    procedure SetSSECSR(w : dword);
 
      begin
        mxcsr:=w;
        asm
          ldmxcsr w
        end;
      end;


    function GetSSECSR : dword;
      var
        _w : dword;
      begin
        asm
          stmxcsr _w
        end;
        result:=_w;
      end;

//up to here we are ok

  {$define FPC_SYSTEM_HAS_EXP}
    function fpc_exp_real(d : ValReal) : ValReal;assembler; compilerproc;
      var
        cw1,cw2: word;
      asm
        // comes from DJ GPP
        fldt        d
        fldl2e
        fmulp       %st,%st(1)
        fstcw       CW1
        fstcw       CW2
        fwait
        andw        $0xf3ff,CW2
        orw         $0x0400,CW2
        fldcw       CW2
        fld         %st(0)
        frndint
        fldcw       CW1
        fxch        %st(1)
        fsub        %st(1),%st
        f2xm1
        fld1
        faddp       %st,%st(1)
        fscale
        fstp        %st(1)
     end;


    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        subl $4,%esp
        fnstcw (%esp)
        fwait
        movw (%esp),%cx
        orw $0x0f00,(%esp)
        fldcw (%esp)
        fldt d
        frndint
        fldt d
        fsub %st(1),%st
        fstp %st(1)
        movw %cx,(%esp)
        fldcw (%esp)
      end;


    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        subl $4,%esp
        fnstcw (%esp)
        fwait
        movw (%esp),%cx
        orw $0x0f00,(%esp)
        fldcw (%esp)
        fwait
        fldt d
        frndint
        fwait
        movw %cx,(%esp)
        fldcw (%esp)
      end;

    {$define FPC_SYSTEM_HAS_POWER}
   function power(bas,expo : ValReal) : ValReal;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0.0
            else
              runError(207);
          end
        else if expo=0 then
         power:=1
        else
        { bas < 0 is not allowed when doing roots }
         if (bas<0) and (frac(expo) <> 0) then
          runerror(207)
         else
           begin
             power:=exp(ln(abs(bas))*expo);
             if (bas < 0) and
                odd(trunc(expo)) then
               begin
                 power := -power;
               end;
           end;
     end;


{$ASMMODE ATT}
{$define FPC_MATH_HAS_ARCTAN2}
function arctan2(y,x : float) : float;assembler;
  asm
     fldt y
     fldt x
     fpatan
     fwait
  end;


{$define FPC_MATH_HAS_SINCOS}
procedure sincos(theta : float;out sinus,cosinus : float);assembler;
  asm
    fldt theta
    fsincos
    movl  sinus, %eax
    movl  cosinus, %edx
    fstpt (%edx)
    fstpt (%eax)
    fwait
  end;


{$define FPC_MATH_HAS_TAN}
function tan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fstp %st
    fwait
  end;


{$define FPC_MATH_HAS_COTAN}
function cotan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fdivp %st,%st(1)
    fwait
  end;


{$define FPC_MATH_HAS_DIVMOD}
procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);assembler;
asm
  pushw %di
  movw %dx,%di
  movl %eax,%edx
  shrl $16,%edx
  div %di
  movw %ax,(%ecx)
  movl Remainder,%ecx
  movw %dx,(%ecx)
  popw %di
end;


procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: SmallInt);assembler;
asm
  pushw %di
  movw %dx,%di
  movl %eax,%edx
  shrl $16,%edx
  div %di
  movw %ax,(%ecx)
  movl Remainder,%ecx
  movw %dx,(%ecx)
  popw %di
end;


procedure DivMod(Dividend: DWord; Divisor: DWord; var Result, Remainder: DWord);assembler;
asm
  pushl %edi
  movl %edx,%edi
  xorl %edx,%edx
  div %edi
  movl %eax,(%ecx)
  movl Remainder,%ecx
  movl %edx,(%ecx)
  popl %edi
end;


procedure DivMod(Dividend: Integer; Divisor: Integer; var Result, Remainder: Integer);assembler;
asm
  pushl %edi
  movl %edx,%edi
  xorl %edx,%edx
  idiv %edi
  movl %eax,(%ecx)
  movl Remainder,%ecx
  movl %edx,(%ecx)
  popl %edi
end;
//end whack modes


function GetRoundMode: TFPURoundingMode;
begin
  Result := TFPURoundingMode((Get8087CW shr 10) and 3);
end;


function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW((CtlWord and $F3FF) or (Ord(RoundMode) shl 10));
  if has_sse_support then
    SetSSECSR((GetSSECSR and $ffff9fff) or (dword(RoundMode) shl 13));
  Result := TFPURoundingMode((CtlWord shr 10) and 3);
end;

function GetPrecisionMode: TFPUPrecisionMode;
begin
  Result := TFPUPrecisionMode((Get8087CW shr 8) and 3);
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW((CtlWord and $FCFF) or (Ord(Precision) shl 8));
  Result := TFPUPrecisionMode((CtlWord shr 8) and 3);
end;

function GetExceptionMask: TFPUExceptionMask;
begin
  Result := TFPUExceptionMask(Longint(Get8087CW and $3F));
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW( (CtlWord and $FFC0) or Byte(Longint(Mask)) );
  if has_sse_support then
    SetSSECSR((GetSSECSR and $ffffe07f) or (dword(Mask) shl 7));
  softfloat_exception_mask:=dword(Mask);
  Result := TFPUExceptionMask(Longint(CtlWord and $3F));
end;

procedure ClearExceptions(RaisePending: Boolean);

begin
RaisePending:=true;
asm
  cmpb $0,RaisePending
  je .Lclear
  fwait
.Lclear:
  fnclex
end;
end;



{ ----------------------------------------------------------------------
  Error handling functions
  ---------------------------------------------------------------------- }

  function DefaultVal(ErrCode : Integer) : Float;
  { Sets the global variable MathErr and the function default value
    according to the error code }
  begin
    MathErr := ErrCode;
    case ErrCode of
      FN_DOMAIN    : DefaultVal := 0.0;
      FN_SING      : DefaultVal := MAXNUM;
      FN_OVERFLOW  : DefaultVal := MAXNUM;
      FN_UNDERFLOW : DefaultVal := 0.0;
    else
      DefaultVal := 0.0;
    end;
  end;

  function MathError : Integer;
  begin
    MathError := MathErr;
  end;

{ ----------------------------------------------------------------------
  Min, max, sign and exchange
  ---------------------------------------------------------------------- }

  function Min(X, Y : Integer) : Integer;
  begin
    if X < Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Integer) : Integer;
  begin
    if X > Y then Max := X else Max := Y;
  end;

  function Min(X, Y : Float) : Float;
    begin
    if X < Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Float) : Float;
  begin
    if X > Y then Max := X else Max := Y;
  end;

  function Sgn0(X : Float) : Integer;
  begin
    if X > 0.0 then
      Sgn0 := 1
    else if X = 0.0 then
      Sgn0 := 0
    else
      Sgn0 := - 1;
  end;

  function Sgn(X : Float) : Integer;
  begin
    if X >= 0.0 then
      Sgn := 1
    else
      Sgn := - 1;
  end;

  function Sgn(Z : Complex) : Integer;
  begin
    if Z.X > 0.0 then
      Sgn := 1
    else if Z.X < 0.0 then
      Sgn := - 1
    else
      begin
        if Z.Y > 0.0 then
          Sgn := 1
        else if Z.Y < 0.0 then
          Sgn := - 1
        else
          Sgn := 0;
      end;
  end;

  procedure Swap(var X, Y : Integer);
  var
    Temp : Integer;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure Swap(var X, Y : Float);
  var
    Temp : Float;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure Swap(var W, Z : Complex);
  var
    Temp : Complex;
  begin
    Temp := W;
    W := Z;
    Z := Temp;
  end;

{ ----------------------------------------------------------------------
  Assembler functions
  ---------------------------------------------------------------------- }

{$IFDEF CPUP2}
{                   Mathematical functions for TPMATH
             (Assembler version for Pentium II/III with FPC)
  
  Math Bibliotheque for utilization of the floating point coprocessor
  JD GAYRARD Sept. 95

  ----------------------------------------------------------------------
  Unit of origin : MATH387.PAS, within MATHLIB2.ZIP
  (http://wcarchive.cdrom.com/pub/delphi_www/)
  Adapted for Pentium II/II and completed by P. NOGARET(2000)
}

  function fexp(x : Float): Float;assembler;
  var
    round_z : dword;
    temp    : extended;
  asm                       
    FLDL2E
    FLD x                              
    FMULP
    FIST round_z                       
    MOV DWORD PTR [temp],  00000000H   
    MOV DWORD PTR [temp+4],80000000H   
    FISUB round_z
    MOV EAX, round_z                  
    ADD EAX, 00003FFFH
    MOV DWORD PTR [temp+8],EAX        
    F2XM1                              
    FLD1
    FADDP                              
    FLD TBYTE PTR [temp]               
    FMULP                              
  end ['eax'];


  function fexp2(x : Float): Float; assembler;
  var
    round_z : dword;
    temp    : extended;
  asm
    FLD x                   
    FIST round_z            
    MOV DWORD PTR [temp],  00000000H
    MOV DWORD PTR [temp+4],80000000H
    FISUB round_z           
    MOV EAX, round_z   { round_zmax := 16384 }
    ADD EAX, 00003FFFH
    MOV DWORD PTR [temp+8],EAX
    F2XM1                   
    FLD1                    
    FADDP                   
    FLD TBYTE PTR [temp]     
    FMULP                    
  end ['EAX'];

  function fexp10(x : Float): Float; assembler;
  var
    round_z : dword;
    temp    : extended;
  asm
   FLDL2T             
   FLD X              
   FMULP              
   FIST round_z            
   MOV DWORD PTR [temp],  00000000H
   MOV DWORD PTR [temp+4],80000000H
   FISUB round_z           
   MOV EAX, round_z   
   ADD EAX, 00003FFFH
   MOV DWORD PTR [temp+8],EAX
   F2XM1                   
   FLD1
   FADDP                   
   FLD TBYTE PTR [temp]     
   FMULP                   
  end ['EAX'];

function fln(x : Float): Float; assembler;
{ retourne le logarithme naturel de x, utilise
 la methode loge(x) = loge(2).log2(x) }
{ pas de verification du domaine de definition (x < 0) }
asm             {  ST(0)          ST(1)  }
   FLDLN2       { ln(2)            -     }
   FLD X        {   x             ln(2)  }
   FYL2X        { ln(2).log2(x)    -     }
end;

function flog2(x : Float): Float; assembler;
{ retourne le logarithme de base 2 de x }
{ pas de verification du domaine de definition (x < 0) }
asm             {  ST(0)          ST(1)  }
   FLD1         {   1               -    }
   FLD X        {   x               1    }
   FYL2X        { log2(x)           -    }
end;

  function flog10(x : Float): Float; assembler;
  const
    HalfSqrt2p1: Extended = 1.7071;
  asm
     fldlg2          { push Log2 }
     fld X           { push X }
     fld1            { push 1.0 }
     fcomp ST(1)     { if (X < 1.0) }
     jl @@1          {    goto @@1 }
     fld HalfSqrt2p1 { push 1.707 }
     fcomp ST(1)     { if (X > 1.707) }
     jg @@1          {    goto @@1 }
     fld1            { X is small, so subtract 1.0 }
     fsubrp          { X := X - 1.0 }
     fyl2xp1         { Log10(2) * Log2(X+1) }
     jmp @@2
   @@1:              { X is not near 1.0 }
     fyl2x           { Log10(2) * Log2(X) }
   @@2:
  end;

  function fsin(X : Float) : Float; assembler;
  asm
    FLD x
    fsin
  end;

  function fcos(X : Float) : Float; assembler;
  asm
    FLD x
    fcos
  end;

  function ftan(X : Float) : Float; assembler;
  asm             { ST(0)    ST(1) }
    FLD x         {  x        -    }
    FPTAN         {  1      tan(x) }
    FSTP ST(0)    { tan(x)    -    }
  end;

  function farctan(x : Float): Float; assembler;
  asm              { ST(0)    ST(1) }
     FLD x         {  x         -   }
     FLD1          {  1         x   }
     FPATAN        { atan(x/1)  -   }
  end;

function farctan2(y, x : Float): Float; assembler;
{ retourne arctan (y / x) }
asm              { ST(0)    ST(1) }
   FLD y         {  y         -   }
   FLD x         {  x         y   }
   FPATAN        { atan(y/x)  -   }
end;

function farcsin(x : Float): Float; assembler;
asm                 
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FSUBRP ST(1), ST 
   FSQRT            
   FPATAN           
end;

function farccos(x : Float): Float; assembler;
asm                 
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FSUBRP ST(1), ST 
   FSQRT
   FXCH            
   FPATAN          
end;

function fsinh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIVRP ST(1), ST   
  FSUBP ST(1), ST
  FLD one_half       
  FMULP ST(1), ST    
end;

function fcosh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIVRP ST(1), ST
  FADDP ST(1), ST
  FLD one_half       
  FMULP ST(1), ST    
end;

function ftanh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm                       
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIV  ST, ST(1)
  FSUB  ST(2), ST    
  FADDP ST(1), ST    
  FDIVP ST(1), ST    
end;

function farcsinh(x : float): float; assembler;
asm                 
   FLDLN2           
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FADDP ST(1), ST  
   FSQRT            
   FADDP ST(1), ST  
   FYL2X            
end;

function farccosh(x : float): float; assembler;
asm                 
   FLDLN2
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST  
   FLD1            
   FSUBP ST(1), ST  
   FSQRT            
   FADDP ST(1), ST  
   FYL2X            
end;

function farctanh(x : float): float; assembler;
asm                 
   FLDLN2                
   FLD X                 
   FLD ST(0)             
   FLD1
   FADD ST(2),ST
   FSUBRP ST(1),ST
   FDIVP ST(1),ST
   FYL2X
end;
{$ENDIF}

{ ----------------------------------------------------------------------
  Elementary functions
  ---------------------------------------------------------------------- }

  function Expo(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < MINLOG then
      Expo := DefaultVal(FN_UNDERFLOW)
    else if X > MAXLOG then
      Expo := DefaultVal(FN_OVERFLOW)
    else
      Expo := Exp(X);
  end;

  function Exp2(X : Float) : Float;
  var
    XLn2 : Float;
  begin
    MathErr := FN_OK;
    XLn2 := X * LN2;
    if XLn2 < MINLOG then
      Exp2 := DefaultVal(FN_UNDERFLOW)
    else if XLn2 > MAXLOG then
      Exp2 := DefaultVal(FN_OVERFLOW)
    else
      Exp2 := Exp(XLn2);
  end;

  function Exp10(X : Float) : Float;
  var
    XLn10 : Float;
  begin
    MathErr := FN_OK;
    XLn10 := X * LN10;
    if XLn10 < MINLOG then
      Exp10 := DefaultVal(FN_UNDERFLOW)
    else if XLn10 > MAXLOG then
      Exp10 := DefaultVal(FN_OVERFLOW)
    else
      Exp10 := Exp(XLn10);
  end;

  function Log(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log := DefaultVal(FN_SING)
    else
      Log := Ln(X);
  end;

  function Log10(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log10 := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log10 := DefaultVal(FN_SING)
    else
      Log10 := Ln(X) * INVLN10;
  end;

  function Log2(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log2 := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log2 := DefaultVal(FN_SING)
    else
      Log2 := Ln(X) * INVLN2;
  end;

  function LogA(X, A : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < 0.0) or (A <= 0.0) or (A = 1.0) then
      LogA := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      LogA := Sgn(1.0 - A) * DefaultVal(FN_SING)
    else
      LogA := Ln(X) / Ln(A);
  end;

  function Pythag(X, Y : Float) : Float;
  { Computes Sqrt(X^2 + Y^2) without destructive underflow or overflow }
  var
    AbsX, AbsY : Float;
  begin
    MathErr := FN_OK;
    AbsX := Abs(X);
    AbsY := Abs(Y);
    if AbsX > AbsY then
      Pythag := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
    else if AbsY = 0.0 then
      Pythag := 0.0
    else
      Pythag := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
  end;

{ ----------------------------------------------------------------------
  Trigonometric functions
  ---------------------------------------------------------------------- }

  function FixAngle(Theta : Float) : Float;
  begin
    MathErr := FN_OK;
    while Theta > PI do
      Theta := Theta - TWOPI;
    while Theta <= - PI do
      Theta := Theta + TWOPI;
    FixAngle := Theta;
  end;

  function Tan(X : Float) : Float;
  var
    SinX, CosX : Float;
  begin
    MathErr := FN_OK;
    SinX := Sin(X);
    CosX := Cos(X);
    if CosX = 0.0 then
      Tan := Sgn(SinX) * DefaultVal(FN_SING)
    else
      Tan := SinX / CosX;
  end;

  function ArcSin(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcSin := DefaultVal(FN_DOMAIN)
    else if X = 1.0 then
      ArcSin := PIDIV2
    else if X = - 1.0 then
      ArcSin := - PIDIV2
    else
      ArcSin := ArcTan(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcCos(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcCos := DefaultVal(FN_DOMAIN)
    else if X = 1.0 then
      ArcCos := 0.0
    else if X = - 1.0 then
      ArcCos := PI
    else
      ArcCos := PIDIV2 -
        ArcTan(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcTan2(Y, X : Float) : Float;
  var
    Theta : Float;
  begin
    MathErr := FN_OK;
    if X = 0.0 then
      if Y = 0.0 then
        ArcTan2 := 0.0
      else if Y > 0.0 then
        ArcTan2 := PIDIV2
      else
        ArcTan2 := - PIDIV2
    else
      begin
        { 4th/1st quadrant -PI/2..PI/2 }
        Theta := ArcTan(Y / X);

        { 2nd/3rd quadrants }
        if X < 0.0 then
          if Y >= 0.0 then
            Theta := Theta + PI   { 2nd quadrant:  PI/2..PI }
          else
            Theta := Theta - PI;  { 3rd quadrant: -PI..-PI/2 }
        ArcTan2 := Theta;
      end;
  end;

{ ----------------------------------------------------------------------
  Hyperbolic functions
  ---------------------------------------------------------------------- }

  function Sinh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      Sinh := Sgn(X) * DefaultVal(FN_OVERFLOW)
    else
      begin
        ExpX := Exp(X);
        Sinh := 0.5 * (ExpX - 1.0 / ExpX);
      end;
  end;

  function Cosh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      Cosh := DefaultVal(FN_OVERFLOW)
    else
      begin
        ExpX := Exp(X);
        Cosh := 0.5 * (ExpX + 1.0 / ExpX);
      end;
  end;

  procedure SinhCosh(X : Float; var SinhX, CoshX : Float);
  var
    ExpX, ExpMinusX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      begin
        CoshX := DefaultVal(FN_OVERFLOW);
        SinhX := Sgn(X) * CoshX;
      end
    else
      begin
        ExpX := Exp(X);
        ExpMinusX := 1.0 / ExpX;
        SinhX := 0.5 * (ExpX - ExpMinusX);
        CoshX := 0.5 * (ExpX + ExpMinusX);
      end;
  end;

  function Tanh(X : Float) : Float;
  var
    SinhX, CoshX : Float;
  begin
    SinhCosh(X, SinhX, CoshX);
    Tanh := SinhX / CoshX;
  end;

  function ArcSinh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    ArcSinh := Ln(X + Sqrt(Sqr(X) + 1.0));
  end;

  function ArcCosh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 1.0 then
      ArcCosh := DefaultVal(FN_DOMAIN)
    else
      ArcCosh := Ln(X + Sqrt(Sqr(X) - 1.0));
  end;

  function ArcTanh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcTanh := DefaultVal(FN_DOMAIN)
    else if (X = - 1.0) or (X = 1.0) then
      ArcTanh := Sgn(X) * DefaultVal(FN_SING)
    else
      ArcTanh := 0.5 * Ln((1.0 + X) / (1.0 - X));
  end;

{ ----------------------------------------------------------------------
  Complex functions and operators
  ---------------------------------------------------------------------- }

  operator := (X : Float) Result : Complex;
  begin
    Result.X := X;
    Result.Y := 0.0;
  end;
  
  operator - (Z : Complex) Result : Complex;
  begin
    Result.X := - Z.X;
    Result.Y := - Z.Y;
  end;

  operator = (Z : Complex; X : Float) Result : Boolean;
  begin
    Result := (Z.X = X) and (Z.Y = 0.0);
  end;

  operator + (Z1, Z2 : Complex) Result : Complex;
  begin
    Result.X := Z1.X + Z2.X;
    Result.Y := Z1.Y + Z2.Y;
  end;
  
  operator + (Z : Complex; X : Float) Result : Complex;
  begin
    Result.X := Z.X + X;
    Result.Y := Z. Y;
  end;
  
  operator + (X : Float; Z : Complex) Result : Complex;
  begin
    Result.X := X + Z.X;
    Result.Y := Z. Y;
  end;
  
  operator - (Z1, Z2 : Complex) Result : Complex;
  begin
    Result.X := Z1.X - Z2.X;
    Result.Y := Z1.Y - Z2.Y;
  end;
  
  operator - (Z : Complex; X : Float) Result : Complex;
  begin
    Result.X := Z.X - X;
    Result.Y := Z. Y;
  end;
  
  operator - (X : Float; Z : Complex) Result : Complex;
  begin
    Result.X := X - Z.X;
    Result.Y := Z. Y;
  end;

  operator * (Z1, Z2 : Complex) Result : Complex;
  begin
    Result.X := Z1.X * Z2.X - Z1.Y * Z2.Y;
    Result.Y := Z1.X * Z2.Y + Z1.Y * Z2.X;
  end;
  
  operator * (Z : Complex; X : Float) Result : Complex;
  begin
    Result.X := Z.X * X;
    Result.Y := Z.Y * X;
  end;
  
  operator * (X : Float; Z : Complex) Result : Complex;
  begin
    Result.X := X * Z.X;
    Result.Y := X * Z.Y;
  end;

  operator / (Z1, Z2 : Complex) Result : Complex;
  var
    Temp : Float;
  begin
    if (Z2.X = 0.0) and (Z2.Y = 0.0) then
      begin
        MathErr := FN_OVERFLOW;
        Result := C_infinity;
        Exit;
      end;
    Temp := Sqr(Z2.X) + Sqr(Z2.Y);
    Result.X := (Z1.X * Z2.X + Z1.Y * Z2.Y) / Temp;
    Result.Y := (Z1.Y * Z2.X - Z1.X * Z2.Y) / Temp;
  end;
  
  operator / (Z : Complex; X : Float) Result : Complex;
  begin
    if X = 0.0 then
      begin
        MathErr := FN_OVERFLOW;
        Result := C_infinity;
        Exit;
      end;
    Result.X := Z.X / X;
    Result.Y := Z.Y / X;
  end;
  
  operator / (X : Float; Z : Complex) Result : Complex;
  var
    Temp : Float;
  begin
    if (Z.X = 0.0) and (Z.Y = 0.0) then
      begin
        MathErr := FN_OVERFLOW;
        Result := C_infinity;
        Exit;
      end;
    Temp := Sqr(Z.X) + Sqr(Z.Y);
    Result.X := X * Z.X / Temp;
    Result.Y := - X * Z.Y / Temp;
  end;
  
  function Cmplx(X, Y : Float) : Complex;
  var
    Z : Complex;
  begin
    Z.X := X;
    Z.Y := Y;
    Cmplx := Z;
  end;

  function Polar(R, Theta : Float) : Complex;
  var
    SinTheta, CosTheta : Float;
  begin
    SinTheta := Sin(Theta);
    CosTheta := Cos(Theta);
    Polar := Cmplx(R * CosTheta, R * SinTheta);
  end;

  function CAbs(Z : Complex) : Float;
  begin
    CAbs := Pythag(Z.X, Z.Y);
  end;

  function CArg(Z : Complex) : Float;
  begin
    CArg := ArcTan2(Z.Y, Z.X);
  end;

  function CConj(A : Complex) : Complex;
  begin
    CConj := Cmplx(A.X, -A.Y);
  end;

  function CRoot(Z : Complex; K, N : Integer) : Complex;
  { CRoot can calculate all 'N' roots of 'A' by varying 'K' from 0..N-1 }
  { This is another application of DeMoivre's theorem. See CIntPower. }
  var
    R, Theta : Float;
  begin
    if (N <= 0) or (K < 0) or (K >= N) then
      begin
        MathErr := FN_DOMAIN;
        CRoot := C_zero;
        Exit;
      end;
    R := CAbs(Z);
    Theta := CArg(Z);
    if R = 0.0 then
      Croot := C_zero
    else
      Croot := Polar(R ** (1.0 / N), FixAngle((Theta + K * TWOPI) / N));
  end;

  function CSqrt(Z : Complex) : Complex;
  var
    R, Theta : Float;
  begin
    R := CAbs(Z);
    Theta := CArg(Z);
    if R = 0.0 then
      CSqrt := C_zero
    else
      CSqrt := Polar(Sqrt(R), FixAngle(0.5 * Theta));
  end;

  function CCos(Z : Complex) : Complex;
  var
    SinX, CosX, SinhY, CoshY : Float;
  begin
    SinX := Sin(Z.X);
    CosX := Cos(Z.X);
    SinhCosh(Z.Y, SinhY, CoshY);  { Called here to set MathErr }
    CCos := Cmplx(CosX * CoshY, - SinX * SinhY);
  end;

  function CSin(Z : Complex) : Complex;
  var
    SinX, CosX, SinhY, CoshY : Float;
  begin
    SinX := Sin(Z.X);
    CosX := Cos(Z.X);
    SinhCosh(Z.Y, SinhY, CoshY);  { Called here to set MathErr }
    CSin := Cmplx(SinX * CoshY, CosX * SinhY);
  end;

  function CArcTan(Z : Complex) : Complex;
  var
    XX, Yp1, Ym1 : Float;
  begin
    if (Z.X = 0.0) and (Abs(Z.Y) = 1.0) then  { Z = +/- i }
      begin
        MathErr := FN_SING;
        CArcTan := Cmplx(0.0, Sgn(Z.Y) * MAXNUM);
        Exit;
      end;
    XX := Sqr(Z.X);
    Yp1 := Z.Y + 1.0;
    Ym1 := Z.Y - 1.0;
    CArcTan := Cmplx(0.5 * (ArcTan2(Z.X, - Ym1) - ArcTan2(- Z.X, Yp1)),
                     0.25 * Log((XX + Sqr(Yp1)) / (XX + Sqr(Ym1))));
  end;

  function Expo(Z : Complex) : Complex;
  var
    ExpX : Float;
  begin
    ExpX := Expo(Z.X);
    if MathErr = FN_OK then
      Expo := Cmplx(ExpX * Cos(Z.Y), ExpX * Sin(Z.Y))
    else
      Expo := Cmplx(ExpX, 0.0);
  end;

  function Log(Z : Complex) : Complex;
  var
    R, Theta, LnR : Float;
  begin
    R := CAbs(Z);
    Theta := CArg(Z);
    LnR := Log(R);
    if MathErr = FN_OK then
      Log := Cmplx(LnR, Theta)
    else
      Log := Cmplx(- MAXNUM, 0.0);
  end;

  function Power(Z : Complex; N : Integer) : Complex;
  var
    R, Theta : Float;
  begin
    R := CAbs(Z);
    Theta := CArg(Z);
    if R = 0.0 then
      if N = 0 then
        Power := C_one
      else if N > 0 then
        Power := C_zero
      else
        begin
          MathErr := FN_SING;
          Power := C_infinity;
        end
    else
      Power := Polar(R ** N, FixAngle(N * Theta));
  end;

  function Power(Z : Complex; X : Float) : Complex;
  var
    R, Theta : Float;
  begin
    R := CAbs(Z);
    Theta := CArg(Z);
    if R = 0.0 then
      if X = 0.0 then
        Power := C_one
      else if X > 0.0 then
        Power := C_zero
      else
        begin
          MathErr := FN_SING;
          Power := C_infinity;
        end
    else
      Power := Polar(R ** X, FixAngle(X * Theta));
  end;

  function Power(A, B : Complex) : Complex;
  begin
    if (A.X = 0.0) and (A.Y = 0.0) then
      if (B.X = 0.0) and (B.Y = 0.0) then
        Power := C_one                     { lim a^a = 1 when a -> 0 }
      else
        Power := C_zero                    { 0^b = 0 }
    else
      Power := Expo(B * Log(A));           { a^b = exp(b * ln(a)) }
  end;
  
  function Tan(Z : Complex) : Complex;
  var
    X2, Y2, SinX2, CosX2, SinhY2, CoshY2, Temp : Float;
  begin
    X2 := 2.0 * Z.X;
    Y2 := 2.0 * Z.Y;
    SinX2 := Sin(X2);
    CosX2 := Cos(X2);
    SinhCosh(Y2, SinhY2, CoshY2);
    if MathErr = FN_OK then
      Temp := CosX2 + CoshY2
    else
      Temp := CoshY2;
    if Temp <> 0.0 then
      Tan := Cmplx(SinX2 / Temp, SinhY2 / Temp)
    else
      begin                  { Z = Pi/2 + k*Pi }
        MathErr := FN_SING;
        Tan := C_infinity;
      end;
  end;

  function ArcSin(Z : Complex) : Complex;
  var
    Rp, Rm, S, T, X2, XX, YY : Float;
    B                        : Complex;
  begin
    B := Cmplx(Z.Y, - Z.X);  { Y - i*X }
    X2 := 2.0 * Z.X;
    XX := Sqr(Z.X);
    YY := Sqr(Z.Y);
    S := XX + YY + 1.0;
    Rp := 0.5 * Sqrt(S + X2);
    Rm := 0.5 * Sqrt(S - X2);
    T := Rp + Rm;
    ArcSin := Cmplx(ArcSin(Rp - Rm), Sgn(B) * Log(T + Sqrt(Sqr(T) - 1.0)));
  end;

  function ArcCos(Z : Complex) : Complex;
  begin
    ArcCos := C_pi_div_2 - ArcSin(Z);  { Pi/2 - ArcSin(Z) }
  end;

  function Sinh(Z : Complex) : Complex;
  var
    SinY, CosY, SinhX, CoshX : Float;
  begin
    SinY := Sin(Z.Y);
    CosY := Cos(Z.Y);
    SinhCosh(Z.X, SinhX, CoshX);
    Sinh := Cmplx(SinhX * CosY, CoshX * SinY);
  end;

  function Cosh(Z : Complex) : Complex;
  var
    SinY, CosY, SinhX, CoshX : Float;
  begin
    SinY := Sin(Z.Y);
    CosY := Cos(Z.Y);
    SinhCosh(Z.X, SinhX, CoshX);
    Cosh := Cmplx(CoshX * CosY, SinhX * SinY)
  end;

  function Tanh(Z : Complex) : Complex;
  var
    X2, Y2, SinY2, CosY2, SinhX2, CoshX2, Temp : Float;
  begin
    X2 := 2.0 * Z.X;
    Y2 := 2.0 * Z.Y;
    SinY2 := Sin(Y2);
    CosY2 := Cos(Y2);
    SinhCosh(X2, SinhX2, CoshX2);
    if MathErr = FN_OK then
      Temp := CoshX2 + CosY2
    else
      Temp := CoshX2;
    if Temp <> 0.0 then
      Tanh := Cmplx(SinhX2 / Temp, SinY2 / Temp)
    else
      begin                  { Z = i * (Pi/2 + k*Pi) }
        MathErr := FN_SING;
        Tanh := Cmplx(0.0, MAXNUM);
      end;
  end;

  function ArcSinh(Z : Complex) : Complex;
  { ArcSinh(Z) = -i*ArcSin(i*Z) }
  begin
    ArcSinh := - C_i * ArcSin(C_i * Z);
  end;

  function ArcCosh(Z : Complex) : Complex;
  { ArcCosh(Z) = CSgn(Y + i(1-X))*i*ArcCos(Z) where Z = X+iY }
  var
    Temp : Complex;
  begin
    Temp := C_i * ArcCos(Z);
    if Sgn(Cmplx(Z.Y, 1.0 - Z.X)) = -1 then
      Temp := - Temp;
    ArcCosh := Temp;
  end;

  function ArcTanh(Z : Complex) : Complex;
  { ArcTanh(Z) = -i*ArcTan(i*Z) }
  begin
    if (Abs(Z.X) = 1.0) and (Z.Y = 0.0) then  { A = +/- 1 }
      begin
        MathErr := FN_SING;
        ArcTanh := Cmplx(Sgn(Z.X) * MAXNUM, 0.0);
      end
    else
      ArcTanh := - C_i * CArcTan(C_i * Z);
  end;

{ ----------------------------------------------------------------------
  Special functions
  ---------------------------------------------------------------------- }

const { Used by IGamma and IBeta }
  BIG    = 9.223372036854775808E18;
  BIGINV = 1.084202172485504434007E-19;

type
  TabCoef = array[0..9] of Float;

  function PolEvl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates polynomial of degree N:

                        2          N
    y  =  C  + C x + C x  +...+ C x
           0    1     2          N

  Coefficients are stored in reverse order:

  Coef[0] = C  , ..., Coef[N] = C
             N                   0

  The function P1Evl() assumes that Coef[N] = 1.0 and is
  omitted from the array. Its calling arguments are
  otherwise the same as PolEvl().
  ---------------------------------------------------------------------- }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := Coef[0];
    for I := 1 to N do
      Ans := Ans * X + Coef[I];
    PolEvl := Ans;
  end;

  function P1Evl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluate polynomial when coefficient of X is 1.0.
  Otherwise same as PolEvl.
  ---------------------------------------------------------------------- }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := X + Coef[0];
    for I := 1 to N - 1 do
      Ans := Ans * X + Coef[I];
    P1Evl := Ans;
  end;

  function SgnGamma(X : Float) : Integer;
  begin
    if X > 0.0 then
      SgnGamma := 1
    else if Odd(Trunc(Abs(X))) then
      SgnGamma := 1
    else
      SgnGamma := - 1;
  end;

  function Stirf(X : Float) : Float;
  { Stirling's formula for the gamma function
    Gamma(x) = Sqrt(2*Pi) x^(x-.5) exp(-x) (1 + 1/x P(1/x))
    where P(x) is a polynomial }
  const
    STIR : TabCoef = (
        7.147391378143610789273E-4,
      - 2.363848809501759061727E-5,
      - 5.950237554056330156018E-4,
        6.989332260623193171870E-5,
        7.840334842744753003862E-4,
      - 2.294719747873185405699E-4,
      - 2.681327161876304418288E-3,
        3.472222222230075327854E-3,
        8.333333333333331800504E-2,
        0);

  var
    W, P : Float;
  begin
    W := 1.0 / X;
    if X > 1024.0 then
      begin
        P := 6.97281375836585777429E-5 * W + 7.84039221720066627474E-4;
        P := P * W - 2.29472093621399176955E-4;
        P := P * W - 2.68132716049382716049E-3;
        P := P * W + 3.47222222222222222222E-3;
        P := P * W + 8.33333333333333333333E-2;
      end
    else
      P := PolEvl(W, STIR, 8);
    Stirf := SQRT2PI * Exp((X - 0.5) *
               Ln(X) - X) * (1.0 + W * P);
  end;

  function GamSmall(X1, Z : Float) : Float;
  { Gamma function for small values of the argument }
  const
    S : TabCoef = (
      - 1.193945051381510095614E-3,
        7.220599478036909672331E-3,
      - 9.622023360406271645744E-3,
      - 4.219773360705915470089E-2,
        1.665386113720805206758E-1,
      - 4.200263503403344054473E-2,
      - 6.558780715202540684668E-1,
        5.772156649015328608253E-1,
        1.000000000000000000000E0,
        0);

    SN : TabCoef = (
        1.133374167243894382010E-3,
        7.220837261893170325704E-3,
        9.621911155035976733706E-3,
      - 4.219773343731191721664E-2,
      - 1.665386113944413519335E-1,
      - 4.200263503402112910504E-2,
        6.558780715202536547116E-1,
        5.772156649015328608727E-1,
      - 1.000000000000000000000E0,
        0);

  var
    P : Float;
  begin
    if X1 = 0.0 then
      begin
        GamSmall := DefaultVal(FN_SING);
        Exit;
      end;
    if X1 < 0.0 then
      begin
        X1 := - X1;
        P := PolEvl(X1, SN, 8);
      end
    else
      P := PolEvl(X1, S, 8);
    GamSmall := Z / (X1 * P);
  end;

  function StirfL(X : Float) : Float;
  { Approximate Ln(Gamma) by Stirling's formula, for X >= 13 }
  const
    P : TabCoef = (
        4.885026142432270781165E-3,
      - 1.880801938119376907179E-3,
        8.412723297322498080632E-4,
      - 5.952345851765688514613E-4,
        7.936507795855070755671E-4,
      - 2.777777777750349603440E-3,
        8.333333333333331447505E-2,
        0, 0, 0);

  var
    Q, W : Float;
  begin
    Q := Ln(X) * (X - 0.5) - X;
    Q := Q + LNSQRT2PI;
    if X > 1.0E+10 then
      StirfL := Q
    else
      begin
        W := 1.0 / Sqr(X);
        StirfL := Q + PolEvl(W, P, 6) / X;
      end;
  end;

  function Gamma(X : Float) : Float;
  const
    P : TabCoef = (
      4.212760487471622013093E-5,
      4.542931960608009155600E-4,
      4.092666828394035500949E-3,
      2.385363243461108252554E-2,
      1.113062816019361559013E-1,
      3.629515436640239168939E-1,
      8.378004301573126728826E-1,
      1.000000000000000000009E0,
      0, 0);

    Q : TabCoef = (
      - 1.397148517476170440917E-5,
        2.346584059160635244282E-4,
      - 1.237799246653152231188E-3,
      - 7.955933682494738320586E-4,
        2.773706565840072979165E-2,
      - 4.633887671244534213831E-2,
      - 2.243510905670329164562E-1,
        4.150160950588455434583E-1,
        9.999999999999999999908E-1,
        0);

  var
    SgnGam, N : Integer;
    A, X1, Z : Float;
  begin
    MathErr := FN_OK;
    SgnGam := SgnGamma(X);

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        Gamma := SgnGam * DefaultVal(FN_SING);
        Exit;
      end;

    if X > MAXGAM then
      begin
        Gamma := DefaultVal(FN_OVERFLOW);
        Exit;
      end;

    A := Abs(X);
    if A > 13.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := A - N;
              end;
            Z := Abs(A * Sin(PI * Z)) * Stirf(A);
            if Z <= PI / MAXNUM then
              begin
                Gamma := SgnGam * DefaultVal(FN_OVERFLOW);
                Exit;
              end;
            Z := PI / Z;
          end
        else
          Z := Stirf(X);
        Gamma := SgnGam * Z;
      end
    else
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3.0 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < - 0.03125 do
          begin
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if X1 <= 0.03125 then
          Gamma := GamSmall(X1, Z)
        else
          begin
            while X1 < 2.0 do
              begin
                Z := Z / X1;
                X1 := X1 + 1.0;
              end;
            if (X1 = 2.0) or (X1 = 3.0) then
              Gamma := Z
            else
              begin
                X1 := X1 - 2.0;
                Gamma := Z * PolEvl(X1, P, 7) / PolEvl(X1, Q, 8);
              end;
          end;
      end;
  end;

  function LnGamma(X : Float) : Float;
  const
    P : TabCoef = (
      - 2.163690827643812857640E3,
      - 8.723871522843511459790E4,
      - 1.104326814691464261197E6,
      - 6.111225012005214299996E6,
      - 1.625568062543700591014E7,
      - 2.003937418103815175475E7,
      - 8.875666783650703802159E6,
        0, 0, 0);

    Q : TabCoef = (
      - 5.139481484435370143617E2,
      - 3.403570840534304670537E4,
      - 6.227441164066219501697E5,
      - 4.814940379411882186630E6,
      - 1.785433287045078156959E7,
      - 3.138646407656182662088E7,
      - 2.099336717757895876142E7,
        0, 0, 0);

  var
    N : Integer;
    A, X1, Z : Float;
  begin
    MathErr := FN_OK;

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        LnGamma := DefaultVal(FN_SING);
        Exit;
      end;

    if X > MAXLGM then
      begin
        LnGamma := DefaultVal(FN_OVERFLOW);
        Exit;
      end;

    A := Abs(X);
    if A > 34.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := N - A;
              end;
            Z := A * Sin(PI * Z);
            if Z = 0.0 then
              begin
                LnGamma := DefaultVal(FN_OVERFLOW);
                Exit;
              end;
            Z := LNPI - Ln(Z) - StirfL(A);
          end
        else
          Z := StirfL(X);
        LnGamma := Z;
      end
    else if X < 13.0 then
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < 2.0 do
          begin
            if Abs(X1) <= 0.03125 then
              begin
                LnGamma := Ln(Abs(GamSmall(X1, Z)));
                Exit;
              end;
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if Z < 0.0 then Z := - Z;
        if X1 = 2.0 then
          LnGamma := Ln(Z)
        else
          begin
            X1 := X1 - 2.0;
            LnGamma := X1 * PolEvl(X1, P, 6) / P1Evl(X1, Q, 7) +
                         Ln(Z);
          end;
      end
    else
      LnGamma := StirfL(X);
  end;

//stopped here
  function ApproxLnGamma(Z : Complex) : Complex;
  { This is the approximation used in the National Bureau of
    Standards "Table of the Gamma Function for Complex Arguments,"
    Applied Mathematics Series 34, 1954. The NBS table was created
    using this approximation over the area 9 < Re(z) < 10 and
    0 < Im(z) < 10. Other table values were computed using the
    relationship:
        _                   _
    ln | (z+1) = ln z + ln | (z) }

  const
    C : array[1..8] of Float =
    (8.33333333333333E-02, - 2.77777777777778E-03,
     7.93650793650794E-04, - 5.95238095238095E-04,
     8.41750841750842E-04, - 1.91752691752692E-03,
     6.41025641025641E-03, - 2.95506535947712E-02);
  var
    I : Integer;
    Powers : array[1..8] of Complex;
    Temp, Sum : Complex;
  begin
    Sum := (Z - 0.5) * Log(Z) - Z;
    Sum.X := Sum.X + LN2PIDIV2;
    Powers[1] := 1.0 / Z;
    Temp := Powers[1] * Powers[1];  { Z^(-2) }
    for I := 2 to 8 do
      Powers[I] := Powers[I - 1] * Temp;
    for I := 8 downto 1 do
      Sum := Sum + Cmplx(C[I] * Powers[I].X, C[I] * Powers[I].Y);
    ApproxLnGamma := Sum;
  end;

  function LnGamma(Z : Complex) : Complex;
  var
    A : Complex;
  begin
    if (Z.X <= 0.0) and (Z.Y = 0.0) then
      if (Int(Z.X - 1E-8) - Z.X) = 0.0 then  { Negative integer? }
        begin
          MathErr := FN_SING;
          LnGamma := C_infinity;
          Exit
        end;
    if Z.Y < 0.0 then            { 3rd or 4th quadrant? }
      begin
        A := LnGamma(CConj(Z));  { Try again in 1st or 2nd quadrant }
        LnGamma := CConj(A);     { Left this out! 1/3/91 }
      end
    else
      begin
        if Z.X < 9.0 then  { "left" of NBS table range }
          LnGamma := LnGamma(Z + 1.0) - Log(Z)
        else
          LnGamma := ApproxLnGamma(Z)  { NBS table range: 9 < Re(z) < 10 }
      end
  end;

  function IGamma(A, X : Float) : Float;
  var
    Ans, Ax, C, R : Float;
  begin
    MathErr := FN_OK;

    if (X <= 0.0) or (A <= 0.0) then
      begin
        IGamma := 0.0;
        Exit;
      end;

    if (X > 1.0) and (X > A) then
      begin
        IGamma := 1.0 - JGamma(A, X);
        Exit;
      end;

    Ax := A * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) - X - LnGamma(A);
    if Ax < MINLOG then
      begin
        IGamma := DefaultVal(FN_UNDERFLOW);
        Exit;
      end;

    Ax := {$IFDEF CPUP2}fExp{$ELSE}Exp{$ENDIF}(Ax);

    { Power series }
    R := A;
    C := 1.0;
    Ans := 1.0;

    repeat
      R := R + 1.0;
      C := C * X / R;
      Ans := Ans + C;
    until C / Ans <= MACHEP;

    IGamma := Ans * Ax / A;
  end;

  function JGamma(A, X : Float) : Float;
  var
    Ans, C, Yc, Ax, Y, Z, R, T,
    Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2 : Float;
  begin
    MathErr := FN_OK;

    if (X <= 0.0) or (A <= 0.0) then
      begin
        JGamma := 1.0;
        Exit;
      end;

    if (X < 1.0) or (X < A) then
      begin
        JGamma := 1.0 - IGamma(A, X);
        Exit;
      end;

    Ax := A * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) - X - LnGamma(A);

    if Ax < MINLOG then
      begin
        JGamma := DefaultVal(FN_UNDERFLOW);
        Exit;
      end;

    Ax := {$IFDEF CPUP2}fExp{$ELSE}Exp{$ENDIF}(Ax);

    { Continued fraction }
    Y := 1.0 - A;
    Z := X + Y + 1.0;
    C := 0.0;
    Pkm2 := 1.0;
    Qkm2 := X;
    Pkm1 := X + 1.0;
    Qkm1 := Z * X;
    Ans := Pkm1 / Qkm1;

    repeat
      C := C + 1.0;
      Y := Y + 1.0;
      Z := Z + 2.0;
      Yc := Y * C;
      Pk := Pkm1 * Z - Pkm2 * Yc;
      Qk := Qkm1 * Z - Qkm2 * Yc;
      if Qk <> 0.0 then
        begin
          R := Pk / Qk;
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;
      if Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 / BIG;
          Pkm1 := Pkm1 / BIG;
          Qkm2 := Qkm2 / BIG;
          Qkm1 := Qkm1 / BIG;
        end;
    until T <= MACHEP;

    JGamma := Ans * Ax;
  end;

  function Fact(N : Integer) : Float;
  begin
    MathErr := FN_OK;
    if N < 0 then
      Fact := DefaultVal(FN_DOMAIN)
    else if N > MAXFAC then
      Fact := DefaultVal(FN_OVERFLOW)
    else if N <= NFACT then
      Fact := FactArray[N]
    else
      Fact := Gamma(N + 1);
  end;

  function Binomial(N, K : Integer) : Float;
  var
    I, N1 : Integer;
    Prod : Float;
  begin
    MathErr := FN_OK;
    if K < 0 then
      Binomial := 0.0
    else if (K = 0) or (K = N) then
      Binomial := 1.0
    else if (K = 1) or (K = N - 1) then
      Binomial := N
    else
      begin
        if K > N - K then K := N - K;
        N1 := Succ(N);
        Prod := N;
        for I := 2 to K do
          Prod := Prod * (Int(N1 - I) / Int(I));
        Binomial := Int(0.5 + Prod);
      end;
  end;

  function Beta(X, Y : Float) : Float;
  { Computes Beta(X, Y) = Gamma(X) * Gamma(Y) / Gamma(X + Y) }
  var
    Lx, Ly, Lxy : Float;
    SgnBeta : Integer;
  begin
    MathErr := FN_OK;
    SgnBeta := SgnGamma(X) * SgnGamma(Y) * SgnGamma(X + Y);
    Lxy := LnGamma(X + Y);
    if MathErr <> FN_OK then
      begin
        Beta := 0.0;
        Exit;
      end;
    Lx := LnGamma(X);
    if MathErr <> FN_OK then
      begin
        Beta := SgnBeta * MAXNUM;
        Exit;
      end;
    Ly := LnGamma(Y);
    if MathErr <> FN_OK then
      begin
        Beta := SgnBeta * MAXNUM;
        Exit;
      end;
    Beta := SgnBeta * {$IFDEF CPUP2}fExp{$ELSE}Exp{$ENDIF}(Lx + Ly - Lxy);
  end;

  function PSeries(A, B, X : Float) : Float;
  { Power series for incomplete beta integral. Use when B*X is small }
  var
    S, T, U, V, T1, Z, Ai : Float;
    N : Integer;
  begin
    Ai := 1.0 / A;
    U := (1.0 - B) * X;
    V := U / (A + 1.0);
    T1 := V;
    T := U;
    N := 2;
    S := 0.0;
    Z := MACHEP * Ai;
    while Abs(V) > Z do
      begin
        U := (N - B) * X / N;
        T := T * U;
        V := T / (A + N);
        S := S + V;
        N := N + 1;
      end;
    S := S + T1;
    S := S + Ai;

    U := A * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X);
    if (A + B < MAXGAM) and (Abs(U) < MAXLOG) then
      begin
        T := Gamma(A + B) / (Gamma(A) * Gamma(B));
        S := S * T * (X ** A);
      end
    else
      begin
        T := LnGamma(A + B) - LnGamma(A) - LnGamma(B) + U +
             {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(S);
        if T < MINLOG then
          S := 0.0
        else
          S := {$IFDEF CPUP2}fExp{$ELSE}Exp{$ENDIF}(T);
      end;
    PSeries := S;
  end;

  function CFrac1(A, B, X : Float) : Float;
  { Continued fraction expansion #1 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := A + B;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := B - 1.0;
    K7 := K4;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MACHEP;

    repeat
      Xk := - (X * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (X * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 + 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 - 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 * BIGINV;
          Pkm1 := Pkm1 * BIGINV;
          Qkm2 := Qkm2 * BIGINV;
          Qkm1 := Qkm1 * BIGINV;
        end;

      if (Abs(Qk) < BIGINV) or (Abs(Pk) < BIGINV) then
        begin
          Pkm2 := Pkm2 * BIG;
          Pkm1 := Pkm1 * BIG;
          Qkm2 := Qkm2 * BIG;
          Qkm1 := Qkm1 * BIG;
        end;
      N := N + 1;
    until N > 400;
    MathErr := FN_PLOSS;

CDone:
    CFrac1 := Ans;
  end;

  function CFrac2(A, B, X : Float) : Float;
  { Continued fraction expansion #2 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Z, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := B - 1.0;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := A + B;
    K7 := A + 1.0;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Z := X / (1.0 - X);
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MACHEP;

    repeat
      Xk := - (Z * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (Z * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 - 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 + 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 * BIGINV;
          Pkm1 := Pkm1 * BIGINV;
          Qkm2 := Qkm2 * BIGINV;
          Qkm1 := Qkm1 * BIGINV;
        end;

      if (Abs(Qk) < BIGINV) or (Abs(Pk) < BIGINV) then
        begin
          Pkm2 := Pkm2 * BIG;
          Pkm1 := Pkm1 * BIG;
          Qkm2 := Qkm2 * BIG;
          Qkm1 := Qkm1 * BIG;
        end;
      N := N + 1;
    until N > 400;
    MathErr := FN_PLOSS;

CDone:
    CFrac2 := Ans;
  end;

  function IBeta(A, B, X : Float) : Float;
  var
    A1, B1, X1, T, W, Xc, Y : Float;
    Flag : Boolean;
  label
    Done;
  begin
    MathErr := FN_OK;

    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) or (X > 1.0) then
      begin
        IBeta := DefaultVal(FN_DOMAIN);
        Exit;
      end;

    if (X = 0.0) or (X = 1.0) then
      begin
        IBeta := X;
        Exit;
      end;

    Flag := False;
    if (B * X <= 1.0) and (X <= 0.95) then
      begin
        T := PSeries(A, B, X);
        goto Done;
      end;

    W := 1.0 - X;

    { Reverse a and b if x is greater than the mean. }
    if X > A / (A + B) then
      begin
        Flag := True;
        A1 := B;
        B1 := A;
        Xc := X;
        X1 := W;
      end
    else
      begin
        A1 := A;
        B1 := B;
        Xc := W;
        X1 := X;
      end;

    if Flag and (B1 * X1 <= 1.0) and (X1 <= 0.95) then
      begin
        T := PSeries(A1, B1, X1);
        goto Done;
      end;

    { Choose expansion for optimal convergence }
    Y := X1 * (A1 + B1 - 2.0) - (A1 - 1.0);
    if Y < 0.0 then
      W := CFrac1(A1, B1, X1)
    else
      W := CFrac2(A1, B1, X1) / Xc;

    { Multiply w by the factor
     a      b   _             _     _
    x  (1-x)   | (a+b) / ( a | (a) | (b) )    }

    Y := A1 * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X1);
    T := B1 * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(Xc);
    if (A1 + B1 < MAXGAM) and (Abs(Y) < MAXLOG) and (Abs(T) < MAXLOG) then
      begin
        T := Xc ** B1;
        T := T * (X1 ** A1);
        T := T / A1;
        T := T * W;
        T := T * Gamma(A1 + B1) / (Gamma(A1) * Gamma(B1));
      end
    else
      begin
        { Resort to logarithms }
        Y := Y + T + LnGamma(A1 + B1) - LnGamma(A1) - LnGamma(B1) +
             {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(W / A1);
        if Y < MINLOG then
          T := 0.0
        else
          T := {$IFDEF CPUP2}fExp{$ELSE}Exp{$ENDIF}(Y);
      end;

Done:
    if Flag then
      if T <= MACHEP then
        T := 1.0 - MACHEP
      else
        T := 1.0 - T;

    IBeta := T;
  end;

  function Erf(X : Float) : Float;
  begin
    if X < 0.0 then
      Erf := - IGamma(0.5, Sqr(X))
    else
      Erf := IGamma(0.5, Sqr(X));
  end;

  function Erfc(X : Float) : Float;
  begin
    if X < 0.0 then
      Erfc := 1.0 + IGamma(0.5, Sqr(X))
    else
      Erfc := JGamma(0.5, Sqr(X));
  end;

{ ----------------------------------------------------------------------
  Probability functions
  ---------------------------------------------------------------------- }

  function PBinom(N : Integer; P : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
      PBinom := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      PBinom := (1.0 - P) ** N
    else if K = N then
      PBinom := P ** N
    else
      PBinom := Binomial(N, K) * (P ** K) * ((1.0 - P) ** (N - K));
  end;

  function FBinom(N : Integer; P : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
      FBinom := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      FBinom := (1.0 - P) ** N
    else if K = N then
      FBinom := 1.0
    else
      FBinom := 1.0 - IBeta(K + 1, N - K, P);
  end;

  function PPoisson(Mu : Float; K : Integer) : Float;
  var
    P : Float;
    I : Integer;
  begin
    MathErr := FN_OK;
    if (Mu <= 0.0) or (K < 0) then
      PPoisson := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      PPoisson := Expo(- Mu)
    else
      begin
        P := Mu;
        for I := 2 to K do
          P := P * Mu / I;
        PPoisson := Expo(- Mu) * P;
      end;
  end;

  function FPoisson(Mu : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (Mu <= 0.0) or (K < 0) then
      FPoisson := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      FPoisson := Expo(- Mu)
    else
      FPoisson := JGamma(K + 1, Mu);
  end;

  function DNorm(X : Float) : Float;
  begin
    DNorm := INVSQRT2PI * Expo(- 0.5 * Sqr(X));
  end;

  function FNorm(X : Float) : Float;
  begin
    FNorm := 0.5 * (1.0 + Erf(X * SQRT2DIV2));
  end;

  function InvNorm(P : Float) : Float;
{ ----------------------------------------------------------------------
  Inverse of Normal distribution function

  Returns the argument, X, for which the area under the Gaussian
  probability density function (integrated from minus infinity to X)
  is equal to P.

  Translated from Cephes library.
  ---------------------------------------------------------------------- }
  const
    P0 : TabCoef = (
        8.779679420055069160496E-3,
      - 7.649544967784380691785E-1,
        2.971493676711545292135E0,
      - 4.144980036933753828858E0,
        2.765359913000830285937E0,
      - 9.570456817794268907847E-1,
        1.659219375097958322098E-1,
      - 1.140013969885358273307E-2,
        0, 0);

    Q0 : TabCoef = (
      - 5.303846964603721860329E0,
        9.908875375256718220854E0,
      - 9.031318655459381388888E0,
        4.496118508523213950686E0,
      - 1.250016921424819972516E0,
        1.823840725000038842075E-1,
      - 1.088633151006419263153E-2,
        0, 0, 0);

    P1 : TabCoef = (
      4.302849750435552180717E0,
      4.360209451837096682600E1,
      9.454613328844768318162E1,
      9.336735653151873871756E1,
      5.305046472191852391737E1,
      1.775851836288460008093E1,
      3.640308340137013109859E0,
      3.691354900171224122390E-1,
      1.403530274998072987187E-2,
      1.377145111380960566197E-4);

    Q1 : TabCoef = (
      2.001425109170530136741E1,
      7.079893963891488254284E1,
      8.033277265194672063478E1,
      5.034715121553662712917E1,
      1.779820137342627204153E1,
      3.845554944954699547539E0,
      3.993627390181238962857E-1,
      1.526870689522191191380E-2,
      1.498700676286675466900E-4,
      0);

    P2 : TabCoef = (
      3.244525725312906932464E0,
      6.856256488128415760904E0,
      3.765479340423144482796E0,
      1.240893301734538935324E0,
      1.740282292791367834724E-1,
      9.082834200993107441750E-3,
      1.617870121822776093899E-4,
      7.377405643054504178605E-7,
      0, 0);

    Q2 : TabCoef = (
      6.021509481727510630722E0,
      3.528463857156936773982E0,
      1.289185315656302878699E0,
      1.874290142615703609510E-1,
      9.867655920899636109122E-3,
      1.760452434084258930442E-4,
      8.028288500688538331773E-7,
      0, 0, 0);

    P3 : TabCoef = (
        2.020331091302772535752E0,
        2.133020661587413053144E0,
        2.114822217898707063183E-1,
      - 6.500909615246067985872E-3,
      - 7.279315200737344309241E-4,
      - 1.275404675610280787619E-5,
      - 6.433966387613344714022E-8,
      - 7.772828380948163386917E-11,
        0, 0);

    Q3 : TabCoef = (
        2.278210997153449199574E0,
        2.345321838870438196534E-1,
      - 6.916708899719964982855E-3,
      - 7.908542088737858288849E-4,
      - 1.387652389480217178984E-5,
      - 7.001476867559193780666E-8,
      - 8.458494263787680376729E-11,
        0, 0, 0);

  var
    X, Y, Z, Y2, X0, X1 : Float;
    Code : Integer;
  begin
    if (P <= 0.0) or (P >= 1.0) then
      begin
        InvNorm := DefaultVal(FN_DOMAIN);
        Exit;
      end;

    Code := 1;
    Y := P;
    if Y > (1.0 - 0.13533528323661269189) then { 0.135... = exp(-2) }
      begin
        Y := 1.0 - Y;
        Code := 0;
      end;
    if Y > 0.13533528323661269189 then
      begin
        Y := Y - 0.5;
        Y2 := Y * Y;
        X := Y + Y * (Y2 * PolEvl(Y2, P0, 7) / P1Evl(Y2, Q0, 7));
        X := X * SQRT2PI;
        InvNorm := X;
        Exit;
      end;

    X := Sqrt(- 2.0 * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(Y));
    X0 := X - {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) / X;
    Z := 1.0 / X;
    if X < 8.0 then
      X1 := Z * PolEvl(Z, P1, 9) / P1Evl(Z, Q1, 9)
    else if X < 32.0 then
      X1 := Z * PolEvl(Z, P2, 7) / P1Evl(Z, Q2, 7)
    else
      X1 := Z * PolEvl(Z, P3, 7) / P1Evl(Z, Q3, 7);
    X := X0 - X1;
    if Code <> 0 then
      X := - X;
    InvNorm := X;
  end;

  function PNorm(X : Float) : Float;
  var
    A : Float;
  begin
    A := Abs(X);
    MathErr := FN_OK;
    if A = 0.0 then
      PNorm := 1.0
    else if A < 1.0 then
      PNorm := 1.0 - Erf(A * SQRT2DIV2)
    else
      PNorm := Erfc(A * SQRT2DIV2);
  end;

  function DStudent(Nu : Integer; X : Float) : Float;
  var
    L, P, Q : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      DStudent := DefaultVal(FN_DOMAIN)
    else
      begin
        P := 0.5 * (Nu + 1);
        Q := 0.5 * Nu;
        L := LnGamma(P) - LnGamma(Q) -
               0.5 * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(Nu * PI) -
               P * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(1.0 + Sqr(X) / Nu);
        DStudent := Expo(L);
      end;
  end;

  function FStudent(Nu : Integer; X : Float) : Float;
  var
    F : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      FStudent := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      FStudent := 0.5
    else
      begin
        F := 0.5 * IBeta(0.5 * Nu, 0.5, Nu / (Nu + Sqr(X)));
        if X < 0.0 then FStudent := F else FStudent := 1.0 - F;
      end;
  end;

  function PStudent(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      PStudent := DefaultVal(FN_DOMAIN)
    else
      PStudent := IBeta(0.5 * Nu, 0.5, Nu / (Nu + Sqr(X)));
  end;

  function DKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    DKhi2 := DGamma(0.5 * Nu, 0.5, X);
  end;

  function FKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu < 1) or (X <= 0.0) then
      FKhi2 := DefaultVal(FN_DOMAIN)
    else
      FKhi2 := IGamma(0.5 * Nu, 0.5 * X);
  end;

  function PKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu < 1) or (X <= 0.0) then
      PKhi2 := DefaultVal(FN_DOMAIN)
    else
      PKhi2 := JGamma(0.5 * Nu, 0.5 * X);
  end;

  function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  var
    P1, P2, R, S, L : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      DSnedecor := DefaultVal(FN_DOMAIN)
    else
      begin
        R := Int(Nu1) / Int(Nu2);
        P1 := 0.5 * Nu1;
        P2 := 0.5 * Nu2;
        S := P1 + P2;
        L := LnGamma(S) - LnGamma(P1) - LnGamma(P2)
             + P1 * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(R) +
             (P1 - 1.0) * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) -
             S * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(1.0 + R * X);
        DSnedecor := Expo(L);
      end;
  end;

  function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      FSnedecor := DefaultVal(FN_DOMAIN)
    else
      FSnedecor := 1.0 - IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
  end;

  function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      PSnedecor := DefaultVal(FN_DOMAIN)
    else
      PSnedecor := IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
  end;

  function DExpo(A, X : Float) : Float;
  begin
    if (A <= 0.0) or (X < 0.0) then
      DExpo := DefaultVal(FN_DOMAIN)
    else
      DExpo := A * Expo(- A * X);
  end;

  function FExpo(A, X : Float) : Float;
  begin
    if (A <= 0.0) or (X < 0.0) then
      FExpo := DefaultVal(FN_DOMAIN)
    else
      FExpo := 1.0 - Expo(- A * X);
  end;

  function DBeta(A, B, X : Float) : Float;
  var
    L : Float;
  begin
    MathErr := FN_OK;
    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) or (X > 1.0) then
      DBeta := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      if A < 1.0 then DBeta := DefaultVal(FN_SING) else DBeta := 0.0
    else if X = 1.0 then
      if B < 1.0 then DBeta := DefaultVal(FN_SING) else DBeta := 0.0
    else
      begin
        L := LnGamma(A + B) - LnGamma(A) - LnGamma(B) +
             (A - 1.0) * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) +
             (B - 1.0) * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(1.0 - X);
        DBeta := Expo(L);
      end;
  end;

  function FBeta(A, B, X : Float) : Float;
  begin
    FBeta := IBeta(A, B, X);
  end;

  function DGamma(A, B, X : Float) : Float;
  var
    L : Float;
  begin
    MathErr := FN_OK;
    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) then
      DGamma := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      if A < 1.0 then
        DGamma := DefaultVal(FN_SING)
      else if A = 1.0 then
        DGamma := B
      else
        DGamma := 0.0
    else
      begin
        L := A * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(B) - LnGamma(A) +
             (A - 1.0) * {$IFDEF CPUP2}fLn{$ELSE}Ln{$ENDIF}(X) - B * X;
        DGamma := Expo(L);
      end;
  end;

  function FGamma(A, B, X : Float) : Float;
  begin
    FGamma := IGamma(A, B * X);
  end;

{ ----------------------------------------------------------------------
  Random numbers ("Multiply-With-Carry" generator from G. Marsaglia)
  ---------------------------------------------------------------------- }

var
  X1, X2     : LongInt;  { Uniform random integers }
  C1, C2     : LongInt;  { Carries }
  Gauss_Save : Float;    { Saves a gaussian random number }
  Gauss_Set  : Boolean;  { Flags if a gaussian number has been saved }

  procedure RMarIn(Seed1, Seed2 : Integer);
  begin
    X1 := Seed1;
    X2 := Seed2;
    C1 := 0;
    C2 := 0;
  end;

  function IRanMar : LongInt;
  var
    Y1, Y2 : LongInt;
  begin
    Y1 := 18000 * X1 + C1;
    X1 := Y1 and 65535;
    C1 := Y1 shr 16;
    Y2 := 30903 * X2 + C2;
    X2 := Y2 and 65535;
    C2 := Y2 shr 16;
    IRanMar := (X1 shl 16) + (X2 and 65535);
  end;

  function RanMar : Float;
  begin
    RanMar := (IRanMar + 2147483648.0) / 4294967296.0;
  end;

  function RanGaussStd : Float;
  { Computes 2 random numbers from the standard normal distribution,
    returns one and saves the other for the next call }
  var
    R, Theta, SinTheta, CosTheta : Float;
  begin
    if not Gauss_Set then
      begin
        R := Sqrt(- 2.0 * Log(RanMar));
        Theta := TWOPI * RanMar;
        SinTheta := {$IFDEF CPUP2}fSin{$ELSE}Sin{$ENDIF}(Theta);
        CosTheta := {$IFDEF CPUP2}fCos{$ELSE}Cos{$ENDIF}(Theta);
        RanGaussStd := R * CosTheta;  { Return 1st number }
        Gauss_Save := R * SinTheta;   { Save 2nd number }
      end
    else
      RanGaussStd := Gauss_Save;      { Return saved number }
    Gauss_Set := not Gauss_Set;
  end;

  function RanGauss(Mu, Sigma : Float) : Float;
  { Returns a random number from the normal distribution
    with mean Mu and standard deviation Sigma }
  begin
    RanGauss := Mu + Sigma * RanGaussStd;
  end;



function Sign(const AValue: Integer): TValueSign;inline;

begin
  If Avalue<0 then
    Result:=NegativeValue
  else If Avalue>0 then
    Result:=PositiveValue
  else
    Result:=ZeroValue;
end;

function Sign(const AValue: Int64): TValueSign;inline;

begin
  If Avalue<0 then
    Result:=NegativeValue
  else If Avalue>0 then
    Result:=PositiveValue
  else
    Result:=ZeroValue;
end;


function Sign(const AValue: Double): TValueSign;inline;

begin
  If Avalue<0.0 then
    Result:=NegativeValue
  else If Avalue>0.0 then
    Result:=PositiveValue
  else
    Result:=ZeroValue;
end;

function degtorad(deg : float) : float;

  begin
     degtorad:=deg*(pi/180.0);
  end;

function radtodeg(rad : float) : float;

  begin
     radtodeg:=rad*(180.0/pi);
  end;

function gradtorad(grad : float) : float;

  begin
     gradtorad:=grad*(pi/200.0);
  end;

function radtograd(rad : float) : float;

  begin
     radtograd:=rad*(200.0/pi);
  end;

function degtograd(deg : float) : float;

  begin
     degtograd:=deg*(200.0/180.0);
  end;

function gradtodeg(grad : float) : float;

  begin
     gradtodeg:=grad*(180.0/200.0);
  end;

function cycletorad(cycle : float) : float;

  begin
     cycletorad:=(2*pi)*cycle;
  end;

function radtocycle(rad : float) : float;

  begin
     { avoid division }
     radtocycle:=rad*(1/(2*pi));
  end;

function tan(x : float) : float;
  var
    _sin,_cos : float;
  begin
    sincos(x,_sin,_cos);
    tan:=_sin/_cos;
  end;

function cotan(x : float) : float;
  var
    _sin,_cos : float;
  begin
    sincos(x,_sin,_cos);
    cotan:=_cos/_sin;
  end;

function cot(x : float) : float; inline;
begin
  cot := cotan(x);
end;

procedure sincos(theta : float;out sinus,cosinus : float);
  begin
    sinus:=sin(theta);
    cosinus:=cos(theta);
  end;

function secant(x : float) : float; inline;
begin
  secant := 1 / cos(x);
end;


function cosecant(x : float) : float; inline;
begin
  cosecant := 1 / sin(x);
end;


function sec(x : float) : float; inline;
begin
  sec := secant(x);
end;


function csc(x : float) : float; inline;
begin
  csc := cosecant(x);
end;


function lnxp1(x : float) : float;

  begin
     if x<-1 then
       InvalidArgument;
     lnxp1:=ln(1+x);
  end;

function ceil(x : float) : integer;

  begin
    Ceil:=Trunc(x);
    If Frac(x)>0 then
      Ceil:=Ceil+1;
  end;

function floor(x : float) : integer;

  begin
     Floor:=Trunc(x);
     If Frac(x)<0 then
       Floor := Floor-1;
  end;


procedure Frexp(X: float; var Mantissa: float; var Exponent: integer);
begin
  Exponent:=0;
  if (X<>0) then
    if (abs(X)<0.5) then
      repeat
        X:=X*2;
        Dec(Exponent);
      until (abs(X)>=0.5)
    else
      while (abs(X)>=1) do
        begin
        X:=X/2;
        Inc(Exponent);
        end;
  Mantissa:=X;
end;

function ldexp(x : float;const p : Integer) : float;

  begin
     ldexp:=x*intpower(2.0,p);
  end;

function mean(const data : array of Double) : float;

  begin
     Result:=Mean(PDouble(@data[0]),High(Data)+1);
  end;

function mean(const data : PDouble; Const N : longint) : float;

  begin
     mean:=sum(Data,N);
     mean:=mean/N;
  end;

function sum(const data : array of Double) : float;

  begin
     Result:=Sum(PDouble(@Data[0]),High(Data)+1);
  end;

function sum(const data : PDouble;Const N : longint) : float;

  var
     i : longint;

  begin
     sum:=0.0;
     for i:=0 to N-1 do
       sum:=sum+data[i];
  end;


function sumInt(const data : PInt64;Const N : longint) : Int64;

  var
     i : longint;

  begin
     sumInt:=0;
     for i:=0 to N-1 do
       sumInt:=sumInt+data[i];
  end;

function sumInt(const data : array of Int64) : Int64;

  begin
     Result:=SumInt(@Data[0],High(Data)+1);
  end;


 function sumofsquares(const data : array of Double) : float;

 begin
   Result:=sumofsquares(PDouble(@data[0]),High(Data)+1);
 end;

 function sumofsquares(const data : PDouble; Const N : Integer) : float;

  var
     i : longint;

  begin
     sumofsquares:=0.0;
     for i:=0 to N-1 do
       sumofsquares:=sumofsquares+sqr(data[i]);
  end;


function randg(mean,stddev : float) : float;

  Var U1,S2 : Float;

  begin
     repeat
       u1:= 2*random-1;
       S2:=Sqr(U1)+sqr(2*random-1);
     until s2<1;
     randg:=Sqrt(-2*ln(S2)/S2)*u1*stddev+Mean;
  end;


function stddev(const data : array of Double) : float;

begin
  Result:=Stddev(PDouble(@Data[0]),High(Data)+1)
end;

function stddev(const data : PDouble; Const N : Integer) : float;

  begin
     StdDev:=Sqrt(Variance(Data,N));
  end;

function MinIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] < Result Then Result := Data[I];
end;

function MaxIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] > Result Then Result := Data[I];
end;

function MinValue(const Data: PInteger; Const N : Integer): Integer;
var
  I: Integer;
begin
  Result := Data[0];
  For I := 1 To N-1 do
    If Data[I] < Result Then Result := Data[I];
end;

function maxvalue(const data : PInteger; Const N : Integer) : Integer;

var
   i : longint;

begin
   { get an initial value }
   maxvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]>maxvalue then
       maxvalue:=data[i];
end;


function minvalue(const data : PDouble; Const N : Integer) : Double;

var
   i : longint;

begin
   { get an initial value }
   minvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]<minvalue then
       minvalue:=data[i];
end;


function maxvalue(const data : PDouble; Const N : Integer) : Double;

var
   i : longint;

begin
   { get an initial value }
   maxvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]>maxvalue then
       maxvalue:=data[i];
end;

function Min(a, b: Integer): Integer;inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Integer): Integer;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;


function Min(a, b: Int64): Int64;inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Int64): Int64;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Double): Double;inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Double): Double;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function InRange(const AValue, AMin, AMax: Integer): Boolean;inline;

begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;

function InRange(const AValue, AMin, AMax: Int64): Boolean;inline;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;

function InRange(const AValue, AMin, AMax: Double): Boolean;inline;

begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;inline;

begin
  Result:=AValue;
  If Result<AMin then
    Result:=AMin
  else if Result>AMax then
    Result:=AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Int64): Int64;inline;

begin
  Result:=AValue;
  If Result<AMin then
    Result:=AMin
  else if Result>AMax then
    Result:=AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Double): Double;inline;

begin
  Result:=AValue;
  If Result<AMin then
    Result:=AMin
  else if Result>AMax then
    Result:=AMax;
end;

Const
  EZeroResolution = 1E-16;
  DZeroResolution = 1E-12;
  SZeroResolution = 1E-4;

function IsZero(const A: Double; Epsilon: Double): Boolean;

begin
  if (Epsilon=0) then
    Epsilon:=DZeroResolution;
  Result:=Abs(A)<=Epsilon;
end;

type
  TSplitDouble = packed record
    cards: Array[0..1] of cardinal;
  end;

function IsNan(const d : Double): Boolean;
  var
    fraczero, expMaximal: boolean;
  begin
    expMaximal := ((TSplitDouble(d).cards[1] shr 20) and $7ff) = 2047;
    fraczero := (TSplitDouble(d).cards[1] and $fffff = 0) and
                (TSplitDouble(d).cards[0] = 0);
    Result:=expMaximal and not(fraczero);
  end;


function IsInfinite(const d : Double): Boolean;
  var
    fraczero, expMaximal: boolean;
  begin
    expMaximal := ((TSplitDouble(d).cards[1] shr 20) and $7ff) = 2047;
    fraczero := (TSplitDouble(d).cards[1] and $fffff = 0) and
                (TSplitDouble(d).cards[0] = 0);
    Result:=expMaximal and fraczero;
  end;

function SameValue(const A, B: Double; Epsilon: Double): Boolean;

begin
  if (Epsilon=0) then
    Epsilon:=Max(Min(Abs(A),Abs(B))*DZeroResolution,DZeroResolution);
  if (A>B) then
    Result:=((A-B)<=Epsilon)
  else
    Result:=((B-A)<=Epsilon);
end;


procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);

begin
  Result:=Dividend Div Divisor;
  Remainder:=Dividend Mod Divisor;
end;


function RoundTo(const AValue: Double; const Digits: TRoundToRange): Double;

var
  RV : Double;

begin
  RV:=IntPower(10,Digits);
  Result:=Round(AValue/RV)*RV;
end;

function SimpleRoundTo(const AValue: Double; const Digits: TRoundToRange = -2): Double;

var
  RV : Double;

begin
  RV := IntPower(10, -Digits);
  if AValue < 0 then
    Result := Trunc((AValue*RV) - 0.5)/RV
  else
    Result := Trunc((AValue*RV) + 0.5)/RV;
end;


{ ----------------------------------------------------------------------
  Initialization code
  ---------------------------------------------------------------------- }

var
  I : Integer;

begin
  { Initialize MathErr }
  MathErr := FN_OK;

  { Store the factorials of the first NFACT integers in a table }
  FactArray[0] := 1.0;
  FactArray[1] := 1.0;
  FactArray[2] := 2.0;
  for I := 3 to NFACT do
    FactArray[I] := FactArray[I - 1] * I;

  { Initialize random number generators }
  Gauss_Save := 0.0;
  Gauss_Set := False;
  RMarIn(1802, 9373);
end.

