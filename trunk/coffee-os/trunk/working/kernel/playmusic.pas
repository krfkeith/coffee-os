{Plays music on the PC Speaker.}

unit playmusic;

interface
//need to soan out timer...way too fast.
procedure Play (Note, wait:integer );

//mostly nursery rhymes right now to test the PC speaker(and our ears) with.
procedure LittleLamb;
procedure TwinkleStar;
procedure LondonBridge;
procedure PopGoesWeasel;
procedure FarmerInDell;
procedure ThisOldMan;
procedure EeensySpider;
procedure InAndOut;
procedure MulberryBush;
procedure StarWars;
procedure makesomenoise;
procedure HappyBirthday; //got it off the net.  :-P Lets celebrate on timer elapsed, shall we?
procedure America;

procedure DONE;
//need a longer menu...

implementation


uses
	x86,console,uconsole,timer,keybrd;  


var
    IsBirthday:boolean;
    temp:longword;
 //pianos never change :-) 

//Still need 'tempo' to override the duration and pause delays.
//It adjusts frequarteruency a tad bit, too, but that can't be helped, its normal and expected.
// For now, we assume 4/4 @ 80bpm @ standard tempo (I think, might be slower than that.)


// NOTE: TIMING MAY BE OFF. But the notes are accurate.
// this is due to music source file differences.

Const
//We are much faster than this and the sound blurs together anyway.

   //TenTwentyFourth=1;
   //Fifundereth=2;
   TwoFiftySixth=4; //Bethoven and Vivaldi
   OneTwentyEigth=8;
   SixtyFourth=16;
   ThirtySecond= 31; //Rare beyond here.
   sixteenth = 63; 
   eigth = 125; 
   quarter = 250; 
   quartert= 400;
   half = 500; 
   halft=800;
   whole = 1000; //1 sec=1000ms     
   wholet=1600;
   doubleWhole= 2000;
   NormalBeats=1;
   TechnoBeats=2.2;


//from an assembly table, see the full piano image for details. These are rounded frequarteruencies.

// changed as constants cant start with a number.	

//	Note   Freq
//number indicates octave... AKA C4= middle C.


	 C0 =    16;
	 CSharp0 =   17;
	 D0   =  18;
	 DSharp0  =  19;
	 E0   =  21; 
	 F0   =  22; 
	 FSharp0  =  23; 
	 G0  =   24; 
	 GSharp0  =  26; 
	 A0   =  27; 
	 ASharp0 =   29; 
	 B0  =   31; 
	 
	 C1 =	33; 
	 CSharp1=	35;
	 D1=	37;
	 DSharp1=	39;
	 E1=	41;
	 F1=	44;
	 FSharp1=	46;
	 G1=	49;
	 GSharp1=	52;
	 A1=	55;
	 ASharp1=	58;
	 B1=	62;

	 C2=	65; 
	 CSharp2=	69;
	 D2=	73;
	 DSharp2=	78;
	 E2=	82;
	 F2=	87;
	 FSharp2=	92;
	 G2=	98;
	 GSharp2=	104;
	 A2=	110;
	 ASharp2=	116;
	 B2=	123;

	 C3=	131;
	 CSharp3=	139;
	 D3=	147;
	 DSharp3=	155;
	 E3=	165;
	 F3=	175;
	 FSharp3=	185;
	 G3=	196;
	 GSharp3=	208;
	 A3=	220;
	 ASharp3=	233;
	 B3=	245;

//(MIDDLE)

	 C4=	262;
	 CSharp4=	277;
	 D4=	294;
	 DSharp4=	311;
	 E4=	330;
	 F4=	349;
	 FSharp4=	370;
	 G4=	392;
	 GSharp4=	415;
	 A4=	440;
	 ASharp4=	466;
	 B4=	494;

	 C5=	523; 
	 CSharp5=	554;
	 D5=	587;
	 DSharp5=	622;
	 E5=	659;
	 F5=	698;
	 FSharp5=	740;
	 G5=	784;
	 GSharp5=	831;
	 A5=	880;
	 ASharp5=	932;
	 B5=	988;

	 C6=	1046; 
	 CSharp6=	1109;
	 D6=	1175;
	 DSharp6=	1244;
	 E6=	1328;
	 F6=	1397;
	 FSharp6=	1480;
	 G6=	1568;
	 GSharp6=	1661;
	 A6=	1760;
	 ASharp6=	1865;
	 B6=	1975;

	 C7=	2093; 
	 CSharp7=	2217;
	 D7=	2349;
	 DSharp7=	2489;
	 E7=	2637;
	 F7=	2794;
	 FSharp7=	2960;
	 G7=	3136;
	 GSharp7=	3322;
	 A7=	3520;
	 ASharp7=	3729;
	 B7=	3951;

	 DefTempo=120;

 	DAMPER_ON = 127;

 	DAMPER_OFF = 0;

procedure PlayMenu;

var
  ch:char;
  x,y:integer;

begin

clearscreen;
writestrln('Play which song? ');
writeline;
writestrln('1.) Mary had a little lamb ');
writestrln('2.) Twinkle Twinkle little star ');
writestrln('3.) London bridge is falling down ');
writestrln('4.) Pop goes the Weasel ');
writestrln('5.) Farmer in the dell ');
writestrln('6.) This old man ');
writestrln('7.) Eensy Weensy Spider ');
writestrln('8.) In and out --??'); 
writestrln('9.) Around the Mulberry Bush');
writestrln('0.) Star Wars');
writestrln('A.) Happy Birthday');
writestrln('B.) America the beautiful');
writestrln('Q.) Exit');
ch:=char(readkey);
repeat
case (ch) of

	'1':begin
	     Littlelamb;
		 Done;
	end;
	
	'2':begin
	    TwinkleStar;
		Done;
	end;
	'3':begin
	    LondonBridge;
		Done;
	end;
	'4':begin
	   PopGoesWeasel;
	   Done;
	end;
	'5':begin
	   FarmerInDell;
	   Done;
	end;
	'6':begin
	   ThisOldMan;
	   Done;
	end;
	'7':begin
	   EeensySpider;
	   Done;
	end;
	'8':begin
	   InandOut;
	   Done;
	end;
	'9':begin
	   MulBerrybush;
	   Done;
	end;
	'0':begin
	  StarWars;
	  Done;
	end;
	'A':begin
	  HappyBirthday;
	  Done;
	end;
	'Q':begin
	   exit;
	end;

	else begin
	  writestrln('Invalid Selection.Press any key.');
	  beep;  
	  ch:=char(readkey);
	  gotoxy(1,y);
	  clreol;
	  gotoxy(1,y);	 
    end;

end; {case }
until ch='q';
end;


procedure Play (Note, wait:integer ); 

//NOTE: the oscillator 'hums' in qemu.
begin  
//Ideally we would get CPU Speed and divide that.
wait:=wait*20; //approx. timings for 1.6Ghz CPU -maybe change timer division?
repeat
   sound(note);
   dec(wait);
until wait=0;


nosound; //ensure sound is OFF.
 
end;

procedure DONE;
var
  Divisor: LongInt;
begin
 //reset PIT2
 Divisor := 1193180 div 1000000; 

    // Prepare the 8253/8254 to receive the frequency data

  WritePortB($43, $36);
  WritePortB($42, Divisor); //HI
  WritePortB($42, Divisor shr 8); //LO
  
end;

Procedure America;


begin

Play (G4,halft); Play (G4,wholet); Play (E4,quartert); Play (E4,halft); Play (G4,halft); Play (G4,wholet);
Play (D4,quartert); Play (D4,halft);
writestrln('O Beautiful for spacious skies,');
delay(250);

Play (E4,halft); Play (F4,halft); Play (G4,halft); Play (A4,halft); Play (B4,halft); Play (G4,doubleWhole);
writestrln('For Amber waves of grain,');
delay(250);

Play (G4,halft); Play (G4,wholet); Play (E4,quartert); Play (E4,halft); Play (G4,halft); Play (G4,Wholet);
Play (D4,quartert); Play (D4,halft);
writestrln('For purple mountain majesties,');
delay(250);

Play (G4,halft); Play (E5,wholet); Play (E5,quartert); Play (D5,halft); 
writestrln('A-mer-i-ca!');
delay(250);

Play (C5,halft); Play (C5,wholet); Play (B4,quartert); Play (B4,halft); 
writestrln('A-mer-i-ca!');
delay(250);

Play (C5,halft); Play (D5,halft); Play (B4,halft); Play (A4,halft); Play (G4,halft); Play (C5,DoubleWhole);
writestrln('God shed His grace on thee,');
delay(250);


Play (C5,halft); Play (C5,wholet); Play (A4,quartert); Play (A4,halft); Play (C5,halft); Play (C5,Wholet);
Play (G4,quartert); Play (G4,halft);
writestrln('God shed His grace on thee,');
delay(250);

Play (G4,halft); Play (A4,wholet); Play (C5,quartert); Play (G4,halft); 
Play (D5,halft);  
writestrln('From sea to shin-ing');
delay(250);

Play(C5,doubleWhole);
Writestrln('sea. ');

end;


procedure LittleLamb;

begin
// might be half notes, not eigth. 'h' is not very descriptive.

     Play (E5,quarter); Play (D5,quarter); Play (C5,quarter);
     Play (D5,quarter); Play (E5,quarter); Play (E5,quarter);
     Play (E5,eigth);
     writestrln('Mary had a');
     delay(250);
     Play (D5,quarter); Play (D5,quarter); Play (D5,eigth);
     writestrln('Lit-tle Lamb,');
     delay(250);
     Play (E5,quarter); Play (G5,quarter); Play (G5,eigth);
     writestrln('Lit-tle Lamb,');

     delay(250);
     Play (E5,quarter); Play (D5,quarter); Play (C5,quarter);Play (D5,quarter);
     Play (E5,quarter); Play (E5,quarter); Play (E5,eigth);                
     writestrln('Mary had a Lit-tle Lamb');
     delay(250);
     Play (D5,quarter); Play (E5,quarter); Play (E5,quarter);
     Play (D5,quarter);
     writestrln('Whose fleece was white');
     delay(250);
     Play (C5,eigth);
     writestrln('as snow.');
     Play (C5,eigth);     
     delay(100);
end;


procedure TwinkleStar;
//Tribute to A/C(4)....
begin
//think the notes are OFF..but anyway, it works...

     Play (C5,quarter);  Play (C5,quarter); Play (E5,quarter);  Play (E5,quarter);
     Play (G5,quarter);  Play (G5,quarter); Play (E5,eigth);
     writestrln('Twinkle Twinkle Little Star...');
     Play (F5,quarter);  Play (F5,quarter);  Play (D5,quarter);  Play (D5,quarter);
     Play (B4,quarter); Play (B4,quarter); Play (G4,eigth);
     writestrln('How I wonder what you are...');
//I though there were 4 verses here....
end;

procedure LondonBridge;

begin
     Play (D5,quartert); Play (E5,whole);  Play (D5,quarter); Play (C5,quarter);
     Play (B4,quarter); Play (C5,quarter);  Play (D5,eigth);
     Play (A4,quarter); Play (B4,quarter); Play (C5,eigth);
     Play (B4,quarter); Play (C5,quarter);  Play (D5,quarter);
     Play (D5,quartert); Play (E5,whole);  Play (D5,quarter); Play (C5,quarter);
     Play (B4,quarter); Play (C5,quarter);  Play (D5,eigth);
     Play (A4,eigth); Play (D5,eigth);  Play (B4,quarter); Play (G4,quartert);
end;

procedure PopGoesWeasel;


begin
      Play (A4,whole);
      Play (D4,quarter);   Play (D4,whole);  Play (E4,quarter); Play (E4,whole);
      Play (FSharp4,quarter);  Play (FSharp4,whole); Play (D4,quarter); Play (A4,whole);
      Play (D4,quarter);   Play (D4,whole);  Play (E4,quarter); Play (E4,whole);
      Play (FSharp4,quartert); Play (D4,quartert);
      Play (D4,quartert);  Play (E4,quarter);  Play (E4,whole);
      Play (FSharp4,quarter);  Play (FSharp4,whole); Play (D4,quartert);
      Play (B5,quartert);   Play (E4,quarter);  Play (G4,whole);
      Play (FSharp4,quartert); Play (D4,quarter);

end;


procedure FarmerInDell;

begin
     Play (C4,whole);
     writestrln('OOO...');
     Play (F4,quarter);  Play (F4,whole); Play (F4,quarter); Play (F4,whole);
     Play (F4,sixteenth); Play (G4,whole);
     writestrln('The farmer"s in the dell...');
     Play (A4,quarter);  Play (A4,whole); Play (A4,quarter); Play (A4,whole);
     Play (A4,sixteenth); Delay (120);
     writestrln('The farmer"s in the dell...');
     Play (C5,quartert);  Play (C5,quarter);  Play (D5,whole);
     Play (C5,quarter);   Play (A4,whole); Play (F4,quarter); Play (G4,whole);
     Play (A4,quarter);  Play (A4,whole); Play (G4,quarter); Play (G4,whole);
     Play (F4,sixteenth);
     writestrln('Hi Ho The merry Oh, The farmer"s in the dell.');
end;

procedure ThisOldMan;

begin
{
THis old man, he played ten....something or other about giving a dog a bone.....

}
      Play (D5,quarter);  Play (B4,quarter); Play (D5,eigth);
      Play (D5,quarter);  Play (B4,quarter); Play (D5,eigth);
      Play (E5,quarter);  Play (D5,quarter);  Play (C5,quarter);  Play (B4,quarter);
      Play (A4,quarter); Play (B4,quarter); Play (C5,eigth);
      Play (D5,quarter);  Play (G4,quarter); Play (G4,whole); Play (G4,whole); Play (G4,quarter);
      Play (G4,whole); Play (A4,whole); Play (B4,whole); Play (C5,whole);  Play (D5,eigth);
      Play (D5,quarter);  Play (A4,quarter); Play (A4,quarter); Play (C5,quarter);
      Play (B4,quarter); Play (A4,quarter); Play (G4,quarter); Delay (220);

end;

procedure EeensySpider;

begin
      Play (F4,quarter);  Play (F4,whole);  Play (F4,quarter); Play (G4,whole);
      Play (A4,quartert); Play (A4,quarter);  Play (A4,whole);
      Play (G4,quarter);  Play (F4,whole);  Play (G4,quarter); Play (A4,whole);
      Play (F4,quartert); Delay (320);
      Play (A4,quartert); Play (A4,quarter);  Play (ASharp4,whole);
      Play (C5,quartert);  Play (C5,quartert);
      Play (ASharp4,quarter); Play (A4,whole);  Play (ASharp4,quarter); Play (C5,whole);
      Play (A4,quartert); Delay (320);
      Play (F4,quartert); Play (F4,quarter);  Play (G4,whole);
      Play (A4,quartert); Play (A4,quarter);  Play (A4,whole);
      Play (G4,quarter);  Play (F4,whole);  Play (G4,quarter);  Play (A4,whole);
      Play (F4,quartert); Play (C4,quarter);  Play (C4,whole);
      Play (F4,quarter);  Play (F4,whole);  Play (F4,quarter);  Play (G4,whole);
      Play (A4,quartert); Play (A4,quarter);  Play (A4,whole);
      Play (G4,quarter);  Play (F4,whole);  Play (G4,quarter);  Play (A4,whole);
      Play (F4,quartert); Delay (320);

end;

procedure InAndOut;

begin
        Play (G4,quarter);
        Play (D4,quarter);  Play (G4,quarter);  Play (B4,quartert);  Play (C5,whole);
        Play (B4,quarter);  Play (A4,half);  Play (A4,quarter);
        Play (D4,quarter);  Play (FSharp4,quarter); Play (A4,quartert);  Play (B4,whole);
        Play (A4,quarter);  Play (G4,half);  Play (G4,quarter);
        Play (D4,quarter);  Play (G4,quarter);  Play (B4,quartert);  Play (C5,whole);
        Play (B4,quarter);  Play (A4,half);  Play (A4,quarter);
        Play (D5,quarter);   Play (D5,quarter);   Play (C5,quarter);    Play (A4,quarter);
        Play (G4,sixteenth);
end;


procedure MulberryBush;

begin
      Play (F4,whole);  Play (F4,whole); Play (F4,whole); Play (F4,quarter); Play (A4,whole);
      Play (C5,whole);   Play (C5,whole);  Play (A4,whole); Play (F4,quarter); Play (F4,whole);
      Play (G4,whole);  Play (G4,whole); Play (G4,whole); Play (G4,quarter); Play (G4,whole);
      Play (E4,whole);  Play (E4,whole); Play (D4,whole); Play (C4,quartert);
      Play (F4,whole);  Play (F4,whole); Play (F4,whole); Play (F4,quarter); Play (A4,whole);
      Play (C5,whole);   Play (C5,whole);  Play (A4,whole); Play (F4,quarter); Play (F4,whole);
      Play (G4,quarter);  Play (G4,whole); Play (C4,whole); Play (D4,whole); Play (E4,whole);
      Play (F4,quartert); Play (F4,quartert);

end;

procedure StarWars; //yes, this complex.
//even ducktales was realtime on a 8088....well close enough.
//and weirdly, multi-task capable on an 8088...

//S? (32nd??)
begin
      Play (C5,eigth);  Play (G5,eigth);  Play (F5,SixtyFourth);   Play (E5,SixtyFourth);  Play (D5,SixtyFourth);
      Play (C6,eigth); Play (G5,eigth);  Play (F5,SixtyFourth);   Play (E5,SixtyFourth);  Play (D5,SixtyFourth);
      Play (C6,eigth); Play (G5,eigth);  Play (F5,SixtyFourth);   Play (E5,SixtyFourth);  Play (F5,SixtyFourth);
      Play (D5,eigth);  Play (G4,SixtyFourth); Play (G4,SixtyFourth);  Play (G4,SixtyFourth); Play (C5,eigth);
      Play (G5,eigth);  Play (F5,SixtyFourth);  Play (E5,SixtyFourth);   Play (D5,SixtyFourth);  Play (C6,eigth);
      Play (G5,eigth);  Play (F5,SixtyFourth);  Play (E5,SixtyFourth);   Play (D5,SixtyFourth);  Play (C6,eigth);
      Play (G5,eigth);  Play (ASharp5,SixtyFourth); Play (A5,SixtyFourth);   Play (ASharp5,SixtyFourth); Play (G5,eigth);
      Play (G5,eigth);  Play (G4,eigth); Play (G4,SixtyFourth);  Play (A4,sixteenth);Play (A4,whole);
      Play (F5,whole);  Play (E5,whole);  Play (D5,whole);   Play (C5,whole);  Play (C5,SixtyFourth);
      Play (D5,SixtyFourth);  Play (E5,SixtyFourth);  Play (D5,SixtyFourth);   Play (A4,whole); Play (B4,quarter);
      Play (G4,quartert);Play (G4,SixtyFourth); Play (A4,sixteenth); Play (A4,whole); Play (F5,whole);
      Play (E5,whole);  Play (D5,whole);  Play (C5,whole);   Play (G5,whole);  Play (D5,sixteenth);
      Play (G4,quartert);Play (G4,SixtyFourth); Play (A4,sixteenth); Play (A4,whole); Play (F5,whole);
      Play (E5,whole);  Play (D5,whole);  Play (C5,whole);   Play (C5,SixtyFourth);  Play (D5,SixtyFourth);
      Play (E5,SixtyFourth);  Play (D5,SixtyFourth);  Play (A4,whole);  Play (B4,quarter); Play (G5,quartert);
      Play (G5,SixtyFourth);  Play (C6,quartert);Play (ASharp5,SixtyFourth);  Play (GSharp5,quartert);Play (G5,SixtyFourth);
      Play (F5,quartert); Play (DSharp5,SixtyFourth); Play (D5,quartert);  Play (C5,quartert); Play (G5,eigth);
      Play (G5,quartert); Play (G5,SixtyFourth);  Play (G5,SixtyFourth);   Play (G5,SixtyFourth);  Play (G5,SixtyFourth);
      Play (C6,whole); Play (C5,SixtyFourth);  Play (C5,SixtyFourth);   Play (C5,SixtyFourth);  Play (C5,sixteenth);
end;


procedure HappyBirthday;
//(in 16th notes)

begin
play(C4,sixteenth); play(C4,sixteenth); play(D4,sixteenth); play(C4,sixteenth); play(F4,sixteenth); play(E4,sixteenth); 
play(C4,sixteenth); play(C4,sixteenth); play(D4,sixteenth); play(C4,sixteenth); play(G4,sixteenth); play(F4,sixteenth); 
play(C4,sixteenth); play(C4,sixteenth); play(C5,sixteenth); play(A4,sixteenth); play(G4,whole); play(F4,eigth); 
play(E4,eigth);
play(B4,sixteenth); play(B4,sixteenth); play(A4,sixteenth); play(F4,sixteenth); play(G4,sixteenth); play(F4,sixteenth);

end;

// PLEASE PLEASE tell me someone has 'old macdonald' lying around somewhere.......


procedure makesomenoise;

begin
//dont assume unless the B-day flag was set. Then TRIP on nearest boot after alarm tripped.. HE HE HE...
if IsBirthday then //cant trip on intterrupt, let the alarm function call us instead.
	HappyBirthDay
else
	playmenu;
end;


begin
        
end.

