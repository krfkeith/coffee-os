
   HM_DOM = (document.getElementById) ? true : false;
   HM_NS4 = (document.layers) ? true : false;
    HM_IE = (document.all) ? true : false;
   HM_IE4 = HM_IE && !HM_DOM;
   HM_Mac = (navigator.appVersion.indexOf("Mac") != -1);
  HM_IE4M = HM_IE4 && HM_Mac;
HM_IsMenu = (HM_DOM || HM_NS4 || (HM_IE4 && !HM_IE4M));

HM_BrowserString = HM_NS4 ? "NS4" : HM_DOM ? "DOM" : "IE4";

if(window.event + "" == "undefined") event = null;
function HM_f_PopUp(){return false};
function HM_f_PopDown(){return false};
popUp = HM_f_PopUp;
popDown = HM_f_PopDown;

HM_PG_MenuWidth = 150;
HM_PG_FontFamily = "Verdana,Arial,sans-serif";
HM_PG_FontSize = 10;
HM_PG_FontBold = 1;
HM_PG_FontItalic = 0;
HM_PG_FontColor = "white";
HM_PG_FontColorOver = "blue";
HM_PG_BGColor = "BLUE";
HM_PG_BGColorOver = "WHITE";
HM_PG_ItemPadding = 0;

HM_PG_BorderWidth = 1;
HM_PG_BorderColor = "#5D6CA8";
HM_PG_BorderStyle = "solid";
HM_PG_SeparatorSize = 1;
HM_PG_SeparatorColor = "#5D6CA8";

HM_PG_ImageSrc = "/menu/HM_More_white_right.gif";
HM_PG_ImageSrcLeft = "/menu/HM_More_white_left.gif";

HM_PG_ImageSrcOver = "/menu/HM_More_blue_right.gif";
HM_PG_ImageSrcLeftOver = "/menu/HM_More_blue_left.gif";

HM_PG_ImageSize = 5;
HM_PG_ImageHorizSpace = 0;
HM_PG_ImageVertSpace = 2;

HM_PG_KeepHilite = true; 
HM_PG_ClickStart = 0;
HM_PG_ClickKill = false;
HM_PG_ChildOverlap = 20;
HM_PG_ChildOffset = 10;
HM_PG_ChildPerCentOver = null;
HM_PG_TopSecondsVisible = .5;
HM_PG_StatusDisplayBuild =0;
HM_PG_StatusDisplayLink = 0;
HM_PG_UponDisplay = null;
HM_PG_UponHide = null;
HM_PG_RightToLeft = false;

HM_PG_CreateTopOnly = 1;
HM_PG_ShowLinkCursor = 1;
HM_PG_NSFontOver = true;

<!-- Universal Serial Bus --> 
HM_Array1 = [
[300,
"mouse_x_position",
"mouse_y_position"],

["USB in a Nutshell - Introduction","/usbnutshell/usb1.htm",1,0,0],
["USB in a Nutshell - Hardware","/usbnutshell/usb2.htm",1,0,0],
["USB in a Nutshell - USB Protocols","/usbnutshell/usb3.htm",1,0,0],
["USB in a Nutshell - Endpoint Types","/usbnutshell/usb4.htm",1,0,0],
["USB in a Nutshell - USB Descriptors","/usbnutshell/usb5.htm",1,0,0],
["USB in a Nutshell - USB Requests","/usbnutshell/usb6.htm",1,0,0],
["USB in a Nutshell - Enumeration","/usbnutshell/usb7.htm",1,0,0],
["PIC16F87x and PDIUSBD11 USB Example","/usbnutshell/usb7.htm#PIC16F876Example",1,0,0],
["USB 1.1 Integrated Circuits","/usb/usbhard.htm",1,0,0],
["USB 2.0 Integrated Circuits","/usb/usbhard2.htm",1,0,0],
["USB Protocol Analysers","/usb/protocolanalysers.htm",1,0,0],
["USB Device Driver Development","/usb/usbdevdrvs.htm",1,0,0],
["USB with the simplicity of RS-232","/usb/ftdi.htm",1,0,0],
["On-The-Go Supplement - Point-to-Point Connectivity for USB.","/usb/otghost.htm",1,0,0],
["ISP1161 Host Controller for Embedded Systems","/usb/otghost.htm",1,0,0],
["PDIUSBD11 USB Interface","/usb/pdiusbd11.pdf",1,0,0],
["Win 2000/XP Driver for DeVaSys USBLPT-PD11 USB Boards","/usb/usblptpd11.htm",1,0,0],
["USB Driver for the Cypress USB Starter Kit","/usb/cypress.htm",1,0,0],
["Links","",1,0,1]
]

HM_Array1_19 = [
[],
["USB-IF Developers Discussion Forum","http://www.usb.org/phpbb/",1,0,0],
["Universal Serial Bus Home Page (www.usb.org)","http://www.usb.org",1,0,0],
["Universal Serial Bus Device Class Specs","http://www.usb.org/developers/devclass.html",1,0,0],
["USB Central / USB Complete (Jan Axelson)","http://www.lvr.com/usb.htm",1,0,0],
["USB by Example (John Hyde)","http://www.USB-By-Example.com",1,0,0],
["DeVaSys (USB I2CIO and USBLPT-PD11)","http://www.devasys.com",1,0,0],
["Universal Serial Bus Technology (Microsoft)","http://www.microsoft.com/hwdev/bus/USB/default.asp",1,0,0],
["POS USB Driver - Emulates COM Driver (Microsoft)","http://www.eu.microsoft.com/hwdev/usb/posusb.htm",1,0,0],
["USBSimm J.Gordon Electronic Design","http://www.jged.com/web_pages/usbsimm.html",1,0,0],
["EzUSB2131 Loader for Linux","http://ezusb2131.sourceforge.net",1,0,0],
["USBMan - The Webs #1 USB Help Source","http://www.usbman.com",1,0,0],
["USB Developer - Information for students and electronics enthusiasts","http://www.usbdeveloper.com",1,0,0],
["USB Snoopy","http://home.jps.net/~koma/",1,0,0],
["USB Snoopy Pro","http://sourceforge.net/projects/usbsnoop",1,0,0],
["UHCI: Universal Host Controller Interface Spec","http://developer.intel.com/design/USB/UHCI11D.htm",1,0,0],
["OHCI: Open Host Controller Interface Spec","http://www.compaq.com/productinfo/development/openhci.html",1,0,0],
["EHCI: Enhanced Host Controller Interface Spec","http://developer.intel.com/technology/usb/ehcispec.htm",1,0,0],
["USB On-The-Go Supplement (www.usb.org)","http://www.usb.org/developers/onthego/",1,0,0]
]

<!-- Embedded Internet --> 
HM_Array2 = [
[300,
"mouse_x_position",
"mouse_y_position"],
["Ethernet & TCP/IP Interfaces","/etherip/ip.htm",1,0,0],
["Embedded Linux, Setting up the Development Environment","/uClinux/uClinux.htm",1,0,0],
["Embedded Linux, Understanding the Build Tools","/uClinux/builduC.htm",1,0,0],
["Embedded Linux, Building gcc-2.95.3 m68k-elf for uClinux","/uClinux/gcc-2.95.3.pdf",1,0,0],
["Embedded Linux, BFLT Binary Flat Format","/uClinux/bflt.htm",1,0,0],
["Exploring the Netcomm NB5","/nb5",1,0,0],
["Links","",1,0,1]
]

HM_Array2_7 = [
[400],
["Ipsil - Home of the IPµ8930, IPµ8932 and IPµ8942","http://www.ipsil.com",1,0,0],
["WizNet Inc - w3100a","http://www.wiznet.co.kr/",1,0,0],
["Connect One - iChip & iChip Lan","http://www.connectone.com/",1,0,0],
["uClinux - A Version of Embedded Linux","http://www.uclinux.org",1,0,0],
["uClinux for the Xilinx Microblaze Soft Processor","http://www.itee.uq.edu.au/~jwilliams/mblaze-uclinux/",1,0,0],
["uCdot - Embedded Linux Developer Forum","http://www.ucdot.org/",1,0,0],
["The uClinux Directory","http://home.at/uclinux/",1,0,0],
["Axis Embedded Linux (eLinux) & ETRAX Processors","http://www.developer.axis.com/",1,0,0],
["openhardware.net (Tom Walsh)","http://www.openhardware.net/",1,0,0],
["PicoWeb - A $25 Web Server using an ATMEL Microcontroller","http://www.picoweb.net/",1,0,0],
["Embedded Ethernet - Crystal Lan CS8900 Controller","http://www.embeddedethernet.com/",1,0,0],
["CS8900A CrystalLAN Ethernet Controller (Cirrus Logic)","http://www.cirrus.com/products/overviews/cs8900a.html",1,0,0],
["Netburner - Embeded Ethernet Controllers","http://www.netburner.com/",1,0,0],
["S-7600A TCP/IP Network Protocol Stack LSI","http://www.seiko-usa-ecd.com/intcir/html/assp/s7600a.html",1,0,0],
["Small Web Server PIC16F84 S-7600A TCP/IP Hardware Stack","http://www.mycal.net/wsweb/",1,0,0],
["iReady.org developers' website","http://www.iready.org/",1,0,0],
["LART StrongARM running Linux","http://www.lart.tudelft.nl/",1,0,0]
]

<!-- Legacy Ports --> 
HM_Array3 = [
[300,
"mouse_x_position",
"mouse_y_position"],
["Standard Parallel Port (SPP)","/spp/parallel.htm",1,0,0],
["Enhanced Parallel Port (EPP)","/epp/epp.htm",1,0,0],
["Extended Capabilities Parallel Port (ECP)","/ecp/ecp.htm",1,0,0],
["Parallel Port Debug Tool","/pardebug/pdebug.htm",1,0,0],
["Parallel Port LCD Interface Example","/parlcd/parlcd.htm",1,0,0],
["RS-232 Hardware & Software Registers","/serial/serial.htm",1,0,0],
["RS-232 Low Level Programming & External Hardware","/serial/serial1.htm",1,0,0],
["Using Interrupts (PC)","/interrupts/interupt.htm",1,0,0],
["Interfacing Example - Analog Sampling Via the RS-232 Port","/serial/serial2.htm",1,0,0],
["Interfacing Example - Connecting a LCD Module to the RS-232 Port","/serial/serial3.htm",1,0,0],
["Quick and Simple RS-232 Terminal","/terminal/terminal.htm",1,0,0],
["RS-232 Protocol Analyser","/protocolanalyser/protocolanalyser.htm",1,0,0],
["Links","",1,0,1]
]

HM_Array3_13 = [
[],
["Portmon for Windows NT/9x (System Internals)","http://www.sysinternals.com/ntw2k/freeware/portmon.shtml",1,0,0],
["Parallel Port Central (Jan Axelson)","http://www.lvr.com/parport.htm",1,0,0],
["PC Parallel Port Mini-FAQ (Kris Heidenstrom)","http://home.clear.net.nz/pages/kheidens/ppmfaq/khppmfaq.htm",1,0,0],
["LPTCAP Parallel Print Data Capture System (Kris Heidenstrom)","http://home.clear.net.nz/pages/kheidens/lptcap/lptcap.htm",1,0,0],
["Serial Port Central / Serial Port Complete (Jan Axelson)","http://www.lvr.com/serport.htm",1,0,0],
["Serial Communications in Win32 (Microsoft)","http://msdn.microsoft.com/library/techart/msdn_serial.htm",1,0,0],
["Windows Serial API And Devices with C++ Builder","http://www.temporaldoorway.com/programming/cbuilder/windowsapi/index.htm",1,0,0],
["Sample Windows Terminal Program with Source for Borland C++ Builder","http://www.traverse.net/people/poinsett/bcbcomm.html",1,0,0]
]

<!-- Device Drivers --> 
HM_Array4 = [
[300,
"mouse_x_position",
"mouse_y_position"],
["PortTalk - A Windows NT I/O Port Device Driver","/porttalk/porttalk.htm",1,0,0],
["Universal Serial Bus Device Driver Development ","/usb/usbdevdrvs.htm",1,0,0],
["Interrupts and Deferred Procedure Calls on Windows NT4/2000/XP ","/interrupts/winnt_isr_dpc.htm",1,0,0],

["USB Driver for the Cypress USB Starter Kit","/usb/cypress.htm",1,0,0],
["Device Driver Fiddler (tools)","/dddtools/dddtools.htm",1,0,0],
["Windows NT Device Driver Installer (tools)","/dddtools/dddtools.htm",1,0,0],
["Device Driver Remover Win9x (tools)","/dddtools/dddtools.htm",1,0,0],
["Links","",1,0,1]
]

HM_Array4_8 = [
[],
["System Internals","http://www.sysinternals.com",1,0,0],
["Jungo WinDriver","http://www.jungo.com/windriver.html",1,0,0],
["Jungo KernelDriver","http://www.jungo.com/kerneldriver.html",1,0,0],
["Microsoft Windows Driver Development Kits","http://www.microsoft.com/ddk/",1,0,0]
]

<!-- Misc --> 
HM_Array5 = [
[300,
"mouse_x_position",
"mouse_y_position"],
["CMOS Digital Image Sensors and Lenses","/imaging/camera.htm",1,0,0],
["Generate Ring Tones on your PIC16F87x Microcontroller","/pic/ringtones.htm",1,0,0],
["Interfacing the AT Keyboard","/keyboard/keybrd.htm",1,0,0],
["How does the Microchip ICD Work?","/pic/icd.htm",1,0,0],
["Trust-No-Exe","/consulting/trust-no-exe/trust-no-exe.htm",1,0,0],
["Command Line Process Viewer/Killer/Suspender for Windows NT/2000/XP","/consulting/processutil/processutil.htm",1,0,0],
["BeyondExec - Spawn Processes on Remote Windows NT/2000/XP WorkStations","/consulting/remoteprocess/BeyondExec.htm",1,0,0],
["Beyond Logic Shutdown Utility for NT/2000/XP","/consulting/shutdown/shutdown.htm",1,0,0],
["Bmail - Command Line SMTP Mailer for Batch Jobs","/consulting/cmdlinemail/cmdlinemail.htm",1,0,0],
["Delete/Copy by Owner utility for Windows NT/2000/XP","/consulting/delbyowner/delbyowner.htm",1,0,0],
["Win32 Pipe Security Editor Windows NT/2000/XP","/consulting/pipesec/pipesec.htm",1,0,0],
["Console Computer Information Utility for 2000/XP","/consulting/compinfo/compinfo.htm",1,0,0],
["SMART & Simple for NT/2000/XP","/consulting/smart/smart.htm",1,0,0],
["Kodak DC215 Support and FAQ","/KodakDC215/dc215.htm",1,0,0],
["South Australian Electricity Generation","/southaustraliapower",1,0,0],
["Links","",1,0,1]
]

HM_Array5_16 = [
[],
["Altium - Making Electronics Design Easier","http://www.altium.com",1,0,0],
["Circuit Cellar (Magazine)","http://www.circuitcellar.com",1,0,0],
["Epanorama.Net - Electronic's Links","http://www.epanorama.net",1,0,0],
["Electronics Index","http://www.weisd.com/store/links/themeindex.html",1,0,0],
["IrDA Specifications (www.irda.org)","http://www.irda.org/standards/specifications.asp",1,0,0],
["PCI Special Interest Group (www.pcisig.com)","http://www.pcisig.com",1,0,0],
["Pic Micro Web Board (Hosted by Microchip)","http://www.microchip.com/webboard/wbpx.dll/~picmicro",1,0,0],
["Pic DevTools Web Board (Hosted by Microchip)","http://www.microchip.com/webboard/wbpx.dll/~devtools",1,0,0],
["PICuWEB - Program PIC micros with C","http://www.microchipc.com",1,0,0],
["Warp-13 MPLAB Compatible PIC Programmer","http://www.newfoundelectronics.com/",1,0,0],
["Embedtronics","http://www.embedtronics.com",1,0,0],
["AVR Freaks","http://www.avrfreaks.net",1,0,0],
["Embedded Design with the PIC18F452 Microcontroller","http://www.picbook.com",1,0,0]
]

if(HM_IsMenu) { document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/menu/HM_Script"+ HM_BrowserString +".js' TYPE='text/javascript'><\/SCR" + "IPT>"); }

<!-- This script and many more are available free online at -->
<!-- The JavaScript Source!! http://javascript.internet.com -->
<!-- Begin
d = new Array("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday");
m = new Array("January","February","March","April","May","June","July","August","September","October","November","December");
today = new Date();
day = today.getDate();
year = today.getYear();
if (year < 2000)    // Y2K Fix, Isaac Powell
year = year + 1900; // http://onyx.idbsu.edu/~ipowell
end = "th";
if (day==1 || day==21 || day==31) end="st";
if (day==2 || day==22) end="nd";
if (day==3 || day==23) end="rd";
day+=end;
document.write("<FONT SIZE=-1>"+d[today.getDay()]+", "+m[today.getMonth()]+" ");
document.write(day+", " + year);
document.write("</FONT><BR></TD></TR></TABLE></CENTER>");

document.write("<table width=100% border=0 cellspacing=0 cellpadding=1 frame=Below><tr><td align=center BGCOLOR=BLUE nowrap>");
document.write("<FONT FACE=ARIAL COLOR=WHITE><B><CENTER><A ID='TITLEBLOCK' HREF='/index.htm#USB' onMouseOver=HM_f_PopUp('elMenu1',event) onMouseOut=HM_f_PopDown('elMenu1')>Universal Serial Bus</a>&nbsp;<img src='/menu/HM_More_white_down.gif'></td><td align='center' BGCOLOR=BLUE nowrap>");
document.write("<FONT FACE=ARIAL COLOR=WHITE><B><CENTER><A ID='TITLEBLOCK' HREF='/index.htm#uClinux' onMouseOver=HM_f_PopUp('elMenu2',event) onMouseOut=HM_f_PopDown('elMenu2')>Embedded Internet</a>&nbsp;<img src='/menu/HM_More_white_down.gif'></td><td align='center' BGCOLOR=BLUE nowrap>");
document.write("<FONT FACE=ARIAL COLOR=WHITE><B><CENTER><A ID='TITLEBLOCK' HREF='/index.htm#Legacy' onMouseOver=HM_f_PopUp('elMenu3',event) onMouseOut=HM_f_PopDown('elMenu3')>Legacy Ports</a>&nbsp;<img src='/menu/HM_More_white_down.gif'></td><td align='center' BGCOLOR=BLUE nowrap>");
document.write("<FONT FACE=ARIAL COLOR=WHITE><B><CENTER><A ID='TITLEBLOCK' HREF='/index.htm#DeviceDrivers' onMouseOver=HM_f_PopUp('elMenu4',event) onMouseOut=HM_f_PopDown('elMenu4')>Device Drivers</a>&nbsp;<img src='/menu/HM_More_white_down.gif'></td><td align='center' BGCOLOR=BLUE nowrap>");
document.write("<FONT FACE=ARIAL COLOR=WHITE><B><CENTER><A ID='TITLEBLOCK' HREF='/index.htm#Misc' onMouseOver=HM_f_PopUp('elMenu5',event) onMouseOut=HM_f_PopDown('elMenu5')>Miscellaneous</a>&nbsp;<img src='/menu/HM_More_white_down.gif'></td>");
document.write("</tr></TABLE><BR>");