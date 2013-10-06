all:
	buildtv

buildtv:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi../inc $(RTLOUTDIR) buildtv.pas
#build TVision units....note there is no sysutils requirement with dos, so see what
#differs between this and FPC's version. Update the sources.
# NOW, this is grey area for FPC..cannot release modified BORLAND code in source format.
# It is part of the RTL sources(which I WILL provide) with the binary units that I use.
# since Im pulling off of FPC, it is THIER code (and name) that gets blundered.
#Im not risking a lawsuit. 
# GVision code builds on this, and we have 3D graph units now.
# --Jazz

clean:
	rm -f *.ppu
	rm -f *.o
	rm -f *.a
	rm -f *.rst
