include ../../make.rules

all: system objpas rtlconst sysconst x86 si_prc sysutils cmem fpintres fpoutres

system:
	$(FPC) $(FPCOPT) -Us -Fi../i386 -Fi../inc $(RTLOUTDIR) system 

objpas:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi../inc $(RTLOUTDIR) ../objpas/objpas

rtlconst:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi../inc $(RTLOUTDIR) ../objpas/rtlconst

sysconst:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi../inc $(RTLOUTDIR) ../objpas/sysconst

x86:
	$(FPC) $(FPCOPT) $(RTLOUTDIR) x86

si_prc:
	$(FPC) $(FPCOPT) $(RTLOUTDIR) si_prc

sysutils:
	$(FPC) $(FPCOPT) -Fi../objpas/sysutils -Fi../i386 -Fi../inc $(RTLOUTDIR) sysutils


cmem:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi./inc $(RTLOUTDIR) cmem

fpintres:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi./inc $(RTLOUTDIR) fpintres

fpoutres:
	$(FPC) $(FPCOPT) -Fi../i386 -Fi./inc $(RTLOUTDIR) fpextres

