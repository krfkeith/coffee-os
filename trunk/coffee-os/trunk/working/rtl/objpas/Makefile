include ../../make.rules

all: dateutils strutils types typinfo math

dateutils:
	$(FPC) $(FPCOPT) -Fi../coffee -Fi../i386 -Fi../inc  $(RTLOUTDIR) dateutils

strutils:
	$(FPC) $(FPCOPT) -Fi../coffee -Fi../i386 -Fi../inc $(RTLOUTDIR) strutils

types:
	$(FPC) $(FPCOPT) -Fi../coffee -Fi../i386 -Fi../inc $(RTLOUTDIR) types

typinfo:
	$(FPC) $(FPCOPT) -Fi../coffee -Fi../i386 -Fi../inc $(RTLOUTDIR) typinfo

math:
	$(FPC) $(FPCOPT) -Fi../coffee -Fi../i386 -Fi../inc $(RTLOUTDIR) math
