ACLOCAL_AMFLAGS = -I m4

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(BINARCHDIR)/orb.so

lib_LTLIBRARIES = orb.la

orb_la_SOURCES = src/orb.c
orb_la_CPPFLAGS = $(GAP_CPPFLAGS)
orb_la_CFLAGS = $(GAP_CFLAGS)
orb_la_LDFLAGS = $(GAP_LDFLAGS) -module -avoid-version
if SYS_IS_CYGWIN
orb_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif

all-local: $(GAPINSTALLLIB)

$(GAPINSTALLLIB): orb.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
if SYS_IS_CYGWIN
	cp .libs/orb.dll $(GAPINSTALLLIB)
else
	cp .libs/orb.so $(GAPINSTALLLIB)
endif

clean-local:
	rm -f $(GAPINSTALLLIB)

distclean-local:
	rm -rf bin/*
	(cd doc ; ./clean)

doc:
	(echo 'Read("makedoc.g");' | $(GAPROOT)/bin/gap.sh -A -q)

.PHONY: doc
