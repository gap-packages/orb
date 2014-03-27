ACLOCAL_AMFLAGS = -I m4

SUBDIRS = 

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(abs_top_srcdir)/$(BINARCHDIR)/orb.so

lib_LTLIBRARIES = orb.la

orb_la_SOURCES = src/orb.c
orb_la_CPPFLAGS = $(GAP_CPPFLAGS) -DCONFIG_H
# Note that the latter is only for GAP 4.4.12
orb_la_LDFLAGS = -module -avoid-version
if SYS_IS_CYGWIN
orb_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif

all-local: orb.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
if SYS_IS_CYGWIN
	cp .libs/orb.dll $(GAPINSTALLLIB)
else
	cp .libs/orb.so $(GAPINSTALLLIB)
endif
	@echo "SUCCESS!"

distclean-local:
	rm -rf bin/*
	(cd doc ; ./clean)

doc:	doc/manual.six

doc/manual.six: doc/basic.xml doc/hash.xml doc/orb.xml doc/search.xml \
		doc/bysuborbit.xml doc/install.xml doc/quotfinder.xml \
		doc/examples.xml doc/intro.xml doc/random.xml PackageInfo.g
	($(GAPROOT)/bin/gap.sh -A makedoc.g)
