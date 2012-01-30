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

all-local: orb.la install-libLTLIBRARIES
#	@$(MAKE) $(AM_MAKEFLAGS) install-libLTLIBRARIES
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
if SYS_IS_CYGWIN
	cp $(libdir)/orb.dll $(GAPINSTALLLIB)
else
	cp $(libdir)/orb.so $(GAPINSTALLLIB)
endif
	@echo "SUCCESS!"

distclean-local:
	rm -rf bin/*
	(cd doc ; ./clean)

doc:	doc/manual.six

doc/manual.six: doc/basic.xml doc/hash.xml doc/orb.xml doc/search.xml \
		doc/bysuborbit.xml doc/install.xml doc/quotfinder.xml \
		doc/examples.xml doc/intro.xml doc/random.xml VERSION
	($(GAPROOT)/bin/gap.sh makedoc.g)

archive: doc
	(cd .. ; tar czvf orb-`cat orb/VERSION`.tar.gz --exclude ".svn" --exclude bin --exclude test --exclude ".git" --exclude ".libs" --exclude lib --exclude orb.la --exclude orb_la-orb.lo --exclude .deps --exclude autom4te.cache --exclude .gitignore --exclude config.log --exclude stamp-h1 --exclude pkgconfig.h --exclude libtool --exclude config.status orb)

WEBPOS=/scratch/neunhoef/mywebpage.pub/Computer/Software/Gap/orb

towww: archive
	echo '<?xml version="1.0" encoding="ISO-8859-1"?>' >${WEBPOS}.version
	echo '<mixer>' >>${WEBPOS}.version
	cat VERSION >>${WEBPOS}.version
	echo '</mixer>' >>${WEBPOS}.version
	cp PackageInfo.g ${WEBPOS}
	cp README ${WEBPOS}/README.orb
	cp doc/manual.pdf ${WEBPOS}/orb.pdf
	cp ../orb-`cat VERSION`.tar.gz ${WEBPOS}
	@echo
	@echo Now please adjust the archive link in 
	@echo "    " ${WEBPOS}.mixer !

