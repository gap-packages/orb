doc: doc/manual.six

doc/manual.six: doc/basic.xml doc/hash.xml doc/orb.xml doc/search.xml \
		doc/bysuborbit.xml doc/install.xml doc/quotfinder.xml \
		doc/examples.xml doc/intro.xml doc/random.xml VERSION
	../../bin/gap.sh makedoc.g

clean:
	(cd doc ; ./clean)

archive: doc
	(cd .. ; tar czvf orb-`cat orb/VERSION`.tar.gz --exclude ".svn" --exclude test orb)

WEBPOS=/home/antares/neunhoef/mywebpage/Computer/Software/Gap/orb

towww: archive
	echo '<?xml version="1.0" encoding="ISO-8859-1"?>' >${WEBPOS}.version
	echo '<mixer>' >>${WEBPOS}.version
	cat VERSION >>${WEBPOS}.version
	echo '</mixer>' >>${WEBPOS}.version
	cp PackageInfo.g ${WEBPOS}
	cp README ${WEBPOS}/README.orb
	cp doc/manual.pdf ${WEBPOS}/orb.pdf
	cp ../orb-`cat VERSION`.tar.gz ${WEBPOS}

