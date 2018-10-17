all: htaccess play worker servers TODO.org TODO.scorecard

####################

clean:
	$(MAKE) -C servers clean
	find . -name \*~ -exec rm {} \;
	-rm dist/play/play.js
	-rm dist/play/worker.js
	-rm TODO.org TODO.scorecard	

servers:	servers/Tootsville

servers/Tootsville:	$(shell find servers \( -name \*.lisp -o -name \*.asd \) -and -not -name .\*)
	$(MAKE) -C servers Tootsville

doc:
	$(MAKE) -C servers doc

####################

htaccess:	htaccess.base bin/make-all-htaccess
	bin/make-all-htaccess

####################

worker:	dist/play/worker.js

dist/play/worker.js:	play/worker.js
	mkdir -p dist/play/
	closure-compiler --create_source_map dist/play/worker.map \
		--language_out ECMASCRIPT5_STRICT \
		--source_map_location_mapping 'play/|/play/' \
		$< > $@
	echo '//# sourceMappingURL=/play/worker.map' >> dist/play/worker.js

####################

play:	dist/play/play.css \
	dist/play/play.js

PLAYJS = $(shell ./bin/find-play-js)

dist/play/play.map:	dist/play/play.js

dist/play/play.css:	$(shell find play -name \*.less -and -not -name .\*)
	mkdir -p dist/play/
	lessc --strict-math=on --source-map play/play.less dist/play/play.css

dist/play/play.js:	${PLAYJS}
	mkdir -p dist/play/
	cat ${PLAYJS} > dist/play/play.max.js
	closure-compiler --create_source_map dist/play/play.map \
		--source_map_location_mapping 'play/|/play/' \
		--language_out ECMASCRIPT5_STRICT \
		dist/play/play.max.js > $@
	echo '//# sourceMappingURL=/play/play.map' >> dist/play/play.js

####################

TODO.org:	$(shell find */ -name \\*.lisp -o -name \\*.css -o -name \\*.js -o -name \\*.org -o \
		   -name \\*.texi -o -name \\*.asd -o -name \\*.txt -o -name \\*.html) \
		 README.org
	-mv TODO.org TODO.org~ 2>/dev/null
	echo '* TODO-type notes found $$(date +%Y-%m-%d)' > TODO.org
	echo '' >> TODO.org
	echo '** FIXME Actual bugs!' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn FIXME: */ README.org | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** TODO To be done ASAP' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn TODO: */ README.org | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** XXX Might Be Nice to do someday' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn XXX: */ README.org | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** ☠☠☠ Bruce-Robert should examine this' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn ☠☠☠: */ README.org | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org

TODO.scorecard:	$(shell find */ -type f) README.org
	echo -n 'TOOTS_FIXME=' > TODO.scorecard
	git grep FIXME: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_TODO=' >> TODO.scorecard
	git grep TODO: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_XXX=' >> TODO.scorecard
	git grep XXX: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_BRP=' >> TODO.scorecard
	git grep ☠☠☠: */ README.org | wc -l >> TODO.scorecard

####################

jscl:	bin/jscl

bin/jscl: $(shell find jscl \( -name \**.lisp -or -name \**.js -or -name \**.asd \)  -and -not -name .\*)
	cd jscl; ./make.sh
