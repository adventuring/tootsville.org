all: play worker servers

####################

clean:
	$(MAKE) -C servers clean
	find . -name \*~ -exec rm {} \;
	rm play/play.js
	rm login/login.js

servers:	servers/Tootsville

servers/Tootsville:	$(shell find servers -name \*.lisp -o -name \*.asd -and -not -path \**/.\*)
	$(MAKE) -C servers Tootsville

doc:
	$(MAKE) -C servers doc

####################

worker:	dist/worker.js

dist/worker.js:	play/worker.js
	mkdir -p dist/
	closure-compiler --create_source_map dist/worker.map \
		--language_out ECMASCRIPT5_STRICT \
		--source_map_location_mapping 'play/|/play/' \
		$< > $@
	echo '//# sourceMappingURL=/play/worker.map' >> dist/worker.js

####################

play:	dist/play.css \
	dist/play.js

PLAYJS = $(shell ./bin/find-play-js)

dist/play.map:	dist/play.js

dist/play.css:	$(shell find play -name \*.less -and -not -name .\*)
	mkdir -p dist/
	lessc --strict-math=on --include-path=include --source-map play/play.less dist/play.css

dist/play.js:	${PLAYJS}
	mkdir -p dist/
	closure-compiler --create_source_map dist/play.map \
		--source_map_location_mapping 'play/|/play/' \
		--language_out ECMASCRIPT5_STRICT \
		$< > $@
	echo '//# sourceMappingURL=/play/play.map' >> dist/play.js

