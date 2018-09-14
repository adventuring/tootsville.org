# Makefile for Tootsville
#
# Targets:
#
# deps — install prerequisites needed from the build-deps file
#
# bin — compile the deployment files (CGI script, compiled JS/CSS, &c)
#
# doc — extract and build documentation in various formats
#
# doc-publish — push docs to Goethe
#
# test — run automated unit tests
#
# ui-test — run Selenium-based unit tests
#
# deploy — send code to production server, compile and test
#

all:	bin doc

clean:
	-rm -f Tootsville Tootsville.new \
		doc/* bin/buildapp
	find . -name \*.fasl -exec rm {} \;
	find . -name dumper\*.lisp -exec rm {} \;
	find . -name \*~ -exec rm {} \;
	git gc --auto

deps:	.deps-installed~ modules

modules:	../.gitmodules $(shell  grep '\[submodule "' ../.gitmodules | \
			cut -d '"' -f 2 | sed -e 's,^,../.git/modules/,g' -e 's,$$,/config,g')
	if [ -d ../.git ]; then cd .. ; git submodule update --init; fi

../.git/modules/%/config:	.gitmodules
	(cd .. ; git submodule update --init )

TAGS:	$(shell find . -name \*.lisp)
	ctags --languages=lisp -e -R -f TAGS

.deps-installed~:	build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.deps-installed~

deploy:	bin test server-push doc-publish

VERSION=$(shell grep :version tootsville.asd | cut -d '"' -f 2)
doc-publish:	doc
	rsync -rv -essh doc/* goethe.tootsville.org:goethe.tootsville.adventuring.click/devel/docs/tootsville/$(VERSION)/

bin:	Tootsville

~/quicklisp/setup.lisp:	src/setup.lisp
	sbcl --non-interactive \
		--load src/setup.lisp \
		--quit

../.gitmodules:
	>> ../.gitmodules

bin/buildapp:	deps
	if which buildapp; \
	then \
		ln -s $$(which buildapp) -f bin/buildapp; \
	else \
		sbcl --non-interactive \
			--load ~/quicklisp/setup.lisp \
			--eval '(ql:quickload :buildapp)' \
			--eval '(buildapp:build-buildapp "bin/buildapp")'; \
	fi

Tootsville:	Tootsville.new
	sleep 1
	./Tootsville.new check
	mv --backup=simple Tootsville.new Tootsville

Tootsville.new:	quicklisp-manifest.tmp bin/buildapp
	bin/buildapp --output Tootsville.new \
		--manifest-file quicklisp-manifest.tmp \
		--load "src/setup.lisp" \
		--load tootsville.asd \
		--load-system tootsville \
		--entry tootsville:entry

quicklisp-manifest.tmp:	tootsville.asd \
		$(shell find . -name \*.lisp \
			-and -not -path \**/.\* \
			-or -name \*.asd \
			-and -not -path \**/.\*)
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load src/setup.lisp \
		--load tootsville.asd \
		--eval '(ql:quickload :tootsville)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.tmp")'

test:	bin
	./Tootsville check

### Docs

doc/tootsville.html.tar.gz:	doc/tootsville.html.tar
	gzip -9 -c < $< > $@

doc/tootsville.html.tar.Z:	doc/tootsville.html.tar
	compress -9 -c < $< > $@

doc/tootsville.html.tar.bz2:	doc/tootsville.html.tar
	bzip2 -9 -c < $< > $@

doc/tootsville.html.tar.xz:	doc/tootsville.html.tar
	xz -9 -c < $< > $@

doc/tootsville.html.tar:	doc/tootsville.html.d/index.html
	cd doc; tar cf tootsville.html.tar tootsville.html.d

doc/tootsville.html.zip:	doc/tootsville.html.d/index.html
	cd doc; zip -9 tootsville.html.zip tootsville.html.d

doc/tootsville.html.d/index.html:	doc/tootsville.texi
	cd doc; makeinfo -o tootsville.html.d/ \
		--html --css-include=../src/static/css/doc.css \
		--split=node tootsville.texi

doc/tootsville.ps:	doc/tootsville.texi
	cd doc; makeinfo --ps -o tootsville.ps tootsville.texi

doc/tootsville.pdf:	doc/tootsville.texi
	cd doc; makeinfo --pdf -o tootsville.pdf tootsville.texi

doc/tootsville.txt:	doc/tootsville.texi
	cd doc; makeinfo --plaintext -o tootsville.txt tootsville.texi

doc/tootsville.info:	doc/tootsville.texi
	cd doc; makeinfo -o tootsville.info tootsville.texi

doc:	doc/tootsville.txt \
	doc/tootsville.pdf \
	doc/tootsville.html.tar.gz

all-docs: \
	doc/tootsville.html.tar.gz	\
	doc/tootsville.html.tar.Z	\
	doc/tootsville.html.tar.bz2	\
	doc/tootsville.html.tar.xz	\
	doc/tootsville.html.zip	\
	doc/tootsville.ps	\
	doc/tootsville.pdf	\
	doc/tootsville.txt 	\
	doc/tootsville.info

doc/tootsville.texi:	Tootsville
	./Tootsville write-docs

install:	tootsville.service Tootsville
	cp Tootsville /usr/local/bin/
	cp tootsville.service /usr/lib/systemd/user/
