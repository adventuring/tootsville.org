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
# 
#
# Copyright © 2008-2017 Bruce-Robert Pocock; © 2018,2019 The Corporation
# for Inter-World Tourism and Adventuring (ciwta.org).
#
# This program is  Free Software: you can redistribute  it and/or modify
# it  under the  terms  of  the GNU  Affero  General  Public License  as
# published by  the Free  Software Foundation; either  version 3  of the
# License, or (at your option) any later version.
#
# This program  is distributed in the  hope that it will  be useful, but
# WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
# MERCHANTABILITY  or FITNESS  FOR  A PARTICULAR  PURPOSE.  See the  GNU
# Affero General Public License for more details.
#
# You  should have  received a  copy of  the GNU  Affero General  Public
# License     along    with     this     program.     If    not,     see
# <https://www.gnu.org/licenses/>.
#
# You can reach CIWTA at https://ciwta.org/, or write to us at:
#
# PO Box 23095
#
# Oakland Park, FL 33307-3095
#
# USA
#
# 

all:	bin doc

clean:
	-rm -rf Tootsville Tootsville.new \
		doc/* bin/buildapp
	-rm -rf $$HOME/.cache/common-lisp/*/$$(pwd)
	find . -name \*.fasl -exec rm {} \;
	find . -name dumper\*.lisp -exec rm {} \;
	find . -name \*~ -exec rm {} \;
	find . -name \*.orig -exec rm {} \;
	git gc --auto || true

modules:	../.gitmodules $(shell  grep '\[submodule "' ../.gitmodules | \
			cut -d '"' -f 2 | sed -e 's,^,../.git/modules/,g' -e 's,$$,/config,g')
	if [ -d ../.git ]; then cd .. ; git submodule update --init; fi

../.git/modules/%/config:	.gitmodules
	(cd .. ; git submodule update --init )

TAGS:	$(shell find . -name \*.lisp)
	ctags --languages=lisp -e -R -f TAGS

deploy:	bin test server-push doc-publish

VERSION=$(shell grep :version tootsville.asd | cut -d '"' -f 2)
doc-publish:	doc
	rsync -rv -essh doc/* goethet@goethe.tootsville.org:goethe.tootsville.org/devel/docs/Tootsville/$(VERSION)/

bin:	Tootsville

~/quicklisp/setup.lisp:	src/setup.lisp
	sbcl --non-interactive \
		--load src/setup.lisp \
		--quit

../.gitmodules:
	>> ../.gitmodules

bin/buildapp:
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
	./Tootsville.new check
	mv --backup=simple Tootsville.new Tootsville

Tootsville.new:	quicklisp-manifest.tmp bin/buildapp
	bin/buildapp --output ../Tootsville.new \
		--manifest-file quicklisp-manifest.tmp \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--entry Tootsville:entry

quicklisp-manifest.tmp:	tootsville.asd \
		$(shell find . -name \*.lisp \
			-and -not -path \**/.\* \
			-or -name \*.asd \
			-and -not -path \**/.\*)
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.tmp")'
	for asd in src/lib/*/*.asd ; \
	do \
		echo $$(pwd)/$$asd >> quicklisp-manifest.tmp ;\
	done
	echo $$(pwd) >> quicklisp-manifest.tmp

test:	bin
	./Tootsville check

doc:	doc/Tootsville.texi

doc/Tootsville.texi:	Tootsville
	./Tootsville write-docs

install:	tootsville.service Tootsville
	cp Tootsville --backup=simple -f /usr/local/bin/
	cp tootsville.service --backup=simple -f /usr/lib/systemd/system/ || \
		cat tootsville.service > /usr/lib/systemd/system/tootsville.service
	mkdir -p ~/.config/Tootsville/
	mv --backup=simple ~/Tootsville.config.lisp ~/.config/Tootsville/
	cp 55-tootsville.conf -f /etc/rsyslog.conf
	sudo -n systemctl enable tootsville || :
	sudo -n systemctl restart tootsville || :
	sudo -n systemctl start tootsville
