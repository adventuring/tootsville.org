# This is the Makefile for Tootsville
#
# Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 CIWTA; © 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA.
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
# You can reach Interworldly Adventuring, LLC at https://interworldly.com/, or write to us at:
#
# Interworldly Adventuring, LLC
# Portland, OR, USA

all: .ready-20250822 htaccess play worker TODO.org TODO.scorecard docs mobile organize-artifacts

test: all test-lisp test-vue

# Run Common Lisp tests with FiveAM
test-lisp:
	@echo "🧪 Running Common Lisp tests with FiveAM..."
	cd lib/tootsville.net && sbcl --load test/run-tests.lisp

# Run Vue.js tests with Vitest
test-vue:
	@echo "🧪 Running Vue.js tests with Vitest..."
	cd play/vue/tootsville-vue && pnpm test:unit --coverage --run

# Show available targets
help:
	@echo "🎮 Tootsville Development Commands"
	@echo ""
	@echo "🚀 Quick Start:"
	@echo "  make run          - Start all development servers (backend + frontend)"
	@echo "  make devel-serve  - Start backend server only"
	@echo "  make devel-play   - Start play server only"
	@echo "  make devel-www    - Start www server only"
	@echo ""
	@echo "🔧 Development:"
	@echo "  make devel-test   - Start development servers in separate terminals"
	@echo "  make devel-play-watch - Start play server with file watching"
	@echo "  make devel-www-watch  - Start www server with file watching"
	@echo ""
	@echo "🏗️ Build:"
	@echo "  make all          - Build everything"
	@echo "  make clean        - Clean build artifacts"
	@echo "  make test         - Run all tests (Lisp + Vue)"
	@echo "  make test-lisp    - Run Common Lisp tests with FiveAM"
	@echo "  make test-vue     - Run Vue.js tests with Vitest"
	@echo ""
	@echo "📦 Packaging:"
	@echo "  make rpm          - Build RPM package"
	@echo "  make docker-test  - Test RPM in Docker container"
	@echo ""
	@echo "🚀 Production:"
	@echo "  make deploy       - Deploy to production"

deploy: all deploy-www deploy-play git-tag-deployment deploy-docs

# Build RPM package
rpm: all
	@echo "📦 Building RPM package..."
	rpmbuild -ba Tootsville.spec

# Test RPM installation in Docker container
test: rpm
	@echo "🧪 Testing Tootsville RPM installation in Docker..."
	docker build -f Dockerfile.fedora42-test -t tootsville-test .
	docker run --rm -it --privileged -v /sys/fs/cgroup:/sys/fs/cgroup:rw -v $(PWD)/RPMS:/tmp tootsville-test /home/pil/test-install.sh

# Live test with server running
live-test: rpm
	@echo "🧪 Live testing Tootsville server in Docker..."
	docker build -f Dockerfile.fedora42-test -t tootsville-live-test .
	docker run --rm -it --privileged -v /sys/fs/cgroup:/sys/fs/cgroup:rw -v $(PWD)/RPMS:/tmp -p 5000:5000 -p 5004:5004 -p 80:80 -p 443:443 tootsville-live-test /home/pil/test-server.sh

####################

.ready-20250822:	build/build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.ready-20250822

####################

JSC=java -jar bin/closure-compiler-v20210202.jar

BUILD=$(shell date +%Y%m%d%H%M%S)
VERSION=$(shell cat build/version)

SED=$(shell which gsed || echo sed)
LESSC=$(shell which lessc || echo ~/.nvm/versions/node/*/lib/node_modules/lessc/node_modules/less/bin/lessc)

#################### vars

# To target alternate clusters:
#	make CLUSTER=qa
#	make CLUSTER=.
CLUSTER:=test
ifeq ($(CLUSTER),.)
clusterorg=tootsville.org
else
clusterorg=$(CLUSTER).tootsville.org
endif

LOCAL_USERNAME=$(shell whoami)
REVISION=$(shell git log -n 1 --pretty=format:"%H")
REALNAME:=$(shell if which finger &>/dev/null ;\
	then \
	    REALNAME=$$(finger $$LOCAL_USERNAME | perl -ne 'if (/Name: (.*)[\t\n]/){print $$1; exit}')  ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(grep ^$$LOCAL_USERNAME: /etc/passwd | cut -d: -f5 | cut -d, -f1) ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(whoami) ;\
	fi ;\
	echo $$REALNAME)

# Rollbar
ACCESS_TOKEN=7c28543f4257495694b50fe59acb2ada

#################### clean

clean:
	find . -name \*~ -exec rm {} \;
	rm -rf dist/ ; mkdir -p dist/
	rm -rf doc/ ; mkdir -p doc/
	rm -f TODO.org TODO.scorecard	

#################### doc

doc:	js-doc

docs:	doc

doc/doc.css:	www/doc.less

js-doc:	dist/doc.texi

doc/texi/tootsville-js.texi:	doc/texi/TootsvilleJS.texi
	perl -ne 'print if /@c END_PREAMBLE/..0' \
		< doc/texi/TootsvilleJS.texi > doc/texi/tootsville-js.texi

doc/conf.py:	build/doc.conf.py ./build/version
	$(SED) -e "s/@@VERSION@@/$$(< build/version)/g" \
		build/doc.conf.py > doc/conf.py

doc/texi/TootsvilleJS.texi: doc/rst/index.rst doc/conf.py
	sphinx-build -b texinfo -j 4 -n -a -c doc \
		doc/rst doc/texi

doc/rst/index.rst: node_modules/.bin/jsdoc node_modules/jsdoc-sphinx/template/publish.js \
		$(< build/js.order )
	mkdir -p doc/rst/
	node_modules/.bin/jsdoc -t node_modules/jsdoc-sphinx/template/ \
		--recurse play --recurse worker \
		-d doc/rst/

node_modules/.bin/jsdoc:
	npm install jsdoc

node_modules/jsdoc-sphinx/template/publish.js:
	npm install jsdoc-sphinx

#################### htaccess

htaccess:	dist/htaccess.all/play.$(clusterorg).htaccess

dist/htaccess.all/play.$(clusterorg).htaccess:	build/htaccess.base bin/make-all-htaccess
	bin/make-all-htaccess

#################### /worker.js

worker:	dist/worker.js

dist/worker.js:	worker/Worker.js worker/WorkerStart.js worker/TootsvilleWorker.js
	mkdir -p dist/
	$(JSC) --create_source_map dist/worker.map \
		$$(< build/closure-compiler.opts)           \
		--js worker/TootsvilleWorker.js            \
		--js worker/Worker.js                      \
		--js worker/WorkerStart.js                 \
		--js_output_file $@
	echo '//# sourceMappingURL=/worker.map' >> $@

#################### dist/node-adopt.js

node_modules/.bin/browserify:	package-lock.json
	npm install browserify --save

node_modules/@openid/openyolo/package.json:
	npm install @openid/openyolo --save

dist/node-adopt.js:	build/node-adopt.js \
		node_modules/@openid/openyolo/package.json \
		package-lock.json node_modules/.bin/browserify
	node_modules/.bin/browserify build/node-adopt.js -o dist/node-adopt.js

#################### dist/play/play.js

dist/play/play.js:	build/js.order $(shell cat build/js.order)
	mkdir -p dist/play/
	$(JSC) --create_source_map dist/play/play.map   \
                    $$(< build/closure-compiler.opts)                \
		--source_map_location_mapping 'play/|/play/'        \
		$$(< build/js.order )                            \
		--js_output_file $@
	echo '//# sourceMappingURL=/play/play.map' >> $@
	$(SED) -e s/@@BUILD@@/$(BUILD)/ $@ > ...$$ && mv -f ...$$ $@

dist/version.js:	build/version
	echo "Tootsville.version = \"$$(< build/version)\";" > dist/version.js

play:	dist/play.$(clusterorg)

dist/play/play.map:	dist/play/play.js

#################### dist/play/play.css

PLAYLESSDEPS=$(wildcard play/*.less play/**/*.less)

dist/play/play.css:	$(PLAYLESSDEPS)
	mkdir -p dist/play/
	$(LESSC) --math=strict --source-map play/play.less dist/play/play.css

dist/play/play.css.map:	dist/play/play.css

#################### TODO

TODO.org:	$(shell find */ -name \\*.lisp -o -name \\*.css -o -name \\*.js -o -name \\*.org -o \
		   -name \\*.texi -o -name \\*.asd -o -name \\*.txt -o -name \\*.html) \
		 README.org
	-mv TODO.org TODO.org~ 2>/dev/null
	echo '* TODO-type notes found $$(date +%Y-%m-%d)' > TODO.org
	echo '' >> TODO.org
	echo '** FIXME Actual bugs!' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn FIXME mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** TODO To be done ASAP' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn TODO mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** XXX Might Be Nice to do someday' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn XXX mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** ☠☠☠ Bruce-Robert should examine this' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn ☠☠☠ mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org

TODO.scorecard:	README.org
	echo -n 'TOOTS_FIXME=' > TODO.scorecard
	git grep FIXME mesh play www build README.org \
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_TODO=' >> TODO.scorecard
	git grep TODO mesh play www build README.org \
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_XXX=' >> TODO.scorecard
	git grep XXX mesh play www build README.org \
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_BRP=' >> TODO.scorecard
	git grep ☠☠☠ mesh play www build README.org \
	 | wc -l >> TODO.scorecard

#################### bin/jscl

jscl:	lib/jscl/jscl.js

lib/jscl/jscl.js: $(shell find lib/jscl \( -name \**.lisp -or -name \**.js -or -name \**.asd \)  -and -not -name .\*)
	cd lib/jscl; ./make.sh
	rm lib/jscl/jscl-node.js
	rm lib/jscl/jscl-web.js

#################### www

dist/www/2019.css:	$(wildcard www/*.less www/**/*.less)
	$(LESSC) --math=strict --source-map www/2019.less dist/www/2019.css

#################### devel-test

devel-test:
	ptyxis --title='Play server' --tab -- $(MAKE) devel-play-watch &
	ptyxis --title='WWW server' --tab -- $(MAKE) devel-www-watch &

# Start local development server configuration
run: build-all start-servers
	@echo "🚀 Tootsville development servers started!"
	@echo "📱 Play server: http://localhost:5002/play/"
	@echo "🌐 WWW server: http://localhost:5001/"
	@echo "🔧 Backend API: http://localhost:5000/"
	@echo "🔌 WebSocket: ws://localhost:5004/"
	@echo ""
	@echo "Press Ctrl+C to stop all servers"

# Build everything needed for development
build-all:
	@echo "🔨 Building Tootsville backend..."
	cd lib/tootsville.net && $(MAKE) clean && $(MAKE) Tootsville
	@echo "🔨 Building React frontend..."
	cd react-migration && npm install --legacy-peer-deps && npm run build
	@echo "🔨 Building play client..."
	cd play && npm install --legacy-peer-deps && $(MAKE)
	@echo "✅ All builds complete!"

# Start all servers
start-servers: start-backend start-frontend
	@echo "✅ All servers started!"

# Start backend server
start-backend:
	@echo "🔧 Starting Tootsville backend server..."
	cd lib/tootsville.net && $(MAKE) devel-serve &

# Start frontend servers
start-frontend:
	@echo "🌐 Starting frontend servers..."
	$(MAKE) devel-play &
	$(MAKE) devel-www &
	sleep 3

devel-play-watch:	devel-play
	while inotifywait -e close_write -r play ; do $(MAKE) devel-play ; done

devel-playtest:	devel-play
	firefox --devtools --new-tab "http://localhost:5002/play/" </dev/null &>/dev/null &

devel-play:	dist/play.$(clusterorg) dist/play/httpd.pid
	-notify-send -t 60000 -i document-new "Build Complete: play" "Finished building devel-play"

devel-wwwtest:	devel-www
	firefox --devtools --new-tab "http://localhost:5001/" </dev/null &>/dev/null &

devel-www:	dist/www.$(clusterorg) dist/www/httpd.pid
	-notify-send -t 60000 -i document-new "Build Complete: www" "Finished building devel-www"

devel-www-watch:	devel-www
	while inotifywait -e close_write -r www ; do $(MAKE) devel-www ; done

dist/www/httpd.pid:	dist/www/dev-www.httpd.conf
	mkdir -p dist/www
	if [ -f dist/www/httpd.pid ]; then \
		kill -HUP $$(< dist/www/httpd.pid ) || \
		httpd -f $(shell pwd)/dist/www/dev-www.httpd.conf ;\
	else \
		httpd -f $(shell pwd)/dist/www/dev-www.httpd.conf ;\
	fi

dist/play/httpd.pid:	dist/play/dev-play.httpd.conf dist/play/play.js
	mkdir -p dist/play
	if [ -f dist/play/httpd.pid ]; then \
		kill -HUP $$(< dist/play/httpd.pid ) || \
		httpd -f $(shell pwd)/dist/play/dev-play.httpd.conf ;\
	else \
		httpd -f $(shell pwd)/dist/play/dev-play.httpd.conf ;\
	fi

dist/play/dev-play.httpd.conf:	bin/dev-play-httpd-conf
	bin/dev-play-httpd-conf $(clusterorg)

dist/www/dev-www.httpd.conf:	bin/dev-www-httpd-conf
	bin/dev-www-httpd-conf $(clusterorg)

dist/play.$(clusterorg)/.well-known/assetlinks.json: play/.well-known/assetlinks.json
	mkdir -p dist/play.$(clusterorg)/.well-known
	cp $< $@

dist/play.$(clusterorg)/play/tootsville.js:	$(shell cat build/js.order)
	mkdir -p dist/play.$(clusterorg)/play/
	for file in $$(< build/js.order) ; \
	do \
	   mkdir -p dist/play.$(clusterorg)/$$(dirname $$file ) ; \
	   cp $$file dist/play.$(clusterorg)/$$file ; \
	done

dist/play.$(clusterorg)/play/play.$(VERSION).js:	dist/play/play.js
	mkdir -p dist/play.$(clusterorg)/play/
	cp $< $@

dist/play.$(clusterorg)/play/game/start.js:	$(shell cat build/js.order)
	mkdir -p dist/play.$(clusterorg)/play/
	rsync -a $$(< build/js.order) dist/play.$(clusterorg)/play/

dist/play.$(clusterorg)/play/play.map:	dist/play/play.map
	mkdir -p dist/play.$(clusterorg)/play/
	cp dist/play/play.map dist/play.$(clusterorg)/play/
	for file in $$(< build/js.order) ; \
	do \
	    mkdir -p dist/play.$(clusterorg)/$$(dirname $$file) ; \
              cp $$file dist/play.$(clusterorg)/$$file ; \
	done

dist/play.$(clusterorg)/play/play.$(VERSION).css:	dist/play/play.css
	mkdir -p dist/play.$(clusterorg)/play/
	cp dist/play/play.css dist/play.$(clusterorg)/play/play.$(VERSION).css

dist/play.$(clusterorg)/play/play.css.map:	dist/play/play.css.map
	mkdir -p dist/play.$(clusterorg)/play/
	cp dist/play/play.css.map dist/play.$(clusterorg)/play/
	for file in $(PLAYLESSDEPS) ; do cp $$file dist/play.$(clusterorg)/$$file ; done

dist/play.$(clusterorg)/worker.js:	dist/worker.js
	mkdir -p dist/play.$(clusterorg)/
	cp $< $@

dist/play.$(clusterorg)/.htaccess:	dist/htaccess.all/play.$(clusterorg).htaccess
	mkdir -p dist/play.$(clusterorg)
	cp $< $@

dist/play.$(clusterorg)/favicon.%:	www/favicon.%
	mkdir -p dist/play.$(clusterorg)/
	cp $< $@

errordocs=$(wildcard www/error/*.var www/error/*.shtml www/error/*.json www/error/*.htmlf www/error/.htaccess)

dist/play.$(clusterorg)/error/404.var:	$(errordocs)
	mkdir -p dist/play.$(clusterorg)/error/
	cp $(errordocs) dist/play.$(clusterorg)/error/

dist/play.$(clusterorg)/play/index.html: play/index.html build/version
	mkdir -p dist/play.$(clusterorg)/play/
	cp $< $@
	$(SED) -e s/@@VERSION@@/$(VERSION)/g -i $@

dist/play.$(clusterorg)/play/system-check/index.html: play/system-check/index.html
	mkdir -p dist/play.$(clusterorg)/play/system-check/
	cp $< $@

dist/play.$(clusterorg)/play/UI/panels/control-panel.html:	$(shell ls -1 play/UI/panels/*)
	mkdir -p dist/play.$(clusterorg)/play/UI/panels
	cp -a play/UI/panels/*.{html,js} dist/play.$(clusterorg)/play/UI/panels

dist/play.$(clusterorg):	worker htaccess mesh \
	dist/play.$(clusterorg)/play/tootsville.js \
	dist/play.$(clusterorg)/.well-known/assetlinks.json \
	dist/play.$(clusterorg)/play/UI/panels/control-panel.html \
	dist/play.$(clusterorg)/play/play.$(VERSION).css \
	dist/play.$(clusterorg)/play/play.css.map \
	dist/play.$(clusterorg)/play/play.$(VERSION).js \
	dist/play.$(clusterorg)/play/game/start.js \
	dist/play.$(clusterorg)/play/play.map \
	dist/play.$(clusterorg)/play/index.html \
	dist/play.$(clusterorg)/worker.js \
	dist/play.$(clusterorg)/.htaccess \
	dist/play.$(clusterorg)/error/404.var \
	dist/play.$(clusterorg)/play/system-check/index.html

#################### mesh

mesh:	dist/play.$(clusterorg)/play/mesh.$(VERSION).min.js \
	dist/play.$(clusterorg)/play/jscl.$(VERSION).min.js

dist/play.$(clusterorg)/play/jscl.$(VERSION).min.js: lib/jscl/jscl.js
	$(JSC) $$(< build/closure-compiler.opts)           \
		--js $<  \
		--js_output_file $@

dist/play.$(clusterorg)/play/mesh.$(VERSION).min.js: dist/mesh.js
	$(JSC) $$(< build/closure-compiler.opts)           \
		--js $< \
		--js_output_file $@

dist/mesh.js: $(find mesh -name \*.lisp)
	sbcl --load 'lib/jscl/jscl.lisp' \
		--eval '(jscl::bootstrap)' \
		--load 'mesh/make-mesh.lisp' \
		--quit

#################### deploy

predeploy:	no-fixmes connectivity remotes

connectivity:
	echo " » Test connectivity"
	ssh play.$(clusterorg) ls -1d play.$(clusterorg)/ | grep play.$(clusterorg)
	ssh www.$(clusterorg) ls -1d www.$(clusterorg)/ | grep $(clusterorg)

no-fixmes:	TODO.scorecard
	TOOT_TODO=$$(grep TODO TODO.scorecard | wc -l) ;\
	TOOT_FIXME=$$(grep FIXME TODO.scorecard | wc -l) ;\
	if [[ $$TOOT_FIXME -gt 0 ]] ;\
	then \
			clear ;\
			echo "There are $$TOOT_FIXME FIXME comments!" ;\
			if [[ "$(CLUSTER)" = . ]] ;\
			then \
				echo " ✗ Refusing to deploy to Production with FIXME notes" ;\
				exit 8 ;\
			fi ;\
			echo "" ;\
			echo "$(REALNAME), are you sure you want to deploy to $(clusterorg)" ;\
			echo "when there are $$TOOT_FIXME FIXME notes and $$TOOT_TODO TODOs?" ;\
			echo "" ;\
			read -p "Deploy anyway? (Y/N) ⇒ " -n 1 yorn ;\
			if [[ "$${yorn}" = "y" ]] ;\
			then \
				echo "" ;\
				echo "Overridden" ;\
				echo "" ;\
				echo "Override accepted. Good luck …" ;\
				break ;\
			elif [[ "$${yorn}" = "n" ]] ;\
			then \
				echo "" ;\
				echo "That seems wise. Exiting." ;\
				echo "" ;\
				exit 8 ;\
			else \
				echo "Only y or n work. Treading $$yorn as No." ; \
				exit 8; \
			fi ;\
	fi

deploy-play:	dist/play.$(clusterorg)
	echo " » Deploy play.$(clusterorg)"
	cd dist/; rsync -essh -rv play.$(clusterorg) play.$(clusterorg):
# TODO: status ∈ "started" "succeeded" "failed" — currently only success is reported
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=play.$(clusterorg) \
	     -F framework=gmake \
	     -F notifier.name=gmake \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)


deploy-www:	dist/www.$(clusterorg)
	echo " » Deploy www.$(clusterorg)"
	cd dist/; rsync -essh -rv www.$(clusterorg) www.$(clusterorg):

dist/www.$(clusterorg):	htaccess dist/www/2019.css
	mkdir -p dist/www.$(clusterorg)
	rsync --exclude='*~' --exclude='*#' -ar \
	      www/* dist/www.$(clusterorg)/
	cp dist/www/2019.css dist/www.$(clusterorg)/2019.css
	if [ "$(CLUSTER)" = "test" ]; \
	then \
		cp www/index.test.html dist/www.$(clusterorg)/index.html ;\
	fi
	if [ "$(CLUSTER)" = "qa" ]; \
	then \
		cp www/index.qa.html dist/www.$(clusterorg)/index.html ;\
	fi

remotes:
	if ! git remote -v | grep github &>/dev/null ;\
	then \
		git remote add github git@github.com:adventuring/tootsville.org ;\
	fi
	# if ! git remote -v | grep gitlab &>/dev/null ;\
	# then \
	# 	git remote add gitlab git@gitlab.com:adventuring/tootsville.org ;\
	# fi
	if ! git remote -v | grep goethe &>/dev/null ;\
	then \
		git remote add goethe goethe.Tootsville.org:devel/git/tootsville.org ;\
	fi

bump-next-version:
	git status | grep modified: && exit 9 || :
	perl -pne 's/(\d+\.\d+)\.(\d+)/"$$1." . (1+ $$2) /e' -i build/version
	git add build/version
	git commit -m "bump version number for next build"

git-tag-deployment:	../tootsville.net/Tootsville ../tootsville.net/tootsville.asd
	now=$$(date +%Y-%m-%d) ;\
	msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	if git rev-parse v$$VERSION &>/dev/null ;\
	then \
	    echo "Previous tag v$$VERSION found, adding v$$VERSION-$$now" ;\
	    if git rev-parse v$$VERSION-$$now &>/dev/null ;\
	    then \
	        now=$$(date +%Y-%m-%d.%H%M) ;\
	        msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	        echo " - I meant v$$VERSION-$$now" ;\
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.org: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    else \
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.org: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    fi ;\
	else \
	    echo "First deploy of v$$VERSION, tagging" ;\
	    git submodule foreach git tag -a tootsville-v$$VERSION -m "for Tootsville.org: $$msg" ;\
	    git tag -a v$$VERSION -m "$$msg" ;\
	fi

	git push --tags origin ||:
	git submodule foreach --recursive 'git push --tags origin ||:'
	git push --tags github ||:
	git submodule foreach --recursive 'git push --tags github ||:'
	# git push --tags gitlab ||:
	# git submodule foreach --recursive 'git push --tags gitlab ||:'
	git push --tags goethe ||:
	git submodule foreach --recursive 'git push --tags goethe ||:'

	$(MAKE) bump-next-version

dist/doc.texi: $(shell cat build/js.order) $(shell ls play/UI/panels/*.js) \
		build/extract-docs \
		build/header.texi build/footer.texi
	bin/make-all-htaccess
	cp -f build/header.texi dist/doc.texi
	perl build/extract-docs $$(grep -v lib/ build/js.order) $$(ls play/UI/panels/*.js) >> dist/doc.texi; \
	cat build/footer.texi >> dist/doc.texi

#################### mobile

mobile: mobile-android mobile-ios mobile-ipad mobile-firetv mobile-windows11 mobile-symbian mobile-webos mobile-webtv

mobile-android:
	cd play/react && npm run build:android

mobile-ios:
	cd play/react && npm run build:ios

mobile-ipad:
	cd play/react && npm run build:ipad

mobile-firetv:
	cd play/react && npm run build:firetv

mobile-windows11:
	cd play/react && npm run build:windows11

mobile-symbian:
	cd play/react && npm run build:symbian

mobile-webos:
	cd play/react && npm run build:lgwebos

mobile-webtv:
	cd play/react && npm run build:webtv

mobile-test: mobile
	cd play/react && npm run test:mobile

mobile-deploy: mobile
	cd play/react && npm run deploy:mobile

#################### organize-artifacts

organize-artifacts: organize-docs organize-apps organize-mobile organize-server
	@echo "✅ All build artifacts organized in dist/"

organize-docs: dist/docs
	@echo "📚 Documentation organized in dist/docs"

dist/docs: docs
	mkdir -p dist/docs
	cp -r play/react/docs/* dist/docs/ 2>/dev/null || true
	cp -r lib/tootsville.net/doc/* dist/docs/ 2>/dev/null || true
	cp TODO.org dist/docs/ 2>/dev/null || true
	cp TODO.scorecard dist/docs/ 2>/dev/null || true

organize-apps: dist/apps
	@echo "📱 Applications organized in dist/apps"

dist/apps: dist/play.$(clusterorg) dist/www.$(clusterorg)
	mkdir -p dist/apps
	cp -r dist/play.$(clusterorg) dist/apps/ 2>/dev/null || true
	cp -r dist/www.$(clusterorg) dist/apps/ 2>/dev/null || true

organize-mobile: dist/mobile
	@echo "📱 Mobile builds organized in dist/mobile"

dist/mobile: mobile
	mkdir -p dist/mobile
	cp -r play/react/build/* dist/mobile/ 2>/dev/null || true

organize-server: dist/server
	@echo "🖥️ Server artifacts organized in dist/server"

dist/server: lib/tootsville.net
	mkdir -p dist/server
	cp -r lib/tootsville.net/* dist/server/ 2>/dev/null || true

#################### clean-artifacts

clean-artifacts:
	rm -rf dist/docs dist/apps dist/mobile dist/server
	rm -rf play/react/build
	@echo "🧹 Build artifacts cleaned"

#################### deploy-docs

deploy-docs:
	scp dist/htaccess.all/goethe.tootsville.net.htaccess goethe.tootsville.org:goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org:goethe.tootsville.org/
	rsync -essh -zar www/error goethe.tootsville.org:goethe.tootsville.org/


