# This is the Makefile for Tootsville
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

all: .deps~ htaccess play worker servers TODO.org TODO.scorecard

test: all servers-test

deploy: all deploy-www deploy-play deploy-servers git-tag-deployment deploy-docs

####################

.deps~:	build/build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.deps~

####################

servers-test:	../tootsville.net/Tootsville
	../tootsville.net/Tootsville check

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
	rm -f TODO.org TODO.scorecard	

#################### servers

servers:	../tootsville.net/Tootsville

../tootsville.net/Tootsville:
	$(MAKE) -C ../tootsville.net CLUSTER=$(CLUSTER) Tootsville test

#################### doc

doc:	js-doc

doc/doc.css:	www/doc.less

js-doc:	doc/texi/tootsville-js.texi

doc/texi/tootsville-js.texi:	doc/texi/TootsvilleJS.texi
	perl -ne 'print if /@c END_PREAMBLE/..0' \
		< doc/texi/TootsvilleJS.texi > doc/texi/tootsville-js.texi

doc/conf.py:	build/doc.conf.py ./build/version
	sed -e "s/@@VERSION@@/$$(< build/version)/g" \
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
	closure-compiler --create_source_map dist/worker.map \
                    $(< build/closure-compiler.opts)           \
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
	closure-compiler --create_source_map dist/play/play.map   \
		--third_party                                   \
                    $(< build/closure-compiler.opts)                \
		--source_map_location_mapping 'play/|/play/'        \
		--language_in ECMASCRIPT6                        \
		--language_out ECMASCRIPT5_STRICT                \
		$$(< build/js.order )                            \
		--js_output_file $@
	echo '//# sourceMappingURL=/play/play.map' >> $@
	sed -e s/@@BUILD@@/$$(date +%Y%m%d%H%M%S)/ -i $@

play:	dist/play.$(clusterorg)

dist/play/play.map:	dist/play/play.js

#################### dist/play/play.css

PLAYLESSDEPS=$(wildcard play/*.less play/**/*.less)

dist/play/play.css:	$(PLAYLESSDEPS)
	mkdir -p dist/play/
	lessc --math=strict --source-map play/play.less dist/play/play.css

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

TODO.scorecard:	$(shell find \( -name \*.lisp -o -name \*.asd \
	-o -name \*.js -o -name \*.less -o -name \*.html -o -name \*.htmlf \
	-o -name \*.shtml \) -and -not -name .\*) \
	README.org
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

jscl:	bin/jscl

jscl/jscl.min.js:	jscl

bin/jscl: $(shell find jscl \( -name \**.lisp -or -name \**.js -or -name \**.asd \)  -and -not -name .\*)
	cd jscl; ./make.sh

#################### www

dist/www/2019.css:	$(wildcard www/*.less www/**/*.less)
	lessc --math=strict --source-map www/2019.less dist/www/2019.css

#################### dev-test

devel-test:	devel-serve devel-play

devel-serve:	../tootsville.net/Tootsville
	../tootsville.net/Tootsville server < /dev/null

devel-play-watch:	devel-play
	while inotifywait -e close_write -r play ; do $(MAKE) devel-play ; done

devel-playtest:	devel-play
	firefox --devtools --new-tab "http://localhost:5002/play/" </dev/null &>/dev/null &

devel-play:	dist/play.$(clusterorg) dist/play/httpd.pid
	-notify-send -i document-new "Build Complete: play" "Finished building devel-play"

devel-wwwtest:	devel-www
	firefox --devtools --new-tab "http://localhost:5001/" </dev/null &>/dev/null &

devel-www:	dist/www.$(clusterorg) dist/www/httpd.pid
	-notify-send -i document-new "Build Complete: www" "Finished building devel-www"

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

dist/play.$(clusterorg)/play/play.js:	dist/play/play.js
	mkdir -p dist/play.$(clusterorg)/play/
	cp dist/play/play.js dist/play.$(clusterorg)/play/

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

dist/play.$(clusterorg)/play/play.css:	dist/play/play.css
	mkdir -p dist/play.$(clusterorg)/play/
	cp dist/play/play.css dist/play.$(clusterorg)/play/

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

dist/play.$(clusterorg)/play/index.html: play/index.html
	cp $< $@

dist/play.$(clusterorg)/play/ui/panels/control-panel.html:	$(shell ls -1 play/ui/panels/*)
	mkdir -p dist/play.$(clusterorg)/play/ui/panels
	cp -ar play/ui/panels/* dist/play.$(clusterorg)/play/ui/panels

dist/play.$(clusterorg):	worker htaccess \
	dist/play.$(clusterorg)/play/tootsville.js \
	dist/play.$(clusterorg)/.well-known/assetlinks.json \
	dist/play.$(clusterorg)/play/ui/panels/control-panel.html \
	dist/play.$(clusterorg)/play/play.css \
	dist/play.$(clusterorg)/play/play.css.map \
	dist/play.$(clusterorg)/play/play.js \
	dist/play.$(clusterorg)/play/game/start.js \
	dist/play.$(clusterorg)/play/play.map \
	dist/play.$(clusterorg)/play/index.html \
	dist/play.$(clusterorg)/worker.js \
	dist/play.$(clusterorg)/.htaccess \
	dist/play.$(clusterorg)/error/404.var

#################### deploy

deploy-play:	predeploy-play
	echo " » Deploy play.$(clusterorg)"
	ssh play.$(clusterorg) "rm -rf play.$(clusterorg).before-deploy && mv play.$(clusterorg) play.$(clusterorg).before-deploy && mv play.$(clusterorg).new play.$(clusterorg)" || exit 9
# TODO: status ∈ "started" "succeeded" "failed" — currently only success is reported
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=play.$(clusterorg) \
	     -F framework=gmake \
	     -F notifier.name=gmake \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

deploy-servers:	../tootsville.net/Tootsville
	$(MAKE) -C ../tootsville.net CLUSTER=$(CLUSTER) deploy

deploy-www:	predeploy-www
	echo " » Deploy www.$(clusterorg)"
	ssh www.$(clusterorg) "mv www.$(clusterorg) www.$(clusterorg).before-deploy && mv www.$(clusterorg).new www.$(clusterorg)"
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=www.$(clusterorg) \
	     -F framework=gmake \
	     -F notifier.name=gmake \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

predeploy:	no-fixmes connectivity predeploy-play predeploy-www predeploy-servers remotes

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

predeploy-play:	dist/play.$(clusterorg)
	echo " » Pre-deploy play.$(clusterorg)"
	bin/shar-stream dist/ play.$(clusterorg) play.$(clusterorg)

predeploy-www:	dist/www.$(clusterorg)
	echo " » Pre-deploy www.$(clusterorg)"
	bin/shar-stream dist/ www.$(clusterorg) www.$(clusterorg)

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

predeploy-servers:
	$(MAKE) -C ../tootsville.net CLUSTOR=$(CLUSTER) predeploy

remotes:
	if ! git remote -v | grep github &>/dev/null ;\
	then \
		git remote add github git@github.com:adventuring/tootsville.org ;\
	fi
	if ! git remote -v | grep gitlab &>/dev/null ;\
	then \
		git remote add gitlab git@gitlab.com:adventuring/tootsville.org ;\
	fi
	if ! git remote -v | grep goethe &>/dev/null ;\
	then \
		git remote add goethe goethe.Tootsville.org:devel/git/tootsville.org ;\
	fi

bump-next-version:
	git status | grep modified: && exit 9 || :
	perl -pne 's/(\d+\.\d+)\.(\d+)/"$$1." . (1+ $$2) /e' -i build/version
	git add build/version
	git commit -m "bump version number for next build"

git-tag-deployment:
	VERSION=$$(servers/Tootsville version-info version) ;\
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
	git push --tags gitlab ||:
	git submodule foreach --recursive 'git push --tags gitlab ||:'
	git push --tags goethe ||:
	git submodule foreach --recursive 'git push --tags goethe ||:'

	$(MAKE) bump-next-version

#################### deploy-docs

deploy-docs:
	make -C ../tootsville.net doc-publish
	scp dist/htaccess.all/goethe.tootsville.net.htaccess goethe.tootsville.org:goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org:goethe.tootsville.org/
	rsync -essh -zar www/error goethe.tootsville.org:goethe.tootsville.org/


