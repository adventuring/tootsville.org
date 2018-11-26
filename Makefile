all: .deps~ htaccess play worker servers TODO.org TODO.scorecard

test: all servers-test

deploy: all deploy-www deploy-play deploy-servers git-tag-deployment deploy-docs

####################

.deps~:	build/build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.deps~

####################

servers-test:	servers/Tootsville
	servers/Tootsville check

#################### vars

# To target alternate clusters:
#	make CLUSTER=qa
#	make CLUSTER=.
CLUSTER:=test
ifeq ($(CLUSTER),.)
clusterorg=tootsville.org
clusternet=tootsville.net
else
clusterorg=$(CLUSTER).tootsville.org
clusternet=$(CLUSTER).tootsville.net
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
	$(MAKE) -C servers clean
	find . -name \*~ -exec rm {} \;
	rm -rf dist/ ; mkdir -p dist/
	rm -f TODO.org TODO.scorecard	

#################### servers

servers:	servers/Tootsville

servers/Tootsville:	$(shell find servers \( -name \*.lisp -o -name \*.asd \) -and -not -name .\*)
	$(MAKE) -C servers Tootsville test

#################### doc

doc:	server-doc js-doc

server-doc: \
	doc/Tootsville.txt \
	doc/Tootsville.pdf \
	doc/Tootsville.html.tar.gz

doc/Tootsville.texi:	servers/doc/Tootsville.texi
	cp servers/doc/Tootsville.texi doc/

servers/doc/Tootsville.texi: servers/Tootsville
	$(MAKE) -C servers doc/Tootsville.texi

doc/Tootsville.html.tar.gz:	doc/Tootsville.html.tar
	gzip -9 -c < $< > $@

doc/Tootsville.html.tar.Z:	doc/Tootsville.html.tar
	compress -9 -c < $< > $@

doc/Tootsville.html.tar.bz2:	doc/Tootsville.html.tar
	bzip2 -9 -c < $< > $@

doc/Tootsville.html.tar.xz:	doc/Tootsville.html.tar
	xz -9 -c < $< > $@

doc/Tootsville.html.tar:	doc/Tootsville.html.d/index.html
	cd doc; tar cf Tootsville.html.tar Tootsville.html.d

doc/Tootsville.html.zip:	doc/Tootsville.html.d/index.html
	cd doc; zip -9 Tootsville.html.zip Tootsville.html.d

doc/Tootsville.html.d/index.html:	doc/Tootsville.texi doc/doc.css
	cd doc; makeinfo -o Tootsville.html.d/ \
		--html --css-include=doc.css \
		--split=node Tootsville.texi

doc/Tootsville.ps:	doc/Tootsville.pdf
	cd doc; pdf2ps Tootsville.pdf

doc/Tootsville.pdf:	doc/Tootsville.texi
	cd doc; PDFLATEX=xelatex texi2pdf Tootsville.texi 

doc/Tootsville.txt:	doc/Tootsville.texi
	cd doc; makeinfo --plaintext -o Tootsville.txt Tootsville.texi

doc/Tootsville.info:	doc/Tootsville.texi
	cd doc; makeinfo -o Tootsville.info Tootsville.texi

doc/doc.css:	www/doc.less

all-docs: \
	doc/Tootsville.html.tar.gz	\
	doc/Tootsville.html.tar.Z	\
	doc/Tootsville.html.tar.bz2	\
	doc/Tootsville.html.tar.xz	\
	doc/Tootsville.html.zip	\
	doc/Tootsville.ps	\
	doc/Tootsville.pdf	\
	doc/Tootsville.txt 	\
	doc/Tootsville.info

js-doc:	doc/texi/tootsville-js.texi

doc/texi/tootsville-js.texi:	doc/texi/TootsvilleJS.texi
	perl -ne 'print if /@c END_PREAMBLE/..0' \
		< doc/texi/TootsvilleJS.texi > doc/texi/tootsville-js.texi

doc/conf.py:	build/doc.conf.py servers/tootsville.asd
	sed -e "s/@@VERSION@@/$$(grep :version servers/tootsville.asd | cut -d \" -f 2)/g" \
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

dist/htaccess.all/play.$(clusterorg).htaccess:	htaccess.base bin/make-all-htaccess
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

play:	dist/play.$(clusterorg)

dist/play/play.map:	dist/play/play.js

#################### dist/play/play.css

LESSFILES=$(shell find play -name \*.less -and -not -name .\*)

dist/play/play.css:	$(LESSFILES)
	mkdir -p dist/play/
	lessc --strict-math=on --source-map play/play.less dist/play/play.css

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

TODO.scorecard:	$(shell find servers \( -name \*.lisp -o -name \*.asd \
	-o -name \*.js -o -name \*.less -o -name \*.html -o -name \*.htmlf \
	-o -name \*.shtml \) -and -not -name .\*) \
	README.org
	echo -n 'TOOTS_FIXME=' > TODO.scorecard
	git grep FIXME: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_TODO=' >> TODO.scorecard
	git grep TODO: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_XXX=' >> TODO.scorecard
	git grep XXX: */ README.org | wc -l >> TODO.scorecard
	echo -n 'TOOTS_BRP=' >> TODO.scorecard
	git grep ☠☠☠: */ README.org | wc -l >> TODO.scorecard

#################### bin/jscl

jscl:	bin/jscl

bin/jscl: $(shell find jscl \( -name \**.lisp -or -name \**.js -or -name \**.asd \)  -and -not -name .\*)
	cd jscl; ./make.sh

#################### www

dist/www/2019.css:	$(shell echo www/*.less)
	lessc --strict-math=on --source-map www/2019.less dist/www/2019.css

#################### dev-test

devel-test:	devel-serve devel-play

devel-serve:	servers/Tootsville
	servers/Tootsville server

devel-playtest:	devel-play
	firefox --devtools --new-tab "http://localhost:5002/play/" </dev/null &>/dev/null &

devel-play:	dist/play.$(clusterorg) dist/play/httpd.pid

dist/play/httpd.pid:	dist/play/dev-play.httpd.conf
	if [ -f dist/play/httpd.pid ]; then kill -SIGHUP $$(< dist/play/httpd.pid ); else \
		httpd -f $(shell pwd)/dist/play/dev-play.httpd.conf ;\
	fi

dist/play/dev-play.httpd.conf:	bin/dev-play-httpd-conf
	bin/dev-play-httpd-conf $(clusterorg)

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
	for file in $(LESSFILES) ; \
	do \
	    mkdir -p dist/play.$(clusterorg)/$$(dirname $$file) ; \
	    cp $$file dist/play.$(clusterorg)/$$file ; \
	done

dist/play.$(clusterorg)/worker.js:	dist/worker.js
	mkdir -p dist/play.$(clusterorg)/
	cp $< $@

dist/play.$(clusterorg)/.htaccess:	dist/htaccess.all/play.$(clusterorg).htaccess
	mkdir -p dist/play.$(clusterorg)
	cp $< $@

dist/play.$(clusterorg)/favicon.%:	www/favicon.%
	mkdir -p dist/play.$(clusterorg)/
	cp $< $@

errordocs=$(shell echo www/error/*.{var,shtml,json,htmlf} )

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

deploy-servers:	predeploy
	for host in game1 game2; \
	do \
		echo " » Deploy $$host.$(clusternet)" ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers install ;\
		VERSION=$(shell servers/Tootsville version-info version) ;\
		curl https://api.rollbar.com/api/1/deploy/ \
		     -F access_token=$(ACCESS_TOKEN) \
		     -F environment=$$host.$(clusternet) \
		     -F framework=gmake \
		     -F notifier.name=gmake \
		     -F revision=$(REVISION) \
		     -F comment="v $(VERSION)" \
		     -F uuid=$(uuidgen) \
		     -F local_username=$(LOCAL_USERNAME) ;\
	done

deploy-www:	predeploy
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
	ssh game1.$(clusternet) sbcl --no-userinit --quit | grep 'This is SBCL'
	ssh game2.$(clusternet) sbcl --no-userinit --quit | grep 'This is SBCL'


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

predeploy-www:	htaccess dist/www/2019.css
	echo " » Pre-deploy www.$(clusterorg)"
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
	bin/shar-stream dist/ www.$(clusterorg) www.$(clusterorg)

predeploy-servers:	servers quicklisp-update-servers
	for host in game1 game2 ;\
	do \
		echo " » Pre-deploy $$host.$(clusternet)" ;\
		rsync -essh --delete -zar * .??* $$host.$(clusternet):tootsville.org/ ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers clean || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers Tootsville || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers test || exit 6 ;\
	done

quicklisp-update-servers:
	for host in game1 game2; \
	do \
		echo " » Ensure latest Quicklisp on $$host.$(clusternet)" ;\
	    ssh $$host.$(clusternet) \
	        sbcl --non-interactive \
	        --no-inform \
	        --eval "'(ql:update-client)'" \
	        --eval "'(ql:update-all-dists)'" \
	        --quit ;\
	done


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
	git status | grep modified: | grep servers/tootsville.asd && exit 9 || :
	perl -pne 's/:version "(\d+\.\d+)\.(\d+)"/ ":version \"$$1." . (1+ $$2) . "\"" /e' -i servers/tootsville.asd
	git add servers/tootsville.asd
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
	make -C servers doc-publish
	scp dist/htaccess.all/goethe.tootsville.net.htaccess goethe.tootsville.org:goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org:goethe.tootsville.org/
	rsync -essh -zar www/error goethe.tootsville.org:goethe.tootsville.org/

