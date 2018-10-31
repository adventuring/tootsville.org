all: htaccess play worker servers TODO.org TODO.scorecard

deploy: all deploy-www deploy-play deploy-servers git-tag-deployment deploy-docs

#################### vars

# To target alternate clusters:
#	make CLUSTER=qa.tootsville.org deploy
#	make CLUSTER=tootsville.org deploy
CLUSTER:=test.tootsville.org

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

js-doc:	play-doc

play-doc:
# TODO. There  doesn't seem  to be  any elegant  way (yet)  to integrate
# JSDoc, and,  in fact, I'm having  a hard time finding  a JSDoc-to-texi
# compiler  at  all  and  may  have   to  write  one;  perhaps  look  at
# post-processing the JSON output from Node.js doctrine, or just rolling
# our own.

#################### htaccess

htaccess:	htaccess.base bin/make-all-htaccess
	bin/make-all-htaccess

#################### play/worker.js

worker:	dist/play/worker.js

dist/play/worker.js:	play/worker.js
	mkdir -p dist/play/
	closure-compiler --create_source_map dist/play/worker.map \
		--third_party					  \
		--language_out ECMASCRIPT5_STRICT		  \
		--language_in ECMASCRIPT6 			  \
		--source_map_location_mapping 'play/|/play/' 	  \
		--js $<                                           \
		--js_output_file $@
	echo '//# sourceMappingURL=/play/worker.map' >> $@

#################### play/play.js

dist/play/play.js:	${PLAYJS}
	mkdir -p dist/play/
	cat ${PLAYJS} > dist/play/play.max.js
	closure-compiler --create_source_map dist/play/play.map \
		--third_party                                   \
		--source_map_location_mapping 'play/|/play/'    \
		--language_in ECMASCRIPT6                       \
		--language_out ECMASCRIPT5_STRICT               \
		--js $<                                         \
		--js_output_file $@
	echo '//# sourceMappingURL=/play/play.map' >> $@

play:	dist/play/play.css \
	dist/play/play.js

PLAYJS = $(shell ./bin/find-play-js)

dist/play/play.map:	dist/play/play.js

dist/play/play.css:	$(shell find play -name \*.less -and -not -name .\*)
	mkdir -p dist/play/
	lessc --strict-math=on --source-map play/play.less dist/play/play.css

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

TODO.scorecard:	$(shell find */ -type f) README.org
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

#################### deploy

deploy-play:	predeploy
	echo " » Deploy play.$(CLUSTER)"
	ssh play.$(CLUSTER) "mv play.$(CLUSTER) play.$(CLUSTER).before-deploy && mv play.$(CLUSTER).new play.$(CLUSTER)"
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=play.$(CLUSTER) \
	     -F framework=gmake \
	     -F notifier.name=gmake \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

deploy-servers:	predeploy
	for host in users gossip world; \
	do \
		echo " » Deploy $$host.$(CLUSTER)" ;\
		ssh $$host.$(CLUSTER) "cp servers/Tootsville --backup=simple -f /usr/local/bin/; \
cp servers/tootsville.service --backup=simple -f /usr/lib/systemd/user/; \
sudo -n systemctl enable tootsville; \
sudo -n systemctl restart tootsville; \
sudo -n systemctl start tootsville" ;\
		VERSION=$(shell servers/Tootsville version-info version) ;\
		curl https://api.rollbar.com/api/1/deploy/ \
		     -F access_token=$(ACCESS_TOKEN) \
		     -F environment=$$host.$(CLUSTER) \
		     -F framework=gmake \
		     -F notifier.name=gmake \
		     -F revision=$(REVISION) \
		     -F comment="v $(VERSION)" \
		     -F uuid=$(uuidgen) \
		     -F local_username=$(LOCAL_USERNAME) ;\
	done

deploy-www:	predeploy
	echo " » Deploy www.$(CLUSTER)"
	ssh www.$(CLUSTER) "mv www.$(CLUSTER) www.$(CLUSTER).before-deploy && mv www.$(CLUSTER).new www.$(CLUSTER)"
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=www.$(CLUSTER) \
	     -F framework=gmake \
	     -F notifier.name=gmake \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

predeploy:	no-fixmes connectivity predeploy-play predeploy-www predeploy-servers remotes

connectivity:
	echo " » Test connectivity"
	ssh play.$(CLUSTER) ls -1d play.$(CLUSTER)/ | grep play.$(CLUSTER)
	ssh $(CLUSTER) ls -1d $(CLUSTER)/ | grep $(CLUSTER)
	ssh users.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'
	ssh gossip.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'
	ssh world.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'


no-fixmes:	TODO.scorecard
	TOOT_TODO=$$(grep TODO TODO.scorecard | wc -l) ;\
	TOOT_FIXME=$$(grep FIXME TODO.scorecard | wc -l) ;\
	if [[ $$TOOT_FIXME -gt 0 ]] ;\
	then \
			clear ;\
			echo "There are $$TOOT_FIXME FIXME comments!" ;\
			if [[ "$(CLUSTER)" = tootsville.org ]] ;\
			then \
				echo " ✗ Refusing to deploy to Production with FIXME notes" ;\
				exit 8 ;\
			fi ;\
			echo "" ;\
			echo "$(REALNAME), are you sure you want to deploy to $(CLUSTER)" ;\
			echo "when there are $$TOOT_FIXME FIXME notes and $$TOOT_TODO TODOs?" ;\
			echo "" ;\
			read -p "Deploy anyway? (Y/N) ⇒ " -n 1 yorn ;\
			if [[ "$${yorn}" = "y" ]] ;\
			then \
				echo "" ;\
				echobig Overridden ;\
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

predeploy-play:	play worker htaccess
	echo " » Pre-deploy play.$(CLUSTER)"
	mkdir -p dist/play.$(CLUSTER)
#	copy in most files
	rsync --exclude='*~' --exclude='*#' -ar \
	      play/* play/.well-known dist/play.$(CLUSTER)/
# 	each host copies error pages and favicons
	rsync --exclude='*~' --exclude='*#'  -ar \
	      www/favicon.??? www/error dist/play.$(CLUSTER)/
# 	mapping to hosts in the cluster
	cp dist/htaccess.all/$(CLUSTER).cluster.json dist/play.$(CLUSTER)/cluster.json
# 	.htaccess generated above
	cp dist/htaccess.all/play.$(CLUSTER).htaccess dist/play.$(CLUSTER)/.htaccess
#
#	Stream a shar/unshar to the host at one go
	bin/shar-stream dist/ play.$(CLUSTER) play.$(CLUSTER)

predeploy-www:	htaccess dist/www/2019.css
	echo " » Pre-deploy www.$(CLUSTER)"
	mkdir -p dist/www.$(CLUSTER)
	rsync --exclude='*~' --exclude='*#' -ar \
	      www/* dist/www.$(CLUSTER)/
	cp dist/www/2019.css dist/www.$(CLUSTER)/2019.css
	if [ "$(CLUSTER)" = "test.tootsville.org" ]; \
	then \
		cp www/index.test.html dist/www.$(CLUSTER)/index.html ;\
	fi
	if [ "$(CLUSTER)" = "qa.tootsville.org" ]; \
	then \
		cp www/index.qa.html dist/www.$(CLUSTER)/index.html ;\
	fi
	bin/shar-stream dist/ www.$(CLUSTER) www.$(CLUSTER)

predeploy-servers:	servers quicklisp-update-servers
	for host in users gossip world ;\
	do \
		echo " » Pre-deploy $$host.$(CLUSTER)" ;\
		mkdir -p dist/$$host.$(CLUSTER) ;\
		cp dist/htaccess.all/$$host.$(CLUSTER).htaccess dist/$$host.$(CLUSTER) || exit 6 ;\
		cp play/.well-known/assetlinks.json dist/$$host.$(CLUSTER) || exit 6 ;\
		rsync -essh -zar servers $$host.$(CLUSTER): ;\
		ssh $$host.$(CLUSTER) make -C servers clean || exit 6 ;\
		ssh $$host.$(CLUSTER) make -C servers Tootsville || exit 6 ;\
		ssh $$host.$(CLUSTER) make -C servers test || exit 6 ;\
		echo " » Deploying static HTML bits of $$host.$(CLUSTER)" ;\
		rsync -zar -essh dist/$$host.$(CLUSTER) $$host.$(CLUSTER):/var/www/ ;\
		rsync --exclude='*~' --exclude='*#' -zar -essh --delete \
			www/error $$host.$(CLUSTER):/var/www/$$host.$(CLUSTER) ;\
		scp www/favicon.??? $$host.$(CLUSTER):/var/www/$$host.$(CLUSTER) ;\
	done

quicklisp-update-servers:
	for host in users gossip world; \
	do \
		echo " » Ensure latest Quicklisp on $$host.$(CLUSTER)" ;\
	    ssh $$host.$(CLUSTER) \
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
		git remote add goethe goethe.tootsville.org:devel/git/tootsville.org ;\
	fi


git-tag-deployment:
	VERSION=$$(servers/Tootsville version-info version) ;\
	now=$$(date +%Y-%m-%d) ;\
	msg="Deployed v$$VERSION to $$CLUSTER $$now" ;\
	if git rev-parse v$$VERSION &>/dev/null ;\
	then \
	    echo "Previous tag v$$VERSION found, adding v$$VERSION-$$now" ;\
	    if git rev-parse v$$VERSION-$$now &>/dev/null ;\
	    then \
	        now=$$(date +%Y-%m-%d.%H:%M) ;\
	        msg="Deployed v$$VERSION to $$cluster $$now" ;\
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

	git push --tags origin
	git submodule foreach git push --tags origin
	git push --tags github
	git submodule foreach git push --tags github
	git push --tags gitlab
	git submodule foreach git push --tags gitlab
	git push --tags goethe
	git submodule foreach git push --tags goethe

#################### deploy-docs

	make -C servers doc-publish
	scp dist/htaccess.goethe goethe.tootsville.org/goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org/goethe.tootsville.org/
	rsync -essh www/error goethe.tootsville.org/goethe.tootsville.org/

