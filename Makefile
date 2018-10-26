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
	-rm dist/play/play.js
	-rm dist/play/worker.js
	-rm TODO.org TODO.scorecard	

#################### servers

servers:	servers/Tootsville

servers/Tootsville:	$(shell find servers \( -name \*.lisp -o -name \*.asd \) -and -not -name .\*)
	$(MAKE) -C servers Tootsville test

doc:
	$(MAKE) -C servers Toosville doc

#################### htaccess

htaccess:	htaccess.base bin/make-all-htaccess
	bin/make-all-htaccess

#################### play/worker.js

worker:	dist/play/worker.js

dist/play/worker.js:	play/worker.js
	mkdir -p dist/play/
	closure-compiler --create_source_map dist/play/worker.map \
		--language_out ECMASCRIPT5_STRICT \
		--source_map_location_mapping 'play/|/play/' \
		$< > $@
	echo '//# sourceMappingURL=/play/worker.map' >> dist/play/worker.js

#################### play/play.js

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
	ssh play.$(CLUSTER) "mv play.$(CLUSTER) play.$(CLUSTER).before-deploy && mv play.$(CLUSTER).new play.$(CLUSTER)"
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=play.$(CLUSTER) \
	     -F framework=deploy-cluster \
	     -F notifier.name=deploy-cluster \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

deploy-servers:	predeploy
	for host in users gossip world; \
	do \
		ssh $$host.$(CLUSTER) "cp servers/Tootsville --backup=simple -f /usr/local/bin/; \
cp servers/tootsville.service --backup=simple -f /usr/lib/systemd/user/; \
sudo -n systemctl enable tootsville; \
sudo -n systemctl restart tootsville; \
sudo -n systemctl start tootsville" ;\
		VERSION=$(shell servers/Tootsville version-info version) ;\
		curl https://api.rollbar.com/api/1/deploy/ \
		     -F access_token=$(ACCESS_TOKEN) \
		     -F environment=$$host.$(CLUSTER) \
		     -F framework=deploy-cluster \
		     -F notifier.name=deploy-cluster \
		     -F revision=$(REVISION) \
		     -F comment="v $(VERSION)" \
		     -F uuid=$(uuidgen) \
		     -F local_username=$(LOCAL_USERNAME) ;\
	done

deploy-www:	predeploy
	ssh www.$(CLUSTER) "mv www.$(CLUSTER) www.$(CLUSTER).before-deploy && mv www.$(CLUSTER).new www.$(CLUSTER)"
	curl https://api.rollbar.com/api/1/deploy/ \
	     -F access_token=$(ACCESS_TOKEN) \
	     -F environment=www.$(CLUSTER) \
	     -F framework=deploy-cluster \
	     -F notifier.name=deploy-cluster \
	     -F revision=$(REVISION) \
	     -F uuid=$(uuidgen) \
	     -F local_username=$(LOCAL_USERNAME)

predeploy:	no-fixmes connectivity predeploy-play predeploy-www predeploy-servers remotes

connectivity:
	ssh play.$(CLUSTER) ls -1d play.$(CLUSTER)/ | grep play.$(CLUSTER)
	ssh $(CLUSTER) ls -1d $(CLUSTER)/ | grep $(CLUSTER)
	ssh users.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'
	ssh gossip.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'
	ssh world.$(CLUSTER) sbcl --no-userinit --quit | grep 'This is SBCL'


no-fixmes:	TODO.scorecard
	TOOT_TODO = $$(grep TODO TODO.scorecard | wc -l) ;\
	TOOT_FIXME = $$(grep FIXME TODO.scorecard | wc -l) ;\
	if [[ $$TOOT_FIXME -gt 0 ]] ;\
	then \
		while true ;\
		do \
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
			if [[ "$${yorn_}" = "y" ]] ;\
			then \
				echo "" ;\
				echobig Overridden ;\
				echo "" ;\
				echo "Override accepted. Good luck …" ;\
				break ;\
			elif [[ "$${yorn_}" = "n" ]] ;\
			then \
				echo "" ;\
				echo "That seems wise. Exiting." ;\
				echo "" ;\
				exit 8 ;\
			fi ;\
		done ;\
	fi

predeploy-play:	play worker htaccess
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
	mkdir -p dist/www.$(CLUSTER)
	rsync --exclude='*~' --exclude='*#' -ar \
	      www/* dist/www.$(CLUSTER)/
	cp dist/www/2019.css dist/www.$(CLUSTER)/2019.css
	if [ "$(CLUSTER)" = "test.tootsville.org" ]; \
	then \
		cp www/index.test.html www.$(CLUSTER)/index.html ;\
	fi
	if [ "$(CLUSTER)" = "qa.tootsville.org" ]; \
	then \
		cp www/index.qa.html www.$(CLUSTER)/index.html ;\
	fi
	bin/shar-stream dist/ www.$(CLUSTER) www.$(CLUSTER)

predeploy-servers:	servers
	quicklisp-update-servers
	for host in users gossip world ;\
	do \
		mkdir -p dist/$$host.$(CLUSTER) ;\
		cp dist/htaccess.all/$$host.$(CLUSTER).htaccess dist/$$host.$(CLUSTER) || exit 6 ;\
		cp play/.well-known/assetlinks.json dist/$$host.$(CLUSTER) || exit 6 ;\
		bin/shar-stream ./ servers/ $$host.$(CLUSTER) ;\
		ssh $$host.$(CLUSTER) make -C servers clean Tootsville test || exit 6 ;\
		rsync -zar -essh dist/$$host.$(CLUSTER) $$host.$(CLUSTER):/var/www/ ;\
		rsync --exclude='*~' --exclude='*#' -zar -essh --delete \
			www/error $$host.$(CLUSTER):/var/www/$$host.$(CLUSTER) ;/
		scp www/favicon.??? $$host.$(CLUSTER):/var/www/$$host.$(CLUSTER) ;/
	done

quicklisp-update-servers:
	for host in users gossip world
	do
	    ssh $$host.$(CLUSTER) \
	        sbcl --non-interactive \
	        --no-inform \
	        --eval "'(ql:update-client)'" \
	        --eval "'(ql:update-all-dists)'" \
	        --quit
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
	git push --tags gitlab
	git push --tags goethe

#################### deploy-docs

	make -C servers doc-publish
	scp dist/htaccess.goethe goethe.tootsville.org/goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org/goethe.tootsville.org/
	rsync -essh www/error goethe.tootsville.org/goethe.tootsville.org/

