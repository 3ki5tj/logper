prj=~/work/logper/logperiod

local::
	cd ~/software/google_appengine && \
	echo "the website can be viewed as localhost:8080" && \
	./dev_appserver.py $(prj)

upload::
	cd ~/software/google_appengine && \
	./appcfg.py update $(prj)

usb::
	rsync -avz --exclude="*~" --exclude=".*" --exclude="fit.log" \
	  --exclude="*ls*.txt"  --exclude="etc" \
	  ../$(prj) /media/C3/logper

htmls = $(wildcard *.html)

check: $(htmls)
	for d in $(htmls); do (aspell -H check $$d ); done

clean:
	find . -name "*~" | xargs rm -f
	find . -name ".*.un~" | xargs rm -f
	rstrip.py -Rv

