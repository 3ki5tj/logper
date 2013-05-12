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

clean:
	rm -f *~ data/*~
	rstrip.py -R *.tex *.html *.c *.h *.py *.ma *.gp *akefile *.css *.js

