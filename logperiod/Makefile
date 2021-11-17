prj=~/work/logper/logperiod

upload::
	gcloud app deploy --project=logperiod app.yaml

local::
	dev_appserver.py $(prj)

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

