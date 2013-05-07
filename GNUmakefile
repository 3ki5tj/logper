# backup the directory to usb, delete useless files
usb::
	rsync -avzL --delete-after --exclude="*~" --exclude=".*" --exclude="fit.log" \
	  --exclude="*ls*.txt" --exclude="etc" \
	  * /media/C3/logper/

clean:
	find . -name "*~" | xargs rm
	rstrip.py -R *.tex *.html *.[ch] *.py *.ma *.gp *akefile

submkdirs = logperiod logperiod/prog/figtree \
	  doc doc/fig prog

deepclean: clean
	-for d in $(submkdirs); do ($(MAKE) -C $$d clean ); done

