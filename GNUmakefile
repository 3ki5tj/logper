# backup the directory to usb, delete useless files

# use --size-only to avoid transfering large files
usb::
	rsync -avzL --delete-after --size-only \
	  --exclude="*~" --exclude=".*" --exclude="fit.log" \
	  * /media/C3/logper/

clean:
	find . -name "*~" | xargs rm
	rstrip.py -Rv

submkdirs = logperiod logperiod/prog/figtree \
	  doc doc/fig prog

deepclean: clean
	-for d in $(submkdirs); do ($(MAKE) -C $$d clean ); done

