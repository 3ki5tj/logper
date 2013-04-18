usb::
	rsync -avzL --exclude="*~" --exclude=".*" --exclude="fit.log" \
	  --exclude="*ls*.txt" --exclude="etc" \
	  * /media/C3/logper/

clean:
	rm -f *~ data/*~
	find . -name "*~" | xargs rm

submkdirs = logperiod logperiod/prog/figtree \
	  doc doc/fig

deepclean: clean
	-for d in $(submkdirs); do ($(MAKE) -C $$d clean ); done

