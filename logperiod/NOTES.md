To upload (make upload)
```
gcloud app deploy --project=logperiod app.yaml
```

To test locally (make local)
```
dev_appserver.py logperiod
```

Dashboard
http://appengine.google.com/

The getting started tutorial
http://code.google.com/appengine/docs/python/gettingstarted/


To synchronize
```
rsync -avz ../logperiod /media/C3/logper/
```
