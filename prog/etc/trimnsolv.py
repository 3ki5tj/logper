#!/usr/bin/env python

''' trim the output `nsolv.txt' of mknsolv.py '''

import re, sys, os


fnin = "nsolv.txt"
if len(sys.argv) >= 2:
  fnin = sys.argv[1]

sa = open(fnin).read().strip()
# indented lines are attached to the previous ones
sa = re.sub(r"\n ", " ", sa)
sa = sa.split('\n')
ls = []

for k in range(len(sa)):
  s = sa[k].strip("{}() ")
  s = re.sub(r"\(", ",", s)
  s = re.sub(r"\)", ",", s)
  s = s.split(",")
  try:
    item = s[1].strip()
    ls = ls + [item]
    id = int(s[0].strip())
    if len(ls) != id:
      sys.stderr.write( "Warning: index mismatch: %s vs %s\n" % (id, len(ls)) )
  except:
    sys.stderr.write( "error occured %s\n" % s )
    raise Exception
s = "{" + ',\n'.join(ls) + "}"
print s
