#!/usr/bin/env python

import re, sys, os


fnin = "inp.txt"
if len(sys.argv) >= 2:
  fnin = sys.argv[1]

sa = open(fnin).read().strip()
sa = re.sub(r"\n ", " ", sa)
sa = sa.split('\n')
ls = []
lsb = []
for k in range(len(sa)):
  s = sa[k].strip("{}() ")
  s = re.sub(r"\(", ",", s)
  s = re.sub(r"\)", ",", s)
  s = s.split(",")
  try:
    item = s[1].strip()
    ls = ls + [item]
    id = int(s[0].strip())
    #if len(ls) != id:
    #  print "Index mismatch: %s vs %s" % (id, len(ls))
    #print s[0].strip(), item
    if item.startswith("-"):
      lsb += [ item[1:] ]
  except:
    print "error occured %s" % s
    raise Exception

lsb.reverse()
ls = ls + lsb
s = "{" + ',\n'.join(ls) + "}"
print s
