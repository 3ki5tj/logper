#!/usr/bin/env python

''' script of calling bmp.ma and bmp.c '''

import os, sys, subprocess


def run(cmd):
  sp = subprocess.Popen(["/bin/bash",  "-i", "-c", cmd])
  sp.communicate()


mathcode = "bmp.ma"
if not os.path.exists(mathcode): raise Exception

ccode = "bmp.c"
exeprog = "bmp.exe"
if not os.path.exists(exeprog):
  if not os.path.exists(ccode): raise Exception
  os.system("cc -O3 %s -o %s" % (ccode, exeprog))

for i in range(1, 15):
  for c in ("a", "b"):
    fnT = "T%s%s.txt" % (i, c)
    fnbit = "bit%s%s.txt" % (i, c)
    fnbmp = "T%s%s.bmp" % (i, c)
    fnpng = "T%s%s.png" % (i, c)
    print fnT, fnbit, fnbmp, fnpng
    if not os.path.exists(fnbit):
      run( "math < %s %s %s" % (mathcode, fnT, fnbit) )
    os.system("./%s %s %s" % (exeprog, fnbit, fnbmp))
    os.system("convert -compress Lossless %s %s" % (fnbmp, fnpng) )
