#!/usr/bin/env python

import os, sys

def mkpbs(id, kmin, kmax, ch="a", n=13):
  return '''#!/bin/bash
#PBS -N cub%s%s%s
#PBS -l nodes=1:ppn=1,walltime=24:00:00
#PBS -V

#which math
#echo "My job ran on: "
#cat $PBS_NODEFILE
cd $PBS_O_WORKDIR
pwd
math < cub0.ma %s %s %s %s > /dev/null
''' % (n, ch, id, n, ch, kmin, kmax)

def lsnm(n, ch):
  return "cls%s%s0.txt" % (n ,ch)

def getkminmax(ls, i):
  ''' (kmin, kmax) '''
  deg = 3392
  base = -deg/2
  step = 106
  kmin = base + step*(i - 1)
  kmax = base + step*i
  try:
    if ls != None:
      s = open(ls, "r").read()
      id = s.rfind("{") # search from the end
      kmin = eval( "1*" + s[id+1:].split(", ")[0] ) + 1
  except:
    pass
  if i == 32: kmax = deg + 2 + base + 5
  return kmin, kmax

def reprod(ch, n):
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.system("mkdir %s" % dir);
    os.chdir(dir)
    os.system("ln -sf ../test/cmats%s.txt ." % n)
    os.system("ln -sf ../test/cub0.ma .")
    kmin, kmax = getkminmax(None, i)
    str = mkpbs(i, kmin, kmax, ch, n);
    open("foo.pbs", "w").write(str)
    os.system("chmod 755 foo.pbs")
    os.chdir("..")

def collect(ch, n):
  ''' collect lists '''
  str = ""
  ls = lsnm(n, ch)
  cnt = 0
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.chdir(dir)
    try:
      s = open(ls, "r").read()
      cnt += s.count("{")
      str += s
    except:
      print "cannot open %s" % (ls)
    os.chdir("..")
  open(ls, "w").write(str)
  print "counted %d solutions" % cnt

def modifypbs(ch, n):
  print "trimming pbs"
  tot = 0
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.chdir(dir)
    ls = lsnm(n, ch)
    try:
      kmin, kmax = getkminmax(ls, i)
      cnt = kmax - kmin
      tot += cnt
      if cnt <= 0:
        print "folder %s is done" % dir
        os.chdir("..")
        continue
      str = mkpbs(i, kmin, kmax, ch, n)
      open("foo.pbs", "w").write(str)
      print (kmin, kmax, cnt)
    except:
      print "cannot open %s" % (ls)
    os.chdir("..")
  print "%s solutions to do" % tot

def submit(ch, n):
  c = raw_input("will now submit jobs?").strip().lower()
  if not c.startswith("y"): return
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.chdir(dir)
    ls = lsnm(n, ch)
    kmin, kmax = getkminmax(ls, i)
    if kmax > kmin:
      os.system("qsub foo.pbs")
    os.chdir("..")

def desubmit(ch, n):
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.system("mqsub -D cub%s%s%s" % (n, ch, i))

def clean(ch, n):
  for i in range(1, 33):
    dir = "%s%s%s" % (n, ch, i)
    os.chdir(dir)
    os.system("rm -f *.[eo]*")
    os.chdir("..")

n=8
ch="b"
#reprod(ch, n)
#clean(ch, n)
modifypbs(ch, n)
collect(ch, n)
#submit(ch, n)
#desubmit(ch, n)

