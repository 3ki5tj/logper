#!/usr/bin/env python

''' make a Magma script for interpolation from a list file
    the output is fit.magma, to run it type
      magma < fit.magma > fit.txt
'''
import os, sys, re

if len(sys.argv) <= 1:
  print "require an argument"
  exit(1)

fninp = sys.argv[1]

s = open(fninp).read().strip("{} \n\r")
s = [a.strip(" \n{").split(", ") for a in s.split("}")]
n = len(s)
print "list contains", n, "items"

for i in range(n):  # remove line-continuations
  s[i][1] = re.sub(r"\\\n", "", s[i][1])

lsx = lsy = ""
delim = ", "
for i in range(n):
  lsx += "%s*4%s" % (s[i][0], delim)
  lsy += "%s%s" % (s[i][1], delim)
lsx = lsx.rstrip(delim)
lsy = lsy.rstrip(delim)
src = """Q := RationalField();
P<T> := PolynomialRing(Q, 1);
x := [Q | %s];
y := [P | %s];
print "loaded";
time f := Interpolation(x, y, 1);
print "interpolation is done";
Write("fit.txt", f : Overwrite := true);
time ff := Factorization(f);
print "factorization is done";
Write("fit.txt", ff : Overwrite := true);
""" % (lsx, lsy);

fnout = "fit.magma"
print "writing", fnout
open(fnout, "w").write(src)

