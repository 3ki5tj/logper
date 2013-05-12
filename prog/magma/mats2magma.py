#!/usr/bin/env python

import os, sys, re
from numbthy import *

if len(sys.argv) <= 1:
  print "need an input file"
  exit(1)

matpfx = "mats"
fn = sys.argv[1];
fn0 = os.path.splitext(fn)[0]
i =  fn0.rfind(matpfx)
if i < 0:
  print "unknown file name", fn
n = int(fn0[len(matpfx):])


def skipsp(buf, i):
  ''' skip spaces '''
  n = len(buf)
  while i < n and buf[i].isspace(): i += 1
  return i


def readch(buf, i, ch):
  ''' skip space until an excepted character is read '''
  i = skipsp(buf, i)
  if i >= len(buf):
    print "EOF met, cannot read", ch
    raise Exception
  if buf[i] != ch:
    print "expected %s got '%s' at %d" % (ch, buf[i:i+15], i)
    raise Exception
  return i+1


def readchopt(buf, i, ch):
  ''' skip optional character '''
  i = skipsp(buf, i)
  if i < len(buf) and buf[i] == ch: i += 1
  return i


def readmats(fn, n):
  ''' read the matrix from mathematica '''
  buf = open(fn).read().strip()[1:-1]
  buf = re.sub(r'\s', '', buf)
  divs = divisors(n)
  fid = 0
  mats = [0]*len(divs)
  for k in range(len(divs)):
    d = divs[k]
    fid = readch(buf, fid, "{")
    Nd = necklace(d)
    #print d, Nd
    # read the matrix for d
    mat = [0]*Nd
    for i in range(Nd): # reading ith row
      fid = readch(buf, fid, "{")
      fjd = buf.find("}", fid)
      if fjd < 0: raise Exception
      mat[i] = [c.strip() for c in buf[fid:fjd].split(",")]
      fid = readchopt(buf, fjd+1, ",")
    mats[k] = mat
    fid = readch(buf, fid, "}")
    fid = readchopt(buf, fid, ",")
  return mats


def mkintmat(mat, n):
  ''' transform the matrix so its on integer domain '''
  dm = len(mat)
  num = "2^%s" % n
  nnum = "-2^%s" % n
  if n % 2 == 0:
    num, nnum = nnum, num;
  for i in range(dm):
    for j in range(dm):
      aij = mat[i][j]
      hasX = 0
      sgn = 1
      if aij.find("X") >= 0:
        hasX = 1
        if aij.find("-X") >= 0:
          aij = re.sub("-X", "", aij)
          sgn = -1
        else:
          aij = re.sub("X", "", aij)
      if aij == "" or aij == "0":
        pass
      elif aij == "1":
        aij = num
      elif aij == "-1":
        aij = nnum
      elif len(aij) == 1:
        aij = "%s*%s" % (num, aij)
      else:
        aij = "%s*(%s)" % (num, aij)
      if hasX:
        if sgn > 0: aij += "+X";
        else: aij += "-X";
      mat[i][j] = aij
  return mat

def mat2magma(mat):
  ''' convert to linear string '''
  dm = len(mat)
  s = ""
  for i in range(dm):
    for j in range(dm):
      s += mat[i][j] + ","
  return "[" + s.strip(", ") + "]"

def tomagma(mats, n):
  divs = divisors(n)
  str = "";
  for k in range(len(divs)):
    d = divs[k]
    mat = mats[k]
    Nd = necklace(d)
    mat = mkintmat(mat, d)
    mat = mat2magma(mat) # magma format
    str += "fmat%d := func<R, X| Matrix(PolynomialRing(Ring0), %s, %s, %s)>;\n" % (
        d, Nd, Nd, mat);
  return str

def mkcodesimple(n,delta=1):
  return '''
P<R> := PolynomialRing(Ring0);
time f := Determinant(fmat%s(R, %s));
time f := Factorization(f);
print f;''' % (n, delta)

def mkcodeinter(n, delta=1):
  ''' magma interpolation code '''
  imax = (necklace(n)*n // 2) + 1
  str = '''
Q1 := RationalField();
P1<T> := PolynomialRing(Q1);
xs := [Q1|];
ys := [Q1|];
for i in [1 .. %s] do
  x := (i - %s)/2;
  y := Determinant(fmat%s(x, %s));
  if 1 eq 1 then
    xs := Append(xs, x*4);
    ys := Append(ys, y);
  end if;
end for;
time f := Interpolation(xs, ys);
time f := Factorization(f);
''' % (imax, imax/2, n, delta)
  return str


mats = readmats(fn, n)
str = "Ring0 := IntegerRing();\n";
str += tomagma(mats, n)
str += mkcodesimple(n)  # 84s for n = 9
#str += mkcodeinter(n)  # 90s for n = 9
print str

