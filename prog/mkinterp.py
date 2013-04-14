#!/usr/bin/env python

''' make a mathematica script for interpolation from a list file
    the output is fit.ma, to run it type
      math < fit.ma >/dev/null
    which writes fit.txt
'''
import os, sys, re

if len(sys.argv) <= 1:
  print "require an argument"
  exit(1)

input = sys.argv[1]

kmin = kmax = None
argid = 2
if len(sys.argv) > argid:
  kmin = int(sys.argv[argid])
  if kmin <= 1: kmin = 1
argid += 1
if len(sys.argv) > argid:
  kmax = int(sys.argv[argid])
  if kmax < kmin:
    print "kmax %s must be >= kmin %s" % (kmin, kmax)
    raise Exception
argid += 1
print kmin, kmax


s = open(input).read().strip("{} \n\r")
s = [a.strip(" \n{").split(", ") for a in s.split("}")]
n = len(s)
print "list contains", n, "items"

for i in range(n):
  #s[i][0] = int(s[i][0])  # note, s[i][0] might be expression like -101/4, so not int
  # only for checking parity
  #s[i][1] = re.sub(r"[\\\n]", "", s[i][1])
  s[i][1] = re.sub(r"\\\n", "", s[i][1])

'''
# add mirrors
t = []
for i in range(n):
  id = s[i][0]
  if id <= 0: continue
  for j in range(i):
    if s[j][0] == -id:
      break
  else:
    print "adding ", -s[i][0]
    t = [[-s[i][0], s[i][1]]] + t
s = t + s
str = ''.join(  [("{%s, %s}\n" % (l[0], l[1])) for l in s]  )
open("xxx.txt", "w").write(str)
'''


'''
# check parity
for i in range(n):
  id = s[i][0]
  if id >= 0: break
  for j in range(i+1, n):
    if s[j][0] == -id:
      break
  else:
    continue

  if s[i][1] == s[j][1]:
    pass #print "i %s, id %s checked" % (i, id)
  else:
    print "i %s, id %s bad" % (i, id)
'''


ls = "ls = {"
for i in range(n):
  ls += "{%s, %s},\n" % (s[i][0], s[i][1])
ls = ls.rstrip(",\n") + "};\n"

# mathematica code for interpolation

xsave = """
  xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];
"""

# logistic map
fit_log = """
p = Numerator[Together[InterpolatingPolynomial[ls, R]/.{R->T/4}]];
xsave["fit.txt", p, False, True];
Print["trying to factorize..."];
Timing[p = Factor[p];][[1]];
xsave["fit.txt", p, False, True];
"""

# cubic map
fit_cubic = """
p = Numerator[Together[InterpolatingPolynomial[ls, r]]];
xsave["fit.txt", p, False, True];
Print["trying to factorize..."];
Timing[p = Factor[p];][[1]];
xsave["fit.txt", p, False, True];
"""

# Henon map
fit_henon = """
(* interpolate polynomials of b *)
interp[xy_, a_, b_, A_:None, k1_:None, k2_:None, fntmp_:None] := Module[
    {n = Length[xy], p, p1, ls, k, kmin = k1, kmax = k2},
  p = Table[xy[[l]][[2]], {l, n}];
  If[kmin === None, kmin = 1];
  If[kmax === None, kmax = Max[Exponent[p, b]]+1];
  ls = Table[PadRight[CoefficientList[Expand[p[[k]]],b], kmax+1], {k, n}];
  For[p = 0; k = kmin, k <= kmax, k++,
    p1 = InterpolatingPolynomial[ Table[{xy[[l]][[1]], ls[[l]][[k]]}, {l, n}], a ];
    If[!(A === None), p1 = p1/.{a->A/4}];
    p1 = Factor[p1] b^(k-1);
    xsave[fntmp, {k, p1}, True];
    p += p1;
  ]; p];

kmin = %s;
kmax = %s;
p = interp[ls, a, b, A, kmin, kmax, "fittmp.txt"];
If[kmin === None && kmax === None,
  %s
  xsave["fit.txt", p];
];
"""

src = ls + xsave
if input.startswith("hls"):
  print "write code for Henon map"
  if input.find("b.txt") >= 0:
    src += fit_henon % (kmin, kmax, "")
  else:
    src += fit_henon % (kmin, kmax, "p = Factor[p];")
elif input.startswith("cls"):
  print "write code for cubic map"
  src += fit_cubic
else:
  print "write code for logistic map"
  src += fit_log

open("fit.ma", "w").write(src)

