#!/usr/bin/env python

''' make a mathematica script for numeric solutions of the logistic and cubic maps

      mknsolv.py file [prec] [wprec] [fnbra] [kmin] [kmax] [tmfindroot] [tmfactor]

    where
      prec is the target precision
      wprec is the working precision
      fnbra is a saved bracket file
      kmin is the smallest root index
      kmax is the largest root index (inclusive)
      tmfindroot is the maximal time for time out
      tmfactor is the maximal time to try to factor
    the output is nsolv.ma, to run it type

      math < nsolv.ma

    which writes nsolv.txt and possibly solvT.txt
    for harder problem the onset
      mknsolv.py T11a.txt 20 1000 fnbra
      math < nsolv.ma
'''
import os, sys, re

if len(sys.argv) <= 1:
  print "require an argument"
  exit(1)

input = sys.argv[1]

prec = 11
wprec = 100
fnbra = None
kmin = kmax = None
tmfindroot = 1000
tmfactor = 1

argid = 2
# precision
if len(sys.argv) > argid:
  prec = int(sys.argv[argid])
argid += 1

# working precision
if len(sys.argv) > argid:
  wprec = int(sys.argv[argid])
argid += 1

# brackets
if len(sys.argv) > argid:
  fnbra = sys.argv[argid]
  if not fnbra.endswith(".txt"):
    print "bracket file %s must end with .txt" % fnbra
    raise Exception
  fnbra = '"%s"' % fnbra # add quotes
argid += 1

if len(sys.argv) > argid:
  kmin = int(sys.argv[argid])
argid += 1
if len(sys.argv) > argid:
  kmax = int(sys.argv[argid])
argid += 1

if len(sys.argv) > argid:
  tmfindroot = int(sys.argv[argid])
argid += 1

if len(sys.argv) > argid:
  tmfactor = int(sys.argv[argid])
argid += 1

s = open(input).read().strip(" \n\r")
if s.endswith("}"):
  id = s.rfind("{")
  if id < 0:
    print "corrupted"
    raise Exception
  s = s[:id].strip()

# construct a one-line equation file for fast loading
s = re.sub(r"\\\n", "", s)
s = re.sub(r"\n", "", s)
open("eq.txt", "w").write(s)

s = 'tm = Timing[eq = xload["eq.txt"];][[1]];\nPrint["Equation loaded in ", tm, "s"];\n'


vars = """
append = False; (* we do not append! *)
prec = %s;
wprec = %s;
fnbra = %s;
kmin = %s;
kmax = %s;
tmfindroot = %s;
tmfactor = %s;
""" % (prec, wprec, fnbra, kmin, kmax, tmfindroot, tmfactor)

xsave = """
xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];

xload[fn_, verbose_: False] := Module[{fp, xp},
  If[verbose, Print["reading ", fn]];
  fp = OpenRead[fn]; xp = Read[fp, Expression]; Close[fp]; xp];
"""

# The code here are more conservative, and saves a lot of data
nsolve = r"""
nsolve[ieq_, x_, prec_: 100] := Module[{k, eq, sols},
  eq = If[Head[ieq] === Equal, ieq, ieq == 0];
  sols = NSolve[eq, x, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]
];
(* nsolve[x^3 + 2 x -1, x] *)

(* solve by bisection, a fallback *)
bsolv0[poly_, x_, xmin_, xmax_, prec_: 10] := Module[{k, eps = 10.0^(-prec),
    x1 = xmin, x2 = xmax, xm, y1, y2, ym, np = 10},
  y1 = N[poly/.{x->x1}, np]; y2 = N[poly/.{x->x2}, np];
  For[k = 0, x2 - x1 > eps, k++,
    ClearSystemCache[];
    xm = (x1 + x2)/2;
    ym = N[poly/.{x->xm}, np];
    If[ym == 0, Return[xm]];
    If[ym*y1 < 0, {x2,y2} = {xm,ym},
      If[ym*y2 < 0, {x1,y1} = {xm,ym},
        Print["Error: ", {{x1, y1}, {xm, ym}, {x2, y2}}];
        Return[xm];
      ];
    ];
  ];
  Print["bsolv converged after ", k, " iterations"];
  (* assuming x is about 1 *)
  N[(x1 + x2)/2, prec]
];

(* find a small rational approximation of x within del with a small denominator *)
farey[x_, del_] := Module[{ix, f, a, b, c, d, m, n},
  ix = Floor[x]; f = x - ix;
  {a, b} = {0, 1}; {c, d} = {1, 1};
  While[True,
    {m, n} = {a + c, b + d};
    If[Abs[m/n - f] < del, Break[]];
    If[f > m/n, {a, b} = {m, n}, {c, d} = {m, n}];
    (*Print["m/n ", m/n, " dx ", N[m/n - f,4]];*) ];
  ix + m/n];

(* solve by secant method *)
bsolv[poly_, x_, xmin_, xmax_, prec_: 10] := Module[{k, eps = 10.0^(-prec),
    x1 = xmin, x2 = xmax, xm, y1, y2, ym, np = 10, rat, grid = 128, mrg = 1 + 1},
  y1 = N[poly/.{x->x1}, np]; y2 = N[poly/.{x->x2}, np];
  For[k = 0, x2 - x1 > eps, k++,
    ClearSystemCache[];
    del = (x2 - x1)/grid;
    rat = y1/(y1 - y2);
    xm = farey[x1 + rat (x2 - x1), del];
    If[xm <= x1, xm = farey[x1 + mrg del, del]; Print["hit left, k: ", k]];
    If[xm >= x2, xm = farey[x2 - mrg del, del]; Print["hit right, k: ", k]];
    ym = N[poly/.{x->xm}, np];
    Print["k: ", k, ",  x: ", {x1, xm, x2}, ", ", N[{x1, xm, x2}, prec], ", ", N[{y1, ym, y2}, 3], ", eps ", N[(x2-x1)/eps,3]];
    If[ym == 0, Return[xm]];
    If[ym*y1 < 0, {x2,y2} = {xm,ym},
      If[ym*y2 < 0, {x1,y1} = {xm,ym},
        Print["Error: ", {{x1, y1}, {xm, ym}, {x2, y2}}];
        Return[xm];
      ];
    ];
  ];
  Print["bsolv converged after ", k, " iterations"];
  (* assuming x is about 1 *)
  N[(x1 + x2)/2, prec]
];

rsolv[poly_, x_, prec_:10, wprec_:100, fnbra_:None, svbra_:False,
      k0_:None, k1_:None, fnout_:None] :=
    Module[{intv, k, ls = {}, sol, x1, x2, y, kmin = k0, kmax = k1, err},

  (* computing or load bracket file *)
  If[fnbra === None,
    y = Timing[intv = RootIntervals[poly][[1]];][[1]];
    Print[Length[intv], " intervals determined ", intv, " in ", y, "s"];
    If[Length[intv] > 0 && svbra, xsave["intv.txt", intv, True, True]],
    intv = xload[fnbra, True];
    Print["Previous intervals loaded from ", fnbra]];
  If[kmin === None, kmin = 1];
  If[kmax === None, kmax = 1000000];
  kmax = Min[kmax, Length[intv]];
  Print["root range: ", {kmin, kmax}];

  (* starting solving roots *)
  For[k = kmin, k <= kmax, k++,
    ClearSystemCache[];
    {x1, x2} = intv[[k]];
(*
    sol = bsolv[poly, x, x1, x2, prec];
*)

    err = False; sol = None;
    TimeConstrained[Check[sol = x /. FindRoot[poly,{x, (x1+x2)/2, x1, x2},
        AccuracyGoal -> Infinity, PrecisionGoal -> prec,
        WorkingPrecision -> wprec, MaxIterations -> 1000],
      Print["FindRoot failed"]; err = True], tmfindroot,
      Print["FindRoot timeout"]; err = True];

    If[err, sol = bsolv[poly, x, x1, x2, prec]];

    sol = N[sol, prec];
    ls = Append[ls, sol];
    Print["sol ", k, " ", sol, " (", N[x1], ", ", N[x2], ") ", fnout];
    If[!(fnout === None), xsave[fnout, {k, sol, x1, x2}, True]; ];
  ];
  ls
];

(* solve equations for T = 4R for the logistic map *)
solveT[eq_, x_, fn_, fns_, prec_:10, wprec_:100, fnbra_:None, k0_:None, k1_:None] :=
    Module[{facs, l, k, sols, ls = {}, r, cnt, s, str = "", fp},
  TimeConstrained[ (* avoid factorization if it takes too long *)
    Print["time to factorize ", Timing[facs = FactorList[eq];][[1]]],
    tmfactor, Print["factorization took too long"]; facs = {{eq, 1}}];
  ClearSystemCache[];
  For[l = 1, l <= Length[facs], l++,
    Print["handling factor ", l, " ", x, "^", Exponent[facs[[l]][[1]],x]];
    (*sols = nsolve[facs[[l]][[1]], x, prec];*)
    sols = rsolv[facs[[l]][[1]], x, prec, wprec,
                 If[l == Length[facs], fnbra, None],
                 If[l == Length[facs], True, False],
                 k0, k1, fns];
    For[cnt = 0; k = 1, k <= Length[sols], k++; cnt++,
      ls = Append[ls, sols[[k]]];
      r = 1 + Sqrt[1 + sols[[k]]];
      s = If[cnt == 0, "solutions of T^"<>ToString[ Exponent[facs[[l]][[1]], T] ]<>" ...:\n", ""];
      s = s<>"r: "<>ToString[N[r,prec]]<>", r/4: "<>ToString[N[r/4,prec]]
           <>"; 4R: "<>ToString[N[sols[[k]],prec]]<>", R: "<>ToString[N[sols[[k]]/4,prec]];
      Print[s];
      str = str<>s<>"\n";
    ]
  ];
  If[!(fn === None), fp = OpenWrite[fn]; WriteString[fp, str]; Close[fp]];
  ls
];
"""

src_log = """
Timing[sol = solveT[eq, T, "solvT.txt", "nsolv.txt", prec, wprec, fnbra, kmin, kmax];][[1]]
"""

src_cubic = """
Timing[sol = rsolv[eq, r, prec, wprec, fnbra, True, kmin, kmax, "nsolv.txt"];][[1]]
"""

if input.startswith("cr"):
  print "write code for cubic map"
  src = src_cubic
elif input.startswith("T"):
  print "write code for logistic map"
  src = src_log
else:
  print "bad input", input

src = vars + xsave + nsolve + s + src
open("nsolv.ma", "w").write(src)

