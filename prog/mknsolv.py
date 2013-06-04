#!/usr/bin/env python

''' write a Mathematica script for numeric solutions of the logistic and cubic maps,
    Cheng Zhang Copyright (c) 2012-2013
    Quick usage example:
    Type
      mknsolv.py T14a.txt
    then
      math < nsolv.ma
    Type `mknsolve.py -h' for details '''

import os, sys, re, getopt

fninp = None  # input polynomial equation
fnout = "nsolv.ma" # output: Mathematica script
fnres = "nsolv.txt" # root file, output of the Mathematica script
fnresT = "solvT.txt" # special root file for the logistic map
                     # output of the Mathematica script
fneq = "eq.txt" # temporary one-line equation for fast loading
fnbra = "intv.txt" # root brackets

prec = 21
wprec = None # WorkingPrecision for FindRoot[]
kmin = kmax = None  # starting and ending root index
tmroot = 1000  # this is based on T14a/b.txt data
               # roughly the time needed for bsolv[] to get a root
               # FindRoot[] usually takes 200~300s (wprec = 8000)
nwarmup = 0 # number of steps for refining brackets before calling FindRoot[]
            # the warm-up step computes the derivative, so it can be expensive
verbose = False


def usage():
  ''' print usage and die '''

  print sys.argv[0], "[OPTIONS] input"
  print """
    Write a Mathematica script for numeric solutions of the logistic and cubic maps
    `input' is the polynomial file, e.g., T14a.txt

    OPTIONS:

    -o, --output    output Mathematica script, default: {{fnout}}
    -O, --result    root file, default: {{fnres}}
    -T, --resultT   extra root file of the logistic map, default: {{fnresT}}
    -q, --eq        temporary equation file for fast loading, default: {{fneq}}
    -b, --bra       root-brackets file, default: {{fnbra}}

    -p, --prec      the target precision
    -w, --wprec     the WorkingPrecision for FindRoot[]
                    reducing from the default value may accelerate the process
                    but if it runs too fast, it may be the wrong answer
    -0, --kmin      smallest root index
    -1, --kmax      largest root index (inclusive)
    -R, --tmroot    the maximal time for FindRoot[]
    -W, --warmup    number of steps of refining the window before calling FindRoot[]
                    the also checks the validity of the window
                    to disable it completely, set `--warmup=-1'
    -v, --verbose   make the Mathematica script more verbose

    The output is {{fnout}}, to run it type

      math < {{fnout}}

    which writes {{fnres}} and possibly {{fnresT}} (logistic map).
    It also saves a root bracket file {{fnbra}}.

    For harder problem, the first run may not be able to finish.
    On the second run, the script will load the root-brackets file,
    instead of compute it, so it will run much faster.
    The user only need to specify (kmin, kmax)

    Note:
    1. The python script will first write a compact one-line equation
       "eq.txt" of the input to facilitate fast loading
       This is the main reason of using a Python layer.

    2. The Mathematica script will first find the intervals of all roots
       by calling the Mathematica function RootIntervals[].
       This step can take some time, but result is saved to {{fnbra}}
       which will be loaded in the next time.

    3. In finding roots, we first try FindRoot[].  If it fails, we use
       the more stable but slower secant method bsolv[].
       If latter is preferred in all cases, set --tmroot=0.

    4. No factorization of the input polynomial is attempted.
       However, if the input is already factorized,
       only the last factor is written into the single-line equation file

    5. By running prog/etc/trimnsolv.py on `nsolv.txt'
       a list of roots is generated, which can be appended
       at the end of files like T13a.txt

  """.replace("{{fnout}}", fnout
    ).replace("{{fnres}}", fnres
    ).replace("{{fnresT}}", fnresT
    ).replace("{{fneq}}", fneq
    ).replace("{{fnbra}}", fnbra
    )
  exit(1)



def doargs():
  ''' Handle common parameters from command line options '''

  try:
    opts, args = getopt.gnu_getopt(sys.argv[1:], "ho:O:q:b:p:w:0:1:R:W:v",
         [ "help",
           "output=", "eq=", "result=", "bra=",
           "prec=", "wprec=", "kmin=", "kmax=", "tmroot=",
           "warmup=", "verbose",
         ])
  except getopt.GetoptError, err:
    # print help information and exit:
    print str(err) # will print something like "option -a not recognized"
    usage()

  global fninp, fnout, fneq, fnres, fnresT, fnbra
  global prec, wprec, kmin, kmax, tmroot, nwarmup, verbose

  if len(args) >= 1:
    fninp = args[0]
  else:
    print "need an input file"
    usage()

  for o, a in opts:
    if o in ("-o", "--output",):
      fnout = a
    elif o in ("-O", "--result",):
      fnres = a
    elif o in ("-T", "--resultT",):
      fnresT = a
    elif o in ("-q", "--eq",):
      fneq = a
    elif o in ("-b", "--bra",):
      fnbra = a
    elif o in ("-p", "--prec",):
      prec = int(a)
    elif o in ("-w", "--wprec",):
      wprec = int(a)
    elif o in ("-0", "--kmin",):
      kmin = int(a)
    elif o in ("-1", "--kmax",):
      kmax = int(a)
    elif o in ("-R", "--tmroot",):
      tmroot = float(a)
    elif o in ("-W", "--warmup",):
      nwarmup = int(a)
    elif o in ("-v", "--verbose",):
      verbose = True
    elif o in ("-h", "--help",):
      usage()



def eqload(fninp, fneq):
  """ load the input file, e.g. T13a.txt or cr8b.txt
      write a single-line polyomial """

  s = s0 = open(fninp).read().strip(" \n\r")
  # remove the existing solutions
  if s.endswith("}"):
    id = s.rfind("{")
    if id < 0:
      print "corrupted"
      raise Exception
    s = s[:id].strip()

  # if it has multiple factors, only do the last factor
  if s.endswith(")"):
    id = s.rfind("(")
    if id >= 0:
      s = s[id+1:].strip(" )")
      print "Eq. truncation: %s will have only the largest factor %s => %s bytes" % (
          fneq, len(s0), len(s))

  # construct a one-line equation file for fast loading
  # also remove the trailing "\", the line-continuation mark
  s = [ ln.strip().rstrip("\\").replace(" ", "") for ln in s.split("\n") ]
  s = ''.join(s) + "\n"
  open(fneq, "w").write(s)



# template of the python script
templ = r"""(* numerical root finder, generated by mknsolv.py *)
fninp = "{{fninp}}";  (* original input polynomial, not used *)
fneq = "{{fneq}}";  (* one-line polynomial file *)
fnres = "{{fnres}}";  (* output root file *)
fnresT = "{{fnresT}}";  (* output root file *)
fnbradef = "{{fnbradef}}";
fnbra = "{{fnbra}}";

precdef = {{prec}};
prec = {{prec}};
wprec = {{wprec}};
kmin = {{kmin}};
kmax = {{kmax}};
tmroot = {{tmroot}};
verbose = {{verbose}};



xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If [ verbose,
    Print[ If [ append, "appending ", "writing " ], fn ]
  ];
  fp = If [ append, OpenAppend[fn], OpenWrite[fn] ];
  Write[fp, xp];
  Close[fp];
];

xload[fn_, verbose_: False] := Module[{fp, xp},
  If [ verbose, Print["reading ", fn] ];
  fp = OpenRead[fn];
  xp = Read[fp, Expression];
  Close[fp];
  xp
];



(* ********* Stable root searching function bsolv[] begins ********* *)

(* find a small rational approximation of x within del with a small denominator *)
farey[x_, del_] := Module[{ix, f, a, b, c, d, m, n},
  ix = Floor[x];
  f = x - ix;
  {a, b} = {0, 1};
  {c, d} = {1, 1};
  While [ True,
    {m, n} = {a + c, b + d};
    If [ Abs[m/n - f] < del, Break[]; ];
    If [ f > m/n, {a, b} = {m, n}, {c, d} = {m, n}];
    (*Print["m/n ", m/n, " dx ", N[m/n - f,4]];*)
  ];
  ix + m/n
];



(* a simple format for large numbers *)
sf[x_] := Module[{s = If [x < 0, "-", ""]},
  s <> "10^" <> ToString[ Round[Log10[Abs[x]], 0.1] ]
];



(* solve by secant method
   if `dydx', stops when the derivative makes sence *)
bsolv[poly_, x_, xmin_, xmax_, prec_: predef, dydx_ : None,
      verbose_: False, itermax_: 10000] :=
  Module[{it, eps = 0.1^prec,
    x1 = xmin, x2 = xmax, xm, y1, y2, ym, np = 5, slope, grid = 128, mrg = 2,
    dy1, dy2, dx, good},

  y1 = N[poly /. {x -> x1}, np];
  y2 = N[poly /. {x -> x2}, np];
  If [ y1*y2 >= 0,
    Print["bsolv same sign error, x: ", ({x1, x2} // InputForm),
        ", y: ", (N[{y1, y2}, 3] // InputForm)];
    Return[{None, None, None}];
  ];

  For [ it = 0, x2 - x1 > eps && it < itermax, it++,
    ClearSystemCache[];

    (* allow at least three iterations *)
    If [ !(dydx === None),
      (* see if the derivative makes sense *)
      good = True;
      dy1 = N[dydx /. {x -> x1}, np];
      If [ dy1 y1 >= 0 || -y1/dy1 >= x2 - x1,
        If [ verbose, Print["it ", it, ", bad der. y1 ", sf[y1], " dy1 ", sf[dy1]]; ];
        good = False;
      ];
      dy2 = N[dydx /. {x -> x2}, np];
      If [ good && (dy2 y2 >= 0 || -y2/dy2 >= x2 - x1),
        If [ verbose, Print["it ", it, ", bad der. y2 ", sf[y2], " dy2 ", sf[dy2]]; ];
        good = False;
      ];
      If [ good,
        Print["it ", it, ", good der. x ", ({x1, x2}//InputForm), " y ", {sf[y1], sf[y2]}, " dy ", {sf[dy1], sf[dy2]}];
        Break[]; (* derivative makes sense now *)
      ];
    ];

    del = (x2 - x1)/grid; (* error for x-interpolation *)
    slope = y1/(y1 - y2);
    xm = farey[x1 + slope (x2 - x1), del];
    If [ xm <= x1,
      xm = farey[x1 + mrg del, del];
      If [ verbose, Print["hit left, it: ", it]; ];
    ];
    If [ xm >= x2,
      xm = farey[x2 - mrg del, del];
      If [ verbose, Print["hit right, it: ", it]; ];
    ];
    ym = N[poly/.{x->xm}, np];
    If [ verbose,
      Print["it: ", it, ",  x: ", ({x1, xm, x2} // InputForm)];
      Print[(Round[{x1, xm, x2}, 0.1^prec] // InputForm)];
      Print["y: ", {sf[y1], sf[ym], sf[y2]}, ", eps ", sf[(x2-x1)/eps]];
    ];
    If [ ym === 0, Break[]; ];
    If [ ym * y1 < 0,
      {x2, y2} = {xm, ym},
      If [ ym * y2 < 0,
        {x1, y1} = {xm,ym},
        (* impossible situation! bad interval *)
        Print["Error: ", {{x1, y1}, {xm, ym}, {x2, y2}} // InputForm];
        Return[{None, None, None}];
      ];
    ];
  ];
  If [ x2 - x1 < eps,
    Print["bsolv ", it, " iterations, dx 10^", Round[Log10[x2-x1],0.1],
      " x ", (Round[N[(x2+x1)/2], 0.1^prec]//InputForm) ];
  ];
  {N[(x1 + x2)/2, prec], x1, x2}
];
(* ********** Stable root searching function bsolv[] ends ********** *)


(* main root-finding function
   It first finds/loads the root brackets.
   For each bracket, it first tries to use the built-in FindRoot[] function.
   If that fails, use the stable function bsolv[].
   `nwarmup' is the number of steps of refining the interval, also using
   bsolv[], before passing it to FindRoot[]. If it is 0, it checks the
   validity of the window.  If it is -1, this step is skipped.  *)
solve[poly_, x_, prec_ : precdef, wprec_ : None, fnbra_ : fnbradef,
      k0_ : None, k1_ : None, fnout_ : None, fnoutT_ : None, verbose_ : None,
      nwarmup_ : {{nwarmup}}] :=
    Module[{intv, k, ls = {}, sol, x1, x2, y, kmin = k0, kmax = k1, err,
            s, rv, fpT, dydx, tm},

  (* compute or load bracket file *)
  If [ fnbra === None || !FileExistsQ[fnbra],
    Print["Computing root-intervals..."];
    y = Timing[
      intv = RootIntervals[poly][[1]];
    ][[1]];
    Print[Length[intv], " intervals determined ", (intv[[1]]//InputForm), "... in ", y, "s"];
    If [ Length[intv] > 0 && !(fnbra === None),
      xsave[fnbra, intv, True, True];
    ],
    intv = xload[fnbra, True];
    Print["Previous intervals loaded from ", fnbra];
  ];
  If [ wprec === None,
    wprec = Exponent[poly, x] + prec;
  ];
  Print["# of root brackets: ", Length[intv], " wprec ", wprec ];

  (* determine which roots to compute *)
  If [ kmin === None, kmin = 1 ];
  If [ kmax === None, kmax = 1000000 ];
  kmax = Min[kmax, Length[intv]];
  Print["root range: ", {kmin, kmax}];

  (* derivative *)
  dydx = D[poly, x];

  (* starting solving roots *)
  For [ k = kmin, k <= kmax, k++,
    ClearSystemCache[];
    {x1, x2} = intv[[k]];
    Print["searching the root in bracket ", k, ": ", ({x1, x2} // InputForm) ];
    sol = None;

    (* 1. try to narrow down the bracket and do some basic checking,
       s.t. the derivative makes sense, this may make FindRoot[] easier *)
    If [ nwarmup >= 0,
      tm = Timing[
        {sol, x1, x2} = bsolv[poly, x, x1, x2, prec, dydx, verbose, nwarmup];
      ][[1]];
      If [ sol === None,
        Print["k ", k, " bad interval: ", ({x1, x2} // InputForm), " time ", tm];
        Continue[];
        ,
        If [ verbose, Print["k ", k, " checking/warm-up bsolv[] used ", tm]; ];
      ];
    ];

    err = True;
    (* 2. try to use FindRoot[] first *)
    err = False;
    If [ verbose, Print["calling FindRoot[]... k: ", k, ", ", {kmin, kmax}]; ];
    TimeConstrained[
      tm = Timing[
        Check[
          sol = x /. FindRoot[poly,{x, (x1+x2)/2, x1, x2},
            AccuracyGoal -> Infinity, PrecisionGoal -> prec,
            WorkingPrecision -> wprec, MaxIterations -> 10000],
          err = True;
        ]; (* end of Check[] *)
      ][[1]];
      Print["k ", k, " FindRoot[] returns in ", tm, ", error? ", err];
      ,
      tmroot,
      Print["k ", k, " FindRoot[] timeout"];
      err = True;
    ]; (* end of TimeConstrained[ FindRoot[...] ] *)

    (* 3. if FindRoot[] fails, use bsolv[] *)
    If [ err,
      Print["using the fallback function bsolv[]"];
      tm = Timing[
        sol = bsolv[poly, x, x1, x2, prec, None, verbose][[1]];
      ][[1]];
      If [ sol === None,
        Print["k ", k, " error in bsolv[] x: ", ({x1, x2} // InputForm), " time ", tm];
        Continue[];
        ,
        Print["k ", k, "  bsolv[] time ", tm];
      ];
    ];

    (* write the standard output file *)
    sol = N[sol, prec];
    ls = Append[ls, sol];
    Print["sol ", k, " ", sol, " (", N[x1], ", ", N[x2], ") ", fnout];
    If [ !(fnout === None),
      xsave[fnout, {k, sol, x1, x2}, True];
    ];

    (* write the special output of the logistic map *)
    rv = 1 + Sqrt[1 + sol];
    s = "";
    If [ k === kmin,
        s = "solutions of T^"
         <> ToString[ Exponent[poly, x] ]
         <> " ...:\n";
    ];
    s1 = s <> "r: "     <> ToString[ N[rv,    prec] ]
           <> ", r/4: " <> ToString[ N[rv/4,  prec] ] <> "; ";
    s2 =  "4R: "  <> ToString[ N[sol,   prec] ]
       <> ", R: "   <> ToString[ N[sol/4, prec] ];
    Print[s1]; Print[s2];
    If [ !(fnoutT === None),
      fpT = OpenAppend[fnoutT];
      WriteString[fpT, s1 <> s2 <> "\n"];
      Close[fpT];
    ];
  ];
  ls
];




(* ******************  Main function starts here ******************** *)

iscub = False;
var = T;
If [ StringTake[fninp, 2] === "cr",
  iscub = True;
  var = r;
  fnresT = None;
];

tm = Timing[
  eq = xload[fneq, True];
][[1]];
deg = Exponent[eq, var];
If [ wprec === None,
  wprec = deg + prec;
];
Print["Equation loaded in ", tm, " seconds, degree ", deg, ", cubic ", iscub ];

tm = Timing[
  sol = solve[eq, var, prec, wprec, fnbra,
  kmin, kmax, fnres, fnresT, verbose];
][[1]];
Print["solve: ", tm];
"""



""" main function starts here """
if __name__ == "__main__":
  doargs()
  eqload(fninp, fneq) # convert the input to a one-line equation
  src = templ.replace("{{fninp}}", fninp
            ).replace("{{fneq}}", fneq
            ).replace("{{fnres}}", fnres
            ).replace("{{fnresT}}", fnresT
            ).replace("{{fnbra}}", fnbra
            ).replace("{{prec}}", str(prec)
            ).replace("{{wprec}}", str(wprec)
            ).replace("{{kmin}}", str(kmin)
            ).replace("{{kmax}}", str(kmax)
            ).replace("{{tmroot}}", str(tmroot)
            ).replace("{{nwarmup}}", str(nwarmup)
            ).replace("{{verbose}}", str(verbose)
            )
  open(fnout, "w").write(src)
  print "Mathematica script %s, now run\n  math < %s" % (fnout, fnout)


