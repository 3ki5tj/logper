(* Factorize the 14-cycle onset polynomial
   Cheng Zhang (c) 2013 *)

xload[fn_, verbose_: False] := Module[{fp, xp},
  If [ verbose, Print["reading ", fn] ];
  fp = OpenRead[fn];
  xp = Read[fp, Expression];
  Close[fp];
  xp
];


xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[ If[append, "appending ", "writing "], fn]; ];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]];
  Write[fp, xp];
  Close[fp];
];


tm = Timing[
  (* run
        python mknsolv.py fit.txt
     to "eq.txt", which a one-line version of fit.txt *)
  poly = xload["eq.txt", True];
][[1]];
Print["loading the main polynomial ", tm];
tm = Timing[
  (* the files are output of
     math < log.ma d x 14  *)
  p1 = xload["T1x14.txt"];
  p2 = xload["T2x14.txt"];
  p7 = xload["T7x14.txt"];
][[1]];
Print["loading factors ", tm];
pnew = (p1 p2 p7) Cancel[ poly/(p1 p2 p7) ];
xsave["T14a.txt", pnew, False, True];


