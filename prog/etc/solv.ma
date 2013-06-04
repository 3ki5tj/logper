(* two functions in the old mknsolv.py *)


(* nsolve *)
nsolve[ieq_, x_, prec_: 100] := Module[{k, eq, sols},
  eq = If[Head[ieq] === Equal, ieq, ieq == 0];
  sols = NSolve[eq, x, Reals, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]
];
(* nsolve[x^3 + 2 x -1, x] *)



(* solve by bisection, a fallback *)
bsolv0[poly_, x_, xmin_, xmax_, prec_: 10] := Module[{k, eps = 10.0^(-prec),
    x1 = xmin, x2 = xmax, xm, y1, y2, ym, np = 10},
  y1 = N[poly/.{x->x1}, np];
  y2 = N[poly/.{x->x2}, np];
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




