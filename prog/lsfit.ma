(* interpolate a polynomial from a list file
   for logistic and cubic maps
   Cheng Zhang (c) 2013 *)
(* USAGE:
    math < lsfit.ma filename method verbose
  where
   `filename' is a list file, e.g. `ls12a.txt'
   `method' can be
      0 (Mathematica's built-in method, fastest, but may fail),
      1 (power, from the highest power to the lowest, slow but robust)
      2 (stair, from the lowerest power to the highest, much faster than power)
   `verbose' is a number: 0, 1 or 2 *)

(* *********************** powfit begins **************************** *)


(* get the leading coefficient *)
lead[xs0_, ys0_, verbose_ : 0] := Module[{n, kmax, k, xs, ys, a, os},
  n = kmax = Length[ys0] - 1;
  If [ n == 0, Return[ ys0[[1]] ]; ];
  (* iterately compute the  list of differences *)
  ys = Table[ys0[[k]], {k, 1, n+1}];

  While [ kmax > 0,
    ys = Table[ys[[k+1]] - ys[[k]], {k, 1, kmax}];
    If [ verbose > 0, Print["kmax ", kmax, "/", n]; ];
    (* Print[ ys[[1 ;; kmax]] ]; *)
    kmax--;
  ];
  a = ys[[1]] / n!;
  (* drop a larger x *)
  os = If [ Abs[xs0[[1]]] > Abs[xs0[[-1]]], 2, 1];
  ys = Table[ys0[[k]] - a xs0[[k]]^n, {k, os, n - 1 + os}];
  xs = xs0[[os ;; n - 1 + os]];
  {a, xs, ys}
];


(* fit by a power series from the top
   since the leading coefficients should be +1 or -1
   it can quickly detect a corruption *)
powfit[ls_, x_, verbose_ : 0] := Module[{n, n0, xs, ys, a, poly = 0, k},
  n = n0 = Length[ls];
  xs = Table[ls[[k]][[1]], {k, n}];
  ys = Table[ls[[k]][[2]], {k, n}];
  For [ k = 1, n > 1, k++,
    {a, xs, ys} = lead[xs, ys, verbose - 1];
    n = Length[xs];
    p = Factor[a x^n];
    poly += p;
    If [ verbose > 0, Print["coefficient of x^", n, " ",
        If [ByteCount[p] <= 180, (p // InputForm), ""],
        " memory ", MemoryInUse[] ]; ];
    (* Print["new term is ", a x^n // InputForm]; *)
    If [ n0 > 1000, xsave["fit.tmp", poly, k > 1, verbose > 1]; ];
    ClearSystemCache[];
  ];
  poly + ys[[1]]
];
(*
ls = {{1,1}, {2,2}, {3,11}, {4,34}};
Print[ powfit[ls, x, 2] // InputForm];
Print[ Expand[InterpolatingPolynomial[ls, x]] // InputForm ];
Exit[];
*)

(* *********************** powfit ends **************************** *)


(* *********************** stairfit begins ************************ *)
(* Here we first express the polynomial as
   f(x) = y1 + (x - x1)*(y2 + (x - x2)*(y3 + (x - x3)*(y4...))) (#)
  then, obtain the normal polynomial coefficients.

  (#) can be expressed iteratively as f(x) = f1(x),
    f1(x) = y1 + (x - x1)*f2(x)
    ...
    fm(x) = ym + (x - xm)*f_{m+1}(x).

  In the first stage, we obtain (x1, y1), then (x2, y2), then
  (x3, y3), .... from the given list of pair values.
  This is accomplished by the function peel[].
  Since fm(xm) = ym, the pair is obtained just by selecting
  one of the values in the current list.
  However, to make the process iterable, we update the list
  of pair values as
  y^(m+1)_k = (y^(m)_k - ym) / (xk - xm).

  In the second stage, we can construct fn(x), then f_{n-1}(x), ...,
  all the way to the f1(x) = f(x).  This is implemented in stairfit[].
*)

(* reduce the function f(x) --> (f(x) - f(xm))/(x - xm) *)
peel[xs0_, ys0_] := Module[{i, im, xm, ym, xs, ys, err},
  (* find the smallest index to peel off *)
  ym = Min[Abs[ys0]];
  For [ im = 1, im <= Length[xs0], im++,
    If [ Abs[ ys0[[im]] ] === ym, Break[]; ];
  ];
  xm = xs0[[im]];
  ym = ys0[[im]];
  (* the interpolated polynomial is supposed to be integral,
     use this to check potential corruption *)
  If [ Head[ym] === Rational,
    Print["corruption at im ", im, " xm ", xm];
    Exit[];
  ];
  (* Print["im ", im, " xm ", xm, " ym ", ym]; *)
  If [ Length[xs0] === 1, Return[ {xm, ym, {}, {}} ]; ];
  xs = Drop[xs0, {im}];
  ys = Drop[ys0, {im}];
  ys = Table[ (ys[[k]] - ym)/(xs[[k]] - xm), {k, Length[xs]} ];
  (* Print[{xm, ym, xs, ys}]; *)

  (* check for rational values *)
  err = False;
  For [ k = 1, k <= Length[xs], k++,
    If [ Head[ys[[k]]] === Rational,
      Print["corruption with im ", im, " xm ", xm, " k ", k, " x ", xs[[k]]];
      err = True;
    ];
  ];
  If [ err, Exit[]; ];

  {xm, ym, xs, ys}
];

stairfit[ls_, x_, verbose_ : 0] := Module[{xs, ys, n, k, pls = {}, poly},
  n = Length[ls];
  xs = Table[ls[[k]][[1]], {k, n}];
  ys = Table[ls[[k]][[2]], {k, n}];

  (* gradually peel off the polynomial *)
  For [ k = 1, True, k++,
    {xm, ym, xs, ys} = peel[xs, ys];
    pls = Append[pls, {xm, ym}];
    If [ Length[xs] === 0, Break[]; ];
    If [ verbose > 0, Print["peeling ", Length[pls], "/", n, " xm ", xm]; ];
    (* save the peeled list *)
    If [ n > 2000, xsave["fitpls.tmp", {xm, ym}, k > 1, verbose > 1] ];
  ];
  If [ verbose > 0, Print["polynomial is peeled off ", Length[pls] ]; ];

  (* compile the polynomial *)
  poly = pls[[-1]][[2]];
  For [ k = Length[pls]-1, k >= 1, k--,
    {xm, ym} = pls[[k]];
    poly = Expand[poly*(x - xm) + ym];
    If [ verbose > 0, Print["piecing together ", k, "/", Length[pls]]; ];
  ];
  poly
];

(*
ls = {{1,0}, {2,1}, {3,10}, {4,33}};
Print[ stairfit[ls, x, 2] // InputForm];
Print[ Expand[InterpolatingPolynomial[ls, x]] // InputForm ];
Exit[];
*)
(* *********************** stairfit ends ************************** *)


xloadn[fn_, verbose_: 0] := Module[{fp, xp, ls = {}},
  If [ verbose > 0, Print["reading ", fn] ];
  fp = OpenRead[fn];
  While [ True,
    xp = Read[fp, Expression];
    If [ xp === EndOfFile, Break[]; ];
    ls = Append[ls, xp];
    If [ verbose > 1, Print["loading item", Length[ls]] ];
  ];
  Close[fp];
  ls
];

(* xloadn["ls4b.txt"]; *)

xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[ If[append, "appending ", "writing "], fn]; ];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]];
  Write[fp, xp];
  Close[fp];
];



(* remove duplicates in the list *)
rmdup[ls0_] := Module[{i, j, k, x, ls},
  ls = ls0[[1 ;; -1]]; (* make a copy *)
  While [ True,
    For [ i = 1, i < Length[ls], i++,
      x = ls[[i]][[1]];
      For [ j = i + 1, j <= Length[ls], j++,
        If [ ls[[j]][[1]] === x,
          Print["a duplicate is found at ", x, " same value? ", ls[[i]][[2]] === ls[[j]][[2]] ];
          Break[];
        ];
      ]; (* end of j loop *)
      If [ j <= Length[ls],
        ls = Drop[ls, {j}];
        Break[];
      ];
    ]; (* end of i loop *)
    If [ i >= Length[ls], Break[]; ];
  ];
  ls
];



fninp = "ls14b.txt";
If [ Length[ $CommandLine ] >= 2,
  fninp = $CommandLine[[2]];
];
method = 3;
If [ Length[ $CommandLine ] >= 3,
  method = ToExpression[ $CommandLine[[3]] ];
];
mtnames = {"built-in", "power", "stair"};
verbose = 1;
If [ Length[ $CommandLine ] >= 4,
  verbose = ToExpression[ $CommandLine[[4]] ];
];

Print["list file ", fninp, " method ", mtnames[[method]], " verbose ", verbose];



tm = Timing[
  ls = xloadn[fninp];
  ][[1]];
Print["loaded ", fninp, " time ", tm, " length ", Length[ls]];
tm = Timing[
  ls = rmdup[ls];
  ][[1]];
Print["rmdup ", tm, " length ", Length[ls]];

var = T/4;  (* fitting variable *)
If [ StringTake[fninp, {1}] === "c", (* cubic map *)
  var = r;
];
tm = Timing[
    If [ method === 0,
      p = InterpolatingPolynomial[ls, var];
      ,
      If [ method === 1,
        p = powfit[ls, var, verbose];
        ,
        p = stairfit[ls, var, verbose];
      ];
    ];
  ][[1]];
Print["interpolation ", tm];
xsave["fit.txt", p, False, True];



Print["trying to factorize..."];
tm = Timing[
  p = Factor[p];
  ][[1]];
Print["factoring ", tm];
xsave["fit.txt", p, False, True];


