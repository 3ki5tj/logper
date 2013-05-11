(* Copyright 2012-2013 Cheng Zhang *)
(* USAGE
    math < log.ma n ch dR kmin kmax
   where
   `n' is the cycle period
   `ch' can be `a' (onset), `b' (bifurcation) or `c' (general solution)
   `kmin' and `kmax' can go from -(degR/2+1) to degR/2
   `ch' can also be `x':
      math < log.ma d x n
    which computes the intersection polynomials of d- and n-cycles
*)
(* Clear[Evaluate[Context[] <> "*"]] *)

precdef = 21; (* precision for numerical calculations *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[ If [ ssub, var <> sub, SubscriptBox[var,sub] ] ];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: a1, a2, ... *)
getvars[n_] := Table[ mksub["a", k], {k, n} ];

Clear[rotl, w2term, mkcycls, geteqv, cycle1, cycle2];

(* rotation to the left *)
rotl[w_, n_] := Module[{w1 = w*2},
    If [ BitGet[w1, n] === 1, w1 - 2^n + 1, w1]
];
(*rotl[rotl[6, 3],3]*)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[ {xp = IntegerDigits[w, 2, n]},
  Product[ vars[[n+1-k]]^xp[[k]], {k, n} ]
];

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[
  {n = Length[vars], rvars = Reverse[vars], map, j, cid, w0, w, cl},

  map = Table[0, {k, 2^n}]; (* map to the cyclic id *)
  For [ cid = 0; cl = {}; w0 = 0, w0 < 2^n, w0++,
    If [ map[[w0+1]] != 0, Continue[] ];
    w = w0;
    map[[w + 1]] = ++cid; (* new cyclic variable *)
    xp = w2term[w, n, vars];
    j = 0;
    While [ ++j < n,
      w = rotl[w, n];
      If [ map[[w+1]] != 0, Break[] ];
      (* xp += w2term[w, n, vars]; *)
      map[[w+1]] = cid;
    ];
    cl = Append[cl, {mksub["C", cid], xp, n/j, w0}];
    (*Print[{IntegerDigits[w0,2,n], cid, n/j, xp}];*)
  ];
  {cl, map}
];
(*mkcycls[getvars[4]]*)

(* map to a cyclic variable *)
geteqv[t_, vars_, pows_, map_] := Module[
  {w = pows.Exponent[t, vars]},

  If [ w === 0,
    {map[[1]], t},
    {map[[w+1]], Coefficient[t, w2term[w, Length[vars], vars]]}
  ]
];
(*vars={a1,a2,a3,a4,a5,a6}; geteqv[3 R a1 a2 a4, vars, {1, 2, 4, 8, 16, 32}, mkcycls[vars][[2]]]*)

cycle1[expr_, vars_, cl_, map_] := Module[
  {ls, pows, xp, k, cid, co},

  pows = Table[2^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]}];
  xp = If [ Head[expr] === Plus, expr, {expr}];
  For [ k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    ls[[cid]] += co cl[[cid]][[3]]; ];
  ls
];
(*vars={a,b,c,d,e,f}; {cl,map}=mkcycls[vars]; cycle1[3 R a b + R^2 a c e, vars, cl, map]*)

(* define a square replacement rule, a1^2 -> R - a2, ... *)
Clear[mksqrrep, hassqr, rmsqrs];
mksqrrep[vars_, R_] := Module[{n = Length[vars]},
  Table[vars[[k]]^2 -> R - vars[[Mod[k, n] + 1]], {k, n}]
];
(*mksqrrep[getvars[9],R]*)

(* check if 'poly' has *at least* square of a variable *)
hassqr[poly_, vars_] := (Max[Exponent[poly, vars]] >= 2);

(* remove square terms *)
rmsqrs[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While [ hassqr[xp, vars],
    xp = Expand[xp /. reps]
  ];
  xp
];

(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] :=
  cycle1[ rmsqrs[expr, vars, reps], vars, cl, map ];
(*
vars={a,b,c,d,e,f};
{cl,map}=mkcycls[vars];
cycle2[0 R a^2 b + a^2 c^2 e^2, vars, cl, map, mksqrrep[vars, R]]
*)

(* make cyclic equations by expanding the product of Xs = a1...an, which is
   the last cyclic polynomial, and the kth cyclic polynomial cl[[k]] *)
Clear[mkeqcycs, getcycmat, getcycmats];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[
  {Xs, k, ls = {}, cvar, xp},

  Xs = cl[[-1]][[2]] * (-2)^Length[vars];

  For [ k = 1, k <= Length[cl], k++, (* for the kth cyclic variable *)
    (* get coefficients of the square-free expansion of the product
       Xs cl[[k]] of Xs and the kth cyclic polynomial *)
    cvar = cl[[k]]; (* cvar is a tuple,
      e.g., {C12, a1 a2, 1} or {C123, a1 a2 a3, 3} *)
    xp = Cancel[
          cycle2[Xs cvar[[2]], vars, cl, map, reps] / cvar[[3]]
    ];
    xp[[k]] -= X;
    ls = Append[ls, xp];
  ];
  ls
];
(*
vars={a,b,c,d,e,f};
{cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mksqrrep[vars, R], X]
*)

(* get the matrix that connects cyclic polynomials *)
getcycmat[n_, R_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mksqrrep[vars, R];
  {cl, map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]
];

(* get the matrix of n, and those of divisors d of n *)
getcycmats[n_, R_, X_] := Table[ getcycmat[d, R, X], {d, Divisors[n]} ];
(* Print[ getcycmats[3, R, X] ]; Exit[1]; *)

(* degree in R of the primitive polynomial of n-cycles *)
Clear[degRp];
degRp[n_] := Sum[MoebiusMu[n/d] 2^(d-1), {d, Divisors[n]}];
(* Print[Table[degRp[k],{k,1,20}]] *)

Clear[xsave, xload];

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



(* stand-alone routine for superstable cycles *)
fnss[R_, n_] := Module[
  {k, x, poly, tab = Table[0, {k, n}], divs, mu, d},

  For [ x = 0; k = 1, k <= n, k++,
    tab[[k]] = x = R - x^2
  ];
  poly = 1;
  divs = Divisors[n];
  For [ k = 1, k <= Length[divs], k++,
    d = divs[[k]];
    mu = MoebiusMu[n/d];
    If [ mu === 0, Continue[],
    If [ mu === 1, poly *= tab[[d]],
                   poly /= tab[[d]]
    ] ]
  ];
  Cancel[poly]
]




(* ******************** Symbolic solution begins *********************** *)
Clear[trigsimp0, trigsimp, mkcprod, mkctprod, symprimfac, calcgnk]

(* simplify an expression of sines and cosines *)
trigsimp0[p_] := FullSimplify[TrigReduce[TrigExpand[p]]];

(* simplify an _integral_ expression
   to use the faster numerical version, set `usen' = True,
   the symbolic version is psychologically safer *)
trigsimp[p_, vars_, usen_: False, prec0_: 10] := Module[
  {p1, p2, prec = prec0, dprec, k},

  (* use the symbolic version, slower *)
  If [!usen, Return[ Collect[ trigsimp0[p], vars] ] ];

  (* increase precision util results from two precisions match *)
  p1 = Collect[ Expand[ N[p, prec] ], vars, Round ];
  dprec = Max[ Round[prec0 / 4], 10 ];
  For [ k = 1, True, k++,
    prec += dprec;
    (* NOTE: coefficients must be integers, so Round[] is safe *)
    p2 = Collect[ Expand[ N[p, prec] ], vars, Round ];
    If [ p2 === p1, Break[] ];
    p1 = p2;
  ];
  Print[k, " iterations, precision: ", prec0, " -> ", prec];
  p1
];

(* compute the cyclic product
   \prod_{k=0..n-1} poly[X^(1/k) e^(2*pi*i*k/n)] *)
mkcprod[poly_, R_, X_, n_, usen_: False] := Module[{Y, pd = 1, k},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    pd *= poly /. { X -> Y * (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression *)
  If [ usen,
    trigsimp[pd, {R, Y}, True],
    (* this code is somewhat faster than the direct call of trigsimp[] *)
    Collect[pd, {R, Y}, trigsimp0]
  ] /. { Y -> X^(1/n) }
];


(* compute the cyclotomic product of `poly' *)
mkctprod[poly_, R_, X_, n_, usen_: False] := Module[{pd = 1, k, prec},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    If [ GCD[k, n] != 1, Continue[] ];
    pd *= poly /. { X -> (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression
     guess the initial precision *)
  prec = Round[ n * (Exponent[poly, X] + 1) * 1.5 + 10 ];
  trigsimp[pd, R, usen, prec]
];


(* solve the primitive polynomial for n-cycles *)
symprimfac[n_, R_, X_, mats_:None, usen_:False, notn_:False] :=
  Module[{k, dls, pf, pf1, d, mu, mat},

  dls = Divisors[n];
  kmax = Length[dls];
  If [ notn, kmax -= 1 ]; (* exclude d === n *)

  (* P(n) = \prod cyc[ A(d), n/d ]^mu(n/d),
     where cyc[A, n/d] means the (n/d)-fold cyclic product of A *)
  For [ pf = 1; k = 1, k <= kmax, k++,
    d = dls[[k]];
    mu = MoebiusMu[n/d];
    If [ mu === 0, Continue[] ];
    If [ mats === None, (* compute the matrix if unavailable *)
      mat = getcycmat[d, R, X],
      mat = mats[[k]]
    ];
    pf1 = Factor[ Det[ mat ] ];
    pf1 = mkcprod[pf1, R, X, n/d, usen];
    (* if n === d is excluded, compute the inverted polynomial *)
    If [ notn, mu *= -1 ];
    If [ mu === 1, pf *= pf1, pf /= pf1 ]
  ];
  pf = Collect[Cancel[pf], R, Simplify];
  (* if n === d is excluded, the factorized form is usually simpler *)
  If [ notn, Factor[pf], pf ]
];


(* compute the polynomial at the intersection of n- and d-cycles *)
calcgnk[n_, d_, R_, usen_: False] := Module[{p, X},
  p = symprimfac[d, R, X, None, usen];
  Numerator[ Together[ mkctprod[p, R, X, n/d, usen] /. {R -> T/4} ] ]
];
(* ******************** Symbolic solution ends ************************* *)



(* ******************** Root finding/printing begins ******************* *)
(* wrapper for NSolve. *)
Clear[nsolve, solveT];
nsolve[ieq_, x_, prec_: precdef] := Module[{k, eq, sols},
  eq = If [ Head[ieq] === Equal, ieq, ieq == 0];
  sols = NSolve[eq, x, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]
];
(* nsolve[x^3 + 2 x -1, x] *)

(* solve equations for T = 4R *)
solveT[eq_, x_, fn_, prec_: precdef] := Module[
  {facs, l, k, sols, ls = {}, r, cnt, s, str = ""},

  For [ facs = FactorList[eq]; l = 1, l <= Length[facs], l++,
    sols = nsolve[facs[[l]][[1]], x, prec];
    For [ cnt = 0; k = 1, k <= Length[sols], k++; cnt++,
      ls = Append[ls, sols[[k]]];
      r = 1 + Sqrt[1 + sols[[k]]];
      s = "";
      If [ cnt === 0,
        s = s <> "solutions of T^"
              <> ToString[ Exponent[ facs[[l]][[1]], T ] ]
              <> " ...:\n"
      ];
      s = s <> "r: " <> ToString[r] <> ", r/4: " <> ToString[r/4]
            <> "; 4R: " <> ToString[sols[[k]]]
            <> ", R: " <> ToString[sols[[k]]/4];
      If [ cnt < 1, Print[s] ];
      str = str <> s <> "\n";
    ]
  ];
  If [!(fn === None),
    fp = OpenWrite[fn];
    WriteString[fp, str];
    Close[fp]
  ];
  ls
];
(* ****************** Root finding/printing ends *********************** *)




(* ***************** NEW Lagrange interpolation begins ******************** *)
(* return the value of the matrix for R = Rv and delta = 2 Pi I frac
normalized such that the coefficient of the highest power of 4R is 1 *)
Clear[interp, numdet];

interp[ls_, R_] := Factor[ InterpolatingPolynomial[ls, R] ];

(* evaluate the primitive polynomial at a few R values *)
numdet[n_, frac_, R_, X_, mats_:None, k0_:None, k1_:None,
       fn_:None, xy0_:{}, dR_:1] :=
  Module[{mymats, xy = xy0, Xv, Rv, Pv, den, denv, mat,
          deg = degRp[n], k, kmin = k0, kmax = k1},

  mymats = If [ mats === None, getcycmats[n, R, X], mats ];
  If [ kmin === None || kmax === None,
    kmin = -Round[deg/2 + 1];
    kmax = -kmin + 10000
  ];
  Xv = Exp[ I 2 Pi frac ];
  (* `den' is the contribution from shorter d-cycles d|n *)
  den = symprimfac[n, R, X, mats, True, True];
  den = den /. {X -> Xv};

  For [ k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; (* free some memory *)
    Rv = k dR;
    denv = den /. {R -> Rv};
    (* leave if a divide by zero is encountered *)
    If [ denv === 0, Continue[] ];
    (* compute the value of the polynomial at R = Rv *)
    mat = mymats[[-1]] /. {R -> Rv, X -> Xv};
    Pv = Cancel[ Det[ mat ] / denv ];
    (* add the new value to the list *)
    xy = Append[xy, {Rv, Pv}];
    If [!(fn === None),
      xsave[fn, {Rv, Pv}, True]
    ]
  ];
  xy
];

(* Print[ interp[ numdet[4, 1/2, R, X], T/4 ] ]; Exit[1]; *)

(* ***************** NEW Lagrange interpolation ends ********************** *)



(* main function starts here *)

(* 1. handle input arguments *)
n = 10;
If [ Length[ $CommandLine ] >= 2,
  n = ToExpression[ $CommandLine[[2]] ]
];

ch = "a";
If [ Length[ $CommandLine ] >= 3,
  ch = $CommandLine[[3]]
];
frac = If [ ch === "b", 1/2,
       If [ ch === "a", 1,
                        0] ];

Print["n ", n, "; frac ", ch, " ", N[frac], "; degR. ", degRp[n]];


(* 2. load or compute the matrix that connects the n cyclic polynomials
      by the square-free reduction, each element of the matrix is a
      polynomial of R, matrices of divisors of n are also obtained *)
fnmats = "lmats" <> ToString[n] <> ".txt";
If [ FileType[fnmats] === File,
  tm = Timing[
    mats = xload[fnmats];
  ][[1]];
  Print["matrix loaded from ", fnmats, ", time ", tm],

  tm = Timing[
    mats = getcycmats[n, R, X];
  ][[1]];
  Print["computing mats: ", tm];
  xsave[fnmats, mats];
];


(* 3. compute the determinant of the matrix *)
If [ frac === 0,

  (* `ch' === `c' or `x', symbolically compute the determinant *)
  If [ ch === "c",
    tm = Timing[
      poly = symprimfac[n, R, X, mats, True];
    ][[1]];
    Print["time for primitive polynomial ", tm];
    xsave["RX" <> ToString[n] <> ".txt", poly],

    (* compute the d/n-bifurcation polynomial *)
    d = n;
    If [ Length[$CommandLine] >= 4,
      n = ToExpression[ $CommandLine[[4]] ]
    ];
    If [ Mod[n, d] != 0,
      Print[n, " and ", d, "-cycles do not intersect"];
      Exit[]
    ];
    tm = Timing[
      poly = calcgnk[n, d, R, True];
    ][[1]];
    Print["time ", tm, ", polynomial ", n, " and ", d];
    If [ n < 100, Print[poly] ];
    xsave["T" <> ToString[d] <> "x" <> ToString[n] <> ".txt", poly];
  ],

  (* `ch' === 'a' or `b', numerically compute the onset or bifurcation point *)
  kmin = kmax = None;
  If [ Length[ $CommandLine ] >= 5,
    {kmin, kmax} = {
      ToExpression[ $CommandLine[[-2]] ],
      ToExpression[ $CommandLine[[-1]] ]
    }
  ];
  dR = 1;
  If [ Length[ $CommandLine ] == 4 || Length[ $CommandLine ] >= 6,
    dR = ToExpression[ $CommandLine[[4]] ]
  ];
  Print["dR ", dR, "; kmin ", kmin, ", kmax ", kmax];

  If [ kmin < kmax || kmax === None,
    fnls = If [ n > 8, "ls" <> ToString[n] <> ch <> ".txt", None];
    If [ kmin === None && !(fnls === None),
      Close[ OpenWrite[fnls] ] (* clear the list file *)
    ];
    tm = Timing[
      xy = numdet[n, frac, R, X, mats, kmin, kmax, fnls];
    ][[1]];
    Print["computing Dets: ", tm];
    If [ Length[xy] >= degRp[n] + 1,
      poly = interp[xy, T/4];
      If [ n < 7, Print[ poly ] ];
      fnT = "T" <> ToString[n] <> ch <> ".txt";
      xsave[fnT, poly, False, True];
      sols = solveT[poly, T, "r" <> ToString[n] <> ch <> ".txt"];
      xsave[fnT, sols, True, False];
    ]
  ]
];

