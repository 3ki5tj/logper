(* Copyright 2012-2013 Cheng Zhang *)
(* USAGE
  math < prd.ma n ch kmin kmax
  `n' is the cycle period
  `ch' is `a' or `b' for onset or bifurcation *)
(* Clear[Evaluate[Context[]<>"*"]] *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[If[ssub, var<>sub, SubscriptBox[var,sub]]];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: x1, x2, ... *)
getvars[n_, ch_:"x"] := Table[mksub[ch, i], {i, n}];

Clear[rotl, w2term, mkcycls, geteqv, cycle1, cycle2];

(* rotation to the left *)
rotl[w_, n_] := Module[{w1 = w*2},
  If [ BitGet[w1, n] == 1, w1 - 2^n + 1, w1 ]
];
(*rotl[rotl[6, 3],3]*)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[
  {xp = IntegerDigits[w,2,n]},
  Product[vars[[n+1-k]]^xp[[k]], {k, n}]
];

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[
  {n = Length[vars], rvars = Reverse[vars], map, j, cid, w0, w, cl},

  map = Table[0, {k, 2^n}]; (* map to the cyclic id *)
  For [cid = 0; cl = {}; w0 = 0, w0 < 2^n, w0++,
    If [ map[[w0+1]] != 0, Continue[] ];
    w = w0;
    map[[w + 1]] = ++cid; (* new cyclic variable *)
    xp = w2term[w, n, vars];
    j = 0;
    While [ ++j < n,
      w = rotl[w, n];
      If[map[[w+1]] != 0, Break[]];
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
geteqv[t_, vars_, pows_, map_] := Module[{w = pows.Exponent[t, vars]},
  If[w == 0,
    { map[[1]], t },
    { map[[w+1]], Coefficient[t, w2term[w, Length[vars], vars] ] }
  ]
];
(* vars=getvars[6]; geteqv[3 R x1 x2 x4, vars, {1, 2, 4, 8, 16, 32}, mkcycls[vars][[2]]] *)

cycle1[expr_, vars_, cl_, map_] := Module[{ls, pows, xp, k, cid, co},
  pows = Table[ 2^(k-1), {k, Length[vars]} ];
  ls = Table[ 0, {k, Length[cl]} ];
  xp = If [ Head[expr] === Plus, expr, {expr} ];
  For [ k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    ls[[cid]] += co cl[[cid]][[3]];
  ];
  ls
];
(*vars=getvars[6]; {cl,map}=mkcycls[vars]; cycle1[3 R x1 x2 + R^2 x1 x3 x5, vars, cl, map] *)

(* define a square replacement rule, x2^2 -> a + b x1 - x3, ... *)
Clear[mksqrrep, hassqr, rmsqrs];
mksqrrep[vars_, a_, b_] := Module[
  {n = Length[vars], ls = {}, j, xk2, mxp, xp},

  xk2[k_] := a + b vars[[Mod[k+n-2, n] + 1]] - vars[[Mod[k, n] + 1]];
  mxp[xp_, ls_] := Expand[Expand[Expand[xp]/.ls]/.ls]/.ls;
  For [ j = 1, j <= n/2 + 1, j++,
    ls = Join[ls,
      mxp[Table[vars[[k]]^(2 j) -> xk2[k]^j, {k, n}], ls]
    ];
    ls = Join[ls,
      mxp[Table[vars[[k]]^(2 j + 1) -> vars[[k]] xk2[k]^j, {k, n}], ls]
    ];
  ];
  ls
];
(*mksqrrep[getvars[8], a, b]*)

(* check if 'poly' has *at least* square of a variable *)
hassqr[poly_, vars_] := (Max[Exponent[poly, vars]] >= 2);

(* remove square terms *)
rmsqrs[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While[hassqr[xp, vars], xp = Expand[xp /. reps]]; xp];

(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] := cycle1[rmsqrs[expr, vars, reps], vars, cl, map];
(*vars=getvars[6]; {cl,map}=mkcycls[vars];
cycle2[3 a x1^2 x2 + x1^2 x3^2 x5^2, vars, cl, map, mksqrrep[vars, a, b]]*)

(* jacobian matrices *)
jac1[b_, x_] := {{-2 x, 1}, {b, 0}};
jac[b_, vars_] := Module[
  {k, n = Length[vars], m = {{1, 0}, {0, 1}}},

  For [ k = 1, k <= n, k++,
    m = m . jac1[ b, vars[[k]] ];
  ];
  m
];

(* return the half of the trace
   lambda^2 - 2 sigma lambda + (-b)^n == 0 *)
sigjac[b_, vars_] := Module[{m = jac[b, vars]},
   Cancel[ (m[[1]][[1]] + m[[2]][[2]])/2 ]
];

(* make cyclic equations *)
Clear[mkeqcycs, getcycmat, getcycmats];

(* X(a1, ..., an) = lambda = +1/-1, and X^2 - 2 sigma X + (-b)^n == 0 *)
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs, k, ls = {}, cvar, xp},
  Xs = -2 X sigjac[b, vars];
  For [ k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[
      cycle2[Xs cvar[[2]], vars, cl, map, reps] / cvar[[3]]
    ];
    xp[[k]] += X^2 + (-b)^Length[vars];
    ls = Append[ls, xp];
  ];
  ls
];
(*
vars = getvars[2];
{cl, map} = mkcycls[vars];
Print[ (mat = mkeqcycs[vars, cl, map, mksqrrep[vars, a, b], X]) // InputForm];
Print[ (det = Factor[ Det[mat] ]) // InputForm ];
Print[ Factor[det/.{X->1}] // InputForm ];
Print[ Factor[det/.{X->-1}] // InputForm ]; Exit[1];
*)

getcycmat[n_, a_, b_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mksqrrep[vars, a, b];
  {cl,map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]
];

getcycmats[n_, a_, b_, X_] := Table[ getcycmat[d, a, b, X], {d, Divisors[n]} ];
(*getcycmats[4, a, b, X]*)

(* degree in R for the cyclic matrix, the primitive factor, and the original factor *)
Clear[degRp];
degRp[n_] := Sum[MoebiusMu[n/d] 2^(d-1), {d, Divisors[n]}];
(*Print[Table[degRp[i],{i,1,20}]]*)

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



(* ******************** Symbolic solution begins *********************** *)
Clear[nicefmt, trigsimp0, trigsimp, mkcprod, mkctprod, symprimfac, calcgnk]

(* nicely format a polynomial for output *)
nicefmt[p_, var_] := Module[{poly = 1, facs, k, f},
  facs = FactorList[p];
  (* simplify each factor, the first is a constant *)
  poly = facs[[1]][[1]];
  For [ k = 2, k <= Length[facs], k++,
    f = Collect[ facs[[k]][[1]], var, Simplify ];
    poly *= f^facs[[k]][[2]]; (* multiply the degeneracy *)
  ];
  poly
]

(* simplify an expression of sines and cosines *)
trigsimp0[p_] := FullSimplify[ TrigReduce[ TrigExpand[p] ] ];

(* simplify an _integral_ polynomial `p' of `vars'
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
mkcprod[poly_, a_, b_, X_, n_, usen_: False] := Module[{Y, pd = 1, k},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    pd *= poly /. { X -> Y * (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression *)
  If [ usen,
    trigsimp[pd, {a, b, Y}, True],
    (* this code is somewhat faster than the direct call of trigsimp[] *)
    Collect[pd, {a, b, Y}, trigsimp0]
  ] /. { Y -> X^(1/n) }
];


(* compute the cyclotomic product of `poly' *)
mkctprod[poly_, a_, b_, X_, n_, usen_: False] := Module[{pd = 1, k, prec},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    If [ GCD[k, n] != 1, Continue[] ];
    pd *= poly /. { X -> (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression
     guess the initial precision *)
  prec = Round[ n * (Exponent[poly, X] + 1) * 1.5 + 10 ];
  trigsimp[pd, {a, b}, usen, prec]
];


(* solve the primitive polynomial for n-cycles *)
symprimfac[n_, a_, b_, X_, mats_:None, usen_:False, notn_:False] :=
  Module[{k, dls, pf, pf1, d, mu, mat, deg},

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
      mat = getcycmat[d, a, b, X],
      mat = mats[[k]]
    ];
    pf1 = Factor[ Det[ mat ] ];
    pf1 = mkcprod[pf1, a, b, X, n/d, usen];
    (* if n === d is excluded, compute the inverted polynomial *)
    If [ notn, mu *= -1 ];
    If [ mu === 1, pf *= pf1, pf /= pf1 ]
  ];
  nicefmt[ Cancel[pf], {a, b} ]
];

(*
Print[ symprimfac[3, a, b, X, None, True, False] // InputForm ]; Exit[1];
*)

(* compute the polynomial at the intersection of n- and d-cycles *)
calcgnk[n_, d_, a_, b_, X_, mats_:None, usen_: False] := Module[{p},
  p = symprimfac[d, a, b, X, mats, usen, False];
  p = mkctprod[p, a, b, X, n/d, usen] /. {a -> A/4};
  nicefmt[p, {A, b}]
];

(*
Print[ calcgnk[6, 2, a, b, X, None, True] // InputForm ]; Exit[1];
*)

(* ******************** Symbolic solution ends ************************* *)


(* ***************** NEW Lagrange interpolation begins ******************** *)
Clear[interp, numdet];

(* interpolate polynomials of `a' *)
interp[xy_, a_, b_] := Module[{n = Length[xy], p, p1, ls, k, kmax},
  p = Table[ xy[[l]][[2]], {l, n} ];
  kmax = Max[ Exponent[p, b] ]; (* highest power in `b' *)
  ls = Table[
    (* get the `b' coefficient list with padding zeros *)
    PadRight[ CoefficientList[ Expand[ p[[k]] ], b ], kmax + 1 ],
  {k, n}];
  For[p = 0; k = 1, k <= kmax+1, k++,
    (* interpolate a polynomial of `A' for each b^k *)
    p1 = InterpolatingPolynomial[
      Table[ { xy[[l]][[1]], ls[[l]][[k]] }, {l, n} ],
      a ];
    p += Factor[p1] b^(k-1);
  ];
  (* sometimes nice[p, b] is more compact *)
  nicefmt[p, a]
];

(* evaluate the primitive polynomial at a few a values
   da can be 1/4, but for large n, it can make Det[] err! *)
numdet[n_, Xv_, a_, b_, X_, mats_:None, k0_:None, k1_:None,
       fn_:None, xy0_:{}, da_:1] :=
  Module[{mymats, xy = xy0, av, Pv, den, denv, mat,
          deg = degRp[n], k, kmin = k0, kmax = k1},

  mymats = If [ mats === None, getcycmats[n, a, b, X], mats ];
  If [ kmin === None || kmax === None,
    kmin = -Round[deg/2 + 1];
    kmax = -kmin + 10000
  ];
  (* `den' is the contribution from shorter d-cycles d|n *)
  den = symprimfac[n, a, b, X, mats, True, True];
  den = den /. {X -> Xv};
  mat = mymats[[-1]] /. {X -> Xv};
  (* Print[ Factor[ Det[mat] ] // InputForm ]; Exit[1]; *)

  For [ k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; (* free some memory *)
    av = k da;
    denv = den /. {a -> av};
    (* leave if a divide by zero is encountered *)
    If [ denv === 0, Continue[] ];
    (* compute the value of the polynomial at a = av *)
    Pv = Cancel[ Det[ mat /. {a -> av} ] / denv ];
    (* add the new value to the list *)
    xy = Append[xy, {av, Pv}];
    If [!(fn === None),
      xsave[fn, {av, Pv}, True]
    ]
  ];
  xy
];

(*
Print[ interp[ numdet[4, -1, a, b, X], A/4, b ] // InputForm ]; Exit[1];
*)

(* ***************** NEW Lagrange interpolation ends ********************** *)




(* main function starst here *)

n = 6;
If [ Length[$CommandLine] >= 2,
  n = ToExpression[ $CommandLine[[2]] ]
];

ch = "a";
If [ Length[$CommandLine] >= 3,
  ch = $CommandLine[[3]]
];
lambda = If [ ch === "b", -1,
         If [ ch === "a", 1,
                          0] ];

Print["n ", n, "; lam ", ch, " ", lambda, "; degR. ", degRp[n]];


(* reading or computing the matrix *)
fnmats = "hmats"<>ToString[n]<>".txt";
If [ FileType[fnmats] === File,
  tm = Timing[
    mats = xload[fnmats]; (* load previous matrices *)
  ][[1]];
  Print["matrix loaded from ", fnmats, ", time ", tm],

  tm = Timing[
    mats = getcycmats[n,a,b,X];
  ][[1]];
  Print["computing mats: ", tm];
  xsave[fnmats, mats];
];

(* 3. compute the determinant of the matrix *)
If [ lambda === 0,

  (* `ch' === 'c' or 'x', symbolically compute the determinant *)
  If [ ch === "c",
    tm = Timing[
      poly = symprimfac[n, a, b, X, mats, True, False];
    ][[1]];
    Print["time for primitive polynomial ", tm];
    xsave["abX" <> ToString[n] <> ".txt", poly, False, True],

    (* compute the d/n-bifurcation polynomial *)
    d = n;
    If [ Length[$CommandLine] >= 4,
      n = ToExpression[ $CommandLine[[4]] ]
    ];
    If [ Mod[n, d] != 0,
      Print[n, "- and ", d, "-cycles do not intersect"];
      Exit[1]
    ];
    tm = Timing[
      poly = calcgnk[n, d, a, b, X, mats, True];
    ][[1]];
    Print["time ", tm, ", polynomial ", n, " and ", d];
    If [ n < 10, Print[poly // InputForm ] ];
    xsave["ab" <> ToString[d] <> "x" <> ToString[n] <> ".txt", poly, False, True];
  ],

  (* `ch' === 'a' or 'b', numerically compute the onset or bifurcation point *)
  kmin = kmax = None;
  If [ Length[ $CommandLine ] >= 5,
    {kmin, kmax} = {
      ToExpression[ $CommandLine[[-2]] ],
      ToExpression[ $CommandLine[[-1]] ]
    }
  ];
  da = 1;
  If [ Length[ $CommandLine ] == 4 || Length[ $CommandLine ] >= 6,
    da = ToExpression[ $CommandLine[[4]] ]
  ];
  Print["da ", da, "; kmin ", kmin, ", kmax ", kmax];

  (* computing the determinant of the matrix *)
  If [ kmin < kmax || kmin === None,
    fnls = If [ n > 4, "hls" <> ToString[n] <> ch <> ".txt", None ];
    If [ kmin === None && !(fnls === None),
      Close[ OpenWrite[fnls] ] (* clear the list *)
    ];
    tm = Timing[
      xy = numdet[n, lambda, a, b, X, mats, kmin, kmax, fnls, {}, da];
    ][[1]];
    Print["computing determinant list: ", tm];
    If [ Length[xy] >= degRp[n] + 1,
      tm = Timing[
        poly = interp[xy, A/4, b];
      ][[1]];
      Print["interpolation and factorization: ", tm];
      If [ n <= 4, Print[ poly // InputForm ] ];
      fnab = "ab" <> ToString[n] <> ch <> ".txt";
      xsave[fnab, poly, False, True];
    ]
  ]
];

