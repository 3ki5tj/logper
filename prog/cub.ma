(* Copyright 2012-2013 Cheng Zhang *)
(* USAGE
    math < cub.ma n ch kmin kmax
  `n' is the cycle period
  `ch': 'a' for the onset point, 'b' for the bifurcation point
        'c' for the general boundary polynomial
        'x' for the d x n cycle-intersection polynomial *)
(* Clear[Evaluate[Context[]<>"*"]] *)

b = 3; (* order of the map *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[If[ssub, var<>sub, SubscriptBox[var,sub]]];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: x1, x2, ... *)
getvars[n_, c_:"x"] := Table[mksub[c, i], {i, n}];

Clear[rotl, w2term, mkcycls, geteqv, cycle1, cycle2];
(* rotation to the left *)
rotl[w_, n_, bn_] := Module[{w1 = w*b, r}, r = Mod[w1, bn]; r+(w1-r)/bn];
(*n=3; Table[{IntegerDigits[k,b,n], IntegerDigits[rotl[k, n, b^n],b,n]}, {k, b^n - 1}]*)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[{xp = IntegerDigits[w,b,n]},
  Product[vars[[n+1-k]]^xp[[k]], {k, n}]];
(*n=3; Table[w2term[w, n, getvars[n]], {w, 0, b^n-1}]*)

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[{n = Length[vars], rvars = Reverse[vars], bn, map, j, cid, w0, w, cl},
  bn = b^n;
  map = Table[0, {k, bn}]; (* map to the cyclic id *)
  For[cid = 0; cl = {}; w0 = 0, w0 < bn, w0++,
    If[map[[w0+1]] != 0, Continue[]];
    If[Mod[Total[IntegerDigits[w0, b]], 2] == 1,
      map[[w0+1]] = -1; Continue[]];
    w = w0;
    map[[w+1]] = ++cid; (* new cyclic variable *)
    xp = w2term[w, n, vars];
    j = 0;
    While[++j < n,
      w = rotl[w, n, bn];
      If[map[[w+1]] != 0, Break[]];
      (* xp += w2term[w, n, vars]; *)
      map[[w+1]] = cid;];
    cl = Append[cl, {mksub["C", cid], xp, n/j, w0}];
    (*Print[{IntegerDigits[w0,2,n], cid, n/j, xp}];*)
  ]; {cl, map} ];
(*mkcycls[getvars[3]]*)

(* map to a cyclic variable *)
geteqv[t_, vars_, pows_, map_] := Module[{xp = Exponent[t, vars]},
  If[Max[xp] >= b, Print[t, " is reducible"]; Abort[]]; w = pows.xp;
  If[w == 0, {map[[1]], t}, {map[[w+1]], Coefficient[t, w2term[w, Length[vars], vars]]}] ];
(*vars=getvars[6]; geteqv[3 r x1^3 x2 x4, vars, {1, 2, 4, 8, 16, 32}, mkcycls[vars][[2]]]*)

cycle1[expr_, vars_, cl_, map_] := Module[{ls, pows, xp, k, cid, co},
  pows = Table[b^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]}];
  xp = If[Head[expr] === Plus, expr, {expr}];
  For[k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    If[cid <= 0, Print["bad representation ", xp[[k]], " cid ", cid]; Abort[]];
    ls[[cid]] += co cl[[cid]][[3]]; ];
  ls];
(*vars={a,p,c,d,e,f}; {cl,map}=mkcycls[vars]; cycle1[3 r a p + r^2 a c e, vars, cl, map]*)

(* define replacement rules, x1^b -> r x1 - x2, ... *)
Clear[mkrep, haspow, rmsqrs];
mkrep[vars_, r_] := Module[{n = Length[vars], c, ls = {}},
  For[c = 0, c <= b - 2, c++,
    ls = Join[ls, Table[vars[[k]]^(b+c) ->
         Expand[vars[[k]]^c (r vars[[k]] - vars[[Mod[k, n] + 1]])], {k, n}]];
  ]; ls];
(*mkrep[getvars[4], r]*)

(* check if 'poly' has *at least* square of a variable *)
haspow[poly_, vars_] := (Max[Exponent[poly, vars]] >= b);
(*haspow[a^b c^b e^b, {a, c, e}]*)

(* remove square terms *)
rmsqrs[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While[haspow[xp, vars], xp = Expand[xp /. reps]]; xp];
(*vars={a,c,e}; rmsqrs[r a^2 e + a^3 c^3 e^3, vars, mkrep[vars,r]]*)

(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] := cycle1[rmsqrs[expr, vars, reps], vars, cl, map];
(*vars={a,c,e}; {cl,map}=mkcycls[vars];
cycle2[3 r a^3 e + a^b c^b e^b, vars, cl, map, mkrep[vars, r]]*)

(* make cyclic equations *)
Clear[mkeqcycs, getcycmat, getcycmats];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs, k, ls = {}, cvar, xp},
  Xs = Product[r - b vars[[k]]^(b-1), {k, Length[vars]}];
  For [ k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[ cycle2[Xs cvar[[2]], vars, cl, map, reps] / cvar[[3]] ];
    xp[[k]] -= X;
    ls = Append[ls, xp];
  ];
  ls
];
(*
vars=getvars[3]; {cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mkrep[vars, r], X];
Timing[det = Factor[Det[mat]]]
Factor[det/.{X->-1}]
*)

getcycmat[n_, r_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mkrep[vars, r];
  {cl,map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]
];

getcycmats[n_, r_, X_] := Table[ getcycmat[d, r, X], {d, Divisors[n]} ];
(*getcycmats[3, r, X]*)

(* degree in r for the cyclic matrix, the primitive factor, and the original factor *)
Clear[degr, degX, degXp, degrp];
degr[n_] := Sum[EulerPhi[n/d] If[Mod[n/d, 2] == 0, 3^d, (3^d+1)/2], {d, Divisors[n]}];
degX[n_] := degr[n]/n;
(*
degXp[n_] := DirichletConvolve[degX[m], MoebiusMu[m], m, n];
degXp2[n_] := Sum[degX[n/d] MoebiusMu[d], {d, Divisors[n]}]; (* for v6 *)
degrp[n_] := degXp[n]*n;
*)
degrp[n_] := Sum[MoebiusMu[n/d] If[Mod[n/d, 2] == 0, 1, (3^d + 1)/2], {d, Divisors[n]}];
degXp[n_] := degrp[n]/n;

(*Print[Table[{degr[i], degrp[i]}, {i,1,10}]]*)

Clear[xsave, xload];

xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If [ verbose,
    Print[ If [ append, "appending ", "writing "], fn ]
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
mkcprod[poly_, r_, X_, n_, usen_: False] := Module[{Y, pd = 1, k},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    pd *= poly /. { X -> Y * (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression *)
  If [ usen,
    trigsimp[pd, {r, Y}, True],
    (* this code is somewhat faster than the direct call of trigsimp[] *)
    Collect[pd, {r, Y}, trigsimp0]
  ] /. { Y -> X^(1/n) }
];


(* compute the cyclotomic product of `poly' *)
mkctprod[poly_, r_, X_, n_, usen_: False] := Module[{pd = 1, k, prec},
  If [ n === 1, Return[poly] ];

  (* form the product *)
  For [ k = 0, k < n, k++,
    If [ GCD[k, n] != 1, Continue[] ];
    pd *= poly /. { X -> (Cos[2 k Pi/n] + I Sin[2 k Pi/n]) }
  ];
  (* simplify the expression
     guess the initial precision *)
  prec = Round[ n * (Exponent[poly, X] + 1) * 1.5 + 10 ];
  trigsimp[pd, r, usen, prec]
];

(* solve the primitive polynomial for n-cycles *)
symprimfac[n_, r_, X_, mats_:None, usen_:False, notn_:False] := Module[
  {k, dls, pf, pf1, d, mu, mat},

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
      mat = getcycmat[d, r, X],
      mat = mats[[k]]
    ];
    pf1 = Factor[ Det[ mat ] ];
    pf1 = mkcprod[pf1, r, X, n/d, usen];
    (* if n === d is excluded, compute the inverted polynomial *)
    If [ notn, mu *= -1 ];
    If [ mu === 1, pf *= pf1, pf /= pf1 ]
  ];
  nicefmt[ Cancel[pf], r ]
];
(*
Print[ symprimfac[3, r, X, None, True] ];
Exit[1];
*)

(* compute the polynomial at the intersection of n- and d-cycles *)
calcgnk[n_, d_, r_, X_, mats_:None, usen_: False] := Module[{p,lam},
  p = symprimfac[d, r, X, mats, usen] /. {X -> lam};
  Factor[ mkctprod[p, r, lam, n/d, usen] ]
];
(* ******************** Symbolic solution ends ************************* *)



(* ******************** Root finding/printing begins ******************* *)
(* wrapper for NSolve. *)
Clear[nsolve, solveT];
nsolve[ieq_, x_, prec_: 10] := Module[{k, eq, sols},
  eq = If [Head[ieq] === Equal, ieq, ieq == 0 ];
  sols = NSolve[eq, x, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]
];
(* nsolve[x^3 + 2 x -1, x] *)
(* ****************** Root finding/printing ends *********************** *)



(* ***************** NEW Lagrange interpolation begins ******************** *)
Clear[interp, numdet];

interp[ls_, r_] := Factor[ InterpolatingPolynomial[ls, r] ];

(* interpolate polynomials of `a', coefficients are polynomials of `b'
   Note: although InterpolatingPolynomial[] officially allows polynomials
   as coefficients, the performance sucks, so interp1[] is faster
   than interp[] in this case *)
interp1[xy_, a_, b_] := Module[{n = Length[xy], p, p1, ls, k, kmax},
  p = Table[ xy[[l]][[2]], {l, n} ];
  kmax = Max[ Exponent[p, b] ]; (* highest power in `b' *)
  ls = Table[
    (* get the `b' coefficient list with padding zeros *)
    PadRight[ CoefficientList[ Expand[ p[[k]] ], b ], kmax + 1 ],
  {k, n}];
  For[p = 0; k = 1, k <= kmax + 1, k++,
    (* interpolate a polynomial of `A' for each b^k *)
    p1 = InterpolatingPolynomial[
      Table[ { xy[[l]][[1]], ls[[l]][[k]] }, {l, n} ],
      a ];
    p += Factor[p1] b^(k-1);
  ];
  nicefmt[p, a]
];

(* extend to the negative side
   with xk = (-)^k i yk, r = -r', yk and r' satisfy the same map
   thus, if the cycle period is a multiple of 4
   r and r' = -r are the same, so any polynomial must be even *)
mksym[xy_] := Module[{k, ls = {}},
  For [ k = 1, k <= Length[xy], k++,
    ls = Append[ls, xy[[k]]];
    If [ xy[[k]][[1]] != 0,
      ls = Append[ ls, {-xy[[k]][[1]], xy[[k]][[2]]} ]
    ];
  ];
  ls
];


(* evaluate the primitive polynomial at a few r values *)
numdet[n_, Xv_, r_, X_, ms_:None, dn_:None, k0_:None, k1_:None,
       fn_:None, xy0_:{}, dr_:1, norm_:True] :=
  Module[{mats = ms, mat, den = dn, denv, xy = xy0, rv, Pv,
          deg = degrp[n], k, kmin = k0, kmax = k1, ttl = 1},

  If [ mats === None, mats = getcycmats[n, r, X]; ];
  mat = mats[[-1]] /. {X -> Xv};

  (* `den' is the contribution from shorter d-cycles d|n *)
  If [ den === None, den = symprimfac[n, r, X, mats, True, True]; ];
  If [ norm, ttl = If[n == 1, 2, 2^degXp[n]]; ];
  den = ( den /. {X -> Xv} ) * ttl;

  If [ kmin === None, kmin = -Round[deg/2 + 1]; ];
  If [ kmax === None, kmax = Round[deg/2 + 10000]; ];
  If [ Mod[n, 4] === 0,
    kmin = 0;
    deg /= 2;
  ]; (* n % 4 == 0, no r^odd terms, see mksym[] *)

  For [ k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; (* free some memory *)
    rv = k dr;
    denv = den /. {r -> rv};
    (* leave if a divide by zero is encountered *)
    If [ denv === 0, Continue[] ];
    (* compute the value of the polynomial at r = rv *)
    Pv = Cancel[ Det[ mat /. {r -> rv} ] / denv ];
    (* add the new value to the list *)
    xy = Append[xy, {rv, Pv}];
    If [ !(fn === None), xsave[fn, {rv, Pv}, True]; ];
  ];
  If [ Mod[n, 4] === 0, xy = mksym[xy] ];
  (* Print[xy]; *)
  xy
];

(*
Print[ interp[ numdet[4, -1, r, X], r ] ]; Exit[1];
*)

(* solve the general boundary condition, faster version of symprimfac[] *)
numdetX[n_, r_, X_, mats_:None, den_:None, fn_:None, dr_:1] := 
    Module[{ls, Y},
  ls = numdet[n, Y, r, X, mats, den, None, None, fn, {}, dr, False];
  nicefmt[ interp1[ls, r, Y] /. {Y -> X}, r]
];

(*
Print[ numdetX[2, r, X] // InputForm ]; Exit[1];
*)

(* ***************** NEW Lagrange interpolation ends ********************** *)



(* main function starts here *)

(* 1. handling input arguments *)
n = 3;
If [ Length[$CommandLine] >= 2,
  n = ToExpression[ $CommandLine[[2]] ]
];

ch = "a";
If [ Length[$CommandLine] >= 3,
  ch = $CommandLine[[3]]
];
lambda = If [ ch === "b", -1,
         If [ ch === "a", 1,
                          0 ] ];

Print["n ", n, "; lambda ", ch, " ", lambda, "; degr. ", degrp[n]];


(* 2. reading or computing the matrix *)
fnmats = "cmats"<>ToString[n]<>".txt";
If [ FileType[fnmats] === File,
  tm = Timing[
    mats = xload[fnmats];
  ][[1]];
  Print["matrix loaded from ", fnmats, ", time ", tm],

  tm = Timing[
    mats = getcycmats[n, r, X];
  ][[1]];
  Print["computing mats: ", tm];
  xsave[fnmats, mats];
];


(* 3. computing the determinant of the matrix *)
If [ lambda === 0,

  (* `ch' === 'c' or 'x', symbolically compute the determinant *)
  If [ ch === "c",
    tm = Timing[
      (* poly = symprimfac[n, r, X, mats, True]; *)
      poly = numdetX[n, r, X, mats];
      (* poly = nicefmt[ poly /. {X -> 3^n - 2 r Y}, Y ]; *)
    ][[1]];
    Print["time for primitive polynomial ", tm];
    xsave["crX" <> ToString[n] <> ".txt", poly, False, True],

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
      poly = calcgnk[n, d, r, X, mats, True];
    ][[1]];
    Print["time ", tm, ", polynomial ", n, " and ", d];
    If [ n < 10, Print[poly] ];
    xsave["cr" <> ToString[d] <> "x" <> ToString[n] <> ".txt", poly, False, True];
  ],

  (* `ch' === 'a' or 'b', numerically compute the onset or bifurcation point *)
  kmin = kmax = None;
  If [ Length[ $CommandLine ] >= 5,
    {kmin, kmax} = { ToExpression[ $CommandLine[[-2]] ],
                     ToExpression[ $CommandLine[[-1]] ] }
  ];
  Print["kmin ", kmin, ", kmax ", kmax];

  If [ kmin < kmax || kmin === None,
    fnls = If [ n > 6, "cls" <> ToString[n] <> ch <> ".txt", None ];
    If [ kmin === None && !(fnls === None),
      Close[OpenWrite[fnls]] (* clear the list *)
    ];
    tm = Timing[
      xy = numdet[n, lambda, r, X, mats, None, kmin, kmax, fnls];
    ][[1]];
    Print["computing Dets: ", tm];
    If [ Length[xy] >= degrp[n] + 1,
      tm = Timing[
        poly = interp[xy, r];
      ][[1]];
      Print["interpolation and factorization: ", tm];
      If [ n <= 4, Print[ poly ] ];
      fnr = "cr" <> ToString[n] <> ch <> ".txt";
      xsave[fnr, poly, False, True];
      sols = nsolve[poly, r];
      xsave[fnr, sols, True, False];
    ]
  ]
];

