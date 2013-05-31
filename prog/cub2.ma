(* Copyright 2012-2013 Cheng Zhang *)
(* Solving the boundary polynomials of the n-cycle
   of the cubic map f(u) = r^2 u (1 - u)^2
   which is related to f(z) = r^2 z (1 - z^2) by u = z^2
   we define R = 1/r^2 below
   USAGE
    math < cub.ma n ch kmin kmax
  `n' is the cycle period
  `ch': 'a' for the onset point, 'b' for the bifurcation point
        'X' or 'Y' for the general boundary polynomial
        'x' for the d x n cycle-intersection polynomial *)
(* Clear[Evaluate[Context[]<>"*"]] *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[If[ssub, var<>sub, SubscriptBox[var,sub]]];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: u1, u2, ... *)
getvars[n_, c_:"u"] := Table[mksub[c, i], {i, n}];

Clear[rotl, w2term, mkcycls, geteqv, cycle1, cycle2];
(* rotation to the left *)
rotl[w_, n_] := Module[{w1 = w*3, rs}, rs = Mod[w1, 3^n]; rs+(w1-rs)/3^n];
(* n=3; Table[{IntegerDigits[k,3,n], IntegerDigits[rotl[k, n],3,n]}, {k, 3^n - 1}] *)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[
  {xp = IntegerDigits[w,3,n]}, (* write `w' as a ternary interger *)
  Product[ vars[[n+1-k]]^xp[[k]], {k, n} ]
];
(*n=3; Table[w2term[w, n, getvars[n]], {w, 0, 3^n-1}]*)

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[{n, rvars, map, j, cid, w0, w, cl},
  n = Length[vars];
  rvars = Reverse[vars];
  map = Table[0, {k, 3^n}]; (* map to the cyclic id *)
  For [ cid = 0; cl = {}; w0 = 0, w0 < 3^n, w0++,
    If [ map[[w0+1]] != 0, (* already mapped *)
      Continue[];
    ];
    w = w0;
    map[[w+1]] = ++cid; (* new cyclic variable *)
    xp = w2term[w, n, vars];
    j = 0;
    While [ ++j < n,
      w = rotl[w, n];
      If [ map[[w+1]] != 0, Break[]; ];
      (* xp += w2term[w, n, vars]; *)
      map[[w+1]] = cid;
    ];
    (* a list of {var-name, expression-in-x, degeneracy, word} *)
    cl = Append[cl, {mksub["C", cid], xp, n/j, w0}];
    (*Print[{IntegerDigits[w0,2,n], cid, n/j, xp}];*)
  ];
  {cl, map}
];
(* mkcycls[getvars[3]] *)

(* map to a cyclic variable, b2k = {b^0, b^1, b^2, ...}
   return the {basic-var-id, coefficient}   *)
geteqv[t_, vars_, b2k_, map_] := Module[
  {e = Exponent[t, vars]},
  (* convert x0^e0 x1^e1 x2^e2 ... to {e0, e1, e2, ... } *)

  If [ Max[e] >= 3,
    Print[t, " is reducible"];
    Abort[];
  ];
  (* map the sequence of exponents to a b-nary word
     b2k = {b^0, b^1, b^2, ...}
     w = e0 b^0 + e1 b^1 + e2 b^2 + ...,  where e0 = 0..b-1 *)
  w = b2k . e;
  If [ w == 0,
    Return[ {map[[1]], t} ],
    Return[ {map[[w+1]],
             Coefficient[t, w2term[w, Length[vars], vars]]
            } ];
  ]
];
(*
vars = {x1, x2, x3, x4, x5, x6};
geteqv[3 r x1^2 x2 x4, vars, {1, 3, 9, 27, 81, 343}, mkcycls[vars][[2]] ]
*)


(* decompose an expression `expr' as a linear combination of basis variables *)
cycle1[expr_, vars_, cl_, map_] := Module[{ls, b2k, xp, k, cid, co},
  b2k = Table[3^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]} ];
  (* convert the monomial expression to a list *)
  xp = If [ Head[expr] === Plus, expr, {expr}];
  (* loop over monomial terms in `xp' *)
  For [ k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, b2k, map];
    If [ cid <= 0,
      Print["bad representation ", xp[[k]], " cid ", cid];
      Abort[];
    ];
    ls[[cid]] += co cl[[cid]][[3]];
  ];
  ls
];
(*vars={a,p,c,d,e,f}; {cl,map}=mkcycls[vars]; cycle1[3 r a p + r^2 a c e, vars, cl, map]*)

(* define replacement rules, x1^3 -> -x1 + 2 x1^2 + R x2, ... *)
Clear[mkrep, haspow, rmpows];
mkrep[vars_, R_] := Module[{n = Length[vars], c, ls = {}},
  For[c = 0, c <= 2, c++,
    ls = Join[ls, Table[vars[[k]]^(3+c) ->
         Expand[vars[[k]]^c (-vars[[k]] + 2 vars[[k]]^2 + R vars[[Mod[k, n] + 1]])], {k, n}]];
  ];
  ls
];
(* mkrep[getvars[4], R] *)

(* check if 'poly' has *at least* bth power of a variable *)
haspow[poly_, vars_] := (Max[Exponent[poly, vars]] >= 3);
(* haspow[a^b c^b e^b, {a, c, e}]*)

(* remove (b+1)th or higher powers *)
rmpows[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While [ haspow[xp, vars],
    xp = Expand[xp /. reps];
  ];
  xp
];
(* vars={a,c,e}; rmpows[r a^2 e + a^3 c^3 e^3, vars, mkrep[vars,R]] *)


(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] :=
  cycle1[rmpows[expr, vars, reps], vars, cl, map];
(*
vars={a,c,e}; {cl,map}=mkcycls[vars];
cycle2[3 r a^3 e + a^3 c^3 e^3, vars, cl, map, mkrep[vars, R]]
Exit[1]
*)

(* make cyclic equations *)
Clear[mkeqcycs, getcycmat, getcycmats];
mkeqcycs[vars_, cl_, map_, reps_, R_, X_] := Module[{Xs, k, n, ls = {}, cvar, xp},
  n = Length[vars];
  Xs = Product[1 - 4 vars[[k]] + 3 vars[[k]]^2, {k, n}];
  For [ k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[ cycle2[Xs cvar[[2]], vars, cl, map, reps] / cvar[[3]] ];
    xp[[k]] -= X R^n;
    ls = Append[ls, xp];
  ];
  ls
];
(*
vars=getvars[4]; {cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mkrep[vars, R], R, X];
Timing[det = Factor[Det[mat]];]
det // InputForm
Print[ Exponent[det, R] ]
Factor[Numerator[Together[det /. {R -> 1/r^2}]]] // InputForm
Exit[]
*)

getcycmat[n_, R_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mkrep[vars, R];
  {cl, map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, R, X]
];

getcycmats[n_, R_, X_] := Table[ getcycmat[d, R, X], {d, Divisors[n]} ];
(*getcycmats[3, R, X]*)

(* degree in R for the cyclic matrix, the primitive factor, and the original factor *)
Clear[degr, degX, degXp, degRp];
degR[n_] := Sum[EulerPhi[n/d] 3^d, {d, Divisors[n]}];
degX[n_] := degR[n]/n;
degRp[n_] := Sum[MoebiusMu[n/d] 3^d, {d, Divisors[n]}];
degXp[n_] := degRp[n]/n;
(*
Print[ Table[degRp[i], {i,10}] ];
Exit[1];
*)

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

  (* increase precision until results from two precisions match *)
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
symprimfac[n_, R_, X_, mats_:None, usen_:False, notn_:False] := Module[
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
      mat = getcycmat[d, R, X],
      mat = mats[[k]]
    ];
    pf1 = Factor[ Det[ mat ] ];
    pf1 = mkcprod[pf1, R, X, n/d, usen];
    (* if n === d is excluded, compute the inverted polynomial *)
    If [ notn, mu *= -1 ];
    If [ mu === 1, pf *= pf1, pf /= pf1 ]
  ];
  nicefmt[ Cancel[pf], R ]
];
(*
n = 4; p = symprimfac[n, R, X, None, True];
Print[ p // InputForm ];
Print[degRp[n], " vs ", Exponent[p, R]];
Exit[1];
*)


(* compute the polynomial at the intersection of n- and d-cycles *)
calcgnk[n_, d_, R_, X_, mats_:None, usen_: False] := Module[{p},
  p = symprimfac[d, R, X, mats, usen];
  Factor[ mkctprod[p, R, X, n/d, usen] ]
];
(* ******************** Symbolic solution ends ************************* *)



(* ******************** Root finding/printing begins ******************* *)
(* wrapper for NSolve. *)
Clear[nsolve, solveT];
nsolve[ieq_, x_, prec_: 10] := Module[{k, eq, sols},
  eq = If [Head[ieq] === Equal, ieq, ieq == 0 ];
  sols = NSolve[eq, x, Reals, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]
];
(* nsolve[x^3 + 2 x -1, x] *)
(* ****************** Root finding/printing ends *********************** *)



(* ***************** NEW Lagrange interpolation begins ******************** *)
Clear[interp, interp1, numdet, numdet1, numdet2];

interp[ls_, R_] := Factor[ InterpolatingPolynomial[ls, R] ];

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


(* evaluate the primitive polynomial at a few R values *)
numdet[n_, Xv_, R_, X_, ms_:None, dn_:None, k0_:None, k1_:None,
       fn_:None, xy0_:{}, dr_:1, norm_:True] :=
  Module[{mats = ms, mat, den = dn, denv, xy = xy0, Rv, Pv, p,
          deg = degRp[n], deg1, k, kmin = k0, kmax = k1, ttl = 1},

  If [ mats === None, mats = getcycmats[n, R, X]; ];
  mat = mats[[-1]] /. {X -> Xv};

  (* `den' is the contribution from shorter d-cycles d|n *)
  If [ den === None,
    den = symprimfac[n, R, X, mats, True, True];
  ];
  If [ norm, ttl = If[n == 1, 2, 2^degXp[n]]; ];
  den = ( den /. {X -> Xv} ) * ttl;

  (* the extra-factor R^deg1 *)
  deg1 = If [ n == 1, 1, deg/2 ];
  deg -= deg1;

  If [ kmin === None, kmin = -Round[deg/2 + 1]; ];
  If [ kmax === None, kmax = Round[deg/2 + 10000]; ];

  For [ k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; (* free some memory *)
    Rv = k dr;
    denv = den R^deg1 /. {R -> Rv};
    (* leave if a divide by zero is encountered *)
    If [ denv === 0, Continue[]; ];
    (* compute the value of the polynomial at R = Rv *)
    Pv = Cancel[ Det[ mat /. {R -> Rv} ] / denv ];
    (* add the new value to the list *)
    xy = Append[xy, {Rv, Pv}];
    If [ !(fn === None),
      Print["k ", k, ", deg ", deg, ", ", fn];
      xsave[fn, {Rv, Pv}, True];
    ];
  ];
  xy
];
(*
xy = numdet[4, -1, R, X];
p = Factor[ interp[xy, R] ];
Print[ p // InputForm ]; Exit[1];
*)

(* solve the general boundary condition, faster version of symprimfac[] *)
numdetX[n_, R_, X_, mats_:None, den_:None, kmin_:None, kmax_:None,
        fn_:None, dr_:1] := Module[{ls},
  ls = numdet[n, X, R, X, mats, den,
      kmin, kmax, fn, {}, dr, False];
  nicefmt[ interp1[ls, R, X], R ]
];

(*
Print[ numdetX[4, R, X] // InputForm ]; Exit[1];
*)

(* alternative to numdetX, better performance *)
numdetY[n_, R_, X_, ms_:None, dn_:None, l0_:None, l1_:None,
        fn_:None, dX_:1] :=
  Module[{mats = ms, den = dn, Xv, l, lmin = l0, lmax = l1,
    degx = degXp[n], ls = {}, xy, XP},

  (* make sure we have the matrix and denominator *)
  If [ mats === None, mats = getcycmats[n, R, X]; ];
  If [ den === None, den = symprimfac[n, R, X, mats, True, True]; ];

  If [ l0 === None, lmin = -Round[degx/2]; ];
  If [ l1 === None, lmax = Round[degx/2] + 10000; ];

  (* extrapolate a polynomial along X *)
  For [ l = lmin, l < lmax && Length[ls] < degx + 1, l++,
    ClearSystemCache[]; (* free some memory *)
    Xv = l dX;
    xy = numdet[n, Xv, R, X, mats, den,
        None, None, None, {}, 1, False];
    Pv = interp[xy, R];
    XP = {Xv, Pv};
    ls = Append[ls, XP];
    (*  Print["l ", l, ", deg ", degx, ", ", InputForm[XP]]; *)
    If [ !(fn === None),
      Print["l ", l, ", deg ", degx, ", ", fn];
      xsave[fn, XP, True];
    ];
  ];
  nicefmt[ interp1[ls, X, R], R ]
];

(*
Print[ numdetY[4, R, X] // InputForm ]; Exit[1];
*)

(* ***************** NEW Lagrange interpolation ends ********************** *)


(* convert a polynomial of R = 1/r^2 to a polynomial of r *)
R2r[p_] := nicefmt[ Factor[
  FromDigits[CoefficientList[p, R], r^2] (* reverse the polynomial *)
], r];



(* main function starts here *)

(* 1. handling input arguments *)
n = 3;
If [ Length[$CommandLine] >= 2,
  n = ToExpression[ $CommandLine[[2]] ];
];

ch = "a";
If [ Length[$CommandLine] >= 3,
  ch = $CommandLine[[3]];
];
If [ !MemberQ[{"a", "b", "X", "Y", "x"}, ch],
  Print["Do not support ", ch];
  Exit[1];
];

lambda = If [ ch === "b", -1,
         If [ ch === "a", 1,
                          0 ] ];

Print["n ", n, "; lambda ", ch, " ", lambda, " degr. ", degRp[n]];

kmin = kmax = None;
If [ Length[ $CommandLine ] >= 5,
  {kmin, kmax} = { ToExpression[ $CommandLine[[-2]] ],
                   ToExpression[ $CommandLine[[-1]] ] }
];
If [ !(kmin === None) && kmin >= kmax, Exit[]; ];
(* prepare a list to save intermediate values *)
diff = If [ MemberQ[{"X", "Y"}, ch], n + 1, n ];
fnls = If [ diff >= 7, "culs" <> ToString[n] <> ch <> ".txt", None ];
If [ kmin === None && !(fnls === None),
  Close[ OpenWrite[fnls] ]; (* clear the list *)
];
Print["kmin ", kmin, ", kmax ", kmax, ", fnls ", fnls];


(* 2. reading or computing the matrix *)
fnmats = "cumats" <> ToString[n] <> ".txt";
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


(* 3. computing the determinant of the matrix *)
If [ lambda === 0,

  (* `ch' === 'X' or 'x', symbolically compute the determinant *)
  If [ ch === "X" || ch === "Y",
    tm = Timing[
      (* poly = symprimfac[n, R, X, mats, True]; *)
      If [ ch === "X",
        poly = numdetX[n, R, X, mats, None, kmin, kmax, fnls],
        poly = numdetY[n, R, X, mats, None, kmin, kmax, fnls];
      ];
      (* poly = nicefmt[ poly /. {X -> 3^n - 2 R Y}, Y ]; *)
    ][[1]];
    Print["time for primitive polynomial ", tm];
    xsave["curX" <> ToString[n] <> ".txt", R2r[poly], False, True],

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
      poly = calcgnk[n, d, R, X, mats, True];
    ][[1]];
    Print["time ", tm, ", polynomial ", n, " and ", d];
    If [ n < 10, Print[poly] ];
    xsave["cur" <> ToString[d] <> "x" <> ToString[n] <> ".txt",
          R2r[poly], False, True];
  ],

  (* `ch' === 'a' or 'b', numerically compute the onset or bifurcation point *)
  tm = Timing[
    xy = numdet[n, lambda, R, X, mats, None, kmin, kmax, fnls];
  ][[1]];
  Print["computing Dets: ", tm];
  If [ Length[xy] >= Floor[degRp[n]/2] + 1,
    tm = Timing[
      poly = R2r[ interp[xy, R] ];
    ][[1]];
    Print["interpolation and factorization: ", tm];
    If [ n <= 4, Print["poly: ",  (poly // InputForm)] ];
    fnr = "cur" <> ToString[n] <> ch <> ".txt";
    xsave[fnr, poly, False, True];
  ]
];

