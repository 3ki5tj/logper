(* Copyright 2012-2013 Cheng Zhang *)
(* remove trivial divisors from the output of cub0.ma,
   ** deprecated, minimal maintanence **
   e.g., input: cr6b0.ma, output: cr6b.ma
   math < cub0d.ma 6 b *)
(* Clear[Evaluate[Context[]<>"*"]] *)

b = 3; (* order of the map *)
drdef = 1; (* default interpolation step size *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[If[ssub, var<>sub, SubscriptBox[var,sub]]];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: x1, x2, ... *)
getvars[n_, c_:"x"] := Table[mksub[c, i], {i, n}];

Clear[rotl, w2term, mkcycls, geteqv, cycle1, cycle2];
(* rotation to the left *)
rotl[w_, n_, bn_] := Module[{w1 = w*b, r}, r = Mod[w1, bn]; r+(w1-r)/bn];

(* word to monomial *)
w2term[w_, n_, vars_] := Module[{xp = IntegerDigits[w,b,n]},
  Product[vars[[n+1-k]]^xp[[k]], {k, n}]];

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

(* map to a cyclic variable *)
geteqv[t_, vars_, pows_, map_] := Module[{xp = Exponent[t, vars]},
  If[Max[xp] >= b, Print[t, " is reducible"]; Abort[]]; w = pows.xp;
  If[w == 0, {map[[1]], t}, {map[[w+1]], Coefficient[t, w2term[w, Length[vars], vars]]}] ];

cycle1[expr_, vars_, cl_, map_] := Module[{ls, pows, xp, k, cid, co},
  pows = Table[b^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]}];
  xp = If[Head[expr] === Plus, expr, {expr}];
  For[k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    If[cid <= 0, Print["bad representation ", xp[[k]], " cid ", cid]; Abort[]];
    ls[[cid]] += co cl[[cid]][[3]]; ];
  ls];

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

(* remove square terms *)
rmsqrs[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While[haspow[xp, vars], xp = Expand[xp /. reps]]; xp];

(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] := cycle1[rmsqrs[expr, vars, reps], vars, cl, map];

(* make cyclic equations *)
Clear[mkeqcycs,getmatcyc, getmatcycsd];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs, k, ls = {}, cvar, xp},
  Xs = Product[r - b vars[[k]]^(b-1), {k, Length[vars]}];
  For[k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[cycle2[Xs cvar[[2]], vars, cl, map, reps]/cvar[[3]]];
    xp[[k]] -= X;
    ls = Append[ls, xp];
    ]; ls];

getmatcyc[n_, r_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mkrep[vars, r]; {cl,map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]];

getmatcycsd[n_, r_, X_] := Module[{ls = {}},
  Do[ls = Append[ls, getmatcyc[d, r, X]], {d, Drop[Divisors[n], -1]}];
  Append[ls, {{}}]];

(* degree in r for the cyclic matrix, the primitive factor, and the original factor *)
Clear[degr, degX, degXp, degrp];
degr[n_] := Sum[EulerPhi[n/d] If[Mod[n/d, 2] == 0, 3^d, (3^d+1)/2], {d, Divisors[n]}];
degX[n_] := degr[n]/n;
degrp[n_] := Sum[MoebiusMu[n/d] If[Mod[n/d, 2] == 0, 1, (3^d + 1)/2], {d, Divisors[n]}];
degXp[n_] := degrp[n]/n;

Clear[xsave, xload];
xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];
xload[fn_, verbose_: False] := Module[{fp, xp},
  If[verbose, Print["reading ", fn]];
  fp = OpenRead[fn]; xp = Read[fp, Expression]; Close[fp]; xp];

(* return the value of the matrix for r = rv and delta = 2 Pi I frac *)
Clear[numsolv0, numsolv1d, numsolv2d, numsolv2dpt, interp];
numsolv0[n_, mat_, frac_, rv_] := Together[Det[mat /. {r -> rv, X -> Exp[2 Pi I frac]}]];

(* return the value of the primitive factor for r = rv, delta = Exp[I 2 Pi frac]
the formula is P(n, n) = prod_{d|n} B(d, n/d)^(mu(n/d))
where B(d, n/d) is the product of A(d) evaluated at all (n/d)th roots of delta *)
numsolv1d[n_, mats_, frac_, Rv_] :=
  Module[{k, c, d, db, mu, divs, facs = 1, den1, poly, Q, deg, deg1, gcd, gcds, nf, df},
    For[divs = Divisors[n]; k = 1, k < Length[divs], k++,(* divisors < n *)
      d = divs[[k]]; (* contribution from a period-d cycle *)
      mu = MoebiusMu[n/d];
      If[mu == 0, Continue[]];
      {nf, df} = {Numerator[frac], Denominator[frac]};
      (* for c=0,...,db-1 select typical c's *)
      For[gcds = {}; db = n/d; c = 0, c < db, c++,
        (* c/db+nf/(df db)= (c df+nf)/(df db); *)
        gcd = GCD[c df + nf, df db];
        If[MemberQ[gcds, gcd], Continue[], gcds = Append[gcds, gcd]];
        den1 = numsolv0[d, mats[[k]], frac/db + c/db, Rv];
        deg1 = EulerPhi[Denominator[Together[(frac + c)/db]]]; (* expected degree *)
        poly = CoefficientList[MinimalPolynomial[den1, Q], Q];
        deg = Length[poly] - 1;
        (* this gives the product of roots of the minimal polynomial *)
        den1 = (poly[[1]]/poly[[-1]]) (-1)^deg;
        If[Mod[deg1, deg] != 0, Print["bad degree ", {deg, deg1}];
          Return[{0, False}]];
        den1 = den1^(deg1/deg);
        If[mu == 1, facs /= den1, If[den1 == 0, Return[{0, False}]]; facs *= den1]];
    ]; {facs, True}];
(*numsolv1d[3, getmatcycsd[3,r,X],1,4]*)

(* evaluate a few r values *)
numsolv2dpt[n_, mats_, frac_, k0_:None, k1_:None, fn_: None, xy0_: {}, dr_: drdef] :=
    Module[{k, xy = xy0, rv, Pv, good, kmin = k0, kmax = k1, deg = degr[n] - degrp[n],
    ttl = 2^If[n == 1, degX[n]-2, degX[n]-1-degXp[n]]},
  If[kmin === None || kmax === None,
    kmin = -Round[deg/2]; kmax = -kmin + 10000;];
  Print["deg is ", deg, ", ttl is ", ttl];
  For[k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; rv = k dr;
    {Pv, good} = numsolv1d[n, mats, frac, rv];
    If[!good, Continue[]];
    Pv /= ttl;
    xy = Append[xy, {rv, Pv}];
    If[!(fn === None), xsave[fn, {rv, Pv}, True]];
  ]; xy];

interp[xy_, r_] := Together[InterpolatingPolynomial[xy, r]];

numsolv2d[n_, mats_, frac_, dr_: drdef] := Module[{xy},
  xy = numsolv2dpt[n, mats, frac, None, None, None, {}, dr];
  interp[xy, r]];
(*Factor[numsolv2d[6, getmatcycsd[6,r,X], 1/2]]*)

(* ********************* main function starts here ********************* *)

(* handling input arguments *)
n = 3;
If[Length[$CommandLine] >= 2, n = ToExpression[$CommandLine[[2]]]];
ch = "a";
If[Length[$CommandLine] >= 3, ch = $CommandLine[[3]]];
frac = If[ch === "b", 1/2, 1];
Print["n ", n, "; frac ", ch, " ", N[frac], "; primfac deg. ", degrp[n]];

(* reading or computing the matrix *)
fnmats = "cmats"<>ToString[n]<>".txt";
If[FileType[fnmats] === File,
  mats = xload[fnmats]; Print["matrix loaded from ", fnmats], (* load previous matrices *)
  Print["computing divisor mats: ", Timing[mats = getmatcycsd[n,r,X]][[1]]];
];

(* computing the determinant of the matrix *)
tm = Timing[polyd = Factor[numsolv2d[n, mats, frac]];][[1]];
Print[polyd, "\ntime ", tm];

fnr0 = "cr"<>ToString[n]<>ch<>"0.txt";
fnr = "cr"<>ToString[n]<>ch<>".txt";
If[FileType[fnr0] == File,
  poly = xload[fnr0];
  poly = Cancel[poly/polyd];
  xsave[fnr, poly];];
