(* Copyright 2012-2013 Cheng Zhang *)
(* A variant of cub.ma for cycles of the cubic map
   This version does not attempt to remove factors from shorter cycles *)
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
(*n=3; Table[{IntegerDigits[k,b,n], IntegerDigits[rotl[k, n, b^n],b,n]}, {k, b^n - 1}]*)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[{xp = IntegerDigits[w,b,n]},
  Product[vars[[n+1-k]]^xp[[k]], {k, n}]];
(*n=3; Table[w2term[w, n, getvars[n]], {w, 0, b^n-1}]*)

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[{n = Length[vars], rvars = Reverse[vars], bn, map, j, cid, w0, w, cl, xp},
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
geteqv[t_, vars_, pows_, map_] := Module[{xp = Exponent[t, vars], w},
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
Clear[mkeqcycs,getmatcyc, getmatcycs];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs, k, ls = {}, cvar, xp},
  Xs = Product[r - b vars[[k]]^(b-1), {k, Length[vars]}];
  For[k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[cycle2[Xs cvar[[2]], vars, cl, map, reps]/cvar[[3]]];
    xp[[k]] -= X;
    ls = Append[ls, xp];
    ]; ls];
(*
vars=getvars[3]; {cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mkrep[vars, r], X];
Timing[det = Factor[Det[mat]]]
Factor[det/.{X->-1}]
*)

getmatcyc[n_, r_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mkrep[vars, r]; {cl,map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]];

getmatcycs[n_, r_, X_] := Module[{ls = {}},
  Do[ls = Append[ls, getmatcyc[d, r, X]], {d, Divisors[n]}]; ls];
(*getmatcycs[3, r, X]*)

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
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];
xload[fn_, verbose_: False] := Module[{fp, xp},
  If[verbose, Print["reading ", fn]];
  fp = OpenRead[fn]; xp = Read[fp, Expression]; Close[fp]; xp];

(* wrapper for NSolve. *)
Clear[nsolve, solveT];
nsolve[ieq_, x_, prec_: 10] := Module[{k, eq, sols},
  eq = If[Head[ieq] === Equal, ieq, ieq == 0];
  sols = NSolve[eq, x, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]];
(* nsolve[x^3 + 2 x -1, x] *)

(* return the value of the matrix for r = rv and delta = 2 Pi I frac *)
Clear[numsolv0, numsolv1, numsolv2, numsolv2pt, interp];
numsolv0[n_, mat_, frac_, rv_] := Together[Det[mat /. {r -> rv, X -> Exp[2 Pi I frac]}]];

(* evaluate a few r values *)
numsolv2pt[n_, mats_, frac_, k0_:None, k1_:None, fn_: None, xy0_: {}, dr_: drdef] :=
    Module[{k, xy = xy0, rv, Pv, good, kmin = k0, kmax = k1,
      deg = degr[n], ttl = 2^(degX[n]-1)},
  If[kmin === None || kmax === None,
    kmin = -Round[deg/2]; kmax = -kmin + 10000;
    If[Mod[n, 2] == 0, kmin = 0; deg /= 2;]; (* even n, no r^odd terms *)
  ];
  For[k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; rv = k dr;
    Pv = numsolv0[d, mats[[-1]], frac, rv];
    Pv /= ttl;
    xy = Append[xy, {rv, Pv}];
    If[! (fn === None), xsave[fn, {rv, Pv}, True]];
  ]; xy];

(* extend to the negative side *)
mksym[xy_] := Module[{k, ls = {}},
  For[k = 1, k <= Length[xy], k++,
    ls = Append[ls, xy[[k]]];
    If[xy[[k]][[1]] != 0, ls = Append[ls, {-xy[[k]][[1]], xy[[k]][[2]]}]];
  ]; ls];

interp[xy_, r_] := Together[InterpolatingPolynomial[xy, r]];

numsolv2[n_, mats_, frac_, dr_: drdef] := Module[{xy},
  xy = numsolv2pt[n, mats, frac, None, None, None, {}, dr];
  If[Mod[n, 2] == 0, xy = mksym[xy]]; interp[xy, r]];


(* USAGE
  math < cub.ma n ch kmin kmax
  n is the cycle period
  ch is a or b for onset or bifurcation *)

(* handling input arguments *)
n = 3;
If[Length[$CommandLine] >= 2, n = ToExpression[$CommandLine[[2]]]];
ch = "a";
If[Length[$CommandLine] >= 3, ch = $CommandLine[[3]]];
kmin = kmax = None;
If[Length[$CommandLine] >= 5,
  {kmin, kmax} = {ToExpression[$CommandLine[[-2]]],ToExpression[$CommandLine[[-1]]]}];
frac = If[ch === "b", 1/2, 1];
Print["n ", n, "; frac ", ch, " ", N[frac], "; kmin ", kmin, ", kmax ", kmax, "; deg. ", degr[n]];

(* reading or computing the matrix *)
fnmats = "cmats"<>ToString[n]<>".txt";
If[FileType[fnmats] === File,
  mats = xload[fnmats]; Print["matrix loaded from ", fnmats], (* load previous matrices *)
  Print["computing mats: ", Timing[mats=getmatcycs[n,r,X]][[1]]];
  xsave[fnmats, mats];
];

(* computing the determinant of the matrix *)
If[kmin < kmax || kmin === None,
  fnls = If[n > 6, "cls"<>ToString[n]<>ch<>"0.txt", None];
  If[kmin === None && !(fnls === None), Close[OpenWrite[fnls]]]; (* clear the list *)
  tm = Timing[ xy = numsolv2pt[n,mats,frac,kmin,kmax,fnls]; ][[1]];
  Print["computing det: ", tm];
  If[Mod[n, 2] == 0, xy = mksym[xy]];
  If[Length[xy] >= degr[n] + 1,
    poly = Factor[interp[xy, r]];
    fnr = "cr"<>ToString[n]<>ch<>"0.txt";
    xsave[fnr, poly, False, True];
    sols = nsolve[poly, r];
    xsave[fnr, sols, True, False];
  ];
];

