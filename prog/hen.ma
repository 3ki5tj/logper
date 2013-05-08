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
rotl[w_, n_] := Module[{w1 = w*2}, If[BitGet[w1, n] == 1, w1 - 2^n + 1, w1]];
(*rotl[rotl[6, 3],3]*)

(* word to monomial *)
w2term[w_, n_, vars_] := Module[{xp = IntegerDigits[w,2,n]},
  Product[vars[[n+1-k]]^xp[[k]], {k, n}]];

(* Define cyclic polynomials *)
mkcycls[vars_] := Module[{n = Length[vars], rvars = Reverse[vars], map, j, cid, w0, w, cl},
  map = Table[0, {k, 2^n}]; (* map to the cyclic id *)
  For[cid = 0; cl = {}; w0 = 0, w0 < 2^n, w0++,
    If[map[[w0+1]] != 0, Continue[]];
    w = w0;
    map[[w + 1]] = ++cid; (* new cyclic variable *)
    xp = w2term[w, n, vars];
    j = 0;
    While[++j < n,
      w = rotl[w, n];
      If[map[[w+1]] != 0, Break[]];
      (* xp += w2term[w, n, vars]; *)
      map[[w+1]] = cid;];
    cl = Append[cl, {mksub["C", cid], xp, n/j, w0}];
    (*Print[{IntegerDigits[w0,2,n], cid, n/j, xp}];*)
  ]; {cl, map} ];
(*mkcycls[getvars[4]]*)

(* map to a cyclic variable *)
geteqv[t_, vars_, pows_, map_] := Module[{w = pows.Exponent[t, vars]},
  If[w == 0, {map[[1]], t}, {map[[w+1]], Coefficient[t, w2term[w, Length[vars], vars]]}] ];
(* vars=getvars[6]; geteqv[3 R x1 x2 x4, vars, {1, 2, 4, 8, 16, 32}, mkcycls[vars][[2]]] *)

cycle1[expr_, vars_, cl_, map_] := Module[{ls, pows, xp, k, cid, co},
  pows = Table[2^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]}];
  xp = If[Head[expr] === Plus, expr, {expr}];
  For[k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    ls[[cid]] += co cl[[cid]][[3]]; ];
  ls];
(*vars=getvars[6]; {cl,map}=mkcycls[vars]; cycle1[3 R x1 x2 + R^2 x1 x3 x5, vars, cl, map] *)

(* define a square replacement rule, x2^2 -> a + b x1 - x3, ... *)
Clear[mksqrrep, hassqr, rmsqrs];
mksqrrep[vars_, a_, b_] := Module[{n = Length[vars], ls = {}, j, xk2, mxp, xp},
  xk2[k_] := a + b vars[[Mod[k+n-2, n] + 1]] - vars[[Mod[k, n] + 1]];
  mxp[xp_, ls_] := Expand[Expand[Expand[xp]/.ls]/.ls]/.ls;
  For[j = 1, j <= n/2 + 1, j++,
    ls = Join[ls, mxp[Table[vars[[k]]^(2 j) -> xk2[k]^j, {k, n}], ls]];
    ls = Join[ls, mxp[Table[vars[[k]]^(2 j + 1) -> vars[[k]] xk2[k]^j, {k, n}], ls]];
  ]; ls];
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

jac1[b_, x_] := {{-2 x, 1}, {b, 0}};
jac[b_, vars_] := Module[{k, n = Length[vars], m = {{1, 0}, {0, 1}}},
   For[k = 1, k <= n, k++, m = m.jac1[b, vars[[k]]];]; m];
(* returns X^2 - 2 sigma X + (-b)^n == 0 *)
sigjac[b_, vars_] := Module[{m = jac[b, vars]},
   Cancel[(m[[1]][[1]] + m[[2]][[2]])/2]];

(* make cyclic equations *)
Clear[mkeqcycs,getmatcyc, getmatcycs];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs, k, ls = {}, cvar, xp},
  Xs = -2 X sigjac[b, vars];
  For[k = 1, k <= Length[cl], k++,(* for the kth cyclic variable *)
    cvar = cl[[k]]; (* e.g., {C12, x1 x2, 1} and {C123, x1 x2 x3, 3} *)
    xp = Cancel[cycle2[Xs cvar[[2]], vars, cl, map, reps]/cvar[[3]]];
    xp[[k]] += X^2 +(-b)^Length[vars];
    ls = Append[ls, xp];
    ]; ls];
(*vars=getvars[4]; {cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mksqrrep[vars, a, b], X]*)
(*det=Factor[Det[mat]]; Print[{det, Factor[det/.{X->1}], Factor[det/.{X->-1}]}];*)

getmatcyc[n_, a_, b_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mksqrrep[vars, a, b]; {cl,map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]];

getmatcycs[n_, a_, b_, X_] := Module[{ls = {}},
  Do[ls = Append[ls, getmatcyc[d, a, b, X]], {d, Divisors[n]}]; ls];
(*getmatcycs[4, a, b, X]*)

(* degree in R for the cyclic matrix, the primitive factor, and the original factor *)
Clear[degRp];
degRp[n_] := Sum[MoebiusMu[n/d] 2^(d-1), {d, Divisors[n]}];
(*Print[Table[degRp[i],{i,1,20}]]*)

Clear[xsave, xload];
xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];

xload[fn_, verbose_: False] := Module[{fp, xp},
  If[verbose, Print["reading ", fn]];
  fp = OpenRead[fn]; xp = Read[fp, Expression]; Close[fp]; xp];


Clear[minpoly];
minpoly[poly_, x_, X_, m_] := Module[{k, ls, n, vars, msym, svars, repsym},
  ls = CoefficientList[poly, x]; n = Length[ls] - 1;
  vars = getvars[n, "z"]; svars = getvars[n, "s"];
  msym = Table[SymmetricPolynomial[k, Table[vars[[l]]^m, {l, n}]], {k, n}];
  repsym = Table[svars[[k]] -> (-1)^k ls[[n + 1 - k]]/ls[[n + 1]], {k, n}];
  X^n + Sum[(-1)^k X^(n-k) SymmetricReduction[msym[[k]], vars, svars][[1]]/.repsym, {k, n}]];
(* minpoly[c - b x + a x^2 - x^3, x, X, 2] *)


Clear[symsolv0, symsolv1, interp, symsolv2pt, symsolv2];
symsolv0[n_, mat_, frac_] := Together[Det[mat /. {X -> Exp[2 Pi I frac]}]];

(* solve divisors, brute force
   prod_{d|n, d < n} B(d, n/d)^(mu(n/d)) *)
symdivs0[n_, mats_] := Module[{k, d, mu, divs, facs = 1, poly, Z},
  (* solve the problem symbolic for the divisors *)
  For[divs = Divisors[n]; k = 1, k < Length[divs], k++, (* divisors *)
    d = divs[[k]]; (* contribution from a period-d cycle *)
    mu = MoebiusMu[n/d];
    If[mu == 0, Continue[]];
    poly = Factor[minpoly[Det[mats[[k]] /. {X -> Z}], Z, X, n/d]];
    If[mu > 0, facs *= poly, facs = Cancel[facs/poly]];];
  facs];
(*Timing[symdivs0[6,getmatcycs[6,a,b,X]]]*)

(* return the value of the primitive factor for X
   the formula is P(n, n) = prod_{d|n} B(d, n/d)^(mu(n/d))
   where B(d, n/d, X) is the product of A(d, X^{d/n}) evaluated at
   different (n/d)th roots of X *)
symsolv1[n_, mats_, Xv_] := Module[{facs},
  facs = Factor[Det[mats[[-1]] /. {X -> Xv}]];
  Cancel[facs symdivs0[n, mats]/.{X->Xv}]];
(*n=3; Timing[sol=symsolv1[n,getmatcycs[n,a,b,X],1];][[1]]
sol = Factor[sol]*)


(* interpolate polynomials of b *)
interp[xy_, a_, b_, A_:None] := Module[{n = Length[xy], p, p1, ls, k, kmax},
  p = Table[xy[[l]][[2]], {l, n}];
  kmax = Max[Exponent[p, b]];
  ls = Table[PadRight[CoefficientList[Expand[p[[k]]],b], kmax+1], {k, n}];
  For[p = 0; k = 1, k <= kmax+1, k++,
    p1 = InterpolatingPolynomial[ Table[{xy[[l]][[1]], ls[[l]][[k]]}, {l, n}], a ];
    If[!(A === None), p1 = p1/.{a->A/4}];
    p += Factor[p1] b^(k-1);
  ]; p];

(* same as symsolv1 but uses interpolation, faster
   only return a list *)
symsolv2pt[n_, mats_, Xv_, k0_:None, k1_:None, fn_:None, df_:None, da_:1] :=
    Module[{facs, dfac, deg = degRp[n], av, k, y, cnt, ls = {}, kmin = k0, kmax = k1},
  y = Timing[dfac = If[df != None, df, symdivs0[n, mats]/.{X->Xv}];][[1]];
  Print["factor from divisors computed ", y];
  If[kmin === None || kmax === None, kmin = -Round[deg/2]; kmax = -kmin + 10000];
  For[k = kmin, k < kmax && Length[ls] < deg + 1, k++,
    ClearSystemCache[]; av = k da;
    facs = Factor[Det[mats[[-1]] /. {X -> Xv, a -> av}]];
    y = Cancel[facs dfac /. {a -> av}];
    ls = Append[ls, {av, y}];
    If[!(fn === None), xsave[fn, {av, y}, True]];
  ]; ls];
(*n = 6; Timing[ls=symsolv2pt[n,getmatcycs[n,a,b,X],-1];][[1]]
Timing[sol = interp[ls, a, b, A];][[1]]*)

symsolv2[n_, mats_, Xv_, da_:1] :=
  interp[symsolv2pt[n, mats, Xv, None, None, None, None, da], a, b, A];

n = 6;
If[Length[$CommandLine] >= 2, n = ToExpression[$CommandLine[[2]]]];
ch = "a";
If[Length[$CommandLine] >= 3, ch = $CommandLine[[3]]];
kmin = kmax = None;
If[Length[$CommandLine] >= 5,
  {kmin, kmax} = {ToExpression[$CommandLine[[-2]]],ToExpression[$CommandLine[[-1]]]}];
delta = If[ch === "b", -1, 1];
Print["n ", n, "; ch ", ch, " ", delta, "; kmin ", kmin, ", kmax ", kmax, "; primfac deg. ", degRp[n]];

(* reading or computing the matrix *)
fnmats = "hmats"<>ToString[n]<>".txt";
If[FileType[fnmats] === File,
  mats = xload[fnmats]; Print["matrix loaded from ", fnmats], (* load previous matrices *)
  Print["computing mats: ", Timing[mats=getmatcycs[n,a,b,X]][[1]]];
  xsave[fnmats, mats];
];

(* computing the determinant of the matrix *)
If[kmin < kmax || kmin === None,
  fnls = If[n > 1, "hls"<>ToString[n]<>ch<>".txt", None];
  If[kmin === None && !(fnls === None), Close[OpenWrite[fnls]]]; (* clear the list *)
  tm = Timing[ xy = symsolv2pt[n,mats,delta,kmin,kmax,fnls]; ][[1]];
  Print["computing det: ", tm];
  If[Length[xy] >= degRp[n] + 1,
    poly = interp[xy, a, b, A];
    fnab = "ab"<>ToString[n]<>ch<>".txt";
    If[!(ch === "b"), poly = Factor[poly]];
    xsave[fnab, poly, False, True];
  ];
];

