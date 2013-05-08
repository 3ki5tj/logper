(* Copyright 2012-2013 Cheng Zhang *)
(* USAGE
    math < prd.ma n ch kmin kmax
   where
   `n' is the cycle period
   `ch' is `a' (onset), `b' (bifurcation) or `c' (general solution)
*)
(* Clear[Evaluate[Context[]<>"*"]] *)

precdef = 21; (* precision for numerical calculations *)

(* make subscript expression *)
ssub = True;
mksub0[var_, sub_] := ToExpression[If[ssub, var<>sub, SubscriptBox[var,sub]]];
mksub[var_, sub_] := mksub0[ToString[var], ToString[sub]];

(* define a list of variables: a1, a2, ... *)
getvars[n_] := Table[mksub["a", i], {i, n}];

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
(*vars={a1,a2,a3,a4,a5,a6}; geteqv[3 R a1 a2 a4, vars, {1, 2, 4, 8, 16, 32}, mkcycls[vars][[2]]]*)

cycle1[expr_, vars_, cl_, map_] := Module[{ls, pows, xp, k, cid, co},
  pows = Table[2^(k-1), {k, Length[vars]}];
  ls = Table[0, {k, Length[cl]}];
  xp = If[Head[expr] === Plus, expr, {expr}];
  For[k = 1, k <= Length[xp], k++,
    {cid, co} = geteqv[xp[[k]], vars, pows, map];
    ls[[cid]] += co cl[[cid]][[3]]; ];
  ls];
(*vars={a,b,c,d,e,f}; {cl,map}=mkcycls[vars]; cycle1[3 R a b + R^2 a c e, vars, cl, map]*)

(* define a square replacement rule, a1^2 -> R - a2, ... *)
Clear[mksqrrep, hassqr, rmsqrs];
mksqrrep[vars_, R_] := Module[{n = Length[vars]},
  Table[vars[[k]]^2 -> R - vars[[Mod[k, n] + 1]], {k, n}]];
(*mksqrrep[getvars[9],R]*)

(* check if 'poly' has *at least* square of a variable *)
hassqr[poly_, vars_] := (Max[Exponent[poly, vars]] >= 2);

(* remove square terms *)
rmsqrs[expr_, vars_, reps_] := Module[{xp = Expand[expr]},
  While[hassqr[xp, vars], xp = Expand[xp /. reps]]; xp];

(* cycle-and-sum an expression, allow square terms *)
cycle2[expr_, vars_, cl_, map_, reps_] := cycle1[rmsqrs[expr, vars, reps], vars, cl, map];
(*
vars={a,b,c,d,e,f};
{cl,map}=mkcycls[vars];
cycle2[0 R a^2 b + a^2 c^2 e^2, vars, cl, map, mksqrrep[vars, R]]
*)

(* make cyclic equations by expanding the product of Xs = a1...an, which is
   the last cyclic polynomial, and the kth cyclic polynomial cl[[k]] *)
Clear[mkeqcycs, getcycmat, getcycmats];
mkeqcycs[vars_, cl_, map_, reps_, X_] := Module[{Xs = cl[[-1]][[2]], k, ls = {}, cvar, xp},
  For[k = 1, k <= Length[cl], k++, (* for the kth cyclic variable *)
    (* get coefficients of the square-free expansion of the product
       Xs cl[[k]] of Xs and the kth cyclic polynomial *)
    cvar = cl[[k]]; (* e.g., {C12, a1a2, 1} and {C123, a1a2a3, 3} *)
    xp = Cancel[cycle2[Xs cvar[[2]], vars, cl, map, reps]/cvar[[3]]];
    xp[[k]] -= X;
    ls = Append[ls, xp];
    ]; ls];
(*
vars={a,b,c,d,e,f};
{cl,map}=mkcycls[vars];
mat=mkeqcycs[vars, cl, map, mksqrrep[vars, R], X]
*)

(* get the matrix that connects cyclic polynomials *)
getcycmat[n_, R_, X_] := Module[{vars = getvars[n], reps, cl, map},
  reps = mksqrrep[vars, R];
  {cl, map} = mkcycls[vars];
  mkeqcycs[vars, cl, map, reps, X]];

(* get the matrix of n, and those of divisors d of n *)
getcycmats[n_, R_, X_] := Module[{ls = {}},
  Do[ls = Append[ls, getcycmat[d, R, X]], {d, Divisors[n]}]; ls];
(* getcycmats[6, R, X] *)

(* degree in R of the primitive polynomial of n-cycles *)
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



(* ******************** Root finding/printing begins ******************* *)
(* wrapper for NSolve. *)
Clear[nsolve, solveT];
nsolve[ieq_, x_, prec_: precdef] := Module[{k, eq, sols},
  eq = If[Head[ieq] === Equal, ieq, ieq == 0];
  sols = NSolve[eq, x, WorkingPrecision -> prec];
  sols = Table[x/.sols[[k]], {k, Length[sols]}];
  Select[sols, Abs[Im[#]] < 10^-10 &]];
(* nsolve[x^3 + 2 x -1, x] *)

(* solve equations for T = 4R *)
solveT[eq_, x_, fn_, prec_: precdef] := Module[{facs, l, k, sols, ls = {}, r, cnt, s, str = ""},
  For[facs = FactorList[eq]; l = 1, l <= Length[facs], l++,
    sols = nsolve[facs[[l]][[1]], x, prec];
    For[cnt = 0; k = 1, k <= Length[sols], k++; cnt++,
      ls = Append[ls, sols[[k]]];
      r = 1 + Sqrt[1 + sols[[k]]];
      s = If[cnt == 0, "solutions of T^"<>ToString[ Exponent[facs[[l]][[1]], T] ]<>" ...:\n", ""];
      s = s<>"r: "<>ToString[r]<>", r/4: "<>ToString[r/4]
        <>"; 4R: "<>ToString[sols[[k]]]<>", R: "<>ToString[sols[[k]]/4];
      Print[s]; str = str<>s<>"\n";]];
  If[!(fn === None), fp = OpenWrite[fn]; WriteString[fp, str]; Close[fp]];
  ls];
(* ****************** Root finding/printing ends *********************** *)



(* for superstable cycles *)
fnss[R_, n_] := Module[{k, x, tab = Table[0, {i, n}], divs, mu, d},
  For[x = 0; k = 1, k <= n, k++, tab[[k]] = x = R - x^2];
  For[x = 1; divs = Divisors[n]; k = 1, k <= Length[divs], k++,
    d = divs[[k]]; mu = MoebiusMu[n/d];
    If[mu == 0, Continue[], If[mu == 1, x *= tab[[d]], x /= tab[[d]]]];
  ]; Cancel[x]]




(* *****************  Lagrange interpolation begins ******************** *)
(* return the value of the matrix for R = Rv and delta = 2 Pi I frac
normalized such that the coefficient of the highest power of 4R is 1 *)
Clear[numsolv0, numsolv1, numsolv2, numsolv2pt, interp];
numsolv0[n_, mat_, frac_, Rv_, norm_: True] := Together[
  Det[mat /. {R -> Rv, X -> Exp[2 Pi I frac]/(-2)^n}]*
    If[norm, 4^Sum[EulerPhi[n/d] 2^(d - 1), {d, Divisors[n]}], 1]];

(* return the value of the primitive factor for R = Rv, delta = Exp[I 2 Pi frac]
the formula is P(n, n) = prod_{d|n} B(d, n/d)^(mu(n/d))
where B(d, n/d) is the product of A(d) evaluated at all (n/d)th roots of delta *)
numsolv1[n_, mats_, frac_, Rv_] :=
  Module[{k, c, d, db, mu, divs, facs = 1, den1, poly, Q, deg, deg1, gcd, gcds, nf, df},
    For[divs = Divisors[n]; k = 1, k <= Length[divs], k++,(* divisors *)
      d = divs[[k]]; (* contribution from a period-d cycle *)
      mu = MoebiusMu[n/d];
      If[mu == 0, Continue[]];
      {nf, df} = {Numerator[frac], Denominator[frac]};
      (* for c=0,...,db-1 select typical c's *)
      For[gcds = {}; db = n/d; c = 0, c < db, c++,
        (* c/db+nf/(df db)= (c df+nf)/(df db); *)
        gcd = GCD[c df + nf, df db];
        If[MemberQ[gcds, gcd], Continue[], gcds = Append[gcds, gcd]];
        den1 = numsolv0[d, mats[[k]], frac/db + c/db, Rv, True];
        deg1 = EulerPhi[Denominator[Together[(frac + c)/db]]]; (* expected degree *)
        (*Print["den1 ",den1];*)
        poly = CoefficientList[MinimalPolynomial[den1, Q], Q];
        deg = Length[poly] - 1;
        (* this gives the product of roots of the minimal polynomial *)
        den1 = (poly[[1]]/poly[[-1]]) (-1)^deg;
        (*Print["factor ",den1, ", n ", n, ", d ",d, ", (c,db,frac) ",{c, db,frac,poly,deg}];*)
        If[Mod[deg1, deg] != 0, Print["bad degree ", {deg, deg1}];
          Return[{0, False}]];
        den1 = den1^(deg1/deg);
        If[mu == 1, facs *= den1, If[den1 == 0, Return[{0, False}]]; facs /= den1]];
    ]; {facs, True}];
(*numsolv1[6,getcycmats[6,R,X],1,8/4]*)

(* evaluate a few R values *)
numsolv2pt[n_, mats_, frac_, k0_:None, k1_:None, fn_: None, xy0_:{}, dR_:1/4] :=
    Module[{k, xy = xy0, Rv, Pv, good, deg = degRp[n], kmin = k0, kmax = k1},
  If[kmin === None || kmax === None, kmin = -Round[deg/2+1]; kmax = -kmin + 10000];
  For[k = kmin, k < kmax && Length[xy] < deg + 1, k++,
    ClearSystemCache[]; Rv = k dR;
    {Pv, good} = numsolv1[n, mats, frac, Rv];
    If[!good, Continue[]];
    xy = Append[xy, {Rv, Pv}];
    If[!(fn === None), xsave[fn, {Rv, Pv}, True]];
  ]; xy];

interp[xy_, R_] := Factor[InterpolatingPolynomial[xy, R]];

numsolv2[n_, mats_, frac_, dR_: 1/4] :=
  interp[numsolv2pt[n, mats, frac, None, None, None, {}, dR], R];
(* *****************  Lagrange interpolation ends ********************** *)



(* ******************** Symbolic solution begins *********************** *)
Clear[trigsimplify, mkcprod, mkctprod, symprimfac, calcgnk]

(* simplify an *integral* expression of sines and cosines *)
trigsimplify0[p_] := FullSimplify[TrigReduce[TrigExpand[p]]];

(* the numerical version `usen' is faster
   the symbolic version is psychologically safer *)
trigsimplify[p_, vars_, usen_: False] := Module[{p1, p2, prec = 10},
  If[!usen, Return[Collect[trigsimplify0[p], vars]]];
  (* increase precision util results from two precisions match *)
  For[p1 = Collect[Expand[N[p, prec]], vars, Round], True, p1 = p2,
    prec += 10;
    (* we assume the coefficients are integers, so Round[] is safe *)
    p2 = Collect[Expand[N[p, prec]], vars, Round];
    If[p2 === p1, Break[]];];
  p1];

(* compute the cyclic product: prod_{k=0..n-1} poly[X^(1/k) e^(2*pi*i*k/n)] *)
mkcprod[poly_, R_, X_, n_, usen_: False] := Module[{Y, pd = 1, k},
  If[n == 1, Return[poly]];
  For[k = 0, k < n, k++, (* form the product *)
    pd *= poly /. {X -> Y (Cos[2 k Pi/n] + I Sin[2 k Pi/n])}];
  If[usen,
    trigsimplify[pd, {R, Y}, True],
    (* this code is somewhat faster than the direct call of trigsimplify[] *)
    Collect[pd, {R, Y}, trigsimplify0]] /. {Y -> X^(1/n)}
];
(* mkcprod[pf1, R, X, 9] *)

(* compute the cyclotomic version of `poly' *)
mkctprod[poly_, R_, X_, n_, usen_: False] := Module[{pd = 1, k, prec},
  If[n == 1, Return[poly]];
  For[k = 0, k < n, k++, (* form the product *)
    If[GCD[k, n] != 1, Continue[]];
  pd *= poly /. {X -> (Cos[2 k Pi/n] + I Sin[2 k Pi/n])}];
  trigsimplify[pd, R, usen]
];

(* solve the primitive polynomial for n-cycles *)
symprimfac[n_, mats_, R_, X_, usen_: False] := Module[{k, dls, pf, pf1, d, mu},
  dls = Divisors[n];
  For[pf = 1; k = 1, k <= Length[dls], k++,
    d = dls[[k]];
    mu = MoebiusMu[n/d];
    If[mu == 0, Continue[]];
    pf1 = Factor[Det[mats[[k]]]];
    pf1 = mkcprod[pf1, R, X, n/d, usen];
    If[mu == 1, pf *= pf1, pf /= pf1]
  ];
  Collect[Cancel[pf], R, Simplify]
  ];

(* compute the polynomial at the intersection of n- and d-cycles *)
calcgnk[n_, d_, R_, usen_: False] := Module[{p, X, lam},
  p = symprimfac[d, getcycmats[d, R, X], R, X] /. {X -> lam/(-2)^d};
  p = Numerator[Together[p]];
  mkctprod[p, R, lam, n/d, usen]];
(* ******************** Symbolic solution ends ************************* *)



(* main function starts here *)

(* 1. handle input arguments *)
n = 10;
If[Length[$CommandLine] >= 2, n = ToExpression[$CommandLine[[2]]]];
ch = "a";
If[Length[$CommandLine] >= 3, ch = $CommandLine[[3]]];
kmin = kmax = None;
If[Length[$CommandLine] >= 5,
  {kmin, kmax} = {ToExpression[$CommandLine[[-2]]],ToExpression[$CommandLine[[-1]]]}];
frac = If[ch == "b", 1/2, If[ch == "c", 0, 1]];
Print["n ", n, "; frac ", ch, " ", N[frac], "; kmin ", kmin, ", kmax ", kmax, "; primfac deg. ", degRp[n]];

(* 2. load or compute the matrix that connects the n cyclic polynomials
      by the square-free reduction, each element of the matrix is a
      polynomial of R, matrices of divisors of n are also obtained *)
fnmats = "mats"<>ToString[n]<>".txt";
If[FileType[fnmats] === File,
  mats = xload[fnmats]; Print["matrix loaded from ", fnmats], (* load previous matrices *)
  Print["computing mats: ", Timing[mats = getcycmats[n,R,X]][[1]]];
  xsave[fnmats, mats];
];

(* 3. compute the determinant of the matrix *)
If[frac == 0, (* do symbolic calculation for a general boundary polynomial *)
  (* caution, we use the numerical version, the last parameter is True *)
  tm = Timing[poly = symprimfac[n, mats, R, X, True];];
  Print["time ", tm];
  xsave["RX"<>ToString[n]<>".txt", poly];
  ,
  (* otherwise do numerical calculation for the onset or bifurcation point *)
  If[kmin < kmax || kmax == None,
    fnls = If[n > 8, "ls"<>ToString[n]<>ch<>".txt", None];
    If[kmin == None && !(fnls == None), Close[OpenWrite[fnls]]]; (* clear the list *)
    tm = Timing[ xy = numsolv2pt[n,mats,frac,kmin,kmax,fnls]; ][[1]];
    Print["computing det: ", tm];
    If[Length[xy] >= degRp[n] + 1,
      poly = Numerator[Together[interp[xy, R]]]/.{R->T/4};
      fnT = "T"<>ToString[n]<>ch<>".txt";
      xsave[fnT, poly, False, True];
      sols = solveT[poly, T, "r"<>ToString[n]<>ch<>".txt"];
      xsave[fnT, sols, True, False];
    ];
  ];
];
