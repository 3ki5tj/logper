(* solving the onset and bifurcation polynomials
   by computing the resultant *)

(* 1. handle input arguments *)
n = 5;
If[Length[$CommandLine] >= 2, n = ToExpression[$CommandLine[[2]]]];
ch = "a";
If[Length[$CommandLine] >= 3, ch = $CommandLine[[3]]];
d = If[ch == "b", -1, 1];
Print["n ", n, "; d ", d];

(* 2. compute the resultant *)
Clear[f, fn, dfn, x, R]
f[x_] := R - x^2;
fn[x_] := Nest[f, x, n];
dfn[x_] := D[fn[x], x];
Timing[poly = Resultant[fn[x] - x, d - dfn[x], x];]
Print[ Factor[(poly /. {R->T/4})] // InputForm ];


