(* renormalization of the quadratic map
   cf. B-L Hao, Elementary Symbolic Dynamics, Sec. 2.7.2 Table 2.3 and Sec. 3.4.2-3.4.3 Table 3.4 *)
n = 3; (* number of terms *)
z = 2;  (* map is f = 1 + A1 x^z + A2 x^(2z) + ... *)
niter = 3; (* 2 for period-doubling, 3 for -trippling *)
A = Table[ToExpression[ToString["A"] <> ToString[k]], {k, 1, n}]; (* coefficients *)
f[x_] := 1 + A.Table[x^(z k), {k, 1, n}]; (* the map *)
del = f[a x] - a Nest[f, x, niter]; (* renormalization equation, del should be 0 *)
eqs = Table[Coefficient[del, x, j z], {j, 0, n}]; (* the coefficients of the x^(j z) term *)
init = Append[ (* initial guess for A1, A2, ..., and a *)
   Table[{A[[k]], If[k == 1, -1.5, 0] }, {k, 1, n}], {a, -8.5} ];
FindRoot[eqs, init, WorkingPrecision -> 10 + n]

