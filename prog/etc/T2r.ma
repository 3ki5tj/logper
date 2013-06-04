(* convert the T-values to r-values for the logistic map *)
xload[fn_, verbose_: False] := Module[{fp, xp},
  If[verbose, Print["reading ", fn]];
  fp = OpenRead[fn]; xp = Read[fp, Expression]; Close[fp]; xp];

xsave[fn_, xp_, append_: False, verbose_: False] := Module[{fp, s},
  If[verbose, Print[If[append, "appending ", "writing "], fn]];
  fp = If[append, OpenAppend[fn], OpenWrite[fn]]; Write[fp, xp]; Close[fp];];

(* solve equations for T = 4R for the logistic map *)
T2r[Ts_, prec_:21] := Module[{n = Length[Ts], i, rs = {}, T, r, s, str = ""},
  For[i = 1, i <= n, i++,
    T = Ts[[i]];
    r = 1 + Sqrt[1 + T];
    rs = Append[rs, r];
    s =    "r: "     <> ToString[N[r,prec]]
        <> ", r/4: " <> ToString[N[r/4,prec]]
        <> "; 4R: "  <> ToString[N[T,prec]]
        <> ", R: "   <> ToString[N[T/4,prec]];
    str = str <> s <> "\n";
  ];
  str
];

fnin = "sol11a.txt";
fnout = "r11a.txt";

If[Length[$CommandLine] >= 2, fnin = $CommandLine[[2]]];
If[Length[$CommandLine] >= 3, fnout = $CommandLine[[3]]];

Ts = xload[fnin];
rs = T2r[Ts];
fp = OpenWrite[fnout];
WriteString[fp, rs];
Close[fp];

