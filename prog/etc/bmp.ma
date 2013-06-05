(* translate a polynomial into bytes
   Cheng Zhang Copyright (c) 2013
   The output has (degree+1)-lines, in each line,
   the first number is the sign
   the following numbers are the coefficents *)
fninp = "T4b.txt";
fnout = "bit.txt";

If [ Length[$CommandLine] > 1,
  fninp = $CommandLine[[2]];
];

If [ Length[$CommandLine] > 2,
  fnout = $CommandLine[[3]];
];

Print["Reading the polynomial in ", fninp];
fp = OpenRead[fninp];
poly = Read[fp, Expression];
Close[fp];

ls = CoefficientList[poly, T];
n = Length[ls];
bits = Table[
  Join[{If [ls[[k]] > 0, 1, 0] },  Reverse[IntegerDigits[Abs[ ls[[k]] ], 256]] ],
  {k, n}
];
fp = OpenWrite[fnout];
Write[fp, n];
Write[fp, Max[ Table[Length[ bits[[k]] ], {k, n}] ] ];
For [ k = 1, k <= n, k++, Write[fp, bits[[k]] ]; ];
Close[fp];

