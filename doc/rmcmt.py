#!/usr/bin/env python

# remove comments

fnin = "pc-ijbc.tex"
fnout = "IJBC-D-12-00312R1.tex"

lines = open(fnin).readlines()
lines = [s for s in lines if
    not s.strip().startswith("%") and
    not s.strip().startswith("\\twocolumn") ]
for i in range(len(lines)):
  s = lines[i]
  pos = s.find("%")
  if pos >= 0:
    lines[i] = s[:pos] + "\n"
open(fnout, "w").writelines(lines)

