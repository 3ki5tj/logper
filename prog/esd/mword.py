#!/usr/bin/env python

''' list median words shorter than a certain length
    Copyright (C) 2013 Cheng Zhang
    cf. Chapter 3.3 of ``Elementary Symbolic Dynamics'' B-l, Hao '''

def ncom(a, b):
  ''' length of the common leading part of two given words '''
  n = min(len(a), len(b))
  for i in range(n):
    if a[i] != b[i]: return i
  return n

def mword(w1, w2):
  ''' median word between w1 and w2 '''
  hw1 = w1 + "RL"[ w1.count("R") % 2 ] + w1 # harmonic word of w1
  aw2 = w2 + "LR"[ w2.count("R") % 2 ] + w2 # aharmonic word of w2
  nc = ncom(hw1, aw2)
  if nc > 2 * len(hw1): return hw1
  elif nc < len(aw2): return hw1[:nc]
  else: return mword(w1, aw2) # iterate

def lsw(n, w1, w2):
  ''' list median words between w1 and w2 (exclusive) with length < n '''
  m = mword(w1, w2)
  if m == w2 or m == w1 or len(m) >= n: return []
  else: return lsw(n, w1, m) + [m,] + lsw(n, m, w2)

import sys
n = 7
if len(sys.argv) > 1: n = int(sys.argv[1])
w1, w2 = "", "R" + "L" * (n - 2) # the first and last words
print '\n'.join( [w1,] + lsw(n, w1, w2) + [w2,] )

