old versions
~~~~~~~~~~~~
report.nb       summary of results
grobner.nb      Using Grobner basis


perX.nb         new pure cyclic polynomial approach (simpler and more compact code, but bit slower)
~~~~~~~
per10.nb  using integer preserving Gaussian eliminations to solve the equations
   * featuring cycsolve8[] using gedet0[],
   on T60, for just the onset eq. used 10s vs. 20s of cycsolve7[] for n = 8,
           used 200s for n = 9, 4563s (76 min) for n = 10
           for both eqs. used 13s for n = 8, 240s for n = 9.
   unfortunately, we still have no way to solve the discriminant/resultant effectively,
           which takes much longer.
           for n = 8, the discriminant/resultant(d = -1) takes 14s/24s,
           for n = 9, the same takes 902s/?s

--
per10pow.nb  expand using X^n as a linear combination of cyclic polynomials
--
periodX.nb   Using cyclic variables to solve equation (use symmetric polynomials as a jump board)
period7.nb   standard version (up to n = 7)
period9.nb   more efficient for larger n


ss.nb  self-contained version for superstable cycles

