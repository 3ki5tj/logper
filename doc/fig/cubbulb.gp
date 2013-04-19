unset multiplot
reset
set terminal postscript enhanced size 7, 4 font "Times, 12"
set output "cubbulb.ps"

set size ratio 1.6/3.0

set xlabel "Re {/Times-Italic r}" offset .0, 0.0
set xtics 1.0
set mxtics 5

set ylabel "Im {/Times-Italic r}" offset 1.0, 0.0
set ytics 1.0
set mytics 5


plot [-3.:3.][-1.6:1.6] \
  "cubp1.txt"   u 1:2 w filledcurves ls 1 lw 0. lc rgb "#f02040" t "{/Times-Italic n} = 1", \
  "cubp2a.txt"  u 1:2 w filledcurves ls 1 lw 0. lc rgb "#006000" t "{/Times-Italic n} = 2", \
  "cubp2b.txt"  u 1:2 w filledcurves ls 1 lw 0. lc rgb "#006000" not, \
  "cubp3.txt"   u 1:2 w filledcurves ls 1 lw 0. lc rgb "#202080" t "{/Times-Italic n} = 3", \
  "cubp4a.txt"  u 1:2 w filledcurves ls 1 lw 0. lc rgb "#ffcc00" t "{/Times-Italic n} = 4", \
  "cubp4b.txt"  u 1:2 w filledcurves ls 1 lw 0. lc rgb "#ffcc00" not, \
  "cubp5.txt"   u 1:2 w filledcurves ls 1 lw 0. lc rgb "#ff60f0" t "{/Times-Italic n} = 5", \
  -2 not


  
unset output
set terminal wxt
reset
