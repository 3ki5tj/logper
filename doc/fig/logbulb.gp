unset multiplot
reset
set terminal postscript enhanced size 7, 7 "Times, 14"
set output "logbulb.ps"

set xlabel "Re {/Times-Italic R}" offset 1.0, 0.0
set xtics 0.5
set mxtics 5

set ylabel "Im {/Times-Italic R}" offset 1.5, 0.0
set ytics 0.5
set mytics 5

set samples 200

plot [-0.6:2][-1.3:1.3] \
  "logp1.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#f02040" t "{/Times-Italic n} = 1", \
  "logp2.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#006000" t "{/Times-Italic n} = 2", \
  "logp3.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#202080" t "{/Times-Italic n} = 3", \
  "logp4.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#f0c000" t "{/Times-Italic n} = 4", \
  "logp5.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#ff60f0" t "{/Times-Italic n} = 5", \
  "logp6.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#505050" t "{/Times-Italic n} = 6", \
  "logp7.txt" u ($1/4):($2/4) w filledcurves ls 1 lw 0. lc rgb "#00ffff" t "{/Times-Italic n} = 7", \
  -2 not
  
unset output
set terminal wxt
reset
