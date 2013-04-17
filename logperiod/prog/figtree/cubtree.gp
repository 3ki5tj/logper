unset multiplot
reset
set terminal postscript enhanced font "Times, 24"
set output "cubtree.ps"

set title "Cubic map"

set xlabel "{/Times-Italic r}" offset 0.0, 0.5
set xtics 0.5
set mxtics 5

set ylabel "{/Times-Italic x}" offset 1.5, 0.0
set ytics 0.5
set mytics 5

plot [1.8:3.0][:] "cubtree.dat" u 1:($5 == 0 ? $2 : 1/0) w d lc rgb "#ff0000" not, \
  "" u 1:($5 == 1 ? $2 : 1/0) w d lc rgb "#0000ff" not
  
unset output
set terminal wxt
reset
