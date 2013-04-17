unset multiplot
reset
set terminal postscript enhanced font "Times, 24"
set output "logtree.ps"

set title "Logistic map"

set xlabel "{/Times-Italic r}" offset 0.0, 0.5
set xtics 0.5
set mxtics 5

set ylabel "{/Times-Italic x}" offset 1.5, 0.0
set ytics 0.2
set mytics 2

plot [2.8:4.0][:] "logtree.dat" u 1:2 w d not
  
unset output
set terminal wxt
reset
