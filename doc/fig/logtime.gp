unset multiplot
reset
set terminal postscript enhanced "Times, 22"
set output "logtime.ps"

# make `-' longer
set encoding iso_8859_1

lbfont = "Times, 22"

set xlabel "{/Times-Italic n}" offset 0.0, 0.0 font lbfont
set xtics 1
# set mxtics 5

set ylabel "Time (seconds)" offset 0.0, 0.0 font lbfont
set logscale y
set format y "10^{%L}"
# set ytics 0.5
# set mytics 5


set key bottom spacing 1.0

plot [6:12][:] \
  "logtime.dat" u 1:($4+$12)  w lp pt 5 ps 2 lw 2 t "This method, Mathematica", \
  ""            u 1:15        w lp pt 6 ps 2 lw 2 t "Gr{\366}bner basis, Magma", \
  ""            u 1:16        w lp pt 7 ps 2 lw 2 t "Gr{\366}bner basis, Mathematica", \
  ""            u 1:17        w lp pt 8 ps 2 lw 2 t "Resultant, Magma", \
  ""            u 1:19        w lp pt 9 ps 2 lw 2 t "Resultant, Mathematica"


unset output
set terminal wxt
reset
