unset multiplot
reset
set terminal postscript enhanced font "Times, 14"
set output "oddcycle.ps"
set encoding iso_8859_1


# set arrow style 1, as a thin arrow
# size length, angle (deg.), backangle (deg.)
set style arrow 1 head size 0.100, 18, 30 filled lw 1.0 lc rgb "#3000ff"

set style line 10 lt 2 lw 1.5 lc rgb "#808080"

dx = 0.015
dy = 0.025
lbfont = "Times, 20"

set label "(a)" at screen dx,      1.0-dy  font lbfont
set label "(b)" at screen .5 + dx, 1.0-dy  font lbfont
set label "(c)" at screen dx,      0.5-dy  font lbfont
set label "(d)" at screen .5 + dx, 0.5-dy  font lbfont

titlefont = "Times, 17"

set tmargin 2.0
set bmargin 2.0
set rmargin 0.5
set lmargin 7.0

tcfont = "Times, 11"

set xlabel "{/Times-Italic x}" offset 0, 1.0 font "Times, 17"
set xtics .5 font tcfont offset 0, 0.3
set mxtics 5

set ylabel "{/Times-Italic f}{/=8 &{i}}({/Times-Italic x}{/=8 &{i}}) = {/Times-Italic r{/=8 &{i}}x} - {/Times-Italic x}{/=10 &{i}^3}" offset 2., 0 font "Times, 17"
set ytics .5 font tcfont offset 0.5, 0
set mytics 5

set multiplot
set size 0.5, 0.5
set origin 0.0, 0.5

r = 2.2
f(x) = r*x - x*x*x
x0 = 1.78885
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
x6 = f(x5)
x7 = f(x6)
xf = sqrt(r-1);

set title "1-odd-cycle, {/Times-Italic r} = 2.2" offset 0, -0.5 font titlefont


# a circle at the fixed point
set object 11 circle at  xf,  xf size 0.04 front fc rgb "#ff0000" fs solid
set object 12 circle at -xf, -xf size 0.04 front fc rgb "#ff0000" fs solid
set object 13 circle at  x0,  x1 size 0.04 front fc rgb "#ff0000" fs empty
set object 14 circle at  x1,  x2 size 0.04 front fc rgb "#ff0000" fs empty

set arrow 100 from 0, -2 to 0, 2 nohead
set arrow 101 from -2, 0 to 2, 0 nohead

set arrow  1 from x0, x0 to x0, x1 as 1
set arrow  2 from x0, x1 to x1, x1 as 1
set arrow  3 from x1, x1 to x1, x2 as 1
set arrow  4 from x1, x2 to x2, x2 as 1

#set tics font "Times, 12"
tcfont = "Times, 12"


plot [-2:2][-2:2] f(x) lw 2.0 not, \
                  x    ls 10  not


unset object
unset arrow


set origin 0.5, 0.5


r = 2.8308115141810246
x0 = sqrt(r/3)
f(x) = r*x - x*x*x
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
x6 = f(x5)
x7 = f(x6)
x8 = f(x7)
x9 = f(x8)
x10 = f(x9)
xf = sqrt(r-1);

set title "2-odd-cycle, {/Times-Italic r} = 2.8308" font titlefont

set object 11 circle at  xf,  xf size 0.04 front fc rgb "#ff0000" fs empty
set object 12 circle at -xf, -xf size 0.04 front fc rgb "#ff0000" fs empty
set object 13 circle at  x0,  x1 size 0.04 front fc rgb "#ff0000" fs solid
set object 14 circle at  x1,  x2 size 0.04 front fc rgb "#ff0000" fs solid
set object 15 circle at  x2,  x3 size 0.04 front fc rgb "#ff0000" fs solid
set object 16 circle at  x3,  x4 size 0.04 front fc rgb "#ff0000" fs solid


set arrow  1 from x0, x0 to x0, x1 as 1
set arrow  2 from x0, x1 to x1, x1 as 1
set arrow  3 from x1, x1 to x1, x2 as 1
set arrow  4 from x1, x2 to x2, x2 as 1
set arrow  5 from x2, x2 to x2, x3 as 1
set arrow  6 from x2, x3 to x3, x3 as 1
set arrow  7 from x3, x3 to x3, x4 as 1
set arrow  8 from x3, x4 to x4, x4 as 1

plot [-2:2][-2:2] f(x) lw 2.0 not, \
                  x    ls 10  not


unset object
unset arrow






set origin 0., 0.


r = 2.700315538597915
x0 = sqrt(r/3)
f(x) = r*x - x*x*x
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
x6 = f(x5)
x7 = f(x6)
x8 = f(x7)
x9 = f(x8)
x10 = f(x9)
xf = sqrt(r-1);

set title "3-odd-cycle, {/Times-Italic r} = 2.7003" font titlefont

set object 11 circle at  xf,  xf size 0.04 front fc rgb "#ff0000" fs empty
set object 12 circle at -xf, -xf size 0.04 front fc rgb "#ff0000" fs empty
set object 13 circle at  x0,  x1 size 0.04 front fc rgb "#ff0000" fs solid
set object 14 circle at  x1,  x2 size 0.04 front fc rgb "#ff0000" fs solid
set object 15 circle at  x2,  x3 size 0.04 front fc rgb "#ff0000" fs solid
set object 16 circle at  x3,  x4 size 0.04 front fc rgb "#ff0000" fs solid
set object 17 circle at  x4,  x5 size 0.04 front fc rgb "#ff0000" fs solid
set object 18 circle at  x5,  x6 size 0.04 front fc rgb "#ff0000" fs solid


set arrow  1 from x0, x0 to x0, x1 as 1
set arrow  2 from x0, x1 to x1, x1 as 1
set arrow  3 from x1, x1 to x1, x2 as 1
set arrow  4 from x1, x2 to x2, x2 as 1
set arrow  5 from x2, x2 to x2, x3 as 1
set arrow  6 from x2, x3 to x3, x3 as 1
set arrow  7 from x3, x3 to x3, x4 as 1
set arrow  8 from x3, x4 to x4, x4 as 1
set arrow  9 from x4, x4 to x4, x5 as 1
set arrow 10 from x4, x5 to x5, x5 as 1
set arrow 11 from x5, x5 to x5, x6 as 1
set arrow 12 from x5, x6 to x6, x6 as 1

plot [-2:2][-2:2] f(x) lw 2.0 not, \
                  x    ls 10  not


unset object
unset arrow



set origin 0.5, 0.


r = 2.640411598803658
x0 = sqrt(r/3)
f(x) = r*x - x*x*x
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
x6 = f(x5)
x7 = f(x6)
x8 = f(x7)
x9 = f(x8)
x10 = f(x9)
xf = sqrt(r-1);

set title "4-odd-cycle, {/Times-Italic r} = 2.6404" font titlefont

set object 11 circle at  xf,  xf size 0.04  front fc rgb "#ff0000" fs empty
set object 12 circle at -xf, -xf size 0.04  front fc rgb "#ff0000" fs empty
set object 13 circle at  x0,  x1 size 0.04  front fc rgb "#ff0000" fs solid
set object 14 circle at  x1,  x2 size 0.04  front fc rgb "#ff0000" fs solid
set object 15 circle at  x2,  x3 size 0.04  front fc rgb "#ff0000" fs solid
set object 16 circle at  x3,  x4 size 0.04  front fc rgb "#ff0000" fs solid
set object 17 circle at  x4,  x5 size 0.04  front fc rgb "#ff0000" fs solid
set object 18 circle at  x5,  x6 size 0.04  front fc rgb "#ff0000" fs solid
set object 19 circle at  x6,  x7 size 0.04  front fc rgb "#ff0000" fs solid
set object 20 circle at  x7,  x8 size 0.04  front fc rgb "#ff0000" fs solid


set arrow  1 from x0, x0 to x0, x1 as 1
set arrow  2 from x0, x1 to x1, x1 as 1
set arrow  3 from x1, x1 to x1, x2 as 1
set arrow  4 from x1, x2 to x2, x2 as 1
set arrow  5 from x2, x2 to x2, x3 as 1
set arrow  6 from x2, x3 to x3, x3 as 1
set arrow  7 from x3, x3 to x3, x4 as 1
set arrow  8 from x3, x4 to x4, x4 as 1
set arrow  9 from x4, x4 to x4, x5 as 1
set arrow 10 from x4, x5 to x5, x5 as 1
set arrow 11 from x5, x5 to x5, x6 as 1
set arrow 12 from x5, x6 to x6, x6 as 1
set arrow 13 from x6, x6 to x6, x7 as 1
set arrow 14 from x6, x7 to x7, x7 as 1
set arrow 15 from x7, x7 to x7, x8 as 1
set arrow 16 from x7, x8 to x8, x8 as 1

plot [-2:2][-2:2] f(x) lw 2.0 not, \
                  x    ls 10  not


unset object
unset arrow


unset multiplot
unset output
set terminal wxt
reset
