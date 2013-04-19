unset multiplot
reset
set terminal postscript enhanced font "Times, 14"
set output "cobweb.ps"
set encoding iso_8859_1


# set arrow style 1, as a thin arrow
# size length, angle (deg.), backangle (deg.)
set style arrow 1 head size 0.040, 18, 30 filled lw 1.0 lc rgb "#3000ff"

set style line 10 lt 2 lw 1.5 lc rgb "#808080"

dx = 0.015
dy = 0.025
lbfont = "Times, 20"

set label "(a)" at screen dx, 1.0-dy           font lbfont   
set label "(b)" at screen .3333 + dx, 1.0-dy   font lbfont
set label "(c)" at screen .6667 + dx, 1.0-dy   font lbfont
set label "(d)" at screen dx, 0.5-dy
set label "(e)" at screen .3333 + dx, .5-dy    font lbfont
set label "(f)" at screen .6667 + dx, .5-dy    font lbfont

titlefont = "Times, 17"

set tmargin 2.0
set bmargin 2.0
set rmargin 0.5
set lmargin 5.5

tcfont = "Times, 11"

set xlabel "{/Times-Italic x}" offset 0, 1.0 font "Times, 15"
set xtics .2 font tcfont offset 0, 0.3
set mxtics 2

set ylabel "{/Times-Italic f}{/=8 &{i}}({/Times-Italic x}{/=8 &{i}}) = {/Times-Italic r{/=8 &{i}}x}{/=8 &{i}}(1 - {/Times-Italic x}{/=8 &{i}})" offset 3.0, 0 font "Times, 15"
set ytics .2 font tcfont offset 0.5, 0
set mytics 2

set multiplot
set size 0.3333, 0.5
set origin 0.0, 0.5

r = 2.8
f(x) = r*x*(1-x)
x0 = 0.25
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
x6 = f(x5)
x7 = f(x6)
xf = 1 - 1/r;

set title 'Fixed point, {/Times-Italic r} = 2.8' offset 0, -0.5 font titlefont


# a circle at the fixed point         
set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs solid

set arrow  1 from x0, x0 to x0, x1 as 1
set arrow  2 from x0, x1 to x1, x1 as 1
set arrow  3 from x1, x1 to x1, x2 as 1
set arrow  4 from x1, x2 to x2, x2 as 1
set arrow  5 from x2, x2 to x2, x3 as 1
set arrow  6 from x2, x3 to x3, x3 as 1
set arrow  7 from x3, x3 to x3, x4 as 1
set arrow  8 from x3, x4 to x4, x4 as 1
#set arrow  9 from x4, x4 to x4, x5 as 1
#set arrow 10 from x4, x5 to x5, x5 as 1
#set arrow 11 from x5, x5 to x5, x6 as 1
#set arrow 12 from x5, x6 to x6, x6 as 1
#set arrow 13 from x6, x6 to x6, x7 as 1
#set arrow 14 from x6, x7 to x7, x7 as 1

#set tics font "Times, 12"
tcfont = "Times, 12"


plot [0:1][:1] f(x) lw 2.0 not, \
               x    ls 10  not

unset object
unset arrow




set origin 0.3333, 0.5


r = 3.4
x0 = 0.157845
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
xf = 1 - 1/r;

set title "2-cycle, {/Times-Italic r} = 3.4" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs empty lw 1.5
set object 12 circle at x2, x3 size 0.020 front fc rgb "#ff0000" fs solid
set object 13 circle at x3, x2 size 0.020 front fc rgb "#ff0000" fs solid


set arrow 1 from x0, x0 to x0, x1 as 1 
set arrow 2 from x0, x1 to x1, x1 as 1
set arrow 3 from x1, x1 to x1, x2 as 1
set arrow 4 from x1, x2 to x2, x2 as 1
set arrow 5 from x2, x2 to x2, x3 as 1
set arrow 6 from x2, x3 to x3, x3 as 1

plot [0:1][:1] f(x) lw 2.0 not, \
               x    ls 10  not


unset object
unset arrow






set origin 0.6667, 0.5


r = 3.84
x0 = 0.149407
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
xf = 1 - 1/r;

set title "3-cycle, {/Times-Italic r} = 3.84" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb "#ff0000" fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb "#ff0000" fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb "#ff0000" fs solid


set arrow 1 from x0, x0 to x0, x1 as 1
set arrow 2 from x0, x1 to x1, x1 as 1
set arrow 3 from x1, x1 to x1, x2 as 1
set arrow 4 from x1, x2 to x2, x2 as 1
set arrow 5 from x2, x2 to x2, x3 as 1
set arrow 6 from x2, x3 to x3, x3 as 1

plot [0:1][0:1] f(x) lw 2.0 not, \
                x    ls 10  not

unset object
unset arrow



set origin 0, 0

r = 3.5
x0 = 0.125003
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
xf = 1 - 1/r;

set title "4-cycle (period-doubling), {/Times-Italic r} = 3.5" font "Times, 14"

set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb "#ff0000" fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb "#ff0000" fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb "#ff0000" fs solid
set object 15 circle at x4, x5 size 0.020 front fc rgb "#ff0000" fs solid


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

plot [0:1][:1] f(x) lw 2.0 not, \
               x    ls 10  not

unset object
unset arrow



set origin 0.3333, 0

r = 3.9607
x0 = 0.0386559
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
xf = 1 - 1/r;

set title "4-cycle (original), {/Times-Italic r} = 3.9607" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb "#ff0000" fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb "#ff0000" fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb "#ff0000" fs solid
set object 15 circle at x4, x5 size 0.020 front fc rgb "#ff0000" fs solid


set arrow 1 from x0, x0 to x0, x1 as 1 
set arrow 2 from x0, x1 to x1, x1 as 1
set arrow 3 from x1, x1 to x1, x2 as 1
set arrow 4 from x1, x2 to x2, x2 as 1
set arrow 5 from x2, x2 to x2, x3 as 1
set arrow 6 from x2, x3 to x3, x3 as 1
set arrow 7 from x3, x3 to x3, x4 as 1
set arrow 8 from x3, x4 to x4, x4 as 1

plot [0:1.02][0:1.02] f(x) lw 2.0 not, \
                      x    ls 10  not

unset object
unset arrow



set origin 0.6667, 0

r = 3.57
f(x) = r*x*(1-x)
x0  = 0.1
x1  = f(x0)
x2  = f(x1)
x3  = f(x2)
x4  = f(x3)
x5  = f(x4)
x6  = f(x5)
x7  = f(x6)
x8  = f(x7)
x9  = f(x8)
x10 = f(x9)
xf = 1 - 1/r;

set title "Chaos, {/Times-Italic r} = 3.57" offset 0, -0.5 font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb "#ff0000" fs empty lw 1.5

set arrow  1 from x0,  x0  to x0,  x1  as 1 
set arrow  2 from x0,  x1  to x1,  x1  as 1    
set arrow  3 from x1,  x1  to x1,  x2  as 1
set arrow  4 from x1,  x2  to x2,  x2  as 1
set arrow  5 from x2,  x2  to x2,  x3  as 1
set arrow  6 from x2,  x3  to x3,  x3  as 1
set arrow  7 from x3,  x3  to x3,  x4  as 1
set arrow  8 from x3,  x4  to x4,  x4  as 1
set arrow  9 from x4,  x4  to x4,  x5  as 1
set arrow 10 from x4,  x5  to x5,  x5  as 1
set arrow 11 from x5,  x5  to x5,  x6  as 1
set arrow 12 from x5,  x6  to x6,  x6  as 1
set arrow 13 from x6,  x6  to x6,  x7  as 1
set arrow 14 from x6,  x7  to x7,  x7  as 1
set arrow 15 from x7,  x7  to x7,  x8  as 1
set arrow 16 from x7,  x8  to x8,  x8  as 1
set arrow 17 from x8,  x8  to x8,  x9  as 1
set arrow 18 from x8,  x9  to x9,  x9  as 1
set arrow 19 from x9,  x9  to x9,  x10 as 1
set arrow 20 from x9,  x10 to x10, x10 as 1



plot [0:1][0:1.0] f(x) lw 2.0 not, \
                  x    ls 10  not

unset object
unset arrow

unset multiplot
unset output
set terminal wxt
reset
