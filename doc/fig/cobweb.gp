unset multiplot
reset
set terminal postscript enhanced font "Times, 14" size 7, 10
set output "cobweb.ps"
set encoding iso_8859_1

colordot = "#cc0000"

# set arrow style 1, as a thin arrow
# size length, angle (deg.), backangle (deg.)
set style arrow 1 head size 0.040, 18, 30 filled lw 1.0 lc rgb "#0000aa"

set style line 10 lt 2 lw 1.5 lc rgb "#808080"

dx = 0.007
dy = 0.030

# width of the left and right colums
wlft = 0.55
wrt = 1 - wlft

# height of the three rows
ht1 = 0.32
ht2 = 0.32
ht3 = 1 - ht1 - ht2
top2 = ht2 + ht3
top3 = ht3


lftmargin = 7.0
hormargin = 1.5

vrtmargin = 0.2
botmargin = 3.0


lbfont = "Times, 20"
titlefont = "Times, 24"
tcfont = "Times, 17"


set label "(a)" at screen dx, 1.000-dy      font lbfont
set label "(b)" at screen dx, top2-dy       font lbfont
set label "(c)" at screen dx, top3-dy       font lbfont
set label "(d)" at screen wlft+dx, 1.000-dy font lbfont
set label "(e)" at screen wlft+dx, top2-dy  font lbfont
set label "(f)" at screen wlft+dx, top3-dy  font lbfont

set xtics .2 font tcfont offset 0, 0.3
set mxtics 2

set ylabel "{/Times-Italic f}{/=8 &{i}}({/Times-Italic x}{/=8 &{i}}) = {/Times-Italic r{/=8 &{i}}x}{/=8 &{i}}(1 - {/Times-Italic x}{/=8 &{i}})" offset 1.5, 0 font titlefont
set ytics .2 font tcfont offset 0.5, 0
set mytics 2



set rmargin 1.0


set multiplot
set size wlft, ht1
set origin 0.0, top2
set bmargin vrtmargin
unset xlabel
set format x ""

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

set title 'Fixed point, {/Times-Italic r} = 2.8' offset 0, 0 font titlefont


# a circle at the fixed point
set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs solid

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


plot [0:1][:1] f(x) lw 2.0 not, \
               x    ls 10  not

unset object
unset arrow



set size wlft, ht2
set origin 0., top3


r = 3.4
x0 = 0.157845
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
xf = 1 - 1/r;

set title "2-cycle, {/Times-Italic r} = 3.4" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs empty lw 1.5
set object 12 circle at x2, x3 size 0.020 front fc rgb colordot fs solid
set object 13 circle at x3, x2 size 0.020 front fc rgb colordot fs solid


#set arrow 1 from x0, x0 to x0, x1 as 1
#set arrow 2 from x0, x1 to x1, x1 as 1
set arrow 3 from x1, x1 to x1, x2 as 1
set arrow 4 from x1, x2 to x2, x2 as 1
set arrow 5 from x2, x2 to x2, x3 as 1
set arrow 6 from x2, x3 to x3, x3 as 1

plot [0:1][:1] f(x) lw 2.0 not, \
               x    ls 10  not


unset object
unset arrow






set size wlft, ht3
set origin 0., 0
set bmargin botmargin
set format x "%g"
set xlabel "{/Times-Italic x}" offset 0, 0.0 font titlefont

r = 3.84
x0 = 0.149407
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
xf = 1 - 1/r;

set title "3-cycle, {/Times-Italic r} = 3.84" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb colordot fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb colordot fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb colordot fs solid


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



set size wrt, ht1
set origin wlft, top2
set lmargin hormargin
set bmargin vrtmargin
unset ylabel
unset xlabel
set format y ""
set format x ""

r = 3.5
x0 = 0.125003
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
xf = 1 - 1/r;

set title "     4-cycle, {/Times-Italic r} = 3.5" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb colordot fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb colordot fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb colordot fs solid
set object 15 circle at x4, x5 size 0.020 front fc rgb colordot fs solid


#set arrow  1 from x0, x0 to x0, x1 as 1
#set arrow  2 from x0, x1 to x1, x1 as 1
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


set size wrt, ht2
set origin wlft, top3

r = 3.9607
x0 = 0.0386559
f(x) = r*x*(1-x)
x1 = f(x0)
x2 = f(x1)
x3 = f(x2)
x4 = f(x3)
x5 = f(x4)
xf = 1 - 1/r;

set title "    4-cycle, {/Times-Italic r} = 3.9607" font titlefont

set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs empty lw 1.5
set object 12 circle at x1, x2 size 0.020 front fc rgb colordot fs solid
set object 13 circle at x2, x3 size 0.020 front fc rgb colordot fs solid
set object 14 circle at x3, x4 size 0.020 front fc rgb colordot fs solid
set object 15 circle at x4, x5 size 0.020 front fc rgb colordot fs solid


set arrow 1 from x0, x0 to x0, x1 as 1
set arrow 2 from x0, x1 to x1, x1 as 1
set arrow 3 from x1, x1 to x1, x2 as 1
set arrow 4 from x1, x2 to x2, x2 as 1
set arrow 5 from x2, x2 to x2, x3 as 1
set arrow 6 from x2, x3 to x3, x3 as 1
set arrow 7 from x3, x3 to x3, x4 as 1
set arrow 8 from x3, x4 to x4, x4 as 1

plot [0:1.0][0:1.0] f(x) lw 2.0 not, \
                      x    ls 10  not

unset object
unset arrow


set size wrt, ht3
set origin wlft, 0
set bmargin botmargin
set format x "%g"
set xlabel "{/Times-Italic x}" offset 0, 0.0 font titlefont

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

set object 11 circle at xf, xf size 0.020 front fc rgb colordot fs empty lw 1.5

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
