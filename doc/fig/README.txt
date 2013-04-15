  logbulb and cubbulb
=========================
logbulb.gp uses logpX.txt, which are complex cycles at different $R = \exp(i \theta)$
The format is 
  Re(R)  Im(R)  cycle-id   phase-id
where `cycle-id' is the index among cycles of the same period,
  e.g., there are three complex 3-cycles, so cycle-id can be 1, 2 or 3.
phase-id is \theta/pi * 180

Similarly, cubbulb.gp uses cubpX.txt




  Converting .ps to .pdf
==========================
Use
  epstopdf  logbulb.ps
b/c it is a square figure

For others use
  ps2pdf
