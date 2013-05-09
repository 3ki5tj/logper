#!/usr/bin/env python

""" generate magma scripts to compute n-cycle polynomials
    of the logistic and cubic maps by Groebner basis """

import os, sys, getopt

n = 8
ch = "b"
param = "R"
func = "R-x^2"
dfunc = "-2*x" # derivative of the function

def mkmagma(n, ch, f, df, par):
  """ return the magma script as a string """

  xs = eqs = der = ""
  for i in range(0, n):
    var = "x" + str(i)
    vas = "x" + str((i+1) % n)
    xs += var + ","
    eqs += f.replace("x", var) + "-" + vas + ","
    der += "(" + df.replace("x", var) + ")*"

  nvar = n + 1
  extra = par
  if ch == "a":
    frac = "1"
  elif ch == "b":
    frac = "-1"
  else:
    frac = "d"
    extra = "d," + param
    nvar += 1
  der = frac + "-" + der[:-1]

  s = "Q:=RationalField();\n"
  s += "P<" + xs + extra + ">:=PolynomialRing(Q," + str(nvar) + ");\n"

  s += "I:=ideal< P|" + eqs + der + ">;\n"
  s += "time B := GroebnerBasis(I);\n"
  s += "Factorization(B[#B]);"
  return s


def usage():
  """ print usage and die """
  print sys.argv[0], "[Options] n [abc]"
  print """
  The first argument `n' is the period length.
  The second argument can be `a' for the onset point,
  `b' for the bifurcation point, or `c' for the general case """
            
  print "OPTIONS:"
  print " -f, --func:   specify the map"
  print " -d, --dfunc:  specify the derivative of the map"
  print " -p, --param:  specify the tunning parameter of the map"
  print " -r:           original logistic map, f(x)=r*x*(1-x)"
  print " -R:           simplified logistic map, f(x)=R-x^2"
  print " -c:           cubic map, f(x)=r*x-x^3"
  print " -v: be verbose"
  exit(1)


def doargs():
  global n, ch, func, dfunc, param

  ''' Handle common parameters from command line options '''
  try:
    opts, args = getopt.gnu_getopt(sys.argv[1:], "fdpcrRvh",
         ["verbose", "func=", "dfunc=", "param=", "help",])
  except getopt.GetoptError, err:
    # print help information and exit:
    print str(err) # will print something like "option -a not recognized"
    usage()

  for o, a in opts:
    if o in ("-v", "--verbose",):
      verbose = True
    elif o in ("-f", "--func",):
      func = a
    elif o in ("-d", "--dfunc",):
      dfunc = a
    elif o in ("-p", "--param",):
      param = p
    elif o in ("-r",):
      func = "r*x*(1-x)"
      dfunc = "r*(1-2*x)"
      param = "r"
    elif o in ("-R",):
      func = "R-x^2"
      dfunc = "-2*x"
      param = "R"
    elif o in ("-c",):
      func = "r*x-x^3"
      dfunc = "r-3*x^2"
      param = "r"
    elif o in ("-h", "--help",):
      usage()

  # get the cycle period
  if len(args) >= 1: n = int(args[0])
  # get the point of calculation
  if len(args) >= 2: ch = args[1]

def main():
  doargs()
  print mkmagma(n, ch, func, dfunc, param)

if __name__ == "__main__":
  main()
