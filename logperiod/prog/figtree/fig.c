/* draw fig tree */
#include <stdio.h>
#include <math.h>

double rmin = 2.0;
double rmax = 3.0;
double delr = 0.002;
int itermax = 100000000;
int trajmax = 100;
double dxtol = 1e-6;
const char *fnout = "cub.fig";

#ifndef xnew
#define xnew(x, n) if ((x = calloc(n, sizeof(*(x)))) == NULL) { \
    fprintf(stderr, "no memory for %s x %u\n", #x, (unsigned) (n)); exit(1); }
#endif

/* logistic map */
double logistic(double r, double x) { return r*x*(1 - x); }

/* cubic map */
double cubic(double r, double x) { 
  // return x*r*(1 - x*x);  // allows the negative part
  return x*(r - x*x); 
}

/* get period trajectories
 * trj[]: trajectories, trjmax is its size
 * itmax: maximal number of iterations  */
int gettrj(double trj[], int trjmax, 
    double r, double (*f)(double, double), int itmax, 
    double x0, double tol)
{
  int i, j;
  double x = x0;

  if (itmax < trjmax) itmax = trjmax;
  for (i = 0; i < itmax; ) {
    /* a set of trjmax iterations */
    for (j = 0; j < trjmax && i < itmax; j++, i++) {
      x = (*f)(r, x);
      if (j > 0 && fabs(trj[0] - x) < tol) {
        //fprintf(stderr, "detected period-%d, r %g, x %g vs %g, i %d\n", j, r, x, trj[0], i);
        break;
      }
      trj[j] = x;
    }
    if (j < trjmax) return j;
  }
  fprintf(stderr, "failed to converge for r = %.8f\n", r);
  return trjmax;
}

/* save the figure tree */
int mkfigtree(double r0, double r1, double dr, 
    int trjmax, int itmax, double tol,
    const char *fn)
{
  int i, j, k, cnt, rcnt;
  double r, *trj, x0;
  FILE *fp;

  xnew(trj, trjmax + 1);
  rcnt = (int)((r1 - r0)/dr + .5);
  dr = (r1 - r0)/rcnt;
  if ((fp = fopen(fn, "w")) == NULL) {
    fprintf(stderr, "cannot open file %s\n", fn);
    return - 1;
  }
  for (i = 0; i <= rcnt; i++) {
    r = r0 + i * dr;
    /* because there are multiple stable period trajectories, 
     * we explore them by different initial values */
    for (x0 = -0.1, k = 0; x0 < 0.2; x0 += 0.2, k++) {
      cnt = gettrj(trj, trjmax, r, cubic, itmax, x0, tol);
      for (j = 0; j < cnt; j++)
        fprintf(fp, "%+.8f %+.8f %d %d %d\n", r, trj[j], j, cnt, k);
      fprintf(fp, "\n");
    }
  }
  fclose(fp);
  free(trj);
  return 0;
}


int main(void)
{
  mkfigtree(rmin, rmax, delr, trajmax, itermax, dxtol, fnout);
  return 0;
}
