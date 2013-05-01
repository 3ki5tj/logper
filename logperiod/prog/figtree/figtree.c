/* draw a fig tree */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int itermax = 100000000;
int trajmax = 100;
double dxtol = 1e-6;

#ifndef xnew
#define xnew(x, n) if ((x = calloc(n, sizeof(*(x)))) == NULL) { \
    fprintf(stderr, "no memory for %s x %u\n", #x, (unsigned) (n)); exit(1); }
#endif

/* logistic map parameters */
double rmin_l = 2.8;
double rmax_l = 4;
double rdel_l = 0.002;
int x0cnt_l = 1;
double x0arr_l[1] = {0.1};
/* logistic map */
double logistic(double r, double x) { return r*x*(1 - x); }

/* cubic map parameters */
double rmin_c = 1.8;
double rmax_c = 3.0;
double rdel_c = 0.002;
int x0cnt_c = 2;
double x0arr_c[2] = {-0.1, 0.1};
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
  fprintf(stderr, "failed to converge r %.8f, x0 %g, x %g\n", r, x0, x);
  return trjmax;
}

/* save the figure tree */
int mkfigtree(double (*func)(double, double),
    double r0, double r1, double dr,
    int x0cnt, double x0arr[],
    int trjmax, int itmax, double tol,
    FILE *fp)
{
  int i, j, k, cnt, rcnt;
  double r, *trj;

  xnew(trj, trjmax + 1);
  rcnt = (int)((r1 - r0)/dr + .5);
  dr = (r1 - r0)/rcnt;
  for (i = 0; i <= rcnt; i++) {
    r = r0 + i * dr;
    /* because there are multiple stable period trajectories,
     * we explore them by different initial values */
    for (k = 0; k < x0cnt; k++) {
      cnt = gettrj(trj, trjmax, r, func, itmax, x0arr[k], tol);
      for (j = 0; j < cnt; j++)
        fprintf(fp, "%+.8f %+.8f %d %d %d\n", r, trj[j], j, cnt, k);
      fprintf(fp, "\n");
    }
  }
  free(trj);
  return 0;
}


int main(int argc, char **argv)
{
  int iscubic = 0;

  if (argc > 1 && strcmp(argv[1], "-c") == 0)
    iscubic = 1;

  if (iscubic) {
    mkfigtree(cubic, rmin_c, rmax_c, rdel_c, x0cnt_c, x0arr_c,
        trajmax, itermax, dxtol, stdout);
  } else {
    mkfigtree(logistic, rmin_l, rmax_l, rdel_l, x0cnt_l, x0arr_l,
        trajmax, itermax, dxtol, stdout);
  }
  return 0;
}
