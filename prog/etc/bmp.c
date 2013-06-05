/* write a grayscale bitmap:  ./bmp input output */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char  BYTE;  /* one byte (0-255) */
typedef unsigned short WORD;  /* two bytes */
typedef unsigned int   DWORD; /* four bytes */

/* Bitmap format cf.
 * http://en.wikipedia.org/wiki/BMP_file_format
 * http://upload.wikimedia.org/wikipedia/commons/c/c4/BMPfileFormat.png
 * http://www.fileformat.info/format/bmp/egff.htm */

typedef struct {
/* BYTE type[2]; */       /* = "BM", it is manipulated separately to make
                             the size of this structure a multiple of 4 */
  DWORD sizeFile;         /* = total file size == offset + bitmap-size */
  DWORD reserved;         /* == 0 */
  DWORD offset;           /* offset from start of file == sizeof(BitmapHeader) with "BM"
                             + sizeof(DIBHeader) + size of palette */
} BitmapHeader;

typedef struct {
  DWORD sizeStruct;       /* sizeof(DIBHeader) */
  DWORD width, height;
  WORD  planes;           /* 1 */
  WORD  bitCount;         /* bits of each pixel, 8 for 256-color, 24 for RGB true color */
  DWORD compression;      /* 0 */
  DWORD sizeImage;        /* (width+?)(till multiple of 4) * height in bytes */
  DWORD xPixelsPerMeter;  /* resolution in mspaint, 2952 */
  DWORD yPixelsPerMeter;  /* resolution in mspaint, 2952 */
  DWORD colorUsed;        /* 256 for 256-color, 0 for true color */
  DWORD colorImportant;   /* 256 for 256-color, 0 for true color */
#if 0
  DWORD maskRed;          /* 0x00ff0000 */
  DWORD maskGreen;        /* 0x0000ff00 */
  DWORD maskBlue;         /* 0x000000ff */
  DWORD maskAlpha;        /* 0xff000000 */
  DWORD colorSpaceType;   /* 1 */
  DWORD colorSpaceEnd[9]; /* 0, 0, 0xfc1e5486, 0, 0, 0xfc666669, 0, 0, 0xff28f5c4 */
  DWORD gammaRed;         /* 0 */
  DWORD gammaGreen;       /* 0 */
  DWORD gammaBlue;        /* 0 */
  DWORD Intent;           /* 4 */
  DWORD ICCData;          /* 0 */
  DWORD ICCSize;          /* 0 */
  DWORD Reserve;          /* 0 */
#endif
} DIBHeader;


typedef struct {
  size_t w;
  size_t ww;  /* ceil4(width) */
  size_t h;
  size_t size;
  unsigned char *data;
} bmp_t;

#define RGB(r, g, b) ((((r)&0xFF)<<16) + (((g)&0xFF)<<8) + ((b)&0xFF))
#define CEIL4(x) ((((x)+3)/4)*4)

/* create a new bitmap file */
static bmp_t *bmp_open(int width, int height)
{
  bmp_t *b;

  if ((b = calloc(1, sizeof(*b))) == NULL) exit(1);

  b->w = (size_t) width;
  b->h = (size_t) height;
  b->ww = CEIL4(b->w);
  b->size = b->ww * b->h;

  if ((b->data = calloc(b->size, 1)) == NULL) exit(1);
  return b;
}



static void bmp_close(bmp_t *b) {
  if (b->data) free(b->data);
  free(b);
}



static int bmp_save(const bmp_t *b, const char *fn)
{
  FILE *fp;
  BitmapHeader bh = {0};
  DIBHeader dh = {0};
  DWORD pal[256];
  int i;

  bh.offset = sizeof(BitmapHeader) + sizeof(DIBHeader) + sizeof(pal) + 2;
  bh.sizeFile = bh.offset + b->size;

  dh.sizeStruct = sizeof(DIBHeader);
  dh.width = b->w;
  dh.height = b->h;
  dh.planes = 1;
  dh.sizeImage = b->size;
  dh.bitCount = 8;
  dh.colorUsed = dh.colorImportant = 256;
  printf("file %u, struct %u\n", bh.sizeFile, dh.sizeStruct);

  for (i = 0; i < 256; i++) {
    pal[255 - i] = RGB(i, i, i);
  }

  if ((fp = fopen(fn, "wb")) == NULL) {
    fprintf(stderr, "cannot save to BMP file %s\n", fn);
    return 1;
  }
  fputc('B', fp);
  fputc('M', fp); /* write type */
  /* fill BMP file header */
  fwrite(&bh, sizeof bh, 1, fp);
  fwrite(&dh, sizeof dh, 1, fp);
  fwrite(pal, sizeof pal, 1, fp);
  if (fwrite(b->data, 1, b->size, fp) != b->size) {
    fclose(fp);
    return 1;
  }
  fclose(fp);
  return 0;
}



__inline static void bmp_dot(bmp_t *b, DWORD x, DWORD y, BYTE color)
{
  if (x < b->w && y < b->h)
    *(b->data + b->ww * y + x) = color;
}



typedef struct {
  int w; /* maximal number of bytes of coefficients */
  int n; /* number of terms */
  BYTE *arr;
} bitdat_t;



/* load "bit.txt" */
static bitdat_t *loaddata(const char *fn)
{
  FILE *fp;
  int w, n, i, id, x, next, lnum;
  bitdat_t *dat;
  char s[1024], *p;

  if ((fp = fopen(fn, "r")) == NULL) {
    fprintf(stderr, "cannot read %s\n", fn);
    return NULL;
  }
  fscanf(fp, "%d %d", &n, &w);

  /* allocate the data structure */
  if ( (dat = calloc(1, sizeof(*dat))) == NULL) return NULL;
  dat->w = w;
  dat->n = n;
  if ( (dat->arr = calloc(w*n, sizeof(BYTE))) == NULL) return NULL;
  printf("w %d, n %d, allocating %d bytes\n", w, n, w*n);

  i = -1;
  for (lnum = 1; ; lnum++) { /* search for lines */
    if (fgets(s, sizeof s, fp) == NULL) break;
    p = s;
    if (*p == '{') { /* start a new term */
      if (++i >= dat->n) {
        fprintf(stderr, "too many # of lines, i %d, n %d\nlnum %d: %s\n", i, n, lnum, s);
        exit(1);
      }
      id = 0;
      p++;
    }
    while (sscanf(p, "%d%n", &x, &next) > 0) {
      if (id >= dat->w) {
        fprintf(stderr, "width exceeds expectation in line %d, w %d\n", i, w);
        exit(1);
      }
      dat->arr[i * dat->w  + id] = (BYTE) x;
      id++;
      p += next; /* move to the next value */
      while (*p && strchr("\n ,}", *p)) p++;
      if (*p == '\0') break;
    }
  }
  fclose(fp);
  return dat;
}



/* change the parity bit */
static void parity(bitdat_t *dat)
{
  int i;

  for (i = 0; i < dat->n; i++) /* 0 -> 0, 1 -> 255 */
    dat->arr[i * dat->w] *= 255;
}



/* write the data to a bitmap
 * `nbar' is the number of pixels for the leading sign bar */
static void writebmp(bitdat_t *dat, const char *fn, int twocolumn, int nbar)
{
  int i, j, k, w[2], n[2], x0, y0, max, hsp, y;
  BYTE x;
  bmp_t *bmp;

  if (nbar < 0) {
    nbar = (dat->w/128 + 4)/4 * 4;
  }

  if (twocolumn) {
    hsp = (dat->w/128 + 4)/4 * 4; /* spacing between the two columns */
    w[0] = dat->w + hsp + nbar - 1;
    w[1] = dat->w + nbar - 1;
    n[0] = (dat->n + 1)/2;
    n[1] = dat->n - n[0];

    /* try to reduce the width of the second column */
    for (max = 0, i = n[0]; i < dat->n; i++) {
      for (j = dat->w - 1; j >= 0; j--) {
        if (dat->arr[i * dat->w + j] > 0)
          break;
      }
      if (j > max) max = j;
    }
    w[1] = max + 1 + nbar;

    /* write the bitmap */
    bmp = bmp_open(w[0] + w[1], n[0]);
    for (x0 = y0 = 0, k = 0; k < 2; k++) { /* columns */
      for (i = 0; i < n[k]; i++) {
        y = bmp->h - 1 - i;
        for (j = 0; j < nbar; j++)
          bmp_dot(bmp, x0 + j, y, dat->arr[(i+y0) * dat->w]);
        for (j = 1; j < dat->w; j++)
          bmp_dot(bmp, x0 + nbar - 1 + j, y, dat->arr[(i+y0) * dat->w + j]);
      }
      x0 += w[k];
      y0 += n[k];
    }
  } else { /* single column */
    bmp = bmp_open(dat->w + nbar - 1, dat->n);
    for (i = 0; i < dat->n; i++) {
      y = bmp->h - 1 - i;
      for (j = 0; j < nbar; j++) /* draw the leading sign bar */
        bmp_dot(bmp, j, y, dat->arr[i * dat->w]);
      for (j = 1; j < dat->w; j++)
        bmp_dot(bmp, nbar - 1 + j, y, dat->arr[i * dat->w + j]);
    }
  }
  bmp_save(bmp, fn);
  bmp_close(bmp);
}



int main(int argc, char *argv[])
{
  bitdat_t *dat;
  const char *fnin = "bit10b.txt", *fnout = "out.bmp";
  int twok = 1, nbar = -1;

  if (argc >= 2) fnin = argv[1];
  if (argc >= 3) fnout = argv[2];
  if (argc >= 4) twok = 0;
  if (argc >= 5) nbar = atoi(argv[3]);
  printf("fnin %s fnout %s\n", fnin, fnout);
  if ((dat = loaddata(fnin)) == NULL) return 1;
  parity(dat); /* change the parity bit */
  writebmp(dat, fnout, twok, nbar);
  free(dat->arr); free(dat);
  return 0;
}

