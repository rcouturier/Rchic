/*----------------------------------------------------------------------
  File    : vecops.c
  Contents: some special vector operations
  Author  : Christian Borgelt
  History : 16.09.1996 file created
            04.02.1999 long int changed to int
----------------------------------------------------------------------*/
#include "vecops.h"

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define TH_INSERT      16       /* threshold for insertion sort */
#define BUFSIZE      4096       /* size of buffers for shifting */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

static void _rec (void **vec, int n, CMPFN cmpfn, void *data)
{                               /* --- recursive part of sort */
  void **l, **r;                /* pointers to exchange positions */
  void *x,  *t;                 /* pivot element and exchange buffer */
  int  m;                       /* number of elements in 2nd section */

  do {                          /* sections sort loop */
    l = vec; r = l +n -1;       /* start at left and right boundary */
    if (cmpfn(*l, *r, data) > 0) {  /* bring the first and last */
      t = *l; *l = *r; *r = t; }    /* element into proper order */
    x = vec[n >> 1];            /* get the middle element as pivot */
    if      (cmpfn(x, *l, data) < 0) x = *l;  /* try to find a */
    else if (cmpfn(x, *r, data) > 0) x = *r;  /* better pivot */
    while (1) {                 /* split and exchange loop */
      while (cmpfn(*++l, x, data) < 0)    /* skip left  elements that */
        ;                       /* are smaller than the pivot element */
      while (cmpfn(*--r, x, data) > 0)    /* skip right elements that */
        ;                       /* are greater than the pivot element */
      if (l >= r) {             /* if less than two elements left, */
        if (l <= r) { l++; r--; } break; }       /* abort the loop */
      t = *l; *l = *r; *r = t;  /* otherwise exchange elements */
    }
    m = (int)(vec +n -l);       /* compute the number of elements */
    n = (int)(r -vec +1);       /* right and left of the split */
    if (n > m) {                /* if right section is smaller, */
      if (m >= TH_INSERT)       /* but larger than the threshold, */
        _rec(l, m, cmpfn, data); } /* sort it by a recursive call, */
    else {                      /* if the left section is smaller, */
      if (n >= TH_INSERT)       /* but larger than the threshold, */
        _rec(vec, n, cmpfn, data); /* sort it by a recursive call, */
      vec = l; n = m;           /* then switch to the right section */
    }                           /* keeping its size m in variable n */
  } while (n >= TH_INSERT);     /* while greater than threshold */
}  /* _rec() */

/*--------------------------------------------------------------------*/

void sort (void *vec, int n, CMPFN cmpfn, void *data)
{                               /* --- quick sort for pointer vectors */
  int  k;                       /* number of elements in 1st section */
  void **l, **r;                /* to traverse the vector */
  void *t;                      /* exchange buffer */

  if (n <= 1) return;           /* do not sort less than two elements */
  if (n < TH_INSERT)            /* if less elements than threshold */
    k = n;                      /* for insertion sort, note the */
  else {                        /* number of elements, otherwise */
    _rec((void**)vec, n, cmpfn, data);  /* call the recursive function */
    k = TH_INSERT -1;           /* and get the number of elements */
  }                             /* in the first vector section */
  for (l = r = (void**)vec; --k > 0; )  /* find the smallest element within */
    if (cmpfn(*++r, *l, data) < 0) l = r;   /* the first k elements */
  r = (void**)vec;                      /* swap the smallest element */
  t = *l; *l = *r; *r = t;      /* to front as a sentinel */
  while (--n > 0) {             /* insertion sort loop */
    t = *++r;                   /* note the element to insert */
    for (l = r; cmpfn(*--l, t, data) > 0; ) /* shift right elements */
      l[1] = *l;                /* that are greater than the one to */
    l[1] = t;                   /* insert and store the element to */
  }                             /* insert in the place thus found */
}  /* sort() */

/*--------------------------------------------------------------------*/

void move (void *vec, int offs, int n, int pos, int esz)
{                               /* --- move a vector section */
  int i;                        /* loop variable */
  int mid, end;                 /* middle and end index */
  int *src, *dst;               /* to traverse vector */
  int buf[BUFSIZE];             /* buffer for vector elements */

  esz /= (int)sizeof(int);      /* adapt size, offsets, and counter */
  pos *= esz; offs *= esz; n *= esz;
  end  = offs +n;               /* normalize vector indices */
  if (pos <= offs) { mid = offs; offs = pos; }
  else             { mid = end;  end  = pos; }
  if (mid -offs < end -mid) {   /* if first section is smaller */
    while (mid > offs) {        /* while there are elements to shift */
      n   = (mid -offs < BUFSIZE) ? mid -offs : BUFSIZE;
      src = (int*)vec +mid -n;  /* get number of elements and */
      dst = buf;                /* copy source to the buffer */
      for (i = n;        --i >= 0; ) *dst++ = *src++;
      dst = (int*)vec +mid -n;  /* shift down/left second section */
      for (i = end -mid; --i >= 0; ) *dst++ = *src++;
      src = buf;                /* copy buffer to destination */
      for (i = n;        --i >= 0; ) *dst++ = *src++;
      mid -= n; end -= n;       /* second section has been shifted */
    } }                         /* down/left cnt elements */
  else {                        /* if second section is smaller */
    while (end > mid) {         /* while there are elements to shift */
      n   = (end -mid < BUFSIZE) ? end -mid : BUFSIZE;
      src = (int*)vec +mid +n;  /* get number of elements and */
      dst = buf +n;             /* copy source to the buffer */
      for (i = n;         --i >= 0; ) *--dst = *--src;
      dst = (int*)vec +mid +n;  /* shift up/right first section */
      for (i = mid -offs; --i >= 0; ) *--dst = *--src;
      src = buf +n;             /* copy buffer to destination */
      for (i = n;         --i >= 0; ) *--dst = *--src;
      mid += n; offs += n;      /* first section has been shifted */
    }                           /* up/right cnt elements */
  }
}  /* move() */
