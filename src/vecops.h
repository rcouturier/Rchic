/*----------------------------------------------------------------------
  File    : vecops.h
  Contents: some special vector operations
  Author  : Christian Borgelt
  History : 16.09.1996 file created
            04.02.1999 long int changed to int
----------------------------------------------------------------------*/
#ifndef __VECOPS__
#define __VECOPS__

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef int CMPFN (const void *p1, const void *p2, void *data);

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
void sort (void *vec, int n, CMPFN cmpfn, void *data);
void move (void *vec, int offs, int n, int pos, int esz);

#endif
