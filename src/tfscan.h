/*----------------------------------------------------------------------
  File    : tfscan.h
  Contents: table file scanner management
  Author  : Christian Borgelt
  History : 04.01.1998 file created
            11.03.1998 additional character flags enabled
            12.08.1998 function tfs_copy added
            26.11.1998 some function parameters changed to const
            29.11.1998 function tfs_dup added
            04.02.1999 long int changed to int
----------------------------------------------------------------------*/
#ifndef __TFSCAN__
#define __TFSCAN__
#include <stdio.h>

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- character flags --- */
#define TFS_RECSEP   0x01       /* flag for record separator */
#define TFS_FLDSEP   0x02       /* flag for field separator */
#define TFS_BLANK    0x04       /* flag for blank character */
#define TFS_OTHER    0x08       /* flag for other character type */

/* --- delimiter types --- */
#define TFS_EOF      0          /* end of file delimiter */
#define TFS_REC      1          /* record delimiter */
#define TFS_FLD      2          /* field  delimiter */

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- table file scanner --- */
  char cflags[256];             /* character flags */
} TFSCAN;                       /* (table file scanner) */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern TFSCAN* tfs_create (void);
extern TFSCAN* tfs_dup    (const TFSCAN *tfs);
extern void    tfs_copy   (TFSCAN *dst, const TFSCAN *src);
extern void    tfs_delete (TFSCAN *tfs);
extern int     tfs_sgetc  (const char *s);
extern int     tfs_chars  (TFSCAN *tfs, int type, const char *chars);
extern void    tfs_istype (const TFSCAN *tfs, int type, int c);
extern int     tfs_getfld (const TFSCAN *tfs, FILE *file,
                           char *buf, int len, int *read);

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define tfs_delete(s)     free(s)
#define tfs_istype(s,t,c) ((s)->cflags[(unsigned char)(c)] & (t))

#endif
