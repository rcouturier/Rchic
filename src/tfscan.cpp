/*----------------------------------------------------------------------
  File    : tfscan.c
  Contents: table file scanner management
  Author  : Christian Borgelt
  History : 04.01.1998 file created
            11.03.1998 additional character flags enabled
            12.08.1998 function tfs_copy added
            01.09.1998 several assertions added
            27.09.1998 function tfs_getfld improved
            21.10.1998 bug in tfs_sgetc removed
            26.11.1998 some function parameters changed to const
            04.02.1999 long int changed to int
            16.11.1999 number of characters cleared for an empty field
----------------------------------------------------------------------*/
#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tfscan.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- functions --- */
#define isblank(c)   tfs_istype(tfs, TFS_BLANK,  c)
#define isfldsep(c)  tfs_istype(tfs, TFS_FLDSEP, c)
#define isrecsep(c)  tfs_istype(tfs, TFS_RECSEP, c)
#define issep(c)     tfs_istype(tfs, TFS_FLDSEP|TFS_RECSEP, c)

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

TFSCAN* tfs_create (void)
{                               /* --- create a table file scanner */
  TFSCAN *tfs;                  /* created table file scanner */
  int    i;                     /* loop variable */
  char   *p;                    /* to traverse character flags */

  tfs = (TFSCAN*)malloc(sizeof(TFSCAN));
  if (!tfs) return NULL;        /* allocate memory */
  for (p = tfs->cflags +256, i = 256; --i >= 0; )
    *--p = '\0';                /* clear and initialize */
  tfs->cflags['\n'] = TFS_RECSEP;    /* character flags */
  tfs->cflags['\t'] = tfs->cflags[' '] = TFS_BLANK|TFS_FLDSEP;
  return tfs;                   /* return created table file scanner */
}  /* tfs_create() */

/*--------------------------------------------------------------------*/

TFSCAN* tfs_dup (const TFSCAN *tfs)
{                               /* --- duplicate a table file scanner */
  TFSCAN *dup;                  /* created duplicate */

  dup = (TFSCAN*)malloc(sizeof(TFSCAN));
  if (!dup) return NULL;        /* create a new table file scanner */
  tfs_copy(dup, tfs);           /* and copy source into it */
  return dup;                   /* return created duplicate */
}  /* tfs_dup() */

/*--------------------------------------------------------------------*/

void tfs_copy (TFSCAN *dst, const TFSCAN *src)
{                               /* --- copy a table file scanner */
  int  i;                       /* loop variable */
  char *d; const char *s;       /* to traverse the character flags */

  assert(src && dst);           /* check arguments */
  s = src->cflags +256; d = dst->cflags +256;
  for (i = 256; --i >= 0; ) *--d = *--s;
}  /* tfs_copy() */             /* copy character flags */

/*--------------------------------------------------------------------*/

int tfs_sgetc (const char *s)
{                               /* --- get character from string */
  static const char *p = "";    /* to traverse string */
  int    c;                     /* character code */

  if (s) p = s;                 /* if new string given, set string */
  if (*p == '\0') return -1;    /* if at end of string, abort */
  if (*p != '\\') return (unsigned char)*p++;
  p++;                          /* skip '\' */
  switch ((unsigned char)*p++){ /* evaluate character after '\' */
    case 'a': return '\a';      /* 0x07 (BEL) */
    case 'b': return '\b';      /* 0x08 (BS)  */
    case 'f': return '\f';      /* 0x0c (FF)  */
    case 'n': return '\n';      /* 0x0a (NL)  */
    case 'r': return '\r';      /* 0x0d (CR)  */
    case 't': return '\t';      /* 0x09 (HT)  */
    case 'v': return '\v';      /* 0x0b (VT)  */
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      c = *p++ -'0';            /* get octal character code */
      if ((*p >= '0') && (*p <= '7')) c = c *8 +*p++ -'0';
      else return c;            /* get second digit */
      if ((*p >= '0') && (*p <= '7')) c = c *8 +*p++ -'0';
      else return c;            /* get third digit */
      if (c >= 256) return 192 +(c % 64);
      else return c;            /* check range and return character */
    case 'x': p++;              /* get hexadecimal character code */
      if      ((*p >= '0') && (*p <= '9')) c = *p++ -'0';
      else if ((*p >= 'a') && (*p <= 'f')) c = *p++ -'a' +10;
      else if ((*p >= 'A') && (*p <= 'F')) c = *p++ -'A' +10;
      else return 'x';          /* get first digit */
      if      ((*p >= '0') && (*p <= '9')) c = c *16 +*p++ -'0';
      else if ((*p >= 'a') && (*p <= 'f')) c = c *16 +*p++ -'a' +10;
      else if ((*p >= 'A') && (*p <= 'F')) c = c *16 +*p++ -'A' +10;
      return c;                 /* get second digit and return char. */
    default:                    /* non-function characters */
      if (*p == '\0') return '\\';
      else            return (unsigned char)*p++;
  }                             /* return character or backslash */
}  /* tfs_sgetc() */

/*--------------------------------------------------------------------*/

int tfs_chars (TFSCAN *tfs, int type, const char *chars)
{                               /* --- set characters */
  int  i, c, d;                 /* loop variable, characters */
  char *p;                      /* to traverse character flags */

  assert(tfs);                  /* check argument */
  if (!chars) return -1;        /* if no characters given, abort */
  p = tfs->cflags +256;         /* clear character flags in type */
  for (i = 256; --i >= 0; ) *--p &= (char)~type;
  for (c = d = tfs_sgetc(chars); c >= 0; c = tfs_sgetc(NULL))
    tfs->cflags[c] |= (char)type;  /* set character flags */
  return (d >= 0) ? d : 0;      /* return first character */
}  /* tfs_chars() */

/*--------------------------------------------------------------------*/

int tfs_getfld (const TFSCAN *tfs, FILE *file, char *buf, int len,
                int *read)
{                               /* --- read a table field */
  int  c;                       /* character read */
  int  d;                       /* delimiter type */
  char *p;                      /* to traverse the buffer */

  assert(tfs && file && buf && (len >= 0));
  p = buf; *p = '\0';           /* clear read buffer */
  do {                          /* --- skip leading blanks */
    c = getc(file);             /* get next character */
    if (c == EOF) return (ferror(file)) ? -1 : TFS_EOF;
  } while (isblank(c));         /* while character is blank */
  if (issep(c)) {               /* check for field/record separator */
    if (read) *read = 0;        /* clear number of characters read */
    return (isfldsep(c)) ? TFS_FLD : TFS_REC;
  }                             /* return delimiter type */
  while (1) {                   /* --- read value */
    if (len >= 0) {             /* if buffer is not full, */
      len--; *p++ = (char)c; }  /* store character in buffer */
    c = getc(file);             /* get next character */
    if (issep(c)) { d = (isfldsep(c))  ? TFS_FLD : TFS_REC; break; }
    if (c == EOF) { d = (ferror(file)) ? -1      : TFS_EOF; break; }
  }                             /* while character is no separator */
  while (isblank(*--p));        /* --- remove trailing blanks */
  *++p = '\0';                  /* terminate string in buffer */
  if (read) *read = (int)(p -buf); /* store number of characters read */
  if (d != TFS_FLD) return d;   /* if not at field separator, abort */
  while (isblank(c)) {          /* --- skip trailing blanks */
    c = getc(file);             /* get next character */
    if (c == EOF) return (ferror(file)) ? -1 : TFS_EOF;
  }                             /* check for end of file */
  if (isrecsep(c))  return TFS_REC;  /* check for record separator */
  if (!isfldsep(c)) ungetc(c, file); /* put back character */
  return TFS_FLD;               /* return delimiter type */
}  /* tfs_getfld() */
