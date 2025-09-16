/*----------------------------------------------------------------------
File    : apriori.c
Contents: apriori-gen algorithm for finding association rules
(Rakesh Agrawal et al.)
Author  : Christian Borgelt
History : 14.02.1996 file created
15.02.1996 program debugged
24.06.1996 function get_item optimized
25.06.1996 check for maximal set size added
01.07.1996 adapted to modified symtab module
26.07.1996 output precision reduced
22.11.1996 options -b, -f, and -r added
24.11.1996 option -e added
27.02.1997 escape evaluation added to function set_chars
18.08.1997 normalized chi^2 measure added
option -m (minimal rule length) added
13.10.1997 quiet version (no output to stdout or stderr)
04.01.1998 scan functions moved to module `tfscan'
27.01.1998 adapted to changed ist_create() function
09.06.1998 vector enlargement modified
20.06.1998 adapted to changed st_create function
07.08.1998 bug in function get_iset, isrec, removed
08.08.1998 optional input file (item appearances) added
09.08.1998 use of temporary file removed
10.08.1998 changes of preceding two days debugged
17.08.1998 item sorting (descending frequency) added
02.09.1998 several assertions added
07.09.1998 hyperedge mode (option -h) added
08.12.1998 output of absolute support (option -a) added
float changed to double
09.12.1998 conversion of names to a scanable form added
05.02.1999 long int changed to int
09.02.1999 input from stdin, output to stdout added
09.08.1999 bug in check of support parameter (<= 0) removed
22.10.1999 bug in item appearances reading removed
05.11.1999 rule evaluation measure EM_AIMP added
08.11.1999 output of add. rule eval. measure value added
11.11.1999 adapted to name/identifier maps
01.12.1999 check of item appearance added to sort function
15.03.2000 removal of unfrequent items added
16.03.2000 optional use of original rule support definition
----------------------------------------------------------------------*/
#include <iostream>
#include <stdio.h>
#include <math.h>


#include <fstream>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#ifndef NIMAPFN
#define NIMAPFN
#endif
#include "symtab.h"
#include "tfscan.h"
#include "istree.h"
#ifdef STORAGE
#include "storage.h"
#endif


#include "apriori.h"

#include <R.h>
//#include <Rcpp.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



#define EPSILON    1e-12

ostream& operator << (ostream& s, const truc& t) { return s<<t.getname(); }


#define MIN(a,b) (a)<(b)?(a):(b)
#define MAX(a,b) (a)>(b)?(a):(b)

/*----------------------------------------------------------------------
Preprocessor Definitions
----------------------------------------------------------------------*/
#define VERSION     "asirules based on  Christian Borgelt's apriori"

/* --- sizes --- */
#define BUFSIZE     256         /* size of read buffer */
#define BLKSIZE     256         /* block size for enlarging vectors */

/* --- error codes --- */
#define OK            0         /* no error */
#define E_NONE        0         /* no error */
#define E_NOMEM     (-1)        /* not enough memory */
#define E_FOPEN     (-2)        /* cannot open file */
#define E_FREAD     (-3)        /* read error on file */
#define E_FWRITE    (-4)        /* write error on file */
#define E_OPTION    (-5)        /* unknown option */
#define E_OPTARG    (-6)        /* missing option argument */
#define E_ARGCNT    (-7)        /* too few/many arguments */
#define E_STDIN     (-8)        /* double assignment of stdin */
#define E_SUPP      (-9)        /* invalid support */
#define E_CONF     (-10)        /* invalid confidence */
#define E_AREM     (-11)        /* invalid evaluation measure */
#define E_AREMVAL  (-12)        /* invalid value for measure */
#define E_RLEN     (-13)        /* invalid rule length */
#define E_SIDEXP   (-14)        /* set id expected */
#define E_ITEMEXP  (-15)        /* item expected */
#define E_DUPITEM  (-16)        /* duplicate item */
#define E_FLDCNT   (-17)        /* too many fields */
#define E_UNKAPP   (-18)        /* unknown appearance indicator */
#define E_UNKNOWN  (-19)        /* unknown error */

/*----------------------------------------------------------------------
Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- an item --- */
int  id;                      /* item identifier */
float frq;                    /* frequency in item sets */
char app;                     /* appearance indicator */
} ITEM;                         /* (item) */

typedef struct {                /* --- an item set --- */
int  cnt;                     /* number of items */
int  *iids;                 /* item identifier vector */
float *freq;								/* added by raph */
} ITEMSET;                      /* (item set) */

/*----------------------------------------------------------------------
Constants
----------------------------------------------------------------------*/
/* --- appearance indicators --- */
static const char *tab_body[] = {   /* item to appear in bodies only */
"i",  "in",  "a", "ante", "antecedent", "b", "body", NULL };
static const char *tab_head[] = {   /* item to appear in heads only */
"o",  "out", "c", "cons", "consequent", "h", "head", NULL };
static const char *tab_both[] = {   /* item to appear in both */
"io", "inout", "ac", "bh", "both", NULL };
static const char *tab_ignore[] = { /* item to ignore */
"n", "neither", "none", "ign", "ignore", "-", "", NULL };

static const char ccltab[256] = {   /* table of character classes */
/* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
/* 00 */    2,   2,   2,   2,   2,   2,   2,  'a',
/*  BS   HT   LF   VT   FF   CR   SO   SI */
'b', 't', 'n', 'v', 'f', 'r',  2,   2,
/* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
/* 10 */    2,   2,   2,   2,   2,   2,   2,   2,
/* CAN   EM  SUB  ESC   FS   GS   RS   US */
2,   2,   2,   2,   2,   2,   2,   2,
/* ' '  '!'  '"'  '#'  '$'  '%'  '&'  ''' */
/* 20 */    1,   1,  '"',  1,   1,   1,   1,   1,
/* '('  ')'  '*'  '+'  ','  '-'  '.'  '/' */
1,   1,   1,   0,   1,   0,   0,   1,
/* '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7' */
/* 30 */    0,   0,   0,   0,   0,   0,   0,   0,
/* '8'  '9'  ':'  ';'  '<'  '='  '>'  '?' */
0,   0,   1,   1,   1,   1,   1,   1,
/* '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G' */
/* 40 */    1,   0,   0,   0,   0,   0,   0,   0,
/* 'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O' */
0,   0,   0,   0,   0,   0,   0,   0,
/* 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W' */
/* 50 */    0,   0,   0,   0,   0,   0,   0,   0,
/* 'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_' */
0,   0,   0,   1, '\\',  1,   1,   0,
/* '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g' */
/* 60 */    1,   0,   0,   0,   0,   0,   0,   0,
/* 'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o' */
0,   0,   0,   0,   0,   0,   0,   0,
/* 'p'  'q'  'r'  's'  't'  'u'  'v'  'w' */
/* 70 */    0,   0,   0,   0,   0,   0,   0,   0,
/* 'x'  'y'  'z'  '{'  '|'  '}'  '~'  DEL */
0,   0,   0,   1,   1,   1,   1,   2,
/* 80 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* 90 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* a0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* b0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* c0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* d0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* e0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2,
/* f0 */    2,   2,   2,   2,   2,   2,   2,   2,
2,   2,   2,   2,   2,   2,   2,   2 };

/* --- error messages --- */
#ifndef QUIET                   /* if not quiet version */
static const char *errmsgs[] = {
  /* E_NONE      0 */  "no error\n",
  /* E_NOMEM    -1 */  "not enough memory\n",
  /* E_FOPEN    -2 */  "cannot open file `%s'\n",
  /* E_FREAD    -3 */  "read error on file `%s'\n",
  /* E_FWRITE   -4 */  "write error on file `%s'\n",
  /* E_OPTION   -5 */  "unknown option -%c\n",
  /* E_OPTARG   -6 */  "missing option argument\n",
  /* E_ARGCNT   -7 */  "wrong number of arguments\n",
  /* E_STDIN    -8 */  "double assignment of standard input\n",
  /* E_SUPP     -9 */  "invalid minimal support %g%%\n",
  /* E_CONF    -10 */  "invalid minimal confidence %g%%\n",
  /* E_AREM    -11 */  "invalid evaluation measure %d\n",
  /* E_AREMVAL -12 */  "invalid value %g%% for evaluation measure\n",
  /* E_RLEN    -13 */  "invalid rule length %d\n",
  /* E_SIDEXP  -14 */  "set id missing in file `%s', record %d\n",
  /* E_ITEMEXP -15 */  "item missing in file `%s', record %d\n",
  /* E_DUPITEM -16 */  "duplicate item in file `%s', record %d\n",
  /* E_FLDCNT  -17 */  "too many fields in file `%s', record %d\n",
  /* E_UNKAPP  -18 */  "unknown indicator in file `%s', record %d\n",
  /* E_UNKNOWN -19 */  "unknown error\n"
};
#endif  /* #ifndef QUIET */

/*----------------------------------------------------------------------
Global Variables
----------------------------------------------------------------------*/
TFSCAN  *tfscan = NULL;  /* table file scanner */
NIMAP   *nimap  = NULL;  /* name/identifier map */
ISTREE  *istree = NULL;  /* item set tree */
FILE    *in     = NULL;  /* input  file */
FILE    *out    = NULL;  /* output file */
int     reccnt  = 0;     /* number of records read */
int     dfltapp = IST_BOTH; /* default appearance indicator */

ITEMSET **isets = NULL;  /* item set vector */
int     isetvsz = 0;     /* size of item set vector */
int     isetcnt = 0;     /* number of item sets */
ITEMSET *cis    = NULL;  /* current item set */
int     cisvsz  = 0;     /* size of current item set */

char    sid[BUFSIZE];    /* current set identifier */
char    buf[4*BUFSIZE];  /* read/write buffer */
int     bufsid = -1;     /* flag for buffered set id */

/*----------------------------------------------------------------------
Auxiliary Functions
----------------------------------------------------------------------*/

static int appcode (const char *s)
{                               /* --- get appearance indicator code */
const char **p;               /* to traverse indicator list */

assert(s);                    /* check the function argument */
for (p = tab_body; *p; p++)   /* check `body' indicators */
if (strcmp(s, *p) == 0) return IST_BODY;
for (p = tab_head; *p; p++)   /* check `head' indicators */
if (strcmp(s, *p) == 0) return IST_HEAD;
for (p = tab_both; *p; p++)   /* check `both' indicators */
if (strcmp(s, *p) == 0) return IST_BOTH;
for (p = tab_ignore; *p; p++) /* check `ignore' indicators */
if (strcmp(s, *p) == 0) return IST_IGNORE;
return E_UNKAPP;              /* return error code */
}  /* appcode() */

/*--------------------------------------------------------------------*/

static int get_item (FILE *file)
{                               /* --- read an item */
ITEM *item;                   /* pointer to item */
int  d;                       /* delimiter type */
int  vsz;                     /* new vector size */
void *tmp;                    /* auxiliary pointer */
float frq;										/* added by raph */

assert(file                   /* check the function argument and */
&& tfscan && nimap && cis);         /* some global variables */
d = tfs_getfld(tfscan, file, buf, BUFSIZE-1, NULL);

if (d == TFS_REC) reccnt++;   /* read next name from file */
if ((d < 0) || (buf[0] == '\0')) return d;
item = (ITEM*)nim_byname(nimap, buf);/* look up name in name/id map */
if (!item)                     /* if item found, */
/*item->frq++;*/                /* count item occurence */
/*else */{                        /* if item not found */
if (dfltapp == IST_IGNORE)  /* if new items should be ignored, */
return d;                 /* do not register the item */
item = (ITEM*)nim_add(nimap, buf, sizeof(ITEM));
if (!item) return E_NOMEM;  /* add the new item to the map, */
item->frq = 0;              /* initialize the frequency counter, */
item->app = dfltapp;        /* and set the appearance indicator */
}
d = tfs_getfld(tfscan, file, buf, BUFSIZE-1, NULL);
if (d == TFS_REC) reccnt++;   /* read next name from file */
frq=(float)atof(buf);
item->frq+=frq;

if (cis->cnt >= cisvsz) {     /* if the item set vector is full, */
vsz = cisvsz                /* compute new vector size */
+ ((cisvsz > BLKSIZE) ? (cisvsz >> 1) : BLKSIZE);
tmp = realloc(cis, sizeof(ITEMSET) +(vsz) *(sizeof(int)+sizeof(float)));
if (!tmp) return E_NOMEM;   /* enlarge item set vector */
cis = (ITEMSET*)tmp; cisvsz = vsz;
cis->iids = (int*)((char*)tmp+sizeof(ITEMSET)); 
cis->freq = (float*)cis->iids+(vsz);
}                             /* set new vector and its size */
cis->freq[cis->cnt] = frq;
cis->iids[cis->cnt++] = item->id;
return d;                     /* add new item to item set */
}  /* get_item() */             /* and return delimiter type */

/*--------------------------------------------------------------------*/

static int get_iset (FILE *file, int isrec)
{                               /* --- read an item set */
int d, e;                     /* delimiter type */

assert(file                   /* check the function argument and */
&& tfscan && nimap && cis);         /* some global variables */
cis->cnt = 0;                 /* start a new item set */

/* --- one item set per record --- */
if (isrec) {                  /* if one item set per record, */
do { d = get_item(file);    /* read the items */
} while ((d == TFS_FLD)     /* of the item set */
&&       (buf[0] != '\0')); /* up to the end of the record */
if (d < 0) return d;        /* check for a read error */
if (cis->cnt > 0) return (buf[0] == '\0') ? E_ITEMEXP : TFS_REC;
return d;                   /* check for an empty item */
}                             /* and return delimiter type */

/* --- set id and item per record --- */
if (bufsid < 0) sid[0] = '\0';/* clear set identifier buffer */
e = TFS_EOF;                  /* init. end of file indicator */
while (1) {                   /* read loop for items */
if (bufsid >= 0) {          /* if there is a  buffered set id, */
d = bufsid; bufsid = -1;} /* get delimiter and clear flag */
else {                      /* if there is no buffered set id, */
do {                      /* read next set identifier */
d = tfs_getfld(tfscan, file, buf, BUFSIZE-1, NULL);
if (d == TFS_REC) reccnt++;   /* count line */
} while ((d == TFS_REC)   /* skip empty lines */
&&       (buf[0] == '\0'));
if (d < 0) return d;      /* check for a read error */
if (buf[0] == '\0') {     /* if no set identifier read */
if (d != TFS_EOF) return E_SIDEXP;
return (cis->cnt > 0) ? TFS_REC : e;
}                         /* check for end of file/end of set */
if (strcmp(buf, sid) !=0){/* if a new set id has been found */
if (sid[0] == '\0')     /* if it is the first set id, */
strcpy(sid, buf);     /* just copy it to the buffer */
else {                  /* if it is not the first set id */
strcpy(sid, buf); bufsid = d; return TFS_REC; }
}                         /* copy it to the buffer, set flag, */
}                           /* and abort the function */
if (d < TFS_FLD) {          /* if there is no item, */
e = TFS_REC; continue; }  /* clear end of file indicator */
d = get_item(file);         /* read item and check result */
if (d < 0)          return d;
if (buf[0] == '\0') return E_ITEMEXP;
if (d > TFS_REC)    return E_FLDCNT;
}  /* while(1) */
}  /* get_iset() */

/*--------------------------------------------------------------------*/

static void prepare (ITEMSET *iset)
{                               /* --- sort set and remove duplicates */
int i;                        /* loop variable */
int *src, *dst;               /* to traverse the item vector */
float *srcf, *dstf;

assert(iset);                 /* check the function argument */
if (iset->cnt <= 1) return;   /* if less than two items, abort */
ist_sort(iset->iids, iset->freq, iset->cnt);   
dst = src = iset->iids;       /* sort items in set and */
dstf = srcf = iset->freq;
for (i = iset->cnt; --i > 0;) {/* remove duplicate items */
++srcf;
if (*++src != *dst) {
  *++dst = *src; 
  *++dstf = *srcf; 
}
}
iset->cnt = (int)(++dst -iset->iids);

/* ATTENTION A CHANGER */

}  /* prepare() */

/*--------------------------------------------------------------------*/

static int frqcmp (const void *p1, const void *p2, void *data)
{                               /* --- compare item frequencies */
if (((ITEM*)p1)->app == IST_IGNORE)      return  1;
if (((ITEM*)p2)->app == IST_IGNORE)      return -1;
if (((ITEM*)p1)->frq < ((ITEM*)p2)->frq) return  1;
if (((ITEM*)p1)->frq > ((ITEM*)p2)->frq) return -1;
return 0;                     /* return sign of frequency diff. */
}  /* frqcmp() */

/*--------------------------------------------------------------------*/

static const char* scform (const char *name)
{                               /* --- convert name to scanable form */
char *dst;                    /* to traverse the buffer */
int  c, ccl;                  /* character and character class */
int  quotes = 0;              /* whether quotes are needed */
int  t;                       /* temporary buffer */

assert(name);                 /* check for a valid name */
if (!*name) quotes = 1;       /* an empty name needs quotes */
dst = buf; *dst++ = '"';      /* get buffer and store quote */
while (*name) {               /* traverse characters of name */
c = (unsigned char)*name++; /* get next character and */
ccl = ccltab[c];            /* its character class */
if (ccl != 0) quotes = 1;   /* check whether quotes are needed */
if (ccl < 2)                /* if normal character, */
*dst++ = c;               /* just store it */
else if (ccl > 2) {         /* if ANSI escape character, */
*dst++ = '\\'; *dst++ = ccl; }     /* store it as '\c' */
else {                      /* if any other character */
*dst++ = '\\'; *dst++ = 'x';
t = c >> 4;    *dst++ = (t > 9) ? (t -10 +'a') : (t +'0');
t = c & 0x0f;  *dst++ = (t > 9) ? (t -10 +'a') : (t +'0');
}                           /* store the character code */
}                             /* as a hexadecimal number */
if (quotes) *dst++ = '"';     /* store closing quote and */
*dst = '\0';                  /* terminate string in buffer */
return buf +1 -quotes;        /* return scanable name */
}  /* scform() */

/*--------------------------------------------------------------------*/
#ifndef NDEBUG                  /* if debug version */

static void del_isets (void)
{                               /* --- delete loaded item sets */
for (isets += isetcnt; --isetcnt >= 0; )
free(*--isets);             /* delete item sets and */
free(isets);                  /* the item set vector */
}  /* del_isets() */

#endif
/*----------------------------------------------------------------------
Main Functions
----------------------------------------------------------------------*/
#ifndef QUIET

static void msg (const char *fmt, ...)
{                               /* --- print a message to stderr */
va_list args;                 /* list of variable arguments */

va_start(args, fmt);          /* get variable arguments, */
vfprintf(stderr, fmt, args);  /* print the message, and */
va_end(args);                 /* end argument evaluation */
}  /* msg() */

#endif
/*--------------------------------------------------------------------*/

static void help (void)
{                               /* --- print help on eval. measures */
msg("\n");                    /* terminate startup message */
printf("additional rule evaluation measures (option -e#)\n");
printf("0: no additional rule evaluation measure\n");
printf("1: absolute confidence difference to prior\n");
printf("2: difference of confidence quotient to 1\n");
printf("3: absolute difference of improvement value to 1\n");
printf("4: information difference to prior\n");
printf("5: normalized chi^2 measure\n");
exit(0);                      /* abort porgram */
}  /* help() */

/*--------------------------------------------------------------------*/

static void error (int code, ...)
{                               /* --- print an error message */
#ifndef QUIET                 /* if not quiet version */
va_list    args;              /* list of variable arguments */
const char *msg;              /* error message */
#endif  /* #ifndef QUIET */

if ((code > 0) || (code < E_UNKNOWN))
code = E_UNKNOWN;           /* check error code */
#ifndef QUIET                 /* if not quiet version */
msg = errmsgs[-code];         /* get error message text */
va_start(args, code);         /* get variable arguments */
vfprintf(stderr, msg, args);  /* print the error message */
va_end(args);                 /* end variable argument evaluation */
#endif  /* #ifndef QUIET */

#ifndef NDEBUG                /* if debug version */
if (istree) ist_delete(istree);
if (tfscan) tfs_delete(tfscan);
if (nimap)  nim_delete(nimap);
if (isets)  del_isets();      /* clean up memory */
if (cis)    free(cis);        /* and close files */
if (in  && (in  != stdin))  fclose(in);
if (out && (out != stdout)) fclose(out);
#endif  /* #ifndef NDEBUG */
#ifdef STORAGE                /* if storage debugging */
showmem("at end of program"); /* check memory usage */
#endif
exit(code);                   /* abort the program */
}  /* error() */

/*--------------------------------------------------------------------*/


//extern "C"{
  
  int asirules (int *argc2, char *argv[])
  {                               /* --- main function */
  int argc=*argc2;
  int     i, k = 0, n;          /* loop variables, counters */
  float kk;
  char    *s;                   /* to traverse options */
  char    **optarg = NULL;      /* option argument */
  char    *fn_in   = NULL;      /* name of input  file */
  char    *fn_out  = NULL;      /* name of output file */
  char    *fn_app  = NULL;      /* name of item appearances file */
  char    *blanks  = NULL;      /* blanks */
  char    *fldseps = NULL;      /* field  separators */
  char    *recseps = NULL;      /* record separators */
  int     *idmap   = NULL;      /* identifier map for recoding */
  char    *apps    = NULL;      /* item appearance indicator vector */
  double  supp     = 10.0;      /* minimal support    (in percent) */
  double  conf     = 80.0;      /* minimal confidence (in percent) */
  double  impli    = 0.;
  double  entro    = 0.;
  double normal_simi=0;
  double entro_simi=0;
  double  phi=0.;
  double occhyp,occcon;
  int     rsdef    = IST_BODY;  /* rule support definition */
  int     hedge    = 0;         /* flag for hyperedge mode */
  int     arem     = 0;         /* additional rule evaluation measure */
  double  minval   = 10.0;      /* minimal evaluation measure value */
  int     minlen   = 1;         /* minimal rule length */
  int     maxlen   = 5;         /* maximal rule length */
  int     skip     = 0;         /* number of records to skip */
  int     isrec    = 0;         /* flag for one item set per record */
  int     load     = 1;         /* flag for loading item sets */
  int     sort     = 1;         /* flag for frequency sorting */
  int     c2scf    = 0;         /* flag for conv. to scanable form */
  char    *fmt     = "%.16f";  /* output format for supp./conf. */
  int     abs      = 0;         /* flag for absolute support output */
  int     aval     = 0;         /* flag for add. eval. measure value */
  int     maxcnt   = 0;         /* maximal number of items per set */
  int Binomial_law=0;
  float		  ori_thre = 0;
  int simple_impli=0;
  ITEM    *item;                /* for reading item appearances */
  ITEMSET *tmp, **isp;          /* new item set, to traverse sets */
  int     vsz;                  /* new item set vector size */
  int     *p;                   /* to traverse item identifiers */
  float   *q;
  int     pos;                  /* position in input file */
  int     d;                    /* delimiter type */
  const char *name;             /* temporary buffer for item names */
  #ifndef QUIET                 /* if not quiet version, */
  clock_t t;                    /* timer */
  #endif
  
  
  
  //  REprintf("AAAAAAAAAAAAAALLLLLLLLLLLLLLLLLLLLLLLLLLLLLOOOOOOOOOOO\n");
  
  #ifndef QUIET                 /* if not quiet version */
  //REprintf("AAAAAAAAAAAAAALLLLLLLLLLLLLLLLLLLLLLLLLLLLLOOOOOOOOOOO %d %s\n",argc,argv[0]);
  
  /* --- print usage message --- */
  if (argc > 1) {               /* if arguments are given */
  msg("%s - find association rules/hyperedges "
  "with apriori algorithm\n", argv[0]);
  msg(VERSION); }             /* print a startup message */
  else {                        /* if no arguments given */
  printf("usage: %s [options] infile outfile [appfile]\n", argv[0]);
  printf("find association rules/hyperedges "
  "with apriori algorithm\n");
  printf("%s\n", VERSION);
  printf("-h       hyperedge mode (default: rule mode)\n");
  printf("-s#      minimal support    of a rule/hyperedge "
  "(default: %g%%)\n", supp);
  printf("-c#      minimal confidence of a rule/hyperedge "
  "(default: %g%%)\n", conf);
  printf("-e#      additional rule evaluation measure "
  "(default: %d)\n",   arem);
  printf("-!       print a list of "
  "additional rule evaluation measures\n");
  printf("-d#      minimal value of additional evaluation measure "
  "(default: %g%%)\n", minval);
  printf("-m#      minimal number of items per rule/hyperedge "
  "(default: %d)\n", minlen);
  printf("-n#      maximal number of items per rule/hyperedge "
  "(default: %d)\n", maxlen);
  printf("-o       use original rule support definition "
  "(body & head)\n");
  printf("-t       write output in scanable form "
  "(quote certain characters)\n");
  printf("-p       print support/confidence with full precision\n");
  printf("-a       print absolute support (number of sets)\n");
  printf("-v       print value of additional "
  "rule evaluation measure\n");
  printf("-y       do not load item sets into memory "
  "(work on input file)\n");
  printf("-q       do not sort items w.r.t. their frequency\n");
  printf("-i#      ignore first # records of infile "
  "(default: %d)\n", skip);
  printf("-l       one set per record instead of set id and item\n");
  printf("-b/f/r#  blanks, field and record separators "
  "(default: \" \\t\", \" \\t\", \"\\n\")\n");
  printf("infile   file to read item sets from\n");
  printf("outfile  file to write association rules/hyperedges to\n");
  printf("appfile  file stating item appearances (optional)\n");
  return 0;                   /* print a usage message */
  }                             /* and abort the program */
  #endif  /* #ifndef QUIET */
  
  /* --- evaluate arguments --- */
  for (i = 1; i < argc; i++) {  /* traverse arguments */
  s = argv[i];                /* get option argument */
  if (optarg) { *optarg = s; optarg = NULL; continue; }
  if (*s == '-') {            /* -- if argument is an option */
  if (!*++s) { k++; continue; }
  while (*s) {              /* traverse options */
  switch (*s++) {         /* evaluate switches */
  case '!': help();                          break;
  case 's': supp   =      strtod(s, &s);     break;
  case 'c': conf   =      strtod(s, &s);     break;
  case 'e': arem   = (int)strtol(s, &s, 10); break;
  case 'd': minval =      strtod(s, &s);     break;
  case 'm': minlen = (int)strtol(s, &s, 10); break;
  case 'n': maxlen = (int)strtol(s, &s, 10); break;
  case 'i': simple_impli = 1; break;
  case 'o': rsdef  = IST_BOTH;               break;
  case 'a': abs    = 1;                      break;
  case 'p': fmt    = "%g%%";                 break;
  case 'v': aval   = 1;                      break;
  case 'h': hedge  = 1;                      break;
  case 'l': isrec  = 1;                      break;
  case 'y': load   = 0;                      break;
  case 'q': sort   = 0;                      break;
  case 't': c2scf  = 1;                      break;
  case 'j': Binomial_law = 1;								 break;
  case 'b': optarg = &blanks;                break;
  case 'f': optarg = &fldseps;               break;
  case 'r': optarg = &recseps;               break;
  case 'g': ori_thre= (float)(strtol(s, &s, 10)/100.);
  if(ori_thre>1 || ori_thre<0) ori_thre=0;
  break;
  default : error(E_OPTION, *--s);           break;
  }                       /* set option variables */
  if (optarg && *s) { *optarg = s; optarg = NULL; break; }
  } }                       /* get option argument */
  else {                      /* -- if argument is no option */
  switch (k++) {            /* evaluate non-options */
  case  0: fn_in  = s;      break;
  case  1: fn_out = s;      break;
  case  2: fn_app = s;      break;
  default: error(E_ARGCNT); break;
  }                         /* note filenames */
  }
  }                             /* check arguments */
  if ((supp  <  0) || (supp > 100)) error(E_SUPP, supp);
  if ((conf  <  0) || (conf > 100)) error(E_CONF, conf);
  if (minlen <= 0) error(E_RLEN, minlen);
  if (maxlen <= 0) error(E_RLEN, maxlen);
  switch (arem) {               /* check and translate measure */
  case  0: arem = EM_NONE;      break;
  case  1: arem = EM_DIFF;      break;
  case  2: arem = EM_QUOT;      break;
  case  3: arem = EM_AIMP;      break;
  case  4: arem = EM_INFO;      break;
  case  5: arem = EM_CHI2;      break;
  default: error(E_AREM, arem); break;
  }
  if ((minval < 0) || ((arem != EM_AIMP) && (minval > 100)))
  error(E_AREMVAL, minval);   /* check the measure parameter */
  if ((k < 2) || (k > 3))       /* check the number of arguments */
  error(E_ARGCNT);            /* (either in/out or in/out/app) */
  if ((!fn_in || !*fn_in) && (k > 2) && (!fn_app || !*fn_app))
  error(E_STDIN);             /* stdin must not be used twice */
  if (hedge) {                  /* in hyperedge mode adapt parameters */
  minval = conf; conf = 100.0F; }
  if (arem == EM_NONE)          /* if no add. rule eval. measure, */
  aval = 0;                   /* clear the corresp. output flag */
  
  /* --- create name/identifier map and table file scanner --- */
  nimap = nim_create(0, 0, (HASHFN*)0, (SYMFN*)0);
  if (!nimap) error(E_NOMEM);   /* create a name/identifier map */
  tfscan = tfs_create();        /* create a table file scanner */
  if (!tfscan) error(E_NOMEM);  /* and set delimiter characters */
  tfs_chars(tfscan, TFS_BLANK,  blanks);
  tfs_chars(tfscan, TFS_FLDSEP, fldseps);
  tfs_chars(tfscan, TFS_RECSEP, recseps);
  cisvsz  = 0;
  isetvsz = 0;
  isetcnt = 0;
  reccnt  = 0;
  isets = NULL;
  cis = (ITEMSET*)malloc(sizeof(ITEMSET) +(BLKSIZE-1) *sizeof(int));
  if (!cis) error(E_NOMEM);     /* allocate an item set buffer */
  #ifndef QUIET                 /* if not quiet version, */
  msg("\n");                    /* terminate startup message */
  #endif
  
  /* --- read item appearances --- */
  if (k > 2) {                  /* if item appearances are given */
  if (!fn_app || !*fn_app) {  /* if no file name is given, */
  in     =   stdin;         /* read from standard input */
  fn_app = "<stdin>"; }     /* set file name for error messages */
  else {                      /* if a file name is given, */
  #ifndef QUIET             /* if not quiet version */
  msg("reading %s ... ", fn_app);
  #endif                    /* print a log message */
  in = fopen(fn_app, "r");  /* open item appearances file */
  if (!in) error(E_FOPEN, fn_app);
  }
  d = tfs_getfld(tfscan, in, buf, BUFSIZE-1, NULL);
  if (d <  0)       error(E_FREAD,  fn_app);
  if (d >= TFS_FLD) error(E_FLDCNT, fn_app, 1);
  dfltapp = appcode(buf);     /* get default appearance code */
  if (dfltapp < 0) error(dfltapp, fn_app, 1);
  while (d > TFS_EOF) {       /* read item/indicator pairs */
  reccnt++;                 /* count the record read */
  d = tfs_getfld(tfscan, in, buf, BUFSIZE-1, NULL);
  if (d <= TFS_EOF) {       /* read next item */
  if (d < 0) error(E_FREAD, fn_app); else break; }
  if (buf[0] == '\0') {     /* if item read is empty */
  if (d >= TFS_FLD) error(E_ITEMEXP, fn_app, reccnt+1);
  continue;               /* check for a missing item */
  }                         /* and empty line/end of file */
  if (d < TFS_FLD) {        /* if no indicator follows */
  if (dfltapp == IST_IGNORE) continue;
  k = IST_IGNORE; }       /* ignore this item */
  else {                    /* if an indicator follows */
  d = tfs_getfld(tfscan, in, sid, BUFSIZE-1, NULL);
  if (d <  0)       error(E_FREAD,  fn_app);
  if (d >= TFS_FLD) error(E_FLDCNT, fn_app, reccnt+1);
  k = appcode(sid);       /* get appearance indicator code */
  if (k < 0) error(k, fn_app, reccnt+1);
  }                         /* check for a correct indicator */
  item = (ITEM*)nim_add(nimap, buf, sizeof(ITEM));
  if (item == EXISTS) error(E_DUPITEM, fn_app, reccnt+1);
  if (item == NULL)   error(E_NOMEM);
  item->frq = 0;            /* add the new item to the map, */
  item->app = (char)k;      /* clear the frequency counter, */
  }                           /* and set the appearance indicator */
  if (in != stdin) fclose(in);/* close the input file */
  #ifndef QUIET               /* if not quiet version */
  msg("[%d item(s)] done.\n", nim_cnt(nimap));
  #endif                      /* print a log message */
  }  /* if (k > 2) ... */
  
  /* --- read input file --- */
  #ifndef QUIET                 /* if not quiet version */
  t = clock();                  /* start the timer */
  #endif
  if (!fn_in || !*fn_in) {      /* if no input file name is given */
  #ifndef QUIET               /* if not quiet version */
  msg("reading %s ... ", fn_in = "<stdin>");
  #endif                      /* print a log message */
  in = stdin; }               /* read from standard input */
  else {                        /* if an input file name is given, */
  #ifndef QUIET               /* if not quiet version */
  msg("reading %s ... ", fn_in);
  #endif                      /* print a log message */
  in = fopen(fn_in, "r");     /* open input file for reading */
  if (!in) error(E_FOPEN, fn_in);
  }
  for (reccnt = d = 0; reccnt < skip; reccnt++) {
    do {                        /* read fields from file */
    d = tfs_getfld(tfscan, in, buf, BUFSIZE-1, NULL);
    } while (d == TFS_FLD);     /* while not at end of record */
    if (d <= TFS_EOF) break;    /* check for read error/end of file */
  }                             /* (skip `skip' records) */
  if (d   < 0) error(E_FREAD, fn_in);
  pos = ftell(in);              /* note the position reached */
  if (pos < 0) error(E_FREAD, fn_in);
  while (1) {                   /* read the item sets */
  d = get_iset(in, isrec);    /* get the next item set and */
  if (d <= TFS_EOF) {         /* check for read error/end of file */
  if (d < 0) error(d, fn_in, reccnt+1); else break; }
  prepare(cis);               /* sort items and remove duplicates */
  if (cis->cnt > maxcnt)      /* determine the maximum number */
  maxcnt = cis->cnt;        /* of items in one item set */
  if (load) {                 /* if to load item sets into memory */
  if (isetcnt >= isetvsz) { /* if item set vector is full, */
  vsz = isetvsz           /* compute new vector size */
  + ((isetvsz > BLKSIZE) ? (isetvsz >> 1) : BLKSIZE);
  isp = (ITEMSET**)realloc(isets, vsz *sizeof(ITEMSET*));
  if (!isp) error(E_NOMEM);  /* allocate a new vector */
  isets = isp; isetvsz = vsz;
  }                         /* set new vector and its size */
  tmp = (ITEMSET*)malloc(sizeof(ITEMSET)+(cis->cnt)*(sizeof(int)+sizeof(float)));
  if (!tmp) error(E_NOMEM); /* allocate a new item set */
  tmp->iids = (int*)((char*)tmp+sizeof(ITEMSET)); 
  tmp->freq = (float*)tmp->iids+(cis->cnt);
  tmp->cnt = cis->cnt;      /* and copy current set into it */
  for (i = cis->cnt; --i >= 0; ) {
    tmp->iids[i] = cis->iids[i];
    tmp->freq[i] = cis->freq[i];
  }
  isets[isetcnt] = tmp;     /* store new item set */
  }                           /* in the item set vector */
  isetcnt++;                  /* count the item set read */
  }  /* while (1) .. */
  if (load) {                   /* if item sets have been loaded, */
  if (in != stdin) fclose(in); in = NULL; } /* close input file */
  #ifndef QUIET                 /* if not quiet version */
  msg("[%d item(s), %d set(s)] done ", nim_cnt(nimap), isetcnt);
  msg("[%.2fs].\n", (clock()-t) /(double)CLOCKS_PER_SEC);
  #endif                        /* print a log message */
  
  /* --- sort items --- */
  idmap = (int*)malloc(nim_cnt(nimap) *sizeof(int));
  if (!idmap) error(E_NOMEM);   /* allocate an identifier map */
  if (sort) {                   /* sort with descending frequency */
  #ifndef QUIET               /* if not quiet version */
  msg("sorting items w.r.t. their frequency ... ");
  t = clock();                /* print a log message */
  #endif                      /* and start the timer */
  nim_sort(nimap, frqcmp, NULL, idmap, 1);
  #ifndef QUIET               /* sort items w.r.t. their frequency */
  msg("done [%.2fs].\n", (clock()-t) /(double)CLOCKS_PER_SEC);
  #endif                      /* if not quiet version, */
  }                             /* print a log message */
  
  /* --- recode loaded item sets --- */
  if (sort && load) {           /* if item sets loaded into memory */
  #ifndef QUIET               /* if not quiet version */
  msg("recoding loaded item sets ... ");
  t = clock();                /* print a log message */
  #endif                      /* and start the timer */
  /*if (rsdef == IST_BODY)    */  /* if rule supp. = body support */
  /*  k = (int)ceil(isetcnt *(supp/100) *(conf/100));*/
  /*else*/                        /* if rule supp. = body&head support */
  /*  k = (int)ceil(isetcnt *(supp/100));*/
  kk = (float)(isetcnt*((float)supp/100.));
  for (n = nim_cnt(nimap); --n >= 0; ) {
    if (((ITEM*)nim_byid(nimap, n))->frq >= kk)
    break;                  /* determine frequent items */
  }
  for (isp = isets +(i = isetcnt); --i >= 0; ) {
    --isp;                    /* traverse loaded item sets */
    for (p = (*isp)->iids +(k = (*isp)->cnt),q=((*isp)->freq +k); --k >= 0; ) {
      --p; *p = idmap[*p];
    }  /* set new item identfiers */
    ist_sort((*isp)->iids, (*isp)->freq, (*isp)->cnt);
    /* resort the item identifiers */
    for (p = (*isp)->iids +(k = (*isp)->cnt),q=((*isp)->freq +k); --k >= 0; )
    if (*--p <= n) {
      --q;
      break;   /* remove all items from the set */
    }
    (*isp)->cnt = k+1;        /* that are not frequent enough */
  }
  #ifndef QUIET               /* if not quiet version */
  msg("done [%.2fs].\n", (clock()-t) /(double)CLOCKS_PER_SEC);
  #endif                      /* print a log message */
  }                           
  
  /* --- check subsets --- */
  if (maxlen > maxcnt)          /* restrict the rule length */
  maxlen = maxcnt;            /* to the maximum set size */
  i = k = nim_cnt(nimap);       /* get the appearance indicators */
  for (apps = (char*)idmap +i; --i >= 0; )
  *--apps = ((ITEM*)nim_byid(nimap, i))->app;
  istree = ist_create(k, supp/100, conf/100, rsdef, apps);
  free(idmap);                  /* create an item set tree and */
  if (!istree) error(E_NOMEM);  /* delete the identifier map */
  #ifndef QUIET                 /* if not quiet version */
  msg("checking subsets of size");
  t = clock();                  /* print a log message */
  #endif                        /* and start the timer */
  do {                          /* build item set tree */
  #ifndef QUIET               /* if not quiet version */
  msg(" %d", ist_height(istree));
  #endif                      /* print a log message */
  if (!load) {                /* if to work on input file */
  if (fseek(in, pos, SEEK_SET) != 0)
  error(E_FREAD, fn_in);  /* reset file position */
  reccnt = skip;            /* reinitalize record counter */
  while (1) {               /* read item sets */
  d = get_iset(in, isrec);/* get next item set */
  if (d <= TFS_EOF) break;/* check for read error/end of file */
  prepare(cis);           /* sort items and remove duplicates */
  ist_count(istree, cis->iids, cis->freq, cis->cnt, 0);
  
  }                         /* count item set in tree */
  if (d < 0) error(d, fn_in, reccnt+1); }
  else {                      /* if item sets loaded into memory */
  isp = isets;              /* traverse loaded item sets */
  for (i = isetcnt; --i >= 0; isp++)
  ist_count(istree, (*isp)->iids, (*isp)->freq, (*isp)->cnt, 0);
  }                           /* count item set in tree */
  if (ist_height(istree) >= maxlen)    /* if maximal number of */
  break;                    /* items per rule reached, abort */
  /*		ist_show(istree);*/
  k = ist_addlvl(istree);     /* add level to item set tree */
  } while (k == 0);             /* loop, if a level was added */
  if (k < 0) error(E_NOMEM);    /* check for an error */
  if (!load) {                  /* if sets were read from a file, */
  if (in != stdin) fclose(in); in = NULL; } /* close input file */
  #ifndef QUIET                 /* if not quiet version */
  msg(" done [%.2fs].\n", (clock()-t) /(double)CLOCKS_PER_SEC);
  #endif                        /* print a log message */
  
  /* --- print rules/hyperedges --- */
  #ifndef QUIET                 /* if not quiet version */
  t = clock();                  /* start the timer */
  #endif
  if (!fn_out || !*fn_out) {    /* if no output file name is given, */
  out    =   stdout;          /* write to standard output and */
  fn_out = "<stdout>"; }      /* set file name for error messages */
  else {                        /* if an output file name is given */
  #ifndef QUIET               /* if not quiet version */
  msg("writing %s ... ", fn_out);
  #endif                      /* print a log message */
  out = fopen(fn_out, "w");   /* open output file */
  if (!out) error(E_FOPEN, fn_out);
  }
  ist_init(istree, minlen, arem, minval/100);
  reccnt = 0;                   /* init. rule/hyperedge counter */
  if (hedge) {                  /* if in hyperedge mode, */
  while (1) {                 /* extract hyperedges from tree */
  cis->cnt = ist_hedge(istree, cis->iids, &supp, &conf);
  if (cis->cnt <= 0) break; /* get next hyperedge and */
  for (i = 0; i < cis->cnt; i++) {  /* traverse items */
  name = nim_name(nim_byid(nimap, cis->iids[i]));
  fputs((c2scf) ? scform(name) : name, out);
  putc(' ', out);         /* print items in hyperedge */
  }                         /* (items separated by blanks) */
  fputs(" ", out); fprintf(out, fmt, supp *100);
  if (abs) fprintf(out, "/%.0f", supp *isetcnt);
  fputs(", ", out); fprintf(out, fmt, conf *100);
  fputs("\n", out);        /* print support and confidence */
  reccnt++;                 /* of the hyperedge and */
  } 
  }                         /* count the hyperedge written */
  else {                        /* if in rule mode, */
  doublemaptype hrules;
  const int size=1000;
  char buf[size];
  char b[100][size];
  double v[100],originality;
  fprintf(out, "hyp -> con,       occurrence(hyp),     occurrence(con),      support(rule),       confidence,      classical index,         entropic index,   implifiance index,  classical simi,       entropic simi\n");
  while (1) {                 /* extract rules from tree */
  cis->cnt = ist_rule(istree, cis->iids, &occhyp, &occcon, &supp, &conf, &minval,&phi, &entro,&impli,&normal_simi,&entro_simi,maxlen,simple_impli,Binomial_law);
  if (cis->cnt <= 0) break; /* get next rule and print its head */
  buf[0]='\0';
  for (i = 1; i < cis->cnt; i++) {  /* traverse the rule body */
  name = nim_name(nim_byid(nimap, cis->iids[i]));
  //fputs((c2scf) ? scform(name) : name, out);
  //putc(' ', out);         /* print the rule body */
  strcat(buf,(c2scf) ? scform(name) : name);
  strcpy(b[i-1],(c2scf) ? scform(name) : name);
  strcat(buf," ");
  }                         /* (items separated by blanks) */
  name = nim_name(nim_byid(nimap, cis->iids[0]));
  strcat(buf,"-> ");
  strcat(buf,(c2scf) ? scform(name) : name);
  strcpy(b[i-1],(c2scf) ? scform(name) : name);
  if(cis->cnt<maxlen)
  hrules[buf]=(float)impli;
  
  
  if(cis->cnt>2) {
    for(i=0;i<cis->cnt-1;i++) {
      sprintf(buf,"%s -> %s",b[i],b[2]);
      v[i]=impli-hrules[buf];
      if(v[i]<0) v[i]=0;
    }
    if(cis->cnt>=4) {
      int a1,a2;
      for(a1=0;a1<cis->cnt-2;a1++)
      for(a2=a1+1;a2<cis->cnt-1;a2++) {
        sprintf(buf,"%s %s -> %s",b[a1],b[a2],b[3]);
        v[i]=impli-hrules[buf];
        if(v[i]<0) v[i]=0;
        i++;
      }
    }
    if(cis->cnt>=5) {
      int a1,a2,a3;
      for(a1=0;a1<cis->cnt-3;a1++)
      for(a2=a1+1;a2<cis->cnt-2;a2++) 
      for(a3=a2+1;a3<cis->cnt-1;a3++) {
        sprintf(buf,"%s %s %s -> %s",b[a1],b[a2],b[a3],b[4]);
        v[i]=impli-hrules[buf];
        if(v[i]<0) v[i]=0;
        i++;
      }
    }	
    if(cis->cnt>=6) {
      int a1,a2,a3,a4;
      for(a1=0;a1<cis->cnt-4;a1++)
      for(a2=a1+1;a2<cis->cnt-3;a2++) 
      for(a3=a2+1;a3<cis->cnt-2;a3++)
      for(a4=a3+1;a4<cis->cnt-1;a4++) {
        sprintf(buf,"%s %s %s %s -> %s",b[a1],b[a2],b[a3],b[a4],b[5]);
        v[i]=impli-hrules[buf];
        if(v[i]<0) v[i]=0;
        i++;
      }
    }
    if(cis->cnt>=7) {
      int a1,a2,a3,a4,a5;
      for(a1=0;a1<cis->cnt-5;a1++)
      for(a2=a1+1;a2<cis->cnt-4;a2++) 
      for(a3=a2+1;a3<cis->cnt-3;a3++)
      for(a4=a3+1;a4<cis->cnt-2;a4++) 
      for(a5=a4+1;a5<cis->cnt-1;a5++) {
        sprintf(buf,"%s %s %s %s %s -> %s",b[a1],b[a2],b[a3],b[a4],b[a5],b[5]);
        v[i]=impli-hrules[buf];
        if(v[i]<0) v[i]=0;
        i++;
      }
      
    }
    double prod=v[0];
    for(i=1;i<pow(2.,cis->cnt-1)-2;i++)
    prod*=v[i];
    
    originality=pow(prod,1/(pow(2.,cis->cnt-1)-2));
    
    if(originality>=ori_thre) {
      for(i=0;i<cis->cnt-1;i++)
      fprintf(out,"%s ",b[i]);
      fprintf(out,"-> %s",b[i]);
      fputs(", ", out); fprintf(out, fmt, occhyp);
      fputs(", ", out); fprintf(out, fmt, occcon);
      fputs(", ", out); fprintf(out, fmt, supp *100);
      fputs(", ", out); fprintf(out, fmt, conf *100);
      fputs(", ", out);fprintf(out, fmt, phi*100);
      fputs(", ", out);fprintf(out, fmt, entro*100);
      fputs(", ", out);fprintf(out, fmt, impli*100);
      fputs(", ", out);fprintf(out, fmt, normal_simi*100);
      fputs(", ", out);fprintf(out, fmt, entro_simi*100);
      fputs(", ", out);fprintf(out, fmt, originality*100);
      fputs("\n", out);
      reccnt++;
    }
  }
  
  else {
    if((simple_impli==0 && impli> istree->conf -EPSILON) || (simple_impli==1 && phi> istree->conf -EPSILON)) {
      //warning if a rule is always true, we remove it
      if(cis->cnt-1!=0) {
        for(i=0;i<cis->cnt-1;i++)
        fprintf(out,"%s ",b[i]);
        fprintf(out,"-> %s",b[i]);
        fputs(", ", out); fprintf(out, fmt, occhyp);
        fputs(", ", out); fprintf(out, fmt, occcon);
        fputs(", ", out); fprintf(out, fmt, supp *100);
        fputs(", ", out); fprintf(out, fmt, conf *100);
        fputs(", ", out);fprintf(out, fmt, phi*100);
        fputs(", ", out);fprintf(out, fmt, entro*100);
        fputs(", ", out);fprintf(out, fmt, impli*100);
        fputs(", ", out);fprintf(out, fmt, normal_simi*100);
        fputs(", ", out);fprintf(out, fmt, entro_simi*100);
        fputs("\n", out);
        reccnt++;
      }
    }
  }
  } 
  }  
  
  if (fflush(out) != 0) error(E_FWRITE, fn_out);
  if (out != stdout) fclose(out);  /* close output file */
  #ifndef QUIET                 /* if not quiet version */
  msg("[%d %s(s)] done ", reccnt, (hedge) ? "hyperedge" : "rule");
  msg("[%.2fs].\n", (clock()-t) /(double)CLOCKS_PER_SEC);
  #endif                        /* print a log message */
  out = NULL;                   /* clear the output file variable */
  
  /* --- clean up --- */
  #ifndef NDEBUG                /* if debug version */
  ist_delete(istree);           /* delete item set tree, */
  tfs_delete(tfscan);           /* table file scanner, */
  nim_delete(nimap);            /* name/identifier map, */
  if (isets) del_isets();       /* loaded item sets, */
  free(cis);                    /* and current item set */
  #endif
  #ifdef STORAGE                /* if storage debugging */
  showmem("at end of program"); /* check memory usage */
  #endif
  return 0;                     /* return 'ok' */
  }  /* main() */
  
//}
