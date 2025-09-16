/*----------------------------------------------------------------------
  File    : istree.h
  Contents: item set tree management
  Author  : Christian Borgelt
  History : 22.01.1996 file created
            29.01.1996 ISNODE.offs and ISNODE.id added
            08.02.1996 ISTREE.setcnt, ISTREE.curr, ISTREE.index,
                       ISTREE.head and ISTREE.conf added
            28.03.1996 support made relative to number of item sets
            23.11.1996 ISTREE.root and ISTREE.leaf0 replaced
                       by ISTREE.levels (first nodes of each level)
            24.11.1996 ISTREE.rem added
            18.08.1997 chi^2 evaluation measure added
                       parameter `minlen' added to function ist_init()
            11.02.1998 parameter `minval' added to function ist_init()
            14.05.1998 item set tree navigation functions added
            08.08.1998 parameter `apps' added to function ist_create()
            20.08.1998 structure ISNODE redesigned
            07.09.1998 function ist_hedge added
            08.12.1998 function ist_setcnt added, float -> double
            05.02.1999 long int changed to int
            25.08.1999 function ist_sort added
            26.08.1999 functions ist_first and ist_last added
            05.11.1999 rule evaluation measure EM_AIMP added
            08.11.1999 parameter `aval' added to function ist_rule
----------------------------------------------------------------------*/
#ifndef __ISTREE__
#define __ISTREE__

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- additional evaluation measures --- */
#define EM_NONE     0           /* no measure */
#define EM_DIFF     1           /* absolute conf. difference to prior */
#define EM_QUOT     2           /* difference of conf. quotient to 1 */
#define EM_AIMP     3           /* abs. diff. of improvement to 1 */
#define EM_INFO     4           /* information difference to prior */
#define EM_CHI2     5           /* normalized chi^2 measure */
#define EM_UNKNOWN  6           /* unknown measure */

/* --- item appearances --- */
#define IST_IGNORE  0           /* ignore item */
#define IST_BODY    1           /* item may appear in rule body */
#define IST_HEAD    2           /* item may appear in rule head */
#define IST_BOTH    (IST_HEAD|IST_BODY)

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct _isnode {        /* --- item set node --- */
  struct _isnode *parent;       /* parent node */
  struct _isnode *succ;         /* successor node on same level */
  int            id;            /* id of prec. item (index in parent) */
  int            offs;          /* offset of counter vector */
  int            size;          /* size   of counter vector */
  int            chcnt;         /* number of child nodes */
  /*unsigned int   cnts[1];*/       /* counter vector */
	float *cnts;
	float *occ_square;
	
} ISNODE;                       /* (item set node) */

typedef struct {                /* --- item set tree --- */
  ISNODE         **levels;      /* first node of each level */
  int            vecsize;       /* size of level vector */
  int            height;        /* current tree height */
  int            setcnt;        /* number of sets counted */
  double         supp;          /* minimal support of an item set */
  double         conf;          /* minimal confidence of a rule */
  int            rsdef;         /* rule support definition */
  int            arem;          /* additional rule evaluation measure */
  double         minval;        /* minimal evaluation measure value */
  ISNODE         *curr;         /* current node in tree */
  ISNODE         *isnode;       /* item set node for rule extraction */
  int            index;         /* index in item set node */
  ISNODE         *hdnode;       /* head item node for rule extraction */
  int            hditem;        /* head item of previous rule */
  int            rulelen;       /* current rule length (tree level) */
  int            *path;         /* path buffer */
  int            pathlen;       /* current path length */
  int            hdonly;        /* head only item in current set */
  char           apps[1];       /* item appearances */
} ISTREE;                       /* (item set tree) */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern ISTREE* ist_create (int itemcnt, double supp, double conf,
                           int rsdef, const char *apps);
extern void    ist_delete (ISTREE *ist);
extern int     ist_addlvl (ISTREE *ist);
extern int     ist_height (ISTREE *ist);
extern int     ist_setcnt (ISTREE *ist);

extern void    ist_clear  (ISTREE *ist);
extern void    ist_count  (ISTREE *ist, int *set, float *freq, int cnt, int sort);
extern int     ist_first  (ISTREE *ist);
extern int     ist_last   (ISTREE *ist);
extern void    ist_up     (ISTREE *ist, int root);
extern int     ist_down   (ISTREE *ist, int item);
extern float     ist_getcnt (ISTREE *ist, int item);

extern void    ist_init   (ISTREE *ist, int minlen,
                           int arem, double minval);
extern int     ist_rule   (ISTREE *ist, int *rule,
                           double *occhyp, double *occcon, double *supp, double *conf, double *aval, double *phi,
													 double *entro, double *impli,  double* normal_simi,double *entro_simi, int maxlen, int simple_impli, int Binomial_law);
extern int     ist_hedge  (ISTREE *ist, int *hedge,
                           double *supp, double *conf);

#ifndef NDEBUG
extern void    ist_show   (ISTREE *ist);
#endif

extern void    ist_sort   (int *set, float *freq, int cnt);

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define ist_height(t)     ((t)->height)
#define ist_setcnt(t)     ((t)->setcnt)
#define ist_first(t)      ((t)->curr->offs)
#define ist_last(t)       ((t)->curr->offs +(t)->curr->size -1)

#endif
