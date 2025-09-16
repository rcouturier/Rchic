/*----------------------------------------------------------------------
	File    : istree.c
	Contents: item set tree management
	Author  : Christian Borgelt
	History : 22.01.1996 file created
	07.02.1996 _child and _count completed,
	ist_addlvl and ist_count programmed
	08.02.1996 ist_rule programmed
	09.02.1996 ist_rule debugged
	10.02.1996 empty rule bodies made optional
	28.03.1996 support made relative to number of item sets
	25.06.1996 function _count optimized
	23.11.1996 rule extraction redesigned
	24.11.1996 rule selection criteria added
	29.07.1997 minor improvements
	18.08.1997 normalized chi^2 measure added
	parameter minlen added to function ist_init()
	15.01.1998 confidence comparison changed to >=
	23.01.1998 integer support computation in ist_rule
	changed from floor to ceil
	26.01.1998 condition added to set extension in _child
	27.01.1998 parameter `conly' added to function ist_create()
	10.02.1998 bug in computation of EM_INFO removed
	14.05.1998 item set tree navigation functions added
	08.08.1998 item appearances considered for rule selection
	20.08.1998 deferred child node vector allocation
	02.09.1998 several assertions added
	05.09.1998 bug concerning node id removed
	07.09.1998 function ist_hedge added
	22.09.1998 bug in rule extraction (item appearances) removed
	23.09.1998 computation of chi^2 measure simplified
	05.02.1999 long int changed to int
	25.08.1999 rule extraction simplified, comments improved
	function ist_sort added (fast item set sort)
	05.11.1999 rule evaluation measure EM_AIMP added
	08.11.1999 parameter `aval' added to function ist_rule
	11.11.1999 rule consequents moved to first field
	01.12.1999 bug in node reallocation removed
	----------------------------------------------------------------------*/
#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include <assert.h>
#include "istree.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
	Preprocessor Definitions
	----------------------------------------------------------------------*/
#define BLKSIZE    32           /* block size for level vector */
#define TH_INSERT  8            /* threshold for insertion (ist_sort) */
#define LN_2       0.69314718055994530942   /* ln(2) */
#define EPSILON    1e-12        /* to cope with roundoff errors */
#define F_HDONLY   0x80000000   /* flag for head only item in path */
#define ID(n)      ((int)((n)->id & ~F_HDONLY))
#define HDONLY(n)  ((int)((n)->id &  F_HDONLY))

/*----------------------------------------------------------------------
	Auxiliary Functions
	----------------------------------------------------------------------*/

static void _deltree (ISNODE *node)
{                               /* --- delete subtree */
	int    i;                     /* loop variable */
	ISNODE **child;               /* pointer to traverse child vector */

	assert(node);                 /* check the function argument */
	child = (ISNODE**)(node->cnts +2*node->size);
	for (child += (i = node->chcnt); --i >= 0; )
		if (*--child) _deltree(*child);
  free(node);                   /* recursively delete subtrees */
}  /* _deltree() */             /* and finally the node itself */

/*--------------------------------------------------------------------*/

static void _clrtree (ISNODE *node)
{                               /* --- clear subtree */
	int      i;                   /* loop variable */
	float *cnt;                /* pointer to traverse counter vector */
	ISNODE   **child;             /* pointer to traverse child vector */

	assert(node);                 /* check the function argument */
	for (cnt = node->cnts +(i = 2*node->size);  --i >= 0; )
		*--cnt = 0;                 /* clear all counters */
	child = (ISNODE**)(node->cnts +2*node->size);
	for (child += (i = node->chcnt); --i >= 0; )
		if (*--child) _clrtree(*child);
}  /* _clrtree() */             /* recursively clear all subtrees */

/*--------------------------------------------------------------------*/
#ifndef NDEBUG

static void _showtree (ISNODE *node, int level)
{                               /* --- show subtree */
	int      i, k, n = 0;         /* loop variables */
	float *cnt;                /* pointer to traverse counter vector */
	ISNODE   **child;             /* pointer to traverse child vector */

	assert(node && (level >= 0)); /* check the function arguments */
	cnt   = node->cnts;           /* traverse counters and children */
	child = (node->chcnt > 0) ? (ISNODE**)(node->cnts +2*node->size) : NULL;
	for (i = node->offs; i < node->offs +node->size; i++) {
		for (k = level; --k >= 0; ) /* indent and print */
			printf("   ");            /* item id and counter */
		printf("%d: %f\n", i, *cnt++);
		if (!child) continue;       /* check for a child node */
		if (*child) {               /* if there is a child node */
			if (i < ID(*child)) continue;
			_showtree(*child, level +1);
		}                           /* recursively show tree */
		child = (++n < node->chcnt) ? child +1 : NULL;
	}                             /* go to the next child */
}  /* _showtree() */

#endif
/*--------------------------------------------------------------------*/

static float/*unsigned*/ _getsupp (ISNODE *node, int *path, int len, double* occ_squ)
{                               /* --- get support of path (set) */
	int    i;                     /* vector index */
	ISNODE **child;               /* pointer to child node */

	assert(node && path && (len >= 0));     /* check arguments */
	while (--len > 0) {           /* follow path from the node */
		if (node->chcnt <= 0)       /* if there are no children, */
			return 0;                 /* support is less than minsupp */
		child = (ISNODE**)(node->cnts +2*node->size);
		i = path[len]-ID(child[0]); /* compute child vector index */
		if ((i < 0) || (i >= node->chcnt))
			return 0;                 /* if index is out of range, abort */
		node = child[i];            /* go to the corresponding child */
		if (!node) return 0;        /* if child does not exists, */
	}                             /* support is less than minsupp */
	i = path[0] -node->offs;      /* compute counter vector index */
	if ((i < 0) || (i >= node->size))
		return 0;                   /* if index is out of range, abort */
	*occ_squ=node->occ_square[i];
	return node->cnts[i];         /* return set support */
}  /* _getsupp() */

/*--------------------------------------------------------------------*/

static ISNODE* _child (ISTREE *ist, ISNODE *node, int item,
                       double s_min, double s_sub)
{                               /* --- create child node (extend set) */
	ISNODE   *curr;               /* to traverse the path to the root */
	int      i, index;            /* loop variable, data vector index */
	int      len;                 /* length of path to check */
	int      frst, last;          /* id. of first/last candidate */
	int      body = 0;            /* enough support for a rule body */
	int      hdonly;              /* head only item in path */
	int      app;                 /* appearance flags of item */
	double s_set;               /* support of some set */
	double dummy;

	assert(ist && node);          /* check the function arguments */
	assert((item >= node->offs) && (item < node->offs +node->size));
	app = ist->apps[item];        /* get the item appearance */
	if ((app == IST_IGNORE)       /* do not extend an item to ignore */
			||  ((HDONLY(node) && (app == IST_HEAD))))
		return NULL;                /* nor a set with two head only items */
	hdonly = HDONLY(node) || (app == IST_HEAD);

	/* --- initialize --- */
	index = item -node->offs;     /* compute index in data vector */
	s_set = node->cnts[index];    /* get support of item set to extend */
	if (s_set <  s_min)           /* if the set has not enough support */
		return NULL;                /* no child is needed, so abort */
	if (s_set >= s_sub)           /* if set support is large enough */
		body = 1;                   /* for a rule body, set body flag */
	ist->path[1] = item;          /* set fixed path element */
	frst = node->size; last = -1; /* initialize index limits */

	/* --- check candidates --- */
	/* The set S represented by the index-th vector element of the    */
	/* current node is extended only by combining it with the sets    */
	/* represented by the fields that follow it in the node vector,   */
	/* i.e. by the sets represented by vec[index+1] to vec[size-1].   */
	/* The sets that can be formed by combining the set S and the     */
	/* sets represented by vec[0] to vec[index-1] are processed in    */
	/* the branches for these sets.                                   */
	/*   In the below loop for each set represented by vec[index+1]   */
	/* to vec[size-1] it is checked, whether this set and all the     */
	/* other subsets of the same size, that can be formed from the    */
	/* union of this set and the set S, have enough support, so that  */
	/* a child node is necessary.                                     */
	/*   Note, that i +offs is the identifier of the item that has    */
	/* to be added to set S to form the union of the set S and the    */
	/* set T represented by vec[i], since S and T have the same path  */
	/* with the exception of the index in the current node. Hence we  */
	/* can speak of candidate items that are added to S.              */
	/*   Checking the support of the other subsets of the union of S  */
	/* and T that have the same size as S and T is done with the aid  */
	/* of a path variable. The items in this variable combined with   */
	/* the items on the path to the current node always represent     */
	/* the subset currently tested. That is, the path variable holds  */
	/* the path to be followed from the current node to arrive at     */
	/* the support counter for the subset. The path variable is       */
	/* initialized to [0]: <i+offs>, [1]: <item>, since the support   */
	/* counters for S and T can be inspected directly. Then this path */
	/* is followed from the parent node of the current node, which    */
	/* is equivalent to checking the subset that can be obtained by   */
	/* removing from the union of S and T the item that corresponds   */
	/* to the parent node (in the path to S or T, resp.).             */
	/*   Iteratively making the parent node the current node, adding  */
	/* its corresponding item to the path and checking the support    */
	/* counter at the end of the path variable when starting from its */
	/* (the new current node's) parent node tests all other subsets.  */
	/*   Another criterion is that the extended set must not contain  */
	/* two items which may appear only in the head of a rule. If two  */
	/* such items are contained in a set, neither can a rule be       */
	/* formed from its items nor can it be the antecedent of a rule.  */
	/* Whether a set contains two head only items is determined from  */
	/* the nodes `hdonly' flag and the appearance flags of the items. */

	for (i = index +1; i < node->size; i++) {
		app = ist->apps[node->offs +i]; /* get appearance flags of item */
		if ((app == IST_IGNORE) || (hdonly && (app == IST_HEAD)))
			continue;                 /* skip sets with 2 head only items */
		s_set = node->cnts[i];      /* traverse candidate items */
		if (s_set <  s_min)         /* if set support is too low, */
			continue;                 /* ignore this candidate */
		if (s_set >= s_sub)         /* if set support is large enough */
			body = 1;                 /* for a rule body, set body flag */
		ist->path[0] = node->offs+i;/* add candidate to path and */
		len  = 2;                   /* set initial path length */
		curr = node;                /* start at current node */
		while (curr->parent) {      /* while not at root node */
			s_set = _getsupp(curr->parent, ist->path, len, &dummy);
			if (s_set <  s_min)       /* get set support and */
				break;                  /* if it is too low, abort loop */
			if (s_set >= s_sub)       /* if some subset has enough support */
				body = 1;               /* for a rule body, set body flag */
			ist->path[len++] = ID(curr);
			curr = curr->parent;      /* add id of current node to path */
		}                           /* and go to parent node */
		if (s_set < s_min)          /* if some set's support is too low, */
			continue;                 /* ignore the corresponding candidate */
		if (i < frst) frst = i;     /* update index of first and */
		last = i;                   /* last successful candidate */
	}
	if (!body || (frst > last))   /* if no extension can have */
		return NULL;                /* enough support, abort function */

	/* --- create child --- */
	curr = (ISNODE*)malloc(sizeof(ISNODE) +(last-frst+1) *2*sizeof(float));
	if (!curr) return (ISNODE*)(void*)-1;  /* create child node */
	curr->parent = node;          /* set pointer to parent */
	curr->succ   = NULL;          /* clear successor pointer */
	curr->chcnt  = 0;             /* there are no children yet */
	curr->id     = item;              /* initialize item id */
	curr->cnts = (float*)((char*)curr+sizeof(ISNODE));
	curr->occ_square = (float*)curr->cnts+(last-frst+1);
	//for(j=0;j<2*(last-frst+1);j++) 
	//	curr->cnts[j]=0;            /* initialize cnts and occ_square */
	if (hdonly) curr->id |= F_HDONLY; /* set head only flag */
	curr->offs   = node->offs +frst;  /* initialize offset and */
	curr->size   = last -frst +1;     /* size of counter vector */
	return curr;                  /* return pointer to created child */
}  /* _child() */

/*--------------------------------------------------------------------*/

static void _cleanup (ISTREE *ist)
{                               /* --- clean up on error */
	ISNODE *node;                 /* to traverse the nodes in a level */
	ISNODE *tmp;                  /* temporary buffer */

	assert(ist);                  /* check the function argument */
	node = ist->levels[ist->height];
	while (node) {                /* traverse the level just added */
		tmp = node; node = node->succ;
		free(tmp);                  /* note first leaf, get next, */
	}                             /* and delete noted one */
	ist->levels[ist->height] = NULL;
	for (node = ist->levels[ist->height -1]; node; node = node->succ)
		node->chcnt = 0;            /* clear child node counters */
}  /* _cleanup() */             /* of current leaf nodes */

/*--------------------------------------------------------------------*/

static void _count (ISNODE *node, int *set, float old,float *freq, int cnt)
{                               /* --- count item set recursively */
	int    i;                     /* vector index */
	float frq;
	ISNODE **children;            /* child node vector */

	assert(node && set && (cnt >= 0));   /* check arguments */
	children = (ISNODE**)(node->cnts +2*node->size);
	while (--cnt >= 0) {          /* traverse item set */
		i = *set++ -node->offs;     /* compute counter vector index */
		frq = *freq++*old ;
		if (i < 0) continue;        /* if less    than first, ignore */
		if (i >= node->size) return;/* if greater than last,  abort */
		/*node->cnts[i]++; */           /* count item set */
		node->cnts[i]+=frq;
		node->occ_square[i]+=frq*frq;
		if (node->chcnt <= 0)       /* if there are no children, */
			continue;                 /* continue with next item */
		i += node->offs -ID(children[0]);   /* compute child vector index */
		if ((i < 0) || (i >= node->chcnt))
			continue;                 /* if index is out of range, continue */
		if (children[i]) _count(children[i], set, frq, freq, cnt);
	}                             /* count item set recursively */
}  /* _count() */

/*--------------------------------------------------------------------*/

static double _eval (int arem, double head, double body, double post)
{                               /* --- evaluate rule */
	double tmp, res;              /* temporary buffers */

	switch (arem) {               /* evaluate add. evaluation measure */
	case EM_DIFF:               /* absolute confidence difference */
		return (double)fabs(post -head);
	case EM_QUOT:               /* difference of conf. quotient to 1 */
		if (post > head) return 1 -head/post;
		return (head <= 0) ? 0 : (1 -post/head);
	case EM_AIMP:               /* abs. diff. of improvement to 1 */
		return (head <= 0) ? 0 : fabs(1 -post/head);
	case EM_INFO:               /* information difference to prior */
		if ((head < EPSILON) || (1-head < EPSILON)
				||  (body < EPSILON) || (1-body < EPSILON)) return 0.0;
		post *= body; res = 0.0;  /* support of     head and     body */
		if (post > 0) res += post *log(post /(   head  *   body));
		tmp = body -post;         /* support of not head and     body */
		if (tmp  > 0) res += tmp  *log(tmp  /((1-head) *   body));
		tmp = head -post;         /* support of     head and not body */
		if (tmp  > 0) res += tmp  *log(tmp  /(   head  *(1-body)));
		tmp = 1-head -body +post; /* support of not head and not body */
		if (tmp  > 0) res += tmp  *log(tmp  /((1-head) *(1-body)));
		return (double)(res/LN_2);/* return information gain in bits */
	case EM_CHI2:               /* normalized chi^2 measure */
		if ((head < EPSILON) || (1-head < EPSILON)
				||  (body < EPSILON) || (1-body < EPSILON)) return 0.0;
		tmp = head *body -post *body;
		return (double)((tmp *tmp)/(head *(1-head) *body *(1-body)));
	default:                    /* no measure -> select all rules */
		return 1.0;               /* return maximal value */
	}
}  /* _eval() */

/*----------------------------------------------------------------------
	Main Functions
	----------------------------------------------------------------------*/

ISTREE* ist_create (int itemcnt, double supp, double conf,
                    int rsdef, const char *apps)
{                               /* --- create an item set tree */
	ISTREE *ist;                  /* created item set tree */
	ISNODE **lvls;                /* level vector */
	int    *path;                 /* path vector */
	ISNODE *root;                 /* root node */
	char   *a;                    /* to traverse appearances vector */

	assert(itemcnt >= 0);         /* check the function arguments */
	assert((supp >= 0) && (supp <= 1) && (conf >= 0) && (conf <= 1));

	/* --- allocate memory --- */
	ist = (ISTREE*)malloc(sizeof(ISTREE) +(itemcnt-1) *sizeof(char));
	if (!ist) return NULL;        /* allocate tree body */
	ist->levels = lvls = (ISNODE**)malloc(BLKSIZE *sizeof(ISNODE*));
	if (!lvls) { free(ist); return NULL; }
	ist->path   = path = (int*)    malloc(BLKSIZE *sizeof(int));
	if (!path) { free(lvls); free(ist); return NULL; }
	lvls[0] = ist->curr = root =  /* allocate root node */
		(ISNODE*)calloc(1, sizeof(ISNODE) +itemcnt*2*sizeof(float));
	root->cnts=(float*)((char*)root+sizeof(ISNODE));
	root->occ_square = (float*)root->cnts+itemcnt;
	if (!root) { free(path); free(lvls); free(ist); return NULL; }

	/* --- initialize structures --- */
	ist->vecsize = BLKSIZE;       /* copy parameters to structure */
	ist->height  = 1;    ist->setcnt = 0;
	ist->supp    = supp; ist->conf   = conf;
	ist->rsdef   = rsdef & IST_BOTH;
	ist_init(ist, 1, EM_NONE, 1.0);
	root->parent = root->succ  = NULL;
	root->offs   = root->chcnt = root->id = 0;
	root->size   = itemcnt;
	a = ist->apps;                /* copy item appearances */
	if (apps) { while (--itemcnt >= 0) *a++ = *apps++ & IST_BOTH; }
	else      { while (--itemcnt >= 0) *a++ = IST_BOTH;           }
	return ist;                   /* return created item set tree */
}  /* ist_create() */

/*--------------------------------------------------------------------*/

void ist_delete (ISTREE *ist)
{                               /* --- delete an item set tree */
	assert(ist && ist->levels && ist->path);
  _deltree(ist->levels[0]);     /* delete nodes recursively */
	free(ist->levels);            /* delete level vector, */
	free(ist->path);              /* path vector, */
	free(ist);                    /* and tree body */
}  /* ist_delete() */

/*--------------------------------------------------------------------*/

int ist_addlvl (ISTREE *ist)
{                               /* --- add a level to item set tree */
	int      i, vsz;              /* loop variable, buffer */
	ISNODE   **plf;               /* to traverse the leaves */
	ISNODE   *leaf;               /* new (reallocated) leaf node */
	ISNODE   **end;               /* end of new level node list */
	ISNODE   *nnew;                /* current node in new level */
	ISNODE   *frst;               /* first child of current leaf */
	ISNODE   *last;               /* last  child of current leaf */
	ISNODE   **vec;               /* child node vector */
	double s_min;               /* minimal support of a set */
	double s_sub;               /* minimal support of a subset */
	void     *tmp;                /* temporary buffer */

	assert(ist && ist->levels && ist->path);  /* check arguments */

	/* --- enlarge level vector --- */
	if (ist->height >= ist->vecsize) {  /* if the level vector is full */
		vsz = ist->vecsize +BLKSIZE;/* compute new vector size */
		tmp = realloc(ist->levels, vsz*sizeof(ISNODE*));
		if (!tmp) return -1;        /* enlarge the level vector */
		ist->levels = (ISNODE**)tmp;/* and set the new vector */
		tmp = realloc(ist->path,   vsz *sizeof(int));
		if (!tmp) return -1;        /* enlarge the path vector */
		ist->path = (int*)tmp; ist->vecsize = vsz;
	}                             /* set new path vector and its size */
	end  = ist->levels +ist->height;
	*end = NULL;                  /* start a new tree level */

	/* --- add tree level --- */
	s_sub = ist->supp;
	if (s_sub < 1) s_sub = 1;     /* minimal support of a subset */
	if (ist->rsdef == IST_BOTH)   /* if rule supp. = body&head support */
		s_min = s_sub;              /* use the subset support */
	else {                        /* if rule supp. = body support */
		s_min = ist->supp *ist->setcnt;
		if (s_min < 1) s_min = 1;   /* use the full set support */
	}
	plf = ist->levels +ist->height -1;    /* traverse leaf nodes */
	for ( ; *plf; plf = &(*plf)->succ) {
		frst = last = NULL;         /* traverse counter vector */
		for (i = 0; i < (*plf)->size; i++) {
			nnew = _child(ist, *plf, (*plf)->offs +i, s_min, s_sub);
			if (!nnew) continue;       /* create a child, if necessary */
			if (nnew == (void*)-1) { _cleanup(ist); return -1; }
			if (!frst) frst = nnew;    /* note first and last child node */
			*end = last = nnew;        /* add node at the end of the list */
			end  = &nnew->succ;        /* that contains the new level */
		}                           /* and advance end pointer */
		if (!frst) continue;        /* if no child node created, continue */
		vsz  = ID(last) -ID(frst) +1;/* compute size of child node vector */
		leaf = (ISNODE*)realloc(*plf, sizeof(ISNODE)
														+ i*2*sizeof(float)
														+ (vsz)   *sizeof(ISNODE*));
		leaf->cnts=(float*)((char*)leaf+sizeof(ISNODE));
		leaf->occ_square = (float*)leaf->cnts+i;
		if (!leaf) { _cleanup(ist); return -1; }
		leaf->chcnt = vsz;          /* add a child node vector to the */
		/* current leaf and set its size */
		if (leaf->parent) {         /* if the current leaf has a parent */
			vec = (ISNODE**)(leaf->parent->cnts +2*leaf->parent->size);
			vec[(vec[0] != *plf) ? ID(leaf) -ID(vec[0]) : 0] = leaf;
		}                           /* adapt pointer from parent node */
		*plf = leaf;                /* set new (reallocated) leaf node */
		vec  = (ISNODE**)(leaf->cnts +2*leaf->size);
		for (i = vsz; --i >= 0; ) vec[i] = NULL;
		i = ID(frst);               /* get item identifier of first child */
		for (nnew = frst; nnew; nnew = nnew->succ) {
			vec[ID(nnew) -i] = nnew;    /* set child node pointers */
			nnew->parent     = leaf;   /* and adapt parent pointers */
		}
	}
	if (!ist->levels[ist->height])/* if no child added, */
		return 1;                   /* abort the function */
	ist->height++;                /* otherwise increment tree height */
	ist_clear(ist);               /* and clear the item set tree */
	return 0;                     /* return 'ok' */
}  /* ist_addlvl() */

/*--------------------------------------------------------------------*/

void ist_clear (ISTREE *ist)
{                               /* --- clear item set tree */
	assert(ist && ist->levels);   /* check the function argument */
	_clrtree(ist->levels[0]);     /* recursively clear all nodes */
	ist->setcnt = 0;              /* clear the total set counter and */
	ist->isnode = NULL;           /* the item set node for rule extr. */
}  /* ist_clear() */

/*--------------------------------------------------------------------*/

void ist_count (ISTREE *ist, int *set, float *freq, int cnt, int sort)
{                               /* --- count item set in tree */
	assert(ist && set && (cnt >= 0));  /* check the function arguments */
	if (sort) ist_sort(set, freq, cnt);      /* sort the item identifiers */
	_count(ist->levels[0], set, 1.,freq, cnt);  /* recursively count item set */
	ist->setcnt++;                     /* increment set counter */
}  /* ist_count() */

/*--------------------------------------------------------------------*/

void ist_up (ISTREE *ist, int root)
{                               /* --- go up in item set tree */
	assert(ist && ist->curr);     /* check the function argument */
	if      (root)                /* if root flag set, */
		ist->curr = ist->levels[0]; /* go to the root node */
	else if (ist->curr->parent)   /* if it exists, go to the parent */
		ist->curr = ist->curr->parent;
}  /* ist_up() */

/*--------------------------------------------------------------------*/

int ist_down (ISTREE *ist, int item)
{                               /* --- go down in item set tree */
	ISNODE **child;               /* child node vector of current node */

	assert(ist && ist->curr);     /* check the function argument */
	if (ist->curr->chcnt <= 0)    /* if there are no child nodes, */
		return -1;                  /* abort the function */
	child = (ISNODE**)(ist->curr->cnts +ist->curr->size);
	item -= ID(child[0]);         /* compute index in child node vector */
	if ((item < 0) || (item >= ist->curr->chcnt)
      ||  !child[item])             /* if index is out of range */
		return -1;                  /* or child does not exist, abort */
	ist->curr = child[item];      /* otherwise go to the child node */
	return 0;                     /* return `ok' */
}  /* ist_down() */

/*--------------------------------------------------------------------*/

float ist_getcnt (ISTREE *ist, int item)
{                               /* --- get counter for item */
	assert(ist && ist->curr);     /* check the function argument */
	item -= ist->curr->offs;      /* compute index in counter vector */
	if ((item < 0) || (item >= ist->curr->size))
		return -1;                  /* if index is out of range, abort */
	return ist->curr->cnts[item]; /* return value of counter */
}  /* ist_getcnt() */

/*--------------------------------------------------------------------*/

void ist_init (ISTREE *ist, int minlen, int arem, double minval)
{                               /* --- initialize rule extraction */
	assert(ist                    /* check the function arguments */
				 && (minlen > 0) && (minval >= 0.0) && (minval <= 1.0));
  ist->index   = ist->hditem = -1;
  ist->isnode  = ist->hdnode = NULL;
  ist->rulelen = minlen;        /* initialize rule extraction */
	if ((arem < EM_NONE) || (arem >= EM_UNKNOWN))
		arem = EM_NONE;             /* check, adapt, and note */
	ist->arem   = arem;           /* additional evaluation measure */
	ist->minval = minval;         /* and its minimal value */
}  /* ist_init() */

/*--------------------------------------------------------------------*/

float Normal(double val)
{
  double t1,b1,b2,b3,b4,b5,res;
  int inv=0;
  if(val<0) {
    inv=1;
    val=-val;
  }
  t1=1./(val*0.2316419+1.);
  b1=0.31938153;
  b2=-0.356563782;
  b3=1.781477937;
  b4=-1.821255978;
  b5=1.330274429;
  res=1./sqrt(2*3.14159265358979323846)*exp(-0.5*val*val);
  res=1-res*(b1*t1+b2*t1*t1+b3*pow(t1,3)+b4*pow(t1,4)+b5*pow(t1,5));
  if(inv) res=1-res;
  return (float)res;
}

/*--------------------------------------------------------------------*/

double xl2xb(double x)
{
  if(x-0.00001>0.00001) return (double)x*(double)log(x)/log(2.);
  else return 0;
}



long double Cnp(int n,int p)
{
  long double res=1;
  int i;
  if(p<n)
		{
			for(i=1;i<=n-p;i++)
				res*=double(i+p)/i;
		}
  return res;
}


double Binomiale(double p, long nb_row,long inter)
{
  double res=0;
  int s;
  for(s=0;s<=inter;s++)
    res+=Cnp(nb_row,s)*pow(p,s)*pow(1.-p,nb_row-s);
  return res;
}


double Poisson(double para,int cumul)
{
  double res=0,f=1;
  int s;
  for(s=0;s<=cumul;s++)
		{
			res+=pow(para,s)/f*exp(-para);
			f*=(double)(s+1);
		}
  return res;
}


/*--------------------------------------------------------------------*/


int ist_rule (ISTREE *ist, int *rule,
              double* occhyp, double* occcon, double *supp, double *conf, double *aval, double *phi,
              double *entro, double *impli, double* normal_simi,double *entro_simi, int maxlen, int simple_impli, int Binomial_law)
{                               /* --- extract next rule */
	int      i;                   /* loop variable */
	int      item;                /* buffer for an item identifier */
	ISNODE   *isnode;             /* current item set node */
	ISNODE   *parent;             /* parent of the item set node */
	unsigned s_rule;              /* minimal support of a rule */
	unsigned s_min;               /* minimal support of a set */
	float s_set;               /* support of set    (body & head) */
	float s_sub;               /* support of subset (body) */


	double occ_a;
	double occ_b;
	double occ_n;
	double occsqa,occsqb;
	double pi;
	double pi2;
	double occ_abb;
	double occ_ab; 
	double tmp_b,tmp_c;
	double alpha, beta, t, h1, h2, ii, unmb;


	double   p_body, p_head;      /* prior confidences/probabilities */
	double   c, v;                /* confidence and measure value */
	int      app;                 /* appearance flag of head item */

	assert(ist && rule && supp && conf);  /* check arguments */

	/* --- initialize --- */
	if (ist->rulelen > ist->height)  /* if the tree is not high enough */
		return -1;                     /* for the rule length, abort */
	s_rule = (unsigned)ceil(ist->setcnt *ist->supp);
	if (s_rule < 1) s_rule = 1;   /* compute the minimal rule support */
	s_min = (ist->rsdef == IST_BOTH) ? s_rule
		: (unsigned)ceil(ist->setcnt *ist->supp *ist->conf);
	if (ist->isnode)              /* if this is not the first rule, */
		isnode = ist->isnode;       /* get the buffered item set node */
	else {                        /* if this is the first rule */
		isnode = ist->isnode = ist->levels[ist->rulelen-1];
		ist->index = ist->hditem = -1;         /* initialize the */
	}                             /* rule extraction variables */

	/* --- find rule --- */
	while (1) {                   /* search for a rule */
		if (ist->hditem >= 0) {     /* --- select next item subset */
			ist->path[ist->pathlen++] = ist->hditem;
			ist->hditem = ID(ist->hdnode); /* add previous head to the path */
			ist->hdnode = ist->hdnode->parent;/* and get the next head item */
			if (!ist->hdnode)         /* if all subsets have been processed */
				ist->hditem = -1;       /* clear the head item to trigger the */
		}                           /* selection of a new item set */
		if (ist->hditem < 0) {      /* --- select next item set */
			if (++ist->index >= isnode->size) { /* if all subsets have been */
				isnode = isnode->succ;  /* processed, go to the successor */
				if (!isnode) {          /* if at the end of a level, go down */
					if (++ist->rulelen > ist->height)
						return -1;          /* if beyond the leaf level, abort */
					isnode = ist->levels[ist->rulelen-1];
				}                       /* get the 1st node of the new level */
				ist->isnode = isnode;   /* note the new item set node and */
				ist->index  = 0;        /* start with the first item set */
			}                         /* of the new item set node */
			i = isnode->offs +ist->index;
			if ((ist->apps[i] == IST_IGNORE)
					||  (HDONLY(isnode) && (ist->apps[i] == IST_HEAD)))
				continue;               /* skip sets with two head only items */
			ist->hditem  = i;         /* set the head item identifier */
			ist->hdonly  = HDONLY(isnode) || (ist->apps[i] == IST_HEAD);
			ist->hdnode  = isnode;    /* get the new head only flag, */
			ist->pathlen = 0;         /* set the new head item node, */
		}                           /* and clear the path */
		app = ist->apps[ist->hditem];  /* get head item appearance */
		if (!(app & IST_HEAD) || (ist->hdonly && (app != IST_HEAD)))
			continue;                 /* if rule is not allowed, skip it */
		s_set = isnode->cnts[ist->index];  /* get the item set support */

		if (s_set < s_min) {        /* if the set support is too low, */
			ist->hditem = -1; continue; }    /* skip this item set */
		if (ist->pathlen <= 0) {    /* if there is no path, */
			parent = isnode->parent;  /* get subset support from parent */
			if (parent) {
				s_sub = parent->cnts[ID(isnode) -parent->offs];
				occsqa = parent->occ_square[ID(isnode) -parent->offs];
			}
			else {
				s_sub = (float)ist->setcnt;
			}
		}
		else {                      /* if there is a path (not 1st subset)*/
			s_sub = _getsupp(ist->hdnode, ist->path, ist->pathlen, &occsqa);
		}                           /* get subset support using the path */
		if (s_sub < s_rule)         /* if the subset support is too low, */
			continue;                 /* get the next subset/next set */
		c = (double)s_set/s_sub;    /* compute the rule confidence */


		occ_a=s_sub;
		occ_b=ist->levels[0]->cnts[ist->hditem];
		occsqb=ist->levels[0]->occ_square[ist->hditem];
		occ_n=ist->setcnt;
		pi=occ_a*(occ_n-occ_b);	/*pi=p(a)p(b barre)*/
		pi2=occsqa*(occ_n-2*occ_b+occsqb);
		occ_ab=s_set;          /*a et b*/
		occ_abb=occ_a-occ_ab;  /*a et b barre*/
		tmp_b=occ_abb-pi/occ_n;


		*occhyp=occ_a;
		*occcon=occ_b;
		int binary_data=(occ_a==occsqa && occ_b==occsqb);

		if(Binomial_law) {
			if(binary_data && pi/occ_n*(1-pi/(double)(occ_n*occ_n))<50.) {
				*phi=1.-Binomiale(pi/(occ_n*occ_n),(long)occ_n,(long)occ_abb);
			}
			else {
				if(pi2==0) tmp_c=0;
				else tmp_c=tmp_b/sqrt(pi2/occ_n*(1.-pi2/(occ_n*occ_n)));
				*phi=1.-Normal(tmp_c);
			}
		}
		else  {
			if(binary_data && (pi/occ_n<=5. ||occ_abb<48.)) {
				*phi=1.-Poisson(occ_a/occ_n*(occ_n-occ_b),(int)occ_abb);
			}
			else {
				if(pi2==0) tmp_c=0;
				else tmp_c=tmp_b/sqrt(pi2/occ_n);
				*phi=1.-Normal(tmp_c);
			}
		}

		
	/*	
		//for MGK test only  uncomment that for MGK
	  //MUST BE COMMENTED
		//MGK
		//p(b|a) = p(a and b)/p(a)
		double pb_knowing_a=occ_ab/occ_a;
		double mgk=(pb_knowing_a-occ_b/occ_n)/(1.-occ_b/occ_n);
		*phi=mgk;
	*/	
		

		alpha=(double)occ_a/occ_n;
		beta=(double)occ_b/occ_n;
		t=(double)occ_abb/occ_n;
		/*		if (t <= alpha/2.0)
					h1 =-xl2xb((alpha-t)/alpha) - xl2xb(t/alpha);
					else 
					h1 =1;
					unmb = 1.0 - beta;
					if (t <= unmb/2.0)
					h2 = -xl2xb((unmb-t)/unmb) - xl2xb(t/unmb);
					else
					h2 = 1;
					ii = pow((1-h1*h1)*(1-h2*h2),0.25);;
					*impli=sqrt(*phi*ii);
					*/


		//  entropic version
		if (t <= alpha/2.0) 
					h1 =0.5*(1+xl2xb(0.5-t/alpha) + xl2xb(0.5+t/alpha));
		else if( t<=alpha)
					h1 =0.5*(1-xl2xb(t/alpha-0.5) - xl2xb(1.5-t/alpha));
		else
			h1=1.;
		unmb = 1.0 - beta;
		if (t <= unmb/2.0)
				h2 = 0.5*(1+xl2xb(0.5-t/unmb) + xl2xb(0.5+t/unmb));
		else if(t<=unmb)
			h2 = 0.5*(1-xl2xb(t/unmb-0.5) - xl2xb(1.5-t/unmb));
		else h2=1.;
		ii = sqrt((1.-h1)*(1.-h2));
		*entro=(1.-1./(2.*sqrt(occ_n)))*ii;
					



		//implifiance
		double occ_nonanonb=occ_n-(occ_b+occ_abb);
		double C1=occ_ab/occ_a;
		double C2=occ_nonanonb/(occ_n-occ_b);
		*impli=*phi*pow(C1*C2,0.25);




		//normal similarity
		double c=(occ_a*occ_b)/occ_n;
		*normal_simi=Normal((occ_ab-c)/sqrt(c));




		if(simple_impli)
			{
				if ((ist->rulelen==maxlen && *phi < ist->conf -EPSILON) || (ist->rulelen<maxlen && ist->conf==0)) /* if the confidence is too low, */
					continue;                 /* get the next item subset/item set */
			}
		else
			{
				if ((ist->rulelen==maxlen && *impli < ist->conf -EPSILON) || (ist->rulelen<maxlen && ist->conf==0)) /* if the confidence is too low, */
					continue;                 /* get the next item subset/item set */
			}

		if (ist->arem == EM_NONE) { /* if no add. eval. measure given, */
			v = 0; break; }           /* abort the loop (select the rule) */
		if (ist->rulelen < 2) {     /* if rule has an empty antecedent, */
			v = 0; break; }           /* abort the loop (select the rule) */
		p_body = (double)s_sub /ist->setcnt;
		p_head = (double)ist->levels[0]->cnts[ist->hditem]
			/ ist->setcnt;       /* compute prior probabilities */
		v = _eval(ist->arem, p_head, p_body, c);
		if (v >= ist->minval)       /* if rule value exceeds the minimal */
			break;                    /* of the add. rule eval. measure, */
	}  /* while (1) */            /* abort the loop (select rule) */

	/* --- build rule --- */
	i    = ist->rulelen;          /* get rule length */
	item = ist->index +isnode->offs; /* if the current item is */
	if (item != ist->hditem)         /* not the head of the rule, */
		rule[--i] = item;              /* add it to the body */
	while (isnode->parent) {         /* traverse path to root and */
		if (ID(isnode) != ist->hditem) /* add all items on this path */
			rule[--i] = ID(isnode);   /* (except the head of the rule) */
		isnode = isnode->parent;    /* to the rule body */
	}
	rule[0] = ist->hditem;        /* set the rule head */
	*supp = ((ist->rsdef == IST_BODY) ? s_sub : s_set)
		/ (double)ist->setcnt;  /* set the rule support */
	*conf = c;                    /* and the rule confidence */
	if (aval) *aval = v;          /* set the value of the add. measure */
	return ist->rulelen;          /* return the rule length */
}  /* ist_rule() */

/*--------------------------------------------------------------------*/

int ist_hedge (ISTREE *ist, int *hedge, double *supp, double *conf)
{                               /* --- extract next hyperedge */
	int      i;                   /* loop variable */
	ISNODE   *isnode;             /* current item set node */
	ISNODE   *hdnode;             /* node containing the rule head */
	int      *path, len;          /* path buffer and path length */
	unsigned s_min;               /* minimal support of a hyperedge */
	double s_set;               /* support of set    (body & head) */
	double s_sub;               /* support of subset (body) */
	double dummy;

	assert(ist && hedge && supp && conf);  /* check arguments */

	/* --- initialize --- */
	if (ist->rulelen > ist->height)  /* if the tree is not high enough */
		return -1;                     /* for the hyperedge size, abort */
	s_min = (unsigned)ceil(ist->setcnt *ist->supp);
	if (s_min < 1) s_min = 1;     /* compute the minimal support */
	if (!ist->isnode)             /* on first hyperedge, initialize */
		ist->isnode = ist->levels[ist->rulelen-1];    /* current node */
	isnode = ist->isnode;         /* get the current item set node */
	path   = ist->path;           /* and the path buffer */

	/* --- find hyperedge --- */
	while (1) {                   /* search for a hyperedge */
		if (++ist->index >= isnode->size) { /* if all subsets have been */
			isnode = isnode->succ;    /* processed, go to the successor */
			if (!isnode) {            /* if at the end of a level, go down */
				if (++ist->rulelen > ist->height)
					return -1;            /* if beyond the leaf level, abort */
				isnode = ist->levels[ist->rulelen-1];
			}                         /* get the 1st node of the new level */
			ist->isnode = isnode;     /* note the new item set node and */
			ist->index  = 0;          /* start with the first item set */
		}                           /* of the new item set node */
		s_set = isnode->cnts[ist->index];
		if (s_set < s_min)          /* if the set support is too low, */
			continue;                 /* skip this item set */
		hdnode = isnode->parent;    /* get subset support from parent */
		if (hdnode) s_sub = hdnode->cnts[ID(isnode) -hdnode->offs];
		else        s_sub = ist->setcnt;
		*conf = (double)s_set/s_sub;/* compute confidence of first rule */
		path[0] = ist->index +isnode->offs;
		len     = 1;                /* initialize path and */
		while (hdnode) {            /* traverse the path up to root */
			s_sub = _getsupp(hdnode, path, len,&dummy);   /* get the set support */
			*conf += (double)s_set/s_sub; /* and sum the rule confidences */
			path[len++] = ID(hdnode); /* store head item in the path */
			hdnode = hdnode->parent;  /* and go to the parent node */
		}                           /* (get the next rule head) */
		*conf /= ist->rulelen;      /* average rule confidences */
		if (*conf >= ist->minval) break;
	}  /* while(1) */             /* if confidence suffices, abort loop */
	*supp = (double)s_set/ist->setcnt;  /* set hyperedge support */

	/* --- build hyperedge --- */
	i          = ist->rulelen;    /* get current hyperedge size and */
	hedge[--i] = ist->index +isnode->offs;  /* store the first item */
	while (isnode->parent) {      /* while not at the root node */
		hedge[--i] = ID(isnode);    /* add item to the hyperedge */
		isnode = isnode->parent;    /* and go to the parent node */
	}
	return ist->rulelen;          /* return hyperedge size */
}  /* ist_hedge() */

/*--------------------------------------------------------------------*/

void _isrec (int *a, float *b, int n)
{                               /* --- recursive part of ist_sort */
	int l, r;                     /* indices of exchange positions */
	int x, t;                     /* pivot element and exchange buffer */
	int m;                        /* number of elements in sections */
	float tt,xx;

	do {                          /* sections sort loop */
		l = 0; r = n -1;            /* start at left and right boundary */
		x = a[n >> 1];              /* get the middle element as pivot */
		if (a[l] > a[r]) { 
			t = a[l]; a[l] = a[r]; a[r] = t;
			tt = b[l]; b[l] = b[r]; b[r] = tt;
		}
		if      (x < a[l]) {x = a[l];xx=b[l];}   /* compute median of three */
		else if (x > a[r]) {x = a[r];xx=b[r];}   /* to find a better pivot */
		while (1) {                 /* split and exchange loop */
			while (a[++l] < x)        /* skip left  elements that are */
				;                       /* smaller than the pivot element */
			while (a[--r] > x)        /* skip right elements that are */
				;                       /* greater than the pivot element */
			if (l >= r) {             /* if less than two elements left, */
				if (l <= r) { l++; r--; } break; }       /* abort the loop */
			t = a[l]; a[l] = a[r]; a[r] = t;
			tt=b[l];b[l]=b[r];b[r]=tt;
		}                           /* otherwise exchange elements */
		m = n -l;                   /* compute the number of elements */
		n = r +1;                   /* right and left of the split */
		if (n > m) {                /* if right section is smaller, */
			if (m >= TH_INSERT)       /* but larger than the threshold, */
				_isrec(a +l, b+l, m); }      /* sort it by an recursive call */
		else {                      /* if the left section is smaller, */
			if (n >= TH_INSERT)       /* but larger than the threshold, */
				_isrec(a,b, n);           /* sort it by an recursive call, */
			a += l; b+=l; n = m;            /* then switch to the right section */
		}                           /* keeping its size m in variable n */
	} while (n >= TH_INSERT);     /* while greater than threshold */
}  /* _isrec() */

/*--------------------------------------------------------------------*/

void ist_sort (int *set, float *freq,int cnt)
{                               /* --- sort an item set */
	int i, k;                     /* indices in number vector */
	int s;                        /* num. of elements in first section */
	int t;                        /* exchange buffer */
	float tt;

	if (cnt <= 1) return;         /* do not sort less than two elements */
	if (cnt < TH_INSERT)          /* if less elements than threshold */
		s = cnt;                    /* for insertion sort, note the */
	else {                        /* number of elements, otherwise */
		_isrec(set, freq, cnt);           /* call the recursive sort function */
		s = TH_INSERT -1;           /* and get the number of elements */
	}                             /* in the first vector section */
	for (i = k = 0; --s > 0; )    /* find position of smallest element */
		if (set[++k] < set[i]) i = k;     /* within the first s elements */
	if (i > 0) {                  /* swap smallest element to front */
		t = set[i]; set[i] = set[0]; set[0] = t; 
		tt = freq[i]; freq[i] = freq[0]; freq[0] = tt;}   /* as a sentinel */
	for (i = 0; ++i < cnt; ) {    /* standard insertion sort */
		t = set[i];                 /* note the number to insert */
		tt = freq[i];
		for (k = i; set[k-1] > t; k--) {   /* shift right all numbers that */
			set[k] = set[k-1];        /* are greater than the one to insert */
			freq[k] = freq[k-1];
		}
		set[k] = t;                 /* and store the number to insert */
		freq[k] = tt;
	}                             /* in the place thus found */
}  /* ist_sort() */

/*--------------------------------------------------------------------*/
#ifndef NDEBUG

void ist_show (ISTREE *ist)
{                               /* --- show an item set tree */
	assert(ist);                  /* check the function argument */
	_showtree(ist->levels[0], 0); /* show nodes recursively */
	printf("total: %f\n", ist->setcnt);
}  /* ist_show() */

#endif
