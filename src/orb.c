/***************************************************************************
**
*A  orb.c               orb-package                        Max Neunhoeffer
**
**  Copyright (C) 2009  Max Neunhoeffer
**  This file is free software, see license information at the end.
**  
*/

const char * Revision_orb_c =
   "$Id: orb.c,v$";

#include <stdlib.h>

#include "src/compiled.h"          /* GAP headers                */

/* This file corresponds to orb/gap/avltree.gi, it imlements some of
 * its functionality on the C level for better performance. */

Obj AVLTreeType;    /* Imported from the library to be able to check type */

/* Conventions:
 *
 * A balanced binary tree (AVLTree) is a positional object having the 
 * following entries:
 *   ![1]     len: last used entry, never shrinks), always = 3 mod 4
 *   ![2]     free: index of first freed entry, if 0, none free
 *   ![3]     nodes: number of nodes currently in the tree
 *   ![4]     alloc: highest allocated index, always = 3 mod 4
 *   ![5]     three-way comparison function
 *   ![6]     top: reference to top node
 *   ![7]     unused
 * 
 * From index 8 on for every position = 0 mod 4:
 *   ![4n]    obj: an object
 *   ![4n+1]  left: left reference or < 8 (elements there are smaller)
 *   ![4n+2]  right: right reference or < 8 (elements there are bigger)
 *   ![4n+3]  rank: number of nodes in left subtree plus one
 * For freed nodes position ![4n] holds the link to the next one
 * For used nodes references are divisible by four, therefore
 * the mod 4 value can be used for other information.
 * We use left mod 4: 0 - balanced
 *                    1 - balance factor +1
 *                    2 - balance factor -1     */

/* Note that we have to check the arguments for functions that are called
 * by user programs since we do not go through method selection! */

Obj AVLCmp_C(Obj self, Obj a, Obj b)
/* A very fast three-way comparison function. */
{
    if (EQ(a,b)) return INTOBJ_INT(0);
    else if (LT(a,b)) return INTOBJ_INT(-1);
    else return INTOBJ_INT(1);
}

/* The following are some internal macros to make the code more readable.
 * We always know that these positions are properly initialized! */

/* Last used entry, never shrinks, always = 3 mod 4: */
#define AVLLen(t) INT_INTOBJ(ELM_PLIST(t,1))
#define SetAVLLen(t,i) SET_ELM_PLIST(t,1,INTOBJ_INT(i))
/* Index of first freed entry, if 0, none free: */
#define AVLFree(t) INT_INTOBJ(ELM_PLIST(t,2))
#define AVLFreeObj(t) ELM_PLIST(t,2)
#define SetAVLFree(t,i) SET_ELM_PLIST(t,2,INTOBJ_INT(i))
#define SetAVLFreeObj(t,i) SET_ELM_PLIST(t,2,i)
/* Number of nodes currently in the tree: */
#define AVLNodes(t) INT_INTOBJ(ELM_PLIST(t,3))
#define SetAVLNodes(t,i) SET_ELM_PLIST(t,3,INTOBJ_INT(i))
/* Highest allocated index, always = 3 mod 4: */
#define AVLAlloc(t) INT_INTOBJ(ELM_PLIST(t,4))
#define SetAVLAlloc(t,i) SET_ELM_PLIST(t,4,INTOBJ_INT(i))
/* Three-way-comparison function: */
#define AVL3Comp(t) ELM_PLIST(t,5)
#define SetAVL3Comp(t,f) SET_ELM_PLIST(t,5,f);CHANGED_BAG(t)
/* Reference to the top node: */
#define AVLTop(t) INT_INTOBJ(ELM_PLIST(t,6))
#define SetAVLTop(t,i) SET_ELM_PLIST(t,6,INTOBJ_INT(i))
/* Reference to the value plist: */
#define AVLValues(t) ELM_PLIST(t,7)
#define SetAVLValues(t,l) SET_ELM_PLIST(t,7,l);CHANGED_BAG(t)

#define AVLmask ((unsigned long)(3L))
#define AVLmask2 ((unsigned long)(-4L))
/* Use the following only if you know that the tree object is long enough and
 * something is bound to position i! */
#define AVLData(t,i) ELM_PLIST(t,i)
#define SetAVLData(t,i,d) SET_ELM_PLIST(t,i,d); CHANGED_BAG(t)
#define AVLLeft(t,i) (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLmask2)
#define SetAVLLeft(t,i,n) SET_ELM_PLIST(t,i+1, \
  INTOBJ_INT( (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLMask) + n ))
#define AVLRight(t,i) INT_INTOBJ(ELM_PLIST(t,i+2))
#define SetAVLRight(t,i,n) SET_ELM_PLIST(t,i+2,INTOBJ_INT(n))
#define AVLRank(t,i) INT_INTOBJ(ELM_PLIST(t,i+3))
#define SetAVLRank(t,i,r) SET_ELM_PLIST(t,i+3,INTOBJ_INT(r))
#define AVLBalFact(t,i) (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLmask)
#define SetAVLBalFact(t,i,b) SET_ELM_PLIST(t,i+1, \
  INTOBJ_INT( (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLMask2) + b ))

static Int AVLNewNode( Obj t )
{
    Int n,a;
    n = AVLFree(t);
    if (n > 0) {
        SetAVLFreeObj(t,ELM_PLIST(t,n));
    } else {
        n = AVLLen(t);
        a = AVLAlloc(t);
        if (n < a) {
            /* There is already enough allocated! */
            SetAVLLen(t,n+4);
            n++;
        } else {
            /* We have to allocate new space! */
            n++;
            a = a*2 + 1;   /* Retain congruent 3 mod 4 */
            SetAVLAlloc(t,a);
            ResizeBag(t,(a+1)*sizeof(Obj));
            SetAVLLen(t,n+3);
        }
    }
    SET_ELM_PLIST(t,n,INTOBJ_INT(0));
    SET_ELM_PLIST(t,n+1,INTOBJ_INT(0));
    SET_ELM_PLIST(t,n+2,INTOBJ_INT(0));
    SET_ELM_PLIST(t,n+3,INTOBJ_INT(0));
    return n;
}

static Obj AVLNewNode_C( Obj self, Obj t )
{
    return INTOBJ_INT(AVLNewNode(t));
}

static inline void AVLFreeNode( Obj t, Int n )
{
    Obj v;
    SET_ELM_PLIST(t,n,AVLFreeObj(t));
    SetAVLFree(t,n);
    n /= 4;
    v = AVLValues(t);
    if (v != Fail && ISB_LIST(v,n)) {
        UNB_LIST(v,n);
    }
}

static Obj AVLFreeNode_C( Obj self, Obj t, Obj n)
{
    AVLFreeNode(t,INT_INTOBJ(n));
    return (Obj) 0;
}

static inline Int AVLFind( Obj t, Obj d )
{
    Obj compare,c;
    Int p;

    compare = AVL3Comp(t);
    p = AVLTop(t);
    while (p >= 8) {
        c = CALL_2ARGS(compare,d,AVLData(t,p));
        if (c == INTOBJ_INT(0))
            return p;
        else if (INT_INTOBJ(c) < 0)   /* d < AVLData(t,p) */
            p = AVLLeft(t,p);
        else                          /* d > AVLData(t,p) */
            p = AVLRight(t,p);
    }
    return 0;
}

static Obj AVLFind_C( Obj self, Obj t, Obj d )
{
    Obj tmp = INTOBJ_INT(AVLFind(t,d));
    if (tmp == INTOBJ_INT(0)) 
        return Fail;
    else
        return tmp;
}
            
static Obj AVLLookup_C( Obj self, Obj t, Obj d )
{
    Int p = AVLFind(t,d);
    if (p == 0) return Fail;
    Obj vals = AVLValues(t);
    p /= 4;
    if (vals == Fail || !ISB_LIST(vals,p))
        return True;
    else
        return ELM_LIST(vals,p);
}
        
static inline Int AVLIndex( Obj t, Int i )
{
    Int p,offset,r;

    if (i < 1 || i > AVLNodes(t)) return 0;
    p = AVLTop(t);
    offset = 0;
    while (1) {   /* will be left by return */
        r = offset + AVLRank(t,p);
        if (i < r)   /* go left: */
            p = AVLLeft(t,p);
        else if (i == r)   /* found! */
            return p;
        else {    /* go right: */
            offset = r;
            p = AVLRight(t,p);
        }
    }
}

static Obj AVLIndex_C( Obj self, Obj t, Obj i )
{
    Int p = AVLIndex( t, INT_INTOBJ(i) );
    if (p == 0) 
        return Fail;
    else
        return AVLData(t,p);
}

static Obj AVLIndexLookup_C( Obj self, Obj t, Obj i )
{
    Int p = AVLIndex(t,INT_INTOBJ(i));
    if (p == 0) return Fail;
    Obj vals = AVLValues(t);
    p /= 4;
    if (vals == Fail || !ISB_LIST(vals,p))
        return True;
    else
        return ELM_LIST(vals,p);
}

static inline void AVLRebalance( Obj tree, Int q, Int *newroot, int *shrink )
/* the tree starting at q has balanced subtrees but is out of balance:
   the depth of the deeper subtree is 2 bigger than the depth of the other
   tree. This function changes this situation following the procedure
   described in Knuth: "The Art of Computer Programming".
   It returns nothing but stores the new start node of the subtree into
   "newroot" and in "shrink" a boolean value which indicates, if the
   depth of the tree was decreased by 1 by this operation. */
{
  Int p, l;

  *shrink = 1;   /* in nearly all cases this happens */
  if (AVLBalFactor(tree,q) == 2) {   /* was: < 0 */
      p = AVLLeft(tree,q);
  else
      p = AVLRight(tree,q);
  if (AVLBalFactor(tree,p) == AVLBalFactor(tree,q)) {
      /* we need a single rotation:
             q++             p=           q--          p=
            / \             / \          / \          / \
           a   p+    ==>   q=  c    OR  p-  c   ==>  a   q=
              / \         / \          / \              / \
             b   c       a   b        a   b            b   c      */
      if (AVLBalFactor(tree,q) == 1) {    /* was: > 0 */
          SetAVLRight(tree,q,AVLLeft(tree,p));
          SetAVLLeft(tree,p,q);
          SetAVLBalFactor(tree,q,0);
          SetAVLBalFactor(tree,p,0);
          SetAVLRank(tree,p,AVLRank(tree,p) + AVLRank(tree,q));
      } else {
          SetAVLLeft(tree,q,AVLRight(tree,p));
          SetAVLRight(tree,p,q);
          SetAVLBalFactor(tree,q,0);
          SetAVLBalFactor(tree,p,0);
          SetAVLRank(tree,q,AVLRank(tree,q) - AVLRank(tree,p));
      }
  } else if (AVLBalFactor(tree,p) == 3 - AVLBalFactor(tree,q)) {
              /* was: = - */
       /* we need a double rotation:
             q++                             q--
            / \             c=              / \            c=
           a   p-         /   \            p+  e         /   \
              / \   ==>  q     p    OR    / \      ==>  p     q
             c   e      / \   / \        a   c         / \   / \
            / \        a   b d   e          / \       a   b d   e
           b   d                           b   d                     */
      if (AVLBalFactor(tree,q) == 1) {   /* was: > 0 */ 
          l = AVLLeft(tree,p);
          SetAVLRight(tree,q,AVLLeft(tree,l));
          SetAVLLeft(tree,p,AVLRight(tree,l));
          SetAVLLeft(tree,l,q);
          SetAVLRight(tree,l,p);
          if (AVLBalFactor(tree,l) == 1) {   /* was: > 0 */
              SetAVLBalFactor(tree,p,0);
              SetAVLBalFactor(tree,q,-1);
          } else if (AVLBalFactor(tree,l) == 0) {
              SetAVLBalFactor(tree,p,0);
              SetAVLBalFactor(tree,q,0);
          } else {   /* AVLBalFactor(tree,l) < 0 */
              SetAVLBalFactor(tree,p,1);
              SetAVLBalFactor(tree,q,0);
          }
          SetAVLBalFactor(tree,l,0);
          SetAVLRank(tree,p,AVLRank(tree,p) - AVLRank(tree,l));
          SetAVLRank(tree,l,AVLRank(tree,l) + AVLRank(tree,q));
          p = l;
      } else {
          l = AVLRight(tree,p);
          SetAVLLeft(tree,q,AVLRight(tree,l));
          SetAVLRight(tree,p,AVLLeft(tree,l));
          SetAVLLeft(tree,l,p);
          SetAVLRight(tree,l,q);
          if (AVLBalFactor(tree,l) == 2) {  /* was: < 0 */
              SetAVLBalFactor(tree,p,0);
              SetAVLBalFactor(tree,q,1);
          } else if (AVLBalFactor(tree,l) == 0) {
              SetAVLBalFactor(tree,p,0);
              SetAVLBalFactor(tree,q,0);
          } else {   /* AVLBalFactor(tree,l) > 0 */
              SetAVLBalFactor(tree,p,2);  /* was: -1 */
              SetAVLBalFactor(tree,q,0);
          }
          SetAVLBalFactor(tree,l,0);
          SetAVLRank(tree,l,AVLRank(tree,l) + AVLRank(tree,p));
          SetAVLRank(tree,q,AVLRank(tree,q) - AVLRank(tree,l));   
                               # new value of AVLRank(tree,l)!
          p = l;
      }
  } else {  /* AVLBalFactor(tree,p) = 0 */
      /* we need a single rotation:
            q++             p-           q--          p+
           / \             / \          / \          / \
          a   p=    ==>   q+  c    OR  p=  c   ==>  a   q-
             / \         / \          / \              / \
            b   c       a   b        a   b            b   c    */
      if (AVLBalFactor(tree,q) == 1) {   /* was: > 0 */
          SetAVLRight(tree,q,AVLLeft(tree,p));
          SetAVLLeft(tree,p,q);
          SetAVLBalFactor(tree,q,1);
          SetAVLBalFactor(tree,p,2);   /* was: -1 */
          SetAVLRank(tree,p,AVLRank(tree,p) + AVLRank(tree,q));
      } else {
          SetAVLLeft(tree,q,AVLRight(tree,p));
          SetAVLRight(tree,p,q);
          SetAVLBalFactor(tree,q,2);  /* was: -1 */
          SetAVLBalFactor(tree,p,1);
          SetAVLRank(tree,q,AVLRank(tree,q) - AVLRank(tree,p));
      }
      *shrink = 0;
  }
}

Obj static AVLRebalance_C( Obj self, Obj tree, Obj q )
{
    Int newroot;
    int shrink;
    Obj tmp;
    AVLRebalance( tree, INT_INTOBJ(q), &newroot, &shrink );
    tmp = NEW_PREC(2);
    AssPRec(tmp,RNamName("newroot"),INTOBJ_INT(newroot));
    AssPRec(tmp,RNamName("shrink"),shrink ? True : False);
    return tmp;
}

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "AVLCmp_C", 2, "a, b",
    AVLCmp_C,
    "orb.c:AVLCmp_C" },

  { "AVLNewNode_C", 1, "t",
    AVLNewNode_C,
    "orb.c:AVLNewNode_C" },

  { "AVLFreeNode_C", 2, "tree, n",
    AVLFreeNode_C,
    "orb.c:AVLFreeNode_C" },

  { "AVLFind_C", 2, "tree, data",
    AVLFind_C,
    "orb.c:AVLFind_C" },

  { "AVLLookup_C", 2, "tree, data",
    AVLLookup_C,
    "orb.c:AVLLookup_C" },

  { "AVLIndex_C", 2, "tree, i",
    AVLIndex_C,
    "orb.c:AVLIndex_C" },

  { "AVLIndexLookup_C", 2, "tree, i",
    AVLIndexLookup_C,
    "orb.c:AVLIndexLookup_C" },

  { "AVLRebalance_C", 2, "tree, q",
    AVLRebalance_C,
    "orb.c:AVLRebalance_C" },

  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    ImportGVarFromLibrary( "AVLTreeType", &AVLTreeType );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary ( StructInitInfo *module )
{
    Int             i, gvar;
    Obj             tmp;

    /* init filters and functions */
    for ( i = 0;  GVarFuncs[i].name != 0;  i++ ) {
      gvar = GVarName(GVarFuncs[i].name);
      AssGVar(gvar,NewFunctionC( GVarFuncs[i].name, GVarFuncs[i].nargs,
                                 GVarFuncs[i].args, GVarFuncs[i].handler ) );
      MakeReadOnlyGVar(gvar);
    }

    tmp = NEW_PREC(0);
    gvar = GVarName("ORBC"); AssGVar( gvar, tmp ); MakeReadOnlyGVar(gvar);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
#ifdef ORBSTATIC
 /* type        = */ MODULE_STATIC,
#else
 /* type        = */ MODULE_DYNAMIC,
#endif
 /* name        = */ "orb",
 /* revision_c  = */ 0,
 /* revision_h  = */ 0,
 /* version     = */ 0,
 /* crc         = */ 0,
 /* initKernel  = */ InitKernel,
 /* initLibrary = */ InitLibrary,
 /* checkInit   = */ 0,
 /* preSave     = */ 0,
 /* postSave    = */ 0,
 /* postRestore = */ 0
};

#ifndef ORBSTATIC
StructInitInfo * Init__Dynamic ( void )
{
  module.revision_c = Revision_orb_c;
  return &module;
}
#endif

StructInitInfo * Init__orb ( void )
{
  module.revision_c = Revision_orb_c;
  return &module;
}

/*
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */



