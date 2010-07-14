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
Obj AVLTree;        /* Constructor function imported from the library */
Obj HTGrow;         /* Operation function imported from the library */

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
 *   ![7]     vals: stored values
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
  INTOBJ_INT( (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLmask) + n ))
#define AVLRight(t,i) INT_INTOBJ(ELM_PLIST(t,i+2))
#define SetAVLRight(t,i,n) SET_ELM_PLIST(t,i+2,INTOBJ_INT(n))
#define AVLRank(t,i) INT_INTOBJ(ELM_PLIST(t,i+3))
#define SetAVLRank(t,i,r) SET_ELM_PLIST(t,i+3,INTOBJ_INT(r))
#define AVLBalFactor(t,i) (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLmask)
#define SetAVLBalFactor(t,i,b) SET_ELM_PLIST(t,i+1, \
  INTOBJ_INT( (INT_INTOBJ(ELM_PLIST(t,i+1)) & AVLmask2) + b ))

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
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLNewNode(avltree)", 0L, 0L );
        return 0L;
    }
    return INTOBJ_INT(AVLNewNode(t));
}

static inline Obj AVLFreeNode( Obj t, Int n )
{
    Obj v,o;
    SET_ELM_PLIST(t,n,AVLFreeObj(t));
    SetAVLFree(t,n);
    n /= 4;
    v = AVLValues(t);
    if (v != Fail && ISB_LIST(v,n)) {
        o = ELM_PLIST(v,n);
        UNB_LIST(v,n);
        return o;
    }
    return True;
}

static Obj AVLFreeNode_C( Obj self, Obj t, Obj n)
{
    if (!IS_INTOBJ(n) ||
        TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLFreeNode(avltree,integer)", 0L, 0L );
        return 0L;
    }
    return AVLFreeNode(t,INT_INTOBJ(n));
}

Obj static inline AVLValue( Obj t, Int n )
{
    Obj vals = AVLValues(t);
    if (vals == Fail) return True;
    n /= 4;
    if (!ISB_LIST(vals,n)) return True;
    return ELM_LIST(vals,n);
}

void static inline SetAVLValue( Obj t, Int n, Obj v )
{
    Obj vals = AVLValues(t);
    n /= 4;
    if (vals == Fail || !IS_LIST(vals)) {
        vals = NEW_PLIST(T_PLIST, n);
        SetAVLValues(t,vals);
    }
    ASS_LIST(vals,n,v);
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
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLFind(avltree, object)", 0L, 0L );
        return 0L;
    }
    Int tmp = AVLFind(t,d);
    if (tmp == 0)
        return Fail;
    else
        return INTOBJ_INT(tmp);
}
            
static inline Int AVLFindIndex( Obj t, Obj d )
{
    Obj compare,c;
    Int p;
    Int offset;

    compare = AVL3Comp(t);
    p = AVLTop(t);
    offset = 0;
    while (p >= 8) {
        c = CALL_2ARGS(compare,d,AVLData(t,p));
        if (c == INTOBJ_INT(0))
            return offset + AVLRank(t,p);
        else if (INT_INTOBJ(c) < 0)   /* d < AVLData(t,p) */
            p = AVLLeft(t,p);
        else {                         /* d > AVLData(t,p) */
            offset += AVLRank(t,p);
            p = AVLRight(t,p);
        }
    }
    return 0;
}

static Obj AVLFindIndex_C( Obj self, Obj t, Obj d )
{
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLFindIndex(avltree, object)", 0L, 0L );
        return 0L;
    }
    Int tmp = AVLFindIndex(t,d);
    if (tmp == 0)
        return Fail;
    else
        return INTOBJ_INT(tmp);
}
            
static Obj AVLLookup_C( Obj self, Obj t, Obj d )
{
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLLookup(avltree, object)", 0L, 0L );
        return 0L;
    }
    Int p = AVLFind(t,d);
    if (p == 0) return Fail;
    return AVLValue(t,p);
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
    if (!IS_INTOBJ(i) || 
        TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLIndex(avltree, integer)", 0L, 0L );
        return 0L;
    }
    Int p = AVLIndex( t, INT_INTOBJ(i) );
    if (p == 0) 
        return Fail;
    else
        return AVLData(t,p);
}

static Obj AVLIndexFind_C( Obj self, Obj t, Obj i )
{
    if (!IS_INTOBJ(i) || 
        TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLIndexFind(avltree, integer)", 0L, 0L );
        return 0L;
    }
    Int p = AVLIndex( t, INT_INTOBJ(i) );
    if (p == 0) 
        return Fail;
    else
        return INTOBJ_INT(p);
}

static Obj AVLIndexLookup_C( Obj self, Obj t, Obj i )
{
    if (!IS_INTOBJ(i) || 
        TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLIndexLookup(avltree, integer)", 0L, 0L );
        return 0L;
    }
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
  if (AVLBalFactor(tree,q) == 2)   /* was: < 0 */
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
              SetAVLBalFactor(tree,q,2);     /* was: -1 */
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
                               /* new value of AVLRank(tree,l)! */
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
  *newroot = p;
}

Obj static AVLRebalance_C( Obj self, Obj tree, Obj q )
{
    Int newroot = 0;
    int shrink;
    Obj tmp;
    AVLRebalance( tree, INT_INTOBJ(q), &newroot, &shrink );
    tmp = NEW_PREC(2);
    AssPRec(tmp,RNamName("newroot"),INTOBJ_INT(newroot));
    AssPRec(tmp,RNamName("shorter"),shrink ? True : False);
    return tmp;
}

Obj static AVLAdd_C( Obj self, Obj tree, Obj data, Obj value )
{
/* Parameters: tree, data, value
    tree is an AVL tree
    data is a data structure defined by the user
    value is the value stored under the key data, if true, nothing is stored
   Tries to add the data as a node in tree. It is an error, if there is
   already a node which is "equal" to data with respect to the comparison
   function. Returns true if everything went well or fail, if an equal 
   object is already present. */

  Obj compare;
  Int p, new;
  /* here all steps are recorded: -1:left, +1:right */
  int path[64];   /* Trees will never be deeper than that! */
  Int nodes[64];
  int n;          /* The length of the list nodes */
  Int q;
  Int rankadds[64];
  int rankaddslen;   /* length of list rankadds */
  Int c;
  Int l;
  Int i;

  if (TNUM_OBJ(tree) != T_POSOBJ || TYPE_POSOBJ(tree) != AVLTreeType) {
      ErrorQuit( "Usage: AVLAdd(avltree, object, object)", 0L, 0L );
      return 0L;
  }

  compare = AVL3Comp(tree);
  p = AVLTop(tree);
  if (p == 0) {   /* A new, single node in the tree */
      new = AVLNewNode(tree);
      SetAVLLeft(tree,new,0);
      SetAVLRight(tree,new,0);
      SetAVLBalFactor(tree,new,0);
      SetAVLRank(tree,new,1);
      SetAVLData(tree,new,data);
      if (value != True)
          SetAVLValue(tree,new,value);
      SetAVLNodes(tree,1);
      SetAVLTop(tree,new);
      return True;
  }
  
  /* let's first find the right position in the tree:
     but: remember the last node on the way with bal. factor <> 0 and the path
          after this node
     and: remember the nodes where the Rank entry is incremented in case we
          find an "equal" element                                           */
  nodes[1] = p;   /* here we store all nodes on our way, nodes[i+1] is reached
                     from nodes[i] by walking one step path[i] */
  n = 1;          /* this is the length of "nodes" */
  q = 0;          /* this is the last node with bal. factor <> 0 */
                  /* index in "nodes" or 0 for no such node */
  rankaddslen = 0;  /* nothing done so far, list of Rank-modified nodes */
  do {
      /* do we have to remember this position? */
      if (AVLBalFactor(tree,p) != 0)
          q = n;       /* forget old last node with balance factor != 0 */
      
      /* now one step: */
      c = INT_INTOBJ(CALL_2ARGS(compare,data,AVLData(tree,p)));
      if (c == 0) {   /* we did not want this! */
          for (p = 1; p <= rankaddslen; p++) {
            SetAVLRank(tree,p,AVLRank(tree,rankadds[p]) - 1);
          }
          return Fail;    /* tree is unchanged */
      }
      
      l = p;     /* remember last position */
      if (c < 0) {    /* data < AVLData(tree,p) */
          SetAVLRank(tree,p,AVLRank(tree,p) + 1);
          rankadds[++rankaddslen] = p;
          p = AVLLeft(tree,p);
      } else {        /* data > AVLData(tree,p) */
          p = AVLRight(tree,p);
      }
      path[n] = c > 0 ? 1 : 2;   /* Internal representation! */
      nodes[++n] = p;
  } while (p != 0);
  /* now p is 0 and nodes[n-1] is the node where data must be attached
     the tree must be modified between nodes[q] and nodes[n-1] along path
     Ranks are already done */
  l = nodes[n-1];   /* for easier reference */
  
  /* a new node: */
  p = AVLNewNode(tree);
  SetAVLLeft(tree,p,0);
  SetAVLRight(tree,p,0);
  SetAVLBalFactor(tree,p,0);
  SetAVLRank(tree,p,1);
  SetAVLData(tree,p,data);
  if (value != True) {
      SetAVLValue(tree,p,value);
  }
  /* insert into tree: */
  if (c < 0) {    /* left */
      SetAVLLeft(tree,l,p);
  } else {
      SetAVLRight(tree,l,p);
  }
  SetAVLNodes(tree,AVLNodes(tree)+1);
  
  /* modify balance factors between q and l: */
  for (i = q+1;i <= n-1;i++) {
      SetAVLBalFactor(tree,nodes[i],path[i]);
  }
  
  /* is rebalancing at q necessary? */
  if (q == 0)     /* whole tree has grown one step */
      return True;
  if (AVLBalFactor(tree,nodes[q]) == 3-path[q]) {
      /* the subtree at q has gotten more balanced */
      SetAVLBalFactor(tree,nodes[q],0);
      return True;   /* Success! */
  }
  
  /* now at last we do have to rebalance at nodes[q] because the tree has
     gotten out of balance: */
  int shrink;   /* not used */
  AVLRebalance(tree,nodes[q],&p,&shrink);
  
  /* finishing touch: link new root of subtree (p) to t: */
  if (q == 1) {    /* q resp. r was First node */
      SetAVLTop(tree,p);
  } else if (path[q-1] == 2) {
      SetAVLLeft(tree,nodes[q-1],p);
  } else {
      SetAVLRight(tree,nodes[q-1],p);
  }
  
  return True;
}

Obj static AVLIndexAdd_C( Obj self, Obj tree, Obj data, Obj value, Obj ind )
{
/* Parameters: tree, data, value
    tree is an AVL tree
    data is a data structure defined by the user
    value is the value stored under the key data, if true, nothing is stored
    index is the index, where data should be inserted in tree 1 ist at
          first position, NumberOfNodes+1 after the last.
    Tries to add the data as a node in tree. Returns true if everything 
    went well or fail, if something went wrong,    */

  Int p, new;
  /* here all steps are recorded: -1:left, +1:right */
  int path[64];   /* Trees will never be deeper than that! */
  Int nodes[64];
  int n;          /* The length of the list nodes */
  Int q;
  Int c;
  Int l;
  Int index;
  Int i;
  Int offset;

  if (TNUM_OBJ(tree) != T_POSOBJ || TYPE_POSOBJ(tree) != AVLTreeType) {
      ErrorQuit( "Usage: AVLAdd(avltree, object, object)", 0L, 0L );
      return 0L;
  }

  index = INT_INTOBJ(ind);
  if (index < 1 || index > AVLNodes(tree)+1) return Fail;

  p = AVLTop(tree);
  if (p == 0) {   /* A new, single node in the tree */
      new = AVLNewNode(tree);
      SetAVLLeft(tree,new,0);
      SetAVLRight(tree,new,0);
      SetAVLBalFactor(tree,new,0);
      SetAVLRank(tree,new,1);
      SetAVLData(tree,new,data);
      if (value != True)
          SetAVLValue(tree,new,value);
      SetAVLNodes(tree,1);
      SetAVLTop(tree,new);
      return True;
  }
  
  /* let's first find the right position in the tree:
     but: remember the last node on the way with bal. factor <> 0 and the path
          after this node
     and: remember the nodes where the Rank entry is incremented in case we
          find an "equal" element                                           */
  nodes[1] = p;   /* here we store all nodes on our way, nodes[i+1] is reached
                     from nodes[i] by walking one step path[i] */
  n = 1;          /* this is the length of "nodes" */
  q = 0;          /* this is the last node with bal. factor <> 0 */
                  /* index in "nodes" or 0 for no such node */
  offset = 0;   /* number of nodes with smaller index than those in subtree */

  do {
      /* do we have to remember this position? */
      if (AVLBalFactor(tree,p) != 0)
          q = n;       /* forget old last node with balance factor != 0 */
      
      /* now one step: */
      if (index <= offset+AVLRank(tree,p))
          c = -1;
      else
          c = +1;
      
      l = p;     /* remember last position */
      if (c < 0) {    /* data < AVLData(tree,p) */
          SetAVLRank(tree,p,AVLRank(tree,p) + 1);
          p = AVLLeft(tree,p);
      } else {        /* data > AVLData(tree,p) */
          offset += AVLRank(tree,p);
          p = AVLRight(tree,p);
      }
      path[n] = c > 0 ? 1 : 2;   /* Internal representation! */
      nodes[++n] = p;
  } while (p != 0);
  /* now p is 0 and nodes[n-1] is the node where data must be attached
     the tree must be modified between nodes[q] and nodes[n-1] along path
     Ranks are already done */
  l = nodes[n-1];   /* for easier reference */
  
  /* a new node: */
  p = AVLNewNode(tree);
  SetAVLLeft(tree,p,0);
  SetAVLRight(tree,p,0);
  SetAVLBalFactor(tree,p,0);
  SetAVLRank(tree,p,1);
  SetAVLData(tree,p,data);
  if (value != True) {
      SetAVLValue(tree,p,value);
  }
  /* insert into tree: */
  if (c < 0) {    /* left */
      SetAVLLeft(tree,l,p);
  } else {
      SetAVLRight(tree,l,p);
  }
  SetAVLNodes(tree,AVLNodes(tree)+1);
  
  /* modify balance factors between q and l: */
  for (i = q+1;i <= n-1;i++) {
      SetAVLBalFactor(tree,nodes[i],path[i]);
  }
  
  /* is rebalancing at q necessary? */
  if (q == 0)     /* whole tree has grown one step */
      return True;
  if (AVLBalFactor(tree,nodes[q]) == 3-path[q]) {
      /* the subtree at q has gotten more balanced */
      SetAVLBalFactor(tree,nodes[q],0);
      return True;   /* Success! */
  }
  
  /* now at last we do have to rebalance at nodes[q] because the tree has
     gotten out of balance: */
  int shrink;   /* not used */
  AVLRebalance(tree,nodes[q],&p,&shrink);
  
  /* finishing touch: link new root of subtree (p) to t: */
  if (q == 1) {    /* q resp. r was First node */
      SetAVLTop(tree,p);
  } else if (path[q-1] == 2) {
      SetAVLLeft(tree,nodes[q-1],p);
  } else {
      SetAVLRight(tree,nodes[q-1],p);
  }
  
  return True;
}

static Obj AVLDelete_C( Obj self, Obj tree, Obj data)
  /* Parameters: tree, data
      tree is an AVL tree
      data is a data structure defined by the user
     Tries to find data as a node in the tree. If found, this node is deleted
     and the tree rebalanced. It is an error, of the node is not found.
     Returns fail in this case, and true normally.       */
{
  Obj compare;
  Int p;
  int path[64];   /* Trees will never be deeper than that! */
  Int nodes[64];
  int n;
  int c;
  int m,i;
  Int r,l;
  Int ranksubs[64];
  int ranksubslen;    /* length of list randsubs */
  Obj old;
  
  if (TNUM_OBJ(tree) != T_POSOBJ || TYPE_POSOBJ(tree) != AVLTreeType) {
      ErrorQuit( "Usage: AVLDelete(avltree, object)", 0L, 0L );
      return Fail;
  }

  compare = AVL3Comp(tree);
  p = AVLTop(tree);
  if (p == 0)     /* Nothing to delete or find */
      return Fail;

  if (AVLNodes(tree) == 1) {
      if (INT_INTOBJ(CALL_2ARGS(compare,data,AVLData(tree,p))) == 0) {
          SetAVLNodes(tree,0);
          SetAVLTop(tree,0);
          return AVLFreeNode(tree,p);
      } else {
          return Fail;
      }
  }
  
  /* let's first find the right position in the tree:
     and: remember the nodes where the Rank entry is decremented in case we
          find an "equal" element */
  nodes[1] = p;   /* here we store all nodes on our way, nodes[i+1] is reached
                     from nodes[i] by walking one step path[i] */
  n = 1;      
  ranksubslen = 0; /* nothing done so far, list of Rank-modified nodes */
  
  do {
      
      /* what is the next step? */
      c = INT_INTOBJ(CALL_2ARGS(compare,data,AVLData(tree,p)));
      
      if (c != 0) {    /* only if data not found! */
          if (c < 0) {    /* data < AVLData(tree,p) */
              SetAVLRank(tree,p,AVLRank(tree,p) - 1);
              ranksubs[++ranksubslen] = p;
              p = AVLLeft(tree,p);
          } else {        /* data > AVLData(tree,p) */
              p = AVLRight(tree,p);
          }
          path[n] = c > 0 ? 1 : 2;   /* Internal representation! */
          nodes[++n] = p;
      }
      
      if (p == 0) {
          /* error, we did not find data */
          for (i = 1; i <= ranksubslen; i++) {
              SetAVLRank(tree,ranksubs[i],AVLRank(tree,ranksubs[i]) + 1);
          }
          return Fail;
      }
      
  } while (c != 0);   /* until we find the right node */
  /* now data is equal to AVLData(tree,p) so this node p must be removed.
     the tree must be modified between AVLTop(tree) and nodes[n] along path
     Ranks are already done up there. */
  
  /* now we have to search a neighbour, we modify "nodes" and "path" but 
   * not n! */
  m = n;
  if (AVLBalFactor(tree,p) == 2) {   /* (was: < 0) search to the left */
      l = AVLLeft(tree,p);   /* must be a node! */
      SetAVLRank(tree,p,AVLRank(tree,p) - 1);   
      /* we will delete in left subtree! */
      path[m] = 2;   /* was: -1 */
      nodes[++m] = l;
      while (AVLRight(tree,l) != 0) {
          l = AVLRight(tree,l);
          path[m] = 1;
          nodes[++m] = l;
      }
      c = -1;       /* we got predecessor */
  } else if (AVLBalFactor(tree,p) > 0) {  /* search to the right */
      l = AVLRight(tree,p);      /* must be a node! */
      path[m] = 1;
      nodes[++m] = l;
      while (AVLLeft(tree,l) != 0) {
          SetAVLRank(tree,l,AVLRank(tree,l) - 1); 
          /* we will delete in left subtree! */
          l = AVLLeft(tree,l);
          path[m] = 2;  /* was: -1 */
          nodes[++m] = l;
      }
      c = 1;        /* we got successor */
  } else {   /* equal depths */
      if (AVLLeft(tree,p) != 0) {
          l = AVLLeft(tree,p);
          SetAVLRank(tree,p,AVLRank(tree,p) - 1);
          path[m] = 2;   /* was: -1 */
          nodes[++m] = l;
          while (AVLRight(tree,l) != 0) {
              l = AVLRight(tree,l);
              path[m] = 1;
              nodes[++m] = l;
          }
          c = -1;     /* we got predecessor */
      } else {        /* we got an end node */
          l = p;
          c = 0;      
      }
  }
  /* l points now to a neighbour, in case c = -1 to the predecessor, in case
     c = 1 to the successor, or to p itself in case c = 0
     "nodes" and "path" is updated, but n could be < m */
  
  /* Copy Data from l up to p: order is NOT modified */
  SetAVLData(tree,p,AVLData(tree,l));   
     /* works for m = n, i.e. if p is end node */
  
  /* Delete node at l = nodes[m] by modifying nodes[m-1]:
     Note: nodes[m] has maximal one subtree! */
  if (c <= 0)
      r = AVLLeft(tree,l);
  else    /*  c > 0 */
      r = AVLRight(tree,l);
  
  if (path[m-1] == 2)    /* was: < 0 */
      SetAVLLeft(tree,nodes[m-1],r);
  else
      SetAVLRight(tree,nodes[m-1],r);
  SetAVLNodes(tree,AVLNodes(tree)-1);
  old = AVLFreeNode(tree,l);
  
  /* modify balance factors:
     the subtree nodes[m-1] has become shorter at its left (resp. right)
     subtree, if path[m-1]=-1 (resp. +1). We have to react according to
     the BalFactor at this node and then up the tree, if the whole subtree
     has shrunk:
     (we decrement m and work until the corresponding subtree has not shrunk) */
  m--;   /* start work HERE */
  while (m >= 1) {
      if (AVLBalFactor(tree,nodes[m]) == 0) {
          SetAVLBalFactor(tree,nodes[m],3-path[m]); /* we made path[m] shorter*/
          return old;
      } else if (AVLBalFactor(tree,nodes[m]) == path[m]) {
          SetAVLBalFactor(tree,nodes[m],0);     /* we made path[m] shorter */
      } else {   /* tree is out of balance */
          int shorter;
          AVLRebalance(tree,nodes[m],&p,&shorter);
          if (m == 1) {
              SetAVLTop(tree,p);
              return old;               /* everything is done */
          } else if (path[m-1] == 2)   /* was: = -1 */
              SetAVLLeft(tree,nodes[m-1],p);
          else
              SetAVLRight(tree,nodes[m-1],p);
          if (!shorter) return old;    /* nothing happens further up */
      }
      m--;
  }
  return old;
}
 
Obj static AVLIndexDelete_C( Obj self, Obj tree, Obj index)
  /* Parameters: tree, index
      tree is an AVL tree
      index is the index of the element to be deleted, must be between 1 and
          AVLNodes(tree) inclusively
     returns fail if index is out of range, otherwise the deleted key;  */

{
  Int p;
  int path[64];   /* Trees will never be deeper than that! */
  Int nodes[64];
  int n;
  int c;
  Int offset;
  int m;
  Int r,l;
  Int ind;
  Obj x;

  if (TNUM_OBJ(tree) != T_POSOBJ || TYPE_POSOBJ(tree) != AVLTreeType) {
      ErrorQuit( "Usage: AVLIndexDelete(avltree, index)", 0L, 0L );
      return 0L;
  }
  if (!IS_INTOBJ(index)) {
      ErrorQuit( "Usage2: AVLIndexDelete(avltree, index)", 0L, 0L );
      return 0L;
  }

  p = AVLTop(tree);
  if (p == 0)     /* Nothing to delete or find */
      return Fail;

  ind = INT_INTOBJ(index);
  if (ind < 1 || ind > AVLNodes(tree))   /* out of range */
      return Fail;

  if (AVLNodes(tree) == 1) {
      x = AVLData(tree,p);
      SetAVLNodes(tree,0);
      SetAVLTop(tree,0);
      AVLFreeNode(tree,p);
      return x;
  }
  
  /* let's first find the right position in the tree:
     and: remember the nodes where the Rank entry is decremented in case we
          find an "equal" element */
  nodes[1] = p;   /* here we store all nodes on our way, nodes[i+1] is reached
                     from nodes[i] by walking one step path[i] */
  n = 1;      
  offset = 0;     /* number of "smaller" nodes than subtree in whole tree */
  
  do {
      
      /* what is the next step? */
      if (ind == offset + AVLRank(tree,p)) {
          c = 0;   /* we found our node! */
          x = AVLData(tree,p);
      } else if (ind < offset + AVLRank(tree,p))
          c = -1;  /* we have to go left */
      else
          c = 1;   /* we have to go right */
      
      if (c != 0) {    /* only if data not found! */
          if (c < 0) {    /* data < AVLData(tree,p) */
              SetAVLRank(tree,p,AVLRank(tree,p) - 1);
              p = AVLLeft(tree,p);
          } else {        /* data > AVLData(tree,p) */
              offset += AVLRank(tree,p);
              p = AVLRight(tree,p);
          }
          path[n] = c > 0 ? 1 : 2;   /* Internal representation! */
          nodes[++n] = p;
      }
      
  } while (c != 0);   /* until we find the right node */
  /* now index is right, so this node p must be removed.
     the tree must be modified between AVLTop(tree) and nodes[n] along path
     Ranks are already done up there. */
  
  /* now we have to search a neighbour, we modify "nodes" and "path" but 
   * not n! */
  m = n;
  if (AVLBalFactor(tree,p) == 2) {   /* (was: < 0) search to the left */
      l = AVLLeft(tree,p);   /* must be a node! */
      SetAVLRank(tree,p,AVLRank(tree,p) - 1);   
      /* we will delete in left subtree! */
      path[m] = 2;   /* was: -1 */
      nodes[++m] = l;
      while (AVLRight(tree,l) != 0) {
          l = AVLRight(tree,l);
          path[m] = 1;
          nodes[++m] = l;
      }
      c = -1;       /* we got predecessor */
  } else if (AVLBalFactor(tree,p) > 0) {  /* search to the right */
      l = AVLRight(tree,p);      /* must be a node! */
      path[m] = 1;
      nodes[++m] = l;
      while (AVLLeft(tree,l) != 0) {
          SetAVLRank(tree,l,AVLRank(tree,l) - 1); 
          /* we will delete in left subtree! */
          l = AVLLeft(tree,l);
          path[m] = 2;  /* was: -1 */
          nodes[++m] = l;
      }
      c = 1;        /* we got successor */
  } else {   /* equal depths */
      if (AVLLeft(tree,p) != 0) {
          l = AVLLeft(tree,p);
          SetAVLRank(tree,p,AVLRank(tree,p) - 1);
          path[m] = 2;   /* was: -1 */
          nodes[++m] = l;
          while (AVLRight(tree,l) != 0) {
              l = AVLRight(tree,l);
              path[m] = 1;
              nodes[++m] = l;
          }
          c = -1;     /* we got predecessor */
      } else {        /* we got an end node */
          l = p;
          c = 0;      
      }
  }
  /* l points now to a neighbour, in case c = -1 to the predecessor, in case
     c = 1 to the successor, or to p itself in case c = 0
     "nodes" and "path" is updated, but n could be < m */
  
  /* Copy Data from l up to p: order is NOT modified */
  SetAVLData(tree,p,AVLData(tree,l));   
     /* works for m = n, i.e. if p is end node */
  
  /* Delete node at l = nodes[m] by modifying nodes[m-1]:
     Note: nodes[m] has maximal one subtree! */
  if (c <= 0)
      r = AVLLeft(tree,l);
  else    /*  c > 0 */
      r = AVLRight(tree,l);
  
  if (path[m-1] == 2)    /* was: < 0 */
      SetAVLLeft(tree,nodes[m-1],r);
  else
      SetAVLRight(tree,nodes[m-1],r);
  SetAVLNodes(tree,AVLNodes(tree)-1);
  AVLFreeNode(tree,l);
  
  /* modify balance factors:
     the subtree nodes[m-1] has become shorter at its left (resp. right)
     subtree, if path[m-1]=-1 (resp. +1). We have to react according to
     the BalFactor at this node and then up the tree, if the whole subtree
     has shrunk:
     (we decrement m and work until the corresponding subtree has not shrunk) */
  m--;   /* start work HERE */
  while (m >= 1) {
      if (AVLBalFactor(tree,nodes[m]) == 0) {
          SetAVLBalFactor(tree,nodes[m],3-path[m]); /* we made path[m] shorter*/
          return x;
      } else if (AVLBalFactor(tree,nodes[m]) == path[m]) {
          SetAVLBalFactor(tree,nodes[m],0);     /* we made path[m] shorter */
      } else {   /* tree is out of balance */
          int shorter;
          AVLRebalance(tree,nodes[m],&p,&shorter);
          if (m == 1) {
              SetAVLTop(tree,p);
              return x;               /* everything is done */
          } else if (path[m-1] == 2)   /* was: = -1 */
              SetAVLLeft(tree,nodes[m-1],p);
          else
              SetAVLRight(tree,nodes[m-1],p);
          if (!shorter) return x;    /* nothing happens further up */
      }
      m--;
  }
  return x;
}
 
static Int RNam_accesses = 0;
static Int RNam_collisions = 0;
static Int RNam_hfd = 0;
static Int RNam_hf = 0;
static Int RNam_els = 0;
static Int RNam_vals = 0;
static Int RNam_nr = 0;
static Int RNam_cmpfunc = 0;
static Int RNam_allocsize = 0;
static Int RNam_cangrow = 0;
static Int RNam_len = 0;

static inline void initRNams(void)
{
    /* Find RNams if not already done: */
    if (!RNam_accesses) {
        RNam_accesses = RNamName("accesses");
        RNam_collisions = RNamName("collisions");
        RNam_hfd = RNamName("hfd");
        RNam_hf = RNamName("hf");
        RNam_els = RNamName("els");
        RNam_vals = RNamName("vals");
        RNam_nr = RNamName("nr");
        RNam_cmpfunc = RNamName("cmpfunc");
        RNam_allocsize = RNamName("allocsize");
        RNam_cangrow = RNamName("cangrow");
        RNam_len = RNamName("len");
    }
}

static Obj HTAdd_TreeHash_C(Obj self, Obj ht, Obj x, Obj v)
{
    Obj els;
    Obj vals;
    Obj tmp;
    Obj hfd;
    Int h;
    Obj t;
    Obj r;

    /* Find RNams if not already done: */
    initRNams();

    /* Increment accesses entry: */
    tmp = ElmPRec(ht,RNam_accesses);
    tmp = INTOBJ_INT(INT_INTOBJ(tmp)+1);
    AssPRec(ht,RNam_accesses,tmp);

    if (ElmPRec(ht,RNam_cangrow) == True &&
        INT_INTOBJ(ElmPRec(ht,RNam_nr))/10 > INT_INTOBJ(ElmPRec(ht,RNam_len)))
    {
        CALL_2ARGS(HTGrow,ht,x);
    }

    /* Compute hash value: */
    hfd = ElmPRec(ht,RNam_hfd);
    tmp = ElmPRec(ht,RNam_hf);
    h = INT_INTOBJ(CALL_2ARGS(tmp,x,hfd));

    /* Lookup slot: */
    els = ElmPRec(ht,RNam_els);
    vals = ElmPRec(ht,RNam_vals);
    tmp = ELM_PLIST(els,h);    /* Note that hash values are always within
                                  the boundaries of this list */
    if (tmp == 0L) { /* Unbound entry! */
        SET_ELM_PLIST(els,h,x);
        CHANGED_BAG(els);
        if (v != True) ASS_LIST(vals,h,v);
        AssPRec(ht,RNam_nr,INTOBJ_INT(INT_INTOBJ(ElmPRec(ht,RNam_nr))+1));
        return INTOBJ_INT(h);
    }

    /* Count collision: */
    AssPRec(ht,RNam_collisions,
            INTOBJ_INT(INT_INTOBJ(ElmPRec(ht,RNam_collisions))+1));
    
    /* Now check whether it is an AVLTree or not: */
    if (TNUM_OBJ(tmp) != T_POSOBJ || TYPE_POSOBJ(tmp) != AVLTreeType) {
        r = NEW_PREC(2);   /* This might trigger a garbage collection */
        AssPRec(r,RNam_cmpfunc,ElmPRec(ht,RNam_cmpfunc));
        AssPRec(r,RNam_allocsize,INTOBJ_INT(3));
        t = CALL_1ARGS(AVLTree,r);
        if (LEN_PLIST(vals) >= h && ELM_PLIST(vals,h) != 0L) {
            AVLAdd_C(self,t,tmp,ELM_PLIST(vals,h));
            UNB_LIST(vals,h);
        } else {
            AVLAdd_C(self,t,tmp,True);
        }
        SET_ELM_PLIST(els,h,t);
        CHANGED_BAG(els);
    } else t = tmp;

    /* Finally add value into tree: */
    if (v != True) {
        r = AVLAdd_C(self,t,x,v);
    } else {
        r = AVLAdd_C(self,t,x,True);
    }

    if (r != Fail) {
        AssPRec(ht,RNam_nr,INTOBJ_INT(INT_INTOBJ(ElmPRec(ht,RNam_nr))+1));
        return INTOBJ_INT(h);
    } else
        return Fail;
}

static Obj HTValue_TreeHash_C(Obj self, Obj ht, Obj x)
{
    Obj els;
    Obj vals;
    Obj hfd;
    Int h;
    Obj t;

    /* Find RNams if not already done: */
    initRNams();

    /* Increment accesses entry: */
    t = ElmPRec(ht,RNam_accesses);
    t = INTOBJ_INT(INT_INTOBJ(t)+1);
    AssPRec(ht,RNam_accesses,t);

    /* Compute hash value: */
    hfd = ElmPRec(ht,RNam_hfd);
    t = ElmPRec(ht,RNam_hf);
    h = INT_INTOBJ(CALL_2ARGS(t,x,hfd));

    /* Lookup slot: */
    els = ElmPRec(ht,RNam_els);
    vals = ElmPRec(ht,RNam_vals);
    t = ELM_PLIST(els,h);    /* Note that hash values are always within
                                  the boundaries of this list */
    if (t == 0L)  /* Unbound entry! */
        return Fail;

    /* Now check whether it is an AVLTree or not: */
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        if (CALL_2ARGS(ElmPRec(ht,RNam_cmpfunc),x,t) == INTOBJ_INT(0)) {
            if (LEN_PLIST(vals) >= h && ELM_PLIST(vals,h) != 0L) 
                return ELM_PLIST(vals,h);
            else
                return True;
        }
        return Fail;
    }

    h = AVLFind(t,x);
    if (h == 0) return Fail;
    return AVLValue(t,h);
}

static Obj HTDelete_TreeHash_C(Obj self, Obj ht, Obj x)
{
    Obj els;
    Obj vals;
    Obj hfd;
    Int h;
    Obj t;
    Obj v;

    /* Find RNams if not already done: */
    initRNams();

    /* Compute hash value: */
    hfd = ElmPRec(ht,RNam_hfd);
    t = ElmPRec(ht,RNam_hf);
    h = INT_INTOBJ(CALL_2ARGS(t,x,hfd));

    /* Lookup slot: */
    els = ElmPRec(ht,RNam_els);
    vals = ElmPRec(ht,RNam_vals);
    t = ELM_PLIST(els,h);    /* Note that hash values are always within
                                  the boundaries of this list */
    if (t == 0L)  /* Unbound entry! */
        return Fail;

    /* Now check whether it is an AVLTree or not: */
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        if (CALL_2ARGS(ElmPRec(ht,RNam_cmpfunc),x,t) == INTOBJ_INT(0)) {
            if (LEN_PLIST(vals) >= h && ELM_PLIST(vals,h) != 0L) {
                v = ELM_PLIST(vals,h);
                UNB_LIST(vals,h);
            } else v = True;
            SET_ELM_PLIST(els,h,0L);
            AssPRec(ht,RNam_nr,INTOBJ_INT(INT_INTOBJ(ElmPRec(ht,RNam_nr))-1));
            return v;
        }
        return Fail;
    }

    v = AVLDelete_C(self,t,x);
    if (v != Fail)
        AssPRec(ht,RNam_nr,INTOBJ_INT(INT_INTOBJ(ElmPRec(ht,RNam_nr))-1));

    return v;
}

static Obj HTUpdate_TreeHash_C(Obj self, Obj ht, Obj x, Obj v)
{
    Obj els;
    Obj vals;
    Obj hfd;
    Int h;
    Obj t;
    Obj old;

    /* Find RNams if not already done: */
    initRNams();

    /* Compute hash value: */
    hfd = ElmPRec(ht,RNam_hfd);
    t = ElmPRec(ht,RNam_hf);
    h = INT_INTOBJ(CALL_2ARGS(t,x,hfd));

    /* Lookup slot: */
    els = ElmPRec(ht,RNam_els);
    vals = ElmPRec(ht,RNam_vals);
    t = ELM_PLIST(els,h);    /* Note that hash values are always within
                                  the boundaries of this list */
    if (t == 0L)  /* Unbound entry! */
        return Fail;

    /* Now check whether it is an AVLTree or not: */
    if (TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        if (CALL_2ARGS(ElmPRec(ht,RNam_cmpfunc),x,t) == INTOBJ_INT(0)) {
            if (LEN_PLIST(vals) >= h && ELM_PLIST(vals,h) != 0L) {
                old = ELM_PLIST(vals,h);
                SET_ELM_PLIST(vals,h,v);
                CHANGED_BAG(vals);
                return old;
            } else return True;
        }
        return Fail;
    }

    h = AVLFind(t,x);
    if (h == 0) return Fail;
    old = AVLValue(t,h);
    SetAVLValue(t,h,v);
    return old;
}

static Obj GenericHashFunc_C(Obj self, Obj x, Obj data)
{
     Int mult = INT_INTOBJ(ELM_PLIST(data,1));
     UChar *p = (UChar *) ADDR_OBJ(x) + INT_INTOBJ(ELM_PLIST(data,2));
     Int len = INT_INTOBJ(ELM_PLIST(data,3));
     UInt mod = INT_INTOBJ(ELM_PLIST(data,4));
     UInt n = 0;
     Int i;
     for (i = 0;i < len;i++) n = ((n*mult)+(UInt)(*p++));
     return INTOBJ_INT((n % mod) + 1);
}

/* For the time being we copy the Jenkins Hash stuff here: */

/******************************************************************************
 *
 *    jhash.h     The Bob Jenkins Hash Function
 *
 *    File      : $RCSfile: jhash.h,v $
 *    Author    : Henrik Bäärnhielm 
 *    Dev start : 2006-07-16 
 *
 *    Version   : $Revision: 1.7 $
 *    Date      : $Date: 2010/02/23 15:12:39 $
 *    Last edit : $Author: gap $
 *
 *    @(#)$Id: jhash.h,v 1.7 2010/02/23 15:12:39 gap Exp $
 *
 *    Definitions for Jenkins Hash
 *
 *****************************************************************************/

#include <sys/types.h>

#ifdef linux
# include <endian.h>
#endif

#if !defined(__CYGWIN__)
typedef u_int32_t uint32_t;
typedef u_int16_t uint16_t;
typedef u_int8_t uint8_t;
#endif


/******************************************************************************
 *
 *    jhash.c     The Bob Jenkins Hash Function
 *
 *    File      : $RCSfile: jhash.c,v $
 *    Author    : Bob Jenkins (cosmetic changes by Henrik Bäärnhielm) 
 *    Dev start : 2006-07-16 
 *
 *    Version   : $Revision: 1.2 $
 *    Date      : $Date: 2006/09/01 16:46:00 $
 *    Last edit : $Author: sal $
 *
 *    @(#)$Id: jhash.c,v 1.2 2006/09/01 16:46:00 sal Exp $
 *
 *    According to Prof. Viggo Kann at the CS department, Royal Institute of 
 *    Technology, Stockholm, Sweden, this is the best hash function known to
 *    humanity. It is fast and results in extremely few collisions. 
 *    Bob Jenkins original documentation is left in the code. 
 *    The file was fetched from http://burtleburtle.net/bob/c/lookup3.c
 *
 *****************************************************************************/

/*
-------------------------------------------------------------------------------
lookup3.c, by Bob Jenkins, May 2006, Public Domain.

These are functions for producing 32-bit hashes for hash table lookup.
hashword(), hashlittle(), hashbig(), mix(), and final() are externally 
useful functions.  Routines to test the hash are included if SELF_TEST 
is defined.  You can use this free for any purpose.  It has no warranty.

You probably want to use hashlittle().  hashlittle() and hashbig()
hash byte arrays.  hashlittle() is is faster than hashbig() on
little-endian machines.  Intel and AMD are little-endian machines.

If you want to find a hash of, say, exactly 7 integers, do
  a = i1;  b = i2;  c = i3;
  mix(a,b,c);
  a += i4; b += i5; c += i6;
  mix(a,b,c);
  a += i7;
  final(a,b,c);
then use c as the hash value.  If you have a variable length array of
4-byte integers to hash, use hashword().  If you have a byte array (like
a character string), use hashlittle().  If you have several byte arrays, or
a mix of things, see the comments above hashlittle().
-------------------------------------------------------------------------------
*/
//#define SELF_TEST 1

/*
 * My best guess at if you are big-endian or little-endian.  This may
 * need adjustment.
 */
#if (defined(__BYTE_ORDER) && defined(__LITTLE_ENDIAN) && \
     __BYTE_ORDER == __LITTLE_ENDIAN) || \
    (defined(i386) || defined(__i386__) || defined(__i486__) || \
     defined(__i586__) || defined(__i686__) || defined(vax) || defined(MIPSEL))
# define HASH_LITTLE_ENDIAN 1
# define HASH_BIG_ENDIAN 0
#elif (defined(__BYTE_ORDER) && defined(__BIG_ENDIAN) && \
       __BYTE_ORDER == __BIG_ENDIAN) || \
      (defined(sparc) || defined(POWERPC) || defined(mc68000) || defined(sel))
# define HASH_LITTLE_ENDIAN 0
# define HASH_BIG_ENDIAN 1
#else
# define HASH_LITTLE_ENDIAN 0
# define HASH_BIG_ENDIAN 0
#endif

#define hashsize(n) ((uint32_t)1<<(n))
#define hashmask(n) (hashsize(n)-1)
#define rot(x,k) (((x)<<(k)) ^ ((x)>>(32-(k))))

/*
-------------------------------------------------------------------------------
mix -- mix 3 32-bit values reversibly.

This is reversible, so any information in (a,b,c) before mix() is
still in (a,b,c) after mix().

If four pairs of (a,b,c) inputs are run through mix(), or through
mix() in reverse, there are at least 32 bits of the output that
are sometimes the same for one pair and different for another pair.
This was tested for:
* pairs that differed by one bit, by two bits, in any combination
  of top bits of (a,b,c), or in any combination of bottom bits of
  (a,b,c).
* "differ" is defined as +, -, ^, or ~^.  For + and -, I transformed
  the output delta to a Gray code (a^(a>>1)) so a string of 1's (as
  is commonly produced by subtraction) look like a single 1-bit
  difference.
* the base values were pseudorandom, all zero but one bit set, or 
  all zero plus a counter that starts at zero.

Some k values for my "a-=c; a^=rot(c,k); c+=b;" arrangement that
satisfy this are
    4  6  8 16 19  4
    9 15  3 18 27 15
   14  9  3  7 17  3
Well, "9 15 3 18 27 15" didn't quite get 32 bits diffing
for "differ" defined as + with a one-bit base and a two-bit delta.  I
used http://burtleburtle.net/bob/hash/avalanche.html to choose 
the operations, constants, and arrangements of the variables.

This does not achieve avalanche.  There are input bits of (a,b,c)
that fail to affect some output bits of (a,b,c), especially of a.  The
most thoroughly mixed value is c, but it doesn't really even achieve
avalanche in c.

This allows some parallelism.  Read-after-writes are good at doubling
the number of bits affected, so the goal of mixing pulls in the opposite
direction as the goal of parallelism.  I did what I could.  Rotates
seem to cost as much as shifts on every machine I could lay my hands
on, and rotates are much kinder to the top and bottom bits, so I used
rotates.
-------------------------------------------------------------------------------
*/
#define mix(a,b,c) \
{ \
  a -= c;  a ^= rot(c, 4);  c += b; \
  b -= a;  b ^= rot(a, 6);  a += c; \
  c -= b;  c ^= rot(b, 8);  b += a; \
  a -= c;  a ^= rot(c,16);  c += b; \
  b -= a;  b ^= rot(a,19);  a += c; \
  c -= b;  c ^= rot(b, 4);  b += a; \
}

/*
-------------------------------------------------------------------------------
final -- final mixing of 3 32-bit values (a,b,c) into c

Pairs of (a,b,c) values differing in only a few bits will usually
produce values of c that look totally different.  This was tested for
* pairs that differed by one bit, by two bits, in any combination
  of top bits of (a,b,c), or in any combination of bottom bits of
  (a,b,c).
* "differ" is defined as +, -, ^, or ~^.  For + and -, I transformed
  the output delta to a Gray code (a^(a>>1)) so a string of 1's (as
  is commonly produced by subtraction) look like a single 1-bit
  difference.
* the base values were pseudorandom, all zero but one bit set, or 
  all zero plus a counter that starts at zero.

These constants passed:
 14 11 25 16 4 14 24
 12 14 25 16 4 14 24
and these came close:
  4  8 15 26 3 22 24
 10  8 15 26 3 22 24
 11  8 15 26 3 22 24
-------------------------------------------------------------------------------
*/
#define final(a,b,c) \
{ \
  c ^= b; c -= rot(b,14); \
  a ^= c; a -= rot(c,11); \
  b ^= a; b -= rot(a,25); \
  c ^= b; c -= rot(b,16); \
  a ^= c; a -= rot(c,4);  \
  b ^= a; b -= rot(a,14); \
  c ^= b; c -= rot(b,24); \
}

/*
--------------------------------------------------------------------
 This works on all machines.  To be useful, it requires
 -- that the key be an array of uint32_t's, and
 -- that all your machines have the same endianness, and
 -- that the length be the number of uint32_t's in the key

 The function hashword() is identical to hashlittle() on little-endian
 machines, and identical to hashbig() on big-endian machines,
 except that the length has to be measured in uint32_ts rather than in
 bytes.  hashlittle() is more complicated than hashword() only because
 hashlittle() has to dance around fitting the key bytes into registers.
--------------------------------------------------------------------
*/
uint32_t hashword(register uint32_t *k, 
						register size_t length, 
						register uint32_t initval)
	/* the key, an array of uint32_t values */
	/* the length of the key, in uint32_ts */
	/* the previous hash, or an arbitrary value */
{
  register uint32_t a,b,c;

  /* Set up the internal state */
  a = b = c = 0xdeadbeef + (((uint32_t)length)<<2) + initval;

  /*------------------------------------------------- handle most of the key */
  while (length > 3)
  {
    a += k[0];
    b += k[1];
    c += k[2];
    mix(a,b,c);
    length -= 3;
    k += 3;
  }

  /*------------------------------------------- handle the last 3 uint32_t's */
  switch(length)                     /* all the case statements fall through */
  { 
  case 3 : c+=k[2];
  case 2 : b+=k[1];
  case 1 : a+=k[0];
    final(a,b,c);
  case 0:     /* case 0: nothing left to add */
    break;
  }
  /*------------------------------------------------------ report the result */
  return c;
}


/*
-------------------------------------------------------------------------------
hashlittle() -- hash a variable-length key into a 32-bit value
  k       : the key (the unaligned variable-length array of bytes)
  length  : the length of the key, counting by bytes
  initval : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Two keys differing by one or two bits will have
totally different hash values.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (uint8_t **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hashlittle( k[i], len[i], h);

By Bob Jenkins, 2006.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.  It's free.

Use for hash table lookup, or anything where one collision in 2^^32 is
acceptable.  Do NOT use for cryptographic purposes.
-------------------------------------------------------------------------------
*/

uint32_t hashlittle( register void *key, register size_t length, 
							register uint32_t initval)
{
  register uint32_t a,b,c;

  /* Set up the internal state */
  a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;

  if (HASH_LITTLE_ENDIAN && !((((uint8_t *)key)-(uint8_t *)0) & 0x3)) {
    register uint32_t *k = key;                /* read 32-bit chunks */

    /*------ all but last block: aligned reads and affect 32 bits of (a,b,c) */
    while (length > 12)
    {
      a += k[0];
      b += k[1];
      c += k[2];
      mix(a,b,c);
      length -= 12;
      k += 3;
    }

    /*----------------------------- handle the last (probably partial) block */
    switch(length)
    {
    case 12: c+=k[2]; b+=k[1]; a+=k[0]; break;
    case 11: c+=k[2]&0xffffff; b+=k[1]; a+=k[0]; break;
    case 10: c+=k[2]&0xffff; b+=k[1]; a+=k[0]; break;
    case 9 : c+=k[2]&0xff; b+=k[1]; a+=k[0]; break;
    case 8 : b+=k[1]; a+=k[0]; break;
    case 7 : b+=k[1]&0xffffff; a+=k[0]; break;
    case 6 : b+=k[1]&0xffff; a+=k[0]; break;
    case 5 : b+=k[1]&0xff; a+=k[0]; break;
    case 4 : a+=k[0]; break;
    case 3 : a+=k[0]&0xffffff; break;
    case 2 : a+=k[0]&0xffff; break;
    case 1 : a+=k[0]&0xff; break;
    case 0 : return c;              /* zero length strings require no mixing */
    }

  } else if (HASH_LITTLE_ENDIAN && !((((uint8_t *)key)-(uint8_t *)0) & 0x1)) {
    register uint16_t *k = key;                      /* read 16-bit chunks */

    /*--------------- all but last block: aligned reads and different mixing */
    while (length > 12)
    {
      a += k[0] + (((uint32_t)k[1])<<16);
      b += k[2] + (((uint32_t)k[3])<<16);
      c += k[4] + (((uint32_t)k[5])<<16);
      mix(a,b,c);
      length -= 12;
      k += 6;
    }

    /*----------------------------- handle the last (probably partial) block */
    switch(length)
    {
    case 12: c+=k[4]+(((uint32_t)k[5])<<16);
             b+=k[2]+(((uint32_t)k[3])<<16);
             a+=k[0]+(((uint32_t)k[1])<<16);
             break;
    case 11: c+=((uint32_t)(k[5]&0xff))<<16;/* fall through */
    case 10: c+=k[4];
             b+=k[2]+(((uint32_t)k[3])<<16);
             a+=k[0]+(((uint32_t)k[1])<<16);
             break;
    case 9 : c+=k[4]&0xff;                /* fall through */
    case 8 : b+=k[2]+(((uint32_t)k[3])<<16);
             a+=k[0]+(((uint32_t)k[1])<<16);
             break;
    case 7 : b+=((uint32_t)(k[3]&0xff))<<16;/* fall through */
    case 6 : b+=k[2];
             a+=k[0]+(((uint32_t)k[1])<<16);
             break;
    case 5 : b+=k[2]&0xff;                /* fall through */
    case 4 : a+=k[0]+(((uint32_t)k[1])<<16);
             break;
    case 3 : a+=((uint32_t)(k[1]&0xff))<<16;/* fall through */
    case 2 : a+=k[0];
             break;
    case 1 : a+=k[0]&0xff;
             break;
    case 0 : return c;                     /* zero length requires no mixing */
    }

  } else {                        /* need to read the key one byte at a time */
    register uint8_t *k = key;

    /*--------------- all but the last block: affect some 32 bits of (a,b,c) */
    while (length > 12)
    {
      a += k[0];
      a += ((uint32_t)k[1])<<8;
      a += ((uint32_t)k[2])<<16;
      a += ((uint32_t)k[3])<<24;
      b += k[4];
      b += ((uint32_t)k[5])<<8;
      b += ((uint32_t)k[6])<<16;
      b += ((uint32_t)k[7])<<24;
      c += k[8];
      c += ((uint32_t)k[9])<<8;
      c += ((uint32_t)k[10])<<16;
      c += ((uint32_t)k[11])<<24;
      mix(a,b,c);
      length -= 12;
      k += 12;
    }

    /*-------------------------------- last block: affect all 32 bits of (c) */
    switch(length)                   /* all the case statements fall through */
    {
    case 12: c+=((uint32_t)k[11])<<24;
    case 11: c+=((uint32_t)k[10])<<16;
    case 10: c+=((uint32_t)k[9])<<8;
    case 9 : c+=k[8];
    case 8 : b+=((uint32_t)k[7])<<24;
    case 7 : b+=((uint32_t)k[6])<<16;
    case 6 : b+=((uint32_t)k[5])<<8;
    case 5 : b+=k[4];
    case 4 : a+=((uint32_t)k[3])<<24;
    case 3 : a+=((uint32_t)k[2])<<16;
    case 2 : a+=((uint32_t)k[1])<<8;
    case 1 : a+=k[0];
             break;
    case 0 : return c;
    }
  }

  final(a,b,c);
  return c;
}



/*
 * hashbig():
 * This is the same as hashword() on big-endian machines.  It is different
 * from hashlittle() on all machines.  hashbig() takes advantage of
 * big-endian byte ordering. 
 */
uint32_t hashbig(register void *key, 
					  register size_t length, 
					  register uint32_t initval)
{
	register uint32_t a,b,c;

  /* Set up the internal state */
  a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;

  if (HASH_BIG_ENDIAN && !((((uint8_t *)key)-(uint8_t *)0) & 0x3)) {
    register uint32_t *k = key;                 /* read 32-bit chunks */

    /*------ all but last block: aligned reads and affect 32 bits of (a,b,c) */
    while (length > 12)
    {
      a += k[0];
      b += k[1];
      c += k[2];
      mix(a,b,c);
      length -= 12;
      k += 3;
    }

    /*----------------------------- handle the last (probably partial) block */
    switch(length)
    {
    case 12: c+=k[2]; b+=k[1]; a+=k[0]; break;
    case 11: c+=k[2]<<8; b+=k[1]; a+=k[0]; break;
    case 10: c+=k[2]<<16; b+=k[1]; a+=k[0]; break;
    case 9 : c+=k[2]<<24; b+=k[1]; a+=k[0]; break;
    case 8 : b+=k[1]; a+=k[0]; break;
    case 7 : b+=k[1]<<8; a+=k[0]; break;
    case 6 : b+=k[1]<<16; a+=k[0]; break;
    case 5 : b+=k[1]<<24; a+=k[0]; break;
    case 4 : a+=k[0]; break;
    case 3 : a+=k[0]<<8; break;
    case 2 : a+=k[0]<<16; break;
    case 1 : a+=k[0]<<24; break;
    case 0 : return c;              /* zero length strings require no mixing */
    }

  } else {                        /* need to read the key one byte at a time */
    register uint8_t *k = key;

    /*--------------- all but the last block: affect some 32 bits of (a,b,c) */
    while (length > 12)
    {
      a += ((uint32_t)k[0])<<24;
      a += ((uint32_t)k[1])<<16;
      a += ((uint32_t)k[2])<<8;
      a += ((uint32_t)k[3]);
      b += ((uint32_t)k[4])<<24;
      b += ((uint32_t)k[5])<<16;
      b += ((uint32_t)k[6])<<8;
      b += ((uint32_t)k[7]);
      c += ((uint32_t)k[8])<<24;
      c += ((uint32_t)k[9])<<16;
      c += ((uint32_t)k[10])<<8;
      c += ((uint32_t)k[11]);
      mix(a,b,c);
      length -= 12;
      k += 12;
    }

    /*-------------------------------- last block: affect all 32 bits of (c) */
    switch(length)                   /* all the case statements fall through */
    {
    case 12: c+=((uint32_t)k[11])<<24;
    case 11: c+=((uint32_t)k[10])<<16;
    case 10: c+=((uint32_t)k[9])<<8;
    case 9 : c+=k[8];
    case 8 : b+=((uint32_t)k[7])<<24;
    case 7 : b+=((uint32_t)k[6])<<16;
    case 6 : b+=((uint32_t)k[5])<<8;
    case 5 : b+=k[4];
    case 4 : a+=((uint32_t)k[3])<<24;
    case 3 : a+=((uint32_t)k[2])<<16;
    case 2 : a+=((uint32_t)k[1])<<8;
    case 1 : a+=k[0];
             break;
    case 0 : return c;
    }
  }

  final(a,b,c);
  return c;
}

Obj FuncJenkinsHashInOrb(Obj self, Obj x, Obj offset, Obj bytelen, Obj hashlen)
{
   void *input;
   uint32_t len;
   uint32_t key;
   uint32_t init = 0;
   uint32_t mod;
   
   input = (void *)((UChar *) ADDR_OBJ(x) + INT_INTOBJ(offset));
   len = (uint32_t)INT_INTOBJ(bytelen);
   mod = INT_INTOBJ(hashlen);
	
// Take advantage of endianness if possible
#ifdef WORDS_BIGENDIAN
   key = hashbig(input, len, init);
#else
   key = hashlittle(input, len, init);
#endif
   
   return INTOBJ_INT((Int)(key % mod + 1));
}

Obj FuncPermLeftQuoTransformationNC(Obj self, Obj t1, Obj t2)
{
    Obj l1, l2;
    Int deg;
    Obj pl;
    Int i;
    Int x;

    /* Get the plain lists out: */
    l1 = ELM_PLIST(t1,1);
    PLAIN_LIST(l1);
    l2 = ELM_PLIST(t2,1);
    PLAIN_LIST(l2);
    deg = LEN_PLIST(l1);
    pl = NEW_PLIST(T_PLIST_CYC_NSORT,deg);
    SET_LEN_PLIST(pl,deg);
    /* From now on no more garbage collections! */
    for (i = 1;i <= deg;i++) {
        x = INT_INTOBJ(ELM_PLIST(l1,i));
        if (ELM_PLIST(pl,x) == NULL) {
            SET_ELM_PLIST(pl,x,ELM_PLIST(l2,i));
        }
    }
    for (i = 1;i <= deg;i++) {
        if (ELM_PLIST(pl,i) == NULL) {
            SET_ELM_PLIST(pl,i,INTOBJ_INT(i));
        }
    }
    return pl;
}

Obj FuncMappingPermSetSetNC(Obj self, Obj src, Obj dst)
{
    Int l;
    Int d,dd;
    Obj out;
    Int i = 1;
    Int j = 1;
    Int next = 1;  /* The next candidate, possibly prevented by being in dst */
    Int k;

    PLAIN_LIST(src);
    PLAIN_LIST(dst);
    l = LEN_PLIST(src);
    d = INT_INTOBJ(ELM_PLIST(src,l));
    dd = INT_INTOBJ(ELM_PLIST(dst,l));
    if (dd > d) d = dd;

    out = NEW_PLIST(T_PLIST_CYC_NSORT,d);
    SET_LEN_PLIST(out,d);
    /* No garbage collection from here on! */

    for (k = 1;k <= d;k++) {
        if (i <= l && k == INT_INTOBJ(ELM_PLIST(src,i))) {
            SET_ELM_PLIST(out,k,ELM_PLIST(dst,i));
            i++;
        } else {
            /* Skip things in dst: */
            while (j <= l) {
                dd = INT_INTOBJ(ELM_PLIST(dst,j));
                if (next < dd) break;
                if (next == dd) next++;
                j++;
            }
            SET_ELM_PLIST(out,k,INTOBJ_INT(next));
            next++;
        }
    }
    return out;
} 
 
#define DEGREELIMITONSTACK 512

Obj FuncMappingPermListListNC(Obj self, Obj src, Obj dst)
{
    Int l;
    Int i;
    Int d;
    Int next;
    Obj out;
    Obj tabdst, tabsrc;
    Int x;
    Int mytabs[DEGREELIMITONSTACK];
    Int mytabd[DEGREELIMITONSTACK];

    PLAIN_LIST(src);
    PLAIN_LIST(dst);
    l = LEN_PLIST(src);
    if (l != LEN_PLIST(dst)) {
        ErrorQuit( "the lists must have equal length", 0L, 0L );
        return 0L;
    }
    d = 0;
    for (i = 1;i <= l;i++) {
        x = INT_INTOBJ(ELM_PLIST(src,i));
        if (x > d) d = x;
    }
    for (i = 1;i <= l;i++) {
        x = INT_INTOBJ(ELM_PLIST(dst,i));
        if (x > d) d = x;
    }
    if (d <= DEGREELIMITONSTACK) {
        /* Small case where we work on the stack: */
        memset(&mytabs,0,sizeof(mytabs));
        memset(&mytabd,0,sizeof(mytabd));
        for (i = 1;i <= l;i++) {
            mytabs[INT_INTOBJ(ELM_PLIST(src,i))] = i;
        }
        for (i = 1;i <= l;i++) {
            mytabd[INT_INTOBJ(ELM_PLIST(dst,i))] = i;
        }
        out = NEW_PLIST(T_PLIST_CYC_NSORT,d);
        SET_LEN_PLIST(out,d);
        /* No garbage collection from here ... */
        next = 1;
        for (i = 1;i <= d;i++) {
            if (mytabs[i]) {   /* if i is in src */
                SET_ELM_PLIST(out,i, ELM_PLIST(dst,mytabs[i]));
            } else {
                /* Skip things in dst: */
                while (mytabd[next]) next++;
                SET_ELM_PLIST(out,i,INTOBJ_INT(next));
                next++;
            }
        }
        /* ... to here! No CHANGED_BAG needed since this is a new object! */
    } else {
        /* Version with intermediate objects: */

        tabsrc = NEW_PLIST(T_PLIST,d);
        SET_LEN_PLIST(tabsrc,0);
        /* No garbage collection from here ... */
        for (i = 1;i <= l;i++) {
            SET_ELM_PLIST(tabsrc,INT_INTOBJ(ELM_PLIST(src,i)),INTOBJ_INT(i));
        }
        /* ... to here! No CHANGED_BAG needed since this is a new object! */
        tabdst = NEW_PLIST(T_PLIST,d);
        SET_LEN_PLIST(tabdst,0);
        /* No garbage collection from here ... */
        for (i = 1;i <= l;i++) {
            SET_ELM_PLIST(tabdst,INT_INTOBJ(ELM_PLIST(dst,i)),INTOBJ_INT(i));
        }
        /* ... to here! No CHANGED_BAG needed since this is a new object! */
        out = NEW_PLIST(T_PLIST_CYC_NSORT,d);
        SET_LEN_PLIST(out,d);
        /* No garbage collection from here ... */
        next = 1;
        for (i = 1;i <= d;i++) {
            if (ELM_PLIST(tabsrc,i)) {   /* if i is in src */
                SET_ELM_PLIST(out,i,
                    ELM_PLIST(dst,INT_INTOBJ(ELM_PLIST(tabsrc,i))));
            } else {
                /* Skip things in dst: */
                while (ELM_PLIST(tabdst,next)) next++;
                SET_ELM_PLIST(out,i,INTOBJ_INT(next));
                next++;
            }
        }
        /* ... to here! No CHANGED_BAG needed since this is a new object! */
    }
    return out;
}

Obj FuncImageAndKernelOfTransformation( Obj self, Obj t )
{
    Int bufstack[DEGREELIMITONSTACK+1];
    Obj bufgap;
    Int *buf;
    Int comps;
    Int i;
    Obj image;
    Int j;
    Obj kernel;
    Obj l;
    Int n;
    Obj tmp;

    l = ELM_PLIST(t,1);
    PLAIN_LIST(l);
    n = LEN_PLIST(l);
    if (n <= DEGREELIMITONSTACK) {
        buf = bufstack;
        for (i = 1;i <= n;i++) buf[i] = 0;
        bufgap = 0L;   /* Just to please the compiler */
    } else{
        bufgap = NEW_PLIST(T_PLIST,n);   /* Only used internally */
        buf = (Int *) (ADDR_OBJ(bufgap));
    }
    /* No garbage collection from here... */
    comps = 0;
    for (j = 1;j <= n;j++) {
        i = INT_INTOBJ(ELM_PLIST(l,j));
        if (!buf[i]) comps++;
        buf[i]++;
    }
    /* ...until here. No there could be some, buf might be wrong then! */
    kernel = NEW_PLIST(T_PLIST,comps);
    image = NEW_PLIST(T_PLIST,comps);
    buf = (n <= DEGREELIMITONSTACK) ? bufstack : (Int *) ADDR_OBJ(bufgap);
    j = 1;
    for (i = 1;i <= n;i++) {
        if (buf[i]) {
            SET_ELM_PLIST(image,j,INTOBJ_INT(i));
            SET_LEN_PLIST(image,j);
            tmp = NEW_PLIST(T_PLIST,buf[i]);
            buf = (n <= DEGREELIMITONSTACK) ? bufstack 
                                            : (Int *) ADDR_OBJ(bufgap);
            SET_ELM_PLIST(kernel,j,tmp);
            SET_LEN_PLIST(kernel,j);
            CHANGED_BAG(kernel);
            buf[i] = j++;
        }
    }
    for (i = 1;i <= n;i++) {
        tmp = ELM_PLIST(kernel,buf[INT_INTOBJ(ELM_PLIST(l,i))]);
        j = LEN_PLIST(tmp);
        SET_ELM_PLIST(tmp,j+1,INTOBJ_INT(i));
        SET_LEN_PLIST(tmp,j+1);
    }
    /* Now sort it: */
    SortDensePlist(kernel);

    tmp = NEW_PLIST(T_PLIST,2);
    SET_LEN_PLIST(tmp,2);
    SET_ELM_PLIST(tmp,1,image);
    SET_ELM_PLIST(tmp,2,kernel);
    return tmp;
}

Obj FuncImageAndKernelOfTransformation2( Obj self, Obj t )
{
    Int bufstack[DEGREELIMITONSTACK+1];
    Obj bufgap;
    Int *buf;
    Int comps;
    Int i;
    Obj image;
    Int j;
    Int k;
    Obj kernel;
    Obj l;
    Int n;
    Obj tmp;

    l = ELM_PLIST(t,1);
    PLAIN_LIST(l);
    n = LEN_PLIST(l);
    kernel = NEW_PLIST(T_PLIST,n);   /* Will hold result */
    SET_LEN_PLIST(kernel,n);
    if (n <= DEGREELIMITONSTACK) {
        buf = bufstack;
        for (i = 1;i <= n;i++) buf[i] = 0;
        bufgap = 0L;   /* Just to please the compiler */
    } else{
        bufgap = NewBag(T_DATOBJ,sizeof(Int)*(n+1));/* Only used internally */
        buf = (Int *) (ADDR_OBJ(bufgap));
    }
    
    comps = 0;
    for (i = 1;i <= n;i++) {
        j = INT_INTOBJ(ELM_PLIST(l,i));
        if (buf[j] == 0) {
            comps++;
            tmp = NEW_PLIST(T_PLIST,1);
            if (n > DEGREELIMITONSTACK) buf = (Int *) ADDR_OBJ(bufgap);
            SET_LEN_PLIST(tmp,1);
            SET_ELM_PLIST(tmp,1,INTOBJ_INT(i));
            SET_ELM_PLIST(kernel,i,tmp);
            CHANGED_BAG(kernel);
            buf[j] = i;
        } else {
            tmp = ELM_PLIST(kernel,buf[j]);
            k = LEN_PLIST(tmp);
            GROW_PLIST(tmp,k+1);
            if (n > DEGREELIMITONSTACK) buf = (Int *) ADDR_OBJ(bufgap);
            SET_ELM_PLIST(tmp,k+1,INTOBJ_INT(i));
            SET_LEN_PLIST(tmp,k+1);
        }
    }
    image = NEW_PLIST(T_PLIST,comps);
    if (n > DEGREELIMITONSTACK) buf = (Int *) ADDR_OBJ(bufgap);
    SET_LEN_PLIST(image,comps);
    /* No garbage collection from here on ... */
    k = 1;
    for (j = 1;j <= n;j++) {
        i = buf[j];
        if (i) {
            SET_ELM_PLIST(image,k,INTOBJ_INT(j));
        }
    }
    /* ... until here. We do not need buf any more from here on. */

    /* Now compactify kernel: */
    j = 1;
    for (i = 1;i <= n;i++) {
        tmp = ELM_PLIST(kernel,i);
        if (tmp) SET_ELM_PLIST(kernel,j++,tmp);
    }
    SET_LEN_PLIST(kernel,comps);
    SHRINK_PLIST(kernel,comps);

    tmp = NEW_PLIST(T_PLIST,2);
    SET_LEN_PLIST(tmp,2);
    SET_ELM_PLIST(tmp,1,image);
    SET_ELM_PLIST(tmp,2,kernel);
    MakeImmutable(tmp);
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

  { "AVLIndexFind_C", 2, "tree, i",
    AVLIndexFind_C,
    "orb.c:AVLIndexFind_C" },

  { "AVLFindIndex_C", 2, "tree, data",
    AVLFindIndex_C,
    "orb.c:AVLFindIndex_C" },

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

  { "AVLAdd_C", 3, "tree, data, value",
    AVLAdd_C,
    "orb.c:AVLAdd_C" },

  { "AVLIndexAdd_C", 4, "tree, data, value, index",
    AVLIndexAdd_C,
    "orb.c:AVLIndexAdd_C" },

  { "AVLDelete_C", 2, "tree, data", 
    AVLDelete_C,
    "orb.c:AVLDelete_C" },

  { "AVLIndexDelete_C", 2, "tree, index", 
    AVLIndexDelete_C,
    "orb.c:AVLIndexDelete_C" },

  { "HTAdd_TreeHash_C", 3, "treehash, x, v",
    HTAdd_TreeHash_C,
    "orb.c:HTAdd_TreeHash_C" },

  { "HTValue_TreeHash_C", 2, "treehash, x",
    HTValue_TreeHash_C,
    "orb.c:HTValue_TreeHash_C" },

  { "HTDelete_TreeHash_C", 2, "treehash, x",
    HTDelete_TreeHash_C,
    "orb.c:HTDelete_TreeHash_C" },

  { "HTUpdate_TreeHash_C", 3, "treehash, x, v",
    HTUpdate_TreeHash_C,
    "orb.c:HTUpdate_TreeHash_C" },

  { "GenericHashFunc_C", 2, "x, data",
    GenericHashFunc_C,
    "orb.c:GenericHashFunc_C" }, 

  { "JENKINS_HASH_IN_ORB", 4, "x, offset, bytelen, hashlen",
    FuncJenkinsHashInOrb, 
    "pkg/orb/src/orb.c:JENKINS_HASH_IN_ORB" },

  { "PermLeftQuoTransformationNC_C", 2, "t1, t2",
    FuncPermLeftQuoTransformationNC,
    "pkg/orb/src/orb.c:FuncPermLeftQuoTransformationNC" },

  { "MappingPermSetSetNC_C", 2, "src, dst",
    FuncMappingPermSetSetNC,
    "pkg/orb/src/orb.c:FuncMappingPermSetSetNC" },

  { "MappingPermListListNC_C", 2, "src, dst",
    FuncMappingPermListListNC,
    "pkg/orb/src/orb.c:FuncMappingPermListListNC" },

  { "ImageAndKernelOfTransformation_C", 1, "t",
    FuncImageAndKernelOfTransformation,
    "pkg/orb/src/orb.c:FuncImageAndKernelOfTransformation" },

  { "ImageAndKernelOfTransformation2_C", 1, "t",
    FuncImageAndKernelOfTransformation2,
    "pkg/orb/src/orb.c:FuncImageAndKernelOfTransformation2" },

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
    ImportFuncFromLibrary( "AVLTree", &AVLTree );
    ImportFuncFromLibrary( "HTGrow", &HTGrow );

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



