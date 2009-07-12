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
    if (!IS_INTOBJ(n) ||
        TNUM_OBJ(t) != T_POSOBJ || TYPE_POSOBJ(t) != AVLTreeType) {
        ErrorQuit( "Usage: AVLFreeNode(avltree,integer)", 0L, 0L );
        return 0L;
    }
    AVLFreeNode(t,INT_INTOBJ(n));
    return (Obj) 0;
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
    Obj tmp = INTOBJ_INT(AVLFind(t,d));
    if (tmp == INTOBJ_INT(0)) 
        return Fail;
    else
        return tmp;
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

Obj static AVLDelete_C( Obj self, Obj tree, Obj data)
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
  
  if (TNUM_OBJ(tree) != T_POSOBJ || TYPE_POSOBJ(tree) != AVLTreeType) {
      ErrorQuit( "Usage: AVLDelete(avltree, object)", 0L, 0L );
      return 0L;
  }

  compare = AVL3Comp(tree);
  p = AVLTop(tree);
  if (p == 0)     /* Nothing to delete or find */
      return Fail;

  if (AVLNodes(tree) == 1) {
      if (INT_INTOBJ(CALL_2ARGS(compare,data,AVLData(tree,p))) == 0) {
          SetAVLNodes(tree,0);
          SetAVLTop(tree,0);
          AVLFreeNode(tree,p);
          return True;
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
          return True;
      } else if (AVLBalFactor(tree,nodes[m]) == path[m]) {
          SetAVLBalFactor(tree,nodes[m],0);     /* we made path[m] shorter */
      } else {   /* tree is out of balance */
          int shorter;
          AVLRebalance(tree,nodes[m],&p,&shorter);
          if (m == 1) {
              SetAVLTop(tree,p);
              return True;               /* everything is done */
          } else if (path[m-1] == 2)   /* was: = -1 */
              SetAVLLeft(tree,nodes[m-1],p);
          else
              SetAVLRight(tree,nodes[m-1],p);
          if (!shorter) return True;    /* nothing happens further up */
      }
      m--;
  }
  return True;
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



