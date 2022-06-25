#############################################################################
##
##                             orb package
##  avltree.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2009-2009 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for AVL trees in GAP.
##
##  adding, removing and finding in O(log n), n is number of nodes
##
##  see Knuth: "The Art of Computer Programming" for algorithms
##
#############################################################################


#
# Conventions:
#
# A balanced binary tree (AVLTree) is a positional object having the 
# following entries:
#   ![1]     len: last used entry (never shrinks), always = 3 mod 4
#   ![2]     free: index of first freed entry, if 0, none free
#   ![3]     nodes: number of nodes currently in the tree
#   ![4]     alloc: highest allocated index, always = 3 mod 4
#   ![5]     three-way comparison function
#   ![6]     top: reference to top node
#   ![7]     value: plain list holding the values stored under the keys
#            can be fail, in which case all stored values are "true"
#            will be bound when first value other than true is set
#
# From index 8 on for every position = 0 mod 4:
#   ![4n]    obj: an object
#   ![4n+1]  left: left reference or < 8 (elements there are smaller)
#   ![4n+2]  right: right reference or < 8 (elements there are bigger)
#   ![4n+3]  rank: number of nodes in left subtree plus one
# For freed nodes position ![4n] holds the link to the next one
# For used nodes references are divisible by four, therefore
# the mod 4 value can be used for other information.
# We use left mod 4: 0 - balanced
#                    1 - balance factor +1
#                    2 - balance factor -1
# 

AVLCmp_GAP := function(a,b)
  if a = b then 
    return 0;
  elif a < b then
    return -1;
  else
    return 1;
  fi;
end;
if IsBound(AVLCmp_C) then
    InstallGlobalFunction(AVLCmp, AVLCmp_C);
else
    InstallGlobalFunction(AVLCmp, AVLCmp_GAP);
fi;

AVLTree_GAP := function(arg)
  # Parameters: options record (optional)
  # Initializes balanced binary tree object, optionally with comparison 
  # function. Returns empty tree object.
  # A comparison function takes 2 arguments and returns respectively -1, 0
  # or 1 if the first argument is smaller than, equal to, or bigger than the
  # second argument.
  # A comparison function is NOT necessary for trees where the ordering is
  # only defined by the tree and not by an ordering of the elements. Such
  # trees are managed by the special functions below. Specify nothing
  # for the cmpfunc (or leave the default one).
  local t,cmpfunc,alloc,opt;
  # defaults:
  cmpfunc := AVLCmp;
  alloc := 11;
  if Length(arg) = 1 then
      opt := arg[1];
      if not(IsRecord(opt)) then
          Error("Argument must be an options record!");
          return fail;
      fi;
      if IsBound(opt.cmpfunc) then
          cmpfunc := opt.cmpfunc;
          if not(IsFunction(cmpfunc)) then
              Error("cmdfunc must be a three-way comparison function");
              return fail;
          fi;
      fi;
      if IsBound(opt.allocsize) then
          alloc := opt.allocsize;
          if not(IsInt(alloc)) then
              Error("allocsize must be a positive integer");
          fi;
          alloc := alloc*4+3;
      fi;
  elif Length(arg) <> 0 then
      Error("Usage: AVLTree( [options-record] )");
      return fail;
  fi;
  t := [11,8,0,alloc,cmpfunc,0,fail,0,0,0,0];
  if alloc > 11 then t[alloc] := fail; fi;    # expand object
  Objectify(AVLTreeTypeMutable,t);
  return t;
end;
if IsBound(AVLTree_C) then
    InstallGlobalFunction(AVLTree, AVLTree_C);
else
    InstallGlobalFunction(AVLTree, AVLTree_GAP);
fi;

InstallMethod( ViewObj, "for an avltree object",
  [IsAVLTree and IsAVLTreeFlatRep],
  function( t )
    Print("<avltree nodes=",t![3]," alloc=",t![4],">");
  end );

AVLNewNode_GAP := function(t)
  local n;
  if t![2] > 0 then
      n := t![2];
      t![2] := t![n];
  elif t![1] < t![4] then
      n := t![1]+1;
      t![1] := t![1]+4;
  else
      n := t![4]+1;
      t![4] := t![4] * 2 + 1;    # retain congruent 3 mod 4
      t![1] := n+3;
      t![t![4]] := fail;    # expand allocation
  fi;
  t![n] := 0;
  t![n+1] := 0;
  t![n+2] := 0;
  t![n+3] := 0;
  return n;
end;
if IsBound(AVLNewNode_C) then
    InstallGlobalFunction(AVLNewNode, AVLNewNode_C);
else
    InstallGlobalFunction(AVLNewNode, AVLNewNode_GAP);
fi;


AVLFreeNode_GAP := function(t,n)
  local o;
  t![n] := t![2];
  t![2] := n;
  n := n/4;
  if t![7] <> fail and IsBound(t![7][n]) then
      o := t![7][n];
      Unbind(t![7][n]);
      return o;
  fi;
  return true;
end;
if IsBound(AVLFreeNode_C) then
    InstallGlobalFunction(AVLFreeNode, AVLFreeNode_C);
else
    InstallGlobalFunction(AVLFreeNode, AVLFreeNode_GAP);
fi;


AVLData_GAP := function(t,n)
  return t![n];
end;
if IsBound(AVLData_C) then
    InstallGlobalFunction(AVLData, AVLData_C);
else
    InstallGlobalFunction(AVLData, AVLData_GAP);
fi;


AVLSetData_GAP := function(t,n,d)
  t![n] := d;
end;
if IsBound(AVLSetData_C) then
    InstallGlobalFunction(AVLSetData, AVLSetData_C);
else
    InstallGlobalFunction(AVLSetData, AVLSetData_GAP);
fi;


AVLLeft_GAP := function(t,n)
  return QuoInt(t![n+1],4)*4;
end;
if IsBound(AVLLeft_C) then
    InstallGlobalFunction(AVLLeft, AVLLeft_C);
else
    InstallGlobalFunction(AVLLeft, AVLLeft_GAP);
fi;


AVLSetLeft_GAP := function(t,n,m)
  t![n+1] := m + t![n+1] mod 4;
end;
if IsBound(AVLSetLeft_C) then
    InstallGlobalFunction(AVLSetLeft, AVLSetLeft_C);
else
    InstallGlobalFunction(AVLSetLeft, AVLSetLeft_GAP);
fi;


AVLRight_GAP := function(t,n)
  return QuoInt(t![n+2],4)*4;
end;
if IsBound(AVLRight_C) then
    InstallGlobalFunction(AVLRight, AVLRight_C);
else
    InstallGlobalFunction(AVLRight, AVLRight_GAP);
fi;


AVLSetRight_GAP := function(t,n,m)
  t![n+2] := m;
end;
if IsBound(AVLSetRight_C) then
    InstallGlobalFunction(AVLSetRight, AVLSetRight_C);
else
    InstallGlobalFunction(AVLSetRight, AVLSetRight_GAP);
fi;


AVLRank_GAP := function(t,n)
  return t![n+3];
end;
if IsBound(AVLRank_C) then
    InstallGlobalFunction(AVLRank, AVLRank_C);
else
    InstallGlobalFunction(AVLRank, AVLRank_GAP);
fi;


AVLSetRank_GAP := function(t,n,r)
  t![n+3] := r;
end;
if IsBound(AVLSetRank_C) then
    InstallGlobalFunction(AVLSetRank, AVLSetRank_C);
else
    InstallGlobalFunction(AVLSetRank, AVLSetRank_GAP);
fi;


AVLBalFactor_GAP := function(t,n)
  local bf;
  bf := t![n+1] mod 4;    # 0, 1 or 2
  if bf = 2 then
    return -1;
  else
    return bf;
  fi;
end;
if IsBound(AVLBalFactor_C) then
    InstallGlobalFunction(AVLBalFactor, AVLBalFactor_C);
else
    InstallGlobalFunction(AVLBalFactor, AVLBalFactor_GAP);
fi;


AVLSetBalFactor_GAP := function(t,n,bf)
  if bf = -1 then
    t![n+1] := QuoInt(t![n+1],4)*4 + 2;
  else
    t![n+1] := QuoInt(t![n+1],4)*4 + bf;
  fi;
end;
if IsBound(AVLSetBalFactor_C) then
    InstallGlobalFunction(AVLSetBalFactor, AVLSetBalFactor_C);
else
    InstallGlobalFunction(AVLSetBalFactor, AVLSetBalFactor_GAP);
fi;

AVLValue_GAP := function(t,n)
  if t![7] = fail then 
      return true;
  elif not(IsBound(t![7][n/4])) then
      return true;
  else
      return t![7][n/4];
  fi;
end;
if IsBound(AVLValue_C) then
    InstallGlobalFunction(AVLValue, AVLValue_C);
else
    InstallGlobalFunction(AVLValue, AVLValue_GAP);
fi;

AVLSetValue_GAP := function(t,n,v)
  n := n/4;
  if t![7] = fail then
      t![7] := EmptyPlist(n);
  fi;
  t![7][n] := v;
end;
if IsBound(AVLSetValue_C) then
    InstallGlobalFunction(AVLSetValue, AVLSetValue_C);
else
    InstallGlobalFunction(AVLSetValue, AVLSetValue_GAP);
fi;

InstallMethod( Display, "for an avltree object",
  [IsAVLTree and IsAVLTreeFlatRep],
  function( t )
    local DoRecursion;
    DoRecursion := function(p,depth,P)
      local i;
      if p = 0 then return; fi;
      for i in [1..depth] do Print(" "); od;
      Print(P,"data=",AVLData(t,p)," rank=",AVLRank(t,p)," pos=",p,
            " bf=",AVLBalFactor(t,p),"\n");
      DoRecursion(AVLLeft(t,p),depth+1,"L:");
      DoRecursion(AVLRight(t,p),depth+1,"R:");
    end;

    Print("<avltree nodes=",t![3]," alloc=",t![4],"\n");
    DoRecursion(t![6],1,"");
    Print(">\n");
  end );

AVLFind_GAP := function(tree,data)
  # Parameters: tree, data
  #  t is a AVL
  #  data is a data structure defined by the user
  # Searches in tree for a node equal to data, returns this node or fail
  # if not found.
  local compare, p, c;
  compare := tree![5];
  p := tree![6];
  while p >= 8 do
    c := compare(data,AVLData(tree,p));
    if c = 0 then
      return p;
    elif c < 0 then    # data < AVLData(tree,p)
      p := AVLLeft(tree,p);
    else               # data > AVLData(tree,p)
      p := AVLRight(tree,p);
    fi;
  od;
  
  return fail;
end;
if IsBound(AVLFind_C) then
    InstallGlobalFunction(AVLFind, AVLFind_C);
else
    InstallGlobalFunction(AVLFind, AVLFind_GAP);
fi;

AVLLookup_GAP := function(t,d)
  local p;
  p := AVLFind(t,d);
  if p = fail then
      return fail;
  else
      return AVLValue(t,p);
  fi;
end;
if IsBound(AVLLookup_C) then
    InstallGlobalFunction(AVLLookup, AVLLookup_C);
else
    InstallGlobalFunction(AVLLookup, AVLLookup_GAP);
fi;

AVLIndex_GAP := function(tree,index)
  # Parameters: tree, index
  #  tree is a AVL
  #  index is an index in the tree
  # Searches in tree for the node with index index, returns the data of
  # this node or fail if not found. Works without comparison function, 
  # just by index.
  local p, offset, r;

  if index < 1 or index > tree![3] then
    return fail;
  fi;
  
  p := tree![6];
  offset := 0;         # Offset of subtree p in tree
  
  while true do   # will terminate!
    r := offset + AVLRank(tree,p);
    if index < r then
      # go left
      p := AVLLeft(tree,p);
    elif index = r then
      # found!
      return AVLData(tree,p);
    else
      # go right!
      offset := r;
      p := AVLRight(tree,p);
    fi;
  od;
end;
if IsBound(AVLIndex_C) then
    InstallGlobalFunction(AVLIndex, AVLIndex_C);
else
    InstallGlobalFunction(AVLIndex, AVLIndex_GAP);
fi;

AVLIndexFind_GAP := function(tree,index)
  # Parameters: tree, index
  #  tree is a AVL
  #  index is an index in the tree
  # Searches in tree for the node with index index, returns the position of
  # this node or fail if not found. Works without comparison function, 
  # just by index.
  local p, offset, r;

  if index < 1 or index > tree![3] then
    return fail;
  fi;
  
  p := tree![6];
  offset := 0;         # Offset of subtree p in tree
  
  while true do   # will terminate!
    r := offset + AVLRank(tree,p);
    if index < r then
      # go left
      p := AVLLeft(tree,p);
    elif index = r then
      # found!
      return p;
    else
      # go right!
      offset := r;
      p := AVLRight(tree,p);
    fi;
  od;
end;
if IsBound(AVLIndexFind_C) then
    InstallGlobalFunction(AVLIndexFind, AVLIndexFind_C);
else
    InstallGlobalFunction(AVLIndexFind, AVLIndexFind_GAP);
fi;

AVLIndexLookup_GAP := function(tree,i)
  local p;
  p := AVLIndexFind(tree,i);
  if p = fail then 
      return fail;
  else
      return AVLValue(tree,p);
  fi;
end;
if IsBound(AVLIndexLookup_C) then
    InstallGlobalFunction(AVLIndexLookup, AVLIndexLookup_C);
else
    InstallGlobalFunction(AVLIndexLookup, AVLIndexLookup_GAP);
fi;

AVLRebalance_GAP := function(tree,q)
  # the tree starting at q has balanced subtrees but is out of balance:
  # the depth of the deeper subtree is 2 bigger than the depth of the other
  # tree. This function changes this situation following the procedure
  # described in Knuth: "The Art of Computer Programming".
  # It returns a record with the new start node of the subtree as entry 
  # "newroot" and in "shorter" a boolean value which indicates, if the
  # depth of the tree was decreased by 1 by this operation.
  local shrink, p, l;

  shrink := true;   # in nearly all cases this happens
  if AVLBalFactor(tree,q) < 0 then
    p := AVLLeft(tree,q);
  else
    p := AVLRight(tree,q);
  fi;
  if AVLBalFactor(tree,p) = AVLBalFactor(tree,q) then   
    # we need a single rotation:
    #       q++             p=           q--          p=
    #      / \             / \          / \          / \
    #     a   p+    ==>   q=  c    OR  p-  c   ==>  a   q=
    #        / \         / \          / \              / \
    #       b   c       a   b        a   b            b   c
    if AVLBalFactor(tree,q) > 0 then
      AVLSetRight(tree,q,AVLLeft(tree,p));
      AVLSetLeft(tree,p,q);
      AVLSetBalFactor(tree,q,0);
      AVLSetBalFactor(tree,p,0);
      AVLSetRank(tree,p,AVLRank(tree,p) + AVLRank(tree,q));
    else
      AVLSetLeft(tree,q,AVLRight(tree,p));
      AVLSetRight(tree,p,q);
      AVLSetBalFactor(tree,q,0);
      AVLSetBalFactor(tree,p,0);
      AVLSetRank(tree,q,AVLRank(tree,q) - AVLRank(tree,p));
    fi;
  elif AVLBalFactor(tree,p) = - AVLBalFactor(tree,q) then   
    # we need a double rotation:
    #       q++                             q--
    #      / \             c=              / \            c=
    #     a   p-         /   \            p+  e         /   \
    #        / \   ==>  q     p    OR    / \      ==>  p     q
    #       c   e      / \   / \        a   c         / \   / \
    #      / \        a   b d   e          / \       a   b d   e
    #     b   d                           b   d
    if AVLBalFactor(tree,q) > 0 then
      l := AVLLeft(tree,p);
      AVLSetRight(tree,q,AVLLeft(tree,l));
      AVLSetLeft(tree,p,AVLRight(tree,l));
      AVLSetLeft(tree,l,q);
      AVLSetRight(tree,l,p);
      if AVLBalFactor(tree,l) > 0 then
        AVLSetBalFactor(tree,p,0);
        AVLSetBalFactor(tree,q,-1);
      elif AVLBalFactor(tree,l) = 0 then
        AVLSetBalFactor(tree,p,0);
        AVLSetBalFactor(tree,q,0);
      else    # AVLBalFactor(tree,l) < 0
        AVLSetBalFactor(tree,p,1);
        AVLSetBalFactor(tree,q,0);
      fi;
      AVLSetBalFactor(tree,l,0);
      AVLSetRank(tree,p,AVLRank(tree,p) - AVLRank(tree,l));
      AVLSetRank(tree,l,AVLRank(tree,l) + AVLRank(tree,q));
      p := l;
    else
      l := AVLRight(tree,p);
      AVLSetLeft(tree,q,AVLRight(tree,l));
      AVLSetRight(tree,p,AVLLeft(tree,l));
      AVLSetLeft(tree,l,p);
      AVLSetRight(tree,l,q);
      if AVLBalFactor(tree,l) < 0 then
        AVLSetBalFactor(tree,p,0);
        AVLSetBalFactor(tree,q,1);
      elif AVLBalFactor(tree,l) = 0 then
        AVLSetBalFactor(tree,p,0);
        AVLSetBalFactor(tree,q,0);
      else    # AVLBalFactor(tree,l) > 0
        AVLSetBalFactor(tree,p,-1);
        AVLSetBalFactor(tree,q,0);
      fi;
      AVLSetBalFactor(tree,l,0);
      AVLSetRank(tree,l,AVLRank(tree,l) + AVLRank(tree,p));
      AVLSetRank(tree,q,AVLRank(tree,q) - AVLRank(tree,l));   
                           # new value of AVLRank(tree,l)!
      p := l;
    fi;
  else   # AVLBalFactor(tree,p) = 0 then  
    # we need a single rotation:
    #       q++             p-           q--          p+
    #      / \             / \          / \          / \
    #     a   p=    ==>   q+  c    OR  p=  c   ==>  a   q-
    #        / \         / \          / \              / \
    #       b   c       a   b        a   b            b   c
    if AVLBalFactor(tree,q) > 0 then
      AVLSetRight(tree,q,AVLLeft(tree,p));
      AVLSetLeft(tree,p,q);
      AVLSetBalFactor(tree,q,1);
      AVLSetBalFactor(tree,p,-1);
      AVLSetRank(tree,p,AVLRank(tree,p) + AVLRank(tree,q));
    else
      AVLSetLeft(tree,q,AVLRight(tree,p));
      AVLSetRight(tree,p,q);
      AVLSetBalFactor(tree,q,-1);
      AVLSetBalFactor(tree,p,1);
      AVLSetRank(tree,q,AVLRank(tree,q) - AVLRank(tree,p));
    fi;
    shrink := false;    
  fi;
  return rec(newroot := p, shorter := shrink);
end;
if IsBound(AVLRebalance_C) then
    InstallGlobalFunction(AVLRebalance, AVLRebalance_C);
else
    InstallGlobalFunction(AVLRebalance, AVLRebalance_GAP);
fi;


AVLAdd_GAP := function(tree,data,value)
  # Parameters: tree, data, value
  #  tree is a AVL
  #  data is a data structure defined by the user
  #  value is the value stored under the key data, if true, nothing is stored
  # Tries to add the data as a node in tree. It is an error, if there is
  # already a node which is "equal" to data with respect to the comparison
  # function. Returns true if everything went well or fail, if an equal 
  # object is already present. 

  local compare, p, new, path, nodes, n, q, rankadds, c, l, i;
  
  compare := tree![5];
  
  p := tree![6];
  if p = 0 then   # A new, single node in the tree
    new := AVLNewNode(tree);
    AVLSetLeft(tree,new,0);
    AVLSetRight(tree,new,0);
    AVLSetBalFactor(tree,new,0);
    AVLSetRank(tree,new,1);
    AVLSetData(tree,new,data);
    if value <> true then
        AVLSetValue(tree,new,value);
    fi;
    tree![3] := 1;
    tree![6] := new;
    return true;
  fi;
  
  # let's first find the right position in the tree:
  # but: remember the last node on the way with bal. factor <> 0 and the path
  #      after this node
  # and: remember the nodes where the Rank entry is incremented in case we
  #      find an "equal" element
  path := EmptyPlist(10);   # here all steps are recorded: -1:left, +1:right
  nodes := EmptyPlist(10);
  nodes[1] := p;   # here we store all nodes on our way, nodes[i+1] is reached
                   # from nodes[i] by walking one step path[i]
  n := 1;          # this is the length of "nodes"
  q := 0;          # this is the last node with bal. factor <> 0
                   # index in "nodes" or 0 for no such node
  rankadds := EmptyPlist(10);# nothing done so far, list of Rank-modified nodes
  repeat
    
    # do we have to remember this position?
    if AVLBalFactor(tree,p) <> 0 then
      q := n;       # forget old last node with balance factor <> 0
    fi;
    
    # now one step:
    c := compare(data,AVLData(tree,p));
    if c = 0 then   # we did not want this!
      for p in rankadds do
        AVLSetRank(tree,p,AVLRank(tree,p) - 1);
      od;
      return fail; # tree is unchanged
    fi;
    
    l := p;     # remember last position
    if c < 0 then   # data < AVLData(tree,p)
      AVLSetRank(tree,p,AVLRank(tree,p) + 1);
      Add(rankadds,p);
      p := AVLLeft(tree,p);
    else            # data > AVLData(tree,p)
      p := AVLRight(tree,p);
    fi;
    Add(nodes,p);
    n := n + 1;
    Add(path,c);
    
  until p = 0;
  # now p is 0 and nodes[n-1] is the node where data must be attached
  # the tree must be modified between nodes[q] and nodes[n-1] along path
  # Ranks are already done
  l := nodes[n-1];   # for easier reference
  
  # a new node:
  p := AVLNewNode(tree);
  AVLSetLeft(tree,p,0);
  AVLSetRight(tree,p,0);
  AVLSetBalFactor(tree,p,0);
  AVLSetRank(tree,p,1);
  AVLSetData(tree,p,data);
  if value <> true then
      AVLSetValue(tree,p,value);
  fi;
  # insert into tree:
  if c < 0 then    # left
    AVLSetLeft(tree,l,p);
  else
    AVLSetRight(tree,l,p);
  fi;
  tree![3] := tree![3] + 1;
  
  # modify balance factors between q and l:
  for i in [q+1..n-1] do
    AVLSetBalFactor(tree,nodes[i],path[i]);
  od;
  
  # is rebalancing at q necessary?
  if q = 0 then    # whole tree has grown one step
    return true;   # Success!
  fi;
  if AVLBalFactor(tree,nodes[q]) = -path[q] then
    # the subtree at q has gotten more balanced
    AVLSetBalFactor(tree,nodes[q],0);
    return true;   # Success!
  fi;
  
  # now at last we do have to rebalance at nodes[q] because the tree has
  # gotten out of balance:
  p := AVLRebalance(tree,nodes[q]);
  p := p.newroot;
  
  # finishing touch: link new root of subtree (p) to t:
  if q = 1 then  # q resp. r was First node
    tree![6] := p;
  elif path[q-1] = -1 then
    AVLSetLeft(tree,nodes[q-1],p);
  else
    AVLSetRight(tree,nodes[q-1],p);
  fi;
  
  return true;
end;
if IsBound(AVLAdd_C) then
    InstallGlobalFunction(AVLAdd, AVLAdd_C);
else
    InstallGlobalFunction(AVLAdd, AVLAdd_GAP);
fi;


AVLIndexAdd_GAP := function(tree,data,value,index)
  # Parameters: index, data, value, tree
  #  tree is a AVL
  #  data is a data structure defined by the user
  #  value is the value to be stored under key data, nothing is stored if true
  #  index is the index, where data should be inserted in tree 1 ist at
  #          first position, NumberOfNodes+1 after the last.
  # Tries to add the data as a node in tree. Returns true if everything 
  # went well or fail, if something went wrong, 
  
  local p, path, nodes, n, q, offset, c, l, i;
  
  if index < 1 or index > tree![3]+1 then
    return fail;
  fi;
  
  p := tree![6];
  if p = 0 then   # A new, single node in the tree
    # index must be equal to 1
    tree![6] := AVLNewNode(tree);
    AVLSetLeft(tree,tree![6],0);
    AVLSetRight(tree,tree![6],0);
    AVLSetBalFactor(tree,tree![6],0);
    AVLSetRank(tree,tree![6],1);
    AVLSetData(tree,tree![6],data);
    if value <> true then
        AVLSetValue(tree,tree![6],value);
    fi;
    tree![3] := 1;
    return true;
  fi;
  
  # let's first find the right position in the tree:
  # but: remember the last node on the way with bal. factor <> 0 and the path
  #      after this node
  # and: remember the nodes where the Rank entry is incremented in case we
  #      find an "equal" element
  path := EmptyPlist(10);     # here all steps are recorded: -1:left, +1:right
  nodes := EmptyPlist(10);
  nodes[1] := p;   # here we store all nodes on our way, nodes[i+1] is reached
                   # from nodes[i] by walking one step path[i]
  n := 1;          # this is the length of "nodes"
  q := 0;          # this is the last node with bal. factor <> 0
                   # index in "nodes" or 0 for no such node
  offset := 0;     # number of nodes with smaller index than those in subtree
  repeat
    
    # do we have to remember this position?
    if AVLBalFactor(tree,p) <> 0 then
      q := n;       # forget old last node with balance factor <> 0
    fi;
    
    # now one step:
    if index <= offset+AVLRank(tree,p) then
      c := -1;    # we have to descend to left subtree
    else
      c := +1;    # we have to descend to right subtree
    fi;
    
    l := p;     # remember last position
    if c < 0 then   # data < AVLData(tree,p)
      AVLSetRank(tree,p,AVLRank(tree,p) + 1);
      p := AVLLeft(tree,p);
    else            # data > AVLData(tree,p)
      offset := offset + AVLRank(tree,p);
      p := AVLRight(tree,p);
    fi;
    Add(nodes,p);
    n := n + 1;
    Add(path,c);
    
  until p = 0;
  # now p is 0 and nodes[n-1] is the node where data must be attached
  # the tree must be modified between nodes[q] and nodes[n-1] along path
  # Ranks are already done
  l := nodes[n-1];   # for easier reference
  
  # a new node:
  p := AVLNewNode(tree);
  AVLSetLeft(tree,p,0);
  AVLSetRight(tree,p,0);
  AVLSetBalFactor(tree,p,0);
  AVLSetRank(tree,p,1);
  AVLSetData(tree,p,data);
  if value <> true then
      AVLSetValue(tree,p,value);
  fi;
  # insert into tree:
  if c < 0 then    # left
    AVLSetLeft(tree,l,p);
  else
    AVLSetRight(tree,l,p);
  fi;
  tree![3] := tree![3] + 1;
  
  # modify balance factors between q and l:
  for i in [q+1..n-1] do
    AVLSetBalFactor(tree,nodes[i],path[i]);
  od;
  
  # is rebalancing at q necessary?
  if q = 0 then    # whole tree has grown one step
    return true;   # Success!
  fi;
  if AVLBalFactor(tree,nodes[q]) = -path[q] then
    # the subtree at q has gotten more balanced
    AVLSetBalFactor(tree,nodes[q],0);
    return true;   # Success!
  fi;
  
  # now at last we do have to rebalance at nodes[q] because the tree has
  # gotten out of balance:
  p := AVLRebalance(tree,nodes[q]);
  p := p.newroot;
  
  # finishing touch: link new root of subtree (p) to t:
  if q = 1 then  # q resp. r was First node
    tree![6] := p;
  elif path[q-1] = -1 then
    AVLSetLeft(tree,nodes[q-1],p);
  else
    AVLSetRight(tree,nodes[q-1],p);
  fi;
  
  return true;
end;
if IsBound(AVLIndexAdd_C) then
    InstallGlobalFunction(AVLIndexAdd, AVLIndexAdd_C);
else
    InstallGlobalFunction(AVLIndexAdd, AVLIndexAdd_GAP);
fi;

AVLDelete_GAP := function(tree,data)
  # Parameters: tree, data
  #  tree is a AVL
  #  data is a data structure defined by the user
  # Tries to find data as a node in the tree. If found, this node is deleted
  # and the tree rebalanced. It is an error, if the node is not found.
  # Returns fail in this case, and the stored value normally.
  local compare, p, path, nodes, n, ranksubs, c, m, l, r, i, old;
  
  compare := tree![5];
  
  p := tree![6];
  if p = 0 then   # Nothing to delete or find
    return fail;
  fi;
  if tree![3] = 1 then
    if compare(data,AVLData(tree,p)) = 0 then
      tree![3] := 0;
      tree![6] := 0;
      return AVLFreeNode(tree,p);
    else
      return fail;
    fi;
  fi;
  
  # let's first find the right position in the tree:
  # and: remember the nodes where the Rank entry is decremented in case we
  #      find an "equal" element
  path := EmptyPlist(10);    # here all steps are recorded: -1:left, +1:right
  nodes := EmptyPlist(10);
  nodes[1] := p;   # here we store all nodes on our way, nodes[i+1] is reached
                   # from nodes[i] by walking one step path[i]
  n := 1;          # this is the length of "nodes"
  ranksubs := EmptyPlist(10);# nothing done so far, list of Rank-modified nodes
  
  repeat
    
    # what is the next step?
    c := compare(data,AVLData(tree,p));
    
    if c <> 0 then  # only if data not found!
      if c < 0 then       # data < AVLData(tree,p)
        AVLSetRank(tree,p,AVLRank(tree,p) - 1);
        Add(ranksubs,p);
        p := AVLLeft(tree,p);
      elif c > 0 then     # data > AVLData(tree,p)
        p := AVLRight(tree,p);
      fi;
      Add(nodes,p);
      n := n + 1;
      Add(path,c);
    fi;
    
    if p = 0 then
      # error, we did not find data
      for i in ranksubs do
        AVLSetRank(tree,i,AVLRank(tree,i) + 1);
      od;
      return fail;
    fi;
    
  until c = 0;   # until we find the right node
  # now data is equal to AVLData(tree,p,) so this node p must be removed.
  # the tree must be modified between tree![6] and nodes[n] along path
  # Ranks are already done up there
  
  # now we have to search a neighbour, we modify "nodes" and "path" but not n!
  m := n;
  if AVLBalFactor(tree,p) < 0 then   # search to the left
    l := AVLLeft(tree,p);   # must be a node!
    AVLSetRank(tree,p,AVLRank(tree,p) - 1);   
    # we will delete in left subtree!
    Add(nodes,l);
    m := m + 1;
    Add(path,-1);
    while AVLRight(tree,l) <> 0 do
      l := AVLRight(tree,l);
      Add(nodes,l);
      m := m + 1;
      Add(path,1);
    od;
    c := -1;       # we got predecessor
  elif AVLBalFactor(tree,p) > 0 then    # search to the right
    l := AVLRight(tree,p);  # must be a node!
    Add(nodes,l);
    m := m + 1;
    Add(path,1);
    while AVLLeft(tree,l) <> 0 do
      AVLSetRank(tree,l,AVLRank(tree,l) - 1); 
      # we will delete in left subtree!
      l := AVLLeft(tree,l);
      Add(nodes,l);
      m := m + 1;
      Add(path,-1);
    od;
    c := 1;        # we got successor
  else   # equal depths
    if AVLLeft(tree,p) <> 0 then  
      l := AVLLeft(tree,p);
      AVLSetRank(tree,p,AVLRank(tree,p) - 1);
      Add(nodes,l);
      m := m + 1;
      Add(path,-1);
      while AVLRight(tree,l) <> 0 do
        l := AVLRight(tree,l);
        Add(nodes,l);
        m := m + 1;
        Add(path,1);
      od;
      c := -1;     # we got predecessor
    else           # we got an end node
      l := p;
      c := 0;      
    fi;
  fi;
  # l points now to a neighbour, in case c = -1 to the predecessor, in case
  # c = 1 to the successor, or to p itself in case c = 0
  # "nodes" and "path" is updated, but n could be < m
  
  # Copy Data from l up to p: order is NOT modified
  AVLSetData(tree,p,AVLData(tree,l));   
     # works for m = n, i.e. if p is end node
  
  # Delete node at l = nodes[m] by modifying nodes[m-1]:
  # Note: nodes[m] has maximal one subtree!
  if c <= 0 then
    r := AVLLeft(tree,l);
  else  #  c > 0
    r := AVLRight(tree,l);
  fi;
  if path[m-1] < 0 then
    AVLSetLeft(tree,nodes[m-1],r);
  else
    AVLSetRight(tree,nodes[m-1],r);
  fi;
  tree![3] := tree![3] - 1;
  old := AVLFreeNode(tree,l);
  
  # modify balance factors:
  # the subtree nodes[m-1] has become shorter at its left (resp. right)
  # subtree, if path[m-1]=-1 (resp. +1). We have to react according to
  # the BalFactor at this node and then up the tree, if the whole subtree
  # has shrunk:
  # (we decrement m and work until the corresponding subtree has not shrunk)
  m := m - 1;  # start work HERE
  while m >= 1 do
    if AVLBalFactor(tree,nodes[m]) = 0 then
      AVLSetBalFactor(tree,nodes[m],-path[m]);  # we made path[m] shorter
      return old;
    elif AVLBalFactor(tree,nodes[m]) = path[m] then
      AVLSetBalFactor(tree,nodes[m],0);         # we made path[m] shorter
    else    # tree is out of balance
      p := AVLRebalance(tree,nodes[m]);
      if m = 1 then
        tree![6] := p.newroot;
        return old;               # everything is done
      elif path[m-1] = -1 then
        AVLSetLeft(tree,nodes[m-1],p.newroot);
      else
        AVLSetRight(tree,nodes[m-1],p.newroot);
      fi;
      if not p.shorter then return old; fi;   # nothing happens further up
    fi;
    m := m - 1;
  od;
  return old;
end;
if IsBound(AVLDelete_C) then
    InstallGlobalFunction(AVLDelete, AVLDelete_C);
else
    InstallGlobalFunction(AVLDelete, AVLDelete_GAP);
fi;

AVLIndexDelete_GAP := function(tree,index)
  # Parameters: tree, index
  #  index is the index of the element to be deleted, must be between 1 and
  #          tree![3] inclusively
  #  tree is a AVL
  # returns fail if index is out of range, otherwise the deleted key;
  local p, path, nodes, n, offset, c, m, l, r, x;
  
  if index < 1 or index > tree![3] then
    return fail;
  fi;
  
  p := tree![6];
  if p = 0 then   # Nothing to delete or find
    return fail;
  fi;
  if tree![3] = 1 then
    # index must be equal to 1
    x := AVLData(tree,tree![6]);
    tree![3] := 0;
    tree![6] := 0;
    AVLFreeNode(tree,p);
    return x;
  fi;
  
  # let's first find the right position in the tree:
  path := EmptyPlist(10);     # here all steps are recorded: -1:left, +1:right
  nodes := EmptyPlist(10);
  nodes[1] := p;   # here we store all nodes on our way, nodes[i+1] is reached
                   # from nodes[i] by walking one step path[i]
  n := 1;          # this is the length of "nodes"
  offset := 0;     # number of "smaller" nodes than subtree in whole tree
  
  repeat
    
    # what is the next step?
    if index = offset+AVLRank(tree,p) then
      c := 0;   # we found our node!
      x := AVLData(tree,p);
    elif index < offset+AVLRank(tree,p) then
      c := -1;  # we have to go left
    else
      c := +1;  # we have to go right
    fi;
    
    if c <> 0 then  # only if data not found!
      if c < 0 then       # data < AVLData(tree,p)
        AVLSetRank(tree,p,AVLRank(tree,p) - 1);
        p := AVLLeft(tree,p);
      elif c > 0 then     # data > AVLData(tree,p)
        offset := offset + AVLRank(tree,p);
        p := AVLRight(tree,p);
      fi;
      Add(nodes,p);
      n := n + 1;
      Add(path,c);
    fi;
    
  until c = 0;   # until we find the right node
  # now index is right, so this node p must be removed.
  # the tree must be modified between tree.First and nodes[n] along path
  # Ranks are already done up there
  
  # now we have to search a neighbour, we modify "nodes" and "path" but not n!
  m := n;
  if AVLBalFactor(tree,p) < 0 then   # search to the left
    l := AVLLeft(tree,p);   # must be a node!
    AVLSetRank(tree,p,AVLRank(tree,p) - 1);   
    # we will delete in left subtree!
    Add(nodes,l);
    m := m + 1;
    Add(path,-1);
    while AVLRight(tree,l) <> 0 do
      l := AVLRight(tree,l);
      Add(nodes,l);
      m := m + 1;
      Add(path,1);
    od;
    c := -1;       # we got predecessor
  elif AVLBalFactor(tree,p) > 0 then    # search to the right
    l := AVLRight(tree,p);  # must be a node!
    Add(nodes,l);
    m := m + 1;
    Add(path,1);
    while AVLLeft(tree,l) <> 0 do
      AVLSetRank(tree,l,AVLRank(tree,l) - 1);  
      # we will delete in left subtree!
      l := AVLLeft(tree,l);
      Add(nodes,l);
      m := m + 1;
      Add(path,-1);
    od;
    c := 1;        # we got successor
  else   # equal depths
    if AVLLeft(tree,p) <> 0 then  
      l := AVLLeft(tree,p);
      AVLSetRank(tree,p,AVLRank(tree,p) - 1);  
      # we will delete in left subtree!
      Add(nodes,l);
      m := m + 1;
      Add(path,-1);
      while AVLRight(tree,l) <> 0 do
        l := AVLRight(tree,l);
        Add(nodes,l);
        m := m + 1;
        Add(path,1);
      od;
      c := -1;     # we got predecessor
    else           # we got an end node
      l := p;
      c := 0;      
    fi;
  fi;
  # l points now to a neighbour, in case c = -1 to the predecessor, in case
  # c = 1 to the successor, or to p itself in case c = 0
  # "nodes" and "path" is updated, but n could be < m
  
  # Copy Data from l up to p: order is NOT modified
  AVLSetData(tree,p,AVLData(tree,l));   
  # works for m = n, i.e. if p is end node
  
  # Delete node at l = nodes[m] by modifying nodes[m-1]:
  # Note: nodes[m] has maximal one subtree!
  if c <= 0 then
    r := AVLLeft(tree,l);
  else  #  c > 0
    r := AVLRight(tree,l);
  fi;
  if path[m-1] < 0 then
    AVLSetLeft(tree,nodes[m-1],r);
  else
    AVLSetRight(tree,nodes[m-1],r);
  fi;
  tree![3] := tree![3] - 1;
  AVLFreeNode(tree,l);
  
  # modify balance factors:
  # the subtree nodes[m-1] has become shorter at its left (resp. right)
  # subtree, if path[m-1]=-1 (resp. +1). We have to react according to
  # the BalFactor at this node and then up the tree, if the whole subtree
  # has shrunk:
  # (we decrement m and work until the corresponding subtree has not shrunk)
  m := m - 1;  # start work HERE
  while m >= 1 do
    if AVLBalFactor(tree,nodes[m]) = 0 then
      AVLSetBalFactor(tree,nodes[m],-path[m]);  # we made path[m] shorter
      return x;
    elif AVLBalFactor(tree,nodes[m]) = path[m] then
      AVLSetBalFactor(tree,nodes[m],0);         # we made path[m] shorter
    else    # tree is out of balance
      p := AVLRebalance(tree,nodes[m]);
      if m = 1 then
        tree![6] := p.newroot;
        return x;               # everything is done
      elif path[m-1] = -1 then
        AVLSetLeft(tree,nodes[m-1],p.newroot);
      else
        AVLSetRight(tree,nodes[m-1],p.newroot);
      fi;
      if not p.shorter then return x; fi;   # nothing happens further up
    fi;
    m := m - 1;
  od;
  return x;
end;
if IsBound(AVLIndexDelete_C) then
    InstallGlobalFunction(AVLIndexDelete, AVLIndexDelete_C);
else
    InstallGlobalFunction(AVLIndexDelete, AVLIndexDelete_GAP);
fi;


AVLToList_GAP := function(tree)
  # walks recursively through the tree and builds a list, where every entry 
  # belongs to a node in the order of the tree and each entry is a list, 
  # containing the data as first entry, the depth in the tree as second 
  # and the balance factor as third. Mainly for test purposes.

  local l, DoRecursion;
  
  l := EmptyPlist(tree![3]);
  
  DoRecursion := function(p,depth)
    # does the work
    if AVLLeft(tree,p) <> 0 then
      DoRecursion(AVLLeft(tree,p),depth+1);
    fi;
    Add(l,[AVLData(tree,p),depth,AVLBalFactor(tree,p)]);
    if AVLRight(tree,p) <> 0 then
      DoRecursion(AVLRight(tree,p),depth+1);
    fi;
  end;
  
  DoRecursion(tree![6],1);
  return l;
end;
if IsBound(AVLToList_C) then
    InstallGlobalFunction(AVLToList, AVLToList_C);
else
    InstallGlobalFunction(AVLToList, AVLToList_GAP);
fi;

BindGlobal( "AVLTest", function(tree)
  # walks recursively through the tree and tests its balancedness. Returns
  # the depth or the subtree where the tree is not balanced. Mainly for test
  # purposes. Returns tree if the NumberOfNodes is not correct.
  
  local error, DoRecursion, depth;
  
  error := false;
  
  DoRecursion := function(p)
    # does the work, returns false, if an error is detected in the subtree
    # and a list with the depth of the tree and the number of nodes in it.
    local ldepth, rdepth;
    
    if AVLLeft(tree,p) <> 0 then
      ldepth := DoRecursion(AVLLeft(tree,p));
      if ldepth = false then 
        return false;
      fi;
    else
      ldepth := [0,0];
    fi;
    if AVLRight(tree,p) <> 0 then
      rdepth := DoRecursion(AVLRight(tree,p));
      if rdepth = false then
        return false;
      fi;
    else
      rdepth := [0,0];
    fi;
    if AbsInt(rdepth[1]-ldepth[1]) > 1 or 
       AVLBalFactor(tree,p) <> rdepth[1]-ldepth[1] or
       AVLRank(tree,p) <> ldepth[2] + 1 then
      error := p;
      return false;
    else
      return [Maximum(ldepth[1],rdepth[1])+1,ldepth[2]+rdepth[2]+1];
    fi;
  end;
  
  if tree![6] = 0 then
    return rec( depth := 0, ok := true );
  else
    depth := DoRecursion(tree![6]);
    if depth = false then
      return rec( badsubtree := error, ok := false );  
                                # set from within DoRecursion
    else
      if depth[2] = tree![3] then
        return rec( depth := depth[1], ok := true );      
                    # Number of Nodes is correct!
      else
        return rec( badsubtree := tree![6], ok := false);
      fi;
    fi;
  fi;
end);

AVLFindIndex_GAP := function(tree,data)
  # Parameters: tree, data
  #  t is a AVL
  #  data is a data structure defined by the user
  # Searches in tree for a node equal to data, returns its index or fail
  # if not found.
  local compare, p, c, index;
  compare := tree![5];
  p := tree![6];
  index := 0;
  while p >= 8 do
    c := compare(data,AVLData(tree,p));
    if c = 0 then
      return index + AVLRank(tree,p);
    elif c < 0 then    # data < AVLData(tree,p)
      p := AVLLeft(tree,p);
    else               # data > AVLData(tree,p)
      index := index + AVLRank(tree,p);
      p := AVLRight(tree,p);
    fi;
  od;
  return fail;
end ;
if IsBound(AVLFindIndex_C) then 
    InstallGlobalFunction(AVLFindIndex, AVLFindIndex_C); 
else
    InstallGlobalFunction(AVLFindIndex, AVLFindIndex_GAP); 
fi;

InstallOtherMethod( ELM_LIST, "for an avl tree and an index",
  [ IsAVLTree and IsAVLTreeFlatRep, IsPosInt ],
  AVLIndex );

InstallOtherMethod( Position, "for an avl tree, an object, and an index",
  [ IsAVLTree and IsAVLTreeFlatRep, IsObject, IsInt ],
  function( t, x, pos )
    local i,j;
    i := AVLFindIndex(t,x);
    if i = fail or i <= pos then 
        return fail; 
    else
        return i;
    fi;
  end);

InstallOtherMethod( Remove, "for an avl tree and an index",
  [ IsAVLTree and IsAVLTreeFlatRep and IsMutable, IsPosInt ],
  AVLIndexDelete );

InstallOtherMethod( Remove, "for an avl tree",
  [ IsAVLTree and IsAVLTreeFlatRep and IsMutable ],
  function( t )
    return AVLIndexDelete(t,t![3]);
  end );

InstallOtherMethod( Length, "for an avl tree",
  [ IsAVLTree and IsAVLTreeFlatRep ],
  function( t )
    return t![3];
  end );
  
InstallOtherMethod( ADD_LIST, "for an avl tree and an object",
  [ IsAVLTree and IsAVLTreeFlatRep and IsMutable, IsObject ],
  function( t, x )
    AVLIndexAdd(t,x,true,t![3]+1);
  end );

InstallOtherMethod( ADD_LIST, "for an avl tree, an object and a position",
  [ IsAVLTree and IsAVLTreeFlatRep and IsMutable, IsObject, IsPosInt ],
  function( t, x, pos )
    AVLIndexAdd(t,x,true,pos);
  end );

InstallOtherMethod( IN, "for an object and an avl tree",
  [ IsObject, IsAVLTree and IsAVLTreeFlatRep ],
  function( x, t )
    return AVLFind(t,x) <> fail;
  end );

        
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <https://www.gnu.org/licenses/>.
##
