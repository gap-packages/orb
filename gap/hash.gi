#############################################################################
##
##                             orb package
##  hash.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for fast hashing.
##
#############################################################################

InstallGlobalFunction( InitHT, function(len, hfun, eqfun)
  return rec(els := [],        # the elements to hash 
             vals := [],       # a value for each element, "true" not stored
             len := len,       # the length of the hash
             nr := 0,          # number of elements in hash
             hf := hfun.func,  # the hash function
             hfd := hfun.data, # data for the second argument to hf
             eqf := eqfun,     # a comparison function
             collisions := 0,  # number of collisions
             accesses := 0,    # number of accesses
             cangrow := false, # flag, whether hash table may grow
             ishash := true,   # a "magic" entry
            );
end );

InstallGlobalFunction( NewHT, function(sample,len)
  local eqfun,hfun,ht;
  hfun := ChooseHashFunction(sample,len);
  if hfun = fail then
      return fail;
  fi;
  eqfun := ApplicableMethod(\=,[sample,sample]);
  if eqfun = fail then eqfun := EQ; fi;
  if len < 11 then len := 11; fi;  # to avoid complete fillup! 
  ht := InitHT(len,hfun,eqfun);
  ht.cangrow := true;
  return ht;
end );

InstallMethod(ViewObj, "for hash tables", [IsRecord],
  function(ht)
    if IsBound(ht.ishash) and
       IsBound(ht.len) and IsBound(ht.nr) and IsBound(ht.els) and
       IsBound(ht.vals) and IsBound(ht.hf) and IsBound(ht.eqf) and
       IsBound(ht.collisions) and IsBound(ht.hfd) then
      # This is obviously a hash table
      Print("<hash table len=",ht.len," used=",ht.nr," colls=",
            ht.collisions," accs=",ht.accesses);
      if IsBound(ht.alert) then
          Print(" COLLISION ALERT!>");
      elif IsBound(ht.cangrow) then
          Print(" (can grow)>");
      else
          Print(">");
      fi;
    else
      TryNextMethod();
    fi;
  end);

InstallGlobalFunction( AddHT, function(ht, x, val)
  local h,g;
  ht.accesses := ht.accesses + 1;
  if ht.nr * 10 > ht.len * 8 then
    if IsBound(ht.cangrow) then
      Info(InfoOrb,3,"Hash table too full, growing...");
      GrowHT(ht,x);
    else
      Info(InfoOrb,1,"Hash table too full, cannot grow...");
      return fail;
    fi;
  fi;
  h := ht.hf(x,ht.hfd);
  if IsBound(ht.els[h]) then
    g := GcdInt(ht.len,h);
    if g = 1 then g := h; else g := 1; fi;
    repeat
      ht.collisions := ht.collisions + 1;
      h := h+g;
      if h>ht.len then h := h - ht.len; fi;
      if not(IsBound(ht.alert)) and QuoInt(ht.collisions,ht.accesses) > 100 then
        # We have a problem!
        Info(InfoOrb,1,"Alarm: Collision warning: Collisions: ",
             ht.collisions," Accesses: ",ht.accesses,"!");
        if not(IsBound(ht.cangrow)) then
          ht.alert := true;
        else
          GrowHT(ht,x);
          return AddHT(ht,x,val);
        fi;
      fi;
    until not(IsBound(ht.els[h]));
  fi;
  ht.els[h] := x;
  if val <> true then ht.vals[h] := val; fi;
  ht.nr := ht.nr+1;
  return h;
end );

InstallGlobalFunction( ValueHT, function(ht, x)
  local h,g;
  ht.accesses := ht.accesses + 1;
  h := ht.hf(x,ht.hfd);
  g := 0;
  while IsBound(ht.els[h]) do
    if ht.eqf(ht.els[h],x) then
        if IsBound(ht.vals[h]) then
            return ht.vals[h];
        else
            return true;
        fi;
    fi;
    if g = 0 then
      g := GcdInt(ht.len,h);
      if g = 1 then g := h; else g := 1; fi;
    fi;
    ht.collisions := ht.collisions + 1;
    h := h+g;
    if h>ht.len then h := h - ht.len; fi;
  od;
  return fail;
end );

InstallGlobalFunction( GrowHT, function(ht,x)
  local i,oldels,oldlen,oldvals;

  oldels := ht.els;
  oldvals := ht.vals;
  oldlen := ht.len;

  ht.els := [];
  ht.vals := [];
  ht.len := NextPrimeInt(ht.len * 2+1);
  Info(InfoOrb,2,"Growing hash table to length ",ht.len," !!!");
  if IsBound(ht.hfbig) and IsBound(ht.htdbig) then
      ht.hf := ORB_HashFunctionModWrapper;
      ht.hfd := [ht.hfbig,ht.hfdbig,ht.len];
  else
      ht.hf := ChooseHashFunction(x,ht.len);
      if ht.hf = fail then
          Error("Could not find hash function for sample object");
          return fail;
      fi;
      ht.hfd := ht.hf.data;
      ht.hf := ht.hf.func;
  fi;
  ht.nr := 0;
  ht.collisions := 0;
  ht.accesses := 0;
  # Now copy into new hash:
  for i in [1..oldlen] do
      if IsBound(oldels[i]) then
          if IsBound(oldvals[i]) then
              AddHT(ht,oldels[i],oldvals[i]);
          else
              AddHT(ht,oldels[i],true);
          fi;
      fi;
  od;
  Info(InfoOrb,3,"Done.");
end );

# The new interface for hashes:

InstallMethod( HTCreate, "for an object",
  [ IsObject ],
  function( x )
    return HTCreate(x,rec());
  end );

InstallMethod( HTCreate, "for an object and an options record",
  [ IsObject, IsRecord ],
  function( x, opt )
    local ht,ty,hfun,cangrow;
    ht := ShallowCopy(opt);
    if IsBound(ht.hashlen) then
        ty := HashTabType;
        ## JM: initial hash length should not be too small,
        ## so that the 80% rule in HTAdd is useful
        ht.len := Maximum(19,ht.hashlen);
    elif IsBound(ht.treehashsize) then
        ty := TreeHashTabType;
        ht.len := ht.treehashsize;
    elif IsBound(ht.treehashtab) then
        ty := TreeHashTabType;
        ht.len := 100003;
    elif IsBound(ht.hashtab) then
        ty := HashTabType;
        ht.len := 10007;
    else
        ty := TreeHashTabType;
        ht.len := 100003;
    fi;

    # Prevent the table from growing too large.
    cangrow := true;
    if GAPInfo.BytesPerVariable = 4 then
        if ht.len > 2^28-2 then
            ht.len := 2^28-57; # largest prime below 2^28
            cangrow := false;
        fi;
    else
        if ht.len > 2^60-2 then
            ht.len := 2^60-93; # largest prime below 2^60
            cangrow := false;
        fi;
    fi;

    ht.els := EmptyPlist(ht.len+1);
    ht.els[ht.len+1] := fail;   # To create proper length!
    ht.vals := [];
    ht.nr := 0;
    if IsBound(ht.forflatplainlists) then
        ht.hf := ORB_HashFunctionForPlainFlatList;
        ht.hfd := ht.len;
        ht.cangrow := cangrow;
    elif not(IsBound(ht.hf) and IsBound(ht.hfd)) then
        hfun := ChooseHashFunction(x,ht.len);
        if hfun = fail then
            Error("Could not find hash function for sample object");
            return fail;
        fi;
        ht.hf := hfun.func;
        ht.hfd := hfun.data;
        ht.cangrow := cangrow;
    else
        ht.cangrow := cangrow and IsBound(ht.hfbig) and IsBound(ht.hfdbig);
    fi;
    ht.collisions := 0;
    ht.accesses := 0;
    if IsIdenticalObj(ty,TreeHashTabType) and not(IsBound(ht.cmpfunc)) then
        ht.cmpfunc := AVLCmp;
    fi;
    if IsIdenticalObj(ty,HashTabType) and not(IsBound(ht.eqf)) then
        ht.eqf := EQ;
    fi;
    Objectify(ty,ht);
    return ht;
  end);

# We first to tree hashes and then standard hash tables:

InstallMethod(ViewObj, "for tree hash tables", 
  [IsHashTab and IsTreeHashTabRep],
  function(ht)
    Print("<tree hash table len=",ht!.len," used=",ht!.nr," colls=",
          ht!.collisions," accs=",ht!.accesses);
    if IsBound(ht!.alert) then
        Print(" COLLISION ALERT!>");
    else
        Print(">");
    fi;
  end);

BindGlobal("HT_Hash", function(ht, x)
    local h;
    h := ht!.hf(x,ht!.hfd);
    if h = fail or h = 0 then
        Error("hash function not applicable to key of type ", TNAM_OBJ(x));
    fi;
    if not IsInt(h) then
        Error("hash function should return small integer or the value 'fail', not a ",
              TNAM_OBJ(h));
    fi;
    # TODO: also do a bounds check?
    return h;
end);

InstallMethod( HTAdd, "for a tree hash table, an object and a value",
  [ IsTreeHashTabRep, IsObject, IsObject ],
  function(ht, x, val)
    local h,t,r;
    ht!.accesses := ht!.accesses + 1;
    if ht!.cangrow and ht!.nr > ht!.len * 10 then
        Info(InfoOrb,3,"Tree hash table too full, growing...");
        HTGrow(ht,x);
    fi;
    h := HT_Hash(ht,x);
    if not(IsBound(ht!.els[h])) then
        ht!.els[h] := x;
        if val <> true then ht!.vals[h] := val; fi;
        ht!.nr := ht!.nr+1;
        return h;
    fi;
    ht!.collisions := ht!.collisions + 1;
    t := ht!.els[h];
    if not(IsAVLTree(t)) then
        # Exactly one element there!
        t := AVLTree(rec(cmpfunc := ht!.cmpfunc, allocsize := 3));
        if IsBound(ht!.vals[h]) then
            AVLAdd(t,ht!.els[h],ht!.vals[h]);
            Unbind(ht!.vals[h]);
        else
            AVLAdd(t,ht!.els[h],true);
        fi;
        ht!.els[h] := t;
    fi;
    if val <> true then
        r := AVLAdd(t,x,val);
    else
        r := AVLAdd(t,x,true);
    fi;
    if r <> fail then 
        ht!.nr := ht!.nr + 1; 
        return h;
    else
        return fail;
    fi;
end );
if IsBound(HTAdd_TreeHash_C) then
    InstallMethod( HTAdd, 
      "for a tree hash table, an object and a value (C version)",
      [ IsTreeHashTabRep, IsObject, IsObject ], 1,
      HTAdd_TreeHash_C );
fi;


InstallMethod( HTValue, "for a tree hash table and an object",
  [ IsTreeHashTabRep, IsObject ],
  function(ht, x)
    local h,t;
    ht!.accesses := ht!.accesses + 1;
    h := HT_Hash(ht,x);
    if not(IsBound(ht!.els[h])) then
        return fail;
    fi;
    t := ht!.els[h];
    if not(IsAVLTree(t)) then
        if ht!.cmpfunc(x,t) = 0 then
            if IsBound(ht!.vals[h]) then
                return ht!.vals[h];
            else
                return true;
            fi;
        fi;
        return fail;
    fi;
    return AVLLookup(t,x);
end );
if IsBound(HTValue_TreeHash_C) then
    InstallMethod( HTValue, "for a tree hash table and an object (C version)",
      [ IsTreeHashTabRep, IsObject ], 1,
      HTValue_TreeHash_C );
fi;

InstallMethod( HTDelete, "for a tree hash table and an object",
  [ IsTreeHashTabRep, IsObject ],
  function(ht, x)
    local h,t,v;
    h := HT_Hash(ht,x);
    if not(IsBound(ht!.els[h])) then
        return fail;
    fi;
    t := ht!.els[h];
    if not(IsAVLTree(t)) then
        if ht!.cmpfunc(x,t) = 0 then
            if IsBound(ht!.vals[h]) then
                v := ht!.vals[h];
                Unbind(ht!.vals[h]);
            else
                v := true;
            fi;
            Unbind(ht!.els[h]);
            ht!.nr := ht!.nr - 1;
            return v;
        fi;
        return fail;
    fi;
    v := AVLDelete(t,x);
    if v <> fail then ht!.nr := ht!.nr - 1; fi;
    return v;
end );
if IsBound(HTDelete_TreeHash_C) then
    InstallMethod( HTDelete, "for a tree hash table and an object (C version)",
      [ IsTreeHashTabRep, IsObject ], 1,
      HTDelete_TreeHash_C );
fi;

InstallMethod( HTUpdate, "for a tree hash table and an object",
  [ IsTreeHashTabRep, IsObject, IsObject ],
  function( ht, x, v )
    local h,t,o;
    h := HT_Hash(ht,x);
    if not(IsBound(ht!.els[h])) then
        return fail;
    fi;
    t := ht!.els[h];
    if not(IsAVLTree(t)) then
        if ht!.cmpfunc(x,t) = 0 then
            if IsBound(ht!.vals[h]) then
                o := ht!.vals[h];
            else
                o := true;
            fi;
            ht!.vals[h] := v;
            return o;
        fi;
        return fail;
    fi;
    h := AVLFind(t,x);
    if h = fail then return fail; fi;
    o := AVLValue(t,h);
    AVLSetValue(t,h,v);
    return o;
end );
if IsBound(HTUpdate_TreeHash_C) then
    InstallMethod( HTUpdate, "for a tree hash table and an object (C version)",
      [ IsTreeHashTabRep, IsObject, IsObject ], 1,
      HTUpdate_TreeHash_C );
fi;

# Now standard hash tables with the new interface:

InstallMethod(ViewObj, "for hash tables", 
  [IsHashTab and IsHashTabRep],
  function(ht)
    Print("<hash table obj len=",ht!.len," used=",ht!.nr," colls=",
          ht!.collisions," accs=",ht!.accesses);
    if IsBound(ht!.alert) then
        Print(" COLLISION ALERT!>");
    elif IsBound(ht!.cangrow) then
        Print(" (can grow)>");
    else
        Print(">");
    fi;
  end);

InstallMethod(HTAdd, "for a hash table, an object and a value",
  [ IsHashTabRep, IsObject, IsObject ],
  function(ht, x, val)
    local h,g;
    ht!.accesses := ht!.accesses + 1;
    if ht!.nr * 10 > ht!.len * 8 then
      if IsBound(ht!.cangrow) then
        Info(InfoOrb,3,"Hash table too full, growing...");
        HTGrow(ht,x);
      else
        Info(InfoOrb,1,"Hash table too full, cannot grow...");
        return fail;
      fi;
    fi;
    h := HT_Hash(ht,x);
    if IsBound(ht!.els[h]) then
      g := GcdInt(ht!.len,h);
      if g = 1 then g := h; else g := 1; fi;
      repeat
        ht!.collisions := ht!.collisions + 1;
        h := h+g;
        if h>ht!.len then h := h - ht!.len; fi;
        if not(IsBound(ht!.alert)) and 
           QuoInt(ht!.collisions,ht!.accesses) > 100 then
          # We have a problem!
          Info(InfoOrb,1,"Alarm: Collision warning: Collisions: ",
               ht!.collisions," Accesses: ",ht!.accesses,"!");
          if not(IsBound(ht!.cangrow)) then
            ht!.alert := true;
          else
            HTGrow(ht,x);
            return HTAdd(ht,x,val);
          fi;
        fi;
      until not(IsBound(ht!.els[h]));
    fi;
    ht!.els[h] := x;
    if val <> true then ht!.vals[h] := val; fi;
    ht!.nr := ht!.nr+1;
    return h;
end );

InstallMethod( HTValue, "for a hash table and an object",
  [ IsHashTabRep, IsObject ],
  function(ht, x)
    local h,g;
    ht!.accesses := ht!.accesses + 1;
    h := HT_Hash(ht,x);
    g := 0;
    while IsBound(ht!.els[h]) do
      if ht!.eqf(ht!.els[h],x) then
          if IsBound(ht!.vals[h]) then
              return ht!.vals[h];
          else
              return true;
          fi;
      fi;
      if g = 0 then
        g := GcdInt(ht!.len,h);
        if g = 1 then g := h; else g := 1; fi;
      fi;
      ht!.collisions := ht!.collisions + 1;
      h := h+g;
      if h>ht!.len then h := h - ht!.len; fi;
      if not(IsBound(ht!.alert)) and
         QuoInt(ht!.collisions,ht!.accesses) > 100 then
         # We have a problem!
        Info(InfoOrb,1,"HTValue Alarm: Collision warning: Collisions: ",
                       ht!.collisions," Accesses: ",ht!.accesses,"!");
        ht!.alert := true;
      fi;
      ##
    od;
    return fail;
end );

InstallMethod( HTDelete, "for a hash table and an object",
  [ IsHashTabRep, IsObject ],
  function( ht, x )
    Error("Hash tables do not support HTDelete, use a tree hash table");
    return fail;
  end );

InstallMethod( HTUpdate, "for a hash table, an object and a value",
  [ IsHashTabRep, IsObject, IsObject ],
  function( ht, x, v )
    local old,h,g;

    ht!.accesses := ht!.accesses + 1;
    h := HT_Hash(ht,x);
    g := 0;
    while IsBound(ht!.els[h]) do
      if ht!.eqf(ht!.els[h],x) then
          if IsBound(ht!.vals[h]) then
              old := ht!.vals[h];
              ht!.vals[h] := v;
              return old;
          else
              ht!.vals[h] := v;
              return true;
          fi;
      fi;
      if g = 0 then
        g := GcdInt(ht!.len,h);
        if g = 1 then g := h; else g := 1; fi;
      fi;
      ht!.collisions := ht!.collisions + 1;
      h := h+g;
      if h>ht!.len then h := h - ht!.len; fi;
    od;
    return fail;
end );

InstallMethod( HTGrow, "for a tree hash table and an object",
  [ IsTreeHashTabRep, IsObject],
  function(ht,x)
    local i,j,oldels,oldlen,oldvals,pos,t;
    oldels := ht!.els;
    oldvals := ht!.vals;
    oldlen := ht!.len;

    ht!.len := NextPrimeInt(ht!.len * 20+1);
    Info(InfoOrb,2,"Growing tree hash table to length ",ht!.len," !!!");
    ht!.els := EmptyPlist(ht!.len+1);
    ht!.els[ht!.len+1] := fail;
    ht!.vals := [];
    if IsBound(ht!.forflatplainlists) then
        ht!.hfd := ht!.len;
    elif IsBound(ht!.hfbig) and IsBound(ht!.htdbig) then
        ht!.hf := ORB_HashFunctionModWrapper;
        ht!.hfd := [ht!.hfbig,ht!.hfdbig,ht!.len];
    else
        ht!.hf := ChooseHashFunction(x,ht!.len);
        if ht!.hf = fail then
            Error("Could not find hash function for sample object");
            return fail;
        fi;
        ht!.hfd := ht!.hf.data;
        ht!.hf := ht!.hf.func;
    fi;
    ht!.nr := 0;
    ht!.collisions := 0;
    ht!.accesses := 0;
    # Now copy into new hash:
    for i in [1..oldlen] do
        if IsBound(oldels[i]) then
            t := oldels[i];
            if not(IsAVLTree(t)) then
                if IsBound(oldvals[i]) then
                    HTAdd(ht,t,oldvals[i]);
                else
                    HTAdd(ht,t,true);
                fi;
            else
                for j in [1..Length(t)] do
                    pos := AVLIndexFind(t,j);
                    HTAdd(ht,AVLData(t,pos),AVLValue(t,pos));
                od;
            fi;
        fi;
    od;
  end );

InstallMethod( HTGrow, "for a hash table and an object",
  [ IsHashTabRep, IsObject ],
function(ht,x)
  local i,oldels,oldlen,oldvals;

  oldels := ht!.els;
  oldvals := ht!.vals;
  oldlen := ht!.len;

  ht!.els := [];
  ht!.vals := [];
  ht!.len := NextPrimeInt(ht!.len * 2+1);
  Info(InfoOrb,2,"Growing hash table to length ",ht!.len," !!!");
  if IsBound(ht!.forflatplainlists) then
      ht!.hfd := ht!.len;
  elif IsBound(ht!.hfbig) and IsBound(ht!.hfdbig) then
      ht!.hf := ORB_HashFunctionModWrapper;
      ht!.hfd := [ht!.hfbig,ht!.hfdbig,ht!.len];
  else
      ht!.hf := ChooseHashFunction(x,ht!.len);
      ht!.hfd := ht!.hf.data;
      ht!.hf := ht!.hf.func;
  fi;
  ht!.nr := 0;
  ht!.collisions := 0;
  ht!.accesses := 0;
  # Now copy into new hash:
  for i in [1..oldlen] do
      if IsBound(oldels[i]) then
          if IsBound(oldvals[i]) then
              HTAdd(ht,oldels[i],oldvals[i]);
          else
              HTAdd(ht,oldels[i],true);
          fi;
      fi;
  od;
  Info(InfoOrb,3,"Done.");
end );


# First a few hash functions:

InstallGlobalFunction( ORB_HashFunctionForShortGF2Vectors,
function(v,data)
  local x;
  x := NumberFFVector(v,2);
  if x = fail then return fail; fi;
  return x mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForShort8BitVectors,
function(v,data)
  local x;
  x := NumberFFVector(v,data[2]);
  if x = fail then return fail; fi;
  return x mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForGF2Vectors,
function(v,data)
  if not IsGF2VectorRep(v) then return fail; fi;
  return HashKeyBag(v,101,2*GAPInfo.BytesPerVariable,data[2]) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionFor8BitVectors,
function(v,data)
  if not Is8BitVectorRep(v) then return fail; fi;
  # TODO: check q
  return HashKeyBag(v,101,3*GAPInfo.BytesPerVariable,data[2]) mod data[1] + 1;
end );

InstallMethod( ChooseHashFunction, "failure method",
  [IsObject,IsInt],
  function(p,hashlen)
    return fail;
  end );

# Now the choosing methods for compressed vectors:

InstallMethod( ChooseHashFunction, "for compressed gf2 vectors",
  [IsGF2VectorRep and IsList,IsInt],
  function(p,hashlen)
    local bytelen;
    bytelen := QuoInt(Length(p),8);
    # Note that unfortunately gf2 vectors are not "clean" after their
    # "official" length, therefore we *must not* use the last, half-used
    # byte. This inevitably leads to collisions!
    if bytelen <= 8 then
        return rec( func := ORB_HashFunctionForShortGF2Vectors,
                    data := [hashlen] );
    else
        return rec( func := ORB_HashFunctionForGF2Vectors,
                    data := [hashlen,bytelen] );
    fi;
  end );

InstallMethod( ChooseHashFunction, "for compressed 8bit vectors",
  [Is8BitVectorRep and IsList,IsInt],
  function(p,hashlen)
    local bytelen,i,q,qq;
    q := Q_VEC8BIT(p);
    qq := q;
    i := 0;
    while qq <= 256 do
        qq := qq * q;
        i := i + 1;
    od;
    # i is now the number of field elements per byte
    bytelen := QuoInt(Length(p),i);
    # Note that unfortunately 8bit vectors are not "clean" after their
    # "official" length, therefore we *must not* use the last, half-used
    # byte. This inevitably leads to collisions!
    if bytelen <= 8 then
        return rec( func := ORB_HashFunctionForShort8BitVectors,
                    data := [hashlen,q] );
    else
        return rec( func := ORB_HashFunctionFor8BitVectors,
                    data := [hashlen,bytelen] );
    fi;
  end );

InstallGlobalFunction( ORB_HashFunctionForCompressedMats,
function(x,data)
  local i,res;
  res := 0;
  if not IsGF2MatrixRep(x) and not Is8BitMatrixRep(x) then
    return fail;
  fi;
  for i in [1..Length(x)] do
      res := (res * data[3] + data[2].func(x[i],data[2].data)) mod data[1];
  od;
  return res + 1;
end );

InstallMethod( ChooseHashFunction, "for compressed gf2 matrices",
  [IsGF2MatrixRep and IsList,IsInt],
  function(p,hashlen)
    local data;
    data := [hashlen,ChooseHashFunction(p[1],hashlen),
             PowerMod(2,Length(p[1]),hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallMethod( ChooseHashFunction, "for compressed 8bit matrices",
  [Is8BitMatrixRep and IsList,IsInt],
  function(p,hashlen)
    local data,q;
    q := Q_VEC8BIT(p[1]);
    data := [hashlen,ChooseHashFunction(p[1],hashlen),
             PowerMod(q,Length(p[1]),hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntegers,
function(x,data)
  if not IsInt(x) then return fail; fi;
  return x mod data[1] + 1;
end );

InstallMethod( ChooseHashFunction, "for integers", [IsInt,IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForIntegers, data := [hashlen] );
  end );
    
InstallGlobalFunction( ORB_HashFunctionForMemory,
function(x,data)
  if not IsObjWithMemory(x) then return fail; fi;
  return data[1](x!.el,data[2]);
end );

InstallMethod( ChooseHashFunction, "for memory objects", 
  [IsObjWithMemory, IsInt],
  function(p,hashlen)
    local hf;
    hf := ChooseHashFunction(p!.el,hashlen);
    return rec( func := ORB_HashFunctionForMemory, data := [hf.func,hf.data] );
  end );

if not IsBound(ORBC) then
  # our kernel extension was not loaded: use a hack to set ORBC.PERM_HASH_SKIP
  ORBC := rec( PERM_HASH_SKIP := SIZE_OBJ( () ) );
fi;

InstallGlobalFunction( ORB_HashFunctionForPermutations,
function(p,data)
  local l;
  if not IsPerm(p) then return fail; fi;
  l:=LARGEST_MOVED_POINT_PERM(p);
  if IsPerm4Rep(p) then
    # is it a proper 4byte perm?
    if l>65536 then
      return HashKeyBag(p,255,ORBC.PERM_HASH_SKIP,4*l) mod data + 1;
    else
      # the permutation does not require 4 bytes. Trim in two
      # byte representation (we need to do this to get consistent
      # hash keys, regardless of representation.)
      TRIM_PERM(p,l);
    fi;
  fi;
  if IsPerm2Rep(p) then
    return HashKeyBag(p,255,ORBC.PERM_HASH_SKIP,2*l) mod data + 1;
  fi;
  return fail;
end );

InstallGlobalFunction( ORB_HashFunctionForPlainFlatList,
  function( x, data )
    if not IsPlistRep(x) then
      return fail;
    fi;
    return (HashKeyBag( x, 0, 0, 
                        GAPInfo.BytesPerVariable*(Length(x)+1)) mod data)+1;
  end );

if IsBound(HASH_FUNC_FOR_TRANS) then
  InstallGlobalFunction( ORB_HashFunctionForTransformations, HASH_FUNC_FOR_TRANS);
elif IsBound(IsTrans2Rep) and IsBound(IsTrans4Rep) then 
  InstallGlobalFunction( ORB_HashFunctionForTransformations, 
  function(t, data)
    local deg;
      if not IsTransformation(x) then return fail; fi;
      deg:=DegreeOfTransformation(t);
      if IsTrans4Rep(t) then
        if deg<=65536 then 
          TrimTransformation(t, deg);
        else
          return HashKeyBag(t,255,0,4*deg) mod data + 1; 
        fi;
      fi;
      if IsTrans2Rep(t) then
        return HashKeyBag(t,255,0,2*deg) mod data + 1;
      fi;
      return fail;
  end);
else
  InstallGlobalFunction( ORB_HashFunctionForTransformations,
    function(t,data)
      if not IsTransformation(x) then return fail; fi;
      return ORB_HashFunctionForPlainFlatList(t![1],data);
    end );
fi;

InstallGlobalFunction( MakeHashFunctionForPlainFlatList,
  function( len )
    return rec( func := ORB_HashFunctionForPlainFlatList,
                data := len );
  end );

InstallMethod( ChooseHashFunction, "for permutations", 
  [IsPerm, IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForPermutations, data := hashlen );
  end );

InstallMethod( ChooseHashFunction, "for transformations",
  [IsTransformation, IsInt],
  function(t,hashlen)
    return rec( func := ORB_HashFunctionForTransformations, data := hashlen );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntList,
function(v,data)
  local i,res;
  res := 0;
  for i in v do
      if not IsInt(i) then return fail; fi;
      res := (res * data[1] + i) mod data[2];
  od;
  return res+1;
end );

InstallMethod( ChooseHashFunction, "for short int lists",
  [IsList, IsInt],
  function(p,hashlen)
    if ForAll(p,IsInt) then
        return rec(func := ORB_HashFunctionForIntList, data := [101,hashlen]);
    fi;
    TryNextMethod();
  end );

InstallGlobalFunction( ORB_HashFunctionForNBitsPcWord,
function(v,data)
  return ORB_HashFunctionForIntList(ExtRepOfObj(v),data);
end );

InstallMethod( ChooseHashFunction, "for N bits Pc word rep",
  [IsNBitsPcWordRep, IsInt],
  function(p,hashlen)
    return rec(func := ORB_HashFunctionForNBitsPcWord, data := [101,hashlen]);
  end );

InstallGlobalFunction( ORB_HashFunctionModWrapper,
  function(p,data)
    return data[1](p,data[2]) mod data[3] + 1;
  end );

InstallGlobalFunction( ORB_HashFunctionForMatList,
  function(ob,data)
    local i,m,res;
    res := 0;
    for m in ob do
        res := (res * data[1] + data[3].func(m,data[3].data)) mod data[2];
    od;
    return res+1;
  end );
    
InstallMethod( ChooseHashFunction, "for lists of matrices",
  [IsList, IsInt],
  function( l, hashlen )
    # FIXME:
    local r;
    if ForAll(l,IsMatrix) then
        r := ChooseHashFunction( l[1], hashlen );
        return rec( func := ORB_HashFunctionForMatList, 
                    data := [101,hashlen,r] );
    fi;
    TryNextMethod();
  end );

InstallMethod( ChooseHashFunction, 
  "for finite field vectors over big finite fields",
  [IsList, IsInt],
  function( l, hashlen )
    local f,q;
    if NestingDepthA(l) = 1 and Length(l) > 0 and IsFFE(l[1]) then
        f := Field(l);
        q := Size(f);
        return rec( func := ORB_HashFunctionForShort8BitVectors,
                    data := [hashlen,q] );
    fi;
    TryNextMethod();
  end );

if IsBound(HASH_FUNC_FOR_PPERM) then 
    InstallGlobalFunction( ORB_HashFunctionForPartialPerms, HASH_FUNC_FOR_PPERM);
elif IsBound(IsPPerm2Rep) and IsBound(IsPPerm4Rep) then
  InstallGlobalFunction( ORB_HashFunctionForPartialPerms, 
  function(t, data)
    local codeg;
    if not IsPartialPerm(x) then return fail; fi;
    if IsPPerm4Rep(t) then 
      codeg:=CodegreeOfPartialPerm(t);
      if codeg<65536 then 
        TrimPartialPerm(t);
      else
        return HashKeyBag(t,255,4,4*DegreeOfPartialPerm(t)) mod data + 1;
      fi;
    fi;
    if IsPPerm2Rep(t) then
      return HashKeyBag(t,255,2,2*DegreeOfPartialPerm(t)) mod data + 1;
    fi;
    return fail;
  end);
fi;

if IsBound(IsPartialPerm) then
  InstallMethod( ChooseHashFunction, "for partial perms",
    [IsPartialPerm, IsInt],
    function(t,hashlen)
      return rec( func := ORB_HashFunctionForPartialPerms, data := hashlen );
    end );
fi;

if not IsBound(HASH_FUNC_FOR_BLIST) then 
  BindGlobal("HASH_FUNC_FOR_BLIST", 
  function(blist, data)
    local h, x;
    if not IsBlistRep(blist) then return fail; fi;
    h := 0;
    for x in blist do
      if x then
        h := h * 2 + 1;
      else
        h := h * 2;
      fi;
    od;
    return h mod data + 1;
  end);
fi;

InstallMethod(ChooseHashFunction, "for a blist and pos int",
[IsBlistRep, IsPosInt],
  function(x, hashlen)
  return rec(func := HASH_FUNC_FOR_BLIST,
             data := hashlen);
end);

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
