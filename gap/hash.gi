#############################################################################
##
##  hash.gi           orb package 
##                                                        by Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
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
             cangrow := true,  # flag, whether hash table may grow
             ishash := true,   # a "magic" entry
            );
end );

InstallGlobalFunction( NewHT, function(sample,len)
  local eqfun,hfun,ht;
  hfun := ChooseHashFunction(sample,len);
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
  local h;
  ht.accesses := ht.accesses + 1;
  if ht.nr * 10 > ht.len * 9 then
    if IsBound(ht.cangrow) then
      Info(InfoOrb,1,"Hash table too full, growing...");
      GrowHT(ht,x);
    else
      Info(InfoOrb,1,"Hash table too full, cannot grow...");
      return fail;
    fi;
  fi;
  h := ht.hf(x,ht.hfd);
  while IsBound(ht.els[h]) do
    h := h+1;
    ht.collisions := ht.collisions + 1;
    if h>ht.len then h:=1; fi;
    if not(IsBound(ht.alert)) and QuoInt(ht.collisions,ht.accesses) > 100 then
      # We have a problem!
      Info(InfoOrb,1,"Alarm: Collision warning: ",
           QuoInt(ht.collisions,ht.accesses),"!");
      if not(IsBound(ht.cangrow)) then
        Info(InfoOrb,1,"Alarm: Collision warning: ",
             QuoInt(ht.collisions,ht.accesses),"!");
        ht.alert := true;
      else
        GrowHT(ht,x);
        return AddHT(ht,x,val);
      fi;
    fi;
  od;
  ht.els[h] := x;
  if val <> true then ht.vals[h] := val; fi;
  ht.nr := ht.nr+1;
  return h;
end );

InstallGlobalFunction( ValueHT, function(ht, x)
  local h;
  ht.accesses := ht.accesses + 1;
  h := ht.hf(x,ht.hfd);
  while IsBound(ht.els[h]) do
    if ht.eqf(ht.els[h],x) then
        if IsBound(ht.vals[h]) then
            return ht.vals[h];
        else
            return true;
        fi;
    fi;
    h := h+1;
    ht.collisions := ht.collisions + 1;
    if h>ht.len then h:=1; fi;
  od;
  return fail;
end );

InstallGlobalFunction( GrowHT, function(ht,x)
  local i,oldels,oldlen,oldvals;

  Info(InfoOrb,1,"Growing hash table to length ",ht.len*2," !!!");

  oldels := ht.els;
  oldvals := ht.vals;
  oldlen := ht.len;

  ht.els := [];
  ht.vals := [];
  ht.len := ht.len * 2 + 1;
  ht.hf := ChooseHashFunction(x,ht.len);
  ht.hfd := ht.hf.data;
  ht.hf := ht.hf.func;
  ht.nr := 0;
  ht.collisions := 0;
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
  Info(InfoOrb,1,"Done.");
end );


# Here comes stuff for hash functions:

# First a few hash functions:

InstallGlobalFunction( ORB_HashFunctionForShortGF2Vectors,
function(v,data)
  return NumberFFVector(v,2) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForShort8BitVectors,
function(v,data)
  return NumberFFVector(v,data[2]) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForGF2Vectors,
function(v,data)
  return HASHKEY_BAG(v,101,8,data[2]) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionFor8BitVectors,
function(v,data)
  return HASHKEY_BAG(v,101,12,data[2]) mod data[1] + 1;
end );

# Now the choosing methods for compressed vectors:

InstallMethod( ChooseHashFunction, "for compressed gf2 vectors",
  [IsGF2VectorRep,IsInt],
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
  [Is8BitVectorRep,IsInt],
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
  for i in [1..Length(x)] do
      res := (res * 10001 + data[2].func(x[i],data[2].data)) mod data[1] + 1;
  od;
  return res mod data[1] + 1;
end );

InstallMethod( ChooseHashFunction, "for compressed gf2 matrices",
  [IsGF2MatrixRep,IsInt],
  function(p,hashlen)
    local data;
    data := [hashlen,ChooseHashFunction(p[1],hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallMethod( ChooseHashFunction, "for compressed 8bit matrices",
  [Is8BitMatrixRep,IsInt],
  function(p,hashlen)
    local data;
    data := [hashlen,ChooseHashFunction(p[1],hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntegers,
function(x,data)
  return x mod data[1] + 1;
end );

InstallMethod( ChooseHashFunction, "for integers", [IsInt,IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForIntegers, data := [hashlen] );
  end );
    
InstallGlobalFunction( ORB_HashFunctionForMemory,
function(x,data)
  return data[1](x!.el,data[2]);
end );

InstallMethod( ChooseHashFunction, "for memory objects", 
  [IsObjWithMemory, IsInt],
  function(p,hashlen)
    local hf;
    hf := ChooseHashFunction(p!.el,hashlen);
    return rec( func := ORB_HashFunctionForMemory, data := [hf.func,hf.data] );
  end );

InstallGlobalFunction( ORB_HashFunctionForPermutations,
function(p,data)
  local l;
  l:=LARGEST_MOVED_POINT_PERM(p);
  if IsPerm4Rep(p) then
    # is it a proper 4byte perm?
    if l>65536 then
      return HashKeyBag(p,255,0,4*l) mod data + 1;
    else
      # the permutation does not require 4 bytes. Trim in two
      # byte representation (we need to do this to get consistent
      # hash keys, regardless of representation.)
      TRIM_PERM(p,l);
    fi;
   fi;
   # now we have a Perm2Rep:
   return HashKeyBag(p,255,0,2*l) mod data + 1;
end );

InstallMethod( ChooseHashFunction, "for permutations", 
  [IsPerm, IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForPermutations, data := hashlen );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntList,
function(v,data)
  local i,res;
  res := 0;
  for i in v do
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

