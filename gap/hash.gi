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
  hfun := MakeHashFunction(sample,len);
  eqfun := ApplicableMethod(\=,[sample,sample]);
  if eqfun = fail then eqfun := CVEC_EQINT; fi;
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
  ht.hf := MakeHashFunction(x,ht.len);
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

