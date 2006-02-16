#############################################################################
##
##  byorbits.gi              orb package                      
##                                                           Max Neunhoeffer
##                                                              Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Implementation stuff for fast orbit enumeration by suborbits.
##
#############################################################################

###########################
# A few helper functions: #
###########################

InstallGlobalFunction( ORB_PrettyStringBigNumber,
function(n)
  local e,i,s;
  if n < 0 then
    e := "-";
    n := -n;
  else
    e := "";
  fi;
  s := String(n);
  i := 1;
  while i < Length(s) do
    Add(e,s[i]);
    if (Length(s)-i) mod 3 = 0 then
      Add(e,' ');
    fi;
    i := i + 1;
  od;
  Add(e,s[i]);
  return e;
end );

InstallGlobalFunction( ORB_InvWord, 
function(w)
  local wi,l,i;
  # Inverts w by changing the sign and reversing
  wi := ShallowCopy(w);
  l := Length(w);
  for i in [1..l] do
    wi[l+1-i] := - w[i];
  od;
  return wi;
end );

InstallGlobalFunction( ORB_ApplyWord, 
function(p,w,l,li,op)
  # p is a point, w is a word given as a list of small integers which are
  # indices in the list l or negatives of indices in the list li,
  # which is a list of group elements g, for which
  # op(p,g) is defined. 
  # Example: ORB_ApplyWord(p,[1,-2,3,2],[g1,g2,g3],[g1^-1,g2^-1,g3^-1],OnRight) 
  #          = p*g1*(g2^-1)*g3*g2
  local i;
  for i in w do
    if i < 0 then
      p := op(p,li[-i]);
    else
      p := op(p,l[i]);
    fi;
  od;
  return p;
end );

#########################
# Stabilizer Iterators: #
#########################

InstallMethod( StabIterator, "without arguments", [],  
  function( )
    local stab;
    stab := rec();
    Objectify( StdStabIteratorsType, stab );
    return stab;
  end );

InstallMethod( ViewObj, "for a stab iterator", 
  [ IsStabIterator and IsStdStabIteratorRep ],
  function( stab )
    if not(IsBound(stab!.i)) then
        Print("<newly born stab iterator>");
    else
        Print("<stabiterator size=",stab!.info," position=",stab!.pos,">");
    fi;
  end );

InstallMethod( Reset, "for a stab iterator",
  [ IsStabIterator and IsStdStabIteratorRep ],
function(stab)
  if not(IsBound(stab!.i)) then
      Error("StabIterator is not yet initialized");
      return;
  fi;
  stab!.pos := List(stab!.info,x->1);
  stab!.point := List([1..stab!.i],x->[]);
  stab!.cache := List([1..stab!.i],x->[]);
end );

InstallOtherMethod( Size, "for a std stab iterator",
  [ IsStabIterator and IsStdStabIteratorRep ],
  function( stab )
    return Product( stab!.info, Length );
  end );

InstallMethod( Next, "for a stab iterator",
  [ IsStabIterator and IsStdStabIteratorRep ],
function(stab)
  local i;
  i := 1;
  while true do
    if i > Length(stab!.pos) then
      return true;   # finished with this iterator
    fi;
    stab!.pos[i] := stab!.pos[i]+1;
    stab!.point[i] := [];    # this is no longer valid
    if stab!.pos[i] <= Length(stab!.info[i]) then
      return false;  # next element found
    fi;
    stab!.pos[i] := 1;
    i := i + 1;
  od;
  # this is never reached
end );

InstallGlobalFunction( ORB_ApplyStabElement,
function(p,j,i,setup,stab,w)
  local ww,www;
  while true do
    if i > 1 and IsBound(stab!.point[i][j]) and p = stab!.point[i][j] then
      if IsList(w) then Append(w,stab!.cache[i][j][1]); fi;
      p := stab!.cache[i][j][2];
    else
      stab!.point[i][j] := p;

      ww := ShallowCopy(setup!.trans[i][stab!.info[i][stab!.pos[i]]]);
      if IsList(w) then Append(w,ww); fi;
      p := ORB_ApplyWord(p,ww,setup!.els[j],setup!.elsinv[j],setup!.op[j]);
      if i = 1 then
        return p;
      fi;
      www := [];
      p := ORB_Minimalize(p,j,i,setup,false,www);
      Append(ww,www);
      if IsList(w) then Append(w,www); fi;

      stab!.cache[i][j] := [ww,p];
    fi;
    i := i - 1;
  od;
  # never comes here
end );


############################################
# The heart of the method: Minimalization: #
############################################

InstallMethod( ViewObj, "for an orbit-by-suborbit setup object",
  [ IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep ],
  function( setup )
    Print("<setup for an orbit-by-suborbit enumeration, k=",setup!.k,">");
  end );

InstallMethod( Memory, "for an orbit-by-suborbit setup object",
  [ IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep ],
  function( setup )
    local k,m,p,i;
    k := setup!.k;
    m := 0;
    for i in [1..k] do
        p := SHALLOW_SIZE(setup!.sample[i]) + 3 * GAPInfo.BytesPerVariable;
        m := m + p * setup!.info[i].nr + 2 * SHALLOW_SIZE(setup!.info[i].els);
    od;
    return m;
  end );

InstallGlobalFunction( ORB_Minimalize,
function(p,j,i,setup,stab,w)
  # p is a point that should be minimalized. j is in 1..k+1 and indicates,
  # whether p is in P_j or in P (for j=k+1). i is in 1..k and indicates, 
  # which group U_i is used to minimalize. So only i <= j makes sense.
  # setup is a record describing the helper subgroups as defined above. 
  # Returns a U_i-minimal point q in the same U_i-orbit pU_i.
  # If stab is "false" nothing happens. If stab is a stabiterator object,
  # (usually will be "newly born" thus empty), that will
  # be filled with information for an iterator object for 
  # Stab_{U_i}(pi[j][i](q)) (see above).
  # If w is a list the word which is applied is appended.
  local m,minpoint,minstab,minstablen,minword,n,oldp,q,qq,qqq,ret,stablen,
        tempstab,v,vv,vvv,ww,www;
#Print("Mini:",j," ",i,"\n");
  oldp := p;  # to make debugging easier!
  if i = 1 then    # this is the smallest helper subgroup

    # go to P_1:
    if j > i then 
      q := setup!.pifunc[j][i](p,setup!.pi[j][i]); 
    else
      q := p;
    fi;
    v := ValueHT(setup!.info[i],q);
    if v = fail then    # we do not yet know this point
      ###Print("<\c");
      # we have to enumerate this U_1-orbit, apply all elements in trans:
      v := [];  # here we collect the stabilizer
      for m in [1..setup!.index[i]] do
        qq := ORB_ApplyWord(q,ORB_InvWord(setup!.trans[i][m]),
                        setup!.els[i],setup!.elsinv[i],setup!.op[i]);
        if q = qq then   # we found a stabilizer element:
          Add(v,m);
        else
          vv := ValueHT(setup!.info[i],qq);
          if vv = fail then    # we did not yet reach this point
            AddHT(setup!.info[i],qq,m);   # store this info
          fi;
        fi;
      od;
      AddHT(setup!.info[i],q,v);
      setup!.suborbnr[i] := setup!.suborbnr[i] + 1;
      setup!.sumstabl[i] := setup!.sumstabl[i] + Length(v);
      ###Print(Length(v),":",QuoInt(setup!.sumstabl[i],
      ###      setup!.suborbnr[i]),">   \r");

      # now p is by definition U_1-minimal
    else    # we already know this U_1-orbit:
      if IsInt(v) then   # this is the number of a word
        if IsList(w) then Append(w,setup!.trans[i][v]); fi;  # store what we do
        p := ORB_ApplyWord(p,setup!.trans[i][v],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        if j > i then
          q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[i],q);
      fi; # otherwise we are already U_1-minimal:
    fi;
    if IsStabIterator(stab) then
        stab!.i := 1;
        stab!.info := [v];
        stab!.pos := [1];
    fi;
#Print("Raus\n");
    return p;

  else   # we are in some higher helper subgroup than U_1:

    # first do a U_{i-1}-minimalization:
    p := ORB_Minimalize(p,j,i-1,setup,stab,w);

    # now try to reach the minimal U_{i-1}-suborbit in the U_i-orbit:
    if j > i then
      q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
    else
      q := p;
    fi;
    v := ValueHT(setup!.info[i],q);

    if v = fail then    # we do not yet know this U_{i-1}-suborbit

      ###Print("<",i,":\c");
      # first we apply all elements of the transversal of U_{i-1} in U_i,
      # U_{i-1}-minimalize them and search for the smallest stabilizer
      # to choose the U_i-minimal U_{i-1}-orbit:
      minpoint := fail;
      minword := fail;
      minstablen := -1;
      minstab := fail;
      for m in [1..setup!.index[i]] do
        tempstab := StabIterator();
        qq := ORB_ApplyWord(q,setup!.trans[i][m],setup!.els[i],
                            setup!.elsinv[i],setup!.op[i]);
        ww := ShallowCopy(setup!.trans[i][m]);
        qq := ORB_Minimalize(qq,i,i-1,setup,tempstab,ww);
        stablen := Product(tempstab!.info,Length);
        if minpoint = fail or stablen < minstablen then
          minpoint := qq;
          minstablen := stablen;
          minword := ww;
          minstab := tempstab;
        fi;
      od;
      # Now U_i-minimalize p:
      p := ORB_ApplyWord(p,minword,setup!.els[j],setup!.elsinv[j],setup!.op[j]);
      if IsList(w) then Append(w,minword); fi;
      q := minpoint;
      # in the second component we have to collect stabilizing transversal 
      # elements for subgroups U_1 to U_i:
      v := [true,List([1..i],x->[])];  
      AddHT(setup!.info[i],q,v);
                        
      # Now the U_{i-1}-orbit of the vector q is the U_i-minimal 
      # U_{i-1}-orbit and q is the U_i-minimal vector
      
      # first find all U_{i-1}-minimal elements in the U_i-minimal 
      # U_{i-1}-orbit:
      Reset(minstab);
      repeat
        ww := [];
        qq := ORB_ApplyStabElement(q,i,i-1,setup,minstab,ww);
        if qq <> q then   # some new U_{i-1}-minimal element?
          vv := ValueHT(setup!.info[i],qq);
          if vv = fail then
            AddHT(setup!.info[i],qq,[false,ORB_InvWord(ww)]);
          fi;
        else   # in this case this is an element of Stab_{U_{i-1}}(q) in P_i
          for n in [1..i-1] do
            AddSet(v[2][n],minstab!.info[n][minstab!.pos[n]]);
          od;
        fi;
      until Next(minstab);
      
      # we have to enumerate this U_i-orbit by U_{i-1}-orbits, storing
      # information for all U_{i-1}-minimal vectors:
      tempstab := StabIterator();
      for m in [1..setup!.index[i]] do
        # Apply t to find other U_{i-1}-orbits
        qq := ORB_ApplyWord(q,setup!.trans[i][m],setup!.els[i],
                            setup!.elsinv[i],setup!.op[i]);
        ww := ShallowCopy(setup!.trans[i][m]);
        qq := ORB_Minimalize(qq,i,i-1,setup,tempstab,ww);
        vv := ValueHT(setup!.info[i],qq);
        if vv <> fail and not(IsInt(vv)) then  
          # we are again in the U_i-minimal U_{i-1}-o.
          # then m has to go in the stabilizer info:
          Add(v[2][i],m);
        fi;
        if vv = fail then   # a new U_{i-1}-orbit
          # note that we now have stabilizer info in tempstab
          ret := setup!.cosetrecog[i](i,ORB_InvWord(ww),setup);
          AddHT(setup!.info[i],qq,ret);
          Reset(tempstab);
          repeat
            www := ShallowCopy(ww);
            qqq := ORB_ApplyStabElement(qq,i,i-1,setup,tempstab,www);
            vvv := ValueHT(setup!.info[i],qqq);
            if vvv = fail then
              ret := setup!.cosetrecog[i](i,ORB_InvWord(www),setup);
              AddHT(setup!.info[i],qqq,ret);
            fi;
          until Next(tempstab);
        fi;
      od;
      # now q is by definition the U_i-minimal point in the orbit and
      # v its setup!.info[i], i.e. [true,stabilizer information]
      setup!.suborbnr[i] := setup!.suborbnr[i] + 1;
      setup!.sumstabl[i] := setup!.sumstabl[i] + Product(v[2],Length);
      ###Print(Product(v[2],Length),":",
      ###      QuoInt(setup!.sumstabl[i],setup!.suborbnr[i]),">      \r");

    else   # we already knew this U_{i-1}-suborbit

      if IsInt(v) then    # this is the number of a word
        if IsList(w) then 
          Append(w,setup!.trans[i][v]);   # remember what we did
        fi; 
        p := ORB_ApplyWord(p,setup!.trans[i][v],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        # we again do a U_{i-1}-minimalization:
        p := ORB_Minimalize(p,j,i-1,setup,stab,w);
        # now we are in the U_i-minimal U_{i-1}-suborbit and on a 
        # U_{i-1}-minimal element
        if j > i then
          q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[i],q);
      fi;
      if v[1] = false then    # not yet U_i-minimal 
        # we still have to apply an element of Stab_{U_{i-1}}(pi[j][i-1](p)):
        if IsList(w) then Append(w,v[2]); fi;  # remember what we did
        p := ORB_ApplyWord(p,v[2],setup!.els[j],setup!.elsinv[j],setup!.op[j]);
        if j > i then
          q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[i],q);
      fi;
      # now q is the U_i-minimal element in qU_i
      # v is now [true,stabilizer information]

    fi;

    if IsStabIterator(stab) then
        stab!.i := i;
        stab!.info := v[2];
        stab!.pos := List([1..i],x->1);
    fi;

    # now we are on the minimal element in the S-orbit
#Print("raus\n");
    return p;
  fi;
end );

InstallGlobalFunction( ORB_StoreWordInCache,
function(setup,w)
  local v;
  v := ValueHT(setup!.wordhash,w);
  if v = fail then
      Add(setup!.wordcache,w);
      v := Length(setup!.wordcache);
      AddHT(setup!.wordhash,w,v);
  fi;
  return v;
end );

InstallGlobalFunction( ORB_Minimalize2,
function(p,j,i,setup,stab,w)
  # p is a point that should be minimalized. j is in 1..k+1 and indicates,
  # whether p is in P_j or in P (for j=k+1). i is in 1..k and indicates, 
  # which group U_i is used to minimalize. So only i <= j makes sense.
  # setup is a record describing the helper subgroups as defined above. 
  # Returns a U_i-minimal point q in the same U_i-orbit pU_i.
  # If stab is "false" nothing happens. If stab is a record
  # (usually will be "newly born" thus empty), then words for
  # generators of Stab_{U_i}(min(pi_i(p)) will be stored in stab.gens
  # and its size in stab.size.
  # If w is a list the word which is applied is appended.
  local cos,m,mm,o,oldp,oo,q,qq,tempstab,tempstabgens,v,ww,www;
  
  #Print("Entering ORB_Minimalize2 j=",j," i=",i,"\n");
  oldp := p;  # to make debugging easier!
  if i = 1 then    # this is the smallest helper subgroup

    # go to P_1:
    if j > 1 then 
      q := setup!.pifunc[j][1](p,setup!.pi[j][1]); 
    else
      q := p;
    fi;
    v := ValueHT(setup!.info[1],q);
    if v = fail then    # we do not yet know this point
      #Print("<\c");
      o := Enumerate(InitOrbit(setup!.els[1],q,setup!.op[1],setup!.size[1]*2,
                               rec( schreier := true, 
                                    grpsizebound := setup!.size[1],
                                    stabchainrandom := setup!.stabchainrandom,
                                    permgens := setup!.permgens[1],
                                    permbase := setup!.permbase[1] )));
      #Print(">\c");
      v := rec( gens := o!.stabwords, size := o!.stabsize );
      AddHT(setup!.info[1],q,v);
      # Now we have to store backward words via the wordcache:
      for m in [2..Length(o!.orbit)] do
          ww := -TraceSchreierTreeBack(o,m);
          ww := ORB_StoreWordInCache(setup,ww);
          AddHT(setup!.info[1],o!.orbit[m],ww);
      od;
      setup!.suborbnr[1] := setup!.suborbnr[1] + 1;
      setup!.sumstabl[1] := setup!.sumstabl[1] + o!.stabsize;
      # now p is by definition U_1-minimal and v contains stabilizer gens
    else    # we already know this U_1-orbit:
      if IsInt(v) then   # this is the number of a word
        if IsList(w) then Append(w,setup!.wordcache[v]); fi;  # store what we do
        p := ORB_ApplyWord(p,setup!.wordcache[v],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        if j > 1 then
          q := setup!.pifunc[j][1](p,setup!.pi[j][1]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[1],q);  # look up stabilizer
      fi; # otherwise we are already U_1-minimal:
    fi;
    if IsRecord(stab) then 
        stab.gens := v.gens;
        stab.size := v.size;
    fi;
    return p;

  else   # we are in some higher helper subgroup than U_1:

    # first do a U_{i-1}-minimalization:
    tempstab := rec();
    p := ORB_Minimalize2(p,j,i-1,setup,tempstab,w);

    # now try to reach the minimal U_{i-1}-suborbit in the U_i-orbit:
    if j > i then
      q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
    else
      q := p;
    fi;
    v := ValueHT(setup!.info[i],q);

    if v = fail then    # we do not yet know this U_{i-1}-suborbit

      #Print("Precalc...\c");
      # we define q*U_{i-1} to be the U_i-minimal U_{i-1}-orbit,
      # and q to be the U_i-minimal point in there.
      # now find the other U_{i-1}-orbits:
      o := OrbitBySuborbit2(setup,q,i,i,i-1,100);

      v := rec( gens := o!.stabwords, size := o!.stabsize );
      AddHT(setup!.info[i],q,v);
      # Now find all U_{i-1}-minimal elements in q*U_{i-1}, note that
      # tempstab contains generators for Stab_{U_{i-1}}(q)!
      Print("[[[\c");
      tempstabgens := List(tempstab.gens,
                           w->ORB_ApplyWord(One(setup!.els[i][1]),w,
                                            setup!.els[i],setup!.elsinv[i],
                                            OnRight));
      Print("<<<\c");
      oo := Enumerate(InitOrbit(tempstabgens,q,setup!.op[i],
                                setup!.hashlen[i],rec(schreier := true)));
      Print(Length(oo!.orbit),"|||\c");
      for m in [2..Length(oo!.orbit)] do
          ww := TraceSchreierTreeForward(oo,m);
          ww := ORB_InvWord(Concatenation( tempstab.gens{ww} ));
          ww := ORB_StoreWordInCache(setup,ww);
          if ValueHT(setup!.info[i],oo!.orbit[m]) <> fail then
              Error(1);
          fi;
          AddHT(setup!.info[i],oo!.orbit[m],-ww);
      od;
      Print(">>>\c");
      
      # Now store all U_{i-1}-minimal elements in the other U_{i-1}-orbits
      # of q*U_i together with a number of a transversal element to reach
      # the minimal U_{i-1}-orbit q*U_{i-1}:

      Print("len:",Length(o!.words),"\c");
      for m in [2..Length(o!.words)] do
          qq := ORB_ApplyWord(q,o!.words[m],setup!.els[i],setup!.elsinv[i],
                              setup!.op[i]);
          ww := ShallowCopy(o!.words[m]);
          tempstab := rec();
          qq := ORB_Minimalize2(qq,i,i-1,setup,tempstab,ww);
          tempstabgens := List(tempstab.gens,
                               w->ORB_ApplyWord(One(setup!.els[i][1]),w,
                                                setup!.els[i],setup!.elsinv[i],
                                                OnRight));
          Print("<<<\c");
          oo := Enumerate(InitOrbit(tempstabgens,qq,setup!.op[i],
                                    setup!.hashlen[i],rec(schreier := true)));
          Print(Length(oo!.orbit),"|||\c");
          for mm in [1..Length(oo!.orbit)] do
              www := TraceSchreierTreeForward(oo,mm);
              www := Concatenation( tempstab.gens{www} );
              cos := setup!.cosetrecog[i]
                        (i,ORB_InvWord(Concatenation(ww,www)),setup);
              if ValueHT(setup!.info[i],oo!.orbit[mm]) <> fail then
                  Error(2);
              fi;
              AddHT(setup!.info[i],oo!.orbit[mm],cos);
          od;
          Print(">>>\c");
      od;
      Print("]]]\c");

      # Now the U_{i-1}-orbit of the vector q is the U_i-minimal 
      # U_{i-1}-orbit and q is the U_i-minimal vector
      
      # now q is by definition the U_i-minimal point in the orbit and
      # v its setup!.info[i], i.e. [true,stabilizer information]
      setup!.suborbnr[i] := setup!.suborbnr[i] + 1;
      setup!.sumstabl[i] := setup!.sumstabl[i] + o!.stabsize;

      #Print("done.\n");

    else   # we already knew this U_{i-1}-suborbit

      if IsInt(v) and v > 0 then    # this is the number of a transversal word
        if IsList(w) then 
          Append(w,setup!.trans[i][v]);   # remember what we did
        fi; 
        p := ORB_ApplyWord(p,setup!.trans[i][v],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        # we again do a U_{i-1}-minimalization:
        p := ORB_Minimalize2(p,j,i-1,setup,stab,w);
        # now we are in the U_i-minimal U_{i-1}-suborbit and on a 
        # U_{i-1}-minimal element
        if j > i then
          q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[i],q);
      fi;
      if IsInt(v) then
        if v > 0 then
            Error("This should never have happened!");
        fi;
        # v < 0 and -v is a number of a word in the word cache:
        # we still have to apply an element of Stab_{U_{i-1}}(pi[j][i-1](p)):
        if IsList(w) then 
            Append(w,setup!.wordcache[-v]); 
        fi;  # remember what we did
        p := ORB_ApplyWord(p,setup!.wordcache[-v],
                           setup!.els[j],setup!.elsinv[j],setup!.op[j]);
        if j > i then
          q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
        else
          q := p;
        fi;
        v := ValueHT(setup!.info[i],q);
      fi;
      # now q is the U_i-minimal element in qU_i
      # v is now a record with generators for Stab_{U_i}(q)

    fi;

    if IsRecord(stab) then 
        stab.gens := v.gens;
        stab.size := v.size;
    fi;

    # now we are on the minimal element in the S-orbit
    return p;
  fi;
end );


#######################
# Suborbit databases: #
#######################

InstallMethod( SuborbitDatabase, "for an orbit by suborbit setup object",
  [ IsOrbitBySuborbitSetup, IsPosInt ],
  function( setup, hashlen )
    # i is the number of subgroups used for the trick, i>=1
    local r;
    r := rec( reps := [], lengths := [], setup := setup, totallength := 0,
              i := setup!.k, j := setup!.k+1 );
    r.mins := NewHT( setup!.sample[setup!.k+1], hashlen );
    Objectify( StdSuborbitDatabasesType, r );
    return r;
  end );

InstallMethod( SuborbitDatabase2, "for an orbit by suborbit setup object",
  [ IsOrbitBySuborbitSetup, IsPosInt, IsPosInt, IsPosInt ],
  function( setup, j, l, i )
    # j is the number of the representation we are working in, j > i
    # i is the number of subgroups used for the trick, i>=1
    # l is the number of subgroup we enumerate
    local r;
    r := rec( reps := [], lengths := [], setup := setup, totallength := 0,
              i := i, l := l, j := j );
    r.mins := NewHT( setup!.sample[j], setup!.hashlen[j] );
    Objectify( StdSuborbitDatabasesType, r );
    return r;
  end );

InstallMethod( ViewObj, "for a suborbit database",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( db )
    Print( "<suborbit database with ",Length(db!.reps)," suborbits, total ",
           "size: ", Sum(db!.lengths), " j=",db!.j," i=",db!.i,">" );
  end );

InstallMethod( StoreSuborbit, 
  "for a suborbit database, a point, a stabiterator, and a setup object",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep, IsObject, IsStabIterator ],
  function(db,p,stab)
  # "db" must be a suborbit database
  # "p" must be a U-minimal element, which is not yet known
  # "stab" must be stabilizer information coming from minimalization
  # all U-minimal elements in the same orbit are calculated and stored
  # in the hash, in addition "p" is appended as representative to
  # "suborbits" and the orbit length is calculated and appended to
  # "lengths".
  local setup, k, firstgen, lastgen, li, i, j, pp, vv, nrmins, length,stabsize;
        
  setup := db!.setup;
  k := setup!.k;
  Add(db!.reps,p);
  AddHT(db!.mins,p,Length(db!.reps));
  ###Print("[",Product(stab.info,Length),"\c");
  if Size(stab) = setup!.size[k] then
    # better use a standard orbit algorithm:
    if k = 1 then
      firstgen := 1;
    else
      firstgen := Length(setup!.els[k-1])+1;
    fi;
    lastgen := Length(setup!.els[k]);
    li := [p];
    i := 1;
    while i <= Length(li) do
      for j in [firstgen..lastgen] do
        pp := setup!.op[k+1](li[i],setup!.els[k+1][j]);  # ???
        vv := ValueHT(db!.mins,pp);
        if vv = fail then
          AddHT(db!.mins,pp,Length(db!.reps));
          Add(li,pp);
        fi;
      od;
      i := i + 1;
    od;
    nrmins := Length(li);
    length := nrmins;
  else
    Reset(stab);
    nrmins := 1;
    stabsize := 0;
    repeat
      pp := ORB_ApplyStabElement(p,k+1,k,setup,stab,false);
      if p = pp then  # we got a real stabilizer element of p
        stabsize := stabsize + 1;
      else  # we could have stepped to some other U-minimal element
        vv := ValueHT(db!.mins,pp);
        if vv = fail then
          AddHT(db!.mins,pp,Length(db!.reps));
          nrmins := nrmins+1;
        fi;
      fi;
    until Next(stab);
    length := setup!.size[k] / stabsize;
  fi;
  Add(db!.lengths,length);
  db!.totallength := db!.totallength + length;
  ###Print("]\r");
  Print("\r#",Length(db!.reps),
        ",S:",ORB_PrettyStringBigNumber(length),",\c");
  Print("M:",nrmins,"  \c");
  return length;
end );

InstallMethod( StoreSuborbit2, 
  "for a suborbit database, a point, a stabiterator, and a setup object",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep, IsObject, IsRecord, IsPosInt ],
  function(db,p,stab,fullstabsize)
  # "db" must be a suborbit database
  # "p" must be a U-minimal element, which is not yet known
  # "stab" must be stabilizer information coming from minimalization
  # "fullstabsize" is only for info purposes
  # all U-minimal elements in the same orbit are calculated and stored
  # in the hash, in addition "p" is appended as representative to
  # "suborbits" and the orbit length is calculated and appended to
  # "lengths".
  local i,j,l,length,m,o,setup,stabgens;
        
  setup := db!.setup;
  i := db!.i;
  j := db!.j;
  l := db!.l;
  Add(db!.reps,p);
  AddHT(db!.mins,p,Length(db!.reps));
  stabgens := List(stab.gens,
                   w->ORB_ApplyWord( setup!.els[j][1]^0, w, setup!.els[j],
                                     setup!.elsinv[j], OnRight ));
  o := Enumerate(InitOrbit(stabgens,p,setup!.op[j],setup!.hashlen[j],
                           rec(schreier := true)));
  for m in [2..Length(o!.orbit)] do
      AddHT( db!.mins, o!.orbit[m], Length(db!.reps) );
  od;
  length := setup!.size[i] / (stab.size / Length(o!.orbit));
  Add(db!.lengths,length);
  db!.totallength := db!.totallength + length;
  Info(InfoOrb,1,"j=",j," l=",l," i=",i," #",Length(db!.reps),
       " Size:",ORB_PrettyStringBigNumber(length),
       "\c Mins:",Length(o!.orbit)," \cTotal:",
       ORB_PrettyStringBigNumber(db!.totallength),
       "\c Stab:",ORB_PrettyStringBigNumber(fullstabsize));
  return length;
end );

InstallMethod( LookupSuborbit, 
  "for a (minimal) point and a std suborbit database",
  [ IsObject, IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( p, db )
    return ValueHT( db!.mins, p );
  end );

InstallMethod( TotalLength, "for a std suborbit database",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( db )
    return db!.totallength;
  end );

InstallMethod( Representatives, "for a std suborbit database",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( db )
    return db!.reps;
  end );

InstallMethod( Memory, "for a std suborbit database",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( db )
    local m,p;
    # The lists:
    m := 2 * SHALLOW_SIZE(db!.reps) + 2 * SHALLOW_SIZE(db!.mins!.els);
    #  (db!.reps and db!.lengths   and    els and vals in db!.mins)
    # Now the points (this assumes vectors!):
    p := SHALLOW_SIZE(db!.setup!.sample[db!.setup!.k+1])
         + 3 * GAPInfo.BytesPerVariable;   # for the bag
    m := m + db!.mins!.nr * p;  # the reps are also in mins!
    return m;
  end );


###################################
# The real thing: OrbitBySuborbit #
###################################

# First a few methods for IsOrbitBySuborbit objects:

InstallMethod( ViewObj, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    Print( "<orbit-by-suborbit size=",o!.orbitlength," stabsize=",
           o!.stabsize );
    if o!.percentage < 100 then
        Print(" (",o!.percentage,"%)");
    fi;
    if o!.db!.mins!.nr <> 0 then
        Print(" saving factor=", QuoInt(o!.db!.totallength,o!.db!.mins!.nr));
    fi;
    Print(">");
  end );

InstallOtherMethod( Size, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.orbitlength;
  end );

InstallOtherMethod( StabilizerOfExternalSet, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.stab;
  end );

InstallMethod( SuborbitsDb, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.db;
  end );

InstallMethod( WordsToSuborbits, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.words;
  end );
  
InstallMethod( Memory, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    local m1,m2;
    m1 := Memory(o!.db);
    m2 := Memory(o!.db!.setup);
    Info(InfoOrb,1,"Memory for suborbits database : ",
         ORB_PrettyStringBigNumber(m1));
    Info(InfoOrb,1,"Memory for setup (factor maps): ",
         ORB_PrettyStringBigNumber(m2));
    return [m1,m2];
  end );

InstallGlobalFunction( OrbitBySuborbit,
function(p,hashlen,size,setup,percentage)
  # Enumerates the orbit of p under the group G generated by "gens" by
  # suborbits for the subgroup U described in "setup". 
  # "p" is a point
  # "hashlen" is an upper bound for the set of U-minimal points in the G-orbit
  # "size" is the group order to stop when we are ready and as upper bound
  #        for the orbit length
  # "setup" is a setup object for the iterated quotient trick,
  #         effectively enabling us to do minimalization with a subgroup
  # "percentage" is a number between 50 and 100 and gives a stopping criterium.
  #         We stop if percentage of the orbit is enumerated.
  #         Only over 50% we know that the stabilizer is correct!
  # Returns a suborbit database with additional field "words" which is
  # a list of words in gens which can be used to reach U-orbit in the G-orbit

  local k,firstgen,lastgen,stab,miniwords,db,stabgens,stabperms,stabilizer,
        fullstabsize,words,todo,i,j,x,mw,done,newperm,newword,oldtodo,sw,xx,v,
        pleaseexitnow,assumestabcomplete;

  pleaseexitnow := false;  # set this to true in a break loop to
                           # let this function exit gracefully
  assumestabcomplete := false;  # set this to true in a break loop to
                                # let this function assume that the 
                                # stabilizer is complete

  # Setup some shortcuts:
  k := setup!.k;
  firstgen := Length(setup!.els[k])+1;
  lastgen := Length(setup!.els[k+1]);

  # A security check:
  if p <> setup!.op[k+1](p,setup!.els[k+1][1]^0) then
      Error("Warning: The identity does not preserve the starting point!\n",
            "Did you normalize your vector?");
  fi;

  # First we U-minimalize p:
  stab := StabIterator();
  p := ORB_Minimalize(p,k+1,k,setup,stab,false);

  miniwords := [[]];  # here we collect U-minimalizing elements
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase(setup,hashlen);
  StoreSuborbit(db,p,stab);

  stabgens := [];
  stabperms := [];
  stabilizer := Group(setup!.permgens[1]^0);
  if IsBound( setup!.stabchainrandom ) and setup!.stabchainrandom <> false then
      StabChain( stabilizer, rec( random := setup!.stabchainrandom ) );
  else
      StabChain(stabilizer);
  fi;
  fullstabsize := 1;
  
  words := [[]];
  todo := [[]];
  while true do

    i := 1;
    while i <= Length(todo) do
      if pleaseexitnow = true then return "didnotfinish"; fi;

      for j in [firstgen..lastgen] do
        x := setup!.op[k+1](p,setup!.els[k+1][j]);
        x := ORB_ApplyWord(x,todo[i],setup!.els[k+1],
                           setup!.elsinv[k+1],setup!.op[k+1]);
        mw := [];
        x := ORB_Minimalize(x,k+1,k,setup,stab,mw);
        v := LookupSuborbit(x,db);
        if v = fail then
          Add(words,Concatenation([j],todo[i]));
          Add(todo,Concatenation([j],todo[i]));
          Add(miniwords,mw);
          StoreSuborbit(db,x,stab);
          Print("t:",ORB_PrettyStringBigNumber(TotalLength(db)),
                " st:",ORB_PrettyStringBigNumber(fullstabsize),"       \r");
          if 2 * TotalLength(db) * fullstabsize > size and
             TotalLength(db) * fullstabsize * 100 >= size*percentage then 
            Print("\nDone!\n");
            return Objectify( StdOrbitBySuborbitsType,
                       rec(db := db,
                       words := words,
                       stabsize := fullstabsize,
                       stab := stabilizer,
                       stabwords := stabgens,
                       groupsize := size,
                       orbitlength := size/fullstabsize,
                       percentage := percentage,
                       seed := p ) );
          fi;
        else
          if assumestabcomplete = false and
             TotalLength(db) * fullstabsize * 2 <= size then
            # otherwise we know that we will not find more stabilizing els.
            # we know now that v is an integer and that
            # p*setup!.els[j]*todo[i]*U = p*words[v]*U
            # p*setup!.els[j]*todo[i]*mw is our new vector
            # p*words[v]*miniwords[v] is our old vector
            # they differ by an element in Stab_U(...)
            Reset(stab);
            done := false;
            repeat
              sw := [];
              xx := ORB_ApplyStabElement(x,k+1,k,setup,stab,sw);
              if xx = Representatives(db)[v] then  
                # we got a real stabilizer element of p
                done := true;
                newword := Concatenation([j],todo[i],mw,sw,
                            ORB_InvWord(miniwords[v]),ORB_InvWord(words[v]));
                newperm := ORB_ApplyWord(setup!.permgens[1]^0,newword,
                                 setup!.permgens,setup!.permgensinv,OnRight);
                if not(IsOne(newperm)) then
                  if not(newperm in stabilizer) then
                    Add(stabgens,newword);
                    Add(stabperms,newperm);
                    stabilizer := GroupWithGenerators(stabperms);
                    Print("\nCalculating new estimate of the stabilizer...\c");
                    if IsBound(setup!.stabchainrandom) and 
                       setup!.stabchainrandom <> false then
                        StabChain(stabilizer, 
                                  rec(random := setup!.stabchainrandom));
                    else
                        StabChain(stabilizer);
                    fi;
                    fullstabsize := Size(stabilizer);
                    Print("done.\nNew stabilizer order: ",fullstabsize,"\n");
                    if TotalLength(db) * fullstabsize * 100
                       >= size*percentage then 
                      Print("Done!\n");
                      return Objectify( StdOrbitBySuborbitsType,
                             rec(db := db,
                                 words := words,
                                 stabsize := fullstabsize,
                                 stab := stabilizer,
                                 stabwords := stabgens,
                                 groupsize := size,
                                 orbitlength := size/fullstabsize,
                                 percentage := percentage,
                                 seed := p) );
                    fi;
                  fi;
                fi;
              fi;
            until done or Next(stab);
          fi;
        fi;
      od;
      i := i + 1;
    od;
  
    oldtodo := todo;
    todo := [];
    for i in [1..Length(stabgens)] do
      Append(todo,List(oldtodo,w->Concatenation(stabgens[i],w)));
    od;
    Print("\nLength of next todo: ",Length(todo),"\n");
  od;
  # this is never reached
end );

InstallGlobalFunction( OrbitBySuborbit2,
function(setup,p,j,l,i,percentage)
  # Enumerates the orbit of p under the group U_j (with G=U_{k+1}) by
  # suborbits for the subgroup U_i described in "setup". 
  # "setup" is a setup object for the iterated quotient trick,
  #         effectively enabling us to do minimalization with a subgroup
  # "p" is a point
  # "l", "j" and "i" are integers with k+1 >= j >= l > i >= 1
  # "j" indicates in which representation we work,
  # "i" indicates how many helper subgroups we use
  # "l" indicates which group we enumerate
  # "percentage" is a number between 50 and 100 and gives a stopping criterium.
  #         We stop if percentage of the orbit is enumerated.
  #         Only over 50% we know that the stabilizer is correct!
  # Returns a suborbit database with additional field "words" which is
  # a list of words in gens which can be used to reach U-orbit in the G-orbit
  local assumestabcomplete,db,firstgen,fullstabsize,ii,lastgen,m,miniwords,
        mw,newperm,newword,o,oldtodo,pleaseexitnow,stab,stabg,stabgens,
        stabilizer,stabperms,sw,todo,v,words,x;

  Info(InfoOrb,2,"Entering OrbitBySuborbit2 j=",j," l=",l," i=",i);

  if not(j >= l and l > i and i >= 1) then
      Error("Need j >= l > i >= 1");
      return;
  fi;

  pleaseexitnow := false;  # set this to true in a break loop to
                           # let this function exit gracefully
  assumestabcomplete := false;  # set this to true in a break loop to
                                # let this function assume that the 
                                # stabilizer is complete

  # Setup some shortcuts:
  firstgen := Length(setup!.els[l-1])+1;
  lastgen := Length(setup!.els[l]);

  # A security check:
  if p <> setup!.op[j](p,setup!.els[j][1]^0) then
      Error("Warning: The identity does not preserve the starting point!\n",
            "Did you normalize your vector?");
  fi;

  # First we U_i-minimalize p:
  stab := rec();
  p := ORB_Minimalize2(p,j,i,setup,stab,false);

  miniwords := [[]];  # here we collect U-minimalizing elements
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase2(setup,j,l,i);
  StoreSuborbit2(db,p,stab,1);

  stabgens := [];
  stabperms := [];
  stabilizer := Group(setup!.permgens[l][1]^0);
  if IsBound( setup!.stabchainrandom ) and setup!.stabchainrandom <> false then
      StabChain( stabilizer, rec( random := setup!.stabchainrandom ) );
  else
      StabChain(stabilizer);
  fi;
  fullstabsize := 1;
  
  words := [[]];
  todo := [[]];
  while true do

    ii := 1;
    while ii <= Length(todo) do
      if pleaseexitnow = true then 
          return ["didnotfinish",db,fullstabsize]; 
      fi;

      for m in [firstgen..lastgen] do
        x := setup!.op[j](p,setup!.els[j][m]);
        x := ORB_ApplyWord(x,todo[ii],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        mw := [];
        x := ORB_Minimalize2(x,j,i,setup,stab,mw);
        v := LookupSuborbit(x,db);
        if v = fail then   # a new suborbit
          Add(words,Concatenation([m],todo[ii]));
          Add(todo,Concatenation([m],todo[ii]));
          Add(miniwords,mw);
          StoreSuborbit2(db,x,stab,fullstabsize);
          if 2 * TotalLength(db) * fullstabsize > setup!.size[l] and
             TotalLength(db) * fullstabsize * 100 >= 
                             setup!.size[l]*percentage then 
            Info(InfoOrb,2,"Leaving OrbitBySuborbit2");
            return Objectify( StdOrbitBySuborbitsType,
                       rec(db := db,
                       words := words,
                       stabsize := fullstabsize,
                       stab := stabilizer,
                       stabwords := stabgens,
                       groupsize := setup!.size[l],
                       orbitlength := setup!.size[l]/fullstabsize,
                       percentage := percentage,
                       seed := p ) );
          fi;
        else
          if assumestabcomplete = false and
             TotalLength(db) * fullstabsize * 2 <= setup!.size[l] then
            # otherwise we know that we will not find more stabilizing els.
            # we know now that v is an integer and that
            # p*setup!.els[m]*todo[ii]*U = p*words[v]*U
            # p*setup!.els[m]*todo[ii]*mw is our new vector
            # p*words[v]*miniwords[v] is our old vector
            # they differ by an element in Stab_U(...)
            stabg := List(stab.gens,
                          w->ORB_ApplyWord(setup!.els[j][1]^0,w,setup!.els[j],
                                           setup!.elsinv[j], OnRight ));
            o := Enumerate(InitOrbit(stabg,x,setup!.op[j],setup!.hashlen[j],
                   rec( lookingfor := [Representatives(db)[v]],
                        schreier := true )));
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( stab.gens{sw} );
            newword := Concatenation([m],todo[ii],mw,sw,
                            ORB_InvWord(miniwords[v]),ORB_InvWord(words[v]));
            Print("[\c");
            newperm := ORB_ApplyWord(setup!.permgens[l][1]^0,newword,
                            setup!.permgens[l],setup!.permgensinv[l],OnRight);
            if not(IsOne(newperm)) then
              Print(".\c");
              if not(newperm in stabilizer) then
                Print(".\c");
                Add(stabgens,newword);
                Add(stabperms,newperm);
                stabilizer := GroupWithGenerators(stabperms);
                Info(InfoOrb,1,"Calculating new estimate of the stabilizer...");
                if IsBound(setup!.stabchainrandom) and
                   setup!.stabchainrandom <> false then
                    StabChain(stabilizer, 
                              rec(random := setup!.stabchainrandom));
                else
                    StabChain(stabilizer);
                fi;
                fullstabsize := Size(stabilizer);
                Info(InfoOrb,1,"New stabilizer order: ",fullstabsize);
                if TotalLength(db) * fullstabsize * 100
                   >= setup!.size[l]*percentage then 
                  Info(InfoOrb,2,"Leaving OrbitBySuborbit2");
                  return Objectify( StdOrbitBySuborbitsType,
                         rec(db := db,
                             words := words,
                             stabsize := fullstabsize,
                             stab := stabilizer,
                             stabwords := stabgens,
                             groupsize := setup!.size[l],
                             orbitlength := setup!.size[l]/fullstabsize,
                             percentage := percentage,
                             seed := p) );
                fi;
              fi;
            fi;
            Print("]\c");
          fi;
        fi;
      od;   # for m in [firstgen..lastgen]
      ii := ii + 1;
    od;
  
    oldtodo := todo;
    todo := [];
    for ii in [1..Length(stabgens)] do
      Append(todo,List(oldtodo,w->Concatenation(stabgens[ii],w)));
    od;
    Info(InfoOrb,1,"Length of next todo: ",Length(todo));
  od;
  # this is never reached
end );

InstallGlobalFunction( OrbitBySuborbit3,
function(setup,p,j,l,i,percentage)
  # Enumerates the orbit of p under the group U_j (with G=U_{k+1}) by
  # suborbits for the subgroup U_i described in "setup". 
  # "setup" is a setup object for the iterated quotient trick,
  #         effectively enabling us to do minimalization with a subgroup
  # "p" is a point
  # "l", "j" and "i" are integers with k+1 >= j >= l > i >= 1
  # "j" indicates in which representation we work,
  # "i" indicates how many helper subgroups we use
  # "l" indicates which group we enumerate
  # "percentage" is a number between 50 and 100 and gives a stopping criterium.
  #         We stop if percentage of the orbit is enumerated.
  #         Only over 50% we know that the stabilizer is correct!
  # Returns a suborbit database with additional field "words" which is
  # a list of words in gens which can be used to reach U-orbit in the G-orbit
  local assumestabcomplete,db,firstgen,fullstabsize,ii,lastgen,m,miniwords,
        mw,newperm,newword,o,oldtodo,pleaseexitnow,stab,stabg,stabgens,
        stabilizer,stabperms,sw,todo,v,words,x,firstgenU,lastgenU;

  Info(InfoOrb,2,"Entering OrbitBySuborbit3 j=",j," l=",l," i=",i);

  if not(j >= l and l > i and i >= 1) then
      Error("Need j >= l > i >= 1");
      return;
  fi;

  pleaseexitnow := false;  # set this to true in a break loop to
                           # let this function exit gracefully
  assumestabcomplete := false;  # set this to true in a break loop to
                                # let this function assume that the 
                                # stabilizer is complete

  # Setup some shortcuts:
  firstgen := Length(setup!.els[l-1])+1;
  lastgen := Length(setup!.els[l]);
  if i = 1 then
      firstgenU := 1;
  else
      firstgenU := Length(setup!.els[i-1])+1;
  fi;
  lastgenU := Length(setup!.els[i]);

  # A security check:
  if p <> setup!.op[j](p,setup!.els[j][1]^0) then
      Error("Warning: The identity does not preserve the starting point!\n",
            "Did you normalize your vector?");
  fi;

  # First we U_i-minimalize p:
  stab := rec();
  p := ORB_Minimalize2(p,j,i,setup,stab,false);

  miniwords := [[]];  # here we collect U-minimalizing elements
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase2(setup,j,l,i);
  StoreSuborbit2(db,p,stab,1);

  stabgens := [];
  stabperms := [];
  stabilizer := Group(setup!.permgens[l][1]^0);
  if IsBound( setup!.stabchainrandom ) and setup!.stabchainrandom <> false then
      StabChain( stabilizer, rec( random := setup!.stabchainrandom ) );
  else
      StabChain(stabilizer);
  fi;
  fullstabsize := 1;
  
  words := [[]];
  todo := [[]];
  while true do

    ii := 1;
    while ii <= Length(todo) do
      if pleaseexitnow = true then 
          return ["didnotfinish",db,fullstabsize]; 
      fi;

      for m in [firstgen..lastgen] do
        x := ORB_ApplyWord(p,todo[ii],setup!.els[j],
                           setup!.elsinv[j],setup!.op[j]);
        x := setup!.op[j](x,setup!.els[j][m]);
        mw := [];
        x := ORB_Minimalize2(x,j,i,setup,stab,mw);
        v := LookupSuborbit(x,db);
        if v = fail then   # a new suborbit
          Add(words,Concatenation(todo[ii],[m]));
          Add(todo,Concatenation(todo[ii],[m]));
          Add(miniwords,mw);
          StoreSuborbit2(db,x,stab,fullstabsize);
          if 2 * TotalLength(db) * fullstabsize > setup!.size[l] and
             TotalLength(db) * fullstabsize * 100 >= 
                             setup!.size[l]*percentage then 
            Info(InfoOrb,2,"Leaving OrbitBySuborbit3");
            return Objectify( StdOrbitBySuborbitsType,
                       rec(db := db,
                       words := words,
                       stabsize := fullstabsize,
                       stab := stabilizer,
                       stabwords := stabgens,
                       groupsize := setup!.size[l],
                       orbitlength := setup!.size[l]/fullstabsize,
                       percentage := percentage,
                       seed := p ) );
          fi;
        else
          if assumestabcomplete = false and
             TotalLength(db) * fullstabsize * 2 <= setup!.size[l] then
            # otherwise we know that we will not find more stabilizing els.
            # we know now that v is an integer and that
            # p*todo[ii]*setup!.els[m]*U = p*words[v]*U
            # p*todo[ii]*setup!.els[m]*mw is our new vector
            # p*words[v]*miniwords[v] is our old vector
            # they differ by an element in Stab_U(...)
            stabg := List(stab.gens,
                          w->ORB_ApplyWord(setup!.els[j][1]^0,w,setup!.els[j],
                                           setup!.elsinv[j], OnRight ));
            o := Enumerate(InitOrbit(stabg,x,setup!.op[j],setup!.hashlen[j],
                   rec( lookingfor := [Representatives(db)[v]],
                        schreier := true )));
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( stab.gens{sw} );
            newword := Concatenation(todo[ii],[m],mw,sw,
                            ORB_InvWord(miniwords[v]),ORB_InvWord(words[v]));
            Print("[\c");
            newperm := ORB_ApplyWord(setup!.permgens[l][1]^0,newword,
                            setup!.permgens[l],setup!.permgensinv[l],OnRight);
            if not(IsOne(newperm)) then
              Print(".\c");
              if not(newperm in stabilizer) then
                Print(".\c");
                Add(stabgens,newword);
                Add(stabperms,newperm);
                stabilizer := GroupWithGenerators(stabperms);
                Info(InfoOrb,1,"Calculating new estimate of the stabilizer...");
                if IsBound(setup!.stabchainrandom) and
                   setup!.stabchainrandom <> false then
                    StabChain(stabilizer, 
                              rec(random := setup!.stabchainrandom));
                else
                    StabChain(stabilizer);
                fi;
                fullstabsize := Size(stabilizer);
                Info(InfoOrb,1,"New stabilizer order: ",fullstabsize);
                if TotalLength(db) * fullstabsize * 100
                   >= setup!.size[l]*percentage then 
                  Info(InfoOrb,2,"Leaving OrbitBySuborbit3");
                  return Objectify( StdOrbitBySuborbitsType,
                         rec(db := db,
                             words := words,
                             stabsize := fullstabsize,
                             stab := stabilizer,
                             stabwords := stabgens,
                             groupsize := setup!.size[l],
                             orbitlength := setup!.size[l]/fullstabsize,
                             percentage := percentage,
                             seed := p) );
                fi;
              fi;
            fi;
            Print("]\c");
          fi;
        fi;
      od;   # for m in [firstgen..lastgen]
      ii := ii + 1;
    od;
  
    oldtodo := todo;
    todo := [];
    for ii in [firstgenU..lastgenU] do
      Append(todo,List(oldtodo,w->Concatenation(w,[ii])));
    od;
    Info(InfoOrb,1,"Length of next todo: ",Length(todo));
  od;
  # this is never reached
end );

InstallMethod( Next, "for a stab iterator and a string",
  [ IsStabIterator and IsStdStabIteratorRep, IsString ],
function(stab,st)
  local i;
  i := stab!.i;
  while true do
    if i < 1 then
      return true;   # finished with this iterator
    fi;
    stab!.pos[i] := stab!.pos[i]+1;
    stab!.point[i] := [];
    if stab!.pos[i] <= Length(stab!.info[i]) then
      return false;  # next element found
    fi;
    stab!.pos[i] := 1;
    i := i - 1;
  od;
  # this is never reached
end );

InstallGlobalFunction( ORB_ApplyUElement,
function(p,j,i,setup,stab,w)
  local ww;
  while i >= 1 do
    ww := ShallowCopy(setup!.trans[i][stab!.info[i][stab!.pos[i]]]);
    if IsList(w) then Append(w,ww); fi;
    p := ORB_ApplyWord(p,ww,setup!.els[j],setup!.elsinv[j],setup!.op[j]);
    i := i - 1;
  od;
  return p;
end );

InstallGlobalFunction( OrbitBySuborbitWithKnownSize,
function(p,hashlen,size,setup,randels)
  # Enumerates the orbit of p under the group G generated by "gens" by
  # suborbits for the subgroup U described in "setup". 
  #   "p" is a point
  #   "hashlen" is an upper bound for the set of U-minimal points in the G-orbit
  #   "size" is the orbit length
  #   "setup" is a record of data for the iterated quotient trick,
  #           effectively enabling us to do minimalization with a subgroup
  #   "randels" number of random elements to use for recognising half orbits
  # Returns a record with components:
  #   "db": suborbit database
  #   "words": a list of words in gens which can be used to reach 
  #            U-orbit in the G-orbit
  local k,stab,q,trans,db,l,i,Ucounter,g,j,x,y,v,ii,w,z,firstgen,lastgen;

  k := setup!.k;
  firstgen := Length(setup!.els[k])+1;
  lastgen := Length(setup!.els[k+1]);

  # First we U-minimalize p:
  stab := StabIterator();
  q := ORB_Minimalize(p,k+1,k,setup,stab,false);

  trans := [[]];    # words for getting to the U-suborbits
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase(setup,hashlen);
  StoreSuborbit(db,q,stab);

  l := [p];
  i := 1;   # counts elements in l
  # use a stabilizer info, which describes all of U:
  Ucounter := StabIterator();
  Ucounter!.i := k;
  Ucounter!.info := List([1..k],i->[1..setup!.index[i]]);
  Ucounter!.pos := List([1..k],i->1);

  # Throw in some vectors gotten by applying random elements:
  g := GroupWithGenerators(setup!.els[k+1]{[firstgen..lastgen]});
  for j in [1..randels] do
      Print("Applying random element ",j," (",randels,") ...\n");
      x := p * PseudoRandom(g);
      y := ORB_Minimalize(x,k+1,k,setup,stab,false);
      v := LookupSuborbit(y,db);
      if v = fail then
          Add(l,x);
          Add(trans,[fail]);
          StoreSuborbit(db,y,stab);
          Print("total: ",ORB_PrettyStringBigNumber(TotalLength(db)),"     \n");
      fi;
      if TotalLength(db) >= size then 
        Print("\nDone.\n");
        return rec( db := db, words := trans ); 
      fi;
  od;
  Unbind(g);
   
  Reset(Ucounter);
  repeat
    while i <= Length(l) do
      for j in [firstgen..lastgen] do
        x := setup!.op[k+1](l[i],setup!.els[k+1][j]);
        y := ORB_Minimalize(x,k+1,k,setup,stab,false);
        v := LookupSuborbit(y,db);
        if v = fail then
          Add(trans,Concatenation(trans[i],[j]));
          Add(l,x);
          StoreSuborbit(db,y,stab);
          Print("total: ",ORB_PrettyStringBigNumber(TotalLength(db)),"     \r");
          if TotalLength(db) >= size then 
            Print("\nDone.\n");
            return rec( db := db, words := trans ); 
          fi;
        fi;
      od;
      i := i + 1;
    od;
    # now we have to to something else, perhaps applying some U elements?
    Print(".\c");
    for ii in [1..Length(l)] do
      w := [];
      z := ORB_ApplyUElement(l[ii],k+1,k,setup,Ucounter,w);
      for j in [firstgen..lastgen] do
        x := setup!.op[k+1](z,setup!.els[k+1][j]);
        y := ORB_Minimalize(x,k+1,k,setup,stab,false);
        v := LookupSuborbit(y,db);
        if v = fail then
          Add(trans,Concatenation(trans[ii],w,[j]));
          Add(l,x);
          StoreSuborbit(db,y,stab);
          Print("total: ",ORB_PrettyStringBigNumber(TotalLength(db)),"     \r");
          if TotalLength(db) >= size then 
            Print("\nDone.\n");
            return rec( db := db, words := trans ); 
          fi;
        fi;
      od;
    od;
  until Next(Ucounter,"fromabove");

  Print("Warning! Orbit not complete!!!\n");
  # this should never happen!
  return rec( db := db, words := trans );
end );


############################
# Convenient preparations: #
############################

InstallGlobalFunction( ORB_NormalizeVector,
  function(v)
    local c;
    c := PositionNonZero(v);
    if c <= Length(v) then
        MultRowVector(v,v[c]^-1);
    fi;
    return v;
  end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForVectors,
function(gens,permgens,sizes,codims)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k
  # codims: a list of dimensions of factor modules
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local dim,doConversions,f,i,j,k,nrgens,nrgenssum,o,regvec,sample,setup,sum,v,
        counter,merk,neededfullspace;

  # For the old compressed matrices:
  if IsGF2MatrixRep(gens[1][1]) or Is8BitMatrixRep(gens[1][1]) then
      doConversions := true;
  else
      doConversions := false;
  fi;

  # Some preparations:
  k := Length(sizes);
  if Length(gens) <> k+1 or Length(permgens) <> k+1 or Length(codims) <> k then
      Error("Need generators for ",k+1," groups and ",k," codimensions.");
      return;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  sample := gens[1][1][1];  # first vector of first generator

  # Do the first step:
  setup := rec(k := 1);
  setup.size := [sizes[1]];
  setup.index := [sizes[1]];
  setup.permgens := Concatenation(permgens);
  setup.permgensinv := List(setup.permgens,x->x^-1);
  setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := List([1..k+1],i->100000);
  dim := Length(gens[1][1]);
  codims[k+1] := dim;   # for the sake of completeness!
  for j in [1..k] do
      setup.els[j] := List(Concatenation(gens{[1..j]}),
                           x->ExtractSubMatrix(x,[1..codims[j]],
                                                 [1..codims[j]]));
      if doConversions then
          for i in setup.els[j] do ConvertToMatrixRep(i); od;
      fi;
      setup.elsinv[j] := List(setup.els[j],x->x^-1);
  od;
  f := BaseField(gens[1][1]);
  regvec := ZeroVector(sample,codims[1]);  
            # a new empty vector over same field
  Info(InfoOrb,1,"Looking for regular U1-orbit in factor space...");
  counter := 0;
  repeat
      counter := counter + 1;
      Randomize(regvec);
      o := Enumerate(InitOrbit(setup.els[1],regvec,OnRight,sizes[1]*2,
                               rec(schreier := true)));
      Info(InfoOrb,2,"Found length: ",Length(o!.orbit));
  until Length(o!.orbit) = sizes[1] or counter >= 10;
  if Length(o!.orbit) < sizes[1] then   # Bad luck, try something else:
    regvec := ZeroMutable(sample);
    Info(InfoOrb,1,"Looking for regular U1-orbit in full space...");
    counter := 0;
    repeat
        counter := counter + 1;
        Randomize(regvec);
        o := Enumerate(InitOrbit(gens[1],regvec,OnRight,sizes[1]*2,
                                 rec(schreier := true)));
        Info(InfoOrb,2,"Found length: ",Length(o!.orbit));
    until Length(o!.orbit) = sizes[1] or counter >= 10;
    if Length(o!.orbit) < sizes[1] then   # Again bad luck, try the regular rep
        Info(InfoOrb,1,"Using the regular permutation representation...");
        o := Enumerate(InitOrbit(gens[1],gens[1]^0,OnRight,sizes[1]*2,
                                 rec(schreier := true)));
    fi;
  fi;
  Info(InfoOrb,2,"Found!");
  setup.trans := [List([1..Length(o!.orbit)],i->TraceSchreierTreeForward(o,i))];

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
  od;
  setup.info := [NewHT(regvec,Size(f)^(codims[1]) * 3)];
  setup.suborbnr := [0];
  setup.sumstabl := [0];
  setup.regvecs := [regvec];
  setup.op := List([1..k+1],i->OnRight);
  setup.sample := [regvec,gens[1][1][1]];

  Objectify( NewType( OrbitBySuborbitSetupFamily,
                      IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep ),
             setup );
  # From now on we can use it and it is an object!

  neededfullspace := false;

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!
      if not(neededfullspace) then
        # if we have needed the full space somewhere, we need it everywhere
        # else, because OrbitBySuborbit is only usable for big vectors!
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in factor space...");
        regvec := ZeroVector(sample,codims[j]);
        counter := 0;
        repeat
            Randomize(regvec);
            counter := counter + 1;
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
            o := OrbitBySuborbit(regvec,(sizes[j]/sizes[j-1])*2+1,sizes[j],
                                 setup,100);
            Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
              counter >= 3;
      fi;
      if neededfullspace or
         Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        neededfullspace := true;
        # Bad luck, try the full space:
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        merk := [setup!.els[j],setup!.elsinv[j]];
        setup!.els[j] := Concatenation(gens{[1..j]});
        setup!.elsinv[j] := List(setup!.els[j],x->x^-1);
        setup!.sample[j] := ShallowCopy(regvec);  # now a longer vector!
        # this is corrected later on!
        repeat
            Randomize(regvec);
            counter := counter + 1;
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
            o := OrbitBySuborbit(regvec,(sizes[j]/sizes[j-1])*2+1,sizes[j],
                                 setup,100);
            Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= 20;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            return;
        fi;
        setup!.els[j] := merk[1];
        setup!.elsinv[j] := merk[2];
      fi;

      Info(InfoOrb,1,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.k := j;
      setup!.size[j] := sizes[j];
      setup!.index[j] := sizes[j]/sizes[j-1];
      setup!.trans[j] := o!.words;
      setup!.suborbnr[j] := 0;
      setup!.sumstabl[j] := 0;
      setup!.info[j] :=
            NewHT(regvec,QuoInt(Size(f)^(codims[j]),sizes[j-1])*4+1); # fixme!
      setup!.regvecs[j] := regvec;
      if not(neededfullspace) then
          setup!.cosetrecog[j] := ORB_CosetRecogGenericFactorSpace;
          setup!.cosetinfo[j] := o!.db;   # the hash table
      else
          setup!.cosetrecog[j] := ORB_CosetRecogGenericFullSpace;
          setup!.cosetinfo[j] := [o!.db,k];   # the hash table
      fi;
      setup!.sample[j] := ZeroVector(sample,codims[j]);
      setup!.sample[j+1] := sample;
  od;
  return setup;
end );

InstallGlobalFunction( ORB_CosetRecogGenericFactorSpace,
  function( j, w, s )
    local x;
    x := ORB_ApplyWord(s!.regvecs[j],w,s!.els[j],s!.elsinv[j],s!.op[j]);
    x := ORB_Minimalize(x,j,j-1,s,false,false);
    return LookupSuborbit(x,s!.cosetinfo[j]);
  end );

InstallGlobalFunction( ORB_CosetRecogGenericFullSpace,
  function( j, w, s )
    local x,k;
    k := s!.cosetinfo[j][2];
    x := ORB_ApplyWord(s!.regvecs[j],w,s!.els[k+1],s!.elsinv[k+1],
                       s!.op[k+1]);
    x := ORB_Minimalize(x,k+1,j-1,s,false,false);
    return LookupSuborbit(x,s!.cosetinfo[j][1]);
  end );

InstallGlobalFunction( ORB_CosetRecogGeneric,
  function( i, w, s )
    local x,j;
    j := s!.cosetinfo[i][2];
    x := ORB_ApplyWord(s!.regvecs[i],w,s!.els[j],s!.elsinv[j],s!.op[j]);
    x := ORB_Minimalize2(x,j,i-1,s,false,false);
    return LookupSuborbit(x,s!.cosetinfo[i][1]);
  end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForLines,
function(gens,permgens,sizes,codims)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k
  # codims: a list of dimensions of factor modules
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local dim,doConversions,f,i,j,k,nrgens,nrgenssum,o,regvec,sample,setup,sum,v,
        counter,merk,neededfullspace,c;

  # For the old compressed matrices:
  if IsGF2MatrixRep(gens[1][1]) or Is8BitMatrixRep(gens[1][1]) then
      doConversions := true;
  else
      doConversions := false;
  fi;

  # Some preparations:
  k := Length(sizes);
  if Length(gens) <> k+1 or Length(permgens) <> k+1 or Length(codims) <> k then
      Error("Need generators for ",k+1," groups and ",k," codimensions.");
      return;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  sample := gens[1][1][1];  # first vector of first generator

  # Do the first step:
  setup := rec(k := 1);
  setup.size := [sizes[1]];
  setup.index := [sizes[1]];
  setup.permgens := Concatenation(permgens);
  setup.permgensinv := List(setup.permgens,x->x^-1);
  setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := List([1..k+1],i->1000000);
  dim := Length(gens[1][1]);
  codims[k+1] := dim;   # for the sake of completeness!
  for j in [1..k] do
      setup.els[j] := List(Concatenation(gens{[1..j]}),
                           x->ExtractSubMatrix(x,[1..codims[j]],
                                                 [1..codims[j]]));
      if doConversions then
          for i in setup.els[j] do ConvertToMatrixRep(i); od;
      fi;
      setup.elsinv[j] := List(setup.els[j],x->x^-1);
  od;
  f := BaseField(gens[1][1]);
  regvec := ZeroVector(sample,codims[1]);  
            # a new empty vector over same field
  Info(InfoOrb,1,"Looking for regular U1-orbit in factor space...");
  counter := 0;
  repeat
      counter := counter + 1;
      Randomize(regvec);
      c := PositionNonZero( regvec );
      if c <= Length( regvec )  then
          regvec := Inverse( regvec[c] ) * regvec;
      fi;
      o := Enumerate(InitOrbit(setup.els[1],regvec,OnLines,sizes[1]*2,
                               rec(schreier := true)));
      Info(InfoOrb,2,"Found length: ",Length(o!.orbit));
  until Length(o!.orbit) = sizes[1] or counter >= 10;
  if Length(o!.orbit) < sizes[1] then   # Bad luck, try something else:
    regvec := ZeroMutable(sample);
    Info(InfoOrb,1,"Looking for regular U1-orbit in full space...");
    counter := 0;
    repeat
        counter := counter + 1;
        Randomize(regvec);
        c := PositionNonZero( regvec );
        if c <= Length( regvec )  then
            regvec := Inverse( regvec[c] ) * regvec;
        fi;
        o := Enumerate(InitOrbit(gens[1],regvec,OnLines,sizes[1]*2,
                                 rec(schreier := true)));
        Info(InfoOrb,2,"Found length: ",Length(o!.orbit));
    until Length(o!.orbit) = sizes[1] or counter >= 10;
    if Length(o!.orbit) < sizes[1] then   # Again bad luck, try the regular rep
        Info(InfoOrb,1,"Using the permutation representation...");
        o := Enumerate(InitOrbit(permgens[1],permgens[1]^0,OnRight,sizes[1]*2,
                                 rec(schreier := true)));
    fi;
  fi;
  Info(InfoOrb,2,"Found!");
  setup.trans := [List([1..Length(o!.orbit)],i->TraceSchreierTreeForward(o,i))];

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
  od;
  setup.info := [NewHT(regvec,Size(f)^(codims[1]) * 3)];
  setup.suborbnr := [0];
  setup.sumstabl := [0];
  setup.regvecs := [regvec];
  setup.op := List([1..k+1],i->OnLines);
  setup.sample := [regvec,gens[1][1][1]];

  Objectify( NewType( OrbitBySuborbitSetupFamily,
                      IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep ),
             setup );
  # From now on we can use it and it is an object!

  neededfullspace := false;

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!
      if not(neededfullspace) then
        # if we have needed the full space somewhere, we need it everywhere
        # else, because OrbitBySuborbit is only usable for big vectors!
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in factor space...");
        regvec := ZeroVector(sample,codims[j]);
        counter := 0;
        repeat
            Randomize(regvec);
            c := PositionNonZero( regvec );
            if c <= Length( regvec )  then
                regvec := Inverse( regvec[c] ) * regvec;
            fi;
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(regvec,(sizes[j]/sizes[j-1])*2+1,sizes[j],
                                 setup,100);
            Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
              counter >= 3;
      fi;
      if neededfullspace or
         Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        neededfullspace := true;
        # Bad luck, try the full space:
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        merk := [setup!.els[j],setup!.elsinv[j]];
        setup!.els[j] := Concatenation(gens{[1..j]});
        setup!.elsinv[j] := List(setup!.els[j],x->x^-1);
        setup!.sample[j] := ShallowCopy(regvec);  # now a longer vector
        # this will be corrected later on
        repeat
            Randomize(regvec);
            c := PositionNonZero( regvec );
            if c <= Length( regvec )  then
                regvec := Inverse( regvec[c] ) * regvec;
            fi;
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(regvec,(sizes[j]/sizes[j-1])*2+1,sizes[j],
                                 setup,100);
            Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= 20;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            return;
        fi;
        setup!.els[j] := merk[1];
        setup!.elsinv[j] := merk[2];
      fi;

      Info(InfoOrb,2,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.k := j;
      setup!.size[j] := sizes[j];
      setup!.index[j] := sizes[j]/sizes[j-1];
      setup!.trans[j] := o!.words;
      setup!.suborbnr[j] := 0;
      setup!.sumstabl[j] := 0;
      setup!.info[j] :=
            NewHT(regvec,QuoInt(Size(f)^(codims[j]),sizes[j-1])*4+1); # fixme!
      setup!.regvecs[j] := regvec;
      if not(neededfullspace) then
          setup!.cosetrecog[j] := ORB_CosetRecogGenericFactorSpace;
          setup!.cosetinfo[j] := o!.db;   # the hash table
      else
          setup!.cosetrecog[j] := ORB_CosetRecogGenericFullSpace;
          setup!.cosetinfo[j] := [o!.db,k];   # the hash table
      fi;
      setup!.sample[j] := ZeroVector(sample,codims[j]);
      setup!.sample[j+1] := sample;
  od;
  return setup;
end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForLines2,
function(gens,permgens,sizes,codims)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k < G
  # codims: a list of dimensions of factor modules
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local counter,dim,doConversions,g,i,j,k,neededfullspace,nrgens,nrgenssum,
        o,q,regvec,sample,setup,sm,sum;

  # For the old compressed matrices:
  if IsGF2MatrixRep(gens[1][1]) or Is8BitMatrixRep(gens[1][1]) then
      doConversions := true;
  else
      doConversions := false;
  fi;

  # Some preparations:
  k := Length(sizes)-1;
  if Length(gens) <> k+1 or Length(permgens) <> k+1 or Length(codims) <> k then
      Error("Need generators for ",k+1," groups and ",k," codimensions.");
      return;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  sample := gens[1][1][1];  # first vector of first generator

  # First preparations:
  setup := rec(k := k);
  setup.size := ShallowCopy(sizes);
  setup.index := sizes{[1..k]};
  for i in [k,k-1..2] do setup.index[i] := setup.index[i]/setup.index[i-1]; od;

  # Calculate stabilizer chain for whole group:
  setup.permgens := [];
  setup.permgensinv := [];
  setup.permbase := [];
  setup.permgens[k+1] := Concatenation(permgens);
  setup.permgensinv[k+1] := List(setup.permgens[k+1],x->x^-1);
  Info(InfoOrb,1,"Calculating stabilizer chain for whole group...");
  g := Group(setup.permgens[k+1]{[nrgenssum[k+1]+1..nrgenssum[k+2]]});
  SetSize(g,sizes[k+1]);
  setup.permbase[k+1] := BaseStabChain(StabChain(g));
  for i in [k,k-1..1] do
      g := Group(setup.permgens[i+1]{[nrgenssum[i]+1..nrgenssum[i+1]]});
      SetSize(g,sizes[i]);
      Info(InfoOrb,1,"Trying smaller degree permutation representation for U",
           i,"...");
      sm := SmallerDegreePermutationRepresentation(g);
      setup.permgens[i] := setup.permgens[i+1]{[1..nrgenssum[i+1]]};
      if not(IsOne(sm)) then   # not the identity
          Info(InfoOrb,1,"Found one on ",
               LargestMovedPoint(GeneratorsOfGroup(Image(sm)))," points.");
          for j in [1..Length(setup.permgens[i])] do
              setup.permgens[i][j] := ImageElm(sm,setup.permgens[i][j]);
          od;
          g := Image(sm);
      fi;
      setup.permgensinv[i] := List(setup.permgens[i],x->x^-1);
      setup.permbase[i] := BaseStabChain(StabChain(g));
  od;
  setup.stabchainrandom := false;

  setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := List([1..k+1],i->1000000);
  setup.sample := [];
  setup.sample[k+1] := sample;
  dim := Length(sample);
  codims[k+1] := dim;   # for the sake of completeness!
  for j in [1..k] do
      setup.els[j] := List(Concatenation(gens{[1..j]}),
                           x->ExtractSubMatrix(x,[1..codims[j]],
                                                 [1..codims[j]]));
      if doConversions then
          for i in setup.els[j] do ConvertToMatrixRep(i); od;
      fi;
      setup.elsinv[j] := List(setup.els[j],x->x^-1);
      setup.sample[j] := sample{[1..codims[j]]};
  od;
  q := Size(BaseField(gens[1][1]));
  setup.trans := [];

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  setup.info := [NewHT(setup.sample[1],(q^codims[1]-1)/(q-1) * 3)];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
      if j < k+1 then
          setup.info[j] :=
             NewHT(setup.sample[j],QuoInt((q^codims[j]-1)/(q-1)*3,sizes[j-1]));
      fi;
  od;
  setup.suborbnr := 0*[1..k];
  setup.sumstabl := 0*[1..k];
  setup.regvecs := [];
  setup.op := List([1..k+1],i->OnLines);
  setup.wordcache := [];
  setup.wordhash := NewHT([1,2,3],1000);

  Objectify( NewType(OrbitBySuborbitSetupFamily,
                     IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep),
             setup );
  # From now on we can use it and it is an object!

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!

      neededfullspace := false;

      Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
           "in factor space...");
      regvec := ZeroVector(sample,codims[j]);
      counter := 0;
      repeat
          Randomize(regvec);
          ORB_NormalizeVector(regvec);
          # Now U_{j-1}-minimalize it, such that the transversal-words
          # returned reach the U_{j-1}-suborbits we find next:
          regvec := ORB_Minimalize2(regvec,j,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit2(setup,regvec,j,j,j-1,100);
          Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
               " suborbits (need ",sizes[j]/sizes[j-1],")");
      until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
            counter >= 3;
      if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        # Bad luck, try the full space:
        neededfullspace := true;
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        repeat
            Randomize(regvec);
            ORB_NormalizeVector(regvec);
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize2(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit2(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,2,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= 20;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            return;
        fi;
      fi;

      Info(InfoOrb,2,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.trans[j] := o!.words;
      setup!.regvecs[j] := regvec;
      setup!.cosetrecog[j] := ORB_CosetRecogGeneric;
      if not(neededfullspace) then
          setup!.cosetinfo[j] := [o!.db,j];   # the hash table
      else
          setup!.cosetinfo[j] := [o!.db,k+1];   # the hash table
      fi;
  od;
  return setup;
end );


####################################################################
# Functions to administrate lists of (halves of) orbitbysuborbits: #
####################################################################

InstallGlobalFunction( InitOrbitBySuborbitList,
function( setup, nrrandels )
  local firstgen,i,lastgen,obsol,pr;
  obsol := rec( obsos := [], nrrandels := nrrandels, randels := [],
                setup := setup );
  firstgen := Length(setup!.els[setup!.k])+1;
  lastgen := Length(setup!.els[setup!.k+1]);
  pr := ProductReplacer( setup!.els[setup!.k+1]{[firstgen..lastgen]} );
  for i in [1..nrrandels] do
      Add(obsol.randels,Next(pr));
  od;
  return obsol;
end );

InstallGlobalFunction( IsVectorInOrbitBySuborbitList,
function(v,obsol)
  local i,j,k,res,s,x;
  s := obsol.setup;
  k := s!.k;
  for j in [1..obsol.nrrandels] do
      x := s!.op[k+1](v,obsol.randels[j]);
      x := ORB_Minimalize(x,k+1,k,s,false,false);
      for i in [1..Length(obsol.obsos)] do
          res := LookupSuborbit(x,obsol.obsos[i]!.db);
          if res <> fail then  # we know this N-orbit
              return i;  # is in orbit number i
          fi;
      od;
  od;
  return fail;
end );

InstallGlobalFunction( IsVectorInOrbitBySuborbitList2,
function(v,obsol)
  local i,j,k,res,s,x;
  s := obsol.setup;
  k := s!.k;
  for j in [1..obsol.nrrandels] do
      x := s!.op[k+1](v,obsol.randels[j]);
      x := ORB_Minimalize2(x,k+1,k,s,false,false);
      for i in [1..Length(obsol.obsos)] do
          res := LookupSuborbit(x,obsol.obsos[i]!.db);
          if res <> fail then  # we know this N-orbit
              return i;  # is in orbit number i
          fi;
      od;
  od;
  return fail;
end );

InstallGlobalFunction( OrbitsFromSeedsToOrbitList,
function( obsol, li, hashsize, grpsize )
  local o,orb,v;
  for v in li do
      orb := IsVectorInOrbitBySuborbitList(v,obsol);
      if orb = fail then
          o := OrbitBySuborbit(v,hashsize,grpsize,obsol.setup,50);
          if o <> "didnotfinish" then
              Add(obsol.obsos,o);
              Print("New suborbit:\n");
              ViewObj(o);
              Print("\nHave now ",Length(obsol.obsos),
                    " orbits with a total of ",
                    ORB_PrettyStringBigNumber(Sum(obsol.obsos,Size)),
                    " elements.\n");
          fi;
      else
          Info(InfoOrb,2,"Already know orbit ",orb);
      fi;
  od;
end );

InstallGlobalFunction( OrbitsFromSeedsToOrbitList2,
function( obsol, li )
  local o,orb,v,k;
  k := obsol.setup!.k;
  for v in li do
      orb := IsVectorInOrbitBySuborbitList2(v,obsol);
      if orb = fail then
          o := OrbitBySuborbit2(obsol.setup,v,k+1,k+1,k,51);
          if IsOrbitBySuborbit(o) then
              Add(obsol.obsos,o);
              Print("New suborbit:\n");
              ViewObj(o);
              Print("\nHave now ",Length(obsol.obsos),
                    " orbits with a total of ",
                    ORB_PrettyStringBigNumber(Sum(obsol.obsos,Size)),
                    " elements.\n");
          fi;
      else
          Info(InfoOrb,2,"Already know orbit ",orb);
      fi;
  od;
end );

InstallGlobalFunction( VerifyDisjointness,
function( obsol )
  local disjoint,i,j,v;
  disjoint := true; # up to now
  for i in [1..Length(obsol.obsos)-1] do
      Info(InfoOrb,2,"Checking orbit number ",i,"...");
      if Size(obsol.obsos[i]) >= 2 * TotalLength(obsol.obsos[i]!.db) then
          Print("WARNING: Orbit number ",i,"not enumerated >50%!\n");
      fi;
      for j in [i+1..Length(obsol.obsos)] do
          if Size(obsol.obsos[i]) = Size(obsol.obsos[j]) then
              for v in Representatives(obsol.obsos[i]!.db) do
                  # They are already U-minimal!
                  if LookupSuborbit(v,obsol.obsos[j]!.db) <> fail then
                      Print("ATTENTION: Orbits ",i," and ",j," are equal!\n");
                      disjoint := false;
                  fi;
              od;
          fi;
      od;
  od;
  return disjoint;
end );
