#############################################################################
##
##                             orb package
##  byorbits.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
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
        p := SIZE_OBJ(setup!.sample[i]) + 3 * GAPInfo.BytesPerVariable;
        m := m + p * setup!.info[i]!.nr + 2 * SIZE_OBJ(setup!.info[i]!.els);
    od;
    return m;
  end );

InstallGlobalFunction( ORB_StoreWordInCache,
function(setup,w)
  local v;
  v := HTValue(setup!.wordhash,w);
  if v = fail then
      Add(setup!.wordcache,w);
      v := Length(setup!.wordcache);
      HTAdd(setup!.wordhash,w,v);
  fi;
  return v;
end );

InstallGlobalFunction( ORB_Minimalize,
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
  local cos,m,mm,o,oldp,oo,q,qq,tempstab,tups,v,ww,www;
  
  oldp := p;  # to make debugging easier!
  if i = 1 then    # this is the smallest helper subgroup

    # go to P_1:
    if j > 1 then 
      q := setup!.pifunc[j][1](p,setup!.pi[j][1]); 
    else
      q := p;
    fi;
    v := HTValue(setup!.info[1],q);
    if v = fail then    # we do not yet know this point
      o := Enumerate(Orb(setup!.els[1],q,setup!.op[1],
                         rec( schreier := true, 
                              grpsizebound := setup!.size[1],
                              hashlen := NextPrimeInt(setup!.size[1]*2),
                              stabchainrandom := setup!.stabchainrandom,
                              permgens := setup!.permgens[1],
                              permbase := setup!.permbase[1] )));
      tups := List(o!.stabwords,w->ORB_SiftWord(setup,1,w));
      v := rec( tups := tups, size := o!.stabsize, 
                cache := List([1..setup!.k+1],i->WeakPointerObj([])) );
      HTAdd(setup!.info[1],q,v);
      # Now we have to store backward words via the wordcache:
      for m in [2..Length(o!.orbit)] do
          ww := -TraceSchreierTreeBack(o,m);
          ww := ORB_StoreWordInCache(setup,ww);
          HTAdd(setup!.info[1],o!.orbit[m],ww);
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
        v := HTValue(setup!.info[1],q);  # look up stabilizer
      fi; # otherwise we are already U_1-minimal:
    fi;
    if IsRecord(stab) then 
        stab.tups := v.tups;
        stab.size := v.size;
        stab.cache := v.cache;
    fi;
    return p;

  else   # we are in some higher helper subgroup than U_1:

    # first do a U_{i-1}-minimalization:
    tempstab := rec();
    p := ORB_Minimalize(p,j,i-1,setup,tempstab,w);

    # now try to reach the minimal U_{i-1}-suborbit in the U_i-orbit:
    if j > i then
      q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
    else
      q := p;
    fi;
    v := HTValue(setup!.info[i],q);

    if v = fail then    # we do not yet know this U_i-suborbit

      # we define q*U_{i-1} to be the U_i-minimal U_{i-1}-orbit,
      # and q to be the U_i-minimal point in there.
      # now find the other U_{i-1}-orbits:
      o := OrbitBySuborbitInner(setup,q,i,i,i-1,100,fail);
      tups := List(o!.stabwords,w->ORB_SiftWord(setup,i,w));
      v := rec( tups := tups, size := o!.stabsize,
                cache := List([1..setup!.k+1],i->WeakPointerObj([])) );
      HTAdd(setup!.info[i],q,v);
      # Now find all U_{i-1}-minimal elements in q*U_{i-1}, note that
      # tempstab contains generators for Stab_{U_{i-1}}(q)!
      Info(InfoOrb,2+ORB.ORBITBYSUBORBITDEPTH,
           "Starting on-the-fly precomputation (i>1) ...");
      oo := ORB_StabOrbitComplete(tempstab,setup,i,q);
      for m in [2..Length(oo!.orbit)] do
          ww := TraceSchreierTreeForward(oo,m);
          ww := ORB_InvWord(Concatenation( oo!.bysuborbitstabgens.words{ww} ));
          ww := ORB_WordTuple(setup,ORB_SiftWord(setup,i,ww));
          ww := ORB_StoreWordInCache(setup,ww);
          # FIXME: Throw this out eventually?
          if HTValue(setup!.info[i],oo!.orbit[m]) <> fail then
              Error(1);
          fi;
          HTAdd(setup!.info[i],oo!.orbit[m],-ww);
      od;
      
      # Now store all U_{i-1}-minimal elements in the other U_{i-1}-orbits
      # of q*U_i together with a number of a transversal element to reach
      # the minimal U_{i-1}-orbit q*U_{i-1}:

      Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
           "Have to go through ",Length(o!.words)-1," suborbits...");
      for m in [2..Length(o!.words)] do
          qq := ORB_ApplyWord(q,o!.words[m],setup!.els[i],setup!.elsinv[i],
                              setup!.op[i]);
          ww := ShallowCopy(o!.words[m]);
          tempstab := rec();
          qq := ORB_Minimalize(qq,i,i-1,setup,tempstab,ww);
          oo := ORB_StabOrbitComplete(tempstab,setup,i,qq);
          for mm in [1..Length(oo!.orbit)] do
              www := TraceSchreierTreeForward(oo,mm);
              www := Concatenation( oo!.bysuborbitstabgens.words{www} );
              cos := setup!.cosetrecog[i]
                        (i,ORB_InvWord(Concatenation(ww,www)),setup);
              # FIXME: Throw this out eventually?
              if HTValue(setup!.info[i],oo!.orbit[mm]) <> fail then
                  Error(2);
              fi;
              HTAdd(setup!.info[i],oo!.orbit[mm],cos);
          od;
          Info(InfoOrb,4+ORB.ORBITBYSUBORBITDEPTH,"done 1 suborbit.");
      od;
      Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
           "ready with on-the-fly precomputation.");

      # Now the U_{i-1}-orbit of the vector q is the U_i-minimal 
      # U_{i-1}-orbit and q is the U_i-minimal vector
      
      # now q is by definition the U_i-minimal point in the orbit and
      # v its setup!.info[i], i.e. [true,stabilizer information]
      setup!.suborbnr[i] := setup!.suborbnr[i] + 1;
      setup!.sumstabl[i] := setup!.sumstabl[i] + o!.stabsize;

    else   # we already knew this U_{i-1}-suborbit

      if IsInt(v) and v > 0 then    # this is the number of a transversal word
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
        v := HTValue(setup!.info[i],q);
      fi;
      if IsInt(v) then
        # FIXME: Throw this out eventually?
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
        v := HTValue(setup!.info[i],q);
      fi;
      # now q is the U_i-minimal element in qU_i
      # v is now a record with generators for Stab_{U_i}(q)

    fi;

    if IsRecord(stab) then 
        stab.tups := v.tups;
        stab.size := v.size;
        stab.cache := v.cache;
    fi;

    # now we are on the minimal element in the S-orbit
    return p;
  fi;
end );


#######################
# Suborbit databases: #
#######################

InstallMethod( SuborbitDatabase, "for an orbit by suborbit setup object",
  [ IsOrbitBySuborbitSetup, IsPosInt, IsPosInt, IsPosInt ],
  function( setup, j, l, i )
    # j is the number of the representation we are working in, j > i
    # i is the number of subgroups used for the trick, i>=1
    # l is the number of subgroup we enumerate
    local r;
    r := rec( reps := [], lengths := [], setup := setup, totallength := 0,
              i := i, l := l, j := j, nrmins := [] );
    r.mins := HTCreate( setup!.sample[j], rec( hashlen := setup!.hashlen[j]) );
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
  [ IsSuborbitDatabase and IsStdSuborbitDbRep, IsObject, IsRecord, IsPosInt,
    IsPosInt ],
  function(db,p,stab,fullstabsize,percentage)
  # "db" must be a suborbit database
  # "p" must be a U-minimal element, which is not yet known
  # "stab" must be stabilizer information coming from minimalization
  # "fullstabsize" is only for info purposes
  # all U-minimal elements in the same orbit are calculated and stored
  # in the hash, in addition "p" is appended as representative to
  # "suborbits" and the orbit length is calculated and appended to
  # "lengths".
  local i,infolevel,j,l,length,m,o,setup,perc;
        
  setup := db!.setup;
  if db!.l = setup!.k+1 and db!.i = setup!.k and
     percentage < 100 and
     IsBound(setup!.stabsizelimitnostore) and
     stab.size > setup!.stabsizelimitnostore then
      # we always enumerate things in the quotient completely!
      Info(InfoOrb,1,"Ignored suborbit because of stabsizelimitnostore, ",
           "stabiliser size: ",stab.size);
      return fail;
  fi;

  i := db!.i;
  j := db!.j;
  l := db!.l;
  Add(db!.reps,p);
  HTAdd(db!.mins,p,Length(db!.reps));
  o := ORB_StabOrbitComplete(stab,setup,j,p);
  for m in [2..Length(o!.orbit)] do
      HTAdd( db!.mins, o!.orbit[m], Length(db!.reps) );
  od;
  length := setup!.size[i] / (stab.size / Length(o!.orbit));
  Add(db!.lengths,length);
  Add(db!.nrmins,Length(o!.orbit));
  db!.totallength := db!.totallength + length;
  if Length(db!.reps) mod ORB.REPORTSUBORBITS = 0 then
      infolevel := 0;
  else
      infolevel := 1;
  fi;
  perc := QuoInt(db!.totallength * fullstabsize * 100,setup!.size[l]);
  Info(InfoOrb,infolevel+ORB.ORBITBYSUBORBITDEPTH,
       "j=",j," l=",l," i=",i," #",Length(db!.reps),
       " Size:",ORB_PrettyStringBigNumber(length),
       "\c Mins:",Length(o!.orbit)," \cTotal:",
       ORB_PrettyStringBigNumber(db!.totallength),
       "\c Stab:",ORB_PrettyStringBigNumber(fullstabsize),
       "\c (",perc,"%)");
  return length;
end );

InstallMethod( LookupSuborbit, 
  "for a (minimal) point and a std suborbit database",
  [ IsObject, IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( p, db )
    return HTValue( db!.mins, p );
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
    m := 2 * SIZE_OBJ(db!.reps) + 2 * SIZE_OBJ(db!.mins!.els);
    #  (db!.reps and db!.lengths   and    els and vals in db!.mins)
    # Now the points (this assumes vectors!):
    p := SIZE_OBJ(db!.setup!.sample[db!.setup!.k+1])
         + 3 * GAPInfo.BytesPerVariable;   # for the bag
    m := m + db!.mins!.nr * p;  # the reps are also in mins!
    return m;
  end );

InstallMethod( SavingFactor, "for a std suborbit database",
  [ IsSuborbitDatabase and IsStdSuborbitDbRep ],
  function( db )
    if db!.mins!.nr > 0 then
        return QuoInt(db!.totallength,db!.mins!.nr);
    else
        return fail;
    fi;
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

InstallMethod( TotalLength, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return TotalLength(o!.db);
  end );

InstallMethod( Seed, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.seed;
  end );

InstallMethod( OrigSeed, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.origseed;
  end );

InstallMethod( StabWords, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return o!.stabwords;
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
    return Memory(o!.db);
  end );

InstallMethod( SavingFactor, "for an orbit-by-suborbit",
  [ IsOrbitBySuborbit and IsStdOrbitBySuborbitRep ],
  function( o )
    return SavingFactor(o!.db);
  end );

ORB.PATIENCEFORSTAB := 1000;
ORB.REPORTSUBORBITS := 1000;
ORB.MINSHASHLEN := 257;
ORB.ORBITBYSUBORBITDEPTH := 1;   # this means inside!
ORB.PLEASEEXITNOW := false;
ORB.PLEASEEXITNOWWITHRESULT := false;
ORB.TRIESINQUOTIENT := 3;
ORB.TRIESINWHOLESPACE := 20;
ORB.STARTTIME := 0;
ORB.TIMEOUT := infinity;
ORB.RANDOMSTABGENERATION := 100;
ORB.NRSTABHITSLIMIT := 20;

InstallMethod( ORB_StabilizerChainKnownSize,
  "GAP library method for permutation groups",
  [IsPermGroup,IsPosInt],
  function(g,size)
    return StabChain(g,rec(size := size));
  end );

InstallMethod( ORB_BaseStabilizerChain,
  "GAP library method for permutation groups",
  [IsRecord],
  function(S)
    return BaseStabChain(S);
  end );

InstallMethod( ORB_StabilizerChainKnownBase,
  "GAP library method for permutation groups",
  [IsPermGroup,IsObject],
  function(g,base)
    if base = fail then
        return StabChain(g,rec(random := 900));
    else
        return StabChain(g,rec(base := base, random := 900,
                               knownBase := base, reduced := false));
    fi;
  end );

InstallMethod( ORB_SizeStabilizerChain,
  "GAP library method for permutation groups",
  [IsRecord], SizeStabChain );

InstallMethod( ORB_IsWordInStabilizerChain,
  "GAP library method for permutation groups",
  [IsList, IsList, IsList, IsRecord],
  function( word, permgens, permgensinv, S )
    local b,bi;
    b := BaseStabChain(S);
    bi := ORB_ApplyWord(b,word,permgens,permgensinv,OnTuples);
    return ORB_SiftBaseImage(S,bi,1);
  end );

InstallMethod( ORB_IsElementInStabilizerChain,
  "GAP library method for permutation groups",
  [IsPerm, IsRecord],
  function( el, S )
    return IsOne(SiftedPermutation(S,el));
  end );

InstallGlobalFunction( ORB_WordOp,
  function(p,w)
    # w a quadruple [word,gens,invgens,op]
    return ORB_ApplyWord(p,w[1],w[2],w[3],w[4]);
  end );

InstallGlobalFunction( ORB_GetTransversalElement,
  function(setup,j,i,t)
    # gets the t-th transversal element of U_{i-1} in U_i in rep. j >= i
    # implements a caching mechanism
    local cn,el,mem;
    cn := ElmWPObj(setup!.transcache[j][i],t);
    if cn <> fail then 
        UseCacheObject(setup!.cache,cn);
        return cn!.ob;
    fi;
    # Now we have to calculate it:
    el := ORB_ApplyWord(setup!.els[j][1]^0,setup!.trans[i][t],
                        setup!.els[j],setup!.elsinv[j],OnRight);
    mem := Length(el)*SIZE_OBJ(el[1]);
    cn := CacheObject(setup!.cache,el,mem);
    SetElmWPObj(setup!.transcache[j][i],t,cn);
    return el;
  end );
    
InstallGlobalFunction( ORB_PrepareStabgens,
  function(stab,setup,j,big)
    local gen,i,mem,r,t,tup,w,cn;
    if big then
        r := rec( gens := [], words := [], op := setup!.op[j] );
        for t in [1..Length(stab.tups)] do
            tup := stab.tups[t];
            cn := ElmWPObj(stab.cache[j],t);
            if cn <> fail then
                Add(r.gens,cn!.ob.gen);
                Add(r.words,cn!.ob.w);
                UseCacheObject(setup!.cache,cn);
            else
                i := Length(tup);
                gen := ORB_GetTransversalElement(setup,j,i,tup[i]);
                w := ShallowCopy(setup!.trans[i][tup[i]]);
                while i > 1 do
                    i := i - 1;
                    gen := gen * ORB_GetTransversalElement(setup,j,i,tup[i]);
                    Append(w,setup!.trans[i][tup[i]]);
                od;
                Add(r.gens,gen);
                Add(r.words,w);
                mem := Length(gen)*SIZE_OBJ(gen[1]);
                # we ignore the memory for the word and the record!
                SetElmWPObj(stab.cache[j],t,CacheObject(setup!.cache,
                                               rec(gen := gen, w := w),mem));
            fi;
        od;
    else
        r := rec( gens := 
                     List( stab.tups, t->[ORB_WordTuple(setup,t),
                           setup!.els[j],setup!.elsinv[j],setup!.op[j]] ),
                  op := ORB_WordOp );
        r.words := List(r.gens,x->x[1]);
    fi;
    return r;
  end );

InstallGlobalFunction( ORB_StabOrbitComplete,
  function(stab,setup,i,p)
    local stabgens,o;
    stabgens := ORB_PrepareStabgens(stab,setup,i,false);
    o := Enumerate(Orb(stabgens.gens,p,stabgens.op,
                       rec(hashlen := ORB.MINSHASHLEN,
                           schreier := true, grpsizebound := stab.size)),
                   setup!.staborblenlimit);
    o!.bysuborbitstabgens := stabgens;   # trick to return this
    if not(IsClosedOrbit(o)) then
        Info(InfoOrb,3,"Long stabiliser orbit found, multiplying out gens...");
        stabgens := ORB_PrepareStabgens(stab,setup,i,true);
        o!.gens := stabgens.gens;
        o!.op := stabgens.op;
        Enumerate(o);   # go on with other implementation of same operation!
    fi;
    return o;
  end );

InstallGlobalFunction( ORB_StabOrbitSearch,
  function(stab,setup,i,p,needle)
    local stabgens,o;
    stabgens := ORB_PrepareStabgens(stab,setup,i,false);
    o := Enumerate(Orb(stabgens.gens,p,stabgens.op,
                       rec(hashlen := ORB.MINSHASHLEN,
                           schreier := true, grpsizebound := stab.size,
                           lookingfor := [needle])),
                   setup!.staborblenlimit);
    o!.bysuborbitstabgens := stabgens;   # trick to return this
    if o!.found = false then
        Info(InfoOrb,3,"Long stabiliser orbit found, multiplying out gens...");
        stabgens := ORB_PrepareStabgens(stab,setup,i,true);
        o!.gens := stabgens.gens;
        o!.op := stabgens.op;
        Enumerate(o);   # go on with other implementation of same operation!
    fi;
    return o;
  end );
    
InstallGlobalFunction( ORB_SiftWord,
  function(setup,i,w)
    # Assumes that w is a word in U_i and tries to write it as a 
    # shorter word in the generators of U_1, ..., U_i.
    # w may contain generators of U_j for j > i!
    # Uses cosetrecog.
    local l,x;
    l := 0*[1..i];
    while i > 0 do
        x := setup!.cosetrecog[i](i,w,setup);
        # now  w \in trans[i][x] U_{i-1}
        # ==>  trans[i][x]^-1 w \in U_{i-1}
        if x > 1 then
            w := Concatenation(ORB_InvWord(setup!.trans[i][x]),w);
        fi;
        l[i] := x;
        i := i - 1;
    od;
    return l;
  end );

InstallGlobalFunction( ORB_WordTuple,
  function( setup, tup )
    local i,w;
    w := [];
    for i in [Length(tup),Length(tup)-1..1] do
        Append(w,setup!.trans[i][tup[i]]);
    od;
    return w;
  end );

# The following is just a wrapper to get the ORBITBYSUBORBITDEPTH right!
# Internally, we always use OrbitBySuborbitInner below.
InstallGlobalFunction( OrbitBySuborbit,
function(setup,p,j,l,i,percentage)
  local o;
  ORB.ORBITBYSUBORBITDEPTH := 0;
  ORB.STARTTIME := Runtime();
  o := OrbitBySuborbitInner(setup,p,j,l,i,percentage,fail);
  ORB.ORBITBYSUBORBITDEPTH := 1;
  return o;
end );

InstallGlobalFunction( OrbitBySuborbitKnownSize,
function(setup,p,j,l,i,percentage,knownsize)
  local o;
  ORB.ORBITBYSUBORBITDEPTH := 0;
  ORB.STARTTIME := Runtime();
  o := OrbitBySuborbitInner(setup,p,j,l,i,percentage,knownsize);
  ORB.ORBITBYSUBORBITDEPTH := 1;
  return o;
end );
  
InstallGlobalFunction( OrbitBySuborbitInner,
function(setup,p,j,l,i,percentage,knownsize)
  # Enumerates the orbit of p under the group U_l (with G=U_{k+1}) by
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
  # "knownsize" is either fail or the known size of the orbit to enumerate
  #         In the latter case the stabilizer is not computed.
  # Returns a suborbit database with additional field "words" which is
  # a list of words in gens which can be used to reach U-orbit in the G-orbit
  local assumestabcomplete,db,firstgen,fullstabsize,ii,lastgen,m,miniwords,
        mw,newperm,newword,o,oldtodo,stab,stabg,stabgens,stabchain,prep,
        stabilizer,stabperms,stabpr,sw,todo,v,words,x,firstgenU,lastgenU,
        triedstabgens,haveappliedU,MakeReturnObj,y,repforsuborbit,
        oldrepforsuborbit,xx,stab2,mw2,sw2,stabg2,todovecs,oldtodovecs,xxx,bi,
        origp,pp,el,slp,count,nrstabhits;

  Info(InfoOrb,3,"Entering OrbitBySuborbit j=",j," l=",l," i=",i);
  ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH + 1;

  if not(j >= l and l > i and i >= 1) then
      Error("Need j >= l > i >= 1");
      ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
      return fail;
  fi;

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
      ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
      return fail;
  fi;

  # First we U_i-minimalize p:
  stab := rec();
  origp := p;
  p := ORB_Minimalize(p,j,i,setup,stab,false);

  miniwords := [[]];  # here we collect U-minimalizing elements
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase(setup,j,l,i);
  if StoreSuborbit(db,p,stab,1,percentage) = fail then
      Print("Error: Cannot store first suborbit\n");
      ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
      return ["didnotfinish",db,1]; 
  fi;

  stabgens := [];
  stabperms := [];
  stabilizer := Group(setup!.permgens[l][1]^0);
  if knownsize <> fail then
      fullstabsize := setup!.size[l] / knownsize;
      assumestabcomplete := true;
  else
      stabchain := ORB_StabilizerChainKnownBase(stabilizer,setup!.permbase[l]);
      ##stabchain := StabChainOp(stabilizer,rec( random:=setup!.stabchainrandom,
      ##                                         base := setup!.permbase[l], 
      ##                                         reduced := false ));
      fullstabsize := 1;
  fi;
  # note j >= l:
  stabpr := ProductReplacer(
               GeneratorsWithMemory(setup!.els[j]{[firstgen..lastgen]}),
               rec( maxdepth := 1000 ));
  
  words := [[]];
  todo := [[]];
  todovecs := [p];
  repforsuborbit := [1];
  
  triedstabgens := 0;     # this counts only the "failed" ones in a row
  haveappliedU := false;  # is set to true at the end of the following loop

  MakeReturnObj := function()
    # This is used twice below, it just gathers some information.
    Info(InfoOrb,1,"OrbitBySuborbit found ",percentage,"% of a U",l,
         "-orbit of size ",
         ORB_PrettyStringBigNumber(setup!.size[l]/fullstabsize));
    if fullstabsize * TotalLength(db) > setup!.size[l] then
        Error("Product of fullstabsize and Size(db) > groupsize!");
    fi;
    return Objectify( StdOrbitBySuborbitsType,
                      rec(db := db,
                      words := words,
                      miniwords := miniwords,
                      stabsize := fullstabsize,
                      stab := stabilizer,
                      stabwords := stabgens,
                      groupsize := setup!.size[l],
                      orbitlength := setup!.size[l]/fullstabsize,
                      percentage := percentage,
                      seed := p,
                      origseed := origp ) );
  end;
    
  # Just for the case that there is only one U_i orbit:
  if TotalLength(db) * fullstabsize * 100 >= setup!.size[l]*percentage then 
    Info(InfoOrb,3,"Leaving OrbitBySuborbit j=",j," l=",l," i=",i);
    ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
    return MakeReturnObj();
  fi;

  nrstabhits := 0;

  while true do

    ii := 1;
    while ii <= Length(todo) do
      # Note: The following only guarantees the correct stabilizer
      # if percentage is >= 50!
      if TotalLength(db) * fullstabsize * 100 >= 
                         setup!.size[l]*percentage or
         ORB.PLEASEEXITNOWWITHRESULT = true then 
          Info(InfoOrb,3,"Leaving OrbitBySuborbit j=",j," l=",l," i=",i);
          ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
          ORB.PLEASEEXITNOWWITHRESULT := false;   # for next time
          return MakeReturnObj();
      fi;
      if ORB.ORBITBYSUBORBITDEPTH = 1 and 
         (ORB.PLEASEEXITNOW = true or
          QuoInt(Runtime() - ORB.STARTTIME,1000) > ORB.TIMEOUT) then 
          ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
          ORB.PLEASEEXITNOW := false;  # for next time
          return ["didnotfinish",db,fullstabsize]; 
      fi;

      for m in [firstgen..lastgen] do
        # old code:
        # xx := ORB_ApplyWord(p,todo[ii],setup!.els[j],
        #                     setup!.elsinv[j],setup!.op[j]);
        xx := todovecs[ii];
        xxx := setup!.op[j](xx,setup!.els[j][m]);
        mw := [];
        x := ORB_Minimalize(xxx,j,i,setup,stab,mw);
        v := LookupSuborbit(x,db);
        if v = fail then   # a new suborbit
          # if StoreSuborbit fails, we just ignore this suborbit!
          if StoreSuborbit(db,x,stab,fullstabsize,percentage) <> fail then
            Add(words,Concatenation(todo[ii],[m]));
            Add(todo,Concatenation(todo[ii],[m]));
            Add(todovecs,xxx);
            Add(miniwords,mw);
            Add(repforsuborbit,Length(db!.reps));
            # Note: The following only guarantees the correct stabilizer
            # if percentage is >= 50!
            if TotalLength(db) * fullstabsize * 100 >= 
                               setup!.size[l]*percentage then 
              Info(InfoOrb,3,"Leaving OrbitBySuborbit j=",j," l=",l," i=",i);
              ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
              return MakeReturnObj();
            fi;
            if haveappliedU then   
                # In this case we still want to calculate a Schreier gen,
                # thus we need v to be the number of the newly stored suborbit
                v := Length(db!.reps);
                # Note that stab is still OK.
            fi;
          fi;
        fi;
        if v <> fail and   # fail happens only for not(haveappliedU)
           assumestabcomplete = false and
           TotalLength(db) * fullstabsize * 2 <= setup!.size[l] then
          # otherwise we know that we will not find more stabilizing els.
          # we know now that v is an integer and that
          # p*todo[ii]*setup!.els[m]*U = p*words[v]*U
          # p*todo[ii]*setup!.els[m]*mw is our new vector
          # p*words[v]*miniwords[v] is our old vector
          # they differ by an element in Stab_U(...)
          #
          # Now we distinguish two cases: if haveappliedU is false, we
          # are in the first phase, that is, todo[ii] is the chosen
          # representative for p*todo[ii]*U, thus we can directly
          # make a Schreier generator:
          if not(haveappliedU) then
            o := ORB_StabOrbitSearch(stab,setup,j,Representatives(db)[v],x);
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( o!.bysuborbitstabgens.words{sw} );
            newword := Concatenation(todo[ii],[m],mw,ORB_InvWord(sw),
                            ORB_InvWord(miniwords[v]),ORB_InvWord(words[v]));
          else
            # in this case todo[ii] is not the chosen representative for
            # p*todo[ii]*U because we have already applied elements of
            # U from the right to those chosen representatives. Thus we
            # have to calculate the chosen representative:
            # First take xx and minimalize it:
            mw2 := [];
            stab2 := rec();
            xx := ORB_Minimalize(xx,j,i,setup,stab2,mw2);
            y := Representatives(db)[repforsuborbit[ii]];
            o := ORB_StabOrbitSearch(stab2,setup,j,y,xx);
            sw2 := TraceSchreierTreeForward(o,o!.found);
            sw2 := Concatenation( o!.bysuborbitstabgens.words{sw2} );
            # Now Concatenation(words[repforsuborbit[ii]],
            #                   miniwords[repforsuborbit[ii]],sw2,mw2^-1)
            # is the transversal element for the original xx
            # Now as in the simpler case:
            o := ORB_StabOrbitSearch(stab,setup,j,Representatives(db)[v],x);
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( o!.bysuborbitstabgens.words{sw} );
            newword := Concatenation(words[repforsuborbit[ii]],
                                     miniwords[repforsuborbit[ii]],sw2,
                                     ORB_InvWord(mw2),[m],mw,ORB_InvWord(sw),
                                     ORB_InvWord(miniwords[v]),
                                     ORB_InvWord(words[v]));
          fi;
          Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
               "Calculated Schreier generator as word");
          ##bi := ORB_ApplyWord(setup!.permbase[l],newword,setup!.permgens[l],
          ##                    setup!.permgensinv[l],OnTuples);
          ##if ORB_SiftBaseImage(stabchain,bi,1) = false then
          if ORB_IsWordInStabilizerChain(newword,setup!.permgens[l],
                 setup!.permgensinv[l],stabchain) = false then
            Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
                 "Schreier generator is new");
              newperm := ORB_ApplyWord(setup!.permgens[l][1]^0,newword,
                            setup!.permgens[l],setup!.permgensinv[l],OnRight);
              triedstabgens := 0;   # we actually found a new one!
              nrstabhits := 0;
              Add(stabgens,newword);
              Add(stabperms,newperm);
              stabilizer := GroupWithGenerators(stabperms);
              Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                   "Calculating new estimate of the stabilizer...");
              stabchain := ORB_StabilizerChainKnownBase(stabilizer,
                                    setup!.permbase[l]);
              fullstabsize := ORB_SizeStabilizerChain(stabchain);
              ##stabchain := StabChainOp(stabilizer,
              ##                         rec( random := setup!.stabchainrandom,
              ##                              base := setup!.permbase[l], 
              ##                              reduced := false ));
              ##fullstabsize := SizeStabChain(stabchain);
              Info(InfoOrb,ORB.ORBITBYSUBORBITDEPTH,
                   "New stabilizer order: ",fullstabsize," (l=",l,")");
              if TotalLength(db) * fullstabsize * 100
                 >= setup!.size[l]*percentage then 
                Info(InfoOrb,3,"Leaving OrbitBySuborbit");
                ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
                return MakeReturnObj();
              fi;
          fi;
          triedstabgens := triedstabgens + 1;
          if triedstabgens > ORB.PATIENCEFORSTAB then  # this is heuristics!
            Info(InfoOrb,ORB.ORBITBYSUBORBITDEPTH,
                 "Lost patience with stabiliser, assuming it is complete...");
            assumestabcomplete := true;
          fi;
        fi;
      od;   # for m in [firstgen..lastgen]
      # Try a random element for the stabiliser:
      if ORB.ORBITBYSUBORBITDEPTH = 1 and ORB.RANDOMSTABGENERATION > 0 and
         assumestabcomplete = false and
         TotalLength(db) * fullstabsize * 2 <= setup!.size[l] then
          Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
               "Trying ",ORB.RANDOMSTABGENERATION,
               " random elements to find a stabiliser element...");
          for count in [1..ORB.RANDOMSTABGENERATION] do
              el := Next(stabpr);
              pp := setup!.op[j](p,StripMemory(el));
              mw := [];
              pp := ORB_Minimalize(pp,j,i,setup,stab,mw);
              v := LookupSuborbit(pp,db);
              if v <> fail then
                  nrstabhits := nrstabhits + 1;
                  Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                       "Random element hit enumerated part, nrhits=",
                       nrstabhits);
                  o := ORB_StabOrbitSearch(stab,setup,j,
                                           Representatives(db)[v],pp);
                  sw := TraceSchreierTreeForward(o,o!.found);
                  sw := Concatenation( o!.bysuborbitstabgens.words{sw} );
                  sw := Concatenation( words[v], miniwords[v], sw,
                                       ORB_InvWord(mw) );
                  slp := SLPOfElm(el);
                  newperm := ORB_ApplyWord( 
                      ResultOfStraightLineProgram(slp,
                         setup!.permgens[l]{[firstgen..lastgen]}),
                      ORB_InvWord(sw), setup!.permgens[l], 
                      setup!.permgensinv[l], OnRight);
                  if not ORB_IsElementInStabilizerChain(newperm,stabchain) then
                      nrstabhits := 0;
                      triedstabgens := 0;   # we actually found a new one!
                      Add(stabgens,"unknown");
                      Add(stabperms,newperm);
                      stabilizer := GroupWithGenerators(stabperms);
                      Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                           "Calculating new estimate of the stabilizer...");
                      stabchain := ORB_StabilizerChainKnownBase(stabilizer,
                                            setup!.permbase[l]);
                      fullstabsize := ORB_SizeStabilizerChain(stabchain);
                      Info(InfoOrb,ORB.ORBITBYSUBORBITDEPTH,
                           "New stabilizer order: ",fullstabsize," (l=",l,")");
                  fi;
              fi;
          od;
          if nrstabhits > ORB.NRSTABHITSLIMIT then
              Info(InfoOrb,ORB.ORBITBYSUBORBITDEPTH,
                   "Enough hits of stabiliser, assuming it is complete...");
              assumestabcomplete := true;
          fi;
      fi;
      ii := ii + 1;
    od;
  
    oldtodo := todo;
    oldtodovecs := todovecs;
    todo := [];
    todovecs := [];
    oldrepforsuborbit := repforsuborbit;
    repforsuborbit := [];
    for ii in [firstgenU..lastgenU] do
      Append(todo,List(oldtodo,w->Concatenation(w,[ii])));
      Append(todovecs,List(oldtodovecs,w->setup!.op[j](w,setup!.els[j][ii])));
      Append(repforsuborbit,oldrepforsuborbit);
    od;
    Info(InfoOrb,2+ORB.ORBITBYSUBORBITDEPTH,
         "Length of next todo: ",Length(todo));
    haveappliedU := true;
  od;
  # this is never reached
end );


############################
# Convenient preparations: #
############################

InstallGlobalFunction( ORB_NormalizeVector,
  function(v)
    local c;
    c := PositionNonZero(v);
    if c <= Length(v) then
        MultVector(v,v[c]^-1);
    fi;
    return v;
  end );

InstallGlobalFunction( ORB_PermuteBasisVectors,
  function( m, ra )
    local i;
    m := m{ra};
    for i in [1..Length(m)] do
        m[i] := m[i]{ra};
    od;
    return m;
  end );

InstallGlobalFunction( ORB_EmbedBaseChangeTopLeft,
  function( t, dim )
    local T,d;
    T := IdentityMatrix(dim,t);
    d := Length(t);
    CopySubMatrix(t,T,[1..d],[1..d],[1..d],[1..d]);
    return T;
  end );

InstallGlobalFunction( ORB_CosetRecogGeneric,
  function( i, w, s )
    local x,j;
    j := s!.cosetinfo[i][2];
    x := ORB_ApplyWord(s!.regvecs[i],w,s!.els[j],s!.elsinv[j],s!.op[j]);
    x := ORB_Minimalize(x,j,i-1,s,false,false);
    return LookupSuborbit(x,s!.cosetinfo[i][1]);
  end );

InstallGlobalFunction( ORB_CosetRecogPermgroup,
  function( i, w, s )
    # only for i=1 possible!
    local x,k;
    k := s!.k;
    x := ORB_ApplyWord(s!.permbase[k],w,
                       s!.permgens[k],s!.permgensinv[k],OnTuples);
    return Position(s!.cosetinfo[1],x);
  end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForVectors,
function(gens,permgens,sizes,codims,opt)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k < G
  # codims: a list of dimensions of factor modules
  # opt: a record for options, can be empty
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local counter,dim,doConversions,g,i,j,k,neededfullspace,nrgens,nrgenssum,
        o,q,regvec,sample,setup,sm,sum,merk;

  merk := ORB.RANDOMSTABGENERATION;
  ORB.RANDOMSTABGENERATION := 0;

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
      ORB.RANDOMSTABGENERATION := merk;
      return fail;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  # the future:
  #sample := ZeroVector(NrCols(gens[1][1]),gens[1][1]);  
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
  if Length(permgens[k+1]) = nrgens[k+1] then
      # old calling convention
      setup.permgens[k+1] := Concatenation(permgens);
  else
      setup.permgens[k+1] := ShallowCopy(permgens[k+1]);
  fi;
  setup.permgensinv[k+1] := List(setup.permgens[k+1],x->x^-1);
  g := Group(setup.permgens[k+1]{[nrgenssum[k+1]+1..nrgenssum[k+2]]});
  SetSize(g,sizes[k+1]);
  if IsTrivial(g) or
     (IsBound(opt.nostabchainfullgroup) and opt.nostabchainfullgroup) then
      setup.permbase[k+1] := fail;
  else
      Info(InfoOrb,1,"Calculating stabilizer chain for whole group...");
      setup.permbase[k+1] := ORB_BaseStabilizerChain(
                                ORB_StabilizerChainKnownSize(g,Size(g)));
      ##setup.permbase[k+1] := BaseStabChain(StabChainOp(g,rec()));
  fi;
  for i in [k,k-1..1] do
      if Length(permgens[i]) = nrgens[i] then
          # old calling convention:
          setup.permgens[i] := setup.permgens[i+1]{[1..nrgenssum[i+1]]};
      else
          setup.permgens[i] := ShallowCopy(permgens[i]);
      fi;
      g := Group(setup.permgens[i]{[nrgenssum[i]+1..nrgenssum[i+1]]});
      SetSize(g,sizes[i]);
      if IsPermGroup(g) then
          Info(InfoOrb,1,"Trying smaller degree permutation representation ",
               "for U",i,"...");
          sm := SmallerDegreePermutationRepresentation(g:cheap);
          if not(IsOne(sm)) then   # not the identity
              Info(InfoOrb,1,"Found one on ",
                   LargestMovedPoint(GeneratorsOfGroup(Image(sm)))," points.");
              for j in [1..Length(setup.permgens[i])] do
                  setup.permgens[i][j] := ImageElm(sm,setup.permgens[i][j]);
              od;
              g := Image(sm);
          fi;
      fi;
      setup.permgensinv[i] := List(setup.permgens[i],x->x^-1);
      ##setup.permbase[i] := BaseStabChain(StabChainOp(g,rec()));
      Info(InfoOrb,1,"Computing a base for helper subgroup #",i);
      setup.permbase[i] := ORB_BaseStabilizerChain(
                              ORB_StabilizerChainKnownSize(g,sizes[i]));
  od;
   setup.stabchainrandom := 1000;

   setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := [NextPrimeInt(3*sizes[1])];
  Append(setup.hashlen,List([2..k+1],i->NextPrimeInt(
                Minimum(3*(sizes[i]/sizes[i-1]),1000000))));
  setup.sample := [];
  setup.sample[k+1] := sample;
  dim := Length(sample);
  codims[k+1] := dim;   # for the sake of completeness!
  setup.staborblenlimit := dim;
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
  setup.cache := LinkedListCache(100000000);  # 100 MB cache
  setup.transcache := List([1..k+1],j->List([1..j],i->WeakPointerObj([])));

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  setup.info := [HTCreate(setup.sample[1], rec( hashlen :=
                          NextPrimeInt((q^codims[1]) * 3)))];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
      if j < k+1 then
          setup.info[j] := HTCreate(setup.sample[j], rec( hashlen :=
                   NextPrimeInt(QuoInt((q^codims[j])*3,sizes[j-1]))) );
      fi;
  od;
  setup.suborbnr := 0*[1..k];
  setup.sumstabl := 0*[1..k];
  setup.regvecs := [];
  setup.op := List([1..k+1],i->OnRight);
  setup.wordcache := [];
  setup.wordhash := HTCreate([1,2,3],rec( hashlen := 1000 ));

  Objectify( NewType(OrbitBySuborbitSetupFamily,
                     IsStdOrbitBySuborbitSetupRep and IsMutable),
             setup );
  # From now on we can use it and it is an object!

  # We do the recognition of elements of U_1 by the permutation rep:
  # FIXME: later devise code if U_1 in permgens is not a permutation grp.
  Info(InfoOrb,1,"Enumerating permutation base images of U_1...");
  setup!.cosetinfo[1] := Orb(setup!.permgens[k]{[1..nrgens[1]]},
                             setup!.permbase[k],OnTuples,
                             NextPrimeInt(3*sizes[1]+1),
                             rec( schreier := true,storenumbers := true ));
  Enumerate(setup!.cosetinfo[1]);
  setup!.cosetrecog[1] := ORB_CosetRecogPermgroup;
  setup!.trans[1] := List([1..Length(setup!.cosetinfo[1])],
                          x->TraceSchreierTreeForward(setup!.cosetinfo[1],x));

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!

      neededfullspace := false;

      Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
           "in factor space...");
      regvec := ZeroVector(codims[k],sample);
      counter := 0;
      repeat
          if IsBound(opt.regvecfachints) and IsBound(opt.regvecfachints[j]) and
             IsBound(opt.regvecfachints[j][counter+1]) then
              CopySubVector(opt.regvecfachints[j][counter+1],regvec,
                            [1..Length(regvec)],[1..Length(regvec)]);
              Info(InfoOrb,1,"Taking hint #",counter+1);
          else
              Randomize(regvec);
          fi;
          # Now U_{j-1}-minimalize it, such that the transversal-words
          # returned reach the U_{j-1}-suborbits we find next:
          regvec := ORB_Minimalize(regvec,k,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit(setup,regvec,k,j,j-1,100);
          Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
               " suborbits (need ",sizes[j]/sizes[j-1],")");
      until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
            counter >= ORB.TRIESINQUOTIENT;
      if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        # Bad luck, try the full space:
        neededfullspace := true;
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        repeat
            if IsBound(opt.regvecfullhints) and 
               IsBound(opt.regvecfullhints[j]) and
               IsBound(opt.regvecfullhints[j][counter+1]) then
                CopySubVector(opt.regvecfullhints[j][counter+1],regvec,
                              [1..Length(regvec)],[1..Length(regvec)]);
                Info(InfoOrb,1,"Taking hint #",counter+1);
            else
                Randomize(regvec);
            fi;
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= ORB.TRIESINWHOLESPACE;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            ORB.RANDOMSTABGENERATION := merk;
            return fail;
        fi;
      fi;

      Info(InfoOrb,2,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.trans[j] := o!.words;
      setup!.regvecs[j] := regvec;
      setup!.cosetrecog[j] := ORB_CosetRecogGeneric;
      if not(neededfullspace) then
          setup!.cosetinfo[j] := [o!.db,k];   # the hash table
      else
          setup!.cosetinfo[j] := [o!.db,k+1];   # the hash table
      fi;
  od;
  if IsBound(opt.stabchainrandom) then
      setup!.stabchainrandom := opt.stabchainrandom;
  else
      setup!.stabchainrandom := 1000;  # no randomization by default
  fi;
  ORB.RANDOMSTABGENERATION := merk;
  return setup;
end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForLines,
function(gens,permgens,sizes,codims,opt)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k < G
  # codims: a list of dimensions of factor modules
  # opt: a record for options, can be empty
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local counter,dim,doConversions,g,i,j,k,neededfullspace,nrgens,nrgenssum,
        o,q,regvec,sample,setup,sm,sum,merk;

  merk := ORB.RANDOMSTABGENERATION;
  ORB.RANDOMSTABGENERATION := 0;

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
      ORB.RANDOMSTABGENERATION := merk;
      return fail;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  # the future:
  #sample := ZeroVector(NrCols(gens[1][1]),gens[1][1]);  
  sample := gens[1][1][1];    # first vector of first generator

  # First preparations:
  setup := rec(k := k);
  setup.size := ShallowCopy(sizes);
  setup.index := sizes{[1..k]};
  for i in [k,k-1..2] do setup.index[i] := setup.index[i]/setup.index[i-1]; od;

  # Calculate stabilizer chain for whole group:
  setup.permgens := [];
  setup.permgensinv := [];
  setup.permbase := [];
  if Length(permgens[k+1]) = nrgens[k+1] then
      # old calling convention
      setup.permgens[k+1] := Concatenation(permgens);
  else
      setup.permgens[k+1] := ShallowCopy(permgens[k+1]);
  fi;
  setup.permgensinv[k+1] := List(setup.permgens[k+1],x->x^-1);
  g := Group(setup.permgens[k+1]{[nrgenssum[k+1]+1..nrgenssum[k+2]]});
  SetSize(g,sizes[k+1]);
  if IsTrivial(g) or
     (IsBound(opt.nostabchainfullgroup) and opt.nostabchainfullgroup) then
      setup.permbase[k+1] := fail;
  else
      Info(InfoOrb,1,"Calculating stabilizer chain for whole group...");
      setup.permbase[k+1] := ORB_BaseStabilizerChain(
                                ORB_StabilizerChainKnownSize(g,Size(g)));
      ##setup.permbase[k+1] := BaseStabChain(StabChainOp(g,rec()));
  fi;
  for i in [k,k-1..1] do
      if Length(permgens[i]) = nrgens[i] then
          # old calling convention:
          setup.permgens[i] := setup.permgens[i+1]{[1..nrgenssum[i+1]]};
      else
          setup.permgens[i] := ShallowCopy(permgens[i]);
      fi;
      g := Group(setup.permgens[i]{[nrgenssum[i]+1..nrgenssum[i+1]]});
      SetSize(g,sizes[i]);
      if IsPermGroup(g) then
          Info(InfoOrb,1,"Trying smaller degree permutation representation ",
               "for U",i,"...");
          sm := SmallerDegreePermutationRepresentation(g:cheap);
          if not(IsOne(sm)) then   # not the identity
              Info(InfoOrb,1,"Found one on ",
                   LargestMovedPoint(GeneratorsOfGroup(Image(sm)))," points.");
              for j in [1..Length(setup.permgens[i])] do
                  setup.permgens[i][j] := ImageElm(sm,setup.permgens[i][j]);
              od;
              g := Image(sm);
          fi;
      fi;
      setup.permgensinv[i] := List(setup.permgens[i],x->x^-1);
      ##setup.permbase[i] := BaseStabChain(StabChainOp(g,rec()));
      Info(InfoOrb,1,"Computing a base for helper subgroup #",i);
      setup.permbase[i] := ORB_BaseStabilizerChain(
                              ORB_StabilizerChainKnownSize(g,sizes[i]));
  od;
  setup.stabchainrandom := 1000;

  setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := [NextPrimeInt(3*sizes[1])];
  Append(setup.hashlen,List([2..k+1],i->NextPrimeInt(
                Minimum(3*(sizes[i]/sizes[i-1]),1000000))));
  setup.sample := [];
  setup.sample[k+1] := sample;
  dim := Length(sample);
  codims[k+1] := dim;   # for the sake of completeness!
  setup.staborblenlimit := dim;
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
  setup.cache := LinkedListCache(100000000);  # 100 MB cache
  setup.transcache := List([1..k+1],j->List([1..j],i->WeakPointerObj([])));

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  setup.info := [HTCreate(setup.sample[1], rec( hashlen :=
                          NextPrimeInt((q^codims[1]-1)/(q-1) * 3) ))];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
      if j < k+1 then
          setup.info[j] := HTCreate(setup.sample[j], rec( hashlen :=
                   NextPrimeInt(QuoInt((q^codims[j]-1)/(q-1)*3,sizes[j-1])) ));
      fi;
  od;
  setup.suborbnr := 0*[1..k];
  setup.sumstabl := 0*[1..k];
  setup.regvecs := [];
  setup.op := List([1..k+1],i->OnLines);
  setup.wordcache := [];
  setup.wordhash := HTCreate([1,2,3],rec( hashlen := 1000 ));

  Objectify( NewType(OrbitBySuborbitSetupFamily,
                     IsStdOrbitBySuborbitSetupRep and IsMutable),
             setup );
  # From now on we can use it and it is an object!

  # We do the recognition of elements of U_1 by the permutation rep:
  # FIXME: later devise code if U_1 in permgens is not a permutation grp.
  Info(InfoOrb,1,"Enumerating permutation base images of U_1...");
  setup!.cosetinfo[1] := Orb(setup!.permgens[k]{[1..nrgens[1]]},
                             setup!.permbase[k],OnTuples,
                             NextPrimeInt(3*sizes[1]+1),
                             rec( schreier := true,storenumbers := true ));
  Enumerate(setup!.cosetinfo[1]);
  setup!.cosetrecog[1] := ORB_CosetRecogPermgroup;
  setup!.trans[1] := List([1..Length(setup!.cosetinfo[1])],
                          x->TraceSchreierTreeForward(setup!.cosetinfo[1],x));

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!

      neededfullspace := false;

      Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
           "in factor space...");
      regvec := ZeroVector(codims[k],sample);
      counter := 0;
      repeat
          if IsBound(opt.regvecfachints) and IsBound(opt.regvecfachints[j]) and
             IsBound(opt.regvecfachints[j][counter+1]) then
              CopySubVector(opt.regvecfachints[j][counter+1],regvec,
                            [1..Length(regvec)],[1..Length(regvec)]);
              Info(InfoOrb,1,"Taking hint #",counter+1);
          else
              Randomize(regvec);
          fi;
          ORB_NormalizeVector(regvec);
          # Now U_{j-1}-minimalize it, such that the transversal-words
          # returned reach the U_{j-1}-suborbits we find next:
          regvec := ORB_Minimalize(regvec,k,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit(setup,regvec,k,j,j-1,100);
          Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
               " suborbits (need ",sizes[j]/sizes[j-1],")");
      until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
            counter >= ORB.TRIESINQUOTIENT;
      if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        # Bad luck, try the full space:
        neededfullspace := true;
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        repeat
            if IsBound(opt.regvecfullhints) and 
               IsBound(opt.regvecfullhints[j]) and
               IsBound(opt.regvecfullhints[j][counter+1]) then
                CopySubVector(opt.regvecfullhints[j][counter+1],regvec,
                              [1..Length(regvec)],[1..Length(regvec)]);
                Info(InfoOrb,1,"Taking hint #",counter+1);
            else
                Randomize(regvec);
            fi;
            ORB_NormalizeVector(regvec);
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= ORB.TRIESINWHOLESPACE;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            ORB.RANDOMSTABGENERATION := merk;
            return fail;
        fi;
      fi;

      Info(InfoOrb,2,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.trans[j] := o!.words;
      setup!.regvecs[j] := regvec;
      setup!.cosetrecog[j] := ORB_CosetRecogGeneric;
      if not(neededfullspace) then
          setup!.cosetinfo[j] := [o!.db,k];   # the hash table
      else
          setup!.cosetinfo[j] := [o!.db,k+1];   # the hash table
      fi;
  od;
  if IsBound(opt.stabchainrandom) then
      setup!.stabchainrandom := opt.stabchainrandom;
  else
      setup!.stabchainrandom := 1000;  # no randomization by default
  fi;
  ORB.RANDOMSTABGENERATION := merk;
  return setup;
end );

InstallGlobalFunction( ORB_ProjDownForSpaces,
  function(x,y)
    return ExtractSubMatrix(x,y[1],y[2]);
  end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForSpaces,
function(gens,permgens,sizes,codims,spcdim,opt)
  # Returns a setup object for a list of helper subgroups
  # gens: a list of lists of generators for U_1 < U_2 < ... < U_k < G
  # permgens: the same in a faithful permutation representation
  # sizes: a list of sizes of groups U_1 < U_2 < ... < U_k < G
  # codims: a list of dimensions of factor modules
  # spcdim: dimension of subspaces to permute
  # opt: a record for options, can be empty
  # note that the basis must be changed to make projection easy!
  # That is, projection is taking the components [1..codim].

  local counter,dim,doConversions,g,i,j,k,neededfullspace,nrgens,nrgenssum,
        o,q,regvec,sample,setup,sm,sum,merk;

  merk := ORB.RANDOMSTABGENERATION;
  ORB.RANDOMSTABGENERATION := 0;

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
      ORB.RANDOMSTABGENERATION := merk;
      return fail;
  fi;
  nrgens := List(gens,Length);
  nrgenssum := 0*nrgens;
  sum := 0;
  for i in [1..k+1] do
      nrgenssum[i] := sum;
      sum := sum + nrgens[i];
  od;
  nrgenssum[k+2] := sum;

  # the future:
  #sample := ZeroVector(NrCols(gens[1][1]),gens[1][1]);  
  sample := ExtractSubMatrix(gens[1][1],[1..spcdim],[1..Length(gens[1][1][1])]);
  TriangulizeMat(sample);

  # First preparations:
  setup := rec(k := k);
  setup.size := ShallowCopy(sizes);
  setup.index := sizes{[1..k]};
  for i in [k,k-1..2] do setup.index[i] := setup.index[i]/setup.index[i-1]; od;

  # Calculate stabilizer chain for whole group:
  setup.permgens := [];
  setup.permgensinv := [];
  setup.permbase := [];
  if Length(permgens[k+1]) = nrgens[k+1] then
      # old calling convention
      setup.permgens[k+1] := Concatenation(permgens);
  else
      setup.permgens[k+1] := ShallowCopy(permgens[k+1]);
  fi;
  setup.permgensinv[k+1] := List(setup.permgens[k+1],x->x^-1);
  g := Group(setup.permgens[k+1]{[nrgenssum[k+1]+1..nrgenssum[k+2]]});
  SetSize(g,sizes[k+1]);
  if IsTrivial(g) or
     (IsBound(opt.nostabchainfullgroup) and opt.nostabchainfullgroup) then
      setup.permbase[k+1] := fail;
  else
      Info(InfoOrb,1,"Calculating stabilizer chain for whole group...");
      setup.permbase[k+1] := ORB_BaseStabilizerChain(
                                ORB_StabilizerChainKnownSize(g,Size(g)));
      ##setup.permbase[k+1] := BaseStabChain(StabChainOp(g,rec()));
  fi;
  for i in [k,k-1..1] do
      if Length(permgens[i]) = nrgens[i] then
          # old calling convention:
          setup.permgens[i] := setup.permgens[i+1]{[1..nrgenssum[i+1]]};
      else
          setup.permgens[i] := ShallowCopy(permgens[i]);
      fi;
      g := Group(setup.permgens[i]{[nrgenssum[i]+1..nrgenssum[i+1]]});
      SetSize(g,sizes[i]);
      if IsPermGroup(g) then
          Info(InfoOrb,1,"Trying smaller degree permutation representation ",
               "for U",i,"...");
          sm := SmallerDegreePermutationRepresentation(g:cheap);
          if not(IsOne(sm)) then   # not the identity
              Info(InfoOrb,1,"Found one on ",
                   LargestMovedPoint(GeneratorsOfGroup(Image(sm)))," points.");
              for j in [1..Length(setup.permgens[i])] do
                  setup.permgens[i][j] := ImageElm(sm,setup.permgens[i][j]);
              od;
              g := Image(sm);
          fi;
      fi;
      setup.permgensinv[i] := List(setup.permgens[i],x->x^-1);
      ##setup.permbase[i] := BaseStabChain(StabChainOp(g,rec()));
      Info(InfoOrb,1,"Computing a base for helper subgroup #",i);
      setup.permbase[i] := ORB_BaseStabilizerChain(
                              ORB_StabilizerChainKnownSize(g,sizes[i]));
  od;
  setup.stabchainrandom := 1000;

  setup.els := [];
  setup.els[k+1] := Concatenation(gens);
  setup.elsinv := [];
  setup.elsinv[k+1] := List(setup.els[k+1],x->x^-1);
  setup.cosetinfo := [];
  setup.cosetrecog := [];
  setup.hashlen := [NextPrimeInt(3*sizes[1])];
  Append(setup.hashlen,List([2..k+1],i->NextPrimeInt(
                Minimum(3*(sizes[i]/sizes[i-1]),1000000))));
  setup.sample := [];
  setup.sample[k+1] := sample;
  dim := Length(sample);
  codims[k+1] := dim;   # for the sake of completeness!
  setup.staborblenlimit := dim;
  for j in [1..k] do
      setup.els[j] := List(Concatenation(gens{[1..j]}),
                           x->ExtractSubMatrix(x,[1..codims[j]],
                                                 [1..codims[j]]));
      if doConversions then
          for i in setup.els[j] do ConvertToMatrixRep(i); od;
      fi;
      setup.elsinv[j] := List(setup.els[j],x->x^-1);
      setup.sample[j] := ExtractSubMatrix(sample,[1..spcdim],[1..codims[j]]);
  od;
  q := Size(BaseField(gens[1][1]));
  setup.trans := [];
  setup.cache := LinkedListCache(100000000);  # 100 MB cache
  setup.transcache := List([1..k+1],j->List([1..j],i->WeakPointerObj([])));

  # Note that for k=1 we set codims[2] := dim
  setup.pi := [];
  setup.pifunc := [];
  setup.info := [HTCreate(setup.sample[1], rec( hashlen :=
                          NextPrimeInt((q^codims[1]-1)/(q-1) * 3) ))];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [[1..spcdim],[1..codims[i]]];
          setup.pifunc[j][i] := ORB_ProjDownForSpaces;
      od;
      if j < k+1 then
          setup.info[j] := HTCreate(setup.sample[j], rec( hashlen :=
                   NextPrimeInt(QuoInt((q^codims[j]-1)/(q-1)*3,sizes[j-1])) ));
      fi;
  od;
  setup.suborbnr := 0*[1..k];
  setup.sumstabl := 0*[1..k];
  setup.regvecs := [];
  setup.op := List([1..k+1],i->OnSubspacesByCanonicalBasis);
  setup.wordcache := [];
  setup.wordhash := HTCreate([1,2,3],rec( hashlen := 1000 ));

  Objectify( NewType(OrbitBySuborbitSetupFamily,
                     IsStdOrbitBySuborbitSetupRep and IsMutable),
             setup );
  # From now on we can use it and it is an object!

  # We do the recognition of elements of U_1 by the permutation rep:
  # FIXME: later devise code if U_1 in permgens is not a permutation grp.
  Info(InfoOrb,1,"Enumerating permutation base images of U_1...");
  setup!.cosetinfo[1] := Orb(setup!.permgens[k]{[1..nrgens[1]]},
                             setup!.permbase[k],OnTuples,
                             NextPrimeInt(3*sizes[1]+1),
                             rec( schreier := true,storenumbers := true ));
  Enumerate(setup!.cosetinfo[1]);
  setup!.cosetrecog[1] := ORB_CosetRecogPermgroup;
  setup!.trans[1] := List([1..Length(setup!.cosetinfo[1])],
                          x->TraceSchreierTreeForward(setup!.cosetinfo[1],x));

  # Now do the other steps:
  for j in [2..k] do
      # First find a vector the orbit of which identifies the U_{j-1}-cosets
      # of U_j, i.e. Stab_{U_j}(v) <= U_{j-1}, 
      # we can use the j-1 infrastructure!

      neededfullspace := false;

      Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
           "in factor space...");
      regvec := ZeroMatrix(spcdim,codims[k],sample);
      counter := 0;
      repeat
          if IsBound(opt.regvecfachints) and IsBound(opt.regvecfachints[j]) and
             IsBound(opt.regvecfachints[j][counter+1]) then
              CopySubMatrix(opt.regvecfachints[j][counter+1],regvec,
                            [1..spcdim],[1..spcdim],
                            [1..Length(regvec)],[1..Length(regvec)]);
              Info(InfoOrb,1,"Taking hint #",counter+1);
          else
              Randomize(regvec);
          fi;
          TriangulizeMat(regvec);
          # Now U_{j-1}-minimalize it, such that the transversal-words
          # returned reach the U_{j-1}-suborbits we find next:
          regvec := ORB_Minimalize(regvec,k,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit(setup,regvec,k,j,j-1,100);
          Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
               " suborbits (need ",sizes[j]/sizes[j-1],")");
      until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or 
            counter >= ORB.TRIESINQUOTIENT;
      if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
        # Bad luck, try the full space:
        neededfullspace := true;
        Info(InfoOrb,1,"Looking for U",j-1,"-coset-recognising U",j,"-orbit ",
             "in full space...");
        regvec := ZeroMutable(sample);
        counter := 0;
        # Go to the original generators, using the infrastructure for k=j-1:
        repeat
            if IsBound(opt.regvecfullhints) and 
               IsBound(opt.regvecfullhints[j]) and
               IsBound(opt.regvecfullhints[j][counter+1]) then
                CopySubMatrix(opt.regvecfullhints[j][counter+1],regvec,
                              [1..spcdim],[1..spcdim],
                              [1..Length(regvec)],[1..Length(regvec)]);
                Info(InfoOrb,1,"Taking hint #",counter+1);
            else
                Randomize(regvec);
            fi;
            TriangulizeMat(regvec);
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
                 " suborbits (need ",sizes[j]/sizes[j-1],")");
        until Length(Representatives(o!.db)) = sizes[j]/sizes[j-1] or
              counter >= ORB.TRIESINWHOLESPACE;
        if Length(Representatives(o!.db)) < sizes[j]/sizes[j-1] then
            Info(InfoOrb,1,"Bad luck, did not find nice orbit, giving up.");
            ORB.RANDOMSTABGENERATION := merk;
            return fail;
        fi;
      fi;

      Info(InfoOrb,2,"Found U",j-1,"-coset-recognising U",j,"-orbit!");
      setup!.trans[j] := o!.words;
      setup!.regvecs[j] := regvec;
      setup!.cosetrecog[j] := ORB_CosetRecogGeneric;
      if not(neededfullspace) then
          setup!.cosetinfo[j] := [o!.db,k];   # the hash table
      else
          setup!.cosetinfo[j] := [o!.db,k+1];   # the hash table
      fi;
  od;
  if IsBound(opt.stabchainrandom) then
      setup!.stabchainrandom := opt.stabchainrandom;
  else
      setup!.stabchainrandom := 1000;  # no randomization by default
  fi;
  ORB.RANDOMSTABGENERATION := merk;
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
  return Objectify( StdOrbitBySuborbitListType, obsol );
end );

InstallMethod( ViewObj, "for a orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep and IsList and
    IsSmallList ],
  function( obsol )
      local i;
      Print("<obsol obsos=[\n");
      for i in [1..Length(obsol!.obsos)] do
          ViewObj(obsol!.obsos[i]);
          Print(",\n");
      od;
      Print("] nrrandels:",obsol!.nrrandels," memory:",
            ORB_PrettyStringBigNumber(Sum(obsol!.obsos,Memory)),
            " setup-memory:",
            ORB_PrettyStringBigNumber(Memory(obsol!.setup)),
            "\n  total length: ",
            ORB_PrettyStringBigNumber(TotalLength(obsol)),
            " total size: ",ORB_PrettyStringBigNumber(Size(obsol)),">");
  end );

InstallOtherMethod( ELM_LIST, "for an orbit-by-suborbit-list and an int",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep, IsInt ],
  function( o,i )
    return o!.obsos[i];
  end );

InstallOtherMethod( Size, "for an orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep and IsList and
    IsSmallList ],
  function(obsol)
    return Sum(obsol!.obsos,Size);
  end );

InstallMethod( Memory, "for an orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep ],
  function(obsol)
    return Sum(obsol!.obsos,Memory);
  end );

InstallMethod( TotalLength, "for an orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep ],
  function(obsol)
    return Sum(obsol!.obsos,TotalLength);
  end );

InstallMethod( SavingFactor, "for an orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep ],
  function( obsol )
    if Length(obsol!.obsos) = 0 then
        return fail;
    else
        return QuoInt(TotalLength(obsol),
                      Sum(obsol!.obsos,o->o!.db!.mins!.nr));
    fi;
  end );

InstallOtherMethod( Length, "for an orbit-by-suborbit-list",
  [ IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep and IsList and
    IsSmallList ],
  function( obsol )
    return Length(obsol!.obsos);
  end );

InstallMethod( TestMembership, "for a point, an orbit-by suborbit, and a plist",
  [ IsObject, IsOrbitBySuborbit, IsMutable and IsList ],
  function( p, o, w )
    local i,j,mw,oo,pp,setup,stab,sw,v,word;
    setup := o!.db!.setup;
    j := o!.db!.j;
    i := o!.db!.i;
    mw := [];
    stab := rec();
    pp := ORB_Minimalize(p,j,i,setup,stab,mw);
    v := LookupSuborbit(pp,o!.db);
    if v = fail then return false; fi;
    oo := ORB_StabOrbitSearch(stab,setup,j,Representatives(o!.db)[v],pp);
    sw := TraceSchreierTreeForward(oo,oo!.found);
    sw := Concatenation( oo!.bysuborbitstabgens.words{sw} );
    word := Concatenation( o!.words[v], o!.miniwords[v], sw,
                           ORB_InvWord(mw) );
    Append(w,word);
    return true;
  end );


InstallGlobalFunction( IsVectorInOrbitBySuborbitList,
function(v,obsol)
  local i,j,k,res,s,x;
  s := obsol!.setup;
  k := s!.k;
  for j in [1..obsol!.nrrandels] do
      x := s!.op[k+1](v,obsol!.randels[j]);
      x := ORB_Minimalize(x,k+1,k,s,false,false);
      for i in [1..Length(obsol!.obsos)] do
          res := LookupSuborbit(x,obsol!.obsos[i]!.db);
          if res <> fail then  # we know this N-orbit
              return i;  # is in orbit number i
          fi;
      od;
  od;
  return fail;
end );

InstallGlobalFunction( OrbitsFromSeedsToOrbitList,
function( obsol, li )
  local o,orb,v,k;
  k := obsol!.setup!.k;
  for v in li do
      orb := IsVectorInOrbitBySuborbitList(v,obsol);
      if orb = fail then
          o := OrbitBySuborbit(obsol!.setup,v,k+1,k+1,k,51);
          if IsOrbitBySuborbit(o) then
              Add(obsol!.obsos,o);
              Print("New suborbit:\n");
              ViewObj(o);
              Print("\nHave now ",Length(obsol!.obsos),
                    " orbits with a total of ",
                    ORB_PrettyStringBigNumber(Sum(obsol!.obsos,Size)),
                    " elements.\n");
          fi;
      else
          Info(InfoOrb,1,"Already know orbit ",orb);
      fi;
  od;
end );

InstallGlobalFunction( VerifyDisjointness,
function( obsol )
  local disjoint,i,j,v;
  disjoint := true; # up to now
  for i in [1..Length(obsol!.obsos)-1] do
      Info(InfoOrb,1,"Checking orbit number ",i,"...");
      if Size(obsol!.obsos[i]) >= 2 * TotalLength(obsol!.obsos[i]!.db) then
          Print("WARNING: Orbit number ",i,"not enumerated >50%!\n");
      fi;
      for j in [i+1..Length(obsol!.obsos)] do
          if Size(obsol!.obsos[i]) = Size(obsol!.obsos[j]) then
              for v in Representatives(obsol!.obsos[i]!.db) do
                  # They are already U-minimal!
                  if LookupSuborbit(v,obsol!.obsos[j]!.db) <> fail then
                      Print("ATTENTION: Orbits ",i," and ",j," are equal!\n");
                      disjoint := false;
                  fi;
              od;
          fi;
      od;
  od;
  return disjoint;
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
