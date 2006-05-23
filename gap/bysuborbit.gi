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
  local cos,m,mm,o,oldp,oo,q,qq,tempstab,tempstabgens,v,ww,www;
  
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
      o := Enumerate(Orb(setup!.els[1],q,setup!.op[1],
                         rec( schreier := true, 
                              grpsizebound := setup!.size[1],
                              hashlen := setup!.size[1]*2,
                              stabchainrandom := setup!.stabchainrandom,
                              permgens := setup!.permgens[1],
                              permbase := setup!.permbase[1] )));
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
    p := ORB_Minimalize(p,j,i-1,setup,tempstab,w);

    # now try to reach the minimal U_{i-1}-suborbit in the U_i-orbit:
    if j > i then
      q := setup!.pifunc[j][i](p,setup!.pi[j][i]);
    else
      q := p;
    fi;
    v := ValueHT(setup!.info[i],q);

    if v = fail then    # we do not yet know this U_{i-1}-suborbit

      # we define q*U_{i-1} to be the U_i-minimal U_{i-1}-orbit,
      # and q to be the U_i-minimal point in there.
      # now find the other U_{i-1}-orbits:
      o := OrbitBySuborbit(setup,q,i,i,i-1,100);

      v := rec( gens := o!.stabwords, size := o!.stabsize );
      AddHT(setup!.info[i],q,v);
      # Now find all U_{i-1}-minimal elements in q*U_{i-1}, note that
      # tempstab contains generators for Stab_{U_{i-1}}(q)!
      Info(InfoOrb,2+ORB.ORBITBYSUBORBITDEPTH,
           "Starting on-the-fly precomputation (i>1) ...");
      tempstabgens := List(tempstab.gens,
                           w->ORB_ApplyWord(One(setup!.els[i][1]),w,
                                            setup!.els[i],setup!.elsinv[i],
                                            OnRight));
      oo := Enumerate(Orb(tempstabgens,q,setup!.op[i],
                          rec(hashlen := setup!.hashlen[i],schreier := true)));
      for m in [2..Length(oo!.orbit)] do
          ww := TraceSchreierTreeForward(oo,m);
          ww := ORB_InvWord(Concatenation( tempstab.gens{ww} ));
          ww := ORB_StoreWordInCache(setup,ww);
          if ValueHT(setup!.info[i],oo!.orbit[m]) <> fail then
              Error(1);
          fi;
          AddHT(setup!.info[i],oo!.orbit[m],-ww);
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
          tempstabgens := List(tempstab.gens,
                               w->ORB_ApplyWord(One(setup!.els[i][1]),w,
                                                setup!.els[i],setup!.elsinv[i],
                                                OnRight));
          oo := Enumerate(Orb(tempstabgens,qq,setup!.op[i],
                              rec(hashlen := setup!.hashlen[i],
                                  schreier := true)));
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
  local i,j,l,length,m,o,setup,stabgens,infolevel;
        
  setup := db!.setup;
  i := db!.i;
  j := db!.j;
  l := db!.l;
  Add(db!.reps,p);
  AddHT(db!.mins,p,Length(db!.reps));
  stabgens := List(stab.gens,
                   w->ORB_ApplyWord( setup!.els[j][1]^0, w, setup!.els[j],
                                     setup!.elsinv[j], OnRight ));
  o := Enumerate(Orb(stabgens,p,setup!.op[j],
                     rec(hashlen := setup!.hashlen[j],schreier := true)));
  for m in [2..Length(o!.orbit)] do
      AddHT( db!.mins, o!.orbit[m], Length(db!.reps) );
  od;
  length := setup!.size[i] / (stab.size / Length(o!.orbit));
  Add(db!.lengths,length);
  db!.totallength := db!.totallength + length;
  if Length(db!.reps) mod ORB.REPORTSUBORBITS = 0 then
      infolevel := 1;
  else
      infolevel := 2;
  fi;
  Info(InfoOrb,infolevel+ORB.ORBITBYSUBORBITDEPTH,
       "j=",j," l=",l," i=",i," #",Length(db!.reps),
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

ORB.PATIENCEFORSTAB := 200;
ORB.REPORTSUBORBITS := 1000;

ORB.ORBITBYSUBORBITDEPTH := 0;   # outside!

InstallGlobalFunction( OrbitBySuborbit,
function(setup,p,j,l,i,percentage)
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
  # Returns a suborbit database with additional field "words" which is
  # a list of words in gens which can be used to reach U-orbit in the G-orbit
  local assumestabcomplete,db,firstgen,fullstabsize,ii,lastgen,m,miniwords,
        mw,newperm,newword,o,oldtodo,pleaseexitnow,stab,stabg,stabgens,
        stabilizer,stabperms,sw,todo,v,words,x,firstgenU,lastgenU,
        triedstabgens,haveappliedU,MakeReturnObj,y,repforsuborbit,
        oldrepforsuborbit,xx,stab2,mw2,sw2,stabg2,todovecs,oldtodovecs,xxx;

  Info(InfoOrb,2,"Entering OrbitBySuborbit j=",j," l=",l," i=",i);
  ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH + 1;

  if not(j >= l and l > i and i >= 1) then
      Error("Need j >= l > i >= 1");
      ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
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
      ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
      return;
  fi;

  # First we U_i-minimalize p:
  stab := rec();
  p := ORB_Minimalize(p,j,i,setup,stab,false);

  miniwords := [[]];  # here we collect U-minimalizing elements
  
  # Start a database with the first U-suborbit:
  db := SuborbitDatabase(setup,j,l,i);
  StoreSuborbit(db,p,stab,1);

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
                      stabsize := fullstabsize,
                      stab := stabilizer,
                      stabwords := stabgens,
                      groupsize := setup!.size[l],
                      orbitlength := setup!.size[l]/fullstabsize,
                      percentage := percentage,
                      seed := p ) );
  end;
    
  while true do

    ii := 1;
    while ii <= Length(todo) do
      if pleaseexitnow = true then 
          ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
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
          Add(words,Concatenation(todo[ii],[m]));
          Add(todo,Concatenation(todo[ii],[m]));
          Add(todovecs,xxx);
          Add(miniwords,mw);
          StoreSuborbit(db,x,stab,fullstabsize);
          Add(repforsuborbit,Length(db!.reps));
          if 2 * TotalLength(db) * fullstabsize > setup!.size[l] and
             TotalLength(db) * fullstabsize * 100 >= 
                             setup!.size[l]*percentage then 
            Info(InfoOrb,2,"Leaving OrbitBySuborbit j=",j," l=",l," i=",i);
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
          stabg := List(stab.gens,
                        w->ORB_ApplyWord(setup!.els[j][1]^0,w,setup!.els[j],
                                         setup!.elsinv[j], OnRight ));
          # Now we distinguish two cases: if haveappliedU is false, we
          # are in the first phase, that is, todo[ii] is the chosen
          # representative for p*todo[ii]*U, thus we can directly
          # make a Schreier generator:
          if not(haveappliedU) then
            o := Enumerate(Orb(stabg,Representatives(db)[v],
                           setup!.op[j],
                           rec( lookingfor := [x],
                                hashlen := setup!.hashlen[j], 
                                schreier := true )));
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( stab.gens{sw} );
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
            stabg2 := List(stab2.gens,
                        w->ORB_ApplyWord(setup!.els[j][1]^0,w,setup!.els[j],
                                         setup!.elsinv[j], OnRight ));
            y := Representatives(db)[repforsuborbit[ii]];
            o := Enumerate(Orb(stabg2,y,setup!.op[j],
                   rec( hashlen := setup!.hashlen[j], 
                        lookingfor := [xx], schreier := true ) ));
            sw2 := TraceSchreierTreeForward(o,o!.found);
            sw2 := Concatenation( stab2.gens{sw2} );
            # Now Concatenation(words[repforsuborbit[ii]],
            #                   miniwords[repforsuborbit[ii]],sw2,mw2^-1)
            # is the transversal element for the original xx
            # Now as in the simpler case:
            o := Enumerate(Orb(stabg,Representatives(db)[v],
                           setup!.op[j],
                           rec( hashlen := setup!.hashlen[j], lookingfor := [x],
                                schreier := true )));
            sw := TraceSchreierTreeForward(o,o!.found);
            sw := Concatenation( stab.gens{sw} );
            newword := Concatenation(words[repforsuborbit[ii]],
                                     miniwords[repforsuborbit[ii]],sw2,
                                     ORB_InvWord(mw2),[m],mw,ORB_InvWord(sw),
                                     ORB_InvWord(miniwords[v]),
                                     ORB_InvWord(words[v]));
          fi;
          Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
               "Calculated Schreier generator");
          newperm := ORB_ApplyWord(setup!.permgens[l][1]^0,newword,
                          setup!.permgens[l],setup!.permgensinv[l],OnRight);
          if not(IsOne(newperm)) then
            Info(InfoOrb,3+ORB.ORBITBYSUBORBITDEPTH,
                 "Schreier generator was non-trivial");
            if not(newperm in stabilizer) then
              triedstabgens := 0;   # we actually found a new one!
              Add(stabgens,newword);
              Add(stabperms,newperm);
              stabilizer := GroupWithGenerators(stabperms);
              Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                   "Calculating new estimate of the stabilizer...");
              if IsBound(setup!.stabchainrandom) and
                 setup!.stabchainrandom <> false then
                  StabChain(stabilizer, 
                            rec(random := setup!.stabchainrandom));
              else
                  StabChain(stabilizer);
              fi;
              fullstabsize := Size(stabilizer);
              Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                   "New stabilizer order: ",fullstabsize);
              if TotalLength(db) * fullstabsize * 100
                 >= setup!.size[l]*percentage then 
                Info(InfoOrb,2,"Leaving OrbitBySuborbit");
                ORB.ORBITBYSUBORBITDEPTH := ORB.ORBITBYSUBORBITDEPTH - 1;
                return MakeReturnObj();
              fi;
            fi;
          fi;
          triedstabgens := triedstabgens + 1;
          if triedstabgens > ORB.PATIENCEFORSTAB then  # this is heuristics!
            Info(InfoOrb,1+ORB.ORBITBYSUBORBITDEPTH,
                 "Lost patience with stabiliser, assuming it is complete...");
            assumestabcomplete := true;
          fi;
        fi;
      od;   # for m in [firstgen..lastgen]
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
        MultRowVector(v,v[c]^-1);
    fi;
    return v;
  end );

InstallGlobalFunction( ORB_CosetRecogGeneric,
  function( i, w, s )
    local x,j;
    j := s!.cosetinfo[i][2];
    x := ORB_ApplyWord(s!.regvecs[i],w,s!.els[j],s!.elsinv[j],s!.op[j]);
    x := ORB_Minimalize(x,j,i-1,s,false,false);
    return LookupSuborbit(x,s!.cosetinfo[i][1]);
  end );

InstallGlobalFunction( OrbitBySuborbitBootstrapForVectors,
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
  setup.info := [NewHT(setup.sample[1],(q^codims[1]) * 3)];
  for j in [2..k+1] do
      setup.pi[j] := [];
      setup.pifunc[j] := [];
      for i in [1..j-1] do
          setup.pi[j][i] := [1..codims[i]];
          setup.pifunc[j][i] := \{\};
      od;
      if j < k+1 then
          setup.info[j] :=
             NewHT(setup.sample[j],QuoInt((q^codims[j])*3,sizes[j-1]));
      fi;
  od;
  setup.suborbnr := 0*[1..k];
  setup.sumstabl := 0*[1..k];
  setup.regvecs := [];
  setup.op := List([1..k+1],i->OnRight);
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
          # Now U_{j-1}-minimalize it, such that the transversal-words
          # returned reach the U_{j-1}-suborbits we find next:
          regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit(setup,regvec,j,j,j-1,100);
          Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
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
            # Now U_{j-1}-minimalize it, such that the transversal-words
            # returned reach the U_{j-1}-suborbits we find next:
            regvec := ORB_Minimalize(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
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

InstallGlobalFunction( OrbitBySuborbitBootstrapForLines,
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
          regvec := ORB_Minimalize(regvec,j,j-1,setup,false,false);
          counter := counter + 1;
          o := OrbitBySuborbit(setup,regvec,j,j,j-1,100);
          Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
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
            regvec := ORB_Minimalize(regvec,k+1,j-1,setup,false,false);
            counter := counter + 1;
            o := OrbitBySuborbit(setup,regvec,k+1,j,j-1,100);
            Info(InfoOrb,1,"Found ",Length(Representatives(o!.db)),
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

InstallGlobalFunction( OrbitsFromSeedsToOrbitList,
function( obsol, li )
  local o,orb,v,k;
  k := obsol.setup!.k;
  for v in li do
      orb := IsVectorInOrbitBySuborbitList(v,obsol);
      if orb = fail then
          o := OrbitBySuborbit(obsol.setup,v,k+1,k+1,k,51);
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
          Info(InfoOrb,1,"Already know orbit ",orb);
      fi;
  od;
end );

InstallGlobalFunction( VerifyDisjointness,
function( obsol )
  local disjoint,i,j,v;
  disjoint := true; # up to now
  for i in [1..Length(obsol.obsos)-1] do
      Info(InfoOrb,1,"Checking orbit number ",i,"...");
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

