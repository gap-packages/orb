#############################################################################
##
##  orbits.gi           orb package                             Max Neunhoeffer
##                                                              Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Implementation stuff for fast standard orbit enumeration.
##
#############################################################################


# A central place for configuration variables:

InstallValue( ORB, rec( ) );

# Possible options:
#  .grpsizebound
#  .orbsizebound
#  .stabsizebound
#  .permgens
#  .matgens
#  .onlystab
#  .schreier
#  .lookingfor
#  .report
#  .stabchainrandom
#  .permbase
#  .stab
#  .storenumbers

# Outputs:
#  .gens
#  .nrgens
#  .op
#  .orbit
#  .pos
#  .tab
#  .ht
#  .stab
#  .stabsize
#  .stabcomplete
#  .schreiergen
#  .schreierpos
#  .found
#  .stabwords

InstallGlobalFunction( InitOrbit,
  function( arg )
    Print("Please rename your function call from \"InitOrbit\" to \"Orb\"!\n");
    return CallFuncList(Orb,arg);
  end );

InstallGlobalFunction( Orb, 
  function( arg )
    local filts,gens,hashlen,lmp,o,op,opt,x;

    # First parse the arguments:
    if Length(arg) = 4 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := rec();
    elif Length(arg) = 5 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := arg[5];
    else
        Print("Usage: Orb( gens, point, action, hashlen [,options] )\n");
        return;
    fi;

    # We make a copy:
    o := ShallowCopy(opt);
    # Now get rid of the group object if necessary but preserve known size:
    if IsGroup(gens) then
        if HasSize(gens) then
            o.grpsizebound := Size(gens);
        fi;
        gens := GeneratorsOfGroup(gens);
    fi;

    # We collect the filters for the type:
    filts := IsOrbit;

    # Now set some default options:
    if IsBound( o.permgens ) then 
        filts := filts and WithSchreierTree and WithPermStabilizer;
        if IsBound(o.stab) then
            # We know already part of the stabilizer:
            if IsBound(o.stabchainrandom) and o.stabchainrandom <> false then
                StabChain( o.stab, rec( random := o.stabchainrandom ) );
            fi;
            o.stabsize := Size( o.stab );
            Info(InfoOrb,1,"Already have partial stabilizer of size ",
                           o.stabsize,".");
        else
            o.stab := Group(o.permgens[1]^0);
            o.stabsize := 1;
        fi;
        o.permgensi := List(o.permgens,x->x^-1);
        o.schreier := true;   # we need a Schreier tree for the stabilizer
        o.stabcomplete := false;
        o.stabwords := [];
        o.storenumbers := true;
        if not IsBound( o.onlystab ) then
            o.onlystab := false;
        fi;
    fi;
    # FIXME: check for matgensi here !
    if IsBound(o.stabsizebound) and IsBound(o.orbsizebound) and
       not(IsBound(o.grpsizebound)) then
        o.grpsizebound := o.stabsizebound * o.orbsizebound;
    fi;
    if IsBound(o.lookingfor) and o.lookingfor <> fail then 
        if IsList(o.lookingfor) then
            filts := filts and LookingForUsingList;
        elif IsRecord(o.lookingfor) and IsBound(o.lookingfor.ishash) then
            filts := filts and LookingForUsingHash;
        elif IsFunction(o.lookingfor) then
            filts := filts and LookingForUsingFunc;
        else
            Error("opt.lookingfor must be a list or a hash table or a",
                  " function");
        fi;
    fi;
    o.found := false; 
    if not (IsBound( o.schreiergen ) or IsBound( o.schreier ) ) then 
        o.schreiergen := fail; 
        o.schreierpos := fail;
    else
        filts := filts and WithSchreierTree;
        o.schreiergen := [fail];
        o.schreierpos := [fail];
    fi;
    if not(IsBound(o.report)) then
        o.report := 0;
    fi;
    if not(IsBound(o.storenumbers)) then
        o.storenumbers := false;
    fi;
    if not(IsBound(o.orbsizebound)) and IsBound(o.grpsizebound) then
        o.orbsizebound := o.grpsizebound;
    fi;
    if not(IsBound(o.stabsizebound)) and IsBound(o.grpsizebound) then
        o.stabsizebound := o.grpsizebound;
    fi;
    if IsBound(o.stabsizebound) and IsBound(o.stabsize) then
        if o.stabsize >= o.stabsizebound then
            Info(InfoOrb,2,"Stabilizer complete.");
            o.stabcomplete := true;
        fi;
    fi;
    
    # Now take this record as our orbit record and return:
    o.gens := gens;
    o.nrgens := Length(gens);
    o.op := op;
    o.orbit := [x];
    o.pos := 1;
    if ForAll(gens,IsPerm) and IsPosInt(x) and op = OnPoints then
        # A special case for permutation acting on integers:
        lmp := LargestMovedPoint(gens);
        if x > lmp then 
            Info(InfoOrb,1,"Warning: start point not in permuted range");
        fi;
        o.tab := 0*[1..lmp];
        o.tab[x] := 1;
        filts := filts and IsPermOnIntOrbitRep;
        if not(IsBound(o.orbsizebound)) then
            o.orbsizebound := lmp;
        fi;
    else
        o.ht := NewHT(x,hashlen);
        if IsBound(o.stab) or o.storenumbers then
            filts := filts and WithStoringNumbers;
            AddHT(o.ht,x,1);
        else
            AddHT(o.ht,x,true);
        fi;
        filts := filts and IsHashOrbitRep;
    fi;
    Objectify( NewType(CollectionsFamily(FamilyObj(x)),filts), o );
    return o;
end );

InstallMethod( ViewObj, "for an orbit", [IsOrbit],
  function( o )
    Print("<");
    if IsClosed(o) then Print("closed "); else Print("open "); fi;
    if IsPermOnIntOrbitRep(o) then
        Print("Int-");
    fi;
    Print("orbit, ", Length(o!.orbit), " points");
    if WithSchreierTree(o) then
        Print(" with Schreier tree");
    fi;
    if WithPermStabilizer(o) or WithMatStabilizer(o) then
        Print(" and stabilizer");
        if o!.onlystab then
            Print(" going for stabilizer");
        fi;
    fi;
    if LookingForUsingList(o) or LookingForUsingHash(o) or 
       LookingForUsingFunc(o) then
        Print(" looking for sth.");
    fi;
    Print(">");
  end );

InstallMethod( ELM_LIST, "for an orbit object, and a positive integer", 
  [IsOrbit and IsDenseList, IsPosInt],
  function( orb, pos )
    return orb!.orbit[pos];
  end );

InstallMethod( ELMS_LIST, "for an orbit object, and a list of integers",
  [IsOrbit and IsDenseList, IsList],
  function( orb, poss )
    return orb!.orbit{poss};
  end );

InstallMethod( Length, "for an orbit object",
  [IsOrbit and IsDenseList],
  function( orb )
    return Length(orb!.orbit);
  end );

InstallMethod( Position, "for an orbit object, an object, and an integer",
  [IsOrbit and IsDenseList, IsObject, IsInt],
  function( orb, ob, pos )
    return Position( orb!.orbit, ob, pos );
  end );

InstallMethod( Position, 
  "for an orbit object storing numbers, an object, and an integer",
  [IsOrbit and IsHashOrbitRep and IsDenseList and WithStoringNumbers, 
   IsObject, IsInt],
  function( orb, ob, pos )
    local p;
    p := ValueHT(orb!.ht,ob);
    if p = fail then
        return fail;
    else
        if p > pos then
            return p;
        else
            return fail;
        fi;
    fi;
  end );

InstallMethod( Position, 
  "for an orbit object perm on ints, an object, and an integer",
  [IsOrbit and IsDenseList and IsPermOnIntOrbitRep, IsInt, IsInt],
  function( orb, ob, pos )
    local p;
    if IsBound(orb!.tab[ob]) then
        p := orb!.tab[ob];
        if p > pos then
            return p;
        else
            return fail;
        fi;
    else
        return fail;
    fi;
  end );

InstallMethod( PositionCanonical,
  "for an orbit object and an object",
  [IsOrbit, IsObject],
  function( o, ob )
    return Position(o,ob);
  end );

InstallMethod( \in, 
  "for an object and an orbit object",
  [IsObject, IsOrbit and IsHashOrbitRep and IsDenseList and WithStoringNumbers],
  function( ob, orb )
    local p;
    p := ValueHT( orb!.ht, ob );
    if p = fail then
      return false;
    else
      return true;
    fi;
  end );

InstallMethod( \in,
  "for an object and an orbit object perms on int",
  [IsInt, IsOrbit and IsDenseList and IsPermOnIntOrbitRep],
  function( ob, orb )
    local p;
    if IsBound(orb!.tab[ob]) then
        return true;
    else
        return false;
    fi;
  end );

InstallMethod( EvaluateWord, "for a list of generators and a word",
  [IsList, IsList],
  function( gens, w )
    local i,res;
    if Length(w) = 0 then
        return gens[1]^0;
    fi;
    res := gens[w[1]];
    for i in [2..Length(w)] do
        res := res * gens[w[i]];
    od;
    return res;
  end );

InstallMethod( ActWithWord, 
  "for a list of generators, a word, an action, and a point",
  [ IsList, IsList, IsFunction, IsObject ],
  function( gens, w, op, p )
    local i;
    for i in w do
        p := op(p,gens[i]);
    od;
    return p;
  end );

InstallMethod( LookFor, "for an orbit with a list and a point",
  [ IsOrbit and LookingForUsingList, IsObject ],
  function( o, p )
    return p in o!.lookingfor;
  end );

InstallMethod( LookFor, "for an orbit with a hash and a point",
  [ IsOrbit and LookingForUsingHash, IsObject ],
  function( o, p )
    return ValueHT(o!.lookingfor,p) <> fail;
  end );

InstallMethod( LookFor, "for an orbit with a function and a point",
  [ IsOrbit and LookingForUsingFunc, IsObject ],
  function( o, p )
    return o!.lookingfor(p);
  end );
    
InstallMethod( LookFor, "for an orbit not looking for something and a point",
  [ IsOrbit, IsObject ],
  function( o, p )
    return false;
  end );

InstallMethod( Enumerate, 
  "for a hash orbit without Schreier tree and a limit", 
  [IsOrbit and IsHashOrbitRep, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,pos,yy,rep;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            pos := ValueHT(o!.ht,yy);
            if pos = fail then
                nr := nr + 1;
                orb[nr] := yy;
                if o!.storenumbers then
                    AddHT(o!.ht,yy,nr);
                else
                    AddHT(o!.ht,yy,true);
                fi;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a hash orbit with Schreier tree and a limit", 
  [IsOrbit and IsHashOrbitRep and WithSchreierTree, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,pos,yy,rep;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            pos := ValueHT(o!.ht,yy);
            if pos = fail then
                nr := nr + 1;
                orb[nr] := yy;
                if o!.storenumbers then
                    AddHT(o!.ht,yy,nr);
                else
                    AddHT(o!.ht,yy,true);
                fi;
                o!.schreiergen[nr] := j;
                o!.schreierpos[nr] := i;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a hash orbit with permutation stabilizer and a limit", 
  [IsOrbit and IsHashOrbitRep and WithSchreierTree and WithPermStabilizer, 
   IsCyclotomic],
  function( o, limit )
    local basimg,i,j,nr,orb,pos,rep,sgen,sgentrivial,wordb,wordf,yy;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr and not(o!.stabcomplete and o!.onlystab) do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            pos := ValueHT(o!.ht,yy);
            if pos = fail then
                nr := nr + 1;
                orb[nr] := yy;
                if o!.storenumbers then
                    AddHT(o!.ht,yy,nr);
                else
                    AddHT(o!.ht,yy,true);
                fi;
                o!.schreiergen[nr] := j;
                o!.schreierpos[nr] := i;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound and
                   o!.stabcomplete then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
                if IsBound(o!.grpsizebound) and not(o!.stabcomplete) then
                    if Length(o!.orbit)*o!.stabsize*2 >= o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            else
                if not( o!.stabcomplete ) then
                    # Is stabilizer element trivial?
                    sgentrivial := false;
                    wordf := TraceSchreierTreeForward(o,i);
                    wordb := TraceSchreierTreeBack(o,pos);
                    if IsBound(o!.permbase) then
                        basimg := ActWithWord(o!.permgens,wordf,
                                              OnTuples,o!.permbase);
                        basimg := OnTuples(basimg,o!.permgens[j]);
                        basimg := ActWithWord(o!.permgensi,wordb,
                                              OnTuples,basimg);
                        if basimg = o!.permbase then
                            sgentrivial := true;
                        fi;
                    fi; 
                    if not(sgentrivial) then
                      Info(InfoOrb,3,"Evaluating stabilizer element...");
                      # Calculate an element of the stabilizer:
                      sgen := EvaluateWord(o!.permgens,wordf)*o!.permgens[j] *
                              EvaluateWord(o!.permgensi,wordb);
                      if not(IsOne(sgen)) and not(sgen in o!.stab) then
                          if IsBound(o!.stabchainrandom) and
                             o!.stabchainrandom <> false then
                            if o!.stabsize = 1 then
                                o!.stab := Group(sgen);
                                SetSize(o!.stab,Order(sgen));
                            else
                                o!.stab := Group(Concatenation(
                                                   GeneratorsOfGroup(o!.stab),
                                                   [sgen]));
                            fi;
                            StabChain(o!.stab,
                                      rec(random := o!.stabchainrandom));
                          else
                            o!.stab := ClosureGroup(o!.stab,sgen);
                          fi;
                          Add(o!.stabwords,Concatenation(wordf,[j],-wordb));
                          o!.stabsize := Size(o!.stab);
                          Info(InfoOrb,2,"New stabilizer size: ",o!.stabsize);
                          if IsBound(o!.stabsizebound) and
                             o!.stabsize >= o!.stabsizebound then
                              o!.stabcomplete := true;
                              Info(InfoOrb,2,"Stabilizer complete.");
                          fi;
                      fi;
                    fi;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a perm on int orbit without Schreier tree and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,tab,yy,rep;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    tab := o!.tab;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            if tab[yy] = 0 then
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a perm on int orbit with Schreier tree and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep and WithSchreierTree, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,tab,yy,rep;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    tab := o!.tab;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            if tab[yy] = 0 then
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;
                o!.schreiergen[nr] := j;
                o!.schreierpos[nr] := i;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a perm on int orbit with permutation stabilizer and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep and WithSchreierTree and WithPermStabilizer, 
   IsCyclotomic],
  function( o, limit )
    local basimg,i,j,nr,orb,rep,sgen,sgentrivial,tab,wordb,wordf,yy;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    tab := o!.tab;
    nr := Length(orb);
    if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
        limit := o!.orbsizebound; 
    fi;
    if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
        o!.found := 1;
        return o;
    fi;
    rep := o!.report;
    while nr <= limit and i <= nr and not(o!.stabcomplete and o!.onlystab) do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            if tab[yy] = 0 then
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;
                o!.schreiergen[nr] := j;
                o!.schreierpos[nr] := i;
                if LookFor(o,yy) = true then
                    o!.pos := i;
                    o!.found := nr;
                    return o;
                fi;
                if IsBound(o!.orbsizebound) and 
                   Length(o!.orbit) >= o!.orbsizebound and
                   o!.stabcomplete then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
                if IsBound(o!.grpsizebound) and not(o!.stabcomplete) then
                    if Length(o!.orbit)*o!.stabsize*2 >= o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            else
                if not( o!.stabcomplete ) then
                    # Is stabilizer element trivial?
                    sgentrivial := false;
                    wordf := TraceSchreierTreeForward(o,i);
                    wordb := TraceSchreierTreeBack(o,tab[yy]);
                    if IsBound(o!.permbase) then
                        basimg := ActWithWord(o!.permgens,wordf,
                                              OnTuples,o!.permbase);
                        basimg := OnTuples(basimg,o!.permgens[j]);
                        basimg := ActWithWord(o!.permgensi,wordb,
                                              OnTuples,basimg);
                        if basimg = o!.permbase then
                            sgentrivial := true;
                        fi;
                    fi; 
                    if not(sgentrivial) then
                      Info(InfoOrb,3,"Evaluating stabilizer element...");
                      # Calculate an element of the stabilizer:
                      sgen := EvaluateWord(o!.permgens,wordf)*o!.permgens[j] *
                              EvaluateWord(o!.permgensi,wordb);
                      if not(IsOne(sgen)) and not(sgen in o!.stab) then
                        if IsBound(o!.stabchainrandom) and
                           o!.stabchainrandom <> false then
                            if o!.stabsize = 1 then
                                o!.stab := Group(sgen,sgen);
                                SetSize(o!.stab,Order(sgen));
                            else
                                o!.stab := Group(Concatenation(
                                                   GeneratorsOfGroup(o!.stab),
                                                   [sgen]));
                            fi;
                            StabChain(o!.stab,
                                      rec(random := o!.stabchainrandom));
                        else
                            o!.stab := ClosureGroup(o!.stab,sgen);
                        fi;
                        Add(o!.stabwords,
                            Concatenation(wordf,[j],-Reversed(wordb)));
                        o!.stabsize := Size(o!.stab);
                        Info(InfoOrb,2,"New stabilizer size: ",o!.stabsize);
                        if IsBound(o!.stabsizebound) and
                           o!.stabsize >= o!.stabsizebound then
                            o!.stabcomplete := true;
                            Info(InfoOrb,2,"Stabilizer complete.");
                        fi;
                      fi;
                    fi;
                fi;
            fi;
        od;
        i := i + 1;
        rep := rep - 1;
        if rep = 0 then
            rep := o!.report;
            Info(InfoOrb,1,"Have ",nr," points.");
        fi;
    od;
    o!.pos := i;
    if i > nr then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, "for an orbit object", [IsOrbit],
  function( o )
    return Enumerate(o,infinity);
  end );
    
InstallMethod( TraceSchreierTreeForward, "for an orbit and a position",
  [ IsOrbit and WithSchreierTree, IsPosInt ],
  function( o, pos )
    local word;
    word := [];
    while pos > 1 do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    return Reversed(word);
  end );
InstallMethod( TraceSchreierTreeForward, "for an orbit and a position",
  [ IsOrbit, IsPosInt ],
  function( o, pos )
    Info(InfoOrb,1,"this orbit does not have a Schreier tree");
    return fail;
  end );

InstallMethod( TraceSchreierTreeBack, "for an orbit and a position",
  [ IsOrbit and WithSchreierTree, IsPosInt ],
  function( o, pos )
    local word;
    word := [];
    while pos > 1 do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    return word;
  end );
InstallMethod( TraceSchreierTreeBack, "for an orbit and a position",
  [ IsOrbit, IsPosInt ],
  function( o, pos )
    Info(InfoOrb,1,"this orbit does not have a Schreier tree");
    return fail;
  end );

InstallOtherMethod( StabilizerOfExternalSet, 
  "for an orbit with permutation stabilizer",
  [ IsOrbit and WithPermStabilizer ],
  function( o ) return o!.stab; end );

InstallOtherMethod( StabilizerOfExternalSet, 
  "for an orbit with matrix stabilizer",
  [ IsOrbit and WithMatStabilizer ],
  function( o ) return o!.stab; end );

InstallOtherMethod( StabilizerOfExternalSet,
  "for an orbit without stabilizer",
  [ IsOrbit ],
  function( o ) 
    Info(InfoOrb,1, "this orbit does not have a stabilizer" ); 
    return fail;
  end );

InstallMethod( ActionOnOrbit,
  "for a closed orbit on integers and a list of elements",
  [ IsOrbit and IsPermOnIntOrbitRep and IsClosed, IsList ],
  function( o, gens )
    local res,i;
    res := [];
    for i in [1..Length(gens)] do
      Add(res,PermList( List([1..Length(o!.orbit)],
                             j->o!.tab[o!.op(o!.orbit[j],gens[i])])));
    od;
    return res;
  end );
    
InstallMethod( ActionOnOrbit, 
  "for a closed orbit with numbers and a list of elements",
  [ IsOrbit and IsHashOrbitRep and WithStoringNumbers and IsClosed, IsList ],
  function( o, gens )
    local res,i;
    res := [];
    for i in [1..Length(gens)] do
      Add(res,PermList(
       List([1..Length(o!.orbit)],
            j->ValueHT(o!.ht,o!.op(o!.orbit[j],gens[i])))));
    od;
    return res;
  end );
 
InstallMethod( ActionOnOrbit, "for a closed orbit and a list of elements",
  [ IsOrbit and IsHashOrbitRep and IsClosed, IsList ],
  function( o, gens )
    local ht,i,res;
    ht := NewHT( o!.orbit[1], Length(o!.orbit)*2+1 );
    for i in [1..Length(o!.orbit)] do
        AddHT(ht,o!.orbit[i],i);
    od;
    res := [];
    for i in [1..Length(gens)] do
      Add(res,PermList(
       List([1..Length(o!.orbit)],j->ValueHT(ht,o!.op(o!.orbit[j],gens[i])))));
    od;
    return res;
  end );


#######################################################################
# The following loads the sub-package "QuotFinder":
# Note that this requires other GAP packages, which are automatically
# loaded by this command if available.
#######################################################################

InstallGlobalFunction( LoadQuotFinder, function()
  if LoadPackage("chop") <> true then
      Error("Could not load the required chop package");
      return;
  fi;
  ReadPackage("orb","gap/quotfinder.gd");
  ReadPackage("orb","gap/quotfinder.gi");
end );

