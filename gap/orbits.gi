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
#  .hashlen

# Outputs:
#  .gens
#  .nrgens
#  .op
#  .orbit
#  .pos
#  .tab
#  .ht
#  .stab
#  .stabchain
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
    if Length(arg) = 3 then
        gens := arg[1]; x := arg[2]; op := arg[3];
        hashlen := 10000; opt := rec();
    elif Length(arg) = 4 and IsInt(arg[4]) then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := rec();
    elif Length(arg) = 4 then
        gens := arg[1]; x := arg[2]; op := arg[3]; opt := arg[4];
        if IsBound(opt.hashlen) then
            hashlen := opt.hashlen;
        else
            hashlen := 10000;
        fi;
    elif Length(arg) = 5 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := arg[5];
    else
        Print("Usage: Orb( gens, point, action [,options] )\n");
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
    o.stabcomplete := false;  # set this even if we do not compute stabiliser!
    if not(IsBound(o.schreiergenaction)) then
        o.schreiergenaction := false;
    fi;

    if IsBound( o.permgens ) then 
        filts := filts and WithSchreierTree and WithPermStabilizer;
        if not(IsBound(o.stabchainrandom)) then
            o.stabchainrandom := 1000;  # no randomisation
        fi;
        if IsBound(o.stab) then
            # We know already part of the stabilizer:
            # we throw away a possible stabchain because we
            ORB_ComputeStabChain(o);
            Info(InfoOrb,1,"Already have partial stabilizer of size ",
                           o.stabsize,".");
        else
            o.stab := Group(One(o.permgens[1]));
            ORB_ComputeStabChain(o);
        fi;
        o.permgensi := List(o.permgens,x->x^-1);
        o.schreier := true;   # we need a Schreier tree for the stabilizer
        o.stabwords := [];
        o.storenumbers := true;
        # The following triggers the generation of Schreier generators:
        o.schreiergenaction := ORB_MakeSchreierGeneratorPerm;
    else
        o.stabsize := 1;   # set this even if we do not compute stabiliser!
    fi;
    if not IsBound( o.onlystab ) then
        o.onlystab := false;
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
    o.stopper := false;   # no stopping condition
    o.genstoapply := [1..Length(gens)];   # an internal trick!
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
        # The standard case using a hash:
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

InstallMethod( ViewObj, "for an orbit", [IsOrbit and IsList and IsFinite],
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
  [IsObject, IsOrbit and IsHashOrbitRep and IsDenseList],
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
    while nr <= limit and i <= nr and i <> o!.stopper do
        for j in o!.genstoapply do
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

## InstallMethod( Enumerate, 
##   "for a hash orbit with Schreier tree and a limit", 
##   [IsOrbit and IsHashOrbitRep and WithSchreierTree, IsCyclotomic ],
##   function( o, limit )
##     local i,j,nr,orb,pos,yy,rep;
##     i := o!.pos;  # we go on here
##     orb := o!.orbit;
##     nr := Length(orb);
##     if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
##         limit := o!.orbsizebound; 
##     fi;
##     if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
##         o!.found := 1;
##         return o;
##     fi;
##     rep := o!.report;
##     while nr <= limit and i <= nr and i <> o!.stopper do
##         for j in o!.genstoapply do
##             yy := o!.op(orb[i],o!.gens[j]);
##             pos := ValueHT(o!.ht,yy);
##             if pos = fail then
##                 nr := nr + 1;
##                 orb[nr] := yy;
##                 if o!.storenumbers then
##                     AddHT(o!.ht,yy,nr);
##                 else
##                     AddHT(o!.ht,yy,true);
##                 fi;
##                 o!.schreiergen[nr] := j;
##                 o!.schreierpos[nr] := i;
##                 if LookFor(o,yy) = true then
##                     o!.pos := i;
##                     o!.found := nr;
##                     return o;
##                 fi;
##                 if IsBound(o!.orbsizebound) and 
##                    Length(o!.orbit) >= o!.orbsizebound then
##                     o!.pos := i;
##                     SetFilterObj(o,IsClosed);
##                     return o;
##                 fi;
##             fi;
##         od;
##         i := i + 1;
##         rep := rep - 1;
##         if rep = 0 then
##             rep := o!.report;
##             Info(InfoOrb,1,"Have ",nr," points.");
##         fi;
##     od;
##     o!.pos := i;
##     if i > nr then SetFilterObj(o,IsClosed); fi;
##     return o;
## end );

InstallGlobalFunction(ORB_MakeSchreierGeneratorPerm,
  function( o, i, j, pos )
    local basimg,sgen,sgennew,wordb,wordf;
    # Is stabilizer element trivial?
    wordf := TraceSchreierTreeForward(o,i);
    wordb := TraceSchreierTreeBack(o,pos);
    if IsBound(o!.permbase) then
        basimg := ActWithWord(o!.permgens,wordf,
                              OnTuples,o!.permbase);
        basimg := OnTuples(basimg,o!.permgens[j]);
        basimg := ActWithWord(o!.permgensi,wordb,
                              OnTuples,basimg);
        if not(ORB_SiftBaseImage(o!.stabchain,basimg,1)) then
            sgennew := true;
            Info(InfoOrb,4,"Evaluating stabilizer element...");
            sgen := EvaluateWord(o!.permgens,wordf)*
                    o!.permgens[j] *
                    EvaluateWord(o!.permgensi,wordb);
        else
            sgennew := false;
        fi;
    else
        Info(InfoOrb,4,"Evaluating stabilizer element...");
        sgen := EvaluateWord(o!.permgens,wordf)*o!.permgens[j] *
                EvaluateWord(o!.permgensi,wordb);
        sgennew := not(IsOne(sgen)) and not(sgen in o!.stab);
    fi; 
    if sgennew then
        # Calculate an element of the stabilizer:
        if o!.stabsize = 1 then
            o!.stab := Group(sgen);
        else
            o!.stab := Group(Concatenation(
                     GeneratorsOfGroup(o!.stab),[sgen]));
        fi;
        ORB_ComputeStabChain(o);
        Add(o!.stabwords,Concatenation(wordf,[j],-wordb));
        Info(InfoOrb,2,"New stabilizer size: ",o!.stabsize);
        return true;
    else
        return false;
    fi;
  end );

InstallMethod( Enumerate, 
  "for a hash orbit with or without permutation stabilizer and a limit", 
  [IsOrbit and IsHashOrbitRep and WithSchreierTree, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,pos,rep,yy;
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
    while nr <= limit and i <= nr and i <> o!.stopper and
          not(o!.stabcomplete and o!.onlystab) do
        for j in o!.genstoapply do
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
                   (not(WithPermStabilizer(o)) or o!.stabcomplete) then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
                if IsBound(o!.grpsizebound) and not(o!.stabcomplete) then
                    if Length(o!.orbit)*o!.stabsize*2 > o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            elif not(o!.stabcomplete) and o!.schreiergenaction <> false then
                # Trigger some action usually to produce Schreier generator:
                if o!.schreiergenaction(o,i,j,pos) then
                    if IsBound(o!.stabsizebound) and
                       o!.stabsize >= o!.stabsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                    if IsBound(o!.grpsizebound) and 
                        Length(o!.orbit)*o!.stabsize*2 > o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
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
    while nr <= limit and i <= nr and i <> o!.stopper do
        for j in o!.genstoapply do
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

## InstallMethod( Enumerate, 
##   "for a perm on int orbit with Schreier tree and a limit", 
##   [IsOrbit and IsPermOnIntOrbitRep and WithSchreierTree, IsCyclotomic],
##   function( o, limit )
##     local i,j,nr,orb,tab,yy,rep;
##     i := o!.pos;  # we go on here
##     orb := o!.orbit;
##     tab := o!.tab;
##     nr := Length(orb);
##     if IsBound(o!.orbsizebound) and o!.orbsizebound < limit then 
##         limit := o!.orbsizebound; 
##     fi;
##     if i = 1 and o!.found = false and LookFor(o,o!.orbit[1]) then
##         o!.found := 1;
##         return o;
##     fi;
##     rep := o!.report;
##     while nr <= limit and i <= nr and i <> o!.stopper do
##         for j in o!.genstoapply do
##             yy := o!.op(orb[i],o!.gens[j]);
##             if tab[yy] = 0 then
##                 nr := nr + 1;
##                 orb[nr] := yy;
##                 tab[yy] := nr;
##                 o!.schreiergen[nr] := j;
##                 o!.schreierpos[nr] := i;
##                 if LookFor(o,yy) = true then
##                     o!.pos := i;
##                     o!.found := nr;
##                     return o;
##                 fi;
##                 if IsBound(o!.orbsizebound) and 
##                    Length(o!.orbit) >= o!.orbsizebound then
##                     o!.pos := i;
##                     SetFilterObj(o,IsClosed);
##                     return o;
##                 fi;
##             fi;
##         od;
##         i := i + 1;
##         rep := rep - 1;
##         if rep = 0 then
##             rep := o!.report;
##             Info(InfoOrb,1,"Have ",nr," points.");
##         fi;
##     od;
##     o!.pos := i;
##     if i > nr then SetFilterObj(o,IsClosed); fi;
##     return o;
## end );

InstallMethod( Enumerate, 
  "for a perm on int orbit with or without permutation stabilizer and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep and WithSchreierTree, IsCyclotomic],
  function( o, limit )
    local i,j,nr,orb,rep,tab,yy;
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
    while nr <= limit and i <= nr and i <> o!.stopper and
          not(o!.stabcomplete and o!.onlystab) do
        for j in o!.genstoapply do
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
                   (not(WithPermStabilizer(o)) or o!.stabcomplete) then
                    o!.pos := i;
                    SetFilterObj(o,IsClosed);
                    return o;
                fi;
                if IsBound(o!.grpsizebound) and not(o!.stabcomplete) then
                    if Length(o!.orbit)*o!.stabsize*2 > o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            elif not(o!.stabcomplete) and o!.schreiergenaction <> false then
                # Trigger some action usually to produce Schreier generator:
                if o!.schreiergenaction(o,i,j,tab[yy]) then
                    if IsBound(o!.stabsizebound) and
                       o!.stabsize >= o!.stabsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                    if IsBound(o!.grpsizebound) and 
                        Length(o!.orbit)*o!.stabsize*2 > o!.grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
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
    
InstallMethod( AddGeneratorToOrbit, "for an orbit and a generator",
  [ IsOrbit, IsObject ],
  function( o, gen )
    local lmp;
    Add(o!.gens,gen);
    o!.nrgens := o!.nrgens + 1;
    if IsPermOnIntOrbitRep(o) then
        lmp := LargestMovedPoint(o!.gens);
        if lmp > Length(o!.tab) then
            Append(o!.tab,ListWithIdenticalEntries(lmp-Length(o!.tab),0));
        fi;
    fi;
    ResetFilterObj(o,IsClosed);
    o!.stopper := o!.pos;
    o!.pos := 1;
    o!.genstoapply := [Length(o!.gens)];
    Enumerate(o);
    if o!.pos <> o!.stopper then
        Error("Unexpected case!");
    fi;
    o!.stopper := false;
    o!.genstoapply := [1..Length(o!.gens)];
    return o;
  end );

InstallMethod( AddGeneratorToOrbit, "for an orbit and a generator",
  [ IsOrbit and WithPermStabilizer, IsObject ],
  function( o, gen )
    local lmp;
    if not(IsList(gen)) or Length(gen) <> 2 then
        Error("Need a pair of generators as second argument.");
        return;
    fi;
    Add(o!.gens,gen[1]);
    o!.nrgens := o!.nrgens + 1;
    ResetFilterObj(o,IsClosed);
    Add(o!.permgens,gen[2]);
    Add(o!.permgensi,gen[2]^-1);
    if IsPermOnIntOrbitRep(o) then
        lmp := LargestMovedPoint(o!.gens);
        if lmp > Length(o!.tab) then
            Append(o!.tab,ListWithIdenticalEntries(lmp-Length(o!.tab),0));
        fi;
    fi;
    o!.stopper := o!.pos;
    o!.pos := 1;
    o!.genstoapply := [Length(o!.gens)];
    Enumerate(o);
    if o!.pos <> o!.stopper then
        Error("Unexpected case!");
    fi;
    o!.stopper := false;
    o!.genstoapply := [1..Length(o!.gens)];
    return o;
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

InstallMethod( StabWords, "for an orbit with stabiliser",
  [IsOrbit and WithPermStabilizer],
  function( o ) return o!.stabwords; end );

InstallMethod( PositionOfFound,"for an orbit looking for something with a list",
  [IsOrbit and LookingForUsingList],
  function( o ) return o!.found; end );
InstallMethod( PositionOfFound,"for an orbit looking for something with a hash",
  [IsOrbit and LookingForUsingHash],
  function( o ) return o!.found; end );
InstallMethod( PositionOfFound,"for an orbit looking for something with a func",
  [IsOrbit and LookingForUsingFunc],
  function( o ) return o!.found; end );

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

#################################################
# A helper function for base image computations:
#################################################

InstallGlobalFunction( ORB_SiftBaseImage,
  function(S,bi,i)
    local bpt,l,te;
    l := Length(bi);
    while i <= Length(bi) do
        bpt := S.orbit[1];
        while bi[i] <> bpt  do
            if not(IsBound(S.transversal[bi[i]])) then
                return false;
            fi;
            te := S.transversal[bi[i]];
            bi{[i..l]} := OnTuples(bi{[i..l]},te);
        od;
        i := i + 1;
        S := S.stabilizer;
    od;
    return true;
  end );

InstallGlobalFunction( ORB_ComputeStabChain,
  function( o )
    # stab must be a perm group, stabchainrandom a number, permbase either
    # a list of integers or fail
    if IsBound(o!.permbase) then
        o!.stabchain := StabChainOp(o!.stab,rec(base := o!.permbase, 
                                                reduced := false,
                                                random := o!.stabchainrandom) );
    else
        o!.stabchain := StabChainOp(o!.stab,rec(random := o!.stabchainrandom));
    fi;
    o!.stabsize := SizeStabChain(o!.stabchain);
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

