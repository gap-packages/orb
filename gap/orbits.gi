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
#  .hashlen      for the call version with 3 or 4 arguments with options
#  .hashfunc     only together with next option, hashs cannot grow!
#  .eqfunc
#  .storingnumbers  indicates whether positions are stored in the hash
#  .looking
#  .lookfunc

# Outputs:
#  .gens
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

InstallGlobalFunction( Orb, 
  function( arg )
    local comp,filts,gens,hashlen,lmp,o,op,opt,x;

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

    # We make a copy, the first two must be accessable very fast,
    # so we assign them first:
    o := rec( stabcomplete := false, stabsize := 1 );
    for comp in RecFields(opt) do
        o.(comp) := opt.(comp);
    od;

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

    if not(IsBound(o.schreiergenaction)) then
        o.schreiergenaction := false;
    fi;

    # Maybe we have some permutations to compute the stabilizer:
    if IsBound( o.permgens ) then 
        o.schreier := true;      # we need a Schreier tree
        o.storenumbers := true;  # and have to index points
        if not(IsBound(o.stabchainrandom)) then
            o.stabchainrandom := 1000;  # no randomisation
        fi;
        if not(IsBound(o.permbase)) then
            o.permbase := false;
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
        o.stabwords := [];
        # The following triggers the generation of Schreier generators:
        o.schreiergenaction := ORB_MakeSchreierGeneratorPerm;
    else
        o.permgens := false;  # to indicate no permgens
        o.permbase := false;
    fi;

    # Only looking for stabilizer?
    if not IsBound( o.onlystab ) then
        o.onlystab := false;
    fi;

    # FIXME: check for matgensi here !
    o!.matgens := false;

    # Are we looking for something?
    if IsBound(o.lookingfor) and o.lookingfor <> fail then 
        o.looking := true;
        if IsList(o.lookingfor) then
            o.lookfunc := ORB_LookForList;
        elif IsRecord(o.lookingfor) and IsBound(o.lookingfor.ishash) then
            o.lookfunc := ORB_LookForHash;
        elif IsFunction(o.lookingfor) then
            o.lookfunc := o.lookingfor;
        else
            Error("opt.lookingfor must be a list or a hash table or a",
                  " function");
        fi;
    else
        o.looking := false;
    fi;
    o.found := false; 

    # Are we building a Schreier tree?
    if not(IsBound(o.schreier)) or o.schreier = false then 
        o.schreier := false;
        o.schreiergen := false; 
        o.schreierpos := false;
    else
        o.schreiergen := [fail];
        o.schreierpos := [fail];
    fi;

    # Should we print a progress report?
    if not(IsBound(o.report)) then
        o.report := false;
    fi;

    # Are we storing positions in the hash?
    if not(IsBound(o.storenumbers)) then o.storenumbers := false; fi;

    # Set grpsizebound if unset:
    if not(IsBound(o.grpsizebound)) then o.grpsizebound := false; fi;

    # Set orbsizebound if unset:
    if not(IsBound(o.orbsizebound)) then o.orbsizebound := false; fi;

    # Set stabsizebound if unset:
    if not(IsBound(o.stabsizebound)) then o.stabsizebound := false; fi;
    
    # Can we learn from the grpsizebound?
    if o.grpsizebound <> false then
        # Can we learn an orbit length bound from a group order bound?
        if o.orbsizebound = false then o.orbsizebound := o.grpsizebound; fi;
        # Can we learn a stabiliser bound from a group order bound?
        if o.stabsizebound = false then o.stabsizebound := o.grpsizebound; fi;
    fi;

    # Do we already know the complete stabiliser?
    # (by now stabsizebound and stabsize are bound)
    if o.stabsizebound <> false then
        if o.stabsize >= o.stabsizebound then
            Info(InfoOrb,2,"Stabilizer complete.");
            o.stabcomplete := true;
        fi;
    fi;
    
    # Do we have a stopper set?
    if not(IsBound(o.stopper)) then
        o.stopper := false;   # no stopping condition
    fi;

    # Now take this record as our orbit record and return:
    o.gens := gens;
    o.genstoapply := [1..Length(gens)];   # an internal trick!
    o.op := op;
    o.orbit := [x];
    o.pos := 1;
    # We distinguish three cases, first permutations on integers:
    if ForAll(gens,IsPerm) and IsPosInt(x) and op = OnPoints then
        # A special case for permutation acting on integers:
        lmp := LargestMovedPoint(gens);
        if x > lmp then 
            Info(InfoOrb,1,"Warning: start point not in permuted range");
        fi;
        o.tab := 0*[1..lmp];
        o.tab[x] := 1;
        filts := filts and IsPermOnIntOrbitRep;
        if o.orbsizebound = false then
            o.orbsizebound := lmp;
        fi;
        o.storenumbers := true;  # we do this anyway!
    else
        # The standard case using a hash:
        if IsBound(o.eqfunc) and IsBound(o.hashfunc) then
            o.ht := InitHT( hashlen, o.hashfunc, o.eqfunc );
            if IsBound(o.hfbig) and IsBound(o.hfdbig) then
                o.ht.hfbig := o.hfbig;
                o.ht.hfdbig := o.hfdbig;
                o.ht.cangrow := true;
            fi;
            filts := filts and IsHashOrbitRep;
        else
            o.ht := NewHT(x,hashlen);
            if o.ht = fail then    # probably we found no hash function
                filts := filts and IsSlowOrbitRep;
            else
                filts := filts and IsHashOrbitRep;
            fi;
        fi;
        if o.ht <> fail then
            # Store the first point, if it is a hash orbit:
            if o.storenumbers then
                AddHT(o.ht,x,1);
            else
                AddHT(o.ht,x,true);
            fi;
        fi;
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
    if o!.schreier then
        Print(" with Schreier tree");
    fi;
    if o!.permgens <> false or o!.matgens <> false then
        Print(" and stabilizer");
        if o!.onlystab then
            Print(" going for stabilizer");
        fi;
    fi;
    if o!.looking then
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
  [IsOrbit and IsHashOrbitRep and IsDenseList, IsObject, IsInt],
  function( orb, ob, pos )
    local p;
    p := ValueHT(orb!.ht,ob);
    if p = fail then
        return fail;
    elif p = true then   # we did not store numbers!
        return Position( orb!.orbit, ob, pos );  # do it the slow way!
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
  [IsOrbit and IsDenseList and IsPermOnIntOrbitRep, IsPosInt, IsInt],
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
  [IsPosInt, IsOrbit and IsDenseList and IsPermOnIntOrbitRep],
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

InstallGlobalFunction( ORB_LookForList,
  function( o, p )
    return p in o!.lookingfor;
  end );

InstallGlobalFunction( ORB_LookForHash,
  function( o, p )
    return ValueHT(o!.lookingfor,p) <> fail;
  end );


InstallGlobalFunction(ORB_MakeSchreierGeneratorPerm,
  function( o, i, j, pos )
    local basimg,sgen,sgennew,wordb,wordf;
    # Is stabilizer element trivial?
    wordf := TraceSchreierTreeForward(o,i);
    wordb := TraceSchreierTreeBack(o,pos);
    if o!.permbase <> false then
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
  "for a hash orbit and a limit", 
  [IsOrbit and IsHashOrbitRep, IsCyclotomic],
  function( o, limit )
    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.
    local gens,genstoapply,grpsizebound,ht,i,j,lookfunc,looking,nr,
          onlystab,op,orb,orbsizebound,permgens,pos,rep,schreier,schreiergen,
          schreiergenaction,schreierpos,stabsizebound,stopper,storenumbers,yy;

    # Set a few local variables for faster access:
    orb := o!.orbit;
    i := o!.pos;  # we go on here
    nr := Length(orb);

    # We copy a few things to local variables to speed up access:
    looking := o!.looking;
    if looking then lookfunc := o!.lookfunc; fi;
    stopper := o!.stopper;
    onlystab := o!.onlystab;
    storenumbers := o!.storenumbers;
    op := o!.op;
    gens := o!.gens;
    ht := o!.ht;
    genstoapply := o!.genstoapply;
    schreier := o!.schreier;
    if schreier then
        schreiergen := o!.schreiergen;
        schreierpos := o!.schreierpos;
    fi;
    orbsizebound := o!.orbsizebound;
    permgens := o!.permgens;
    grpsizebound := o!.grpsizebound;
    schreiergenaction := o!.schreiergenaction;
    stabsizebound := o!.stabsizebound;

    # Maybe we are looking for something and it is the start point:
    if i = 1 and o!.found = false and looking then
        if lookfunc(o,orb[1]) then
            o!.found := 1;
            return o;
        fi;
    fi;

    rep := o!.report;
    while nr <= limit and i <= nr and i <> stopper do
        # Catch the case that we only want the stabilizer:
        if onlystab and o!.stabcomplete then break; fi;

        # Now apply generators:
        for j in genstoapply do
            yy := op(orb[i],gens[j]);
            pos := ValueHT(ht,yy);
            if pos = fail then
                nr := nr + 1;
                orb[nr] := yy;
                if storenumbers then
                    AddHT(ht,yy,nr);
                else
                    AddHT(ht,yy,true);
                fi;

                # Handle Schreier tree if desired:
                if schreier then
                    schreiergen[nr] := j;
                    schreierpos[nr] := i;
                fi;
                
                # Are we looking for something?
                if looking then
                    if lookfunc(o,yy) then
                        o!.pos := i;
                        o!.found := nr;
                        return o;
                    fi;
                fi;

                # Do we have a bound for the orbit length?
                if orbsizebound <> false and nr >= orbsizebound then
                    # The orbit is ready, but do we have the stabiliser?
                    if permgens = false or o!.stabcomplete then
                        o!.pos := i;
                        SetFilterObj(o,IsClosed);
                        return o;
                    fi;
                fi;

                # Do we have a bound for the group and a stabiliser?
                if grpsizebound <> false and not o!.stabcomplete then
                    if nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;

            elif schreiergenaction <> false and not(o!.stabcomplete) then

                # Trigger some action usually to produce Schreier generator:
                if schreiergenaction(o,i,j,pos) then
                    # o!.stabsize has changed:
                    if stabsizebound <> false and
                       o!.stabsize >= stabsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                    if grpsizebound <> false and
                       nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            fi;
        od;
        i := i + 1;
        if rep <> false then
            rep := rep - 1;
            if rep = 0 then
                rep := o!.report;
                Info(InfoOrb,1,"Have ",nr," points.");
            fi;
        fi;
    od;
    o!.pos := i;
    if i > nr or nr >= o!.orbsizebound then SetFilterObj(o,IsClosed); fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a perm on int orbit with or without permutation stabilizer and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep, IsCyclotomic],
  function( o, limit )
    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.
    local gens,genstoapply,grpsizebound,i,j,lookfunc,looking,nr,onlystab,orb,
          orbsizebound,permgens,rep,schreier,schreiergen,schreiergenaction,
          schreierpos,stabsizebound,stopper,tab,yy;

    orb := o!.orbit;
    i := o!.pos;  # we go on here
    tab := o!.tab;
    nr := Length(orb);

    # We copy a few things to local variables to speed up access:
    looking := o!.looking;
    if looking then lookfunc := o!.lookfunc; fi;
    stopper := o!.stopper;
    onlystab := o!.onlystab;
    gens := o!.gens;
    genstoapply := o!.genstoapply;
    schreier := o!.schreier;
    if schreier then
        schreiergen := o!.schreiergen;
        schreierpos := o!.schreierpos;
    fi;
    orbsizebound := o!.orbsizebound;
    permgens := o!.permgens;
    grpsizebound := o!.grpsizebound;
    schreiergenaction := o!.schreiergenaction;
    stabsizebound := o!.stabsizebound;

    # Maybe we are looking for something and it is the start point:
    if i = 1 and o!.found = false and looking then
        if lookfunc(o,o!.orbit[1]) then
            o!.found := 1;
            return o;
        fi;
    fi;

    rep := o!.report;
    while nr <= limit and i <= nr and i <> stopper do
        # Catch the case that we only want the stabilizer:
        if onlystab and o!.stabcomplete then break; fi;

        # Now apply generators:
        for j in genstoapply do
            yy := orb[i]^gens[j];
            if tab[yy] = 0 then
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;

                # Handle Schreier tree if desired:
                if schreier then
                    o!.schreiergen[nr] := j;
                    o!.schreierpos[nr] := i;
                fi;

                # Are we looking for something?
                if looking then
                    if lookfunc(o,yy) then
                        o!.pos := i;
                        o!.found := nr;
                        return o;
                    fi;
                fi;

                # Do we have a bound for the orbit length?
                if orbsizebound <> false and nr >= orbsizebound then
                    # The orbit is ready, but do we have the stabiliser?
                    if permgens = false or o!.stabcomplete then
                        o!.pos := i;
                        SetFilterObj(o,IsClosed);
                        return o;
                    fi;
                fi;

                # Do we have a bound for the group and a stabiliser?
                if grpsizebound <> false and not o!.stabcomplete then
                    if nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;

            elif schreiergenaction <> false and not(o!.stabcomplete) then

                # Trigger some action usually to produce Schreier generator:
                if schreiergenaction(o,i,j,tab[yy]) then
                    # o!.stabsize has changed:
                    if stabsizebound <> false and
                       o!.stabsize >= stabsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                    if grpsizebound <> false and 
                       nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,2,"Stabilizer complete.");
                    fi;
                fi;
            fi;
        od;
        i := i + 1;
        if rep <> false then
            rep := rep - 1;
            if rep = 0 then
                rep := o!.report;
                Info(InfoOrb,1,"Have ",nr," points.");
            fi;
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
  [ IsOrbit, IsObject ],
  function( o, gen )
    local lmp;
    if not(IsList(gen)) or Length(gen) <> 2 then
        Error("Need a pair of generators as second argument.");
        return;
    fi;
    Add(o!.gens,gen[1]);
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
  [ IsOrbit, IsPosInt ],
  function( o, pos )
    local word;
    if not(o!.schreier) then
        Error("Orbit does not have a Schreier tree");
        return fail;
    fi;
    word := [];
    while pos > 1 do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    return Reversed(word);
  end );

InstallMethod( TraceSchreierTreeBack, "for an orbit and a position",
  [ IsOrbit, IsPosInt ],
  function( o, pos )
    local word;
    if not(o!.schreier) then
        Error("Orbit does not have a Schreier tree");
        return fail;
    fi;
    word := [];
    while pos > 1 do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    return word;
  end );

InstallMethod( StabWords, "for an orbit with stabiliser",
  [IsOrbit],
  function( o ) 
    if not(IsBound(o!.stabwords)) then
        Error("Orbit does not have stabiliser information");
        return fail;
    else
        return o!.stabwords;
    fi;
  end );

InstallMethod( PositionOfFound,"for an orbit",
  [IsOrbit],
  function( o ) 
    if not(o!.looking) then
        Error("Orbit is not looking for something");
        return fail;
    else
        return o!.found; 
    fi;
  end );

InstallOtherMethod( StabilizerOfExternalSet, 
  "for an orbit with permutation stabilizer",
  [ IsOrbit ],
  function( o ) 
    if not(IsBound(o!.stab)) then
        Error("Orbit does not have stabiliser information");
        return fail;
    else
        return o!.stab;
    fi;
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
  [ IsOrbit and IsHashOrbitRep and IsClosed, IsList ],
  function( o, gens )
    local res,i;
    if not(o!.storenumbers) then
        return ORB_ActionOnOrbitIntermediateHash(o,gens);
    fi;
    res := [];
    for i in [1..Length(gens)] do
      Add(res,PermList(
       List([1..Length(o!.orbit)],
            j->ValueHT(o!.ht,o!.op(o!.orbit[j],gens[i])))));
    od;
    return res;
  end );
 
InstallGlobalFunction( ORB_ActionOnOrbitIntermediateHash,
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
    if o!.permbase <> false then
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

