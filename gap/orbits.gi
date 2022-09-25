#############################################################################
##
##                             orb package
##  orbits.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for fast standard orbit enumeration.
##
#############################################################################


# Possible options in the `ORB` record:
#  .eqfunc
#  .genstoapply
#  .gradingfunc
#  .onlygrades
#  .onlygradesdata
#  .grpsizebound
#  .hashfunc        only together with next option, hashs cannot grow!
#  .hashlen         for the call version with 3 or 4 arguments with options
#  .hfbig
#  .hfdbig
#  .log             either false or a list of length 2 orbitlength
#  .lookingfor
#  .matgens
#  .onlystab
#  .orbsizebound
#  .orbitgraph
#  .permbase
#  .permgens
#  .report
#  .schreier
#  .schreiergenaction
#  .stab
#  .stabchainrandom
#  .stabsizebound
#  .storenumbers    indicates whether positions are stored in the hash
#  .treehashsize    indicates that we want to use a tree hash of this size

# Outputs:
#  .depth
#  .depthmarks
#  .found
#  .gens
#  .grades
#  .ht
#  .log
#  .logind          index into log
#  .logpos          write position in log
#  .lookfunc
#  .looking
#  .memorygens
#  .op
#  .orbind
#  .orbit
#  .permgensi
#  .pos
#  .schreiergen
#  .schreierpos
#  .stab
#  .stabchain
#  .stabcomplete
#  .stabsize
#  .stabwords
#  .tab

InstallGlobalFunction( Orb, 
  function( arg )
    local comp,filts,gens,hashlen,i,lmp,o,op,opt,re,x,f;

    # First parse the arguments:
    if Length(arg) = 3 then
        gens := arg[1]; x := arg[2]; op := arg[3];
        hashlen := 10007; opt := rec();
    elif Length(arg) = 4 and IsInt(arg[4]) then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := rec();
    elif Length(arg) = 4 then
        gens := arg[1]; x := arg[2]; op := arg[3]; opt := arg[4];
        if IsBound(opt.hashlen) then
            hashlen := opt.hashlen;
        else
            hashlen := 10007;
        fi;
    elif Length(arg) = 5 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := arg[5];
    else
        ErrorNoReturn("Usage: Orb( gens, point, action [,options] )");
    fi;

    # We make a copy, the first two must be accessible very fast,
    # so we assign them first:
    o := rec( stabcomplete := false, stabsize := 1 );
    for comp in RecNames(opt) do
        o.(comp) := opt.(comp);
    od;

    # Now get rid of the group object if necessary but preserve known size:
    if IsGroup(gens) then
        if HasSize(gens) then
            o.grpsizebound := Size(gens);
        fi;
        gens := ShallowCopy(GeneratorsOfGroup(gens));
    elif IsMonoid(gens) then
        gens := ShallowCopy(GeneratorsOfMonoid(gens));
    elif IsSemigroup(gens) then
        gens := ShallowCopy(GeneratorsOfSemigroup(gens));
    elif not(IsMutable(gens)) then
        gens := ShallowCopy(gens);
    fi;


    # We collect the filters for the type:
    filts := IsOrbit and IsMutable;

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
        elif IsHashTab(o.lookingfor) then
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

    # Do we write a log?
    if IsBound(o.log) and o.log = true then
        o.log := [];
        o.logind := [];
        o.logpos := 1;
        filts := filts and IsOrbitWithLog;
        o.storenumbers := true;   # otherwise AddGeneratorsToOrbit doesn't work
    else
        o.log := false;
    fi;

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
            Info(InfoOrb,3,"Stabilizer complete.");
            o.stabcomplete := true;
        fi;
    fi;
    
    # Do we have a stopper set?
    if not(IsBound(o.stopper)) then
        o.stopper := false;   # no stopping condition
    fi;

    # Do we compute a grading?
    if IsBound(o.gradingfunc) then
        o.grades := [];
        if not(IsBound(o.onlygrades)) then
            o.onlygrades := false;
        else
            if IsList(o.onlygrades) then
                o.onlygradesdata := o.onlygrades;
                o.onlygrades := IN;
            elif IsHashTab(o.onlygrades) then
                o.onlygradesdata := o.onlygrades;
                o.onlygrades := ORB_CheckGradeForHash;
            elif IsFunction(o.onlygrades) then
                if not(IsBound(o.onlygradesdata)) then
                    o.onlygradesdata := fail;
                fi;
            else
                Error("option onlygrades must be a list, a hash or a function");
            fi;
        fi;
        filts := filts and IsGradedOrbit;
    else
        o.gradingfunc := false;
        o.grades := false;
        o.onlygrades := false;
        o.onlygradesdata := false;
    fi;

    # Now take this record as our orbit record and return:
    o.gens := gens;
    if Length(gens) > 0 and IsObjWithMemory(gens[1]) then
        o.memorygens := true;
    else
        o.memorygens := false;
    fi;
    o.genstoapply := [1..Length(gens)];   # an internal trick!
    o.op := op;
    o.orbit := [x];
    if IsBound(o.seeds) then
        Append(o.orbit,o.seeds);
    fi;
    o.pos := 1;
    if o.schreier or o.log <> false then
        o.depth := 0;
        o.depthmarks := [Length(o.orbit)+1]; # depth 1 starts at first new pt
    fi;

    if not(IsBound(o.orbitgraph)) then
        o.orbitgraph := false;
    elif o.orbitgraph <> false then
        o.orbitgraph := [];
        for i in [1..Length(o.orbit)] do
            o.orbitgraph[i] := EmptyPlist(Length(gens));
        od;
        o.storenumbers := true;
    fi;

    # We distinguish three cases, first permutations on integers:
    if ForAll(gens,IsPerm) and IsPosInt(x) and op = OnPoints then
        # A special case for permutation acting on integers:
        lmp := LargestMovedPoint(gens);
        if x > lmp and lmp > 0 then 
            Info(InfoOrb,1,"Warning: start point not in permuted range");
        fi;
        o.tab := 0*[1..lmp];
        for i in [1..Length(o.orbit)] do
            o.tab[o.orbit[i]] := 1;
        od;
        filts := filts and IsPermOnIntOrbitRep;
        if o.orbsizebound = false then
            o.orbsizebound := lmp;
        fi;
        o.storenumbers := true;  # we do this anyway!
    else
        # The standard case using a hash:
        if IsBound(o.treehashsize) then
            re := rec( treehashsize := o.treehashsize );
            if IsBound(o.cmpfunc) then
                re.cmpfunc := o.cmpfunc;
            fi;
            if IsBound(o.hashfunc) then
                re.hf := o.hashfunc.func;
                re.hfd := o.hashfunc.data;
            fi;
            if IsBound(o.forflatplainlists) then
                re.forflatplainlists := true;
            fi;
            # Handle the special case of matrices over a finite field
            # acting on vectors with OnRight or OnLines, because the
            # field of the vector could be smaller than the field of the
            # matrices:
            if not(IsBound(re.hf)) and not(IsBound(re.forflatplainlists)) and
               IsRowVector(x) and IsFFECollection(x) and
               ForAll(o.gens,IsFFECollColl) then
                f := FieldOfMatrixList(o.gens);
                if Size(DefaultField(x)) < Size(f) then
                    o.ht := HTCreate(x*PrimitiveRoot(f),re);
                else
                    o.ht := HTCreate(x,re);
                fi;
            else
                o.ht := HTCreate(x,re);
            fi;
            filts := filts and IsHashOrbitRep;
        elif IsBound(o.eqfunc) and IsBound(o.hashfunc) then
            o.ht := HTCreate(x,rec( hf := o.hashfunc.func,
                                    hfd := o.hashfunc.data,
                                    eqf := o.eqfunc,
                                    hashlen := hashlen ));
            if IsBound(o.hfbig) and IsBound(o.hfdbig) then
                o.ht!.hfbig := o.hfbig;
                o.ht!.hfdbig := o.hfdbig;
                o.ht!.cangrow := true;
            fi;
            filts := filts and IsHashOrbitRep;
        else
            re := rec( hashlen := hashlen );
            if IsBound(o.forflatplainlists) then
                re.forflatplainlists := true;
            fi;
            o.ht := HTCreate(x,re);
            if o.ht = fail then    # probably we found no hash function
                Info(InfoOrb,1,"Warning: No hash function found!");
                filts := filts and IsSlowOrbitRep;
            else
                filts := filts and IsHashOrbitRep;
            fi;
        fi;
        if o.ht <> fail then
            # Store the first point, if it is a hash orbit:
            for i in [1..Length(o.orbit)] do
                if o.schreier then 
                  o.schreiergen[i]:=fail;
                  o.schreierpos[i]:=fail;
                fi;
                if o.storenumbers then
                    HTAdd(o.ht,o.orbit[i],i);
                else
                    HTAdd(o.ht,o.orbit[i],true);
                fi;
            od;
        fi;
    fi;
    Objectify( NewType(OrbitFamily,filts), o );
    if o!.gradingfunc <> false then
        for i in [1..Length(o!.orbit)] do
            o!.grades[i] := o!.gradingfunc(o,x);
        od;
    fi;
    return o;
end );

InstallMethod( IsClosed, "for an orbit", [IsOrbit], IsClosedOrbit );

InstallMethod( ViewObj, "for an orbit", [IsOrbit],
  function( o )
    Print("<");
    if IsClosedOrbit(o) then Print("closed "); else Print("open "); fi;
    if IsPermOnIntOrbitRep(o) then Print("Int-"); fi;
    Print("orbit, ", Length(o!.orbit), " points");
    if o!.schreier then Print(" with Schreier tree"); fi;
    if o!.permgens <> false or o!.matgens <> false then
        Print(" and stabilizer");
        if o!.onlystab then Print(" going for stabilizer"); fi;
    fi;
    if o!.looking then Print(" looking for sth."); fi;
    if o!.log <> false then Print(" with log"); fi;
    if o!.grades <> false then Print(" with grading"); fi;
    Print(">");
  end );

InstallMethod( ELM_LIST, "for an orbit object, and a positive integer", 
  [IsOrbit, IsPosInt],
  function( orb, pos )
    return orb!.orbit[pos];
  end );

InstallMethod( ELMS_LIST, "for an orbit object, and a list of integers",
  [IsOrbit, IsList],
  function( orb, poss )
    return orb!.orbit{poss};
  end );

InstallMethod( Length, "for an orbit object",
  [IsOrbit],
  function( orb )
    return Length(orb!.orbit);
  end );

InstallMethod( Position, "for an orbit object, an object, and an integer",
  [IsOrbit, IsObject, IsInt],
  function( orb, ob, pos )
    return Position( orb!.orbit, ob, pos );
  end );

InstallMethod( Position, 
  "for an orbit object storing numbers, an object, and an integer",
  [IsOrbit and IsHashOrbitRep, IsObject, IsInt],
  function( orb, ob, pos )
    local p;
    p := HTValue(orb!.ht,ob);
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
  [IsOrbit and IsPermOnIntOrbitRep, IsPosInt, IsInt],
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
  [IsObject, IsOrbit and IsHashOrbitRep],
  function( ob, orb )
    local p;
    p := HTValue( orb!.ht, ob );
    if p = fail then
      return false;
    else
      return true;
    fi;
  end );

InstallMethod( \in,
  "for an object and an orbit object perms on int",
  [IsPosInt, IsOrbit and IsPermOnIntOrbitRep],
  function( ob, orb )
    local p;
    if IsBound(orb!.tab[ob]) and orb!.tab[ob] <> 0 then
        return true;
    else
        return false;
    fi;
  end );

InstallMethod( Intersection2, "for two hash orbits",
  [ IsHashOrbitRep, IsHashOrbitRep ],
  function( o, oo )
    local l;
    l := Filtered(o,x->x in oo);
    return Set(l);
  end );

InstallMethod( Intersection2, "for two perm on int orbits",
  [ IsPermOnIntOrbitRep, IsPermOnIntOrbitRep ],
  function( o, oo )
    local l;
    l := Filtered(o,x->x in oo);
    return Set(l);
  end );

InstallMethod( IntersectSet, "for a mutable list and a hash orbit",
  [ IsDenseList and IsMutable, IsHashOrbitRep ],
  function( l, o )
    local int,i;
    int := Filtered(l,x->x in o);
    l{[1..Length(int)]} := int;
    for i in [Length(l),Length(l)-1..Length(int)+1] do
        Unbind(l[i]);
    od;
  end );

InstallMethod( IntersectSet, "for a mutable list and a perm on int orbit",
  [ IsDenseList and IsMutable, IsPermOnIntOrbitRep ],
  function( l, o )
    local int,i;
    int := Filtered(l,x->x in o);
    l{[1..Length(int)]} := int;
    for i in [Length(l),Length(l)-1..Length(int)+1] do
        Unbind(l[i]);
    od;
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
    return HTValue(o!.lookingfor,p) <> fail;
  end );

InstallGlobalFunction( ORB_CheckGradeForHash,
  function( p, data )
    return HTValue(data,p) <> fail;
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
    local orb, i, nr, looking, lookfunc, found, stopper, onlystab, storenumbers, op, gens, memorygens, ht, genstoapply, schreier, schreiergen, schreierpos, orbsizebound, permgens, grpsizebound, schreiergenaction, stabsizebound, log, logind, logpos, depth, depthmarks, grades, gradingfunc, onlygrades, onlygradesdata, orbitgraph, rep, suc, yy, pos, grade, j;
    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.

    # Set a few local variables for faster access:
    orb := o!.orbit;
    i := o!.pos;  # we go on here
    nr := Length(orb);

    # We copy a few things to local variables to speed up access:
    looking := o!.looking;
    if looking then 
      lookfunc := o!.lookfunc; 
      found:=o!.found;
      if found<>false then 
        for j in [found+1..nr] do 
          if lookfunc(o,orb[j]) then
            o!.found := j;
            return o;
          fi;
        od;
      elif i=1 and lookfunc(o, orb[1]) then 
      # Maybe we are looking for something and it is the start point:
        o!.found:=1;
        return o;
      fi;
    fi;
    
    found := false;
    stopper := o!.stopper;
    onlystab := o!.onlystab;
    storenumbers := o!.storenumbers;
    op := o!.op;
    gens := o!.gens;
    memorygens := o!.memorygens;
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
    log := o!.log;
    if log <> false then
        logind := o!.logind;
        logpos := o!.logpos;
    fi;
    if o!.schreier or o!.log <> false then
        depth := o!.depth;
        depthmarks := o!.depthmarks;
    fi;
    grades := o!.grades;
    gradingfunc := o!.gradingfunc;
    onlygrades := o!.onlygrades;
    onlygradesdata := o!.onlygradesdata;
    orbitgraph := o!.orbitgraph;

    rep := o!.report;
    while nr <= limit and i <= nr and i <> stopper do
        # Handle depth of Schreier tree:
        if (o!.schreier or o!.log <> false) and i >= depthmarks[depth+1] then
            depth := depth + 1;
            depthmarks[depth+1] := nr+1;
            Info(InfoOrb,3,"Going to depth ",depth);
        fi;

        # Catch the case that we only want the stabilizer:
        if onlystab and o!.stabcomplete then break; fi;

        # Are we logging?
        if log <> false then logind[i] := logpos; suc := false; fi;

        # Now apply generators:
        for j in genstoapply do
            if memorygens then
                yy := op(orb[i],gens[j]!.el);
            else
                yy := op(orb[i],gens[j]);
            fi;
            pos := HTValue(ht,yy);
            if gradingfunc <> false then
                grade := gradingfunc(o,yy);
                if onlygrades <> false and 
                   not(onlygrades(grade,onlygradesdata)) then
                    pos := false;
                fi;
            fi;
                
            if pos = fail then
                nr := nr + 1;
                orb[nr] := yy;
                if grades <> false then
                    grades[nr] := grade;
                fi;
                if storenumbers then
                    HTAdd(ht,yy,nr);
                else
                    HTAdd(ht,yy,true);
                fi;
                if orbitgraph <> false then
                    orbitgraph[nr] := EmptyPlist(Length(gens));
                    orbitgraph[i][j] := nr;
                fi;

                # Handle Schreier tree if desired:
                if schreier then
                    schreiergen[nr] := j;
                    schreierpos[nr] := i;
                fi;
                
                # Handle logging if desired:
                if log <> false then
                    suc := true;
                    log[logpos] := j;
                    log[logpos+1] := nr;
                    logpos := logpos+2;
                    o!.logpos := logpos;   # write back to preserve
                fi;
                
                # Are we looking for something?
                if looking and not found then
                    if lookfunc(o,yy) then
                        found:=true;
                        o!.found := nr;
                    fi;
                fi;

                # Do we have a bound for the orbit length?
                if orbsizebound <> false and nr >= orbsizebound then
                    # The orbit is ready, but do we have the stabiliser?
                    # Also, only early stop if no log is written!
                    if (permgens = false or o!.stabcomplete) and
                       log = false then
                        o!.pos := i;
                        SetFilterObj(o,IsClosedOrbit);
                        if o!.schreier or o!.log <> false then
                            o!.depth := depth;
                        fi;
                        return o;
                    fi;
                fi;

                # Do we have a bound for the group and a stabiliser?
                if grpsizebound <> false and not o!.stabcomplete then
                    if nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,3,"Stabilizer complete.");
                    fi;
                fi;

            elif pos <> false then    # false if point was rejected by grade
                if orbitgraph <> false then
                    orbitgraph[i][j] := pos;
                fi;
                if schreiergenaction <> false and not(o!.stabcomplete) then
                    # Trigger some action usually to produce Schreier generator:
                    if schreiergenaction(o,i,j,pos) then
                        # o!.stabsize has changed:
                        if stabsizebound <> false and
                           o!.stabsize >= stabsizebound then
                            o!.stabcomplete := true;
                            Info(InfoOrb,3,"Stabilizer complete.");
                        fi;
                        if grpsizebound <> false and
                           nr*o!.stabsize*2 > grpsizebound then
                            o!.stabcomplete := true;
                            Info(InfoOrb,3,"Stabilizer complete.");
                        fi;
                    fi;
                fi;
            fi;
        od;
        # Now close the log for this point:
        if log <> false then
            if suc then
                log[logpos-2] := -log[logpos-2];
                if looking and found then i:=i+1; break; fi;
                # we only look among new points
            else
                logind[i] := 0;
            fi;
        elif looking and found then 
          i:=i+1; break;
        fi;
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
    if o!.schreier or o!.log <> false then o!.depth := depth; fi;
    if (orbsizebound <> false and nr >= orbsizebound and log = false) or
       i > nr then
        # Only if not logging!
        SetFilterObj(o,IsClosedOrbit);
        if log <> false then 
            o!.orbind := [1..nr];
        fi;
    fi;
    return o;
end );

InstallMethod( Enumerate, 
  "for a perm on int orbit with or without permutation stabilizer and a limit", 
  [IsOrbit and IsPermOnIntOrbitRep, IsCyclotomic],
  function( o, limit )
    local orb, i, tab, nr, looking, lookfunc, found, stopper, onlystab, gens,
      memorygens, genstoapply, schreier, schreiergen, schreierpos, orbsizebound,
      permgens, grpsizebound, schreiergenaction, stabsizebound, log, logind, logpos,
      depth, depthmarks, orbitgraph, rep, suc, yy, j;
    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.

    orb := o!.orbit;
    i := o!.pos;  # we go on here
    tab := o!.tab;
    nr := Length(orb);

    # We copy a few things to local variables to speed up access:
    looking := o!.looking;
    if looking then 
      lookfunc := o!.lookfunc; 
      found:=o!.found;
      if found<>false then 
        for j in [found+1..nr] do 
          if lookfunc(o,orb[j]) then
            o!.found := j;
            return o;
          fi;
        od;
      elif i=1 and lookfunc(o, orb[1]) then 
      # Maybe we are looking for something and it is the start point:
        o!.found:=1;
        return o;
      fi;
    fi;
    found := false;
    stopper := o!.stopper;
    onlystab := o!.onlystab;
    gens := o!.gens;
    memorygens := o!.memorygens;
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
    log := o!.log;
    if log <> false then
        logind := o!.logind;
        logpos := o!.logpos;
    fi;
    if o!.schreier or o!.log <> false then
        depth := o!.depth;
        depthmarks := o!.depthmarks;
    fi;
    orbitgraph := o!.orbitgraph;

    rep := o!.report;
    while nr <= limit and i <= nr and i <> stopper do
        # Handle depth of Schreier tree:
        if (o!.schreier or o!.log <> false) and i >= depthmarks[depth+1] then
            depth := depth + 1;
            depthmarks[depth+1] := nr+1;
            Info(InfoOrb,3,"Going to depth ",depth);
        fi;

        # Catch the case that we only want the stabilizer:
        if onlystab and o!.stabcomplete then break; fi;

        # Are we logging?
        if log <> false then logind[i] := logpos; suc := false; fi;

        # Now apply generators:
        for j in genstoapply do
            if memorygens then
                yy := orb[i]^(gens[j]!.el);
            else
                yy := orb[i]^gens[j];
            fi;
            if tab[yy] = 0 then
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;
                if orbitgraph <> false then
                    orbitgraph[nr] := EmptyPlist(Length(gens));
                    orbitgraph[i][j] := nr;
                fi;

                # Handle Schreier tree if desired:
                if schreier then
                    o!.schreiergen[nr] := j;
                    o!.schreierpos[nr] := i;
                fi;

                # Handle logging if desired:
                if log <> false then
                    suc := true;
                    log[logpos] := j;
                    log[logpos+1] := nr;
                    logpos := logpos+2;
                    o!.logpos := logpos;  # write back to preserve
                fi;
                
                # Are we looking for something?
                if looking and not found then
                    if lookfunc(o,yy) then
                        found:=true;
                        o!.found := nr;
                    fi;
                fi;

                # Do we have a bound for the orbit length?
                if (orbsizebound <> false and nr >= orbsizebound) then
                    # The orbit is ready, but do we have the stabiliser?
                    # Also only stop early if not log is written.
                    if (permgens = false or o!.stabcomplete) and
                       log = false then
                        o!.pos := i;
                        if o!.schreier or o!.log <> false then
                            o!.depth := depth;
                        fi;
                        SetFilterObj(o,IsClosedOrbit);
                        return o;
                    fi;
                fi;

                # Do we have a bound for the group and a stabiliser?
                if grpsizebound <> false and not o!.stabcomplete then
                    if nr*o!.stabsize*2 > grpsizebound then
                        o!.stabcomplete := true;
                        Info(InfoOrb,3,"Stabilizer complete.");
                    fi;
                fi;

            else
                if orbitgraph <> false then
                    orbitgraph[i][j] := tab[yy];
                fi;
                
                if schreiergenaction <> false and not(o!.stabcomplete) then
                    # Trigger some action usually to produce Schreier generator:
                    if schreiergenaction(o,i,j,tab[yy]) then
                        # o!.stabsize has changed:
                        if stabsizebound <> false and
                           o!.stabsize >= stabsizebound then
                            o!.stabcomplete := true;
                            Info(InfoOrb,3,"Stabilizer complete.");
                        fi;
                        if grpsizebound <> false and 
                           nr*o!.stabsize*2 > grpsizebound then
                            o!.stabcomplete := true;
                            Info(InfoOrb,3,"Stabilizer complete.");
                        fi;
                    fi;
                fi;
            fi;
        od;
        # Now close the log for this point:
        if log <> false then
            if suc then
                log[logpos-2] := -log[logpos-2];
                if looking and found then i:=i+1; break; fi;
                # we only look among new points
            else
                logind[i] := 0;
            fi;
        elif looking and found then 
          i:=i+1; break; 
        fi;
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
    if o!.schreier or o!.log <> false then o!.depth := depth; fi;
    if (orbsizebound <> false and nr >= orbsizebound and log = false) or
       i > nr then
        # Only if not logging!
        SetFilterObj(o,IsClosedOrbit);
        if log <> false then 
            o!.orbind := [1..nr];
        fi;
    fi;
    return o;
end );

InstallMethod( Enumerate, "for an orbit object", [IsOrbit],
  function( o )
    return Enumerate(o,infinity);
  end );
    
InstallMethod( AddGeneratorsToOrbit, "for an orbit and a list of generators",
  [ IsOrbit, IsList ],
  function( o, gens )
    local lmp,oldnrgens;
    oldnrgens := Length(o!.gens);
    Append(o!.gens,gens);
    if IsPermOnIntOrbitRep(o) then
        lmp := LargestMovedPoint(o!.gens);
        if lmp > Length(o!.tab) then
            Append(o!.tab,ListWithIdenticalEntries(lmp-Length(o!.tab),0));
        fi;
    fi;
    ResetFilterObj(o,IsClosedOrbit);
    o!.stopper := o!.pos;
    o!.pos := 1;
    o!.genstoapply := [oldnrgens+1..Length(o!.gens)];
    Enumerate(o);
    if o!.pos <> o!.stopper then
        Error("Unexpected case!");
    fi;
    o!.stopper := false;
    o!.genstoapply := [1..Length(o!.gens)];
    return o;
  end );

InstallMethod( AddGeneratorsToOrbit, "for an orbit, a list of generators, and a list of permutations",
  [ IsOrbit, IsList, IsList ],
  function( o, gens, permgens )
    local lmp,oldnrgens;
    oldnrgens := Length(o!.gens);
    if not(Length(gens) = Length(permgens)) then
        Error("Need two lists of identical length as 2nd and 3rd argument.");
        return fail;
    fi;
    Append(o!.gens,gens);
    ResetFilterObj(o,IsClosedOrbit);
    Append(o!.permgens,permgens);
    Append(o!.permgensi,List(permgens,x->x^-1));
    if IsPermOnIntOrbitRep(o) then
        lmp := LargestMovedPoint(o!.gens);
        if lmp > Length(o!.tab) then
            Append(o!.tab,ListWithIdenticalEntries(lmp-Length(o!.tab),0));
        fi;
    fi;
    o!.stopper := o!.pos;
    o!.pos := 1;
    o!.genstoapply := [oldnrgens+1..Length(o!.gens)];
    Enumerate(o);
    if o!.pos <> o!.stopper then
        Error("Unexpected case!");
    fi;
    o!.stopper := false;
    o!.genstoapply := [1..Length(o!.gens)];
    return o;
  end );

InstallMethod( AddGeneratorsToOrbit, 
  "for a closed hash orbit with log and a list of generators",
  [ IsOrbit and IsHashOrbitRep and IsOrbitWithLog and IsClosedOrbit, IsList ],
  function( o, newgens )
    # We basically reimplement the breadth-first orbit enumeration
    # without looking for something and generating Schreier generators:
    local depth,depthmarks,gen,genabs,gens,genstoapply,ht,i,ii,inneworbit,j,
          log,logind,logpos,memorygens,nr,nr2,nrgens,oldlog,oldlogind,oldnr,
          oldnrgens,op,orb,orbind,pos,pt,rep,schreier,schreiergen,schreierpos,
          storenumbers,suc,yy,gradingfunc,grades,onlygrades,onlygradesdata,
          grade,orbitgraph;

    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.

    # Set a few local variables for faster access:
    orb := o!.orbit;
    ii := 1;  # we start from the beginning here
    nr := Length(orb);
    oldnr := nr;    # retain old orbit length

    # We copy a few things to local variables to speed up access:
    storenumbers := o!.storenumbers;
    op := o!.op;
    oldnrgens := Length(o!.gens);
    o!.gens := Concatenation(o!.gens,newgens);
    gens := o!.gens;
    memorygens := o!.memorygens;
    nrgens := Length(gens);
    ht := o!.ht;
    schreier := o!.schreier;
    if schreier then
        schreiergen := o!.schreiergen;
        schreierpos := o!.schreierpos;
    fi;
    oldlog := o!.log;
    oldlogind := o!.logind;
    log := 0*[1..Length(oldlog)];
    o!.log := log;
    logind := 0*[1..Length(oldlogind)];
    o!.logind := logind;
    logpos := 1;
    orbind := 0*[1..nr];  # this will be at least as long as the old orbit
    orbind[1] := 1;       # the start point
    o!.orbind := orbind;
    inneworbit := BlistList([1..nr],[1]);  # only first point is already there
    grades := o!.grades;
    gradingfunc := o!.gradingfunc;
    onlygrades := o!.onlygrades;
    onlygradesdata := o!.onlygradesdata;
    orbitgraph := o!.orbitgraph;

    rep := o!.report;
    # In the following loop ii is always a position in the new tree
    # and i is its "official" number:
    nr2 := 1;   # this is the counter for the length of the new orbit
    depth := 0; # we are at the beginning
    depthmarks := [2];   # depth 1 starts at new points number 2
    o!.depthmarks := depthmarks;
    while ii <= nr2 do
        if ii >= depthmarks[depth+1] then
            depth := depth + 1;
            depthmarks[depth+1] := nr2+1;
            Info(InfoOrb,3,"Going to depth ",depth);
        fi;
        i := orbind[ii];   # the official number
        logind[i] := logpos;  # note position on log
        suc := false;

        # Is this a new point or an old one?
        if i <= oldnr then # an old point, distinguish between old and new gens
            # First "apply" old generators:
            j := oldlogind[i];
            if j > 0 then   # there have been successful old gens
                repeat
                    gen := oldlog[j];  # negative indicates end
                    genabs := AbsInt(gen);
                    pt := oldlog[j+1];
                    # We know that generator number genabs maps point number i
                    # to point number pt:
                    # But is it perhaps already in the new orbit?
                    if not(inneworbit[pt]) then
                        nr2 := nr2 + 1;
                        orbind[nr2] := pt;
                        if schreier then
                            schreiergen[pt] := genabs;
                            schreierpos[pt] := i;
                        fi;
                        suc := true;
                        log[logpos] := genabs;
                        log[logpos+1] := pt;
                        logpos := logpos+2;
                        # Mark old point to be in new orbit:
                        inneworbit[pt] := true;
                    fi;
                    j := j + 2;
                until gen < 0;
            fi;
            genstoapply := [oldnrgens+1..nrgens];
        else   # one of the new points, apply all generators:
            genstoapply := [1..nrgens];
        fi;
        for j in genstoapply do
            if memorygens then
                yy := op(orb[i],gens[j]!.el);
            else
                yy := op(orb[i],gens[j]);
            fi;
            pos := HTValue(ht,yy);
            if gradingfunc <> false then
                grade := gradingfunc(o,yy);
                if onlygrades <> false and 
                   not(onlygrades(grade,onlygradesdata)) then
                    pos := false;
                fi;
            fi;

            if pos = fail then   # a completely new point
                # Put it into the database:
                nr := nr + 1;
                orb[nr] := yy;
                if grades <> false then
                    grades[nr] := grade;
                fi;
                if storenumbers then
                    HTAdd(ht,yy,nr);
                else
                    HTAdd(ht,yy,true);
                fi;
                if orbitgraph <> false then
                    orbitgraph[nr] := EmptyPlist(Length(gens));
                    orbitgraph[i][j] := nr;
                fi;

                # Now put it into the new orbit:
                nr2 := nr2 + 1;
                orbind[nr2] := nr;
          
                # Handle Schreier tree if desired:
                if schreier then
                    schreiergen[nr] := j;
                    schreierpos[nr] := i;
                fi;
                
                # Handle logging:
                suc := true;
                log[logpos] := j;
                log[logpos+1] := nr;
                logpos := logpos+2;
            elif pos <> false then
                if orbitgraph <> false then
                    orbitgraph[i][j] := pos;
                fi;
                if pos <= oldnr and not(inneworbit[pos]) then
                    # we know this point, is it already in the new orbit?
                    # no, transfer it:
                    nr2 := nr2 + 1;
                    orbind[nr2] := pos;
                    inneworbit[pos] := true;

                    # Handle Schreier tree if desired:
                    if schreier then
                        schreiergen[pos] := j;
                        schreierpos[pos] := i;
                    fi;
                    
                    # Handle logging:
                    suc := true;
                    log[logpos] := j;
                    log[logpos+1] := pos;
                    logpos := logpos+2;
                    # Otherwise: nothing to do (we do not create Schreier gens
                fi;
            fi;
        od;
        # Now close the log for this point:
        if suc then
            log[logpos-2] := -log[logpos-2];
        else
            logind[i] := 0;
        fi;
        ii := ii + 1;
        if rep <> false then
            rep := rep - 1;
            if rep = 0 then
                rep := o!.report;
                Info(InfoOrb,1,"Have ",nr2," points in new orbit.");
            fi;
        fi;
    od;
    o!.pos := nr+1;  # orbit stays closed
    o!.logpos := logpos;
    o!.depth := depth;   # the depth of the tree
    # Close log:
    logind[nr+1] := logpos;
    return o;
end );

InstallMethod( AddGeneratorsToOrbit, 
  "for a closed int orbit with log and a list of generators",
  [ IsOrbit and IsPermOnIntOrbitRep and IsOrbitWithLog and IsClosedOrbit, IsList ],
  function( o, newgens )
    # We basically reimplement the breadth-first orbit enumeration
    # without looking for something and generating Schreier generators:
    local depth,depthmarks,gen,genabs,gens,genstoapply,i,ii,inneworbit,j,log,
          logind,logpos,memorygens,nr,nr2,nrgens,oldlog,oldlogind,oldnr,
          oldnrgens,orb,orbind,pos,pt,rep,schreier,schreiergen,schreierpos,
          suc,tab,yy,orbitgraph;

    # We have lots of local variables because we copy stuff from the
    # record into locals to speed up the access.

    # Set a few local variables for faster access:
    orb := o!.orbit;
    ii := 1;  # we start from the beginning here
    nr := Length(orb);
    oldnr := nr;    # retain old orbit length

    # We copy a few things to local variables to speed up access:
    tab := o!.tab;
    oldnrgens := Length(o!.gens);
    o!.gens := Concatenation(o!.gens,newgens);
    gens := o!.gens;
    memorygens := o!.memorygens;
    nrgens := Length(gens);
    schreier := o!.schreier;
    if schreier then
        schreiergen := o!.schreiergen;
        schreierpos := o!.schreierpos;
    fi;
    oldlog := o!.log;
    oldlogind := o!.logind;
    log := 0*[1..Length(oldlog)];
    o!.log := log;
    logind := 0*[1..Length(oldlogind)];
    o!.logind := logind;
    logpos := 1;
    orbind := 0*[1..nr];  # this will be at least as long as the old orbit
    orbind[1] := 1;       # the start point
    o!.orbind := orbind;
    inneworbit := BlistList([1..nr],[1]);  # only first point is already there
    orbitgraph := o!.orbitgraph;

    rep := o!.report;
    # In the following loop ii is always a position in the new tree
    # and i is its "official" number:
    nr2 := 1;   # this is the counter for the length of the new orbit
    depth := 0; # we are at the beginning
    depthmarks := [2];   # depth 1 starts at new points number 2
    o!.depthmarks := depthmarks;
    while ii <= nr2 do
        if ii >= depthmarks[depth+1] then
            depth := depth + 1;
            depthmarks[depth+1] := nr2+1;
            Info(InfoOrb,3,"Going to depth ",depth);
        fi;
        i := orbind[ii];   # the official number
        logind[i] := logpos;  # note position on log
        suc := false;

        # Is this a new point or an old one?
        if i <= oldnr then # an old point, distinguish between old and new gens
            # First "apply" old generators:
            j := oldlogind[i];
            if j > 0 then   # there have been successful old gens
                repeat
                    gen := oldlog[j];  # negative indicates end
                    genabs := AbsInt(gen);
                    pt := oldlog[j+1];
                    # We know that generator number genabs maps point number i
                    # to point number pt:
                    # But is it perhaps already in the new orbit?
                    if not(inneworbit[pt]) then
                        nr2 := nr2 + 1;
                        orbind[nr2] := pt;
                        if schreier then
                            schreiergen[pt] := genabs;
                            schreierpos[pt] := i;
                        fi;
                        suc := true;
                        log[logpos] := genabs;
                        log[logpos+1] := pt;
                        logpos := logpos+2;
                        # Mark old point to be in new orbit:
                        inneworbit[pt] := true;
                    fi;
                    j := j + 2;
                until gen < 0;
            fi;
            genstoapply := [oldnrgens+1..nrgens];
        else   # one of the new points, apply all generators:
            genstoapply := [1..nrgens];
        fi;
        for j in genstoapply do
            if memorygens then
                yy := orb[i]^(gens[j]!.el);
            else
                yy := orb[i]^gens[j];
            fi;
            if not(IsBound(tab[yy])) or tab[yy] = 0 then
                pos := fail;
            else
                pos := tab[yy];
            fi;
            if pos = fail then   # a completely new point
                # Put it into the database:
                nr := nr + 1;
                orb[nr] := yy;
                tab[yy] := nr;
                if orbitgraph <> false then
                    orbitgraph[nr] := EmptyPlist(Length(gens));
                    orbitgraph[i][j] := nr;
                fi;

                # Now put it into the new orbit:
                nr2 := nr2 + 1;
                orbind[nr2] := nr;
          
                # Handle Schreier tree if desired:
                if schreier then
                    schreiergen[nr] := j;
                    schreierpos[nr] := i;
                fi;
                
                # Handle logging:
                suc := true;
                log[logpos] := j;
                log[logpos+1] := nr;
                logpos := logpos+2;
            else
                if orbitgraph <> false then
                    orbitgraph[i][j] := pos;
                fi;
                
                if pos <= oldnr and not(inneworbit[pos]) then
                    # we know this point, is it already in the new orbit?
                    # no, transfer it:
                    nr2 := nr2 + 1;
                    orbind[nr2] := pos;
                    inneworbit[pos] := true;

                    # Handle Schreier tree if desired:
                    if schreier then
                        schreiergen[pos] := j;
                        schreierpos[pos] := i;
                    fi;
                    
                    # Handle logging:
                    suc := true;
                    log[logpos] := j;
                    log[logpos+1] := pos;
                    logpos := logpos+2;
                    # Otherwise: nothing to do (we do not create Schreier gens
                fi;
            fi;
        od;
        # Now close the log for this point:
        if suc then
            log[logpos-2] := -log[logpos-2];
        else
            logind[i] := 0;
        fi;
        ii := ii + 1;
        if rep <> false then
            rep := rep - 1;
            if rep = 0 then
                rep := o!.report;
                Info(InfoOrb,1,"Have ",nr2," points in new orbit.");
            fi;
        fi;
    od;
    o!.pos := nr+1;  # orbit stays closed
    o!.logpos := logpos;
    o!.depth := depth;   # the depth of the tree
    # Close log:
    logind[nr+1] := logpos;
    return o;
end );

InstallMethod( MakeSchreierTreeShallow, "for a closed orbit with log",
  [ IsOrbit and IsClosedOrbit and IsOrbitWithLog, IsPosInt ],
  function( o, l )
    local i,w,x,tries,nr;
    if not(o!.schreier) then
        Error("Orbit has no Schreier tree");
        return;
    fi;
    if Length(o) < 20 then 
        Info(InfoOrb,2,"Very small orbit, doing nothing.");
        return;
    fi;
    tries := 1;
    while o!.depth > l and tries <= 3 do
        tries := tries + 1;
        x := [];
        nr := QuoInt(o!.depth,l)+1;
        if nr > 9 then 
            nr := 9;
        elif nr < 3 then
            nr := 3;
        fi;
        for i in [1..nr] do
            w := TraceSchreierTreeForward(o,
                          o!.orbind[Random(2,QuoInt(Length(o),2))]);
            Add(x,Product(o!.gens{w}));
        od;
        Info(InfoOrb,2,"Adding ",Length(x),
             " new generators to decrease depth...");
        AddGeneratorsToOrbit(o,x);
        Info(InfoOrb,2,"Depth is now ",o!.depth);
    od;
    if tries > 3 then
        Info(InfoOrb,1,"Giving up, Schreier tree is not shallow.");
    fi;
  end );
            
InstallMethod( MakeSchreierTreeShallow, "for a closed orbit with log",
  [ IsOrbit and IsClosedOrbit and IsOrbitWithLog ],
  function( o )
    MakeSchreierTreeShallow(o, LogInt(Length(o),2) );
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

InstallMethod( DepthOfSchreierTree, "for an orbit",
  [IsOrbit],
  function( o )
    if not(o!.schreier) then
        Error("Orbit does not have a Schreier tree");
        return fail;
    else
        return o!.depth;
    fi;
  end );

InstallMethod( Grades, "for a graded orbit",
  [IsOrbit and IsGradedOrbit],
  function( o )
    return o!.grades;
  end );

InstallMethod( Grades, "for an orbit",
  [IsOrbit],
  function( o )
    if o!.grades = false then
        Error("Orbit does not have a grading");
        return fail;
    else
        return o!.grades;
    fi;
  end );

InstallMethod( ConstantTimeAccessList, "for an orb orbit",
  [IsOrbit],
  function( o )
    return ShallowCopy( o!.orbit );
  end );

InstallMethod( Enumerator, "for an orb orbit",
  [IsOrbit],
  function( o )
    return ShallowCopy( o!.orbit );
  end );

InstallMethod( AsList, "for an orb orbit",
  [IsOrbit],
  function( o )
    return ShallowCopy( o!.orbit );
  end );

InstallMethod( UnderlyingPlist, "for an orb orbit",
  [IsOrbit],
  function( o )
    return o!.orbit;
  end );

InstallMethod( Iterator, "for an orb orbit",
  [IsOrbit],
  function( o )
    return IteratorList( Immutable( o!.orbit ) );
  end );

InstallMethod( OrbitGraphAsSets, "for an orbit",
  [IsOrbit],
  function(o)
    return List(OrbitGraph(o),Set);
  end );

InstallMethod( OrbitGraph, "for an orbit",
  [IsOrbit],
  function( o )
    local g,gg,pos,i,j;
    if o!.orbitgraph = false then
        g := EmptyPlist(Length(o));
        for i in [1..Length(o)] do
            gg := EmptyPlist(Length(o!.gens));
            for j in [1..Length(o!.gens)] do
                pos := Position(o,o!.op(o[i],o!.gens[j]));
                if pos <> fail then 
                    gg[j] := pos; 
                fi;
            od;
            Add(g,gg);
        od;
        o!.orbitgraph := g;
        return g;
    else
        return o!.orbitgraph;
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
  [ IsOrbit and IsPermOnIntOrbitRep and IsClosedOrbit, IsList ],
  function( o, gens )
    local res,i;
    res := [];
    if o!.memorygens then
        for i in [1..Length(gens)] do
          Add(res,PermList(o!.tab{OnTuples(o!.orbit,gens[i]!.el)}));
        od;
    else
        for i in [1..Length(gens)] do
          Add(res,PermList(o!.tab{OnTuples(o!.orbit,gens[i])}));
        od;
    fi;
    return res;
  end );
    
InstallMethod( ActionOnOrbit, 
  "for a closed orbit with numbers and a list of elements",
  [ IsOrbit and IsHashOrbitRep and IsClosedOrbit, IsList ],
  function( o, gens )
    local res,i;
    if not(o!.storenumbers) then
        return ORB_ActionOnOrbitIntermediateHash(o,gens);
    fi;
    res := [];
    if o!.memorygens then
        for i in [1..Length(gens)] do
          Add(res,PermList(
           List([1..Length(o!.orbit)],
                j->HTValue(o!.ht,o!.op(o!.orbit[j],gens[i]!.el)))));
        od;
    else
        for i in [1..Length(gens)] do
          Add(res,PermList(
           List([1..Length(o!.orbit)],
                j->HTValue(o!.ht,o!.op(o!.orbit[j],gens[i])))));
        od;
    fi;
    return res;
  end );
 
InstallGlobalFunction( ORB_ActionOnOrbitIntermediateHash,
  function( o, gens )
    local ht,i,res;
    ht := HTCreate( o!.orbit[1], rec( hashlen := Length(o!.orbit)*2+1 ) );
    for i in [1..Length(o!.orbit)] do
        HTAdd(ht,o!.orbit[i],i);
    od;
    res := [];
    if o!.memorygens then
        for i in [1..Length(gens)] do
          Add(res,PermList(
           List([1..Length(o!.orbit)],j->HTValue(ht,
                                      o!.op(o!.orbit[j],gens[i]!.el)))));
        od;
    else
        for i in [1..Length(gens)] do
          Add(res,PermList(
           List([1..Length(o!.orbit)],j->HTValue(ht,
                                      o!.op(o!.orbit[j],gens[i])))));
        od;
    fi;
    return res;
  end );

InstallGlobalFunction( "ORB_ActionHomMapper",
  function(data,el)
    return ActionOnOrbit(data.orb,[el])[1];
  end );

InstallMethod( OrbActionHomomorphism, "for a closed orbit",
  [IsGroup, IsOrbit and IsClosedOrbit],
  function(g,orb)
    local data,h,hom,newgens;
    if IsHashOrbitRep(orb) and not(orb!.storenumbers) then
        Info(InfoWarning,1,"This OrbActionHomomorphism will not be efficient,",
             " use \"storenumbers\"!");
    fi;
    newgens := ActionOnOrbit(orb,GeneratorsOfGroup(g));
    h := GroupWithGenerators(newgens);
    data := rec( orb := orb );
    hom := GroupHomByFuncWithData(g,h,ORB_ActionHomMapper,data);
    return hom;
  end );


InstallMethod( ForgetMemory, "for an orbit object",
  [ IsOrbit ],
  function( o )
    if o!.memorygens then
        ForgetMemory(o!.gens);
        o!.memorygens := false;
    fi;
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
# A generic way to find out about the memory needed by an object:
#######################################################################

InstallMethod( Memory, "fallback method returning fail",
  [IsObject], function( ob ) return fail; end );


#######################################################################
# Things to work with suborbits:
#######################################################################

InstallOtherMethod( FindSuborbits, "without args", [ ],
  function()
    Error("Usage: FindSuborbits( o, subgens [,op] [,nrsuborbits] )");
  end );

InstallMethod( FindSuborbits, "for an orbit, and subgroup gens",
  [ IsOrbit, IsList ],
  function( o, subgens) return FindSuborbits(o,subgens,o!.op,1); end );

InstallMethod( FindSuborbits, "for an orbit, subgroup gens and an action",
  [ IsOrbit, IsList, IsFunction ],
  function( o, subgens, op) return FindSuborbits(o,subgens,op,1); end );

InstallMethod( FindSuborbits, "for an orbit, subgroup gens and a limit",
  [ IsOrbit, IsList, IsCyclotomic ],
  function( o, subgens, limit) 
    return FindSuborbits(o,subgens,o!.op,limit); 
  end);

InstallMethod( FindSuborbits, "for an orbit, subgroup gens, and limit",
  [ IsOrbit, IsList, IsFunction, IsCyclotomic ],
  function( o, subgens, op, nrsubs )
    local fusetrupps,gensi,h,i,j,l,len,min,nr,nrtrupps,res,succ,
          tried,truppnr,truppst,v,wo,x;

    if not(IsClosedOrbit(o)) then
        Error("Orbit must be closed");
        return fail;
    fi;
    len := Length(o);

    succ := 0*[1..len];
    truppnr := 1*[1..len];
    truppst := 1*[1..len];
    nrtrupps := len;

    fusetrupps := function(i,j)
      local dummy,lastx,truppi,truppj,x;
      truppi := truppnr[i];
      truppj := truppnr[j];
      if truppi = truppj then
          return;
      fi;
      if truppj < truppi then
          dummy := i; i := j; j := dummy;
          dummy := truppi; truppi := truppj; truppj := dummy;
      fi;
      x := truppst[truppj];
      repeat
          truppnr[x] := truppi;
          lastx := x;
          x := succ[x];
      until x = 0;
      succ[lastx] := truppst[truppi];
      truppst[truppi] := truppst[truppj];
      Unbind(truppst[truppj]);
      nrtrupps := nrtrupps - 1;
      if nrtrupps mod 100000 = 0 then
          Info(InfoOrb,2,nrtrupps," trupps left.\n");
      fi;
      return;
    end;

    tried := 0;
    i := 1;
    while i <= len and nrtrupps > nrsubs do
        for h in subgens do
            j := Position(o,op(o[i],h));
            fusetrupps(i,j);
        od;
        tried := tried + 1;
        if tried mod 100000 = 0 then
            Info(InfoOrb,2,"Have tried ",tried," out of ",len," points.\n");
        fi;
        i := i + 1;
    od;

    Info(InfoOrb,1,"Have suborbits, compiling result record...");

    # Now return the result:
    truppst := Compacted(truppst);
    res := rec(
        o := o,
        nrsuborbits := nrtrupps,
        reps := 0*[1..nrtrupps],
        words := 0*[1..nrtrupps],
        lens := 0*[1..nrtrupps],
        suborbnr := 0*[1..len],
        suborbs := 0*[1..nrtrupps],
        conjsuborbit := 0*[1..nrtrupps],
        issuborbitrecord := true,
    );

    for i in [1..nrtrupps] do
        nr := 1;
        x := truppst[i];
        min := x;
        l := [x];
        res.suborbnr[x] := i;
        while succ[x] <> 0 do
            x := succ[x];
            res.suborbnr[x] := i;
            Add(l,x);
            if x < min then min := x; fi;
            nr := nr + 1;
        od;
        res.reps[i] := min;
        if o!.schreier then
            res.words[i] := TraceSchreierTreeForward(o,min);
        fi;
        res.lens[i] := nr;
        Sort(l);
        res.suborbs[i] := l;
    od;
    
    # Now provide information about conjugate suborbits:
    if o!.schreier and IsIdenticalObj(op,o!.op) then
        Info(InfoOrb,1,"Computing conjugate suborbits...");
        gensi := List(o!.gens,x->x^-1);
        for i in [1..nrtrupps] do
            v := o[1];
            wo := res.words[i];
            for x in [Length(wo),Length(wo)-1..1] do
                v := o!.op(v,gensi[wo[x]]);
            od;
            res.conjsuborbit[i] := res.suborbnr[Position(o,v)];
        od;
    fi;
    
    return res;
  end ); 

InstallMethod( OrbitIntersectionMatrix, "for a record, and a group element",
  [ IsRecord, IsObject ], 
  function( r, el )
    # this computes |O_i g_l \cap O_j| where
    # Orbit = \bigcup_{i=1}^k O_i     disjoint union
    # and g_l maps the first point of O_1 into O_l for l=1,..,k
    local i,j,k,len,m,o,v,y;
    len := r.nrsuborbits;
    o := r.o;
    m := List([1..len],i->0*[1..len]);
    for i in [1..len] do
        v := m[i];
        for j in r.suborbs[i] do
            y := o!.op(o[j],el);
            k := r!.suborbnr[Position(o,y)];
            v[k] := v[k] + 1;
        od;
    od;
    return m;
  end );

InstallMethod( RegularRepresentationSchurBasisElm,
  "for a record, a list of inverses of double coset reps, a positive number",
  [ IsRecord, IsList, IsPosInt ],
  function( r, reps, j )
    local i,l,len,m,o,suborbj,x,y;
    j := r.conjsuborbit[j];
    len := r.nrsuborbits;
    o := r.o;
    m := List([1..len],i->0*[1..len]);
    suborbj := r.suborbs[j];
    for l in [1..len] do
        for x in suborbj do
            y := o!.op(o[x],reps[r.conjsuborbit[l]]);
            i := r!.suborbnr[Position(o,y)];
            m[i][l] := m[i][l]+1;
        od;
    od;
    return m;
  end );


#######################################################################
# The following lays down an infrastructure for group size estimation
# which can be used by other packages to plug into:
#######################################################################

InstallMethod( SizeMC, "one-argument fallback",
  [ IsGroup ],
  function( G )
    return SizeMC(G,1/20);
  end );

InstallOtherMethod( SizeMC, "zero-argument usage info",
  [ ],
  function()
    Print("Usage: SizeMC( <group> [,<errorprobability>] )\n");
    Print("       uses a Monte Carlo method to determine the size\n");
    Print("       result cannot be too large, but can be too small\n");
    Print("       with error probability bounded by the 2nd arg.\n");
    return fail;
  end );

InstallMethod( SizeMC, "high ranked method for groups that know the size",
  [ IsGroup and HasSize, IsRat ], SUM_FLAGS,
  function( G, err )
    return Size(G);
  end );

InstallMethod( SizeMC, "standard method for permutation groups",
  [ IsGroup and IsPermGroup, IsRat ],
  function( G, err )
    local S,rand,H;
    rand := QuoInt( (1-err)*1000, 1000 );
    H := Group(GeneratorsOfGroup(G));
    S := StabChain( H, rec( random := rand ) );
    return SizeStabChain(S);
  end );

if not(IsBound(MACFLOAT_INT)) then
  MACFLOAT_INT := FLOAT_INT;
fi;

if not(IsBound(SQRT_MACFLOAT)) then
  SQRT_MACFLOAT := function(x)
    return EXP_FLOAT(LOG_FLOAT(x)/FLOAT_INT(2));
  end;
fi;

## Orbit size estimation using the birthday principle
InstallGlobalFunction( ORB_EstimateOrbitSize,
  function( pr, pt, op, L, limit, timeout )
    local starttime, endtime, ht, tries, coinc, grpcoinc, S, x, ptx,
          elm, estimate, upper, lower;
    starttime := Runtime();
    endtime := starttime + timeout;
    ht := HTCreate( pt, 
               rec( hashlen := NextPrimeInt( Minimum( 100000, limit ) ) ) );
    tries := 0;
    coinc := 0;
    grpcoinc := 0;
    S := [];
    while tries <= 3 * limit and coinc < L do
      if ( tries mod 100 = 1 ) and
         Runtime() >= endtime then
        # check every 100 random elements if we have exceeded the
        # maximum alloted time. If so, bail out.
        Info( InfoOrb, 4, "Time limit exceeded." );
        return fail;
      fi;

      x := Next( pr );
      ptx := op( pt, x );
      elm := HTValue( ht, ptx );
      if elm = fail then
        # new
        HTAdd( ht, ptx, x );
      else
        # coincidence
        coinc := coinc + 1;
        # record stabilizer element
        if elm = x then
          # we have found a coincidence in the group
          grpcoinc := grpcoinc + 1;
        else
          Add( S, elm * x^-1 );
        fi;
      fi;
      tries := tries + 1;
      if tries > limit and coinc < QuoInt( L, 3 ) then
        # if we don't find at least a third of the necessary coincidences
        # within the maximum number of tries, we bail out.
        # Otherwise we allow three times the limit, to find the remaining
        # coincidences.
        Info( InfoOrb, 4, "Too few coincidences found within limit." );
        return fail;
      fi;
    od;
    if coinc = L then
      estimate := QuoInt( tries^2, 2*L );
      Info( InfoOrb, 4, "Estimated orbit size is ", estimate, ".");
      upper := Int( 2*tries^2 / (-MACFLOAT_INT( 196 )/100 +
                    SQRT_MACFLOAT( MACFLOAT_INT( 4*L - 1 ) ) )^2
                    + MACFLOAT_INT(1)/2 );
      lower := Int( 2*tries^2 / ( MACFLOAT_INT( 196 )/ 100 +
               SQRT_MACFLOAT( MACFLOAT_INT( 4*L - 1 ) ) )^2 );
      Info( InfoOrb, 4, "Confidence interval with error 5% is [ ",
               lower, ", ", upper," ]." );
        return rec( estimate := estimate,
                    lowerbound := lower,
                    upperbound := upper,
                    Sgens := S,
                    grpcoinc := grpcoinc );
      #fi;
    else
      return fail;
    fi;
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

if IsBound( MappingPermSetSet_C ) then
    InstallGlobalFunction( MappingPermSetSet, MappingPermSetSet_C );
else
    InstallGlobalFunction( MappingPermSetSet,
      function(src, dst)
        local l, d, out, i, j, next, k;
        l:=Length(src);
        if l <> Length(dst) then
            Error("both arguments must be lists of the same length");
            return;
        fi;
        d:=Maximum(src[l], dst[l]);
        out:=EmptyPlist(d);

        i:=1;
        j:=1;
        next:=1;   # the next candidate, possibly prevented from being in dst

        for k in [1..d] do
          if i<=l and k=src[i] then
            out[k]:=dst[i];
            i:=i+1;
          else
            # Skip things in dst:
            while j<=l and next>=dst[j] do
              if next = dst[j] then next:=next+1; fi;
              j:=j+1;
            od;
            out[k]:=next;
            next:=next+1;
          fi;
        od;

        return PermList(out);
      end ); 
fi;


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
