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

InstallValue( OrbitsType, NewType( OrbitsFamily, IsOrbit ) );

InstallGlobalFunction( InitOrbit, 
  function( arg )
    # First parse the arguments:
    local gens,hashlen,o,op,opt,x;
    if Length(arg) = 4 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := rec();
    elif Length(arg) = 5 then
        gens := arg[1]; x := arg[2]; op := arg[3]; hashlen := arg[4];
        opt := arg[5];
    else
        Print("Usage: InitOrbit( gens, point, action, hashlen [,options] )\n");
        return;
    fi;
    # We make a copy:
    o := ShallowCopy(opt);
    # Now get rid of the group object if necessary but preserve known size:
    if IsGroup(gens) then
        if HasSize(gens) then
            o.grpsize := Size(gens);
            if not IsBound(o.maxsize) then
                o.maxsize := o.grpsize;
            fi;
        fi;
        gens := GeneratorsOfGroup(gens);
    fi;
    # Now set some default options:
    if not IsBound( o.permgens ) then 
        o.stab := fail; 
    else
        o.stab := Group(o.permgens[1]^0);
    fi;
    if IsBound(o.lookingfor) then
        if o.lookingfor <> fail then 
            if not (IsList(o.lookingfor) or IsRecord(o.lookingfor) or
                    IsFunction(o.lookingfor)) then
                Error("opt.lookingfor must be a list or a hash table or a",
                      " function");
            fi;
            o.found := false; 
        fi;
        o.schreiergen := true;
    else
        o.lookingfor := fail;
    fi;
    if not IsBound( o.schreiergen ) then 
        o.schreiergen := fail; 
        o.schreierpos := fail;
    else
        o.schreiergen := [fail];
        o.schreierpos := [fail];
    fi;
    
    # Now take this record as our orbit record and return:
    o.gens := gens;
    o.nrgens := Length(gens);
    o.op := op;
    o.ht := NewHT(x,hashlen);
    o.orbit := [x];
    o.pos := 1;
    AddHT(o.ht,x,true);
    Objectify( OrbitsType, o );
    return o;
end );

InstallMethod( ViewObj, "for an orbit", [IsOrbit],
  function( o )
    Print("<");
    if IsClosed(o) then Print("closed"); else Print("open"); fi;
    Print(" orbit with ", Length(o!.orbit), " points");
    if o!.stab <> fail then
        Print(" with stabilizer");
    fi;
    if o!.schreiergen <> fail then
        Print(" with Schreier tree");
    fi;
    if o!.lookingfor <> fail then
        Print(" looking for something");
    fi;
    Print(">");
  end );

InstallMethod( Enumerate, "for an orbit and a limit", [IsOrbit, IsCyclotomic],
  function( o, limit )
    local i,j,orb,pos,yy;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    if IsBound(o!.maxsize) and o!.maxsize < limit then limit := o!.maxsize; fi;
    while Length(orb) <= limit and i <= Length(orb) do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            pos := ValueHT(o!.ht,yy);
            if pos = fail then
                Add(orb,yy);
                AddHT(o!.ht,yy,Length(orb));
                if o!.schreiergen <> fail then
                    Add(o!.schreiergen,j);
                    Add(o!.schreierpos,i);
                    if o!.lookingfor <> fail then
                        if IsList(o!.lookingfor) then
                            pos := Position(o!.lookingfor,yy);
                            if pos <> fail then
                                o!.pos := i;
                                o!.found := Length(orb);
                                return o;
                            fi;
                        elif IsFunction(o!.lookingfor) then
                            pos := o!.lookingfor(yy);
                            if pos = true then
                                o!.pos := i;
                                o!.found := Length(orb);
                                return o;
                            fi;
                        else  # a hash table
                            pos := ValueHT(o!.lookingfor,yy);
                            if pos <> fail then
                                o!.pos := i;
                                o!.found := Length(orb);
                                return o;
                            fi;
                        fi;
                    fi;
                fi;
            else
                if o!.stab <> fail then
                    # Calculate an element of the stabilizer:
                    Error("not yet implemented");
                fi;
            fi;
        od;
        i := i + 1;
    od;
    o!.pos := i;
    if i > Length(orb) or IsBound(o!.maxsize) and Length(orb) >= o!.maxsize then
        SetFilterObj(o,IsClosed);
    fi;
    return o;
end );

InstallMethod( TraceSchreierTree, "for an orbit and a position",
  [ IsOrbit, IsPosInt ],
  function( o, pos )
    local word;
    word := [];
    while pos > 1 do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    return Reversed(word);
  end );


