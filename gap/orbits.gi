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
    # Now get rid of the group object if necessary but preserve known size:
    if IsGroup(gens) then
        if HasSize(gens) then
            opt.grpsize := Size(gens);
            gens := GeneratorsOfGroup(gens);
        fi;
    fi;
    # Now set some default options:
    if not IsBound( opt.makeperms ) then opt.makeperms := false; fi;
    if not IsBound( opt.makestab ) then opt.makestab := false; fi;
    
    # Now take this record as our orbit record and return:
    o := opt;
    o.gens := gens;
    o.nrgens := Length(gens);
    o.op := op;
    o.ht := NewHT(x,hashlen);
    o.orbit := [x];
    if o.makeperms then o.perms := List(gens,v->[]); else o.perms := fail; fi;
    o.pos := 1;
    AddHT(o.ht,x,true);
    Objectify( OrbitsType, o );
    return o;
end );

InstallMethod( ViewObj, "for an orbit", [IsOrbit],
  function( o )
    Print("<");
    if IsReady(o) then Print("closed"); else Print("open"); fi;
    Print(" orbit with ", Length(o!.orbit), " points>");
  end );

InstallMethod( Enumerate, "for an orbit and a limit", [IsOrbit, IsCyclotomic],
  function( o, limit )
    local i,j,orb,perms,pos,yy;
    i := o!.pos;  # we go on here
    orb := o!.orbit;
    perms := o!.perms;
    if IsBound(o!.maxsize) and o!.maxsize < limit then limit := o!.maxsize; fi;
    while Length(orb) <= limit and i <= Length(orb) do
        for j in [1..o!.nrgens] do
            yy := o!.op(orb[i],o!.gens[j]);
            pos := ValueHT(o!.ht,yy);
            if pos = fail then
                Add(orb,yy);
                AddHT(o!.ht,yy,Length(orb));
                if perms <> fail then Add(perms[j],Length(orb)); fi;
            else
                if perms <> fail then Add(perms[j],pos); fi;
            fi;
        od;
        i := i + 1;
    od;
    o!.pos := i;
    if i > Length(orb) or IsBound(o!.maxsize) and Length(orb) >= o!.maxsize then
        SetFilterObj(o,IsReady);
    fi;
    return o;
end );


