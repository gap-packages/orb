#############################################################################
##
##  search.gi           orb package 
##                                                        by Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Implementation stuff for searching in groups.
##
#############################################################################

# Random sources:

InstallValue(RandomSourceType, NewType(RandomSourcesFamily, IsRandomSource));

InstallMethod( RandomSource, "the global random source", [IsString],
  function( type )
    local r;
    if type <> "global" then TryNextMethod(); fi;
    # Return the global random source:
    r := rec(origstate := StateRandom()); 
    Objectify(RandomSourceType,r);
    SetFilterObj(r,IsGlobalRandomSourceRep);
    return r;
  end );

InstallMethod( Reset, "for the global random source",
  [IsRandomSource and IsGlobalRandomSourceRep],
  function( r )
    RestoreStateRandom( r!.origstate );
  end );

InstallMethod( Random, 
  "for a random source using the global source and two integers",
  [IsRandomSource and IsGlobalRandomSourceRep, IsInt, IsInt],
  function( rs, from, to )
    return Random(from,to);
  end );

InstallMethod( Random, 
  "for a random source using the global source and a list",
  [IsRandomSource and IsGlobalRandomSourceRep, IsList],
  function( rs, list )
    return Random(list);
  end );

InstallMethod( ViewObj, "for a random source using the global source",
  [IsRandomSource and IsGlobalRandomSourceRep],
  function(rs)
    Print("<the global random source>");
  end );

InstallMethod( ViewObj, "for a random source",
  [IsRandomSource],
  function(rs)
    Print("<a random source>");
  end );

# Random vectors and matrices:

InstallMethod( Randomize, "for a compressed GF2 vector",
  [ IsGF2VectorRep and IsMutable ],
  function( v )
    local i;
    MultRowVector(v,0*Z(2));
    for i in [1..Length(v)] do
      if Random(0,1) = 1 then v[i] := Z(2); fi;
    od;
    return v;
  end );

InstallMethod( Randomize, 
  "for a compressed GF2 vector and a random source",
  [ IsGF2VectorRep and IsMutable, IsRandomSource ],
  function( v, rs )
    local i;
    MultRowVector(v,0*Z(2));
    for i in [1..Length(v)] do
      if Random(rs,0,1) = 1 then v[i] := Z(2); fi;
    od;
    return v;
  end );

InstallMethod( Randomize, "for a compressed 8bit vector",
  [ Is8BitVectorRep and IsMutable ],
  function( v )
    local q,i,z,r;
    q := BaseField(v);
    z := PrimitiveRoot(q);
    q := Size(q);
    MultRowVector(v,0*z);
    for i in [1..Length(v)] do
        r := Random(0,q-1);
        if r <> 0 then v[i] := z^r; fi;
    od;
    return v;
  end );

InstallMethod( Randomize, 
  "for a compressed 8bit vector and a random source",
  [ Is8BitVectorRep and IsMutable, IsRandomSource ],
  function( v, rs )
    local q,i,z,r;
    q := BaseField(v);
    z := PrimitiveRoot(q);
    q := Size(q);
    MultRowVector(v,0*z);
    for i in [1..Length(v)] do
        r := Random(0,q-1);
        if r <> 0 then v[i] := z^r; fi;
    od;
    return v;
  end );

InstallMethod( Randomize,
  "for a compressed gf2 matrix",
  [ IsGF2MatrixRep and IsMutable ],
  function( m )
    local v;
    for v in m do Randomize(v); od;
    return m;
  end );

InstallMethod( Randomize,
  "for a compressed gf2 matrix and a random source",
  [ IsGF2MatrixRep and IsMutable, IsRandomSource ],
  function( m, rs )
    local v;
    for v in m do Randomize(v,rs); od;
    return m;
  end );

InstallMethod( Randomize,
  "for a compressed 8bit matrix",
  [ Is8BitMatrixRep and IsMutable ],
  function( m )
    local v;
    for v in m do Randomize(v); od;
    return m;
  end );

InstallMethod( Randomize,
  "for a compressed 8bit matrix and a random source",
  [ Is8BitMatrixRep and IsMutable, IsRandomSource ],
  function( m, rs )
    local v;
    for v in m do Randomize(v,rs); od;
    return m;
  end );

# Product replacers:

InstallValue( ProductReplacersType, 
   NewType( ProductReplacersFamily, IsProductReplacer ) );

InstallMethod( ProductReplacer, "for a list of group generators",
  [IsList], function( gens )
    return ProductReplacer( gens, rec( ) );
  end );

InstallMethod( ProductReplacer, 
  "for a list of group generators and an options record",
  [IsList, IsRecord],
  function( gens, opt )
    # First add some default options if not set:
    local pr;
    pr := ShallowCopy(opt);
    if not IsBound(pr.randomsource) then
        pr.randomsource := RandomSource("global");
    fi;
    if not IsBound(pr.scramble) then 
        pr.scramble := 100; 
    fi;
    if not IsBound(pr.scramblefactor) then 
        pr.scramblefactor := 10;
    fi;
    if not IsBound(pr.addslots) then
        pr.addslots := 10;
    fi;
    if not IsBound(pr.maxdepth) then
        pr.maxdepth := infinity;
    fi;
    if Length(gens) = 0 then
        Error("Need at least one generator");
        return;
    fi;
    if CanEasilySortElementsFamily(FamilyObj(gens[1])) then
        pr.gens := Set(gens);
    else
        pr.gens := gens;
    fi;
    pr.nrgens := Length(pr.gens);
    pr.slots := pr.nrgens + pr.addslots;
    pr.initialized := false;
    pr.steps := 0;
    Objectify(ProductReplacersType,pr);
    Reset(pr);
    return pr;
  end );

InstallMethod( Reset, "for a product replacer", [IsProductReplacer],
  function(pr)
    pr!.state := ShallowCopy(pr!.gens);
    while Length(pr!.state) < pr!.slots do
        Add(pr!.state,pr!.gens[Random(pr!.randomsource,1,pr!.nrgens)]);
    od;
    pr!.initialized := false;
    pr!.steps := 0;
  end );

InstallMethod( Next, "for a product replacer", [IsProductReplacer],
  function(pr)
    local OneStep,i;
    OneStep := function(pr)
        local a,b,c;
        pr!.steps := pr!.steps + 1;
        a := Random(pr!.randomsource,1,pr!.slots);
        b := Random(pr!.randomsource,1,pr!.slots-1);
        if b >= a then b := b + 1; fi;
        c := Random(pr!.randomsource,1,2);
        if c = 1 then
            pr!.state[a] := pr!.state[a] * pr!.state[b];
            return pr!.state[a];
        else
            pr!.state[b] := pr!.state[a] * pr!.state[b];
            return pr!.state[b];
        fi;
    end;
    if pr!.steps > pr!.maxdepth then Reset(pr); fi;
    if not(pr!.initialized) then
        for i in [1..Maximum(pr!.nrgens * pr!.scramblefactor, pr!.scramble)] do
            OneStep(pr);
        od;
        pr!.initialized := true;
    fi;
    return OneStep(pr);
  end );

InstallMethod( ViewObj, "for a product replacer", [IsProductReplacer],
  function(pr)
    Print("<product replacer nrgens=",pr!.nrgens," slots=",pr!.slots,
          " scramble=",Maximum(pr!.nrgens*pr!.scramblefactor,pr!.scramble),
          " maxdepth=",pr!.maxdepth," steps=",pr!.steps,">");
  end );


# Finding things in groups, random searchers:

InstallValue( RandomSearchersType, 
              NewType( RandomSearchersFamily, IsRandomSearcher ) );

InstallMethod( RandomSearcher, 
  "for a list of generators and a test function", 
  [IsList, IsFunction],
  function( gens, testfunc )
    return RandomSearcher( gens, testfunc, rec() );
  end );

InstallMethod( RandomSearcher,
  "for a list of generators, a test function, and an options record",
  [IsList, IsFunction, IsRecord],
  function( gens, testfunc, opt )
    # Set some default options:
    local r;
    r := ShallowCopy(opt); 
    if not IsBound(r.exceptions) then
        r.exceptions := [];
    fi;
    if not IsBound(r.maxdepth) then
        r.maxdepth := 4 * Maximum(100,10*Length(gens));
    fi;
    if not IsBound(r.addslots) then
        r.addslots := 10;
    fi;
    r.stops := 0;
    r.tries := 0;
    r.testfunc := testfunc;
    if IsBound( r.scramble ) then
        r.productreplacer := ProductReplacer( gens,
              rec( maxdepth := r.maxdepth,
                   scramble := 100,
                   scramblefactor := 10,
                   addslots := r.addslots ) );
              # this is the standard PseudoRandom behaviour
    else  # the standard case using all random elements:
        r.productreplacer := ProductReplacer( gens,
              rec( maxdepth := r.maxdepth,
                   scramble := 0,
                   scramblefactor := 0,
                   addslots := r.addslots ) );
              # this uses all random elements from the first on
    fi;
    Objectify( RandomSearchersType,r );
    return r;
  end );

InstallMethod( ViewObj, "for a random searcher", [IsRandomSearcher],
  function( rs )
    Print("<random searcher");
    if IsBound(rs!.target) then
        Print(" for ",rs!.target);
    fi;
    Print(", tries: ",rs!.tries," stops: ",rs!.stops,", exceptions: ",
          Length(rs!.exceptions),">");
  end );

InstallMethod( Search, "for a random searcher", [IsRandomSearcher],
  function( rs )
    local x,y;
    repeat
        repeat
            x := Next( rs!.productreplacer );
            rs!.tries := rs!.tries + 1;
            if rs!.tries mod 10000 = 0 then
                Info(InfoOrb,2,"Tried ",rs!.tries," elements.");
            fi;
            if IsObjWithMemory(x) then
                y := x!.el;
            else
                y := x;
            fi;
        until not( y in rs!.exceptions );
    until rs!.testfunc(y);
    rs!.stops := rs!.stops + 1;
    Add(rs!.exceptions,y);
    return x;
  end );


###################################################
# Involution centralisers and the dihedral trick: #
###################################################

InstallGlobalFunction( FindInvolution,
function(pr)
  # g a matrix group
  local i,o,x;
  for i in [1..1000] do
      x := Next(pr);
      o := Order(StripMemory(x));
      if o mod 2 = 0 then
          return x^(o/2);
      fi;
  od;
  return fail;
end );

InstallGlobalFunction( FindCentralisingElementOfInvolution,
function(pr,x)
  # pr a product replacer for G, x an involution in G
  local o,r,y,z;
  r := Next(pr);
  y := x^r;
  # Now x and y generate a dihedral group
  if x=y then return r; fi;
  z := x*y;
  o := Order(StripMemory(z));
  if IsEvenInt(o) then
      return z^(o/2);
  else
      return z^((o+1)/2)*r^(-1);
  fi;
end );

InstallGlobalFunction( FindInvolutionCentraliser,
function(pr,x,n)
  # pr a product replacer for G, x an involution in G, n an integer
  local i,l,y;
  l := [];
  for i in [1..n] do   # find 20 generators of the centraliser
      y := FindCentralisingElementOfInvolution(pr,x);
      AddSet(l,y);
  od;
  return l;
end );

InstallGlobalFunction( ReduceNumberOfGeneratorsUsingRecog,
function(l,Sizerecogniser)
  local drop,i,ll,ri,ri2,si,wholegroup;
  wholegroup := Group(StripMemory(l));
  ri := Sizerecogniser(wholegroup);
  si := Size(ri);
  Print("Whole size: ",si,"\n");
  i := Length(l);
  l := ShallowCopy(l);
  while i >= 1 do
      drop := false;
      ll := Concatenation(l{[1..i-1]},l{[i+1..Length(l)]});
      ri := Sizerecogniser(Group(StripMemory(ll)));
      if Size(ri) <> si then
          ri2 := Sizerecogniser(Group(StripMemory(ll)));
          if Size(ri2) <> Size(ri) then
              if Size(ri2) = si then
                  # we do not need this generator
                  drop := true;
              fi;
              # otherwise non-conclusive, leave the generator where it is
          fi;
      else
          # we most probably do not need this generator
          drop := true;
      fi;
      if drop then
          Remove(l,i);
          i := i - 1;
          Print("-\c");
      else
          i := i - 1;
          Print("+\c");
      fi;
  od;
  Print("\n");
  return l;
end );


###############################################
# Find class representatives using powermaps: #
###############################################

InstallGlobalFunction( ClassMaker,
function(ct,cyc)
  # ct an ordinary character table and cyc a list of class names we have
  local cln,done,erg,i,l,m,p,po,pow,todo,start;

  pow := ComputedPowerMaps(ct);
  cln := ClassNames(ct);

  cyc := List(cyc,LowercaseString);
  for i in [1..Length(cyc)] do
      p := Position(cyc[i],'-');
      if p <> fail then
          Info(InfoOrb,1,"Warning: Class name containing \"-\"!");
          cyc[i] := cyc[i]{[1..p-1]};
      fi;
  od;
  erg := [];
  todo := [];
  for i in [1..Length(cyc)] do
      po := Position(cln,cyc[i]);
      erg[po] := [i,1];
      Add(todo,po);
  od;
  done := Length(cyc);
  while done < Length(cln) and Length(todo) > 0 do
      i := todo[1];
      todo := todo{[2..Length(todo)]};
      for p in [1..Length(pow)] do
          if IsBound(pow[p]) then
              m := pow[p];
              if not(IsBound(erg[m[i]])) then
                  erg[m[i]] := [erg[i][1],erg[i][2]*p];
                  Add(todo,m[i]);
                  done := done + 1;
              fi;
          fi;
      od;
  od;
  if done < Length(cln) then
      Error("Unvorhergesehener Fall Nummer 1!");
  fi;
  if cln[1] = "1a" then
      l := [[1,0]];
      start := 2;
  else
      l := [];
      start := 1;
  fi;
  for i in [start..Length(cln)] do
      Add(l,erg[i]);
  od;
  return StraightLineProgramNC([l],Length(cyc));
end);


###########################
# Finding nice quotients: #
###########################

InstallGlobalFunction( OrbitsStatisticOnVectorSpace,
  function(gens,size,ti)
  # gens must be a list of compressed matrices, size the order of the group
  local len,nr,o,t,total,v;
  v := ShallowCopy(gens[1][1]);
  t := Runtime();
  total := 0;
  nr := 0;

  while Runtime() < t + ti*1000 do
      Randomize(v);
      o := InitOrbit(gens,v,OnRight,3*size,rec(grpsizebound := size,
                                               report := 0));
      Enumerate(o);
      len := Length(o!.orbit);
      total := total + len;
      nr := nr + 1;
      Print("Found length ",String(Length(o!.orbit),9),", have now ",
            String(nr,4)," orbits, average length: ",
            QuoInt(total+QuoInt(nr,2),nr),"     \r");
  od;
  Print("\n");
end);

