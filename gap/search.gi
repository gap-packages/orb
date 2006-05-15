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

InstallGlobalFunction( MakeRandomVectors,
  function( arg )
    local i,l,number,randomsource,sample;
    
    if Length(arg) <= 1 or Length(arg) > 3 then
        Print("Usage: MakeRandomVectors( sample, number [,randomsource] )\n");
        return;
    fi;
    sample := arg[1];
    number := arg[2];
    if Length(arg) = 3 then
        randomsource := arg[3];
    else
        randomsource := RandomSource("global");
    fi;
    
    l := [];
    for i in [number,number-1..1] do
        sample := ShallowCopy(sample);
        Randomize(sample,randomsource);
        l[i] := sample;
    od;
    return l;
  end );

InstallGlobalFunction( MakeRandomLines,
  function( arg )
    local i,l,number,pos,randomsource,sample;
    
    if Length(arg) <= 1 or Length(arg) > 3 then
        Print("Usage: MakeRandomLines( sample, number [,randomsource] )\n");
        return;
    fi;
    sample := arg[1];
    number := arg[2];
    if Length(arg) = 3 then
        randomsource := arg[3];
    else
        randomsource := RandomSource("global");
    fi;
    
    l := [];
    for i in [number,number-1..1] do
        sample := ShallowCopy(sample);
        repeat
            Randomize(sample,randomsource);
            pos := PositionNonZero(sample);
        until pos <= Length(sample);
        MultRowVector(sample,sample[pos]^-1);
        l[i] := sample;
    od;
    return l;
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

InstallGlobalFunction( OrbitStatisticOnVectorSpace,
  function(gens,size,ti)
  # gens must be a list of compressed matrices, size the order of the group
  local len,nr,o,t,total,v;
  v := ShallowCopy(gens[1][1]);
  t := Runtime();
  total := 0;
  nr := 0;

  while Runtime() < t + ti*1000 do
      Randomize(v);
      o := Orb(gens,v,OnRight,rec(grpsizebound := size,
                                  hashlen := 3*size, report := 0));
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

InstallGlobalFunction( OrbitStatisticOnVectorSpaceLines,
  function(gens,size,ti)
  # gens must be a list of compressed matrices, size the order of the group
  local len,nr,o,t,total,v,c;
  v := ShallowCopy(gens[1][1]);
  t := Runtime();
  total := 0;
  nr := 0;

  while Runtime() < t + ti*1000 do
      Randomize(v);
      c := PositionNonZero(v);
      if c <= Length(v) then
          v := v / v[c];
      fi;
      o := Orb(gens,v,OnLines,rec(grpsizebound := size,
                                  hashlen := 3*size, report := 0));
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

###############################################
# Finding nice generating sets for subgroups: #
###############################################

InstallGlobalFunction( ORB_PowerSet,
function(s)
  local i,l,le,ll;
  if Length(s) = 0 then
      return [[]];
  elif Length(s) = 1 then
      return [[],[s[1]]];
  else
      l := ORB_PowerSet(s{[1..Length(s)-1]});
      le := Length(l);
      for i in [1..le] do
          ll := ShallowCopy(l[i]);
          Add(ll,s[Length(s)]);
          Add(l,ll);
      od;
      return l;
  fi;
end );

InstallGlobalFunction( ORB_SLPLineFromWord, 
function(wo)
  local li,i,j;
  li := [];
  i := 1;
  while i <= Length(wo) do
      j := i+1;
      while j <= Length(wo) and wo[j] = wo[i] do
          j := j + 1;
      od;
      if wo[i] > 0 then
          Add(li,wo[i]);
          Add(li,j-i);
      else
          Add(li,-wo[i]);
          Add(li,-(j-i));
      fi;
      i := j;
  od;
  return li;
end );

InstallGlobalFunction( FindShortGeneratorsOfSubgroup,
function(G,U,membershiptest)
  local su,o,si,s,ps,min,minsi,subgens,subwords,i,l;
  su := Size(U);
  if su = 1 then   # the trivial subgroup is easy to generate:
      return rec( gens := [One(U)], 
                  slp := StraightLineProgram( [[[1,0]]],
                                              Length(GeneratorsOfGroup(G)) ) );
  fi;
  o := Orb(GeneratorsOfGroup(G),One(G),OnRight,
           rec( lookingfor := x -> membershiptest(x,U), 
                schreier := true ) );
  Enumerate(o);
  subgens := [o!.orbit[o!.found]];
  subwords := [TraceSchreierTreeForward(o,o!.found)];
  l := 1;   # will always be the length of subgens and subwords
  si := Size(Group(ShallowCopy(subgens)));
  Info(InfoOrb,2,"Found subgroup of size ",si,":",subwords);
  if si = su then
      # Cyclic subgroup
      s := StraightLineProgram([[ORB_SLPLineFromWord(subwords[1])]],
                               Length(GeneratorsOfGroup(G)));
      return rec( gens := subgens, slp := s );
  fi;
  while true do
      Enumerate(o);
      Add(subgens,o!.orbit[o!.found]);
      Add(subwords,TraceSchreierTreeForward(o,o!.found));
      l := l + 1;
      if l <> Length(subgens) or l <> Length(subwords) then
          Error();
      fi;
      s := Size(Group(ShallowCopy(subgens)));
      if s = su then
          # OK, we have got a generating set:
          # Now try shortening generating set:
          Info(InfoOrb,2,"Found ",l," generators.");
          if l <= 8 then
              ps := ORB_PowerSet([1..l]);
              min := Length(ps);
              minsi := l;
              for i in [2..Length(ps)-1] do
                  s := Size(Group(subgens{ps[i]}));
                  if s = su and Length(ps[i]) < minsi then
                      min := i;
                      minsi := Length(ps[i]);
                      Info(InfoOrb,2,"Found ",minsi," generators.");
                  fi;
              od;
              subgens := subgens{ps[min]};
              subwords := subwords{ps[min]};
          else
              i := 1;
              while i <= Length(subgens) do
                  s := Size(Group(subgens{Concatenation([1..i-1],[i+1..l])}));
                  if s = su then  # we do not need generator i
                      Remove(subgens,i);
                      Remove(subwords,i);
                      l := l - 1;
                  else
                      i := i + 1;
                  fi;
              od;
          fi;
          l := List(subwords,ORB_SLPLineFromWord);
          s := StraightLineProgram([l],Length(GeneratorsOfGroup(G)));
          return rec( gens := subgens, slp := s );
      fi;
      if s=si then
          Unbind(subgens[l]);
          Unbind(subwords[l]);
          l := l - 1;
      else
          si := s;
          Info(InfoOrb,2,"Found subgroup of size ",si,":",subwords);
      fi;
  od;
end );

##############################################################
# Helpers for permutation characters for certain operations: #
##############################################################

InstallGlobalFunction( NumberFixedVectors,
  function( mat )
    local f,n,q;
    n := NullspaceMat(mat - mat^0);
    f := BaseField(mat);
    q := Size(f);
    return q^Length(n);
  end );

InstallGlobalFunction( NumberFixedLines,
  function( mat )
    local el,f,n,nr,q;
    f := BaseField(mat);
    q := Size(f);
    nr := 0;
    for el in f do
        if not(IsZero(el)) then
            n := NullspaceMat(mat - el*mat^0);
            nr := nr + (q^Length(n)-1)/(q-1);
        fi;
    od;
    return nr;
  end );

InstallGlobalFunction( SpacesOfFixedLines,
  function( mats )
    local el,f,i,n,q,spcs;
    f := BaseField(mats);
    q := Size(f);
    spcs := List(mats,i->[]);
    for i in [1..Length(mats)] do
        for el in f do
            if not(IsZero(el)) then
                n := NullspaceMat(mats[i] - el*mats[i]^0);
                Add(spcs[i],n);
            fi;
        od;
    od;
    # to be continued...
    return spcs;
  end );

##################################################
# Helpers for making short SLPs from word lists: #
##################################################

InstallGlobalFunction( SLPForWordList,
  function( wordlist, nrgens )
    local bestlen,bestn,bestword,havewords,i,j,l,line,n,p,slp,w,where,wo,
          wordset,writepos,ww;
    havewords := [[]];
    where := [0];
    for i in [1..nrgens] do
        Add(havewords,[i]);
        Add(where,i);
        Add(havewords,[-i]);
        Add(where,-i);
    od;
    wordset := Set(wordlist);
    slp := [];
    writepos := nrgens+1;
    for w in wordset do
      if not(w in havewords) then
        wo := ShallowCopy(w);
        line := [];
        while Length(wo) > 0 do
            # Look through all havewords and see which one helps most:
            bestlen := 0;
            bestword := 0;
            bestn := 0;
            for j in [2..Length(havewords)] do
                ww := havewords[j];
                l := Length(ww);
                if l <= Length(wo) and wo{[1..l]} = ww then
                    # How often does it fit here?
                    n := 1;
                    while (n+1) * l <= Length(wo) and
                          ww = wo{[n*l+1..(n+1)*l]} do
                        n := n + 1;
                    od;
                    if n*l > bestlen then
                        bestlen := n*l;
                        bestword := j;
                        bestn := n;
                    fi;
                fi;
            od;
            if where[bestword] > 0 then
                Add(line,where[bestword]);
                Add(line,bestn);
            else
                Add(line,-where[bestword]);
                Add(line,-bestn);
            fi;
            l := Length(havewords[bestword]);
            wo := wo{[l*bestn+1..Length(wo)]};
        od;
        Add(slp,line);
        Add(havewords,w);
        Add(where,writepos);
        Add(havewords,-Reversed(w));
        Add(where,-writepos);
        writepos := writepos+1;
      fi;
    od;
    line := [];
    for w in wordlist do
        p := Position(havewords,w);
        if where[p] > 0 then
            Add(line,[where[p],1]);
        elif where[p] = 0 then   # The empty word
            Add(line,[1,0]);
        else
            Add(line,[-where[p],-1]);
        fi;
    od;
    Add(slp,line);
    return [StraightLineProgram(slp,nrgens),havewords,where];
end );

