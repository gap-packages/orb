#############################################################################
##
##                             orb package
##  search.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for searching in groups.
##
#############################################################################

# Random vectors and matrices:

# Methods for Randomize for gf2 and 8bit vectors have been taken out
# they will be in GAP >= 4.5 in the library. In the meantime use
# cmats.

InstallGlobalFunction( MakeRandomVectors,
  function( arg )
    local i,l,number,randomsource,sample;
    
    if Length(arg) <= 1 or Length(arg) > 3 then
        ErrorNoReturn("Usage: MakeRandomVectors( sample, number [,randomsource] )");
    fi;
    sample := arg[1];
    number := arg[2];
    if Length(arg) = 3 then
        randomsource := arg[3];
    else
        randomsource := GlobalRandomSource;
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
        ErrorNoReturn("Usage: MakeRandomLines( sample, number [,randomsource] )");
    fi;
    sample := arg[1];
    number := arg[2];
    if Length(arg) = 3 then
        randomsource := arg[3];
    else
        randomsource := GlobalRandomSource;
    fi;
    
    l := [];
    for i in [number,number-1..1] do
        sample := ShallowCopy(sample);
        repeat
            Randomize(sample,randomsource);
            pos := PositionNonZero(sample);
        until pos <= Length(sample);
        MultVector(sample,sample[pos]^-1);
        l[i] := sample;
    od;
    return l;
  end );

# Product replacers:

BindGlobal( "ProductReplacersType", 
   NewType( ProductReplacersFamily, IsProductReplacer and IsMutable) );

InstallMethod( ProductReplacer, "for a list of group generators",
  [IsList], function( gens )
    return ProductReplacer( gens, rec( ) );
  end );

InstallMethod( ProductReplacer,
  "for a group object and an options record",
  [IsGroup, IsRecord],
  function(g,r) return ProductReplacer(GeneratorsOfGroup(g),r); end );

InstallMethod( ProductReplacer,
  "for a group object",
  [IsGroup],
  function(g) return ProductReplacer(GeneratorsOfGroup(g),rec()); end );

InstallMethod( ProductReplacer, 
  "for a list of group generators and an options record",
  [IsList, IsRecord],
  function( gens, opt )
    # First add some default options if not set:
    local pr;
    if Length(gens) = 0 then
        Error("ProductReplacer: need at least one generator");
        return fail;
    fi;
    pr := ShallowCopy(opt);
    if CanEasilySortElementsFamily(FamilyObj(gens[1])) then
        pr.gens := Set(gens);
    else
        pr.gens := ShallowCopy(gens);
    fi;
    pr.nrgens := Length(pr.gens);
    if not IsBound(pr.addslots) then
        pr.addslots := 5;
    fi;
    if not IsBound(pr.minslots) then
        pr.minslots := 0;
    fi;
    if Length(gens) = 1 and pr.addslots = 0 then
        # Make sure that the case of one generator is OK!
        pr.minslots := Maximum(2,pr.minslots);
    fi;
    if not IsBound(pr.randomsource) then
        pr.randomsource := GlobalRandomSource;
    fi;
    if not IsBound(pr.scramble) then 
        pr.scramble := 30; 
    fi;
    if not IsBound(pr.scramblefactor) then 
        pr.scramblefactor := 4;
    fi;
    pr.scramble := Maximum(pr.scramble,pr.scramblefactor*pr.nrgens);
    if not IsBound(pr.retirecaptain) then
        pr.retirecaptain := 2 * pr.scramble;
    fi;
    if not IsBound(pr.maxdepth) then
        pr.maxdepth := infinity;
    fi;
    if not IsBound(pr.noaccu) then
        pr.noaccu := false;
    fi;
    if not IsBound(pr.accus) or pr.accus < 0 then
        pr.accus := 5;
    elif pr.accus = 0 then
        pr.noaccu := true;
    fi;
    if not IsBound(pr.accelerator) then
        pr.accelerator := true;
    fi;
    if not IsBound(pr.normalin) then
        pr.normalin := false;
    else
        if pr.normalin <> false and
           not(IsProductReplacer(pr.normalin)) then
            Error("normalin option must be a product replacer");
        fi;
    fi;
    pr.slots := pr.nrgens + pr.addslots;
    if pr.slots < pr.minslots then
        pr.slots := pr.minslots;
    fi;
    pr.initialized := false;
    pr.steps := 0;
    if pr.noaccu = false then
        pr.accu := ListWithIdenticalEntries(pr.accus,pr.gens[1]^0);
        pr.accupos := 0;
    else
        pr.accu := fail;
        pr.accupos := 0;
    fi;
    Objectify(ProductReplacersType,pr);
    pr!.team := ShallowCopy(pr!.gens);
    while Length(pr!.team) < pr!.slots do
         Add(pr!.team,pr!.gens[Random(pr!.randomsource,1,pr!.nrgens)]);
    od;
    return pr;
  end );

InstallMethod( Reset, "for a product replacer", [IsProductReplacer],
  function(pr)
    if not(pr!.initialized) then return; fi;
    pr!.team := ShallowCopy(pr!.resetteam);
    pr!.accu := ShallowCopy(pr!.resetaccu);
    pr!.accupos := pr!.resetaccupos;
    pr!.steps := pr!.resetsteps;
    if pr!.normalin <> false then
        Reset(pr!.normalin);
    fi;
  end );

InstallMethod( Next, "for a product replacer", [IsProductReplacer],
  function(pr)
    local OneStep,i;
    OneStep := function(pr)
        local a, b, c, l, x, result, i;
        pr!.steps := pr!.steps + 1;
        c := Random(pr!.randomsource,1,2);
        if pr!.accelerator and pr!.steps <= pr!.retirecaptain then
            a := Random(pr!.randomsource,2,pr!.slots);
            b := Random(pr!.randomsource,2,pr!.slots);
            if c = 1 then
                if pr!.normalin <> false then
                    pr!.team[1] := pr!.team[1]*pr!.team[b]^Next(pr!.normalin);
                    pr!.team[a] := pr!.team[a]*pr!.team[1];
                else
                    pr!.team[1] := pr!.team[1]*pr!.team[b];
                    pr!.team[a] := pr!.team[a]*pr!.team[1];
                fi;
            else
                if pr!.normalin <> false then
                    pr!.team[1] := pr!.team[b]^Next(pr!.normalin)*pr!.team[1];
                    pr!.team[a] := pr!.team[1] * pr!.team[a];
                else
                    pr!.team[1] := pr!.team[b] * pr!.team[1];
                    pr!.team[a] := pr!.team[1] * pr!.team[a];
                fi;
            fi;
            result := pr!.team[a];
        else
            a := Random(pr!.randomsource,1,pr!.slots);
            b := Random(pr!.randomsource,1,pr!.slots-1);
            if b >= a then b := b + 1; fi;
            if c = 1 then
                if pr!.normalin <> false then
                    pr!.team[a]:=pr!.team[a]*pr!.team[b]^Next(pr!.normalin);
                else
                    pr!.team[a]:=pr!.team[a]*pr!.team[b];
                fi;
            else
                if pr!.normalin <> false then
                    pr!.team[a]:=pr!.team[b]^Next(pr!.normalin)*pr!.team[a];
                else
                    pr!.team[a]:=pr!.team[b]*pr!.team[a];
                fi;
            fi;
            result := pr!.team[a];
        fi;
        if pr!.noaccu or
           pr!.steps <= Maximum(pr!.nrgens * pr!.scramblefactor, pr!.scramble)
                        - pr!.accus then
            return result;
        else
            pr!.accupos := pr!.accupos + 1;
            if pr!.accupos > pr!.accus then pr!.accupos := 1; fi;
            pr!.accu[pr!.accupos] := pr!.accu[pr!.accupos] * result;   # Rattle
            return pr!.accu[pr!.accupos];
        fi;
    end;
    if pr!.steps > pr!.maxdepth then Reset(pr); fi;
    if not(pr!.initialized) then
        for i in [1..Maximum(pr!.nrgens * pr!.scramblefactor, pr!.scramble)] do
            OneStep(pr);
        od;
        pr!.resetteam := ShallowCopy(pr!.team);  # Remember for reset
        pr!.resetaccu := ShallowCopy(pr!.accu);
        pr!.resetaccupos := pr!.accupos;
        pr!.resetsteps := pr!.steps;
        pr!.initialized := true;
    fi;
    return OneStep(pr);
  end );

InstallMethod( AddGeneratorToProductReplacer, 
  "for a product replacer and a new generator",
  [ IsProductReplacer, IsObject ],
  function(pr,el)
    local i;
    Add(pr!.gens,el);
    pr!.nrgens := pr!.nrgens + 1;
    for i in [1..Length(pr!.team)] do
        pr!.team[i] := pr!.team[i] * el^Random(0,7);
    od;
    Add(pr!.team,el);
    if pr!.initialized then
        for i in [1..Length(pr!.resetteam)] do
            pr!.resetteam[i] := pr!.resetteam[i] * el^Random(0,7);
        od;
        Add(pr!.resetteam,el);
    fi;
    pr!.slots := pr!.slots + 1;
  end );

InstallMethod( ViewObj, "for a product replacer", [IsProductReplacer],
  function(pr)
    Print("<product replacer gens=",pr!.nrgens," slots=",pr!.slots,
          " scramble=",Maximum(pr!.nrgens*pr!.scramblefactor,pr!.scramble),
          " maxdepth=",pr!.maxdepth,"\n         steps=",pr!.steps);
    if pr!.noaccu then Print(" (without accu");
    else Print(" (rattle accus=",pr!.accus); fi;
    if pr!.accelerator then Print(", with accelerator)");
    else Print(")"); fi;
    if pr!.normalin <> false then
        Print("\n         normalin=");
        ViewObj(pr!.normalin);
    fi;
    Print(">");
  end );


# Finding things in groups, random searchers:

BindGlobal( "RandomSearchersType", 
              NewType( RandomSearchersFamily, 
                       IsRandomSearcher and IsMutable ) );

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
    if not IsBound(r.limit) then
        r.limit := infinity;
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
            if rs!.tries > rs!.limit then
                return fail;
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
      o := Orb(gens,v,OnRight,rec(orbsizebound := size,
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
      o := Orb(gens,v,OnLines,rec(orbsizebound := size,
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

InstallMethod( FindShortGeneratorsOfSubgroup, "without option rec or func",
  [ IsGroup, IsGroup ],
  function(G,U)
    return FindShortGeneratorsOfSubgroup(G,U,
               rec( membershiptest := \in, sizetester := Size ) );
  end );

InstallMethod( FindShortGeneratorsOfSubgroup, "with option rec or func",
  [ IsGroup, IsGroup, IsObject ],
function(G,U,opt)
  local su,o,si,s,ps,min,minsi,subgens,subwords,i,l,membershiptest,sizetester;
  if IsFunction(opt) then
      membershiptest := opt;
      sizetester := Size;
      su := Size(U);
  elif IsRecord(opt) then
      if IsBound(opt.membershiptest) then
          membershiptest := opt.membershiptest;
      else
          membershiptest := \in;
      fi;
      if IsBound(opt.sizetester) then
          sizetester := opt.sizetester;
      else
          sizetester := Size;
      fi;
      if IsBound(opt.sizeU) then
          su := opt.sizeU;
      else
          if HasSize(U) then
              su := Size(U);
          else
              su := sizetester(U);
          fi;
      fi;
  fi;
  if su = 1 then   # the trivial subgroup is easy to generate:
      return rec( gens := [One(U)], 
                  slp := StraightLineProgram( [[[1,0]]],
                                              Length(GeneratorsOfGroup(G)) ) );
  fi;
  o := Orb(GeneratorsOfGroup(G),One(G),OnRight,
           rec( lookingfor := function(o,x) return membershiptest(x,U); end, 
                schreier := true ) );
  Enumerate(o);
  subgens := [o!.orbit[o!.found]];
  subwords := [TraceSchreierTreeForward(o,o!.found)];
  l := 1;   # will always be the length of subgens and subwords
  si := sizetester(Group(ShallowCopy(subgens)));
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
      s := sizetester(Group(ShallowCopy(subgens)));
      if s = su then
          # OK, we have got a generating set:
          # Now try shortening generating set:
          Info(InfoOrb,2,"Found full subgroup of size ",si,":",subwords);
          Info(InfoOrb,2,"Need ",l," generators.");
          if l <= 8 then
              ps := ORB_PowerSet([1..l]);
              min := Length(ps);
              minsi := l;
              for i in [2..Length(ps)-1] do
                  s := sizetester(Group(subgens{ps[i]}));
                  if s = su and Length(ps[i]) < minsi then
                      min := i;
                      minsi := Length(ps[i]);
                      Info(InfoOrb,2,"Need ",minsi," generators.");
                  fi;
              od;
              subgens := subgens{ps[min]};
              subwords := subwords{ps[min]};
          else
              i := 1;
              while i <= Length(subgens) do
                  s := sizetester(
                          Group(subgens{Concatenation([1..i-1],[i+1..l])}));
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


#############################################################################
# A generic way to find stabilizers:
#############################################################################

InstallGlobalFunction( ORB_EstimatePermGroupSize,
  function( gens )
    local g,s;
    g := Group(gens);
    s := StabChain(g,rec( random := 900 ));
    return SizeStabChain(s);
  end );


InstallGlobalFunction( ORB_FindStabilizerMC,
  function( gens, pt, op, memory, limit, errorbound, estimatefunc, opt )
    local counter,done,est,gensi,mem,o,oldest,orbpart,p,pr,prob,stab,tries,
          x,y,z;
    mem := Memory(pt);
    if mem = fail then
        mem := SIZE_OBJ(pt);
        if mem = 0 then mem := GAPInfo.BytesPerVariable; fi;
    fi;
    orbpart := QuoInt(memory,mem);
    o := Orb( gens, pt, op, rec( report := QuoInt(orbpart,10), 
                                 schreier := true, 
                                 hashlen := NextPrimeInt( orbpart*2 ) ) );
    Info( InfoOrb, 2, "Enumerating up to ",orbpart," elements of orbit..." );
    Enumerate(o,orbpart);

    # Now find stabilizer generators:
    stab := [];
    done := false;
    tries := 0;
    pr := ProductReplacer(gens,opt);
    est := 1;
    prob := 1;
    gensi := List(gens,x->x^-1);
    while prob > errorbound do
        counter := 0;
        repeat
            counter := counter + 1;
            x := Next(pr);
            y := op(pt,x);
            p := Position(o,y);
            Print(".\c");
        until p <> fail or counter > limit;
        Print("\n");
        if p = fail then
            Info( InfoOrb, 1, "Giving up..." );
            return rec(stab := stab, prob := fail);
        fi;
        z := x * Product(gensi{TraceSchreierTreeBack(o,p)});
        if IsOne(z) then
            Info( InfoOrb, 2, "Found identity element." );
        else
            AddSet(stab,z);
            oldest := est;
            if IsFunction(estimatefunc) then
                Info( InfoOrb, 2, "Starting estimation (old est=", est,")..." );
                est := estimatefunc(stab);
                Info( InfoOrb, 2, "Finished estimation: ", est );
            fi;
            if est = oldest then
                prob := prob / 2;
                Info( InfoOrb, 2, "New estimate is the same, prob=",prob );
            else
                prob := 1;
                Info( InfoOrb, 2, "New estimate is ",est,", prob=",prob );
            fi;
        fi;
    od;
    return rec(stab := stab, prob := prob);
  end );


InstallGlobalFunction( ORB_FindNeedleMappers,
  function( gens, pt, op, needles, memory, mappers, opt )
    local foundmappers,gensi,i,j,lookfunc,mem,o,p,r,size,x;
    size := fail;
    if IsGroup(gens) then
        if HasSize(gens) then
            size := Size(gens);
        fi;
        gens := GeneratorsOfGroup(gens);
    fi;
    if not IsBound(opt.orblen) then
        mem := Memory(pt);
        if mem = fail then
            mem := SIZE_OBJ(pt);
            if mem = 0 then 
                mem := GAPInfo.BytesPerVariable;
            fi;
        fi;
        opt.orblen := QuoInt(memory,Length(needles)*mem);
        if size <> fail and opt.orblen * opt.orblen > size then
            opt.orblen := 2^(QuoInt(Log2Int(size),2)+1);
        fi;
    fi;
    o := [];
    Info( InfoOrb, 1, "Enumerating ",Length(needles)," orbits up to length ",
          opt.orblen, "..." );
    for i in [1..Length(needles)] do
        o[i] := Orb( gens, needles[i], op, rec( report := QuoInt(opt.orblen,5),
                                                schreier := true ) );
        Enumerate(o[i],opt.orblen);
        Info( InfoOrb, 2, "Finished one orbit." );
    od;

    j := fail;
    p := fail;

    lookfunc := function( x )
      # Uses o from outer function!
      # Writes j and p in outer function!
      # This uses the feature of GAP, that j and p here always refer to
      # the outer function ORB_FindNeedleMappers
      # Reads pt and op from outer function.
      j := 1;
      while j <= Length(o) do
          p := Position(o[j],op(pt,x));
          if p <> fail then return true; fi;
          j := j + 1;
      od;
      j := fail;
      p := fail;
      return false;
    end;

    gensi := List(gens,x->x^-1);
    r := RandomSearcher( gens, lookfunc, opt );
    # we just hand down the options record to the random searcher!

    foundmappers := [];
    i := 1;
    while Length(foundmappers) <= mappers and i <= 15 * mappers do
        x := Search(r);
        if x = fail then return foundmappers; fi;
        AddSet(foundmappers,x * Product(gensi{TraceSchreierTreeBack(o[j],p)}));
        Info( InfoOrb, 2, "Found a needle mapper, have now ",
              Length(foundmappers), ".");
        i := i + 1;
    od;

    return foundmappers;
  end );

############################################################################
# A method to find transversals in matrix groups:
############################################################################

InstallGlobalFunction( FindWordsForRightTransversal,
  function( Ggens, Hgens, index )
    # Ggens and Hgens must be lists of invertible matrices over a
    # finite field. The group H generated by Hgens must be a subgroup
    # of G, which is generated by Ggens, index must be the index.
    # This function returns words in Ggens that are a complete set
    # of coset representatives of the right cosets {Hg}.
    local Check,i,inv,l,n,res,v;

    # Find invariant vectors:
    Info(InfoOrb,2,"Computing invariant spaces of generators...");
    l := List(Hgens,x->NullspaceMat(x-One(x)));
    Info(InfoOrb,2,"Found dimensions: ",List(l,Length));
    if Length(l) = 1 then
        inv := l[1];
    else
        inv := SumIntersectionMat(l[1],l[2]);
        inv := inv[2];
        Info(InfoOrb,2,"Intersection has dim ",NrRows(inv));
        i := 3;
        while i <= Length(l) and NrRows(inv) > 0 do
            inv := SumIntersectionMat(inv,l[i]);
            inv := inv[2];
            Info(InfoOrb,2,"Intersection has dim ",NrRows(inv));
            i := i + 1;
        od;
    fi;
    # Now inv contains vectors that span the invariant space.
    if NrRows(inv) = 0 then
        Info(InfoOrb,1,"no H-invariants except 0");
        return fail;
    fi;
    Unbind(l);

    Check := function( pt, op )
      local i,o,words;
      o := Orb(Ggens,pt,op,rec( report := 2000, schreier := true, 
                                hashlen := NextPrimeInt( index*3 ) ));
      Enumerate(o);
      if Length(o) = index then   
          # Hurra!
          words := [[]];
          for i in [2..Length(o)] do
              Add(words,TraceSchreierTreeForward(o,i));
          od;
          return rec( words := words, orbit := o );
      fi;
      Info(InfoOrb,2,"Found orbit length ",Length(o));
      return fail;
    end;
      
    # First try whether one basis vector provides an orbit of the
    # right length:
    Info(InfoOrb,2,"Trying to find a nice orbit on vectors...");
    for i in [1..NrRows(inv)] do
        Info(InfoOrb,2,"Using vector #",i,"(",NrRows(inv),")");
        res := Check(ShallowCopy(inv[i]),OnRight);
        if res <> fail then return res; fi;
    od;

    # Now try tuples of vectors:
    n := 2;  # first pairs, then maybe more
    repeat
        l := Matrix([],NrCols(inv),inv);
        for i in [1..n] do
            v := ZeroVector(NrRows(inv),inv[1]);
            Randomize(v);
            Add(l,v*inv);
        od;
        Info(InfoOrb,2,"Using ",n,"-tuple...");
        res := Check(l,OnRight);
        if res <> fail then return res; fi;
        n := n + 1;
    until n = NrRows(inv);

    # Now try the last resort:
    Info(InfoOrb,2,"Trying full invariant space...");
    return Check(inv,OnRight);
  end );

InstallGlobalFunction( FindWordsForLeftTransversal,
  function( Ggens, Hgens, index )
    # The same as above, but gives words for coset reps of the left
    # cosets {gH}. This is done by using the transposition
    # anti-automorphism. This maps a right transversal of the
    # transposed groups to a left transversal. Since we take the
    # transposed generators in both groups, the words have to be
    # reversed. Note that the orbit is the one obtained for the
    # transposed generators!
    local res;
    res := FindWordsForRightTransversal(List(Ggens,TransposedMat),
                                        List(Hgens,TransposedMat),index);
    if res = fail then return fail; fi;
    res.words := List(res.words,Reversed);
    return res;
  end );

############################################################################
# Find transforming matrices:
############################################################################

InstallGlobalFunction( TransformingMatsLSE, 
  function( A, B )
  local M,n;
  # Returns the matrix of the linear system of equations to solve TA=BT 
  # such that the nullspace of that matrix gives a basis of the solution 
  # space in flat notation.
  # "Flat notation" means Concatenation(T) instead of T.
  # Each row of M will be an equation:
  # M := [ [A[1][1],A[2][1],A[3][1],0,0,0,0,0,0],
  #        [A[1][2],A[2][2],A[3][2],0,0,0,0,0,0],
  #        [A[1][3],A[2][3],A[3][3],0,0,0,0,0,0],
  #        [0,0,0,A[1][1],A[2][1],A[3][1],0,0,0],
  #        [0,0,0,A[1][2],A[2][2],A[3][2],0,0,0],
  #        [0,0,0,A[1][3],A[2][3],A[3][3],0,0,0],
  #        [0,0,0,0,0,0,A[1][1],A[2][1],A[3][1]],
  #        [0,0,0,0,0,0,A[1][2],A[2][2],A[3][2]],
  #        [0,0,0,0,0,0,A[1][3],A[2][3],A[3][3]] ]
  #     -[ [B[1][1],0,0,B[1][2],0,0,B[1][3],0,0],
  #        [0,B[1][1],0,0,B[1][2],0,0,B[1][3],0],
  #        [0,0,B[1][1],0,0,B[1][2],0,0,B[1][3]],
  #        [B[2][1],0,0,B[2][2],0,0,B[2][3],0,0],
  #        [0,B[2][1],0,0,B[2][2],0,0,B[2][3],0],
  #        [0,0,B[2][1],0,0,B[2][2],0,0,B[2][3]],
  #        [B[3][1],0,0,B[3][2],0,0,B[3][3],0,0],
  #        [0,B[3][1],0,0,B[3][2],0,0,B[3][3],0],
  #        [0,0,B[3][1],0,0,B[3][2],0,0,B[3][3]] ];
  n := NrRows(A);
  if n <> NrCols(A) or n <> NrRows(B) or n <> NrCols(B) then
      Error("need square matrices of same size");
  fi;
  M :=   KroneckerProduct(OneMutable(A),TransposedMat(A))
       - KroneckerProduct(B,OneMutable(B));
  return TransposedMat(M);
end);

InstallGlobalFunction( TransformingMats,
  function(A, B)
    return NullspaceMat(TransformingMatsLSE(A,B));
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
