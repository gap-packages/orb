#FindShortEl := function(arg)
#  # To start call with G,f[,exc]
#  # Result: [el,word,status]
#  # To go on call with status
#  # Result: [el,word,status]
#  local g,i,l,s,wo,x;
#  if Length(arg) = 1 then
#      s := arg[1];
#  else
#      s := rec(G := arg[1],f := arg[2],isorbitrecord := true);
#      if Length(arg) > 2 then
#          s.exc := arg[3];
#      else
#          s.exc := [];
#      fi;
#      s.gens := GeneratorsOfGroup(s.G);
#      s.orb := [[One(s.G),[]]];
#      s.set := [One(s.G)];
#      s.i := 1;
#      s.j := 1;
#      if not(One(s.G) in s.exc) and s.f(One(s.G)) then
#          return [s.orb[1][1],s.orb[1][2],s];
#      fi;
#  fi;
#  l := Length(s.gens);
#  while s.i <= Length(s.orb) do
#      x := s.orb[s.i][1] * s.gens[s.j];
#      g := s.j;
#      i := s.i;
#      s.j := s.j + 1;
#      if s.j > Length(s.gens) then
#          s.j := 1;
#          s.i := s.i + 1;
#      fi;
#      if not(x in s.set) then
#          AddSet(s.set,x);
#          wo := ShallowCopy(s.orb[i][2]);
#          Add(wo,g);
#          Add(s.orb,[x,wo]);
#          if not(x in s.exc) and s.f(x) then
#              AddSet(s.exc,x);  # for next time
#              return [x,wo,s];
#          fi;
#      fi;
#  od;
#  Error("Found no element in group satisfying condition!");
#end;
#
#PowerSet := function(s)
#  local i,l,le,ll;
#  if Length(s) = 0 then
#      return [[]];
#  elif Length(s) = 1 then
#      return [[],[s[1]]];
#  else
#      l := PowerSet(s{[1..Length(s)-1]});
#      le := Length(l);
#      for i in [1..le] do
#          ll := ShallowCopy(l[i]);
#          Add(ll,s[Length(s)]);
#          Add(l,ll);
#      od;
#      return l;
#  fi;
#end;
#
#SLPLineFromWord := function(wo)
#  local li,i,j;
#  li := [];
#  i := 1;
#  while i <= Length(wo) do
#      j := i+1;
#      while j <= Length(wo) and wo[j] = wo[i] do
#          j := j + 1;
#      od;
#      Add(li,wo[i]);
#      Add(li,j-i);
#      i := j;
#  od;
#  return li;
#end;
#
#FindShortGeneratorsSubgroup := function(G,U)
#  local su,l,subgens,r,s,ps,min,minsi,subgensalone,i,si;
#  su := Size(U);
#  l := FindShortEl(G,x->x in U,[One(U)]);
#  subgens := [[l[1],l[2]]];
#  r := l[3];
#  si := Size(Group(List(subgens,y->y[1])));
#  Print("Found subgroup of size ",si,":",List(subgens,x->x[2]),"\n");
#  if si = su then
#      # Cyclic subgroup
#      s := StraightLineProgram([[SLPLineFromWord(l[2])]],
#                               Length(GeneratorsOfGroup(G)));
#      return [subgens,s];
#  fi;
#  while true do
#      l := FindShortEl(r);
#      Add(subgens,[l[1],l[2]]);
#      s := Size(Group(List(subgens,y->y[1])));
#      if s = su then
#          # OK, we have got a generating set:
#          # Now try shortening generating set:
#          Print("Found ",Length(subgens)," generators.\n");
#          ps := PowerSet([1..Length(subgens)]);
#          min := Length(ps);
#          minsi := Length(subgens);
#          subgensalone := List(subgens,x->x[1]);
#          for i in [2..Length(ps)-1] do
#              s := Size(Group(subgensalone{ps[i]}));
#              Print("s=",s,"                    \r");
#              if s = su and Length(ps[i]) < minsi then
#                  min := i;
#                  minsi := Length(ps[i]);
#                  Print("Found ",minsi," generators.\n");
#              fi;
#          od;
#          subgens := subgens{ps[min]};
#          l := List(subgens,x->SLPLineFromWord(x[2]));
#          s := StraightLineProgram([l],Length(GeneratorsOfGroup(G)));
#          return [subgens,s];
#      fi;
#      if s=si then
#          Unbind(subgens[Length(subgens)]);
#      else
#          si := s;
#          Print("Found subgroup of size ",si,":",List(subgens,x->x[2]),"\n");
#      fi;
#  od;
#end;
#
#PrepareChain := function(l)
#  # l is an ascending chain of groups in some nice representation
#  local G,L,P,U,ac,i,ind,j,o,os,pgens,q,r,s;
#  L := Length(l);
#  G := l[L];
#  if L = 1 then
#    r := rec(r := false, slp := false, height := 0);
#    U := TrivialSubgroup(G);
#  else
#    U := l[L-1];
#    r := rec(slp := FindShortGeneratorsSubgroup(G,U)[2]);
#    U := Group(ResultOfStraightLineProgram(r.slp,GeneratorsOfGroup(G)));
#    r.r := PrepareChain(Concatenation(l{[1..L-2]},[U]));
#    r.height := r.r.height+1;
#  fi;
#  r.size := Size(G);
#  # Now we have to calculate a Schreier vector to have a transversal:
#  ac := FactorCosetAction(G,U);
#  P := Image(ac);
#  pgens := GeneratorsOfGroup(P);
#  o := [1];
#  os := [1];
#  s := [false];
#  i := 1;
#  ind := Index(G,U);
#  while i <= Length(o) and Length(o) < ind do
#    for j in [1..Length(pgens)] do
#      q := o[i]^pgens[j];
#      if not(q in os) then
#        Add(o,q);
#        AddSet(os,q);
#        Add(s,[i,j]);
#      fi;
#    od;
#    i := i + 1;
#  od;
#  r.sv := s;
#  return r;
#end;
#
ChainWorker := function(G,r,dowork,seed)
  # G and U lists of generators
  local U,i,o,res,subres,t;
  #Print("ChainWorker\n");
  if r.r = false then
    subres := seed;
  else
    U := ResultOfStraightLineProgram(r.slp,G);
    subres := ChainWorker(U,r.r,dowork,seed);
  fi;
  o := [One(G[1])];
  res := dowork(One(G[1]),fail,subres);
  for i in [2..Length(r.sv)] do
    Print(r.height,": ",i," (",Length(r.sv),")\r");
    t := o[r.sv[i][1]] * G[r.sv[i][2]];
    Add(o,t);
    res := dowork(t,res,subres);
  od;
  return res;
end;

ChainWorker2 := function(G,GG,r,dowork,seed)
  # G, GG, U and UU lists of generators
  local U,UU,i,o,oo,res,subres,t,tt;
  #Print("ChainWorker\n");
  if r.r = false then
    subres := seed;
  else
    U := ResultOfStraightLineProgram(r.slp,G);
    UU := ResultOfStraightLineProgram(r.slp,GG);
    subres := ChainWorker2(U,UU,r.r,dowork,seed);
  fi;
  o := [One(G[1])];
  oo := [One(GG[1])];
  res := dowork(One(G[1]),One(GG[1]),fail,subres);
  for i in [2..Length(r.sv)] do
    Print(r.height,": ",i," (",Length(r.sv),")\r");
    t := o[r.sv[i][1]] * G[r.sv[i][2]];
    Add(o,t);
    tt := oo[r.sv[i][1]] * GG[r.sv[i][2]];
    Add(oo,tt);
    res := dowork(t,tt,res,subres);
  od;
  return res;
end;

DoworkMaschke := function(t,res,subres)
  if res = fail then
    return subres;
  else
    return res + t^-1 * subres * t;
  fi;
end;

DoworkMaschke2 := function(t,tt,res,subres)
  if res = fail then
    return subres;
  else
    return res + t^-1 * subres * tt;
  fi;
end;

CalcChain := function(gens,r,l)
  if r.slp <> false then
      CalcChain(ResultOfStraightLineProgram(r.slp,gens),r.r,l);
  fi;
  Add(l,gens);
  return l;
end;

MakeSuperBasis := function(l)
  local i,workmat;
  if Length(l[1]) <> 0 then
    workmat := MutableCopyMat(l[1]);
    i := 2;
  else
    workmat := MutableCopyMat(l[2]);
    i := 3;
  fi;
  Print("Echelonizing i=",i-1," (",Length(l),")\r");
  SemiEchelonMatDestructive(workmat);
  while i <= Length(l) do
    Append(workmat,MutableCopyMat(l[i]));
    Print("Echelonizing i=",i," (",Length(l),")\r");
    SemiEchelonMatDestructive(workmat);
    workmat := Filtered(workmat,x->x <> Zero(x));
    i := i + 1;
  od;
  ConvertToMatrixRep(workmat);
  return workmat;
end;

Chop := function(gens,koerp)
  # gens is a list of compressed square matrices, generating an algebra
  # we basically call the MeatAxe and return a base change matrix which
  # changes gens into block lower triangular form.
  local bas,basi,l,m;
  m := GModuleByMats(gens,koerp);
  l := MTX.BasesCompositionSeries(m);
  bas := MakeSuperBasis(l);
  basi := bas^-1;
  return rec(gens := List(gens,x->bas*x*basi), bas := bas, basi := basi,
             dimssubs := List(l,Length), len :=Length(l)-1,
             dimsfacts := List([1..Length(l)-1],i->Length(l[i+1])-
                                                   Length(l[i])));
end;
 
ChopDirectSum := function(gens,r)
  # input and output as for `Chop', but algebra has to be semisimple, the
  # basechange changes gens into block diagonal form. For this we need
  # a series of subgroups together with transversals as provided by
  # `PrepareChain' to calculate complements.
  local DoRecurse,re,re2,koerp;

  koerp := Field(gens[1][1]);

  # First do a standard Chop:
  re := Chop(gens,koerp);
  #merkre := re;
  #re := merkre;
  gens := re.gens;
  
  DoRecurse := function(gens,dims)
    local d,dd,e,ei,i,j,l,pi,pihut,res;

    for i in gens do ConvertToMatrixRep(i); od;

    l := Length(dims);
    i := First([1..l],x->2 * dims[x] >= dims[l]);
    if dims[l] - dims[i-1] * 2 < dims[i] * 2 - dims[l] then
        i := i - 1;
    fi;
    Print("Chopping ",dims[l]," into ",dims[i],"+",dims[l]-dims[i],"...\n");
    d := dims[i];
    dd := dims[l];
    e := OneMutable(gens[1]);
    #pi := ZeroMutable(e);
    pi := e*0;
    pi{[1..d]}{[1..d]} := e{[1..d]}{[1..d]};
    
    pihut := ChainWorker(gens,r,DoworkMaschke,pi) / (One(koerp)*r.size);
    e{[d+1..dd]} := e{[d+1..dd]} * (e - pihut);
    ei := e^-1;
    gens := List(gens,x->e*x*ei);

    # Do the submodule:
    if i > 2 then
        Print("Handle submodule:\n");
        res := DoRecurse(List(gens,x->x{[1..d]}{[1..d]}),dims{[1..i]});
        for j in [1..Length(gens)] do
            gens[j]{[1..d]}{[1..d]} := res.gens[j];
        od;
        e{[1..d]}{[1..d]} := res.bas;
        ei{[1..d]}{[1..d]} := res.basi;
        ei{[d+1..dd]}{[1..d]} := ei{[d+1..dd]}{[1..d]} * res.basi;
    fi;

    # Do the complement:
    if i < l-1 then
        Print("Handle factor module:\n");
        res := DoRecurse(List(gens,x->x{[d+1..dd]}{[d+1..dd]}),dims{[i..l]}-d);
        for j in [1..Length(gens)] do
            gens[j]{[d+1..dd]}{[d+1..dd]} := res.gens[j];
        od;
        e{[d+1..dd]} := res.bas * e{[d+1..dd]};
        ei{[d+1..dd]}{[d+1..dd]} := ei{[d+1..dd]}{[d+1..dd]} * res.basi;
    fi;

    return rec(gens := gens, bas := e, basi := ei);
  end;
  
  re2 := DoRecurse(gens,re.dimssubs);
  re2.bas := re2.bas * re.bas;
  re2.basi := re.basi * re2.basi;
  re2.dimssubs := re.dimssubs;
  re2.dimsfacts := re.dimsfacts;
  re2.len := re.len;
  return re2;
end;

RestrictToSummands := function(re,l)
  # re a record coming from ChopDirectSum and l a list of numbers of
  # constituents. Restricts the representation to the corresponding
  # direct summands.
  local i,ll,res;
  ll := [];
  for i in l do
      Append(ll,[re.dimssubs[i]+1..re.dimssubs[i+1]]);
  od;
  res := List(re.gens,x->x{ll}{ll});
  for i in res do
      ConvertToMatrixRep(i);
  od;
  return res;
end;

###############################################################################
##
## Cleans a vector by a subspace given by a matrix <echmatrix> in semi-echelon
## form.
## If the vector does not lie in the subspace and <extflag> is set to 1, 
## the cleaned and normalized vector is appended to echmatrix.
## Note that all function arguments except <field> and <extflag> are altered
## in the process.
##
CleanAndExtend:=function(echmatrix, pivotlist, vector, extflag)
  local   field, decomp,  i,  coeff,  pos;
  
  #initialize output vector
  decomp := vector * 0;
  
  for i in [1..Length(pivotlist)] do
  
    coeff:=vector[ pivotlist[ i ] ];
    if coeff <> 0 * coeff then
      vector := vector - coeff * echmatrix[ i ];
      decomp[ pivotlist[ i ] ] := coeff;
    fi;
    
  od;
  #<vector> is now cleaned and <decomp> stores the performed operations,
  #possibly the decomposition of <vector> in the subspace spanned by the
  #rows of <echmatrix>
  
  #check if <vector> has been decomposed
  pos := PositionNonZero( vector );
  
  #if <vector> could not be decomposed and we wish to append it,
  #we will do just that and update <pivotlist>
  if pos <= Length( vector ) and extflag = 1 then
    echmatrix[ Length( echmatrix ) + 1 ]:=vector * ( vector[ pos ]^-1 );
    Add( pivotlist, pos );
  fi;
  
  return decomp;
  
end;

ZSB:=function( rep, vec )
  # rep is a list of generators in a matrix rep
  # vec is the vector we wish to spin up
  local   nrgen,  basis,  pivots,  pos,  workbasis,  counter,  i,  w;
  
  nrgen:=Length(rep);
  basis:=[ShallowCopy(vec)];
  pivots:=[];
  pos:=PositionNonZero(vec);
  Add(pivots, pos);
  workbasis:=[vec * vec[pos]^-1];
  counter:=1;
  
  while counter <= Length(basis) do
    for i in [1..nrgen] do
      
      w:=basis[counter] * rep[i];
      CleanAndExtend(workbasis, pivots, ShallowCopy(w), 1);
      if Length(basis) < Length(workbasis) then
        Add(basis, w);
      fi;
      
    od;
    counter:=counter+1;
  od;
  return basis;
end;

PermutedSummands := function(re,plan)
  # re a record coming from ChopDirectSum or ChopDirectSumAfterBurner
  # plan a list of length re.len, containing the numbers of summands in
  # the order as they should appear in the resulting module.
  # Changes re in place.
  local basperm,baspermi,i,j,newgens,pos;

  if Set(plan) <> [1..re.len] then
      Error("Seid Ihr denn??? plan must describe a permutation!");
  fi;

  newgens := List(re.gens,x->0*x);
  for i in [1..Length(re.gens)] do
      if not(IsMutable(newgens[i])) then
          newgens[i] := MutableCopyMat(newgens[i]);    # Eiertanz
      fi;
  od;
  
  basperm := [];   # Here we collect the basis permutation, such that in
                   # the end, we have to do:
                   #   basnew := bas{basperm}
  pos := 0;
  for i in plan do
      for j in [1..Length(re.gens)] do
          newgens[j]{[pos+1..pos+re.dimsfacts[i]]}
                    {[pos+1..pos+re.dimsfacts[i]]} := 
               re.gens[j]{[re.dimssubs[i]+1..re.dimssubs[i]+re.dimsfacts[i]]}
                         {[re.dimssubs[i]+1..re.dimssubs[i]+re.dimsfacts[i]]};
          basperm{[pos+1..pos+re.dimsfacts[i]]} := 
               [re.dimssubs[i]+1..re.dimssubs[i]+re.dimsfacts[i]];
      od;
      pos := pos + re.dimsfacts[i];
  od;
  re.gens := newgens;
  re.bas := re.bas{basperm};
  ConvertToMatrixRep(re.bas);
  re.dimsfacts := re.dimsfacts{plan};
  if IsBound(re.isotypes) then
      re.isotypes := re.isotypes{plan};
  fi;
  re.dimssubs := [0];
  for i in [1..re.len] do
      re.dimssubs[i+1] := re.dimssubs[i] + re.dimsfacts[i];
  od;
  if IsBound(re.representatives) then
      re.representatives := List(re.representatives,i->Position(plan,i));
  fi;
  re.basi := List(re.basi,v->v{basperm});
  ConvertToMatrixRep(re.basi);
  re.firstwithdim := List(Set(re.dimsfacts),
                          x->[x,First([1..re.len],i->re.dimsfacts[i]=x)]);
  return re;
end;



ChopDirectSumAfterBurner := function(re)
  # re a result record from ChopDirectSum
  local B1,B1i,OurSorter,PrepareComparison,current,d,dims,i,iso,isotypes,j,l,
        len,li,m1,newgens,plan,pos,prototypes,typenr,OurIsomorphism;

  len := re.len;
  l := List([1..len],i->RestrictToSummands(re,[i]));
  dims := Set(re.dimsfacts);
  isotypes := 0 * [1..len];
  prototypes := 0 * [1..len];
  typenr := 0;

  PrepareComparison := function(gens1)
    # Prepares everything to compare other with this one:
    local B1,F,m1,merkil,v1;
    
    # make module with gens1 to obtain identifying word
    F := Field(gens1[1][1]);
    merkil := InfoLevel(InfoMeatAxe);
    SetInfoLevel(InfoMeatAxe,0);
    m1 := GModuleByMats(gens1,F);
    MTX.GoodElementGModule( m1 );
    SetInfoLevel(InfoMeatAxe,merkil);

    # Now to some serious spinning
    # do sb for first module first
    v1 := MTX.AlgElNullspaceVec(m1);
    B1 := ZSB( gens1, v1 );
    ConvertToMatrixRep(B1);

    return [m1,B1];
  end;

  OurIsomorphism:=function(gens1, m1, gens2)
    # gens1 already in standard basis
    local B2,B2i,M,N,dim,el,fac,genpair,i,mat,ngens,orig_ngens,p;

    # Now prepare word in second representation
    dim:=MTX.Dimension(m1);
    el:=MTX.AlgEl(m1);
    ngens := Length(gens1);
    orig_ngens := ngens;
    
    # Build Idword in second module
    for genpair in el[1] do
      ngens:=ngens + 1;
      gens2[ngens]:=gens2[genpair[1]] * gens2[genpair[2]];
    od;
    M:=0*gens2[1];
    if not(IsMutable(M)) then
        M := MutableCopyMat(M);    # Eiertanz
    fi;
    for i in [1..ngens] do
      M:=M + el[2][i] * gens2[i];
    od;
    # Having done that, we no longer want the extra generators of module2, 
    # so we throw them away again.
    for i in [orig_ngens + 1..ngens] do
      Unbind (gens2[i]);
    od;

    #Calculate characteristic polynomial for second module
    p := CharacteristicPolynomialMatrixNC (Field(M[1]),M,1);
    if p <> MTX.AlgElCharPol(m1) then
      return fail;
    fi;
    fac:=MTX.AlgElCharPolFac(m1);
    mat:=Value(fac, M,M^0);
    # Calculate nullspace for second module
    N:=NullspaceMat (mat);
    if Length(N) <> MTX.AlgElNullspaceDimension(m1) then
      return fail;
    fi;
    
    # now do SB for second module
    # Note that any nonzero vector of nullspace will suffice
    B2 := ZSB( gens2, N[1] );
    ConvertToMatrixRep(B2);
    
    # If A1 is a representing matrix in a basis for the first module 
    # and A2 likewise for the second, then we should now have:
    # A1 = B2 * A2 * B2^-1
    B2i := B2 ^ -1;
    for i in [1..orig_ngens] do
        if gens1[i] <> B2 * gens2[i] * B2i then
            return fail;
        fi;
    od;
    return B2;
  end;


  Print("Occuring dimensions: ",dims,"\n");
    
  for d in dims do
      Print("Handling summands of dimension ",d,"...                   \n");
      for i in [1..len] do
          if re.dimsfacts[i] = d and isotypes[i] = 0 then
              typenr := typenr + 1;
              isotypes[i] := typenr;
              li := PrepareComparison(l[i]);
              m1 := li[1];
              B1 := li[2];
              B1i := B1^-1;
              l[i] := List(l[i],x->B1 * x * B1i);
              prototypes[i] := i;
              re.bas{[re.dimssubs[i]+1..re.dimssubs[i]+re.dimsfacts[i]]}
               :=B1*re.bas{[re.dimssubs[i]+1..re.dimssubs[i]+re.dimsfacts[i]]};
              for j in [i+1..len] do
                  if re.dimsfacts[j] = d and isotypes[j] = 0 then
                      Print("Comparing summands ",i," and ",j," of ",len,
                            "...     \r");
                      iso := OurIsomorphism(l[i],m1,l[j]);
                      if iso <> fail then
                          isotypes[j] := typenr;
                          prototypes[j] := i;
                          re.bas{[re.dimssubs[j]+1..
                                           re.dimssubs[j]+re.dimsfacts[j]]} :=
                             iso
                             *re.bas{[re.dimssubs[j]+1..
                                           re.dimssubs[j]+re.dimsfacts[j]]};
                      fi;
                  fi;
              od;
          fi;
      od;
  od;
  Print("\n");

  OurSorter := function(a,b)
    if re.dimsfacts[a] < re.dimsfacts[b] then 
        return true;
    elif re.dimsfacts[a] > re.dimsfacts[b] then
        return false;
    else
        return isotypes[a] < isotypes[b];
    fi;
  end;

  plan := [1..len];
  Sort(plan,OurSorter);

  newgens := List(re.gens,x->0*x);
  for i in [1..Length(re.gens)] do
      if not(IsMutable(newgens[i])) then
          newgens[i] := MutableCopyMat(newgens[i]);    # Eiertanz
      fi;
  od;
  
  pos := 0;
  for i in [1..re.len] do
      for j in [1..Length(re.gens)] do
          newgens[j]{[pos+1..pos+re.dimsfacts[i]]}
                    {[pos+1..pos+re.dimsfacts[i]]} := 
               l[prototypes[i]][j];
      od;
      pos := pos + re.dimsfacts[i];
  od;
  re.basi := re.bas^-1;
  re.gens := newgens;
  re.isotypes := isotypes;
  PermutedSummands(re,plan);
  re.representatives := [];
  current := 0;
  for i in [1..re.len] do
      if re.isotypes[i] <> current then
          Add(re.representatives,i);
          current := re.isotypes[i];
      fi;
  od;
  return re;
end;


  
  
DirectSumRepresentations := function(arg)
  # arg must be a list of lists of the same generators of a group in
  # different matrix representations. The result is again a list of
  # the same generators in the direct sum representation.
  local M,MM,c,d,i,j,l,ll,res,sum,v;
  ll := Length(arg);
  l := Length(arg[1]);
  d := List(arg,x->Length(x[1]));
  c := [0];
  sum := 0;
  for i in [1..ll] do
      sum := sum + d[i];
      Add(c,sum);
  od;
  res := [];
  M := [];
  v := Zero(arg[1][1][1][1])*[1..sum];
  ConvertToVectorRep(v);
  for i in [1..sum] do
      Add(M,ShallowCopy(v));
  od;
  for i in [1..l] do
      MM := List(M,ShallowCopy);
      ConvertToMatrixRep(MM);
      for j in [1..ll] do
          MM{[1+c[j]..c[j+1]]}{[1+c[j]..c[j+1]]} := arg[j][i];
      od;
      Add(res,MM);
  od;
  return res;
end;

TracesSummand := function(re,nr)
  return List([1..Length(re.gens)],i->Sum([1..re.dimsfacts[nr]],
              j->re.gens[i][j+re.dimssubs[nr]][j+re.dimssubs[nr]]));
end;

VisualizeMatrix := function(f,m)
  local i,j,pnmtopng,s,sp,st,z;
  pnmtopng := Filename(DirectoriesSystemPrograms(), "pnmtopng"); 
  z := Length(m);
  sp := Length(m[1]);
  st := "";
  s := OutputTextString(st,false);
  PrintTo(s,"P1\n",sp," ",z,"\n");
  for i in [1..z] do
      for j in [1..sp] do
          if IsZero(m[i][j]) then
              PrintTo(s,"0 ");
          else
              PrintTo(s,"1 ");
          fi;
      od;
      PrintTo(s,"\n");
  od;
  CloseStream(s);
  FileString(Concatenation(f,".pbm"),st);
  Exec(Concatenation("pnmtopng ",f,".pbm >",f,".png"));
  Exec(Concatenation("rm ",f,".pbm"));
  Exec(Concatenation("gqview ",f,".png &"));
end;

