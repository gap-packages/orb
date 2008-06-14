
##
LoadPackage("orb");
LoadPackage("io");
LoadPackage("cvec");
LoadPackage("atlasrep");
LoadPackage("chop");
LoadPackage("genss");

##
gens := AtlasGenerators("B",1).generators;
bgens := List(gens,CMat);
slpbtoe := AtlasStraightLineProgram("B",1).program;;
egens := ResultOfStraightLineProgram(slpbtoe,bgens);

##
x := egens[1]-egens[1]^0;;
nsx := NullspaceMat(x);
y := nsx * (egens[2]-egens[2]^0);;
nsy := NullspaceMat(y);
v := nsy[1]*nsx;

##
SetInfoLevel(InfoChop,2);
m := Module(egens);
r := Chop(m);
i := Position(List(r.db,Dimension),78);;
egens78 := GeneratorsWithMemory(RepresentingMatrices(r.db[i]));

##
o := Orb(egens78,StripMemory(egens78[1])^0,OnRight,rec(schreier := true,
         lookingfor := function(o,x) return Order(x)=22; end));
Enumerate(o);
word := TraceSchreierTreeForward(o,PositionOfFound(o));
g2a := Product(egens78{word})^11;
o := Orb(egens78,StripMemory(egens78[1])^0,OnRight,rec(schreier := true,
         lookingfor := function(o,x) return Order(x)=13; end));
Enumerate(o);
word := TraceSchreierTreeForward(o,PositionOfFound(o));
b := Product(egens78{word});

##
pr := ProductReplacer(egens78,rec(maxdepth := 150));
i := 0;;
repeat
      i := i + 1;
      x := Next(pr);
      a := g2a^x;
until IsOne((a*b)^11) and IsOne(((a*b)^4*b*a*b*(a*b*b)^2)^12) and
      IsOne((a*b^2)^21) and IsOne(Comm(a,b)^3) and
      IsOne(Comm(a,b^2)^3) and IsOne(Comm(a,b^3)^3) and
      IsOne(Comm(a,b^4)^2) and IsOne(Comm(a,b^5)^3) and
      IsOne(Comm(a,b*a*b^2)^3) and IsOne(Comm(a,b^-1*a*b^-2)^2) and
      IsOne(Comm(a,b*a*b^5)^2) and IsOne(Comm(a,b^2*a*b^5)^2);
i;

##
S := StabilizerChain(Group(a,b),rec(TryShortOrbit := 30,
                                    OrbitLengthLimit := 5000));
Size(S)=Size(CharacterTable("Fi22"));
p := Group(ActionOnOrbit(S!.orb,[a,b]));;
DisplayCompositionSeries(p);

##
SetInfoLevel(InfoSLP,2);
slpetofi22 := SLPOfElms([a,b]);
Length(LinesOfStraightLineProgram(slpetofi22));
SlotUsagePattern(slpetofi22);;
fgens := ResultOfStraightLineProgram(slpetofi22,egens);
a := fgens[1];;
b := fgens[2];;
IsOne(b^13);
IsOne((a*b)^11);
IsOne((a*b^2)^21);

##
slpfi22tom12 := AtlasStraightLineProgram("Fi22",14).program;;
slpm12tol211 := AtlasStraightLineProgram("M12",5).program;;
mgens := ResultOfStraightLineProgram(slpfi22tom12,fgens);
lgens := ResultOfStraightLineProgram(slpm12tol211,mgens);
m := Module(mgens);;
r := Chop(m);;
rad := RadicalSeries(m,r.db);

##
i := Position(List(rad.db,Dimension),32);;
mgens32 := RepresentingMatrices(rad.db[i]);
OrbitStatisticOnVectorSpace(mgens32,95040,30);

##
bgens := List(bgens,x->rad.basis*x*rad.ibasis);;
egens := List(egens,x->rad.basis*x*rad.ibasis);;
fgens := List(fgens,x->rad.basis*x*rad.ibasis);;
mgens := List(mgens,x->rad.basis*x*rad.ibasis);;
lgens := List(lgens,x->rad.basis*x*rad.ibasis);;
j := Position(rad.isotypes[1],i);;
l := rad.cfposs[1][j];;
Append(l,Difference([1..4370],l));
bgens := List(bgens,x->ORB_PermuteBasisVectors(x,l));;
egens := List(egens,x->ORB_PermuteBasisVectors(x,l));;
fgens := List(fgens,x->ORB_PermuteBasisVectors(x,l));;
mgens := List(mgens,x->ORB_PermuteBasisVectors(x,l));;
lgens := List(lgens,x->ORB_PermuteBasisVectors(x,l));; ]]>

##
lgens32 := List(lgens,x->ExtractSubMatrix(x,[1..32],[1..32]));
m := Module(lgens32);;
r := Chop(m);
soc := SocleSeries(m,r.db);
i := Position(List(soc.db,x->[Dimension(x),DegreeOfSplittingField(x)]),
              [10,1]);;
j := Position(soc.isotypes[1],i);;
l := Concatenation(soc.cfposs[1]{[j,j+1]});;
lgens32 := List(lgens32,x->soc.basis*x*soc.ibasis);
lgens20 := List(lgens32,x->ExtractSubMatrix(x,l,l));
OrbitStatisticOnVectorSpace(lgens20,660,30);

##
t := ORB_EmbedBaseChangeTopLeft(soc.basis,4370);
ti := ORB_EmbedBaseChangeTopLeft(soc.ibasis,4370);
bgens := List(bgens,x->t*x*ti);;
egens := List(egens,x->t*x*ti);;
fgens := List(fgens,x->t*x*ti);;
mgens := List(mgens,x->t*x*ti);;
lgens := List(lgens,x->t*x*ti);;
Append(l,Difference([1..4370],l));
bgens := List(bgens,x->ORB_PermuteBasisVectors(x,l));;
egens := List(egens,x->ORB_PermuteBasisVectors(x,l));;
fgens := List(fgens,x->ORB_PermuteBasisVectors(x,l));;
mgens := List(mgens,x->ORB_PermuteBasisVectors(x,l));;
lgens := List(lgens,x->ORB_PermuteBasisVectors(x,l));; ]]>

##
x := egens[1]-egens[1]^0;;
nsx := NullspaceMat(x);;
y := nsx * (egens[2]-egens[2]^0);;
nsy := NullspaceMat(y);;
v := nsy[1]*nsx;; ]]>

##
mgens32 := List(mgens,x->ExtractSubMatrix(x,[1..32],[1..32]));
S := StabilizerChain(Group(mgens32),rec(TryShortOrbit := 10));
p := Group(ActionOnOrbit(S!.orb,mgens32));
pp := Image(SmallerDegreePermutationRepresentation(p));
m12 := MathieuGroup(12);;
i := IsomorphismGroups(pp,m12);;
mpermgens := List(GeneratorsOfGroup(pp),x->ImageElm(i,x));
lpermgens := ResultOfStraightLineProgram(slpm12tol211,mpermgens);

##
f := IO_File("data.gp","w");;
IO_Pickle(f,"seed");;
IO_Pickle(f,v);;
IO_Pickle(f,"generators");;
IO_Pickle(f,bgens);;
IO_Pickle(f,egens);;
IO_Pickle(f,fgens);;
IO_Pickle(f,mgens);;
IO_Pickle(f,lgens);;
IO_Pickle(f,"permutations");;
IO_Pickle(f,mpermgens);;
IO_Pickle(f,lpermgens);;
IO_Close(f);; ]]>

##
LoadPackage("orb");;
LoadPackage("cvec");;
f := IO_File("data.gp");
IO_Unpickle(f);
v:=IO_Unpickle(f);;
IO_Unpickle(f);
bgens := IO_Unpickle(f);;
egens := IO_Unpickle(f);;
fgens := IO_Unpickle(f);;
mgens := IO_Unpickle(f);;
lgens := IO_Unpickle(f);;
IO_Unpickle(f);
mpermgens := IO_Unpickle(f);;
lpermgens := IO_Unpickle(f);;
IO_Close(f);; ]]>

##

##

##
##
##
