#
# This is used by
#  GOrbitByKOrbitsSearch35.g
# and
#  GOrbitByKOrbitsVerify.g
# and prepares the groups and representations.
# It uses the files
#   chainworker.g
#   fi23m7.g
# and the MeatAxe ASCII-format matrices
#   fi23.1494.1.t
#   fi23.1494.2.t
#

# The random seed:
Reset(GlobalRandomSource,
[ 45, [ 66318732, 86395905, 22233618, 21989103, 237245480, 264566285, 
      240037038, 264902875, 9274660, 180361945, 94688010, 24032135, 
      106293216, 27264613, 126456102, 243761907, 80312412, 2522186, 59575208, 
      70682510, 228947516, 173992210, 175178224, 250788150, 73030390, 
      210575942, 128491926, 194508966, 201311350, 63569414, 185485910, 
      62786150, 213986102, 88913350, 94904086, 252860454, 247700982, 
      233113990, 75685846, 196780518, 74570934, 7958751, 130274620, 
      247708693, 183364378, 82600777, 28385464, 184547675, 20423483, 
      75041763, 235736203, 54265107, 49075195, 100648387, 114539755 ] ]
);

# First fetch the group in the right representation:
LoadPackage("orb");
LoadPackage("cvec");
LoadPackage("atlasrep");

# Read in some stuff:
ReadPackage("orb","examples/fi23m7/chainworker.g");
ReadPackage("orb","examples/fi23m7/fi23m7.g");     # the slps
Print("Reading matrices...\n");
n1 := Filename(DirectoriesPackageLibrary("orb","examples/fi23m7"),
               "fi23.1494.1.t");
n2 := Filename(DirectoriesPackageLibrary("orb","examples/fi23m7"),
               "fi23.1494.2.t");
ggens := [ScanMeatAxeFile(n1),ScanMeatAxeFile(n2)];
Print("Have fi23 matrices over F2 with dim 1494.\n");
ngens := ResultOfStraightLineProgram(s,ggens);
Print("Have generators of normalizer.\n");
sygens := ResultOfStraightLineProgram(syslp,ngens);
l := List(ngens,x->VectorSpace(GF(2),NullspaceMat(x+x^0)));
i := Intersection(l);
v := GeneratorsOfVectorSpace(i)[1];
Print("Have fixed vector.\n");

l := CalcChain(sygens,sychain,[]);
lllll := l;
u2gens := l[8];
u1gens := l[4];
sr := State(GlobalRandomSource);
SetInfoLevel(InfoMeatAxe,1);
re := ChopDirectSum(u2gens,sychain.r.r.r.r.r);
ChopDirectSumAfterBurner(re);
plan := [1..re.len];
plan := Difference(plan,[52,58,145]);
Append(plan,[52,58,145]);
PermutedSummands(re,plan);
dim := Length(ggens[1]);
codim2 := 30;

Print("Doing first basechange...\n");
ggens := List(ggens,x->re.bas*x*re.basi);
ngens := List(ngens,x->re.bas*x*re.basi);
u2gens := List(u2gens,x->re.bas*x*re.basi);
u1gens := List(u1gens,x->re.bas*x*re.basi);
u1gensf2 := List(u1gens,x->x{[dim-codim2+1..dim]}{[dim-codim2+1..dim]});
v := v * re.basi;
Print("Running MeatAxe 2nd time...\n");

re2 := ChopDirectSum(u1gensf2,sychain.r.r.r.r.r.r.r.r.r);
ChopDirectSumAfterBurner(re2);
plan2 := Difference([1..15],re2.representatives);
Append(plan2,re2.representatives);
PermutedSummands(re2,plan2);
bas := u2gens[1]^0;
bas{[dim-codim2+1..dim]}{[dim-codim2+1..dim]} := re2.bas;
codim1 := 10;
basi := bas^-1;
Print("Doing second basechange...\n");
ggens := List(ggens,x->bas*x*basi);
ngens := List(ngens,x->bas*x*basi);
u2gens := List(u2gens,x->bas*x*basi);
u1gens := List(u1gens,x->bas*x*basi);
v := v * basi;

# adjust to necessities for "OrbitBySuborbit":
ggensp := AtlasGenerators([ "Fi23", [ "F23G1-p31671B0.m1", "F23G1-p31671B0.m2" ], 1, 31671 ]).generators;
ngensp := ResultOfStraightLineProgram(s,ggensp);
Print("Finding smaller degree permutation representation...\n");
ii := SmallerDegreePermutationRepresentation(Group(ngensp):cheap);
Print("done.\n");
ngensp := GeneratorsOfGroup(Image(ii));
sygensp := ResultOfStraightLineProgram(syslp,ngensp);
lll := CalcChain(sygensp,sychain,[]);
u1gensp := lll[4];
u2gensp := lll[8];

for i in [1..Length(u1gens)] do
    u1gens[i] := Reversed(List(u1gens[i],Reversed));
    ConvertToMatrixRep(u1gens[i]);
od;
for i in [1..Length(u2gens)] do
    u2gens[i] := Reversed(List(u2gens[i],Reversed));
    ConvertToMatrixRep(u2gens[i]);
od;
for i in [1..Length(ngens)] do
    ngens[i] := Reversed(List(ngens[i],Reversed));
    ConvertToMatrixRep(ngens[i]);
od;
for i in [1..Length(ggens)] do
    ggens[i] := Reversed(List(ggens[i],Reversed));
    ConvertToMatrixRep(ggens[i]);
od;
v := Reversed(v);

cu1gens := List(u1gens,CMat);
cu2gens := List(u2gens,CMat);
cngens := List(ngens,CMat);
cggens := List(ggens,CMat);
v := CVec(v);

