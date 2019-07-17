LoadPackage("orb");
LoadPackage("io");
LoadPackage("cvec");
LoadPackage("atlasrep");
#SetInfoLevel(InfoOrb,2);  # to see more

pg := AtlasGenerators([ "Co1", [ "Co1G1-p98280B0.m1", "Co1G1-p98280B0.m2" ], 1, 98280 ]).generators;
gens := AtlasGenerators([ "2.Co1", [ "2Co1G1-f5r24B0.m1", "2Co1G1-f5r24B0.m2" ], 1, 5 ]).generators;
cgens := List(gens,CMat);
basech := CVEC_ReadMatFromFile(Filename(DirectoriesPackageLibrary("orb",""),
          "examples/co1F5d24/co1basech.cmat"));
basechi := basech^-1;
cgens := List(cgens,x->basech*x*basechi);

ReadPackage("orb","examples/co1F5d24/slpco1.g");
pgmax5 := ResultOfStraightLineProgram(slpmax5,pg);
pgu3 := pgmax5;
pgu2 := ResultOfStraightLineProgram(slpc4a,pgmax5);
pgu1 := ResultOfStraightLineProgram(slpstab10752,pgu2);

cmax5 := ResultOfStraightLineProgram(slpmax5,cgens);
cu3 := cmax5;
cu2 := ResultOfStraightLineProgram(slpc4a,cmax5);
cu1 := ResultOfStraightLineProgram(slpstab10752,cu2);

ORB.MINSHASHLEN := NextPrimeInt(30000);
setup := OrbitBySuborbitBootstrapForLines(
    [cu1,cu2,cu3,cgens],
    [pgu1,pgu2,pgu3,pg],
    [10752,371589120,89181388800,4157776806543360000],
    [8,8,16],rec());

v := ZeroMutable(cgens[1][1]);
Randomize(v);
ORB_NormalizeVector(v);

Print("Now do\n  o := OrbitBySuborbit(setup,v,4,4,3,51);\n");
