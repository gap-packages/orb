# Enumerate orbits in the linear action of 2.Co1 on F_5^{24} given by
# the 5-modular matrix representation of 2.Co1.
#
# Packages needed by this example:
#   IO, cvec
# Files needed by this example:
#   co1basech
#   slpco1.g
# Memory needed by this example: at least 600MB
#
LoadPackage("cvec");
#SetInfoLevel(InfoOrb,2);  # to see more
ReadPackage("orb","examples/co1F5d24/slpco1.g");
gens := AtlasGenerators([ "2.Co1", [ "2Co1G1-f5r24B0.m1", "2Co1G1-f5r24B0.m2" ], 1, 5 ]).generators;
cgens := List(gens,CMat);
basech := CVEC_ReadMatFromFile(
       Filename(DirectoriesPackageLibrary("orb",""),
                "examples/co1F5d24/co1basech.cmat"));
basechi := basech^-1;
cgens := List(cgens,x->basech*x*basechi);
cmax5 := ResultOfStraightLineProgram(slpmax5,cgens);
cu3 := cmax5;
cu2 := ResultOfStraightLineProgram(slpc4a,cmax5);
cu1 := ResultOfStraightLineProgram(slpstab10752,cu2);
pg := AtlasGenerators([ "2.Co1", [ "2Co1G1-p196560B0.m1", "2Co1G1-p196560B0.m2" ], 1, 196560 ]).generators;
pgmax5 := ResultOfStraightLineProgram(slpmax5,pg);
pgu3 := pgmax5;
pgu2 := ResultOfStraightLineProgram(slpc4a,pgmax5);
pgu1 := ResultOfStraightLineProgram(slpstab10752,pgu2);
2co1 := CharacterTable("2.Co1");
setup := OrbitBySuborbitBootstrapForVectors(
    [cu1,cu2,cu3,cgens],
    [pgu1,pgu2,pgu3,pg],
    [21504,743178240,178362777600,Size(2co1)],
    [8,8,16],rec());
#setup!.stabchainrandom := 900;
v := ZeroMutable(cgens[1][1]);
Randomize(v);
Print("Now do\n  o := OrbitBySuborbit(setup,v,4,4,3,51);\n");
