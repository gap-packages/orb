#LoadPackage("chop");
LoadPackage("cvec");
SetInfoLevel(InfoOrb,2);
ReadPackage("orb","examples/slpco1.g");
gens := AtlasGenerators("2.Co1",3).generators;
cgens := List(gens,CMat);
basech := CVEC_ReadMatFromFile("co1basech");
basechi := basech^-1;
cgens := List(cgens,x->basech*x*basechi);
cmax5 := ResultOfStraightLineProgram(slpmax5,cgens);
cu3 := cmax5;
cu2 := ResultOfStraightLineProgram(slpc4a,cmax5);
cu1 := ResultOfStraightLineProgram(slpstab10752,cu2);
pg := AtlasGenerators("Co1",1).generators;
pgmax5 := ResultOfStraightLineProgram(slpmax5,pg);
pgu3 := pgmax5;
pgu2 := ResultOfStraightLineProgram(slpc4a,pgmax5);
pgu1 := ResultOfStraightLineProgram(slpstab10752,pgu2);
co1 := CharacterTable("Co1");
setup := OrbitBySuborbitBootstrapForLines2(
    [cu1,cu2,cu3,cgens],
    [pgu1,pgu2,pgu3,pg],
    [10752,371589120,89181388800,Size(co1)],
    [8,8,16]);
#setup!.stabchainrandom := 900;
v := ZeroMutable(cgens[1][1]);
Randomize(v);
v := v/v[PositionNonZero(v)];
Print("Now do\n  o := OrbitBySuborbit(setup,v,4,4,3,51);\n");
