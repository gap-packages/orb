LoadPackage("orb");
LoadPackage("cvec");
SetInfoLevel(InfoOrb,2);
ReadPackage("orb","examples/m11slps.g");
gens := AtlasGenerators("M11",14).generators;
cgens := List(gens,CMat);
basech := CVEC_ReadMatFromFile(
       Filename(DirectoriesPackageLibrary("orb",""),"examples/m11basech.cmat"));
basechi := basech^-1;
cgens := List(cgens,x->basech*x*basechi);
pgens := AtlasGenerators("M11",1).generators;
cu2 := ResultOfStraightLineProgram(s2,cgens);
cu1 := ResultOfStraightLineProgram(s1,cu2);
pgu2 := ResultOfStraightLineProgram(s2,pgens);
pgu1 := ResultOfStraightLineProgram(s1,pgu2);
setup := OrbitBySuborbitBootstrapForLines(
    [cu1,cu2,cgens],[pgu1,pgu2,pgens],[20,720,7920],[5,11],rec());
setup!.stabchainrandom := 900;
v := ZeroMutable(cgens[1][1]);
Randomize(v);
ORB_NormalizeVector(v);
Print("Now do:   o := OrbitBySuborbit(setup,v,3,3,2,51);\n");


