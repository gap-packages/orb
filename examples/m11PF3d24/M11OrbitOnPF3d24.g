LoadPackage("orb");
LoadPackage("io");
LoadPackage("cvec");
LoadPackage("atlasrep");

SetInfoLevel(InfoOrb,2);
pgens := AtlasGenerators("M11",1).generators;

gens := AtlasGenerators("M11",14).generators;
cgens := List(gens,CMat);
basech := CVEC_ReadMatFromFile(Filename(DirectoriesPackageLibrary("orb",""),
	  "examples/m11PF3d24/m11basech.cmat"));
basechi := basech^-1;
cgens := List(cgens,x->basech*x*basechi);

ReadPackage("orb","examples/m11PF3d24/m11slps.g");
pgu2 := ResultOfStraightLineProgram(s2,pgens);
pgu1 := ResultOfStraightLineProgram(s1,pgu2);
cu2 := ResultOfStraightLineProgram(s2,cgens);
cu1 := ResultOfStraightLineProgram(s1,cu2);

setup := OrbitBySuborbitBootstrapForLines(
         [cu1,cu2,cgens],[pgu1,pgu2,pgens],[20,720,7920],[5,11],rec());
setup!.stabchainrandom := 900;

v := ZeroMutable(cgens[1][1]);
Randomize(v);
ORB_NormalizeVector(v);

Print("Now do\n  o := OrbitBySuborbit(setup,v,3,3,2,100);\n");
