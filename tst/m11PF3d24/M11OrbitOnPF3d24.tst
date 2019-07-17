gap> START_TEST("M11OrbitOnPF3d24.tst");

#
gap> LoadPackage("atlasrep", false);
true
gap> LoadPackage("cvec", false);
true

#
gap> pgens := AtlasGenerators([ "M11", [ "M11G1-p11B0.m1", "M11G1-p11B0.m2" ], 1, 11 ]).generators;
[ (2,10)(4,11)(5,7)(8,9), (1,4,3,8)(2,5,6,9) ]
gap> gens := AtlasGenerators([ "M11", [ "M11G1-f3r24B0.m1", "M11G1-f3r24B0.m2" ], 1, 3 ]).generators;
[ < immutable compressed matrix 24x24 over GF(3) >, 
  < immutable compressed matrix 24x24 over GF(3) > ]
gap> cgens := List(gens,CMat);
[ <cmat 24x24 over GF(3,1)>, <cmat 24x24 over GF(3,1)> ]
gap> basech := CVEC_ReadMatFromFile(Filename(DirectoriesPackageLibrary("orb",""),
>      "examples/m11PF3d24/m11basech.cmat"));
<cmat 24x24 over GF(3,1)>
gap> basechi := basech^-1;;
gap> cgens := List(cgens, x -> basech * x * basechi);;

#
gap> ReadPackage("orb","examples/m11PF3d24/m11slps.g");
true
gap> pgu2 := ResultOfStraightLineProgram(s2,pgens);
[ (1,4)(2,10)(3,7)(6,9), (1,6,10,7,11,3,9,2)(4,5) ]
gap> pgu1 := ResultOfStraightLineProgram(s1,pgu2);
[ (1,4,11,2,7)(3,5,6,9,10), (1,7,11,2)(3,10,6,9) ]
gap> cu2 := ResultOfStraightLineProgram(s2,cgens);
[ <cmat 24x24 over GF(3,1)>, <cmat 24x24 over GF(3,1)> ]
gap> cu1 := ResultOfStraightLineProgram(s1,cu2);
[ <cmat 24x24 over GF(3,1)>, <cmat 24x24 over GF(3,1)> ]

#
gap> setup := OrbitBySuborbitBootstrapForLines(
>             [cu1,cu2,cgens],[pgu1,pgu2,pgens],[20,720,7920],[5,11],rec());
#I  Calculating stabilizer chain for whole group...
#I  Trying smaller degree permutation representation for U2...
#I  Computing a base for helper subgroup #2
#I  Trying smaller degree permutation representation for U1...
#I  Found one on 5 points.
#I  Computing a base for helper subgroup #1
#I  Enumerating permutation base images of U_1...
#I  Looking for U1-coset-recognising U2-orbit in factor space...
#I  New stabilizer order: 3 (l=2)
#I  New stabilizer order: 9 (l=2)
#I  OrbitBySuborbit found 100% of a U2-orbit of size 80
#I  Found 4 suborbits (need 36)
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  Found 36 suborbits (need 36)
<setup for an orbit-by-suborbit enumeration, k=2>
gap> setup!.stabchainrandom := 900;
900

#
gap> v := ZeroMutable(cgens[1][1]);
<cvec over GF(3,1) of length 24>
gap> Randomize(v);
<cvec over GF(3,1) of length 24>
gap> ORB_NormalizeVector(v);
<cvec over GF(3,1) of length 24>

#
gap> o := OrbitBySuborbit(setup,v,3,3,2,100);
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 360
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  OrbitBySuborbit found 100% of a U2-orbit of size 720
#I  Enough hits of stabiliser, assuming it is complete...
#I  OrbitBySuborbit found 100% of a U3-orbit of size 7 920
<orbit-by-suborbit size=7920 stabsize=1 saving factor=660>

#
gap> STOP_TEST("M11OrbitOnPF3d24.tst", 1);
