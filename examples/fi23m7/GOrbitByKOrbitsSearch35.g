LoadPackage("cvec");

cu1gens := List(u1gens,CMat);
cu2gens := List(u2gens,CMat);
cngens := List(ngens,CMat);
cggens := List(ggens,CMat);
v := CVec(v);

setup := OrbitBySuborbitBootstrapForVectors(
	 [cu1gens,cu2gens,cngens],[u1gensp,u2gensp,ngensp],
         [81,6561,3265173504],[10,30],rec());

obsol := InitOrbitBySuborbitList(setup,40); 
l := Orb(cggens,v,OnRight,rec(schreier := true));
Enumerate(l,100000);
OrbitsFromSeedsToOrbitList(obsol,l);
origseeds := List(obsol,OrigSeed);
positions :=  List(origseeds,x->Position(l,x));
words := List(positions,x->TraceSchreierTreeForward(l,x));

# Note that this finds only 35 of the representatives!
o := OrbitBySuborbit(setup,v,3,3,2,50);
Enumerate(l,100000);
