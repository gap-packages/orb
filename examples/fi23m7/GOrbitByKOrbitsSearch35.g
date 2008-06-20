# Note that this finds only 35 of the representatives!
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

