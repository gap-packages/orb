oo := InitOrbitBySuborbitList(setup,80);
l := MakeRandomLines(v,1000);
OrbitsFromSeedsToOrbitList(oo,l);
intervecs := CVEC_ReadMatFromFile(Filename(DirectoriesPackageLibrary("orb",""),
	     "examples/co1F5d24/co1interestingvecs.cmat"));
OrbitsFromSeedsToOrbitList(oo,intervecs);
Length(oo!.obsos);
Sum(oo!.obsos,Size);
(5^24-1)/(5-1);
