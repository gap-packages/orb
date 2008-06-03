# First read Cd1OrbitOnPF5d24.g, then do:
oo := InitOrbitBySuborbitList(setup,80);
l := MakeRandomLines(v,1000);
OrbitsFromSeedsToOrbitList(oo,l);
fn := Filename(DirectoriesPackageLibrary("orb",""),
               "examples/co1F5d24/co1interestingvecs.cmat");
intervecs := CVEC_ReadMatFromFile(fn);
OrbitsFromSeedsToOrbitList(oo,intervecs);
# Then check whether we are complete:
Length(oo!.obsos);
Sum(oo!.obsos,Size);
(5^24-1)/(5-1);

# Runtime: About 296 minutes on Pentium 4 3,2 GHz
# Memory: Fits in 2GB of main memory.
