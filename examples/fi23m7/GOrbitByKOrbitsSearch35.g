# This file enumerates N\G by N-orbits, for
#  G = Fi23
#  N = max7 of Fi23
# More precisely, it finds 35 of the 36 N-N-double coset representatives.
# The last was found by thinking!

#
# Do all the preparations:
#
# Read("GOrbitByKOrbitsPrepare.g"); must have been done!
LoadPackage("cvec");

cu1gens := List(u1gens,CMat);
cu2gens := List(u2gens,CMat);
cngens := List(ngens,CMat);

# Now prepare everything for the big moment:
setup := OrbitBySuborbitBootstrapForVectors([cu1gens,cu2gens,cngens],
                                            [lll[4],lll[8],ngensp],
                                            [3^4,3^8],[codim1,codim2]);

v := cngens[1][1];
o := OrbitBySuborbit(v,100000,3265173504,setup,50);
oo := OrbitBySuborbitWithKnownSize(v,100000,373248,setup,30);

# Old:
#dcrs := InitDoubleCosetRepsSearcher(v,3265173504,setup);
# Find the 35 first N orbits by enumerating half of each:
#DoubleCosetRepsSearcher(dcrs,ggens,35,150001);

