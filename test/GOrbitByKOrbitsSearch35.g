# This file enumerates N\G by N-orbits, for
#  G = Fi23
#  N = max7 of Fi23
# More precisely, it finds 35 of the 36 N-N-double coset representatives.
# The last was found by thinking!

#
# Do all the preparations:
#
Read("GOrbitByKOrbitsPrepare.g");

# Now prepare everything for the big moment:
setup := OrbitBySuborbitBootstrap([u1gens,u2gens,ngens],
                                  [lll[4],lll[8],ngensp],
                                  [3^4,3^8],[codim1,codim2]);

dcrs := InitDoubleCosetRepsSearcher(v,3265173504,setup);
# Find the 35 first N orbits by enumerating half of each:
DoubleCosetRepsSearcher(dcrs,ggens,35,150001);

