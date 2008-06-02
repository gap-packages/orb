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
cggens := List(ggens,CMat);

# Now prepare everything for the big moment:
setup := OrbitBySuborbitBootstrapForVectors([cu1gens,cu2gens,cngens],
                         [lll[4],lll[8],ngensp],
                         [3^4,3^8,3265173504],[codim1,codim2],rec());

vv := v;
v := CVec(v);
#v := cngens[1][1];
l := MakeRandomVectors(v,100);
obsol := InitOrbitBySuborbitList(setup,40); 
o := OrbitBySuborbit(setup,v,3,3,2,50);
OrbitsFromSeedsToOrbitList(obsol,l);

