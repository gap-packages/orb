gap> START_TEST("Orb package: bugfix.tst");

# See https://github.com/gap-packages/orb/issues/10
gap> S := Semigroup(PartialPerm([]));;
gap> o := Orb(GeneratorsOfSemigroup(S), [1], OnSets);
<open orbit, 1 points>
gap> Enumerate(o);
<closed orbit, 2 points>
gap> AsList(o);
[ [ 1 ], [  ] ]
gap> [] in o;
true

#
gap> STOP_TEST("Orb package: bugfix.tst", 0);
