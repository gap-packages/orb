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

# Verify crash in https://github.com/gap-packages/orb/issues/29 is fixed
gap> bl := BlistList([1 .. 10000], []);;
gap> ht := HTCreate(bl);;
gap> HTAdd(ht, 1, 1);
Error, hash function not applicable to key of type integer

#
gap> STOP_TEST("Orb package: bugfix.tst", 0);
