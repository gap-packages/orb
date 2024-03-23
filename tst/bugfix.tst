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

# verify equal vectors either have equal hashes, or at least one gives fail
# (here for 8 bit vector rep)
gap> v:=[1..100]*Z(5);;
gap> w:=[1..100]*Z(5);;
gap> ConvertToVectorRep(w,5);;
gap> v = w;
true
gap> IsPlistRep(v);
true
gap> Is8BitVectorRep(w);
true
gap> hf:=ChooseHashFunction(w, 1009);;
gap> hf.func(w, hf.data);
799
gap> hf.func(v, hf.data);
fail

# now the other way around
gap> hf:=ChooseHashFunction(v, 1009);;
gap> hf.func(w, hf.data);
651
gap> hf.func(v, hf.data);
651

# same check for GF(2)
gap> v:=[1..100]*Z(2);;
gap> w:=[1..100]*Z(2);;
gap> ConvertToVectorRep(w,2);;
gap> v = w;
true
gap> IsPlistRep(v);
true
gap> IsGF2VectorRep(w);
true
gap> hf:=ChooseHashFunction(w, 1009);;
gap> hf.func(w, hf.data);
573
gap> hf.func(v, hf.data);
fail

# now the other way around
gap> hf:=ChooseHashFunction(v, 1009);;
gap> hf.func(w, hf.data);
446
gap> hf.func(v, hf.data);
446

#
gap> STOP_TEST("Orb package: bugfix.tst", 0);
