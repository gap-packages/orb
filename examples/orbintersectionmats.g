# This shows how to construct a permutation representation out of
# a matrix representation and a straight line program for the 
# point stabilizer using the Chop, Orb and cvec packages:

# Here we do J2 on the cosets of its 5-Sylow-subgroup:

LoadPackage("chop");

# Find an slp for the 5-Sylow:
pgens := AtlasGenerators("J2",1).generators;
g := GroupWithGenerators(pgens);
sy5 := SylowSubgroup(g,5);
guck := FindShortGeneratorsOfSubgroup(g,sy5,\in);
slp := guck.slp;

# Now to the matrix rep:
gens := AtlasGenerators("J2",9).generators;
hgens := ResultOfStraightLineProgram(slp,gens);

# Make cmats out of the matrices:
gens := List(gens,CMat);
hgens := List(hgens,CMat);

# Chop the module:
m := Module(hgens);
r := Chop(m);
soc := SocleOfModule(m,r.db);

# Take a submodule in the socle:
v := MutableCopyMat(soc.basis{soc.cfposs[1]});

# Now enumerate the orbit:
TriangulizeMat(v);
o := Orb(gens,v,OnSubspacesByCanonicalBasis,
         rec( report := 2000, schreier := true, storenumbers := true, 
              hashlen := 100000 ) );
Enumerate(o);
if Length(o) <> Size(g)/Size(sy5) then
    Error("orbit size not correct");
fi;

# Find the suborbits:
sub := FindSuborbits(o,hgens);

# Now compute the double coset reps:
dcosetreps := List(sub.words,w->Product(gens{w}));

# Orbit intersection matrices:
one := OrbitIntersectionMatrix( sub, dcosetreps[1]);;   # the identity!
guck := OrbitIntersectionMatrix( sub, dcosetreps[2]);;
guck2 := OrbitIntersectionMatrix( sub, dcosetreps[3]);;

# Regular representation matrices:
dcosetrepsi := List(dcosetreps,x->x^-1);
regrep1 := RegularRepresentationSchurBasisElm(sub,dcosetrepsi,1);;
regrep2 := RegularRepresentationSchurBasisElm(sub,dcosetrepsi,2);;
regrep24 := RegularRepresentationSchurBasisElm(sub,dcosetrepsi,24);;

