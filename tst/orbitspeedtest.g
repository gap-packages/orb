# mygap4currL -m 500m
LoadPackage("orb");
gens := AtlasGenerators("HN",8).generators;                         
s := AtlasStraightLineProgram("HN",1).program;
ugens := ResultOfStraightLineProgram(s,gens);
guck := List(ugens,x->x-One(x));
guck := List(guck,NullspaceMat);
v := SumIntersectionMat(guck[1],guck[2])[2][1];
v*ugens[1]=v;
v*ugens[2]=v;
o := Orb(gens,v,OnRight,rec( hashlen := 2000000, report := 100000,
                             storenumbers := true ));
Enumerate(o);
time;
LoadPackage("cvec");
cgens := List(gens,CMat);
cv := CVec(v);
Unbind(o);
GASMAN("collect");
o := Orb(cgens,cv,OnRight,rec( hashlen := 2000000, report := 100000,
                               storenumbers := true ));
Enumerate(o);
time;

