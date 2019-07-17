# mygap4currL -m 500m
LoadPackage("atlasrep");
LoadPackage("orb");
gens := AtlasGenerators([ "HN", [ "HNG1-f5r133B0.m1", "HNG1-f5r133B0.m2" ], 1, 5 ]).generators;                         
s := AtlasStraightLineProgram([ "HN", "HNG1-max1W1", 1 ]).program;
ugens := ResultOfStraightLineProgram(s,gens);
guck := List(ugens,x->x-One(x));
guck := List(guck,NullspaceMat);
v := SumIntersectionMat(guck[1],guck[2])[2][1];
v*ugens[1]=v;
v*ugens[2]=v;
o := Orb(gens,v,OnRight,rec( treehashsize := 2000000, report := 100000,
                             storenumbers := true ));
Enumerate(o);
time;

t := HTCreate(o[1],rec(treehashsize := 200000));
meths := ApplicableMethod(HTAdd,[t,o[1],1],1,"all");;
l := o!.orbit;;

t := HTCreate(o[1],rec(treehashsize := 200000));
a:=1;;b:=2;;c:=3;; GASMAN("collect");
ti := Runtime();
for i in [1..Length(l)] do
    HTAdd(t,l[i],i);
od;
Print("Time: ",Runtime()-ti,"\n");

t := HTCreate(o[1],rec(treehashsize := 200000));
f := meths[1];
a:=1;b:=2;c:=3; GASMAN("collect");
ti := Runtime();
for i in [1..Length(l)] do
    f(t,l[i],i);
od;
Print("Time: ",Runtime()-ti,"\n");

t := HTCreate(o[1],rec(treehashsize := 200000));
f := meths[2];
a:=1;b:=2;c:=3; GASMAN("collect");
ti := Runtime();
for i in [1..Length(l)] do
    f(t,l[i],i);
od;
Print("Time: ",Runtime()-ti,"\n");

#t := NewHT(o[1],200000);
#a:=1;b:=2;c:=3; GASMAN("collect");
#ti := Runtime();
#for i in [1..Length(l)] do
#    AddHT(t,l[i],i);
#od;
#Print("Time: ",Runtime()-ti,"\n");

ti := Runtime();
for i in [1..Length(l)] do
    if HTValue(t,l[i]) <> i then Error(); fi;
od;
Print("Time: ",Runtime()-ti,"\n");

meths := ApplicableMethod(HTValue,[t,o[1]],1,"all");;

f := meths[1];
ti := Runtime();
for i in [1..Length(l)] do
    if f(t,l[i]) <> i then Error(); fi;
od;
Print("Time: ",Runtime()-ti,"\n");

f := meths[2];
ti := Runtime();
for i in [1..Length(l)] do
    if f(t,l[i]) <> i then Error(); fi;
od;
Print("Time: ",Runtime()-ti,"\n");

LoadPackage("cvec");
cgens := List(gens,CMat);
cv := CVec(v);
Unbind(o);
GASMAN("collect");
GreaseMat(cgens[1]);
GreaseMat(cgens[2]);
o := Orb(cgens,cv,OnRight,rec( hashlen := 2000000, report := 100000,
                               storenumbers := true ));
Enumerate(o);
time;

gens := AtlasGenerators([ "Co1", [ "Co1G1-f2r24B0.m1", "Co1G1-f2r24B0.m2" ], 1, 2 ]).generators;                         
s := AtlasStraightLineProgram([ "Co1", "Co1G1-max3W1", 1 ]).program;
ugens := ResultOfStraightLineProgram(s,gens);
guck := List(ugens,x->x-One(x));
guck := List(guck,NullspaceMat);
v := SumIntersectionMat(guck[1],guck[2])[2][1];
v*ugens[1]=v;
v*ugens[2]=v;
o := Orb(gens,v,OnRight,rec( treehashsize := 100000, report := 1000000,
                             storenumbers := true ));
Enumerate(o);
time;

cgens := List(gens,CMat);
GreaseMat(cgens[1]);
GreaseMat(cgens[2]);
cv := CVec(v);
Unbind(o);
GASMAN("collect");
o := Orb(cgens,cv,OnRight,rec( treehashsize := 100000, report := 1000000,
                               storenumbers := true ));
Enumerate(o);
time;


