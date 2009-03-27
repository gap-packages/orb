LoadPackage("orb");
n := 100000;
t := AVLTree();
l := EmptyPlist(n);
s := EmptyPlist(n);
Print("Creating ",n," different random numbers...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    repeat
        x := Random(-10000000,10000000);
    until not(x in s);
    Add(l,x);
    AddSet(s,x);
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Adding ",n," numbers...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    AVLAdd(t,l[i],i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Finding ",n," numbers...\c");
ti := Runtimes().user_time;
for i in [n,n-1..1] do
    x := AVLFind(t,l[i]);
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Looking up ",n," numbers...\c");
ll := EmptyPlist(n);
ti := Runtimes().user_time;
for i in [n,n-1..1] do
    ll[i] := AVLLookup(t,l[i]);
od;
Print("time=",Runtimes().user_time-ti,"\n");
if ll <> [1..n] then Error("Something is messed up with Lookup!"); fi;
Print("Looking up ",n," numbers by index...\c");
lll := EmptyPlist(n);
ti := Runtimes().user_time;
for i in [1..n] do
    lll[i] := AVLIndexLookup(t,i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Checking result...\c");
if lll <> List(s,x->Position(l,x)) then 
    Error("Something is messed up with IndexLookup!"); 
fi;
Print("done\n");
Print("Accessing ",n," numbers by index...\c");
llll := EmptyPlist(n);
ti := Runtimes().user_time;
for i in [n,n-1..1] do
    llll[i] := AVLIndex(t,i);
od;
if llll <> s then
    Error("Something is messed up with Index!");
fi;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Deleting ",n," numbers and readding them...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    AVLDelete(t,l[i]);
    AVLAdd(t,l[i],i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
if AVLTest(t).ok <> true then 
    Error("Deleting messed things up!");
fi;
Print("Creating a list of all elements in the tree...\c");
ti := Runtimes().user_time;
ss := AVLToList(t);
Print("time=",Runtimes().user_time-ti,"\n");
if List(ss,x->x[1]) <> s then
    Error("AVLToList returned wrong list!");
fi;
