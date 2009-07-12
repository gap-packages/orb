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
Print("Inserting entries by position...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    AVLIndexAdd(t,i+17,true,2*i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Checking status using AVLIndex...\c");
ti := Runtimes().user_time;
lllll := EmptyPlist(n);
for i in [1..n] do
    lllll[i] := AVLIndex(t,2*i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
if lllll <> [1..n]+17 then
    Error("Something is messed up with IndexAdd!");
fi;
Print("Cleaning up the mess using AVLIndexDelete...\c");
ti := Runtimes().user_time;
for i in [n,n-1..1] do
    AVLIndexDelete(t,2*i);
od;
Print("time=",Runtimes().user_time-ti,"\n");
if AVLTest(t).ok <> true then
    Error("Index delete messed up!");
fi;
Print("Checking AVLFindIndex...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    if AVLFindIndex(t,AVLIndex(t,i)) <> i then 
        Error("AVLFindIndex does not work...");
    fi;
od;
Print("time=",Runtimes().user_time-ti,"\n");
Print("Checking Position and ELM_LIST...\c");
ti := Runtimes().user_time;
for i in [1..n] do
    if Position(t,t[i]) <> i then 
        Error("Position or ELM_LIST do not work...");
    fi;
od;
Print("time=",Runtimes().user_time-ti,"\n");

