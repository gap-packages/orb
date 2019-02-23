gap> n := 100000;;
gap> t := AVLTree();
<avltree nodes=0 alloc=11>
gap> l := EmptyPlist(n);;
gap> s := EmptyPlist(n);;

# Creating different random numbers...
gap> for i in [1..n] do
>      repeat
>        x := Random(-10000000,10000000);
>      until not (x in s);
>      Add(l,x);
>      AddSet(s,x);
>    od;

# Adding numbers...
gap> for i in [1..n] do AVLAdd(t,l[i],i); od;

# Finding numbers...
gap> for i in [n,n-1..1] do x := AVLFind(t,l[i]); od;

# Looking up numbers...
gap> ll := EmptyPlist(n);;
gap> for i in [n,n-1..1] do ll[i] := AVLLookup(t,l[i]); od;
gap> ll = [1..n];
true

# Looking up numbers by index...
gap> lll := EmptyPlist(n);;
gap> for i in [1..n] do lll[i] := AVLIndexLookup(t,i); od;
gap> lll = List(s,x->Position(l,x));
true

# Accessing numbers by index...
gap> llll := EmptyPlist(n);;
gap> for i in [n,n-1..1] do llll[i] := AVLIndex(t,i); od;
gap> llll = s;
true

# Deleting numbers and readding them...
gap> for i in [1..n] do
>      AVLDelete(t,l[i]);
>      AVLAdd(t,l[i],i);
>    od;
gap> AVLTest(t).ok;
true

# Creating a list of all elements in the tree...
gap> ss := AVLToList(t);;
gap> List(ss,x->x[1]) = s;
true

# Inserting entries by position...
gap> for i in [1..n] do AVLIndexAdd(t,i+17,true,2*i); od;

# Checking status using AVLIndex...
gap> lllll := EmptyPlist(n);;
gap> for i in [1..n] do lllll[i] := AVLIndex(t,2*i); od;
gap> lllll = [1..n]+17;
true

# Cleaning up the mess using AVLIndexDelete...
gap> for i in [n,n-1..1] do AVLIndexDelete(t,2*i); od;
gap> AVLTest(t).ok;
true

# Checking AVLFindIndex...
gap> First([1..n], i -> AVLFindIndex(t,AVLIndex(t,i)) <> i);
fail

# Checking Position and ELM_LIST...
gap> First([1..n], i -> Position(t,t[i]) <> i);
fail
