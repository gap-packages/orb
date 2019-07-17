gap> START_TEST("Orb package: m22p770.tst");

#
gap> gens := ShallowCopy(AtlasGenerators([ "M22", [ "M22G1-p770B0.m1", "M22G1-p770B0.m2" ], 1, 770 ]).generators);;
gap> pgens := ShallowCopy(AtlasGenerators([ "M22", [ "M22G1-p22B0.m1", "M22G1-p22B0.m2" ], 1, 22 ]).generators);;
gap> v := 2;;

#
gap> sometests := function(o)
>   local l,wo;
>   l := Length(o);
>   if not(o[l] in o) then Error(1); fi;
>   if o!.storenumbers then
>       if Position(o,o[l]) <> l then Error(2); fi;
>       if PositionCanonical(o,o[l]) <> l then Error(3); fi;
>   else
>       if Position(o,o[l]) <> l then Error(2); fi;
>       if PositionCanonical(o,o[l]) <> l then Error(3); fi;
>   fi;
>   if not(IsPermOnIntOrbitRep(o)) then
>       if 0*o[1] in o then Error(4); fi;
>       if Position(o,0*o[1]) <> fail then Error(5); fi;
>       if PositionCanonical(o,0*o[1]) <> fail then Error(6); fi;
>   fi;
>   if o!.schreier then
>       wo := TraceSchreierTreeForward(o,l);
>       if ActWithWord(o!.gens,wo,o!.op,o[1]) <> o[l] then Error(7); fi;
>       wo := TraceSchreierTreeBack(o,l);
>       if ActWithWord(List(o!.gens,x->x^-1),wo,o!.op,o[l]) <> o[1] then
>           Error(8);
>       fi;
>   fi;
> end;;

#
gap> o := Orb(gens,v,OnPoints,1000000);
<open Int-orbit, 1 points>
gap> Enumerate(o);
<closed Int-orbit, 770 points>
gap> sometests(o);

#
gap> o := Orb(gens,v,OnPoints,1000000,rec(orbsizebound := 770));
<open Int-orbit, 1 points>
gap> Enumerate(o);
<closed Int-orbit, 770 points>
gap> sometests(o);

# Now with Schreier tree:
gap> o := Orb(gens,v,OnPoints,1000000,rec(schreier := true));
<open Int-orbit, 1 points with Schreier tree>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree>
gap> sometests(o);

# Now with storing numbers:
gap> o := Orb(gens,v,OnPoints,1000000,rec(schreier := true, storenumbers := true));
<open Int-orbit, 1 points with Schreier tree>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree>
gap> sometests(o);

# Now with stabiliser:
gap> o := Orb(gens,v,OnPoints,1000000,rec(permgens := pgens));
<open Int-orbit, 1 points with Schreier tree and stabilizer>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree and stabilizer>
gap> sometests(o);
gap> StabWords(o);
[ [ 2, 2, 2, 1, -2, -2, -2 ], [ 2, 1, 2, 2, 1, 2, -1, -2, -2, -1, -2 ], 
  [ 2, 1, 2, 1, 2, 2, 1, -2, -2, -1, -2, -1, -2 ] ]
gap> GeneratorsOfGroup(Stabilizer(o));
[ (2,22)(3,11)(5,13)(7,20)(8,9)(14,18)(15,19)(16,21), 
  (1,17)(2,19,22,15)(3,9,11,8)(5,7,13,20)(10,12)(14,21,18,16), 
  (1,14)(2,9)(4,8)(5,15)(6,12)(10,22)(16,17)(19,20) ]

# Now with stabiliser, but with known group size:
gap> o := Orb(gens,v,OnPoints,1000000,
>          rec(permgens := pgens, grpsizebound := 443520));
<open Int-orbit, 1 points with Schreier tree and stabilizer>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree and stabilizer>
gap> sometests(o);
gap> StabWords(o);
[ [ 2, 2, 2, 1, -2, -2, -2 ], [ 2, 1, 2, 2, 1, 2, -1, -2, -2, -1, -2 ], 
  [ 2, 1, 2, 1, 2, 2, 1, -2, -2, -1, -2, -1, -2 ] ]
gap> GeneratorsOfGroup(Stabilizer(o));
[ (2,22)(3,11)(5,13)(7,20)(8,9)(14,18)(15,19)(16,21), 
  (1,17)(2,19,22,15)(3,9,11,8)(5,7,13,20)(10,12)(14,21,18,16), 
  (1,14)(2,9)(4,8)(5,15)(6,12)(10,22)(16,17)(19,20) ]

# Now with stabiliser and known stabiliser size:
gap> o := Orb(gens,v,OnPoints,1000000,
>          rec(permgens := pgens, stabsizebound := 576));
<open Int-orbit, 1 points with Schreier tree and stabilizer>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree and stabilizer>
gap> sometests(o);
gap> StabWords(o);
[ [ 2, 2, 2, 1, -2, -2, -2 ], [ 2, 1, 2, 2, 1, 2, -1, -2, -2, -1, -2 ], 
  [ 2, 1, 2, 1, 2, 2, 1, -2, -2, -1, -2, -1, -2 ] ]
gap> GeneratorsOfGroup(Stabilizer(o));
[ (2,22)(3,11)(5,13)(7,20)(8,9)(14,18)(15,19)(16,21), 
  (1,17)(2,19,22,15)(3,9,11,8)(5,7,13,20)(10,12)(14,21,18,16), 
  (1,14)(2,9)(4,8)(5,15)(6,12)(10,22)(16,17)(19,20) ]

# Now with stabiliser and known orbit length and stabiliser size:
gap> o := Orb(gens,v,OnPoints,1000000,
>          rec(permgens := pgens, stabsizebound := 576, orbsizebound := 770));
<open Int-orbit, 1 points with Schreier tree and stabilizer>
gap> Enumerate(o);
<closed Int-orbit, 770 points with Schreier tree and stabilizer>
gap> sometests(o);
gap> StabWords(o);
[ [ 2, 2, 2, 1, -2, -2, -2 ], [ 2, 1, 2, 2, 1, 2, -1, -2, -2, -1, -2 ], 
  [ 2, 1, 2, 1, 2, 2, 1, -2, -2, -1, -2, -1, -2 ] ]
gap> GeneratorsOfGroup(Stabilizer(o));
[ (2,22)(3,11)(5,13)(7,20)(8,9)(14,18)(15,19)(16,21), 
  (1,17)(2,19,22,15)(3,9,11,8)(5,7,13,20)(10,12)(14,21,18,16), 
  (1,14)(2,9)(4,8)(5,15)(6,12)(10,22)(16,17)(19,20) ]

# Now with stabiliser and known group size bound only stab:
gap> o := Orb(gens,v,OnPoints,1000000,
>          rec(permgens := pgens, grpsizebound := 443520,
>              onlystab := true));
<open Int-orbit, 
1 points with Schreier tree and stabilizer going for stabilizer>
gap> Enumerate(o);
<open Int-orbit, 
387 points with Schreier tree and stabilizer going for stabilizer>
gap> sometests(o);
gap> StabWords(o);
[ [ 2, 2, 2, 1, -2, -2, -2 ], [ 2, 1, 2, 2, 1, 2, -1, -2, -2, -1, -2 ], 
  [ 2, 1, 2, 1, 2, 2, 1, -2, -2, -1, -2, -1, -2 ] ]
gap> GeneratorsOfGroup(Stabilizer(o));
[ (2,22)(3,11)(5,13)(7,20)(8,9)(14,18)(15,19)(16,21), 
  (1,17)(2,19,22,15)(3,9,11,8)(5,7,13,20)(10,12)(14,21,18,16), 
  (1,14)(2,9)(4,8)(5,15)(6,12)(10,22)(16,17)(19,20) ]

#
gap> STOP_TEST("Orb package: m22p770.tst", 0);
