#############################################################################
##
##  orbits.gd              orb package                       Max Neunhoeffer
##                                                              Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Declaration stuff for fast standard orbit enumeration.
##
#############################################################################


DeclareInfoClass( "InfoOrb" );
SetInfoLevel( InfoOrb, 1 );

DeclareCategory( "IsOrbit", IsComponentObjectRep and IsDenseList );
DeclareFilter( "IsClosed", IsOrbit );
DeclareRepresentation( "IsPermOnIntOrbitRep", IsOrbit, [] );
DeclareRepresentation( "IsHashOrbitRep", IsOrbit, [] );
DeclareFilter( "WithStoringNumbers" );
DeclareFilter( "WithSchreierTree" );
DeclareFilter( "WithPermStabilizer" );
DeclareFilter( "WithMatStabilizer" );
DeclareFilter( "LookingForUsingList" );
DeclareFilter( "LookingForUsingHash" );
DeclareFilter( "LookingForUsingFunc" );
DeclareOperation( "LookFor", [ IsOrbit, IsObject ] );

DeclareGlobalFunction( "InitOrbit" );

DeclareOperation( "Enumerate", [ IsOrbit, IsCyclotomic ] );
DeclareOperation( "Enumerate", [ IsOrbit ] );
DeclareOperation( "TraceSchreierTreeBack", [ IsOrbit, IsPosInt ] );
DeclareOperation( "TraceSchreierTreeForward", [ IsOrbit, IsPosInt ] );
DeclareOperation( "EvaluateWord", [ IsList, IsList ] );
DeclareOperation( "ActWithWord", [IsList, IsList, IsFunction, IsObject] );
DeclareOperation( "ActionOnOrbit", [IsOrbit and IsClosed, IsList] );

DeclareGlobalFunction( "LoadQuotFinder" );
