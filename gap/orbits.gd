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

BindGlobal( "OrbitsFamily", NewFamily( "OrbitsFamily" ) );
DeclareRepresentation( "IsOrbit", IsComponentObjectRep, [] );
DeclareGlobalVariable( "OrbitsType" );
DeclareFilter( "IsReady", IsOrbit );

DeclareGlobalFunction( "InitOrbit" );

DeclareOperation( "Enumerate", [ IsOrbit, IsCyclotomic ] );


