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


# Our info class:
DeclareInfoClass( "InfoOrb" );
SetInfoLevel( InfoOrb, 1 );

# A central place for configurations:
DeclareGlobalVariable( "ORB" );


####################################
# Declarations for standard orbits:
####################################

# A new category of objects:
DeclareCategory( "IsOrbit", IsComponentObjectRep and IsDenseList and
                            IsFinite and IsSmallList);

# Indicates, whether the orbit is already completely enumerated:
DeclareFilter( "IsClosed", IsOrbit );

# We have different representations, because we handle the case of perms
# on numbers differently:
DeclareRepresentation( "IsPermOnIntOrbitRep", IsOrbit, [] );
DeclareRepresentation( "IsHashOrbitRep", IsOrbit, [] );

# The following filters indicate, what extra stuff is computed/stored:
DeclareFilter( "WithStoringNumbers" );     # for later lookup of points
DeclareFilter( "WithSchreierTree" );       # Schreier tree
DeclareFilter( "WithPermStabilizer" );     # Stabiliser as perm group
DeclareFilter( "WithMatStabilizer" );      # Stabiliser as matrix group
DeclareFilter( "LookingForUsingList" );    # for searching
DeclareFilter( "LookingForUsingHash" );    # for searching
DeclareFilter( "LookingForUsingFunc" );    # for searching

# Now the constructor method:
DeclareGlobalFunction( "Orb" );
DeclareGlobalFunction( "InitOrbit" );  # the original name, still works

# Orbit enumeration is triggered by "Enumerate":
DeclareOperation( "Enumerate", [ IsOrbit, IsCyclotomic ] );
DeclareOperation( "Enumerate", [ IsOrbit ] );
DeclareGlobalFunction( "ORB_MakeSchreierGeneratorPerm" );

# Later addition of a generator to an orbit:
DeclareOperation( "AddGeneratorToOrbit", [ IsOrbit, IsObject ] );

# This is for the searching infrastructure, an operation to decide
# whether a newly found is one of the points we are looking for:
DeclareOperation( "LookFor", [ IsOrbit, IsObject ] );

# Things to get information out of an orbit enumeration:
DeclareOperation( "TraceSchreierTreeBack", [ IsOrbit, IsPosInt ] );
DeclareOperation( "TraceSchreierTreeForward", [ IsOrbit, IsPosInt ] );
DeclareOperation( "EvaluateWord", [ IsList, IsList ] );
DeclareOperation( "ActWithWord", [IsList, IsList, IsFunction, IsObject] );
DeclareOperation( "StabWords", [ IsOrbit ] );
DeclareOperation( "PositionOfFound", [ IsOrbit ] );

# To calculate the action on the orbit:
DeclareOperation( "ActionOnOrbit", [IsOrbit and IsClosed, IsList] );

# A helper function for base image computations:
DeclareGlobalFunction( "ORB_SiftBaseImage" );
DeclareGlobalFunction( "ORB_ComputeStabChain" );

  
#######################################################################
# The following loads the sub-package "QuotFinder":
# Note that this requires other GAP packages, which are automatically
# loaded by this command if available.
#######################################################################
DeclareGlobalFunction( "LoadQuotFinder" );

