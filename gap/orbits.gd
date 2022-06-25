#############################################################################
##
##                             orb package
##  orbits.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Declaration stuff for fast standard orbit enumeration.
##
#############################################################################


# Our info class:
DeclareInfoClass( "InfoOrb" );
SetInfoLevel( InfoOrb, 1 );

# A central place for configurations:
BindGlobal( "ORB", rec( ) );


####################################
# Declarations for standard orbits:
####################################

# A new category of objects:
DeclareCategory( "IsOrbit", IsComponentObjectRep and IsDenseList and
                            IsFinite and IsSmallList );

BindGlobal("OrbitFamily", NewFamily("OrbitFamily", IsOrbit));

# Indicates, whether the orbit is already completely enumerated:
DeclareFilter( "IsClosedOrbit", IsOrbit );
# for backwards compatibility and convenience, declare an operation
# IsClosed which just returns IsClosedOrbit. This way, other packages
# can also use the term "IsClosed" for their purposes.
DeclareOperation( "IsClosed", [ IsOrbit ] );

# We have different representations, because we handle the case of perms
# on numbers differently and have a slow version for nasty cases:
DeclareRepresentation( "IsPermOnIntOrbitRep", IsOrbit, [] );
DeclareRepresentation( "IsHashOrbitRep", IsOrbit, [] );
DeclareRepresentation( "IsSlowOrbitRep", IsOrbit, [] );
DeclareRepresentation( "IsGradedOrbit", IsOrbit, [] );
DeclareFilter( "IsOrbitWithLog" );

# Now the constructor method:
DeclareGlobalFunction( "Orb" );

# Orbit enumeration is triggered by "Enumerate":
DeclareOperation( "Enumerate", [ IsOrbit, IsCyclotomic ] );
DeclareOperation( "Enumerate", [ IsOrbit ] );
DeclareGlobalFunction( "ORB_MakeSchreierGeneratorPerm" );

# Later addition of generators to an orbit:
DeclareOperation( "AddGeneratorsToOrbit", [ IsOrbit, IsList ] );
DeclareOperation( "AddGeneratorsToOrbit", [ IsOrbit, IsList, IsList ] );
DeclareOperation( "MakeSchreierTreeShallow", [ IsOrbit, IsPosInt ] );
DeclareOperation( "MakeSchreierTreeShallow", [ IsOrbit ] );

# This is for the searching infrastructure, some functions that check
# whether a newly found is one of the points we are looking for:
DeclareGlobalFunction( "ORB_LookForList" );
DeclareGlobalFunction( "ORB_LookForHash" );
DeclareGlobalFunction( "ORB_CheckGradeForHash" );

# Things to get information out of an orbit enumeration:
DeclareOperation( "TraceSchreierTreeBack", [ IsOrbit, IsPosInt ] );
DeclareOperation( "TraceSchreierTreeForward", [ IsOrbit, IsPosInt ] );
DeclareOperation( "EvaluateWord", [ IsList, IsList ] );
DeclareOperation( "ActWithWord", [IsList, IsList, IsFunction, IsObject] );
DeclareOperation( "StabWords", [ IsOrbit ] );
DeclareOperation( "PositionOfFound", [ IsOrbit ] );
DeclareOperation( "DepthOfSchreierTree", [ IsOrbit ] );
DeclareOperation( "Grades", [ IsOrbit ] );
DeclareOperation( "OrbitGraph", [ IsOrbit ] );
DeclareOperation( "OrbitGraphAsSets", [ IsOrbit ] );
DeclareOperation( "UnderlyingPlist", [ IsOrbit ] );

# To calculate the action on the orbit:
DeclareOperation( "ActionOnOrbit", [IsOrbit and IsClosedOrbit, IsList] );
DeclareGlobalFunction( "ORB_ActionOnOrbitIntermediateHash" );
DeclareGlobalFunction( "ORB_ActionHomMapper" );
DeclareOperation( "OrbActionHomomorphism", [IsGroup, IsOrbit and IsClosedOrbit] );

# A helper function for base image computations:
DeclareGlobalFunction( "ORB_SiftBaseImage" );
DeclareGlobalFunction( "ORB_ComputeStabChain" );

# A generic way to find out about the memory needed by an object:
DeclareOperation( "Memory", [IsObject] );
  
# Things to work with suborbits:
DeclareOperation( "FindSuborbits", [ ] );
DeclareOperation( "FindSuborbits", [ IsOrbit, IsList ] );
DeclareOperation( "FindSuborbits", [ IsOrbit, IsList, IsCyclotomic ] );
DeclareOperation( "FindSuborbits", [ IsOrbit, IsList, IsFunction ] );
DeclareOperation( "FindSuborbits", 
  [ IsOrbit, IsList, IsFunction, IsCyclotomic ] );
DeclareOperation( "OrbitIntersectionMatrix", [ IsRecord, IsObject ] );
DeclareOperation( "RegularRepresentationSchurBasisElm",
  [ IsRecord, IsList, IsPosInt ] );

# Things that are hooks for other packages:
DeclareOperation( "SizeMC", [ IsGroup, IsRat ] );
DeclareOperation( "SizeMC", [ IsGroup ] );

# To miraculously estimate orbit sizes
DeclareGlobalFunction( "ORB_EstimateOrbitSize" );

#######################################################################
# The following loads the sub-package "QuotFinder":
# Note that this requires other GAP packages, which are automatically
# loaded by this command if available.
#######################################################################
DeclareGlobalFunction( "LoadQuotFinder" );

DeclareGlobalFunction( "MappingPermSetSet" );

##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <https://www.gnu.org/licenses/>.
##
