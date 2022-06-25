#############################################################################
##
##                             orb package
##  search.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Declaration stuff for searching in groups.
##
#############################################################################

# Making lists of random vectors:
DeclareGlobalFunction( "MakeRandomVectors" );
DeclareGlobalFunction( "MakeRandomLines" );


######################
# Product replacers: #
######################

BindGlobal( "ProductReplacersFamily", NewFamily( "ProductReplacersFamily" ) );
DeclareCategory( "IsProductReplacer", IsComponentObjectRep);

# The constructors:
DeclareOperation( "ProductReplacer", [IsList, IsRecord] );
DeclareOperation( "ProductReplacer", [IsList] );
DeclareOperation( "ProductReplacer", [IsGroup, IsRecord] );
DeclareOperation( "ProductReplacer", [IsGroup] );

# Usage:
DeclareOperation( "Next", [IsProductReplacer] );
DeclareOperation( "Reset", [IsProductReplacer] );
DeclareOperation( "AddGeneratorToProductReplacer",
                  [IsProductReplacer,IsObject] );


#####################
# Random searchers: #
#####################

BindGlobal( "RandomSearchersFamily", NewFamily( "RandomSearchersFamily" ) );
DeclareCategory( "IsRandomSearcher", IsComponentObjectRep);

# The constructors:
DeclareOperation( "RandomSearcher", [IsList, IsFunction, IsRecord] );
DeclareOperation( "RandomSearcher", [IsList, IsFunction] );

# Usage:
DeclareOperation( "Search", [IsRandomSearcher] );
DeclareOperation( "Reset", [IsRandomSearcher] );


###################################################
# Involution centralisers and the dihedral trick: #
###################################################

DeclareGlobalFunction( "FindInvolution" );
DeclareGlobalFunction( "FindCentralisingElementOfInvolution" );
DeclareGlobalFunction( "FindInvolutionCentraliser" );
DeclareGlobalFunction( "ReduceNumberOfGeneratorsUsingRecog" );

DeclareGlobalFunction( "ClassMaker" );


###########################
# Finding nice quotients: #
###########################

DeclareGlobalFunction( "OrbitStatisticOnVectorSpace" );
DeclareGlobalFunction( "OrbitStatisticOnVectorSpaceLines" );


############################################
# Finding short generators of a subgroup : #
############################################

DeclareGlobalFunction( "ORB_PowerSet" );
DeclareGlobalFunction( "ORB_SLPLineFromWord" );
DeclareOperation( "FindShortGeneratorsOfSubgroup", 
  [ IsGroup, IsGroup, IsObject ] );
DeclareOperation( "FindShortGeneratorsOfSubgroup", [ IsGroup, IsGroup ] );


##############################################################
# Helpers for permutation characters for certain operations: #
##############################################################

DeclareGlobalFunction( "NumberFixedVectors" );
DeclareGlobalFunction( "NumberFixedLines" );
DeclareGlobalFunction( "SpacesOfFixedLines" );


##################################################
# Helpers for making short SLPs from word lists: #
##################################################

DeclareGlobalFunction( "SLPForWordList" );


#############################################################################
# A generic way to find stabilizers:
#############################################################################

DeclareGlobalFunction( "ORB_EstimatePermGroupSize" );
DeclareGlobalFunction( "ORB_FindStabilizerMC" );
DeclareGlobalFunction( "ORB_FindNeedleMappers" );

############################################################################
# A method to find transversals in matrix groups:
############################################################################

DeclareGlobalFunction( "FindWordsForRightTransversal" );
DeclareGlobalFunction( "FindWordsForLeftTransversal" );

############################################################################
# Find transforming matrices:
############################################################################

DeclareGlobalFunction( "TransformingMatsLSE" );
DeclareGlobalFunction( "TransformingMats" );

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
