#############################################################################
##
##  search.gd           orb package 
##                                                        by Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
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
DeclareCategory( "IsProductReplacer", IsComponentObjectRep );
DeclareGlobalVariable( "ProductReplacersType" );

# The constructors:
DeclareOperation( "ProductReplacer", [IsList, IsRecord] );
DeclareOperation( "ProductReplacer", [IsList] );

# Usage:
DeclareOperation( "Next", [IsProductReplacer] );
DeclareOperation( "Reset", [IsProductReplacer] );


#####################
# Random searchers: #
#####################

BindGlobal( "RandomSearchersFamily", NewFamily( "RandomSearchersFamily" ) );
DeclareCategory( "IsRandomSearcher", IsComponentObjectRep );
DeclareGlobalVariable( "RandomSearchersType" );

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

