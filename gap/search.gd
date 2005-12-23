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

###################
# Random sources: #
###################

BindGlobal( "RandomSourcesFamily", NewFamily( "RandomSourcesFamily" ) );
DeclareCategory( "IsRandomSource", IsComponentObjectRep );
DeclareRepresentation( "IsGlobalRandomSourceRep", IsRandomSource, [] );
DeclareGlobalVariable( "RandomSourceType" );

# The constructor:
DeclareOperation( "RandomSource", [IsString] );

# Usage:
DeclareOperation( "Reset", [IsRandomSource] );
DeclareOperation( "Random", [IsRandomSource, IsInt, IsInt] );
DeclareOperation( "Random", [IsRandomSource, IsList] );

# For other objects:
DeclareFilter( "HasRandomSource" );


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

DeclareGlobalFunction( "OrbitsStatisticOnVectorSpace" );

