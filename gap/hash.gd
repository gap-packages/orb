#############################################################################
##
##  hash.gd           orb package 
##                                                        by Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Declaration stuff for hashing.
##
#############################################################################


########################
# Generic hashing code:
########################

DeclareGlobalFunction( "InitHT" );
DeclareGlobalFunction( "NewHT" );
DeclareGlobalFunction( "AddHT" );
DeclareGlobalFunction( "ValueHT" );
DeclareGlobalFunction( "GrowHT" );


#########################################################################
# Infrastructure for choosing hash functions looking at example objects:
#########################################################################

DeclareOperation( "ChooseHashFunction", [IsObject, IsInt] );

DeclareGlobalFunction( "ORB_HashFunctionForShortGF2Vectors" );
DeclareGlobalFunction( "ORB_HashFunctionForShort8BitVectors" );
DeclareGlobalFunction( "ORB_HashFunctionForGF2Vectors" );
DeclareGlobalFunction( "ORB_HashFunctionFor8BitVectors" );
DeclareGlobalFunction( "ORB_HashFunctionForCompressedMats" );
DeclareGlobalFunction( "ORB_HashFunctionForIntegers" );
DeclareGlobalFunction( "ORB_HashFunctionForMemory" );
DeclareGlobalFunction( "ORB_HashFunctionForPermutations" );
DeclareGlobalFunction( "ORB_HashFunctionForIntList" );
DeclareGlobalFunction( "ORB_HashFunctionForNBitsPcWord" );

