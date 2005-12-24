#############################################################################
##
##  byorbits.gd              orb package                      
##                                                           Max Neunhoeffer
##                                                              Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Declaration stuff for fast orbit enumeration by suborbits.
##
#############################################################################

DeclareGlobalFunction( "ORB_PrettyStringBigNumber" );

DeclareGlobalFunction( "ORB_InvWord" );
DeclareGlobalFunction( "ORB_ApplyWord" );
DeclareGlobalFunction( "ORB_ResetStabIterator" );
DeclareGlobalFunction( "ORB_NextStabIterator" );
DeclareGlobalFunction( "ORB_ApplyStabElement" );
DeclareGlobalFunction( "ORB_Minimalize" );

DeclareGlobalFunction( "OrbitBySuborbitBootstrap" );

DeclareGlobalFunction( "OrbitBySuborbit" );

DeclareGlobalFunction( "ORB_NextStabIterator2" );
DeclareGlobalFunction( "ORB_ApplyUElement" );

DeclareGlobalFunction( "OrbitBySuborbitWithKnownSize" );

# Still missing:
# RepresentativeActionForVectorsPrepare (write memory full)
# SearchBackward (depth first)
# DoubleCosetRepsSearcher: (InitDoubleCosetRepsSearcher)
# FeedKnownOrbitsIntoRecord
