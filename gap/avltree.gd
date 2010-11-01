#############################################################################
##
##                             orb package
##  avltree.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2009-2009 by the authors.
##  This file is free software, see license information at the end.
##
##  Declaration stuff for AVL trees in GAP.
##
##  adding, removing and finding in O(log n), n is number of nodes
##
##  see Knuth: "The Art of Computer Programming" for algorithms
##
#############################################################################

BindGlobal( "AVLTreeFamily", NewFamily("AVLTreeFamily") );
DeclareCategory( "IsAVLTree", IsPositionalObjectRep );
DeclareRepresentation( "IsAVLTreeFlatRep", IsAVLTree, [] );
BindGlobal( "AVLTreeType", NewType(AVLTreeFamily,IsAVLTreeFlatRep) );
BindGlobal( "AVLTreeTypeMutable", NewType(AVLTreeFamily,
                                          IsAVLTreeFlatRep and IsMutable) );

# All of the following functions exist on the GAP level and some of
# them on the C level for speedup. The GAP versions have "_GAP" appended
# to their name, the C versions have "_C" appended. The version with
# nothing appended is the one to be used, it is assigned to the C
# version if it is there and otherwise to the GAP version.

DeclareGlobalFunction( "AVLCmp" );
DeclareGlobalFunction( "AVLTree" );
DeclareGlobalFunction( "AVLNewNode" );
DeclareGlobalFunction( "AVLFreeNode" );
DeclareGlobalFunction( "AVLData" );
DeclareGlobalFunction( "AVLSetData" );
DeclareGlobalFunction( "AVLLeft" );
DeclareGlobalFunction( "AVLSetLeft" );
DeclareGlobalFunction( "AVLRight" );
DeclareGlobalFunction( "AVLSetRight" );
DeclareGlobalFunction( "AVLRank" );
DeclareGlobalFunction( "AVLSetRank" );
DeclareGlobalFunction( "AVLBalFactor" );
DeclareGlobalFunction( "AVLSetBalFactor" );
DeclareGlobalFunction( "AVLValue" );
DeclareGlobalFunction( "AVLSetValue" );
DeclareGlobalFunction( "AVLFind" );
DeclareGlobalFunction( "AVLFindIndex" );
DeclareGlobalFunction( "AVLLookup" );
DeclareGlobalFunction( "AVLIndex" );
DeclareGlobalFunction( "AVLIndexFind" );
DeclareGlobalFunction( "AVLRebalance" );
DeclareGlobalFunction( "AVLIndexLookup" );
DeclareGlobalFunction( "AVLAdd" );
DeclareGlobalFunction( "AVLIndexAdd" );
DeclareGlobalFunction( "AVLDelete" );
DeclareGlobalFunction( "AVLIndexDelete" );
DeclareGlobalFunction( "AVLToList" );

