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

# All of the following functions exist on the GAP level and some of
# them on the C level for speedup. The GAP versions have "_GAP" appended
# to their name, the C versions have "_C" appended. The version with
# nothing appended is the one to be used, it is assigned to the C
# version if it is there and otherwise to the GAP version.

DeclareGlobalFunction( "AVLCmp_GAP" );
AVLCmp := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTree_GAP" );
AVLTree := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLNewNode_GAP" );
AVLNewNode := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLFreeNode_GAP" );
AVLFreeNode := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLData_GAP" );
AVLData := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLSetData_GAP" );
AVLSetData := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLLeft_GAP" );
AVLLeft := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLSetLeft_GAP" );
AVLSetLeft := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLRight_GAP" );
AVLRight := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLSetRight_GAP" );
AVLSetRight := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLRank_GAP" );
AVLRank := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLSetRank_GAP" );
AVLSetRank := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLBalFactor_GAP" );
AVLBalFactor := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLSetBalFactor_GAP" );
AVLSetBalFactor := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLValue_GAP" );
AVLValue := fail;     # placeholder for later assignment
DeclareGlobalFunction( "AVLSetValue_GAP" );
AVLSetValue := fail;     # placeholder for later assignment
DeclareGlobalFunction( "AVLFind_GAP" );
AVLFind := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLFindIndex_GAP" );
AVLFindIndex := AVLFindIndex_GAP;  # placeholder for later assignment
DeclareGlobalFunction( "AVLLookup_GAP" );
AVLLookup := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLIndex_GAP" );
AVLIndex := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLIndexFind_GAP" );
AVLIndexFind := AVLIndexFind_GAP;  # placeholder for later assignment
DeclareGlobalFunction( "AVLRebalance_GAP" );
AVLRebalance := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLIndexLookup_GAP" );
AVLIndexLookup := fail;   # placeholder for later assignment
DeclareGlobalFunction( "AVLAdd_GAP" );
AVLAdd := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLIndexAdd_GAP" );
AVLIndexAdd := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLDelete_GAP" );
AVLDelete := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLIndexDelete_GAP" );
AVLIndexDelete := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLToList_GAP" );
AVLToList := fail;  # placeholder for later assignment

