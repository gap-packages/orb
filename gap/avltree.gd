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
BindGlobal( "AVLTreeType", NewType(AVLTreeFamily,IsAVLTree) );

# All of the following functions exist on the GAP level and some of
# them on the C level for speedup. The GAP versions have "_GAP" appended
# to their name, the C versions have "_C" appended. The version with
# nothing appended is the one to be used, it is assigned to the C
# version if it is there and otherwise to the GAP version.

DeclareGlobalFunction( "AVLTreeCmp_GAP" );
AVLTreeCmp := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTree_GAP" );
AVLTree := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeNewNode_GAP" );
AVLTreeNewNode := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeFreeNode_GAP" );
AVLTreeFreeNode := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeData_GAP" );
AVLTreeData := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetData_GAP" );
AVLTreeSetData := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeLeft_GAP" );
AVLTreeLeft := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetLeft_GAP" );
AVLTreeSetLeft := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeRight_GAP" );
AVLTreeRight := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetRight_GAP" );
AVLTreeSetRight := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeRank_GAP" );
AVLTreeRank := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetRank_GAP" );
AVLTreeSetRank := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeBalFactor_GAP" );
AVLTreeBalFactor := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetBalFactor_GAP" );
AVLTreeSetBalFactor := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeValue_GAP" );
AVLTreeValue := fail;     # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeSetValue_GAP" );
AVLTreeSetValue := fail;     # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeFind_GAP" );
AVLTreeFind := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeLookup_GAP" );
AVLTreeLookup := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeIndex_GAP" );
AVLTreeIndex := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeRebalance_GAP" );
AVLTreeRebalance := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeIndexLookup_GAP" );
AVLTreeIndexLookup := fail;   # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeAdd_GAP" );
AVLTreeAdd := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeIndexAdd_GAP" );
AVLTreeIndexAdd := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeDelete_GAP" );
AVLTreeDelete := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeIndexDelete_GAP" );
AVLTreeIndexDelete := fail;  # placeholder for later assignment
DeclareGlobalFunction( "AVLTreeToList_GAP" );
AVLTreeToList := fail;  # placeholder for later assignment

