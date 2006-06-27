#############################################################################
##
##  cache.gd           orb package 
##                                                        by Juergen Mueller
##                                                       and Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2006 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Declaration stuff for caching.
##
#############################################################################


########################
# Generic caching code:
########################

BindGlobal( "CacheNodesFamily", NewFamily( "CacheNodesFamily" ) );
BindGlobal( "CachesFamily", CollectionsFamily( CacheNodesFamily ) );

DeclareCategory("IsCache", IsComponentObjectRep);
DeclareRepresentation("IsLinkedListCacheRep", IsCache,
  [ "head", "tail", "nrobs", "memory", "memorylimit" ]);
DeclareCategory("IsCacheNode", IsComponentObjectRep);
DeclareRepresentation("IsLinkedListCacheNodeRep", IsCacheNode,
  [ "next", "prev", "ob", "mem" ] );
BindGlobal( "LinkedListCacheNodeType", 
  NewType( CacheNodesFamily, IsLinkedListCacheNodeRep ) );

DeclareOperation("LinkedListCache", [IsInt]);
DeclareOperation("ClearCache", [IsCache]);
DeclareGlobalFunction("CacheObject");
DeclareGlobalFunction("EnforceCachelimit");
DeclareGlobalFunction("UseCacheObject");


