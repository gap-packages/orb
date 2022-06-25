#############################################################################
##
##                             orb package
##  cache.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
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
  NewType( CacheNodesFamily, IsLinkedListCacheNodeRep and IsMutable) );

DeclareOperation("LinkedListCache", [IsInt]);
DeclareOperation("ClearCache", [IsCache]);
DeclareGlobalFunction("CacheObject");
DeclareGlobalFunction("EnforceCachelimit");
DeclareGlobalFunction("UseCacheObject");


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
