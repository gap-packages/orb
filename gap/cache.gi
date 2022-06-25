#############################################################################
##
##                             orb package
##  cache.gi
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

InstallMethod( LinkedListCache, "for an integer", [ IsInt ],
  function(memorylimit)
    local c;
    c := rec(head := fail, tail := fail, nrobs := 0,
             memory := 0, memorylimit := memorylimit);
    Objectify(NewType(CachesFamily,IsLinkedListCacheRep and IsMutable),c);
    return c;
end );

InstallMethod( ViewObj, "for a linked list cache object",
  [ IsCache and IsLinkedListCacheRep ],
  function(c)
    Print("<linked list cache with ",c!.nrobs," objects using ",
          c!.memory," <= ",c!.memorylimit," bytes>");
  end );

InstallMethod( Display, "for a linked list cache object",
  [ IsCache and IsLinkedListCacheRep ],
  function(c)
    local cn,i;
    cn := c!.head;
    Print("<linked list cache with ",c!.nrobs," objects using ",
          c!.memory," <= ",c!.memorylimit," bytes containing:\n");
    i := 0;
    while cn <> fail do
        i := i + 1;
        Print("#",i," ob=");
        ViewObj(cn!.ob);
        Print(" mem=",cn!.mem,"\n");
        cn := cn!.next;
    od;
    Print(">\n");
  end );

InstallMethod( ClearCache, "for a linked list cache object",
  [ IsCache and IsLinkedListCacheRep ],
  function(c)
    c!.head := fail;
    c!.tail := fail;
    c!.nrobs := 0;
    c!.memory := 0;
  end );

InstallGlobalFunction( CacheObject,
  function( c, ob, mem )
    local r;
    r := rec( ob := ob, next := c!.head, prev := fail, mem := mem );
    Objectify( LinkedListCacheNodeType, r );
    c!.head := r;
    c!.memory := c!.memory + mem;
    if c!.tail = fail then 
        c!.tail := r; 
    else
        r!.next!.prev := r;
    fi;
    c!.nrobs := c!.nrobs + 1;
    EnforceCachelimit(c);
    return r;
  end );

InstallGlobalFunction( EnforceCachelimit,
  function(c)
    local s;
    # Delete something if memory usage too high:
    while c!.memory > c!.memorylimit do
        s := c!.tail;
        c!.memory := c!.memory - s!.mem;
        c!.tail := s!.prev;
        if s!.prev = fail then 
            c!.head := fail; 
        else
            s!.prev!.next := fail;
        fi;
        s!.prev := false;   # mark node as no longer in cache
        c!.nrobs := c!.nrobs - 1;
    od;
  end );


InstallMethod( ViewObj, "for a linked list cache node",
  [ IsCacheNode and IsLinkedListCacheNodeRep ],
  function( cn )
    Print("<linked list cache node ob=");
    ViewObj(cn!.ob);
    Print(" mem=",cn!.mem,">");
  end );

InstallGlobalFunction( UseCacheObject,
  function( c, r )
    local s;
    if r!.prev = false then
        # the object is no longer in the cache, we cache it again:
        # note that we cannot use CacheObject, because we want to retain
        # the cache node r itself!
        r!.prev := fail;
        r!.next := c!.head;
        c!.head := r;
        c!.memory := c!.memory + r!.mem;
        if c!.tail = fail then
            c!.tail := r;
        else
            r!.next!.prev := r;
        fi;
        c!.nrobs := c!.nrobs + 1;
        EnforceCachelimit(c);
        return;
    fi;
    if r!.prev = fail then return; fi;   # nothing to do
    s := r!.prev;
    s!.next := r!.next;
    if r!.next = fail then
        c!.tail := s;
    else
        r!.next!.prev := s;
    fi;
    r!.prev := fail;
    r!.next := c!.head;
    r!.next!.prev := r;
    c!.head := r;
  end );

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
