#############################################################################
##
##  cache.gi           orb package 
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

InstallMethod( LinkedListCache, "for an integer", [ IsInt ],
  function(memorylimit)
    local c;
    c := rec(head := fail, tail := fail, nrobs := 0,
             memory := 0, memorylimit := memorylimit);
    Objectify( NewType( CachesFamily, IsLinkedListCacheRep ), c );
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
    local r,s;
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
    # Now delete something if memory usage too high:
    while c!.memory > c!.memorylimit do
        s := c!.tail;
        c!.memory := c!.memory - s!.mem;
        c!.tail := s!.prev;
        if s!.prev = fail then 
            c!.head := fail; 
        else
            s!.prev!.next := fail;
        fi;
        c!.nrobs := c!.nrobs - 1;
    od;
    return r;
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

