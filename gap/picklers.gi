#############################################################################
##
##                             orb package
##  picklers.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Pickling of objects in the orb package.
##  This is only read when the IO package is loaded, but it is also
##  loaded, if the IO package is read after the orb package.
##
#############################################################################

InstallMethod( IO_Pickle, "for an orbit object",
  [ IsFile, IsOrbit ],
  function(f,o)
    return IO_GenericObjectPickler( f, "ORBI",[o[1]], o, [],   # no attributes
      [ IsPermOnIntOrbitRep, IsHashOrbitRep, IsSlowOrbitRep,
        IsOrbitWithLog, IsClosedOrbit ], NamesOfComponents(o) );
  end );


IO_Unpicklers.ORBI := function(f)
  local o,x;
  o := rec();
  x := IO_Unpickle(f); if x = IO_Error then return IO_Error; fi;
  Objectify( NewType( OrbitFamily, IsOrbit ), o );
  return IO_GenericObjectUnpickler(f, o, [],    # no attributes
      [ IsPermOnIntOrbitRep, IsHashOrbitRep, IsSlowOrbitRep,
        IsOrbitWithLog, IsClosedOrbit ] );
end;

InstallMethod( IO_Pickle, "for a cache",
  [ IsFile, IsCache and IsLinkedListCacheRep ],
  function( f, c )
    return IO_GenericObjectPickler( f, "CACL", [], c, [], [],
               ["memorylimit","nrobs","memory","head","tail"] );
  end );

IO_Unpicklers.CACL := 
  function(f)
    local c;
    c := LinkedListCache(100);
    return IO_GenericObjectUnpickler(f,c,[],[]);
  end;

InstallMethod( IO_Pickle, "for a cache node",
  [ IsFile, IsLinkedListCacheNodeRep ],
  function(f,cn)
    # When we see one of those, we do the whole chain of them to avoid
    # deep recursion, first find the first one:
    local c;
    if IO_IsAlreadyPickled(cn) <> false then
        # this will only pickle a reference!
        return IO_GenericObjectPickler(f,"CACN",[cn!.ob,cn!.mem],cn,[],[],[]);
    fi;
    c := cn;
    while c!.prev <> fail do c := c!.prev; od;
    # Write a start tag:
    if IO_Write(f,"CACS") = fail then return IO_Error; fi;
    while c <> fail do
        if IO_GenericObjectPickler(f,"CACN",[c!.ob,c!.mem],c,[],[],[])
            = IO_Error then return IO_Error; 
        fi;
        c := c!.next;
    od;
    if IO_Pickle(f,fail) = IO_Error then return IO_Error; fi;
    # The following will always pickle a reference to cn:
    return IO_GenericObjectPickler(f,"CACN",[cn!.ob,cn!.mem],cn,[],[],[]);
  end );

IO_Unpicklers.CACS :=
  function(f)
    # This is the start of a full sequence of cache nodes, we will read
    # them all and link them to each other. After the trailing fail we
    # get the "root":
    local c,cc;
    c := fail;
    while true do   # will be left by break
        cc := IO_Unpickle(f);
        if cc = fail then
            if c <> fail then
                c!.next := fail;
            fi;
            break;
        fi;
        if c <> fail then
            c!.next := cc;
        fi;
        cc!.prev := c;
        c := cc;
    od;
    return IO_Unpickle(f);
  end;

IO_Unpicklers.CACN :=
  function(f)
    local c,mem,ob;
    c := rec(ob := fail, mem := fail, next := fail, prev := fail);
    c.ob := IO_Unpickle(f); 
    if c.ob = IO_Error then 
        IO_FinalizeUnpickled();
        return IO_Error; 
    fi;
    c.mem := IO_Unpickle(f); 
    if c.mem = IO_Error then 
        IO_FinalizeUnpickled();
        return IO_Error; 
    fi;
    IO_AddToUnpickled(c);
    if IO_Unpickle(f) <> fail then return IO_Error; fi;
    Objectify( LinkedListCacheNodeType, c );
    IO_FinalizeUnpickled();
    return c;
  end;

InstallMethod( IO_Pickle, "for an orbit-by-suborbit setup record",
  [ IsFile, IsStdOrbitBySuborbitSetupRep ],
  function(f, s)
    return IO_GenericObjectPickler( f, "OBSS",[], s, [], [],
                                    NamesOfComponents(s) );
  end );

IO_Unpicklers.OBSS := 
  function(f)
    local s;
    s := rec();
    Objectify( NewType(OrbitBySuborbitSetupFamily,
                       IsOrbitBySuborbitSetup and IsStdOrbitBySuborbitSetupRep),
               s);
    return IO_GenericObjectUnpickler(f, s, [], []);
  end;
  
InstallMethod( IO_Pickle, "for a suborbit database",
  [ IsFile, IsStdSuborbitDbRep ],
  function(f, db)
    return IO_GenericObjectPickler( f, "OBSD", [], db, [], [],
                                    NamesOfComponents(db) );
  end );
   
IO_Unpicklers.OBSD := 
  function(f)
    local db;
    db := rec();
    Objectify( StdSuborbitDatabasesType, db);
    return IO_GenericObjectUnpickler(f, db, [], []);
  end;
  
InstallMethod( IO_Pickle, "for an orbit-by-suborbit",
  [ IsFile, IsStdOrbitBySuborbitRep ],
  function(f, o)
    return IO_GenericObjectPickler( f, "OBSO", [], o, [], [],
                                    NamesOfComponents(o) );
  end );
   
IO_Unpicklers.OBSO := 
  function(f)
    local o;
    o := rec();
    Objectify(StdOrbitBySuborbitsType , o);
    return IO_GenericObjectUnpickler(f, o, [], []);
  end;
  
InstallMethod( IO_Pickle, "for an orbit-by-suborbit list",
  [ IsFile, IsStdOrbitBySuborbitListRep],
  function(f, l)
    return IO_GenericObjectPickler( f, "OBSL", [], l, [], [],
                                    NamesOfComponents(l) );
  end );
   
IO_Unpicklers.OBSL := 
  function(f)
    local l;
    l := rec();
    Objectify( StdOrbitBySuborbitListType, l);
    return IO_GenericObjectUnpickler(f, l, [], []);
  end;
  
InstallMethod( IO_Pickle, "for a product replacer object",
  [ IsFile, IsProductReplacer ],
  function( f, pr )
    return IO_GenericObjectPickler(f,"PRPL",[],pr,[],[],
                                   NamesOfComponents(pr));
  end );

IO_Unpicklers.PRPL := 
  function(f)
    local pr;
    pr := rec();
    Objectify(ProductReplacersType,pr);
    return IO_GenericObjectUnpickler(f,pr,[],[]);
  end;

InstallMethod( IO_Pickle, "for a random searcher",
  [ IsFile, IsRandomSearcher ],
  function( f, rs )
    return IO_GenericObjectPickler(f,"RSEA",[],rs,[],[],
                                   NamesOfComponents(rs));
  end );

IO_Unpicklers.RSEA :=
  function(f)
    local rs;
    rs := rec();
    Objectify(RandomSearchersType,rs);
    return IO_GenericObjectUnpickler(f,rs,[],[]);
  end;

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
