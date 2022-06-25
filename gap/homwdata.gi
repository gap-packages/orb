#############################################################################
##
##                             orb package
##  homwdata.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Things for our group homomorphisms with data.
##
#############################################################################

# Eventually this should all go into the main GAP library.

InstallGlobalFunction( GroupHomByFuncWithData,
  function( arg )
    local data,h;
    # Say Group if arg[2] is a list of generators:
    if not(IsGroup(arg[2])) then
        arg[2] := Group(arg[2]);
    fi;
    data := arg[Length(arg)];
    Unbind(arg[Length(arg)]);
    h := CallFuncList(GroupHomomorphismByFunction,arg);
    h!.data := data;
    SetFilterObj(h, IsMappingByFunctionWithData);
    return h;
  end);

InstallMethod( ImageElm, "for a mapping by function with data and an object",
  [ IsMappingByFunctionRep and IsMappingByFunctionWithData, IsObject ],
  function(h,o)
    return h!.fun(h!.data,o);
  end);

InstallMethod( ImagesElm, "for a mapping by function with data and an object",
  [ IsMappingByFunctionRep and IsMappingByFunctionWithData, IsObject ],
  function(h,o)
    return [h!.fun(h!.data,o)];
  end);

InstallMethod( ImagesRepresentative,
  "for a mapping by function with data and an object",
  [ IsMappingByFunctionRep and IsMappingByFunctionWithData, IsObject ],
  function(h,o)
    return h!.fun(h!.data,o);
  end);

InstallMethod( PreImageElm, "for a mapping by function with data, and an obj",
  [ IsMappingByFunctionWithInverseRep and IsMappingByFunctionWithData, 
    IsObject ], 0,
  function (h,o)
  return h!.invFun(h!.data,o);
  end );

InstallMethod( PreImagesElm, "for a mapping by function with data, and an obj",
  [ IsMappingByFunctionWithInverseRep and IsMappingByFunctionWithData, 
    IsObject ], 0,
  function (h,o)
  return [h!.invFun(h!.data,o)];
  end );

InstallMethod( PreImagesRepresentative, 
  "for a mapping by function with data, and an obj",
  [ IsMappingByFunctionWithInverseRep and IsMappingByFunctionWithData, 
    IsObject ], 0,
  function (h,o)
  return h!.invFun(h!.data,o);
  end );

InstallMethod( PreImagesRepresentative, 
  "for a mapping by function with invmap with data, and an obj",
  [ IsMappingByFunctionRep and IsMappingByFunctionWithData, 
    IsObject ], 0,
  function (h,o)
  return h!.prefun(h!.data,o);
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
