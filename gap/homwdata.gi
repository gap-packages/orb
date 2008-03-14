#############################################################################
##
##  homwdata.gi           orb package                   Max Neunhoeffer
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
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


