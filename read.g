#############################################################################
##
##  read.g                orb package                       Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Reading the implementation part of the orb package.
##
#############################################################################

ReadPackage("orb","gap/homwdata.gi");
ReadPackage("orb","gap/hash.gi");
ReadPackage("orb","gap/cache.gi");
ReadPackage("orb","gap/orbits.gi");
ReadPackage("orb","gap/search.gi");
ReadPackage("orb","gap/bysuborbit.gi");

if IsBound(IO_PackageIsLoaded) then
    ReadPackage("orb","gap/picklers.gi");
else
    if not(IsBound(IO_PkgThingsToRead)) then
        IO_PkgThingsToRead := [];
    fi;
    Add(IO_PkgThingsToRead,["orb","gap/picklers.gi"]);
fi;

