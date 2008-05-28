#############################################################################
##
##  init.g                orb package                       Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
##
##  Reading the declaration part of the orb package.
##
#############################################################################

# Compatibility things for GAP 4.4:
if not(IsBound(RowLength)) then
    ReadPackage("orb","gap/gap4r4.g");
fi;

ReadPackage("orb","gap/homwdata.gd");
ReadPackage("orb","gap/hash.gd");
ReadPackage("orb","gap/cache.gd");
ReadPackage("orb","gap/orbits.gd");
ReadPackage("orb","gap/search.gd");
ReadPackage("orb","gap/bysuborbit.gd");

