#############################################################################
##
##                             orb package
##  init.g
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Reading the declaration part of the orb package.
##
#############################################################################

################################
# First look after our C part: #
################################

# load kernel function if it is installed:
if (not IsBound(ORBC)) and ("orb" in SHOW_STAT()) then
  # try static module
  LoadStaticModule("orb");
fi;
if (not IsBound(ORBC)) and
   (Filename(DirectoriesPackagePrograms("orb"), "orb.so") <> fail) then
  LoadDynamicModule(Filename(DirectoriesPackagePrograms("orb"), "orb.so"));
fi;

# Compatibility things for GAP 4.4:
if not(IsBound(RowLength)) then
    ReadPackage("orb","gap/gap4r4.g");
fi;

ReadPackage("orb","gap/homwdata.gd");
ReadPackage("orb","gap/avltree.gd");
ReadPackage("orb","gap/hash.gd");
ReadPackage("orb","gap/cache.gd");
ReadPackage("orb","gap/orbits.gd");
ReadPackage("orb","gap/search.gd");
ReadPackage("orb","gap/bysuborbit.gd");
if not(CompareVersionNumbers(GAPInfo.Version),"4.7") then
    ReadPackage("orb","gap/transform.gd");
fi;

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
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
