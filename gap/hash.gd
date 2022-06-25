#############################################################################
##
##                             orb package
##  hash.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Declaration stuff for hashing.
##
#############################################################################


########################
# Generic hashing code:
########################

DeclareGlobalFunction( "InitHT" );
DeclareGlobalFunction( "NewHT" );
DeclareGlobalFunction( "AddHT" );
DeclareGlobalFunction( "ValueHT" );
DeclareGlobalFunction( "GrowHT" );

BindGlobal( "HashTabFamily", NewFamily("HashTabFamily") );
DeclareCategory( "IsHashTab", IsComponentObjectRep);
DeclareRepresentation( "IsHashTabRep", IsHashTab, [] );
DeclareRepresentation( "IsTreeHashTabRep", IsHashTab, [] );
BindGlobal( "HashTabType", NewType(HashTabFamily,IsHashTabRep and IsMutable) );
BindGlobal( "TreeHashTabType", 
  NewType(HashTabFamily,IsTreeHashTabRep and IsMutable) );

DeclareOperation( "HTCreate", [ IsObject, IsRecord ] );
DeclareOperation( "HTCreate", [ IsObject ] );
DeclareOperation( "HTAdd", [ IsHashTab, IsObject, IsObject ] );
DeclareOperation( "HTValue", [ IsHashTab, IsObject ] );
DeclareOperation( "HTDelete", [ IsHashTab, IsObject ] );
DeclareOperation( "HTUpdate", [ IsHashTab, IsObject, IsObject ] );
DeclareOperation( "HTGrow", [ IsHashTab, IsObject ] );


#########################################################################
# Infrastructure for choosing hash functions looking at example objects:
#########################################################################

DeclareOperation( "ChooseHashFunction", [IsObject, IsInt] );

DeclareGlobalFunction( "ORB_HashFunctionForShortGF2Vectors" );
DeclareGlobalFunction( "ORB_HashFunctionForShort8BitVectors" );
DeclareGlobalFunction( "ORB_HashFunctionForGF2Vectors" );
DeclareGlobalFunction( "ORB_HashFunctionFor8BitVectors" );
DeclareGlobalFunction( "ORB_HashFunctionForCompressedMats" );
DeclareGlobalFunction( "ORB_HashFunctionForIntegers" );
DeclareGlobalFunction( "ORB_HashFunctionForMemory" );
DeclareGlobalFunction( "ORB_HashFunctionForPermutations" );
DeclareGlobalFunction( "ORB_HashFunctionForIntList" );
DeclareGlobalFunction( "ORB_HashFunctionForNBitsPcWord" );
DeclareGlobalFunction( "ORB_HashFunctionModWrapper" );
DeclareGlobalFunction( "ORB_HashFunctionForMatList" );
DeclareGlobalFunction( "ORB_HashFunctionForPlainFlatList" );
DeclareGlobalFunction( "ORB_HashFunctionForTransformations" );
DeclareGlobalFunction( "ORB_HashFunctionForPartialPerms" );
DeclareGlobalFunction( "MakeHashFunctionForPlainFlatList" );


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
