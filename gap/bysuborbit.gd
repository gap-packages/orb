#############################################################################
##
##                             orb package
##  byorbits.gd
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Declaration stuff for fast orbit enumeration by suborbits.
##
#############################################################################

###########################
# A few helper functions: #
###########################

DeclareGlobalFunction( "ORB_PrettyStringBigNumber" );
DeclareGlobalFunction( "ORB_InvWord" );
DeclareGlobalFunction( "ORB_ApplyWord" );


########################################################################
# Documentation of a data structure for a step of the "quotient trick":
########################################################################

# Preliminaries:
# let G act on the right on some set of points P
# let 1 = U_0 < U_1 < U_2 < U_3 < ... < U_k < G be subgroups, 
# let P_1, P_2, ..., P_k be sets with U_i acting on P_i
# let P -> P_k -> ... -> P_2 -> P_1 be maps, such that
#     P -> P_k -> ... -> P_i is a homomorphism of U_i-sets for each i
# set P_{k+1} := P
# let pi[j][i] : P_j -> ... -> P_i be the above "projections"

BindGlobal( "OrbitBySuborbitSetupFamily", 
            NewFamily( "OrbitBySuborbitSetupFamily" ) );
DeclareCategory( "IsOrbitBySuborbitSetup", IsComponentObjectRep);
DeclareRepresentation( "IsStdOrbitBySuborbitSetupRep", IsOrbitBySuborbitSetup,
  [ "k",          # number of helper subgroups U_1 < U_2 < ... < U_k
    "size",       # sizes of those groups (i:1..k+1)
    "index",      # index of U_{i-1} in U_i (i:1..k)
    "els",        # els stores a list of l group elements of G, 
                  # they all are stored in their action on P, P_1, ..., P_k
                  # therefore els[i] is a list of l elements of G, that
                  # act on points in P_i, els[k+1] is a list the same
                  # elements of G acting on P (i:1..k+1)
    "elsinv",     # the inverses of the elements in els, but only in their
                  # action on P_1, ..., P_{k+1}
    "trans",      # a list of words, which are lists of integers between 
                  # 1 and l, which are indices of elements in els.  
                  # trans[i] describes a left transversal of U_{i-1}
                  # in U_i.
                  # trans[i][?] and els[j] together are suitable to be
                  # used with "ORB_ApplyWord" (i:2..k)
    "pifunc",     # usually equal to \{\}
                  # (pifunc[j][i], for j:1..k+1, i:1..j-1)
    "pi",         # projection function P_j ->> P_i
                  # this is stored as a range used to slice something out
                  # (pi[j][i], for j:1..k+1, i:1..j-1)
    "op",         # Action function/operation for each level (i:1..k+1)
                  # usually OnRight for vectors
    "info",       # the hash table for points x in P_i, that are
                  # for i=1 we store either the number of a word in the
                  # wordcache mapping the point to the minimal point in the
                  # same U_1 orbit or a record with components "gens" and
                  # "size" describing the stabilizer. "gens" is a list of
                  # words.
                  # for i>1 there are 3 possibilities:
                  # Let y be the U_i-minimal point in yU_i with y in P_i
                  #   for z in yU_i \setminus xU_{i-1} we store a number
                  #      of a transversal element mapping into yU_{i-1}
                  #   for z in yU_{i-1} but z \neq y we store the negative
                  #      of the number of a word in the wordcache
                  #      mapping y to z
                  #   for z = y we store a stabilizer record as above.
                  # info grows as we go, starts up empty
                  # (i:1..k)
    "cosetrecog", # a function that gets a word w in inverses of
                  # the generators of the groups U_1, ...,
                  # U_i and this setup-record and
                  # returns the number of an element
                  # in trans[i], such that the element of U_i
                  # described by w is in trans[i] U_{i-1}. (i:2..k)
    "cosetinfo",  # information used by cosetrecog[i] (i:2..k)
    "suborbnr",   # number of suborbits archived in hash table i (i:1..k)
    "sumstabl",   # sum of stabilizer (in transversal) lengths
                  # archived in table i (i:1..k)
    "permgens",   # all group elements in a nice faithful 
                  # permutation representation
                  # same order as in els and elsinv
    "permgensinv", # the inverses of the elements in permgens
    "sample",     # a sample point in each set (i:1..k+1) for choosing hash
                  # functions
    "stabchainrandom",   # if bound, the value for the "random" option of
                  # "StabChain"
    "wordhash",   # a hash storing word numbers
    "wordcache",  # a list of words used somewhere
    "hashlen",    # initial length of hashes for minimal vectors (i:1..k+1)
    "staborblenlimit",   # limit, up to which orbits of stabilizers are
                         # computed using word action
  ] );
#Already there generically:
#DeclareOperation( "Memory", [IsOrbitBySuborbitSetup] );


############################################
# The heart of the method: Minimalization: #
############################################

DeclareGlobalFunction( "ORB_StoreWordInCache" );
DeclareGlobalFunction( "ORB_Minimalize" );


#######################
# Suborbit databases: #
#######################

DeclareCategory( "IsSuborbitDatabase", IsComponentObjectRep);
DeclareRepresentation( "IsStdSuborbitDbRep", IsSuborbitDatabase, 
  [ "reps",        # a list of suborbit representatives
    "mins",        # a hash table to recognise minimal vectors
    "lengths",     # a list of lengths of suborbits
    "setup",       # a reference to the setup object
    "totallength", # total length
  ] );
BindGlobal( "SuborbitDatabasesFamily", 
  NewFamily( "SuborbitDatabasesFamily", IsSuborbitDatabase ) );
BindGlobal( "StdSuborbitDatabasesType",
  NewType( SuborbitDatabasesFamily, 
           IsStdSuborbitDbRep and IsMutable) );

DeclareOperation( "SuborbitDatabase", 
                  [ IsOrbitBySuborbitSetup, IsPosInt, IsPosInt, IsPosInt ] );
DeclareOperation( "StoreSuborbit", 
     [ IsSuborbitDatabase, IsObject, IsRecord, IsPosInt, IsPosInt ] );
DeclareOperation( "LookupSuborbit", [ IsObject, IsSuborbitDatabase ] );
DeclareOperation( "TotalLength", [ IsSuborbitDatabase ] );
DeclareOperation( "Representatives", [ IsSuborbitDatabase ] );
# Already there generically:
#DeclareOperation( "Memory", [IsSuborbitDatabase] );
DeclareOperation( "SavingFactor", [ IsSuborbitDatabase ] );


#######################################################
# Objects representing orbits enumerated by suborbits:
#######################################################

DeclareCategory( "IsOrbitBySuborbit", IsComponentObjectRep);
DeclareRepresentation( "IsStdOrbitBySuborbitRep", IsOrbitBySuborbit,
  [ "db",           # a suborbit database object
    "words",        # a list of words to reach the suborbits
    "stabsize",     # size of the stabilizer
    "stab",         # the stabilizer in the permutation rep
    "groupsize",    # the full group size
    "orbitlength",  # the length of the orbit
    "percentage",   # percentage (as an int) that the user asked for
  ] );
BindGlobal( "OrbitBySuborbitFamily", 
            NewFamily( "OrbitBySuborbitFamily", IsOrbitBySuborbit ) );
BindGlobal( "StdOrbitBySuborbitsType",
  NewType( OrbitBySuborbitFamily, 
           IsStdOrbitBySuborbitRep and IsMutable) );
DeclareOperation( "SuborbitsDb", [IsOrbitBySuborbit] );
DeclareOperation( "WordsToSuborbits", [IsOrbitBySuborbit] );
# Already there generically:
#DeclareOperation( "Memory", [IsOrbitBySuborbit] );
DeclareOperation( "Seed", [IsOrbitBySuborbit] );
DeclareOperation( "OrigSeed", [IsOrbitBySuborbit] );
DeclareOperation( "StabWords", [IsOrbitBySuborbit] );
DeclareOperation( "TotalLength", [ IsOrbitBySuborbit ] );
DeclareOperation( "SavingFactor", [ IsOrbitBySuborbit ] );

DeclareOperation( "TestMembership", [IsObject, IsOrbitBySuborbit, IsList] );

##################
# The real thing:
##################

DeclareOperation( "ORB_StabilizerChainKnownSize", [IsGroup,IsPosInt] );
DeclareOperation( "ORB_BaseStabilizerChain", [IsObject] );
                                             # stabilizer chain
DeclareOperation( "ORB_StabilizerChainKnownBase", [IsGroup,IsObject] );
DeclareOperation( "ORB_SizeStabilizerChain", [IsObject] );
DeclareOperation( "ORB_IsWordInStabilizerChain", 
  [IsList,IsList,IsList,IsObject] );
    # word, permgens, permgensinv, and stabilizer chain
DeclareOperation( "ORB_IsElementInStabilizerChain", [IsObject,IsObject] );

DeclareGlobalFunction( "ORB_WordOp" );
DeclareGlobalFunction( "ORB_GetTransversalElement" );
DeclareGlobalFunction( "ORB_PrepareStabgens" );
DeclareGlobalFunction( "OrbitBySuborbit" );
DeclareGlobalFunction( "OrbitBySuborbitKnownSize" );
DeclareGlobalFunction( "OrbitBySuborbitInner" );
DeclareGlobalFunction( "ORB_SiftWord" );
DeclareGlobalFunction( "ORB_WordTuple" );
DeclareGlobalFunction( "ORB_StabOrbitComplete" );
DeclareGlobalFunction( "ORB_StabOrbitSearch" );


###############################
# And helpers for preparation:
###############################

DeclareGlobalFunction( "ORB_NormalizeVector" );
DeclareGlobalFunction( "ORB_CosetRecogGeneric" );
DeclareGlobalFunction( "ORB_CosetRecogPermgroup" );
DeclareGlobalFunction( "OrbitBySuborbitBootstrapForVectors" );
DeclareGlobalFunction( "OrbitBySuborbitBootstrapForLines" );
DeclareGlobalFunction( "ORB_ProjDownForSpaces" );
DeclareGlobalFunction( "OrbitBySuborbitBootstrapForSpaces" );
DeclareGlobalFunction( "ORB_PermuteBasisVectors" );
DeclareGlobalFunction( "ORB_EmbedBaseChangeTopLeft" );


#############################################
# Administrate lists of orbits by suborbits:
#############################################

DeclareCategory( "IsOrbitBySuborbitList", IsComponentObjectRep);
DeclareRepresentation( "IsStdOrbitBySuborbitListRep", IsOrbitBySuborbitList,
  [ "obsos",       # a list of orbit-by-suborbits
    "nrrandels",   # number of random elements
    "randels",     # a list of random elements
    "setup",       # setup
  ] );
BindGlobal( "StdOrbitBySuborbitListType",
  NewType( CollectionsFamily(OrbitBySuborbitFamily), 
           IsOrbitBySuborbitList and IsStdOrbitBySuborbitListRep and
           IsSmallList and IsList and IsMutable) );

# Already there generically:
#DeclareOperation( "Memory", [IsOrbitBySuborbitList] );
DeclareOperation( "TotalLength", [IsOrbitBySuborbitList] );
DeclareOperation( "SavingFactor", [IsOrbitBySuborbitList] );
DeclareGlobalFunction( "InitOrbitBySuborbitList" );
DeclareGlobalFunction( "IsVectorInOrbitBySuborbitList" );
DeclareGlobalFunction( "OrbitsFromSeedsToOrbitList" );
DeclareGlobalFunction( "VerifyDisjointness" );


# Still missing:
# RepresentativeActionForVectorsPrepare (write memory full)
# SearchBackward (depth first)
# DoubleCosetRepsSearcher: (InitDoubleCosetRepsSearcher)
# FeedKnownOrbitsIntoRecord
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
