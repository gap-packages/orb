#############################################################################
##
##  byorbits.gd              orb package                      
##                                                           Max Neunhoeffer
##                                                              Felix Noeske
##
##  Copyright 2005 Lehrstuhl D für Mathematik, RWTH Aachen
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


#########################
# Stabilizer Iterators: #
#########################

#
# For stabilizers Stab_{U_i}(q) for an U_i-minimal q in P_i the following
# data structure can be used to iterate through the stabilizer:
#
#  stab is a component object with the following components:
#
BindGlobal( "StabIteratorsFamily", NewFamily( "StabIteratorsFamily" ) );
DeclareCategory( "IsStabIterator", IsComponentObjectRep );
DeclareRepresentation( "IsStdStabIteratorRep", IsStabIterator,
  [ "i",     # number of the subgroup in question
    "info",  # a list of length i, at position j we have a list of elements
             # in the transversal of U_{j-1} in U_j. These elements are stored
             # as numbers indexing words in setup.trans[j].
    "pos",   # a list of length i, at position j there is a number indexing
             # an element in info[j]. This is set to all 1 when Reset
             # is called and is incremented after a call to Next
    "cache", # intermediate results to reuse, a list of length i (as above,
             # height of stabilizer, each element is again a list of length
             # k+1 (for each representation), each element 
             # in there is an intermediate result)
    "point", # list of starting points to verify whether cache is valid
  ] );
BindGlobal( "StdStabIteratorsType", 
  NewType( StabIteratorsFamily, IsStabIterator and IsStdStabIteratorRep ) );
DeclareOperation( "StabIterator", [] );   # the constructor
DeclareOperation( "Reset", [ IsStabIterator ] );
DeclareOperation( "Next", [ IsStabIterator ] );
DeclareOperation( "Next", [ IsStabIterator, IsString ] );
DeclareGlobalFunction( "ORB_ApplyStabElement" );


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
DeclareCategory( "IsOrbitBySuborbitSetup", IsComponentObjectRep );
DeclareRepresentation( "IsStdOrbitBySuborbitSetupRep", IsOrbitBySuborbitSetup,
  [ "k",          # number of helper subgroups U_1 < U_2 < ... < U_k
    "size",       # sizes of those groups (i:1..k)
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
                  # used with "ORB_ApplyWord" (i:1..k)
    "pifunc",     # usually equal to \{\}
                  # (pifunc[j][i], for j:1..k+1, i:1..j-1)
    "pi",         # projection function P_j ->> P_i
                  # this is stored as a range used to slice something out
                  # (pi[j][i], for j:1..k+1, i:1..j-1)
    "op",         # Action function/operation for each level (i:1..k+1)
                  # usually OnRight for vectors
    "info",       # the hash table for points x in P_i, that are
                  # U_{i-1}-minimal,
                  # if xU_{i-1} is not the U_i-minimal U_{i-1}-orbit in 
                  #    xU_i, we store:
                  #   - a number of an element t in trans[i] with 
                  #     xtU_{i-1} is the U_i-minimal U_{i-1}-orbit in xU_i
                  # if xU_{i-1} is the U_i-minimal U_{i-1}-orbit in xU_i, 
                  # we store a pair:
                  #   - first entry is boolean,
                  #     - true means: this is the U_i-minimal element in
                  #       xU_i, the second entry is then "stabilizer info":
                  #       the list of numbers of elements t in trans[i] 
                  #       with the property that xtU_{i-1}=xU_{i-1}, 
                  #       which is used for stabs.
                  #     - false means: this is not the U_i-minimal element
                  #       in xU_i, the second entry then describes an
                  #       element in Stab_{U_{i-1}}(pi[i][i-1](p)) to
                  #       apply to get to the minimal one. It is stored
                  #       as a word in the elements in elsinv
                  #   for i=1 we omit the first component, only storing
                  #   the list for the stabilizer info
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
  ] );


############################################
# The heart of the method: Minimalization: #
############################################

DeclareGlobalFunction( "ORB_Minimalize" );


#######################
# Suborbit databases: #
#######################

DeclareCategory( "IsSuborbitDatabase", IsComponentObjectRep );
DeclareRepresentation( "IsStdSuborbitDbRep", IsSuborbitDatabase, 
  [ "reps",        # a list of suborbit representatives
    "mins",        # a hash table to recognise minimal vectors
    "lengths",     # a list of lengths of suborbits
    "setup",       # a reference to the setup object
    "totallength", # total length
  ] );
BindGlobal( "StdSuborbitDatabasesType",
  NewType( OrbitsFamily, IsSuborbitDatabase and IsStdSuborbitDbRep ) );

DeclareOperation( "SuborbitDatabase", [ IsOrbitBySuborbitSetup, IsPosInt ] );
DeclareOperation( "StoreSuborbit", 
                  [ IsSuborbitDatabase, IsObject, IsStabIterator ] );
DeclareOperation( "LookupSuborbit", [ IsObject, IsSuborbitDatabase ] );
DeclareOperation( "TotalLength", [ IsSuborbitDatabase ] );
DeclareOperation( "Representatives", [ IsSuborbitDatabase ] );

DeclareGlobalFunction( "OrbitBySuborbit" );

DeclareGlobalFunction( "OrbitBySuborbitBootstrapForVectors" );
DeclareGlobalFunction( "ORB_CosetRecogGeneric" );

DeclareGlobalFunction( "ORB_NextStabIterator2" );
DeclareGlobalFunction( "ORB_ApplyUElement" );

DeclareGlobalFunction( "OrbitBySuborbitWithKnownSize" );

# Still missing:
# RepresentativeActionForVectorsPrepare (write memory full)
# SearchBackward (depth first)
# DoubleCosetRepsSearcher: (InitDoubleCosetRepsSearcher)
# FeedKnownOrbitsIntoRecord
