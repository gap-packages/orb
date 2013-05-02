#############################################################################
##
##                             orb package
##  transform.gi
##                                                        by Juergen Mueller
##                                                       and Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005-2010 by the authors.
##  This file is free software, see license information at the end.
##
##  Optimisations for transformations.
##
#############################################################################

# Note that this file is no longer loaded for GAP >= 4.7.

InstallGlobalFunction( ImageAndKernelOfTransformation,
  function( t )
    local buf,comps,i,image,j,kernel,l,ll,n;
    if IsTransformation(t) then l := t![1]; else l := t; fi;
    n := Length(l);
    buf := 0*[1..n];   # 0 if image does not occur, otherwise equal to first
                       # preimage x, the list is collected in kernel[x]
    kernel := EmptyPlist(n);
    comps := 0;
    for i in [1..Length(l)] do
        j := l[i];
        if buf[j] = 0 then
            comps := comps + 1;
            kernel[i] := [i];
            buf[j] := i;
        else
            Add(kernel[buf[j]],i);
        fi;
    od;
    image := EmptyPlist(comps);
    for j in [1..n] do
        i := buf[j];
        if i > 0 then
            image[Length(image)+1] := j;
        fi;
    od;
    ll := [image,Compacted(kernel)];
    MakeImmutable(ll);
    return ll;
  end );

InstallMethod( ImageSetOfTransformation, "for a transformation",
  [ IsTransformation and IsTransformationRep ], 1,    # to beat the library one
  function( t )
    local tmp;
    if IsBound(t![2]) then return t![2]; fi;
    tmp := ImageAndKernelOfTransformation(t);
    t![2] := tmp[1];
    t![3] := tmp[2];
    return tmp[1];
  end );

InstallMethod( KernelOfTransformation, "for a transformation",
  [ IsTransformation and IsTransformationRep ], 1,    # to beat the library one
  function( t )
    local tmp;
    if IsBound(t![3]) then return t![3]; fi;
    tmp := ImageAndKernelOfTransformation(t);
    t![2] := tmp[1];
    t![3] := tmp[2];
    return tmp[2];
  end );



InstallMethod( PermLeftQuoTransformationNC, "GAP version",
  [IsTransformation and IsTransformationRep,
   IsTransformation and IsTransformationRep],
function ( t1, t2 )
  local  pl, i, deg;
  deg := DegreeOfTransformation( t1 );
  pl := [ 1 .. deg ];
  for i  in [ 1 .. deg ]  do
      pl[i ^ t1] := i ^ t2;
  od;
  return PermList( pl );
end );
InstallOtherMethod( PermLeftQuoTransformationNC, "GAP version",
  [IsList,IsList],
function ( t1, t2 )
  local  pl, i, deg;
  deg := Length(t1);
  pl := [ 1 .. deg ];
  for i  in [ 1 .. deg ]  do
      pl[t1[i]] := t2[i];
  od;
  return PermList( pl );
end );

InstallGlobalFunction( "CanonicalTransSameKernel", 
  function( t )
    local i,l,n,next,res,tab;
    if IsTransformation(t) then
        l := t![1];
    else
        l := t;
    fi;
    n := Length(l);
    tab := 0*[1..n];
    res := EmptyPlist(n);
    next := 1;
    for i in [1..n] do
        if tab[l[i]] <> 0 then
            res[i] := tab[l[i]];
        else
            tab[l[i]] := next;
            res[i] := next;
            next := next + 1;
        fi;
    od;
    return res;
  end );

InstallGlobalFunction( "IsInjectiveTransOnList",
  function( t, l )
    local i,li,n,tab;
    if IsTransformation(t) then li := t![1]; else li := t; fi;
    n := Length(l);
    tab := EmptyPlist(n);
    for i in l do
        if IsBound(tab[li[i]]) then
            return false;
        else
            tab[li[i]] := 1;
        fi;
    od;
    return true;
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
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
