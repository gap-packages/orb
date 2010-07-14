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


if IsBound(ImageAndKernelOfTransformation_C) then
  InstallGlobalFunction( ImageAndKernelOfTransformation,
                         ImageAndKernelOfTransformation_C );
else
  InstallGlobalFunction( ImageAndKernelOfTransformation,
  function( t )
    local buf,comps,i,image,j,kernel,l,ll,n;
    l := t![1];
    n := Length(l);
    buf := 0*[1..n];
    comps := 0;
    for i in l do
        if buf[i] = 0 then
            comps := comps + 1;
        fi;
        buf[i] := buf[i] + 1;
    od;
    kernel := EmptyPlist(comps);
    image := EmptyPlist(comps);
    j := 1;
    for i in [1..n] do
        if buf[i] > 0 then
            image[j] := i;
            kernel[j] := EmptyPlist(buf[i]);
            buf[i] := j;
            j := j + 1;
        fi;
    od;
    for i in [1..n] do
        ll := kernel[buf[l[i]]];
        ll[Length(ll)+1] := i;
    od;
    Sort(kernel);
    ll := [image,kernel];
    MakeImmutable(ll);
    return ll;
  end );
fi;

ImageAndKernelOfTransformation2 :=
  function( t )
    local buf,comps,i,image,j,kernel,l,ll,n;
    l := t![1];
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
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
