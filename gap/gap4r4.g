#############################################################################
##
##                             orb package
##  gap4r4.g
##                                                        by Juergen Mueller
##                                                       and Max Neunhoeffer
##                                                          and Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Compatibility things for GAP 4.4 as opposed to >= 4.5
##
#############################################################################

BindGlobal( "RowLength", function( m ) return Length(m[1]); end );

BindGlobal( "Matrix", function( l, rowlength, sample )
  if IsGF2MatrixRep(sample) or Is8BitMatrixRep(sample) then
      ConvertToMatrixRep(l);
  fi;
  return l;
end );

DeclareOperation( "Randomize", [IsObject] );
DeclareOperation( "Randomize", [IsObject, IsRandomSource] );

InstallMethod( Randomize, "for a mutable gf2 vector",
  [ IsGF2VectorRep and IsMutable ],
  function( v )
    local i;
    MultRowVector(v,0);
    for i in [1..Length(v)] do
        if Random(0,1) = 1 then v[i] := Z(2); fi;
    od;
    return v;
  end );

InstallMethod( Randomize, "for a mutable gf2 vector and a random source",
  [ IsGF2VectorRep and IsMutable, IsRandomSource ],
  function( v, rs )
    local i;
    MultRowVector(v,0);
    for i in [1..Length(v)] do
        if Random(rs,0,1) = 1 then v[i] := Z(2); fi;
    od;
    return v;
  end );

InstallMethod( Randomize, "for a mutable gf2 matrix",
  [IsGF2MatrixRep and IsMutable],
  function( m )
    local v;
    for v in m do Randomize(v); od;
    return m;
  end );

InstallMethod( Randomize, "for a mutable gf2 matrix, and a random source",
  [IsGF2MatrixRep and IsMutable, IsRandomSource],
  function( m, rs )
    local v;
    for v in m do Randomize(v,rs); od;
    return m;
  end );

InstallMethod( Randomize, "for a mutable 8bit vector",
  [ Is8BitVectorRep and IsMutable ],
  function( v )
    local f,i;
    f := GF(Q_VEC8BIT(v));
    for i in [1..Length(v)] do v[i] := Random(f); od;
  end );

InstallMethod( Randomize, "for a mutable 8bit vector and a random source",
  [ Is8BitVectorRep and IsMutable, IsRandomSource ],
  function( v, rs )
    local l,i;
    l := AsSSortedList(GF(Q_VEC8BIT(v)));
    for i in [1..Length(v)] do v[i] := Random(rs,l); od;
    return v;
  end );

InstallMethod( Randomize, "for a mutable 8bit matrix",
  [Is8BitMatrixRep and IsMutable],
  function( m )
    local v;
    for v in m do Randomize(v); od;
    return m;
  end );

InstallMethod( Randomize, "for a mutable 8bit matrix, and a random source",
  [Is8BitMatrixRep and IsMutable, IsRandomSource],
  function( m, rs )
    local v;
    for v in m do Randomize(v,rs); od;
    return m;
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
