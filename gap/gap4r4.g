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

DeclareAttribute( "SlotUsagePattern", IsStraightLineProgram );
DeclareAttribute( "LargestNrSlots", IsStraightLineProgram );
##
#I  InfoSLP
##
DeclareInfoClass( "InfoSLP" );
SetInfoLevel(InfoSLP,1);
##
#A  SlotUsagePattern( <s> )
##
##  <ManSection>
##  <Attr Name="SlotUsagePattern" Arg="s"/>
##
##  <Description>
##  Analyses the straight line program <A>s</A> for more efficient
##  evaluation. When this attribute is known, the evaluation of the
##  SLP needs less memory.
##  </Description>
##  </ManSection>
InstallMethod( SlotUsagePattern, "for an slp",
  [ IsStraightLineProgram ],
  function( slp )
    local addedun,cur,deletions,i,j,l,li,nr,res,slotusage,step,su,
          unnecessary,used;
    l := LinesOfStraightLineProgram(slp);
    nr := NrInputsOfStraightLineProgram(slp);

    unnecessary := [];

    repeat     # we repeat leaving out unused lines until it becomes stable
        addedun := false;
        # First compute what is when read and written:
        slotusage := List([1..nr],i->[0]);   # means: written in step 0
        cur := nr+1;  # the current slot
        for step in [1..Length(l)] do
            if step in unnecessary then continue; fi;
            li := l[step];
            if IsInt(li[1]) then     # standard line without write
                # We write to cur and read from every second entry in li:
                if not(IsBound(slotusage[cur])) then slotusage[cur] := []; fi;
                used := Set(li{[1,3..Length(li)-1]});
                for i in used do
                    Add(slotusage[i],step);
                od;
                Add(slotusage[cur],-step);
                res := cur;
                cur := cur + 1;
            elif IsList(li[1]) and IsInt(li[2]) then  # a standard line with w.
                if not(IsBound(slotusage[li[2]])) then 
                    slotusage[li[2]] := []; 
                fi;
                used := Set(li[1]{[1,3..Length(li[1])-1]});
                for i in used do
                    Add(slotusage[i],step);
                od;
                Add(slotusage[li[2]],-step);
                res := li[2];
                cur := Maximum(cur,li[2]+1);
            else   # a return line
                used := [];
                for i in [1..Length(li)] do
                    for j in li[i]{[1,3..Length(li[i])-1]} do
                        AddSet(used,j);
                    od;
                od;
                for i in used do
                    Add(slotusage[i],step);
                od;
                res := 0;
            fi;
        od;
        # Note the reading of the result if needed:
        if res <> 0 then
            Add(slotusage[res],Length(l)+1);
        fi;

        # Compute possible deletions from the slotusage:
        deletions := List([1..Length(l)],i->[]);
        for i in [1..Length(slotusage)] do
            su := slotusage[i];
            for j in [1..Length(su)-1] do
                if su[j] > 0 and su[j+1] < 0 and su[j] <> -su[j+1] then
                    Add(deletions[su[j]],i);
                fi;
            od;
            if Length(su) > 0 then
                if su[Length(su)] < 0 then
                    Add(unnecessary,-su[Length(su)]);
                    addedun := true;
                elif su[Length(su)] < Length(l) then
                    Add(deletions[su[Length(su)]],i);
                fi;
            fi;
        od;
        if addedun then
            Info(InfoSLP,3,"#Warning: Unnecessary steps: ",unnecessary);
        fi;
    until not(addedun);
    if Length(unnecessary) > 0 then
        Info(InfoSLP,1,"#Warning: Total unnecessary steps: ",
             Length(unnecessary));
    fi;
    return rec( slotusage := slotusage, largestused := Length(slotusage),
                unnecessary := unnecessary, deletions := deletions );
  end );

InstallMethod( ResultOfStraightLineProgram,
  "for a straight line program with slot usage pattern, a list",
  [ IsStraightLineProgram and HasSlotUsagePattern, IsHomogeneousList ],
  function( prog, gens )
    local cur,i,line,r,res,step,sup,nrslots,maxnrslots;

    # Initialize the list of intermediate results.
    r:= ShallowCopy( gens );
    res:= false;
    sup := SlotUsagePattern(prog);
    step := 1;
    cur := Length(r)+1;
    nrslots := Length(r);
    maxnrslots := nrslots;

    # Loop over the program.
    for line in LinesOfStraightLineProgram( prog ) do
      if not(step in sup.unnecessary) then
          if   not IsEmpty( line ) and IsInt( line[1] ) then
            # The line describes a word to be appended.
            r[cur] := ResultOfLineOfStraightLineProgram( line, r );
            res:= r[cur];
            cur := cur + 1;
            nrslots := nrslots + 1;
            if nrslots > maxnrslots then maxnrslots := nrslots; fi;
          elif 2 <= Length( line ) and IsInt( line[2] ) then
            # The line describes a word that shall replace.
            if not(IsBound(r[line[2]])) then
                nrslots := nrslots + 1;
                if nrslots > maxnrslots then maxnrslots := nrslots; fi;
            fi;
            r[ line[2] ]:= ResultOfLineOfStraightLineProgram( line[1], r );
            res:= r[line[2]];
            cur := Maximum(cur,line[2]+1);
          else
            # The line describes a list of words to be returned.
            res := 0*[1..Length(line)];
            for i in [1..Length(line)] do
                res[i] := ResultOfLineOfStraightLineProgram(line[i],r);
                Info(InfoSLP,2,"Have computed result ",i," of ",
                     Length(line),".");
            od;
            return res;
          fi;
          # Delete unused stuff:
          for i in sup.deletions[step] do 
              Unbind(r[i]); 
              nrslots := nrslots-1;
          od;
          if InfoLevel(InfoSLP) >= 2 then
              Print("Step ",step," of ",
               Length(LinesOfStraightLineProgram(prog))," done, used slots: ",
               nrslots,"/",maxnrslots,".\r");
          fi;
      else
          if InfoLevel(InfoSLP) >= 3 then
              Print("Unnecessary step ",step," of ",
               Length(LinesOfStraightLineProgram(prog))," skipped.        \n");
          fi;
      fi;

      step := step + 1;
    od;

    # Return the result.
    return res;
  end );

##
#A  LargestNrSlots( <s> )
##
##  <ManSection>
##  <Attr Name="LargestNrSlots" Arg="s"/>
##
##  <Description>
##  Returns the maximal number of slots used during the evaluation of
##  the SLP <A>s</A>.
##  </Description>
##  </ManSection>

InstallMethod( LargestNrSlots, "for a straight line program",
  [ IsStraightLineProgram ],
  function( slp )
    local cur,i,line,maxnrslots,nrslots,r,step,sup;
    sup := SlotUsagePattern(slp);
    nrslots := NrInputsOfStraightLineProgram(slp);
    step := 1;
    cur := nrslots+1;
    r := 0*[1..nrslots];
    maxnrslots := nrslots;

    # Loop over the program.
    for line in LinesOfStraightLineProgram( slp ) do
      if not(step in sup.unnecessary) then
          if   not IsEmpty( line ) and IsInt( line[1] ) then
            # The line describes a word to be appended.
            r[cur] := 1;
            nrslots := nrslots + 1;
            if nrslots > maxnrslots then maxnrslots := nrslots; fi;
            cur := cur + 1;
          elif 2 <= Length( line ) and IsInt( line[2] ) then
            # The line describes a word that shall replace.
            if not(IsBound(r[line[2]])) then
                r[line[2]] := 1;
                nrslots := nrslots + 1;
                if nrslots > maxnrslots then maxnrslots := nrslots; fi;
            fi;
            cur := Maximum(cur,line[2]+1);
          else
            # The line describes a list of words to be returned.
            return maxnrslots;
          fi;
          # Delete unused stuff:
          for i in sup.deletions[step] do 
              Unbind(r[i]); 
              nrslots := nrslots - 1;
          od;
      fi;
      step := step + 1;
    od;
    return maxnrslots;
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
