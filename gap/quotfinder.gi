#############################################################################
##
##                             orb package
##  quotfinder.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for the quotfinder
##
#############################################################################

InstallGlobalFunction( QuotFinder,
  function( gens, chain )
    local qf;
    qf := rec( origgens := gens, chain := chain, bcgens := gens,
               dim := Length(gens[1]), subgrpdepth := 0,
               curgens := gens, curchain := chain, curcodim := Length(gens[1]),
               basech := OneMutable(gens[1]), basechi := OneMutable(gens[1]),
               bcplan := OneMutable(gens[1]), bcplani := OneMutable(gens[1]) );
    return qf;
  end );

InstallGlobalFunction( QF_Recalccurgens,
  function( qf )
    # uses bcgens, dim, curcodim, and subgrpdepth
    # updates curgens and curchain
    local gens,i;
    qf.curchain := qf.chain;
    gens := qf.bcgens;
    for i in [1..qf.subgrpdepth] do
        gens := ResultOfStraightLineProgram(qf.curchain!.slp,gens);
        qf.curchain := qf.curchain!.r;
    od;
    if qf.curcodim < qf.dim then
        for i in [1..Length(gens)] do
            if not(IsZero(ExtractSubMatrix(gens[i],[1..qf.dim-qf.curcodim],
                                     [qf.dim-qf.curcodim+1..qf.dim]))) then
               Print("Warning: generator #",i," does not respect subspace!\n");
            fi;
        od;
        qf.curgens := List(gens,x->
             ExtractSubMatrix(x,[qf.dim-qf.curcodim+1..qf.dim],
                                [qf.dim-qf.curcodim+1..qf.dim]));
    else
        qf.curgens := gens;
    fi;
    Unbind(qf.m);
    Unbind(qf.r);
    Unbind(qf.blocksizes);
    qf.bcplan := OneMutable(qf.curgens[1]);
    qf.bcplani := OneMutable(qf.curgens[1]);
  end );

InstallGlobalFunction( QF_Chop,
  function( qf )
    local codim, i, m, r;
    m := Module( ShallowCopy(qf.curgens) );
    qf.m := m;
    r := Chop( m, rec( compbasis := true ) );      # with base change
    qf.r := r;
    qf.bcplan := r!.basis;
    qf.bcplani := r!.basis^-1;
    Print("Codimensions: ");
    codim := 0;
    for i in [Length(r!.acs),Length(r!.acs)-1..1] do
        codim := codim + Dimension(r!.db[r!.acs[i]]);
        Print( codim," " );
    od;
    Print("\n");
  end );

InstallGlobalFunction( QF_Semisimple,
  function( qf )
    local i,d;
    if not( IsBound( qf.m ) ) then
        Print("Not yet chopped! Use QF_Chop first.\n");
        return;
    fi;
    SemiSimplicityBasis( qf.r, qf.curchain );
    qf.bcplan := qf.r!.basis;
    qf.bcplani := qf.r!.ibasis;
    Print("Block sizes: ");
    qf.blocksizes := [];
    for i in [1..Length(qf.r!.acs)] do
        d := Dimension(qf.r!.db[qf.r!.acs[i]]);
        Print(i,":",d," ");
        Add( qf.blocksizes, d );
    od;
    Print("\n");
  end );

InstallGlobalFunction( QF_ApplyBaseChange,
  function( qf )
    local bc,bci;
    if qf.curcodim < qf.dim then  # we have to expand the basechange
        bc := OneMutable( qf.origgens[1] );
        CopySubMatrix( qf.bcplan, bc, 
                       [1..qf.curcodim], [qf.dim-qf.curcodim+1..qf.dim],
                       [1..qf.curcodim], [qf.dim-qf.curcodim+1..qf.dim] );
        bci := bc^-1;
    else
        bc := qf.bcplan;
        bci := qf.bcplani;
    fi;
    qf.curgens := List(qf.curgens,x->qf.bcplan*x*qf.bcplani);
    qf.bcgens := List(qf.bcgens,x->bc*x*bci);
    qf.basech := bc*qf.basech;
    qf.basechi := qf.basechi*bci;
    qf.bcplan := OneMutable(qf.origgens[1]);
    qf.bcplani := OneMutable(qf.origgens[1]);
  end );

InstallGlobalFunction( QF_Show,
  function( arg )
    local s,qf,what,i;
    if Length(arg) = 0 then
        Print("Usage: QF_Show( qf [,what] ), where what can be:\n");
        Print("       \"orig\" : the original generators\n");
        Print("       \"cur\"  : the current generators in quotient\n");
        Print("       \"bc\"   : the current generators in full module\n");
        Print("       \"plan\" : the would-be generators according to plan\n");
        return;
    elif Length(arg) = 1 then
        qf := arg[1];
        if IsOne( qf.bcplan ) then
            what := "cur";
        else
            what := "plan";
        fi;
    else
        qf := arg[1];
        what := arg[2];
    fi;
    Print("Current subgroup size: ",Size(qf.curchain),"\n");
    if what = "cur" then
        Print("Current codimension: ",qf.curcodim, 
              ", sum of current generators:\n");
        s := Sum(qf.curgens);
    elif what = "bc" then
        Print("Dimension: ",qf.dim,", Sum of current generators:\n");
        s := Sum(qf.bcgens);
    elif what = "orig" then
        Print("Dimension: ",qf.dim,", Sum of original generators:\n");
        s := Sum(qf.origgens);
    else   # what = "plan"
        Print("Current codimension: ",qf.curcodim,
              ", sum of would-be generators according to plan:\n");
        s := qf.bcplan * Sum(qf.curgens) * qf.bcplani;
    fi;
    if Length(s) < 75 then
        Display(s);
    else
        OverviewMat(s);
    fi;
    if IsBound( qf.blocksizes ) then
        Print("Blocksizes: ");
        for i in [1..Length(qf.blocksizes)] do
            Print(i,":",qf.blocksizes[i]," ");
        od;
        Print("\n");
    fi;
  end );

InstallGlobalFunction( QF_Home,
  function( qf )
    qf.subgrpdepth := 0;
    qf.curcodim := qf.dim;
    QF_Recalccurgens(qf);
    Print("Back to full dimension ",qf.dim," and full group with size ",
          Size(qf.curchain),"\n");
  end );

InstallGlobalFunction( QF_GotoSubgroup,
  function( qf, n )
    qf.subgrpdepth := n-1;
    QF_Recalccurgens(qf);
    Print("Current subgroup size: ",Size(qf.curchain),"\n");
  end );

InstallGlobalFunction( QF_GotoQuotient,
  function( qf, codim )
    qf.curcodim := codim;
    QF_Recalccurgens(qf);
    Print("Current quotient dimension: ",codim,"\n");
  end );

InstallGlobalFunction( QF_OrbitStatistics,
  function( arg )
    local qf,seconds;
    if Length(arg) = 0 then
        Print("Usage: QF_OrbitStatistics( qf [, seconds] )\n");
        return;
    elif Length(arg) = 1 then
        seconds := 10;
    else
        seconds := arg[2];
    fi;
    qf := arg[1];
    OrbitStatisticOnVectorSpace( qf.curgens,Size(qf.curchain),seconds );
  end );

InstallGlobalFunction( QF_OrbitStatisticsLines,
  function( arg )
    local qf,seconds;
    if Length(arg) = 0 then
        Print("Usage: QF_OrbitStatisticsLines( qf [, seconds] )\n");
        return;
    elif Length(arg) = 1 then
        seconds := 10;
    else
        seconds := arg[2];
    fi;
    qf := arg[1];
    OrbitStatisticOnVectorSpaceLines( qf.curgens,Size(qf.curchain),seconds );
  end );

InstallGlobalFunction( QF_PermuteBlocks,
  function( qf, perm )
    local nrblocks,newblocksizes,newblockorder,posold,posnew,oldblockstarts,
          o,i,j,bc;

    if not( IsBound( qf.blocksizes ) ) then
        Print("Do not have blocksizes. Either set or use QF_Chop and ",
              "QF_Semisimple first.\n");
        return;
    fi;
    nrblocks := Length(qf.blocksizes);
    newblocksizes := Permuted(qf.blocksizes,perm);
    newblockorder := Permuted([1..nrblocks],perm);
    posold := 1;
    oldblockstarts := [];
    for i in [1..nrblocks] do
        Add(oldblockstarts,posold);  posold := posold + qf.blocksizes[i];
    od;
    bc := ZeroMutable(qf.curgens[1]);
    posnew := 1;
    o := One(BaseField(bc));
    for i in [1..nrblocks] do
        # Move an old block into its new i-th position:
        posold := oldblockstarts[newblockorder[i]];
        for j in [1..newblocksizes[i]] do
            bc[posnew][posold] := o;
            posnew := posnew + 1;
            posold := posold + 1;
        od;
    od;
    qf.bcplan := bc * qf.bcplan;
    qf.bcplani := qf.bcplani * bc^-1;
    qf.blocksizes := newblocksizes;
  end );
    
InstallGlobalFunction( QF_Flip,
  function( qf )
    local m;
    m := Reversed(OneMutable(qf.bcplan));
    qf.bcplan := m * qf.bcplan;
    qf.bcplani := qf.bcplani * m;
    if IsBound(qf.blocksizes) then
        qf.blocksizes := Reversed(qf.blocksize);
    fi;
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
##  along with this program.  If not, see <https://www.gnu.org/licenses/>.
##
