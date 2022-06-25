#############################################################################
##
##                             orb package
##  transform.gd
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

# Note that this file is not loaded for GAP >= 4.7 any more.

DeclareGlobalFunction( "ImageAndKernelOfTransformation" );
DeclareOperation( "PermLeftQuoTransformationNC", 
  [ IsTransformation, IsTransformation ]);
DeclareGlobalFunction( "CanonicalTransSameKernel" );
DeclareGlobalFunction( "IsInjectiveTransOnList" );

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
