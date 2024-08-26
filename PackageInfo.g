#############################################################################
##  
##  PackageInfo.g for the package `orb'                       
##

SetPackageInfo( rec(

PackageName := "orb",
Subtitle := "Methods to enumerate orbits",
Version := "4.9.1",
Date := "26/08/2024", # dd/mm/yyyy format
License := "GPL-3.0-or-later",

##  Information about authors and maintainers.
Persons := [
  rec( 
    LastName      := "Mueller",
    FirstNames    := "Juergen",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "juergen.mueller@math.rwth-aachen.de",
    WWWHome       := "http://www.math.rwth-aachen.de/~Juergen.Mueller",
    PostalAddress := Concatenation( [
                       "Juergen Mueller\n",
                       "Lehrstuhl D fuer Mathematik, RWTH Aachen\n",
                       "Templergraben 64\n",
                       "52056 Aachen\n",
                       "Germany" ] ),
    Place         := "Aachen",
    Institution   := "RWTH Aachen"
  ),
  rec( 
    LastName      := "Neunhöffer",
    FirstNames    := "Max",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "max@9hoeffer.de",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~neunhoef",
    PostalAddress := Concatenation( [
                       "Gustav-Freytag-Straße 40\n",
                       "50354 Hürth\n",
                       "Germany" ] ),
    #Place         := "St Andrews",
    #Institution   := "University of St Andrews"
  ),
  rec( 
    LastName      := "Noeske",
    FirstNames    := "Felix",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "felix.noeske@math.rwth-aachen.de",
    WWWHome       := "http://www.math.rwth-aachen.de/~Felix.Noeske",
    PostalAddress := Concatenation( [
                       "Felix Noeske\n",
                       "Lehrstuhl D fuer Mathematik, RWTH Aachen\n",
                       "Templergraben 64\n",
                       "52056 Aachen\n",
                       "Germany" ] ),
    Place         := "Aachen",
    Institution   := "RWTH Aachen"
  ),
  rec(
    LastName      := "Horn",
    FirstNames    := "Max",
    IsAuthor      := false,
    IsMaintainer  := true,
    Email         := "mhorn@rptu.de",
    WWWHome       := "https://www.quendi.de/math",
    PostalAddress := Concatenation(
                       "Fachbereich Mathematik\n",
                       "RPTU Kaiserslautern-Landau\n",
                       "Gottlieb-Daimler-Straße 48\n",
                       "67663 Kaiserslautern\n",
                       "Germany" ),
    Place         := "Kaiserslautern, Germany",
    Institution   := "RPTU Kaiserslautern-Landau"
  ),
],

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "deposited"     for packages for which the GAP developers agreed 
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages 
##    "other"         for all other packages
##
# Status := "accepted",
Status := "deposited",

##  You must provide the next two entries if and only if the status is 
##  "accepted" because is was successfully refereed:
# format: 'name (place)'
# CommunicatedBy := "Mike Atkinson (St. Andrews)",
#CommunicatedBy := "",
# format: mm/yyyy
# AcceptDate := "08/1999",
#AcceptDate := "",

SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/orb",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://gap-packages.github.io/orb",
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/orb-", ~.Version ),
ArchiveFormats := ".tar.gz",

##  Here you  must provide a short abstract explaining the package content 
##  in HTML format (used on the package overview Web page) and an URL 
##  for a Webpage with more detailed information about the package
##  (not more than a few lines, less is ok):
##  Please, use '<span class="pkgname">GAP</span>' and
##  '<span class="pkgname">MyPKG</span>' for specifing package names.
##  
AbstractHTML := 
  "The <span class=\"pkgname\">orb</span> package is about enumerating \
orbits in various ways.",
#

PackageDoc := rec(
  BookName  := "orb",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Methods to enumerate orbits",
),

Dependencies := rec(
  GAP := ">=4.12",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [["IO",">= 3.3"]],
  ExternalConditions := []
),

AvailabilityTest := function()
    if IsKernelExtensionAvailable("orb") = false then
      LogPackageLoadingMessage( PACKAGE_WARNING,
              [ "kernel functions for orb are not available." ] );
      return false;
    fi;
    return true;
end,

##  *Optional*, but recommended: path relative to package root to a file which 
##  contains as many tests of the package functionality as sensible.
TestFile := "tst/testall.g",

##  *Optional*: Here you can list some keyword related to the topic 
##  of the package.
Keywords := ["Orbit huge", "OrbitBySuborbit", "hash tables", 
             "searching in groups"],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
                    "&copyright; 2005-2014 by Jürgen Müller, Max Neunhöffer and Felix Noeske<P/>\n",
                    "\n",
                    "This program is free software: you can redistribute it and/or modify\n",
                    "it under the terms of the GNU General Public License as published by\n",
                    "the Free Software Foundation, either version 3 of the License, or\n",
                    "(at your option) any later version.\n",
                    "\n",
                    "This program is distributed in the hope that it will be useful,\n",
                    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n",
                    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",
                    "GNU General Public License for more details.\n",
                    "\n",
                    "You should have received a copy of the GNU General Public License\n",
                    "along with this program.  If not, see\n",
                    "<URL><Link>https://www.gnu.org/licenses/</Link></URL>.\n"
                ),
    )
),

));


