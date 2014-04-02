#############################################################################
##  
##  PackageInfo.g for the package `orb'                       
##                                                            Juergen Mueller
##                                                            Max Neunhoeffer
##                                                               Felix Noeske
##

SetPackageInfo( rec(

PackageName := "orb",
Subtitle := "Methods to enumerate Orbits",
Version := "4.7",
Date := "31/03/2014", # dd/mm/yyyy format

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
    LastName      := "Neunhoeffer",
    FirstNames    := "Max",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "neunhoef@mcs.st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~neunhoef",
    PostalAddress := Concatenation( [
                       "School of Mathematics and Statistics\n",
                       "Mathematical Institute\n",
                       "North Haugh\n",
                       "St Andrews, Fife KY16 9SS\n",
                       "Scotland, UK" ] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
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

PackageWWWHome := "http://neunhoef.github.io/orb/",
README_URL     := Concatenation(~.PackageWWWHome, "README"),
PackageInfoURL := Concatenation(~.PackageWWWHome, "PackageInfo.g"),
ArchiveURL     := Concatenation("https://github.com/neunhoef/orb/",
                                "releases/download/v", ~.Version,
                                "/orb-", ~.Version),
ArchiveFormats := ".tar.gz .tar.bz2",

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
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Methods to enumerate orbits",
),

Dependencies := rec(
  GAP := ">=4.4.12",
  NeededOtherPackages := [["GAPDoc", ">= 1.2"]],
  SuggestedOtherPackages := [["IO",">= 3.3"]],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,

##  *Optional*, but recommended: path relative to package root to a file which 
##  contains as many tests of the package functionality as sensible.
#TestFile := "tst/testall.g",

##  *Optional*: Here you can list some keyword related to the topic 
##  of the package.
Keywords := ["Orbit huge", "OrbitBySuborbit", "hash tables", 
             "searching in groups"]

));


