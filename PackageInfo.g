#############################################################################
##  
##  PackageInfo.g for the package `orb'                       
##                                                            Juergen Mueller
##                                                            Max Neunhoeffer
##                                                               Felix Noeske
##

##  With a new release of the package at least the entries .Version, .Date and
##  .ArchiveURL must be updated.

SetPackageInfo( rec(

##  This is case sensitive, use your preferred spelling.
#
PackageName := "orb",

Subtitle := "orb - Methods to enumerate Orbits",

Version := "4.3",
##  Please adjust also the VERSION file in the package directory when
##  changing this.

##  Release date of the current version in dd/mm/yyyy format.
# 
Date := "28/05/2012",

ArchiveURL := "http://www-groups.mcs.st-and.ac.uk/~neunhoef/Computer/Software/Gap/orb/orb-4.3",

ArchiveFormats := ".tar.gz",

##  If not all of the archive formats mentioned above are provided, these 
##  can be produced at the GAP side. Therefore it is necessary to know which
##  files of the package distribution are text files which should be unpacked
##  with operating system specific line breaks. There are the following 
##  possibilities to specify the text files:
##  
##    - specify below a component 'TextFiles' which is a list of names of the 
##      text files, relative to the package root directory (e.g., "lib/bla.g")
##    - specify below a component 'BinaryFiles' as list of names, then all other
##      files are taken as text files.
##    - if no 'TextFiles' or 'BinaryFiles' are given and a .zoo archive is
##      provided, then the files in that archive with a "!TEXT!" comment are
##      taken as text files
##    - otherwise: exactly the files with names matching the regular expression
##      ".*\(\.txt\|\.gi\|\.gd\|\.g\|\.c\|\.h\|\.htm\|\.html\|\.xml\|\.tex\|\.six\|\.bib\|\.tst\|README.*\|INSTALL.*\|Makefile\)"
##      are taken as text files
##  
##  (Remark: Just providing a .tar.gz file will often result in useful
##  archives)
##  
##  These entries are *optional*.
#TextFiles := ["init.g", ......],
#BinaryFiles := ["doc/manual.dvi", ......],


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
    IsMaintainer  := true,
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

README_URL := 
  "http://www-groups.mcs.st-and.ac.uk/~neunhoef/Computer/Software/Gap/orb/README.orb",
PackageInfoURL := 
  "http://www-groups.mcs.st-and.ac.uk/~neunhoef/Computer/Software/Gap/orb/PackageInfo.g",

##  Here you  must provide a short abstract explaining the package content 
##  in HTML format (used on the package overview Web page) and an URL 
##  for a Webpage with more detailed information about the package
##  (not more than a few lines, less is ok):
##  Please, use '<span class="pkgname">GAP</span>' and
##  '<span class="pkgname">MyPKG</span>' for specifing package names.
##  
# AbstractHTML := "This package provides  a collection of functions for \
# computing the Smith normal form of integer matrices and some related \
# utilities.",
AbstractHTML := 
  "The <span class=\"pkgname\">orb</span> package is about enumerating \
orbits in various ways.",
#
PackageWWWHome := "http://www-groups.mcs.st-and.ac.uk/~neunhoef/Computer/Software/Gap/orb.html",

PackageDoc := rec(
  BookName  := "orb",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "orb - Methods to enumerate orbits",
  Autoload  := true
),


Dependencies := rec(
  GAP := ">=4.4.12",
  NeededOtherPackages := [["GAPDoc", ">= 1.2"]],
  SuggestedOtherPackages := [["IO",">= 3.3"]],
  ExternalConditions := []
                      
),

##  Provide a test function for the availability of this package.
##  For packages which will not fully work, use 'Info(InfoWarning, 1,
##  ".....")' statements. For packages containing nothing but GAP code,
##  just say 'ReturnTrue' here.
##  With the new package loading mechanism (GAP >=4.4)  the availability
##  tests of other packages, as given under .Dependencies above, will be 
##  done automatically and need not be included in this function.
AvailabilityTest := ReturnTrue,

##  Suggest here if the package should be *automatically loaded* when GAP is 
##  started.  This should usually be 'false'. Say 'true' only if your package 
##  provides some improvements of the GAP library which are likely to enhance 
##  the overall system performance for many users.
Autoload := false,

##  *Optional*, but recommended: path relative to package root to a file which 
##  contains as many tests of the package functionality as sensible.
#TestFile := "tst/testall.g",

##  *Optional*: Here you can list some keyword related to the topic 
##  of the package.
Keywords := ["Orbit huge", "OrbitBySuborbit", "hash tables", 
             "searching in groups"]

));


