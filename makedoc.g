##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##

SetPackagePath("orb", ".");
PrintTo("VERSION", PackageInfo("orb")[1].Version);

LoadPackage("GAPDoc");

MakeGAPDocDoc("doc", "orb", [], "orb");
CopyHTMLStyleFiles("doc");
GAPDocManualLab("orb");

QUIT;
