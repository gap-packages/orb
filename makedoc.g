##  this creates the documentation, needs: GAPDoc and AutoDoc packages, pdflatex
##  
##  Call this with GAP from within the package directory.
##

if fail = LoadPackage("AutoDoc", ">= 2019.07.17") then
    Error("AutoDoc 2019.07.17 or newer is required");
fi;

AutoDoc(rec( extract_examples := true, scaffold := rec( MainPage := false )));
