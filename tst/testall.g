#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage("orb");
d := DirectoriesPackageLibrary("orb", "tst");
Read(Filename(d, "avltest.g"));
Read(Filename(d, "m22p770.g"));

#Test(Filename(d, "bugfix.tst"));
