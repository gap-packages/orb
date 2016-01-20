#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage("orb");
d := DirectoriesPackageLibrary("orb", "tst");

Test(Filename(d, "hash.tst"));

Read(Filename(d, "avltest.g"));
Read(Filename(d, "m22p770.g"));
