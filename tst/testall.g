#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage("atlasrep");
LoadPackage("cvec");
LoadPackage("io");
LoadPackage("orb");

d := DirectoriesPackageLibrary("orb", "tst");
exclude:=[];
if not CompareVersionNumbers(GAPInfo.Version, "4.12") then
  Append(exclude, [
    "m11PF3d24/M11OrbitOnPF3d24.tst",
    "m22p770.tst",
  ]);
fi;

TestDirectory(d[1], rec(exitGAP := true, exclude:=exclude));

FORCE_QUIT_GAP(1);
