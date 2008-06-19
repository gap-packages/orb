##
LoadPackage("orb");;
LoadPackage("cvec");;
f := IO_File("data.gp");
IO_Unpickle(f);
v:=IO_Unpickle(f);;
IO_Unpickle(f);
bgens := IO_Unpickle(f);;
egens := IO_Unpickle(f);;
fgens := IO_Unpickle(f);;
mgens := IO_Unpickle(f);;
lgens := IO_Unpickle(f);;
IO_Unpickle(f);
mpermgens := IO_Unpickle(f);;
lpermgens := IO_Unpickle(f);;
IO_Close(f);;

##
setup := OrbitBySuborbitBootstrapForVectors(
         [lgens,mgens,bgens],[lpermgens,mpermgens,[(),()]],
         [660,95040,4154781481226426191177580544000000],[20,32],rec());
o := OrbitBySuborbitKnownSize(setup,v,3,3,2,51,13571955000);
