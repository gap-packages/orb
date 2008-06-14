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
IO_Close(f);; ]]>

##

##

##
##
##
