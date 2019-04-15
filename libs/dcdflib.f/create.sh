# used to create library dcdflib.a
gfortran -c src/*.f 
ar r ../dcdflib-cygwin.a *.o
rm *.o
