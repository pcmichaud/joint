# ============================================================================
# Name        : Makefile
# Author      : Pierre-Carl Michaud
# Version     : 0.0
# Copyright   : Your copyright notice
# Description : Makefile for SP Project
# ============================================================================

.PHONY: all clean

# Change this line if you are using a different Fortran compiler
FORTRAN_COMPILER = gfortran
FFLAGS = -funroll-all-loops -fopenmp -finit-local-zero -mfpmath=sse -fstrength-reduce  -fbounds-check -llapack   -lblas

OBJ = obj
MAIN1 =  ./runtime/runestimation
MAIN2 = libs/dcdflib.a
SRC1 = $(OBJ)/spmodel.o src/runestimation.f95 

all: $(MAIN2) $(MAIN1) 

$(MAIN1): $(SRC1)
	$(FORTRAN_COMPILER) $(SRC1) libs/dcdflib.a -J$(OBJ) -O3 -o ./runtime/runestimation  $(FFLAGS)  

$(MAIN2):
	$(FORTRAN_COMPILER) -c libs/dcdflib.f/src/*.f
	ar r libs/dcdflib.a *.o
	rm *.o 

$(OBJ)/spmodel.o: src/spmodel.f95
	$(FORTRAN_COMPILER) -J$(OBJ) -O3 -c $< -o $@  $(FFLAGS) #-g

clean:
	rm -f runtime/runestimation obj/* libs/dcdflib.a