FC=gfortran -c
#FC=ifort -c
#FC=nagfor -c

FFLAGS=-std=f2008 -Wall -fcheck=all
#FFLAGS=-std=f2008 -Wall -fcheck=all -g -O0
#FFLAGS=-std -warn all -check all -traceback
#FFLAGS=-f2008 -w=all -C=all -gline

LD=gfortran
#LD=ifort
#LD=nagfor

LDFLAGS=
#LDFLAGS=-g

OBJS=constants.o char_utilities.o values.o targets.o switches.o files.o getopt.o symbols.o expressions.o coco.o

coco: ${OBJS}
	${LD} ${LDFLAGS} ${OBJS} -o coco

constants.o: constants.f90
	${FC} ${FFLAGS} constants.f90

char_utilities.o: char_utilities.f90
	${FC} ${FFLAGS} char_utilities.f90

values.o: values.f90
	${FC} ${FFLAGS} values.f90

targets.o: targets.f90
	${FC} ${FFLAGS} targets.f90

switches.o: switches.f90
	${FC} ${FFLAGS} switches.f90

files.o: files.f90
	${FC} ${FFLAGS} files.f90

getopt.o: getopt.f90
	${FC} ${FFLAGS} getopt.f90

symbols.o: symbols.f90
	${FC} ${FFLAGS} symbols.f90

expressions.o: expressions.f90
	${FC} ${FFLAGS} expressions.f90

coco.o: coco.f90
	${FC} ${FFLAGS} coco.f90

clean:
	rm -f *~ *.o *.mod coco
