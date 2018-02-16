COMP_FLAGS = -Wall -Wextra

test_dumby_mod.fin dumby_mod_user.fin: dumby_mod.o test_dumby_mod.o dumby_mod_user.o
	gfortran -o test_dumby_mod.fin $(COMP_FLAGS) dumby_mod.o test_dumby_mod.o
	gfortran -o dumby_mod_user.fin $(COMP_FLAGS) dumby_mod.o dumby_mod_user.o

test_dumby_mod.o: test_dumby_mod.f90
	gfortran -O -c test_dumby_mod.f90

dumby_mod_user.o: dumby_mod_user.f90
	gfortran -O -c dumby_mod_user.f90

dumby_mod.o: dumby_mod.f90
	gfortran -O -c dumby_mod.f90

clean :
	rm -f dumby_mod.o dumby_mod_user.o test_dumby_mod.o test_dumby_mod.fin dumby_mod_user.fin
