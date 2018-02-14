dumby_mod_user.fin: dumby_mod.o dumby_mod_user.o
	gfortran -o dumby_mod_user.fin -Wall -Wextra dumby_mod.o dumby_mod_user.o

dumby_mod_user.o: dumby_mod_user.f90
	gfortran -O -c dumby_mod_user.f90

dumby_mod.o: dumby_mod.f90
	gfortran -O -c dumby_mod.f90
