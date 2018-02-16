COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

test_dumby_mod.fin dumby_mod_user.fin: dumby_mod.o test_dumby_mod.o dumby_mod_user.o
	$(COMPILER) -o test_dumby_mod.fin $(COMP_FLAGS) dumby_mod.o test_dumby_mod.o
	$(COMPILER) -o dumby_mod_user.fin $(COMP_FLAGS) dumby_mod.o dumby_mod_user.o

test_dumby_mod.o: test_dumby_mod.f90
	$(COMPILER) -O -c test_dumby_mod.f90

dumby_mod_user.o: dumby_mod_user.f90
	$(COMPILER) -O -c dumby_mod_user.f90

dumby_mod.o: dumby_mod.f90
	$(COMPILER) -O -c dumby_mod.f90

clean :
	rm -f dumby_mod.o dumby_mod_user.o test_dumby_mod.o test_dumby_mod.fin dumby_mod_user.fin
