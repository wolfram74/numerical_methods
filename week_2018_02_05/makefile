COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
dumby_mod.o\
test_dumby_mod.o\
dumby_mod_user.o

RESULTS = \
test_dumby_mod\
dumby_mod_user

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_dumby_mod $(COMP_FLAGS) dumby_mod.o test_dumby_mod.o
	$(COMPILER) -o dumby_mod_user $(COMP_FLAGS) dumby_mod.o dumby_mod_user.o

$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)


clean :
	rm -f $(INTERMEDIATES) $(RESULTS)
