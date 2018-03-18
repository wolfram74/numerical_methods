COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
romberg_mod.o\
test_romberg.o\
use_romberg.o

RESULTS = \
test_romberg.ken\
use_romberg.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_romberg.ken $(COMP_FLAGS) romberg_mod.o test_romberg.o
	$(COMPILER) -o use_romberg.ken $(COMP_FLAGS) romberg_mod.o use_romberg.o

$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)


clean :
	rm -f $(INTERMEDIATES) $(RESULTS)

test :
	make clean; make; ./test_romberg.ken
