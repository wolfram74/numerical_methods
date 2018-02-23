COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
neville_mod.o\
test_neville.o\
use_neville.o

RESULTS = \
test_neville.ken\
use_neville.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_neville.ken $(COMP_FLAGS) neville_mod.o test_neville.o
	$(COMPILER) -o use_neville.ken $(COMP_FLAGS) neville_mod.o use_neville.o

$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)


clean :
	rm -f $(INTERMEDIATES) $(RESULTS)
