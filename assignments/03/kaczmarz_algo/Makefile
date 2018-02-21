COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
kaczmarz_mod.o\
test_kaczmarz.o\
use_kaczmarz.o

RESULTS = \
test_kaczmarz.ken\
use_maczmarz.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_kaczmarz.ken $(COMP_FLAGS) kaczmarz_mod.o test_kaczmarz.o
	$(COMPILER) -o use_kaczmarz.ken $(COMP_FLAGS) kaczmarz_mod.o use_kaczmarz.o

$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)


clean :
	rm -f $(INTERMEDIATES) $(RESULTS)
