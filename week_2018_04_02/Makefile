COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
assets_mod.o\
algorithms_mod.o\
test_algorithms.o\
main_project.o

RESULTS = \
test_algorithms.ken\
main_project.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o main_project.ken $(COMP_FLAGS) algorithms_mod.o main_project.o
	$(COMPILER) -o test_algorithms.ken $(COMP_FLAGS) algorithms_mod.o test_algorithms.o assets_mod.o
$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)

clean :
	 rm -f $(INTERMEDIATES) $(RESULTS)

test :
	make clean; make; ./test_algorithms.ken
run :
	make clean; make; ./main_project.ken
