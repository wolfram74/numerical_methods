COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
runge_mod.o\
test_runge.o\
main_project.o

RESULTS = \
test_runge.ken\
main_project.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_runge.ken $(COMP_FLAGS) runge_mod.o test_runge.o
	$(COMPILER) -o main_project.ken $(COMP_FLAGS) runge_mod.o main_project.o
$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)

clean :
	 rm -f $(INTERMEDIATES) $(RESULTS)

test :
	make clean; make; ./test_runge.ken
run :
	make clean; make; ./main_project.ken