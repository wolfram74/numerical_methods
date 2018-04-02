COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \
mod_a_mod.o\
mod_b_mod.o\
test_mod_a.o\
test_mod_b.o\
homework_n.o

RESULTS = \
test_mod_a.ken\
test_mod_b.ken\
homework_n.ken

$(RESULTS): $(INTERMEDIATES)
	$(COMPILER) -o test_mod_a.ken $(COMP_FLAGS) mod_a_mod.o test_mod_a.o
	$(COMPILER) -o test_mod_b.ken $(COMP_FLAGS) mod_b_mod.o test_mod_b.o
	$(COMPILER) -o homework_n.ken $(COMP_FLAGS) mod_a_mod.o mod_b_mod.o homework_n.o
$(INTERMEDIATES):
	$(COMPILER) -O -c $(@:.o=.f90)

clean :
	 rm -f $(INTERMEDIATES) $(RESULTS)

test :
	make clean; make; ./test_mod_a.ken; ./test_mod_b.ken
run :
	make clean; make; ./homework_n.ken