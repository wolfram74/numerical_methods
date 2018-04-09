'''
objective:
    read
        from a description.txt file a terminal file name and some number of module names
    generate
        empty source files for
            the modules
            test files for the modules
            the terminal source
        a makefile that will compile the contents properly
'''
import re

def description_parse():
    description_file = open('./description.txt', 'r')
    main = description_file.readline().strip()
    modules=[]
    for line in description_file:
        print(line)
        modules.append(line.strip())
    return main, modules

def file_name_gen(main, modules):
    intermediate_names = []
    results_names = []
    for name in modules:
        intermediate_names.append("%s_mod.o"%name)
    for name in modules:
        intermediate_names.append("test_%s.o"%name)
        results_names.append("test_%s.ken"%name)
    intermediate_names.append("%s.o"%main)
    results_names.append("%s.ken"%main)
    return intermediate_names, results_names

def results_relationships(main, results, intermediates):
    compilation_groups = {}
    for result in results:
        if not 'test' in result:
            compilation_groups[result] = filter(lambda x: not 'test' in x, intermediates)
            continue
        matches = re.search('(?<=test_).*(?=\.ken)', result)
        name = matches.group(0)
        compilation_groups[result] = filter(lambda x: name in x, intermediates)
    return compilation_groups

def gen_makefile(intermediates, results, dependencies):
    makefile = open('./Makefile', 'w')
    makefile.write('''COMP_FLAGS = -Wall -Wextra
COMPILER = gfortran

INTERMEDIATES = \\\n''')
    for line_num in range(len(intermediates)):
        if line_num==len(intermediates)-1:
            makefile.write(intermediates[line_num]+'\n')
            break
        makefile.write(intermediates[line_num]+'\\\n')
    makefile.write('\n')
    makefile.write('RESULTS = \\\n')
    for line_num in range(len(results)):
        if line_num==len(results)-1:
            makefile.write(results[line_num]+'\n')
            break
        makefile.write(results[line_num]+'\\\n')
    makefile.write('\n')
    makefile.write('$(RESULTS): $(INTERMEDIATES)\n')
    for target in dependencies.keys():
        compile_line = '\t$(COMPILER) -o %s $(COMP_FLAGS) %s\n' % (target, ' '.join(dependencies[target]))
        makefile.write(compile_line)
    makefile.write('$(INTERMEDIATES):\n')
    makefile.write('\t$(COMPILER) -O -c $(@:.o=.f90)\n')
    makefile.write('\n')
    makefile.write('clean :\n')
    makefile.write('\t rm -f $(INTERMEDIATES) $(RESULTS)\n')
    makefile.write('\n')
    makefile.write('test :\n')
    makefile.write('\tmake clean; make; ./' + '; ./'.join(filter(lambda x: 'test' in x, results)))
    makefile.write('\n')
    makefile.write('run :\n')
    makefile.write('\tmake clean; make; ./' + (filter(lambda x: not 'test' in x, results))[0])
    makefile.close()

def gen_empty_sources(intermediates, results, dependencies):
    print(intermediates)
    print(results)
    module_template = """
module %s
  implicit none
  contains
end module %s
        """
    usage_template ="""
program %s
  use %s
  implicit none
  contains
end program %s
        """

    for name in intermediates:
        if 'mod.o' in name:
            print('module found')
            lable = name[:-6]
            module = open('./%s_mod.f90'%lable, 'w')
            module.write(module_template % (lable, lable))
            module.close()
            continue
        lable = name[:-2]
        ken_file = lable+'.ken'
        imports = filter(lambda x: 'mod.o' in x, dependencies[ken_file])
        import_lables = map(lambda x: x[:-6], imports)
        imports_string = '\n    use '.join(import_lables)
        source = open('./%s.f90'%lable, 'w')
        source.write(usage_template%(lable, imports_string, lable))
        # print(imports)
if __name__ == '__main__':
    print('extracting description')
    main, modules  = description_parse()
    # print( main, modules)
    intermediates, results = file_name_gen(main, modules)
    # print(intermediates)
    # print(results)
    dependencies = results_relationships(main, results, intermediates)
    # print(dependencies)
    gen_makefile(intermediates, results, dependencies)
    gen_empty_sources(intermediates, results, dependencies)
