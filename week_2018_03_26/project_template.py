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

def description_parse():
    description_file = open('./description.txt', 'r')
    main = description_file.readline().strip()
    modules=[]
    for line in description_file:
        print(line)
        modules.append(line.strip())
    return main, modules

def intermediates_gen(main, modules):
    intermediate_names = []
    for name in modules:
        intermediate_names.append("%s_mod.o"%name)
    for name in modules:
        intermediate_names.append("test_%s.o"%name)
    intermediate_names.append("%s.o"%main)
    return intermediate_names

if __name__ == '__main__':
    print('extracting description')
    main, modules  = description_parse()
    print( main, modules)
    intermediates = intermediates_gen(main, modules)
    print(intermediates)
