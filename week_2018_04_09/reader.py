data_in = open('1523289952.txt', 'r')
for line in data_in:
    print [float(n) for n in line.split()]
