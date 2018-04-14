from matplotlib import pyplot
# data_in = open('1523726444.txt', 'r')
# data_in = open('1523726444.txt', 'r')
# data_in = open('1523726785.txt', 'r')
def one_D_Plotter():
    data_in = open('1523726898.txt', 'r')
    t_vals = []
    x_vals = []
    for line in data_in:
        values = [float(n) for n in line.split()]
        t_vals.append(values[0])
        x_vals.append(values[1])

    pyplot.plot(t_vals, x_vals)
    pyplot.show()
    return

if __name__ =='__main__':
    one_D_Plotter()
