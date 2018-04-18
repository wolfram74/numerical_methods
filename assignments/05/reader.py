from matplotlib import pyplot
# data_in = open('1523726444.txt', 'r')
# data_in = open('1523726444.txt', 'r')
# data_in = open('1523726785.txt', 'r')
def one_D_Plotter():
    # data_in = open('1523726785.txt', 'r') #resonant driving fixed-step
    data_in = open('1523726898.txt', 'r') #off resonant driving fixed-step
    t_vals_fixed = []
    x_vals_fixed = []
    last_zero_t_fixed = 0.
    zero_count=0
    for line in data_in:
        values = [float(n) for n in line.split()]
        if values[0]==0.0 and len(t_vals_fixed)!=0:
            print(len(t_vals_fixed))
            break
        t_vals_fixed.append(values[0])
        x_vals_fixed.append(values[1])
        if len(x_vals_fixed)==1:
            continue
        if 0>(x_vals_fixed[-1]*x_vals_fixed[-2]):
            last_zero_t_fixed = t_vals_fixed[-1]
            zero_count+=1

    pyplot.plot(t_vals_fixed, x_vals_fixed)
    # data_in = open('1524065121.txt', 'r') #sho adaptative
    # data_in = open('1524065717.txt', 'r') #off resonant adaptative
    data_in = open('1524065810.txt', 'r') #off resonant adaptative
    # data_in = open('1524065915.txt', 'r') #off resonant adaptative
    t_vals_adaptive = []
    x_vals_adaptive = []
    last_zero_t_adaptive = 0.
    for line in data_in:
        values = [float(n) for n in line.split()]
        if values[0]==0.0 and len(t_vals_adaptive)!=0:
            print(len(t_vals_adaptive))
            break
        t_vals_adaptive.append(values[0])
        x_vals_adaptive.append(values[1])
        if len(x_vals_adaptive)==1:
            continue
        if 0>(x_vals_adaptive[-1]*x_vals_adaptive[-2]):
            last_zero_t_adaptive = t_vals_adaptive[-1]
    pyplot.plot(t_vals_adaptive, x_vals_adaptive)
    print('%d zeros found' % zero_count)
    print(last_zero_t_fixed, last_zero_t_adaptive)
    print(last_zero_t_fixed-last_zero_t_adaptive)
    print(2.*(
        last_zero_t_fixed-last_zero_t_adaptive
        )/(
        last_zero_t_fixed+last_zero_t_adaptive
        ))
    print(len(x_vals_fixed), len(x_vals_adaptive))
    pyplot.show()
    return

if __name__ =='__main__':
    one_D_Plotter()
