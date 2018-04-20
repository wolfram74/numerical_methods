from matplotlib import pyplot
def one_D_Plotter():
    # data_in = open('resonant_fixed_step.txt', 'r')
    data_in = open('off_resonant_fixed_step.txt', 'r')
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
    # data_in = open('off_resonant_adaptative_657.txt', 'r')
    # data_in = open('off_resonant_adaptative_714.txt', 'r')
    # data_in = open('off_resonant_adaptative_1116.txt', 'r')
    # data_in = open('resonant_adaptive_1731.txt', 'r')
    # data_in = open('1524069410.txt', 'r')
    data_in = open('1524069462.txt', 'r')
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

def orbit_plotter():
    # data_in = open('non_adaptive_keplerian.txt', 'r')
    data_in = open('non_adaptive_keplerian_120steps.txt', 'r')
    x_vals = []
    y_vals = []
    for line in data_in:
        values = [float(n) for n in line.split()]
        x_vals.append(values[1])
        y_vals.append(values[2])
    pyplot.plot(x_vals,y_vals)
    pyplot.show()

if __name__ =='__main__':
    # one_D_Plotter()
    orbit_plotter()
