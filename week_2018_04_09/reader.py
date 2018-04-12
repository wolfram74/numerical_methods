import time
start = time.time()
# import random
# import matplotlib
# import numpy
# import math
# import sympy
# import scipy
import collections
# import kivy
data_in = open('1523289952.txt', 'r')
for line in data_in:
    print [float(n) for n in line.split()]
end = time.time()
print end-start
