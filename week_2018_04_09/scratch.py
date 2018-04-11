import sympy

def euler_err():
    v, x = sympy.symbols('v x', real=True)
    h = sympy.symbols('h', real=True, positive=True)
    u = x**2/2
    # u = sympy.exp(-x**2)
    a = -u.diff(x)
    # maxA = sympy.solve(a.diff(x), x)[1]
    te1 = u+v**2/2
    x2 = x+v*h
    v2 = v+a*h
    te2 = u.subs(x, x2) + v2**2/2
    print('energy expressions')
    sympy.pprint(te1)
    sympy.pprint(te2)
    print('difference')
    discrep = (te2-te1)
    sympy.pprint(discrep)
    sympy.pprint(discrep.expand(h).collect(h))
    try:
        maxA
    except NameError:
        maxA=None
    if maxA != None:
        print('discrep at max acceleration')
        sympy.pprint(maxA)
        sympy.pprint(discrep.expand(h).subs(x, maxA))
    # sympy.pprint(discrep.diff(x))
    # sympy.pprint(discrep.diff(x).subs(x, maxA))
    # sympy.pprint(discrep.diff(x).subs(x, 0))
    # sympy.pprint(
    #     (
    #         discrep.diff(x).subs(x, maxA)
    #         -discrep.diff(x).subs(x, 0)
    #     ).simplify().collect(v).subs(h, .1)
    #     )

    # sympy.pprint(sympy.solve(discrep.diff(x),x))

    return
def verlet_err():
    v, x = sympy.symbols('v x', real=True)
    h = sympy.symbols('h', real=True, positive=True)
    u = x**2/2
    # u = sympy.exp(-x**2)
    a = -u.diff(x)
    # maxA = sympy.solve(a.diff(x), x)[1]
    te1 = u+v**2/2
    x2 = x+v*h+a*h**2/2
    v2 = v+(a+a.subs(x, x2))*h/2
    te2 = u.subs(x, x2) + v2**2/2
    print('energy expressions')
    sympy.pprint(te1)
    sympy.pprint(te2)
    print('difference')
    discrep = (te2-te1)
    sympy.pprint(discrep)
    sympy.pprint(discrep.expand(h).collect(h))
    try:
        maxA
    except NameError:
        maxA=None
    if maxA != None:
        print('discrep at max acceleration')
        sympy.pprint(maxA)
        sympy.pprint(discrep.expand(h).subs(x, maxA).collect(h))

    return

if __name__ == '__main__':
    euler_err()
    # verlet_err()
