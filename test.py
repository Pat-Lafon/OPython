def f(x):
    if x == 0 or x == 1:
        return 1
    return f(x-2) + f(x-1)


x = 0
while x < 10:
    f(x)
    x = x + 1
