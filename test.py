def f(x):
    if x == 0 or x == 1:
        return 1
    return f(x-2) + f(x-1)

while x < 20:
    print(f(x))
    x = x + 1