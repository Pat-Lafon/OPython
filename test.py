def f(x):
    if x == 0 or x == 1:
        return 1
    return f(x-2) + f(x-1)

y = []
x=0
while x < 10:
    if x == 5:
        y.append(x)
    x += 1
y