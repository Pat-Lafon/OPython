def f(x):
    if x == 0 or x == 1:
        return 1
    return f(x-2) + f(x-1)

<<<<<<< HEAD

x = 0
while x < 20:
    f(x)
    x = x + 1
=======
x = 0
while x < 10:
    f(x)
    x = x + 1
>>>>>>> 7c785f7596ccb9f705b202fdd533836ad7b94f04
