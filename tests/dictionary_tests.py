a = {}

assert(not a)

a[1] = 5
a[2] = 10

assert(a[1] == 5)
assert(a[2] == 10)
assert(print(a[4]) == 1)
assert(0)