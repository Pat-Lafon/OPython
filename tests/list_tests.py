l = []
l.append(5)
assert(l == [5])

l.append(20)
assert(l == [5, 20])

# Index checking
assert(l[0] == 5)
assert(l[1] == 20)

l[1] = 30
assert(l == [5, 30])
assert(l[1] == 30)