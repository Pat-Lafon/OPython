# fibonacci function
def fib5(x):
    return fib(5)

def fib(x):
    if x == 0 or x == 1:
        return 1
    return fib(x-2) + fib(x-1)

x = fib5(0)
print(x)

assert(fib(10) == 89)

# Test while loop
x = 0
y = 0
count = 20
while count > 0:
    x = x + 1
    y = y + 1
    assert(x == y)
    count = count - 1
    assert(count + x == 20)
assert(x == 20)
assert(y == 20)

# syracuse series of Collatz conjecture
# Test early return, if/else inside a function
def syra(x, iter):
    if x == 1:
        return iter
    if x % 2 == 0:
        return syra(x/2, iter+1)
    else:
        return syra(3 * x + 1, iter+1)
assert(syra(27, 0) == 111)
assert(syra(9, 0) == 19)
assert(syra(97, 0) == 118)
assert(syra(871, 0) == 178)

# if statement
if 1:
    assert(1)
else:
    assert(0)

# else test
if 0:
    assert(0)
else:
    assert(1)

# elif statements, boolean casting
if 0:
    assert(0)
elif False:
    assert(0)
elif []:
    assert(0)
elif "":
    assert(0)
else:
    assert(1)

# boolean expression tests
assert("a")
assert(not "")
assert([1])
assert(not [])
assert([] or 1)
assert(5 - 5 == 0)
assert(5 - 4)
# TODO: boolean for None types
# TODO: a = None
# assert(not [].append(5))

# Assignment tests
a = 5
assert(a == 5)
a = []
assert(a == [])
# TODO: None of the street assertions work
# a = ""
# assert(a == "")
# a = a + "b"
# assert(a == "b")
# a = a + "b"
# assert(a == "bb")

# Nested if in while loop
x = 1
while x < 100:
    if x == 90:
        x = x + 30
    else:
        x = x + 1
assert(x == 120)

# Testing lists
assert([] == [])
l = []
# TODO: this is printing the array and it shouldn't
l.append(5)
assert(l == [5])

# Appending list with while loop
x = 0
l = []
while x < 10:
    # TODO: this is printing the array and it shouldn't
    # TODO: Commenting at the end of a line 
    l.append(x)
    x = x + 1
# assert(l == range(10))
# TODO: this is actually incorrect, list(range(x)) gives a list, range gives an iterator
# We should have a list() function
# assert(l == list(range(10)))

x = 0
l = []
while x < 10:
    # TODO: this is printing the array and it shouldn't
    if x % 2 == 0:
        l.append(x)
    x = x + 1
assert(l == [0,2,4,6,8])

# Implement prefix sum
def prefix_arr(arr):
    # TODO: This doesn't work because indexing returns a list instead of a value!
    # sums = [arr[0]]
    # sums = [] + arr[0]
    # i = 0
    # while i < len(arr):
    #     sums.append(sums[-1] + arr[i])
    #     i = i + 1
    # print(sums)
    return 1

prefix_arr([1,2,3,4])

# TODO indexing must work before these work
def bin_search_right(arr, val):
    lo, hi = 0, len(arr) - 1
    while lo < hi:
        mid = (lo + hi)//2
        if arr[mid] <= val:
            lo = mid + 1
        else:
            hi = mid
    return lo

def bin_search_left(arr, val):
    lo, hi = 0, len(arr) - 1
    while lo < hi:
        mid = (lo + hi + 1)//2
        if arr[mid] < val:
            lo = mid 
        else:
            hi = mid - 1
    return lo
