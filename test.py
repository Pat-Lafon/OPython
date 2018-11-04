# fibonacci function
def fib(x):
    if x == 0 or x == 1:
        return 1
    return fib(x-2) + fib(x-1)

# Reference one function from another
def fib5(x):
    return fib(5)

x = fib5(0)

# 10th fib number is 89
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
assert(not None)
assert(None == None)
assert(not [].append(5))


# Assignment tests
a = 5
assert(a == 5)
a = []
assert(a == [])
a = ""
assert(a == "")
a = a + "b"
assert(a == "b")
a = a + "b"
assert(a == "bb")

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
    # TODO: Commenting at the end of a line 
    l.append(x)
    x = x + 1
# assert(l == range(10))
# TODO: this is actually incorrect, list(range(x)) gives a list, range gives an iterator
# We should have a list() function
assert(l == list(range(10)))

x = 0
l = []
while x < 10:
    if x % 2 == 0:
        l.append(x)
    x = x + 1
assert(l == [0,2,4,6,8])

# Implement prefix sum
def prefix_arr(arr):
    sums = [arr[0]]
    i = 1
    while i < len(arr):
        sums.append(sums[-1] + arr[i])
        i = i + 1
    return sums

prefix_sums = prefix_arr([1,2,3,4,5,6,7])
assert(prefix_sums == [1, 3, 6, 10, 15, 21, 28])

# Return the value right of val
def bin_search_right(arr, val):
    lo, hi = 0, len(arr) - 1
    while lo < hi:
        mid = (lo + hi)//2
        if arr[mid] <= val:
            lo = mid + 1
        else:
            hi = mid
    return lo

# Return the value left of val
def bin_search_left(arr, val):
    lo, hi = 0, len(arr) - 1
    while lo < hi:
        mid = (lo + hi + 1)//2
        if arr[mid] < val:
            lo = mid 
        else:
            hi = mid - 1
    return lo

arr = [1, 3, 5, 7, 7, 7, 9, 11, 11, 11, 13, 16, 22, 24, 25, 25, 26, 27, 27, 29]
assert(bin_search_left(arr, 7) == 2)
assert(bin_search_right(arr, 7) == 6)
assert(bin_search_left(arr, 11) == 6)
assert(bin_search_right(arr, 11) == 10)