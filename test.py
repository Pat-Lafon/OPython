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