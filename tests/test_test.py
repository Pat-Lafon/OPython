def d(arr):
    arr[1] = 5

def f(arr):
    l = [1,2,3]
    d(l)
    print(l)

a = [1,2,3,4]
f(a)
print(a)
