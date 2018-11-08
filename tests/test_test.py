def isPrime(n):
    if n < 2:
        return (False)
    for i in range(1, (n//2)+1):
        if n % i == 0 and (i >= 2):
            return (False)
    return (True)

print(isPrime(13))
print(isPrime(12))
print(isPrime(25))
print(isPrime(30))
print(isPrime(39))
print(isPrime(57))