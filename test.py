i = 5
for i in "abcdasdasd":
    if i == "a":
        print(i)

print(i)

x = [i for i in range(5)]
x = [[i for i in range(5)] for j in range(6) if j % 2 == 0]
x = [[i for i in range(5)] for j in [1,2,3] if j % 2 == 0]