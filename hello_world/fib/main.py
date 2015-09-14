print(0)
print(1)
buf = [0, 1]
for i in range(2, 50):
    buf.append(buf[i - 1] + buf[i - 2])
    print(buf[i])
