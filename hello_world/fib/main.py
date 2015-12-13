buf = []
for i in range(0, 50):
    buf.append(i if i < 2 else buf[i - 1] + buf[i - 2])
    print(buf[i])
