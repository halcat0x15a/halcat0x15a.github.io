buf = []
for i in 0...50
  buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2]
  puts buf[i]
end
