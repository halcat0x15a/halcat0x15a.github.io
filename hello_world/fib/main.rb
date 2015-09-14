puts 0, 1
buf = [0, 1]
for i in 2...50
  buf[i] = buf[i - 1] + buf[i - 2]
  puts buf[i]
end
