#include <iostream>
#include <array>

int main() {
  std::array<long, 50> buf;
  for (int i = 0; i < buf.size(); i++) {
    buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
    std::cout << buf[i] << std::endl;
  }
  return 0;
}
