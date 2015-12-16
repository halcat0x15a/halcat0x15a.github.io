#include <iostream>
#include <vector>

int main() {
  std::vector<long> buf(50);
  for (int i = 0; i < buf.size(); i++) {
    buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
    std::cout << buf[i] << std::endl;
  }
  return 0;
}
