#include <iostream>

int main() {
  std::cout << 0 << std::endl << 1 << std::endl;
  long buf[50] = {0, 1};
  for (int i = 2; i < 50; i++) {
    buf[i] = buf[i - 1] + buf[i - 2];
    std::cout << buf[i] << std::endl;
  }
  return 0;
}
