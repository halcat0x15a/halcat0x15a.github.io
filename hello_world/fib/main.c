#include <stdio.h>

int main() {
  puts("0");
  puts("1");
  long buf[50] = {0, 1};
  for (int i = 2; i < 50; i++) {
    buf[i] = buf[i - 1] + buf[i - 2];
    printf("%ld\n", buf[i]);
  }
  return 0;
}
