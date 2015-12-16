#include <stdio.h>

int main() {
  long buf[50];
  for (int i = 0; i < 50; i++) {
    buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
    printf("%ld\n", buf[i]);
  }
  return 0;
}
