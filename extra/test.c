#include <stdlib.h>
#include <stdio.h>

const char * strarr[] = {"one", "two"};

int main(int argc, char **argv) {
  /*
  if(argc != 3) {
    printf("Wrong arg count %d\n", argc);
    exit(1);
  }
  x = strtod(argv[1], NULL);
  y = atoi(argv[2]);
  z = 0;
  */

  signed char x = 0xc5;
  printf("x %d xff %d %d\n", (int) x, ((int)(x)) & 0xff, 0xc5);

  // printf("Result %d %d %f\n", i, j, 0. & (double) (1 << 10));
  exit(0);
}
