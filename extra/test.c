#include <stdlib.h>
#include <stdio.h>

const char * strarr[] = {"one", "two"};
struct test {
  char a;
  char b;
};

#define TEST(a, b, c) x.a b (c)

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

  struct test x;
  x. a = 1;
  printf("x %d\n", TEST( b, >, 2));

  // printf("Result %d %d %f\n", i, j, 0. & (double) (1 << 10));
  exit(0);
}



