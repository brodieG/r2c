#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
  double x, z;
  int y;
  if(argc != 3) {
    printf("Wrong arg count %d\n", argc);
    exit(1);
  }
  x = strtod(argv[1], NULL);
  y = atoi(argv[2]);
  z = 0;

  for(int i = 0; i < y; ++i) {
    z += x;
  }
  printf("Result %f\n", z);
  exit(0);
}
