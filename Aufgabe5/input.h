#ifndef INPUT_H
#define INPUT_H

#include "def.h"

#include <stdio.h>

int get_num_cities(FILE *fcities);
int read_cities(FILE *fcities, struct CITY *cities, int max_cities);
int read_partnerships(
  FILE *fpartnerships,
  bool *partnerships,
  int *num_partnerships,
  struct CITY *cities,
  int num_cities);

#endif
