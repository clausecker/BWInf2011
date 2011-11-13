#ifndef OUTPUT_H
#define OUTPUT_H

#include "def.h"

#include <stdio.h>

int dump_cities(FILE *stream, const struct CITY *cities, int max_cities, int hide_empty);
int dump_partnerships(FILE *stream, const bool *partnerships, int max_cities);

#endif
