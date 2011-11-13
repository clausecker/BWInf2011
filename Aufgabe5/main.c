
#include "input.h"
#include "process.h"

#include "def.h"

#include <stdio.h>
#include <stdlib.h>


int main(int argc, char **argv)
{
  FILE *fcities;
  FILE *fpartnerships;
  int num_cities;
  struct CITY *cities;
  bool *partnerships;
  int num_partnerships;
  int failure;

  if(argc != 3)
  {
    printf("Usage: %s <cities> <partnerships>\n", argv[0]);
    return -1;
  }

  fcities = fopen(argv[1], "r");
  fpartnerships = fopen(argv[2], "r");

  if(!fcities)
  {
    printf("Could not open \"%s\"\n", argv[1]);
    return -1;
  }

  if(!fpartnerships)
  {
    printf("Could not open \"%s\"\n", argv[2]);
    return -1;
  }

  num_cities = get_num_cities(fcities);

  fseek(fcities, 0, SEEK_SET);

  cities = calloc(num_cities, sizeof(struct CITY));
  partnerships = calloc(num_cities * num_cities, sizeof(bool));

  failure = read_cities(fcities, cities, num_cities);
  if(failure)
  {
    printf("failure while reading cities (%d)\n" , failure);
    return -1;
  }
  failure = read_partnerships(
    fpartnerships,
    partnerships,
    &num_partnerships,
    cities, num_cities);
  if(failure)
  {
    printf("failure while reading partnerships (%d)\n" , failure);
    return -1;
  }

  failure = process(cities, partnerships, num_cities, num_partnerships);

  if(!failure) // don't dump cities if processing failed
  {
    int i;
    int k;
    for(i = 0; i < num_cities; i++)
      for(k = 0; k < num_cities; k++)
        if(partnerships[i + num_cities * k])
          printf("'%s' - '%s'\n", cities[i].name, cities[k].name);
  }
  else
  {
    printf("impossible!\n");
  }

#ifdef _MSC_VER
  getchar();
#endif

  return (!failure) ? 0 : 1;
}

