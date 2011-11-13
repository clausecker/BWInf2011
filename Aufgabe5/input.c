
#include "input.h"

#include "def.h"

#include <ctype.h>
#include <string.h>

int get_num_cities(FILE *fcities)
{
  int result = 0;
  int c;
  while((c = getc(fcities)) != EOF)
    if(c == '\n')
      result++;
  return result;
}

int read_city_name(FILE *stream, char city_name[MAX_CITY_NAME_LENGTH])
{
  int c;
  int pos = 0;

  if(getc(stream) != '"')
    return 1; // city name has to begin with a '"'

  while((c = getc(stream)) != EOF)
  {
    if(c == '"')
    {
      city_name[pos] = 0;
      return 0;
    }
    else if(c >= ' ')
    {
      if(pos >= MAX_CITY_NAME_LENGTH - 1)
      {
        city_name[MAX_CITY_NAME_LENGTH - 1] = 0;
        printf("string too short for city '%s'\n", city_name);
        return 2; // string too short
      }

      city_name[pos++] = c;
    }
    else
      return 3; // strange character in city name
  }

  return 4; // unexpected eof
}

int read_int(FILE *stream, int *integer)
{
  int c;
  c = getc(stream);
  if(c == EOF)
    return 2;
  if(!isdigit(c))
    return 1;
  *integer = c - '0';

  while(isdigit((c = getc(stream))))
    *integer = *integer * 10 + c - '0';

  // put the last non-numeric character back to the stream
  ungetc(c, stream);

  return 0;
}

int read_cities(FILE *fcities, struct CITY *cities, int max_cities)
{
  int num_cities = 0;

  while(1)
  {
    struct CITY city = { 0 };
    int failure;
    failure = read_int(fcities, &city.max_festivals);

    if(failure == 2)
      return 0; // EOF found
    if(failure)
      return 1;
    if(num_cities >= max_cities)
      return 2;

    if(getc(fcities) != ' ')
      return 3;

    failure = read_city_name(fcities, city.name);
    if(failure)
      return failure + 4;

    cities[num_cities++] = city;

    if(getc(fcities) != '\n')
      return 4;
  }
  return 0;
}

int read_partnerships(
  FILE *fpartnerships,
  bool *partnerships,
  int *num_partnerships,
  struct CITY *cities, int num_cities) {

  *num_partnerships = 0;

  while(1)
  {
    char city_names[2][MAX_CITY_NAME_LENGTH];
    int city_indexes[2] = { -1, -1 };
    int failure;

    int c = getc(fpartnerships);
    if(c == EOF)
      return 0;
    ungetc(c, fpartnerships);

    failure = read_city_name(fpartnerships, city_names[0]);

    if(getc(fpartnerships) != ',')
      return 2;

    if(getc(fpartnerships) != ' ')
      return 3;

    failure = read_city_name(fpartnerships, city_names[1]);

    if(getc(fpartnerships) != '\n')
      return 4;
    {
      int i;
      for(i = 0;
              i < num_cities &&
              (city_indexes[0] == -1 || city_indexes[1] == -1);
              i++) {
        if(city_indexes[0] == -1 &&
           !strcmp(city_names[0], cities[i].name))
          city_indexes[0] = i;
        if(city_indexes[1] == -1 &&
           !strcmp(city_names[1], cities[i].name))
          city_indexes[1] = i;
      }
    }

    if(city_indexes[0] == -1 || city_indexes[1] == -1)
    {
      printf("city1='%s' city2='%s' not found\n",
                   city_names[0],
                   city_names[1]);
      return 5; // city not found
    }


    partnerships[city_indexes[0] * num_cities + city_indexes[1]] = 1;
    partnerships[city_indexes[1] * num_cities + city_indexes[0]] = 1;
    cities[city_indexes[0]].num_partnerships++;
    cities[city_indexes[1]].num_partnerships++;
    (*num_partnerships)++;
  }
}

