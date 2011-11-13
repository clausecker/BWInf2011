
#include "output.h"

#include "def.h"

int dump_cities(FILE *stream, const struct CITY *cities, int max_cities, int hide_empty)
{
	int i;
	int count = 0;
	for(i = 0; i < max_cities; i++)
	{
		if(!hide_empty || (cities[i].max_festivals != 0 || cities[i].num_partnerships != 0))
		{
			fprintf(stream, "%d %d '%s'\n", cities[i].max_festivals, cities[i].num_partnerships, cities[i].name);
			count++;
		}
	}
	fprintf(stream, "count: %d\n", count);
	return 0;
}

int dump_partnerships(FILE *stream, const bool *partnerships, int max_cities)
{
	int i;
	int k;
	for(i = 0; i < max_cities; i++)
	{
		for(k = 0; k < max_cities; k++)
			fprintf(stream, "%d ", partnerships[i * max_cities + k]);
		fprintf(stream, "\n");
	}
	return 0;
}

