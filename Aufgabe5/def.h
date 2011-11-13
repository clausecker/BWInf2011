#ifndef DEF_H
#define DEF_H

#ifdef _MSC_VER
typedef char bool; // Microsoft Visual C unterstützt kein C99
#define true 1
#define false 0
#define _CRT_SECURE_NO_WARNINGS
#else
#include <stdbool.h>
#endif


#define MAX_CITY_NAME_LENGTH 64

struct CITY
{
	int max_festivals;
	char name[MAX_CITY_NAME_LENGTH];
	int num_partnerships;
};

struct PARTNERSHIP
{
	short int host;
	short int guest;
	bool certain;
};
#endif
