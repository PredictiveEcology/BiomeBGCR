/*
pointbgc.c
front-end to BIOME-BGC for single-point, single-biome simulations
Uses BIOME-BGC function library

*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
Biome-BGC version 4.2 (final release)
See copyright.txt for Copyright information
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
*/

#include "pointbgc.h"
#include "pointbgc_execute.h"

int main(int argc, char *argv[])
{
	int result = execute(argv, argc, 0);
	exit(result);
} /* end of main */

