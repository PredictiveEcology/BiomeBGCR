#ifndef POINTBGC_H
#define POINTBGC_H

/*
pointbgc.h
header file to hold includes needed for pointbgc.

*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
Biome-BGC version 4.2 (final release)
See copyright.txt for Copyright information
Andrew A Neuschwander, andrew@ntsg.umt.edu

*/

#include <time.h>

#ifndef WIN32
#include <getopt.h>
#endif

#ifdef WIN32
#include "getopt.h"
#define HOST "Windows"
#endif

#ifndef VERS
#define VERS "4.2"
#endif

#ifndef USER
#define USER "unknown"
#endif

#ifndef HOST
#define HOST "unknown"
#endif

#include "bgc.h"
#include "pointbgc_struct.h"
#include "pointbgc_func.h"

#ifdef __cplusplus
extern "C"
{
#endif

/* non include stuff here */

int execute(char *argv[], int argc);

#ifdef __cplusplus
}
#endif


#endif
