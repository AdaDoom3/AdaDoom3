/* file: posix-macros-sockets.c
   ----------------------------
   These subprograms provide access to POSIX functionality that is
   provided for C programs via macros.

 */

/*
#include <netinet/in.h>
#include <arpa/inet.h>
 */

#include "pconfig.h"

unsigned long c_ntohl (unsigned long val) {
   return ntohl (val);
}

unsigned long c_htonl (unsigned long val) {
   return htonl (val);
}

unsigned short c_ntohs (unsigned short val) {
   return ntohs (val);
}

unsigned long c_htons (unsigned long val) {
   return htons (val);
}
