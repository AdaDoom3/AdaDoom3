/* $Source: /home/harp/1/proto/monoBANK/xbind/dump.c,v $ */
/* $Revision: 1.3 $ $Date: 95/12/05 08:15:46 $ $Author: mg $ */

/* --------------------------------------------------------------------------
 * THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS" WITHOUT 
 * WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
 * PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy 
 * and the use of this file. 
 *  
 * Copyright (c) Intermetrics, Inc. 1994 
 * Royalty-free, unlimited, worldwide, non-exclusive use, modification, 
 * reproduction and further distribution of this file is permitted. 
 * --------------------------------------------------------------------------
 */

/* for debugging stdarg.ad* */

dump(addr, nb_int)
    long *addr, nb_int;
{
    int i;

    printf("buffer at addr %d %08x, %d ints: ", addr, addr, nb_int);
    if(nb_int > 100) nb_int = 100;
    for(i=0; i<nb_int; i++)
	printf("%08x ", *addr++);
    printf("\n\n");
}
