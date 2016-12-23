
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

-- Custom binding to the ZDelta API: https://web.archive.org/web/20160316211900/http://cis.poly.edu/zdelta/manual.shtml
package Neo.API.ZDelta is

  -----------
  -- Flags --
  -----------

  -- 

  ---------------
  -- Constants --
  ---------------

#define Z_NO_FLUSH      0
#define Z_PARTIAL_FLUSH 1 /* will be removed, use Z_SYNC_FLUSH instead */

#define ZD_SYNC_FLUSH    2
#define ZD_FULL_FLUSH    3
#define ZD_FINISH        4

#define ZD_HUFFMAN_ONLY        0
#define ZD_FILTERED            5
#define ZD_DEFAULT_STRATEGY    7

#define ZD_NO_COMPRESSION         0
#define ZD_BEST_SPEED             1
#define ZD_BEST_COMPRESSION       9
#define ZD_DEFAULT_COMPRESSION  (-1)

#define Z_BINARY   0
#define Z_ASCII    1
#define Z_UNKNOWN  2

#define ZD_DEFLATED  5 /* randomly choosen 8,15 are reserved */

#define ZD_NULL  0  /* for initializing zalloc, zfree, opaque */


#define ZD_OK            0
#define ZD_STREAM_END    1
#define ZD_ERRNO        (-1)
#define ZD_STREAM_ERROR (-2)
#define ZD_DATA_ERROR   (-3)
#define ZD_MEM_ERROR    (-4)
#define ZD_BUF_ERROR    (-5)
#define ZD_VERSION_ERROR (-6)

#define ZDLIB_VERSION "2.1"

  ----------------
  -- Structures --
  ----------------

typedef voidpf (*alloc_func) OF((voidpf opaque, uInt items, uInt size));
typedef void   (*free_func)  OF((voidpf opaque, voidpf address));

struct zd_internal_state;

typedef struct zd_stream_s {
  Bytef    *next_in;  /* next input byte */
  uInt     avail_in;  /* number of bytes available at next_in */
  uLong    total_in;  /* total nb of input bytes read so far */
  
  Bytef    *next_out; /* next output byte should be put there */
  uInt     avail_out; /* remaining free space at next_out */
  uLong    total_out; /* total nb of bytes output so far */
  
  Bytef    *base[REFNUM];      /* pointer to the base window    */
  uLong    base_out[REFNUM];   /* total read bytes from the base window */
  uLong    base_avail[REFNUM];
  int      refnum;

  char     *msg;      /* last error message, NULL if no error */
  struct zd_internal_state FAR *state; /* not visible by applications */

  alloc_func zalloc;  /* used to allocate the internal state */
  free_func  zfree;   /* used to free the internal state */
  voidpf     opaque;  /* private data object passed to zalloc and zfree */

  int     data_type;  /* best guess about the data type: ascii or binary */
  uLong   adler;      /* adler32 value of the uncompressed data */
  uLong   reserved;   /* reserved for future use */
 
} zd_stream;

typedef zd_stream FAR *zd_streamp;

  -----------------
  -- Subprograms --
  -----------------

ZEXTERN const char * ZEXPORT zdlibVersion OF((void));

ZEXTERN int ZEXPORT zd_deflate OF((zd_streamp strm, int flush));


ZEXTERN int ZEXPORT zd_deflateEnd OF((zd_streamp strm));



ZEXTERN int ZEXPORT zd_inflate OF((zd_streamp strm, int flush));


ZEXTERN int ZEXPORT zd_inflateEnd OF((zd_streamp strm));

ZEXTERN int ZEXPORT zd_deflateReset OF((zd_streamp strm));


ZEXTERN int ZEXPORT zd_deflateParams OF((zd_streamp strm,
                     int level,
                     int strategy));

ZEXTERN int ZEXPORT zd_inflateSync OF((zd_streamp strm));


ZEXTERN int ZEXPORT zd_inflateReset OF((zd_streamp strm));

ZEXTERN int ZEXPORT zd_compress OF ((const Bytef *ref, uLong rsize,
                     const Bytef *tar, uLong tsize,
                     Bytef *delta, uLongf* dsize));

ZEXTERN int ZEXPORT zd_compress1 OF ((const Bytef *ref, uLong rsize,
                      const Bytef *tar, uLong tsize,
                      Bytef **delta, uLongf *dsize));

ZEXTERN int ZEXPORT zd_compressN OF ((const Bytef *ref[],uLong rsize[],int rw,
                      const Bytef *tar, uLong tsize,
                      Bytef *delta, uLongf *dsize));

ZEXTERN int ZEXPORT zd_uncompress OF ((const Bytef *ref, uLong rsize,
                       Bytef *tar, uLongf *tsize,
                       const Bytef *delta, uLong dsize));

ZEXTERN int ZEXPORT zd_uncompress1 OF ((const Bytef *ref, uLong rsize,
                    Bytef **tar, uLongf *tsize,
                    const Bytef *delta, uLong dsize));


ZEXTERN int ZEXPORT 
zd_uncompressN OF((Bytef *ref[],uLong rsize[],int rw,
           const Bytef *tar, uLong *tsize,
           const Bytef *delta, uLongf dsize));

ZEXTERN int ZEXPORT zd_compressN1 OF ((const Bytef *ref[],uLong rsize[],int rw,
                       const Bytef *tar, uLong tsize,
                       Bytef **delta, uLongf *dsize));

  ZEXTERN int ZEXPORT 
  zd_uncompressN1 OF ((const Bytef **ref, uLong* rsize,int rw,
               Bytef **tar, uLongf *tsize,
               const Bytef *delta, uLong dsize));

ZEXTERN uLong ZEXPORT zd_adler32 OF((uLong adler, const Bytef *buf, uInt len));
end;
