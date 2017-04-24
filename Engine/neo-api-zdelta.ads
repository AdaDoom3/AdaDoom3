
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
  -- Types --
  -----------

typedef char charf;
typedef int intf;
typedef void *voidpf;
  -- uInt    Int_32_Unsigned_C 
  -- uLong   Int_Ptr
  -- uLongf  Int_Ptr
typedef void *voidp;
typedef unsigned char Byte;
typedef off_t z_off_t;
typedef void *const voidpc;

  ---------------
  -- Constants --
  ---------------

  -- 
  Z_NO_FLUSH      : constant Int_C := 0; -- 0
  Z_PARTIAL_FLUSH : constant Int_C := 1; -- 0

  -- 
  ZD_SYNC_FLUSH : constant Int_C := 2; -- 0
  ZD_FULL_FLUSH : constant Int_C := 3; -- 0
  ZD_FINISH     : constant Int_C := 4; -- 0

  -- 
  ZD_HUFFMAN_ONLY     : constant Int_C := 0; -- 0
  ZD_FILTERED         : constant Int_C := 5; -- 0
  ZD_DEFAULT_STRATEGY : constant Int_C := 7; -- 0

  -- 
  ZD_NO_COMPRESSION      : constant Int_C :=  0; -- 0
  ZD_BEST_SPEED          : constant Int_C :=  1; -- 0
  ZD_BEST_COMPRESSION    : constant Int_C :=  9; -- 0
  ZD_DEFAULT_COMPRESSION : constant Int_C := -1; -- 0

  -- 
  Z_BINARY  : constant Int_C := 0; -- 0
  Z_ASCII   : constant Int_C := 1; -- 0
  Z_UNKNOWN : constant Int_C := 2; -- 0

  -- 
  ZD_DEFLATED : constant Int_C := 5; -- 0

  -- 
  ZD_NULL : constant Int_C := 0; -- 0

  -- 
  ZD_OK            : constant Int_C :=  0; -- 0
  ZD_STREAM_END    : constant Int_C :=  1; -- 0
  ZD_ERRNO         : constant Int_C := -1; -- 0
  ZD_STREAM_ERROR  : constant Int_C := -2; -- 0
  ZD_DATA_ERROR    : constant Int_C := -3; -- 0
  ZD_MEM_ERROR     : constant Int_C := -4; -- 0
  ZD_BUF_ERROR     : constant Int_C := -5; -- 0
  ZD_VERSION_ERROR : constant Int_C := -6; -- 0

  -- 
  ZDLIB_VERSION : constant String_C := "2.1";

  ---------------
  -- Accessors --
  ---------------

  -- 
  type alloc_func is access function (opaque : ; -- voidpf
                                      items  : ; -- uInt
                                      size   : ) -- uInt
                                      return  -- voidpf
                                      with Convention => C;

  -- 
  type free_func is access procedure (opaque  : ; -- voidpf
                                      address : ) -- voidpf
                                      with Convention => C;

  ----------------
  -- Structures --
  ----------------

  -- 
  type zd_stream is record 
      next_in    :  := ; -- Bytef*
      avail_in   :  := ; -- uInt
      total_in   :  := ; -- uLong 
      next_out   :  := ; -- Bytef* 
      avail_out  :  := ; -- uInt
      total_out  :  := ; -- uLong 
      base       :  := ; -- Bytef*[REFNUM]      
      base_out   :  := ; -- uLong[REFNUM] 
      base_avail :  := ; -- uLong[REFNUM]
      refnum     : Int_C := ; -- int
      msg        :  := ; -- char*      
      state      :  := ; -- struct zd_internal_state FAR* 
      zalloc     :  := ; -- alloc_func 
      zfree      :  := ; -- free_func  
      opaque     :  := ; -- voidpf
      data_type  : Int_C := ; -- int
      adler      :  := ; -- uLong    
      reserved   :  := ; -- uLong  
    end record with Convention => C;

  -----------------
  -- Subprograms --
  -----------------

  -- 
  function zdlibVersion return Ptr_Char_8_C -- const char* 
                        with Import => True, Convention => StdCall, External_Name => "zdlibVersion"; 

  -- 
  function zd_deflate (strm  : access zd_stream; -- zd_streamp
                       flush :        Int_C)     -- int
                       return Int_C              -- int
                       with Import => True, Convention => StdCall, External_Name => "zd_inflate"; 

  -- 
  function zd_deflateEnd (strm  : access zd_stream) -- zd_streamp
                          return Int_C              -- int
                          with Import => True, Convention => StdCall, External_Name => "zd_deflateEnd"; 


  -- 
  function zd_inflate (strm  : access zd_stream; -- zd_streamp
                       flush :        Int_C)     -- int
                       return Int_C              -- int
                       with Import => True, Convention => StdCall, External_Name => "zd_inflate"; 


  -- 
  function zd_inflateEnd (strm  : access zd_stream) -- zd_streamp
                          return Int_C              -- int
                          with Import => True, Convention => StdCall, External_Name => "zd_inflateEnd"; 

  -- 
  function zd_deflateReset (strm : access zd_stream) -- zd_streamp
                            return Int_C             -- int
                            with Import => True, Convention => StdCall, External_Name => "zd_deflateReset"; 

  -- 
  function zd_deflateParams (strm     : access zd_stream; -- zd_streamp
                             level    : Int_C;            -- int
                             strategy : Int_C)            -- int
                             return Int_C                 -- int
                             with Import => True, Convention => StdCall, External_Name => "zd_deflateParams"; 

  -- 
  function zd_inflateSync (strm : access zd_stream) -- zd_streamp
                           return Int_C;            -- int
                           with Import => True, Convention => StdCall, External_Name => "zd_inflateSync"; 

  -- 
  function zd_inflateReset (strm : access zd_stream) -- zd_streamp
                            return Int_C             -- int
                            with Import => True, Convention => StdCall, External_Name => "zd_inflateReset"; 

  -- 
  function zd_compress (ref   : ; -- const Bytef*
                        rsize : ; -- uLong 
                        tar   : ; -- const Bytef*
                        tsize : ; -- uLong 
                        delta : ; -- Bytef*
                        dsize : ) -- uLongf* 
                        return Int_C -- int
                        with Import => True, Convention => StdCall, External_Name => "zd_compress"; 

  -- 
  function zd_compress1 (ref   : ; -- const Bytef*
                         rsize : ; -- uLong 
                         tar   : ; -- const Bytef*
                         tsize : ; -- uLong 
                         delta : ; -- Bytef**
                         dsize : ) -- uLongf*
                         return Int_C -- int
                         with Import => True, Convention => StdCall, External_Name => "zd_compress1"; 

  -- 
  function zd_compressN (ref   : ; -- const Bytef*[]
                         rsize : ; -- uLong []
                         rw    : Int_C; -- int 
                         tar   : ; -- const Bytef*
                         tsize : ; -- uLong
                         delta : ; -- Bytef*
                         dsize : ) -- uLongf*
                         return Int_C -- int
                         with Import => True, Convention => StdCall, External_Name => "zd_compressN"; 

  -- 
  function zd_uncompress (ref   : ; -- const Bytef*
                          rsize : ; -- uLong 
                          tar   : ; -- const Bytef*
                          tsize : ; -- uLong 
                          delta : ; -- Bytef*
                          dsize : ) -- uLongf* 
                          return Int_C -- int
                          with Import => True, Convention => StdCall, External_Name => "zd_uncompress"; 

  -- 
  function zd_uncompress1 (ref   : ; -- Bytef*[]
                           rsize : ; -- uLong []
                           rw    : Int_C; -- int 
                           tar   : ; -- const Bytef*
                           tsize : ; -- uLong*
                           delta : ; -- const Bytef*
                           dsize : ) -- uLongf 
                           return Int_C -- int
                           with Import => True, Convention => StdCall, External_Name => "zd_uncompress1"; 

  -- 
  function zd_uncompressN (ref   : ; -- Bytef*[]
                           rsize : ; -- uLong []
                           rw    : Int_C; -- int 
                           tar   : ; -- const Bytef*
                           tsize : ; -- uLong*
                           delta : ; -- const Bytef*
                           dsize : ) -- uLongf
                           return Int_C -- int
                           with Import => True, Convention => StdCall, External_Name => "zd_uncompressN"; 

  -- 
  function zd_uncompressN1 (ref   : ; -- const Bytef**
                            rsize : ; -- uLong* 
                            rw    : Int_C; -- int 
                            tar   : ; -- Bytef**
                            tsize : ; -- uLongf*
                            delta : ; -- const Bytef*
                            dsize : ) -- uLong 
                            return Int_C -- int
                            with Import => True, Convention => StdCall, External_Name => "zd_uncompressN1"; 

  -- 
  function zd_adler32 (adler : Int_Ptr_C; -- uLong
                       buf   : ; -- const Bytef*
                       len   : ) -- uInt
                       return  -- uLong
                       with Import => True, Convention => StdCall, External_Name => "zd_adler32"; 
end;
