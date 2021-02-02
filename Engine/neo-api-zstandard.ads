
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

-- Custom binding to the ZStandard API: https://github.com/facebook/zstd/blob/master/lib/zstd.h
package Neo.API.ZStandard is

  -----------
  -- Types --
  -----------

  -- size_t
  -- Int_Unsigned
  -- int
  -- void*
  -- const char*

  -----------------
  -- Structures --
  ----------------

  type ZSTD_inBuffer_s is record
      src  : chars_ptr;
      size : size_t;
      pos  : size_t;
    end record with Convention => C;

  type ZSTD_outBuffer_s is record
      dst  : chars_ptr;
      size : size_t;
      pos  : size_t;
    end record with Convention => C;

  -----------------
  -- Subprograms --
  -----------------

  function ZSTD_versionNumber return Int_Unsigned_C -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_versionNumber";

  function ZSTD_isError (code : size_t) -- size_t
                         return Int_Unsigned_C -- Int_Unsigned_C
                         with Import => True, Convention => C, External_Name => "ZSTD_isError";

  function ZSTD_getErrorName (code : size_t) -- size_t
                              return chars_ptr -- const char*
                              with Import => True, Convention => C, External_Name => "ZSTD_getErrorName";

  function ZSTD_compressBound (srcSize : size_t) -- size_t 
                               return size_t -- size_t
                               with Import => True, Convention => C, External_Name => "ZSTD_compressBound";

  function ZSTD_maxCLevel return int -- int
                          with Import => True, Convention => C, External_Name => "ZSTD_maxCLevel";

  function ZSTD_compress (dst              : Ptr;        -- void*
                          dstCapacity      : Int_Size_C; -- size_t
                          src              : Ptr;        -- void*
                          srcSize          : Int_Size_C; -- size_tsize_t
                          compressionLevel : Int_C)      -- int 
                          return Int_Size_C              -- size_t
                          with Import => True, Convention => C, External_Name => "ZSTD_compress";

  function ZSTD_getDecompressedSize (src     : chars_ptr; -- 
                                     srcSize : size_t) -- 
                                     return Zstd_uint64v
                                     with Import => True, Convention => C, External_Name => "ZSTD_getDecompressedSize";

  function ZSTD_decompress (dst            : chars_ptr; -- 
                            dstCapacity    : size_t;v
                            src            : chars_ptr; -- 
                            compressedSize : size_t) -- 
                            return size_t -- 
                            with Import => True, Convention => C, External_Name => "ZSTD_decompress";

  function ZSTD_createCCtx return Ptr -- 
                           with Import => True, Convention => C, External_Name => "ZSTD_createCCtx";

  function ZSTD_freeCCtx (cctx : Ptr) -- 
                          return size_t -- 
                          with Import => True, Convention => C, External_Name => "ZSTD_freeCCtx";

  function ZSTD_compressCCtx (ctx              : Ptr; -- 
                              dst              : chars_ptr; -- 
                              dstCapacity      : Int_Size_C; -- size_t
                              src              : chars_ptr; -- 
                              srcSize          : Int_Size_C; -- size_t
                              compressionLevel : int) -- 
                              return size_t -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_compressCCtx";

  function ZSTD_createDCtx return Ptr -- 
                           with Import => True, Convention => C, External_Name => "ZSTD_createDCtx";

  function ZSTD_freeDCtx (dctx : Ptr) -- 
                          return size_t -- 
                          with Import => True, Convention => C, External_Name => "ZSTD_freeDCtx";

  function ZSTD_decompressDCtx (ctx         : Ptr; -- 
                                dst         : chars_ptr; -- 
                                dstCapacity : Int_Size_C; -- size_t
                                src         : chars_ptr; -- 
                                srcSize     : size_t) -- 
                                return size_t -- 
                                with Import => True, Convention => C, External_Name => "ZSTD_decompressDCtx";

  function ZSTD_createCStream return Ptr -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_createCStream";

  function ZSTD_freeCStream (zcs : Ptr) -- 
                             return size_t -- 
                             with Import => True, Convention => C, External_Name => "ZSTD_freeCStream";

  function ZSTD_initCStream (zcs : Ptr; -- 
                             compressionLevel : int) -- 
                             return size_t -- 
                             with Import => True, Convention => C, External_Name => "ZSTD_initCStream";

  function ZSTD_CStreamInSize return size_t -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_CStreamInSize";

  function ZSTD_CStreamOutSize return size_t
                               with Import => True, Convention => C, External_Name => "ZSTD_CStreamOutSize";

  function ZSTD_compressStream (zcs    : Ptr; -- 
                                output : access ZSTD_outBuffer_s; -- 
                                input  : access ZSTD_inBuffer_s) -- 
                                return size_t -- 
                                with Import => True, Convention => C, External_Name => "ZSTD_compressStream";

  function ZSTD_flushStream (zcs    : Ptr; -- 
                             output : access ZSTD_outBuffer_s) -- 
                             return size_t -- 
                             with Import => True, Convention => C, External_Name => "ZSTD_flushStream";

  function ZSTD_endStream (zcs    : Ptr; -- 
                           output : access ZSTD_outBuffer_s) -- 
                           return size_t -- 
                           with Import => True, Convention => C, External_Name => "ZSTD_endStream";

  function ZSTD_createDStream return Ptr -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_createDStream";

  function ZSTD_freeDStream (zds : Ptr) -- 
                             return size_t -- 
                             with Import => True, Convention => C, External_Name => "ZSTD_freeDStream";

  function ZSTD_initDStream (zds : Ptr) -- 
                             return size_t -- 
                             with Import => True, Convention => C, External_Name => "ZSTD_initDStream";

  function ZSTD_DStreamInSize return size_t -- 
                              with Import => True, Convention => C, External_Name => "ZSTD_DStreamInSize";

  function ZSTD_DStreamOutSize return size_t -- 
                               with Import => True, Convention => C, External_Name => "ZSTD_DStreamOutSize";

  function ZSTD_decompressStream (zds    : Ptr; -- 
                                  output : access ZSTD_outBuffer_s; -- 
                                  input  : access ZSTD_inBuffer_s) -- 
                                  return size_t -- 
                                  with Import => True, Convention => C, External_Name => "ZSTD_decompressStream";

   --  Compression using a predefined Dictionary (see dictBuilder/zdict.h).
   --  Note : This function load the dictionary, resulting in a significant startup time.
   function ZSTD_compress_usingDict
     (ctx  : ZSTD_CCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress_usingDict, "ZSTD_compress_usingDict");

   --  Decompression using a predefined Dictionary (see dictBuilder/zdict.h).
   --  Dictionary must be identical to the one used during compression.
   --  Note : This function load the dictionary, resulting in a significant startup time
   function ZSTD_decompress_usingDict
     (dctx : ZSTD_DCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDict, "ZSTD_decompress_usingDict");

   ---------------------------
   --  Fast Dictionary API  --
   ---------------------------

   --  Create a digested dictionary, ready to start compression operation without startup delay.
   --  `dict` can be released after creation
   function ZSTD_createCDict
     (dict : ICS.chars_ptr; dictSize : IC.size_t;
      compressionLevel : IC.int) return ZSTD_CDict_ptr;
   pragma Import (C, ZSTD_createCDict, "ZSTD_createCDict");

   function ZSTD_freeCDict (CDict : ZSTD_CDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeCDict, "ZSTD_freeCDict");

   --  Compression using a pre-digested Dictionary.
   --  Faster startup than ZSTD_compress_usingDict(), recommended when same dictionary is
   --  used multiple times.  Note that compression level is decided during dictionary creation.
   function ZSTD_compress_usingCDict
     (ctx   : ZSTD_CCtx_ptr;
      dst   : ICS.chars_ptr; dstCapacity : IC.size_t;
      src   : ICS.chars_ptr; srcSize     : IC.size_t;
      CDict : ZSTD_CDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_compress_usingCDict, "ZSTD_compress_usingCDict");

   --  Create a digested dictionary, ready to start decompression operation without startup delay.
   --  `dict` can be released after creation
   function ZSTD_createDDict (dict : ICS.chars_ptr; dictSize : IC.size_t) return ZSTD_DDict_ptr;
   pragma Import (C, ZSTD_createDDict, "ZSTD_createDDict");

   function ZSTD_freeDDict (ddict : ZSTD_DDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeDDict, "ZSTD_freeDDict");

   --  Decompression using a digested Dictionary
   --  Faster startup than ZSTD_decompress_usingDict(), recommended when same dictionary is
   --  used multiple times.
   function ZSTD_decompress_usingDDict
     (dctx  : ZSTD_DCtx_ptr;
      dst   : ICS.chars_ptr; dstCapacity : IC.size_t;
      src   : ICS.chars_ptr; srcSize     : IC.size_t;
      ddict : ZSTD_DDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDDict, "ZSTD_decompress_usingDDict");
end;
