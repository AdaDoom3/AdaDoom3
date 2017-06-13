
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

package body Neo.Core.Compress is

  type Compressor is tagged private;
  type Decompressor is tagged private;
   type Decompressor is tagged
      record
         source_stream    : access STR.Root_Stream_Type'Class;
         zstd_stream      : Thin.ZSTD_DStream_ptr := Thin.Null_DStream_pointer;
      end record;
   type Compressor is tagged
      record
         target_stream  : access STR.Root_Stream_Type'Class;
         zstd_stream    : Thin.ZSTD_CStream_ptr := Thin.Null_CStream_pointer;
         data_size_hint : Thin.IC.size_t := Buffer_Output_Size;
      end record;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null access STR.Root_Stream_Type'Class)
   is
      use type Thin.ZSTD_DStream_ptr;
      initResult : Thin.IC.size_t;
   begin
      mechanism.source_stream := input_stream;
      mechanism.zstd_stream   := Thin.ZSTD_createDStream;

      if mechanism.zstd_stream = Thin.Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := Thin.ZSTD_initDStream (zds => mechanism.zstd_stream);
      if Natural (Thin.ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;
   end Initialize;


   -----------------------
   --  Decompress_Data  --
   -----------------------
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      complete     : out Boolean;
      output_data  : out Output_Data_Container;
      last_element : out Natural)
   is
      use type Thin.ZSTD_DStream_ptr;

      sin_last  : STR.Stream_Element_Offset := STR.Stream_Element_Offset (Recommended_Chunk_Size);
      Last      : STR.Stream_Element_Offset;

      data_in   : aliased Thin.IC.char_array := (1 .. Recommended_Chunk_Size => Thin.IC.nul);
      data_out  : aliased Thin.IC.char_array := (1 .. Output_container_size  => Thin.IC.nul);
      data_sin  : STR.Stream_Element_Array (1 .. sin_last);
      index     : Thin.IC.size_t := data_in'First;
      size_hint : Thin.IC.size_t;
      inbuffer  : aliased Thin.ZSTD_inBuffer_s :=
                          (src  => Thin.ICS.To_Chars_Ptr (data_in'Unchecked_Access),
                           size => Recommended_Chunk_Size,
                           pos  => 0);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Output_container_size,
                           pos  => 0);
   begin
      if mechanism.zstd_stream = Thin.Null_DStream_pointer then
         raise streaming_decompression_error with "Run initialize procedure first";
      end if;

      mechanism.source_stream.Read (Item => data_sin, Last => Last);

      if Natural (Last) = 0 then
         last_element := 0;
         complete := True;
         return;
      end if;

      data_in := convert_to_char_array (data_sin (1 .. Last), Recommended_Chunk_Size);
      complete := (Natural (Last) /= Natural (Recommended_Chunk_Size));

      size_hint := Thin.ZSTD_decompressStream (zds    => mechanism.zstd_stream,
                                               output => outbuffer'Unchecked_Access,
                                               input  => inbuffer'Unchecked_Access);

      last_element := Natural (outbuffer.pos);
      output_data (1 .. STR.Stream_Element_Offset (last_element)) :=
        convert_to_stream_array (data_out, outbuffer.pos);

   end Decompress_Data;


   -------------------------------
   --  convert_to_stream_array  --
   -------------------------------
   function convert_to_stream_array
     (char_data         : Thin.IC.char_array;
      number_characters : Thin.IC.size_t) return STR.Stream_Element_Array
   is
      product : STR.Stream_Element_Array (1 .. STR.Stream_Element_Offset (number_characters));
      dondx   : Thin.IC.size_t;
   begin
      for z in product'Range loop
         dondx := Thin.IC.size_t (z);
         product (z) := STR.Stream_Element (Character'Pos (Thin.IC.To_Ada (char_data (dondx))));
      end loop;
      return product;
   end convert_to_stream_array;


   -----------------------------
   --  convert_to_char_array  --
   -----------------------------
   function convert_to_char_array
     (stream_data       : STR.Stream_Element_Array;
      output_array_size : Thin.IC.size_t) return Thin.IC.char_array
   is
      use type Thin.IC.size_t;

      product : Thin.IC.char_array := (1 .. output_array_size => Thin.IC.nul);
      dondx   : Thin.IC.size_t := 1;
   begin
      for z in stream_data'Range loop
         product (dondx) := Thin.IC.To_C (Character'Val (stream_data (z)));
         dondx := dondx + 1;
      end loop;
      return product;
   end convert_to_char_array;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism     : out Compressor;
      output_stream : not null access STR.Root_Stream_Type'Class;
      quality       : Compression_Level := Default_Compression)
   is
      use type Thin.ZSTD_CStream_ptr;
      initResult : Thin.IC.size_t;
   begin
      mechanism.target_stream := output_stream;
      mechanism.zstd_stream := Thin.ZSTD_createCStream;
      if mechanism.zstd_stream = Thin.Null_CStream_pointer then
         raise streaming_compression_initialization with "ZSTD_createCStream failure";
      end if;
      initResult := Thin.ZSTD_initCStream (zcs              => mechanism.zstd_stream,
                                           compressionLevel => Thin.IC.int (quality));
      if Natural (Thin.ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_compression_initialization with "ZSTD_initCStream failure";
      end if;
   end Initialize;


   ---------------------
   --  Compress_Data  --
   ---------------------
   procedure Compress_Data (mechanism : out Compressor;
                            data      : STR.Stream_Element_Array)
   is
      use type Thin.IC.size_t;
      use type Thin.ZSTD_CStream_ptr;

      data_in   : aliased Thin.IC.char_array := (1 .. data'Length => Thin.IC.nul);
      data_out  : aliased Thin.IC.char_array := (1 .. Buffer_Output_Size => Thin.IC.nul);
      index     : Thin.IC.size_t := data_in'First;
      inbuffer  : aliased Thin.ZSTD_inBuffer_s :=
                          (src  => Thin.ICS.To_Chars_Ptr (data_in'Unchecked_Access),
                           size => data'Length,
                           pos  => 0);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Buffer_Output_Size,
                           pos  => 0);
   begin
      if mechanism.zstd_stream = Thin.Null_CStream_pointer then
         raise streaming_compression_error with "Run initialize procedure first";
      end if;
      for z in data'Range loop
         data_in (index) := Thin.IC.To_C (Character'Val (data (z)));
         index := index + 1;
      end loop;
      loop
         exit when inbuffer.pos >= inbuffer.size;
         outbuffer.pos := 0;
         mechanism.data_size_hint :=
           Thin.ZSTD_compressStream (zcs    => mechanism.zstd_stream,
                                     output => outbuffer'Unchecked_Access,
                                     input  => inbuffer'Unchecked_Access);
         mechanism.target_stream.Write (convert_to_stream_array (data_out, outbuffer.pos));
      end loop;
   end Compress_Data;


   ----------------------------------
   --  Finalize_Compression_Frame  --
   ----------------------------------
   procedure Finalize_Compression_Frame (mechanism : Compressor)
   is
      data_out  : aliased Thin.IC.char_array := (1 .. Buffer_Output_Size => Thin.IC.nul);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Buffer_Output_Size,
                           pos  => 0);
      remaining : Thin.IC.size_t;
   begin
      remaining := Thin.ZSTD_endStream (zcs    => mechanism.zstd_stream,
                                        output => outbuffer'Unchecked_Access);
      if Natural (remaining) > 0 then
         raise streaming_compression_finalization with "not fully flushed";
      end if;
      mechanism.target_stream.Write (convert_to_stream_array (data_out, outbuffer.pos));
   end Finalize_Compression_Frame;


   ------------------------------------
   --  Recommended_Data_Buffer_Size  --
   ------------------------------------
   function Next_Data_Size_Recommendation (mechanism : Compressor) return Positive is
   begin
      return Positive (mechanism.data_size_hint);
   end Next_Data_Size_Recommendation;


   -------------------------------
   --  convert_to_stream_array  --
   -------------------------------
   function convert_to_stream_array
     (char_data         : Thin.IC.char_array;
      number_characters : Thin.IC.size_t) return STR.Stream_Element_Array
   is
      product : STR.Stream_Element_Array (1 .. STR.Stream_Element_Offset (number_characters));
      dondx   : Thin.IC.size_t;
   begin
      for z in product'Range loop
         dondx := Thin.IC.size_t (z);
         product (z) := STR.Stream_Element (Character'Pos (Thin.IC.To_Ada (char_data (dondx))));
      end loop;
      return product;
   end convert_to_stream_array;


end Zstandard.Functions.Streaming_Compression;