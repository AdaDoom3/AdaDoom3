--  Experimental generic LZ77 encoder, based on LZHUF by OKUMURA & YOSHIZAKI
--  Here the Huffman code is used only to find quickly matching patterns.

generic

  ----- LZSS Parameters -----
  String_buffer_size : Integer := 2**12;
  Look_Ahead         : Integer := 65;
  Threshold          : Integer := 2;

  -- Input:
  with function  Read_byte return Byte;
  with function  More_bytes return Boolean;
  -- Output:
  with procedure Write_byte( b: Byte );
  with procedure Write_code( distance, length: Integer );

procedure Zip.LZ77;
