--
-- Translated by P2Ada (v. 15-Nov-2006)
-- from R. P. Byrne's Shrink.Pas (v. 1.2, 1989) in Turbo Pascal
-- and 16-bit assembler, then reworked by G. de Montmollin
--
-- Note about the LZW patent: as on 10-Dec-2007, one could read on
-- http://www.unisys.com/about__unisys/lzw :
--
--  Unisys U.S. LZW Patent No. 4,558,302 expired on June 20, 2003,
--  the counterpart patents in the United Kingdom, France, Germany
--  and Italy expired on June 18, 2004,
--  the Japanese counterpart patents expired on June 20, 2004
--  and the counterpart Canadian patent expired on July 7, 2004.
--

private procedure Zip.Compress.Shrink(
  input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type; -- ignored if unknown
  feedback        : Feedback_proc;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed <= uncompressed
);
