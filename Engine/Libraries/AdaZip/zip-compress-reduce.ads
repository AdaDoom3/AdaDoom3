-- Just for fun, playing with the old Zip "Reduce" format...
--
-- "Reduce" combines the LZ77 method with a probabilistic
-- predictor using a Markov chain.
--
-- The format has a single block, then a single matrix,
-- for the whole data to be compressed :-(.
--
-- NB: this compressor works in two passes, the first pass
--     serving to compute the exact optimal Markov matrix for the whole
--     data to be compressed. Hence, it is slow. However,
--     we cache the last n=LZ_cache_size bytes compressed by LZ77.
--     The result is optimal within the constraints of that format
--     (one block, poor encoding of compressed data and of the
--     compression structure, the Markov matrix itself).
--
-- Author: G. de Montmollin, January 2009
--

private procedure Zip.Compress.Reduce(
  input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type; -- ignored if unknown
  feedback        : Feedback_proc;
  method          : Reduction_Method;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates when compressed <= uncompressed
);
