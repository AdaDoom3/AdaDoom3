--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip.Compress
---------------
--
-- Created 9-Dec-2007
--
-- This package facilitates the storage or compression of data.
--
-- Note that unlike decompression where the decoding is unique,
-- there are an undefinite number of ways of compressing data into
-- formats which include compression structures, like Deflate.
-- As a result, you may want to use your own way (e.g. interfacing
-- with zlib).
-- This package is only a portable one, and doesn't claim
-- to be the best or the fastest

with Zip.CRC,
     Zip.Compress.Shrink,
     Zip.Compress.Reduce,
     Zip.Compress.Deflate;

package body Zip.Compress is

  use Zip_Streams;

  -------------------
  -- Compress_data --
  -------------------

  procedure Compress_data
   (input,
    output          : in out Zip_Streams.Root_Zipstream_Type'Class;
    input_size_known: Boolean;
    input_size      : File_size_type;
    method          : Compression_Method;
    feedback        : Feedback_proc;
    CRC             : out Interfaces.Unsigned_32;
    output_size     : out File_size_type;
    zip_type        : out Interfaces.Unsigned_16
   )
  is
    use Interfaces;
    counted: File_size_type;
    user_aborting: Boolean;
    idx_in:  constant Positive:= Index(input);
    idx_out: constant Positive:= Index(output);
    compression_ok: Boolean;
    first_feedback: Boolean:= True;
    --
    procedure Store_data is
    begin
      zip_type:= 0; -- "Store" method
      counted:= 0;
      while not End_Of_Stream(input) loop
        if input_size_known and counted >= input_size then
          exit;
        end if;
        -- Copy data
        declare
          -- The usage of Stream_Element_Array instead of Byte_Buffer is
          -- a workaround for the severe xxx'Read xxx'Write performance
          -- problems in the GNAT and ObjectAda compilers (as in 2009)
          Buffer      : Ada.Streams.Stream_Element_Array (1 .. buffer_size);
          Last_Read   : Ada.Streams.Stream_Element_Offset;
        begin
          Read (input, Buffer, Last_Read);
          counted:= counted + File_size_type (Last_Read);
          Write (output, Buffer (1 .. Last_Read));
          for I in 1 .. Last_Read loop
            Zip.CRC.Update(CRC, (1 => Byte (Buffer (I))));
          end loop;
        end;
        -- Feedback
        if feedback /= null and then
          (first_feedback or (counted mod (2**16)=0) or
          (input_size_known and counted = input_size))
        then
          if input_size_known then
            feedback(
              percents_done =>
                Natural( (100.0 * Float(counted)) / Float(input_size) ),
              entry_skipped => False,
              user_abort    => user_aborting );
          else
            feedback(
              percents_done => 0,
              entry_skipped => False,
              user_abort    => user_aborting );
          end if;
          first_feedback:= False;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end loop;
      output_size:= counted;
      compression_ok:= True;
    end Store_data;
    --
  begin
    Zip.CRC.Init(CRC);
    case method is
      --
      when Store =>
        Store_data;
      --
      when Shrink =>
        Zip.Compress.Shrink(
          input, output, input_size_known, input_size, feedback,
          CRC, output_size, compression_ok
        );
        zip_type:= 1; -- "Shrink" method
      --
      when Reduction_Method =>
        Zip.Compress.Reduce(
          input, output, input_size_known, input_size, feedback,
          method,
          CRC,
          output_size,
          compression_ok
        );
        zip_type:= 2 + Unsigned_16(
          Compression_Method'Pos(method) -
          Compression_Method'Pos(Reduce_1)
        );
      when Deflation_Method =>
        Zip.Compress.Deflate(
          input, output, input_size_known, input_size, feedback,
          method,
          CRC,
          output_size,
          compression_ok
        );
        zip_type:= 8;
    end case;
    CRC:= Zip.CRC.Final(CRC);
    --
    -- Handle case where compression has been unefficient:
    -- data to be compressed is too "random"; then compressed data
    -- happen to be larger than uncompressed data
    --
    if not compression_ok then
      -- Go back to the beginning and just store the data
      Set_Index(input, idx_in);
      Set_Index(output, idx_out);
      Zip.CRC.Init(CRC);
      Store_data;
      CRC:= Zip.CRC.Final(CRC);
    end if;
  end Compress_data;

end Zip.Compress;
