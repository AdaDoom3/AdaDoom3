private package GID.Buffering is

  -- Attach a buffer to a stream.
  procedure Attach_Stream(
    b   :    out Input_buffer;
    stm : in     Stream_Access
  );

  function Is_stream_attached(b: Input_buffer) return Boolean;

  -- From the first call to Get_Byte, subsequent bytes must be read
  -- through Get_Byte as well since the stream is partly read in advance
  procedure Get_Byte(b: in out Input_buffer; byte: out U8);
  pragma Inline(Get_Byte);

private

  subtype Size_test_a is Byte_Array(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);

  -- is_mapping_possible: Compile-time test for checking if
  -- a Byte_Array is equivalemnt to a Ada.Streams.Stream_Element_Array.
  --
  is_mapping_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

end GID.Buffering;
