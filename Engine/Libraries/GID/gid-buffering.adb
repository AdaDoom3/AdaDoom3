with Ada.IO_Exceptions;

package body GID.Buffering is

  procedure Fill_Buffer(b: in out Input_buffer);
  -- ^ Spec here to avoid warning by 'Get_Byte' below (GNAT 2009):
  -- warning: call to subprogram with no separate spec prevents inlining

  procedure Fill_Buffer(b: in out Input_buffer)
  is
    --
    procedure BlockRead(
      buffer       :    out Byte_Array;
      actually_read:    out Natural
    )
    is
      use Ada.Streams;
      Last_Read: Stream_Element_Offset;
    begin
      if is_mapping_possible then
        declare
          SE_Buffer_mapped: Stream_Element_Array (1 .. buffer'Length);
          -- direct mapping: buffer = SE_Buffer_mapped
          for SE_Buffer_mapped'Address use buffer'Address;
          pragma Import (Ada, SE_Buffer_mapped);
        begin
          Read(b.stream.all, SE_Buffer_mapped, Last_Read);
        end;
      else
        declare
          SE_Buffer: Stream_Element_Array (1 .. buffer'Length);
          -- need to copy array (slightly slower)
        begin
          Read(b.stream.all, SE_Buffer, Last_Read);
          for i in buffer'Range loop
            buffer(i):= U8(SE_Buffer(Stream_Element_Offset(i-buffer'First)+SE_buffer'First));
          end loop;
        end;
      end if;
      actually_read:= Natural(Last_Read);
    end BlockRead;
    --
  begin
    BlockRead(
      buffer        => b.data,
      actually_read => b.MaxInBufIdx
    );
    b.InputEoF:= b.MaxInBufIdx = 0;
    b.InBufIdx := 1;
  end Fill_Buffer;

  procedure Attach_Stream(
    b   :    out Input_buffer;
    stm : in     Stream_Access
  )
  is
  begin
    b.stream:= stm;
    -- Fill_Buffer(b) will be performed on first call of Get_Byte
  end Attach_Stream;

  function Is_stream_attached(b: Input_buffer) return Boolean is
  begin
    return b.stream /= null;
  end Is_stream_attached;

  procedure Get_Byte(b: in out Input_buffer; byte: out U8) is
  begin
    if b.InBufIdx > b.MaxInBufIdx then
      Fill_Buffer(b);
      if b.InputEoF then
        raise Ada.IO_Exceptions.End_Error;
      end if;
    end if;
    byte:= b.data(b.InBufIdx);
    b.InBufIdx:= b.InBufIdx + 1;
  end Get_Byte;

end GID.Buffering;
