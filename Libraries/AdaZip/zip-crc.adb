package body Zip.CRC is

  CRC32_Table : array( Unsigned_32'(0)..255 ) of Unsigned_32;

  procedure Prepare_table is
    -- CRC-32 algorithm, ISO-3309
    Seed: constant:= 16#EDB88320#;
    l: Unsigned_32;
  begin
    for i in CRC32_Table'Range loop
      l:= i;
      for bit in 0..7 loop
        if (l and 1) = 0 then
          l:= Shift_Right(l,1);
        else
          l:= Shift_Right(l,1) xor Seed;
        end if;
      end loop;
      CRC32_Table(i):= l;
    end loop;
  end Prepare_table;

  procedure Update( CRC: in out Unsigned_32; InBuf: Zip.Byte_Buffer ) is
    local_CRC: Unsigned_32;
  begin
    local_CRC:= CRC ;
    for i in InBuf'Range loop
      local_CRC :=
        CRC32_Table( 16#FF# and ( local_CRC xor Unsigned_32( InBuf(i) ) ) )
        xor
        Shift_Right( local_CRC , 8 );
    end loop;
    CRC:= local_CRC;
  end Update;

  table_empty: Boolean:= True;

  procedure Init( CRC: out Unsigned_32 ) is
  begin
    if table_empty then
      Prepare_table;
      table_empty:= False;
    end if;
    CRC:= 16#FFFF_FFFF#;
  end Init;

  function Final( CRC: Unsigned_32 ) return Unsigned_32 is
  begin
    return not CRC;
  end Final;

end Zip.CRC;
