-- Cyclic redundancy check to verify archived data integrity

package Zip.CRC is

  use Interfaces;

  procedure Init( CRC: out Unsigned_32 );

  function  Final( CRC: Unsigned_32 ) return Unsigned_32;

  procedure Update( CRC: in out Unsigned_32; InBuf: Zip.Byte_Buffer );
  pragma Inline( Update );

end Zip.CRC;
