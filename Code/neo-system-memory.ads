--                                                                                                                      
--                                                                                                                      
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  System,
  Interfaces.C,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  System,
  Interfaces,
  Interfaces.C,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Memory -- Memory allocation, all in one place
  is
  -------------
  -- Numbers --
  -------------
    type Integer_Memory_Identifier
      is new Integer_1_Unsigned;
  -------------
  -- Records --
  -------------
    type Record_Memory
      is record
        Load                       : Float_4_Percent;
        Free_Space_In_Gigabytes    : Integer_8_Natural;
        Physical_Total             : Integer_8_Natural;
        Physical_Available         : Integer_8_Natural;
        Page_File_Total            : Integer_8_Natural;
        Page_File_Available        : Integer_8_Natural;
        Virtual_Total              : Integer_8_Natural;
        Virtual_Available          : Integer_8_Natural;
        Virtual_Available_Extended : Integer_8_Natural;
      end record;
  ---------------
  -- Constants --
  ---------------
    CLEARED_MEMORY_VALUE  : constant Boolean                   := False;
    UNASSIGNED_IDENTIFIER : constant Integer_Memory_Identifier := 0;
    MEMORY_ALIGNMENT      : constant Integer_4_Unsigned        := 16;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned);
    function Get_Data
      return Record_Memory;
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean;
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean;
    function Allocate(
      Number_Of_Bits    : in Integer_4_Unsigned;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address;
    function Allocate_Dirty(
      Number_Of_Bits    : in Integer_4_Unsigned;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address;
    procedure Free(
      Item : in Address);
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        function Get
          return Record_Memory;
        procedure Set_Byte_Limits(
          Minimum : in Integer_4_Unsigned;
          Maximum : in Integer_4_Unsigned);
        function Lock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned)
          return Boolean;
        function Unlock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned)
          return Boolean;
        function Clear(
          Location      : in Address;
          Size          : in Integer_4_Unsigned;
          Initial_Value : in Boolean := CLEARED_MEMORY_VALUE)
          return Address;
        function Allocate(
          Size      : in Integer_4_Unsigned;
          Alignment : in Integer_4_Unsigned)
          return Address;
        procedure Free(
          Data : in Address);
      end Implementation;
  end Neo.System.Memory;
