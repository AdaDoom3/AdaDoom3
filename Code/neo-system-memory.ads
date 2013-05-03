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
  ----------------
  -- Exceptions --
  ----------------
    -- Lock, unlock stuff here
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
    function Allocate(
      Number_Of_Bytes   : in Integer_Address;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Array_Integer_1_Unsigned;
    function Allocate_Dirty(
      Number_Of_Bytes   : in Integer_Address;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Array_Integer_1_Unsigned;
    generic
      type Type_To_Lock;
    procedure Lock(
      Item : in out Type_To_Lock);
    generic
      type Type_To_Unlock;
    procedure Unlock(
      Item : in out Type_To_Unlock);
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
        procedure Lock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned);
        procedure Unlock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned);
        function Clear(
          Location      : in Address;
          Size          : in Integer_4_Unsigned;
          Initial_Value : in Boolean := CLEARED_MEMORY_VALUE)
          return Address;
        function Allocate(
          Size      : in Integer_4_Unsigned;
          Alignment : in Integer_4_Unsigned)
          return Address;
      end Implementation;
  end Neo.System.Memory;
