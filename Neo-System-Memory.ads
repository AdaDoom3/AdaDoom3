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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Memory -- Memory allocation, all in one place
  is
  -------------
  -- Numbers --
  -------------
    type Integer_Memory_Identifier
      is new Integer_1_Natural;
  -------------
  -- Records --
  -------------
    type Record_Memory
      is record
        Load                       : Float_4_Percent;
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
    MEMORY_ALIGNMENT      : constant Integer_4_Positive        := 16;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put;
    procedure Set(
      Minimum_Number_Of_Bytes : in Integer_4_Natural;
      Maximum_Number_Of_Bytes : in Integer_4_Natural);
    procedure Set_Minimum_Number_Of_Bytes(
      New_Minimum_Number_Of_Bytes : in Integer_4_Natural);
    procedure Set_Maximum_Number_Of_Bytes(
      New_Maximum_Number_Of_Bytes : in Integer_4_Natural);
    function Get
      return Record_Memory;
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Natural)
      return Boolean;
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Natural)
      return Boolean;
    function Allocate(
      Number_Of_Bits    : in Integer_4_Positive;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address;
    function Allocate_Dirty(
      Number_Of_Bits    : in Integer_4_Positive;
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
    package Implementation_For_Operating_System
      is
        function Get
          return Record_Memory;
        procedure Set(
          Minimum_Number_Of_Bytes : in Integer_4_Natural;
          Maximum_Number_Of_Bytes : in Integer_4_Natural);
        function Lock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned)
          return Boolean;
        function Unlock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_4_Unsigned)
          return Boolean;
        function Clear(
          Location      : in ;=> Allocate_Dirty(Number_Of_Bits, Memory_Identifier),
          Initial_Value : in ;=> CLEARED_MEMORY_VALUE,
          Size          : in ;=> Number_Of_Bits)
          return Address;
        function Allocate(
          Size      : in Integer_4_Positive;
          Alignment : in Integer_4_Positive)
          return Address;
        procedure Free(
          Data : in Address);
      end Implementation;
    package body Implementation_For_Operating_System
      is separate;
    package Implementation
      renames Implementation_For_Operating_System;
  end Neo.System.Memory;
