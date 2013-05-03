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
      is new Integer_Address;
  -------------
  -- Records --
  -------------
    type Record_State
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
    CLEARED_MEMORY_VALUE      : constant Boolean                   := False;
    UNASSIGNED_IDENTIFIER     : constant Integer_Memory_Identifier := 0;
    MEMORY_ALIGNMENT_IN_BYTES : constant Integer_4_Unsigned        := 2;
  --------------
  -- Packages --
  --------------
    generic
      type Type_To_Manage
        is private;
      Manager_Identifier : Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER;
    package Manager -- wip
      is
        type Array_Type_To_Manage
          is array(Positive range <>)
          of Type_To_Manage;
        function Allocate
          return Type_To_Manage;
        function Allocate(
          Number_To_Allocate : in Integer_4_Positive)
          return Array_Type_To_Manage;
        function Allocate_Dirty
          return Type_To_Manage;
        function Allocate_Dirty(
          Number_To_Allocate : in Integer_4_Positive)
          return Array_Type_To_Manage;
        procedure Lock(
          Item : in out Type_To_Manage);
        procedure Unlock(
          Item : in out Type_To_Manage);
      end Manager;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned);
    function Get_State
      return Record_State;
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        function Get_State
          return Record_State;
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
