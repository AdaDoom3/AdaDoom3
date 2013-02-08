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
package body Neo.System.Memory
  is
  ----------
  -- Test --
  ----------
    procedure Test
      is
      Memory : Record_Memory := NULL_RECORD_MEMORY;
      begin
        Display_Title("SYSTEM TEST");
        Put;
        Hang_Window;
      end Test;
  ---------
  -- Put --
  ---------
    procedure Put
      is
      Memory : Record_Memory := NULL_RECORD_MEMORY;
      begin
        Memory := Get_Memory_Status;
        Put_Line(TAB & "Load"                       & Float_Percent'Wide_Image(Memory.Load));
        Put_Line(TAB & "Physical_Total"             & Integer_8_Natural'Wide_Image(Memory.Physical_Total));
        Put_Line(TAB & "Physical_Available"         & Integer_8_Natural'Wide_Image(Memory.Physical_Available));
        Put_Line(TAB & "Page_File_Total"            & Integer_8_Natural'Wide_Image(Memory.Page_File_Total));
        Put_Line(TAB & "Page_File_Available"        & Integer_8_Natural'Wide_Image(Memory.Page_File_Available));
        Put_Line(TAB & "Virtual_Total"              & Integer_8_Natural'Wide_Image(Memory.Virtual_Total));
        Put_Line(TAB & "Virtual_Available"          & Integer_8_Natural'Wide_Image(Memory.Virtual_Available));
        Put_Line(TAB & "Virtual_Available_Extended" & Integer_8_Natural'Wide_Image(Memory.Virtual_Available_Extended));
      end Put;
  ---------
  -- Set --
  ---------
    procedure Set(
      Minimum_Number_Of_Bytes : in Integer_4_Natural;
      Maximum_Number_Of_Bytes : in Integer_4_Natural)
      renames Set(Minimum_Number_Of_Bytes, Maximum_Number_Of_Bytes);
  ---------
  -- Get --
  ---------
    function Get
      return Record_Memory
      renames Implementation.Get;
  ----------
  -- Lock --
  ----------
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      renames Implementation.Lock(Location, Number_Of_Bytes);
  ------------
  -- Unlock --
  ------------
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      renames Implementation.Unlock(Location, Number_Of_Bytes);
  --------------
  -- Allocate --
  --------------
    function Allocate(
      Number_Of_Bits    : in Integer_4_Positive;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address
      is
      begin
        return
          Clear(
            Location      => Allocate_Dirty(Number_Of_Bits, Memory_Identifier),
            Initial_Value => CLEARED_MEMORY_VALUE,
            Size          => Number_Of_Bits);
      end Allocate;
  --------------------
  -- Allocate_Dirty --
  --------------------
    function Allocate_Dirty(
      Number_Of_Bits    : in Integer_4_Positive;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address
      is
      begin
        return
          Allocate(
            Size =>
              Integer_4_Positive(
                (Integer_4_Unsigned(Number_Of_Bits) + (MEMORY_ALIGNMENT - 1))
                and not (MEMORY_ALIGNMENT - 1)),
            Alignment => MEMORY_ALIGNMENT);
      end Allocate_Dirty;
  ----------
  -- Free --
  ----------
    procedure Free(
      Item : in Address)
      renames Free(Item);
  end Neo.System.Memory;
