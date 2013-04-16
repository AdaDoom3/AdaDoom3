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
  --------------------
  -- Implementation --
  --------------------
    package body Implementation
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      Memory : Record_Memory := (others => <>);
      begin
        Put_Title("MEMORY TEST");
        Memory := Get_Data;
        Put_Line("Load"                       & Float_4_Percent'Wide_Image(Memory.Load));
        Put_Line("Physical_Total"             & Integer_8_Natural'Wide_Image(Memory.Physical_Total));
        Put_Line("Physical_Available"         & Integer_8_Natural'Wide_Image(Memory.Physical_Available));
        Put_Line("Page_File_Total"            & Integer_8_Natural'Wide_Image(Memory.Page_File_Total));
        Put_Line("Page_File_Available"        & Integer_8_Natural'Wide_Image(Memory.Page_File_Available));
        Put_Line("Virtual_Total"              & Integer_8_Natural'Wide_Image(Memory.Virtual_Total));
        Put_Line("Virtual_Available"          & Integer_8_Natural'Wide_Image(Memory.Virtual_Available));
        Put_Line("Virtual_Available_Extended" & Integer_8_Natural'Wide_Image(Memory.Virtual_Available_Extended));
        Hang_Window;
      end Test;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned)
      renames Implementation.Set_Byte_Limits;
  --------------
  -- Get_Data --
  --------------
    function Get_Data
      return Record_Memory
      renames Implementation.Get;
  ----------
  -- Lock --
  ----------
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      renames Implementation.Lock;
  ------------
  -- Unlock --
  ------------
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      renames Implementation.Unlock;
  ----------
  -- Free --
  ----------
    procedure Free(
      Item : in Address)
      renames Implementation.Free;
  --------------
  -- Allocate --
  --------------
    function Allocate(
      Number_Of_Bits    : in Integer_4_Unsigned;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address
      is
      begin
        return
          Implementation.Clear(
            Location      => Allocate_Dirty(Number_Of_Bits, Memory_Identifier),
            Initial_Value => CLEARED_MEMORY_VALUE,
            Size          => Number_Of_Bits);
      end Allocate;
  --------------------
  -- Allocate_Dirty --
  --------------------
    function Allocate_Dirty(
      Number_Of_Bits    : in Integer_4_Unsigned;
      Memory_Identifier : in Integer_Memory_Identifier := UNASSIGNED_IDENTIFIER)
      return Address
      is
      begin
        return
          Implementation.Allocate(
            Size =>
              Integer_4_Unsigned(
                (Number_Of_Bits + (MEMORY_ALIGNMENT - 1))
                and not (MEMORY_ALIGNMENT - 1)),
            Alignment => MEMORY_ALIGNMENT);
      end Allocate_Dirty;
  end Neo.System.Memory;
