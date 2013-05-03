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
package Neo.System.Memory
  is
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
  --------------
  -- Packages --
  --------------
    generic
      type Type_To_Manage
        is private;
    package Manager
      is
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
      Minimum : in Integer_Address;
      Maximum : in Integer_Address)
      with Pre => Minimum < Maximum;
    function Get_State
      return Record_State;
  ---------------
  -- Constants --
  ---------------
    LAUNCH_STATE : constant Record_State := Get_State;
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
          Minimum : in Integer_Address;
          Maximum : in Integer_Address)
          with Pre => Minimum < Maximum;
        procedure Lock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_Address);
        procedure Unlock(
          Location        : in Address;
          Number_Of_Bytes : in Integer_Address);
      end Implementation;
  end Neo.System.Memory;
