with System;       use System;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
package Neo.System.Memory is
  pragma Suppress(Elaboration_Check);
  type Record_State is record
      Load                                       : Float_4_Percent    := 0.0;
      Number_Of_Disk_Bytes_Total                 : Integer_8_Unsigned := 0;
      Number_Of_Disk_Bytes_Available             : Integer_8_Unsigned := 0;
      Number_Of_Physical_Bytes_Total             : Integer_8_Unsigned := 0;
      Number_Of_Physical_Bytes_Available         : Integer_8_Unsigned := 0;
      Number_Of_Page_File_Bytes_Total            : Integer_8_Unsigned := 0;
      Number_Of_Page_File_Bytes_Available        : Integer_8_Unsigned := 0;
      Number_Of_Virtual_Bytes_Total              : Integer_8_Unsigned := 0;
      Number_Of_Virtual_Bytes_Available          : Integer_8_Unsigned := 0;
      Number_Of_Virtual_Bytes_Available_Extended : Integer_8_Unsigned := 0;
    end record;
  generic
    type Type_To_Manage is private;
  package Manager is
      procedure Lock   (Item : in out Type_To_Manage);
      procedure Unlock (Item : in out Type_To_Manage);
    end Manager;
  procedure Set_Byte_Limits (Minimum, Maximum : in Integer_8_Unsigned) with pre => Minimum < Maximum;
  function Get_State        return Record_State;
  INITIAL_STATE : constant Record_State := Get_State;
private
  FAILED_LOCK            : constant String_2 := "Failed to lock memory!";
  FAILED_UNLOCK          : constant String_2 := "Failed to unlock memory!";
  FAILED_GET_STATE       : constant String_2 := "Failed to get memory state!";
  FAILED_SET_BYTE_LIMITS : constant String_2 := "Failed to set memory byte limits!";
  package Import is
      procedure Set_Byte_Limits (Minimum, Maximum : in Integer_8_Unsigned) with Pre => Minimum < Maximum;
      procedure Unlock          (Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned);
      procedure Lock            (Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned);
      function Get_State        return Record_State;
    end Import;
end Neo.System.Memory;
