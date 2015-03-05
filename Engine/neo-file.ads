with Neo.System;                   use Neo.System;
with Ada.Finalization;             use Ada.Finalization;
with Ada.Wide_Text_IO;             use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
package Neo.File is
  Unknown : Exception;
  procedure Save      (Name : in String_2; Item : in Array_Stream_Element);
  function Load       (Name : in String_2) return Array_Stream_Element;
  function Build_Path (Name : in String_2) return String_2;
  generic
    type Enumerated_Format is (<>);
    type Type_To_Handle is private;
  package Handler is
      Unsupported      : Exception;
      Duplicate_Format : Exception;
      generic
        Kind       : Enumerated_Format;
        Save       : access procedure (Name : in String_2; Item : in Type_To_Handle);
        Load       : access function  (Name : in String_2) return Type_To_Handle;
        Extensions : String_2;
      package Format is
        private
          type Record_Controller is new Limited_Controlled with null record;
          procedure Initialize (Item : in out Record_Controller);
          procedure Finalize   (Item : in out Record_Controller);
          Controller : Record_Controller;
        end Format;
      procedure Save (Name : in String_2; Item : in Type_To_Handle);
      function Load  (Name : in String_2) return Type_To_Handle;
    private
      function Match_Extension (Value : in String_2) return Enumerated_Format;
      type Record_Format is record
          Save       : access procedure (Name : in String_2; Item : in Type_To_Handle) := null;
          Load       : access function  (Name : in String_2) return Type_To_Handle     := null;
          Extensions : String_2_Unbounded                                              := NULL_STRING_2_UNBOUNDED;
        end record;
      package Ordered_Map_Record_Format is new Ordered_Maps(Enumerated_Format, Record_Format);
      Formats : Ordered_Map_Record_Format.Protected_Map;
    end Handler;
  generic
    Path               : String_2;
    Comment            : String_2 := NULL_STRING_2;
    Separator          : String_2 := " ";
    Do_Convert_Tabs    : Boolean  := True;
    Do_Ignore_Multiple : Boolean  := True;
  package Parser is -- Task unsafe
      Unlexable : Exception;
      function At_End      return Boolean;
      function Peek        return String_2_Unbounded;
      function Next        return String_2_Unbounded;
      function Next_Number return Float_8_Real; -- Signs must be flush with numbers
      function Next_Set    (Starting, Ending : in String_2) return String_2_Unbounded;
      procedure Skip_Set   (Starting, Ending : in String_2);
      procedure Skip       (Number_To_Skip : in Integer_4_Positive := 1);
      procedure Assert     (Text : in String_2);
    private
      procedure Seek;
      package Vector_String_2_Unbounded is new Vectors(String_2_Unbounded);
      Data   : Vector_String_2_Unbounded.Unprotected.Vector;
      Row    : Integer_4_Positive := 1;
      Column : Integer_4_Positive := 1;
    end Parser;
private
  function Get_Extension(Name : in String_2) return String_2;
end Neo.File;
