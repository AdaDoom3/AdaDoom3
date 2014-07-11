with System;                 use System;
with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;
package Neo.System is
    pragma Suppress(Elaboration_Check);
    Call_Failure : Exception;
    Unsupported  : Exception;
    type Enumerated_Icon    is (No_Icon, Warning_Icon, Information_Icon, Error_Icon);
    type Enumerated_Buttons is (Yes_No_Buttons, Okay_Button, Okay_Cancel_Buttons, Retry_Cancel_Buttons);
    type Enumerated_System  is (Unknown_System,
      Linux_System,             Linux_2_System,         Linux_2_1_System,      Linux_2_2_System,      
      Linux_2_3_System,         Linux_2_4_System,       Linux_2_5_System,      Linux_2_6_System,      
      Linux_3_System,           Linux_3_1_System,       Linux_3_2_System,      Linux_3_3_System,      
      Linux_3_4_System,         Linux_3_5_System,       Linux_3_6_System,      Linux_3_7_System,      
      Linux_3_8_System,         Linux_3_9_System,       Macintosh_System,      Macintosh_8_System,
      Macintosh_8_5_System,     Macintosh_8_6_System,   Macintosh_9_System,    Macintosh_9_1_System,
      Macintosh_9_2_System,     Macintosh_10_System,    Macintosh_10_1_System, Macintosh_10_2_System,
      Macintosh_10_3_System,    Macintosh_10_4_System,  Macintosh_10_5_System, Macintosh_10_6_System,
      Macintosh_10_7_System,    Macintosh_10_8_System,  Windows_System,        Windows_1_System,
      Windows_1_4_System,       Windows_1_4_A_System,   Windows_1_4_B_System,  Windows_1_4_10_A_System,
      Windows_1_4_10_B_System,  Windows_1_4_90_System,  Windows_2_System,      Windows_2_5_System,   
      Windows_2_5_1_System,     Windows_2_6_System,     Windows_2_6_1_System,  Windows_2_6_2_System);
    subtype Enumerated_Linux_System     is Enumerated_System range Linux_System..Linux_3_9_System;
    subtype Enumerated_Windows_System   is Enumerated_System range Windows_System..Windows_2_6_2_System;
    subtype Enumerated_Macintosh_System is Enumerated_System range Macintosh_System..Macintosh_10_8_System;
    type Record_Specifics is record
        Version  : Enumerated_System  := Unknown_System;
        Bit_Size : Integer_4_Positive := Integer_4_Unsigned'size;
        Username : String_2_Unbounded := To_String_2_Unbounded("Unnamed");
        Name     : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
        Path     : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      end record;
    type Record_Requirements is record
        Minimum_Linux     : Enumerated_Linux_System     := Linux_System;
        Minimum_Windows   : Enumerated_Windows_System   := Windows_System;
        Minimum_Macintosh : Enumerated_Macintosh_System := Macintosh_System;
      end record;
    generic
      with procedure Run;
    package Threads is
        task type Task_Unsafe is end Task_Unsafe;
        type Access_Task_Unsafe is access all Task_Unsafe;
        procedure Finalize is new Ada.Unchecked_Deallocation(Task_Unsafe, Access_Task_Unsafe);
        protected type Protected_Thread is
            procedure Initialize;
            procedure Finalize;
            function Is_Running return Boolean;
          private
            Thread : access Task_Unsafe := null;
          end Protected_Thread;
      end Threads;
    procedure Test;
    procedure Increment_Thread_Count;
    procedure Decrement_Thread_Count;
    procedure Assert_Dummy    (Value : in Boolean);
    procedure Assert_Dummy    (Value : in Address);
    procedure Assert_Dummy    (Value : in Integer_4_Signed_C);
    procedure Assert          (Value : in Boolean);
    procedure Assert          (Value : in Address);
    procedure Assert          (Value : in Integer_4_Signed_C);
    procedure Set_Alert       (Value : in Boolean);
    procedure Set_Icon        (Path : in String_2);
    procedure Open_Text       (Path : in String_2);
    procedure Open_Webpage    (Path : in String_2);
    procedure Execute         (Path : in String_2; Do_Fullscreen : in Boolean := False);
    function Get_Thread_Count return Integer_4_Positive;
    function Get_Specifics    return Record_Specifics;
    function Get_Icon         return String_2;
    function Get_Last_Error   return String_2;
    function Is_Alerting      return Boolean;
    function Is_Supported     (Requirements : in Record_Requirements) return Boolean;
    function Is_Okay
      (Name, Message : in String_2; Buttons : in Enumerated_Buttons := Okay_Button; Icon : in Enumerated_Icon := No_Icon) 
      return Boolean with pre => Name'length > 0 and Message'length > 0;
    SPECIFICS : constant Record_Specifics := Get_Specifics;
private
    package Import is
        procedure Set_Alert     (Value : in Boolean);
        procedure Assert        (Value : in Integer_4_Signed_C);
        procedure Execute       (Path : in String_2; Do_Fullscreen : in Boolean);
        procedure Open_Webpage  (Path : in String_2);
        procedure Open_Text     (Path : in String_2);
        function Get_Specifics  return Record_Specifics;
        function Get_Last_Error return Integer_4_Unsigned;
        function Is_Okay(Name, Message : in String_2; Buttons : in Enumerated_Buttons; Icon : in Enumerated_Icon)
                        return Boolean with pre => Name'length > 0 and Message'length > 0;
       end Import;
    FAILED_GET_SPECIFICS : constant String_2 := "Failed to get specifics!";
    FAILED_OPEN          : constant String_2 := "Failed to open ";
    FAILED_SET_ALERT     : constant String_2 := "Failed to alert!";
    FAILED_IS_OKAY       : constant String_2 := "Failed is okay!";
    FAILED_EXECUTE       : constant String_2 := "Failed to execute ";
    PREFIX_ERROR_NUMBER  : constant String_2 := "Error number: ";
    protected type Protected_Data is
        procedure Set_Thread_Count(Value : in Integer_4_Positive);
        procedure Set_Icon (Path  : in String_2);
        function Get_Thread_Count return Integer_4_Positive;
        function Get_Icon         return String_2;
      private
        Thread_Count : Integer_4_Positive := 1;
        Name         : String_2_Unbounded := To_String_2_Unbounded("Untitled");
        Icon         : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      end Protected_Data;
    Data         : Protected_Data;
    Alert_Status : Protected_Status;
  end Neo.System;
