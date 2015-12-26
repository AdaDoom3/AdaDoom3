with GNAT.Traceback;          use GNAT.Traceback; -- GNAT Ada compiler required
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic; 
with System;                  use System;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Wide_Fixed;  use Ada.Strings.Wide_Fixed;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions;          use Ada.Exceptions;
with Neo.Command;             use Neo.Command;
package Neo.System is
  pragma Suppress(Elaboration_Check);
  pragma Elaborate_Body;
  Task_Initialized_Without_Being_Finalized : Exception;
  Task_Finalized_Without_Begin_Initialized : Exception;
  Call_Failure                             : Exception;
  Unsupported                              : Exception;
  type Enumerated_Icon    is (No_Icon, Warning_Icon, Information_Icon, Error_Icon);
  type Enumerated_Buttons is (Yes_No_Buttons, Okay_Button, Okay_Cancel_Buttons, Retry_Cancel_Buttons);
  
  type System_Type is (Unknown_System, Windows_System, Linux_System, Macintosh_System);
  
  type System_Version_Component_Type is (Major_Version, Minor_Version); -- On Linux called "Kernel Version" and "Major Revision" respectively.
  
  type System_Version_Type is array(System_Version_Component_Type) of Natural;
  
  type System_Requirments_Array_Type is array(System_Type range System_Type'succ(Unknown_System) .. System_Type'Last) of System_Version_Type;
  
  
  type Record_Specifics is record
      --Version   : Enumerated_System  := Unknown_System;
      System    : System_Type         := Unknown_System;
      Version   : System_Version_Type := (0, 0);
      Bit_Size  : Integer_4_Positive := Integer_4_Unsigned'size;
      Username  : String_2_Unbounded := To_String_2_Unbounded("Unnamed");
      Name      : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Path      : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Separator : Character_2        := NULL_CHARACTER_2;
    end record;
  
  procedure Trace;
  procedure Handle_Exception (Occurrence : in Exception_Occurrence);
  procedure Assert_Dummy     (Value : in Boolean);
  procedure Assert_Dummy     (Value : in Address);
  procedure Assert_Dummy     (Value : in Integer_Address);
  procedure Assert_Dummy     (Value : in Integer_4_Signed_C);
  procedure Assert_Dummy     (Value : in Integer_4_Unsigned_C);
  procedure Assert           (Value : in Boolean);
  procedure Assert           (Value : in Address);
  procedure Assert           (Value : in Integer_4_Signed_C);
  procedure Set_Alert        (Value : in Boolean);
  procedure Open_Text        (Path : in String_2);
  procedure Open_Webpage     (Path : in String_2);
  procedure Execute          (Path : in String_2; Do_Fullscreen : in Boolean := False);
  function Get_Specifics     return Record_Specifics;
  function Get_Last_Error    return String_2;
  function Is_Alerting       return Boolean;
  function Is_Supported      (Requirements : in System_Requirments_Array_Type) return Boolean;
  function Is_Okay           (Name, Message : in String_2; Buttons : in Enumerated_Buttons := Okay_Button; Icon : in Enumerated_Icon := No_Icon) return Boolean with pre => Name'length > 0 and Message'length > 0;
  SPECIFICS       : constant Record_Specifics := Get_Specifics;
  PATH_VARIABLES  : constant String_2         := PATH_SETTINGS & SPECIFICS.Separator & "variables.csv";
  PATH_LOCALS     : constant String_2         := PATH_SETTINGS & SPECIFICS.Separator & "locals.csv";
  VARIABLE_PREFIX : constant String_2         := "s_";
  package Is_In_Menu is new Variable(VARIABLE_PREFIX & "menu",    "Query cursor captured state",           Boolean, True, False, False);
  package Is_Active  is new Variable(VARIABLE_PREFIX & "active",  "Query activity of main application",    Boolean, False);
  package Is_Running is new Variable(VARIABLE_PREFIX & "running", "Controls state - set to False to quit", Boolean, True);
  generic
    with procedure Run;
  package Tasks is
      type Task_Unsafe;
      type Access_Task_Unsafe is access all Task_Unsafe;
      task type Task_Unsafe is entry Initialize(Id : in out Task_Id); end Task_Unsafe;
      procedure Finalize is new Ada.Unchecked_Deallocation(Task_Unsafe, Access_Task_Unsafe);
      protected type Protected_Task is
          procedure Initialize;
          procedure Finalize;
          function Is_Running return Boolean;
        private
          Current_Task : Access_Task_Unsafe := null;
          Current_Id   : Task_Id            := NULL_TASK_ID;
        end Protected_Task;
    end Tasks;
private
  package Import is
      procedure Open_Webpage  (Path : in String_2);
      procedure Open_Text     (Path : in String_2);
      procedure Execute       (Path : in String_2; Do_Fullscreen : in Boolean);
      procedure Set_Alert     (Value : in Boolean);
      function Get_Specifics  return Record_Specifics;
      function Get_Last_Error return Integer_4_Unsigned;
      function Is_Okay(Name, Message : in String_2; Buttons : in Enumerated_Buttons; Icon : in Enumerated_Icon) return Boolean with pre => Name'length > 0 and Message'length > 0;
    end Import;
  --function Localize(Item : in String_2) return String_2;
  PREFIX_ERROR_NUMBER  : constant String_2 := "Error number: ";
  FAILED_GET_SPECIFICS : constant String_2 := "Failed to get specifics!";
  FAILED_SET_ALERT     : constant String_2 := "Failed to alert!";
  FAILED_SAVE_LOG      : constant String_2 := "Failed to save log!";
  FAILED_SEND_LOG      : constant String_2 := "Failed to send log!";
  FAILED_COPY_LOG      : constant String_2 := "Failed to copy log!";
  FAILED_IS_OKAY       : constant String_2 := "Failed is okay!";
  FAILED_EXECUTE       : constant String_2 := "Failed to execute ";
  FAILED_OPEN          : constant String_2 := "Failed to open ";
  Data         : Protected_Data;
  Alert_Status : Protected_Status;
end Neo.System;
