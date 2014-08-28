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
  Neo.Archetype;
use
  Neo.Archetype;
separate(Neo.Command.System.Window)
generic
  with
    procedure Handle_Finalization;
  with
    procedure Handle_Activation(
      Do_Activate     : in Boolean;
      Do_Detect_Click : in Boolean;
      X               : in Integer_4_Signed;
      Y               : in Integer_4_Signed);
  with
    procedure Handle_State_Change(
      Change : in Enumerated_Window_Change);
  with
    procedure Handle_Window_Move(
      Window_X : in Integer_4_Signed;
      Window_Y : in Integer_4_Signed;
      Screen_X : in Integer_4_Signed;
      Screen_Y : in Integer_4_Signed);
  with
    function Handle_Resize(
      Resize_Location : in Enumerated_Resize;
      Current_Screen  : in Record_Window_Border)
      return Record_Window_Border;
package body Implementation
  is pragma Source_File_Name("neo-window-implementation.adb");
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Class_Name  : in String_2;
      Icon_Path   : in String_2;
      Cursor_Path : in String_2)
      is
      begin
        raise Unimplemented_Feature;
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
        raise Unimplemented_Feature;
      end Finalize;
  ------------------------------
  -- Initialize_Multi_Monitor --
  ------------------------------
    procedure Initialize_Multi_Monitor(
      Monitors : in Array_Record_Monitor)
      is
      begin
        raise Unimplemented_Feature;
      end Initialize_Multi_Monitor;
  ----------------------------
  -- Finalize_Multi_Monitor --
  ----------------------------
    procedure Finalize_Multi_Monitor
      is
      begin
        raise Unimplemented_Feature;
      end Finalize_Multi_Monitor;
  ----------------------------
  -- Get_Screen_Information --
  ----------------------------
    procedure Get_Screen_Information(
      Bits_Per_Pixel : in out Integer_4_Positive;
      Native_Width   : in out Integer_4_Positive;
      Native_Height  : in out Integer_4_Positive)
      is
      begin
        raise Unimplemented_Feature;
      end Get_Screen_Information;
  ------------------
  -- Get_Monitors --
  ------------------
    function Get_Monitors
      return Array_Record_Monitor
      is
      begin
        raise Unimplemented_Feature;
      end Get_Monitors;
  -----------------------
  -- Get_Window_Border --
  -----------------------
    function Get_Window_Border
      return Record_Window_Border
      is
      begin
        raise Unimplemented_Feature;
      end Get_Window_Border;
  -----------------------
  -- Get_Screen_Border --
  -----------------------
    function Get_Screen_Border
      return Record_Window_Border
      is
      begin
        raise Unimplemented_Feature;
      end Get_Screen_Border;
  ----------------------
  -- Set_Custom_Mouse --
  ----------------------
    procedure Set_Custom_Mouse(
      Do_Restore_System_Mouse : in Boolean := False)
      is
      begin
        raise Unimplemented_Feature;
      end Set_Custom_Mouse;
  -------------------
  -- Handle_Events --
  -------------------
    function Handle_Events(
      Index : in Integer_4_Natural := 0)
      return Boolean
      is
      begin
        raise Unimplemented_Feature;
      end Handle_Events;
  ------------
  -- Adjust --
  ------------
    procedure Adjust(
      X             : in Integer_4_Signed;
      Y             : in Integer_4_Signed;
      Title         : in String_2;
      Width         : in Integer_4_Positive;
      Height        : in Integer_4_Positive;
      Do_Fullscreen : in Boolean)
      is
      begin
        raise Unimplemented_Feature;
      end Adjust;
  ----------------------
  -- Is_Only_Instance --
  ----------------------
    function Is_Only_Instance(
      Name : in String_2)
      return Boolean
      is
      begin
        raise Unimplemented_Feature;
      end Is_Only_Instance;
  -----------------------------------------
  -- Move_Topmost_Windows_Out_Of_The_Way --
  -----------------------------------------
    procedure Move_Topmost_Windows_Out_Of_The_Way
      is
      begin
        raise Unimplemented_Feature;
      end Move_Topmost_Windows_Out_Of_The_Way;
  ----------------
  -- Hide_Mouse --
  ----------------
    procedure Hide_Mouse(
      Do_Hide            : in Boolean;
      Do_Ignore_Titlebar : in Boolean := False)
      is
      begin
        raise Unimplemented_Feature;
      end Hide_Mouse;
  -------------
  -- Iconize --
  -------------
    procedure Iconize
      is
      begin
        raise Unimplemented_Feature;
      end Iconize;
  end Implementation;

