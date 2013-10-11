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
  Interfaces,
  Interfaces.C;
use
  System,
  Interfaces,
  Interfaces.C;
generic
  with
    procedure Handle_Graphics;
package Neo.Command.System.Window
  is pragma Source_File_Name("neo-window.ads");
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Window_State
      is(
      Multi_Monitor_State,
      Fullscreen_State,
      Windowed_State);
  -------------
  -- Records --
  -------------
    type Record_Specifics
      is record
        Color_Bits              : Integer_4_Positive := 1;
        Depth_Bits              : Integer_4_Positive := 1;
        Stencil_Bits            : Integer_4_Positive := 1;
        Mutlisamples            : Integer_4_Natural  := 0;
        Has_Swap_Control_Tear   : Boolean            := False;
        Has_Stereo_Pixel_Format : Boolean            := False;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Finalize;
    function Get_Specifics
      return Record_Specifics;
  ---------------
  -- Constants --
  ---------------
    VARIABLE_PREFIX : constant String_2         := "w_";
    SPECIFICS       : constant Record_Specifics := Get_Specifics;
  --------------
  -- Packages --
  --------------
    package State
      is new Variable(
        Type_To_Vary => Enumerated_Window_State,
        Initial      => Fullscreen_State,
        Name         => VARIABLE_PREFIX & "state",
        Description  => "Window state");
    package Gamma_Red
      is new Variable(
        Type_To_Vary => Integer_4_Natural,
        Initial      => 0,
        Name         => VARIABLE_PREFIX & "red",
        Description  => "Gamma red correction");
    package Gamma_Blue
      is new Variable(
        Type_To_Vary => Integer_4_Natural,
        Initial      => 0,
        Name         => VARIABLE_PREFIX & "blue",
        Description  => "Gamma blue correction");
    package Gamma_Green
      is new Variable(
        Type_To_Vary => Integer_4_Natural,
        Initial      => 0,
        Name         => VARIABLE_PREFIX & "green",
        Description  => "Gamma green correction");
    package Refreashes_Per_Second
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 15,
        Name         => VARIABLE_PREFIX & "hz",
        Description  => "Screen refreshes per second");
    package Multi_Samples
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 1,
        Name         => VARIABLE_PREFIX & "multisam"
        Description  => "Multi-samples");
    package Aspect_Wide_Horizontal
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 9,
        Name         => VARIABLE_PREFIX & "widehorz",
        Description  => "Windowed mode minimum narrow aspect ratio horizontal");
    package Aspect_Wide_Vertical
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 17,
        Name         => VARIABLE_PREFIX & "widevert",
        Description  => "Windowed mode minimum wide aspect ratio vertical");
    package Aspect_Narrow_Horizontal
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 1,
        Name         => VARIABLE_PREFIX & "narrowhorz",
        Description  => "Windowed mode minimum narrow aspect ratio horizontal");
    package Aspect_Narrow_Vertical
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 1,
        Name         => VARIABLE_PREFIX & "narrowvert",
        Description  => "Windowed mode minimum narrow aspect ratio vertical");
    package Height
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 480,
        Name         => VARIABLE_PREFIX & "height",
        Description  => "Windowed mode height");
    package Width
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 640,
        Name         => VARIABLE_PREFIX & "width",
        Description  => "Windowed mode width");
    package X
      is new Variable(
        Type_To_Vary     => Integer_4_Positive,
        Initial          => 1,
        Name             => VARIABLE_PREFIX & "x",
        Description      => "Windowed mode X coordinate",
        Is_User_Settable => False);
    package Y
      is new Variable(
        Type_To_Vary => Integer_4_Positive,
        Initial      => 1,
        Name         => VARIABLE_PREFIX & "y",
        Description  => "Windowed mode Y coordinate");
    package Is_Iconized
      is new Variable(
        Type_To_Vary     => Boolean,
        Initial          => False,
        Name             => VARIABLE_PREFIX & "minimized",
        Description      => "Windowed mode minimized flag",
        Is_Saved         => False,
        Is_User_Settable => False);
    package Is_In_Menu_Mode
      is new Variable(
        Type_To_Vary     => Integer_4_Positive,
        Initial          => True,
        Name             => VARIABLE_PREFIX & "menumode",
        Description      => "Controls capture of the cursor, in non-menu mode it is invisible",
        Is_Saved         => False,
        Is_User_Settable => False);
-------
private
-------
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Window_Change
      is(
      Fullscreen_Change,
      Windowed_Change,
      Iconic_Change);
    type Enumerated_Resize
      is(
      Left_Resize,
      Right_Resize,
      Top_Resize,
      Bottom_Resize,
      Top_Left_Resize,
      Top_Right_Resize,
      Bottom_Right_Resize,
      Bottom_Left_Resize);
  ---------------
  -- Constants --
  ---------------
    MULTI_MONITOR_NAME  : constant String_2         := " Multi Monitor ";
    MINIMUM_DIMENSION_X : constant Integer_4_Signed := 256;
  -------------
  -- Records --
  -------------
    type Record_Window_Border
      is record
        Left   : Integer_4_Signed := 0;
        Top    : Integer_4_Signed := 0;
        Right  : Integer_4_Signed := 0;
        Bottom : Integer_4_Signed := 0;
      end record;
    type Record_Monitor
      is Record
        Work_Area : Record_Window_Border := <>;
        Desktop   : Record_Window_Border := <>;
      end record;
  ------------
  -- Arrays --
  ------------
    type Array_Record_Auxiliary_Graphics_Card
      is array (Integer_4_Positive range <>)
      of Record_Monitor;
  ---------------
  -- Accessors --
  ---------------
    type Access_Array_Record_Monitor
      is access all Array_Record_Monitor;
  -----------
  -- Tasks --
  -----------
    task type Task_Multi_Monitor_Window
      is
        entry Initialize(
          I : in Integer_4_Positive);
        entry Finalize;
      end Task_Multi_Monitor_Window;
  ---------------
  -- Variables --
  ---------------
    Data   : Protected_Data;
    Center : array(1..2) of Integer_4_Signed := (others => 0);
  -----------------
  -- Subprograms --
  -----------------
    procedure Handle_Activation(
      Do_Activate     : in Boolean;
      Do_Detect_Click : in Boolean;
      X               : in Integer_4_Signed;
      Y               : in Integer_4_Signed);
    procedure Handle_State_Change(
      Change : in Enumerated_Window_Change);
    procedure Handle_Window_Move(
      Window_X : in Integer_4_Signed;
      Window_Y : in Integer_4_Signed;
      Screen_X : in Integer_4_Signed;
      Screen_Y : in Integer_4_Signed);
    function Handle_Reshape(
      Resize_Location : in Enumerated_Resize;
      Current_Screen  : in Record_Window_Border)
      return Record_Window_Border;
  ------------
  -- Import --
  ------------
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
        function Handle_Reshape(
          Resize_Location : in Enumerated_Resize;
          Current_Screen  : in Record_Window_Border)
          return Record_Window_Border;
    package Import
      is
        procedure Initialize(
          Class_Name  : in String_2;
          Icon_Path   : in String_2;
          Cursor_Path : in String_2);
        procedure Finalize;
        procedure Initialize_Multi_Monitor(
          Monitors : in Array_Record_Monitor);
        procedure Finalize_Multi_Monitor;
        procedure Adjust(
          X             : in Integer_4_Signed;
          Y             : in Integer_4_Signed;
          Title         : in String_2;
          Width         : in Integer_4_Positive;
          Height        : in Integer_4_Positive;
          Do_Fullscreen : in Boolean);
        function Handle_Events(
          Index : in Integer_4_Natural := 0)
          return Boolean;
        procedure Get_Screen_Information(
          Bits_Per_Pixel : in out Integer_4_Positive;
          Native_Width   : in out Integer_4_Positive;
          Native_Height  : in out Integer_4_Positive);
        function Get_Window_Border
          return Record_Window_Border;
        function Get_Screen_Border
          return Record_Window_Border;
        function Get_Monitors
          return Array_Record_Monitor;
        function Is_Only_Instance(
          Name : in String_2)
          return Boolean;
        procedure Iconize;
        procedure Set_Mouse_Position(
          X : in Integer_4_Signed;
          Y : in Integer_4_Signed);
        procedure Hide_Mouse(
          Do_Hide            : in Boolean;
          Do_Ignore_Titlebar : in Boolean := False);
        procedure Set_Custom_Mouse(
          Do_Restore_System_Mouse : in Boolean := False);
        procedure Move_Topmost_Windows_Out_Of_The_Way;
      end Import;
  end Neo.Command.System.Window;
