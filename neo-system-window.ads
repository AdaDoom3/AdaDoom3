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
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing,
  Neo.Foundation.Generic_Protected;
use
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
package Neo.System.Window
  is
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Renderer
      is(
      OpenGL,
      Direct3D);
    type Enumerated_Window_State
      is(
      Multi_Monitor_State,
      Fullscreen_State,
      Windowed_State);
  -------------
  -- Records --
  -------------
    type Record_Gamma
      is record
        Red   : Integer_4_Natural := 0;
        Green : Integer_4_Natural := 0;
        Blue  : Integer_4_Natural := 0;
      end record;
    type Record_Aspect_Ratio
      is record
        Horizontal : Integer_4_Positive := 1;
        Vertical   : Integer_4_Positive := 1;
      end record;
    type Record_Window
      is record
        Title                  : Access_String_2         := null;
        Icon_Path              : Access_String_2         := null;
        Cursor_Path            : Access_String_2         := null;
        Renderer               : Enumerated_Renderer     := OpenGL;
        State                  : Enumerated_Window_State := Windowed_State;
        Refreshes_Per_Second   : Integer_4_Positive      := 15;
        Multi_Samples          : Integer_4_Positive      := 1;
        Height                 : Integer_4_Positive      := 480;
        Width                  : Integer_4_Positive      := 640;
        X                      : Integer_4_Signed        := 0;
        Y                      : Integer_4_Signed        := 0;
        Is_In_Menu_Mode        : Boolean                 := True;
        Is_Changing_Mode       : Boolean                 := False;
        Is_Iconized            : Boolean                 := False;
        Is_Done                : Boolean                 := False;
        Is_Handling_Characters : Boolean                 := False;
        Aspect_Wide            : Record_Aspect_Ratio     := (17, 9);
        Aspect_Narrow          : Record_Aspect_Ratio     := (1, 1);
        Gamma                  : Record_Gamma            := <>;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    generic
      with
        procedure Handle_Character(
          Item : in Character_2);
      with
        procedure 
    procedure Run(
      Title                       : in String_2;
      Icon_Path                   : in String_2;
      Cursor_Path                 : in String_2;
      Do_Allow_Multiple_Instances : in Boolean := False;);
    procedure Finalize;
    procedure Test;
    procedure Put;
    function Get
      return Record_Window;
    procedure Set(
      Renderer             : in Enumerated_Renderer;
      Refreshes_Per_Second : in Integer_4_Positive;
      Multi_Samples        : in Integer_4_Positive;
      Gamma                : in Record_Gamma;
      Height               : in Integer_4_Positive;
      Width                : in Integer_4_Positive;
      X                    : in Integer_4_Signed;
      Y                    : in Integer_4_Signed;
      Aspect_Wide          : in Record_Aspect_Ratio;
      Aspect_Narrow        : in Record_Aspect_Ratio;
      Do_Enter_Menu_Mode   : in Boolean;
      State                : in Enumerated_Window_State);
    procedure Set_Menu_Mode(
      Do_Enter_Menu_Mode : in Boolean);
    procedure Set_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed);
    procedure Set_Position(
      Position : in Array_1x2_Integer_4_Signed);
    procedure Set_Renderer(
      Renderer : in Enumerated_Renderer);
    procedure Set_State(
      State : in Enumerated_Window_State);
    procedure Set_Refreshes_Per_Second(
      Refreshes_Per_Second : in Integer_4_Positive);
    procedure Set_Multi_Samples(
      Multi_Samples : in Integer_4_Positive);
    procedure Set_Gamma(
      Gamma : in Record_Gamma);
    procedure Set_Height(
      Height : in Integer_4_Positive);
    procedure Set_Width(
      Width : in Integer_4_Positive);
    procedure Set_Dimensions(
      Height : in Integer_4_Positive;
      Width  : in Integer_4_Positive);
    procedure Set_Narrow_Aspect_Ratio(
      Aspect_Narrow : in Record_Aspect_Ratio);
    procedure Set_Wide_Aspect_Ratio(
      Aspect_Wide : in Record_Aspect_Ratio);
    procedure Take_Control;
    procedure Enable_Character_Handling;
    procedure Disable_Character_Handling;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    MINIMUM_DIMENSION_X : constant Integer_4_Signed := 256;
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
  --------------
  -- Packages --
  --------------
    package Protected_Record_Window
      is new Neo.Foundation.Generic_Protected(Record_Window);
  ---------------
  -- Variables --
  ---------------
    Protected_Data : Protected_Record_Window.Data;
    Center         : array (1..2) of Integer_4_Signed := (others => 0);
  -----------------
  -- Subprograms --
  -----------------
    procedure Handle_Finalization;
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
    function Handle_Resize(
      Resize_Location : in Enumerated_Resize;
      Current_Screen  : in Record_Window_Border)
      return Record_Window_Border;
  -----------
  -- Tasks --
  -----------
    task type Task_Multi_Monitor_Window
      is
      entry Initialize(
        I : in Integer_4_Positive);
      entry Finalize;
      end Task_Multi_Monitor_Window;
  --------------------
  -- Implementation --
  --------------------
    generic
      procedure Handle_Finalization;
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
      function Handle_Resize(
        Resize_Location : in Enumerated_Resize;
        Current_Screen  : in Record_Window_Border)
        return Record_Window_Border;
    package Implementation_For_Operating_System
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
      end Implementation_For_Operating_System;
    package body Implementation_For_Operating_System
      is separate;
    package Implementation
      is new Implementation_For_Operating_System(
        Handle_Finalization => Handle_Finalization,
        Handle_Activation   => Handle_Activation, 
        Handle_State_Change => Handle_State_Change,
        Handle_Window_Move  => Handle_Window_Move,
        Handle_Resize       => Handle_Resize);
  end Neo.System.Window;
