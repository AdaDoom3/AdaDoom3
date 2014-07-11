with Neo.System.Graphics; use Neo.System.Graphics;
with Interfaces.C;        use Interfaces.C;
with Interfaces;          use Interfaces;
with System;              use System;
package Neo.System.Window is
    type Enumerated_State is (Fullscreen_State, Multi_Monitor_State, Windowed_State);
    type Record_Specifics is record
        Width                   : Integer_4_Positive := 1;
        Height                  : Integer_4_Positive := 1;
        Color_Bits              : Integer_4_Positive := 1;
        Depth_Bits              : Integer_4_Positive := 1;
        Stencil_Bits            : Integer_4_Positive := 1;
        Bits_Per_Pixel          : Integer_4_Positive := 1;
        Mutlisamples            : Integer_4_Natural  := 0;
        Has_Swap_Control_Tear   : Boolean            := False;
        Has_Stereo_Pixel_Format : Boolean            := False;
      end record;
    procedure Test;
    procedure Run; -- To set the window icon, Neo.System.Set_Icon must be called before Run; it cannot be changed during execution
    procedure Finalize;
    function Get_Specifics return Record_Specifics;
    VARIABLE_PREFIX : constant String_2         := "w_";
    SPECIFICS       : constant Record_Specifics := Get_Specifics;
    package Width                    is new Variable(VARIABLE_PREFIX & "width",      "Windowed mode width",                     Integer_4_Positive, 640);
    package Height                   is new Variable(VARIABLE_PREFIX & "height",     "Windowed mode height",                    Integer_4_Positive, 480);
    package Multi_Samples            is new Variable(VARIABLE_PREFIX & "multi",      "Multi-samples",                           Integer_4_Positive, 5);
    package Refreashes_Per_Second    is new Variable(VARIABLE_PREFIX & "hz",         "Screen refreshes per second",             Integer_4_Positive, 9);
    package Aspect_Wide_Vertical     is new Variable(VARIABLE_PREFIX & "widevert",   "Minimum wide vertical aspect ratio",      Integer_4_Positive, 17);
    package Aspect_Wide_Horizontal   is new Variable(VARIABLE_PREFIX & "widehorz",   "Minimum wide horizontal aspect ratio",    Integer_4_Positive);
    package Aspect_Narrow_Horizontal is new Variable(VARIABLE_PREFIX & "narrowhorz", "Minimum narrow horizontal aspect ratio ", Integer_4_Positive);
    package Aspect_Narrow_Vertical   is new Variable(VARIABLE_PREFIX & "narrowvert", "Minimum narrow vertical aspect ratio ",   Integer_4_Positive);
    package X                        is new Variable(VARIABLE_PREFIX & "x",          "Windowed mode X coordinate",              Integer_4_Positive);
    package Y                        is new Variable(VARIABLE_PREFIX & "y",          "Windowed mode Y coordinate",              Integer_4_Positive);
    package Gamma_Red                is new Variable(VARIABLE_PREFIX & "red",        "Gamma red correction",                    Integer_4_Natural);
    package Gamma_Blue               is new Variable(VARIABLE_PREFIX & "blue",       "Gamma blue correction",                   Integer_4_Natural);
    package Gamma_Green              is new Variable(VARIABLE_PREFIX & "green",      "Gamma green correction",                  Integer_4_Natural);
    package State                    is new Variable(VARIABLE_PREFIX & "state",      "Window state",                            Enumerated_State, Fullscreen_State);
    package Is_Iconized              is new Variable(VARIABLE_PREFIX & "minimized",  "Query if minimized",                      Boolean, False, False, False);
    package Is_In_Menu               is new Variable(VARIABLE_PREFIX & "menu",       "Query cursor captured state",             Boolean, True,  False, False);
private
    type Enumerated_Window_Change is (Fullscreen_Change, Windowed_Change, Iconic_Change);
    type Enumerated_Resize        is (Left_Resize, Right_Resize, Top_Resize, Bottom_Resize, Top_Left_Resize, Top_Right_Resize, Bottom_Right_Resize, Bottom_Left_Resize);
    MULTI_MONITOR_NAME  : constant String_2         := " Multi Monitor";
    MINIMUM_DIMENSION_X : constant Integer_4_Signed := 256;
    Center : Array_Integer_4_Signed := (0, 0);
    type Record_Window_Border is record
        Left   : Integer_4_Signed := 0;
        Top    : Integer_4_Signed := 0;
        Right  : Integer_4_Signed := 0;
        Bottom : Integer_4_Signed := 0;
      end record;
    type Record_Monitor is Record
        Work_Area : Record_Window_Border := (others => <>);
        Desktop   : Record_Window_Border := (others => <>);
      end record;
    type Array_Record_Auxiliary_Graphics_Card is array (Integer_4_Positive range <>) of Record_Monitor;
    type Access_Array_Record_Monitor          is access all Array_Record_Monitor;
    task type Task_Multi_Monitor_Window is
        entry Initialize(I : in Integer_4_Positive);
        entry Finalize;
      end Task_Multi_Monitor_Window;
    procedure Handle_State_Change (Change : in Enumerated_Window_Change);
    procedure Handle_Window_Move  (Window_X, Window_Y, Screen_X, Screen_Y : in Integer_4_Signed);
    procedure Handle_Activation   (Do_Activate, Do_Detect_Click : in Boolean; X, Y : in Integer_4_Signed);
    function Handle_Reshape       (Resize_Location : in Enumerated_Resize; Current_Screen : in Record_Window_Border) return Record_Window_Border;
    generic
      with procedure Handle_State_Change (Change : in Enumerated_Window_Change);
      with procedure Handle_Window_Move  (Window_X, Window_Y, Screen_X, Screen_Y : in Integer_4_Signed);
      with procedure Handle_Activation   (Do_Activate, Do_Detect_Click : in Boolean; X, Y : in Integer_4_Signed);
      with function Handle_Reshape       (Resize_Location : in Enumerated_Resize; Current_Screen : in Record_Window_Border) return Record_Window_Border;
    package Import
      is
        procedure Iconize;
        procedure Move_Topmost;
        procedure Finalize;
        procedure Finalize_Multi_Monitor;
        procedure Initialize;
        procedure Initialize_Multi_Monitor (Monitors : in Array_Record_Monitor);
        procedure Set_Custom_Mouse         (Do_Restore : in Boolean);
        procedure Set_Mouse_Position       (X, Y : in Integer_4_Signed);
        procedure Adjust                   (X, Y : in Integer_4_Signed; Title : in String_2; Width, Height : in Integer_4_Positive; Do_Fullscreen : in Boolean);
        procedure Hide_Mouse               (Do_Hide, Do_Ignore_Titlebar : in Boolean);
        function Is_Only_Instance          (Name : in String_2)                return Boolean;
        function Handle_Events             (Index : in Integer_4_Natural := 0) return Boolean;
        function Get_Specifics                                                 return Record_Specifics
        function Get_Window_Border                                             return Record_Window_Border;
        function Get_Screen_Border                                             return Record_Window_Border;
        function Get_Monitors                                                  return Array_Record_Monitor;
      end Import;
  end Neo.Command.System.Window;