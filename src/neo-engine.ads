
--                                                                                                                    
--                                                              N E O  E N G I N E                                                    
--                                                                                                                    
--                                                      Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

with System;                       use System;
with Interfaces;                   use Interfaces;
with Interfaces.C;                 use Interfaces.C;
with Ada.Finalization;             use Ada.Finalization;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Calendar;                 use Ada.Calendar;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Task_Identification;      use Ada.Task_Identification;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Neo.Vulkan;                   use Neo.Vulkan;
with Neo.OpenAL;                   use Neo.OpenAL;
with Neo.Image;                    use Neo.Image;
with Neo.Model;                    use Neo.Model;
with Neo.Audio;                    use Neo.Audio;
with GNAT.Traceback;               use GNAT.Traceback;
with GNAT.Traceback.Symbolic;      use GNAT.Traceback.Symbolic; 

package Neo.Engine is

  ----------
  -- Game --
  ----------

  procedure Game is separate;

  -----------
  -- Paths --
  -----------

  PATH_LOGS            : constant Str_16 := "./logs";
  PATH_ASSETS          : constant Str_16 := "./assets";
  PATH_SETTINGS        : constant Str_16 := "./settings";
  PATH_ICON            : constant Str_16 := PATH_ASSETS & "icon";
  PATH_CURSOR_ACTIVE   : constant Str_16 := PATH_ASSETS & "cursor_active";
  PATH_CURSOR_INACTIVE : constant Str_16 := PATH_ASSETS & "cursor_inactive";
  PATH_LOCALIZATION    : constant Str_16 := PATH_SETTINGS & "/lang.csv";
  PATH_CONFIGURATION   : constant Str_16 := PATH_SETTINGS & "/conf.txt";

  -----------------
  -- Information --
  -----------------

  type Information_State is record
      CPU_Megahertz : Int_64_Unsigned;
      Bit_Size      : Int_32_Positive;
      Username      : Str_16_Unbound;
      Name          : Str_16_Unbound;
      Launch_Path   : Str_16_Unbound;
    end record;
  function Information return Information_State;

  ---------------
  -- Clipboard --
  ---------------

  function Paste return Str_16;
  procedure Copy (Item : Str_16);

  -------------
  -- Text_IO --
  -------------

  function Debug                         return Bool;
  function Log                           return Str_16;
  function Input_Entry                   return Str_16;
  function Line_Size                     return Int_32_Positive;
  function Localize     (Item  : Str_16) return Str_16;
  procedure Put         (Item  : Char_16);
  procedure Put         (Item  : Str_16);
  procedure Put         (Item  : Str_16_Unbound);
  procedure Line        (Item  : Char_16);
  procedure Line        (Item  : Str_16);
  procedure Line        (Item  : Str_16_Unbound);
  procedure Debug       (Item  : Char_16);
  procedure Debug       (Item  : Str_16);
  procedure Debug       (Item  : Str_16_Unbound);
  procedure Debug_Line  (Item  : Char_16);
  procedure Debug_Line  (Item  : Str_16);
  procedure Debug_Line  (Item  : Str_16_Unbound);
  procedure Title       (Item  : Str_16);
  procedure Line        (Count : Int_32_Positive := 1);
  procedure Line_Size   (Val   : Int_32_Positive);
  procedure Debug       (Val   : Bool);
  procedure Input_Entry (Val   : Str_16);
  procedure Send_Log;
  procedure Save_Log;

  -------------
  -- Command --
  -------------

  procedure Load        (Path  : Str_16);
  procedure Handle      (Text  : Str_16);
  function Autocomplete (Text  : Str_16;
                         Limit : Int_32_Positive := 1)
                         return Array_Str_16_Unbound;

  generic
    Name        : Str_16;
    Help        : Str_16;
    type Var_T is (<>);
    Initial     : Var_T := Var_T'First;
    Saved       : Bool  := False;
    Settable    : Bool  := True;
    Overridable : Bool  := False;
  package CVar is
      procedure Set (Val : Var_T);
      function Get return Var_T;
    end; 
  generic
    Name : Str_16;
    with procedure Callback (Parameters : Array_Str_16_Unbound);
  package Command is end; 

  -------------
  -- Console --
  -------------

  procedure Initialize_Console;
  procedure Finalize_Console;  
  function Running_Console return Bool;

  --------------------
  -- Error_Handling --
  --------------------

  type Icon_Kind    is (No_Icon, Warning_Icon, Information_Icon, Error_Icon);
  type Buttons_Kind is (Yes_No_Buttons, Okay_Button, Okay_Cancel_Buttons, Retry_Cancel_Buttons);
  function Ok            (Name, Message : Str_16;
                          Buttons       : Buttons_Kind := Okay_Button;
                          Icon          : Icon_Kind    := No_Icon)
                          return Bool;
  function Alerting      return Bool;
  procedure Open_Webpage (Path : Str_16);
  procedure Alert        (To   : Bool := True);

  ------------
  -- Memory --
  ------------

  type Memory_State is record
      Load                 : Real_32_Percent;
      Disk                 : Int_64_Unsigned;
      Disk_Available       : Int_64_Unsigned;
      Physical             : Int_64_Unsigned;
      Physical_Available   : Int_64_Unsigned;
      Page_File            : Int_64_Unsigned;
      Page_File_Available  : Int_64_Unsigned;
      Virtual              : Int_64_Unsigned;
      Virtual_Available    : Int_64_Unsigned;
      Virtual_Available_Ex : Int_64_Unsigned;
    end record;
  function Initial_Memory return Memory_State;
  function Memory         return Memory_State;

  ---------------
  -- Processor --
  ---------------

  function Clock_Ticks return Int_64_Unsigned;
  generic
    with procedure Run;
  package Tasks is
      protected type Task_Protected is
          procedure Initialize;
          procedure Finalize;
          function Running return Bool;      
        end;
    end;

  -----------
  -- Input --
  -----------

  NO_COMBO : constant := 0;
  type Stick_Kind   is (Left_Stick,           Right_Stick);
  type Trigger_Kind is (Left_Trigger,         Right_Trigger);
  type Mouse_Kind   is (Left_Button,          Right_Button,        Middle_Button,        Aux_1_Button,          
                        Aux_2_Button,         Wheel_Up_Button,     Wheel_Down_Button,    Wheel_Left_Button,
                        Wheel_Right_Button);
  type Device_Kind  is (Keyboard_Device,      Mouse_Device,        Gamepad_Device);
  type Impulse_Kind is (Stick_Impulse,        Gamepad_Impulse,     Trigger_Impulse,      Text_Impulse,       
                        Key_Impulse,          Cursor_Impulse,      Mouse_Impulse);
  type Gamepad_Kind is (Y_Button,             B_Button,            A_Button,             X_Button,          
                        Start_Button,         Back_Button,         System_Button,        Left_Bumper_Button, 
                        Right_Bumper_Button,  DPad_Up_Button,      DPad_Down_Button,     DPad_Left_Button,  
                        DPad_Right_Button,    Left_Stick_Button,   Right_Stick_Button);
  type Key_Kind is     (Null_Key,             PA1_Key,             Alt_Key,              Shift_Key,
                        Escape_Key,           One_Key,             Two_Key,              Three_Key,
                        Four_Key,             Five_Key,            Six_Key,              Seven_Key,
                        Eight_Key,            Nine_Key,            Zero_Key,             Dash_Key,
                        Equals_Key,           Backspace_Key,       Tab_Key,              Q_Key,
                        W_Key,                E_Key,               R_Key,                T_Key,
                        Y_Key,                U_Key,               I_Key,                O_Key,
                        P_Key,                Left_Bracket_Key,    Right_Bracket_Key,    Enter_Key,
                        Left_Control_Key,     A_Key,               S_Key,                D_Key,
                        F_Key,                G_Key,               H_Key,                J_Key,
                        K_Key,                L_Key,               Semicolon_Key,        Apostrophe_Key,
                        Grave_Accent_Key,     Left_Shift_Key,      Backslash_Key,        Z_Key,
                        X_Key,                C_Key,               V_Key,                B_Key,
                        N_Key,                M_Key,               Comma_Key,            Period_Key,
                        Slash_Key,            Right_Shift_Key,     Star_Key,             Left_Alt_Key,
                        Space_Key,            Capital_Lock_Key,    F1_Key,               F2_Key,
                        F3_Key,               F4_Key,              F5_Key,               F6_Key,
                        F7_Key,               F8_Key,              F9_Key,               F10_Key,
                        Number_Lock_Key,      Scroll_Lock_Key,     Pad_Seven_Key,        Pad_Eight_Key,
                        Pad_Nine_Key,         Pad_Dash_Key,        Pad_Four_Key,         Pad_Five_Key,
                        Pad_Size_Key,         Pad_Plus_Key,        Pad_One_Key,          Pad_Two_Key,
                        Pad_Three_Key,        Pad_Zero_Key,        Pad_Period_Key,       OEM_102_Key,
                        F11_Key,              F12_Key,             Left_Windows_Key,     Right_Windows_Key,
                        Middle_Windows_Key,   F13_Key,             F14_Key,              F15_Key,
                        Kana_Key,             Brazilian_1_Key,     Convert_Key,          No_Convert_Key,
                        Yen_Key,              Brazilian_2_Key,     Pad_Equals_Key,       Previous_Track_Key,
                        At_Symbol_Key,        Colon_Key,           Underline_Key,        Kanji_Key,
                        Stop_Key,             Ax_Key,              Unlabeled_Key,        Next_Track_Key,
                        Pad_Enter_Key,        Right_Control_Key,   Volume_Mute_Key,      Calculator_Key,
                        Play_Pause_Track_Key, Stop_Track_Key,      Volume_Down_Key,      Volume_Up_Key,
                        Web_Home_Key,         Pad_Comma_Key,       Pad_Slash_Key,        Print_Screen_Key,
                        Right_Alt_Key,        Pause_Break_Key,     Home_Key,             Up_Arrow_Key,
                        Page_Up_Key,          Left_Arrow_Key,      Right_Arrow_Key,      End_Key,
                        Down_Arrow_Key,       Page_Down_Key,       Insert_Key,           Delete_Key,
                        Left_Windows_2_Key,   Right_Windows_2_Key, Application_Menu_Key, System_Power_Key,
                        System_Sleep_Key,     System_Wake_Key,     Web_Search_Key,       Web_Favorites_Key,
                        Web_Refresh_Key,      Web_Stop_Key,        Web_Forward_Key,      Web_Backward_Key,
                        My_Computer_Key,      Web_Mail_Key,        Media_Select_Key,     Cancel_Key,
                        Junja_Key,            Final_Key,           Hanja_Key,            Accept_Key,
                        Mode_Change_Key,      Select_Key,          Execute_Key,          Print_Key,
                        Help_Key,             OEM_1_Key,           OEM_2_Key,            OEM_3_Key,
                        OEM_4_Key,            OEM_5_Key,           OEM_6_Key,            OEM_7_Key,
                        OEM_8_Key,            OEM_9_Key,           OEM_10_Key,           OEM_11_Key,
                        OEM_12_Key,           OEM_13_Key,          OEM_14_Key,           OEM_15_Key,
                        OEM_16_Key,           OEM_17_Key,          OEM_18_Key,           OEM_19_Key,
                        OEM_20_Key,           OEM_21_Key,          OEM_22_Key,           OEM_23_Key,
                        OEM_24_Key,           F16_Key,             F17_Key,              F18_Key,
                        F19_Key,              F20_Key,             F21_Key,              F22_Key,
                        F23_Key,              F24_Key,             Pad_Six_Key,          Pad_Star_Key,
                        Separator_Key,        Application_1_Key,   Application_2_Key,    Control_Key,
                        Plus_Key,             Play_Key,            Zoom_Key,             Clear_Key,
                        Erase_EOF_Key,        Attention_Key,       Process_Key,          Exsel_Key,
                        Clear_selection_Key);
  type Cursor_State is record
      X : Int_64_Signed;
      Y : Int_64_Signed;
    end record;
  type Stick_State is record
      X : Real_32_Percent;
      Y : Real_32_Percent;
    end record;
  type Press_State is record
      Pressed : Bool := False;
      Last    : Time := Initial_time;
    end record;
  type Mouse_Array   is array (Mouse_Kind)   of Press_State;
  type Key_Array     is array (Key_Kind)     of Press_State;
  type Gamepad_Array is array (Gamepad_Kind) of Press_State;
  type Trigger_Array is array (Trigger_Kind) of Real_32_Percent;
  type Stick_Array   is array (Stick_Kind)   of Stick_State;
  type Player_State is record
      Triggers : Trigger_Array;
      Gamepad  : Gamepad_Array;
      Sticks   : Stick_Array;
      Keys     : Key_Array;
      Text     : Str_16_Unbound;
      Mouse    : Mouse_Array;
      Cursor   : Cursor_State;
    end record;
  type Device_State (Kind : Device_Kind) is record
      Player : Int_32_Positive := 1;
      case Kind is
        when Gamepad_Device =>
          Triggers : Trigger_Array;
          Gamepad  : Gamepad_Array;
          Sticks   : Stick_Array;
        when Keyboard_Device =>
          Keys     : Key_Array;
          Text     : Str_16_Unbound;
        when Mouse_Device =>
          Mouse    : Mouse_Array;
          Cursor   : Cursor_State;
      end case;
    end record;
  type Binding_State (Kind : Impulse_Kind) is record
      Player : Int_32_Positive;
      Combo  : Int_32_Natural := NO_COMBO;
      case Kind is
        when Cursor_Impulse   => Cursor   : Cursor_State;
        when Mouse_Impulse    => Mouse    : Mouse_Kind;
        when Keyboard_Impulse => Keyboard : Key_Kind;
        when Text_Impulse     => Text     : Str_16_Unbound;
        when Stick_Impulse    => Stick    : Stick_Kind;
        when Gamepad_Impulse  => Gamepad  : Gamepad_Kind;
        when Trigger_Impulse  => Trigger  : Trigger_Kind;
      end case;
    end record;x
  package Ordered_Device is new Ordered (Device_State);
  package Vector_Binding is new Vectors (Binding_State);
  function Get_Cursor   return Cursor_State;
  function Get_Devices  return Ordered_Device.Unsafe.Map;
  function Get_Device   (ID : Int_Address) return Device_State;
  procedure Set_Device  (ID : Int_Address; Player : Int_32_Positive := 1);
  procedure Vibrate     (Hz_High, Hz_Low : Real_32_Percent; Player : Int_32_Positive := 1);
  function Keyboard     (Key     : Key_Kind;     Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Key_Kind,     Player, Combo, Key     => Key,     others => <>));
  function Gamepad      (Trigger : Trigger_Kind; Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Trigger_Kind, Player, Combo, Trigger => Trigger, others => <>));
  function Gamepad      (Stick   : Stick_Kind;   Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Stick_Kind,   Player, Combo, Stick   => Stick,   others => <>));
  function Gamepad      (Button  : Gamepad_Kind; Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Gamepad_Kind, Player, Combo, Gamepad => Button,  others => <>));
  function Mouse        (Button  : Mouse_Kind;   Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Mouse_Kind,   Player, Combo, Mouse   => Button,  others => <>));
  function Mouse                                (Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Binding_State is ((Cursor_Kind,  Player, Combo,                     others => <>));
  generic 
    Name : Str_16;
    with procedure Callback (Binding : Binding_State);
    Is_Player_Settable : Bool := False;
  package Impulse is
      Bindings : aliased Vector_Binding.Vector;
      procedure Enable;
      procedure Disable;
    end;

  ---------------
  -- Windowing --
  ---------------

  type Mode_Kind is (Fullscreen_Mode, Multi_Monitor_Mode, Windowed_Mode);
  type Border_State is record
      Top    : Int_64_Positive;
      Bottom : Int_64_Positive;
      Left   : Int_64_Positive;
      Right  : Int_64_Positive;
    end record;
  type Border_Array is array (Int_4_Positive) of Border_State;
  function Get_Borders return Border_Array;
  package Window_X is new CVar (Name     => "winx",
                                Help     => -"Query focus of main window",   
                                Vary_T   => Int_64_Positive, 
                                Initial  => False);
  package Window_Y is new CVar (Name     => "winy"
  package Aspect_X is new CVar (Name     => "aspectx"
  package Aspect_Y is new CVar (Name     => "aspecty"   
                                Help     => -"Query if the cursor is captured",        
                                Vary_T   => Bool,
                                Initial  => True);
  package Running  is new CVar (Name     => "running", 
                                Help     => -"Set to False to quit", 
                                Vary_T   => Bool, 
                                Initial  => True);
  package Active   is new CVar (Name     => "active",  
                                Help     => -"Query focus of main window",   
                                Vary_T   => Bool, 
                                Initial  => False);
  package Mode     is new CVar (Name     => "mode",
                                Help     => -"Query window state - windowed or fullscreen",   
                                Vary_T   => State_Kind, 
                                Initial  => Fullscreen_State);
  package Menu     is new CVar (Name     => "menu",   
                                Help     => -"Query if the cursor is captured",        
                                Vary_T   => Bool,
                                Initial  => True,
                                Saved    => False,
                                Settable => False);

  ------------
  -- Vulkan --
  ------------

  type Material_Kind is (Bump_Material,   Diffuse_Material, Specular_Material, Coverage_Material, Ambient_Material, 
                         Mirror_Material, Xray_Material,    Cube_Material);
  type Light_Kind    is (Fog_Light,       Blend_Light,      Normal_Light);
  type Material_State is new Controlled with record
      Scale            : Matrix_2x3;
      Color            : Pixel_State;
      Opacity_Color    : Real_32_Percent := 100.0;
      Opacity          : Real_32_Percent := 100.0;
      Offset           : Real_32;
      Frame            : Int_32_Positive;
      Frame_Rate       : Int_32_Natural; -- Non-animated materials have a zero frame rate
      Texture          : Str_16_Unbound;
      Fragment_Program : Str_16_Unbound;
      Vertex_Program   : Str_16_Unbound;
      Fragment_Images  : Vector_Str_16_Unbound.Unsafe.Vector;
      Vertex_Args      : Vector_Vertex_Args.Unsafe.Vector;
    end record;
  type Animation_Instance_State is record
       Animation : 
    end record;
  type Mesh_Instance_State (Animated : Bool := True) is record -- will have vulkan stuff
      Location   :
      Rotation   :  
      Animations :
      Mesh       : Str_16_Unbound;
      Skeleton   :
    end record;
  type GPU_State is record
      Version              : Real_32;
      Color_Bits           : Int_4_Positive;
      Depth_Bits           : Int_4_Positive;
      Stencil_Bits         : Int_4_Positive;
      Bits_Per_Pixel       : Int_4_Positive;
      Maximum_Texture_Size : Int_4_Natural;
      Multisamples         : Int_4_Natural;
    end record;
  function Get_GPU return GPU_Specifics;
  Meshes    : Hashed_Mesh.Safe.Map;
  Images    : Hashed_Image.Safe.Map;
  Shaders   : Hashed_Stream.Safe.Map;
  Materials : Hashed_Materials.Safe.Map;


  type Shader_State is new Controlled with record
      Path : 
      Kind :


  ------------
  -- OpenAL --
  ------------

  procedure Initialize_OpenAL;
  procedure Finalize_OpenAL;

  ----------------
  -- Networking --
  ----------------

  type Connection_State is private;
  type Connection_Stats_State is record
      IP              : Str_16 (1..64);
      Packets_Read    : Int_64_Unsigned;
      Packets_Written : Int_64_Unsigned;
      Bytes_Read      : Int_64_Unsigned;
      Bytes_Written   : Int_64_Unsigned;
    end record;
  procedure Silence  (Connection : in out Connection_State);
  procedure Vocalize (Connection : in out Connection_State);
  procedure Connect  (Connection : in out Connection_State; Address   : Str_16);
  procedure Send     (Connection :        Connection_State; Recipient : Str_16; Data : Stream_Element_Array);
  function Recieve   (Connection :        Connection_State;
                      Sender     :    out Str_2_Unbound;
                      Timeout    :        Duration := 0.0)  return Stream_Array;
  function Get_Stats (Connection :        Connection_State) return Connection_Stats_State;
  function IP                                               return Str_16;
private
  type Connection_State is record
    end record;
end;
