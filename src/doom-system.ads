
--                                                                                                                    
--                                                 A D A  D O O M  III                                                    
--                                                                                                                    
--                                         Copyright (C) 2016 Justin Squirek                                          
-- 
-- AdaDoom3 is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- AdaDoom3 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with AdaDoom3. If not, see gnu.org/licenses     
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
with Neo.Config;                   use Neo.Config;
with Neo.Image;                    use Neo.Image;
with Neo.Model;                    use Neo.Model;
with Neo.Audio;                    use Neo.Audio;
with GNAT.Traceback;               use GNAT.Traceback;
with GNAT.Traceback.Symbolic;      use GNAT.Traceback.Symbolic; 

package Neo.System is

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
  PATH_CONFIGURATION   : constant Str_16 := PATH_SETTINGS & "/conf.ini";

  -----------------
  -- Information --
  -----------------

  type Information_State is record
      CPU_Megahertz : Int_64_Unsigned;
      Bit_Size      : Int_32_Positive;
      Username      : Str_16_Unbounded;
      Name          : Str_16_Unbounded;
      Launch_Path   : Str_16_Unbounded;
    end record;
  function Get_Information return Information_State;

  ---------------
  -- Clipboard --
  ---------------

  function Paste return Str_16;
  procedure Copy (Item : Str_16);

  -------------
  -- Text_IO --
  -------------

  function Localize          (Item : Str_16) return Str_16;
  function Do_Put_Debug      return Bool;
  function Get_Log           return Str_16;
  function Get_Input_Entry   return Str_16;
  function Get_Line_Size     return Int_32_Positive;
  procedure New_Line         (Count : Int_32_Positive := 1);
  procedure Set_Line_Size    (Value : Int_32_Positive);
  procedure Set_Do_Put_Debug (Value : Bool);
  procedure Set_Input_Entry  (Value : Str_16);
  procedure Put              (Item  : Char_16);
  procedure Put              (Item  : Str_16);
  procedure Put              (Item  : Str_16_Unbounded);
  procedure Put_Line         (Item  : Char_16);
  procedure Put_Line         (Item  : Str_16);
  procedure Put_Line         (Item  : Str_16_Unbounded);
  procedure Put_Debug        (Item  : Char_16);
  procedure Put_Debug        (Item  : Str_16);
  procedure Put_Debug        (Item  : Str_16_Unbounded);
  procedure Put_Debug_Line   (Item  : Char_16);
  procedure Put_Debug_Line   (Item  : Str_16);
  procedure Put_Debug_Line   (Item  : Str_16_Unbounded);
  procedure Put_Title        (Item  : Str_16);
  procedure Send_Log;
  procedure Save_Log;

  -------------
  -- Command --
  -------------

  procedure Load        (Path : Str_16);
  procedure Handle      (Text : Str_16);
  function Autocomplete (Text  : Str_16;
                         Limit : Int_32_Positive := 1)
                         return Array_Str_16_Unbounded;

  generic
    Name                 : Str_16;
    Description          : Str_16;
    type Var_T is (<>);
    Initial              : Var_T := Var_T'First;
    Is_Saved             : Bool  := False;
    Is_Player_Settable   : Bool  := True;
    Is_Server_Overridden : Bool  := False;
  package Variable is
      procedure Set (Value : Var_T);
      function Get return Var_T;
    end; 

  generic
    Name : Str_16;
    with procedure Callback (Parameters : Array_Str_16_Unbounded);
  package Action is end; 

  -------------
  -- Console --
  -------------

  procedure Initialize_Console;
  procedure Finalize_Console;  
  function Is_Console_Running return Bool;

  --------------------
  -- Error_Handling --
  --------------------

  type Icon_Kind    is (No_Icon, Warning_Icon, Information_Icon, Error_Icon);
  type Buttons_Kind is (Yes_No_Buttons, Okay_Button, Okay_Cancel_Buttons, Retry_Cancel_Buttons);
  function Is_Okay       (Name, Message : Str_16;
                          Buttons       : Buttons_Kind := Okay_Button;
                          Icon          : Icon_Kind    := No_Icon)
                          return Bool;
  function Did_Fail      return Bool;
  function Is_Alerting   return Bool;
  procedure Open_Webpage (Path : Str_16);
  procedure Set_Alert    (Value : Bool := True);

  ------------
  -- Memory --
  ------------

  type Memory_State is record
      Load                             : Float_32_Percent;
      Disk_Bytes_Total                 : Int_64_Unsigned;
      Disk_Bytes_Available             : Int_64_Unsigned;
      Physical_Bytes_Total             : Int_64_Unsigned;
      Physical_Bytes_Available         : Int_64_Unsigned;
      Page_File_Bytes_Total            : Int_64_Unsigned;
      Page_File_Bytes_Available        : Int_64_Unsigned;
      Virtual_Bytes_Total              : Int_64_Unsigned;
      Virtual_Bytes_Available          : Int_64_Unsigned;
      Virtual_Bytes_Available_Extended : Int_64_Unsigned;
    end record;
  function Get_Initial_Memory_State return Memory_State;
  function Get_Memory_State         return Memory_State;

  ---------------
  -- Processor --
  ---------------

  function Get_Clock_Ticks return Int_64_Unsigned;

  generic
    with procedure Run;
  package Tasks is
      protected type Task_Protected is
          procedure Initialize;
          procedure Finalize;
          function Is_Running return Bool;      
        end;
    end;

  ---------------
  -- Windowing --
  ---------------

  type Mode_Kind is (Fullscreen_Mode, Multi_Monitor_Mode, Windowed_Mode);
  type Border_State is record
      Bottom : Int_64_Signed;
      Top    : Int_64_Signed;
      Left   : Int_64_Signed;
      Right  : Int_64_Signed;
    end record;
  package Vector_Border is new Vectors  (Record_Border);
  package Is_In_Menu    is new Variable (Name               => "menu",   
                                         Description        => Localize("Query if the cursor is captured"),        
                                         Type_To_Vary       => Bool,
                                         Initial            => True,
                                         Is_Saved           => False,
                                         Is_Player_Settable => False);
  package Is_Running    is new Variable (Name               => "running", 
                                         Description        => Localize("Set to False to quit"), 
                                         Type_To_Vary       => Bool, 
                                         Initial            => True);
  package Is_Active     is new Variable (Name               => "active",  
                                         Description        => Localize("Query focus of main window"),   
                                         Type_To_Vary       => Bool, 
                                         Initial            => False);
  package Window_Mode   is new Variable (Name               => "winstate",
                                         Description        => Localize("Query window state - windowed or fullscreen"),   
                                         Type_To_Vary       => State_Kind, 
                                         Initial            => Fullscreen_State);
  procedure Run_Window;
  function Get_Borders           return Vector_Record_Border.Unprotected.Vector;
  function Get_Decoration        return Record_Border;
  function Get_Normalized_Cursor return Record_Location;

  -----------
  -- Input --
  -----------

  NO_COMBO : constant := 0;
  type Device_Kind  is (Keyboard_Device, Mouse_Device, Gamepad_Device);
  type Stick_Kind   is (Left_Stick, Right_Stick);
  type Trigger_Kind is (Left_Trigger, Right_Trigger);
  type Gamepad_Kind is (Y_Button,            B_Button,          A_Button,         X_Button,          
                        Start_Button,        Back_Button,       System_Button,    Left_Bumper_Button, 
                        Right_Bumper_Button, DPad_Up_Button,    DPad_Down_Button, DPad_Left_Button,  
                        DPad_Right_Button,   Left_Stick_Button, Right_Stick_Button);
  type Mouse_Kind is   (Left_Button,           Right_Button,           Middle_Button,        
                        Aux_1_Button,          Aux_2_Button,           Vertical_Up_Button,
                        Vertical_Down_Button,  Horizontal_Left_Button, Horizontal_Right_Button);
  type Impulse_Kind is (Gamepad_Stick_Impulse, Gamepad_Button_Impulse, Gamepad_Trigger_Impulse,
                        Keyboard_Text_Impulse, Keyboard_Key_Impulse,    Mouse_Cursor_Impulse,  
                        Mouse_Button_Impulse);
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
      X : Float_32_Percent;
      Y : Float_32_Percent;
    end record;
  type Press_State is record
      Is_Pressed : Bool := False;
      Last       : Time    := INITIAL_TIME;
    end record;
  type Mouse_Array    is array (Mouse_Kind)   of Press_State;
  type Keyboard_Array is array (Key_Kind)     of Press_State;
  type Gamepad_Array  is array (Gamepad_Kind) of Press_State;
  type Trigger_Array  is array (Trigger_Kind) of Float_32_Percent;
  type Stick_Array    is array (Stick_Kind)   of Stick_State;
  type Player_State is record
      Gamepad_Triggers : Trigger_Array;
      Gamepad_Buttons  : Gamepad_Array;
      Gamepad_Sticks   : Stick_Array;
      Keyboard_Keys    : Keyboard_Array;
      Keyboard_Text    : Str_16_Unbounded;
      Mouse_Buttons    : Mouse_Array;
      Mouse_Cursor     : Cursor_State;
    end record;
  type Device_State (Kind : Device_Kind := Mouse_Device) is record
      Player : Int_32_Positive := 1;
      case Kind is
        when Gamepad_Device =>
          Triggers : Trigger_Array;
          Gamepad  : Gamepad_Array;
          Sticks   : Stick_Array;
        when Keyboard_Device =>
          Keyboard : Keyboard_Array;
          Text     : Str_16_Unbounded;
        when Mouse_Device =>
          Mouse  : Mouse_Array;
          Cursor : Cursor_State;
      end case;
    end record;
  type Binding_State (Kind : Impulse_Kind := Keyboard_Text_Impulse) is record
      Player : Int_32_Positive := 1;
      Combo  : Int_32_Natural  := NO_COMBO;
      case Kind is
        when Cursor_Impulse   => Cursor   : Cursor_State;
        when Mouse_Impulse    => Mouse    : Mouse_Kind;
        when Keyboard_Impulse => Keyboard : Key_Kind;
        when Text_Impulse     => Text     : Str_16_Unbounded;
        when Stick_Impulse    => Stick    : Stick_Kind;
        when Gamepad_Impulse  => Gamepad  : Gamepad_Kind;
        when Trigger_Impulse  => Trigger  : Trigger_Kind;
      end case;
    end record;
  package Ordered_Map_Device_State is new Ordered_Maps (Device_State);
  package Vector_Binding_State     is new Vectors      (Binding_State);
  function Get_Cursor    return Cursor_State;
  function Get_Devices   return Ordered_Map_Device_State.Unprotected.Map;
  function Get_Device    (ID : Int_Address) return Device_State;
  procedure Set_Device   (ID : Int_Address; Player : Int_32_Positive := 1);
  procedure Set_Feedback (Hz_High, Hz_Low : Float_32_Percent; Player : Int_32_Positive := 1);
  function Keyboard (Key            : Keyboard_Key_Kind;  
                     Player         : Int_32_Positive := 1 
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  function Gamepad  (Trigger        : Trigger_Kind;      
                     Player         : Int_32_Positive := 1;
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  function Gamepad  (Stick          : Gamepad_Stick_Kind;    
                     Player         : Int_32_Positive := 1;    
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  function Gamepad  (Button         : Gamepad_Button_Kind;    
                     Player         : Int_32_Positive := 1;  
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  function Mouse    (Button         : Mouse_Button_Kind;     
                     Player         : Int_32_Positive := 1
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  function Mouse    (Player         : Int_32_Positive := 1
                     Combo_Id       : Int_32_Natural  := NO_COMBO) 
                     return Binding_State;
  generic 
    Name : Str_16;
    with procedure Callback (Binding : Binding_State);
    Is_Player_Settable : Bool := False;
  package Impulse is
      Bindings : aliased Vector_Binding_State.Protected_Vector;
      procedure Enable;
      procedure Disable;
    end;

  ------------
  -- Vulkan --
  ------------

  procedure Initialize_Vulkan;
  procedure Finalize_Vulkan;

  ------------
  -- OpenAL --
  ------------

  procedure Initialize_OpenAL;
  procedure Finalize_OpenAL;

  ----------------
  -- Networking --
  ----------------

  type Connection_State is private;
  type Networking_State is record
      Network_Address           : Str_16 (1..64);
      Number_Of_Packets_Read    : Int_64_Unsigned;
      Number_Of_Packets_Written : Int_64_Unsigned;
      Number_Of_Bytes_Read      : Int_64_Unsigned;
      Number_Of_Bytes_Written   : Int_64_Unsigned;
    end record;
  procedure Silence     (Connection : in out Connection_State);
  procedure Vocalize    (Connection : in out Connection_State);
  procedure Set_Address (Connection : in out Connection_State; Address   : Str_16);
  procedure Send        (Connection :        Connection_State; Recipient : Str_16; Data : Stream_Element_Array);
  function Recieve      (Connection :        Connection_State;
                         Sender     :    out Str_2_Unbounded;
                         Timeout    :        Duration := 0.0)  return Stream_Element_Array;
  function Get_State    (Connection :        Connection_State) return Networking_State;
  function Get_IP                                              return Str_16;
private
  type Connection_State is record
    end record;
end;
