
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --    

with GNAT.Sockets;         use GNAT.Sockets;
with Neo.API.Vulkan;       use Neo.API.Vulkan;
with Neo.Data;             use Neo.Data;
with Neo.Data.Model;       use Neo.Data.Model;
with Neo.Data.Texture;     use Neo.Data.Texture;
with Neo.Core;             use Neo.Core;
with Neo.Core.Math;        use Neo.Core.Math;
with Neo.Core.Console;     use Neo.Core.Console;
--with Neo.Core.Compression; use Neo.Core.Compression;
with Neo.Core.Strings;     use Neo.Core.Strings;
with Neo.Core.Arrays;      use Neo.Core.Arrays;
with Neo.Core.Hashed;
with Neo.Core.Ordered;
with Neo.Core.Vectors;
with Neo.Core.Vectors_Unconstrained;

-- Primary interface for the "Game" layer, see Games/.../Base/neo-engine-game.adb for more information
package Neo.Engine is

  -- Main entry point, this should only be called by main.adb
  procedure Run;
  
  ------------
  -- Assets --
  ------------
  
  Materials : Hashed_Material.Safe_Map;
  
  -----------------
  -- Information --
  -----------------

  type OS_Info_State is record
      Path     : Str_Unbound := NULL_STR_UNBOUND;
      App_Name : Str_Unbound := NULL_STR_UNBOUND;
      Version  : Str_Unbound := NULL_STR_UNBOUND;
      Username : Str_Unbound := NULL_STR_UNBOUND;
      Bit_Size : Positive    := 1;
    end record;
  function OS_Info return OS_Info_State;

  ---------------
  -- Windowing --
  ---------------

  -- Enumerated types for cvar settings
  type Mode_Kind      is (Fullscreen_Mode, Multi_Monitor_Mode, Windowed_Mode);
  type Cursor_Kind    is (System_Cursor,   Inactive_Cursor,    Active_Cursor);
  type Activated_Kind is (Other_Activated, Click_Activated,    Other_Deactivated, Minimize_Deactivated);
  type Sampling_Kind  is (No_Sampling, X2_Sampling, X4_Sampling, X8_Sampling, X16_Sampling);

  -- Window and desktop location information
  type Border_State is record
      Top, Bottom, Left, Right : Int := 0;
    end record;
  package Vector_Border is new Neo.Core.Vectors (Border_State);
  function Get_Windows return Vector_Border.Unsafe_Array;

  ---------------
  -- Clipboard --
  ---------------

  function Paste return Str;
  procedure Copy (Item : Str);

  -----------
  -- Input --
  -----------

  -- Input device descriptions
  type Stick_Kind   is (Left_Stick,          Right_Stick);
  type Trigger_Kind is (Left_Trigger,        Right_Trigger);
  type Mouse_Kind   is (Left_Button,         Right_Button,        Middle_Button,     Aux_1_Button,          
                        Aux_2_Button,        Wheel_Up_Button,     Wheel_Down_Button, Wheel_Left_Button,    
                        Wheel_Right_Button);
  type Device_Kind  is (Keyboard_Device,     Mouse_Device,        Gamepad_Device);
  type Impulse_Kind is (Stick_Impulse,       Gamepad_Impulse,     Trigger_Impulse,   Text_Impulse,       
                        Key_Impulse,         Cursor_Impulse,      Mouse_Impulse);
  type Gamepad_Kind is (Y_Button,            B_Button,            A_Button,          X_Button,          
                        Start_Button,        Back_Button,         System_Button,     Left_Bumper_Button, 
                        Right_Bumper_Button, DPad_Up_Button,      DPad_Down_Button,  DPad_Left_Button,  
                        DPad_Right_Button,   Left_Stick_Button,   Right_Stick_Button);
  type Key_Kind is     (Null_Key,            PA1_Key,             Alt_Key,           Shift_Key,
                        Escape_Key,          One_Key,             Two_Key,           Three_Key,
                        Four_Key,            Five_Key,            Six_Key,           Seven_Key,
                        Eight_Key,           Nine_Key,            Zero_Key,          Dash_Key,
                        Equals_Key,          Backspace_Key,       Tab_Key,           Q_Key,
                        W_Key,               E_Key,               R_Key,             T_Key,
                        Y_Key,               U_Key,               I_Key,             O_Key,
                        P_Key,               Left_Bracket_Key,    Right_Bracket_Key, Enter_Key,
                        Left_Ctrl_Key,       A_Key,               S_Key,             D_Key,
                        F_Key,               G_Key,               H_Key,             J_Key,
                        K_Key,               L_Key,               Semicolon_Key,     Apostrophe_Key,
                        Grave_Accent_Key,    Left_Shift_Key,      Backslash_Key,     Z_Key,
                        X_Key,               C_Key,               V_Key,             B_Key,
                        N_Key,               M_Key,               Comma_Key,         Period_Key,
                        Slash_Key,           Right_Shift_Key,     Star_Key,          Left_Alt_Key,
                        Space_Key,           Capital_Lock_Key,    F1_Key,            F2_Key,
                        F3_Key,              F4_Key,              F5_Key,            F6_Key,
                        F7_Key,              F8_Key,              F9_Key,            F10_Key,
                        Number_Lock_Key,     Scroll_Lock_Key,     Pad_Seven_Key,     Pad_Eight_Key,
                        Pad_Nine_Key,        Pad_Dash_Key,        Pad_Four_Key,      Pad_Five_Key,
                        Pad_Size_Key,        Pad_Plus_Key,        Pad_One_Key,       Pad_Two_Key,
                        Pad_Three_Key,       Pad_Zero_Key,        Pad_Period_Key,    OEM_102_Key,
                        F11_Key,             F12_Key,             Left_Windows_Key,  Right_Windows_Key,
                        Middle_Windows_Key,  F13_Key,             F14_Key,           F15_Key,
                        Kana_Key,            Brazilian_1_Key,     Convert_Key,       No_Convert_Key,
                        Yen_Key,             Brazilian_2_Key,     Pad_Equals_Key,    Previous_Track_Key,
                        At_Symbol_Key,       Colon_Key,           Underline_Key,     Kanji_Key,
                        Stop_Key,            Ax_Key,              Unlabeled_Key,     Next_Track_Key,
                        Pad_Enter_Key,       Right_Ctrl_Key,      Volume_Mute_Key,   Calculator_Key,
                        Play_Pause_Key,      Stop_Track_Key,      Volume_Down_Key,   Volume_Up_Key,
                        Web_Home_Key,        Pad_Comma_Key,       Pad_Slash_Key,     Print_Screen_Key,
                        Right_Alt_Key,       Pause_Break_Key,     Home_Key,          Up_Arrow_Key,
                        Page_Up_Key,         Left_Arrow_Key,      Right_Arrow_Key,   End_Key,
                        Down_Arrow_Key,      Page_Down_Key,       Insert_Key,        Delete_Key,
                        Left_Windows_2_Key,  Right_Windows_2_Key, App_Menu_Key,      System_Power_Key,
                        System_Sleep_Key,    System_Wake_Key,     Web_Search_Key,    Web_Favorites_Key,
                        Web_Refresh_Key,     Web_Stop_Key,        Web_Forward_Key,   Web_Backward_Key,
                        My_Computer_Key,     Web_Mail_Key,        Media_Select_Key,  Cancel_Key,
                        Junja_Key,           Final_Key,           Hanja_Key,         Accept_Key,
                        Mode_Change_Key,     Select_Key,          Execute_Key,       Print_Key,
                        Help_Key,            OEM_1_Key,           OEM_2_Key,         OEM_3_Key,
                        OEM_4_Key,           OEM_5_Key,           OEM_6_Key,         OEM_7_Key,
                        OEM_8_Key,           OEM_9_Key,           OEM_10_Key,        OEM_11_Key,
                        OEM_12_Key,          OEM_13_Key,          OEM_14_Key,        OEM_15_Key,
                        OEM_16_Key,          OEM_17_Key,          OEM_18_Key,        OEM_19_Key,
                        OEM_20_Key,          OEM_21_Key,          OEM_22_Key,        OEM_23_Key,
                        OEM_24_Key,          F16_Key,             F17_Key,           F18_Key,
                        F19_Key,             F20_Key,             F21_Key,           F22_Key,
                        F23_Key,             F24_Key,             Pad_Six_Key,       Pad_Star_Key,
                        Separator_Key,       App_1_Key,           App_2_Key,         Ctrl_Key,
                        Plus_Key,            Play_Key,            Zoom_Key,          Clear_Key,
                        Erase_EOF_Key,       Attention_Key,       Process_Key,       Exsel_Key,
                        Clear_selection_Key);

  -- Binding states
  subtype Real_Interval is Real_64 range -100.0..100.0;
  type Stick_State  is record X, Y : Real_Interval := 0.0; end record;
  type Cursor_State is record X, Y : Int           := 0;   end record;
  type Press_State is record
      Down : Bool := False;
      Last : Time := Get_Start_Time;
    end record;

  -- Enumerated binding state arrays
  type Mouse_Array   is array (Mouse_Kind)   of Press_State;
  type Key_Array     is array (Key_Kind)     of Press_State;
  type Gamepad_Array is array (Gamepad_Kind) of Press_State;
  type Trigger_Array is array (Trigger_Kind) of Real_Percent;
  type Stick_Array   is array (Stick_Kind)   of Stick_State;

  -- Device to test for currently activated bindings
  type Device_State (Kind : Device_Kind := Keyboard_Device) is record
      Player : Positive := 1;
      case Kind is
        when Keyboard_Device =>
          Text     : Str_Unbound   := NULL_STR_UNBOUND;
          Keys     : Key_Array     := (others => (others => <>));
        when Gamepad_Device =>
          Triggers : Trigger_Array := (others => <>);
          Gamepad  : Gamepad_Array := (others => (others => <>));
          Sticks   : Stick_Array   := (others => (others => <>));
        when Mouse_Device =>
          Mouse    : Mouse_Array   := (others => (others => <>));
          Cursor   : Cursor_State  := (others => <>);
      end case;
    end record;

  -- Virbrate a player's set of gamepad devices
  procedure Vibrate (Hz_High, Hz_Low : Real_Percent; Player : Positive := 1);

  -- Various cursor operations
  function Get_Cursor_Normalized return Cursor_State;
  function Get_Cursor            return Cursor_State;
  function In_Main_Window        (Pos : Cursor_State := Get_Cursor) return Bool;
  procedure Set_Cursor           (Pos : Cursor_State);

  -- Operations to assign devices to different players or query each device's state
  package Ordered_Device is new Ordered (Int_Ptr, Device_State);
  procedure Set_Device (Id : Int_Ptr; Player : Positive := 1);
  function Get_Device  (Id : Int_Ptr) return Device_State;
  function Get_Devices                return Ordered_Device.Unsafe.Map;

  -------------
  -- Impulse --
  -------------
  --
  -- An "impulse" is how an interaction between... more info !!!
  --
  -- Ex:
  --   procedure Callback_Shoot (Args : Impulse_Arg_Array) is
  --     begin
  --       if Args (Args'First).Press.Down then
  --         Fire_Laser;
  --       end if;
  --     end;
  --   package Primary_Fire is new Impulse ("primaryfire", Callback_Shoot);
  --   ...
  --   Primary_Fire.Bindings.Append (Mouse (Left_Button));
  --

  -- Discriminate union to represent an impulse "binding"
  NO_COMBO : constant Natural := 0;
  type Binding_State (Kind : Impulse_Kind := Key_Impulse) is record
      Player : Positive;
      Combo  : Natural := NO_COMBO;
      case Kind is
        when Mouse_Impulse   => Mouse   : Mouse_Kind   := Mouse_Kind'First;
        when Key_Impulse     => Key     : Key_Kind     := Key_Kind'First;
        when Stick_Impulse   => Stick   : Stick_Kind   := Stick_Kind'First;
        when Gamepad_Impulse => Gamepad : Gamepad_Kind := Gamepad_Kind'First;
        when Trigger_Impulse => Trigger : Trigger_Kind := Trigger_Kind'First;
      when others => null; end case;
    end record;

  -- Functions to ease the creation of Binding_States
  function Keyboard                         (Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Text_Impulse,    Player, Combo,                     others => <>));
  function Keyboard (Key     : Key_Kind;     Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Key_Impulse,     Player, Combo, Key     => Key,     others => <>));
  function Gamepad  (Trigger : Trigger_Kind; Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Trigger_Impulse, Player, Combo, Trigger => Trigger, others => <>));
  function Gamepad  (Stick   : Stick_Kind;   Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Stick_Impulse,   Player, Combo, Stick   => Stick,   others => <>));
  function Gamepad  (Button  : Gamepad_Kind; Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Gamepad_Impulse, Player, Combo, Gamepad => Button,  others => <>));
  function Mouse    (Button  : Mouse_Kind;   Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Mouse_Impulse,   Player, Combo, Mouse   => Button,  others => <>));
  function Mouse                            (Combo : Natural := NO_COMBO; Player : Positive := 1) return Binding_State is ((Cursor_Impulse,  Player, Combo,                     others => <>));

  -- For convience to avoid common case statements in impulse callbacks... this needs more explaination...
  type Impulse_Arg_State (Kind : Impulse_Kind := Key_Impulse) is record 
      Binding : Binding_State (Kind);
      case Kind is
        when Trigger_Impulse => Trigger : Real_Percent := 0.0;
        when Text_Impulse    => Text    : Str_Unbound  := NULL_STR_UNBOUND;
        when Stick_Impulse   => Stick   : Stick_State  := (others => <>);
        when Cursor_Impulse  => Cursor  : Cursor_State := (others => <>);
        when Mouse_Impulse | Key_Impulse | Gamepad_Impulse => Press : Press_State := (others => <>);
      end case;
    end record;
  package Vector_Impulse_Arg is new Vectors (Impulse_Arg_State);

  -- Needed for the task-safe manipulation of bindings for each impulse
  package Vector_Binding is new Vectors (Binding_State);

  -- Actual impulse package used to dispatch input callbacks
  generic 
    Name : Str;
    with procedure Callback (Args : Vector_Impulse_Arg.Unsafe_Array);
    Settable : Bool := False;
  package Impulse is
      Bindings : aliased Vector_Binding.Safe_Vector;
      procedure Enable;
      procedure Disable;
    end;

  --------------------
  -- Error Handling --
  --------------------

  type Icon_Kind is (No_Icon, Error_Icon, Warning_Icon, Information_Icon);
  type Buttons_Kind is (Okay_Button, Yes_No_Buttons, Okay_Cancel_Buttons, Retry_Cancel_Buttons);

  -- Message box function to query user action
  function Ok (Message : Str;
               Buttons : Buttons_Kind := Okay_Button;
               Icon    : Icon_Kind    := No_Icon)
               return Bool;

  -- Use the OS to notify the user that the game needs your attention (flashing, bouncing, etc.)
  function Alerting return Bool;
  procedure Alert (Val  : Bool := True);

  -- Subprograms controlling the spawning and desturction of the auxiliary OS console window
  procedure Initialize_Console;
  procedure Finalize_Console;  
  function Running_Console return Bool;

  -- Generate debugging info for a given exception
  procedure Handle (Occurrence : Exception_Occurrence);

  -------------
  -- Tasking --
  -------------

  -- Package to create spawnable tasks in a simple way (coordination between them is done via protected types and cvars)
  generic
    with procedure Run;
  package Tasks is
      task type Task_Unsafe is
          entry Initialize (Id : out Task_ID); end;
      type Task_Unsafe_Ptr is access all Task_Unsafe;
      protected type Safe_Task is
          procedure Initialize;
          procedure Finalize;
          function Running return Bool;
        private
          Current_Task : Task_Unsafe_Ptr := null;
          Current_Id   : Task_Id         := NULL_TASK_ID;
        end;
    end;
end;
