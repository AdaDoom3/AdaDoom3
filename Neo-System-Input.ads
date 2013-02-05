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
  Neo.Foundation.Generic_Protected;
use
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options;
package Neo.System.Input
  is
  ---------------
  -- Constants --
  ---------------
    MICROSECOND_DELAY                                 : constant Integer_4_Natural  := 0;
    NUMBER_OF_PERIPHERALS_TO_CHECK_ON_TICK_FOR_LEGACY : constant Integer_4_Positive := 5;
    DO_CHECK_ALL_PERIPHERALS_ON_TICK_FOR_LEGACY       : constant Boolean            := False;
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Key
      is(
      Escape_Key,            One_Key,               Two_Key,               Three_Key,
      Four_Key,              Five_Key,              Six_Key,               Seven_Key,
      Eight_Key,             Nine_Key,              Zero_Key,              Dash_Key,
      Equals_Key,            Backspace_Key,         Tab_Key,               Q_Key,
      W_Key,                 E_Key,                 R_Key,                 T_Key,
      Y_Key,                 U_Key,                 I_Key,                 O_Key,  
      P_Key,                 Left_Bracket_Key,      Right_Bracket_Key,     Enter_Key,  
      Left_Control_Key,      A_Key,                 S_Key,                 D_Key,  
      F_Key,                 G_Key,                 H_Key,                 J_Key,  
      K_Key,                 L_Key,                 Semicolon_Key,         Apostrophe_Key,  
      Grave_Accent_Key,      Left_Shift_Key,        Backslash_Key,         Z_Key,  
      X_Key,                 C_Key,                 V_Key,                 B_Key,  
      N_Key,                 M_Key,                 Comma_Key,             Period_Key,  
      Slash_Key,             Right_Shift_Key,       Star_Key,              Left_Alternative_Key,  
      Space_Key,             Capital_Lock_Key,      F1_Key,                F2_Key,  
      F3_Key,                F4_Key,                F5_Key,                F6_Key,  
      F7_Key,                F8_Key,                F9_Key,                F10_Key,  
      Number_Lock_Key,       Scroll_Lock_Key,       Pad_Seven_Key,         Pad_Eight_Key,  
      Pad_Nine_Key,          Pad_Dash_Key,          Pad_Four_Key,          Pad_Five_Key,  
      Pad_Size_Key,          Pad_Plus_Key,          Pad_One_Key,           Pad_Two_Key,  
      Pad_Three_Key,         Pad_Zero_Key,          Pad_Period_Key,        OEM_102_Key,  
      F11_Key,               F12_Key,               Left_Windows_Key,      Right_Windows_Key,  
      Middle_Windows_Key,    F13_Key,               F14_Key,               F15_Key, 
      Kana_Key,              Brazilian_1_Key,       Convert_Key,           No_Convert_Key, 
      Yen_Key,               Brazilian_2_Key,       Pad_Equals_Key,        Previous_Track_Key, 
      At_Symbol_Key,         Colon_Key,             Underline_Key,         Kanji_Key, 
      Stop_Key,              Ax_Key,                Unlabeled_Key,         Next_Track_Key, 
      Pad_Enter_Key,         Right_Control_Key,     Volume_Mute_Key,       Calculator_Key, 
      Play_Pause_Track_Key,  Stop_Track_Key,        Volume_Down_Key,       Volume_Up_Key, 
      Web_Home_Key,          Pad_Comma_Key,         Pad_Slash_Key,         Print_Screen_Key, 
      Right_Alternative_Key, Pause_Break_Key,       Home_Key,              Up_Arrow_Key, 
      Page_Up_Key,           Left_Arrow_Key,        Right_Arrow_Key,       End_Key, 
      Down_Arrow_Key,        Page_Down_Key,         Insert_Key,            Delete_Key, 
      Left_Windows_2_Key,    Right_Windows_2_Key,   Application_Menu_Key,  System_Power_Key, 
      System_Sleep_Key,      System_Wake_Key,       Web_Search_Key,        Web_Favorites_Key, 
      Web_Refresh_Key,       Web_Stop_Key,          Web_Forward_Key,       Web_Backward_Key, 
      My_Computer_Key,       Web_Mail_Key,          Media_Select_Key,      Left_Mouse_Key,
      Right_Mouse_Key,       Middle_Mouse_Key,      Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key,
      Junja_Key,             Final_Key,             Hanja_Key,             Accept_Key,
      Mode_Change_Key,       Select_Key,            Execute_Key,           Print_Key,
      Help_Key,              OEM_Specific_1_Key,    OEM_Specific_2_Key,    OEM_Specific_3_Key,
      OEM_Specific_4_Key,    OEM_Specific_5_Key,    OEM_Specific_6_Key,    OEM_Specific_7_Key,
      OEM_Specific_8_Key,    OEM_Specific_9_Key,    OEM_Specific_10_Key,   OEM_Specific_11_Key,
      OEM_Specific_12_Key,   OEM_Specific_13_Key,   OEM_Specific_14_Key,   OEM_Specific_15_Key,
      OEM_Specific_16_Key,   OEM_Specific_17_Key,   OEM_Specific_18_Key,   OEM_Specific_19_Key,
      OEM_Specific_20_Key,   OEM_Specific_21_Key,   OEM_Specific_22_Key,   OEM_Specific_23_Key,
      OEM_Specific_24_Key,   F16_Key,               F17_Key,               F18_Key,
      F19_Key,               F20_Key,               F21_Key,               F22_Key,
      F23_Key,               F24_Key,               Pad_Six_Key,           Pad_Star_Key,
      Separator_Key,         Application_1_Key,     Application_2_Key,     Vertical_Wheel_Mouse_Down_Key,
      Plus_Key,              Play_Key,              Zoom_Key,              Vertical_Wheel_Mouse_Up_Key,
      Erase_EOF_Key,         Attention_Key,         Process_Key,           Horizontal_Wheel_Mouse_Right_Key,
      Clear_selection_Key,   PA1_Key,               Cancel_Key,            Horizontal_Wheel_Mouse_Left_Key,
      Null_Key,              Exsel_Key,             Clear_Key,             X_Button,
      Y_Button,              B_Button,              A_Button,              Start_Button,
      Select_Button,         System_Button,         Left_Button,           Directional_Pad_Button,
      Right_Button,          Left_Stick_Button,     Right_Stick_Button,    Circle_Button,
      Square_Button,         Triangle_Button,       Generic_Button,        Hat_North,
      Hat_North_East,        Hat_East,              Hat_South_East,        Hat_South,
      Hat_South_West,        Hat_West,              Hat_North_West);
    type Enumerated_Stick
      is(
      Left_Stick, Right_Stick, Generic_Stick);
    type Enumerated_Pedal
      is(
      Left_Trigger, Right_Trigger, Generic_Pedal);
    type Enumerated_Wheel
      is(
      Generic_Wheel);
  --------------
  -- Accessors --
  ---------------
    type Access_Procedure_Handle_Axis
      is access procedure(
        Information  : in Event_Information;
        Player       : in Integer_4_Positive;
        Identifier   : in Integer_4_Positive;
        Axis         : in Enumerated_Axis;
        Degree       : in Float_Degree;
        Displacement : in Float_Percent);
    type Access_Procedure_Handle_Movement
      is access procedure(
        Information    : in Event_Information;
        Player         : in Integer_4_Positive;
        X              : in Integer_4_Signed;
        Y              : in Integer_4_Signed);
    type Access_Procedure_Handle_Key
      is access procedure(
        Information  : in Event_Information;
        Player       : in Integer_4_Positive;
        Identifier   : in Integer_4_Positive;
        Is_Press     : in Boolean;
        Key          : in Enumerated_Key);
    type Access_Procedure_Handle_Accelerometer
      is access procedure(
        Information  : in Event_Information;
        Player       : in Integer_4_Positive;
        Identifier   : in Integer_4_Positive);
    type Access_Procedure_Handle_Character
      is access procedure(
        Information  : in Event_Information;
        Player       : in Integer_4_Positive;
        Item         : in Character_2);
    type Access_Procedure_Handle_Pedal
      is access procedure(
        Information  : in Event_Information;
        Player       : in Integer_4_Positive;
        Identifier   : in Integer_4_Positive;
        Pedal        : in Enumerated_Pedal;
        Displacement : in Float_Percent);
    type Access_Procedure_Handle_Peripheral
      is access procedure(
        Information         : in Event_Information;
        Is_Being_Plugged_In : in Boolean;
        Player              : in Integer_4_Positive;
        Peripheral          : in Record_Peripheral);
  -------------
  -- Records --
  -------------
    type Record_Peripheral
      is record
        Identifier    : Access_String_2;
        Player_Number : Integer_4_Positive;
      end record;
    type Record_Input
      is record
        Number_Of_Mice                 : Integer_4_Natural;
        Number_Of_Keyboards            : Integer_4_Natural;
        Number_Of_Xbox_360_Controllers : Integer_4_Natural;
        Number_Of_Joysticks            : Integer_4_Natural;
        Number_Of_Controllers          : Integer_4_Natural;
        Number_Of_Touch_Screen         : Integer_4_Natural;
        Number_Of_Accelerometers       : Integer_4_Natural;
        Handle_Axis                    : Access_Procedure_Handle_Axis;
        Handle_Movement                : Access_Procedure_Handle_Movement;
        Handle_Pedal                   : Access_Procedure_Handle_Pedal;
        Handle_Character               : Access_Procedure_Handle_Character;
        Handle_Peripheral              : Access_Procedure_Handle_Peripheral;
        Handle_Accelerometer           : Access_Procedure_Handle_Accelerometer;
        Handle_Key                     : Access_Procedure_Handle_Key;
      end record;
  ------------
  -- Arrays --
  ------------
    type Array_Record_Peripheral
      is array (Positive range <>)
      of Record_Peripheral;
  -----------------
  -- Subprograms --
  -----------------
    procedure Initalize;
    procedure Finalize;
    procedure Disable;
    procedure Enable;
    function Get_State
      return Record_Input;
    function Get_Peripherals
      return Array_Record_Peripheral;
    procedure Update_Vibration(
      Player  : in Integer_4_Positive;
      Percent : in Float_Percent);
    procedure Set_Player(
      Peripheral : in Record_Peripheral;
      Player     : in Integer_4_Positive);
    procedure Set_Axis_Handler(
      Handler : in Access_Procedure_Handle_Axis);
    procedure Set_Movement_Handler(
      Handler : in Access_Procedure_Handle_Movement);
    procedure Set_Pedal_Handler(
      Handler : in Access_Procedure_Handle_Pedal);
    procedure Set_Character_Handler(
      Handler : in Access_Procedure_Handle_Character);
    procedure Set_Peripheral_Handler(
      Handler : in Access_Procedure_Handle_Peripheral);
    procedure Set_Accelerometer_Handler(
      Handler : in Access_Procedure_Handle_Accelerometer);
    procedure Set_Key_Handler(
      Handler : in Access_Procedure_Handle_Key);
-------
private
-------
  -----------
  -- Tasks --
  -----------
    task type Task_Input
      is
      entry Initialize;
      entry Finalize;
      end Task_Input;
  --------------
  -- Packages --
  --------------
    package Protected_Record_Input
      is new Foundational.Generic_Protected(Record_Input, NULL_RECORD_INPUT);
  ---------------
  -- Variables --
  ---------------
    Input          : Task_Input;
    Protected_Data : Protected_Record_Input.Data;
  end Neo.System.Input;
