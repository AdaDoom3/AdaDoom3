with Ada.Calendar; use Ada.Calendar;
package Neo.System.Input is
    No_Input_Devices_Detected            : Exception;
    Input_Enabled_Without_Being_Disabled : Exception;
    Input_Disabled_Without_Being_Enabled : Exception;
    Invalid_Device_Identifier            : Exception;
    Invalid_Player_Index                 : Exception;
    Invalid_Trigger_Index                : Exception;
    Invalid_Button_Index                 : Exception;
    Invalid_Stick_Index                  : Exception;
    type Enumerated_Stick   is (Left_Stick,                Right_Stick);
    type Enumerated_Trigger is (Left_Trigger,              Right_Trigger);
    type Enumerated_Key     is (Escape_Key,                One_Key,                    Two_Key,               Three_Key,
                                Four_Key,                  Five_Key,                   Six_Key,               Seven_Key,
                                Eight_Key,                 Nine_Key,                   Zero_Key,              Dash_Key,
                                Equals_Key,                Backspace_Key,              Tab_Key,               Q_Key,
                                W_Key,                     E_Key,                      R_Key,                 T_Key,
                                Y_Key,                     U_Key,                      I_Key,                 O_Key,
                                P_Key,                     Left_Bracket_Key,           Right_Bracket_Key,     Enter_Key,
                                Left_Control_Key,          A_Key,                      S_Key,                 D_Key,
                                F_Key,                     G_Key,                      H_Key,                 J_Key,
                                K_Key,                     L_Key,                      Semicolon_Key,         Apostrophe_Key,
                                Grave_Accent_Key,          Left_Shift_Key,             Backslash_Key,         Z_Key,
                                X_Key,                     C_Key,                      V_Key,                 B_Key,
                                N_Key,                     M_Key,                      Comma_Key,             Period_Key,
                                Slash_Key,                 Right_Shift_Key,            Star_Key,              Left_Alternative_Key,
                                Space_Key,                 Capital_Lock_Key,           F1_Key,                F2_Key,
                                F3_Key,                    F4_Key,                     F5_Key,                F6_Key,
                                F7_Key,                    F8_Key,                     F9_Key,                F10_Key,
                                Number_Lock_Key,           Scroll_Lock_Key,            Pad_Seven_Key,         Pad_Eight_Key,
                                Pad_Nine_Key,              Pad_Dash_Key,               Pad_Four_Key,          Pad_Five_Key,
                                Pad_Size_Key,              Pad_Plus_Key,               Pad_One_Key,           Pad_Two_Key,
                                Pad_Three_Key,             Pad_Zero_Key,               Pad_Period_Key,        OEM_102_Key,
                                F11_Key,                   F12_Key,                    Left_Windows_Key,      Right_Windows_Key,
                                Middle_Windows_Key,        F13_Key,                    F14_Key,               F15_Key,
                                Kana_Key,                  Brazilian_1_Key,            Convert_Key,           No_Convert_Key,
                                Yen_Key,                   Brazilian_2_Key,            Pad_Equals_Key,        Previous_Track_Key,
                                At_Symbol_Key,             Colon_Key,                  Underline_Key,         Kanji_Key,
                                Stop_Key,                  Ax_Key,                     Unlabeled_Key,         Next_Track_Key,
                                Pad_Enter_Key,             Right_Control_Key,          Volume_Mute_Key,       Calculator_Key,
                                Play_Pause_Track_Key,      Stop_Track_Key,             Volume_Down_Key,       Volume_Up_Key,
                                Web_Home_Key,              Pad_Comma_Key,              Pad_Slash_Key,         Print_Screen_Key,
                                Right_Alternative_Key,     Pause_Break_Key,            Home_Key,              Up_Arrow_Key,
                                Page_Up_Key,               Left_Arrow_Key,             Right_Arrow_Key,       End_Key,
                                Down_Arrow_Key,            Page_Down_Key,              Insert_Key,            Delete_Key,
                                Left_Windows_2_Key,        Right_Windows_2_Key,        Application_Menu_Key,  System_Power_Key,
                                System_Sleep_Key,          System_Wake_Key,            Web_Search_Key,        Web_Favorites_Key,
                                Web_Refresh_Key,           Web_Stop_Key,               Web_Forward_Key,       Web_Backward_Key,
                                My_Computer_Key,           Web_Mail_Key,               Media_Select_Key,      Cancel_Key,
                                Junja_Key,                 Final_Key,                  Hanja_Key,             Accept_Key,
                                Mode_Change_Key,           Select_Key,                 Execute_Key,           Print_Key,
                                Help_Key,                  OEM_Specific_1_Key,         OEM_Specific_2_Key,    OEM_Specific_3_Key,
                                OEM_Specific_4_Key,        OEM_Specific_5_Key,         OEM_Specific_6_Key,    OEM_Specific_7_Key,
                                OEM_Specific_8_Key,        OEM_Specific_9_Key,         OEM_Specific_10_Key,   OEM_Specific_11_Key,
                                OEM_Specific_12_Key,       OEM_Specific_13_Key,        OEM_Specific_14_Key,   OEM_Specific_15_Key,
                                OEM_Specific_16_Key,       OEM_Specific_17_Key,        OEM_Specific_18_Key,   OEM_Specific_19_Key,
                                OEM_Specific_20_Key,       OEM_Specific_21_Key,        OEM_Specific_22_Key,   OEM_Specific_23_Key,
                                OEM_Specific_24_Key,       F16_Key,                    F17_Key,               F18_Key,
                                F19_Key,                   F20_Key,                    F21_Key,               F22_Key,
                                F23_Key,                   F24_Key,                    Pad_Six_Key,           Pad_Star_Key,
                                Separator_Key,             Application_1_Key,          Application_2_Key,     Control_Key,
                                Plus_Key,                  Play_Key,                   Zoom_Key,              Clear_Key,
                                Erase_EOF_Key,             Attention_Key,              Process_Key,           Exsel_Key,
                                Clear_selection_Key,       PA1_Key,                    Alternative_Key,       Shift_Key,
                                Null_Key);
    type Enumerated_Button  is (Horizontal_Wheel_Left_Key, Horizontal_Wheel_Right_Key, Left_Mouse_Key,        Right_Mouse_Key,
                                Vertical_Wheel_Up_Key,     Vertical_Wheel_Down_Key,    Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key,
                                Middle_Mouse_Key,          Y_Button,                   B_Button,              A_Button,
                                X_Button,                  Start_Button,               Select_Button,         System_Button,
                                Left_Button,               Right_Button,               Left_Stick_Button,     Right_Stick_Button,
                                Circle_Button,             Square_Button,              Triangle_Button,       Directional_Pad_Up,
                                Directional_Pad_Down,      Directional_Pad_Left,       Directional_Pad_Right);
    type Record_Device is record
        Description                : String_2(1..128)   := (others => NULL_CHARACTER_2);
        Player                     : Integer_4_Positive := 1;
        Player_Index               : Integer_4_Positive := 1;
        Identifier                 : Integer_8_Unsigned := 0;
        Number_Of_Generic_Buttons  : Integer_4_Natural  := 0;
        Number_Of_Generic_Triggers : Integer_4_Natural  := 0;
        Number_Of_Generic_Sticks   : Integer_4_Natural  := 0;
      end record;
    type Record_Input_Coordinate is record
        X : Integer_8_Signed := 0;
        Y : Integer_8_Signed := 0;
      end record;
    procedure Test;
    procedure Initalize;
    procedure Finalize;
    procedure Disable;
    procedure Enable;
    procedure Set_Vibration(Player : in Integer_4_Positive; Frequency_High, Frequency_Low : in Float_4_Percent);
    procedure Set_Device_Owner(Identifier : in Integer_8_Unsigned; Player : in Integer_4_Positive);
    function Get_Devices return Array_Record_Device;
    function Get_Device(Identifier : in Integer_8_Unsigned) return Record_Device;
    function Get_Character_Pressed(Player : in Integer_4_Positive) return Character_2;
    function Get_Trigger(Player : in Integer_4_Positive; Trigger : in Enumerated_Trigger) return Float_4_Percent;
    function Get_Trigger(Player, Trigger : in Integer_4_Positive; Device : in Integer_8_Unsigned) return Float_4_Percent;
    function Get_Stick(
      Player : in Integer_4_Positive;
      Stick  : in Enumerated_Stick)
      return Record_Input_Coordinate;
    function Get_Stick(
      Player : in Integer_4_Positive;
      Stick  : in Integer_4_Positive;
      Device : in Integer_8_Unsigned)
      return Record_Input_Coordinate;
    function Get_Mouse(
      Player : in Integer_4_Positive)
      return Record_Input_Coordinate;
    procedure Center_Mouse;
    function Is_Pressed(
      Player : in Integer_4_Positive;
      Key    : in Enumerated_Key)
      return Boolean;
    function Is_Pressed(
      Player : in Integer_4_Positive;
      Button : in Enumerated_Button)
      return Boolean;
    function Is_Pressed(
      Player : in Integer_4_Positive;
      Button : in Integer_4_Positive;
      Device : in Integer_4_Natural)
      return Boolean;
private
    DURATION_TO_WAIT_BEFORE_KEY_REPEAT : constant Duration           := 0.1;
    DURATION_TO_WAIT_BEFORE_POLLING    : constant Duration           := 0.002;
    MAXIMUM_DEVICES_PER_PLAYER         : constant Integer_4_Positive := 32;
    task type Task_Input is
        entry Initialize;
        entry Disable;
        entry Enable;
        entry Finalize;
      end Task_Input;
    type Record_Device_Input;
    type Array_Record_Input_Coordinate
	  is array(Positive range <>)
	  of Record_Input_Coordinate;

        Triggers           : Array_Float_4_Percent         (Enumerated_Trigger'Range) := (others => <>);
        Sticks             : Array_Record_Input_Coordinate (Enumerated_Stick'Range)   := (others => <>);
        Buttons            : Array_Record_Key              (Enumerated_Button'Range)  := (others => (others => <>));
        Keys               : Array_Record_Key              (Enumerated_Key'Range)     := (others => (others => <>));

    type Access_Record_Device_Input
	  is access all Record_Device_Input;
    type Access_Array_Record_Input_Coordinate
	  is access all Array_Record_Input_Coordinate;

    type Record_Device_Input
      is record
        Description      : String_2(1..128)                     := (others => NULL_CHARACTER_2);
        Identifier       : Integer_Address                      := 0;
        Player           : Integer_4_Positive                   := 1;
        Player_Index     : Integer_4_Positive                   := 1;
        Generic_Buttons  : Access_Array_Boolean                 := null;
        Generic_Triggers : Access_Array_Float_4_Percent         := null;
        Generic_Sticks   : Access_Array_Record_Input_Coordinate := null;
        Next             : Access_Record_Device_Input           := null;
	  end Record_Device_Input;
	type Record_Player
	  is record
	    Devices  : Array_Integer_Address(1..MAXIMUM_DEVICES_PER_PLAYER) := (others => 0);
		Mouse    : Record_Input_Coordinate                              := (others => <>);
        Triggers : Array_Trigger                := (others => 0.0);
        Sticks   : Array_Stick                  := (others => (others => <>));
        Buttons  : Array_Button                 := (others => False);
        Keys     : Array_Key                    := (others => False);
		Next     : Access_Record_Player         := null;
	  end record;

    protected type Protected_Input
      is
      private



    type Record_Player

        Devices            : Access_Record_Generic_Device_Input                       := null;
        Last_Character_Key : Record_Key                                               := (others => <>);
                                       := (others => <>);
        Number_Of_Devices : Integer_4_Natural;
        Players           :
        Device_List_Head  : Access_;
	  end Protected_Input;

    Player_Input : Protected_Player_Input;
    Input        : Task_Input;

    procedure Add_Device(
      Device : in Record_Device);
    procedure Remove_Device(
      Identifier : in Integer_8_Unsigned);
    procedure Handle_Key(
      Key : in Enumerated_Key;
    function Get_Device(
      Identifier : in Integer_8_Unsigned)
      return Record_Device;
    procedure Handle_Button(
      Device : in Integer_8_Unsigned;
      Button : in Enumerated_Button);
    procedure Handle_Button(
      Device : in Integer_8_Unsigned;
      Button : in Integer_4_Positive);
    procedure Handle_Mouse(
      Device : in Integer_8_Unsigned;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed);
    procedure Handle_Stick(
      Device : in Integer_8_Unsigned;
      Stick  : in Enumerated_Stick;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed);
    procedure Handle_Stick(
      Device : in Integer_8_Unsigned
      Stick  : in Integer_4_Positive;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed);
    procedure Handle_Trigger(
      Device  : in Integer_8_Unsigned;
      Trigger : in Enumerated_Trigger;
      Percent : in Float_4_Percent);
    procedure Handle_Trigger(
      Device  : in Integer_8_Unsigned;
      Trigger : in Integer_4_Positive;
      Percent : in Float_4_Percent);

    generic
      with
        procedure Add_Device(
          Device : in Record_Device);
      with
        procedure Remove_Device(
          Identifier : in Integer_8_Unsigned);
      with
        function Get_Device(
          Identifier : in Integer_8_Unsigned)
          return Record_Device;
      with
        procedure Handle_Key(
          Device : in Integer_8_Unsigned;
          Key    : in Enumerated_Key);
      with
        procedure Handle_Button(
          Device : in Integer_8_Unsigned;
          Button : in Enumerated_Button);
      with
        procedure Handle_Button(
          Device : in Integer_8_Unsigned;
          Button : in Integer_4_Positive);
      with
        procedure Handle_Mouse(
          Device : in Integer_8_Unsigned;
          X      : in Integer_8_Signed;
          Y      : in Integer_8_Signed);
      with
        procedure Handle_Stick(
          Device : in Integer_8_Unsigned;
          Stick  : in Enumerated_Stick;
          X      : in Integer_8_Signed;
          Y      : in Integer_8_Signed);
      with
        procedure Handle_Stick(
          Device : in Integer_8_Unsigned
          Stick  : in Integer_4_Positive;
          X      : in Integer_8_Signed;
          Y      : in Integer_8_Signed);
      with
        procedure Handle_Trigger(
          Device  : in Integer_8_Unsigned;
          Trigger : in Enumerated_Trigger;
          Percent : in Float_4_Percent);
      with
        procedure Handle_Trigger(
          Device  : in Integer_8_Unsigned;
          Trigger : in Integer_4_Positive;
          Percent : in Float_4_Percent);
    package Import
      is
        procedure Initialize(
          Title : in String_2);
        procedure Finalize;
        procedure Handle_Events;
        procedure Update_Devices;
        procedure Set_Vibration(
          Device                 : in Integer_8_Unsigned;
          Percent_Frequency_High : in Float_4_Percent;
          Percent_Frequency_Low  : in Float_4_Percent);
        function Lookup_Character(
          Key                              : in Enumerated_Key;
          Is_Capital_Lock_Enabled          : in Boolean;
          Is_Number_Lock_Enabled           : in Boolean;
          Is_Left_Shift_Key_Pressed        : in Boolean;
          Is_Right_Shift_Key_Pressed       : in Boolean;
          Is_Left_Control_Key_Pressed      : in Boolean;
          Is_Right_Control_Key_Pressed     : in Boolean;
          Is_Left_Alternative_Key_Pressed  : in Boolean;
          Is_Right_Alternative_Key_Pressed : in Boolean;
          Is_Left_System_Key_Pressed       : in Boolean;
          Is_Right_System_Key_Pressed      : in Boolean;
          Is_Application_Menu_Key_Pressed  : in Boolean)
          return Character_2;
      end Import;
  end Neo.System.Input;


