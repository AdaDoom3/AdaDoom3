with Ada.Calendar;                 use Ada.Calendar;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Finalization;
package Neo.System.Input is
  No_Printable_Character    : Exception;
  Invalid_Device_Identifier : Exception;
  Invalid_Trigger_Index     : Exception;
  Invalid_Button_Index      : Exception;
  Invalid_Stick_Index       : Exception;
  type Enumerated_Device  is  (Keyboard_Device, Mouse_Device, Xbox_Device, Playstation_Device, Generic_Device);
  type Enumerated_Trigger is  (Left_Trigger,               Right_Trigger);
  type Enumerated_Stick   is  (Left_Stick,                 Right_Stick);
  type Enumerated_Kind    is  (Keyboard_Key_Kind,          Mouse_Key_Kind,             Mouse_Cursor_Kind,
    Xbox_Key_Kind,             Xbox_Stick_Kind,            Xbox_Trigger_Kind,          Generic_Key_Kind,
    Generic_Trigger_Kind,      Generic_Stick_Kind,         Playstation_Key_Kind,       Playstation_Stick_Kind,
    Playstation_Trigger_Kind,  Character_Kind);
  type Enumerated_Key     is  (Null_Key,                   Alternative_Key,            Shift_Key,
    Escape_Key,                One_Key,                    Two_Key,                    Three_Key,
    Four_Key,                  Five_Key,                   Six_Key,                    Seven_Key,
    Eight_Key,                 Nine_Key,                   Zero_Key,                   Dash_Key,
    Equals_Key,                Backspace_Key,              Tab_Key,                    Q_Key,
    W_Key,                     E_Key,                      R_Key,                      T_Key,
    Y_Key,                     U_Key,                      I_Key,                      O_Key,
    P_Key,                     Left_Bracket_Key,           Right_Bracket_Key,          Enter_Key,
    Left_Control_Key,          A_Key,                      S_Key,                      D_Key,
    F_Key,                     G_Key,                      H_Key,                      J_Key,
    K_Key,                     L_Key,                      Semicolon_Key,              Apostrophe_Key,
    Grave_Accent_Key,          Left_Shift_Key,             Backslash_Key,              Z_Key,
    X_Key,                     C_Key,                      V_Key,                      B_Key,
    N_Key,                     M_Key,                      Comma_Key,                  Period_Key,
    Slash_Key,                 Right_Shift_Key,            Star_Key,                   Left_Alternative_Key,
    Space_Key,                 Capital_Lock_Key,           F1_Key,                     F2_Key,
    F3_Key,                    F4_Key,                     F5_Key,                     F6_Key,
    F7_Key,                    F8_Key,                     F9_Key,                     F10_Key,
    Number_Lock_Key,           Scroll_Lock_Key,            Pad_Seven_Key,              Pad_Eight_Key,
    Pad_Nine_Key,              Pad_Dash_Key,               Pad_Four_Key,               Pad_Five_Key,
    Pad_Size_Key,              Pad_Plus_Key,               Pad_One_Key,                Pad_Two_Key,
    Pad_Three_Key,             Pad_Zero_Key,               Pad_Period_Key,             OEM_102_Key,
    F11_Key,                   F12_Key,                    Left_Windows_Key,           Right_Windows_Key,
    Middle_Windows_Key,        F13_Key,                    F14_Key,                    F15_Key,
    Kana_Key,                  Brazilian_1_Key,            Convert_Key,                No_Convert_Key,
    Yen_Key,                   Brazilian_2_Key,            Pad_Equals_Key,             Previous_Track_Key,
    At_Symbol_Key,             Colon_Key,                  Underline_Key,              Kanji_Key,
    Stop_Key,                  Ax_Key,                     Unlabeled_Key,              Next_Track_Key,
    Pad_Enter_Key,             Right_Control_Key,          Volume_Mute_Key,            Calculator_Key,
    Play_Pause_Track_Key,      Stop_Track_Key,             Volume_Down_Key,            Volume_Up_Key,
    Web_Home_Key,              Pad_Comma_Key,              Pad_Slash_Key,              Print_Screen_Key,
    Right_Alternative_Key,     Pause_Break_Key,            Home_Key,                   Up_Arrow_Key,
    Page_Up_Key,               Left_Arrow_Key,             Right_Arrow_Key,            End_Key,
    Down_Arrow_Key,            Page_Down_Key,              Insert_Key,                 Delete_Key,
    Left_Windows_2_Key,        Right_Windows_2_Key,        Application_Menu_Key,       System_Power_Key,
    System_Sleep_Key,          System_Wake_Key,            Web_Search_Key,             Web_Favorites_Key,
    Web_Refresh_Key,           Web_Stop_Key,               Web_Forward_Key,            Web_Backward_Key,
    My_Computer_Key,           Web_Mail_Key,               Media_Select_Key,           Cancel_Key,
    Junja_Key,                 Final_Key,                  Hanja_Key,                  Accept_Key,
    Mode_Change_Key,           Select_Key,                 Execute_Key,                Print_Key,
    Help_Key,                  OEM_Specific_1_Key,         OEM_Specific_2_Key,         OEM_Specific_3_Key,
    OEM_Specific_4_Key,        OEM_Specific_5_Key,         OEM_Specific_6_Key,         OEM_Specific_7_Key,
    OEM_Specific_8_Key,        OEM_Specific_9_Key,         OEM_Specific_10_Key,        OEM_Specific_11_Key,
    OEM_Specific_12_Key,       OEM_Specific_13_Key,        OEM_Specific_14_Key,        OEM_Specific_15_Key,
    OEM_Specific_16_Key,       OEM_Specific_17_Key,        OEM_Specific_18_Key,        OEM_Specific_19_Key,
    OEM_Specific_20_Key,       OEM_Specific_21_Key,        OEM_Specific_22_Key,        OEM_Specific_23_Key,
    OEM_Specific_24_Key,       F16_Key,                    F17_Key,                    F18_Key,
    F19_Key,                   F20_Key,                    F21_Key,                    F22_Key,
    F23_Key,                   F24_Key,                    Pad_Six_Key,                Pad_Star_Key,
    Separator_Key,             Application_1_Key,          Application_2_Key,          Control_Key,
    Plus_Key,                  Play_Key,                   Zoom_Key,                   Clear_Key,
    Erase_EOF_Key,             Attention_Key,              Process_Key,                Exsel_Key,
    Clear_selection_Key,       PA1_Key,                    Horizontal_Wheel_Left_Key,  Horizontal_Wheel_Right_Key,
    Vertical_Wheel_Up_Key,     Vertical_Wheel_Down_Key,    Left_Mouse_Key,             Right_Mouse_Key,
    Auxiliary_1_Mouse_Key,     Auxiliary_2_Mouse_Key,      Middle_Mouse_Key,           Yellow_Y_Key,
    Red_B_Button_Key,          Green_A_Key,                Blue_X_Key,                 Start_Key,
    Back_Key,                  System_Key,                 Left_Bumper_Key,            Right_Bumper_Key,
    Directional_Pad_Up_Key,    Directional_Pad_Down_Key,   Directional_Pad_Left_Key,   Directional_Pad_Right_Key,
    Left_Stick_Key,            Right_Stick_Key,            Circle_Key,                 Square_Key,
    Triangle_Key);
  NO_COMBO : constant Integer_4_Natural := 0;
  subtype Enumerated_Playstation_Key is Enumerated_Key range Select_Key..Triangle_Key;
  subtype Enumerated_Keyboard_Key    is Enumerated_Key range Null_Key..PA1_Key;
  subtype Enumerated_Xbox_Key        is Enumerated_Key range Y_Key..Right_Stick_Key;
  subtype Enumerated_Mouse_Key       is Enumerated_Key range Horizontal_Wheel_Left_Key..Auxiliary_2_Mouse_Key;
  type Record_Location is record
      X : Integer_8_Signed := 0;
      Y : Integer_8_Signed := 0;
    end record;
  type Record_State is record
      Is_Pressed : Boolean := False;
      Last       : Time    := INITIAL_TIME;
    end record;
  type Array_Keyboard_Keys    is array(Enumerated_Keyboard_Key)    of Record_State;
  type Array_Xbox_Keys        is array(Enumerated_Xbox_Key)        of Record_State;
  type Array_Playstation_Keys is array(Enumerated_Playstation_Key) of Record_State;
  type Array_Mouse_Keys       is array(Enumerated_Mouse_Key)       of Record_State;
  type Array_Triggers         is array(Enumerated_Trigger)         of Float_4_Percent;
  type Array_Sticks           is array(Enumerated_Stick)           of Record_Location;
  package Vector_Boolean  is new Ada.Containers.Vectors(Integer_4_Positive, Record_State);
  package Vector_Percent  is new Ada.Containers.Vectors(Integer_4_Positive, Float_4_Percent);
  package Vector_Location is new Ada.Containers.Vectors(Integer_4_Positive, Record_Location);
  type Record_Generic_Device is record
      Description      : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Generic_Triggers : Vector_Percent.Vector;
      Generic_Sticks   : Vector_Location.Vector;
      Generic_Keys     : Vector_Boolean.Vector;
    end record;
  package Map_Generic_Device is new Hashed_Maps(Record_Generic_Device);
  type Record_Player is record
      Xbox_Triggers        : Array_Triggers                := (others => 0.0);
      Xbox_Sticks          : Array_Sticks                  := (others => (others => <>));
      Xbox_Keys            : Array_Xbox_Keys               := (others => (others => <>));
      Playstation_Triggers : Array_Triggers                := (others => 0.0);
      Playstation_Sticks   : Array_Sticks                  := (others => (others => <>));
      Playstation_Keys     : Array_Playstation_Keys        := (others => (others => <>));
      Keyboard_Keys        : Array_Keyboard_Keys           := (others => (others => <>));
      Mouse_Keys           : Array_Mouse_Keys              := (others => (others => <>));
      Mouse_Location       : Record_Location               := (others => <>);
      Generic_Devices      : Map_Generic_Device.Unsafe.Map;
    end record;
  package Map_Players is new Ordered_Maps(Integer_4_Positive, Record_Player);
  type Record_Device(Kind : Enumerated_Device := Keyboard_Device) is record
      Player : Integer_4_Positive := 1;
      case Kind is
        when Xbox_Device | Playstation_Device =>
          Triggers : Array_Triggers := (others => 0.0);
          Sticks   : Array_Sticks   := (others => (others => <>));
          case Kind is
            when Xbox_Device =>
              Xbox_Keys : Array_Xbox_Keys := (others => (others => <>));
            when Playstation_Device =>
              Playstation_Keys : Array_Playstation_Keys := (others => (others => <>));
            when others => null;
          end case;
        when Keyboard_Device =>
          Keyboard_Keys : Array_Keyboard_Keys := (others => (others => <>));
          Last_Press    : Duration            := 0.0;
        when Mouse_Device =>
          Mouse_Keys     : Array_Mouse_Keys := (others => (others => <>));
          Mouse_Location : Record_Location  := (others => <>);
        when Generic_Device =>
          Generic_Triggers : Vector_Percent.Vector;
          Generic_Sticks   : Vector_Location.Vector;
          Generic_Keys     : Vector_Boolean.Vector;
      end case;
    end record;
  package Map_Devices is new Ordered_Maps(Integer_Address, Record_Device);
  type Record_Binding(Kind : Enumerated_Kind := Keyboard_Key_Kind) is record
      Player : Integer_4_Positive := 1;
      Combo  : Integer_4_Natural  := NO_COMBO;
      case Kind is
        when Keyboard_Key_Kind | Mouse_Key_Kind | Xbox_Key_Kind | Playstation_Key_Kind | Generic_Key_Kind =>
          State : Record_State := (others => <>);
          case Kind is
            when Keyboard_Key_Kind    => Keyboard_Key    : Enumerated_Keyboard_Key    := Enumerated_Keyboard_Key'first;
            when Mouse_Key_Kind       => Mouse_Key       : Enumerated_Mouse_Key       := Enumerated_Mouse_Key'first;
            when Xbox_Key_Kind        => Xbox_Key        : Enumerated_Xbox_Key        := Enumerated_Xbox_Key'first;
            when Playstation_Key_Kind => Playstation_Key : Enumerated_Playstation_Key := Enumerated_Playstation_Key'first;
            when Generic_Key_Kind     => Key_Number      : Integer_4_Positive         := 1;
                                         Key_Device      : String_2_Unbounded         := NULL_STRING_2_UNBOUNDED;
            when others => null;
          end case;
        when Playstation_Stick_Kind | Xbox_Stick_Kind | Mouse_Cursor_Kind | Generic_Stick_Kind =>
          Location : Record_Location := (others => <>);
          case Kind is
            when Playstation_Stick_Kind | Xbox_Stick_Kind => Stick        : Enumerated_Stick   := Enumerated_Stick'first;
            when Generic_Stick_kind                       => Stick_Number : Integer_4_Positive := 1;
                                                             Stick_Device : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
            when others => null;
          end case;
        when Playstation_Trigger_Kind | Xbox_Trigger_Kind | Generic_Trigger_Kind =>
          Position : Float_4_Percent    := 0.0;
          case Kind is
            when Playstation_Trigger_Kind | Xbox_Trigger_Kind => Trigger        : Enumerated_Trigger := Enumerated_Trigger'first;
            when Generic_Trigger_kind                         => Trigger_Number : Integer_4_Positive := 1;
                                                                 Trigger_Device : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
            when others => null;
          end case;
        when Character_Kind =>
          Keyboard_Keys : Array_Keyboard_Keys := (others => (others => <>));
          Input         : Character_2         := NULL_CHARACTER_2;
      end case;
    end record;
  package Vector_Bindings is new Vectors(Record_Binding);
  generic
    Name : String_2;
    with procedure Trip(Binding : in Record_Binding);
  package Impulse is
      Bindings : aliased Vector_Bindings.Protected_Vector;
      procedure Enable;
      procedure Disable;
    private
      LOWER_NAME : constant String_2 := To_Lower(Name);
      type Record_Controller is new Ada.Finalization.Controlled with null record;
      procedure Not_A_Formal_Subprogram (Binding : in Record_Binding) renames Trip;
      overriding procedure Initialize   (Controller : in out Record_Controller);
      overriding procedure Finalize     (Controller : in out Record_Controller);
      Controller : Record_Controller;
    end Impulse;
  procedure Test;
  procedure Handle        (Input  : in String_2);
  procedure Set_Device    (Player : in Integer_4_Positive := 1; Identifier : in Integer_Address);
  procedure Set_Vibration (Player : in Integer_4_Positive := 1; Frequency_High, Frequency_Low : in Float_4_Percent);
  function Get_Device     (Identifier : in Integer_Address) return Record_Device;
  function Get_Devices    return Map_Devices.Unsafe.Map;
  function Get_Players    return Map_Players.Unsafe.Map;
private
  NULL_RECORD_LOCATION               : constant Record_Location := (others => 0);
  COMMAND_BIND                       : constant String_2        := "bind";
  COMMAND_UNBIND                     : constant String_2        := "unbind";
  DURATION_TO_WAIT_BEFORE_KEY_REPEAT : constant Duration        := 0.1;
  DURATION_BETWEEN_KEY_REPEAT        : constant Duration        := 0.05;
  DURATION_TO_WAIT_BEFORE_POLLING    : constant Duration        := 0.002;
  type Not_Null_Access_Vector_Bindings is not null access all Vector_Bindings.Protected_Vector;
  type Not_Null_Access_Procedure_Trip is not null access procedure(Binding : in Record_Binding);
  type Record_Impulse is record
      Trip       : Not_Null_Access_Procedure_Trip;
      Bindings   : Not_Null_Access_Vector_Bindings;
      Is_Garbage : Boolean := False;
      Is_Enabled : Boolean := True;
    end record;
  procedure Run;
  procedure Handle_Stick           (Identifier : in Integer_Address; Stick    : in Enumerated_Stick;   Location   : in Record_Location);
  procedure Handle_Generic_Stick   (Identifier : in Integer_Address; Stick    : in Integer_4_Positive; Location   : in Record_Location);
  procedure Handle_Trigger         (Identifier : in Integer_Address; Trigger  : in Enumerated_Trigger; Position   : in Float_4_Percent);
  procedure Handle_Generic_Trigger (Identifier : in Integer_Address; Trigger  : in Integer_4_Positive; Position   : in Float_4_Percent);
  procedure Handle_Key             (Identifier : in Integer_Address; Key      : in Enumerated_Key;     Is_Pressed : in Boolean);
  procedure Handle_Generic_Key     (Identifier : in Integer_Address; Key      : in Integer_4_Positive; Is_Pressed : in Boolean);
  procedure Handle_Mouse           (Identifier : in Integer_Address; Location : in Record_Location);
  procedure Add_Device             (Identifier : in Integer_Address; Device   : in Record_Device);
  procedure Remove_Device          (Identifier : in Integer_Address);
  function Has_ELement             (Identifier : in Integer_Address) return Boolean;
  generic
    with procedure Handle_Stick           (Identifier : in Integer_Address; Stick    : in Enumerated_Stick;   Location   : in Record_Location);
    with procedure Handle_Generic_Stick   (Identifier : in Integer_Address; Stick    : in Integer_4_Positive; Location   : in Record_Location);
    with procedure Handle_Trigger         (Identifier : in Integer_Address; Trigger  : in Enumerated_Trigger; Position   : in Float_4_Percent);
    with procedure Handle_Generic_Trigger (Identifier : in Integer_Address; Trigger  : in Integer_4_Positive; Position   : in Float_4_Percent);
    with procedure Handle_Key             (Identifier : in Integer_Address; Key      : in Enumerated_Key;     Is_Pressed : in Boolean);
    with procedure Handle_Generic_Key     (Identifier : in Integer_Address; Key      : in Integer_4_Positive; Is_Pressed : in Boolean);
    with procedure Handle_Mouse           (Identifier : in Integer_Address; Location : in Record_Location);
    with procedure Add_Device             (Identifier : in Integer_Address; Device   : in Record_Device);
    with procedure Remove_Device          (Identifier : in Integer_Address);
    with function Get_Device              (Identifier : in Integer_Address) return Record_Device;
    with function Has_ELement             (Identifier : in Integer_Address) return Boolean;
    --with Function  Get_Devices    (
  package Import is
      procedure Initialize;
      procedure Handle_Events;
      procedure Update_Devices;
      procedure Set_Vibration(Identifier : in Integer_Address; Frequency_High, Frequency_Low : in Float_4_Percent);
      function Lookup_Character(Keyboard : in Array_Keyboard_Keys) return Character_2;
    end Import;
  package Map_Impulses is new Hashed_Maps(Record_Impulse);
  package Task_Main is new Tasks(Run);
  Main_Task : Task_Main.Protected_Task;
  Impulses  : Map_Impulses.Protected_Map;
  Devices   : Map_Devices.Protected_Map;
  Players   : Map_Players.Protected_Map;
end Neo.System.Input;
