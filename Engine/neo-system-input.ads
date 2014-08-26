with Ada.Calendar;                 use Ada.Calendar;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Finalization;
package Neo.System.Input is
  Invalid_Device_Identifier : Exception;
  Invalid_Trigger_Index     : Exception;
  Invalid_Button_Index      : Exception;
  Invalid_Stick_Index       : Exception;
  Impulse_Out_Of_Scope      : Exception;
  NO_COMBO : constant Integer_4_Natural := 0;
  type Enumerated_Device  is  (Keyboard_Device, Mouse_Device, Xbox_Device, Playstation_Device);
  type Enumerated_Trigger is  (Left_Trigger,               Right_Trigger);
  type Enumerated_Stick   is  (Left_Stick,                 Right_Stick);
  type Enumerated_Kind    is  (Keyboard_Key_Kind,          Mouse_Key_Kind,             Mouse_Cursor_Kind,
    Xbox_Key_Kind,             Xbox_Stick_Kind,            Xbox_Trigger_Kind,          Playstation_Key_Kind,
    Playstation_Stick_Kind,    Playstation_Trigger_Kind,   Text_Kind);
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
  subtype Enumerated_Playstation_Key is Enumerated_Key range Back_Key..Triangle_Key;
  subtype Enumerated_Keyboard_Key    is Enumerated_Key range Null_Key..PA1_Key;
  subtype Enumerated_Xbox_Key        is Enumerated_Key range Y_Key..Right_Stick_Key;
  subtype Enumerated_Mouse_Key       is Enumerated_Key range Horizontal_Wheel_Left_Key..Middle_Mouse_Key;
  type Record_Location is record
      X : Integer_8_Signed := 0;
      Y : Integer_8_Signed := 0;
    end record;
  type Record_Axis is record
      X : Float_4_Range := 0.0;
      Y : Float_4_Range := 0.0;
    end record;
  type Record_State is record
      Is_Pressed : Boolean := False;
      Last       : Time    := INITIAL_TIME;
    end record;
  type Array_Playstation_Keys is array(Enumerated_Playstation_Key) of Record_State;
  type Array_Keyboard_Keys    is array(Enumerated_Keyboard_Key)    of Record_State;
  type Array_Xbox_Keys        is array(Enumerated_Xbox_Key)        of Record_State;
  type Array_Mouse_Keys       is array(Enumerated_Mouse_Key)       of Record_State;
  type Array_Sticks           is array(Enumerated_Stick)           of Record_Axis;
  type Array_Triggers         is array(Enumerated_Trigger)         of Float_4_Percent;
  type Record_Player is record
      Playstation_Triggers : Array_Triggers         := (others => 0.0);
      Playstation_Sticks   : Array_Sticks           := (others => (others => <>));
      Playstation_Keys     : Array_Playstation_Keys := (others => (others => <>));
      Xbox_Triggers        : Array_Triggers         := (others => 0.0);
      Xbox_Sticks          : Array_Sticks           := (others => (others => <>));
      Xbox_Keys            : Array_Xbox_Keys        := (others => (others => <>));
      Mouse_Keys           : Array_Mouse_Keys       := (others => (others => <>));
      Mouse_Cursor         : Record_Location        := (others => <>);
      Keyboard_Keys        : Array_Keyboard_Keys    := (others => <>);
      Text                 : String_2_Unbounded     := NULL_STRING_2_UNBOUNDED;
    end record;
  package Ordered_Map_Record_Player is new Ordered_Maps(Integer_4_Positive, Record_Player);
  type Record_Device(Kind : Enumerated_Device := Mouse_Device) is record
      Player : Integer_4_Positive := 1;
      Text   : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      case Kind is
        when Xbox_Device | Playstation_Device =>
          Triggers : Array_Triggers := (others => 0.0);
          Sticks   : Array_Sticks   := (others => (others => <>));
          case Kind is
            when Xbox_Device        => Xbox_Keys        : Array_Xbox_Keys        := (others => (others => <>));
            when Playstation_Device => Playstation_Keys : Array_Playstation_Keys := (others => (others => <>));
            when others => null;
          end case;
        when Keyboard_Device => Keyboard_Keys : Array_Keyboard_Keys := (others => <>);
        when Mouse_Device =>
          Mouse_Keys   : Array_Mouse_Keys := (others => (others => <>));
          Mouse_Cursor : Record_Location  := (others => <>);
      end case;
    end record;
  package Ordered_Map_Record_Device is new Ordered_Maps(Integer_Address, Record_Device);
  package Vector_Record_Device is new Vectors(Record_Device);
  type Record_Binding(Kind : Enumerated_Kind := Keyboard_Key_Kind) is record
      Player : Integer_4_Positive := 1;
      Combo  : Integer_4_Natural  := NO_COMBO;
      case Kind is
        when Keyboard_Key_Kind | Mouse_Key_Kind | Xbox_Key_Kind | Playstation_Key_Kind =>
          State : Record_State := (others => <>);
          case Kind is
            when Playstation_Key_Kind => Playstation_Key : Enumerated_Playstation_Key := Enumerated_Playstation_Key'first;
            when Keyboard_Key_Kind    => Keyboard_Key    : Enumerated_Keyboard_Key    := Enumerated_Keyboard_Key'first;
            when Mouse_Key_Kind       => Mouse_Key       : Enumerated_Mouse_Key       := Enumerated_Mouse_Key'first;
            when Xbox_Key_Kind        => Xbox_Key        : Enumerated_Xbox_Key        := Enumerated_Xbox_Key'first;
            when others => null;
          end case;
        when Playstation_Stick_Kind | Xbox_Stick_Kind =>
          Axis : Record_Axis := (others => <>);
          case Kind is
            when Playstation_Stick_Kind | Xbox_Stick_Kind => Stick : Enumerated_Stick := Enumerated_Stick'first;
            when others => null;
          end case;
        when Mouse_Cursor_Kind =>
          Location : Record_Location := (others => <>);
        when Playstation_Trigger_Kind | Xbox_Trigger_Kind =>
          Position : Float_4_Percent    := 0.0;
          Trigger  : Enumerated_Trigger := Enumerated_Trigger'first;
        when Text_Kind => Text : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      end case;
    end record;
  package Vector_Record_Binding is new Vectors(Record_Binding);
  generic -- Do not let Impulses go out of scope while Neo.System.Is_Running!
    Name : String_2;
    with procedure Trip(Binding : in Record_Binding);
    Is_User_Settable : Boolean := False;
  package Impulse is
      Bindings : aliased Vector_Record_Binding.Protected_Vector;
      procedure Enable;
      procedure Disable;
    private
      LOWER_NAME : constant String_2 := To_Lower(Name);
      type Record_Controller is new Ada.Finalization.Controlled with null record;
      procedure Not_A_Formal_Subprogram (Binding : in Record_Binding) renames Trip;
      overriding procedure Initialize (Controller : in out Record_Controller);
      overriding procedure Finalize   (Controller : in out Record_Controller);
      Controller : Record_Controller;
    end Impulse;
  procedure Perform_Unbind (Parameters : in Vector_String_2_Unbounded.Vector);
  procedure Perform_Bind   (Parameters : in Vector_String_2_Unbounded.Vector);
  procedure Inject         (Binding : in Record_Binding); -- Injections affect all players, not task safe
  procedure Set_Vibration  (Player : in Integer_4_Positive := 1; Frequency_High, Frequency_Low : in Float_4_Percent);
  procedure Set_Device     (Identifier : in Integer_Address; Player : in Integer_4_Positive := 1);
  function Get_Device      (Identifier : in Integer_Address) return Record_Device;
  function Get_Devices                                       return Ordered_Map_Record_Device.Unsafe.Map;
  function Get_Cursor                                        return Record_Location;
  function Playstation     (Trigger : in Enumerated_Trigger;         Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Playstation     (Stick   : in Enumerated_Stick;           Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Playstation     (Key     : in Enumerated_Playstation_Key; Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Keyboard        (Key     : in Enumerated_Keyboard_Key;    Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Xbox            (Trigger : in Enumerated_Trigger;         Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Xbox            (Stick   : in Enumerated_Stick;           Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Xbox            (Key     : in Enumerated_Xbox_Key;        Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Mouse           (Key     : in Enumerated_Mouse_Key;       Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  function Mouse                                                   (Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding;
  package Bind   is new Action("bind",   Perform_Bind);
  package Unbind is new Action("unbind", Perform_Unbind);
private
  COMMAND_UNBIND                  : constant String_2 := "unbind";
  COMMAND_BIND                    : constant String_2 := "bind";
  COMMAND_HELP                    : constant String_2 := "Below is an example call, player# and device are optional";
  COMMAND_EXAMPLE                 : constant String_2 := "player# impulse device value";
  NO_SUCH_IMPULSE                 : constant String_2 := "No such impulse!";
  NO_SUCH_DEVICE                  : constant String_2 := "No such device!";
  NO_SUCH_VALUE                   : constant String_2 := "No such value!";
  NAME_POSTFIX                    : constant String_2 := "Input";
  DURATION_FOR_MUTEX_WAIT         : constant Duration := 0.0001;
  DURATION_TO_WAIT_BEFORE_POLLING : constant Duration := 0.002;
  type Not_Null_Access_Vector_Record_Binding is not null access all Vector_Record_Binding.Protected_Vector;
  type Not_Null_Access_Procedure_Trip is not null access procedure(Binding : in Record_Binding);
  type Record_Impulse is record
      Trip       : Not_Null_Access_Procedure_Trip;
      Bindings   : Not_Null_Access_Vector_Record_Binding;
      Is_Enabled : Boolean := True;
    end record;
  procedure Run;
  procedure Inject_Trigger (Identifier : in Integer_Address; Trigger  : in Enumerated_Trigger; Position   : in Float_4_Percent);
  procedure Inject_Stick   (Identifier : in Integer_Address; Stick    : in Enumerated_Stick;   Axis       : in Record_Axis);
  procedure Inject_Key     (Identifier : in Integer_Address; Key      : in Enumerated_Key;     Is_Pressed : in Boolean);
  procedure Inject_Text    (Identifier : in Integer_Address; Text     : in String_2_Unbounded);
  procedure Add_Device     (Identifier : in Integer_Address; Device   : in Record_Device);
  procedure Inject_Mouse   (Identifier : in Integer_Address; Location : in Record_Location);
  procedure Remove_Device  (Identifier : in Integer_Address);
  function Has_Device      (Identifier : in Integer_Address) return Boolean;
  package Import is
      function Get_Cursor                       return Record_Location;
      function Does_Support_Playstation_Devices return Boolean;
      function Does_SUpport_Xbox_Devices        return Boolean;
      function Update                           return Boolean;
      procedure Initialize;
      procedure Finalize;
      procedure Set_Vibration(Identifier : in Integer_Address; Frequency_High, Frequency_Low : in Float_4_Percent);
    end Import;
  package Hashed_Map_Record_Impulse is new Hashed_Maps(Record_Impulse);
  package Task_Main is new Tasks(Run);
  Status    : Protected_Status;
  Injection : Record_Player;
  Main_Task : Task_Main.Protected_Task;
  Impulses  : Hashed_Map_Record_Impulse.Protected_Map;
  Devices   : Ordered_Map_Record_Device.Protected_Map;
  Players   : Ordered_Map_Record_Player.Protected_Map;
end Neo.System.Input;
