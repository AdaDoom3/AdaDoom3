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
  Windows,
  Ada.Characters.Handling,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO,
  Interfaces,
  Interfaces.C,
  Systemic,
  Foundational.Data_Types;
use
  System,
  Windows,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Ada.Wide_Text_IO,
  Interfaces,
  Interfaces.C,
  Systemic,
  Foundational.Data_Types;
procedure Systemic_Input_Implementation
  is
  ---------------
  -- Constants --
  ---------------
    RAW_INPUT_WINDOW_STYLE : constant Integer_4_Unsigned_C :=(
      STYLE_NO_ACTIVATE);
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
      Null_Key,              Exsel_Key,             Clear_Key);
    type Enumerated_Button
      is(
      Y_Button,                   B_Button,             A_Button,                   X_Button,
      Select_Button,              System_Button,        Left_Button,                Start_Button,
      Right_Button,               Left_Stick_Button,    Right_Stick_Button,         Circle_Button,
      Square_Button,              Triangle_Button,      Generic_Button,             Hat_North,
      Hat_North_East,             Hat_East,             Hat_South_East,             Hat_South,
      Hat_South_West,             Hat_West,             Hat_North_West,             Directional_Pad_North,
      Directional_Pad_North_East, Directional_Pad_East, Directional_Pad_South_East, Directional_Pad_South,
      Directional_Pad_South_West, Directional_Pad_West, Directional_Pad_North_West);
    type Enumerated_Stick
      is(
      Left_Stick, Right_Stick, Generic_Stick);
    type Enumerated_Pedal
      is(
      Left_Trigger, Right_Trigger, Generic_Pedal);
    type Enumerated_Rotator
      is(
      Generic_Rotator);
  -------------
  -- Records --
  -------------
    type Record_Button
      is record
        Button : Enumerated_Button;
        Is_Down : Boolean;
      end record;
    type Record_Stick
      is record
        Stick    : Enumerated_Stick;
        Forward  : Float_Percent;
        Rotation : Float_Degree;
      end record;
    type Record_Pedal
      is record
        Pedal : Enumerated_Pedal;
        Down  : Float_Degree;
      end record;
    type Record_Rotator
      is record
        Rotator  : Enumerated_Rotator;
        Rotation : Float_Degree;
      end record;
  ------------
  -- Arrays --
  ------------
    type Array_Enumerated_Key_Range_Boolean
      is array (Enumerated_Key'Range)
      of Boolean;
    type Array_Record_Button
      is array (Positive range <>)
      of Record_Button;
    type Array_Record_Stick
      is array (Positive range <>)
      of Record_Stick;
    type Array_Record_Pedal
      is array (Positive range <>)
      of Record_Pedal;
    type Array_Record_Rotator
      is array (Positive range <>)
      of Record_Rotator;
    type Access_Array_Record_Button
      is access all Array_Record_Button;
    type Access_Array_Record_Stick
      is access all Array_Record_Stick;
    type Access_Array_Record_Pedal
      is access all Array_Record_Pedal;
    type Access_Array_Record_Rotator
      is access all Array_Record_Rotator;
    type Record_Device
      is record
        Player       : Integer_4_Positive;
        Name         : Access_String_2;
        Manufacturer : Access_String_2;
        Handle       : Address;
        Buttons      : Access_Array_Record_Button;
        Sticks       : Access_Array_Record_Stick;
        Pedals       : Access_Array_Record_Pedal;
        Rotators     : Access_Array_Record_Rotator;
      end record;
    type Record_Keyboard
      is record
        Player : Integer_4_Positive;
        Handle : Address;
        Keys   : Array_Enumerated_Key_Range_Boolean;
      end record;
    type Record_Mouse
      is record
        Player : Integer_4_Positive;
        Handle : Address;
      end record;
  --------------------
  -- Type_Constants --
  --------------------
    MAP_KEY:
      constant array (SUBEVENT_KEY_LEFT_MOUSE..SUBEVENT_KEY_CLEAR) of Enumerated_Key :=(
      Left_Mouse_Key,        Right_Mouse_Key,       Cancel_Key,            Middle_Mouse_Key,
      Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key, Null_Key,              Backspace_Key,
      Tab_Key,               Null_Key,              Null_Key,              Clear_Key,
      Enter_Key,             Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Pause_Break_Key,       Capital_Lock_Key, 
      Kana_Key,              Null_Key,              Junja_Key,             Final_Key,
      Hanja_Key,             Kanji_Key,             Escape_Key,            Convert_Key,
      No_Convert_Key,        Accept_Key,            Mode_Change_Key,       Space_Key, 
      Page_Up_Key,           Page_Down_Key,         End_Key,               Home_Key,
      Left_Arrow_Key,        Up_Arrow_Key,          Right_Arrow_Key,       Down_Arrow_Key,
      Select_Key,            Print_Key,             Execute_Key,           Print_Screen_Key,
      Insert_Key,            Delete_Key,            Help_Key,              Zero_Key,
      One_Key,               Two_Key,               Three_Key,             Four_Key,
      Five_Key,              Six_Key,               Seven_Key,             Eight_Key,
      Nine_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      A_Key,                 B_Key,                 C_Key,                 D_Key,
      E_Key,                 F_Key,                 G_Key,                 H_Key,
      I_Key,                 J_Key,                 K_Key,                 L_Key,
      M_Key,                 N_Key,                 O_Key,                 P_Key,
      Q_Key,                 R_Key,                 S_Key,                 T_Key,
      U_Key,                 V_Key,                 W_Key,                 X_Key,
      Y_Key,                 Z_Key,                 Left_Windows_Key,      Right_Windows_Key,
      Application_Menu_Key,  Null_Key,              System_Sleep_Key,      Pad_Zero_Key, 
      Pad_One_Key,           Pad_Two_Key,           Pad_Three_Key,         Pad_Four_Key,
      Pad_Five_Key,          Pad_Six_Key,           Pad_Seven_Key,         Pad_Eight_Key,
      Pad_Nine_Key,          Pad_Star_Key,          Pad_Plus_Key,          Separator_Key,
      Pad_Dash_Key,          Pad_Period_Key,        Pad_Slash_Key,         F1_Key,
      F2_Key,                F3_Key,                F4_Key,                F5_Key,
      F6_Key,                F7_Key,                F8_Key,                F9_Key,
      F10_Key,               F11_Key,               F12_Key,               F13_Key,
      F14_Key,               F15_Key,               F16_Key,               F17_Key,
      F18_Key,               F19_Key,               F20_Key,               F21_Key,
      F22_Key,               F23_Key,               F24_Key,               Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Number_Lock_Key,
      Scroll_Lock_Key,       OEM_Specific_1_Key,    OEM_Specific_2_Key,    OEM_Specific_3_Key,
      OEM_Specific_4_Key,    OEM_Specific_5_Key,    Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Left_Shift_Key,
      Right_Shift_Key,       Left_Control_Key,      Right_Control_Key,     Left_Alternative_Key,
      Right_Alternative_Key, Web_Backward_Key,      Web_Forward_Key,       Web_Refresh_Key,
      Web_Stop_Key,          Web_Search_Key,        Web_Favorites_Key,     Web_Home_Key,
      Volume_Mute_Key,       Volume_Down_Key,       Volume_Up_Key,         Next_Track_Key,
      Previous_Track_Key,    Stop_Track_Key,        Play_Pause_Track_Key,  Web_Mail_Key,
      Media_Select_Key,      Application_1_Key,     Application_2_Key,     Null_Key,
      Null_Key,              Semicolon_Key,         Equals_Key,            Comma_Key,
      Dash_Key,              Period_Key,            Slash_Key,             Grave_Accent_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Left_Bracket_Key,      Backslash_Key,
      Right_Bracket_Key,     Apostrophe_Key,        OEM_Specific_7_Key,    Null_Key,
      OEM_Specific_8_Key,    OEM_102_Key,           OEM_Specific_9_Key,    OEM_Specific_10_Key,
      Process_Key,           OEM_Specific_11_Key,   Null_Key,              Null_Key,
      OEM_Specific_12_Key,   OEM_Specific_13_Key,   OEM_Specific_14_Key,   OEM_Specific_15_Key,
      OEM_Specific_16_Key,   OEM_Specific_17_Key,   OEM_Specific_18_Key,   OEM_Specific_19_Key,
      OEM_Specific_20_Key,   OEM_Specific_21_Key,   OEM_Specific_22_Key,   OEM_Specific_23_Key,
      OEM_Specific_24_Key,   Attention_Key,         Clear_Selection_Key,   Exsel_Key,
      Erase_EOF_Key,         Play_Key,              Zoom_Key,              Null_Key,
      PA1_Key,               Clear_Key);
    NULL_RECORD_BUTTON:
      constant Record_Button :=(
      Button  => Generic_Button,
      Is_Down => False);
    NULL_RECORD_STICK:
      constant Record_Stick :=(
      Stick    => Generic_Stick,
      Forward  => 0.0,
      Rotation => 0.0);
    NULL_RECORD_PEDAL:
      constant Record_Pedal :=(
      Pedal => Generic_Pedal,
      Down  => 0.0);
    -- NULL_RECORD_WHEEL:
    --   constant Record_Wheel :=(
    --   Wheel    => Generic_Rotator,
    --   Rotation => 0.0);
  --------------
  -- Get_List --
  --------------
    function Get_List
      return Array_Record_Device_List_Element
      is
      Number_Of_Devices : Integer_4_Unsigned_C := 0;
      begin
        if
        Get_Device_List(
          List  => NULL_ADDRESS,
          Count => Number_Of_Devices'Address,
          Size  => Record_Device_List_Element'Size / 8) = -1
        then
          raise System_Call_Failure;
        end if;
        if Number_Of_Devices = 0 then
          raise No_Input_Devices;
        end if;
        -----------
        Fetch_List:
        -----------
          declare
          List : Array_Record_Device_List_Element(1..Integer_4_Signed(Number_Of_Devices)) :=(
            others => NULL_RECORD_DEVICE_LIST_ELEMENT);
          begin
            if
            Get_Device_List(
              List  => List(1)'Address,
              Count => Number_Of_Devices'Address,
              Size  => Record_Device_List_Element'Size / 8) = -1
            then
              raise System_Call_Failure;
            end if;
            return List;
          end Fetch_List;
      end Get_List;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Class_Name : in String_2)
      is
      ---------------------------------
      function Get_Beginning_Of_String(
      ---------------------------------
        Item : in String_2)
        return Access_String_2
        is
        Dynamic_Item : Access_String_2 := null;
        begin
          for I in Item'Range loop
            if Item(I) = NULL_CHARACTER_2 then
              if I = Item'First then
                raise System_Call_Failure;
              end if;
              Dynamic_Item := new String_2(Item'First..I - 1);
              Dynamic_Item.All := Item(Item'First..I - 1);
              return Dynamic_Item;
            end if;
          end loop;
          raise System_Call_Failure;
        end Get_Beginning_Of_String;
      -------------------------
      function Window_Callback(
      -------------------------
        Window        : Address;
        Message       : Integer_4_Unsigned_C;
        Data_Unsigned : Integer_4_Unsigned_C;
        Data_Signed   : Integer_4_Signed_C)
        return Integer_4_Signed_C;
        pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(
        Window        : Address;
        Message       : Integer_4_Unsigned_C;
        Data_Unsigned : Integer_4_Unsigned_C;
        Data_Signed   : Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        Size   : Integer_4_Unsigned_C           := 0;
        Device : Access_Record_Raw_Input_Device := To_Access_Record_Raw_Input_Device(Data_Signed);
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_RAW_INPUT =>
              if
              Get_Raw_Input_Data(
                Raw_Input   => To_Address(Data_Signed),
                Command     => GET_RAW_INPUT_DEVICE_DATA,
                Data        => NULL_ADDRESS,
                Size        => Size'Address,
                Header_Size => Record_Raw_Input_Header'Size / 8) /= 0
              then
                raise System_Call_Failure;
              end if;
              if Size < Record_RAWINPUT'Size / 8 then
                Put_Line("unexpected packet");
                return;
              end if;
              -------------
              Handle_Input:
              -------------
                declare
                Data : array (1..Size) of Integer_1_Unsigned_C := (others => 0);
                begin
                  if 
                  Get_Raw_Input_Data(
                    Raw_Input   => To_Address(Data_Signed),
                    Command     => GET_RAW_INPUT_DEVICE_DATA,
                    Data        => Data'Address,
                    Size        => Size'Address,
                    Header_Size => Record_Raw_Input_Header'Size / 8) /= Size
                  then
                    raise System_Call_Failure;
                  end if;
                  case Raw_Input.Header.Kind is
                    when KIND_IS_RAW_KEYBOARD =>
                      null;
                      -- if Device. = EVENT_KEY_PRESS then
                      --   null;
                      -- else
                      --   null;
                      -- end if;
                    when KIND_IS_RAW_MOUSE =>
                      if Mouse.US_Flags and MOUSE_MOVE_ABSOLUTE then -- How do we get the min and max values for absmotion?
                        Mouse.Last_X;
                        Mouse.Last_Y;
                      elsif Raw.Mouse.usFlags and MOUSE_MOVE_RELATIVE then
                        if Raw.Mouse.Last_X /= 0 then
                          Event.Item  := 0;
                          Event.Value := Raw.Mouse.Last_X;
                        end if;
                        if Raw.Mouse.Last_Y /= 0 then
                          Event.Value := Raw.Mouse.Last_Y;
                        end if;
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_LEFT_DOWN then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_LEFT_UP then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_RIGHT_DOWN then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_RIGHT_UP then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_MIDDLE_DOWN then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_MIDDLE_UP then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_EXTRA_1_DOWN then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_EXTRA_1_UP then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_EXTRA_2_DOWN then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_EXTRA_2_UP then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_WHEEL_VERTICAL then
                        Put_Line("Button");
                      end if;
                      if Mouse.Button_Flags and SUBEVENT_RAW_MOUSE_WHEEL_HORIZONTAL then
                        Put_Line("Button");
                      end if;
                    when KIND_IS_RAW_HUMAN_INTERFACE_DEVICE =>
                      Count := Button_Capabilities.Usage_Maximum - Button_Capabilities.Usage_Minimum + 1;
                      --------
                      Another:
                      --------
                        declare
                        Usages : array (1..Count) of Integer_2_Unsigned_C := (others => 0);
                        Axis_X : Integer_8_Unsigned_C                     := 0;
                        Axis_Y : Integer_8_Unsigned_C                     := 0;
                        Axis_Z : Integer_8_Unsigned_C                     := 0;
                        Axis_R : Integer_8_Unsigned_C                     := 0;
                        Hat    : Integer_8_Unsigned_C                     := 0;
                        begin 
                          if
                          Get_Device_Usages(
                            Kind            => KIND_HUMAN_INTERFACE_DEVICE_INPUT,
                            Page            => Button_Capabilities.Page,
                            Link_Collection => 0,
                            Usage           => Usages'Address, 
                            Value           => Count'Address,
                            Preparsed_Data  => Preparsed_Data,
                            Report          => Raw_Input.Data.hid.Raw_Data,
                            Report_Length   => Raw_Input.Data.hid.SizeHid) /= SUCCESSFUL_HUMAN_INTEFACE_DEVICE_OPERATION
                          then
                            raise System_Call_Failure;
                          end if;
                          Button_States := (others => 0);
                          for I in 1..Usage_Length loop
                            Button_States(Usage(I) - Button_Capabilities.Usage_Minimum) := True;
                          end loop;
                          for I in 1..Capabilities.Number_Of_Input_Value_Capabilities loop
                            if
                            Get_Human_Interface_Device_Usage_Value(
                              Kind            => KIND_HUMAN_INTERFACE_DEVICE_INPUT,
                              Page            => Value_Capabilities(I).Page,
                              Link_Collection => 0, 
                              Usage           => Value_Capabilities(I).Usage_Minimum,
                              Value           => Value'Address,
                              Preparsed_Data  => Preparsed_Data,
                              Report          => Data.hid.bRawData'Address,
                              Report_Length   => pRawInput.data.hid.dwSizeHid) /= SUCCESSFUL_HUMAN_INTEFACE_DEVICE
                            then
                              raise System_Call_Failure;
                            end if;
                            case Value_Capabilities(I).Usage_Minimum is
                              when 16#30# =>
                                Axis_X := Integer_8_Unsigned_C(Value) - 128;
                              when 16#31# =>
                                Axis_Y := Integer_8_Unsigned_C(Value) - 128;
                              when 16#32# =>
                                Axis_Z := Integer_8_Unsigned_C(Value) - 128;
                              when 16#35# =>
                                Axis_R := Integer_8_Unsigned_C(Value) - 128;
                              when 16#39# =>
                                Hat := value;
                              when others =>
                                null;
                            end case;
                          end loop;
                        end Another;
                    when others =>
                      raise System_Call_Failure;
                  end case;
                end Handle_Input;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Identifier           : String_2                         := Identifier & NULL_CHARACTER_2;
      Buffer               : String_2(1..512)                 := (others => ' ');
      List                 : Array_Record_Device_List_Element := Get_List;
      Capabilities         : Record_Device_Capabilities       := NULL_RECORD_DEVICE_CAPABILITIES;
      File                 : Address                          := NULL_ADDRESS; 
      Window               : Address                          := NULL_ADDRESS;
      Number_Of_Characters : Integer_4_Unsigned_C             := 0;
      Size                 : Integer_4_Unsigned_C             := 0;
      Name_Start           : Integer_4_Signed                 := 0;
      Class:
        Record_Window_Class :=(
        Size       => Record_Window_Class'Size / 8,
        Style      => 0,
        Callback   => Window_Callback'Address,
        Extra_A    => 0,
        Extra_B    => 0,
        Instance   => Get_Current_Instance,
        Icon_Small => Load_Icon(Get_Current_Instance, GENERIC_ICON),
        Icon_Large => Load_Icon(Get_Current_Instance, GENERIC_ICON),
        Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
        Background => BRUSH_GRAY,
        Menu_Name  => null,
        Class_Name => To_Access_Constant_Character_2_C(Class_Name));
      Device_Setups:
        Array_Record_Device_Setup :=(
        1 =>(
          Page   => GENERIC_DESKTOP_CONTROL,
          Usage  => USE_RAW_KEYBOARD,
          Flags  => TAKE_INPUT_ON_NON_ACTIVE and IGNORE_LEGACY_MESSAGES,
          Target => Window),
        2 =>(
          Page   => GENERIC_DESKTOP_CONTROL,
          Usage  => USE_RAW_MOUSE,
          Flags  => TAKE_INPUT_ON_NON_ACTIVE and IGNORE_LEGACY_MESSAGES,
          Target => Window),
        3 =>(
          Page   => GENERIC_DESKTOP_CONTROL,
          Usage  => USE_RAW_JOYSTICK,
          Flags  => TAKE_INPUT_ON_NON_ACTIVE,
          Target => Window),
        4 =>(
          Page   => GENERIC_DESKTOP_CONTROL,
          Usage  => USE_RAW_GAMEPAD,
          Flags  => TAKE_INPUT_ON_NON_ACTIVE,
          Target => Window));
      begin
        for I in 1..List'Length loop
          if 
          Get_Raw_Input_Device_Information(
            Device  => Device_Handle,
            Command => GET_RAW_DEVICE_NAME,
            Data    => NULL_ADDRESS,
            Size    => Number_Of_Characters'Address) /= 0
          then
            raise System_Call_Failure;
          end if;
          if Number_Of_Characters < 2 then
            raise System_Call_Failure;
          end if;
          ---------------
          Get_Identifier:
          ---------------
            declare
            Identifier : String_2(1..Integer_4_Signed(Number_Of_Characters)) := (Others => NULL_CHARACTER_2);
            begin
              if 
              Get_Raw_Input_Device_Information(
                Device  => Device_Handle,
                Command => GET_RAW_DEVICE_NAME,
                Data    => Identifier(1)'Address,
                Size    => Number_Of_Characters'Address) < 0
              then
                raise System_Call_Failure;
              end if;
              if Name_Start = 0 then
                for I in Identifier'Range loop
                  if Identifier(I) /= '?' and Identifier(I) /= '\' then
                    Name_Start := I;
                    exit;
                  end if;
                  if I = Identifier'Last then
                    raise System_Call_Failure;
                  end if;
                end loop;
              elsif Name_Start >= Identifier'Last then
                raise System_Call_Failure;
              end if;
              if
              Identifier'Last > Name_Start + 3 and then(
              Identifier(Name_Start..Name_Start + 3) = "Root" or
              Identifier(Name_Start..Name_Start + 3) = "ROOT" or
              Identifier(Name_Start..Name_Start + 3) = "root")
              then
                case List(I).Kind is
                  when KIND_RAW_MOUSE =>
                    if Mouse = null then
                      Mouse := new Record_Keyboard;
                    else
                      Mouse.Next   := new Record_Mouse;
                      Mouse        := Mouse.Next;
                      Mouse.Handle := List(I).Handle;
                    end if;
                  when KIND_RAW_KEYBOARD => 
                    if Keyboard = null then
                      Keyboard := new Record_Keyboard;
                    else
                      Keyboard.Next   := new Record_Keyboard;
                      Keyboard        := Keyboard.Next;
                      Keyboard.Handle := List(I).Handle;
                    end if;
                  when KIND_RAW_HUMAN_INTERFACE_DEVICE =>
                    if Device = null then
                      Device := new Record_Device;
                    else
                      Device.Next   := new Record_Device;
                      Device        := Device.Next;
                      Device.Handle := List(I).Handle;
                    end if;
                    if
                    Get_Raw_Input_Device_Information(
                      Device  => Device.Handle, 
                      Command => GET_RAW_DEVICE_PREPARSED_DATA,
                      Data    => NULL_ADDRESS,
                      Size    => Size'Address) /= 0
                    then 
                      raise System_Call_Failure;
                    end if;
                    ---------------------
                    Get_Device_Specifics:
                    ---------------------
                      declare
                      Data : Array_Integer_1_Unsigned_C(1..Integer_4_Signed(Size)) := (others => 0);
                      begin
                        File :=
                          Create_File(
                            Name                 => Identifier(1)'Address,
                            Desired_Access       => GENERIC_READ or GENERIC_WRITE,
                            Share_Mode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                            Security_Attributes  => NULL_ADDRESS,
                            Creation_Desposition => OPEN_EXISTING,
                            Flags_And_Attributes => 0,
                            Template_File        => NULL_ADDRESS);
                        if File = NULL_ADDRESS or Data'Size < 8 then
                          raise System_Call_Failure;
                        end if;
                        if
                        Get_Device_Information(
                          Device  => Device.Handle, 
                          Command => GET_RAW_DEVICE_PREPARSED_DATA,
                          Data    => Data(1)'Address,
                          Size    => Size'Address) < 0
                        then 
                          raise System_Call_Failure;
                        end if;
                        if
                        Get_Device_Product(
                          File   => File,
                          Buffer => Buffer(1)'Address,
                          Size   => Buffer'Size / 8) /= FAILED
                        then
                          Device.Name := Get_Beginning_Of_String(Buffer);
                        end if;
                        if
                        Get_Device_Manufacturer(
                          File   => File,
                          Buffer => Buffer(1)'Address,
                          Size   => Buffer'Size / 8) /= FAILED
                        then
                          Device.Manufacturer := Get_Beginning_Of_String(Buffer);
                        end if;
                        if
                        Get_Device_Capabilities(
                          Preparsed_Data => Preparsed_Data,
                          Capabilities   => Capabilities'Address) /= SUCCESSFUL_DEVICE_OPERATION
                        then
                          raise System_Call_Failure;
                        end if;
                        ------------------------
                        Get_Button_Capabilities:
                        ------------------------
                          declare
                          Button_Capabilities:
                            array (1..Capabilities.Number_Of_Input_Button_Capabilities)
                            of Record_Device_Button_Capabilities :=(
                              others => NULL_RECORD_DEVICE_BUTTON_CAPABILITIES);
                          Count : Integer_4_Unsigned_C := Capabilities.Number_Of_Input_Button_Capabilities;
                          begin
                            if
                            Get_Device_Button_Capabilities(
                              Kind                => KIND_HUMAN_INTERFACE_DEVICE_INPUT,
                              Button_Capabilities => Button_Capabilities(1)'Address, 
                              Length              => Count'Address,
                              Perparsed_Data      => Preparsed_Data'Address) /= SUCCESSFUL_DEVICE_OPERATION
                            then
                              raise System_Call_Failure;
                            end if;
                          end Get_Button_Capabilities;
                      end Get_Device_Specifics;
                  when others =>
                    raise System_Call_Failure;
                end case;
              end if;
          end Get_Identifier;
        end loop;
        if Register_Class(Class'Address) = Integer_2_Unsigned_C(FAILED) then
          raise System_Call_Failure;
        end if;
        Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Name),
            Window_Name => To_String_2_C(Class_Name), 
            Style       => 0,
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        if Window = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if
        Register_Devices(
          Devices           => Device_Setups'Address,
          Number_Of_Devices => Register_Raw_Input_Devices'Length,
          Size              => Device_Setups'Size / 8) = FAILED
        then
          raise System_Call_Failure;
        end if;
        Put_Line(Identifier.All(Name_Start..Name_End));
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is 
      begin
        null;
        -- Free devices
        -- /* unregister WM_INPUT devices... */
        -- RAWINPUTDEVICE rid;
        -- ZeroMemory(&rid, sizeof (rid));
        -- rid.usUsagePage = 1; /* GenericDesktop page */
        -- rid.usUsage = 2; /* GeneralDestop Mouse usage. */
        -- rid.dwFlags |= RIDEV_REMOVE;
        -- pRegisterRawInputDevices(&rid, 1, sizeof (rid));
        -- cleanup_window();
        -- available_mice = 0;
        -- pDeleteCriticalSection(&mutex);
      end Finalize;
  ----------
  -- Main --
  ----------
  begin
    Initialize("Testit!");
  end Neo.System.;
