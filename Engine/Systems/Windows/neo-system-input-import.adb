with Ada.Finalization;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Interfaces.C;                 use Interfaces.C;
with Interfaces;                   use Interfaces;
with System;                       use System;
with Neo.Windows;                  use Neo.Windows;
separate (Neo.System.Input) package body Import is
  MAP_KEY : constant array(VIRTUAL_KEY_LEFT_MOUSE..VIRTUAL_KEY_CLEAR) of Enumerated_Key :=(
    Left_Mouse_Key,        Right_Mouse_Key,       Cancel_Key,            Middle_Mouse_Key,
    Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key, Null_Key,              Backspace_Key,
    Tab_Key,               Null_Key,              Null_Key,              Clear_Key,
    Enter_Key,             Null_Key,              Null_Key,              Shift_Key,
    Control_Key,           Alternative_Key,       Pause_Break_Key,       Capital_Lock_Key,
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
  Gamepad_States : aliased array(0..3) of Record_Gamepad := (others => <>);
  Name_Class     : aliased String_2_C                    := To_String_2_C(SPECIFICS.Name & " " & Localize(NAME_POSTFIX));
  Window         :         Address                       := NULL_ADDRESS;
  function Does_Support_Playstation_Devices return Boolean is begin return False; end Does_Support_Playstation_Devices;
  function Does_SUpport_Xbox_Devices        return Boolean is begin return True;  end Does_Support_Xbox_Devices;
  procedure Set_Vibration(Identifier : in Integer_Address; Frequency_High, Frequency_Low : in Float_4_Percent) is
    Vibration : aliased Record_Vibration := (
      Left_Motor_Speed  => Integer_2_Unsigned_C(Frequency_Low  / 100.0 * Float_4_Real(Integer_2_Unsigned_C'last)),
      Right_Motor_Speed => Integer_2_Unsigned_C(Frequency_High / 100.0 * Float_4_Real(Integer_2_Unsigned_C'last)));
    begin
      if Identifier in 0..3 then Assert(Set_XInput_State(Integer_4_Unsigned_C(Identifier), Vibration'unchecked_access) = 0); end if;
    end Set_Vibration;
  function Window_Callback(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned : in Integer_Address; Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Window_Callback);
  function Window_Callback(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned : in Integer_Address; Data_Signed : in Integer_Address) return Integer_Address is
    Identifier      :         Integer_Address      := 0;
    Player          :         Integer_4_Positive   := 1;
    Number_Of_Bytes : aliased Integer_4_Unsigned_C := 0;
    Header          : aliased Record_Device_Header := (others => <>);
    begin
      case Message is
        when EVENT_CLOSE => Post_Quit_Message(0); return Integer_Address(C_FALSE);
        when EVENT_INPUT =>
          Number_Of_Bytes := Record_Device_Header'object_size / Byte'object_size;
          Assert(Get_Device_Input_Data(
            Device      => To_Unchecked_Address(Data_Signed),
            Command     => GET_DEVICE_HEADER,
            Data        => Header'address,
            Size        => Number_Of_Bytes'address,
            Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Header'object_size / Byte'object_size);
          Identifier := To_Unchecked_Integer_Address(Header.Device);
          if not Devices.Has_Element(Identifier) then return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed); end if;
          case Header.Kind is
            when KIND_IS_KEYBOARD =>
              declare
              Keyboard   : aliased Record_Device_Keyboard := (others => <>);
              Is_Pressed :         Boolean                := False;
              begin
                Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Keyboard'object_size / Byte'object_size);
                Assert(Get_Device_Input_Data(
                  Device      => To_Unchecked_Address(Data_Signed),
                  Command     => GET_DEVICE_DATA,
                  Data        => Keyboard'address,
                  Size        => Number_Of_Bytes'address,
                  Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Keyboard'object_size / Byte'object_size);
                if Keyboard.Data.Key <= MAP_KEY'last and Keyboard.Data.Key >= MAP_KEY'first then
                  if Keyboard.Data.Message = EVENT_KEY_DOWN or Keyboard.Data.Message = EVENT_SYSTEM_KEY_DOWN then Is_Pressed := True; end if;
                  Inject_Key(Identifier, Is_Pressed => Is_Pressed, Key => (case MAP_KEY(Keyboard.Data.Key) is
                    when Shift_Key       => (if Keyboard.Data.Make_Code = KEY_MAKE_CODE_FOR_LEFT          then Left_Shift_Key       else Right_Shift_Key),
                    when Control_Key     => (if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Control_Key     else Right_Control_Key),
                    when Alternative_Key => (if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Alternative_Key else Right_Alternative_Key),
                    when others          => MAP_KEY(Keyboard.Data.Key)));
                end if;
              end;
            when KIND_IS_MOUSE =>
              declare
              Mouse : aliased Record_Device_Mouse := (others => <>);
              begin
                Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Mouse'object_size / Byte'object_size);
                Assert(Get_Device_Input_Data(
                  Device      => To_Unchecked_Address(Data_Signed),
                  Command     => GET_DEVICE_DATA,
                  Data        => Mouse'address,
                  Size        => Number_Of_Bytes'address,
                  Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Mouse'object_size / Byte'object_size);
                if Mouse.Data.Last_X /= 0 or Mouse.Data.Last_Y /= 0 then Inject_Mouse(Identifier, (Integer_8_Signed(Mouse.Data.Last_X), Integer_8_Signed(Mouse.Data.Last_Y))); end if;
                if    (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_DOWN)       > 0 then Inject_Key(Identifier, Left_Mouse_Key,        True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_UP)         > 0 then Inject_Key(Identifier, Left_Mouse_Key,        False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN)      > 0 then Inject_Key(Identifier, Right_Mouse_Key,       True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_UP)        > 0 then Inject_Key(Identifier, Right_Mouse_Key,       False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN)     > 0 then Inject_Key(Identifier, Middle_Mouse_Key,      True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_UP)       > 0 then Inject_Key(Identifier, Middle_Mouse_Key,      False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN)    > 0 then Inject_Key(Identifier, Auxiliary_1_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP)      > 0 then Inject_Key(Identifier, Auxiliary_1_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN)    > 0 then Inject_Key(Identifier, Auxiliary_2_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP)      > 0 then Inject_Key(Identifier, Auxiliary_2_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL) > 0 or (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                  Inject_Key(Identifier, Is_Pressed => True, Key =>(
                    if To_Unchecked_Integer_2_Signed(Integer_2_Unsigned(Shift_Right(Amount => 16, Value => Integer_8_Unsigned(Mouse.Data.Button_Flags) and 16#0000_0000_FFFF_0000#))) / MOUSE_WHEEL_DELTA < 0
                    then (if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then Horizontal_Wheel_Left_Key else Vertical_Wheel_Down_Key)
                    else (if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then Horizontal_Wheel_Right_Key else Vertical_Wheel_Up_Key)));
                end if;
              end;
          when others => null; end case;
      when others => null; end case;
      return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
    end Window_Callback;
  function Get_Cursor return Record_Location is
    Point : aliased Record_Point := (others => <>);
    begin
      Assert(Get_Cursor_Position(Point'address));
      return (Integer_8_Signed(Point.X), Integer_8_Signed(Point.Y));
    end Get_Cursor;
  procedure Initialize is
    Current_Virtual_Key : Integer_2_Unsigned_C := 0;
    Class : aliased Record_Window_Class :=(
      Size       => Record_Window_Class'object_size / Byte'object_size,
      Style      => 0,
      Callback   => Window_Callback'address,
      Extra_A    => 0,
      Extra_B    => 0,
      Instance   => Get_Current_Instance,
      Icon_Small => Load_Icon(Get_Current_Instance, GENERIC_ICON),
      Icon_Large => Load_Icon(Get_Current_Instance, GENERIC_ICON),
      Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
      Background => BRUSH_GRAY,
      Menu_Name  => null,
      Class_Name => To_Access_Constant_Character_2_C(To_String_2(Name_Class)));
    begin
      Assert(Register_Class(Class'unchecked_access) /= Integer_2_Unsigned_C(FAILED));
      Window := Create_Window(
        Style_Extra => 0,
        Class_Name  => Name_Class,
        Window_Name => Name_Class,
        Style       => STYLE_NO_ACTIVATE,
        X           => 0,
        Y           => 0,
        Width       => 0,
        Height      => 0,
        Parent      => NULL_ADDRESS,
        Menu        => 0,
        Instance    => Get_Current_Instance,
        Parameter   => NULL_ADDRESS);
      Assert(Window);
      declare
      Device_Setups : aliased Array_Record_Device_Setup :=(
        (GENERIC_DESKTOP_CONTROL, USE_RAW_KEYBOARD, TAKE_INPUT_ON_NON_ACTIVE, Window),
        (GENERIC_DESKTOP_CONTROL, USE_RAW_MOUSE,    TAKE_INPUT_ON_NON_ACTIVE, Window),
        (GENERIC_DESKTOP_CONTROL, USE_RAW_JOYSTICK, TAKE_INPUT_ON_NON_ACTIVE, Window));
      begin
        Assert(Register_Devices(
          Devices => Device_Setups'address,
          Number  => Device_Setups'Length,
          Size    => Record_Device_Setup'object_size / Byte'object_size));
      end;
    end Initialize;
  function Update return Boolean is
    Number_Of_Devices : aliased Integer_4_Unsigned_C := 0;
    State             : aliased Record_Gamepad_State := (others => <>);
    Message           : aliased Record_Message       := (others => <>);
    Has_Gamepad       :         Array_Boolean(1..4)  := (others => False);
    File              :         Address              := NULL_ADDRESS;
    Current_Device    :         Ordered_Map_Record_Device.Cursor;
    procedure Unpack_Button(Player : in Integer_4_Signed; Raw : in Integer_2_Unsigned_C; Key : in Enumerated_Xbox_Key) is
      begin
        if (State.Gamepad.Buttons and Raw) /= (Gamepad_States(Player).Buttons and Raw) then
          if (State.Gamepad.Buttons and Raw) > 0 then Inject_Key(Integer_Address(Player), Key, True);
          else Inject_Key(Integer_Address(Player), Key, False); end if;
        end if;
      end Unpack_Button;
    procedure Unpack_Stick(Player : in Integer_4_Signed; Stick : in Enumerated_Stick; X, Y : in Integer_2_Signed_C) is
      begin
        Inject_Stick(Integer_Address(Player), Stick,((
          if X > 0 then Float_4_Range(Float_4_Real(X) / Float_4_Real(Integer_2_Signed_C'last))
          else          Float_4_Range(Float_4_Real(X) / Float_4_Real(Integer_2_Signed_C'first))),(
          if Y > 0 then Float_4_Range(Float_4_Real(Y) / Float_4_Real(Integer_2_Signed_C'last))
          else          Float_4_Range(Float_4_Real(Y) / Float_4_Real(Integer_2_Signed_C'first)))));
      end Unpack_Stick;
    begin
      Assert(Get_Device_List(NULL_ADDRESS, Number_Of_Devices'address, Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
      Assert(Number_Of_Devices /= 0);
      declare
      List : aliased Array_Record_Device_List_Element(1..Integer(Number_Of_Devices)) := (others => <>);
      begin
        Assert(Get_Device_List(List(List'first)'address, Number_Of_Devices'address, Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
        for I in List'range loop
          if not Has_Device(To_Unchecked_Integer_Address(List(I).Handle)) then
            case List(I).Kind is
              when KIND_IS_KEYBOARD => Add_Device(To_Unchecked_Integer_Address(List(I).Handle), (Keyboard_Device, others => <>));
              when KIND_IS_MOUSE    => Add_Device(To_Unchecked_Integer_Address(List(I).Handle), (Mouse_Device,    others => <>));
            when others => null; end case;
          end if;
        end loop;
        Current_Device := Devices.First;
        while Devices.Has_Element(Current_Device) loop
          if Devices.Key(Current_Device) in 0..3 then
            Has_Gamepad(Integer_4_Signed(Devices.Key(Current_Device)) + 1) := True;
            if Get_XInput_State(Integer_4_Unsigned_C(Devices.Key(Current_Device)), State'unchecked_access) /= 0 then Devices.Delete(Current_Device); end if;
          else
            for J in List'range loop
              if Devices.Key(Current_Device) = To_Unchecked_Integer_Address(List(J).Handle) then exit; end if;
              if J = List'last then Devices.Delete(Current_Device); end if;
            end loop;
          end if;
          Devices.Next(Current_Device);
        end loop;
        for I in Gamepad_States'range loop
          if not Has_Gamepad(I + 1) then
            if Get_XInput_State(Integer_4_Unsigned_C(I), State'unchecked_access) = 0 then Add_Device(Integer_Address(I), (XBox_Device, others => <>)); end if;
          end if;
        end loop;
      end;
      while Peek_Message(
        Window         => Window,
        Message        => Message'unchecked_access,
        Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
        Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
        Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
      loop
        if Message.Data = MESSAGE_QUIT then return False; end if;
        Assert_Dummy(Translate_Message(Message'unchecked_access));
        Assert_Dummy(Dispatch_Message(Message'unchecked_access));
      end loop;
      begin for I in Gamepad_States'range loop
        if Get_XInput_State(Integer_4_Unsigned_C(I), State'unchecked_access) = 0 and then Gamepad_States(I) /= State.Gamepad then
          Unpack_Button(I, GAMEPAD_A,                     Green_A_Key);
          Unpack_Button(I, GAMEPAD_B,                     Red_B_Button_Key);
          Unpack_Button(I, GAMEPAD_X,                     Blue_X_Key);
          Unpack_Button(I, GAMEPAD_Y,                     Yellow_Y_Key);
          Unpack_Button(I, GAMEPAD_BACK,                  Back_Key);
          Unpack_Button(I, GAMEPAD_START,                 Start_Key);
          Unpack_Button(I, GAMEPAD_THUMB_LEFT,            Left_Stick_Key);
          Unpack_Button(I, GAMEPAD_THUMB_RIGHT,           Right_Stick_Key);
          Unpack_Button(I, GAMEPAD_BUMPER_LEFT,           Left_Bumper_Key);
          Unpack_Button(I, GAMEPAD_BUMPER_RIGHT,          Right_Bumper_Key);
          Unpack_Button(I, GAMEPAD_DIRECTIONAL_PAD_UP,    Directional_Pad_Up_Key);
          Unpack_Button(I, GAMEPAD_DIRECTIONAL_PAD_DOWN,  Directional_Pad_Down_Key);
          Unpack_Button(I, GAMEPAD_DIRECTIONAL_PAD_LEFT,  Directional_Pad_Left_Key);
          Unpack_Button(I, GAMEPAD_DIRECTIONAL_PAD_RIGHT, Directional_Pad_Right_Key);
          if State.Gamepad.Thumb_Left_X  /= Gamepad_States(I).Thumb_Left_X  or State.Gamepad.Thumb_Left_Y  /= Gamepad_States(I).Thumb_Left_Y  then Unpack_Stick(I, Left_Stick,  State.Gamepad.Thumb_Left_X,  State.Gamepad.Thumb_Left_Y);  end if;
          if State.Gamepad.Thumb_Right_X /= Gamepad_States(I).Thumb_Right_X or State.Gamepad.Thumb_Right_Y /= Gamepad_States(I).Thumb_Right_Y then Unpack_Stick(I, Right_Stick, State.Gamepad.Thumb_Right_X, State.Gamepad.Thumb_Right_Y); end if;
          if State.Gamepad.Left_Trigger  /= Gamepad_States(I).Left_Trigger  then Inject_Trigger(Integer_Address(I), Left_Trigger,  Float_4_Real(State.Gamepad.Left_Trigger)  / Float_4_Real(Integer_1_Unsigned_C'last) * 100.0); end if;
          if State.Gamepad.Right_Trigger /= Gamepad_States(I).Right_Trigger then Inject_Trigger(Integer_Address(I), Right_Trigger, Float_4_Real(State.Gamepad.Right_Trigger) / Float_4_Real(Integer_1_Unsigned_C'last) * 100.0); end if;
          Gamepad_States(I) := State.Gamepad;
        end if;
      end loop; exception when others => null; end; -- "TERRIBLE HORRIBLE NO GOOD VERY BAD HACK"
      return True;
    end Update;
  procedure Finalize is
    Empty_Device_Setup : aliased Record_Device_Setup := (GENERIC_DESKTOP_CONTROL, USE_RAW_MOUSE, STOP_READING_TOP_LEVEL_DEVICES, NULL_ADDRESS);
    begin
      Assert_Dummy(Register_Devices(Empty_Device_Setup'address, Empty_Device_Setup'size / Record_Device'object_size, Record_Device'object_size / Byte'object_size));
      if Window /= NULL_ADDRESS then Assert_Dummy(Destroy_Window(Window)); end if;
      Window := NULL_ADDRESS;
    end Finalize;
end Import;
