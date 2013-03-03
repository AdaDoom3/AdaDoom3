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
  Neo.Windows;
use
  System,
  Interfaces,
  Interfaces.C,
  Neo.Windows;
separate (Neo.System.Input)
generic
  with
  with
  with

package body Implementation
  is
  ---------------
  -- Constants --
  ---------------
    INPUT_WINDOW_STYLE : constant Integer_4_Unsigned_C := STYLE_NO_ACTIVATE;
    MAP_KEY:
      constant array (SUBEVENT_KEY_LEFT_MOUSE..SUBEVENT_KEY_CLEAR) of Enumerated_Key :=(
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
  ---------------
  -- Variables --
  ---------------
    Window : Address := NULL_ADDRESS;
  -------
  -- ¢ --
  -------
    procedure ¢
      renames To_Unchecked_Integer_8_Unsigned;
  --------------------
  -- Update_Devices --
  --------------------
    procedure Update_Devices
      is
      Number_Of_New_Devices : Integer_4_Unsigned_C := 0;
      begin
        if
        Get_Device_List(
          List  => NULL_ADDRESS,
          Count => Number_Of_New_Devices'Address,
          Size  => Record_Device_List_Element'Size / Integer_1_Unsigned'Size) = -1
        then
          raise System_Call_Failure;
        end if;
        if Number_Of_New_Devices = 0 then
          raise No_Input_Devices_Detected;
        end if;
        -----------
        Build_List:
        -----------
          declare
          List                : Array_Record_Device_List_Element(1..Integer(Number_Of_New_Devices)) := (others => <>);
          Have_Checked_Device : Array(List'Range) of Boolean                                        := (others => False);
          begin
            if
            Get_Device_List(
              List  => List(List'First)'Address,
              Count => Number_Of_New_Devices'Address,
              Size  => Record_Device_List_Element'Size / Integer_1_Unsigned'Size) = -1
            then
              Put_Line(Integer_4_Unsigned_C'Wide_Image(Get_Last_Error));
              raise System_Call_Failure;
            end if;  
            for I in Devices'First..Devices'First - 1 + Integer(Number_Of_Devices) loop
              for J in List'Range loop
                if ¢(List(J).Handle) = Devices(I).Identifier then
                  Have_Checked_Device(J) := True;
                  exit;
                end if;
                if J = List'Last then
                  Remove_Device(¢(Handle));
                end if;
              end loop;
            end loop;
            for I in Have_Checked_Device'Range loop
              if not Have_Checked_Device(I) then
                case List(I).Kind is
                  when KIND_IS_KEYBOARD =>
                    Add_Device(Keyboard_Device, ¢(Handle));
                  when KIND_IS_MOUSE =>
                    Add_Device(Keyboard_Device, ¢(Handle));
                  when KIND_IS_HUMAN_INTERFACE_DEVICE =>
                    --------------
                    Create_Device:
                    --------------
                      declare
                      Number_Of_Characters : aliased Integer_4_Unsigned_C                  := 0;
                      Name_Manufacturer    : aliased String_2(1..MAXIMUM_DEVICE_NAME_SIZE) := (others => NULL_CHARACTER_2);
                      Name_Product         : aliased String_2(1..MAXIMUM_DEVICE_NAME_SIZE) := (others => NULL_CHARACTER_2);
                      File                 :         Address                               := NULL_ADDRESS;
                      begin
                        if 
                        Get_Device_Information(
                          Device  => Handle,
                          Command => GET_DEVICE_NAME,
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
                          Identifier : aliased String_2(1..Integer(Number_Of_Characters)) := (Others => NULL_CHARACTER_2);
                          begin
                            if 
                            Get_Device_Information(
                              Device  => Handle,
                              Command => GET_DEVICE_NAME,
                              Data    => Identifier(Identifier'First)'Address,
                              Size    => Number_Of_Characters'Address) < 0
                            then
                              raise System_Call_Failure;
                            end if;
                            File :=
                              Create_File(
                                Name                 => Identifier(Identifier'First)'Address,
                                Desired_Access       => GENERIC_READ or GENERIC_WRITE,
                                Share_Mode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                                Security_Attributes  => NULL_ADDRESS,
                                Creation_Desposition => OPEN_EXISTING,
                                Flags_And_Attributes => 0,
                                Template_File        => NULL_ADDRESS);
                            if File = NULL_ADDRESS then
                              raise System_Call_Failure;
                            end if;
                            if
                            Get_Device_Product(
                              File   => File,
                              Buffer => Name_Product(Name_Product'First)'Address,
                              Size   => Name_Product'Size / Integer_1_Unsigned'Size) = FAILED
                            then
                              Name_Product(Name_Product'First) := NULL_CHARACTER_2;
                            end if;
                            if
                            Get_Device_Manufacturer(
                              File   => File,
                              Buffer => Name_Manufacturer(Name_Manufacturer'First)'Address,
                              Size   => Name_Manufacturer'Size / Integer_1_Unsigned'Size) = FAILED
                            then
                              if Name_Product(Name_Product'First) = NULL_CHARACTER_2 then
                                Add_Device(Human_Interface_Device, ¢(Handle));
                              else
                                Add_Device(Trim_Null(Name_Product), ¢(Handle));
                              end if;
                            else
                              Add_Device(Trim_Null(Name_Manufacturer) & " " & Trim_Null(Name_Product), ¢(Handle));
                            end if;
                            if Close_Handle(File) = FAILED then
                              raise System_Call_Failure;
                            end if;
                          end Get_Identifier;
                        return Result;
                      end Create_Device;
                  when others =>
                    raise System_Call_Failure;
                end case;
              end if;
            end loop;
          end Build_List;
      end Update_Devices;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Class_Name : in String_2)
      is
      -------------------------
      function Window_Callback(
      -------------------------
        Window        : in Address;
        Message       : in Integer_4_Unsigned_C;
        Data_Unsigned : in Integer_4_Unsigned_C;
        Data_Signed   : in Integer_4_Signed_C)
        return Integer_4_Signed_C;
        pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(
        Window        : in Address;
        Message       : in Integer_4_Unsigned_C;
        Data_Unsigned : in Integer_4_Unsigned_C;
        Data_Signed   : in Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        Player          :         Integer_4_Positive   := 1;
        Number_Of_Bytes : aliased Integer_4_Unsigned_C := 0;
        Header          : aliased Record_Device_Header := (others => <>);
        ----------------------
        procedure Queue_Event(
        ----------------------
          )
          is
          begin
          end Queue_Event;
        ----------------------
        procedure Queue_Event(
        ----------------------
          )
          is
          begin
          end Queue_Event;
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_INPUT =>
              if
              Get_Device_Input_Data(
                Device      => To_Unchecked_Address(Data_Signed),
                Command     => GET_DEVICE_HEADER,
                Data        => Header'Address,
                Size        => Number_Of_Bytes'Address,
                Header_Size => Record_Device_Header'Size / Integer_1_Unsigned'Size)
                  /= Record_Device_Header'Size / Integer_1_Unsigned'Size
              then
                raise System_Call_Failure;
              end if;
              case Header.Kind is
                when KIND_IS_KEYBOARD =>
                  ----------------
                  Handle_Keyboard:
                  ----------------
                    declare
                    Keyboard    : aliased Record_Device_Keyboard := (others => <>);
                    Was_Pressed :         Boolean                := False;
                    begin
                      Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Keyboard'Size / Integer_1_Unsigned'Size);
                      if 
                      Get_Device_Input_Data(
                        Device      => To_Unchecked_Address(Data_Signed),
                        Command     => GET_DEVICE_DATA,
                        Data        => Keyboard'Address,
                        Size        => Number_Of_Bytes'Address,
                        Header_Size => Record_Device_Header'Size / Integer_1_Unsigned'Size)
                          /= Record_Device_Keyboard'Size / Integer_1_Unsigned'Size 
                      then
                        raise System_Call_Failure;
                      end if;
                      if Keyboard.Data.Key <= MAP_KEY'Last and Keyboard.Data.Key >= MAP_KEY'First then 
                        if Keyboard.Data.Message = EVENT_KEY_DOWN or Keyboard.Data.Message = EVENT_SYSTEM_KEY_DOWN then
                          Was_Pressed := True;
                        end if;
                        case MAP_KEY(Keyboard.Data.Key) is
                          when Shift_Key =>
                            if Keyboard.Data.Make_Code = KEY_MAKE_CODE_FOR_LEFT then
                              Queue_Event(Left_Shift_Key, Was_Pressed);
                            else
                              Queue_Event(Right_Shift_Key, Was_Pressed);
                            end if;
                          when Control_Key =>
                            if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0 then
                              Queue_Event(Right_Control_Key, Was_Pressed);
                            else
                              Queue_Event(Left_Control_Key, Was_Pressed);
                            end if;
                          when Alternative_Key =>
                            if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0  then
                              Queue_Event(Right_Alternative_Key, Was_Pressed);
                            else
                              Queue_Event(Left_Alternative_Key, Was_Pressed);
                            end if;
                          when others =>
                            Queue_Event(MAP_KEY(Keyboard.Data.Key), Was_Pressed);
                        end case;
                      end if;
                    end Handle_Keyboard;
                when KIND_IS_MOUSE =>
                  -------------
                  Handle_Mouse:
                  -------------
                    declare
                    Mouse : aliased Record_Device_Mouse := (others => <>);
                    begin
                      Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Mouse'Size / Integer_1_Unsigned'Size);                      
                      if 
                      Get_Device_Input_Data(
                        Device      => To_Unchecked_Address(Data_Signed),
                        Command     => GET_DEVICE_DATA,
                        Data        => Mouse'Address,
                        Size        => Number_Of_Bytes'Address,
                        Header_Size => Record_Device_Header'Size / Integer_1_Unsigned'Size)
                          /= Record_Device_Mouse'Size / Integer_1_Unsigned'Size 
                      then
                        raise System_Call_Failure;
                      end if;
                      if Mouse.Data.Last_X /= 0 or Mouse.Data.Last_Y /= 0 then
                        Queue_Event(Mouse.Data.Last_X, Mouse.Data.Last_Y);
                      end if;
                      if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_DOWN) > 0 then
                        Queue_Event(Left_Mouse_Key, True);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_UP) > 0 then
                        Queue_Event(Left_Mouse_Key, False);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN) > 0 then
                        Queue_Event(Right_Mouse_Key, True);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_UP) > 0 then
                        Queue_Event(Right_Mouse_Key, Flase);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN) > 0 then
                        Queue_Event(Middle_Mouse_Key, True);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_UP) > 0 then
                        Queue_Event(Middle_Mouse_Key, False);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN) > 0 then
                        Queue_Event(Auxiliary_1_Mouse_Key, True);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP) > 0 then
                        Queue_Event(Auxiliary_1_Mouse_Key, False);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN) > 0 then
                        Queue_Event(Auxiliary_2_Mouse_Key, True);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP) > 0 then
                        Queue_Event(Auxiliary_2_Mouse_Key, False);
                      end if;
                      if
                      (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL) > 0 or
                      (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0
                      then
                        --------------------------
                        Extract_Mouse_Wheel_Delta:
                        --------------------------
                          declare
                          Δ : Integer_2_Signed :=
                            To_Unchecked_Integer_2_Signed(
                              Integer_2_Unsigned(
                                Shift_Right(Integer_4_Unsigned(Mouse.Data.Button_Flags), 16))) / MOUSE_WHEEL_DELTA;
                          begin
                            if Δ < 0 then
                              for I in Δ..-1 loop
                                if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                                  Queue_Event(Horizontal_Wheel_Mouse_Left_Key, True);
                                  Queue_Event(Horizontal_Wheel_Mouse_Left_Key, False);
                                else
                                  Queue_Event(Vertical_Wheel_Mouse_Down_Key, True);
                                  Queue_Event(Vertical_Wheel_Mouse_Down_Key, False);
                                end if;
                              end loop;
                            elsif Δ > 0
                              for I in 1..Δ loop
                                if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then;
                                  Queue_Event(Horizontal_Wheel_Mouse_Right_Key, True);
                                  Queue_Event(Horizontal_Wheel_Mouse_Right_Key, False);
                                else
                                  Queue_Event(Vertical_Wheel_Mouse_Up_Key, True);
                                  Queue_Event(Vertical_Wheel_Mouse_Up_Key, False);
                                end if;
                              end loop;
                            end if;
                          end Extract_Mouse_Wheel_Delta;
                      end if;
                    end Handle_Mouse;
                when KIND_IS_HUMAN_INTERFACE_DEVICE =>
                  null;
                when others =>
                  null;
              end case;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Class:
        Record_Window_Class :=(
        Size       => Record_Window_Class'Size / Integer_1_Unsigned'Size,
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
      begin
        if Register_Class(Class'Address) = Integer_2_Unsigned_C(FAILED) then
          raise System_Call_Failure;
        end if;
        Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Name),
            Window_Name => To_String_2_C(Class_Name), 
            Style       => INPUT_WINDOW_STYLE,
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
        --------------------------------------
        Set_Message_Loop_To_Recieve_Raw_Input:
        --------------------------------------
          declare
          Device_Setups:
            Array_Record_Device_Setup :=(
            1 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_KEYBOARD,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            2 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_MOUSE,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            3 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_JOYSTICK,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            4 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_HUMAN_INTERFACE_DEVICE,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window));
          begin
            if
            Register_Devices(
              Devices => Device_Setups'Address,
              Number  => Device_Setups'Length,
              Size    => Record_Device_Setup'Size / Integer_1_Unsigned'Size) = FAILED
            then
              raise System_Call_Failure;
            end if;
          end Set_Message_Loop_To_Recieve_Raw_Input;
      end Initialize;
  ------------------
  -- Poll_Devices --
  ------------------
    procedure Poll_Devices
      is
      Message : aliased Record_Message := (others => <>);
      begin
        if
        Peek_Message(
          Message        => Message'Address,
          Window         => Window,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        then
          if Message.Data = MESSAGE_QUIT then
            exit;
          elsif
          Translate_Message(Message'Address) < 2 and then
          Dispatch_Message(Message'Address) = 0
          then
            null;
          end if;
          if Did_Recieve_Input_From_Unrecognized_Device then
            raise Unrecognized_Device;
          end if;
        end if;
      end Poll_Devices;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is 
      Empty_Device_Setup : aliased Record_Device_Setup :=(
        Page   => GENERIC_DESKTOP_CONTROL,
        Usage  => USE_RAW_MOUSE,
        Flags  => STOP_READING_TOP_LEVEL_DEVICES,
        Target => NULL_ADDRESS);
      begin
        if
        Register_Devices(
          Devices => Empty_Device_Setup'Address,
          Number  => 1,
          Size    => Record_Device'Size / Integer_1_Unsigned'Size) = FAILED
        then
          raise System_Call_Failure;
        end if;
        if Window /= NULL_ADDRESS then
          if Destroy_Window(Window) = FAILED then 
            raise System_Call_Failure;
          end if;
        end if;
        Window := NULL_ADDRESS;
      end Finalize;
  end Neo.System.Input.Implementation;





