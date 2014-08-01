with Neo.Windows; use Neo.Windows;
with Interfaces.C;     use Interfaces.C;
with Interfaces;       use Interfaces;
with System;           use System;
with Ada.Exceptions; use Ada.Exceptions;
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
  MAP_VIRTUAL_KEY : array(Enumerated_Keyboard_Key'range) of Integer_2_Unsigned_C := (others => 0);
  Current_Library_Path      : aliased String_2_C(1..9)              := (others => NULL_CHARACTER_2_C);
  Keyboard_Library          : aliased Address                       := NULL_ADDRESS;
  Window                    :         Address                       := NULL_ADDRESS;
  Keyboard_Layer_Descriptor :         Access_Record_Keyboard_Tables := null;
  Keyboard_Table            :         Array_Integer_Address(1..10)  := (others => 0);
  function Lookup_Character(Keyboard : in Array_Keyboard_Keys) return Character_2 is -- This is a key logger
    Result                 :         Character_2_C        := NULL_CHARACTER_2_C;
    Dead                   :         Character_2_C        := NULL_CHARACTER_2_C;
    Base                   :         Character_2_C        := NULL_CHARACTER_2_C;
    Diacritic              :         Character_2_C        := NULL_CHARACTER_2_C;
    Result_Length          :         Integer_4_Signed_C   := 0;
    Shift                  :         Integer_4_Signed     := -1;
    Modifiers              :         Integer_4_Signed     := 1;
    Virtual_Key            :         Integer_2_Unsigned_C := 0;
    Current_Keyboard_Table :         Access_Record_Virtual_Key_To_Character_2_C := null;
    Library_Path           : aliased String_2_C(1..9)     := (others => NULL_CHARACTER_2_C);
    State                  :         Record_State         := (others => <>);
    Key                    :         Enumerated_Key       := Null_Key;
    J                      :         Integer              := 0;
    Is_Capital_Locked      :         Boolean              :=(
      if (Get_Key_State(Integer_4_Signed_C(Integer_4_Unsigned_C(MAP_VIRTUAL_KEY(Capital_Lock_Key)))) and 16#0001#) > 0 then True
      else False);
    begin
      Assert(Get_Keyboard_Layout_Name(Library_Path'address));
      if Current_Library_Path /= Library_Path then
        if Current_Library_Path(1) /= NULL_CHARACTER_2_C then Assert(Free_Library(Keyboard_Library)); end if;
        Current_Library_Path := Library_Path;
        declare
        Get_Keyboard_Layer_Descriptor :         Access_Function_Get_Keyboard_Layer_Descriptor := null;
        Key                           : aliased Address                                       := NULL_ADDRESS;
        Variable_Kind                 : aliased Integer_4_Unsigned_C                          := REG_SZ;
        Maximum_Path                  : aliased Integer_4_Unsigned_C                          := Integer_4_Unsigned_C(MAXIMUM_PATH_LENGTH);
        Layout_Path                   : aliased String_2_C(1..MAXIMUM_PATH_LENGTH)            := (others => NULL_CHARACTER_2_C);
        System_Path                   : aliased String_2_C(1..MAXIMUM_PATH_LENGTH)            := (others => NULL_CHARACTER_2_C);
        begin
          Assert(Registry_Open_Key(
            Key     => HKEY_LOCAL_MACHINE,
            Sub_Key => To_String_2_C("SYSTEM\CurrentControlSet\Control\Keyboard Layouts\" & To_String_2(Library_Path)),
            Options => 0,
            Desired => KEY_QUERY_VALUE,
            Result  => Key'address) = 0);
          Assert(Registry_Query_Value(
            Key        => Key,
            Value_Name => To_String_2_C("Layout File"),
            Reserved   => null,
            Kind       => Variable_Kind'unchecked_access,
            Data       => Layout_Path'address,
            Data_Size  => Maximum_Path'unchecked_access) = 0);
          Assert(Registry_Close_Key(Key) = 0);
          if Neo.System.SPECIFICS.Bit_Size = 64 and WORD_SIZE = 32 then -- WOW! 64
            Assert(Get_Folder_Path(NULL_ADDRESS, PATH_SYSTEM_X86, NULL_ADDRESS, 0, System_Path'address) = NULL_ADDRESS);
          else
            Assert(Get_System_Directory(System_Path'address, Integer_4_Unsigned_C(MAXIMUM_PATH_LENGTH)) /= 0);
          end if;
          Keyboard_Library := Load_Library(To_String_2_C(To_String_2(System_Path)  & "\" & To_String_2(Layout_Path)));
          Assert(Keyboard_Library);
          Get_Keyboard_Layer_Descriptor := To_Unchecked_Access_Function_Get_Keyboard_Layer_Descriptor(
            Get_Procedure_Address(Keyboard_Library, To_String_1_C("KbdLayerDescriptor")));
          Assert(Get_Keyboard_Layer_Descriptor /= null);
          Keyboard_Layer_Descriptor := Get_Keyboard_Layer_Descriptor.all;
          Assert(Keyboard_Layer_Descriptor /= null);
          for I in Keyboard_Layer_Descriptor.Virtual_Key_To_Character_2_C_Table.all'range loop
            exit when Keyboard_Layer_Descriptor.Virtual_Key_To_Character_2_C_Table(I).Size = 0;
            for J in Keyboard_Table'range loop
              if (Keyboard_Layer_Descriptor.Virtual_Key_To_Character_2_C_Table(I).Size - 2) / 2 = Integer_1_Unsigned_C(J) then
                Keyboard_Table(J) := Keyboard_Layer_Descriptor.Virtual_Key_To_Character_2_C_Table(I).Virtual_Key_To_Character_2_C;
              end if;
            end loop;
          end loop;
        end;
      end if;
      for I in Keyboard'range loop
        if Keyboard(I).Is_Pressed and (Keyboard(I).Last > State.Last or Key = Null_Key) then
          Key   := I;
          State := Keyboard(I);
        end if;
      end loop;
      Virtual_Key := MAP_VIRTUAL_KEY(Key);
      for I in Keyboard_Layer_Descriptor.Character_Modifiers.Virtual_Key_To_Bit'range loop
        exit when Keyboard_Layer_Descriptor.Character_Modifiers.Virtual_Key_To_Bit(I).Virtual_Key = 0;
        Key := MAP_KEY(Integer_2_Unsigned_C(Keyboard_Layer_Descriptor.Character_Modifiers.Virtual_Key_To_Bit(I).Virtual_Key));
        if Key = Shift_Key then
          --Put_Line("Found shift!");
          Shift := I + 1;
        end if;
        if Keyboard(Key).Is_Pressed then
          --Put_Line("Modified!");
          modifiers := I + 1;
        end if;
      end loop;
      for I in Keyboard_Table'range loop
        if Keyboard_Table(I) /= 0 then--and Integer_4_Signed(Modifiers) <= I then
          Current_Keyboard_Table := To_Unchecked_Access_Record_Virtual_Key_To_Character_2_C(Keyboard_Table(I));
          J := 0;
          loop
            if Integer_2_Unsigned_C(Current_Keyboard_Table.Virtual_Key) = Virtual_Key then
              if Current_Keyboard_Table.Attributes = CAPITAL_LOCK and Is_Capital_Locked then
                Modifiers := (if Modifiers = Shift then 1 else Shift);
              end if;
              Result        := Current_Keyboard_Table.Characters(Modifiers);
              Result_Length := 1;
              if Result = KEYBOARD_NO_CHARACTER then Result_Length := 0;
              elsif Result = KEYBOARD_DEAD_CHARACTER then
                Result_Length  := 0;
                --Dead_Character := Keyboard_Table(I).all(J + 1).Characters(Integer_4_Signed(Modifiers));
              end if;
              exit;
            end if;
            J := J + 1;
            Current_Keyboard_Table := To_Unchecked_Access_Record_Virtual_Key_To_Character_2_C( -- Pointer arithmetic!
              Keyboard_Table(I) + Integer_Address((Record_Virtual_Key_To_Character_2_C'object_size - (10 - I) * Character_2_C'object_size) / 8 * J));
            exit when Current_Keyboard_Table.Virtual_Key = 0;
          end loop;
        end if;
      end loop;
      if Result_Length = 0 then raise No_Printable_Character; end if;
      if Dead /= NULL_CHARACTER_2_C then -- "I see dead characters..."
        for I in Keyboard_Layer_Descriptor.Dead_Keys'range loop
          exit when Keyboard_Layer_Descriptor.Dead_Keys(I).Both = 0;
          Base := Character_2_C'val(Integer_4_Signed(Keyboard_Layer_Descriptor.Dead_Keys(I).Both and 16#0000_FFFF#));
          Diacritic := Character_2_C'val(Integer_4_Signed(Shift_Right(Integer_4_Unsigned(Keyboard_Layer_Descriptor.Dead_Keys(I).Both), 16)));
          if Base = Result and Diacritic = Dead then
            return Character_2(Keyboard_Layer_Descriptor.Dead_Keys(I).Composed);
          end if;
        end loop;
      end if;
      return Character_2(Result);
    end Lookup_Character;
  procedure Set_Vibration(Identifier : in Integer_Address; Frequency_High, Frequency_Low : in Float_4_Percent) is
    begin
      null;
    end Set_Vibration;
  procedure Update_Devices is
    --Product      : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
    --Manufacturer : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
    --Capabilities : aliased Record_Device_Capabilities                 := (others => <>);
    Number_Of_X  : aliased Integer_4_Unsigned_C                       := 0;
    Device       :         Record_Device                              := (others => <>);
    File         :         Address                                    := NULL_ADDRESS;
    begin
      Assert(Get_Device_List(
        List  => NULL_ADDRESS,
        Count => Number_Of_X'address,
        Size  => Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
      Assert(Number_Of_X /= 0);
        declare
        List : aliased Array_Record_Device_List_Element(1..Integer(Number_Of_X)) := (others => <>);
        begin
          Assert(Get_Device_List(
            List  => List(List'first)'address,
            Count => Number_Of_X'address,
            Size  => Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
          for I in List'range loop
            if not Has_Element(To_Unchecked_Integer_Address(List(I).Handle)) then
              case List(I).Kind is
                when KIND_IS_KEYBOARD =>
                  Add_Device(To_Unchecked_Integer_Address(List(I).Handle), (Keyboard_Device, others => <>));
                when KIND_IS_MOUSE =>
                  Add_Device(To_Unchecked_Integer_Address(List(I).Handle), (Mouse_Device, others => <>));
                when others => null;
              end case;
            end if;
          end loop;
          --for I in Devices'range loop
          --  for J in List'range loop
          --    if Devices(I).Identifier = List(J).Handle then
          --      exit;
          --    end if;
          --    if J = List'last then
          --      Remove_Device(Devices(I).Identifier);
          --    end if;
          --  end loop;
          --end loop;
        end;
    -- Product      : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
    -- Manufacturer : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
    -- Capabilities : aliased Record_Device_Capabilities                 := (others => <>);
    -- Number_Of_X  : aliased Integer_4_Unsigned_C                       := 0;
    -- Device       :         Record_Device                              := (others => <>);
    -- File         :         Address                                    := NULL_ADDRESS;
    -- begin
    --   Assert(Get_Device_List(
    --     List  => NULL_ADDRESS,
    --     Count => Number_Of_X'address,
    --     Size  => Record_Device_List_Element'object_size / Byte'object_size) /= -1);
    --   Assert(Number_Of_X /= 0);
    --     declare
    --     List : aliased Array_Record_Device_List_Element(1..Integer(Number_Of_X)) := (others => <>);
    --     begin
    --       Assert(Get_Device_List(
    --         List  => List(List'first)'address,
    --         Count => Number_Of_X'address,
    --         Size  => Record_Device_List_Element'object_size / Byte'object_size) /= -1);
    --       for I in List'range loop
    --         if not Is_Device_Present(Handle) then
    --           case List(I).Kind is
    --             when KIND_IS_KEYBOARD | KIND_IS_MOUSE =>
    --               Add_Device((Handle, others => <>));
    --             when KIND_IS_HUMAN_INTERFACE_DEVICE =>
    --               Device := (others => <>);
    --               Assert(Get_Device_Information(
    --                 Device  => Handle,
    --                 Command => GET_DEVICE_NAME,
    --                 Data    => NULL_ADDRESS,
    --                 Size    => Number_Of_X'access) = 0);
    --               Assert(Number_Of_X >= 2);
    --                 declare
    --                 Identifier : aliased String_2(1..Integer(Number_Of_X)) := (Others => NULL_CHARACTER_2);
    --                 begin
    --                   if
    --                   Get_Device_Information(
    --                     Device  => Handle,
    --                     Command => GET_DEVICE_NAME,
    --                     Data    => Identifier'access,
    --                     Size    => Number_Of_X'access) < 0
    --                   then
    --                     raise Call_Failure;
    --                   end if;
    --                   File :=
    --                     Create_File(
    --                       Name                 => Identifier'access,
    --                       Desired_Access       => GENERIC_READ or GENERIC_WRITE,
    --                       Share_Mode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
    --                       Security_Attributes  => NULL_ADDRESS,
    --                       Creation_Desposition => OPEN_EXISTING,
    --                       Flags_And_Attributes => 0,
    --                       Template_File        => NULL_ADDRESS);
    --                   Assert(File);
    --                   Assert(Get_Device_Product(
    --                     File   => File,
    --                     Buffer => Product'access,
    --                     Size   => Product'object_size / Byte'object_size));
    --                   Assert(Get_Device_Manufacturer(
    --                     File   => File,
    --                     Buffer => Manufacturer'access,
    --                     Size   => Manufacturer'object_size / Byte'object_size));
    --                   Assert(Close_Handle(File));
    --                   Assert(Get_Device_Information(
    --                     Device  => Handle,
    --                     Command => GET_PREPARSED_DATA,
    --                     Data    => NULL_ADDRESS,
    --                     Size    => Number_Of_X'access) /= 0);
    --                   Assert(Number_Of_X > 1);
    --                     declare
    --                     Data : aliased Array_Integer_1_Unsigned_C(1..Integer(Number_Of_X)) := (others => 0);
    --                     begin
    --                       Assert(Get_Device_Information(
    --                         Device  => Handle,
    --                         Command => GET_PREPARSED_DATA,
    --                         Data    => Data'access,
    --                         Size    => Number_Of_X'access) /= 0);
    --                       Assert(Get_Device_Capabilities(Data'access, Capabilities'access));
    --                       if Number_Of_X > 0 then
    --                           declare
    --                           Buttons : aliased Array_Record_Button_Capability(1..Integer(Number_Of_X)) := (others => <>);
    --                           begin
    --                             if
    --                             Get_Device_Buttons(
    --                               Report_Kind => DEVICE_INPUT,
    --                               Buttons     => Buttons'access,
    --                               Length      => Number_Of_X'access,
    --                               Data        => Data'access) = FAILED
    --                             then
    --                               raise Call_Failure;
    --                             end if;
    --                             Device.Number_Of_Generic_Buttons := Buttons.Bounds.Usage_Maximum - Buttons.Bounds.Usage_Minimum + 1;
    --                               declare
    --                               Button_Values : aliased Array_Record_Button_Values(1..Integer(Number_Of_X)) := (others => <>);
    --                               begin
    --                                 if
    --                                 Get_Device_Button_Values(
    --                                   Report_Kind   => DEVICE_INPUT,
    --                                   Button_Values => Button_Values'access,
    --                                   Length        => Number_Of_X'access,
    --                                   Data          => Data'access) = FAILED
    --                                 then
    --                                   raise Call_Failure;
    --                                 end if;
    --                                 -- ???
    --                               end;
    --                           end;
    --                       end if;
    --                 end;
    --             when others =>
    --               raise Call_Failure;
    --           end case;
    --       end loop;
    --       for I in Devices'range loop
    --         for J in List'range loop
    --           if Devices(I).Identifier = List(J).Handle then
    --             exit;
    --           end if;
    --           if J = List'last then
    --             Remove_Device(Devices(I).Identifier);
    --           end if;
    --         end loop;
    --       end loop;
    --     end Build_List;
    end Update_Devices;
  procedure Handle_Events is
    Message : aliased Record_Message := (others => <>);
    --XINPUT_STATE  joyData[MAX_JOYSTICKS];
    begin
      if Peek_Message(
        Window         => Window,
        Message        => Message'unchecked_access,
        Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
        Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
        Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
      then
        if Message.Data = MESSAGE_QUIT then
          Put_Line("WHAT?!");
        end if;
        Assert_Dummy(Translate_Message(Message'unchecked_access));
        Assert_Dummy(Dispatch_Message(Message'unchecked_access));
      end if;
      --for I in 1..NUMBER_OF_GAMEPADS loop
      --  Get_(I);
      --end loop;
    end Handle_Events;
  procedure Finalize is
    Empty_Device_Setup : aliased Record_Device_Setup :=(
      Page   => GENERIC_DESKTOP_CONTROL,
      Usage  => USE_RAW_MOUSE,
      Flags  => STOP_READING_TOP_LEVEL_DEVICES,
      Target => NULL_ADDRESS);
    begin
      Assert(Register_Devices(
        Devices => Empty_Device_Setup'address,
        Number  => Empty_Device_Setup'size / Record_Device'object_size,
        Size    => Record_Device'object_size / Byte'object_size));
      if Window /= NULL_ADDRESS then
        Assert(Destroy_Window(Window));
      end if;
      Window := NULL_ADDRESS;
    end Finalize;
  function Window_Callback(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C; pragma Convention(Stdcall, Window_Callback);
  function Window_Callback(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
    Player          :         Integer_4_Positive   := 1;
    Number_Of_Bytes : aliased Integer_4_Unsigned_C := 0;
    Header          : aliased Record_Device_Header := (others => <>);
    begin
      case Message is
        when EVENT_CLOSE =>
          Post_Quit_Message(0);
          return C_FALSE;
        when EVENT_INPUT =>
          Number_Of_Bytes := Record_Device_Header'object_size / Byte'object_size;
          Assert(Get_Device_Input_Data(
            Device      => To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(Data_Signed))),
            Command     => GET_DEVICE_HEADER,
            Data        => Header'address,
            Size        => Number_Of_Bytes'address,
            Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Header'object_size / Byte'object_size);
          case Header.Kind is
            when KIND_IS_KEYBOARD =>
              declare
              Keyboard   : aliased Record_Device_Keyboard := (others => <>);
              Is_Pressed :         Boolean                := False;
              begin
                Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Keyboard'object_size / Byte'object_size);
                Assert(Get_Device_Input_Data(
                  Device      => To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(Data_Signed))),
                  Command     => GET_DEVICE_DATA,
                  Data        => Keyboard'address,
                  Size        => Number_Of_Bytes'address,
                  Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Keyboard'object_size / Byte'object_size);
                if Keyboard.Data.Key <= MAP_KEY'last and Keyboard.Data.Key >= MAP_KEY'first then
                  if Keyboard.Data.Message = EVENT_KEY_DOWN or Keyboard.Data.Message = EVENT_SYSTEM_KEY_DOWN then
                    Is_Pressed := True;
                  end if;
                  case MAP_KEY(Keyboard.Data.Key) is
                    when Shift_Key =>
                      if Keyboard.Data.Make_Code = KEY_MAKE_CODE_FOR_LEFT then
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Left_Shift_Key, Is_Pressed);
                      else
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Right_Shift_Key, Is_Pressed);
                      end if;
                    when Control_Key =>
                      if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0 then
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Right_Control_Key, Is_Pressed);
                      else
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Left_Control_Key, Is_Pressed);
                      end if;
                    when Alternative_Key =>
                      if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0  then
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Right_Alternative_Key, Is_Pressed);
                      else
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Left_Alternative_Key, Is_Pressed);
                      end if;
                    when others =>
                      Handle_Key(To_Unchecked_Integer_Address(Header.Device), MAP_KEY(Keyboard.Data.Key), Is_Pressed);
                  end case;
                end if;
              end;
            when KIND_IS_MOUSE =>
              declare
              Mouse : aliased Record_Device_Mouse := (others => <>);
              begin
                Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Mouse'object_size / Byte'object_size);
                Assert(Get_Device_Input_Data(
                  Device      => To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(Data_Signed))),
                  Command     => GET_DEVICE_DATA,
                  Data        => Mouse'address,
                  Size        => Number_Of_Bytes'address,
                  Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Mouse'object_size / Byte'object_size);
                if Mouse.Data.Last_X /= 0 or Mouse.Data.Last_Y /= 0 then
                  Handle_Mouse(To_Unchecked_Integer_Address(Header.Device), (Integer_8_Signed(Mouse.Data.Last_X), Integer_8_Signed(Mouse.Data.Last_Y)));
                end if;
                if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_DOWN) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Left_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_UP) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Left_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Right_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_UP) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Right_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Middle_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_UP) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Middle_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Auxiliary_1_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Auxiliary_1_Mouse_Key, False);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Auxiliary_2_Mouse_Key, True);
                elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP) > 0 then
                  Handle_Key(To_Unchecked_Integer_Address(Header.Device), Auxiliary_2_Mouse_Key, False);
                elsif
                (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL) > 0 or
                (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0
                then
                  declare
                  Wheel_Delta : Integer_2_Signed :=
                    To_Unchecked_Integer_2_Signed(
                      Integer_2_Unsigned(
                        Shift_Right(
                          Value  => Integer_4_Unsigned(Mouse.Data.Button_Flags),
                          Amount => Integer_4_Unsigned'object_size - Integer_2_Unsigned'object_size)))
                    / MOUSE_WHEEL_DELTA;
                  begin
                    if Wheel_Delta < 0 then
                      if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Horizontal_Wheel_Left_Key, True);
                      else
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Vertical_Wheel_Down_Key, True);
                      end if;
                    elsif Wheel_Delta > 0 then
                      if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Horizontal_Wheel_Right_Key, True);
                      else
                        Handle_Key(To_Unchecked_Integer_Address(Header.Device), Vertical_Wheel_Up_Key, True);
                      end if;
                    end if;
                  end;
                end if;
              end;
            --when KIND_IS_GAMEPAD_OR_JOYSTICK => null;
              --Assert(Get_Device_Button_Usages(
              --  Report_Kind     => DEVICE_INPUT,
              --  Usage_Page      => Buttons(1).Usage_Page,
              --  Link_Collection => 0,
              --  Usage_List      => ,
              --  Usage_Length    => Number_Of_X'access,
              --  Data            => Data'access,
              --  Report          => Data.Hid.Raw_Data'access,
              --  Report_Length   => Data.Hid.Size_Of_Hid'access));
              --for I in 1..Integer(Number_Of_X) loop
              --  null;
              --end loop;
            when others =>
              null;
          end case;
        when others =>
          null;
      end case;
      return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
    end Window_Callback;
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
    Class_Name => To_Access_Constant_Character_2_C(To_String_2(Neo.System.SPECIFICS.Name)));
  begin
    Assert(Register_Class(Class'unchecked_access) /= Integer_2_Unsigned_C(FAILED));
    Window :=
      Create_Window(
        Style_Extra => 0,
        Class_Name  => To_String_2_C(Neo.System.SPECIFICS.Name),
        Window_Name => To_String_2_C(Neo.System.SPECIFICS.Name),
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
        Target => Window)
      --4 =>(
      --  Page   => GENERIC_DESKTOP_CONTROL,
      --  Usage  => USE_RAW_GAMEPAD,
      --  Flags  => TAKE_INPUT_ON_NON_ACTIVE,
      --  Target => Window)
      );
    begin
      Assert(Register_Devices(
        Devices => Device_Setups'address,
        Number  => Device_Setups'Length,
        Size    => Record_Device_Setup'object_size / Byte'object_size));
    end;
    for I in MAP_VIRTUAL_KEY'range loop
      Current_Virtual_Key := 0;
      for J in MAP_KEY'range loop
        if I = MAP_KEY(J) then
          Current_Virtual_Key := J;
          exit;
        end if;
      end loop;
      MAP_VIRTUAL_KEY(I) := Current_Virtual_Key;
    end loop;
    end Initialize;
  end Import;







