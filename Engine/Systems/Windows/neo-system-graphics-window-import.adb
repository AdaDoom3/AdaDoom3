with Neo.Windows; use Neo.Windows;
separate(Neo.System.Graphics.Window) package body Import is
  package Vector_Address is new Vectors(Address);
  STYLE_FULLSCREEN      : constant Integer_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBARLESS_AND_BORDERLESS;
  STYLE_WINDOWED        : constant Integer_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBAR or STYLE_BORDER_THIN_LINE or STYLE_BORDER_SIZABLE or STYLE_BOX_ICONIZE;
  Original_Clip         : aliased Record_Rectangle      := (others => <>);
  --Primary_Window        :         Address               := NULL_ADDRESS; -- Moved to Windows as part of a hack
  Icon                  :         Address               := NULL_ADDRESS;
  Cursor_Inactive       :         Address               := NULL_ADDRESS;
  Cursor_Active         :         Address               := NULL_ADDRESS;
  Cursor_Current        :         Enumerated_Cursor     := Inactive_Cursor;
  Do_Disable_Cursor     :         Boolean               := False;
  Multi_Monitor_Windows :         Vector_Address.Unprotected.Vector;
  procedure Iconize                          is begin Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_GO_TO_ICONIC) = 0); end Iconize;
  function Is_Fullscreen_Only return Boolean is begin return False;                                                            end Is_Fullscreen_Only;
  procedure Set_Cursor(X, Y : in Integer_8_Signed) is
    begin null;
    end Set_Cursor;
  function Get_Borders return Vector_Record_Border.Unprotected.Vector is
    Borders   : Vector_Record_Border.Unprotected.Vector;
    Rectangle : Record_Rectangle := (others => <>);
    function Callback_Monitor(Monitor, Device_Context : in Address; Screen : in Access_Record_Rectangle; Data : in Integer_Address) return Integer_4_Signed_C; pragma Convention(Stdcall, Callback_Monitor);
    function Callback_Monitor(Monitor, Device_Context : in Address; Screen : in Access_Record_Rectangle; Data : in Integer_Address) return Integer_4_Signed_C is
      Monitor_Information : aliased Record_Monitor_Information := (others => <>);
      begin
        Assert(Get_Monitor_Information(Monitor, Monitor_Information'address));
        Borders.Append((
          Left   => Integer_8_Signed(Monitor_Information.Monitor.Left),
          Right  => Integer_8_Signed(Monitor_Information.Monitor.Right),
          Top    => Integer_8_Signed(Monitor_Information.Monitor.Top),
          Bottom => Integer_8_Signed(Monitor_Information.Monitor.Bottom)));
        return C_TRUE;
      end Callback_Monitor;
    begin
      Assert(Enumerate_Display_Monitor(
        Device_Context => NULL_ADDRESS,
        Clip           => NULL_ADDRESS,
        Callback       => Callback_Monitor'address,
        Data           => 0));
      Assert(Borders.Length > 0);
      Assert(Get_Window_Rectangle(Primary_Window, Rectangle'address));
      Borders.Replace_Element(1,(
        Bottom => Integer_8_Signed(Rectangle.Bottom),
        Top    => Integer_8_Signed(Rectangle.Top),
        Left   => Integer_8_Signed(Rectangle.Left),
        Right  => Integer_8_Signed(Rectangle.Right)));
      return Borders;
    end Get_Borders;
  function Get_Decoration return Record_Border is
    begin
      return(
        Top    => Integer_8_Signed(Get_System_Metrics(DATA_TITLE_BAR_HEIGHT) + Get_System_Metrics(DATA_BORDER_HEIGHT)),
        Right  => Integer_8_Signed(Get_System_Metrics(DATA_BORDER_WIDTH)),
        Left   => Integer_8_Signed(Get_System_Metrics(DATA_BORDER_WIDTH)),
        Bottom => Integer_8_Signed(Get_System_Metrics(DATA_BORDER_HEIGHT)));
    end Get_Decoration;
  procedure Set_Cursor_Style(Cursor : in Enumerated_Cursor := Inactive_Cursor) is
    begin
      Cursor_Current := Cursor;
      if WORD_SIZE = 32 then case Cursor is
        when System_Cursor   => Assert_Dummy(Change_Class_Setting_32(Primary_Window, SET_CLASS_CURSOR, Integer_4_Unsigned_C(To_Unchecked_Integer_Address(Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR)))));
        when Inactive_Cursor => Assert_Dummy(Change_Class_Setting_32(Primary_Window, SET_CLASS_CURSOR, Integer_4_Unsigned_C(To_Unchecked_Integer_Address(Cursor_Inactive))));
        when Active_Cursor   => Assert_Dummy(Change_Class_Setting_32(Primary_Window, SET_CLASS_CURSOR, Integer_4_Unsigned_C(To_Unchecked_Integer_Address(Cursor_Active))));
      end case; else case Cursor is
        when System_Cursor   => Assert_Dummy(Change_Class_Setting_64(Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Integer_Address(Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR))));
        when Inactive_Cursor => Assert_Dummy(Change_Class_Setting_64(Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Integer_Address(Cursor_Inactive)));
        when Active_Cursor   => Assert_Dummy(Change_Class_Setting_64(Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Integer_Address(Cursor_Active)));
      end case; end if;
    end Set_Cursor_Style;
  procedure Assert_Only_Instance is
    Handle : Address := NULL_ADDRESS;
    Window : Address := NULL_ADDRESS;
    begin
      Handle := Create_Mutex(NULL_ADDRESS, C_TRUE, To_String_2_C(Neo.System.SPECIFICS.Name));
      if Get_Last_Error /= NO_ERROR then
        Assert(Release_Mutex(Handle));
        Primary_Window := Find_Window(To_String_2_C(Neo.System.SPECIFICS.Name), NULL_STRING_2_C);
        if Primary_Window /= NULL_ADDRESS then
          Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0);
          Handle := Set_Focus(Primary_Window);
          Assert(Set_Foreground_Window(Primary_Window));
          Handle := Set_Active_Window(Primary_Window);
        end if;
        raise Call_Failure;
      end if;
    end Assert_Only_Instance;
  procedure Clip_Mouse(Undo : in Boolean := False; Do_Hide : in Boolean := False) is
    Rectangle : aliased Record_Rectangle := (others => <>);
    begin
      Assert(Get_Window_Rectangle(Primary_Window, Rectangle'address));
      if Do_Hide then while Show_Cursor(Boolean'pos(not Do_Hide)) > -1 loop null; end loop;
      else            while Show_Cursor(Boolean'pos(not Do_Hide)) <  0 loop null; end loop; end if;
      if Undo and Original_Clip /= (others => <>) then
        Assert(Clip_Cursor(Original_Clip'address));
        Original_Clip := (others => <>);
      else
        if Original_Clip = (others => <>) then
          Assert(Get_Clip_Cursor_Area(Original_Clip'address));
          Assert(Original_Clip /= (others => <>));
        end if;
        Assert(Clip_Cursor(Rectangle'address));
      end if;
    end Clip_Mouse;
  procedure Adjust(X, Y : in Integer_4_Signed; Width, Height : in Integer_4_Positive; Do_Fullscreen : in Boolean) is
    begin
      if Primary_Window = NULL_ADDRESS then
        Primary_Window := Create_Window(
          Style_Extra => 0,
          Class_Name  => To_String_2_C(Neo.System.SPECIFICS.Name),
          Window_Name => To_String_2_C(Neo.System.SPECIFICS.Name),
          Style       => (if Do_Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED) or STYLE_ICONIC_INITIALLY,
          X           => Integer_4_Signed_C(X),
          Y           => Integer_4_Signed_C(Y),
          Width       => Integer_4_Signed_C(Width),
          Height      => Integer_4_Signed_C(Height),
          Parent      => NULL_ADDRESS,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS);
        Assert(Primary_Window);
        Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_GO_TO_ICONIC));
        Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_RESTORE));
      else
        Assert(Change_Window_Setting(Primary_Window, SET_WINDOW_STYLE, (if Do_Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED)) /= 0);
        Assert_Dummy(Set_Window_Position(
          Window       => Primary_Window,
          Insert_After => (if Do_Fullscreen then INSERT_ON_TOP_OF_EVERYTHING else REMOVE_ON_TOP_OF_EVERYTHING),
          X            => Integer_4_Signed_C(X),
          Y            => Integer_4_Signed_C(Y),
          Width        => Integer_4_Signed_C(Width),
          Height       => Integer_4_Signed_C(Height),
          Flags        => 0));
      end if;
    end Adjust;
  procedure Adjust_Fullscreen is
    Context : Address := Get_Device_Context(Get_Desktop_Window);
    begin
      Assert(Context);
      Adjust(
        X             => 0,
        Y             => 0,
        Width         => Integer_4_Positive(Get_Device_Capabilities(Context, DATA_HORIZONTAL_RESOLUTION)),
        Height        => Integer_4_Positive(Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION)),
        Do_Fullscreen => True);
      Assert(Release_Device_Context(Get_Desktop_Window, Context));
    end Adjust_Fullscreen;
  procedure Adjust_Windowed(Width, Height : in Integer_4_Positive) is
    Context : Address := Get_Device_Context(Get_Desktop_Window);
    begin
      Assert(Context);
      Adjust(
        X             => Integer_4_Positive(Get_Device_Capabilities(Context, DATA_HORIZONTAL_RESOLUTION)) / 2 - Width  / 2,
        Y             => Integer_4_Positive(Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION))   / 2 - Height / 2,
        Width         => Width,
        Height        => Height,
        Do_Fullscreen => False);
      Assert(Release_Device_Context(Get_Desktop_Window, Context));
    end Adjust_Windowed;
  procedure Initialize_Multi_Monitor is
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Callback_Window);
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address is
      begin
        case Message is when EVENT_CLOSE => Post_Quit_Message(0); return Integer_Address(C_FALSE);
        when others => null; end case;
        return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    Borders : Vector_Record_Border.Unprotected.Vector;
    Class   : aliased Record_Window_Class :=(
      Size       => Record_Window_Class'size / Byte'size,
      Style      => 0,
      Callback   => Callback_Window'address,
      Extra_A    => 0,
      Extra_B    => 0,
      Instance   => Get_Current_Instance,
      Icon_Small => Icon,
      Icon_Large => Icon,
      Background => BRUSH_GRAY,
      Menu_Name  => null,
      Class_Name => To_Access_Constant_Character_2_C(To_String_2(Neo.System.SPECIFICS.Name) & Localize(MULTI_MONITOR_NAME)),
      Cursor     => (case Cursor_Current is
        when Inactive_Cursor => Cursor_Inactive,
        when Active_Cursor   => Cursor_Active,
        when System_Cursor   => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR)));
    begin
      Assert(Register_Class(Class'unrestricted_access) /= Integer_2_Unsigned_C(FAILED));
      for Border of Borders loop
        if Border.Left /= 0 and Border.Top /= 0 then
          Multi_Monitor_Windows.Append(
            Create_Window(
              Style_Extra => 0,
              Style       => STYLE_FULLSCREEN or STYLE_NO_ACTIVATE,
              X           => Integer_4_Signed_C(Border.Left),
              Y           => Integer_4_Signed_C(Border.Top),
              Width       => Integer_4_Signed_C(Border.Right  - Border.Left),
              Height      => Integer_4_Signed_C(Border.Bottom - Border.Top),
              Parent      => NULL_ADDRESS,
              Menu        => 0,
              Instance    => Get_Current_Instance,
              Parameter   => NULL_ADDRESS,
              Class_Name  => To_String_2_C(To_String_2(Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME),
              Window_Name => To_String_2_C(To_String_2(Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME)));
            Assert(Multi_Monitor_Windows.Last_Element);
            Assert_Dummy(Show_Window(Multi_Monitor_Windows.Last_Element, MAKE_WINDOW_NORMALIZE));
            Assert(Update_Window(Multi_Monitor_Windows.Last_Element));
          end if;
        end loop;
      Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE));
      Assert(Update_Window(Primary_Window));
    end Initialize_Multi_Monitor;
  procedure Finalize_Multi_Monitor is
    begin
      for Window of Multi_Monitor_Windows loop
        Assert_Dummy(Show_Window(Window, MAKE_WINDOW_HIDE));
        Assert(Destroy_Window(Window));
      end loop;
      Assert(Unregister_Class(To_String_2_C(To_String_2(Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME), NULL_ADDRESS));
    end Finalize_Multi_Monitor;
  procedure Initialize is
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Callback_Window);
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address is
      Point : Record_Point;
      begin
        case Message is
          when EVENT_CLOSE     => Post_Quit_Message(0); return Integer_Address(C_FALSE);
          when EVENT_COMMAND   => if Data_Unsigned = SUBEVENT_MENU_POPOUT or Data_Unsigned = SUBEVENT_SCREEN_SAVER_START then return 0; end if;
          when EVENT_CHARACTER =>
            if (Get_Key_State(Integer_4_Signed_C(VIRTUAL_KEY_CONTROL)) and 16#8000#) = 0 then
              Inject((Text_Kind, Text => To_String_2_Unbounded(Character_2'val(Integer_4_Signed(Data_Unsigned))), others => <>));
            end if;
          when EVENT_ACTIVATION_CHANGE =>
            if    (Data_Unsigned and 16#0000_FFFF#) = 0 or (Data_Unsigned and 16#FFFF_0000#) /= 0 then Activate(False, False, Integer_8_Signed(Point.X), Integer_8_Signed(Point.Y));
            elsif (Data_Unsigned and 16#0000_FFFF#) = SUBEVENT_CLICK_ACTIVATION then                   Activate(True,  True,  Integer_8_Signed(Point.X), Integer_8_Signed(Point.Y));
            else                                                                                       Activate(True,  False, Integer_8_Signed(Point.X), Integer_8_Signed(Point.Y));
            end if;
          when EVENT_SIZE_CHANGED =>
            case Data_Unsigned is
              when SUBEVENT_ICONIZED     => Change_State(Iconic_Change);
              when SUBEVENT_FULLSCREENED => Change_State(Fullscreen_Change);
            when others => null; end case;
            return 0;
          when EVENT_SIZING =>
            declare
            Screen : Access_Record_Rectangle := To_Unchecked_Access_Record_Rectangle(Data_Signed);
            Result : Record_Border           :=
              Resize(
                Border =>(
                  Left   => Integer_8_Signed(Screen.Left),
                  Right  => Integer_8_Signed(Screen.Right),
                  Top    => Integer_8_Signed(Screen.Top),
                  Bottom => Integer_8_Signed(Screen.Bottom)),
                Kind =>(case Data_Unsigned is
                  when SUBEVENT_RESIZE_LEFT         => Left_Resize,
                  when SUBEVENT_RESIZE_RIGHT        => Right_Resize,
                  when SUBEVENT_RESIZE_TOP          => Top_Resize,
                  when SUBEVENT_RESIZE_TOP_LEFT     => Top_Left_Resize,
                  when SUBEVENT_RESIZE_TOP_RIGHT    => Top_Right_Resize,
                  when SUBEVENT_RESIZE_BOTTOM       => Bottom_Resize,
                  when SUBEVENT_RESIZE_BOTTOM_LEFT  => Bottom_Left_Resize,
                  when SUBEVENT_RESIZE_BOTTOM_RIGHT => Bottom_Right_Resize,
                  when others                       => Other_Resize));
            begin
              Screen.all :=(
                Left   => Integer_4_Signed_C(Result.Left),
                Right  => Integer_4_Signed_C(Result.Right),
                Top    => Integer_4_Signed_C(Result.Top),
                Bottom => Integer_4_Signed_C(Result.Bottom));
            end;
        when others => null; end case;
        return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    Class : aliased Record_Window_Class := (others => <>);
    begin
      Icon := Load_Image(
        Instance  => Get_Current_Instance,
        Name      => To_Access_Constant_Character_2_C(PATH_ASSETS & "\ico\" & NAME_ICON & ".ico"),
        Kind      => LOAD_ICO,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_ADDRESS then Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON); end if;
      Cursor_Inactive := Load_Image(
        Instance  => Get_Current_Instance,
        Name      => To_Access_Constant_Character_2_C(PATH_ASSETS & "\cur\" & NAME_CURSOR_INACTIVE & ".cur"),
        Kind      => LOAD_CUR,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Cursor_Inactive = NULL_ADDRESS then Cursor_Inactive := Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR); end if;
      Cursor_Active := Load_Image(
        Instance  => Get_Current_Instance,
        Name      => To_Access_Constant_Character_2_C(PATH_ASSETS & "\cur\" & NAME_CURSOR_ACTIVE & ".cur"),
        Kind      => LOAD_CUR,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE);
      if Cursor_Active = NULL_ADDRESS then Cursor_Active := Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR); end if;
      Class :=(
        Callback   => Callback_Window'address,
        Instance   => Get_Current_Instance,
        Icon_Small => Icon,
        Icon_Large => Icon,
        Cursor     => Cursor_Inactive,
        Background => BRUSH_GRAY,
        Class_Name => To_Access_Constant_Character_2_C(Neo.System.SPECIFICS.Name),
        others     => <>);
      Assert(Register_Class(Class'unchecked_access) /= Integer_2_Unsigned_C(FAILED));
    end Initialize;
  function Update return Boolean is
    Message : aliased Record_Message := (others => <>);
    begin
      while Peek_Message(
        Message        => Message'unchecked_access,
        Window         => NULL_ADDRESS,
        Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
        Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
        Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
      loop
        if Message.Data = MESSAGE_QUIT then return False; end if;
        Assert_Dummy(Translate_Message(Message'unchecked_access));
        Assert_Dummy(Dispatch_Message(Message'unchecked_access));
      end loop;
      return True;
    end Update;
  procedure Finalize is
    begin
      if Primary_Window /= NULL_ADDRESS then
        Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_HIDE) = 0);
        Assert_Dummy(Destroy_Window(Primary_Window));
      end if;
      Assert_Dummy(Unregister_Class(To_String_2_C(Neo.System.SPECIFICS.Name), NULL_ADDRESS));
      Primary_Window := NULL_ADDRESS;
    end Finalize;
end Import;
