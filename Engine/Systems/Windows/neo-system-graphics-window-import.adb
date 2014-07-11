with Neo.Link.Windows; use Neo.Link.Windows;
separate(Neo.System.Window) package body Import is
    FULLSCREEN_STYLE : constant Integer_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBAR_LESS_AND_BORDERLESS;
    WINDOWED_STYLE   : constant Integer_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBAR or STYLE_BORDER_THIN_LINE or STYLE_BORDER_SIZABLE or STYLE_BOX_ICONIZE;
    Class_Title           :         Access_String_2      := null;
    Operating_System_Name :         Access_String_2      := null;
    Multi_Monitor_Windows :         Access_Array_Address := null;
    Mouse_Default         :         Address              := NULL_ADDRESS;
    Mouse                 :         Address              := NULL_ADDRESS;
    Mouse_Inactive        :         Address              := NULL_ADDRESS;
    Icon                  :         Address              := NULL_ADDRESS;
    Original_Clip         : aliased Record_Rectangle     := <>;
    procedure Iconize is begin Dummy_Assert(Show_Window(Primary_Window, MAKE_WINDOW_GO_TO_ICONIC) = 0); end Iconize;
    procedure Initialize(Class_Name : in String_2; Icon_Path   : in String_2; Cursor_Path : in String_2) is
      function Window_Callback(Window : in Address; Message, Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C; pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(Window : in Address; Message, Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
        Point : Record_Point;
        begin
          case Message is
            when EVENT_COMMAND => if Data_Unsigned = SUBEVENT_MENU_POPOUT or Data_Unsigned = SUBEVENT_SCREEN_SAVER_START then return C_FALSE; end if;
            when EVENT_CLOSE => Post_Quit_Message(0); return C_FALSE;
            when EVENT_MOVE =>
                declare
                Screen : Record_Rectangle;
                begin
                  Assert(Get_Window_Rectangle(Window, Screen'access));
                  Handle_Window_Move(
                    Window_X => Integer_4_Signed(Screen.Left),
                    Window_Y => Integer_4_Signed(Screen.Top),
                    Screen_X =>
                      Integer_4_Signed( -- Extract low
                        To_Integer_2_Signed(
                          Integer_2_Unsigned(
                            To_Integer_4_Unsigned(Data_Signed) and 16#0000_FFFF#)))),
                    Screen_Y =>
                      Integer_4_Signed( -- Extract high
                        To_Integer_2_Signed(
                          Integer_2_Unsigned(
                             Shift_Right(To_Integer_4_Unsigned(Data_Signed), Integer_2_Unsigned'size)))));
                end;
            when EVENT_ACTIVATION_CHANGE =>
              if (Data_Unsigned and SUBEVENT_WORD_LOW) = Integer_4_Unsigned_C(C_FALSE) or (Data_Unsigned and SUBEVENT_WORD_HIGH) /= 0 then
                Handle_Activation(False, False, Integer_4_Signed(Point.X), Integer_4_Signed(Point.Y));
              elsif (Data_Unsigned and SUBEVENT_WORD_LOW) = SUBEVENT_ACTIVATED_BY_CLICK then
                Handle_Activation(True, True, Integer_4_Signed(Point.X), Integer_4_Signed(Point.Y));
              else
                Handle_Activation(True, False, Integer_4_Signed(Point.X), Integer_4_Signed(Point.Y));
              end if;
            when EVENT_SIZE_CHANGED =>
              case Data_Unsigned is
                when SUBEVENT_ICONIZED     => Handle_State_Change(Iconic_Change);
                when SUBEVENT_FULLSCREENED => Handle_State_Change(Fullscreen_Change);
              when others => null; end case;
              return C_FALSE;
            when EVENT_SIZING =>
                declare
                Screen : Access_Record_Rectangle := To_Access_Record_Rectangle(Data_Signed);
                Result : Record_Window_Border    :=
                  Handle_Resize(
                    Current_Screen  =>(
                      Left   => Integer_4_Signed(Screen.Left),
                      Right  => Integer_4_Signed(Screen.Right),
                      Top    => Integer_4_Signed(Screen.Top),
                      Bottom => Integer_4_Signed(Screen.Bottom))
                    Resize_Location =>(
                      case Data_Unsigned is
                        when SUBEVENT_RESIZE_TOP          => Top_Resize
                        when SUBEVENT_RESIZE_LEFT         => Left_Resize
                        when SUBEVENT_RESIZE_RIGHT        => Right_Resize
                        when SUBEVENT_RESIZE_BOTTOM       => Bottom_Resize
                        when SUBEVENT_RESIZE_TOP_LEFT     => Top_Right_Resize
                        when SUBEVENT_RESIZE_BOTTOM_LEFT  => Bottom_Left_Resize
                        when SUBEVENT_RESIZE_BOTTOM_RIGHT => Bottom_Right_Resize
                        when others                       => goto Ignore_Resize));
                begin
                  Screen.all :=(
                    Left   => Integer_4_Signed_C(Result.Left),
                    Right  => Integer_4_Signed_C(Result.Right),
                    Top    => Integer_4_Signed_C(Result.Top),
                    Bottom => Integer_4_Signed_C(Result.Bottom));
                end Handle_Sizing;
          when others => null; end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Operating_System :         String_2            := Get_Operating_System_Name;
      Class            : aliased Record_Window_Class := (others => <>);
      begin
        Operating_System_Name     := new String_2(1..Operating_System'length);
        Operating_System_Name.all := Operating_System;
        Class_Title               := new String_2(1..Class_Name'length);
        Class_Title.all           := Class_Name;
        Icon :=
          Load_Image(
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Icon_Path),
            Kind      => LOAD_ICO,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE);
        if Icon = NULL_ADDRESS then
          Put_Debug("Warning, could not find an application icon at " & Icon_Path & ", loading default.");
          Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON);
        end if;
        Mouse :=
          Load_Image(
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Cursor_Path),
            Kind      => LOAD_CUR,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE);
        if Mouse = NULL_ADDRESS then
          Put_Line("Warning, could not find cursor at path " & Cursor_Path & " to load.");
          Mouse := Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR);
        else
          Do_Disable_Setting_Of_Cursor := False;
        end if;
        Class :=(
          Size       => Record_Window_Class'size / Byte'size,
          Style      => 0,
          Callback   => Window_Callback'access,
          Extra_A    => 0,
          Extra_B    => 0,
          Instance   => Get_Current_Instance,
          Icon_Small => Icon,
          Icon_Large => Icon,
          Cursor     => Mouse,
          Background => BRUSH_GRAY,
          Menu_Name  => null,
          Class_Name => To_Access_Constant_Character_2_C(Class_Title.all));
        Assert(Register_Class(Class'access) = Integer_2_Unsigned_C(FAILED));
        Primary_Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Title.all),
            Window_Name => To_String_2_C(Class_Title.all),
            Style       => WINDOWED_STYLE, --0,
-- Style has to be set to WINDOWED_STYLE otherwise the titlebar icon is not loaded in the Aero theme
-- http://social.msdn.microsoft.com/Forums/en-US/windowscompatibility/thread/7b5ef777-ff0d-4f15-afed-5588f93f0e23/
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        Assert(Primary_Window);
        Dummy_Assert(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0);
        Assert(Update_Window(Primary_Window));
      end Initialize;
    procedure Finalize is
      begin
        if Primary_Window /= NULL_ADDRESS then
          Dummy_Assert(Show_Window(Primary_Window, MAKE_WINDOW_HIDE) = 0);
          Assert(Destroy_Window(Primary_Window));
        end if;
        Primary_Window := NULL_ADDRESS;
        -- Free strings
      end Finalize;
    procedure Initialize_Multi_Monitor(Monitors : in Array_Record_Monitor) is
      function Window_Callback(Window : in Address; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(Window : in Address; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
        begin
          case Message is
            when EVENT_CLOSE => Post_Quit_Message(0); return C_FALSE;
          when others => null; end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Multi_Monitor_Window :         Address             := NULL_ADDRESS;
      Class                : aliased Record_Window_Class :=(
        Size       => Record_Window_Class'size / Byte'size,
        Style      => 0,
        Callback   => Window_Callback'access,
        Extra_A    => 0,
        Extra_B    => 0,
        Instance   => Get_Current_Instance,
        Icon_Small => Icon,
        Icon_Large => Icon,
        Cursor     => Mouse,
        Background => BRUSH_GRAY_TEXT,
        Menu_Name  => null,
        Class_Name => To_Access_Constant_Character_2_C(Class_Title.all & Localize(MULTI_MONITOR_NAME)));
      begin
        Assert(Register_Class(Class'access) = Integer_2_Unsigned_C(FAILED));
        Multi_Monitor_Windows := new Array_Address(1..Monitors'length - 1);
          declare
          J                     : Integer_4_Signed     := 1;
          Primary_Window_Border : Record_Window_Border := Get_Window_Border;
          begin
            for I in 1..Monitors'length loop
              if Primary_Window_Border /= Monitors(I).Desktop then
                Multi_Monitor_Windows(J) :=
                  Create_Window(
                    Style_Extra => 0,
                    Style       => FULLSCREEN_STYLE or STYLE_NO_ACTIVATE;
                    X           => Integer_4_Signed_C(Monitors(I).Desktop.Left),
                    Y           => Integer_4_Signed_C(Monitors(I).Desktop.Top),
                    Width       => Integer_4_Signed_C(Monitors(I).Desktop.Right  - Monitors(I).Desktop.Left),
                    Height      => Integer_4_Signed_C(Monitors(I).Desktop.Bottom - Monitors(I).Desktop.Top),
                    Parent      => NULL_ADDRESS,
                    Menu        => NULL_ADDRESS,
                    Instance    => Get_Current_Instance,
                    Parameter   => NULL_ADDRESS,
                    Class_Name  => To_String_2_C(Class_Title.all & MULTI_MONITOR_NAME),
                    Window_Name => To_String_2_C(Class_Title.all & Integer_4_Signed'Wide_Image(I)));
                Assert(Multi_Monitor_Windows(J));
                Dummy_Assert(Show_Window(Multi_Monitor_Windows(J), MAKE_WINDOW_NORMALIZE) = 0);
                Assert(Update_Window(Multi_Monitor_Windows(J)));
                J := J + 1;
              end if;
            end loop;
          end;
        Dummy_Assert(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0);
        Assert(Update_Window(Primary_Window));
      end Initialize_Multi_Monitor;
    procedure Finalize_Multi_Monitor is
      begin
        for I in 1..Multi_Monitor_Windows'length loop
          Assert(Show_Window(Multi_Monitor_Windows(I), MAKE_WINDOW_HIDE) = 0);
          Assert(Destroy_Window(Multi_Monitor_Windows(I)));
        end loop;
        Assert(Unregister_Class(
          Window_Class => NULL_ADDRESS,
          Class_Name   => To_String_2_C(
            Class_Title.all & SEPORATOR_LEFT & MULTI_MONITOR_NAME & SEPORATOR_RIGHT)));
        -- Free Multi_Monitor_Windows
      end Finalize_Multi_Monitor;
    function Get_Monitors return Array_Record_Monitor is
      Count : Integer_4_Natural := 0;
      function Monitor_Count_Callback(Monitor Device_Context : in Address; Screen : in Access_Record_Rectangle; Data: in Integer_4_Signed_C) return Integer_4_Signed_C is
        begin Count := Count + 1; return C_TRUE; end Monitor_Count_Callback;
      function Monitor_Callback(Monitor Device_Context : in Address; Screen : in Access_Record_Rectangle; Monitors : in Access_Array_Record_Monitor)) return Integer_4_Signed_C is
        Monitor_Information : aliased Record_Monitor_Information := (others => <>);
        begin
          Assert(Get_Monitor_Information(Monitor, Monitor_Information'access)));
          Monitors(Count) :=(
            Desktop =>(
              Left   => Integer_4_Signed(Monitor_Information.Monitor.Left),
              Right  => Integer_4_Signed(Monitor_Information.Monitor.Right),
              Top    => Integer_4_Signed(Monitor_Information.Monitor.Top),
              Bottom => Integer_4_Signed(Monitor_Information.Monitor.Bottom)),
            Work_Area =>(
              Left   => Integer_4_Signed(Monitor_Information.Work_Area.Left),
              Right  => Integer_4_Signed(Monitor_Information.Work_Area.Right),
              Top    => Integer_4_Signed(Monitor_Information.Work_Area.Top),
              Bottom => Integer_4_Signed(Monitor_Information.Work_Area.Bottom)));
          Count := Count + 1;
          return C_TRUE;
        end Monitor_Callback;
      begin
        Assert(Enumerate_Display_Monitor(
          Device_Context => NULL_ADDRESS,
          Clip           => NULL_ADDRESS,
          Callback       => Monitor_Count_Callback'access,
          Data           => 0));
        Assert(Count = 0);
          declare
          Monitors : Array_Record_Monitor(1..Count);
          begin
            Count := 1; -- It becomes an index
            Assert(Enumerate_Display_Monitor(
              Device_Context => NULL_ADDRESS,
              Clip           => NULL_ADDRESS,
              Callback       => Monitor_Callback'access,
              Data           => Monitors'access));
            return Monitors;
          end;
      end Get_Monitors;
    function Get_Window_Border return Record_Window_Border is
      Screen : aliased Record_Rectangle := <>;
      begin
        Assert(Get_Window_Rectangle(Primary_Window, Screen'access));
        return((
          Left   => Integer_4_Signed(Screen.Left),
          Right  => Integer_4_Signed(Screen.Right),
          Top    => Integer_4_Signed(Screen.Top),
          Bottom => Integer_4_Signed(Screen.Bottom)));
      end Get_Window_Border;
    function Get_Screen_Border return Record_Window_Border is
      Decoration : aliased Record_Rectangle   := (others => <>);
      Screen     : aliased Record_Rectangle   := (others => <>);
      Border     :         Integer_4_Signed_C := 0;
      Top        :         Integer_4_Signed_C := 0;
      begin
        Assert(Adjust_Window_Rectangle(
          Rectangle   => Decoration'access,
          Style       => WINDOWED_STYLE,
          Menu        => C_FALSE,
          Extra_Style => 0));
        Assert(Get_Window_Rectangle(Primary_Window, Screen'access));
        Border := (Decoration.Right  - Decoration.Left) / 2;
        Top    := (Decoration.Bottom - Decoration.Top) - Border;
        return((
          Left   => Integer_4_Signed(Screen.Left   + Border),
          Right  => Integer_4_Signed(Screen.Right  - Border),
          Top    => Integer_4_Signed(Screen.Top    + Top),
          Bottom => Integer_4_Signed(Screen.Bottom - Border)));
      end Get_Screen_Border;
    procedure Set_Custom_Mouse(Do_Restore_System_Mouse : in Boolean := False) is
      begin
        if not Do_Disable_Setting_Of_Cursor then
          if Do_Restore_System_Mouse then
            Assert(Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR))) = 0);
          else
            Assert(Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(Mouse)) = 0);
          end if;
        end if;
      end Set_Custom_Mouse;
    function Handle_Events(Index : in Integer_4_Natural := 0) return Boolean is
      Message : aliased Record_Message := (others => <>);
      Window  :         Address        := (if Index /= 0 then Multi_Monitor_Windows(Index) else Primary_Window);
      begin
        if Index /= 0 and then Multi_Monitor_Windows = null or Index > Multi_Monitor_Windows'size then
          Put_Debug_Line(Localize("Handle_Events is being passed bad indexes!"));
          return False;
        end if;
        if Peek_Message(
          Message        => Message'access,
          Window         => Window,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        then
          if Message.Data = MESSAGE_QUIT then return False; elsif
          Dummy_Assert(Translate_Message(Message'access) < 2);
          Dummy_Assert(Dispatch_Message(Message'access) = 0);
        end if;
        return True;
      end Handle_Events;
    procedure Adjust(X Y : in Integer_4_Signed; Title : in String_2; Width Height : in Integer_4_Positive; Do_Fullscreen : in Boolean) is
      begin
        Assert(Primary_Window);
        if Do_Fullscreen then
          Assert(Change_Window_Setting(Primary_Window, SET_WINDOW_STYLE, FULLSCREEN_STYLE) = 0);
          Assert(Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_EVERYTHING,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0));
        else
          -- Use of the 'window style' that allows fullscreen boxes on windowed applications also enables
          -- Aero-Snap(R) in newer versions of Windows, which breaks resizing higher up.
          if Get_Version < Windows_2_6_System then
            Assert(Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE));
          else
            Assert(Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE or STYLE_BOX_FULLSCREEN));
          end if;
          Assert(Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_APPLICATIONS,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0));
        end if;
        Assert_Dummy(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE))
        Assert(Update_Window(Primary_Window));
      end Adjust;
    function Is_Only_Instance(Name : in String_2) return Boolean is
      Handle : Address := NULL_ADDRESS;
      Window : Address := NULL_ADDRESS;
      begin
        Handle := Create_Mutex(NULL_ADDRESS, C_TRUE, To_String_2_C(Name));
        if Get_Last_Error /= NO_ERROR then
          Assert(Release_Mutex(Handle));
          Primary_Window := Find_Window(To_String_2_C(Name), NULL_ADDRESS);
          if Primary_Window /= NULL_ADDRESS then
            Dummy_Assert(Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0);
            Handle := Set_Focus(Primary_Window);
            Assert(Set_Foreground_Window(Primary_Window));
            Handle := Set_Active_Window(Primary_Window);
          end if;
          return False;
        end if;
        return True;
      end Is_Only_Instance;
    procedure Move_Topmost is
      function Is_Covered return Boolean is
        Clip           : aliased Record_Rectangle := (others => <>);
        Client         : aliased Record_Rectangle := (others => <>);
        Result         :         Boolean          := False;
        Device_Context :         Address          := Get_Device_Context(Primary_Window);
        begin
          Assert(Device_Context);
          case Get_Clip_Box(Device_Context, Clip'access) is
            when REGION_null | REGION_COMPLEX => Result := True;
            when REGION_SIMPLE =>
              Assert(Get_Client_Rectangle(Primary_Window, Client'access));
              Assert(Rectangles_Are_Equal(Client'access, Clip'access) = C_FALSE);
          when others => raise Call_Failure; end case;
          Assert(Release_Device_Context(Primary_Window, Device_Context));
          return Result;
        end Is_Covered;
      begin
        if Is_Covered then
          -- loop through windows with higher Z value
            -- Find_Intersecting_Rectangle
              -- Make it move behind, if that fails minimize it, if that fails, move it
            -- if it is not covered exit
          null;
        end if;
      end Move_Topmost;
    procedure Hide_Mouse(Do_Hide : in Boolean; Do_Ignore_Titlebar : in Boolean := False) is
      procedure Clip_Mouse(Do_Clip : in Boolean; Do_Ignore_Titlebar : in Boolean) is
        Border        :         Record_Window_Border := Get_Screen_Border;
        New_Clip_Area : aliased Record_Rectangle :=(
          Left   => Integer_4_Signed_C(Border.Left),
          Top    => Integer_4_Signed_C(Border.Top),
          Right  => Integer_4_Signed_C(Border.Right),
          Bottom => Integer_4_Signed_C(Border.Bottom));
        begin
          if Do_Ignore_Titlebar then
            Assert(Get_Window_Rectangle(Primary_Window, New_Clip_Area'access));
          end if;
          if not Do_Clip then
            if Original_Clip /= (others => <>) then
              Assert(Clip_Cursor(Original_Clip'access));
              Original_Clip := (others => <>);
            end if;
          else
            if Original_Clip = (others => <>) then
              Assert(Get_Clip_Cursor_Area(Original_Clip'access));
              Assert(Original_Clip /= (others => <>));
            end if;
            Assert(Clip_Cursor(New_Clip_Area'access));
          end if;
        end Clip_Mouse;
      begin
        if Do_Hide then
          while Show_Cursor(Boolean'pos(not Do_Hide)) > -1 loop null; end loop;
          if not Do_Disable_Blank_Mouse then
            if Previous_Mouse = NULL_ADDRESS then
              Previous_Mouse := To_Address(Get_Class_Setting(Primary_Window, GET_CLASS_CURSOR));
              Assert(Previous_Mouse);
            end if;
            Assert(Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Blank_Mouse)) = 0);
          end if;
          Clip_Mouse(True, Do_Ignore_Titlebar);
        else
          Assert(not Do_Disable_Blank_Mouse and then Assert(Previous_Mouse) and then
            Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Previous_Mouse)) = 0);
          Clip_Mouse(False, False);
          while Show_Cursor(Boolean'pos(not Do_Hide)) < 0 loop null; end loop;
        end if;
      end Hide_Mouse;
    function Get_Specifics return Record_Specifics is
      function Callback(Window : Address; Message : Data_Unsigned : Integer_4_Unsigned_C; Data_Signed : Integer_4_Signed_C) return Integer_4_Signed_C is
        Device_Context    : Record_Device_Context    := 0;
        Rendering_Context : Record_Rendering_Context := hGLRC;
        begin
          if Message = EVENT_DESTROY then Post_Quit_Message(0); end if;
          if Message /= EVENT_CREATE then return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed); end if;
          Device_Context := Get_Device_Context(Window);
          Set_Pixel_Format(
            Device_Context => Device_Context,
            Format         => Choose_Pixel_Format(Device_Context, Pixel_Format'address),
            Descriptor     => Pixel_Format'address);
          Rendering_Context := OpenGL_Create_Context(Device_Context);
          Make_Current(Device_Context, Rendering_Context);
          Make_Current(NULL_ADDRESS, NULL_ADDRESS);
          Delete_Context(Rendering_Context);
          Release_Device_Context(Window, Device_Context);
          return 1;
        end Faux_Callback;
        pragma Convention(Stdcall, Callback);
      Message           : Record_Message := NULL_RECORD_MESSAGE;
      Window            : Address        := NULL_ADDRESS;
      Device_Context    : Address        := NULL_ADDRESS;
      Rendering_Context : Address        := NULL_ADDRESS;
      Context : Address := NULL_ADDRESS;
      begin
        Context        := Get_Device_Context(Get_Desktop_Window);
        Bits_Per_Pixel := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_NUMBER_OF_BITS_PER_PIXEL));
        Native_Width   := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_HORZONTAL_RESOLUTION));
        Native_Height  := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION));
        Assert(Release_Device_Context(Get_Desktop_Window, Context));
        Assert(Create_Class(Callback'address, " "));
        Window := Create_Window(
          Class_Name  => WIN32_FAKE_WINDOW_CLASS_NAME,
          Window_Name => GAME_NAME,
          Style       => WS_OVERLAPPEDWINDOW);
        Assert(Window = NULL_ADDRESS);
        Device_Context    := Get_Device_Context(Window);
        Rendering_Context := Create_Rendering_Context(Device_Context);
        Make_Current(Device_Context, Rendering_Context);
        Operating_System_Supports_Swap_Control_Tear := Index(, "WGL_EXT_swap_control_tear") /= 0;
        Release_Device_Context(Window, Device_Context);
        while Get_Message(Message'address, NULL_ADDRESS, 0, 0) loop
          Result := Translate_Message(Message'address);
          Result := Dispatch_Message(Message'address);
        end loop;
        Device_Context := Get_Device_Context(Window);
        Assert(Device_Context);
        if Choose_Pixel_Format /= null and Multi_Samples > 1 then
          Choose_Pixel_Format(
            Device_Context     => Device_Context,
            Attributes_Integer =>(
            WGL_SAMPLE_BUFFERS_ARB, 1,
            WGL_SAMPLES_ARB,        Multi_Samples,
            WGL_DOUBLE_BUFFER_ARB,  1,
            WGL_STENCIL_BITS_ARB,   8,
            WGL_DEPTH_BITS_ARB,     24,
            WGL_RED_BITS_ARB,       8,
            WGL_BLUE_BITS_ARB,      8,
            WGL_GREEN_BITS_ARB,     8,
            WGL_ALPHA_BITS_ARB,     8, 0, 0),
            Attributes_Float  => (0.0, 0.0),
            Pixel_Format      => Pixel_Format'address,
            Number_Of_Formats => null);
        else
          Assert(Choose_Pixel_Format(Device_Context, FORMAT'address));
        end if;
        Describe_Pixel_Format(Device_Context, Pixel_Format, Pixel_Descriptor'size / Byte'size, Pixel_Descriptor'address);
        Color_Bits   := Pixel_Descriptor.Color_Bits;
        Depth_Bits   := Pixel_Descriptor.Depth_Bits;
        Stencil_Bits := (if Pixel_Descriptor.Stencil_Bits /= 0 then Pixel_Descriptor.Stencil_Bits else Byte'size); -- Windows XP seems to set this incorrectly
        Assert(Set_Pixel_Format(Device_Context, Pixel_Format, &win32.pfd));
        Assert(Create_Context(Device_Context));
      end Get_Specifics;
  end Import;
