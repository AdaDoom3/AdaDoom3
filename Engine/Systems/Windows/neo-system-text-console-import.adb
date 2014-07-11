-- Clip board
-- Insertion
-- Scroll bar
-- Minimum size of buttons
-- Set sizes on res change
with Neo.Link.Windows;             use Neo.Link.Windows;
with Interfaces.C;                 use Interfaces.C;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
separate(Neo.System.Text.Console) package body Import is -- "Abandon all hope, you who enter here"
    procedure Run
      is
      DO_DISABLE_RESIZE       : constant Boolean                  := False;
      FONT_GROUP_BOX_SIZE     : constant Float                    := 1.2;
      FONT_CONSOLE_SIZE       : constant Float                    := 1.1; -- The amount to divide 1 pt size by
      FONT_CONSOLE            : constant String_2                 := "Courier New";
      FONT_DIALOG             : constant String_2                 := "Tahoma";
      NAME_BUTTON             : constant String_2_C               := To_String_2_C("Button");
      NAME_GROUP              : constant String_2_C               := To_String_2_C("Group");
      NAME_EDIT               : constant String_2_C               := To_String_2_C("Edit");
      IDENTIFIER_START        : constant Integer_4_Signed         := 16#0000_0666#;
      PIXELS_PER_INCH         : constant Integer_4_Signed_C       := 72;
      BUTTON_WIDTH_DLU        : constant Integer_4_Signed_C       := 50;
      BUTTON_HEIGHT_DLU       : constant Integer_4_Signed_C       := 14;
      MARGIN                  : constant Integer_4_Signed_C       := 7;
      MARGIN_GROUP_SIDE       : constant Integer_4_Signed_C       := 6;
      MARGIN_BUTTON           : constant Integer_4_Signed_C       := 4;
      MARGIN_CRUD             : constant Integer_4_Signed_C       := Get_System_Metrics(7);
      Hack                    : aliased String_2_C                := To_String_2_C("SINISTER HACK TO GET DIALOG BASE UNITS");
      Name_Class              : aliased String_2_C                := To_String_2_C(SPECIFICS.Name & NAME_POSTFIX_CONSOLE);
      Size                    : aliased Record_Size               := (others => <>);
      Message                 : aliased Record_Message            := (others => <>);
      Text_Metric             : aliased Record_Text_Metric        := (others => <>);
      Class                   : aliased Record_Window_Class       := (others => <>);
      Non_Client_Metrics      : aliased Record_Non_Client_Metrics := (others => <>);
      Scroll_Information      : aliased Record_Scroll_Information := (others => <>);
      Margin_Group_Top        :         Integer_4_Signed_C        := 0;
      Title_Bar_Height        :         Integer_4_Signed_C        := 0;
      Message_Box_Font_Height :         Integer_4_Signed_C        := 0;
      Border_Width            :         Integer_4_Signed_C        := 0;
      Border_Height           :         Integer_4_Signed_C        := 0;
      Box_Padding             :         Integer_4_Signed_C        := 0;
      Scroll_Bar_Width        :         Integer_4_Signed_C        := 0;
      Button_Height           :         Integer_4_Signed_C        := 0;
      Button_Width            :         Integer_4_Signed_C        := 0;
      Pixels_Y                :         Integer_4_Signed_C        := 0;
      Pixels_X                :         Integer_4_Signed_C        := 0;
      No_Output_Width         :         Integer_4_Signed_C        := 1;
      No_Output_Height        :         Integer_4_Signed_C        := 1;
      Dialog_Base_Unit_Height :         Integer_4_Signed_C        := 0;
      Dialog_Base_Unit_Width  :         Integer_4_Signed_C        := 0;
      Text_Box_Font_Height    :         Integer_4_Signed_C        := 1;
      Text_Box_Font_Width     :         Integer_4_Signed_C        := 1;
      Dialog_Font_Height      :         Integer_4_Signed_C        := 1;
      Dialog_Font_Width       :         Integer_4_Signed_C        := 1;
      Y                       :         Integer_4_Signed_C        := 0;
      Output_Box_Height       :         Integer_4_Signed_C        := 0;
      Output_Box_Width        :         Integer_4_Signed_C        := 0;
      Input_Box_Height        :         Integer_4_Signed_C        := 0;
      Console_Height          :         Integer_4_Signed_C        := 0;
      Console_Width           :         Integer_4_Signed_C        := 0;
      Right_Count             :         Integer_4_Signed_C        := 0;
      Left_Count              :         Integer_4_Signed_C        := 0;
      Current_Lines           :         Integer_8_Unsigned        := 0;
      Was_At_Bottom : Boolean := False;
      Do_Process_Character    :         Boolean                   := False;
      Do_Skip_Message         :         Boolean                   := False;
      Current_Log             :         String_2_Unbounded        := NULL_STRING_2_UNBOUNDED;
      Current_Input           :         Character_2               := NULL_CHARACTER_2;
      Hook                    :         Address                   := NULL_ADDRESS;
      Message_Box_Window      :         Address                   := NULL_ADDRESS;
      Edit_Background         :         Address                   := NULL_ADDRESS;
      Output_Group_Box        :         Address                   := NULL_ADDRESS;
      Input_Group_Box         :         Address                   := NULL_ADDRESS;
      Font_Text_Box           :         Address                   := NULL_ADDRESS;
      Font_Buttons            :         Address                   := NULL_ADDRESS;
      Output_Box              :         Address                   := NULL_ADDRESS;
      Input_Box               :         Address                   := NULL_ADDRESS;
      Icon                    :         Address                   := NULL_ADDRESS;
      Context                 :         Address                   := NULL_ADDRESS;
      Console                 :         Address                   := NULL_ADDRESS;
      Buttons : array(CONSOLE_BUTTONS'range)of Address := (others => NULL_ADDRESS);
      function Is_Scroll_Bar_At_Bottom return Boolean is
        begin
          Assert_Dummy(Get_Scroll_Information(Console, 1, Scroll_Information'unchecked_access));
          --Put_Debug_Line(Integer_4_Signed_C'wide_image(Scroll_Information.Maximum));-- - Scroll_Information.Position));
          return Scroll_Information.Maximum = Scroll_Information.Position;
        end Is_Scroll_Bar_At_Bottom;
      procedure Wait_For_Hook_Set_With_Pseudo_Inifinite_Loop is
        begin
          for I in Integer_4_Signed'range loop -- The Windows API made me do it. I swear
            exit when Dialog_Base_Unit_Width /= 0;
            delay 0.0001;
          end loop;
          if Dialog_Base_Unit_Width = 0 then raise Call_Failure; end if;
        end Wait_For_Hook_Set_With_Pseudo_Inifinite_Loop;
      procedure Kill_It is
        begin
          Post_Quit_Message(0);
          Assert_Dummy(Show_Window(Console, MAKE_WINDOW_HIDE));
          Assert(Destroy_Window(Console));
          Assert(Unregister_Class(Name_Class, NULL_ADDRESS));
          Console := NULL_ADDRESS;
        end Kill_It;
      procedure Set_Sizes is
        Temp : Integer_4_Signed_C := 0;
        begin
          Title_Bar_Height  := Get_System_Metrics(DATA_TITLE_BAR_HEIGHT);
          Border_Width      := Get_System_Metrics(DATA_BORDER_WIDTH);
          Border_Height     := Get_System_Metrics(DATA_BORDER_HEIGHT);
          Box_Padding       := Text_Box_Font_Width / 2;--Integer_4_Signed_C(Float(Text_Box_Font_Width) / 1.5);
          Scroll_Bar_Width  := Get_System_Metrics(DATA_SCROLL_BAR_WIDTH);
          Output_Box_Width  := 2 * Box_Padding + (Text_Box_Font_Width * Integer_4_Signed_C(Get_Line_Size)) + Scroll_Bar_Width;
          Output_Box_Height := 2 * Box_Padding + (Text_Box_Font_Height * Integer_4_Signed_C(NUMBER_OF_OUTPUT_ROWS));
          Input_Box_Height  := 2 * Box_Padding + Text_Box_Font_Height;
          Console_Width     := Dialog_Base_Unit_Width * (MARGIN + MARGIN_GROUP_SIDE) * 2 + Output_Box_Width + Border_Width * 2;
          Console_Height    := Margin_Group_Top * 4 + Dialog_Base_Unit_Height * (MARGIN * 2 + MARGIN_BUTTON * 2) +
            Output_Box_Height + Border_Height * 2 + Input_Box_Height + BUTTON_HEIGHT + Title_Bar_Height;
          No_Output_Width   := Console_Width - Output_Box_Width;
          No_Output_Height  := Console_Height - Output_Box_Height;
        end Set_Sizes;
      procedure Set_Text_Box_Font is
        begin
        Font_Text_Box :=
          Create_Font(
            Height           => Integer_4_Signed_C(Float(-(Pixels_Y / PIXELS_PER_INCH)) / FONT_CONSOLE_SIZE),
            Width            => 0,
            Escapement       => 0,
            Orientation      => 0,
            Weight           => FONT_WEIGHT_LIGHT,
            Italic           => 0,
            Underline        => 0,
            Strike_Out       => 0,
            Character_Set    => DEFAULT_CHARACTER_SET,
            Output_Precision => FONT_OUT_DEFAULT_PRECISION,
            Clip_Precision   => FONT_CLIP_DEFAULT_PRECISION,
            Quality          => FONT_DEFAULT_QUALITY,
            Pitch_And_Family => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
            Face             => To_Access_Constant_Character_2_C(FONT_CONSOLE));
        Assert(Font_Text_Box);
        Assert_Dummy(Select_Object(Context, Font_Text_Box));
        Assert(Get_Text_Metrics(Context, Text_Metric'address));
        Text_Box_Font_Height := Text_Metric.Height;
        Assert(Get_Text_Extent_Point(
          Device_Context => Context,
          Text           => To_Access_Character_2_C("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
          Count          => 52,
          Size           => Size'address));
        Text_Box_Font_Width := Integer_4_Signed_C((Size.X / 26 + 1) / 2);
        end Set_Text_Box_Font;
      function Callback_Message_Box(Code : in Integer_4_Signed_C; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C;
        pragma Convention(Stdcall, Callback_Message_Box);
      function Callback_Message_Box(Code : in Integer_4_Signed_C; Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
        Class_Name  : aliased String_2_C(1..256) := (others => NULL_CHARACTER_2_C);
        Window_Text : aliased String_2_C(1..256) := (others => NULL_CHARACTER_2_C);
        Window      :         Address            := To_Unchecked_Address(Integer_Address(Data_Unsigned));
        function Callback_Message_Box_Children(Window : in Address; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C;
          pragma Convention(Stdcall, Callback_Message_Box_Children);
        function Callback_Message_Box_Children(Window : in Address; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
          Rectangle : aliased Record_Rectangle := (others => <>);
          begin
            Assert(Get_Class_Name(Window, Class_Name'Unrestricted_Access, Class_Name'length));
            Assert_Dummy(Get_Window_Text(Window, Window_Text'Unrestricted_Access, Window_Text'length) = 0);
            if Dialog_Base_Unit_Width = 0 and "Button" = To_String_2(Class_Name) then -- Get those pesky dialog base units
              Assert(Get_Window_Rectangle(Window, Rectangle'address));
              Button_Width            := (Rectangle.Right - Rectangle.Left);
              Button_Height           := (Rectangle.Bottom - Rectangle.Top);
              Assert_Dummy(Select_Object(Context, To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(
                Send_Message(Window, MESSAGE_GET_FONT, 0, 0))))));
              Assert(Get_Text_Metrics(Context, Text_Metric'address));
              Message_Box_Font_Height := Text_Metric.Height;
              Dialog_Base_Unit_Width  := Button_Width / BUTTON_WIDTH_DLU;
              Dialog_Base_Unit_Height := Button_Height / BUTTON_HEIGHT_DLU;
            end if;
            return C_TRUE;
          end Callback_Message_Box_Children;
        begin
          Assert(Get_Class_Name(Window, Class_Name'Unrestricted_Access, Class_Name'length));
          Assert_Dummy(Get_Window_Text(Window, Window_Text'Unrestricted_Access, Window_Text'length) = 0);
          if Code = COMPUTER_BASED_TRAINING_ACTIVATE and To_String_2(Class_Name) = CLASS_NAME_DIALOG and To_String_2(Window_Text) = To_String_2(Hack) then
            Assert_Dummy(Enumerate_Child_Windows(Window, Callback_Message_Box_Children'address, 0) = 0); -- I heard you like callbacks
            Wait_For_Hook_Set_With_Pseudo_Inifinite_Loop;
            Assert(Destroy_Window(Window));
            Assert(Unhook_Windows_Hook(Hook));
          else Assert_Dummy(Call_Next_Hook(Hook, Code, Data_Unsigned, Data_Signed) = 0); end if;
          return 0;
        end Callback_Message_Box;
      function Callback_Console(Window : in Address; Message, Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C;
        pragma Convention(Stdcall, Callback_Console);
      function Callback_Console(Window : in Address; Message, Data_Unsigned : in Integer_4_Unsigned_C; Data_Signed : in Integer_4_Signed_C) return Integer_4_Signed_C is
        Point : Record_Point;
        function To_Windows_Color(Color : in Record_Color) return Integer_4_Unsigned_C is
          begin
            return
              Integer_4_Unsigned_C(
                Integer_4_Unsigned(Color.Red) or
                Shift_Left(Integer_4_Unsigned(Color.Green), 8) or
                Shift_Left(Integer_4_Unsigned(Color.Blue), 16));
          end To_Windows_Color;
        begin
          case Message is
            when EVENT_CLOSE => Kill_It; return C_FALSE;
            when EVENT_COMMAND =>
              case Data_Unsigned is
                when SUBEVENT_MENU_POPOUT | SUBEVENT_SCREEN_SAVER_START => return C_FALSE;
              when others => null; end case;
            when EVENT_CREATE =>
              Edit_Background := Create_Solid_Brush(To_Windows_Color(COLOR_BACKGROUND));
              Assert(Edit_Background);
            when EVENT_CONTROL_STATIC_COLOR | EVENT_CONTROL_DYNAMIC_COLOR =>
              if To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Signed)) = Output_Box or
              To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Signed)) = Input_Box then
                Assert(Set_Background_Color(
                  Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
                  Color          => To_Windows_Color(COLOR_BACKGROUND)) /= INVALID_COLOR);
                Assert(Set_Text_Color(
                  Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
                  Color          => To_Windows_Color(COLOR_TEXT)) /= INVALID_COLOR);
                return To_Unchecked_Integer_4_Signed_C(To_Unchecked_Integer_Address(Edit_Background));
              end if;
            when EVENT_BUTTON_COMMAND =>
              for I in CONSOLE_BUTTONS'range loop
                if Data_Unsigned = Integer_4_Unsigned_C(I + IDENTIFIER_START) then
                  if CONSOLE_BUTTONS(I).Action = null then Kill_It; return C_FALSE;
                  else CONSOLE_BUTTONS(I).Action.all; end if;
                  exit;
                end if;
              end loop;
            when EVENT_GET_MINIMUM_MAXIMUM_SIZE_INFORMATION =>
              declare
              Minimum_Maximum_Information : Access_Record_Minimum_Maximum_Information :=
                To_Access_Record_Minimum_Maximum_Information(To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(Data_Signed))));
              begin
                Minimum_Maximum_Information.Minimum_Track_Size.X := Console_Width;
                Minimum_Maximum_Information.Minimum_Track_Size.Y := Console_Height;
              end;
              return C_FALSE;
            when EVENT_SIZE_CHANGED =>
              Assert_Dummy(Change_Window_Setting(
                Window  => Console,
                Command => -20,
                Setting => STYLE_EXTRA_COMPOSITED) /= 0);
              if Data_Unsigned /= SUBEVENT_ICONIZED then
                Set_Sizes;
                Output_Box_Width := -No_Output_Width + Border_Width * 2 +
                  Integer_4_Signed_C( -- Extract low
                    To_Unchecked_Integer_2_Signed(
                      Integer_2_Unsigned(
                        To_Unchecked_Integer_4_Unsigned(Data_Signed) and 16#0000_FFFF#)));
                Output_Box_Height := -No_Output_Height + Border_Height * 2 + Title_Bar_Height +
                  Integer_4_Signed_C( -- Extract high
                    To_Unchecked_Integer_2_Signed(
                      Integer_2_Unsigned(
                        Shift_Right(To_Unchecked_Integer_4_Unsigned(Data_Signed), Integer_2_Unsigned'size))));
                Y := Dialog_Base_Unit_Height * MARGIN;
                Assert(Set_Window_Position(
                  Window       => Output_Group_Box,
                  Insert_After => NULL_ADDRESS,
                  X            => MARGIN * Dialog_Base_Unit_Width,
                  Y            => Y,
                  Width        => Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2 -  MARGIN_CRUD,
                  Height       => Output_Box_Height + Dialog_Base_Unit_Height * MARGIN + Margin_Group_Top,
                  Flags        => 0));
                Y := Y + Margin_Group_Top;
                Assert(Set_Window_Position(
                  Window       => Output_Box,
                  Insert_After => NULL_ADDRESS,
                  X            => Dialog_Base_Unit_Width * (MARGIN + MARGIN_GROUP_SIDE),
                  Y            => Y,
                  Width        => Output_Box_Width,
                  Height       => Output_Box_Height,
                  Flags        => 0));
                Y := Y + Output_Box_Height + Dialog_Base_Unit_Height * MARGIN_BUTTON + Margin_Group_Top;
                Assert(Set_Window_Position(
                  Window       => Input_Group_Box,
                  Insert_After => NULL_ADDRESS,
                  X            => Dialog_Base_Unit_Width * MARGIN,
                  Y            => Y,
                  Height       => Input_Box_Height + Dialog_Base_Unit_Height * MARGIN + Margin_Group_Top,
                  Width        => Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2 -  MARGIN_CRUD,
                  Flags        => 0));
                Y := Y + Margin_Group_Top;
                Assert(Set_Window_Position(
                  Window       => Input_Box,
                  Insert_After => NULL_ADDRESS,
                  X            => Dialog_Base_Unit_Width * (MARGIN + MARGIN_GROUP_SIDE),
                  Y            => Y,
                  Width        => Output_Box_Width,
                  Height       => Input_Box_Height,
                  Flags        => 0));
                Y := Y + Input_Box_Height + Dialog_Base_Unit_Height * (MARGIN_BUTTON) + Margin_Group_Top;
                Right_Count := 0;
                Left_Count  := 0;
                for I in Buttons'range loop
                  Assert(Set_Window_Position(
                    Window       => Buttons(I),
                    Insert_After => NULL_ADDRESS,
                    Width        => Button_Width,
                    Height       => Button_Height,
                    Flags        => 0,
                    Y            => Y,
                    X            =>(
                      if CONSOLE_BUTTONS(I).Action /= null then -- Left justify
                        Dialog_Base_Unit_Width * (MARGIN + Left_Count * MARGIN_BUTTON) + Left_Count * BUTTON_WIDTH
                      else -- Right justify
                        MARGIN * Dialog_Base_Unit_Width + Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2 -
                        (Right_Count + 1) * BUTTON_WIDTH - MARGIN_CRUD)));
                  if CONSOLE_BUTTONS(I).Action = null then Right_Count := Right_Count + 1;
                  else Left_Count := Left_Count + 1; end if;
                end loop;
--                  if Is_Scroll_Bar_At_Bottom then
--                    Assert_Dummy(Send_Message(
--                      Window        => Output_Box,
--                      Message       => EVENT_SCROLL_VERTICALLY,
--                      Data_Unsigned => 7, -- SB_BOTTOM
--                      Data_Signed   => 0));
--                  end if;
                Assert(Redraw_Window(Window, NULL_ADDRESS, NULL_ADDRESS, 1));
                Assert_Dummy(Change_Window_Setting(
                  Window  => Console,
                  Command => -20,
                  Setting => 0) /= 0);
                return C_TRUE;
              end if;
            when 16#0000_007E# =>
              Set_Text_Box_Font;
        Assert(Send_Message(
          Window        => Input_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Text_Box))));
              Put_Debug_Line("Triggered!");
          when others => null; end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Callback_Console;
      begin
        Context := Get_Device_Context(Get_Desktop_Window);
        Assert(Context);
        Hook := Set_Windows_Hook(COMPUTER_BASED_TRAINING_HOOK, Callback_Message_Box'address, NULL_ADDRESS, Get_Current_Thread_Id);
        Assert_Dummy(Message_Box(NULL_ADDRESS, To_String_2_C(SPECIFICS.Name), Hack, 0) = 0);
        Wait_For_Hook_Set_With_Pseudo_Inifinite_Loop;
        Pixels_Y := Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION);
        Pixels_X := Get_Device_Capabilities(Context, DATA_HORIZONTAL_RESOLUTION);
        Icon :=
          Load_Image( -- Loads the icon nicely for the Aero theme, but on "classic" theme it looks pixelated on the title bar
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Get_Icon),
            Kind      => LOAD_ICO,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
        if Icon = NULL_ADDRESS then Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON); end if;
        Set_Text_Box_Font;
        Assert(System_Parameter_Information(
          Action       => GET_NON_CLIENT_METRICS,
          User_Profile => 0,
          Parameter_B  => Non_Client_Metrics'address,
          Parameter_A  =>(
            if SPECIFICS.Version >= Windows_2_6_System then Non_Client_Metrics.Size
            else Non_Client_Metrics.Size - Integer_4_Unsigned_C(Integer_4_Signed_C'size / Byte'size))));
        Font_Buttons := Create_Font_Indirect(Non_Client_Metrics.Message_Font'unchecked_access);
        Assert(Font_Buttons);
--          Font_Buttons :=
--            Create_Font(
--              Height           => Integer_4_Signed_C(Float(Message_Box_Font_Height) / 1.1),---(Pixels_Y / PIXELS_PER_INCH),
--              Width            => 0,
--              Escapement       => 0,
--              Orientation      => 0,
--              Weight           => FONT_WEIGHT_LIGHT,
--              Italic           => 0,
--              Underline        => 0,
--              Strike_Out       => 0,
--              Character_Set    => DEFAULT_CHARACTER_SET,
--              Output_Precision => FONT_OUT_DEFAULT_PRECISION,
--              Clip_Precision   => FONT_CLIP_DEFAULT_PRECISION,
--              Quality          => FONT_DEFAULT_QUALITY,
--              Pitch_And_Family => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
--              Face             => To_Access_Constant_Character_2_C(FONT_DIALOG));
--          Assert(Font_Buttons);
        Assert_Dummy(Select_Object(Context, Font_Buttons));
        Assert(Get_Text_Metrics(Context, Text_Metric'address));
        Margin_Group_Top := Integer_4_Signed_C(Float(Text_Metric.Height) * FONT_GROUP_BOX_SIZE);
        Set_Sizes;
        Class :=(
          Callback   => Callback_Console'address,
          Instance   => Get_Current_Instance,
          Icon_Small => Icon,
          Icon_Large => Icon,
          Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
          Background => BRUSH_WINDOW,
          Class_Name => To_Access_Constant_Character_2_C(To_String_2(Name_Class)),
          others     => <>);
        Assert(Register_Class(Class'unchecked_access) /= Integer_2_Unsigned_C(FAILED));
        Console :=
          Create_Window(
            Class_Name  => Name_Class,
            Window_Name => Name_Class,
            X           => Pixels_X / 2 - Console_Width  / 2,
            Y           => Pixels_Y / 2 - Console_Height / 2,
            Width       => Console_Width,
            Height      => Console_Height,
            Parent      => NULL_ADDRESS,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       => STYLE_ICONIC_INITIALLY or STYLE_BORDER_SIZABLE or STYLE_TITLEBAR_MENU or STYLE_BORDER_THIN_LINE or STYLE_BOX_ICONIZE or STYLE_BOX_FULLSCREEN);
        Assert(Console);
        Y := Dialog_Base_Unit_Height * MARGIN;
        Output_Group_Box :=
          Create_Window(
            Class_Name  => NAME_BUTTON,
            Window_Name => NULL_STRING_2_C,
            X           => MARGIN * Dialog_Base_Unit_Width,
            Y           => Y,
            Width       => Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2 -  MARGIN_CRUD,
            Height      => Output_Box_Height + Dialog_Base_Unit_Height * MARGIN + Margin_Group_Top,
            Parent      => Console,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       => STYLE_VISIBLE_INITIALLY or STYLE_GROUP_BOX or STYLE_CHILD);
        Assert(Output_Group_Box);
        Assert_Dummy(Send_Message(
          Window        => Output_Group_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Buttons))));
        Assert(Send_Message(
          Window        => Output_Group_Box,
          Message       => EVENT_SET_TEXT,
          Data_Unsigned => 0,
          Data_Signed   =>
            To_Unchecked_Integer_4_Signed_C(
              To_Unchecked_Integer_Address(
                To_Access_Constant_Character_2_C(Localize(LABEL_OUTPUT))))));
        Y := Y + Margin_Group_Top;
        Output_Box :=
          Create_Window(
            Class_Name  => NAME_EDIT,
            Window_Name => NULL_STRING_2_C,
            X           => Dialog_Base_Unit_Width * (MARGIN + MARGIN_GROUP_SIDE),
            Y           => Y,
            Width       => Output_Box_Width,
            Height      => Output_Box_Height,
            Parent      => Console,
            Menu        => 4,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       =>
              STYLE_HAS_VERTICAL_SCROLL_BAR   or
              STYLE_VISIBLE_INITIALLY         or
              STYLE_BORDER_THIN_LINE          or
              STYLE_ALIGN_TEXT_TO_LEFT        or
              STYLE_MULTI_LINE                or
              STYLE_AUTOMATIC_VERTICAL_SCROLL or
              STYLE_NO_USER_EDITING           or
              STYLE_CHILD);
        Assert(Output_Box);
        Assert(Send_Message(
          Window        => Output_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Text_Box))));
        Y := Y + Output_Box_Height + Dialog_Base_Unit_Height * ( MARGIN_BUTTON) + Margin_Group_Top;
        Input_Group_Box :=
          Create_Window(
            Class_Name  => NAME_BUTTON,
            Window_Name => NULL_STRING_2_C,
            X           => Dialog_Base_Unit_Width * MARGIN,
            Y           => Y,
            Height      => Input_Box_Height + Dialog_Base_Unit_Height * MARGIN + Margin_Group_Top -  MARGIN_CRUD,
            Width       => Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2,
            Parent      => Console,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       => STYLE_VISIBLE_INITIALLY or STYLE_GROUP_BOX or STYLE_CHILD);
        Assert(Input_Group_Box);
        Assert_Dummy(Send_Message(
          Window        => Input_Group_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Buttons))));
        Assert(Send_Message(
          Window        => Input_Group_Box,
          Message       => EVENT_SET_TEXT,
          Data_Unsigned => 0,
          Data_Signed   =>
            To_Unchecked_Integer_4_Signed_C(
              To_Unchecked_Integer_Address(
                To_Access_Constant_Character_2_C(Localize(LABEL_INPUT_ENTRY))))));
        Y := Y + Margin_Group_Top;
        Input_Box :=
          Create_Window(
            Class_Name  => NAME_EDIT,
            Window_Name => NULL_STRING_2_C,
            X           => Dialog_Base_Unit_Width * (MARGIN + MARGIN_GROUP_SIDE),
            Y           => Y,
            Width       => Output_Box_Width,
            Height      => Input_Box_Height,
            Parent      => Console,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       =>
              STYLE_VISIBLE_INITIALLY  or
              STYLE_BORDER_THIN_LINE   or
              STYLE_ALIGN_TEXT_TO_LEFT or
              STYLE_MULTI_LINE         or
              STYLE_CHILD);
        Assert(Input_Box);
        Assert(Send_Message(
          Window        => Input_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Text_Box))));
        Y := Y + Input_Box_Height + Dialog_Base_Unit_Height * MARGIN_BUTTON + Margin_Group_Top;
        for I in Buttons'range loop
          Buttons(I) :=
            Create_Window(
              Class_Name  => NAME_BUTTON,
              Window_Name => NULL_STRING_2_C,
              Width       => Button_Width,
              Height      => Button_Height,
              Parent      => Console,
              Menu        => Integer_Address(I + IDENTIFIER_START),
              Instance    => Get_Current_Instance,
              Parameter   => NULL_ADDRESS,
              Style_Extra => 0,
              Style       => STYLE_PUSH_BUTTON or STYLE_VISIBLE_INITIALLY or STYLE_CHILD,
              Y           => Y,
              X =>(
                if CONSOLE_BUTTONS(I).Action /= null then -- Left justify
                  Dialog_Base_Unit_Width * (MARGIN + Left_Count * MARGIN_BUTTON) + Left_Count * BUTTON_WIDTH
                else -- Right justify
                  MARGIN * Dialog_Base_Unit_Width + Output_Box_Width + Dialog_Base_Unit_Width * MARGIN * 2 - (Right_Count + 1) * BUTTON_WIDTH - MARGIN_CRUD)); -- Bleh
          Assert(Buttons(I));
          if CONSOLE_BUTTONS(I).Action = null then Right_Count := Right_Count + 1;
          else Left_Count := Left_Count + 1; end if;
          Assert_Dummy(Send_Message(
            Window        => Buttons(I),
            Message       => EVENT_SET_FONT,
            Data_Signed   => 0,
            Data_Unsigned =>
              To_Unchecked_Integer_4_Unsigned_C(
                To_Unchecked_Integer_Address(Font_Buttons))));
          Assert(Send_Message(
            Window        => Buttons(I),
            Message       => EVENT_SET_TEXT,
            Data_Unsigned => 0,
            Data_Signed   =>
              To_Unchecked_Integer_4_Signed_C(
                To_Unchecked_Integer_Address(
                  To_Access_Constant_Character_2_C(Localize(CONSOLE_BUTTONS(I).Message))))));
        end loop;
        Assert(Update_Window(Console));

        -- "The fact that this works is a major windows bug, good find!"
        Assert_Dummy(Show_Window(Console, MAKE_WINDOW_GO_TO_ICONIC));
        Assert_Dummy(Show_Window(Console, MAKE_WINDOW_RESTORE));

        while Message.Data /= MESSAGE_QUIT loop
          if Get_Log'length /= Length(Current_Log) then
            Current_Log := To_String_2_Unbounded(Get_Log);
            Assert(Send_Message(
              Window        => Output_Box,
              Message       => EVENT_SET_REDRAW,
              Data_Unsigned => Integer_4_Unsigned_C(C_FALSE),
              Data_Signed   => 0));
            Was_At_Bottom := Is_Scroll_Bar_At_Bottom;
            Assert(Send_Message(
              Window        => Output_Box,
              Message       => EVENT_SET_TEXT,
              Data_Unsigned => 0,
              Data_Signed   =>
                To_Unchecked_Integer_4_Signed_C(
                  To_Unchecked_Integer_Address(
                    To_Access_Constant_Character_2_C(Get_Log)))));
            if Was_At_Bottom then
            Assert(Send_Message(
              Window        => Output_Box,
              Message       => EVENT_SCROLL_VERTICALLY,
              Data_Unsigned => 7, -- SB_BOTTOM
              Data_Signed   => 0));
            end if;
            Assert(Send_Message(
              Window        => Output_Box,
              Message       => EVENT_SET_REDRAW,
              Data_Unsigned => Integer_4_Unsigned_C(C_TRUE),
              Data_Signed   => 0));
          end if;
          if Peek_Message(
          Message        => Message'unchecked_access,
          Window         => Console,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED then
            if Message.Data = EVENT_KEY_DOWN then
              Do_Process_Character := True;
            elsif Message.Data = EVENT_CHARACTER and Do_Process_Character then
              -- Paste without focus
              Do_Process_Character := False;
              Current_Input := Character_2'val(Integer_4_Signed(Message.Data_Unsigned));
              if not Is_Control(Current_Input) then
                Set_Input_Entry(Get_Input_Entry & Current_Input);
                if Get_Focus /= Input_Box then
                  Assert(Send_Message(
                    Window        => Input_Box,
                    Message       => EVENT_SET_TEXT,
                    Data_Unsigned => 0,
                    Data_Signed   =>
                      To_Unchecked_Integer_4_Signed_C(
                        To_Unchecked_Integer_Address(
                          To_Access_Constant_Character_2_C(Get_Input_Entry)))));
                end if;
              elsif Current_Input = Character_2'val(Character_1'pos(ASCII.CR)) then
                Put_Line(Get_Input_Entry);
                if Get_Input_Entry /= NULL_STRING_2 then
                  Handle(Get_Input_Entry);
                  Set_Input_Entry(NULL_STRING_2);
                  Assert(Send_Message(
                    Window        => Input_Box,
                    Message       => EVENT_SET_TEXT,
                    Data_Unsigned => 0,
                    Data_Signed   =>
                      To_Unchecked_Integer_4_Signed_C(
                        To_Unchecked_Integer_Address(
                          To_Access_Constant_Character_2_C(NULL_STRING_2)))));
                end if;
                Do_Skip_Message := True;
              elsif Current_Input = Character_2'val(Character_1'pos(ASCII.BS)) and Get_Input_Entry /= NULL_STRING_2 then
                Set_Input_Entry(Get_Input_Entry(1..Get_Input_Entry'last - 1));
                if Get_Focus /= Input_Box then
                  Assert(Send_Message(
                    Window        => Input_Box,
                    Message       => EVENT_SET_TEXT,
                    Data_Unsigned => 0,
                    Data_Signed   =>
                      To_Unchecked_Integer_4_Signed_C(
                        To_Unchecked_Integer_Address(
                          To_Access_Constant_Character_2_C(Get_Input_Entry)))));
                end if;
              elsif Current_Input = Character_2'val(Character_1'pos(ASCII.HT)) then Do_Skip_Message := True;
              end if;
            end if;
            if Get_Focus = Input_Box then null;
              -- Detect clipboard here, insertion, and delete
            end if;
            if not Do_Skip_Message then
              Assert_Dummy(Translate_Message(Message'unchecked_access));
              Assert_Dummy(Dispatch_Message(Message'unchecked_access));
            end if;
            Do_Skip_Message := False;
          end if;
        end loop;
        Assert(Release_Device_Context(Get_Desktop_Window, Context));
      end Run;
  end Import;
