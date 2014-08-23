with Neo.Windows;                  use Neo.Windows;
with Interfaces.C;                 use Interfaces.C;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
separate(Neo.System.Text.Console) package body Import is
  procedure Run is
    DO_DISABLE_RESIZE       : constant Boolean                  := False;
    FONT_GROUP_BOX_SIZE     : constant Float                    := 1.2;
    GROUP_BOX_SIDE_MARGIN   : constant Float                    := 1.2;
    FONT_CONSOLE            : constant String_2                 := "Courier New";
    FONT_DIALOG             : constant String_2                 := "Tahoma";
    NAME_BUTTON             : constant String_2_C               := To_String_2_C("Button");
    NAME_GROUP              : constant String_2_C               := To_String_2_C("Group");
    NAME_EDIT               : constant String_2_C               := To_String_2_C("Edit");
    IDENTIFIER_START        : constant Integer_4_Signed         := 16#0000_0666#;
    PIXELS_PER_INCH         : constant Integer_4_Signed_C       := 72;
    BUTTON_WIDTH_DLU        : constant Integer_4_Signed_C       := 50;
    BUTTON_HEIGHT_DLU       : constant Integer_4_Signed_C       := 14;
    FONT_CONSOLE_SIZE       : constant Integer_4_Signed_C       := -11;
    SCROLL_FACTOR           : constant Integer_8_Natural        := 500;
    MARGIN_BUTTON           : constant Integer_4_Signed_C       := 4;
    MARGIN                  : constant Integer_4_Signed_C       := 7;
    Name_Class              : aliased String_2_C                := To_String_2_C(SPECIFICS.Name & " " & Localize(NAME_POSTFIX));
    Hack                    : aliased String_2_C                := To_String_2_C("SINISTER HACK TO GET DIALOG BASE UNITS");
    Buffer                  : aliased String_2_C(1..10000)      := (others => NULL_CHARACTER_2_C);
    Size                    : aliased Record_Size               := (others => <>);
    Message                 : aliased Record_Message            := (others => <>);
    Rectangle               : aliased Record_Rectangle          := (others => <>);
    Text_Metric             : aliased Record_Text_Metric        := (others => <>);
    Class                   : aliased Record_Window_Class       := (others => <>);
    Non_Client_Metrics      : aliased Record_Non_Client_Metrics := (others => <>);
    Message_Box_Font_Height :         Integer_4_Signed_C        := 0;
    Dialog_Base_Unit_Height :         Integer_4_Signed_C        := 0;
    Dialog_Base_Unit_Width  :         Integer_4_Signed_C        := 0;
    Text_Box_Font_Height    :         Integer_4_Signed_C        := 1;
    Text_Box_Font_Width     :         Integer_4_Signed_C        := 1;
    Dialog_Font_Height      :         Integer_4_Signed_C        := 1;
    Dialog_Font_Width       :         Integer_4_Signed_C        := 1;
    Output_Box_Height       :         Integer_4_Signed_C        := 0;
    Output_Box_Width        :         Integer_4_Signed_C        := 0;
    Input_Box_Height        :         Integer_4_Signed_C        := 0;
    Start_Selection         :         Integer_4_Signed_C        := 0;
    End_Selection           :         Integer_4_Signed_C        := 0;
    Margin_Group_Top        :         Integer_4_Signed_C        := 0;
    Margin_Group            :         Integer_4_Signed_C        := 0;
    Current_Height          :         Integer_4_Signed_C        := 0;
    Current_Width           :         Integer_4_Signed_C        := 0;
    Console_Height          :         Integer_4_Signed_C        := 0;
    Console_Width           :         Integer_4_Signed_C        := 0;
    Button_Height           :         Integer_4_Signed_C        := 0;
    Button_Width            :         Integer_4_Signed_C        := 0;
    Right_Count             :         Integer_4_Signed_C        := 0;
    Left_Count              :         Integer_4_Signed_C        := 0;
    Y                       :         Integer_4_Signed_C        := 0;
    Number_Of_Lines         :         Integer_4_Signed_C        := 0;
    Current_Line            :         Integer_4_Signed_C        := 0;
    Current_Lines           :         Integer_8_Natural         := 0;
    Is_At_Bottom            :         Boolean                   := True;
    Is_First_Time           :         Boolean                   := True;
    Was_At_Bottom           :         Boolean                   := False;
    Was_At_Minimum_Width    :         Boolean                   := False;
    Was_At_Minimum_Height   :         Boolean                   := False;
    Failed_Button_Font_Set  :         Boolean                   := False;
    Do_Process_Character    :         Boolean                   := False;
    Do_Skip_Message         :         Boolean                   := False;
    Current_Input           :         Character_2               := NULL_CHARACTER_2;
    Message_Box_Window      :         Address                   := NULL_ADDRESS;
    Output_Group_Box        :         Address                   := NULL_ADDRESS;
    Input_Group_Box         :         Address                   := NULL_ADDRESS;
    Edit_Background         :         Address                   := NULL_ADDRESS;
    Font_Text_Box           :         Address                   := NULL_ADDRESS;
    Font_Buttons            :         Address                   := NULL_ADDRESS;
    Output_Box              :         Address                   := NULL_ADDRESS;
    Input_Box               :         Address                   := NULL_ADDRESS;
    Context                 :         Address                   := NULL_ADDRESS;
    Console                 :         Address                   := NULL_ADDRESS;
    Icon                    :         Address                   := NULL_ADDRESS;
    Buttons : array(CONSOLE_BUTTONS'range) of Address := (others => NULL_ADDRESS);
    function Is_Scrollbar_At_Bottom return Boolean is
      Scroll_Information : aliased Record_Scroll_Information := (others => <>);
      begin
        Assert(Get_Scroll_Information(Output_Box, 1, Scroll_Information'unchecked_access));
        return Scroll_Information.Position + Number_of_Lines >= Scroll_Information.Maximum;
      end Is_Scrollbar_At_Bottom;
    procedure Set_Text(Handle : in Address; Text : in String_2) is
      begin
        Assert(Send_Message(
          Window        => Handle,
          Message       => EVENT_SET_TEXT,
          Data_Unsigned => 0,
          Data_Signed   => To_Unchecked_Integer_Address(To_Access_Constant_Character_2_C(Localize(Text)))));  
      end Set_Text;
    procedure Set_Font(Handle : in Address; Font : in Address) is
      begin
        Assert_Dummy(Send_Message(
          Window        => Handle,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned => To_Unchecked_Integer_Address(Font)));
      end Set_Font;
    procedure Set_Sizes is
      Minimum_Width : Integer_4_Signed_C := 0;
      Box_Padding   : Integer_4_Signed_C := 0;
      Border_Height : Integer_4_Signed_C := Get_System_Metrics(DATA_BORDER_HEIGHT);
      Border_Width  : Integer_4_Signed_C := Get_System_Metrics(DATA_BORDER_WIDTH);
      begin
        Margin_Group      := Integer_4_Signed_C(Float(Text_Metric.Height) / GROUP_BOX_SIDE_MARGIN);
        Margin_Group_Top  := Integer_4_Signed_C(Float(Text_Metric.Height) * FONT_GROUP_BOX_SIZE);
        Box_Padding       := Integer_4_Signed_C(Float(Text_Box_Font_Width) / 1.5);
        Output_Box_Width  := 2 * Box_Padding + (Text_Box_Font_Width * Integer_4_Signed_C(Get_Line_Size)) + Get_System_Metrics(DATA_SCROLL_BAR_WIDTH);
        Output_Box_Height := 2 * Box_Padding + (Text_Box_Font_Height * Integer_4_Signed_C(NUMBER_OF_OUTPUT_ROWS));
        Input_Box_Height  := 2 * Box_Padding + Text_Box_Font_Height;
        Console_Width     := (MARGIN * Dialog_Base_Unit_Width + Margin_Group + Border_Width) * 2 + Output_Box_Width;
        Console_Height    := (MARGIN * Dialog_Base_Unit_Height) * 4 + (Border_Height + Margin_Group + Margin_Group_Top) * 2 +
                             Output_Box_Height + Input_Box_Height + BUTTON_HEIGHT + Get_System_Metrics(DATA_TITLE_BAR_HEIGHT);
        if Buttons'length > 0 then
          Minimum_Width := (Buttons'length - 1) * MARGIN_BUTTON * Dialog_Base_Unit_Width + MARGIN * 2 * Dialog_Base_Unit_Width + BUTTON_WIDTH * Buttons'length + Border_Width * 2;
          if Console_Width < Minimum_Width then
            Output_Box_Width := Output_Box_Width + Minimum_Width - Console_Width;
            Console_Width := Minimum_Width;
          end if;
        end if;
        if Current_Height < Console_Height or (Was_At_Minimum_Height and Current_Height > Console_Height) then Current_Height := Console_Height; end if;
        if Current_Width < Console_Width or (Was_At_Minimum_Width and Current_Width > Console_Width) then Current_Width := Console_Width; end if;
        Output_Box_Width      := Current_Width - (Console_Width - Output_Box_Width);
        Output_Box_Height     := Current_Height - (Console_Height - Output_Box_Height);
        Was_At_Minimum_Height := Current_Height < Console_Height + Border_Height;
        Was_At_Minimum_Width  := Current_Width < Console_Width + Border_Width;
        Number_Of_Lines       := (Output_Box_Height - 2 * Box_Padding) / Text_Box_Font_Height;
      end Set_Sizes;
    procedure Create_Fonts is 
      begin
        if Failed_Button_Font_Set then
          Font_Buttons :=
            Create_Font(
              Height           => Message_Box_Font_Height,
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
              Face             => To_Access_Constant_Character_2_C(FONT_DIALOG));
          Assert(Font_Buttons);
        end if;
        Font_Text_Box :=
          Create_Font(
            Height           => FONT_CONSOLE_SIZE,
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
        Assert_Dummy(Select_Object(Context, Font_Buttons));
        Assert(Get_Text_Metrics(Context, Text_Metric'address));
      end Create_Fonts;
    procedure Get_Sizes_From_Message_Box is
      Hook : Address := NULL_ADDRESS;
      procedure Wait_For_Set_With_Inifinite_Loop is
        begin
          for I in Integer_4_Signed'range loop -- The Windows API made me do it. I swear
            exit when Dialog_Base_Unit_Width /= 0;
            delay 0.0001;
          end loop;
          if Dialog_Base_Unit_Width = 0 then raise Call_Failure; end if;
        end Wait_For_Set_With_Inifinite_Loop;
      function Callback_Message_Box(Code : in Integer_4_Signed_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Callback_Message_Box);
      function Callback_Message_Box(Code : in Integer_4_Signed_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address is
        Class_Name  : aliased String_2_C(1..256) := (others => NULL_CHARACTER_2_C);
        Window_Text : aliased String_2_C(1..256) := (others => NULL_CHARACTER_2_C);
        Window      :         Address            := To_Unchecked_Address(Integer_Address(Data_Unsigned));
        function Callback_Message_Box_Children(Window : in Address; Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Callback_Message_Box_Children);
        function Callback_Message_Box_Children(Window : in Address; Data_Signed : in Integer_Address) return Integer_Address is
          Rectangle : aliased Record_Rectangle := (others => <>);
          begin
            Assert(Get_Class_Name(Window, Class_Name'Unrestricted_Access, Class_Name'length));
            Assert_Dummy(Get_Window_Text(Window, Window_Text'Unrestricted_Access, Window_Text'length));
            if Dialog_Base_Unit_Width = 0 and "Button" = To_String_2(Class_Name) then
              Assert(Get_Window_Rectangle(Window, Rectangle'address));
              Button_Width            := (Rectangle.Right - Rectangle.Left);
              Button_Height           := (Rectangle.Bottom - Rectangle.Top);
              Assert_Dummy(Select_Object(Context, To_Unchecked_Address(Integer_Address(To_Unchecked_Integer_4_Unsigned(Send_Message(Window, MESSAGE_GET_FONT, 0, 0))))));
              Assert(Get_Text_Metrics(Context, Text_Metric'address));
              Message_Box_Font_Height := Text_Metric.Height;
              Dialog_Base_Unit_Width  := Button_Width / BUTTON_WIDTH_DLU;
              Dialog_Base_Unit_Height := Button_Height / BUTTON_HEIGHT_DLU;
            end if;
            return Integer_Address(C_TRUE);
          end Callback_Message_Box_Children;
        begin
          Assert(Get_Class_Name(Window, Class_Name'Unrestricted_Access, Class_Name'length));
          Assert_Dummy(Get_Window_Text(Window, Window_Text'Unrestricted_Access, Window_Text'length));
          if Code = COMPUTER_BASED_TRAINING_ACTIVATE and To_String_2(Class_Name) = CLASS_NAME_DIALOG and To_String_2(Window_Text) = To_String_2(Hack) then
            Assert_Dummy(Enumerate_Child_Windows(Window, Callback_Message_Box_Children'address, 0)); -- I heard you like callbacks
            Wait_For_Set_With_Inifinite_Loop;
            Assert(Destroy_Window(Window));
            Assert(Unhook_Windows_Hook(Hook));
          else Assert_Dummy(Call_Next_Hook(Hook, Code, Data_Unsigned, Data_Signed)); end if;
          return Integer_Address(C_FALSE);
        end Callback_Message_Box;
      begin
        Hook := Set_Windows_Hook(COMPUTER_BASED_TRAINING_HOOK, Callback_Message_Box'address, NULL_ADDRESS, Get_Current_Thread_Id);
        Assert_Dummy(Message_Box(NULL_ADDRESS, To_String_2_C(SPECIFICS.Name), Hack, 0));
        Wait_For_Set_With_Inifinite_Loop;
      end Get_Sizes_From_Message_Box;
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address; pragma Convention(Stdcall, Callback_Window);
    function Callback_Window(Window : in Address; Message : in Integer_4_Unsigned_C; Data_Unsigned, Data_Signed : in Integer_Address) return Integer_Address is
      Point : Record_Point;
      function To_Windows_Color(Color : in Record_Color) return Integer_4_Unsigned_C is
        begin
          return Integer_4_Unsigned_C(Integer_4_Unsigned(Color.Red) or Shift_Left(Integer_4_Unsigned(Color.Green), 8) or Shift_Left(Integer_4_Unsigned(Color.Blue), 16));
        end To_Windows_Color;
      begin
        case Message is
          when EVENT_CLOSE => Post_Quit_Message(0); return Integer_Address(C_TRUE);
          when EVENT_COMMAND =>
            case Data_Unsigned is
              when Integer_Address(SUBEVENT_MENU_POPOUT) | Integer_Address(SUBEVENT_SCREEN_SAVER_START) => return Integer_Address(C_TRUE);
            when others => null; end case;
          when EVENT_CREATE =>
            Edit_Background := Create_Solid_Brush(To_Windows_Color(COLOR_BACKGROUND));
            Assert(Edit_Background);
          when EVENT_CONTROL_STATIC_COLOR | EVENT_CONTROL_DYNAMIC_COLOR =>
            if To_Unchecked_Address(Data_Signed) = Output_Box or To_Unchecked_Address(Data_Signed) = Input_Box then
              Assert(Set_Background_Color(
                Device_Context => To_Unchecked_Address(Integer_Address(Data_Unsigned)),
                Color          => To_Windows_Color(COLOR_BACKGROUND)) /= INVALID_COLOR);
              Assert(Set_Text_Color(
                Device_Context => To_Unchecked_Address(Integer_Address(Data_Unsigned)),
                Color          => To_Windows_Color(COLOR_TEXT)) /= INVALID_COLOR);
              return To_Unchecked_Integer_Address(Edit_Background);
            end if;
          when EVENT_BUTTON_COMMAND =>
            for I in CONSOLE_BUTTONS'range loop
              if Data_Unsigned = Integer_Address(I + IDENTIFIER_START) then
                if CONSOLE_BUTTONS(I).Action = null then Post_Quit_Message(0);
                else CONSOLE_BUTTONS(I).Action.all; end if;
                return Integer_Address(C_TRUE);
              end if;
            end loop;
          when EVENT_GET_MINIMUM_MAXIMUM_SIZE_INFORMATION =>
            declare
            Minimum_Maximum_Information : Access_Record_Minimum_Maximum_Information := To_Access_Record_Minimum_Maximum_Information(To_Unchecked_Address(Data_Signed));
            begin
              Minimum_Maximum_Information.Minimum_Track_Size.X := Console_Width;
              Minimum_Maximum_Information.Minimum_Track_Size.Y := Console_Height;
            end;
            return Integer_Address(C_TRUE);
          when EVENT_SIZE_CHANGED =>
            if Data_Unsigned /= Integer_Address(SUBEVENT_ICONIZED) then
              Was_At_Minimum_Height := False;
              Was_At_Minimum_Width  := False;
              Assert_Dummy(Change_Window_Setting(
                Window  => Console,
                Command => SET_WINDOW_STYLE_EXTRA,
                Setting => STYLE_EXTRA_COMPOSITED));
              Is_At_Bottom := Is_Scrollbar_At_Bottom;
              Assert(Get_Window_Rectangle(Console, Rectangle'address));
              Current_Width := Rectangle.Right - Rectangle.Left;
              Current_Height := Rectangle.Bottom - Rectangle.Top;
              Set_Sizes;
              Assert(Set_Window_Position(
                Window       => Console,
                Insert_After => NULL_ADDRESS,
                X            => Rectangle.Left,
                Y            => Rectangle.Top,
                Width        => Current_Width,
                Height       => Current_Height,
                Flags        => 0));
              Y := Dialog_Base_Unit_Width * MARGIN;
              Assert(Set_Window_Position(
                Window       => Output_Group_Box,
                Insert_After => NULL_ADDRESS,
                X            => Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width + Margin_Group * 2,
                Height       => Output_Box_Height + Margin_Group_Top + Margin_Group,
                Flags        => 0));
              Y := Y + Margin_Group_Top;
              Assert(Set_Window_Position(
                Window       => Output_Box,
                Insert_After => NULL_ADDRESS,
                X            => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width,
                Height       => Output_Box_Height,
                Flags        => 0));
              Y := Y + Output_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;
              Assert(Set_Window_Position(
                Window       => Input_Group_Box,
                Insert_After => NULL_ADDRESS,
                X            => Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Height       => Input_Box_Height + Margin_Group_Top + Margin_Group,
                Width        => Output_Box_Width + Margin_Group * 2,
                Flags        => 0));
              Y := Y + Margin_Group_Top;
              Assert(Set_Window_Position(
                Window       => Input_Box,
                Insert_After => NULL_ADDRESS,
                X            => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width,
                Height       => Input_Box_Height,
                Flags        => 0));
              Y := Y + Input_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;
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
                      Output_Box_Width + Margin_Group * 2 + Dialog_Base_Unit_Width * MARGIN - (Right_Count + 1) * BUTTON_WIDTH -
                      Dialog_Base_Unit_Width * Right_Count * MARGIN_BUTTON)));
                if CONSOLE_BUTTONS(I).Action = null then Right_Count := Right_Count + 1;
                else Left_Count := Left_Count + 1; end if;
              end loop;
              if Is_At_Bottom then
                Assert_Dummy(Send_Message(
                  Window        => Output_Box,
                  Message       => EVENT_SCROLL_VERTICALLY,
                  Data_Unsigned => SUBEVENT_SCROLL_BOTTOM,
                  Data_Signed   => 0));
              end if;
              Assert(Redraw_Window(Window, NULL_ADDRESS, NULL_ADDRESS, 1));
              Assert_Dummy(Change_Window_Setting(
                Window  => Console,
                Command => SET_WINDOW_STYLE_EXTRA,
                Setting => 0));
              return Integer_Address(C_TRUE);
            end if;
        when others => null; end case;
        return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    begin        
      Context := Get_Device_Context(Get_Desktop_Window);
      Assert(Context);
      Icon :=
        Load_Image( -- Loads the icon nicely for the Aero theme, but on "classic" theme it looks pixelated on the title bar
          Instance  => Get_Current_Instance,
          Name      => To_Access_Constant_Character_2_C(PATH_ASSETS & "\ico\" & NAME_ICON & ".ico"),
          Kind      => LOAD_ICO,
          Desired_X => 0,
          Desired_Y => 0,
          Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_ADDRESS then
        Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON);
      end if;
      if System_Parameter_Information( -- Fails under XP....
        Action       => GET_NON_CLIENT_METRICS,
        User_Profile => 0,
        Parameter_B  => Non_Client_Metrics'address,
        Parameter_A  => Non_Client_Metrics.Size) = FAILED then
        Failed_Button_Font_Set := True;
      else
        Font_Buttons := Create_Font_Indirect(Non_Client_Metrics.Message_Font'unchecked_access);
        Assert(Font_Buttons);
      end if;
      Get_Sizes_From_Message_Box;
      Create_Fonts;
      Set_Sizes;
      Class :=(
        Callback   => Callback_Window'address,
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
          X           => Get_Device_Capabilities(Context, DATA_HORIZONTAL_RESOLUTION) / 2 - Console_Width  / 2,
          Y           => Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION)   / 2 - Console_Height / 2,
          Width       => Console_Width,
          Height      => Console_Height,
          Parent      => NULL_ADDRESS,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       =>
            STYLE_ICONIC_INITIALLY or
            STYLE_BORDER_SIZABLE   or
            STYLE_TITLEBAR_MENU    or
            STYLE_BORDER_THIN_LINE or
            STYLE_BOX_ICONIZE      or
            STYLE_BOX_FULLSCREEN);
      Assert(Console);
      Output_Group_Box :=
        Create_Window(
          Class_Name  => NAME_BUTTON,
          Window_Name => NULL_STRING_2_C,
          X           => 0,
          Y           => 0,
          Width       => 0,
          Height      => 0,
          Parent      => Console,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       => STYLE_VISIBLE_INITIALLY or STYLE_GROUP_BOX or STYLE_CHILD);
      Assert(Output_Group_Box);
      Set_Font(Output_Group_Box, Font_Buttons);
      Set_Text(Output_Group_Box, Localize(LABEL_OUTPUT));
      Output_Box :=
        Create_Window(
          Class_Name  => NAME_EDIT,
          Window_Name => NULL_STRING_2_C,
          X           => 0,
          Y           => 0,
          Width       => 0,
          Height      => 0,
          Parent      => Console,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       =>
            STYLE_HAS_VERTICAL_SCROLL_BAR   or
            STYLE_VISIBLE_INITIALLY         or
            STYLE_BORDER_THIN_LINE          or
            STYLE_ALIGN_TEXT_TO_LEFT        or
            STYLE_MULTI_LINE                or
            STYLE_NO_USER_EDITING           or
            STYLE_CHILD);
      Assert(Output_Box);
      Set_Font(Output_Box, Font_Text_Box);
      Input_Group_Box :=
        Create_Window(
          Class_Name  => NAME_BUTTON,
          Window_Name => NULL_STRING_2_C,
          X           => 0,
          Y           => 0,
          Height      => 0,
          Width       => 0,
          Parent      => Console,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       => STYLE_VISIBLE_INITIALLY or STYLE_GROUP_BOX or STYLE_CHILD);
      Assert(Input_Group_Box);
      Set_Font(Input_Group_Box, Font_Buttons);
      Set_Text(Input_Group_Box, LABEL_INPUT_ENTRY);
      Input_Box :=
        Create_Window(
          Class_Name  => NAME_EDIT,
          Window_Name => NULL_STRING_2_C,
          X           => 0,
          Y           => 0,
          Width       => 0,
          Height      => 0,
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
      Set_Font(Input_Box, Font_Text_Box);
      for I in Buttons'range loop
        Buttons(I) :=
          Create_Window(
            Class_Name  => NAME_BUTTON,
            Window_Name => NULL_STRING_2_C,
            Width       => 0,
            Height      => 0,
            Parent      => Console,
            Menu        => Integer_Address(I + IDENTIFIER_START),
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style_Extra => 0,
            Style       => STYLE_PUSH_BUTTON or STYLE_VISIBLE_INITIALLY or STYLE_CHILD,
            Y           => 0,
            X           => 0);
        Assert(Buttons(I));
        Set_Font(Buttons(I), Font_Buttons);
        Set_Text(Buttons(I), Localize(CONSOLE_BUTTONS(I).Message));
      end loop;
      Assert_Dummy(Show_Window(Console, MAKE_WINDOW_GO_TO_ICONIC)); -- "The fact that this works is a major windows bug, good find!"
      Assert_Dummy(Show_Window(Console, MAKE_WINDOW_RESTORE));
      while Message.Data /= MESSAGE_QUIT loop
        if Get_Number_Of_Lines /= Current_Lines then
          Is_At_Bottom := Is_Scrollbar_At_Bottom;
          Assert_Dummy(Send_Message(
            Window        => Output_Box,
            Message       => EVENT_TEXT_GET_SELECTION,
            Data_Unsigned => To_Unchecked_Integer_Address(Start_Selection'address),
            Data_Signed   => To_Unchecked_Integer_Address(End_Selection'address)));
          Current_Line := Send_Message(Output_Box, EVENT_TEXT_GET_LINE, 0, 0);
          Assert_Dummy(Send_Message(
            Window        => Output_Box,
            Message       => EVENT_SET_REDRAW,
            Data_Unsigned => Integer_Address(C_FALSE),
            Data_Signed   => 0));
          Set_Text(Output_Box, Get_Log);
          Assert_Dummy(Send_Message(
            Window        => Output_Box,
            Message       => EVENT_TEXT_SET_SELECTION,
            Data_Unsigned => Integer_Address(Start_Selection),
            Data_Signed   => Integer_Address(End_Selection)));            
          if Is_At_Bottom or Is_First_Time then
            Is_First_Time := False;
            Assert_Dummy(Send_Message(
              Window        => Output_Box,
              Message       => EVENT_SCROLL_VERTICALLY,
              Data_Unsigned => SUBEVENT_SCROLL_BOTTOM,
              Data_Signed   => 0));
            Current_Line := Send_Message(Output_Box, EVENT_TEXT_GET_LINE, 0, 0);
          else
            for I in 1..Current_Line loop
              Assert_Dummy(Send_Message(
                Window        => Output_Box,
                Message       => EVENT_SCROLL_VERTICALLY,
                Data_Unsigned => SUBEVENT_SCROLL_DOWN_LINE,
                Data_Signed   => 0));
            end loop;
          end if;
          Current_Lines := Get_Number_Of_Lines;
          Assert(Send_Message(
            Window        => Output_Box,
            Message       => EVENT_SET_REDRAW,
            Data_Unsigned => Integer_Address(C_TRUE),
            Data_Signed   => 0));
        end if;
        if Peek_Message(
          Message        => Message'unchecked_access,
          Window         => NULL_ADDRESS,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED then
          case Message.Data is
            when EVENT_MOUSE_WHEEL_VERTICAL =>
              if Get_Focus /= Output_Box then
                declare
                Wheel_Delta : Integer_2_Signed := To_Unchecked_Integer_2_Signed(Integer_2_Unsigned( 
                  Shift_Right(Amount => 16, Value => Integer_8_Unsigned(Message.Data_Unsigned) and 16#0000_0000_FFFF_0000#))) / MOUSE_WHEEL_DELTA; 
                begin
                  if Wheel_Delta < 0 then
                    for I in 1..Current_Lines / SCROLL_FACTOR loop Assert_Dummy(Send_Message(Output_Box, EVENT_SCROLL_VERTICALLY, 1, 0)); end loop;
                  elsif Wheel_Delta > 0 then
                    for I in 1..Current_Lines / SCROLL_FACTOR loop Assert_Dummy(Send_Message(Output_Box, EVENT_SCROLL_VERTICALLY, 0, 0)); end loop;
                  end if;
                end;
              end if;
            when EVENT_KEY_DOWN =>
              Do_Process_Character := True;
              case Integer_2_Unsigned_C(Message.Data_Unsigned) is
                when VIRTUAL_KEY_V =>
                  if (Get_Key_State(Integer_4_Signed_C(VIRTUAL_KEY_CONTROL)) and 16#8000#) > 0 and Get_Focus /= Input_Box then
                    Set_Input_Entry(Get_Input_Entry & Get_Clipboard);
                    Set_Text(Input_Box, Get_Input_Entry);
                  end if;
              when others => null; end case;
            when EVENT_CHARACTER =>
              if Do_Process_Character then
                Do_Process_Character := False;
                Current_Input := Character_2'val(Integer_4_Signed(Message.Data_Unsigned));
                if not Is_Control(Current_Input) then
                  Assert_Dummy(
                    Send_Message(
                      Window        => Input_Box,
                      Message       => EVENT_TEXT_GET_BUFFER,
                      Data_Unsigned => Integer_Address(Buffer'length),
                      Data_Signed   => To_Unchecked_Integer_Address(Buffer'address)));
                  Set_Input_Entry(To_String_2(Buffer) & Current_Input);
                  if Get_Focus /= Input_Box then Set_Text(Input_Box, Get_Input_Entry); end if;
                elsif Current_Input = Character_2'val(Character_1'pos(ASCII.CR)) then
                  Assert_Dummy(
                    Send_Message(
                      Window        => Input_Box,
                      Message       => EVENT_TEXT_GET_BUFFER,
                      Data_Unsigned => Integer_Address(Buffer'length),
                      Data_Signed   => To_Unchecked_Integer_Address(Buffer'address)));
                  Set_Input_Entry(To_String_2(Buffer));
                  if Get_Input_Entry /= NULL_STRING_2 then
                    Put_Line(Get_Input_Entry);
                    if Get_Input_Entry /= NULL_STRING_2 then
                      Handle(Get_Input_Entry);
                      Set_Input_Entry(NULL_STRING_2);
                      Set_Text(Input_Box, NULL_STRING_2);
                    end if;
                  end if;
                  Do_Skip_Message := True;
                elsif Current_Input = Character_2'val(Character_1'pos(ASCII.BS)) and Get_Input_Entry /= NULL_STRING_2 then
                  Set_Input_Entry(Get_Input_Entry(1..Get_Input_Entry'last - 1));
                  if Get_Focus /= Input_Box then Set_Text(Input_Box, Get_Input_Entry); end if;
                elsif Current_Input = Character_2'val(Character_1'pos(ASCII.HT)) then Do_Skip_Message := True; end if;
              end if;
          when others => null; end case;
          if not Do_Skip_Message then
            Assert_Dummy(Translate_Message(Message'unchecked_access));
            Assert_Dummy(Dispatch_Message(Message'unchecked_access));
          end if;
          Do_Skip_Message := False;
        end if;
      end loop;
      Assert_Dummy(Show_Window(Console, MAKE_WINDOW_HIDE));
      Assert(Destroy_Window(Console));
      Assert(Unregister_Class(Name_Class, NULL_ADDRESS));
      Console := NULL_ADDRESS;
      Assert(Release_Device_Context(Get_Desktop_Window, Context));
    end Run;
end Import;
