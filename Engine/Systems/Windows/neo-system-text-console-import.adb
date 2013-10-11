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
  Interfaces.C,
  Neo.Link.Windows;
use
  Interfaces.C,
  Neo.Link.Windows;
separate(Neo.System.Text.Console)
package body Import
  is
  ---------------
  -- Set_Alert --
  ---------------
    procedure Set_Alert(
      Status : in Boolean)
      is
      Window            :         Address                  := Find_Window(To_String_2_C(Get_Name), NULL_ADDRESS);
      Flash_Information : aliased Record_Flash_Information :=(
        if Status then
          (Flags => FLASH_CONTINUOUSLY, others => <>)
        else
          (Flags => FLASH_END, others => <>));
      begin
        if Window = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        if Flash_Window(Flash_Information'unchecked_access) = FAILED then
          raise Call_Failure;
        end if;
      end Set_Alert;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Name    : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons;
      Icon    : in Enumerated_Icon)
      return Boolean
      is
      Window : Address := Find_Window(To_String_2_C(Get_Name), NULL_ADDRESS);
      begin
        return(
          case
            Message_Box(
              Window  => Window,
              Caption => To_String_2_C(Get_Name),
              Text    => To_String_2_C(Message),
              Kind    =>(
                if Window = NULL_ADDRESS then
                  MESSAGE_BOX_SYSTEM_MODAL -- Force the message box into the foreground
                else
                  0)
                or(
                  case Icon is
                    when Warning_Icon     => ICON_WARNING,
                    when Information_Icon => ICON_INFORMATION,
                    when Error_Icon       => ICON_ERROR,
                    when No_Icon          => 0)
                or(
                  case Buttons is
                    when Okay_Button          => BUTTON_OKAY,
                    when Yes_No_Buttons       => BUTTONS_YES_NO,
                    when Okay_Cancel_Buttons  => BUTTONS_CANCEL_OKAY,
                    when Retry_Cancel_Buttons => BUTTONS_CANCEL_RETRY))
            is
              when PRESSED_OKAY | PRESSED_RETRY | PRESSED_YES =>
                True,
              when others =>
                False);
      end Is_Okay;
  ---------
  -- Run --
  ---------
    procedure Run
      is
      NAME_BUTTON      : constant String_2             := "Button";
      NAME_EDIT        : constant String_2             := "Edit";
      BORDER_WIDTH     : constant Integer_4_Signed_C   := 3;
      BORDER_HEIGHT    : constant Integer_4_Signed_C   := 22;
      CONSOLE_YIELD    : constant Duration             := 0.005;
      CONSOLE_WIDTH    : constant Integer_4_Signed_C   := 525 + BORDER_WIDTH * 2;
      CONSOLE_HEIGHT   : constant Integer_4_Signed_C   := 550 + BORDER_WIDTH + BORDER_HEIGHT;
      BUTTON_WIDTH     : constant Integer_4_Signed_C   := 72;
      BUTTON_HEIGHT    : constant Integer_4_Signed_C   := 24;
      PADDING          : constant Integer_4_Signed_C   := 7;
      FONT_SIZE        : constant Integer_4_Signed_C   := 13;
      IDENTIFIER_START : constant Integer_4_Signed     := 16#0000_0101#;
      COLOR_TEXT       : constant Integer_4_Unsigned_C := 16#0080_0000#; -- 16#00BB_GGRR#
      COLOR_BACKGROUND : constant Integer_4_Unsigned_C := 16#0000_FFFF#; -- 16#00BB_GGRR#
      STYLE_BUTTON     : constant Integer_4_Unsigned_C :=
      STYLE_PUSH_BUTTON                     or
      STYLE_PUSH_BUTTON_PRESELECTED_DEFAULT or
      STYLE_VISIBLE_INITIALLY               or
      STYLE_CHILD;
    STYLE_WINDOW : constant Integer_4_Unsigned_C :=
      STYLE_TITLEBAR_MENU                   or
      STYLE_BOX_ICONIZE                     or
      STYLE_BORDER_THIN_LINE                or
      STYLE_TITLEBAR_MENU;
    STYLE_EDIT : constant Integer_4_Unsigned_C :=
      STYLE_HAS_VERTICAL_SCROLL_BAR         or
      STYLE_VISIBLE_INITIALLY               or
      STYLE_BORDER_THIN_LINE                or
      STYLE_ALIGN_TEXT_TO_LEFT              or
      STYLE_MULTI_LINE                      or
      STYLE_AUTOMATIC_VERTICAL_SCROLL       or
      STYLE_NO_USER_EDITING                 or
      STYLE_CHILD;
      Non_Client_Metrics : aliased Record_Non_Client_Metrics               := (others => <>);
      Message            : aliased Record_Message                          := (others => <>);
      Class              : aliased Record_Window_Class                     := (others => <>);
      Buttons            :         array(CONSOLE_BUTTONS'range) of Address := (others => NULL_ADDRESS);
      Right_Count        :         Integer_4_Signed_C                      := 1;
      Context            :         Address                                 := Get_Device_Context(Get_Desktop_Window);
      Text_Box           :         Address                                 := NULL_ADDRESS;
      Console            :         Address                                 := NULL_ADDRESS;
      Edit_Background    :         Address                                 := NULL_ADDRESS;
      Font_Buttons       :         Address                                 := NULL_ADDRESS;
      Font_Text_Box      :         Address                                 :=
        Create_Font(
          Height           => FONT_SIZE,
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
          Face             => To_Access_Constant_Character_2_C("Courier New"));
      Icon : Address :=
        Load_Image(
          Instance  => Get_Current_Instance,
          Name      => To_Access_Constant_Character_2_C(Get_Icon_Path),
          Kind      => LOAD_ICO,
          Desired_X => 0,
          Desired_Y => 0,
          Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
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
        Point : Record_Point;
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_COMMAND =>
              case Data_Unsigned is
                when SUBEVENT_MENU_POPOUT | SUBEVENT_SCREEN_SAVER_START =>
                  return C_FALSE;
                when others =>
                  null;
              end case;
            when EVENT_CREATE =>
              Edit_Background := Create_Solid_Brush(COLOR_TEXT);
              if Edit_Background = NULL_ADDRESS then
                raise Call_Failure;
              end if;
            when EVENT_CONTROL_STATIC_COLOR =>
              if To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Signed)) = Text_Box then
                if
                Set_Background_Color(
                  Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
                  Color          => COLOR_TEXT) = INVALID_COLOR
                then
                  raise Call_Failure;
                end if;
                if
                Set_Text_Color(
                  Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
                  Color          => COLOR_BACKGROUND) = INVALID_COLOR
                then
                  raise Call_Failure;
                end if;
		return To_Unchecked_Integer_4_Signed_C(To_Unchecked_Integer_Address(Edit_Background));
              end if;
            when EVENT_BUTTON_COMMAND =>
              for I in CONSOLE_BUTTONS'range loop
                if Data_Unsigned = Integer_4_Unsigned_C(I + IDENTIFIER_START) then
                  if CONSOLE_BUTTONS(I).Action = null then
                    Post_Quit_Message(0);
                    return C_FALSE;
                  else
                    CONSOLE_BUTTONS(I).Action.all;
                  end if;
                  exit;
                end if;
              end loop;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      -----
      begin
      -----
        if Font_Text_Box = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        if Context = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        Icon :=(
          if Icon = NULL_ADDRESS then
            Load_Icon(Get_Current_Instance, GENERIC_ICON)
          else
            Icon);
        Class :=(
          Callback   => Window_Callback'address,
          Instance   => Get_Current_Instance,
          Icon_Small => Icon,
          Icon_Large => Icon,
          Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
          Background => BRUSH_WINDOW,
          Class_Name => To_Access_Constant_Character_2_C(Get_Name),
          others     => <>);
        if Register_Class(Class'unchecked_access) = Integer_2_Unsigned_C(FAILED) then
          raise Call_Failure;
        end if;
        Console :=
          Create_Window(
            Class_Name  => To_String_2_C(Get_Name),
            Window_Name => To_String_2_C(Get_Name),
            X           => Get_Device_Capabilities(Context, DATA_HORZONTAL_RESOLUTION) / 2 - CONSOLE_WIDTH  / 2,
            Y           => Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION)  / 2 - CONSOLE_HEIGHT / 2,
            Width       => CONSOLE_WIDTH,
            Height      => CONSOLE_HEIGHT,
            Parent      => NULL_ADDRESS,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style       => STYLE_WINDOW,
            Style_Extra => 0);
        if Console = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        if
        System_Parameter_Information(
          Action       => GET_NON_CLIENT_METRICS,
          User_Profile => 0,
          Parameter_B  => Non_Client_Metrics'address,
          Parameter_A  =>(
            if SPECIFICS.Version >= Windows_2_6_System then
              Non_Client_Metrics.Size
            else
              Non_Client_Metrics.Size - Integer_4_Unsigned_C(Integer_4_Signed_C'size / Byte'size))) = FAILED
        then
          raise Call_Failure;
        end if;
        Font_Buttons := Create_Font_Indirect(Non_Client_Metrics.Message_Font'unchecked_access);
        if Font_Buttons = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        if Release_Device_Context(Get_Desktop_Window, Context) = FAILED then
          raise Call_Failure;
        end if;
        Context := Get_Device_Context(Console);
        if Context = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        for I in Buttons'range loop
          if CONSOLE_BUTTONS(I).Action = null then
            Right_Count := Right_Count + 1;
          end if;
          Buttons(I) :=
            Create_Window(
              Class_Name  => To_String_2_C(NAME_BUTTON),
              Window_Name => To_String_2_C(NULL_STRING_2),
              Y           => CONSOLE_HEIGHT - BORDER_HEIGHT - BORDER_WIDTH - BUTTON_HEIGHT - PADDING,
              Width       => BUTTON_WIDTH,
              Height      => BUTTON_HEIGHT,
              Parent      => Console,
              Menu        => Integer_Address(I + IDENTIFIER_START),
              Instance    => Get_Current_Instance,
              Parameter   => NULL_ADDRESS,
              Style       => STYLE_BUTTON,
              Style_Extra => 0,
              X           =>(
                if CONSOLE_BUTTONS(I).Action /= null then -- Left justify
                  Integer_4_Signed_C(I) * PADDING + (Integer_4_Signed_C(I) - 1) * BUTTON_WIDTH
                else -- Right justify
                  CONSOLE_WIDTH - BORDER_WIDTH - (Right_Count * PADDING + (Right_Count - 1) * BUTTON_WIDTH)));
          if Buttons(I) = NULL_ADDRESS then
            raise Call_Failure;
          end if;
          if
          Send_Message(
            Window        => Buttons(I),
            Message       => EVENT_SET_FONT,
            Data_Signed   => 0,
            Data_Unsigned =>
              To_Unchecked_Integer_4_Unsigned_C(
                To_Unchecked_Integer_Address(Font_Buttons))) = FAILED
          then
            null; --raise Call_Failure; Why does this fail???
          end if;
          if
          Send_Message(
            Window        => Buttons(I),
            Message       => EVENT_SET_TEXT,
            Data_Unsigned => 0,
            Data_Signed   =>
              To_Unchecked_Integer_4_Signed_C(
                To_Unchecked_Integer_Address(
                  To_Access_Constant_Character_2_C(Localize(CONSOLE_BUTTONS(I).Message))))) = FAILED
          then
            raise Call_Failure;
          end if;
        end loop;
        Text_Box :=
          Create_Window(
            Class_Name  => To_String_2_C(NAME_EDIT),
            Window_Name => To_String_2_C(NULL_STRING_2),
            X           => PADDING,
            Y           => PADDING,
            Width       => CONSOLE_WIDTH  - PADDING * 2 - BORDER_WIDTH * 2,
            Height      => CONSOLE_HEIGHT - PADDING * 3 - BORDER_WIDTH - BORDER_HEIGHT  - BUTTON_HEIGHT,
            Parent      => Console,
            Menu        => 0,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS,
            Style       => STYLE_EDIT,
            Style_Extra => 0);
        if Text_Box = NULL_ADDRESS then
          Put_Line(Integer_4_Unsigned_C'wide_Image(Get_Last_Error));
          raise Call_Failure;
        end if;
        if
        Send_Message(
          Window        => Text_Box,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned =>
            To_Unchecked_Integer_4_Unsigned_C(
              To_Unchecked_Integer_Address(Font_Text_Box))) = FAILED
        then
          raise Call_Failure;
        end if;
        if
	Send_Message(
          Window        => Text_Box,
          Message       => MESSAGE_REPLACE_TEXT,
          Data_Unsigned => 0,
          Data_Signed   =>
            To_Unchecked_Integer_4_Signed_C(
              To_Unchecked_Integer_Address(
                To_Access_Constant_Character_2_C(Get_Log)))) = FAILED
        then
          raise Call_Failure;
        end if;
        if Send_Message(Text_Box, MESSAGE_SCROLL_TEXT, 0, 16#0000_FFFF#) = FAILED then
          raise Call_Failure;
        end if;
        if Send_Message(Text_Box, MESSAGE_SCROLL_CARET, 0, 0) = FAILED then
          raise Call_Failure;
        end if;
        if Show_Window(Console, MAKE_WINDOW_NORMALIZE) = 0 then
          null;
        end if;
        if Release_Device_Context(Console, Context) = FAILED then
          raise Call_Failure;
        end if;
        if Update_Window(Console) = FAILED then
          raise Call_Failure;
        end if;
        if Set_Foreground_Window(Console) = FAILED then
          raise Call_Failure;
        end if;
        while Message.Data /= MESSAGE_QUIT loop
          if
          Peek_Message(
            Message        => Message'unchecked_access,
            Window         => Console,
            Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
            Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
            Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED and then
          Translate_Message(Message'unchecked_access) < 2 and then
          Dispatch_Message(Message'unchecked_access) = 0
          then
            null;
          end if;
          --delay CONSOLE_YIELD;
        end loop;
        if Show_Window(Console, MAKE_WINDOW_HIDE) = 0 then
          null;
        end if;
        if Destroy_Window(Console) = FAILED then
          raise Call_Failure;
        end if;
        if Unregister_Class(To_String_2_C(Get_Name), NULL_ADDRESS) = FAILED then
          raise Call_Failure;
        end if;
      end Run;
--      procedure Run
--        is
--        BUTTON_WIDTH            : constant Integer_4_Signed_C             := 50;
--        BUTTON_HEIGHT           : constant Integer_4_Signed_C             := 14;
--        MARGIN                  : constant Integer_4_Signed_C             := 7;
--        MARGIN_GROUP_VERTICAL   : constant Integer_4_Signed_C             := 12;
--        MARGIN_GROUP_HORIZONTAL : constant Integer_4_Signed_C             := 9;
--        BOX_PADDING             : constant Integer_4_Signed_C             := 1;
--        OUTPUT_BOX_INITIAL_ROWS : constant Integer_4_Signed_C             := 500;
--        IDENTIFIER_START        : constant Integer_4_Signed               := 16#0000_0101#;
--        Non_Client_Metrics      : aliased  Record_Non_Client_Metrics      := (others => <>);
--        Message                 : aliased  Record_Message                 := (others => <>);
--        Class                   : aliased  Record_Window_Class            := (others => <>);
--        Buttons                 : array(CONSOLE_BUTTONS'range) of Address := (others => NULL_ADDRESS);
--        Output_Box_WIdth        :          Integer_4_Signed_C             := 0;
--        Output_Box_Height       :          Integer_4_Signed_C             := 0;
--        Input_Box_Height        :          Integer_4_Signed_C             := 0;
--        Console_Width           :          Integer_4_Signed_C             := 0;
--        Console_Height          :          Integer_4_Signed_C             := 0;
--        Current_Lines           :          Integer_8_Unsigned             := 0;
--        Right_Count             :          Integer_4_Signed_C             := 1;
--        Dialog_Base_Unit_Height :          Integer_4_Signed_C             := 1;
--        Dialog_Base_Unit_Width  :          Integer_4_Signed_C             := 1;
--        Context                 :          Address                        := NULL_ADDRESS;
--        Output_Box              :          Address                        := NULL_ADDRESS;
--        Input_Box               :          Address                        := NULL_ADDRESS;
--        Output_Group_Box        :          Address                        := NULL_ADDRESS;
--        Input_Group_Box         :          Address                        := NULL_ADDRESS;
--        Console                 :          Address                        := NULL_ADDRESS;
--        Edit_Background         :          Address                        := NULL_ADDRESS;
--        Font_Buttons            :          Address                        := NULL_ADDRESS;
--        Font_Text_Box           :          Address                        := NULL_ADDRESS;
--        Icon                    :          Address                        := NULL_ADDRESS;
--        --------------------------
--        function To_Windows_Color(
--        --------------------------
--          Color : in Record_Color)
--          return Integer_4_Unsigned_C
--          is
--          begin
--            return
--              Integer_4_Unsigned_C(
--                Integer_4_Unsigned(Color.Red)                  or
--                Shift_Left(Integer_4_Unsigned(Color.Green), 8) or
--                Shift_Left(Integer_4_Unsigned(Color.Blue), 16));
--          end To_Windows_Color;
--        -------------------------
--        function Window_Callback(
--        -------------------------
--          Window        : in Address;
--          Message       : in Integer_4_Unsigned_C;
--          Data_Unsigned : in Integer_4_Unsigned_C;
--          Data_Signed   : in Integer_4_Signed_C)
--          return Integer_4_Signed_C;
--          pragma Convention(Stdcall, Window_Callback);
--        function Window_Callback(
--          Window        : in Address;
--          Message       : in Integer_4_Unsigned_C;
--          Data_Unsigned : in Integer_4_Unsigned_C;
--          Data_Signed   : in Integer_4_Signed_C)
--          return Integer_4_Signed_C
--          is
--          Point : Record_Point;
--          begin
--            case Message is
--              when EVENT_CLOSE =>
--                Post_Quit_Message(0);
--                return C_FALSE;
--              when EVENT_COMMAND =>
--                case Data_Unsigned is
--                  when SUBEVENT_MENU_POPOUT | SUBEVENT_SCREEN_SAVER_START =>
--                    return C_FALSE;
--                  when others =>
--                    null;
--                end case;
--              when EVENT_CREATE =>
--                Edit_Background := Create_Solid_Brush(To_Windows_Color(COLOR_TEXT));
--                if Edit_Background = NULL_ADDRESS then
--                  raise Call_Failure;
--                end if;
--              when EVENT_CONTROL_STATIC_COLOR =>
--                if To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Signed)) = Output_Box then
--                  if
--                  Set_Background_Color(
--                    Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
--                    Color          => To_Windows_Color(COLOR_TEXT)) = INVALID_COLOR
--                  then
--                    raise Call_Failure;
--                  end if;
--                  if
--                  Set_Text_Color(
--                    Device_Context => To_Unchecked_Address(To_Unchecked_Integer_Address(Data_Unsigned)),
--                    Color          => To_Windows_Color(COLOR_BACKGROUND)) = INVALID_COLOR
--                  then
--                    raise Call_Failure;
--                  end if;
--                  return To_Unchecked_Integer_4_Signed_C(To_Unchecked_Integer_Address(Edit_Background));
--                end if;
--              when EVENT_BUTTON_COMMAND =>
--                for I in CONSOLE_BUTTONS'range loop
--                  if Data_Unsigned = Integer_4_Unsigned_C(I + IDENTIFIER_START) then
--                    if CONSOLE_BUTTONS(I).Action = null then
--                      Post_Quit_Message(0);
--                      return C_FALSE;
--                    else
--                      CONSOLE_BUTTONS(I).Action.all;
--                    end if;
--                    exit;
--                  end if;
--                end loop;
--              when others =>
--                null;
--            end case;
--            return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
--          end Window_Callback;
--        -----
--        begin
--        -----
--          --
--          -- Context taken
--          --
--          Context := Get_Device_Context(Get_Desktop_Window);
--          if Context = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- Icon
--          --
--          Icon :=
--            Load_Image( -- Loads the icon nicely for the Aero theme, but on "classic" theme it looks pixelated on the title bar
--              Instance  => Get_Current_Instance,
--              Name      => To_Access_Constant_Character_2_C(Get_Icon_Path),
--              Kind      => LOAD_ICO,
--              Desired_X => 0,
--              Desired_Y => 0,
--              Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
--          if Icon = NULL_ADDRESS then
--            Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON);
--          end if;
--          --
--          -- Text box font
--          --
--          Font_Text_Box :=
--            Create_Font(
--              Height           => 13,
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
--              Face             => To_Access_Constant_Character_2_C("Courier New"));
--          if Font_Text_Box = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- General font
--          --
--          if
--          System_Parameter_Information(
--            Action       => GET_NON_CLIENT_METRICS,
--            User_Profile => 0,
--            Parameter_B  => Non_Client_Metrics'address,
--            Parameter_A  =>(
--              if SPECIFICS.Version >= Windows_2_6_System then
--                Non_Client_Metrics.Size
--              else
--                Non_Client_Metrics.Size - Integer_4_Unsigned_C(Integer_4_Signed_C'size / Byte'size))) = FAILED
--          then
--            raise Call_Failure;
--          end if;
--          Font_Buttons := Create_Font_Indirect(Non_Client_Metrics.Message_Font'unchecked_access);
--          if Font_Buttons = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- Window
--          --
--          Class :=(
--            Callback   => Window_Callback'address,
--            Instance   => Get_Current_Instance,
--            Icon_Small => Icon,
--            Icon_Large => Icon,
--            Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
--            Background => BRUSH_WINDOW,
--            Class_Name => To_Access_Constant_Character_2_C(Get_Name),
--            others     => <>);
--          if Register_Class(Class'unchecked_access) = Integer_2_Unsigned_C(FAILED) then
--            raise Call_Failure;
--          end if;
--          Output_Box_WIdth  := 2 * BOX_PADDING + Text_Box_Font_Width  * Get_Line_Size;
--          Output_Box_Height := 2 * BOX_PADDING + Text_Box_Font_Height * OUTPUT_BOX_INITIAL_ROWS;
--          Input_Box_Height  := 2 * BOX_PADDING + Text_Box_Font_Height;
--          Console_Width     := 32 * Dialog_Base_Unit_Width  + Output_Box_WIdth;
--          Console_Height    := 84 * Dialog_Base_Unit_Height + Output_Box_Height;
--          Console :=
--            Create_Window(
--            Class_Name  => To_String_2_C(Get_Name),
--            Window_Name => To_String_2_C(Get_Name),
--            X           => Get_Device_Capabilities(Context, DATA_HORZONTAL_RESOLUTION) / 2 - Console_Width  / 2,
--            Y           => Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION)  / 2 - Console_Height / 2,
--            Width       => Console_Width,
--            Height      => Console_Height,
--            Parent      => NULL_ADDRESS,
--            Menu        => 0,
--            Instance    => Get_Current_Instance,
--            Parameter   => NULL_ADDRESS,
--            Style_Extra => 0,
--            Style       =>
--              STYLE_TITLEBAR_MENU    or
--              STYLE_BORDER_THIN_LINE or
--              STYLE_TITLEBAR_MENU);
--          if Console = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- Context released
--          --
--          if Release_Device_Context(Get_Desktop_Window, Context) = FAILED then
--            raise Call_Failure;
--          end if;
--          --
--          -- Buttons
--          --
--          for I in Buttons'range loop
--            if CONSOLE_BUTTONS(I).Action = null then
--              Right_Count := Right_Count + 1;
--            end if;
--            Buttons(I) :=
--              Create_Window(
--                Class_Name  => To_String_2_C("Button"),
--                Window_Name => To_String_2_C(NULL_STRING_2),
--                Y           => Console_Height - BORDER_HEIGHT - BORDER_WIDTH - BUTTON_HEIGHT - PADDING,
--                Width       => BUTTON_WIDTH,
--                Height      => BUTTON_HEIGHT,
--                Parent      => Console,
--                Menu        => Integer_Address(I + IDENTIFIER_START),
--                Instance    => Get_Current_Instance,
--                Parameter   => NULL_ADDRESS,
--                Style_Extra => 0,
--                Style       =>
--                  STYLE_PUSH_BUTTON                     or
--                  STYLE_PUSH_BUTTON_PRESELECTED_DEFAULT or
--                  STYLE_VISIBLE_INITIALLY               or
--                  STYLE_CHILD,
--                X =>(
--                  if CONSOLE_BUTTONS(I).Action /= null then -- Left justify
--                    Integer_4_Signed_C(I) * PADDING + (Integer_4_Signed_C(I) - 1) * BUTTON_WIDTH
--                  else -- Right justify
--                    Console_Width - BORDER_WIDTH - (Right_Count * PADDING + (Right_Count - 1) * BUTTON_WIDTH)));
--            if Buttons(I) = NULL_ADDRESS then
--              raise Call_Failure;
--            end if;
--            if
--            Send_Message(
--              Window        => Buttons(I),
--              Message       => EVENT_SET_FONT,
--              Data_Signed   => 0,
--              Data_Unsigned =>
--                To_Unchecked_Integer_4_Unsigned_C(
--                  To_Unchecked_Integer_Address(Font_Buttons))) = FAILED
--            then
--              null; --raise Call_Failure; Why does this fail???
--            end if;
--            if
--            Send_Message(
--              Window        => Buttons(I),
--              Message       => EVENT_SET_TEXT,
--              Data_Unsigned => 0,
--              Data_Signed   =>
--                To_Unchecked_Integer_4_Signed_C(
--                  To_Unchecked_Integer_Address(
--                    To_Access_Constant_Character_2_C(Localize(CONSOLE_BUTTONS(I).Message))))) = FAILED
--            then
--              raise Call_Failure;
--            end if;
--          end loop;
--          --
--          -- Output group box
--          --
--          Output_Group_Box :=
--            Create_Window(
--              Class_Name  => To_String_2_C("Group"),
--              Window_Name => To_String_2_C(NULL_STRING_2),
--              X           =>  7 * Dialog_Base_Unit_Width,
--              Y           =>  7 * Dialog_Base_Unit_Height,
--              Width       => 18 * Dialog_Base_Unit_Width  + Output_Box_WIdth,
--              Height      => 21 * Dialog_Base_Unit_Height + Output_Box_Height,
--              Parent      => NULL_ADDRESS,
--              Menu        => 0,
--              Instance    => Get_Current_Instance,
--              Parameter   => NULL_ADDRESS,
--              Style_Extra => 0,
--              Style       =>
--                STYLE_GROUP_BOX        or
--                STYLE_TITLEBAR_MENU    or
--                STYLE_BORDER_THIN_LINE or
--                STYLE_TITLEBAR_MENU);
--          if Output_Group_Box = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- Input group box
--          --
--          Input_Group_Box :=
--            Create_Window(
--              Class_Name  => To_String_2_C("Group"),
--              Window_Name => To_String_2_C(NULL_STRING_2),
--              X           =>  7 * Dialog_Base_Unit_Width,
--              Y           => 35 * Dialog_Base_Unit_Height + Output_Box_Height,
--              Width       => Output_Box_WIdth,
--              Height      => Input_Box_Height,
--              Parent      => NULL_ADDRESS,
--              Menu        => 0,
--              Instance    => Get_Current_Instance,
--              Parameter   => NULL_ADDRESS,
--              Style_Extra => 0,
--              Style       =>
--                STYLE_GROUP_BOX        or
--                STYLE_TITLEBAR_MENU    or
--                STYLE_BORDER_THIN_LINE or
--                STYLE_TITLEBAR_MENU);
--          if Input_Group_Box = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          --
--          -- Output box
--          --
--          Output_Box :=
--            Create_Window(
--              Class_Name  => To_String_2_C("Edit"),
--              Window_Name => To_String_2_C(NULL_STRING_2),
--              X           => 16 * Dialog_Base_Unit_Width,
--              Y           => 20 * Dialog_Base_Unit_Height,
--              Width       => Output_Box_WIdth,
--              Height      => Output_Box_Height,
--              Parent      => Console,
--              Menu        => 0,
--              Instance    => Get_Current_Instance,
--              Parameter   => NULL_ADDRESS,
--              Style_Extra => 0,
--              Style       =>
--                STYLE_HAS_VERTICAL_SCROLL_BAR   or
--                STYLE_VISIBLE_INITIALLY         or
--                STYLE_BORDER_THIN_LINE          or
--                STYLE_ALIGN_TEXT_TO_LEFT        or
--                STYLE_MULTI_LINE                or
--                STYLE_AUTOMATIC_VERTICAL_SCROLL or
--                STYLE_NO_USER_EDITING           or
--                STYLE_CHILD);
--          if Output_Box = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          if
--          Send_Message(
--            Window        => Output_Box,
--            Message       => EVENT_SET_FONT,
--            Data_Signed   => 0,
--            Data_Unsigned =>
--              To_Unchecked_Integer_4_Unsigned_C(
--                To_Unchecked_Integer_Address(Font_Text_Box))) = FAILED
--          then
--            raise Call_Failure;
--          end if;
--          --
--          -- Input box
--          --
--          Input_Box :=
--            Create_Window(
--              Class_Name  => To_String_2_C("Edit"),
--              Window_Name => To_String_2_C(NULL_STRING_2),
--              X           =>  7 * Dialog_Base_Unit_Width,
--              Y           => 35 * Dialog_Base_Unit_Height + Output_Box_Height,
--              Width       => Output_Box_WIdth,
--              Height      => Input_Box_Height,
--              Parent      => Console,
--              Menu        => 0,
--              Instance    => Get_Current_Instance,
--              Parameter   => NULL_ADDRESS,
--              Style_Extra => 0,
--              Style       =>
--                STYLE_VISIBLE_INITIALLY  or
--                STYLE_BORDER_THIN_LINE   or
--                STYLE_ALIGN_TEXT_TO_LEFT or
--                STYLE_MULTI_LINE         or
--                STYLE_CHILD);
--          if Input_Box = NULL_ADDRESS then
--            raise Call_Failure;
--          end if;
--          if
--          Send_Message(
--            Window        => Input_Box,
--            Message       => EVENT_SET_FONT,
--            Data_Signed   => 0,
--            Data_Unsigned =>
--              To_Unchecked_Integer_4_Unsigned_C(
--                To_Unchecked_Integer_Address(Font_Text_Box))) = FAILED
--          then
--            raise Call_Failure;
--          end if;
--          --
--          -- Initialize
--          --
--          if Show_Window(Console, MAKE_WINDOW_NORMALIZE) = 0 then
--            null;
--          end if;
--          if Update_Window(Console) = FAILED then
--            raise Call_Failure;
--          end if;
--          if Set_Foreground_Window(Console) = FAILED then
--            raise Call_Failure;
--          end if;
--          --
--          -- Run
--          --
--          while Message.Data /= MESSAGE_QUIT loop
--            if Current_Lines < Get_Number_Of_Lines then
--              Current_Lines := Get_Number_Of_Lines;
--              if
--              Send_Message(
--                Window        => Output_Box,
--                Message       => MESSAGE_REPLACE_TEXT,
--                Data_Unsigned => 0,
--                Data_Signed   =>
--                  To_Unchecked_Integer_4_Signed_C(
--                    To_Unchecked_Integer_Address(
--                      To_Access_Constant_Character_2_C(Get_Log)))) = FAILED
--              then
--                raise Call_Failure;
--              end if;
--              if Scrollbar_At_Bottom then
--                if Send_Message(Output_Box, MESSAGE_SCROLL_TEXT, 0, 16#0000_FFFF#) = FAILED then
--                  raise Call_Failure;
--                end if;
--                if Send_Message(Output_Box, MESSAGE_SCROLL_CARET, 0, 0) = FAILED then
--                  raise Call_Failure;
--                end if;
--                if Set_Focus(Output_Box) = NULL_ADDRESS then
--                  raise Call_Failure;
--                end if;
--              end if;
--            end if;
--            if
--            Peek_Message(
--              Message        => Message'unchecked_access,
--              Window         => Console,
--              Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
--              Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
--              Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED and then
--            Translate_Message(Message'unchecked_access) < 2                 and then
--            Dispatch_Message(Message'unchecked_access) = 0
--            then
--              null;
--            end if;
--            delay 0.005;
--          end loop;
--          --
--          -- Finalize
--          --
--          if Show_Window(Console, MAKE_WINDOW_HIDE) = 0 then
--            null;
--          end if;
--          if Destroy_Window(Console) = FAILED then
--            raise Call_Failure;
--          end if;
--          if Unregister_Class(To_String_2_C(Get_Name), NULL_ADDRESS) = FAILED then
--            raise Call_Failure;
--          end if;
--        end Run;
  end Import;
