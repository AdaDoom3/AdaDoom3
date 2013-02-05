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
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.System.Window)
generic
  with
    procedure Handle_Finalization;
  with
    procedure Handle_Activation(
      Do_Activate     : in Boolean;
      Do_Detect_Click : in Boolean;
      X               : in Integer_4_Signed;
      Y               : in Integer_4_Signed);
  with
    procedure Handle_State_Change(
      Change : in Enumerated_Window_Change);
  with
    procedure Handle_Window_Move(
      Window_X : in Integer_4_Signed;
      Window_Y : in Integer_4_Signed;
      Screen_X : in Integer_4_Signed;
      Screen_Y : in Integer_4_Signed);
  with
    function Handle_Resize(
      Resize_Location : in Enumerated_Resize;
      Current_Screen  : in Record_Window_Border)
      return Record_Window_Border;
package body Implementation
  is
  ---------------
  -- Constants --
  ---------------
    SEPORATOR_LEFT                             : constant String_2             := "(";
    SEPORATOR_RIGHT                            : constant String_2             := ")";
    MULTI_MONITOR_NAME                         : constant String_2             := "Multi Monitor";
    BLANK_MOUSE_PATH                           : constant String_2             := "Blank.cur";
    -- Use of the 'window style' that allows for fullscreen boxes on the windowed application also
    -- enables the Aero-Snap™ feature in newer versions of Windows, which breaks resizing higher up in the System.
    DO_DISABLE_FULLSCREEN_BOX_IN_NEWER_THAN_XP : constant Boolean              := True;
    DO_SET_BLANK_MOUSE                         : constant Boolean              := False; -- Removes possibility of a cursor flicker
    FULLSCREEN_STYLE_POSITION                  : constant Integer_4_Unsigned_C := STYLE_EXTRA_ALWAYS_ON_TOP;
    FULLSCREEN_STYLE:
      constant Integer_4_Unsigned_C :=
        STYLE_VISIBLE_INITIALLY or
        STYLE_TITLEBAR_MENU     or
        STYLE_TITLEBAR_LESS_AND_BORDERLESS;
    WINDOWED_STYLE:
      constant Integer_4_Unsigned_C :=
        STYLE_VISIBLE_INITIALLY or
        STYLE_TITLEBAR_MENU     or
        STYLE_TITLEBAR          or
        STYLE_BORDER_THIN_LINE  or
        STYLE_BORDER_SIZABLE    or
        STYLE_BOX_ICONIZE; 
    MULTI_MONITOR_STYLE:
      constant Integer_4_Unsigned_C :=
        FULLSCREEN_STYLE or
        STYLE_NO_ACTIVATE;
  ---------------
  -- Variables --
  ---------------
    Class_Title                  : Access_String_2      := null;
    Operating_System_Name        : Access_String_2      := null;
    Multi_Monitor_Windows        : Access_Array_Address := null;
    Do_Disable_Setting_Of_Cursor : Boolean              := True;
    Do_Disable_Blank_Mouse       : Boolean              := not DO_SET_BLANK_MOUSE;
    Previous_Mouse               : Address              := NULL_ADDRESS;
    Blank_Mouse                  : Address              := NULL_ADDRESS;
    Hook_Keyboard                : Address              := NULL_ADDRESS;
    Hook_Mouse                   : Address              := NULL_ADDRESS;
    Mouse                        : Address              := NULL_ADDRESS;
    Icon                         : Address              := NULL_ADDRESS;
    Original_Clip                : Record_Rectangle;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Class_Name  : in String_2;
      Icon_Path   : in String_2;
      Cursor_Path : in String_2)
      is
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
        ---------------------
        function Extract_Low(
        ---------------------
          Data_Signed : in Integer_4_Signed_C)
          return Integer_4_SIgned
          is
          begin
            return
              Integer_4_Signed(
                To_Integer_2_Signed(
                  Integer_2_Unsigned(
                    To_Integer_4_Unsigned(Data_Signed) and 16#0000_FFFF#)));
          end Extract_Low;
        ----------------------
        function Extract_High(
        ----------------------
          Data_Signed : in Integer_4_Signed_C)
          return Integer_4_SIgned
          is
          begin
            return
              Integer_4_Signed(
                To_Integer_2_Signed(
                  Integer_2_Unsigned(
                     Shift_Right(To_Integer_4_Unsigned(Data_Signed), Integer_2_Unsigned'Size))));
          end Extract_High;
        Point : Record_Point;
        -----
        begin
        -----
          if not DO_USE_LOW_LEVEL_MOUSE and then Get_Cursor_Position(Point'Access) = FAILED then
            null;--raise System_Call_Failure;
          end if;
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0); -- Necessary?
              Handle_Finalization; 
              return C_FALSE;
            when EVENT_MOVE =>
              ------------
              Handle_Move:
              ------------
                declare
                Screen : Record_Rectangle;
                begin
                  if Get_Window_Rectangle(Window, Screen'Access) = FAILED then
                    raise System_Call_Failure;
                  end if;
                  Handle_Window_Move(
                    Window_X => Integer_4_Signed(Screen.Left),
                    Window_Y => Integer_4_Signed(Screen.Top),
                    Screen_X => Extract_Low(Data_Signed),
                    Screen_Y => Extract_High(Data_Signed));
                end Handle_Move;
            when EVENT_COMMAND =>
              case Data_Unsigned is
                when SUBEVENT_MENU_POPOUT | SUBEVENT_SCREEN_SAVER_START =>
                  return C_FALSE;
                when others =>
                  null;
              end case;
            when EVENT_ACTIVATION_CHANGE =>
              if DO_USE_LOW_LEVEL_MOUSE and then Get_Cursor_Position(Point'Access) = FAILED then
                null;--raise System_Call_Failure;
              end if;
              if
              (Data_Unsigned and SUBEVENT_WORD_LOW) = Integer_4_Unsigned_C(C_FALSE) or
              (Data_Unsigned and SUBEVENT_WORD_HIGH) /= 0
              then
                Handle_Activation(
                  Do_Activate     => False,
                  Do_Detect_Click => False,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              elsif (Data_Unsigned and SUBEVENT_WORD_LOW) = SUBEVENT_ACTIVATED_BY_CLICK then
                Handle_Activation(
                  Do_Activate     => True,
                  Do_Detect_Click => True,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              else
                Handle_Activation(
                  Do_Activate     => True,
                  Do_Detect_Click => False,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              end if;
            when EVENT_SIZE_CHANGED =>
              case Data_Unsigned is
                when SUBEVENT_ICONIZED =>
                  Handle_State_Change(Iconic_Change);
                when SUBEVENT_FULLSCREENED =>
                  Handle_State_Change(Fullscreen_Change);
                when SUBEVENT_RESTORED =>
                  null;
                when others =>
                  null;
              end case;
              return C_FALSE;
            when EVENT_SIZING =>
              --------------
              Handle_Sizing:
              --------------
                declare
                Resize_Location : Enumerated_Resize       := Left_Resize;
                Screen          : Access_Record_Rectangle := To_Access_Record_Rectangle(Data_Signed);
                Result          : Record_Window_Border;
                begin
                  case Data_Unsigned is
                    when SUBEVENT_RESIZE_LEFT =>
                      Resize_Location := Left_Resize;
                    when SUBEVENT_RESIZE_RIGHT =>
                      Resize_Location := Right_Resize;
                    when SUBEVENT_RESIZE_TOP =>
                      Resize_Location := Top_Resize;
                    when SUBEVENT_RESIZE_BOTTOM =>
                      Resize_Location := Bottom_Resize;
                    when SUBEVENT_RESIZE_TOP_LEFT =>
                      Resize_Location := Top_Left_Resize;
                    when SUBEVENT_RESIZE_TOP_RIGHT =>
                      Resize_Location := Top_Right_Resize;
                    when SUBEVENT_RESIZE_BOTTOM_RIGHT =>
                      Resize_Location := Bottom_Right_Resize;
                    when SUBEVENT_RESIZE_BOTTOM_LEFT =>
                      Resize_Location := Bottom_Left_Resize;
                    when SUBEVENT_RESIZE_SNAPBACK => -- Not documented
                      null;
                    when others =>
                      raise System_Call_Failure;
                  end case;
                  Result := Handle_Resize(
                    Resize_Location => Resize_Location,
                    Current_Screen  =>(
                      Left   => Integer_4_Signed(Screen.Left),
                      Right  => Integer_4_Signed(Screen.Right),
                      Top    => Integer_4_Signed(Screen.Top),
                      Bottom => Integer_4_Signed(Screen.Bottom)));
                  Screen.All :=(
                    Left   => Integer_4_Signed_C(Result.Left),
                    Right  => Integer_4_Signed_C(Result.Right),
                    Top    => Integer_4_Signed_C(Result.Top),
                    Bottom => Integer_4_Signed_C(Result.Bottom));
                end Handle_Sizing;
            when others =>
              null;
          end case;
          if not DO_USE_LOW_LEVEL_MOUSE then
            Check_Mouse(Message, Data_Unsigned, Point.X, Point.Y);
          end if;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Operating_System : String_2 := Get_Operating_System_Name; 
      Class            : Record_Window_Class;
      -----
      begin
      -----
        Operating_System_Name     := new String_2(1..Operating_System'Length);
        Operating_System_Name.All := Operating_System;
        Class_Title               := new String_2(1..Class_Name'Length);
        Class_Title.All           := Class_Name;
        Icon :=
          Load_Image(
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Icon_Path),
            Kind      => LOAD_ICO,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE);
        if Icon = NULL_ADDRESS then
          Put_Line("Warning, could not find an application icon at " & Icon_Path & ", loading default.");
          Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON);
        end if;
        if DO_SET_BLANK_MOUSE then
          Blank_Mouse := 
            Load_Image(
              Instance  => Get_Current_Instance,
              Name      => To_Access_Constant_Character_2_C(BLANK_MOUSE_PATH),
              Kind      => LOAD_CUR,
              Desired_X => 0,
              Desired_Y => 0,
              Load      => LOAD_FROM_FILE);
          if Blank_Mouse = NULL_ADDRESS then
            Put_Line("Warning, could not find the blank cursor at " & BLANK_MOUSE_PATH & ", ignoring.");
            Do_Disable_Blank_Mouse := True;
          end if;
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
          Size       => Record_Window_Class'Size / 8,
          Style      => 0,
          Callback   => Window_Callback'Access,
          Extra_A    => 0,
          Extra_B    => 0,
          Instance   => Get_Current_Instance,
          Icon_Small => Icon,
          Icon_Large => Icon,
          Cursor     => Mouse,
          Background => BRUSH_GRAY,
          Menu_Name  => null,
          Class_Name => To_Access_Constant_Character_2_C(Class_Title.All));
        if Register_Class(Class'Access) = Integer_2_Unsigned_C(FAILED) then
          raise System_Call_Failure;
        end if;
        Primary_Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Title.All),
            Window_Name => To_String_2_C(Class_Title.All), 
            Style       => WINDOWED_STYLE, --0,
-- Style has to be set to WINDOWED_STYLE otherwise the titlebar icon is not loaded in the Aero™ theme
-- http://social.msdn.microsoft.com/Forums/en-US/windowscompatibility/thread/7b5ef777-ff0d-4f15-afed-5588f93f0e23/
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        if Primary_Window = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 then 
          null;
        end if;
        if Update_Window(Primary_Window) = FAILED then
          raise System_Call_Failure;
        end if;
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
        if Primary_Window /= NULL_ADDRESS then
          if Show_Window(Primary_Window, MAKE_WINDOW_HIDE) = 0 then 
            null;
          end if;
          if Destroy_Window(Primary_Window) = FAILED then 
            raise System_Call_Failure;
          end if;
        end if;
        Primary_Window := NULL_ADDRESS;
        -- Free strings
      end Finalize;
  ------------------------------
  -- Initialize_Multi_Monitor --
  ------------------------------
    procedure Initialize_Multi_Monitor(
      Monitors : in Array_Record_Monitor)
      is
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
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Multi_Monitor_Window : Address             := NULL_ADDRESS;
      Class                : Record_Window_Class :=(
        Size       => Record_Window_Class'Size / 8,
        Style      => 0,
        Callback   => Window_Callback'Access,
        Extra_A    => 0,
        Extra_B    => 0,
        Instance   => Get_Current_Instance,
        Icon_Small => Icon,
        Icon_Large => Icon,
        Cursor     => Mouse,
        Background => BRUSH_GRAY_TEXT,
        Menu_Name  => null,
        Class_Name =>
          To_Access_Constant_Character_2_C(
            Class_Title.All & SEPORATOR_LEFT & MULTI_MONITOR_NAME & SEPORATOR_RIGHT));
      -----
      begin
      -----
        if Register_Class(Class'Access) = Integer_2_Unsigned_C(FAILED) then
          raise System_Call_Failure;
        end if;
        Multi_Monitor_Windows := new Array_Address(1..Monitors'Length - 1);
        -----------------------------
        Create_Multi_Monitor_Windows:
        -----------------------------
          declare
          J                     : Integer_4_Signed     := 1;
          Primary_Window_Border : Record_Window_Border := Get_Window_Border;
          begin
            for I in 1..Monitors'Length loop
              if Primary_Window_Border /= Monitors(I).Desktop then
                Multi_Monitor_Windows(J) :=
                  Create_Window(
                    Style_Extra => 0,
                    Style       => MULTI_MONITOR_STYLE,
                    X           => Integer_4_Signed_C(Monitors(I).Desktop.Left),
                    Y           => Integer_4_Signed_C(Monitors(I).Desktop.Top),
                    Width       => Integer_4_Signed_C(Monitors(I).Desktop.Right  - Monitors(I).Desktop.Left),
                    Height      => Integer_4_Signed_C(Monitors(I).Desktop.Bottom - Monitors(I).Desktop.Top),
                    Parent      => NULL_ADDRESS,
                    Menu        => NULL_ADDRESS,
                    Instance    => Get_Current_Instance,
                    Parameter   => NULL_ADDRESS,
                    Class_Name  =>
                      To_String_2_C(
                        Class_Title.All & SEPORATOR_LEFT & MULTI_MONITOR_NAME & SEPORATOR_RIGHT),
                    Window_Name =>
                      To_String_2_C(
                        Class_Title.All & SEPORATOR_LEFT & Integer_4_Signed'Wide_Image(I) & SEPORATOR_RIGHT));
                if Multi_Monitor_Windows(J) = NULL_ADDRESS then
                  raise System_Call_Failure;
                end if;
                if Show_Window(Multi_Monitor_Windows(J), MAKE_WINDOW_NORMALIZE) = 0 then 
                  null;
                end if;
                if Update_Window(Multi_Monitor_Windows(J)) = FAILED then
                  raise System_Call_Failure;
                end if;
                J := J + 1;
              end if;
            end loop;
          end Create_Multi_Monitor_Windows;
        if Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 then 
          null;
        end if;
        if Update_Window(Primary_Window) = FAILED then
          raise System_Call_Failure;
        end if;
      end Initialize_Multi_Monitor;
  ----------------------------
  -- Finalize_Multi_Monitor --
  ----------------------------
    procedure Finalize_Multi_Monitor
      is
      begin
        for I in 1..Multi_Monitor_Windows'Length loop
          if Show_Window(Multi_Monitor_Windows(I), MAKE_WINDOW_HIDE) = 0 then 
            null;
          end if;
          if Destroy_Window(Multi_Monitor_Windows(I)) = FAILED then 
            raise System_Call_Failure;
          end if;
        end loop;
        if
        Unregister_Class(
          Window_Class => NULL_ADDRESS,
          Class_Name   => To_String_2_C(
            Class_Title.All    & SEPORATOR_LEFT &
            MULTI_MONITOR_NAME & SEPORATOR_RIGHT)) = FAILED
        then
          raise System_Call_Failure;
        end if;
        -- Free Multi_Monitor_Windows
      end Finalize_Multi_Monitor;
  ----------------------------
  -- Get_Screen_Information --
  ----------------------------
    procedure Get_Screen_Information(
      Bits_Per_Pixel : in out Integer_4_Positive;
      Native_Width   : in out Integer_4_Positive;
      Native_Height  : in out Integer_4_Positive)
      is
      Context : Address := NULL_ADDRESS;
      begin
        Context        := Get_Device_Context(Get_Desktop_Window);
        Bits_Per_Pixel := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_NUMBER_OF_BITS_PER_PIXEL));
        Native_Width   := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_HORZONTAL_RESOLUTION));
        Native_Height  := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION));
        if Release_Device_Context(Get_Desktop_Window, Context) = FAILED then
          raise System_Call_Failure;
        end if;
      end Get_Screen_Information;
  ------------------
  -- Get_Monitors --
  ------------------
    function Get_Monitors
      return Array_Record_Monitor
      is
      Count : Integer_4_Natural := 0;
      --------------------------------
      function Monitor_Count_Callback( -- To avoid temporary linked lists
      --------------------------------
        Monitor        : in Address;
        Device_Context : in Address;
        Screen         : in Access_Record_Rectangle;
        Data           : in Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        begin
          Count := Count + 1;
          return C_TRUE;
        end Monitor_Count_Callback;
      --------------------------
      function Monitor_Callback(
      --------------------------
        Monitor        : in Address;
        Device_Context : in Address;
        Screen         : in Access_Record_Rectangle;
        Monitors       : in Access_Array_Record_Monitor))
        return Integer_4_Signed_C
        is
        Monitor_Information : Record_Monitor_Information := <>;
        begin
          if
          Get_Monitor_Information(
            Monitor     => Monitor,
            Information => Monitor_Information'Access) = FAILED
          then
            raise System_Call_Failure;
          end if;
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
      -----
      begin
      -----
        if
        Enumerate_Display_Monitor(
          Device_Context => NULL_ADDRESS,
          Clip           => NULL_ADDRESS,
          Callback       => Monitor_Count_Callback'Access,
          Data           => 0) = FAILED 
        then
          raise System_Call_Failure;
        end if;
        if Count = 0 then
          raise System_Call_Failure;
        end if;
        ---------------
        Compose_Result:
        ---------------
          declare
          Monitors : Array_Record_Monitor(1..Count);
          begin
            Count := 1; -- It becomes an index
            if
            Enumerate_Display_Monitor(
              Device_Context => NULL_ADDRESS,
              Clip           => NULL_ADDRESS,
              Callback       => Monitor_Callback'Access,
              Data           => Monitors'Access) = FAILED 
            then
              raise System_Call_Failure;
            end if;
            return Monitors;
          end Compose_Result;
      end Get_Monitors;
  -----------------------
  -- Get_Window_Border --
  -----------------------
    function Get_Window_Border
      return Record_Window_Border
      is
      Screen : Record_Rectangle;
      begin
        if Get_Window_Rectangle(Primary_Window, Screen'Access) = FAILED then
          raise System_Call_Failure;
        end if;
        return(
          Left   => Integer_4_Signed(Screen.Left),
          Right  => Integer_4_Signed(Screen.Right),
          Top    => Integer_4_Signed(Screen.Top),
          Bottom => Integer_4_Signed(Screen.Bottom));
      end Get_Window_Border;
  -----------------------
  -- Get_Screen_Border --
  -----------------------
    function Get_Screen_Border
      return Record_Window_Border
      is
      Decoration : Record_Rectangle;
      Screen     : Record_Rectangle;
      Border     : Integer_4_Signed_C := 0;
      Top        : Integer_4_Signed_C := 0;
      begin
        if
        Adjust_Window_Rectangle(
          Rectangle   => Decoration'Access,
          Style       => WINDOWED_STYLE,
          Menu        => C_FALSE,
          Extra_Style => 0) = FAILED
        then
          raise System_Call_Failure;
        end if;
        if Get_Window_Rectangle(Primary_Window, Screen'Access) = FAILED then
          raise System_Call_Failure;
        end if;
        Border := (Decoration.Right  - Decoration.Left) / 2;
        Top    := (Decoration.Bottom - Decoration.Top) - Border;
        return(
          Left   => Integer_4_Signed(Screen.Left   + Border),
          Right  => Integer_4_Signed(Screen.Right  - Border),
          Top    => Integer_4_Signed(Screen.Top    + Top),
          Bottom => Integer_4_Signed(Screen.Bottom - Border));
      end Get_Screen_Border;
  ----------------------
  -- Set_Custom_Mouse --
  ----------------------
    procedure Set_Custom_Mouse(
      Do_Restore_System_Mouse : in Boolean := False)
      is
      begin
        if not Do_Disable_Setting_Of_Cursor then
          if Do_Restore_System_Mouse then
            if
            Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(
                Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR))) = 0
            then
              raise System_Call_Failure;
            end if;
          else
            if Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(Mouse)) = 0
            then
              raise System_Call_Failure;
            end if;
          end if;
        end if;
      end Set_Custom_Mouse;
  -------------------
  -- Handle_Events --
  -------------------
    function Handle_Events(
      Index : in Integer_4_Natural := 0)
      return Boolean
      is
      Message : Record_Message;
      Window  : Address := Primary_Window;
      begin
        if Index /= 0 then
          if Multi_Monitor_Windows = null or Index > Multi_Monitor_Windows'Size then
            return False;
          end if;
          Window := Multi_Monitor_Windows(Index);
        end if;
        if
        Peek_Message(
          Message        => Message'Access,
          Window         => Window,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        then
          if Message.Data = MESSAGE_QUIT then
            return False;
          elsif
          Translate_Message(Message'Access) < 2 and then
          Dispatch_Message(Message'Access) = 0
          then
            null;
          end if;
        end if;
        return True;
      end Handle_Events;
  ------------
  -- Adjust --
  ------------
    procedure Adjust(
      X             : in Integer_4_Signed;
      Y             : in Integer_4_Signed;
      Title         : in String_2;
      Width         : in Integer_4_Positive;
      Height        : in Integer_4_Positive;
      Do_Fullscreen : in Boolean)
      is
      begin
        if Primary_Window = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if Do_Fullscreen then
          if
          Change_Window_Setting(
            Window  => Primary_Window,
            Command => SET_WINDOW_STYLE,
            Setting => FULLSCREEN_STYLE) = 0
          then
            raise System_Call_Failure;
          end if;
          if
          Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_EVERYTHING,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0) = FAILED
          then
            raise System_Call_Failure;
          end if;
        else
          if
          DO_DISABLE_FULLSCREEN_BOX_IN_NEWER_THAN_XP and then
          Is_Newer_Than("Windows XP")
          then
            if
            Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE) = 0
            then
              raise System_Call_Failure;
            end if;
          else
            if
            Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE or STYLE_BOX_FULLSCREEN) = 0
            then
              raise System_Call_Failure;
            end if;
          end if;
          if
          Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_APPLICATIONS,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0) = FAILED
          then
            raise System_Call_Failure;
          end if;
        end if;
        if Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 then 
          null;
        end if;
        if Update_Window(Primary_Window) = FAILED then
          raise System_Call_Failure;
        end if;
      end Adjust;
  ----------------------
  -- Is_Only_Instance --
  ----------------------
    function Is_Only_Instance(
      Name : in String_2)
      return Boolean
      is
      Handle : Address := NULL_ADDRESS;
      Window : Address := NULL_ADDRESS;
      begin
        Handle := Create_Mutex(NULL_ADDRESS, C_TRUE, To_String_2_C(Name));
        if Get_Last_Error /= NO_ERROR then
          if Release_Mutex(Handle) = FAILED then
            raise System_Call_Failure;
          end if;
          Primary_Window := Find_Window(To_String_2_C(Name), NULL_ADDRESS);
          if Primary_Window /= NULL_ADDRESS then
            if Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 then
              null;
            end if;
            Handle := Set_Focus(Primary_Window);
            if Set_Foreground_Window(Primary_Window) = FAILED then
              raise System_Call_Failure;
            end if;
            Handle := Set_Active_Window(Primary_Window);
          end if;
          return False;
        end if;
        return True;
      end Is_Only_Instance;
  -----------------------------------------
  -- Move_Topmost_Windows_Out_Of_The_Way --
  -----------------------------------------
    procedure Move_Topmost_Windows_Out_Of_The_Way
      is
      --------------------
      function Is_Covered
      --------------------
        return Boolean
        is
        Clip           : Record_Rectangle;
        Client         : Record_Rectangle;
        Result         : Boolean := False;
        Device_Context : Address := Get_Device_Context(Primary_Window);
        begin
          if Device_Context = NULL_ADDRESS then
            raise System_Call_Failure;
          end if;
          case Get_Clip_Box(Device_Context, Clip'Access) is
            when REGION_NULL | REGION_COMPLEX =>
              Result := True;
            when REGION_SIMPLE =>
              if Get_Client_Rectangle(Primary_Window, Client'Access) = FAILED then
                raise System_Call_Failure;
              end if;
              if Rectangles_Are_Equal(Client'Access, Clip'Access) = C_FALSE then
                Result := True;
              end if;
            when others =>
              raise System_Call_Failure;
          end case;
          if Release_Device_Context(Primary_Window, Device_Context) = FAILED then
            raise System_Call_Failure;
          end if;
          return Result;
        end Is_Covered;
      -----
      begin
      -----
        if Is_Covered then
          -- loop through windows with higher Z value
            -- Find_Intersecting_Rectangle
              -- Make it move behind, if that fails minimize it, if that fails, move it
            -- If it is not covered exit
          null;
        end if;
      end Move_Topmost_Windows_Out_Of_The_Way;
  ----------------
  -- Hide_Mouse --
  ----------------
    procedure Hide_Mouse(
      Do_Hide            : in Boolean;
      Do_Ignore_Titlebar : in Boolean := False)
      is
      ---------------------
      procedure Clip_Mouse(
      ---------------------
        Do_Clip            : in Boolean;
        Do_Ignore_Titlebar : in Boolean)
        is
        Border        : Record_Window_Border := Get_Screen_Border;
        New_Clip_Area : Record_Rectangle :=(
          Left   => Integer_4_Signed_C(Border.Left),
          Top    => Integer_4_Signed_C(Border.Top),
          Right  => Integer_4_Signed_C(Border.Right),
          Bottom => Integer_4_Signed_C(Border.Bottom));
        begin
          if Do_Ignore_Titlebar and then Get_Window_Rectangle(Primary_Window, New_Clip_Area'Access) = FAILED then
            raise System_Call_Failure;
          end if;
          if not Do_Clip then
            if Original_Clip /= <> then
              if Clip_Cursor(Original_Clip'Access) = FAILED then
                raise System_Call_Failure;
              end if;
              Original_Clip := <>;
            end if;
          else
            if Original_Clip = <> then
              if Get_Clip_Cursor_Area(Original_Clip'Access) = FAILED then
                raise System_Call_Failure;
              end if;
              if Original_Clip = <> then
                raise System_Call_Failure;
              end if;
            end if;
            if Clip_Cursor(New_Clip_Area'Access) = FAILED then
              raise System_Call_Failure;
            end if;
          end if;
        end Clip_Mouse;
      -----
      begin
      -----
        if Do_Hide then
          while Show_Cursor(Boolean'Pos(not Do_Hide)) > -1 loop
            null;
          end loop;
          if not Do_Disable_Blank_Mouse then
            if Previous_Mouse = NULL_ADDRESS then
              Previous_Mouse := To_Address(Get_Class_Setting(Primary_Window, GET_CLASS_CURSOR));
              if Previous_Mouse = NULL_ADDRESS then
                raise System_Call_Failure;
              end if;
            end if;
            if Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Blank_Mouse)) = 0 then
              raise System_Call_Failure;
            end if;
          end if;
          Clip_Mouse(True, Do_Ignore_Titlebar);
        else
          if
          not Do_Disable_Blank_Mouse     and then
          Previous_Mouse /= NULL_ADDRESS and then
          Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Previous_Mouse)) = 0
          then
            raise System_Call_Failure;
          end if;
          Clip_Mouse(False, False);
          while Show_Cursor(Boolean'Pos(not Do_Hide)) < 0 loop
            null;
          end loop;
        end if;
      end Hide_Mouse;
  -------------
  -- Iconize --
  -------------
    procedure Iconize
      is
      begin
        if Show_Window(Primary_Window, MAKE_WINDOW_GO_TO_ICONIC) = 0 then
          null;
        end if;
      end Iconize;
  end Implementation;
