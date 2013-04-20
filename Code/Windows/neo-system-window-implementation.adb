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
WITH
  Neo.Windows;
USE
  Neo.Windows;
SEPARATE(Neo.System.Window)
GENERIC
  WITH
    PROCEDURE Handle_Finalization;
  WITH
    PROCEDURE Handle_Activation(
      Do_Activate     : IN Boolean;
      Do_Detect_Click : IN Boolean;
      X               : IN Integer_4_Signed;
      Y               : IN Integer_4_Signed);
  WITH
    PROCEDURE Handle_State_Change(
      Change : IN Enumerated_Window_Change);
  WITH
    PROCEDURE Handle_Window_Move(
      Window_X : IN Integer_4_Signed;
      Window_Y : IN Integer_4_Signed;
      Screen_X : IN Integer_4_Signed;
      Screen_Y : IN Integer_4_Signed);
  WITH
    FUNCTION Handle_Resize(
      Resize_Location : IN Enumerated_Resize;
      Current_Screen  : IN Record_Window_Border)
      RETURN Record_Window_Border;
PACKAGE BODY Implementation
  IS
  ---------------
  -- CONSTANTS --
  ---------------
    -- Use of the 'window style' that allows FOR fullscreen boxes on the windowed application also
    -- enables the Aero-Snap™ feature IN newer versions of Windows, which breaks resizing higher up IN the System.
    DO_DISABLE_FULLSCREEN_BOX_IN_NEWER_THAN_XP : CONSTANT Boolean              := True;
    DO_SET_BLANK_MOUSE                         : CONSTANT Boolean              := False; -- Removes possibility of a cursor flicker
    SEPORATOR_LEFT                             : CONSTANT String_2             := "(";
    SEPORATOR_RIGHT                            : CONSTANT String_2             := ")";
    MULTI_MONITOR_NAME                         : CONSTANT String_2             := "Multi Monitor";
    FULLSCREEN_STYLE_POSITION                  : CONSTANT Integer_4_Unsigned_C := STYLE_EXTRA_ALWAYS_ON_TOP;
    FULLSCREEN_STYLE:
      CONSTANT Integer_4_Unsigned_C :=
        STYLE_VISIBLE_INITIALLY OR
        STYLE_TITLEBAR_MENU     OR
        STYLE_TITLEBAR_LESS_AND_BORDERLESS;
    WINDOWED_STYLE:
      CONSTANT Integer_4_Unsigned_C :=
        STYLE_VISIBLE_INITIALLY OR
        STYLE_TITLEBAR_MENU     OR
        STYLE_TITLEBAR          OR
        STYLE_BORDER_THIN_LINE  OR
        STYLE_BORDER_SIZABLE    OR
        STYLE_BOX_ICONIZE; 
    MULTI_MONITOR_STYLE:
      CONSTANT Integer_4_Unsigned_C :=
        FULLSCREEN_STYLE OR
        STYLE_NO_ACTIVATE;
  ---------------
  -- VARIABLES --
  ---------------
    Class_Title                  :         Access_String_2      := NULL;
    Operating_System_Name        :         Access_String_2      := NULL;
    Multi_Monitor_Windows        :         Access_Array_Address := NULL;
    Do_Disable_Setting_Of_Cursor :         Boolean              := True;
    Do_Disable_Blank_Mouse       :         Boolean              := NOT DO_SET_BLANK_MOUSE;
    Previous_Mouse               :         Address              := NULL_ADDRESS;
    Blank_Mouse                  :         Address              := NULL_ADDRESS;
    Hook_Keyboard                :         Address              := NULL_ADDRESS;
    Hook_Mouse                   :         Address              := NULL_ADDRESS;
    Mouse                        :         Address              := NULL_ADDRESS;
    Icon                         :         Address              := NULL_ADDRESS;
    Original_Clip                : ALIASED Record_Rectangle     := <>;
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize(
      Class_Name  : IN String_2;
      Icon_Path   : IN String_2;
      Cursor_Path : IN String_2)
      IS
      -------------------------
      FUNCTION Window_Callback(
      -------------------------
        Window        : IN Address;
        Message       : IN Integer_4_Unsigned_C;
        Data_Unsigned : IN Integer_4_Unsigned_C;
        Data_Signed   : IN Integer_4_Signed_C)
        RETURN Integer_4_Signed_C;
        PRAGMA Convention(Stdcall, Window_Callback);
      FUNCTION Window_Callback(
        Window        : IN Address;
        Message       : IN Integer_4_Unsigned_C;
        Data_Unsigned : IN Integer_4_Unsigned_C;
        Data_Signed   : IN Integer_4_Signed_C)
        RETURN Integer_4_Signed_C
        IS
        ---------------------
        FUNCTION Extract_Low(
        ---------------------
          Data_Signed : IN Integer_4_Signed_C)
          RETURN Integer_4_SIgned
          IS
          BEGIN
            RETURN
              Integer_4_Signed(
                To_Integer_2_Signed(
                  Integer_2_Unsigned(
                    To_Integer_4_Unsigned(Data_Signed) AND 16#0000_FFFF#)));
          END Extract_Low;
        ----------------------
        FUNCTION Extract_High(
        ----------------------
          Data_Signed : IN Integer_4_Signed_C)
          RETURN Integer_4_SIgned
          IS
          BEGIN
            RETURN
              Integer_4_Signed(
                To_Integer_2_Signed(
                  Integer_2_Unsigned(
                     Shift_Right(To_Integer_4_Unsigned(Data_Signed), Integer_2_Unsigned'Size))));
          END Extract_High;
        Point : Record_Point;
        -----
        BEGIN
        -----
          CASE Message IS
            WHEN EVENT_CLOSE =>
              Post_Quit_Message(0); -- Necessary?
              Handle_Finalization; 
              RETURN C_FALSE;
            WHEN EVENT_MOVE =>
              ------------
              Handle_Move:
              ------------
                DECLARE
                Screen : Record_Rectangle;
                BEGIN
                  IF Get_Window_Rectangle(Window, Screen'Access) = FAILED THEN
                    RAISE System_Call_Failure;
                  END IF;
                  Handle_Window_Move(
                    Window_X => Integer_4_Signed(Screen.Left),
                    Window_Y => Integer_4_Signed(Screen.Top),
                    Screen_X => Extract_Low(Data_Signed),
                    Screen_Y => Extract_High(Data_Signed));
                END Handle_Move;
            WHEN EVENT_COMMAND =>
              CASE Data_Unsigned IS
                WHEN SUBEVENT_MENU_POPOUT | SUBEVENT_SCREEN_SAVER_START =>
                  RETURN C_FALSE;
                WHEN OTHERS =>
                  NULL;
              END CASE;
            WHEN EVENT_ACTIVATION_CHANGE =>
              IF
              (Data_Unsigned AND SUBEVENT_WORD_LOW) = Integer_4_Unsigned_C(C_FALSE) OR
              (Data_Unsigned AND SUBEVENT_WORD_HIGH) /= 0
              THEN
                Handle_Activation(
                  Do_Activate     => False,
                  Do_Detect_Click => False,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              ELSIF (Data_Unsigned AND SUBEVENT_WORD_LOW) = SUBEVENT_ACTIVATED_BY_CLICK THEN
                Handle_Activation(
                  Do_Activate     => True,
                  Do_Detect_Click => True,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              ELSE
                Handle_Activation(
                  Do_Activate     => True,
                  Do_Detect_Click => False,
                  X               => Integer_4_Signed(Point.X),
                  Y               => Integer_4_Signed(Point.Y));
              END IF;
            WHEN EVENT_SIZE_CHANGED =>
              CASE Data_Unsigned IS
                WHEN SUBEVENT_ICONIZED =>
                  Handle_State_Change(Iconic_Change);
                WHEN SUBEVENT_FULLSCREENED =>
                  Handle_State_Change(Fullscreen_Change);
                WHEN OTHERS =>
                  NULL;
              END CASE;
              RETURN C_FALSE;
            WHEN EVENT_SIZING =>
              --------------
              Handle_Sizing:
              --------------
                DECLARE
                Resize_Location : Enumerated_Resize       := Left_Resize;
                Screen          : Access_Record_Rectangle := To_Access_Record_Rectangle(Data_Signed);
                Result          : Record_Window_Border;
                BEGIN
                  CASE Data_Unsigned IS
                    WHEN SUBEVENT_RESIZE_TOP =>
                      Resize_Location := Top_Resize;
                    WHEN SUBEVENT_RESIZE_LEFT =>
                      Resize_Location := Left_Resize;
                    WHEN SUBEVENT_RESIZE_RIGHT =>
                      Resize_Location := Right_Resize;
                    WHEN SUBEVENT_RESIZE_BOTTOM =>
                      Resize_Location := Bottom_Resize;
                    WHEN SUBEVENT_RESIZE_TOP_LEFT =>
                      Resize_Location := Top_Left_Resize;
                    WHEN SUBEVENT_RESIZE_TOP_RIGHT =>
                      Resize_Location := Top_Right_Resize;
                    WHEN SUBEVENT_RESIZE_BOTTOM_LEFT =>
                      Resize_Location := Bottom_Left_Resize;
                    WHEN SUBEVENT_RESIZE_BOTTOM_RIGHT =>
                      Resize_Location := Bottom_Right_Resize;
                    WHEN OTHERS =>
                      GOTO Ignore_Resize;
                  END CASE;
                  Result :=
                    Handle_Resize(
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
                  <<Ignore_Resize>>
                END Handle_Sizing;
            WHEN OTHERS =>
              NULL;
          END CASE;
          RETURN Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        END Window_Callback;
      Operating_System :         String_2            := Get_Operating_System_Name; 
      Class            : ALIASED Record_Window_Class := <>;
      -----
      BEGIN
      -----
        Operating_System_Name     := NEW String_2(1..Operating_System'Length);
        Operating_System_Name.All := Operating_System;
        Class_Title               := NEW String_2(1..Class_Name'Length);
        Class_Title.All           := Class_Name;
        Icon :=
          Load_Image(
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Icon_Path),
            Kind      => LOAD_ICO,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE);
        IF Icon = NULL_ADDRESS THEN
          Put_Line("Warning, could NOT find an application icon at " & Icon_Path & ", loading default.");
          Icon := Load_Icon(Get_Current_Instance, GENERIC_ICON);
        END IF;
        IF DO_SET_BLANK_MOUSE THEN
          Blank_Mouse := 
            Load_Image(
              Instance  => Get_Current_Instance,
              Name      => To_Access_Constant_Character_2_C(BLANK_MOUSE_PATH),
              Kind      => LOAD_CUR,
              Desired_X => 0,
              Desired_Y => 0,
              Load      => LOAD_FROM_FILE);
          IF Blank_Mouse = NULL_ADDRESS THEN
            Put_Line("Warning, could NOT find the blank cursor at " & BLANK_MOUSE_PATH & ", ignoring.");
            Do_Disable_Blank_Mouse := True;
          END IF;
        END IF;
        Mouse := 
          Load_Image(
            Instance  => Get_Current_Instance,
            Name      => To_Access_Constant_Character_2_C(Cursor_Path),
            Kind      => LOAD_CUR,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE);
        IF Mouse = NULL_ADDRESS THEN
          Put_Line("Warning, could NOT find cursor at path " & Cursor_Path & " to load.");
          Mouse := Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR);
        ELSE
          Do_Disable_Setting_Of_Cursor := False;
        END IF;
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
          Menu_Name  => NULL,
          Class_Name => To_Access_Constant_Character_2_C(Class_Title.All));
        IF Register_Class(Class'Access) = Integer_2_Unsigned_C(FAILED) THEN
          RAISE System_Call_Failure;
        END IF;
        Primary_Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Title.All),
            Window_Name => To_String_2_C(Class_Title.All), 
            Style       => WINDOWED_STYLE, --0,
-- Style has to be set to WINDOWED_STYLE otherwise the titlebar icon IS NOT loaded IN the Aero™ theme
-- http://social.msdn.microsoft.com/Forums/en-US/windowscompatibility/thread/7b5ef777-ff0d-4f15-afed-5588f93f0e23/
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        IF Primary_Window = NULL_ADDRESS THEN
          RAISE System_Call_Failure;
        END IF;
        IF Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 THEN 
          NULL;
        END IF;
        IF Update_Window(Primary_Window) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
      END Initialize;
  --------------
  -- Finalize --
  --------------
    PROCEDURE Finalize
      IS
      BEGIN
        IF Primary_Window /= NULL_ADDRESS THEN
          IF Show_Window(Primary_Window, MAKE_WINDOW_HIDE) = 0 THEN 
            NULL;
          END IF;
          IF Destroy_Window(Primary_Window) = FAILED THEN 
            RAISE System_Call_Failure;
          END IF;
        END IF;
        Primary_Window := NULL_ADDRESS;
        -- Free strings
      END Finalize;
  ------------------------------
  -- Initialize_Multi_Monitor --
  ------------------------------
    PROCEDURE Initialize_Multi_Monitor(
      Monitors : IN Array_Record_Monitor)
      IS
      -------------------------
      FUNCTION Window_Callback(
      -------------------------
        Window        : IN Address;
        Message       : IN Integer_4_Unsigned_C;
        Data_Unsigned : IN Integer_4_Unsigned_C;
        Data_Signed   : IN Integer_4_Signed_C)
        RETURN Integer_4_Signed_C;
        PRAGMA Convention(Stdcall, Window_Callback);
      FUNCTION Window_Callback(
        Window        : IN Address;
        Message       : IN Integer_4_Unsigned_C;
        Data_Unsigned : IN Integer_4_Unsigned_C;
        Data_Signed   : IN Integer_4_Signed_C)
        RETURN Integer_4_Signed_C
        IS
        BEGIN
          CASE Message IS
            WHEN EVENT_CLOSE =>
              Post_Quit_Message(0);
              RETURN C_FALSE;
            WHEN OTHERS =>
              NULL;
          END CASE;
          RETURN Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        END Window_Callback;
      Multi_Monitor_Window :         Address             := NULL_ADDRESS;
      Class                : ALIASED Record_Window_Class :=(
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
        Menu_Name  => NULL,
        Class_Name =>
          To_Access_Constant_Character_2_C(
            Class_Title.All & SEPORATOR_LEFT & MULTI_MONITOR_NAME & SEPORATOR_RIGHT));
      -----
      BEGIN
      -----
        IF Register_Class(Class'Access) = Integer_2_Unsigned_C(FAILED) THEN
          RAISE System_Call_Failure;
        END IF;
        Multi_Monitor_Windows := NEW Array_Address(1..Monitors'Length - 1);
        -----------------------------
        Create_Multi_Monitor_Windows:
        -----------------------------
          DECLARE
          J                     : Integer_4_Signed     := 1;
          Primary_Window_Border : Record_Window_Border := Get_Window_Border;
          BEGIN
            FOR I IN 1..Monitors'Length LOOP
              IF Primary_Window_Border /= Monitors(I).Desktop THEN
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
                IF Multi_Monitor_Windows(J) = NULL_ADDRESS THEN
                  RAISE System_Call_Failure;
                END IF;
                IF Show_Window(Multi_Monitor_Windows(J), MAKE_WINDOW_NORMALIZE) = 0 THEN 
                  NULL;
                END IF;
                IF Update_Window(Multi_Monitor_Windows(J)) = FAILED THEN
                  RAISE System_Call_Failure;
                END IF;
                J := J + 1;
              END IF;
            END LOOP;
          END Create_Multi_Monitor_Windows;
        IF Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 THEN 
          NULL;
        END IF;
        IF Update_Window(Primary_Window) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
      END Initialize_Multi_Monitor;
  ----------------------------
  -- Finalize_Multi_Monitor --
  ----------------------------
    PROCEDURE Finalize_Multi_Monitor
      IS
      BEGIN
        FOR I IN 1..Multi_Monitor_Windows'Length LOOP
          IF Show_Window(Multi_Monitor_Windows(I), MAKE_WINDOW_HIDE) = 0 THEN 
            NULL;
          END IF;
          IF Destroy_Window(Multi_Monitor_Windows(I)) = FAILED THEN 
            RAISE System_Call_Failure;
          END IF;
        END LOOP;
        IF
        Unregister_Class(
          Window_Class => NULL_ADDRESS,
          Class_Name   => To_String_2_C(
            Class_Title.All & SEPORATOR_LEFT & MULTI_MONITOR_NAME & SEPORATOR_RIGHT)) = FAILED
        THEN
          RAISE System_Call_Failure;
        END IF;
        -- Free Multi_Monitor_Windows
      END Finalize_Multi_Monitor;
  ----------------------------
  -- Get_Screen_Information --
  ----------------------------
    PROCEDURE Get_Screen_Information(
      Bits_Per_Pixel : IN OUT Integer_4_Positive;
      Native_Width   : IN OUT Integer_4_Positive;
      Native_Height  : IN OUT Integer_4_Positive)
      IS
      Context : Address := NULL_ADDRESS;
      BEGIN
        Context        := Get_Device_Context(Get_Desktop_Window);
        Bits_Per_Pixel := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_NUMBER_OF_BITS_PER_PIXEL));
        Native_Width   := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_HORZONTAL_RESOLUTION));
        Native_Height  := Integer_4_Positive(Get_Device_Capabilities(Context, DATA_VERTICAL_RESOLUTION));
        IF Release_Device_Context(Get_Desktop_Window, Context) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
      END Get_Screen_Information;
  ------------------
  -- Get_Monitors --
  ------------------
    FUNCTION Get_Monitors
      RETURN Array_Record_Monitor
      IS
      Count : Integer_4_Natural := 0;
      --------------------------------
      FUNCTION Monitor_Count_Callback( -- To avoid temporary linked lists
      --------------------------------
        Monitor        : IN Address;
        Device_Context : IN Address;
        Screen         : IN Access_Record_Rectangle;
        Data           : IN Integer_4_Signed_C)
        RETURN Integer_4_Signed_C
        IS
        BEGIN
          Count := Count + 1;
          RETURN C_TRUE;
        END Monitor_Count_Callback;
      --------------------------
      FUNCTION Monitor_Callback(
      --------------------------
        Monitor        : IN Address;
        Device_Context : IN Address;
        Screen         : IN Access_Record_Rectangle;
        Monitors       : IN Access_Array_Record_Monitor))
        RETURN Integer_4_Signed_C
        IS
        Monitor_Information : ALIASED Record_Monitor_Information := <>;
        BEGIN
          IF
          Get_Monitor_Information(
            Monitor     => Monitor,
            Information => Monitor_Information'Access) = FAILED
          THEN
            RAISE System_Call_Failure;
          END IF;
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
          RETURN C_TRUE;
        END Monitor_Callback;
      -----
      BEGIN
      -----
        IF
        Enumerate_Display_Monitor(
          Device_Context => NULL_ADDRESS,
          Clip           => NULL_ADDRESS,
          Callback       => Monitor_Count_Callback'Access,
          Data           => 0) = FAILED 
        THEN
          RAISE System_Call_Failure;
        END IF;
        IF Count = 0 THEN
          RAISE System_Call_Failure;
        END IF;
        ---------------
        Compose_Result:
        ---------------
          DECLARE
          Monitors : Array_Record_Monitor(1..Count);
          BEGIN
            Count := 1; -- It becomes an index
            IF
            Enumerate_Display_Monitor(
              Device_Context => NULL_ADDRESS,
              Clip           => NULL_ADDRESS,
              Callback       => Monitor_Callback'Access,
              Data           => Monitors'Access) = FAILED 
            THEN
              RAISE System_Call_Failure;
            END IF;
            RETURN Monitors;
          END Compose_Result;
      END Get_Monitors;
  -----------------------
  -- Get_Window_Border --
  -----------------------
    FUNCTION Get_Window_Border
      RETURN Record_Window_Border
      IS
      Screen : ALIASED Record_Rectangle := <>;
      BEGIN
        IF Get_Window_Rectangle(Primary_Window, Screen'Access) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
        RETURN((
          Left   => Integer_4_Signed(Screen.Left),
          Right  => Integer_4_Signed(Screen.Right),
          Top    => Integer_4_Signed(Screen.Top),
          Bottom => Integer_4_Signed(Screen.Bottom)));
      END Get_Window_Border;
  -----------------------
  -- Get_Screen_Border --
  -----------------------
    FUNCTION Get_Screen_Border
      RETURN Record_Window_Border
      IS
      Decoration : ALIASED Record_Rectangle   := <>;
      Screen     : ALIASED Record_Rectangle   := <>;
      Border     :         Integer_4_Signed_C := 0;
      Top        :         Integer_4_Signed_C := 0;
      BEGIN
        IF
        Adjust_Window_Rectangle(
          Rectangle   => Decoration'Access,
          Style       => WINDOWED_STYLE,
          Menu        => C_FALSE,
          Extra_Style => 0) = FAILED
        THEN
          RAISE System_Call_Failure;
        END IF;
        IF Get_Window_Rectangle(Primary_Window, Screen'Access) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
        Border := (Decoration.Right  - Decoration.Left) / 2;
        Top    := (Decoration.Bottom - Decoration.Top) - Border;
        RETURN((
          Left   => Integer_4_Signed(Screen.Left   + Border),
          Right  => Integer_4_Signed(Screen.Right  - Border),
          Top    => Integer_4_Signed(Screen.Top    + Top),
          Bottom => Integer_4_Signed(Screen.Bottom - Border)));
      END Get_Screen_Border;
  ----------------------
  -- Set_Custom_Mouse --
  ----------------------
    PROCEDURE Set_Custom_Mouse(
      Do_Restore_System_Mouse : IN Boolean := False)
      IS
      BEGIN
        IF NOT Do_Disable_Setting_Of_Cursor THEN
          IF Do_Restore_System_Mouse THEN
            IF
            Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(
                Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR))) = 0
            THEN
              RAISE System_Call_Failure;
            END IF;
          ELSE
            IF Change_Class_Setting(
              Window  => Primary_Window,
              Command => SET_CLASS_CURSOR,
              Setting => To_Integer_4_Unsigned_C(Mouse)) = 0
            THEN
              RAISE System_Call_Failure;
            END IF;
          END IF;
        END IF;
      END Set_Custom_Mouse;
  -------------------
  -- Handle_Events --
  -------------------
    FUNCTION Handle_Events(
      Index : IN Integer_4_Natural := 0)
      RETURN Boolean
      IS
      Message : ALIASED Record_Message := (OTHERS => <>);
      Window  :         Address        := Primary_Window;
      BEGIN
        IF Index /= 0 THEN
          IF Multi_Monitor_Windows = NULL OR Index > Multi_Monitor_Windows'Size THEN
            RETURN False;
          END IF;
          Window := Multi_Monitor_Windows(Index);
        END IF;
        IF
        Peek_Message(
          Message        => Message'Access,
          Window         => Window,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        THEN
          IF Message.Data = MESSAGE_QUIT THEN
            RETURN False;
          ELSIF
          Translate_Message(Message'Access) < 2 AND THEN
          Dispatch_Message(Message'Access) = 0
          THEN
            NULL;
          END IF;
        END IF;
        RETURN True;
      END Handle_Events;
  ------------
  -- Adjust --
  ------------
    PROCEDURE Adjust(
      X             : IN Integer_4_Signed;
      Y             : IN Integer_4_Signed;
      Title         : IN String_2;
      Width         : IN Integer_4_Positive;
      Height        : IN Integer_4_Positive;
      Do_Fullscreen : IN Boolean)
      IS
      BEGIN
        IF Primary_Window = NULL_ADDRESS THEN
          RAISE System_Call_Failure;
        END IF;
        IF Do_Fullscreen THEN
          IF
          Change_Window_Setting(
            Window  => Primary_Window,
            Command => SET_WINDOW_STYLE,
            Setting => FULLSCREEN_STYLE) = 0
          THEN
            RAISE System_Call_Failure;
          END IF;
          IF
          Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_EVERYTHING,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0) = FAILED
          THEN
            RAISE System_Call_Failure;
          END IF;
        ELSE
          IF
          DO_DISABLE_FULLSCREEN_BOX_IN_NEWER_THAN_XP AND THEN
          Is_Newer_Than("Windows XP")
          THEN
            IF
            Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE) = 0
            THEN
              RAISE System_Call_Failure;
            END IF;
          ELSE
            IF
            Change_Window_Setting(
              Window  => Primary_Window,
              Command => SET_WINDOW_STYLE,
              Setting => WINDOWED_STYLE OR STYLE_BOX_FULLSCREEN) = 0
            THEN
              RAISE System_Call_Failure;
            END IF;
          END IF;
          IF
          Set_Window_Position(
            Window       => Primary_Window,
            Insert_After => INSERT_ON_TOP_OF_APPLICATIONS,
            X            => Integer_4_Signed_C(X),
            Y            => Integer_4_Signed_C(Y),
            Width        => Integer_4_Signed_C(Width),
            Height       => Integer_4_Signed_C(Height),
            Flags        => 0) = FAILED
          THEN
            RAISE System_Call_Failure;
          END IF;
        END IF;
        IF Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 THEN 
          NULL;
        END IF;
        IF Update_Window(Primary_Window) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
      END Adjust;
  ----------------------
  -- Is_Only_Instance --
  ----------------------
    FUNCTION Is_Only_Instance(
      Name : IN String_2)
      RETURN Boolean
      IS
      Handle : Address := NULL_ADDRESS;
      Window : Address := NULL_ADDRESS;
      BEGIN
        Handle := Create_Mutex(NULL_ADDRESS, C_TRUE, To_String_2_C(Name));
        IF Get_Last_Error /= NO_ERROR THEN
          IF Release_Mutex(Handle) = FAILED THEN
            RAISE System_Call_Failure;
          END IF;
          Primary_Window := Find_Window(To_String_2_C(Name), NULL_ADDRESS);
          IF Primary_Window /= NULL_ADDRESS THEN
            IF Show_Window(Primary_Window, MAKE_WINDOW_NORMALIZE) = 0 THEN
              NULL;
            END IF;
            Handle := Set_Focus(Primary_Window);
            IF Set_Foreground_Window(Primary_Window) = FAILED THEN
              RAISE System_Call_Failure;
            END IF;
            Handle := Set_Active_Window(Primary_Window);
          END IF;
          RETURN False;
        END IF;
        RETURN True;
      END Is_Only_Instance;
  -----------------------------------------
  -- Move_Topmost_Windows_Out_Of_The_Way --
  -----------------------------------------
    PROCEDURE Move_Topmost_Windows_Out_Of_The_Way
      IS
      --------------------
      FUNCTION Is_Covered
      --------------------
        RETURN Boolean
        IS
        Clip           : ALIASED Record_Rectangle := (OTHERS => <>);
        Client         : ALIASED Record_Rectangle := (OTHERS => <>);
        Result         :         Boolean          := False;
        Device_Context :         Address          := Get_Device_Context(Primary_Window);
        BEGIN
          IF Device_Context = NULL_ADDRESS THEN
            RAISE System_Call_Failure;
          END IF;
          CASE Get_Clip_Box(Device_Context, Clip'Access) IS
            WHEN REGION_NULL | REGION_COMPLEX =>
              Result := True;
            WHEN REGION_SIMPLE =>
              IF Get_Client_Rectangle(Primary_Window, Client'Access) = FAILED THEN
                RAISE System_Call_Failure;
              END IF;
              IF Rectangles_Are_Equal(Client'Access, Clip'Access) = C_FALSE THEN
                Result := True;
              END IF;
            WHEN OTHERS =>
              RAISE System_Call_Failure;
          END CASE;
          IF Release_Device_Context(Primary_Window, Device_Context) = FAILED THEN
            RAISE System_Call_Failure;
          END IF;
          RETURN Result;
        END Is_Covered;
      -----
      BEGIN
      -----
        IF Is_Covered THEN
          -- LOOP through windows WITH higher Z value
            -- Find_Intersecting_Rectangle
              -- Make it move behind, IF that fails minimize it, IF that fails, move it
            -- IF it IS NOT covered exit
          NULL;
        END IF;
      END Move_Topmost_Windows_Out_Of_The_Way;
  ----------------
  -- Hide_Mouse --
  ----------------
    PROCEDURE Hide_Mouse(
      Do_Hide            : IN Boolean;
      Do_Ignore_Titlebar : IN Boolean := False)
      IS
      ---------------------
      PROCEDURE Clip_Mouse(
      ---------------------
        Do_Clip            : IN Boolean;
        Do_Ignore_Titlebar : IN Boolean)
        IS
        Border        :         Record_Window_Border := Get_Screen_Border;
        New_Clip_Area : ALIASED Record_Rectangle :=(
          Left   => Integer_4_Signed_C(Border.Left),
          Top    => Integer_4_Signed_C(Border.Top),
          Right  => Integer_4_Signed_C(Border.Right),
          Bottom => Integer_4_Signed_C(Border.Bottom));
        BEGIN
          IF Do_Ignore_Titlebar AND THEN Get_Window_Rectangle(Primary_Window, New_Clip_Area'Access) = FAILED THEN
            RAISE System_Call_Failure;
          END IF;
          IF NOT Do_Clip THEN
            IF Original_Clip /= (OTHERS => <>) THEN
              IF Clip_Cursor(Original_Clip'Access) = FAILED THEN
                RAISE System_Call_Failure;
              END IF;
              Original_Clip := (OTHERS => <>);
            END IF;
          ELSE
            IF Original_Clip = (OTHERS => <>) THEN
              IF Get_Clip_Cursor_Area(Original_Clip'Access) = FAILED THEN
                RAISE System_Call_Failure;
              END IF;
              IF Original_Clip = (OTHERS => <>) THEN
                RAISE System_Call_Failure;
              END IF;
            END IF;
            IF Clip_Cursor(New_Clip_Area'Access) = FAILED THEN
              RAISE System_Call_Failure;
            END IF;
          END IF;
        END Clip_Mouse;
      -----
      BEGIN
      -----
        IF Do_Hide THEN
          WHILE Show_Cursor(Boolean'Pos(NOT Do_Hide)) > -1 LOOP
            NULL;
          END LOOP;
          IF NOT Do_Disable_Blank_Mouse THEN
            IF Previous_Mouse = NULL_ADDRESS THEN
              Previous_Mouse := To_Address(Get_Class_Setting(Primary_Window, GET_CLASS_CURSOR));
              IF Previous_Mouse = NULL_ADDRESS THEN
                RAISE System_Call_Failure;
              END IF;
            END IF;
            IF Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Blank_Mouse)) = 0 THEN
              RAISE System_Call_Failure;
            END IF;
          END IF;
          Clip_Mouse(True, Do_Ignore_Titlebar);
        ELSE
          IF
          NOT Do_Disable_Blank_Mouse     AND THEN
          Previous_Mouse /= NULL_ADDRESS AND THEN
          Change_Class_Setting(Primary_Window, SET_CLASS_CURSOR, To_Integer_4_Unsigned_C(Previous_Mouse)) = 0
          THEN
            RAISE System_Call_Failure;
          END IF;
          Clip_Mouse(False, False);
          WHILE Show_Cursor(Boolean'Pos(NOT Do_Hide)) < 0 LOOP
            NULL;
          END LOOP;
        END IF;
      END Hide_Mouse;
  -------------
  -- Iconize --
  -------------
    PROCEDURE Iconize
      IS
      BEGIN
        IF Show_Window(Primary_Window, MAKE_WINDOW_GO_TO_ICONIC) = 0 THEN
          NULL;
        END IF;
      END Iconize;
  END Implementation;

