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
PACKAGE BODY Neo.System.Window
  IS
  --------------------
  -- Implementation --
  --------------------
    PACKAGE BODY Generic_Implementation
      IS SEPARATE;
    PACKAGE Implementation
      IS NEW Generic_Implementation(
        Handle_Finalization => Handle_Finalization,
        Handle_Activation   => Handle_Activation, 
        Handle_State_Change => Handle_State_Change,
        Handle_Window_Move  => Handle_Window_Move,
        Handle_Resize       => Handle_Resize);
  -------------------------------
  -- Task_Multi_Monitor_Window --
  -------------------------------
    TASK BODY Task_Multi_Monitor_Window
      IS
      Index          : Integer_4_Positive := 1;
      Do_Quit        : Boolean := False;
      --Render_Backend : Access_Procedure   := NULL;
      BEGIN
        ACCEPT Initialize(
          I       : IN Integer_4_Positive
          )--Backend : IN Access_Procedure)
          DO
            Index          := I;
            --Render_Backend := Backend;
          END Initialize;
        WHILE Implementation.Handle_Events(Index) AND NOT Do_Quit LOOP
          select 
            ACCEPT Finalize
              DO
                Do_Quit := True;
              END Finalize;
          ELSE
            NULL;--Render_Backend.All;
          END select;
        END LOOP;
      END Task_Multi_Monitor_Window;
  ---------
  -- Run --
  ---------
    PROCEDURE Run(
      Title                       : IN String_2;
      Icon_Path                   : IN String_2;
      Cursor_Path                 : IN String_2;
      --Render_Backend              : IN Access_Procedure;
      Do_Allow_Multiple_Instances : IN Boolean := False)
      IS
      Native_Width   : Integer_4_Positive      := 1;
      Native_Height  : Integer_4_Positive      := 1;
      Bits_Per_Pixel : Integer_4_Positive      := 1;
      Previous_State : Enumerated_Window_State := Protected_Data.Get.State;
      X              : Integer_4_Signed        := Protected_Data.Get.X;
      Y              : Integer_4_Signed        := Protected_Data.Get.Y;
      BEGIN
        IF Protected_Data.Get.Title /= NULL THEN
          RETURN;
        END IF;
        IF NOT Do_Allow_Multiple_Instances AND THEN NOT Implementation.Is_Only_Instance(Title) THEN
          RAISE System_Call_Failure;
        END IF;
        Implementation.Initialize(Title, Icon_Path, Cursor_Path);
        -------------
        Setup_Window:
        -------------
          DECLARE
          Window : Record_Window := Protected_Data.Get;
          BEGIN
            Window.Title           := NEW String_2(1..Title'Length);
            Window.TItle.All       := Title;
            Window.Icon_Path       := NEW String_2(1..Icon_Path'Length);
            Window.Icon_Path.All   := Icon_Path;
            Window.Cursor_Path     := NEW String_2(1..Cursor_Path'Length);
            Window.Cursor_Path.All := Cursor_Path;
            Protected_Data.Set(Window);
          END Setup_Window;
        Outter:LOOP
          Get_Screen_Information(Bits_Per_Pixel, Native_Width, Native_Height);
          -- Check IF monitor native resolution violates the minimum/maximum aspect requirements
          CASE Protected_Data.Get.State IS
            WHEN Fullscreen_State | Multi_Monitor_State =>
              Implementation.Adjust(
                Title         => Title,
                Do_Fullscreen => True,
                X             => 0,
                Y             => 0,
                Width         => Native_Width,
                Height        => Native_Height);
              IF Protected_Data.Get.State = Multi_Monitor_State THEN
                Implementation.Initialize_Multi_Monitor(
                  Monitors => Implementation.Get_Monitors);
                -- Detect number of graphics cards
                -- Spawn_Tasks_For_Auxiliarary_Video_Cards:
              END IF;
              Center := (Native_Width / 2, Native_Height / 2);
            WHEN Windowed_State =>
              -- IF the current width AND height are greater than native, THEN accomications must be made
              Implementation.Adjust(
                Title         => Title,
                Do_Fullscreen => False,
                X             => X,
                Y             => Y,
                Width         => Protected_Data.Get.Width,
                Height        => Protected_Data.Get.Height);
          END CASE;
          Previous_State := Protected_Data.Get.State;
          Protected_Data.Set_Initialized(True);
          Inner:LOOP
            IF Protected_Data.Get.Is_Changing_Mode THEN
              IF Protected_Data.Get.Is_In_Menu_Mode THEN
                Implementation.Hide_Mouse(False, False);
                Implementation.Set_Custom_Mouse(False);
              ELSE
                Take_Control;
              END IF;
              ------------------
              Unset_Mode_Change:
              ------------------
                DECLARE
                Window : Record_Window := Protected_Data.Get;
                BEGIN
                  Window.Is_Changing_Mode := False;
                  Protected_Data.Set(Window);
                END Unset_Mode_Change;
            END IF;
            --Render_Backend.All;
            EXIT Outter WHEN OR Protected_Data.Get.Is_Done OR NOT Implementation.Handle_Events;
            EXIT Inner WHEN NOT Protected_Data.Is_Initialized;
          END LOOP Inner;
          CASE Previous_State IS
            WHEN Windowed_State =>
              X := Protected_Data.Get.X;
              Y := Protected_Data.Get.Y;
            WHEN Multi_Monitor_State =>
              Implementation.Finalize_Multi_Monitor;
            WHEN others =>
              NULL;
          END CASE;
        END LOOP Outter;
        Implementation.Finalize;
        Protected_Data.Set(DEFAULT_RECORD_WINDOW);
      END Run;
  --------------
  -- Finalize --
  --------------
    PROCEDURE Finalize
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Is_Done := True;
        Protected_Data.Set(Window);
        -- Free strings
      END Finalize;
  ---------
  -- Get --
  ---------
    FUNCTION Get
      RETURN Record_Window
      IS
      BEGIN
        RETURN Protected_Data.Get.Window;
      END Get;
  ---------
  -- Set --
  ---------
    PROCEDURE Set(
      Window : IN Record_Window)
      IS
      Window      : Record_Window := Protected_Data.Get;
      Mode_Change : Boolean       := False;
      BEGIN
        IF Do_Enter_Menu_Mode /= Window.Is_In_Menu_Mode THEN
          Mode_Change := True;
        END IF;
        Protected_Data.Set((
          Title                => Window.Title,
          Icon_Path            => Window.Icon_Path,
          Cursor_Path          => Window.Cursor_Path,
          Renderer             => Renderer,
          State                => State,
          Refreshes_Per_Second => Refreshes_Per_Second,
          Multi_Samples        => Multi_Samples,
          Gamma                => Gamma,
          Height               => Height,
          Width                => Width,
          X                    => X,
          Y                    => Y,
          Aspect_Wide          => Aspect_Wide,
          Aspect_Narrow        => Aspect_Narrow,
          Is_In_Menu_Mode      => Do_Enter_Menu_Mode,
          Is_Changing_Mode     => Mode_Change,
          Is_Iconized          => Window.Is_Iconized,
          Is_Done              => Window.Is_Done));
        Protected_Data.Set_Initialized(False);
      END Set;
  ------------------
  -- Set_Position --
  ------------------
    PROCEDURE Set_Position(
      X : IN Integer_4_Signed;
      Y : IN Integer_4_Signed)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.X := X;
        Window.Y := Y;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Position;
  ------------------
  -- Set_Renderer --
  ------------------
    PROCEDURE Set_Renderer(
      Renderer : IN Enumerated_Renderer)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Renderer := Renderer;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Renderer;
  ---------------
  -- Set_State --
  ---------------
    PROCEDURE Set_State(
      State : IN Enumerated_Window_State)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.State := State;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_State;
  ------------------------------
  -- Set_Refreshes_Per_Second --
  ------------------------------
    PROCEDURE Set_Refreshes_Per_Second(
      Refreshes_Per_Second : IN Positive)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Refreshes_Per_Second := Refreshes_Per_Second;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Refreshes_Per_Second;
  -----------------------
  -- Set_Multi_Samples --
  -----------------------
    PROCEDURE Set_Multi_Samples(
      Multi_Samples : IN Positive)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Multi_Samples := Multi_Samples;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Multi_Samples;
  ---------------
  -- Set_Gamma --
  ---------------
    PROCEDURE Set_Gamma(
      Gamma : IN Record_Gamma)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Gamma := Gamma;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Gamma;
  ----------------
  -- Set_Height --
  ----------------
    PROCEDURE Set_Height(
      Height : IN Positive)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Height := Height;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Height;
  ---------------
  -- Set_Width --
  ---------------
    PROCEDURE Set_Width(
      Width : IN Positive)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Width := Width;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Width;
  --------------------
  -- Set_Resolution --
  --------------------
    PROCEDURE Set_Resolution(
      Height : IN Positive;
      Width  : IN Positive)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Height := Height;
        Window.Width  := Width;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Resolution;
-----------------------------
-- Set_Narrow_Aspect_Ratio --
-----------------------------
  PROCEDURE Set_Narrow_Aspect_Ratio(
    Aspect_Narrow : IN Record_Aspect_Ratio)
    IS
    Window : Record_Window := Protected_Data.Get;
    BEGIN
      Window.Aspect_Narrow := Aspect_Narrow;
      Protected_Data.Set(Window);
      Protected_Data.Set_Initialized(False);
    END Set_Narrow_Aspect_Ratio;
  ---------------------------
  -- Set_Wide_Aspect_Ratio --
  ---------------------------
    PROCEDURE Set_Wide_Aspect_Ratio(
      Aspect_Wide : IN Record_Aspect_Ratio)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Aspect_Wide := Aspect_Wide;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      END Set_Wide_Aspect_Ratio;
  -------------------
  -- Set_Menu_Mode --
  -------------------
    PROCEDURE Set_Menu_Mode(
      Do_Enter_Menu_Mode : IN Boolean)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.Is_Changing_Mode := True;
        IF Do_Enter_Menu_Mode THEN
          Window.Is_In_Menu_Mode := True;
          Protected_Data.Set_Busy(False);
        ELSE
          Window.Is_In_Menu_Mode := False;
          Protected_Data.Set_Busy(True);
        END IF;
        Protected_Data.Set(Window);
      END Set_Menu_Mode;
  ------------------
  -- Take_Control --
  ------------------
    PROCEDURE Take_Control
      IS
      BEGIN
        IF Protected_Data.Get.State = Windowed_State THEN
          -----------------
          Move_From_Hiding:
          -----------------
            DECLARE
            Screen    : Record_Window_Border       := Get_Screen_Border;
            Work_Area : Array_Record_Window_Border := Get_Work_Area;
            BEGIN
              Screen.Right  := Screen.Right  - Screen.Left;
              Screen.Bottom := Screen.Bottom - Screen.Top;
              IF Screen.Left < Work_Area.Left THEN
                Screen.Left := Work_Area.Left; 
              ELSIF Screen.Right + Screen.Left > Work_Area.Right THEN
                Screen.Left := Work_Area.Right - Screen.Right;
              END IF;
              IF Screen.Top < Work_Area.Top THEN
                Screen.Top := Work_Area.Top; 
              ELSIF Screen.Bottom + Screen.Top > Work_Area.Bottom THEN
                Screen.Top := Work_Area.Bottom - Screen.Bottom;
              END IF;
              IF
              Set_Window_Position(
                Window       => Window,
                Y            => Screen.Top,
                X            => Screen.Left,
                Width        => Screen.Right,
                Height       => Screen.Bottom,
                Insert_After => INSERT_ON_TOP_OF_APPLICATIONS,
                Flags        => 0) = FAILED
              THEN
                RAISE System_Call_Failure;
              END IF;
            END Move_From_Hiding;
          Implementation.Move_Topmost_Windows_Out_Of_The_Way;
          Implementation.Hide_Mouse(True, False);
        ELSE
          Implementation.Hide_Mouse(True, True);
        END IF;
        Protected_Data.Set_Busy(True);
      END Take_Control;
  ------------------
  -- Is_In_Border --
  ------------------
    FUNCTION Is_In_Border(
      X : IN Integer_4_Signed;
      Y : IN Integer_4_Signed)
      RETURN Boolean
      IS
      Border : Record_Window_Border := Implementation.Get_Screen_Border;
      BEGIN
        IF
        X > Border.Left AND X < Border.Right AND
        Y > Border.Top  AND Y < Border.Bottom
        THEN
          RETURN True;
        END IF;
        RETURN False;
      END Is_In_Border;
  -------------------------
  -- Handle_Finalization --
  -------------------------
    PROCEDURE Handle_Finalization
      IS
      BEGIN
        Finalize;
        -- Free Strings
      END Handle_Finalization;
  ------------------------
  -- Handle_Window_Move --
  ------------------------
    PROCEDURE Handle_Window_Move(
      Window_X : IN Integer_4_Signed;
      Window_Y : IN Integer_4_Signed;
      Screen_X : IN Integer_4_Signed;
      Screen_Y : IN Integer_4_Signed)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        Window.X := Window_X;
        Window.Y := Window_Y;
        Protected_Data.Set(Window);
        IF Protected_Data.Get.State = Windowed_State THEN
          Center :=(
            Screen_X + Protected_Data.Get.Width  / 2,
            Screen_Y + Protected_Data.Get.Height / 2);
        END IF;
      END Handle_Window_Move;  
  -------------------------
  -- Handle_State_Change --
  -------------------------
    PROCEDURE Handle_State_Change(
      Change : IN Enumerated_Window_Change)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        CASE Change IS
          WHEN Iconic_Change =>
            Protected_Data.Set_Busy(False);
            Window.Is_Iconized := True;
            Protected_Data.Set(Window);
          WHEN Fullscreen_Change =>
            Window.State       := Fullscreen_State;
            Window.Is_Iconized := False;
            Protected_Data.Set(Window);
            Protected_Data.Set_Initialized(False);
          WHEN Windowed_Change =>
            Window.Is_Iconized := False;
            Protected_Data.Set(Window);
        END CASE;
      END Handle_State_Change;
  -----------------------
  -- Handle_Activation --
  -----------------------
    PROCEDURE Handle_Activation(
      Do_Activate     : IN Boolean;
      Do_Detect_Click : IN Boolean;
      X               : IN Integer_4_Signed;
      Y               : IN Integer_4_Signed)
      IS
      Window : Record_Window := Protected_Data.Get;
      BEGIN
        IF Protected_Data.Is_Initialized THEN
          IF Do_Activate THEN
            IF Protected_Data.Get.Is_In_Menu_Mode THEN
              Implementation.Set_Custom_Mouse(False);
            ELSE
              Implementation.Set_Custom_Mouse(True);
              IF Is_In_Border(X, Y) THEN
                Take_Control;
              END IF;
            END IF;
          ELSE
            Protected_Data.Set_Busy(False);
            IF Protected_Data.Get.State /= Windowed_State THEN
              IF Protected_Data.Get.State = Multi_Monitor_State THEN
                Implementation.Finalize_Multi_Monitor;
              END IF;
              Implementation.Iconize;
              Window.Is_Iconized := True;
            END IF;
            Implementation.Hide_Mouse(False);
            Implementation.Set_Custom_Mouse(True);
            Keys_Down := (others => False);
          END IF;
          Protected_Data.Set(Window);
        END IF;
      END Handle_Activation;
  -------------------
  -- Handle_Resize --
  -------------------
    FUNCTION Handle_Resize(
      Resize_Location : IN Enumerated_Resize;
      Current_Screen  : IN Record_Window_Border)
      RETURN Record_Window_Border
      IS
      Screen            : Record_Window_Border := Current_Screen;
      -- Window            : Record_Window        := Protected_Data.Get;
      -- Narrow_Ratio      : Float_4_Natural      := Float_4_Natural(Window.Aspect_Wide.Horizontal)   / Float_4_Natural(Window.Aspect_Wide.Vertical);
      -- Wide_Ratio        : Float_4_Natural      := Float_4_Natural(Window.Aspect_Narrow.Horizontal) / Float_4_Natural(Window.Aspect_Narrow.Vertical);
      -- Decoration_Width  : Integer_4_Signed     := Decoration_Area.Right  - Decoration_Area.Left;
      -- Decoration_Height : Integer_4_Signed     := Decoration_Area.Bottom - Decoration_Area.Top;
      -- Current_Width     : Integer_4_Signed     := Screen.Right  - Decoration_Width  - Screen.Left;
      -- Current_Height    : Integer_4_Signed     := Screen.Bottom - Decoration_Height - Screen.top;
      -- Previous_Width    : Integer_4_Signed     := Current_Width;
      -- Previous_Height   : Integer_4_Signed     := Current_Height;
      -- Previous_Top      : Integer_4_Signed     := 0; 
      BEGIN
        -- IF Current_Width < Integer_4_Signed(Float_4_Natural(Current_Height) * Wide_Ratio) THEN
        --   Current_Width := Integer_4_Signed(Float_4_Natural(Current_Height) * Wide_Ratio);
        -- END IF;
        -- IF Current_Width < MINIMUM_DIMENSION_X THEN
        --   Current_Width := MINIMUM_DIMENSION_X;
        -- END IF;
        -- IF Current_Height < Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio) THEN
        --   Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio);
        -- END IF;
        -- CASE Resize_Location IS
        --   WHEN
        --   Bottom_Resize |
        --   Top_Resize    =>
        --     Current_Height := Previous_Height;
        --     IF Float_4_Natural(Current_Width) > Narrow_Ratio * Float_4_Natural(Current_Height) THEN
        --       Current_Width := Integer_4_Signed(Narrow_Ratio * Float_4_Natural(Current_Height));
        --     END IF;
        --     IF Current_Width < MINIMUM_DIMENSION_X THEN
        --       Current_Width := MINIMUM_DIMENSION_X;
        --     END IF;
        --     IF Current_Height < Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio) THEN
        --       Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio);
        --     END IF;
        --     IF Screen.Left <= Work_Area.Left AND Current_Width > Previous_Width THEN
        --       Screen.Right := Screen.Right + (Current_Width - Previous_Width);
        --     ELSIF Screen.Right >= Work_Area.Right AND Current_Width > Previous_Width  THEN
        --       Screen.Left := Screen.Left - (Current_Width - Previous_Width);
        --     ELSE
        --       Screen.Left  := Screen.Left  - (Current_Width - Previous_Width) / 2;
        --       Screen.Right := Screen.Right + (Current_Width - Previous_Width) / 2;
        --     END IF;
        --     IF Resize_Location = Resize_Location THEN
        --       Screen.Top := Screen.Bottom - (Current_Height + Decoration_Height);
        --     ELSE
        --       Screen.Bottom := Screen.Top + (Current_Height + Decoration_Height);
        --     END IF;
        --   WHEN
        --   Left_Resize  |
        --   Right_Resize =>
        --     Current_Width := Previous_Width;
        --     IF Float_4_Natural(Current_Height) > Float_4_Natural(Current_Width) / Wide_Ratio AND
        --       Current_Width > MINIMUM_DIMENSION_X THEN
        --       Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Wide_Ratio);
        --     END IF;
        --     IF Current_Width < MINIMUM_DIMENSION_X THEN
        --       Current_Width := MINIMUM_DIMENSION_X;
        --     END IF;
        --     IF Screen.Top <= Work_Area.Top AND Current_Height > Previous_Height THEN
        --       Screen.Bottom := Screen.Top + Decoration_Height + Current_Height;
        --     ELSIF Screen.Bottom >= Work_Area.Bottom AND Current_Height > Previous_Height THEN
        --       Screen.Top := Screen.Bottom - Decoration_Height - Current_Height;
        --     ELSE
        --       Previous_Top  := Screen.Top;
        --       Screen.Top    := Screen.Bottom - Decoration_Height - (Current_Height + Previous_Height) / 2;
        --       Screen.Bottom := Previous_Top  + Decoration_Height + (Current_Height + Previous_Height) / 2;
        --     END IF;
        --     IF Resize_Location = Resize_Location THEN
        --       Screen.Left  := Screen.Right - (Current_Width + Decoration_Width);
        --     ELSE
        --       Screen.Right := Screen.Left + Current_Width + Decoration_Width;
        --     END IF;
        --   WHEN
        --   Bottom_Right_Resize |
        --   Bottom_Left_Resize  |
        --   Top_Left_Resize     |
        --   Top_Right_Resize    =>
        --     IF Resize_Location = Bottom_Left_Resize OR Resize_Location = Bottom_Right_Resize THEN
        --       Screen.Bottom := Screen.Top + Current_Height + Decoration_Height;
        --     END IF;
        --     IF Resize_Location = Top_Right_Resize OR Resize_Location = Top_Left_Resize THEN
        --       Screen.Top := Screen.Bottom - Current_Height - Decoration_Height;
        --     END IF;
        --     IF Resize_Location = Bottom_Left_Resize OR Resize_Location = Top_Left_Resize THEN
        --       Screen.Left := Screen.Right - Decoration_Width - Current_Width;
        --     END IF;
        --     IF Resize_Location = Bottom_Right_Resize OR Resize_Location = Top_Right_Resize THEN
        --       Screen.Right := Screen.Left + Decoration_Width + Current_Width;
        --     END IF;
        --     --------------------
        --     --Bound_Corner_Sizing:
        --     --------------------
        --     --  DECLARE
        --     --  Did_Resize_Right  : Boolean := False;
        --     --  Did_Resize_Left   : Boolean := False;
        --     --  Did_Resize_Top    : Boolean := False;
        --     --  Did_Resize_Bottom : Boolean := False;
        --       -- BEGIN
        --       --   IF Resize_Location = Bottom_Left_Resize OR Resize_Location = Bottom_Right_Resize THEN
        --       --     Screen.Bottom := Screen.Top + Current_Height + Decoration_Height;
        --       --     IF Screen.Bottom > Work_Area.Bottom THEN
        --       --       Did_Resize_Bottom := True;
        --       --       Screen.Bottom := Work_Area.Bottom;
        --       --     END IF;
        --       --   END IF;
        --       --   IF Resize_Location = Top_Right_Resize OR Resize_Location = Top_Left_Resize THEN
        --       --     Screen.Top := Screen.Bottom - Current_Height - Decoration_Height;
        --       --     IF Screen.Top < Work_Area.Top THEN
        --       --       Did_Resize_Top := True;
        --       --       Screen.Top := Work_Area.Top;
        --       --     END IF;
        --       --   END IF;
        --       --   IF Resize_Location = Bottom_Left_Resize OR Resize_Location = Top_Left_Resize THEN
        --       --     Screen.Left := Screen.Right - Decoration_Width - Current_Width;
        --       --     IF Screen.Left < Work_Area.Left THEN
        --       --       Did_Resize_Left := True;
        --       --       Screen.Left := Work_Area.Left;
        --       --     END IF;
        --       --   END IF;
        --       --   IF Resize_Location = Bottom_Right_Resize OR Resize_Location = Top_Right_Resize THEN
        --       --     Screen.Right := Screen.Left + Decoration_Width + Current_Width;
        --       --     IF Screen.Right > Work_Area.Right THEN
        --       --       Did_Resize_Right := True;
        --       --       Screen.Right := Work_Area.Right;
        --       --     END IF;
        --       --   END IF;
        --       --   IF
        --       --     Did_Resize_Right OR
        --       --     Did_Resize_Left OR
        --       --     Did_Resize_Top OR
        --       --     Did_Resize_Bottom THEN
        --       --     NULL;--RETURN Handle_Resize(
        --       --     --  Resize_Location => Resize_Location,
        --       --     --  Previous_Screen => Previous_Screen,
        --       --     --  Current_Screen  => Screen,
        --       --     --  Work_Area       => Work_Area,
        --       --     --  Decoration_Area => Decoration_Area);
        --       --   END IF;
        --     CASE Resize_Location IS
        --       WHEN Bottom_Right_Resize =>
        --         IF Screen.Right > Work_Area.Right OR Screen.Bottom > Work_Area.Bottom THEN
        --           Screen := Previous_Screen;
        --         END IF;
        --       WHEN Bottom_Left_Resize =>
        --         IF Screen.Left < Work_Area.Left OR Screen.Bottom > Work_Area.Bottom THEN
        --           Screen := Previous_Screen;
        --         END IF;
        --       WHEN Top_Left_Resize =>
        --         IF Screen.Top < Work_Area.Top OR Screen.Left < Work_Area.Left THEN
        --           Screen := Previous_Screen;
        --         END IF;
        --       WHEN Top_Right_Resize =>
        --         IF Screen.Top < Work_Area.Top OR Screen.Right > Work_Area.Right THEN
        --           Screen := Previous_Screen;
        --         END IF;
        --       WHEN others =>
        --         NULL;
        --     END CASE;
        --   WHEN others =>
        --     NULL;
        -- END CASE;
        -- Window.X      := Screen.Left;
        -- Window.Y      := Screen.Top;
        -- Window.Width  := (Screen.Right  - Screen.Left);
        -- Window.Height := (Screen.Bottom - Screen.Top);
        -- Protected_Data.Set(Window);
        -- Center_X := Screen.Left + (Window.Width)  / 2;
        -- Center_Y := Screen.Top  + (Window.Height) / 2;
        RETURN Screen;
      END Handle_Resize;
  END Neo.System.Window;

