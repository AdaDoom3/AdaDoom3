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
package body Neo.System.Window
  is
  -------------------------------
  -- Task_Multi_Monitor_Window --
  -------------------------------
    task body Task_Multi_Monitor_Window
      is
      Index          : Integer_4_Positive := 1;
      Do_Quit        : Boolean := False;
      --Render_Backend : Access_Procedure   := null;
      begin
        accept Initialize(
          I       : in Integer_4_Positive
          )--Backend : in Access_Procedure)
          do
            Index          := I;
            --Render_Backend := Backend;
          end Initialize;
        while Instantiated_Implementation.Handle_Events(Index) and not Do_Quit loop
          select 
            accept Finalize
              do
                Do_Quit := True;
              end Finalize;
          else
            null;--Render_Backend.All;
          end select;
        end loop;
      end Task_Multi_Monitor_Window;
  ---------
  -- Run --
  ---------
    procedure Run( 
      Title                       : in String_2;
      Icon_Path                   : in String_2;
      Cursor_Path                 : in String_2;
      --Render_Backend              : in Access_Procedure;
      Do_Allow_Multiple_Instances : in Boolean := False)
      is
      Native_Width   : Integer_4_Positive      := 1;
      Native_Height  : Integer_4_Positive      := 1;
      Bits_Per_Pixel : Integer_4_Positive      := 1;
      Previous_State : Enumerated_Window_State := Protected_Data.Get.State;
      X              : Integer_4_Signed        := Protected_Data.Get.X;
      Y              : Integer_4_Signed        := Protected_Data.Get.Y;
      begin
        if Protected_Data.Get.Title /= null then
          return;
        end if;
        if not Do_Allow_Multiple_Instances and then not Instantiated_Implementation.Is_Only_Instance(Title) then
          raise System_Call_Failure;
        end if;
        Instantiated_Implementation.Initialize(Title, Icon_Path, Cursor_Path);
        -------------
        Setup_Window:
        -------------
          declare
          Window : Record_Window := Protected_Data.Get;
          begin
            Window.Title           := new String_2(1..Title'Length);
            Window.TItle.All       := Title;
            Window.Icon_Path       := new String_2(1..Icon_Path'Length);
            Window.Icon_Path.All   := Icon_Path;
            Window.Cursor_Path     := new String_2(1..Cursor_Path'Length);
            Window.Cursor_Path.All := Cursor_Path;
            Protected_Data.Set(Window);
          end Setup_Window;
        Outter:loop
          Get_Screen_Information(Bits_Per_Pixel, Native_Width, Native_Height);
          -- Check if monitor native resolution violates the minimum/maximum aspect requirements
          case Protected_Data.Get.State is
            when Fullscreen_State | Multi_Monitor_State =>
              Instantiated_Implementation.Adjust(
                Title         => Title,
                Do_Fullscreen => True,
                X             => 0,
                Y             => 0,
                Width         => Native_Width,
                Height        => Native_Height);
              if Protected_Data.Get.State = Multi_Monitor_State then
                Instantiated_Implementation.Initialize_Multi_Monitor(
                  Monitors => Instantiated_Implementation.Get_Monitors);
                -- Detect number of graphics cards
                -- Spawn_Tasks_For_Auxiliarary_Video_Cards:
              end if;
              Center := (Native_Width / 2, Native_Height / 2);
            when Windowed_State =>
              -- If the current width and height are greater than native, then accomications must be made
              Instantiated_Implementation.Adjust(
                Title         => Title,
                Do_Fullscreen => False,
                X             => X,
                Y             => Y,
                Width         => Protected_Data.Get.Width,
                Height        => Protected_Data.Get.Height);
          end case;
          Previous_State := Protected_Data.Get.State;
          Protected_Data.Set_Initialized(True);
          Inner:loop
            if Protected_Data.Get.Is_Changing_Mode then
              if Protected_Data.Get.Is_In_Menu_Mode then
                Instantiated_Implementation.Hide_Mouse(False, False);
                Instantiated_Implementation.Set_Custom_Mouse(False);
              else
                Take_Control;
              end if;
              ------------------
              Unset_Mode_Change:
              ------------------
                declare
                Window : Record_Window := Protected_Data.Get;
                begin
                  Window.Is_Changing_Mode := False;
                  Protected_Data.Set(Window);
                end Unset_Mode_Change;
            end if;
            --Render_Backend.All;
            exit Outter when or Protected_Data.Get.Is_Done or not Instantiated_Implementation.Handle_Events;
            exit Inner when not Protected_Data.Is_Initialized;
          end loop Inner;
          case Previous_State is
            when Windowed_State =>
              X := Protected_Data.Get.X;
              Y := Protected_Data.Get.Y;
            when Multi_Monitor_State =>
              Instantiated_Implementation.Finalize_Multi_Monitor;
            when others =>
              null;
          end case;
        end loop Outter;
        Instantiated_Implementation.Finalize;
        Protected_Data.Set(DEFAULT_RECORD_WINDOW);
      end Run;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Is_Done := True;
        Protected_Data.Set(Window);
        -- Free strings
      end Finalize;
  ---------
  -- Get --
  ---------
    function Get
      return Record_Window
      is
      begin
        return Protected_Data.Get.Window;
      end Get;
  ---------
  -- Set --
  ---------
    procedure Set(
      Window : in Record_Window)
      is
      Window      : Record_Window := Protected_Data.Get;
      Mode_Change : Boolean       := False;
      begin
        if Do_Enter_Menu_Mode /= Window.Is_In_Menu_Mode then
          Mode_Change := True;
        end if;
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
      end Set;
  ------------------
  -- Set_Position --
  ------------------
    procedure Set_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.X := X;
        Window.Y := Y;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Position;
  ------------------
  -- Set_Renderer --
  ------------------
    procedure Set_Renderer(
      Renderer : in Enumerated_Renderer)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Renderer := Renderer;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Renderer;
  ---------------
  -- Set_State --
  ---------------
    procedure Set_State(
      State : in Enumerated_Window_State)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.State := State;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_State;
  ------------------------------
  -- Set_Refreshes_Per_Second --
  ------------------------------
    procedure Set_Refreshes_Per_Second(
      Refreshes_Per_Second : in Positive)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Refreshes_Per_Second := Refreshes_Per_Second;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Refreshes_Per_Second;
  -----------------------
  -- Set_Multi_Samples --
  -----------------------
    procedure Set_Multi_Samples(
      Multi_Samples : in Positive)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Multi_Samples := Multi_Samples;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Multi_Samples;
  ---------------
  -- Set_Gamma --
  ---------------
    procedure Set_Gamma(
      Gamma : in Record_Gamma)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Gamma := Gamma;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Gamma;
  ----------------
  -- Set_Height --
  ----------------
    procedure Set_Height(
      Height : in Positive)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Height := Height;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Height;
  ---------------
  -- Set_Width --
  ---------------
    procedure Set_Width(
      Width : in Positive)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Width := Width;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Width;
  --------------------
  -- Set_Resolution --
  --------------------
    procedure Set_Resolution(
      Height : in Positive;
      Width  : in Positive)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Height := Height;
        Window.Width  := Width;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Resolution;
-----------------------------
-- Set_Narrow_Aspect_Ratio --
-----------------------------
  procedure Set_Narrow_Aspect_Ratio(
    Aspect_Narrow : in Record_Aspect_Ratio)
    is
    Window : Record_Window := Protected_Data.Get;
    begin
      Window.Aspect_Narrow := Aspect_Narrow;
      Protected_Data.Set(Window);
      Protected_Data.Set_Initialized(False);
    end Set_Narrow_Aspect_Ratio;
  ---------------------------
  -- Set_Wide_Aspect_Ratio --
  ---------------------------
    procedure Set_Wide_Aspect_Ratio(
      Aspect_Wide : in Record_Aspect_Ratio)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Aspect_Wide := Aspect_Wide;
        Protected_Data.Set(Window);
        Protected_Data.Set_Initialized(False);
      end Set_Wide_Aspect_Ratio;
  -------------------
  -- Set_Menu_Mode --
  -------------------
    procedure Set_Menu_Mode(
      Do_Enter_Menu_Mode : in Boolean)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.Is_Changing_Mode := True;
        if Do_Enter_Menu_Mode then
          Window.Is_In_Menu_Mode := True;
          Protected_Data.Set_Busy(False);
        else
          Window.Is_In_Menu_Mode := False;
          Protected_Data.Set_Busy(True);
        end if;
        Protected_Data.Set(Window);
      end Set_Menu_Mode;
  ------------------
  -- Take_Control --
  ------------------
    procedure Take_Control
      is
      begin
        if Protected_Data.Get.State = Windowed_State then
          -----------------
          Move_From_Hiding:
          -----------------
            declare
            Screen    : Record_Window_Border       := Get_Screen_Border;
            Work_Area : Array_Record_Window_Border := Get_Work_Area;
            begin
              Screen.Right  := Screen.Right  - Screen.Left;
              Screen.Bottom := Screen.Bottom - Screen.Top;
              if Screen.Left < Work_Area.Left then
                Screen.Left := Work_Area.Left; 
              elsif Screen.Right + Screen.Left > Work_Area.Right then
                Screen.Left := Work_Area.Right - Screen.Right;
              end if;
              if Screen.Top < Work_Area.Top then
                Screen.Top := Work_Area.Top; 
              elsif Screen.Bottom + Screen.Top > Work_Area.Bottom then
                Screen.Top := Work_Area.Bottom - Screen.Bottom;
              end if;
              if
              Set_Window_Position(
                Window       => Window,
                Y            => Screen.Top,
                X            => Screen.Left,
                Width        => Screen.Right,
                Height       => Screen.Bottom,
                Insert_After => INSERT_ON_TOP_OF_APPLICATIONS,
                Flags        => 0) = FAILED
              then
                raise System_Call_Failure;
              end if;
            end Move_From_Hiding;
          Instantiated_Implementation.Move_Topmost_Windows_Out_Of_The_Way;
          Instantiated_Implementation.Hide_Mouse(True, False);
        else
          Instantiated_Implementation.Hide_Mouse(True, True);
        end if;
        Protected_Data.Set_Busy(True);
      end Take_Control;
  ------------------
  -- Is_In_Border --
  ------------------
    function Is_In_Border(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed)
      return Boolean
      is
      Border : Record_Window_Border := Instantiated_Implementation.Get_Screen_Border;
      begin
        if X > Border.Left and X < Border.Right and Y > Border.Top and Y < Border.Bottom then
          return True;
        end if;
        return False;
      end Is_In_Border;
  -------------------------
  -- Handle_Finalization --
  -------------------------
    procedure Handle_Finalization
      is
      begin
        Finalize;
        -- Free Strings
      end Handle_Finalization;
  ------------------------
  -- Handle_Window_Move --
  ------------------------
    procedure Handle_Window_Move(
      Window_X : in Integer_4_Signed;
      Window_Y : in Integer_4_Signed;
      Screen_X : in Integer_4_Signed;
      Screen_Y : in Integer_4_Signed)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        Window.X := Window_X;
        Window.Y := Window_Y;
        Protected_Data.Set(Window);
        if Protected_Data.Get.State = Windowed_State then
          Center :=(
            Screen_X + Protected_Data.Get.Width  / 2,
            Screen_Y + Protected_Data.Get.Height / 2);
        end if;
      end Handle_Window_Move;  
  -------------------------
  -- Handle_State_Change --
  -------------------------
    procedure Handle_State_Change(
      Change : in Enumerated_Window_Change)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        case Change is
          when Iconic_Change =>
            Protected_Data.Set_Busy(False);
            Window.Is_Iconized := True;
            Protected_Data.Set(Window);
          when Fullscreen_Change =>
            Window.State       := Fullscreen_State;
            Window.Is_Iconized := False;
            Protected_Data.Set(Window);
            Protected_Data.Set_Initialized(False);
          when Windowed_Change =>
            Window.Is_Iconized := False;
            Protected_Data.Set(Window);
        end case;
      end Handle_State_Change;
  -----------------------
  -- Handle_Activation --
  -----------------------
    procedure Handle_Activation(
      Do_Activate     : in Boolean;
      Do_Detect_Click : in Boolean;
      X               : in Integer_4_Signed;
      Y               : in Integer_4_Signed)
      is
      Window : Record_Window := Protected_Data.Get;
      begin
        if Protected_Data.Is_Initialized then
          if Do_Activate then
            if Protected_Data.Get.Is_In_Menu_Mode then
              Instantiated_Implementation.Set_Custom_Mouse(False);
            else
              Instantiated_Implementation.Set_Custom_Mouse(True);
              if Is_In_Border(X, Y) then
                Take_Control;
              end if;
            end if;
          else
            Protected_Data.Set_Busy(False);
            if Protected_Data.Get.State /= Windowed_State then
              if Protected_Data.Get.State = Multi_Monitor_State then
                Instantiated_Implementation.Finalize_Multi_Monitor;
              end if;
              Instantiated_Implementation.Iconize;
              Window.Is_Iconized := True;
            end if;
            Instantiated_Implementation.Hide_Mouse(False);
            Instantiated_Implementation.Set_Custom_Mouse(True);
            Keys_Down := (others => False);
          end if;
          Protected_Data.Set(Window);
        end if;
      end Handle_Activation;
  -------------------
  -- Handle_Resize --
  -------------------
    function Handle_Resize(
      Resize_Location : in Enumerated_Resize;
      Current_Screen  : in Record_Window_Border)
      return Record_Window_Border
      is
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
      begin
        -- if Current_Width < Integer_4_Signed(Float_4_Natural(Current_Height) * Wide_Ratio) then
        --   Current_Width := Integer_4_Signed(Float_4_Natural(Current_Height) * Wide_Ratio);
        -- end if;
        -- if Current_Width < MINIMUM_DIMENSION_X then
        --   Current_Width := MINIMUM_DIMENSION_X;
        -- end if;
        -- if Current_Height < Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio) then
        --   Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio);
        -- end if;
        -- case Resize_Location is
        --   when
        --   Bottom_Resize |
        --   Top_Resize    =>
        --     Current_Height := Previous_Height;
        --     if Float_4_Natural(Current_Width) > Narrow_Ratio * Float_4_Natural(Current_Height) then
        --       Current_Width := Integer_4_Signed(Narrow_Ratio * Float_4_Natural(Current_Height));
        --     end if;
        --     if Current_Width < MINIMUM_DIMENSION_X then
        --       Current_Width := MINIMUM_DIMENSION_X;
        --     end if;
        --     if Current_Height < Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio) then
        --       Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Narrow_Ratio);
        --     end if;
        --     if Screen.Left <= Work_Area.Left and Current_Width > Previous_Width then
        --       Screen.Right := Screen.Right + (Current_Width - Previous_Width);
        --     elsif Screen.Right >= Work_Area.Right and Current_Width > Previous_Width  then
        --       Screen.Left := Screen.Left - (Current_Width - Previous_Width);
        --     else
        --       Screen.Left  := Screen.Left  - (Current_Width - Previous_Width) / 2;
        --       Screen.Right := Screen.Right + (Current_Width - Previous_Width) / 2;
        --     end if;
        --     if Resize_Location = Resize_Location then
        --       Screen.Top := Screen.Bottom - (Current_Height + Decoration_Height);
        --     else
        --       Screen.Bottom := Screen.Top + (Current_Height + Decoration_Height);
        --     end if;
        --   when
        --   Left_Resize  |
        --   Right_Resize =>
        --     Current_Width := Previous_Width;
        --     if Float_4_Natural(Current_Height) > Float_4_Natural(Current_Width) / Wide_Ratio and
        --       Current_Width > MINIMUM_DIMENSION_X then
        --       Current_Height := Integer_4_Signed(Float_4_Natural(Current_Width) / Wide_Ratio);
        --     end if;
        --     if Current_Width < MINIMUM_DIMENSION_X then
        --       Current_Width := MINIMUM_DIMENSION_X;
        --     end if;
        --     if Screen.Top <= Work_Area.Top and Current_Height > Previous_Height then
        --       Screen.Bottom := Screen.Top + Decoration_Height + Current_Height;
        --     elsif Screen.Bottom >= Work_Area.Bottom and Current_Height > Previous_Height then
        --       Screen.Top := Screen.Bottom - Decoration_Height - Current_Height;
        --     else
        --       Previous_Top  := Screen.Top;
        --       Screen.Top    := Screen.Bottom - Decoration_Height - (Current_Height + Previous_Height) / 2;
        --       Screen.Bottom := Previous_Top  + Decoration_Height + (Current_Height + Previous_Height) / 2;
        --     end if;
        --     if Resize_Location = Resize_Location then
        --       Screen.Left  := Screen.Right - (Current_Width + Decoration_Width);
        --     else
        --       Screen.Right := Screen.Left + Current_Width + Decoration_Width;
        --     end if;
        --   when
        --   Bottom_Right_Resize |
        --   Bottom_Left_Resize  |
        --   Top_Left_Resize     |
        --   Top_Right_Resize    =>
        --     if Resize_Location = Bottom_Left_Resize or Resize_Location = Bottom_Right_Resize then
        --       Screen.Bottom := Screen.Top + Current_Height + Decoration_Height;
        --     end if;
        --     if Resize_Location = Top_Right_Resize or Resize_Location = Top_Left_Resize then
        --       Screen.Top := Screen.Bottom - Current_Height - Decoration_Height;
        --     end if;
        --     if Resize_Location = Bottom_Left_Resize or Resize_Location = Top_Left_Resize then
        --       Screen.Left := Screen.Right - Decoration_Width - Current_Width;
        --     end if;
        --     if Resize_Location = Bottom_Right_Resize or Resize_Location = Top_Right_Resize then
        --       Screen.Right := Screen.Left + Decoration_Width + Current_Width;
        --     end if;
        --     --------------------
        --     --Bound_Corner_Sizing:
        --     --------------------
        --     --  declare
        --     --  Did_Resize_Right  : Boolean := False;
        --     --  Did_Resize_Left   : Boolean := False;
        --     --  Did_Resize_Top    : Boolean := False;
        --     --  Did_Resize_Bottom : Boolean := False;
        --       -- begin
        --       --   if Resize_Location = Bottom_Left_Resize or Resize_Location = Bottom_Right_Resize then
        --       --     Screen.Bottom := Screen.Top + Current_Height + Decoration_Height;
        --       --     if Screen.Bottom > Work_Area.Bottom then
        --       --       Did_Resize_Bottom := True;
        --       --       Screen.Bottom := Work_Area.Bottom;
        --       --     end if;
        --       --   end if;
        --       --   if Resize_Location = Top_Right_Resize or Resize_Location = Top_Left_Resize then
        --       --     Screen.Top := Screen.Bottom - Current_Height - Decoration_Height;
        --       --     if Screen.Top < Work_Area.Top then
        --       --       Did_Resize_Top := True;
        --       --       Screen.Top := Work_Area.Top;
        --       --     end if;
        --       --   end if;
        --       --   if Resize_Location = Bottom_Left_Resize or Resize_Location = Top_Left_Resize then
        --       --     Screen.Left := Screen.Right - Decoration_Width - Current_Width;
        --       --     if Screen.Left < Work_Area.Left then
        --       --       Did_Resize_Left := True;
        --       --       Screen.Left := Work_Area.Left;
        --       --     end if;
        --       --   end if;
        --       --   if Resize_Location = Bottom_Right_Resize or Resize_Location = Top_Right_Resize then
        --       --     Screen.Right := Screen.Left + Decoration_Width + Current_Width;
        --       --     if Screen.Right > Work_Area.Right then
        --       --       Did_Resize_Right := True;
        --       --       Screen.Right := Work_Area.Right;
        --       --     end if;
        --       --   end if;
        --       --   if
        --       --     Did_Resize_Right or
        --       --     Did_Resize_Left or
        --       --     Did_Resize_Top or
        --       --     Did_Resize_Bottom then
        --       --     null;--return Handle_Resize(
        --       --     --  Resize_Location => Resize_Location,
        --       --     --  Previous_Screen => Previous_Screen,
        --       --     --  Current_Screen  => Screen,
        --       --     --  Work_Area       => Work_Area,
        --       --     --  Decoration_Area => Decoration_Area);
        --       --   end if;
        --     case Resize_Location is
        --       when Bottom_Right_Resize =>
        --         if Screen.Right > Work_Area.Right or Screen.Bottom > Work_Area.Bottom then
        --           Screen := Previous_Screen;
        --         end if;
        --       when Bottom_Left_Resize =>
        --         if Screen.Left < Work_Area.Left or Screen.Bottom > Work_Area.Bottom then
        --           Screen := Previous_Screen;
        --         end if;
        --       when Top_Left_Resize =>
        --         if Screen.Top < Work_Area.Top or Screen.Left < Work_Area.Left then
        --           Screen := Previous_Screen;
        --         end if;
        --       when Top_Right_Resize =>
        --         if Screen.Top < Work_Area.Top or Screen.Right > Work_Area.Right then
        --           Screen := Previous_Screen;
        --         end if;
        --       when others =>
        --         null;
        --     end case;
        --   when others =>
        --     null;
        -- end case;
        -- Window.X      := Screen.Left;
        -- Window.Y      := Screen.Top;
        -- Window.Width  := (Screen.Right  - Screen.Left);
        -- Window.Height := (Screen.Bottom - Screen.Top);
        -- Protected_Data.Set(Window);
        -- Center_X := Screen.Left + (Window.Width)  / 2;
        -- Center_Y := Screen.Top  + (Window.Height) / 2;
        return Screen;
      end Handle_Resize;
  end Neo.System.Window;
