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
package body Neo.Command.System.Window
  is pragma Source_File_Name("neo-window.adb");
  ------------
  -- Import --
  ------------
    package body Import
      is separate;
    package Instantiation
      is new Import(
        Handle_Finalization => Handle_Finalization,
        Handle_Activation   => Handle_Activation,
        Handle_State_Change => Handle_State_Change,
        Handle_Window_Move  => Handle_Window_Move,
        Handle_Resize       => Handle_Resize);
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
        while Instantiation.Handle_Events(Index) and not Do_Quit loop
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
              Instantiation.Set_Custom_Mouse(False);
            else
              Instantiation.Set_Custom_Mouse(True);
              if Is_In_Border(X, Y) then
                Take_Control;
              end if;
            end if;
          else
            Protected_Data.Set_Busy(False);
            if Protected_Data.Get.State /= Windowed_State then
              if Protected_Data.Get.State = Multi_Monitor_State then
                Instantiation.Finalize_Multi_Monitor;
              end if;
              Instantiation.Iconize;
              Window.Is_Iconized := True;
            end if;
            Instantiation.Hide_Mouse(False);
            Instantiation.Set_Custom_Mouse(True);
            Keys_Down := (others => False);
          end if;
          Protected_Data.Set(Window);
        end if;
      end Handle_Activation;
  --------------------
  -- Handle_Reshape --
  --------------------
    function Handle_Reshape(
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
        --     ELSif Screen.Right >= Work_Area.Right and Current_Width > Previous_Width  then
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
        --     ELSif Screen.Bottom >= Work_Area.Bottom and Current_Height > Previous_Height then
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
        --     --  DECLARE
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
      end Handle_Reshape;
  -----
  begin
  -----
    if Protected_Data.Get.Title /= null then
      return;
    end if;
    if not Do_Allow_Multiple_Instances and then not Instantiation.Is_Only_Instance(Title) then
      raise Call_Failure;
    end loop;
    Instantiated.Initialize;
    Outter:loop
      Get_Screen_Information(Bits_Per_Pixel, Native_Width, Native_Height);
      -- Check if monitor native resolution violates the minimum/maximum aspect requirements
      case Protected_Data.Get.State is
        when Fullscreen_State | Multi_Monitor_State =>
          Instantiation.Adjust(
            Title         => Title,
            Do_Fullscreen => True,
            X             => 0,
            Y             => 0,
            Width         => Native_Width,
            Height        => Native_Height);
          if Protected_Data.Get.State = Multi_Monitor_State then
            Instantiation.Initialize_Multi_Monitor(
            Monitors => Instantiation.Get_Monitors);
            -- Detect number of graphics cards
            -- Spawn_Tasks_For_Auxiliarary_Video_Cards:
          end if;
          Center := (Native_Width / 2, Native_Height / 2);
        when Windowed_State =>
          -- if the current width and height are greater than native, then accomications must be made
          Instantiation.Adjust(
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
          if Protected_Data.Get.Is_In_Menu_Mode then
            Instantiation.Hide_Mouse(False, False);
            Instantiation.Set_Custom_Mouse(False);
          else
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
              raise Call_Failure;
            end if;
          end Move_From_Hiding;
          Instantiation.Move_Topmost_Windows_Out_Of_The_Way;
          Instantiation.Hide_Mouse(True, False);
        else
          Instantiation.Hide_Mouse(True, True);
        end if;
        Protected_Data.Set_Busy(True);
      end if;
      --Render_Backend.All;
      exit Outter when or Protected_Data.Get.Is_Done or not Instantiation.Handle_Events;
      exit Inner when not Protected_Data.Is_Initialized;
    end loop Inner;
    case Previous_State is
      when Windowed_State =>
        X := Protected_Data.Get.X;
        Y := Protected_Data.Get.Y;
      when Multi_Monitor_State =>
        Instantiation.Finalize_Multi_Monitor;
      when others =>
        null;
      end case;
    end loop Outter;
  end Neo.Command.System.Window;

