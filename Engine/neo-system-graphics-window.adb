package body Neo.System.Graphics.Window is
  package body Import is separate;
  function Get_Borders    return Vector_Record_Border.Unsafe.Vector renames Import.Get_Borders;
  function Get_Specifics  return Record_Specifics                   renames Import.Get_Specifics;
  function Get_Decoration return Record_Border                      renames Import.Get_Decoration;
  procedure Preform_Exit_To_Menu(Binding : in Record_Binding) is
    begin
      if Binding.Kind = Mouse_Key_Kind and then Binding.State.Is_Pressed then
        null;--Is_In_Menu.Set(not Is_In_Menu.Get);--False); Toggle for now
      end if;
    end Preform_Exit_To_Menu;
  procedure Preform_Toggle_Fullscreen(Binding : in Record_Binding) is
    begin
      if Binding.State.Is_Pressed then
        State.Set((case State.Get is when Multi_Monitor_State | Fullscreen_State => Windowed_State, when Windowed_State => Fullscreen_State));
      end if;
    end Preform_Toggle_Fullscreen;
  procedure Preform_Detect_Menu_Mode_Entry(Binding : in Record_Binding) is
    Location : Record_Location := Get_Cursor;
    Border   : Record_Border   := Get_Borders.Element(1);
    begin
      --Put_Line(Integer_8_Signed'wide_image(Border.Right) & "   " & Integer_8_Signed'wide_image(Border.Bottom));
      null;--  Take_Control;
      --  Impulse_Detect_Menu_Mode_Entry.Disable;
      --end if;
    end Preform_Detect_Menu_Mode_Entry;
  procedure Change_State(Kind : in Enumerated_Change) is
    begin
      case Kind is
        when Iconic_Change     => Is_Iconized.Set(True);
        when Fullscreen_Change => Is_Iconized.Set(False); State.Set(Fullscreen_State);
        when Windowed_Change   => Is_Iconized.Set(False); State.Set(Windowed_State);
      end case;
    end Change_State;
  function Get_Normalized_Cursor return Record_Location is
    begin
      return (others => <>);
    end Get_Normalized_Cursor;
  procedure Activate(Do_Activate, Do_Detect_Click : in Boolean; X, Y : in Integer_8_Signed) is
    begin
      if Do_Activate then
        Is_Active.Set(True);
        if Is_In_Menu.Get then Import.Set_Custom_Mouse(Inactive_Cursor); end if;
      else
        Is_Active.Set(False);
        Import.Set_Custom_Mouse(System_Cursor);
        --Import.Clip_Mouse(Undo => True);
        if State.Get /= Windowed_State then
          --if State.Get = Multi_Monitor_State then Import.Finalize_Multi_Monitor; end if;
          Import.Iconize;
        end if;
      end if;
    end Activate;
  function Resize(Kind : in Enumerated_Resize; Border : in Record_Border) return Record_Border is
    Result         : Record_Border    := Border;
    Decoration     : Record_Border    := Get_Decoration;
    Extra_Width    : Integer_8_Signed := Decoration.Right  + Decoration.Left;
    Extra_Height   : Integer_8_Signed := Decoration.Bottom + Decoration.Top;
    Current_Width  : Integer_8_Signed := (if Border.Right  - Border.Left - Extra_Width  < MINIMUM_FACTOR then MINIMUM_FACTOR else Border.Right - Border.Left - Extra_Width);
    Current_Height : Integer_8_Signed := (if Border.Bottom - Border.Top  - Extra_Height < Integer_8_Signed(Integer_4_Positive(MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get / Aspect_Narrow_Horizontal.Get) then
                                         Integer_8_Signed(Integer_4_Positive(MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get) else Border.Bottom - Border.Top  - Extra_Height);
    Maximum_Width  : Integer_8_Signed := Integer_8_Signed(Integer_4_Positive(Current_Height) * Aspect_Narrow_Horizontal.Get / Aspect_Narrow_Vertical.Get);
    Maximum_Height : Integer_8_Signed := Integer_8_Signed(Integer_4_Positive(Current_Width)  * Aspect_Wide_Vertical.Get     / Aspect_Wide_Horizontal.Get);
    Minimum_Width  : Integer_8_Signed := Integer_8_Signed(Integer_4_Positive(Current_Height) * Aspect_Wide_Horizontal.Get   / Aspect_Wide_Vertical.Get);
    Minimum_Height : Integer_8_Signed := Integer_8_Signed(Integer_4_Positive(Current_Width)  * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get);
    Fit_Width      : Integer_8_Signed := (if Current_Width  > Maximum_Width  then Maximum_Width  elsif Current_Width  < Minimum_Width  then Minimum_Width  else Current_Width);
    Fit_Height     : Integer_8_Signed := (if Current_Height > Maximum_Height then Maximum_Height elsif Current_Height < Minimum_Height then Minimum_Height else Current_Height);
    Resize_Factor  : Integer_8_Signed := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) / 2;
    Resize_Extra   : Integer_8_Signed := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) rem 2;
    begin
      case Kind is
        when Left_Resize =>
          Result.Left   := Result.Right  - Current_Width  - Extra_Width;
          Result.Bottom := Result.Top    + Fit_Height     + Extra_Height;
        when Right_Resize =>
          Result.Right  := Result.Left   + Current_Width  + Extra_Width;
          Result.Bottom := Result.Top    + Fit_Height     + Extra_Height;
        when Bottom_Right_Resize =>
          Result.Right  := Result.Left   + Fit_Width      + Extra_Width;
          Result.Bottom := Result.Top    + Current_Height + Extra_Height;
        when Top_Right_Resize =>
          Result.Top    := Result.Bottom - Current_Height - Extra_Height;
          Result.Right  := Result.Left   + Fit_Width      + Extra_Width;
        when Bottom_Left_Resize =>
          Result.Left   := Result.Right  - Fit_Width      - Extra_Width;
          Result.Bottom := Result.Top    + Current_Height + Extra_Height;
        when Top_Left_Resize =>
          Result.Left   := Result.Right  - Fit_Width      - Extra_Width;
          Result.Top    := Result.Bottom - Current_Height - Extra_Height;
        when Bottom_Resize =>
          Result.Right  := Border.Right  + Resize_Factor;
          Result.Left   := Result.Right  - Fit_Width      - Extra_Width + Resize_Extra;
          Result.Bottom := Result.Top    + Current_Height + Extra_Height;
        when Top_Resize =>
          Result.Right  := Border.Right  + Resize_Factor;
          Result.Left   := Result.Right  - Fit_Width      - Extra_Width + Resize_Extra;
          Result.Top    := Result.Bottom - Current_Height - Extra_Height;
      when Other_Resize => Result := Border; end case;
      Width.Set  (Integer_4_Positive(Result.Right  - Result.Left - Extra_Width));
      Height.Set (Integer_4_Positive(Result.Bottom - Result.Top  - Extra_Height));
      return Result;
    end Resize;
  procedure Run_Multi_Monitor_Window is
    Index : Integer_4_Positive := 1;--Current_Number_Of_Monitors.Get;
    begin
      --Current_Number_Of_Monitors.Set(Index + 1);
      while Import.Update(Index) and Is_Running.Get loop null;
        --Render_Backend;
      end loop;
    end Run_Multi_Monitor_Window;
  procedure Run is
    Border        : Record_Border    := (others => <>);
    Current_State : Enumerated_State := State.Get;
    Current_Menu  : Boolean          := Is_In_Menu.Get;
    begin
      Import.Assert_Only_Instance;
      Import.Initialize;
      Detect_Menu_Mode_Entry.Bindings.Append(Mouse(Left_Mouse_Key));
      Toggle_Fullscreen.Bindings.Append(Keyboard(F11_Key));
      Exit_To_Menu.Bindings.Append(Keyboard(Escape_Key));
      while Is_Running.Get loop
        case Current_State is
          when Fullscreen_State | Multi_Monitor_State =>
            Import.Adjust_Fullscreen;
            --if Current_State = Multi_Monitor_State then Import.Initialize_Multi_Monitor; end if;
          when Windowed_State =>
            Border := Import.Get_Decoration;
            Import.Adjust_Windowed(
              Width  => Width.Get  + Integer_4_Positive(Border.Right  + Border.Left),
              Height => Height.Get + Integer_4_Positive(Border.Bottom + Border.Top));
        end case;
        --Previous_State := State.Get;
        --if Is_In_Menu_Mode.Get then
        --  Import.Hide_Mouse(False, False);
        --  Import.Set_Custom_Mouse(False);
        --else
        ----  Import.Hide_Mouse(True, True);
        --end if;
        while Current_State = State.Get and Is_Running.Get loop
          if not Import.Update then Is_Running.Set(False); end if;
          --if Current_Menu /= Is_In_Menu.Get then
          --  if Current_Menu then
          --    Current_Menu := Is_In_Menu.Get;
          --    Border := Get_Borders.First_Element;
          --    Import.Set_Cursor(X => Border.Left + (Border.Right  - Border.Left) / 2,Y => Border.Top  + (Border.Bottom - Border.Top)  / 2);
          --    if Current_State = Fullscreen_State then Import.Clip_Mouse(Undo => False, Do_Hide => False);
          --    else Import.Clip_Mouse(Undo => True, Do_Hide => False); end if;
          --  elsif Do_Hide_Mouse then
          --    Current_Menu := Is_In_Menu.Get;
          --    Import.Clip_Mouse(Undo => False, Do_Hide => True);
          --  end if;
          --end if;
        end loop;
        --if Current_State = Multi_Monitor_State and not Is_Iconized.Get then Import.Finalize_Multi_Monitor; end if;
        Current_State := State.Get;
      end loop;
      Import.Finalize;
    end Run;
begin
  Put_Title(Localize("WINDOW"));
  New_Line;
  Put_Line(Localize("Color:")          & Integer_4_Positive'wide_image(SPECIFICS.Color_Bits));
  Put_Line(Localize("Depth:")          & Integer_4_Positive'wide_image(SPECIFICS.Depth_Bits));
  Put_Line(Localize("Stencil:")        & Integer_4_Positive'wide_image(SPECIFICS.Stencil_Bits));
  Put_Line(Localize("Bits per pixel:") & Integer_4_Positive'wide_image(SPECIFICS.Bits_Per_Pixel));
  Put_Line(Localize("Multisamples:")   & Integer_4_Natural'wide_image(SPECIFICS.Mutlisamples));
  if SPECIFICS.Has_Swap_Control_Tear   then Put_Line(Localize("Has swap control tear"));   end if;
  if SPECIFICS.Has_Stereo_Pixel_Format then Put_Line(Localize("Has stereo pixel format")); end if;
  New_Line;
end Neo.System.Graphics.Window;
