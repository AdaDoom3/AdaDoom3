package body Neo.System.Input is
  package body Import is separate;
  package body Impulse is
      procedure Enable  is Impulse : Record_Impulse := Impulses.Element(LOWER_NAME); begin Impulse.Is_Enabled := True;  Impulses.Replace(LOWER_NAME, Impulse); end Enable;
      procedure Disable is Impulse : Record_Impulse := Impulses.Element(LOWER_NAME); begin Impulse.Is_Enabled := False; Impulses.Replace(LOWER_NAME, Impulse); end Disable;
      procedure Finalize   (Controller : in out Record_Controller) is begin if Is_Running.Get                   then raise Impulse_Out_Of_Scope; end if; Impulses.Delete(LOWER_NAME);                                                                                      end Finalize;
      procedure Initialize (Controller : in out Record_Controller) is begin if Impulses.Has_Element(LOWER_NAME) then raise Duplicate;            end if; Impulses.Insert(LOWER_NAME, (Not_A_Formal_Subprogram'unrestricted_access, Bindings'unrestricted_access, True));   end Initialize;
    end Impulse;
  function Playstation (Trigger : in Enumerated_Trigger;         Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Playstation_Trigger_Kind, Player, Combo, Trigger         => Trigger, others => <>); end Playstation;
  function Playstation (Stick   : in Enumerated_Stick;           Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Playstation_Stick_Kind,   Player, Combo, Stick           => Stick,   others => <>); end Playstation;
  function Playstation (Key     : in Enumerated_Playstation_Key; Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Playstation_Key_Kind,     Player, Combo, Playstation_Key => Key,     others => <>); end Playstation;
  function Keyboard    (Key     : in Enumerated_Keyboard_Key;    Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Keyboard_Key_Kind,        Player, Combo, Keyboard_Key    => Key,     others => <>); end Keyboard;
  function Xbox        (Trigger : in Enumerated_Trigger;         Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Xbox_Trigger_Kind,        Player, Combo, Trigger         => Trigger, others => <>); end Xbox;
  function Xbox        (Stick   : in Enumerated_Stick;           Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Xbox_Stick_Kind,          Player, Combo, Stick           => Stick,   others => <>); end Xbox;
  function Xbox        (Key     : in Enumerated_Xbox_Key;        Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Xbox_Key_Kind,            Player, Combo, Xbox_Key        => Key,     others => <>); end Xbox;
  function Mouse       (Key     : in Enumerated_Mouse_Key;       Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Mouse_Key_Kind,           Player, Combo, Mouse_Key       => Key,     others => <>); end Mouse;
  function Mouse                                                (Combo : in Integer_4_Natural := NO_COMBO; Player : in Integer_4_Positive := 1) return Record_Binding is begin return (Mouse_Cursor_Kind,        Player, Combo,                             others => <>); end Mouse;
  function Get_Cursor                                        return Record_Location                      renames Import.Get_Cursor;
  function Get_Devices                                       return Ordered_Map_Record_Device.Unsafe.Map is begin return Devices.Get;                                                                                                                                      end Get_Devices;
  function Get_Device      (Identifier : in Integer_Address) return Record_Device                        is begin return Devices.Element(Identifier);                                                                                                                      end Get_Device;
  function Has_Device      (Identifier : in Integer_Address) return Boolean                              is begin return Devices.Has_Element(Identifier);                                                                                                                  end Has_Device;
  procedure Remove_Device  (Identifier : in Integer_Address)                                             is begin Devices.Delete(Identifier);                                                                                                                              end Remove_Device;
  procedure Add_Device     (Identifier : in Integer_Address; Device   : in Record_Device)                is begin if not Players.Has_Element(Device.Player) then Players.Insert(Device.Player, (others => <>)); end if;             Devices.Insert(Identifier, Device);    end Add_Device;
  procedure Set_Device     (Identifier : in Integer_Address; Player   : in Integer_4_Positive := 1)                           is Device : Record_Device := Devices.Element(Identifier); begin Device.Player            := Player;   Devices.Replace(Identifier, Device);   end Set_Device;
  procedure Inject_Text    (Identifier : in Integer_Address; Text     : in String_2_Unbounded)                                is Device : Record_Device := Devices.Element(Identifier); begin Device.Text              := Text;     Devices.Replace(Identifier, Device);   end Inject_Text;
  procedure Inject_Trigger (Identifier : in Integer_Address; Trigger  : in Enumerated_Trigger; Position : in Float_4_Percent) is Device : Record_Device := Devices.Element(Identifier); begin Device.Triggers(Trigger) := Position; Devices.Replace(Identifier, Device);   end Inject_Trigger;
  procedure Inject_Stick   (Identifier : in Integer_Address; Stick    : in Enumerated_Stick;   Axis     : in Record_Axis)     is Device : Record_Device := Devices.Element(Identifier); begin Device.Sticks(Stick)     := Axis;     Devices.Replace(Identifier, Device);   end Inject_Stick;
  procedure Inject_Mouse   (Identifier : in Integer_Address;                                   Location : in Record_Location) is Device : Record_Device := Devices.Element(Identifier); begin Device.Mouse_Cursor      := Location; Devices.Replace(Identifier, Device);   end Inject_Mouse;
  procedure Inject_Key(Identifier : in Integer_Address; Key : in Enumerated_Key; Is_Pressed : in Boolean) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      --Put_Line((if Is_Pressed then "Pressed " else "Released ") & Enumerated_Key'wide_image(Key) & " from" & Integer_Address'wide_image(Identifier));
      case Device.Kind is
        when Playstation_Device => if Device.Playstation_Keys(Key).Is_Pressed /= Is_Pressed then Device.Playstation_Keys(Key) := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Keyboard_Device    => if Device.Keyboard_Keys(Key).Is_Pressed    /= Is_Pressed then Device.Keyboard_Keys(Key)    := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Mouse_Device       => if Device.Mouse_Keys(Key).Is_Pressed       /= Is_Pressed then Device.Mouse_Keys(Key)       := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Xbox_Device        => if Device.Xbox_Keys(Key).Is_Pressed        /= Is_Pressed then Device.Xbox_Keys(Key)        := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when others             => raise Constraint_Error;
      end case;
    end Inject_Key;
  procedure Inject(Binding : in Record_Binding) is
    begin
      while Status.Is_Doing_Something loop delay DURATION_FOR_MUTEX_WAIT; end loop; Status.Set_Is_Doing_Something(True);
      case Binding.Kind is
        when Playstation_Trigger_Kind => Injection.Playstation_Triggers(Binding.Trigger)     := Binding.Position;
        when Playstation_Stick_Kind   => Injection.Playstation_Sticks(Binding.Stick)         := Binding.Axis;
        when Playstation_Key_Kind     => Injection.Playstation_Keys(Binding.Playstation_Key) := Binding.State;
        when Keyboard_Key_Kind        => Injection.Keyboard_Keys(Binding.Keyboard_Key)       := Binding.State;
        when Xbox_Trigger_Kind        => Injection.Xbox_Triggers(Binding.Trigger)            := Binding.Position;
        when Xbox_Stick_Kind          => Injection.Xbox_Sticks(Binding.Stick)                := Binding.Axis;
        when Xbox_Key_Kind            => Injection.Xbox_Keys(Binding.Xbox_Key)               := Binding.State;
        when Mouse_Key_Kind           => Injection.Mouse_Keys(Binding.Mouse_Key)             := Binding.State;
        when Mouse_Cursor_Kind        => Injection.Mouse_Cursor                              := Binding.Location;
        when Text_Kind                => if Is_In_Menu.Get then Injection.Text := Injection.Text & Binding.Text; end if;
      end case;
      Status.Set_Is_Doing_Something(False);
    end Inject;
  procedure Set_Vibration(Player : in Integer_4_Positive := 1; Frequency_High, Frequency_Low : in Float_4_Percent) is
    Current_Device : Ordered_Map_Record_Device.Cursor := Devices.First;
    begin
      while Devices.Has_Element(Current_Device) loop
        if Devices.Element(Current_Device).Player = Player then Import.Set_Vibration(Devices.Key(Current_Device), Frequency_High, Frequency_Low); end if;
        Devices.Next(Current_Device);
      end loop;
    end Set_Vibration;
  procedure Perform_Unbind(Parameters : in Vector_String_2_Unbounded.Vector) is
    begin
      null;
    end Perform_Unbind;
  procedure Perform_Bind(Parameters : in Vector_String_2_Unbounded.Vector) is
    begin
    --Impulse : Record_Impulse                   := (others => <>);
    --Success : Boolean                          := False;
    --procedure Dispatch(Command : in String_2; Binding : in Record_Binding) is
    --  begin
    --    if Command = COMMAND_BIND then
    --      for Binding of Impulse.Bindings.all loop
    --        if
    --      end loop;
    --    elsif Command = COMMAND_UNBIND then
    --
    --    end if;
    --  end Dispatch;
      --if To_String_2(Line.First_Element) /= COMMAND_BIND and To_String_2(Line.First_Element) /= COMMAND_UNBIND then raise Parse; end if;
      --case Length(Line) is
      --  when 1 =>
      --    Put_Line(Localize(COMMAND_HELP));
      --    Put_Line(COMMAND_BIND "/" & COMMAND_UNBIND & " " & Localize(COMMAND_EXAMPLE));
      --  when 3 =>
      --    Player  := 1;
      --    Impulse := Impulses.Element(To_String_2(Lines(2)));
      --  when 4 =>
      --    Player := Line(2);
      --    Line(2).Delete;
      --    Impulse := Impulses.Element(To_String_2(Lines(3)));
      --  when 5 =>
      --
      --when others => raise Parse; end case;
      --for I in Enumerated_Key'range loop
      --  if To_String_2(Line(2)) = Type_To_Vary'wide_image(I) then
      --    if I in Enumerated_Xbox_Key'range then
      --    elsif I in Enumerated_Playstation_Key'range
      --    return;
      --  end if;
      --end loop;
      null;
    end Perform_Bind;
  procedure Run is
    Last_Time        : Time          := Clock;
    Player           : Record_Player := (others => <>);
    Previous_Impulse : Hashed_Map_Record_Impulse.Cursor;
    Current_Impulse  : Hashed_Map_Record_Impulse.Cursor;
    Current_Device   : Ordered_Map_Record_Device.Cursor;
    Current_Player   : Ordered_Map_Record_Player.Cursor;
    Current_Binding  : Vector_Record_Binding.Cursor;
    begin
      Status.Set_Is_Doing_Something(False);
      Import.Initialize;
      while Is_Running.Get and Import.Update loop
        if Is_Active.Get then
          while Status.Is_Doing_Something loop delay DURATION_FOR_MUTEX_WAIT; end loop; Status.Set_Is_Doing_Something(True);
          Current_Player := Players.First;
          while Players.Has_Element(Current_Player) loop
            Players.Replace(Current_Player, Injection);
            Players.Next(Current_Player);
          end loop;
          Current_Device := Devices.First;
          while Devices.Has_Element(Current_Device) loop
            declare
            Device : Record_Device := Devices.Element(Current_Device);
            begin
              Player := Players.Element(Device.Player);
              if Device.Text /= NULL_STRING_2_UNBOUNDED then Player.Text := Player.Text & Device.Text; end if;
              case Device.Kind is
                when Keyboard_Device =>
                  for Key in Enumerated_Keyboard_Key'range loop
                    if Device.Keyboard_Keys(Key).Is_Pressed and (not Player.Keyboard_Keys(Key).Is_Pressed or Device.Keyboard_Keys(Key).Last < Player.Keyboard_Keys(Key).Last) then
                      Player.Keyboard_Keys(Key) := Device.Keyboard_Keys(Key);
                      case Key is
                        when Left_Shift_Key       | Right_Shift_Key       => Player.Keyboard_Keys(Shift_Key)       := Player.Keyboard_Keys(Key);
                        when Left_Control_Key     | Right_Control_Key     => Player.Keyboard_Keys(Control_Key)     := Player.Keyboard_Keys(Key);
                        when Left_Alternative_Key | Right_Alternative_Key => Player.Keyboard_Keys(Alternative_Key) := Player.Keyboard_Keys(Key);
                      when others => null; end case;
                    elsif not Device.Keyboard_Keys(Key).Is_Pressed and Device.Keyboard_Keys(Key).Last > Player.Keyboard_Keys(Key).Last then Player.Keyboard_Keys(Key) := Device.Keyboard_Keys(Key); end if;
                  end loop;
                when Mouse_Device =>
                  for Key in Enumerated_Mouse_Key'range loop
                    if Device.Mouse_Keys(Key).Is_Pressed and (not Player.Mouse_Keys(Key).Is_Pressed or Device.Mouse_Keys(Key).Last < Player.Mouse_Keys(Key).Last) then Player.Mouse_Keys(Key) := Device.Mouse_Keys(Key); end if;
                    if Device.Mouse_Keys(Key).Is_Pressed and Key in Horizontal_Wheel_Left_Key..Vertical_Wheel_Down_Key then
                      Device.Mouse_Keys(Key).Is_Pressed := False;
                      Devices.Replace(Current_Device, Device);
                    end if;
                  end loop;
                  Player.Mouse_Cursor := (Player.Mouse_Cursor.X + Device.Mouse_Cursor.X, Player.Mouse_Cursor.Y + Device.Mouse_Cursor.Y);
                when Xbox_Device =>
                  for Key in Enumerated_Xbox_Key'range loop
                    if Device.Xbox_Keys(Key).Is_Pressed and (not Player.Xbox_Keys(Key).Is_Pressed or Device.Xbox_Keys(Key).Last < Player.Xbox_Keys(Key).Last) then Player.Xbox_Keys(Key) := Device.Xbox_Keys(Key); end if;
                  end loop;
                  for Stick in Enumerated_Stick'range loop Player.Xbox_Sticks(Stick) := (Player.Xbox_Sticks(Stick).X + Device.Sticks(Stick).X, Player.Xbox_Sticks(Stick).Y + Device.Sticks(Stick).Y); end loop;
                  for Trigger in Enumerated_Trigger'range loop
                    if Player.Xbox_Triggers(Trigger) + Device.Triggers(Trigger) > 100.0 then Player.Xbox_Triggers(Trigger) := 100.0;
                    else Player.Xbox_Triggers(Trigger) := Player.Xbox_Triggers(Trigger) + Device.Triggers(Trigger); end if;
                  end loop;
                when Playstation_Device =>
                  for Key in Enumerated_Playstation_Key'range loop
                    if Device.Playstation_Keys(Key).Is_Pressed and (not Player.Playstation_Keys(Key).Is_Pressed or Device.Playstation_Keys(Key).Last < Player.Playstation_Keys(Key).Last) then Player.Playstation_Keys(Key) := Device.Playstation_Keys(Key); end if;
                  end loop;
                  for Stick in Enumerated_Stick'range loop Player.Playstation_Sticks(Stick) := (Player.Playstation_Sticks(Stick).X + Device.Sticks(Stick).X, Player.Playstation_Sticks(Stick).Y + Device.Sticks(Stick).Y); end loop;
                  for Trigger in Enumerated_Trigger'range loop
                    if Player.Playstation_Triggers(Trigger) + Device.Triggers(Trigger) > 100.0 then Player.Playstation_Triggers(Trigger) := 100.0;
                    else Player.Playstation_Triggers(Trigger) := Player.Playstation_Triggers(Trigger) + Device.Triggers(Trigger); end if;
                  end loop;
              end case;
              Players.Replace(Device.Player, Player);
            end;
            Devices.Next(Current_Device);
          end loop;
          Current_Impulse := Impulses.First;
          while Impulses.Has_Element(Current_Impulse) loop
            declare
            Impulse : Record_Impulse := Impulses.Element(Current_Impulse);
            begin
              Current_Binding := Impulse.Bindings.all.First;
              while Impulse.Bindings.all.Has_Element(Current_Binding) loop
                declare
                Binding : Record_Binding := Impulse.Bindings.all.Element(Current_Binding);
                procedure Handle_Common is
                  begin
                    Impulse.Bindings.all.Replace(Current_Binding, Binding);
                    if Binding.Combo /= NO_COMBO then
                      for Other_Binding of Impulse.Bindings.all.Get loop
                        if Other_Binding.Combo = Binding.Combo then
                          case Other_Binding.Kind is
                            when Text_Kind                                                                 => if Other_Binding.Text = NULL_STRING_2_UNBOUNDED then return; end if;
                            when Playstation_Stick_Kind | Xbox_Stick_Kind | Mouse_Cursor_Kind              => if Other_Binding.Location = (others => <>)      then return; end if;
                            when Keyboard_Key_Kind | Mouse_Key_Kind | Xbox_Key_Kind | Playstation_Key_Kind => if not Other_Binding.State.Is_Pressed           then return; end if;
                            when Playstation_Trigger_Kind | Xbox_Trigger_Kind                              => if Other_Binding.Position = 0.0                 then return; end if;
                          end case;
                        end if;
                      end loop;
                    end if;
                    Impulse.Trip(Binding);
                    if Binding.Kind = Text_Kind then Binding.Text := NULL_STRING_2_UNBOUNDED; Impulse.Bindings.all.Replace(Current_Binding, Binding); end if;
                  end Handle_Common;
                begin
                  Player := Players.Element(Binding.Player);
                  case Binding.Kind is
                    when Playstation_Trigger_Kind => if Player.Playstation_Triggers(Binding.Trigger)                /= Binding.Position         then Binding.Position := Player.Playstation_Triggers(Binding.Trigger);     Handle_Common; end if;
                    when Playstation_Stick_Kind   => if Player.Playstation_Sticks(Binding.Stick)                    /= Binding.Axis             then Binding.Axis     := Player.Playstation_Sticks(Binding.Stick);         Handle_Common; end if;
                    when Playstation_Key_Kind     => if Player.Playstation_Keys(Binding.Playstation_Key).Is_Pressed /= Binding.State.Is_Pressed then Binding.State    := Player.Playstation_Keys(Binding.Playstation_Key); Handle_Common; end if;
                    when Keyboard_Key_Kind        => if Player.Keyboard_Keys(Binding.Keyboard_Key).Is_Pressed       /= Binding.State.Is_Pressed then Binding.State    := Player.Keyboard_Keys(Binding.Keyboard_Key);       Handle_Common; end if;
                    when Xbox_Trigger_Kind        => if Player.Xbox_Triggers(Binding.Trigger)                       /= Binding.Position         then Binding.Position := Player.Xbox_Triggers(Binding.Trigger);            Handle_Common; end if;
                    when Xbox_Stick_Kind          => if Player.Xbox_Sticks(Binding.Stick)                           /= Binding.Axis             then Binding.Axis     := Player.Xbox_Sticks(Binding.Stick);                Handle_Common; end if;
                    when Xbox_Key_Kind            => if Player.Xbox_Keys(Binding.Xbox_Key).Is_Pressed               /= Binding.State.Is_Pressed then Binding.State    := Player.Xbox_Keys(Binding.Xbox_Key);               Handle_Common; end if;
                    when Mouse_Key_Kind           => if Player.Mouse_Keys(Binding.Mouse_Key).Is_Pressed             /= Binding.State.Is_Pressed then Binding.State    := Player.Mouse_Keys(Binding.Mouse_Key);             Handle_Common; end if;
                    when Mouse_Cursor_Kind        => if Player.Mouse_Cursor                                         /= Binding.Location         then Binding.Location := Player.Mouse_Cursor;                              Handle_Common; end if;
                    when Text_Kind                => if Player.Text                                                 /= Binding.Text             then Binding.Text     := Player.Text;                                      Handle_Common; end if;
                  end case;
                end;
                Impulse.Bindings.all.Next(Current_Binding);
              end loop;
            end;
            Impulses.Next(Current_Impulse);
          end loop;
          Injection.Text                                             := NULL_STRING_2_UNBOUNDED;
          Injection.Mouse_Keys(Vertical_Wheel_Down_Key).Is_Pressed   := False;
          Injection.Mouse_Keys(Horizontal_Wheel_Left_Key).Is_Pressed := False;
          Status.Set_Is_Doing_Something(False);
        end if;
        delay DURATION_TO_WAIT_BEFORE_POLLING - (Clock - Last_Time);
        Last_Time := Clock;
      end loop;
      Devices.Clear;
      Import.Finalize;
    end Run;
begin
  Main_Task.Initialize;
end Neo.System.Input;
