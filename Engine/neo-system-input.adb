with Ada.Exceptions;          use Ada.Exceptions;
package body Neo.System.Input is
  package body Import is separate;
  package Implementation is new Import(Handle_Stick, Handle_Generic_Stick, Handle_Trigger, Handle_Generic_Trigger, Handle_Key, Handle_Generic_Key, Handle_Mouse, Add_Device, Remove_Device, Get_Device, Has_ELement);
  package body Impulse is
      procedure Enable is
        Impulse : Record_Impulse := Impulses.Element(LOWER_NAME);
        begin
          Impulse.Is_Enabled := True;
          Impulses.Replace(LOWER_NAME, Impulse);
        end Enable;
      procedure Disable is
        Impulse : Record_Impulse := Impulses.Element(LOWER_NAME);
        begin
          Impulse.Is_Enabled := False;
          Impulses.Replace(LOWER_NAME, Impulse);
        end Disable;
      overriding procedure Initialize(Controller : in out Record_Controller) is
        begin
          if Impulses.Has_Element(LOWER_NAME) then raise Duplicate; end if;
          Impulses.Insert(LOWER_NAME, (Not_A_Formal_Subprogram'unrestricted_access, Bindings'unrestricted_access, False, True));
        end Initialize;
      overriding procedure Finalize(Controller : in out Record_Controller) is
        Impulse : Record_Impulse := Impulses.Element(LOWER_NAME);
        begin
          Impulse.Is_Garbage := True;
          Impulses.Replace(LOWER_NAME, Impulse);
          while Impulses.Has_Element(LOWER_NAME) loop delay 0.001; end loop;
        end Finalize;
    end Impulse;
  procedure Trip1(Binding : in Record_Binding) is
    begin
      Put_Line("Tripped!");
      if Binding.Kind = Character_Kind then
        Put_Line("Key: " & Binding.Input);
      end if;
    end Trip1;
  package Trip12 is new Impulse("Test!", Trip1);
  procedure Test is
    begin
      Put_Title("INPUT TEST");
      Trip12.Bindings.Append((Kind => Keyboard_Key_Kind, Keyboard_Key => Shift_Key, others => <>));
      Trip12.Bindings.Append((Kind => Mouse_Key_Kind, Mouse_Key => Vertical_Wheel_Down_Key, others => <>));
      Trip12.Bindings.Append((Kind => Character_Kind, others => <>));
    end Test;
  procedure Run_Update is
    Last_Time        : Time          := Clock;
    Player           : Record_Player := (others => <>);
    Previous_Impulse : Map_Impulses.Cursor;
    Current_Impulse  : Map_Impulses.Cursor;
    Current_Device   : Map_Devices.Cursor;
    Current_Player   : Map_Players.Cursor;
    Current_Binding  : Vector_Bindings.Cursor;
    begin
      Implementation.Initialize;
      while Is_Running.Get loop
        Last_Time := Clock;
        Implementation.Update_Devices;
        Implementation.Handle_Events;
        Current_Player := Players.First;
        while Players.Has_Element(Current_Player) loop
          Players.Replace(Current_Player, (others => <>));
          Players.Next(Current_Player);
        end loop;
        Current_Device := Devices.First;
        while Devices.Has_Element(Current_Device) loop
          declare
          Device : Record_Device := Devices.Element(Current_Device);
          begin
          Player := Players.Element(Device.Player);
          case Device.Kind is
            when Keyboard_Device =>
              for Key in Enumerated_Keyboard_Key'range loop
                if Device.Keyboard_Keys(Key).Is_Pressed and (not Player.Keyboard_Keys(Key).Is_Pressed or Device.Keyboard_Keys(Key).Last < Player.Keyboard_Keys(Key).Last) then
                  Player.Keyboard_Keys(Key) := Device.Keyboard_Keys(Key);
                  case Key is
                    when Left_Shift_Key       | Right_Shift_Key       => Player.Keyboard_Keys(Shift_Key)       := Player.Keyboard_Keys(Key);
                    when Left_Alternative_Key | Right_Alternative_Key => Player.Keyboard_Keys(Alternative_Key) := Player.Keyboard_Keys(Key);
                    when Left_Control_Key     | Right_Control_Key     => Player.Keyboard_Keys(Control_Key)     := Player.Keyboard_Keys(Key);
                    when others => null;
                  end case;
                elsif not Device.Keyboard_Keys(Key).Is_Pressed and Device.Keyboard_Keys(Key).Last > Player.Keyboard_Keys(Key).Last then
                  Player.Keyboard_Keys(Key) := Device.Keyboard_Keys(Key);
                end if;
              end loop;
            when Mouse_Device =>
              for Key in Enumerated_Mouse_Key'range loop
                if Device.Mouse_Keys(Key).Is_Pressed and (not Player.Mouse_Keys(Key).Is_Pressed or Device.Mouse_Keys(Key).Last < Player.Mouse_Keys(Key).Last) then
                  Player.Mouse_Keys(Key) := Device.Mouse_Keys(Key);
                end if;
                if Device.Mouse_Keys(Key).Is_Pressed and Key in Horizontal_Wheel_Left_Key..Vertical_Wheel_Down_Key then
                  Device.Mouse_Keys(Key).Is_Pressed := False;
                  Devices.Replace(Current_Device, Device);
                end if;
              end loop;
              Player.Mouse_Location := (Player.Mouse_Location.X + Device.Mouse_Location.X, Player.Mouse_Location.Y + Device.Mouse_Location.Y);
            when Xbox_Device =>
              for Key in Enumerated_Xbox_Key'range loop
                if Device.Xbox_Keys(Key).Is_Pressed and (not Player.Xbox_Keys(Key).Is_Pressed or Device.Xbox_Keys(Key).Last < Player.Xbox_Keys(Key).Last) then
                  Player.Xbox_Keys(Key) := Device.Xbox_Keys(Key);
                end if;
              end loop;
              for Stick in Enumerated_Stick'range loop
                Player.Xbox_Sticks(Stick) := (Player.Xbox_Sticks(Stick).X + Device.Sticks(Stick).X, Player.Xbox_Sticks(Stick).Y + Device.Sticks(Stick).Y);
              end loop;
              for Trigger in Enumerated_Trigger'range loop
                if Player.Xbox_Triggers(Trigger) + Device.Triggers(Trigger) > 100.0 then Player.Xbox_Triggers(Trigger) := 100.0;
                else Player.Xbox_Triggers(Trigger) := Player.Xbox_Triggers(Trigger) + Device.Triggers(Trigger); end if;
              end loop;
            when Playstation_Device =>
              for Key in Enumerated_Playstation_Key'range loop
                if Device.Playstation_Keys(Key).Is_Pressed and (not Player.Playstation_Keys(Key).Is_Pressed or Device.Playstation_Keys(Key).Last < Player.Playstation_Keys(Key).Last) then
                  Player.Playstation_Keys(Key) := Device.Playstation_Keys(Key);
                end if;
              end loop;
              for Stick in Enumerated_Stick'range loop
                Player.Playstation_Sticks(Stick) := (Player.Playstation_Sticks(Stick).X + Device.Sticks(Stick).X, Player.Playstation_Sticks(Stick).Y + Device.Sticks(Stick).Y);
              end loop;
              for Trigger in Enumerated_Trigger'range loop
                if Player.Playstation_Triggers(Trigger) + Device.Triggers(Trigger) > 100.0 then Player.Playstation_Triggers(Trigger) := 100.0;
                else Player.Playstation_Triggers(Trigger) := Player.Playstation_Triggers(Trigger) + Device.Triggers(Trigger); end if;
              end loop;
            when Generic_Device => null;
          end case;
          Players.Replace(Device.Player, Player); end; Devices.Next(Current_Device);
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
              begin
                Player := Players.Element(Binding.Player);
                case Binding.Kind is
                  when Character_Kind =>
                    if Player.Keyboard_Keys /= Binding.Keyboard_Keys then
                      begin
                        Binding.Keyboard_Keys := Player.Keyboard_Keys;
                        Binding.Input := Implementation.Lookup_Character(Player.Keyboard_Keys);
                        Impulse.Trip(Binding);
                      exception when No_Printable_Character => Binding.Input := NULL_CHARACTER_2; end;
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                    end if;
                  when Keyboard_Key_Kind =>
                    if Player.Keyboard_Keys(Binding.Keyboard_Key).Is_Pressed /= Binding.State.Is_Pressed then
                      Binding.State := Player.Keyboard_Keys(Binding.Keyboard_Key);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Mouse_Key_Kind =>
                    if Player.Mouse_Keys(Binding.Mouse_Key).Is_Pressed /= Binding.State.Is_Pressed then
                      Binding.State := Player.Mouse_Keys(Binding.Mouse_Key);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Mouse_Cursor_Kind =>
                    if Player.Mouse_Location /= Binding.Location then
                      Binding.Location := Player.Mouse_Location;
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Xbox_Key_Kind =>
                    if Player.Xbox_Keys(Binding.Xbox_Key).Is_Pressed /= Binding.State.Is_Pressed then
                      Binding.State := Player.Xbox_Keys(Binding.Xbox_Key);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Playstation_Key_Kind =>
                    if Player.Playstation_Keys(Binding.Playstation_Key).Is_Pressed /= Binding.State.Is_Pressed then
                      Binding.State := Player.Playstation_Keys(Binding.Playstation_Key);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Xbox_Stick_Kind =>
                    if Player.Xbox_Sticks(Binding.Stick) /= Binding.Location then
                      Binding.Location := Player.Xbox_Sticks(Binding.Stick);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Playstation_Stick_Kind =>
                    if Player.Playstation_Sticks(Binding.Stick) /= Binding.Location then
                      Binding.Location := Player.Playstation_Sticks(Binding.Stick);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Xbox_Trigger_Kind =>
                    if Player.Xbox_Triggers(Binding.Trigger) /= Binding.Position then
                      Binding.Position := Player.Xbox_Triggers(Binding.Trigger);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Playstation_Trigger_Kind =>
                    if Player.Playstation_Triggers(Binding.Trigger) /= Binding.Position then
                      Binding.Position := Player.Playstation_Triggers(Binding.Trigger);
                      Impulse.Bindings.all.Replace(Current_Binding, Binding);
                      Impulse.Trip(Binding);
                    end if;
                  when Generic_Key_Kind => null;
                    --if Player.Generic_Keys.Element(Binding.Key_Number) /= Binding.Is_Pressed then
                    --  Binding.Is_Pressed := Player.Generic_Keys.Element(Binding.Key_Number);
                    --  Impulse.Bindings.all.Replace(Current_Binding, Binding);
                    --  Impulse.Trip(Binding);
                    --end if;
                  when Generic_Stick_Kind => null;
                    --if Player.Generic_Sticks.Element(Binding.Stick_Number) /= Binding.Location then
                    --  Binding.Location := Player.Generic_Sticks.Element(Binding.Stick_Number);
                    --  Impulse.Bindings.all.Replace(Current_Binding, Binding);
                    --  Impulse.Trip(Binding);
                    --end if;
                  when Generic_Trigger_Kind => null;
                    --if Player.Generic_Triggers.Element(Binding.Trigger_Number) /= Binding.Position then
                    --  Binding.Position := Player.Generic_Triggers.Element(Binding.Trigger_Number);
                    --  Impulse.Bindings.all.Replace(Current_Binding, Binding);
                    --  Impulse.Trip(Binding);
                    --end if;
                end case;
              exception when others =>
                Put_Line("Player error?");
              end;
              Impulse.Bindings.all.Next(Current_Binding);
            end loop;
          end;
          Impulses.Next(Current_Impulse);
        end loop;
        Current_Impulse := Impulses.First;
        while Impulses.Has_Element(Current_Impulse) loop
          Previous_Impulse := Current_Impulse;
          if Impulses.Element(Previous_Impulse).Is_Garbage then Impulses.Delete(Previous_Impulse); end if;
          Impulses.Next(Current_Impulse);
        end loop;
        delay DURATION_TO_WAIT_BEFORE_POLLING - (Clock - Last_Time);
      end loop;
      Devices.Clear;
    end Run_Update;
  function Has_ELement(Identifier : in Integer_Address) return Boolean is
    begin
      return Devices.Has_Element(Identifier);
    end Has_Element;
  function Get_Players return Map_Players.Unsafe.Map is
    begin
      return Players.Get;
    end Get_Players;
  procedure Handle(Input : in String_2) is
    begin
      null;
    end Handle;
  procedure Set_Vibration(Player : in Integer_4_Positive := 1; Frequency_High, Frequency_Low : in Float_4_Percent) is
    Current_Device : Map_Devices.Cursor := Devices.First;
    begin
      while Devices.Has_Element(Current_Device) loop
        if Devices.Element(Current_Device).Player = Player then Implementation.Set_Vibration(Devices.Key(Current_Device), Frequency_High, Frequency_Low); end if;
        Devices.Next(Current_Device);
      end loop;
    end Set_Vibration;
  procedure Set_Device(Player : in Integer_4_Positive := 1; Identifier : in Integer_Address) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Player := Player;
      Devices.Replace(Identifier, Device);
    end Set_Device;
  function Get_Devices return Map_Devices.Unsafe.Map is
    begin
      return Devices.Get;
    end Get_Devices;
  function Get_Device(Identifier : in Integer_Address) return Record_Device is
    begin
      return Devices.Element(Identifier);
    end Get_Device;
  procedure Handle_Stick(Identifier : in Integer_Address; Stick : in Enumerated_Stick; Location : in Record_Location) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Sticks(Stick) := Location;
      Devices.Replace(Identifier, Device);
    end Handle_Stick;
  procedure Handle_Generic_Stick(Identifier : in Integer_Address; Stick : in Integer_4_Positive; Location : in Record_Location) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Generic_Sticks.Replace_Element(Stick, Location);
      Devices.Replace(Identifier, Device);
    end Handle_Generic_Stick;
  procedure Handle_Trigger(Identifier : in Integer_Address; Trigger : in Enumerated_Trigger; Position : in Float_4_Percent) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Triggers(Trigger) := Position;
      Devices.Replace(Identifier, Device);
    end Handle_Trigger;
  procedure Handle_Generic_Trigger(Identifier : in Integer_Address; Trigger : in Integer_4_Positive; Position : in Float_4_Percent) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Generic_Triggers.Replace_Element(Trigger, Position);
      Devices.Replace(Identifier, Device);
    end Handle_Generic_Trigger;
  procedure Handle_Key(Identifier : in Integer_Address; Key : in Enumerated_Key; Is_Pressed : in Boolean) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      case Device.Kind is
        when Keyboard_Device    => if Device.Keyboard_Keys(Key).Is_Pressed    /= Is_Pressed then Device.Keyboard_Keys(Key)    := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Xbox_Device        => if Device.Xbox_Keys(Key).Is_Pressed        /= Is_Pressed then Device.Xbox_Keys(Key)        := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Playstation_Device => if Device.Playstation_Keys(Key).Is_Pressed /= Is_Pressed then Device.Playstation_Keys(Key) := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when Mouse_Device       => if Device.Mouse_Keys(Key).Is_Pressed       /= Is_Pressed then Device.Mouse_Keys(Key)       := (Is_Pressed, Clock); Devices.Replace(Identifier, Device); end if;
        when others             => raise Constraint_Error;
      end case;
    end Handle_Key;
  procedure Handle_Generic_Key(Identifier : in Integer_Address; Key : in Integer_4_Positive; Is_Pressed : in Boolean) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Generic_Keys.Replace_Element(Key, (Is_Pressed, Clock));
      Devices.Replace(Identifier, Device);
    end Handle_Generic_Key;
  procedure Handle_Mouse(Identifier : in Integer_Address; Location : in Record_Location) is
    Device : Record_Device := Devices.Element(Identifier);
    begin
      Device.Mouse_Location := Location;
      Devices.Replace(Identifier, Device);
    end Handle_Mouse;
  procedure Remove_Device(Identifier : in Integer_Address) is
    begin
      Devices.Delete(Identifier);
    end Remove_Device;
  procedure Add_Device(Identifier : in Integer_Address; Device : in Record_Device) is
    begin
      if not Players.Has_Element(Device.Player) then Players.Insert(Device.Player, (others => <>)); end if;
      Devices.Insert(Identifier, Device);
    end Add_Device;
begin
  Main_Task.Initialize;
end Neo.System.Input;
