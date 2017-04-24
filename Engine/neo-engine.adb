
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Neo.Engine is

  --------------
  -- Separate --
  --------------

  -- System-dependant subprograms. Note: Some may have side effects like setting of cvars or calling a private subprogram !!!
  package System is

      -- Vulkan
      procedure Initialize_Vulkan_Library;
      procedure Finalize_Vulkan_Library;
      function Create_Vulkan_Surface (Instance : Ptr) return Ptr;
      function Get_Vulkan_Subprogram (Name : Str)     return Ptr;
      function Get_Vulkan_Extensions                  return Str_Unbound;

      -- Information
      procedure Copy           (Item : Str);
      function Paste           return Str;
      function Get_Information return Information_State;

      -- Input
      procedure Initialize_Input;
      procedure Finalize_Input;
      procedure Vibrate     (Id : Int_Ptr; Hz_High, Hz_Low : Percent);
      function Update_Input return Bool;

      -- Error Handling
      procedure Run_Console;
      procedure Alert        (Val : Bool);
      procedure Open_Text    (Path : Str);
      procedure Open_Webpage (Path : Str);
      procedure Execute      (Path : Str);
      function Last_Error    return Int_32_Unsigned;
      function Ok            (Name, Message : Str; Buttons : Buttons_Kind; Icon : Icon_Kind)
                             return Bool;

      -- Windowing
      procedure Initialize_Multi_Monitor;
      procedure Finalize_Multi_Monitor;
      procedure Initialize_Windowing;
      procedure Finalize_Windowing;
      procedure Minimize;
      procedure Maximize;
      procedure Make_Windowed;
      procedure Clip_Cursor      (Do_Clip : Bool := True);
      procedure Hide_Cursor      (Do_Hide : Bool := True);
      procedure Set_Cursor_Style (Kind : Cursor_Kind);
      procedure Set_Cursor       (Pos  : Cursor_State);
      function Get_Cursor        return Cursor_state;
      function Get_Windows       return Vector_Border.Unsafe_Array;
      function Get_Decoration    return Border_State;
      function Fullscreen_Only   return Bool;
      function Only_Instance     return Bool;
      function Update_Windowing  return Bool; -- Set Activated and Mode cvars
    end; use System;

  -- Rendering is reactive to global data types, the visible subprograms here are for the main window (e.g. backend)
  package Renderer is
      procedure Initialize;
      procedure Present;
      procedure Finalize;
    end;

  -----------------
  -- Information --
  -----------------

  function Get_Information return Information_State renames System.Get_Information;
  function Paste           return Str               renames System.Paste;
  procedure Copy           (Item : Str)             renames System.Copy;

  ----------------
  -- Networking --
  ----------------

  procedure Silence  (Connection : in out Connection_State) is begin null; end;
  procedure Vocalize (Connection : in out Connection_State) is begin null; end;
  procedure Connect  (Connection : in out Connection_State; Address   : Str) is begin null; end;
  procedure Send     (Connection :        Connection_State; Recipient : Str; Data : Stream_Element_Array) is begin null; end;
  function Recieve   (Connection :        Connection_State;
                      Sender     :    out Str_16_Unbound;
                      Timeout    :        Duration := 0.0)  return Array_Stream is ((0, 0));
  function Get_Stats (Connection :        Connection_State) return Connection_Info_State is ((others => <>)); 
  function IP                                               return Str is ("");

  -------------------
  -- Error Handing --
  -------------------

  -- Display a message-box prompt, if this fails we have serious problems
  function Ok (Name, Message : Str; Buttons : Buttons_Kind := Okay_Button; Icon : Icon_Kind := No_Icon) return Bool renames System.Ok;

  -- Fetch imported system error codes
  function Last_Error return Str is ("System error: " & To_Str (Trim (System.Last_Error'Img, Both)));

  -- Subprograms used during alerts. They are non-vital so don't propagate errors
  Alert_Status : Safe_Status;
  function Alerting return Bool is (Alert_Status.Occupied);
  procedure Alert (Val : Bool := True) is
    begin
      Alert_Status.Occupied (Val);
      System.Alert (Val);
    end;

  -----------
  -- Tasks --
  -----------

  package Task_Count is new CVar ("taskcount", "Number of running tasks", Positive, 1, Settable => False);
  package body Tasks is
      procedure Finalize is new Unchecked_Deallocation (Task_Unsafe, Task_Unsafe_Ptr);
      task body Task_Unsafe is
        begin
          accept Initialize (Id : out Task_Id) do Id := Current_Task; end;
          begin Run; exception when Occurrence: others => Handle (Occurrence); end;
          Task_Count.Set (Task_Count.Get - 1);
        end;
      protected body Safe_Task is
          procedure Initialize is
            begin
              if Current_Id /= NULL_TASK_ID and then not Is_Terminated (Current_id) then return; end if;
              Current_Task := new Task_Unsafe;

-- warning: potentially blocking operation in protected operation
pragma Warnings (Off); 
              Current_Task.Initialize (Current_Id);
              Task_Count.Set (Task_Count.Get + 1);
            end;
          procedure Finalize is
            begin
              if Current_Id = NULL_TASK_ID or else Is_Terminated (Current_id) then return; end if;
              Abort_Task (Current_Id);
pragma Warnings (On);

              Current_Id := NULL_TASK_ID;
              Finalize (Current_Task);
              Task_Count.Set (Task_Count.Get - 1);
            end;
          function Running return Bool is
            begin
              return Current_Task /= null and not Is_Terminated (Current_Id);
            exception when others => return False; end;
        end;
    end;

  -------------
  -- Console --
  -------------

  -- Colors used in the console GUI
  COLOR_BACKGROUND : Color_State := COLOR_BLACK;
  COLOR_FOREGROUND : Color_State := COLOR_CRIMSON;

  -- URL to go to when "sending" a log
  ERROR_REPORTING_URL : Str := "www.google.com";

  -- Console task
  package Console_Tasks is new Tasks (Run_Console);
  Console_Task : Console_Tasks.Safe_Task;

  -- External access to console task
  function Running_Console return Bool is (Console_Task.Running);
  procedure Initialize_Console is begin Console_Task.Initialize; delay 0.2; end; -- We delay to wait for console window to appear
  procedure Finalize_Console is begin Console_Task.Finalize; end;

  -- Console button actions
  procedure Send_Log is begin Open_Webpage (ERROR_REPORTING_URL); end;
  procedure Save_Log is
    use Ada.Streams.Stream_IO;
    Path        : Str_8 := To_Str_8 (To_Str (Get_Information.Username) & Date_Str & ".txt");
    File        : Ada.Streams.Stream_IO.File_Type;
    File_Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin
      Ada.Streams.Stream_IO.Create (File, Out_File, To_Str_8 (To_Str (Get_Information.Path) & "/" & PATH_LOGS & "/") & Path);
      File_Stream := Ada.Streams.Stream_IO.Stream (File);
      for Element of Log loop Char_16'Write (File_Stream, Element); end loop;
      Ada.Streams.Stream_IO.Close (File);
      Open_Text (To_Str (Get_Information.Path) & "/" & PATH_LOGS & "/" & To_Str (Path));
    end;

  ---------------
  -- Windowing --
  ---------------

  -- Renames for fetching system and game window properties
  function Get_Windows return Vector_Border.Unsafe_Array renames System.Get_Windows;

  -- Resize window respecting minimum narrow and wide aspect ratios
  MINIMUM_FACTOR : constant Int_64_Positive := 256;
  type Resize_Kind is (Left_Resize,         Right_Resize,       Top_Right_Resize, Top_Left_Resize, Top_Resize, 
                       Bottom_Right_Resize, Bottom_Left_Resize, Bottom_Resize,    Other_Resize);
  function Resize (Kind : Resize_Kind; Border : Border_State) return Border_State is
    Result         : Border_State := Border;
    Decoration     : Border_State := Get_Decoration;
    Extra_Width    : Int_64 := Decoration.Right  + Decoration.Left;
    Extra_Height   : Int_64 := Decoration.Bottom + Decoration.Top;
    Current_Width  : Int_64 := (if Border.Right  - Border.Left - Extra_Width  < MINIMUM_FACTOR then MINIMUM_FACTOR else Border.Right - Border.Left - Extra_Width);
    Current_Height : Int_64 := (if Border.Bottom - Border.Top  - Extra_Height < MINIMUM_FACTOR * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get then
                               MINIMUM_FACTOR * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get else Border.Bottom - Border.Top - Extra_Height);
    Maximum_Width  : Int_64 := Current_Height * Aspect_Narrow_X.Get / Aspect_Narrow_Y.Get;
    Maximum_Height : Int_64 := Current_Width  * Aspect_Wide_Y.Get   / Aspect_Wide_X.Get;
    Minimum_Width  : Int_64 := Current_Height * Aspect_Wide_X.Get   / Aspect_Wide_Y.Get;
    Minimum_Height : Int_64 := Current_Width  * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get;
    Fit_Width      : Int_64 := (if Current_Width  > Maximum_Width  then Maximum_Width  elsif Current_Width  < Minimum_Width  then Minimum_Width  else Current_Width);
    Fit_Height     : Int_64 := (if Current_Height > Maximum_Height then Maximum_Height elsif Current_Height < Minimum_Height then Minimum_Height else Current_Height);
    Resize_Factor  : Int_64 := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) / 2;
    Resize_Extra   : Int_64 := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) rem 2;
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
      Windowed_Width.Set  (Result.Right  - Result.Left - Extra_Width);
      Windowed_Height.Set (Result.Bottom - Result.Top  - Extra_Height);
      return Result;
    end;

  -----------
  -- Input --
  -----------

  -- Input polling statuses
  Input_Status  : Safe_Status;
  Cursor_Status : Safe_Status;

  -- Delay constant for input polling
  DURATION_BEFORE_POLLING : constant Duration := 0.005; -- Suitable input response range is 0.05 (poor) to 0.005 (excellent)

  -- Global mouse cursor operations
  procedure Set_Cursor (Pos : Cursor_State) renames System.Set_Cursor;
  function Get_Cursor return Cursor_state renames System.Get_Cursor;
  function Get_Cursor_Normalized return Cursor_State is
    Main_Window : Border_State := Get_Windows (1);
    Cursor      : Cursor_State := Get_Cursor;
    begin
      return (Cursor.X - Main_Window.Left, Cursor.Y - Main_Window.Top); 
    end;

  -- Main game window operations
  function In_Main_Window (Cursor : Cursor_State := Get_Cursor) return Bool is
    Decoration  : Border_State := (if Mode.Get = Windowed_Mode then Get_Decoration else (others => 0));
    Main_Window : Border_State := Get_Windows (1);
    begin
      return Cursor.X >= Main_Window.Left + Decoration.Left and Cursor.X <= Main_Window.Right  - Decoration.Right and
             Cursor.Y >= Main_Window.Top  + Decoration.Top  and Cursor.Y <= Main_Window.Bottom - Decoration.Bottom; 
    end;
  function Main_Window_Center return Cursor_State is
    Main_Window : Border_State := Get_Windows (1);
    begin
      return (X => (Main_Window.Left + ((Main_Window.Right  - Main_Window.Left) / 2)),
              Y => (Main_Window.Top  + ((Main_Window.Bottom - Main_Window.Top)  / 2)));
    end;

  -- Internal state for storing registered impules
  type Impulse_State is record
      Callback : not null access procedure (Args : Vector_Impulse_Arg.Unsafe_Array);
      Bindings : not null access Vector_Binding.Safe_Vector;
      Enabled  : Boolean := True;
    end record;
  package Hashed_Impulse is new Hashed (Impulse_State);
  Impulses : Hashed_Impulse.Safe_Map;
  package body Impulse is
      Duplicate : Exception;

      -- Rename the impulse callback to make it passable to outter scopes...
      procedure Informal_Callback (Args : Vector_Impulse_Arg.Unsafe_Array) renames Callback;

      -- Constructor overhead...
      type Control_State is new Controlled with null record;
      procedure Finalize   (Control : in out Control_State);
      procedure Initialize (Control : in out Control_State);
      procedure Finalize   (Control : in out Control_State) is begin Impulses.Delete (Name); end;
      procedure Initialize (Control : in out Control_State) is
        begin 
          if Impulses.Has (Name) then raise Duplicate; end if;
          Impulses.Insert (Name, (Callback => Informal_Callback'Unrestricted_Access,
                                  Bindings => Bindings'Unrestricted_Access,
                                  Enabled  => True));
        end;
      Control : Control_State;

      -- Enable or disable triggering
      procedure Enable  is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := True;  Impulses.Replace (Name, Impulse); end;
      procedure Disable is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := False; Impulses.Replace (Name, Impulse); end;
    end;

  -- Each player's global state, it is a combination of all devices owned by that player
  type Player_State is record
      Triggers : Trigger_Array;
      Gamepad  : Gamepad_Array;
      Sticks   : Stick_Array;
      Keys     : Key_Array;
      Text     : Str_16_Unbound;
      Mouse    : Mouse_Array;
      Cursor   : Cursor_State;
    end record;
  package Ordered_Player is new Ordered (Positive, Player_State);
  Players : Ordered_Player.Safe_Map;

  -- Device operations
  Devices : aliased Ordered_Device.Safe_Map;
  function Get_Devices                   return Ordered_Device.Unsafe.Map is (Devices.Get);
  function Get_Device     (Id : Int_Ptr) return Device_State              is (Devices.Get (Id));
  function Has_Device     (Id : Int_Ptr) return Bool                      is (Devices.Has (Id));
  procedure Remove_Device (Id : Int_Ptr)                                  is begin Devices.Delete (Id); end;
  procedure Add_Device    (Id : Int_Ptr; Device  : Device_State)          is begin if not Players.Has (Device.Player) then Players.Insert (Device.Player, (others => <>)); end if; Devices.Insert (Id, Device); end;

  -- Convenience functions
  procedure Set_Device     (Id : Int_Ptr; Player  : Positive := 1)                     is Device : Device_State := Devices.Get (Id); begin Device.Player             := Player; Devices.Replace (Id, Device); end;
  procedure Inject_Text    (Id : Int_Ptr; Text    : Str_16_Unbound)                    is Device : Device_State := Devices.Get (Id); begin Device.Text               := Text;   Devices.Replace (Id, Device); end;
  procedure Inject_Cursor  (Id : Int_Ptr; Cursor  : Cursor_State)                      is Device : Device_State := Devices.Get (Id); begin Device.Cursor             := Cursor; Devices.Replace (Id, Device); end;
  procedure Inject_Trigger (Id : Int_Ptr; Trigger : Trigger_Kind; Press : Percent)     is Device : Device_State := Devices.Get (Id); begin Device.Triggers (Trigger) := Press;  Devices.Replace (Id, Device); end;
  procedure Inject_Stick   (Id : Int_Ptr; Stick   : Stick_Kind;   State : Stick_State) is Device : Device_State := Devices.Get (Id); begin Device.Sticks   (Stick)   := State;  Devices.Replace (Id, Device); end;
  procedure Inject_Button  (Id : Int_Ptr; Button  : Mouse_Kind;   Down : Bool)         is Device : Device_State := Devices.Get (Id); begin if Device.Mouse   (Button).Down /= Down then Device.Mouse   (Button) := (Down, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Button  (Id : Int_Ptr; Button  : Gamepad_Kind; Down : Bool)         is Device : Device_State := Devices.Get (Id); begin if Device.Gamepad (Button).Down /= Down then Device.Gamepad (Button) := (Down, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Key     (Id : Int_Ptr; Key     : Key_Kind;     Down : Bool)         is Device : Device_State := Devices.Get (Id); begin if Device.Keys    (Key).Down    /= Down then Device.Keys    (Key)    := (Down, Clock); Devices.Replace (Id, Device); end if; end;

  -- Hack for cases where the system does not support threaded input, the notion of "devices" is ignored
  procedure Inject_Into_Player_1 (Arg : Impulse_Arg_State) is 
    Arg2 : Impulse_Arg_State := Arg;
    begin
      for Impulse of Impulses.Get loop
        for Binding of Impulse.Bindings.Get loop
          if Binding.Player = 1 and Binding.Kind = Arg.Kind then
            Arg2.Binding := Binding;
            Impulse.Callback ((1 => Arg2));
          end if;
        end loop;
      end loop;
    end;

  -- Vibrate a specific player's Xbox controllers
  procedure Vibrate (Hz_High, Hz_Low : Percent; Player : Positive := 1) is
    begin
      for Device in Devices.Get.Iterate loop
        if Ordered_Device.Unsafe.Element (Device).Player = Player then
          Vibrate (Ordered_Device.Unsafe.Key (Device), Hz_High, Hz_Low);
        end if;
      end loop;
    end;

  -- Main input loop
  procedure Run_Input is

    -- Setters for descriminate unions
    function Changed (Binding : Binding_State; Player, Old_Player : Player_State) return Bool is
      begin 
        return ((Binding.Kind = Trigger_Impulse and then Player.Triggers (Binding.Trigger)      /= Old_Player.Triggers (Binding.Trigger))      or
                (Binding.Kind = Stick_Impulse   and then Player.Sticks   (Binding.Stick)        /= Old_Player.Sticks   (Binding.Stick))        or
                (Binding.Kind = Gamepad_Impulse and then Player.Gamepad  (Binding.Gamepad).Down /= Old_Player.Gamepad  (Binding.Gamepad).Down) or
                (Binding.Kind = Mouse_Impulse   and then Player.Mouse    (Binding.Mouse).Down   /= Old_Player.Mouse    (Binding.Mouse).Down)   or
                (Binding.Kind = Key_Impulse     and then Player.Keys     (Binding.Key).Down     /= Old_Player.Keys     (Binding.Key).Down)     or
                (Binding.Kind = Cursor_Impulse  and then Player.Cursor                          /= Old_Player.Cursor)                          or
                (Binding.Kind = Text_Impulse    and then Player.Text                            /= NULL_STR_16_UNBOUND));
      end;
    function Build_Impulse_Arg (Binding : Binding_State; Player : Player_State) return Impulse_Arg_State is
      begin
        return (case Binding.Kind is
                 when Mouse_Impulse   => (Mouse_Impulse,   Binding, Player.Mouse    (Binding.Mouse)),
                 when Key_Impulse     => (Key_Impulse,     Binding, Player.Keys     (Binding.Key)),
                 when Gamepad_Impulse => (Gamepad_Impulse, Binding, Player.Gamepad  (Binding.Gamepad)),
                 when Stick_Impulse   => (Stick_Impulse,   Binding, Player.Sticks   (Binding.Stick)),
                 when Trigger_Impulse => (Trigger_Impulse, Binding, Player.Triggers (Binding.Trigger)),
                 when Cursor_Impulse  => (Cursor_Impulse,  Binding, Player.Cursor),
                 when Text_Impulse    => (Text_Impulse,    Binding, Player.Text));
      end;

    -- Register combined keys
    procedure Build_Key_Down (Key : Key_Kind; Device : Device_State; Player : in out Player_State) is
      begin
        Player.Keys (Key) := Device.Keys (Key);
        case Key is
          when Left_Shift_Key | Right_Shift_Key => if Player.Keys (Left_Shift_Key) = Player.Keys (Right_Shift_Key) then Player.Keys (Shift_Key) := Player.Keys (Key); end if;
          when Left_Ctrl_Key  | Right_Ctrl_Key  => if Player.Keys (Left_Ctrl_Key)  = Player.Keys (Right_Ctrl_Key)  then Player.Keys (Ctrl_Key)  := Player.Keys (Key); end if;
          when Left_Alt_Key   | Right_Alt_Key   => if Player.Keys (Left_Alt_Key)   = Player.Keys (Right_Alt_Key)   then Player.Keys (Alt_Key)   := Player.Keys (Key); end if;
        when others => null; end case;
      end;

    -- Declare locals and initialize
    Old_Players,
    New_Players    : Ordered_Player.Unsafe.Map;
    Player         : Player_State;
    Devices_Unsafe : Ordered_Device.Unsafe.Map;
    Args           : Vector_Impulse_Arg.Unsafe.Vector;
    Last_Time      : Time := Clock;
    begin
      Initialize_Input;
      while Update_Input loop

        -- Clear all of the players
        Old_Players := Players.Get;
        New_Players := Players.Get;
        for Player of New_Players loop Player := (others => <>); end loop;
        Players.Set (New_Players);

        -- Loop through all of the devices and rebuild a the players for each input frame
        Devices_Unsafe := Devices.Get;
        for Device of Devices_Unsafe loop
          Player := Players.Get (Device.Player);
          case Device.Kind is

            -- Combine keyboard keys and text
            when Keyboard_Device =>
              if Device.Text /= NULL_STR_16_UNBOUND then Player.Text := Player.Text & Device.Text; end if;
              for Key in Key_Kind'Range loop
                if Device.Keys (Key).Down then
                  if Device.Keys (Key).Last < Player.Keys (Key).Last or Player.Keys (Key).Last = Get_Start_Time then
                    --Line ("Down! " & Key_Kind'Wide_Image (Key) & " " & Positive'Wide_Image (Device.Player));
                    Build_Key_Down (Key, Device, Player);
                  end if;
                elsif Device.Keys (Key).Last < Player.Keys (Key).Last then
                  --Line ("Up! " & Key_Kind'Wide_Image (Key) & " " & Positive'Wide_Image (Device.Player));
                  Build_Key_Down (Key, Device, Player);
                end if;
              end loop;

            -- Combine mice
            when Mouse_Device =>
              Player.Cursor := (Player.Cursor.X + Device.Cursor.X, Player.Cursor.Y + Device.Cursor.Y);
              for Button in Mouse_Kind'Range loop
                if Device.Mouse (Button).Down then
                  if Device.Mouse (Button).Last < Player.Mouse (Button).Last or Player.Mouse (Button).Last = Get_Start_Time then
                    Player.Mouse (Button) := Device.Mouse (Button);
                  end if;
                elsif Device.Mouse (Button).Last < Player.Mouse (Button).Last then
                  Player.Mouse (Button) := Device.Mouse (Button);
                end if;

                -- The scroll wheel is (somewhat incorrectly) seen a button, as such it will never recieve an up, so force it
                if Device.Mouse (Button).Down and Button in Wheel_Left_Button..Wheel_Down_Button then
                  Device.Mouse (Button).Down := False;
                end if;
              end loop;

            -- Combine gamepads
            when Gamepad_Device =>
              for Button in Gamepad_Kind'Range loop
                if Device.Gamepad (Button).Down then
                  if Device.Gamepad (Button).Last < Player.Gamepad (Button).Last or Player.Gamepad (Button).Last = Get_Start_Time then
                    Player.Gamepad (Button) := Device.Gamepad (Button);
                  end if;
                elsif Device.Gamepad (Button).Last < Player.Gamepad (Button).Last then
                  Player.Gamepad (Button) := Device.Gamepad (Button);
                end if;
              end loop;

              -- Range checks need to be made for sticks and triggers
              for Side in Stick_Kind'Range loop
                Player.Sticks (Side).X := (if Player.Sticks (Side).X + Device.Sticks (Side).X > Real_Range'Last then Real_Range'Last
                                           else Player.Sticks (Side).X + Device.Sticks (Side).X); 
                Player.Sticks (Side).Y := (if Player.Sticks (Side).Y + Device.Sticks (Side).Y > Real_Range'Last then Real_Range'Last
                                           else Player.Sticks (Side).Y + Device.Sticks (Side).Y); 
              end loop;
              for Side in Trigger_Kind'Range loop
                Player.Triggers (Side) := (if Player.Triggers (Side) + Device.Triggers (Side) > Real_Percent'Last then Real_Percent'Last
                                           else Player.Triggers (Side) + Device.Triggers (Side));
              end loop;
          end case;
          Players.Replace (Device.Player, Player);
        end loop;
        Devices.Set (Devices_Unsafe);

        -- Trigger the impulses based on each player's state
        if Input_Status.Occupied then
          for Impulse of Impulses.Get loop
            for Binding of Impulse.Bindings.Get loop
              if Changed (Binding, Players.Get (Binding.Player), Old_Players.Element (Binding.Player)) then
                Args.Clear; -- TODO do not register left or right clicks or cursor movement if the cursor is not In_Main_Window when in Windowed_Mode
                Args.Append (Build_Impulse_Arg (Binding, Players.Get (Binding.Player)));

                -- Handle combinations
                if Binding.Combo /= NO_COMBO then -- Failed combos get repeated, but its OK for now
                  for Other_Binding of Impulse.Bindings.Get loop
                    if Other_Binding.Combo = Binding.Combo then
                      if not Changed (Binding, Players.Get (Other_Binding.Player), Old_Players.Element (Other_Binding.Player)) then

-- A binding in the combo is not active
goto Combo_Not_Activated;
                      end if;
                      Args.Append (Build_Impulse_Arg (Other_Binding, Players.Get (Other_Binding.Player)));
                    end if;
                  end loop;
                end if;
                Impulse.Callback (Vector_Impulse_Arg.To_Unsafe_Array (Args));
              end if;

-- Skip further tests for combo activation
<<Combo_Not_Activated>>
            end loop;
          end loop;

          -- Center cursor
          if Cursor_Status.Occupied then Set_Cursor (Main_Window_Center); end if;
        end if;

        -- Delay the main loop if you have some spare time
        delay DURATION_BEFORE_POLLING - (Clock - Last_Time); Last_Time := Clock;
      end loop;
      Finalize_Input;
    end;

  -- Input task creation
  package Input_Tasks is new Tasks (Run_Input);
  Input_Task : Input_Tasks.Safe_Task;

  --------------
  -- Impulses --
  --------------

  -- There has to be a better way.. but it's OK for
  Game_Entry_Check_Status : Safe_Status;

  -- Enter or exit menu mode 
  procedure Callback_Enter_Game (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and then In_Main_Window then
        if Menu.Get then Menu.Set (False);
        else Game_Entry_Check_Status.Occupied (True); end if;
      end if;
    end;
  procedure Callback_Exit_To_Menu (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then Menu.Set (True); end if;
    end;

  -- Toggle fullscreen mode
  procedure Callback_Fullscreen (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then
        Mode.Set ((case Mode.Get is
                     when Multi_Monitor_Mode | Fullscreen_Mode => Windowed_Mode,
                     when Windowed_Mode => Fullscreen_Mode));
      end if;
    end;

  -- Impulse declarions
  package Fullscreen   is new Impulse ("togglemode", Callback_Fullscreen);
  package Enter_Game   is new Impulse ("entergame",  Callback_Enter_Game);
  package Exit_To_Menu is new Impulse ("exittomenu", Callback_Exit_To_Menu);

  --------------
  -- Commands --
  --------------

  -- Add or remove impule bindings
  procedure Callback_Unbind (Args : Array_Str_16_Unbound) is
    begin
      null;
    end;
  procedure Callback_Bind (Args : Array_Str_16_Unbound) is
    begin
      null;
    end;
  function Save_Binds return Str is (NULL_STR);

  -- Command declarations
  package Bind is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Callback_Bind,
                               Save     => Save_Binds'Access);

  -----------
  -- CVars --
  -----------

  -- Internal cvars
  type Activated_Kind is (Other_Activated, Click_Activated, Other_Deactivated, Minimize_Deactivated);
  package Activated is new CVar ("activated", "Query last activation action for window", Activated_Kind, Other_Activated, False);

  ----------
  -- Menu --
  ----------

  

  --------------
  -- Separate --
  --------------

  -- Game task creation
  procedure Game is separate;
  package Game_Tasks is new Tasks (Game);
  Game_Task : Game_Tasks.Safe_Task;

  -- Give the System and Renderer packages access to everything in Neo.Engine's body
  package body System is separate;
  package body Renderer is separate;

  ---------
  -- Run --
  ---------

  procedure Run is

    -- Set the windowing mode based on the cvar Mode
    procedure Set_Windowing_Mode is
      begin
        case Mode.Get is
          when Multi_Monitor_Mode => Initialize_Multi_Monitor;
          when Windowed_Mode      => Make_Windowed;           
          when Fullscreen_Mode    => Maximize;                              
        end case;
        Clip_Cursor (Mode.Get = Fullscreen_Mode);
      end;
    begin
      Use_Ada_Put;

      -- Make sure this is the only game instance running and launch console by default
      if not Only_Instance then return; end if;
      Initialize_Console; -- Always show the separate console... for now

      -- Initialize
      Initialize_Windowing;
      Set_Windowing_Mode;
      Renderer.Initialize;
      Input_Task.Initialize;
      Game_Task.Initialize;

      -- Set system-level bindings
      Enter_Game.Bindings.Append   (Mouse (Left_Button));
      Exit_To_Menu.Bindings.Append (Keyboard (Escape_Key));
      Fullscreen.Bindings.Append   (Keyboard (F11_Key));

      -- Log system information in case of error reports
      Line ("Engine: "              & NAME_ID & " " & VERSION);
      Line ("OS: "                  & To_Str (Get_Information.OS));
      Line ("Username: "            & To_Str (Get_Information.Username));
      Line ("Directory: "           & To_Str (Get_Information.Path));
      Line ("Name: "                & To_Str (Get_Information.Name));
      Line ("Application bit size"  & To_Str (WORD_SIZE'Img));
      Line ("System bit size:"      & To_Str (Get_Information.Bit_Size'Img));

      -- Main loop
      declare
      Saved_Pos         : Cursor_State   := Get_Cursor;
      Current_Menu      : Bool           := Menu.Get;
      Current_Mode      : Mode_Kind      := Mode.Get;
      Current_Cursor    : Cursor_Kind    := Cursor.Get;
      Current_Activated : Activated_Kind := Activated.Get;
      begin

        -- Setup
        Enter_Game.Enable;
        Input_Status.Occupied (True);
        while Update_Windowing and Game_Task.Running loop

          -- Set cursor
          if Current_Cursor /= Cursor.Get then
            if Menu.Get then Set_Cursor_Style (Cursor.Get); end if;
            Current_Cursor := Cursor.Get;
          end if;

          -- Handle mode switching
          if Current_Mode /= Mode.Get then
            if Current_Mode = Multi_Monitor_Mode then Finalize_Multi_Monitor; end if;
            Set_Windowing_Mode;
            if Mode.Get /= Multi_Monitor_Mode then -- Decide to set or save cursor position
              if not In_Main_Window then Set_Cursor (Main_Window_Center); end if;
              if not In_Main_Window (Saved_Pos) then Saved_Pos := Main_Window_Center; end if;
            end if;
            Current_Mode := Mode.Get;
          end if;

          -- Handle menu entry
          if Current_Menu /= Menu.Get or Game_Entry_Check_Status.Occupied then
            Game_Entry_Check_Status.Occupied (False);
            if Menu.Get then
              Cursor_Status.Occupied (False);
              Exit_To_Menu.Disable;
              if Mode.Get /= Fullscreen_Mode then Clip_Cursor (False); end if;
              Set_Cursor_Style (Cursor.Get);
              Set_Cursor (Saved_Pos);
              Hide_Cursor (False);
              Enter_Game.Enable;
            else
              Saved_Pos := Get_Cursor;
              Enter_Game.Disable;
              Hide_Cursor;
              Clip_Cursor;
              Cursor_Status.Occupied (True);
              Exit_To_Menu.Enable;
            end if;
            Current_Menu := Menu.Get;
          end if;

          -- Activation is complex and branching can easily lead to problems, so handle cases explicitly
          if Current_Activated /= Activated.Get then
            case Activated.Get is
              when Other_Activated =>
                Input_Status.Occupied (True);
                case Mode.Get is
                  when Windowed_Mode =>
                    Input_Status.Occupied (True);
                    if not Menu.Get then
                      Cursor_Status.Occupied (False);
                      Exit_To_Menu.Disable;
                      Enter_Game.Enable;
                    else Set_Cursor_Style (Cursor.Get); end if;
                  when Multi_Monitor_Mode | Fullscreen_Mode =>
                    Maximize;
                    if Mode.Get = Multi_Monitor_Mode then Initialize_Multi_Monitor; end if;
                    if Menu.Get then Set_Cursor_Style (Cursor.Get);
                    elsif Mode.Get = Multi_Monitor_Mode then
                      Hide_Cursor;
                      Set_Cursor (Saved_Pos);
                      Cursor_Status.Occupied (True);
                    else
                      Hide_Cursor;
                      Clip_Cursor;
                      Set_Cursor (Saved_Pos);
                      Cursor_Status.Occupied (True);                      
                    end if;
                end case;
              when Click_Activated =>
                case Mode.Get is
                  when Windowed_Mode =>
                    Input_Status.Occupied (True);
                    if Menu.Get then Set_Cursor_Style (Cursor.Get);
                    elsif In_Main_Window then
                      Saved_Pos := Get_Cursor;
                      Hide_Cursor;
                      Clip_Cursor;
                      Cursor_Status.Occupied (True);
                    else -- This is a title-bar click in game mode - tricky, tricky...
                      Cursor_Status.Occupied (False);
                      Exit_To_Menu.Disable;
                      Enter_Game.Enable;
                    end if;
                when others => null; end case;
              when Other_Deactivated | Minimize_Deactivated =>
                if not Menu.Get and In_Main_Window then Set_Cursor (Saved_Pos); end if;
                Input_Status.Occupied (False);
                Clip_Cursor (False);
                Set_Cursor_Style (System_Cursor);
                Hide_Cursor (False);
                if Activated.Get = Other_Deactivated then
                  case Mode.Get is
                    when Multi_Monitor_Mode => Finalize_Multi_Monitor; Minimize;
                    when Fullscreen_Mode    => Minimize; 
                  when others => null; end case;
                elsif Mode.Get = Multi_Monitor_Mode then Finalize_Multi_Monitor; end if;
            end case;
            Current_Activated := Activated.Get;
          end if;

          -- Show the frame buffer
          Renderer.Present;
        end loop;
      end;

      -- Finalize
      Finalize_Windowing;
      Input_Task.Finalize;
      Renderer.Finalize;
      Game_Task.Finalize;

    -- Handle exceptions
    exception when Occurrence: others => Handle (Occurrence);
      if not Running_Console and then Ok (Icon    => Error_Icon,
                                          Name    => To_Str (Get_Information.Name),
                                          Message => Localize ("An error has occurred, would you like to view more information?"),
                                          Buttons => Yes_No_Buttons) then null; Initialize_Console; end if;
    end;
end;