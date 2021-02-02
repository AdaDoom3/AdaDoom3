
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Neo.World.CVars; use Neo.World.CVars;

package body Neo.Engine is
 
  procedure Resize (To : Border_State) is begin null; end;
  
  --------------
  -- Subunits --
  --------------
  
  -- Architecture dependant subprograms
  package CPU is
      
      -- CPU Info
      function Get_CPU return CPU_State;
        
      -- Stack checking
      procedure Put_Stack;
      procedure Clear_Stack;
      procedure Check_Exceptions;
      function Is_Stack_Empty return Boolean;
      
      -- Settings
      procedure Set_Rounding  (Val : Rounding_Kind);
      procedure Set_Precision (Val : Precision_Kind);
    end; 
    
  -- System dependant subprograms
  package System is

      -- Vulkan
      procedure Initialize_Vulkan_Library;
      procedure Finalize_Vulkan_Library;
      function Create_Vulkan_Surface (Instance : Ptr) return Ptr;
      function Get_Vulkan_Subprogram (Name     : Str) return Ptr;
      function Get_Vulkan_Extension return Ptr_Char_8_C;

      -- Information
      procedure Copy   (Item : Str);
      function Paste   return Str;
      function OS_Info return OS_Info_State;

      -- Input
      procedure Initialize_Input;
      procedure Finalize_Input;
      procedure Vibrate     (Id : Int_Ptr; Hz_High, Hz_Low : Real_Percent);
      function Update_Input return Bool;

      -- Error Handling
      procedure Run_Console;
      procedure Open_Text    (Path : Str);
      procedure Open_Webpage (Path : Str);
      procedure Execute      (Path : Str);
      function Last_Error    return Int_Unsigned;
      function Ok            (Message : Str; Buttons : Buttons_Kind; Icon : Icon_Kind)
                             return Bool;

      -- Windowing
      procedure Initialize_Multi_Monitor;
      procedure Finalize_Multi_Monitor;
      procedure Initialize_Windowing;
      procedure Finalize_Windowing;
      procedure Minimize;
      procedure Maximize;
      procedure Restore;
      procedure Clip_Cursor      (Do_Clip : Bool := True);
      procedure Hide_Cursor      (Do_Hide : Bool := True);
      procedure Set_Cursor_Style (Kind : Cursor_Kind);
      procedure Set_Cursor       (Pos  : Cursor_State);
      procedure Resize           (To   : Border_State);
      function Get_Cursor        return Cursor_state;
      function Get_Windows       return Vector_Border.Unsafe_Array;
      function Get_Decoration    return Border_State;
      function Fullscreen_Only   return Bool;
      function Only_Instance     return Bool;
      function Update_Windowing  return Bool; -- Set Activated and Mode cvars
    end;
  
  -----------------
  -- Information --
  -----------------

  function OS_Info return OS_Info_State renames System.OS_Info;
  
  ---------------
  -- Clipboard --
  ---------------
  
  function Paste return Str renames System.Paste;
  procedure Copy (Item : Str) renames System.Copy;
  
  ------------
  -- Vulkan --
  ------------

  procedure Initialize_Vulkan_Library                        renames System.Initialize_Vulkan_Library;
  procedure Finalize_Vulkan_Library                          renames System.Finalize_Vulkan_Library;
  function Create_Vulkan_Surface (Instance : Ptr) return Ptr renames System.Create_Vulkan_Surface;
  function Get_Vulkan_Subprogram (Name     : Str) return Ptr renames System.Get_Vulkan_Subprogram;
  function Get_Vulkan_Extension return Ptr_Char_8_C          renames System.Get_Vulkan_Extension;
      
  -----------
  -- Tasks --
  -----------

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
  
  -------------------
  -- Error Handing --
  -------------------

  -- Console task
  package Console_Tasks is new Tasks (System.Run_Console);
  Console_Task : Console_Tasks.Safe_Task;

  -- External access to console task
  function Running_Console return Bool is (Console_Task.Running);
  procedure Initialize_Console is begin Console_Task.Initialize; delay 0.2; end; -- We delay to wait for console window to appear
  procedure Finalize_Console is begin Console_Task.Finalize; end;

  -- Console button actions
  procedure Send_Log is begin System.Open_Webpage (CONSOLE_ERROR_REPORTING_URL); end;
  procedure Save_Log is
    use Ada.Streams.Stream_IO;
    Path        : Str_8 := To_Str_8 (S (OS_Info.Username) & ".txt");
    File        : Ada.Streams.Stream_IO.File_Type;
    File_Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin
      Ada.Streams.Stream_IO.Create (File, Out_File, To_Str_8 (S (OS_Info.App_Path) & S & PATH_LOGS & S) & Path);
      File_Stream := Ada.Streams.Stream_IO.Stream (File);
      for Element of Log loop Char'Write (File_Stream, Element); end loop;
      Ada.Streams.Stream_IO.Close (File);
      System.Open_Text (To_Str (OS_Info.App_Path) & S & PATH_LOGS & S & To_Str (Path));
    end;

  -- Display a message-box prompt, if this fails we have serious problems
  function Ok (Message : Str; Buttons : Buttons_Kind := Okay_Button; Icon : Icon_Kind := No_Icon) return Bool renames System.Ok;

  -- Called by all tasks (including the main execution) during an error to report exception and trace information
  procedure Handle (Occurrence : Exception_Occurrence) is
    begin
      Set_Exit_Status (Failure);
      Line (To_Str (Exception_Name    (Occurrence)));
      Line (To_Str (Exception_Message (Occurrence)));  
      Line ("System status:" & System.Last_Error'Wide_Image); 
    end;

  ---------
  -- CPU --
  ---------
   
  -- System renames
  procedure Put_Stack                            renames CPU.Put_Stack;
  procedure Clear_Stack                          renames CPU.Clear_Stack;
  procedure Check_Exceptions                     renames CPU.Check_Exceptions;
  procedure Set_Rounding  (Val : Rounding_Kind)  renames CPU.Set_Rounding;
  procedure Set_Precision (Val : Precision_Kind) renames CPU.Set_Precision;
  function Is_Stack_Empty return Bool            renames CPU.Is_Stack_Empty;   
  function Get_CPU        return CPU_State       renames CPU.Get_CPU;
  
  -- Return a comma-separated string with all of the CPU extensions listed
  function Get_Extensions_Image (CPU : CPU_State) return Str is
  
    -- Result state and helper function to make it all pretty
    Result : Str_Unbound := NULL_STR_UNBOUND;
    Count  : Natural     := 0;
    procedure Put_Into_Result (Item : Str) is
      begin
        if Count /= 0 then Result := Result & ", "; end if;
        Result := Result & Item;
        Count  := Count + 1;
      end;
      
    -- Start of Get_Extensions_Image
    begin
      case CPU.Vendor is
        when Advanced_RISC_Machines_Vendor =>
          if CPU.Has_NEON                                       then Put_Into_Result ("NEON");      end if;
          if CPU.Has_Vector_Floating_Point                      then Put_Into_Result ("VFP");       end if;
        when Apple_IBM_Motorola_Vendor =>
          if CPU.Has_Vector_Multimedia_Instructions             then Put_Into_Result ("VMI");       end if;
          if CPU.Has_Vector_Scalar_Instructions                 then Put_Into_Result ("VSI");       end if;
          if CPU.Has_Altivec_Additional_Registers               then Put_Into_Result ("VMX128");    end if;
          if CPU.Has_Altivec                                    then Put_Into_Result ("Altivec");   end if;
        when Intel_Vendor | Advanced_Micro_Devices_Vendor =>
          if CPU.Has_Multi_Media_Extensions                     then Put_Into_Result ("MMX");       end if;
          if CPU.Has_Streaming_SIMD_Extensions_1                then Put_Into_Result ("SSE");       end if;
          if CPU.Has_Streaming_SIMD_Extensions_2                then Put_Into_Result ("SSE2");      end if;
          if CPU.Has_Streaming_SIMD_Extensions_3                then Put_Into_Result ("SSE3");      end if;
          if CPU.Has_Streaming_SIMD_Extensions_3_Supplement     then Put_Into_Result ("SSSE3");     end if;
          if CPU.Has_Streaming_SIMD_Extensions_4_1              then Put_Into_Result ("SSE4.1");    end if;
          if CPU.Has_Streaming_SIMD_Extensions_4_2              then Put_Into_Result ("SSE4.2");    end if;
          if CPU.Has_Carryless_Multiplication_Of_Two_64_Bit     then Put_Into_Result ("PCLMULQDQ"); end if;
          if CPU.Has_Advanced_Vector_Extensions_Enabled         then Put_Into_Result ("AVX");       end if;
          if CPU.Has_Advanced_Vector_Extensions_2               then Put_Into_Result ("AVX2");      end if;
          if CPU.Has_Advanced_Encryption_Service                then Put_Into_Result ("AES");       end if;
          if CPU.Has_Advanced_State_Operations                  then Put_Into_Result ("FXSR");      end if;
          if CPU.Has_Bit_Manipulation_Extensions_1              then Put_Into_Result ("BMI1");      end if;
          if CPU.Has_Bit_Manipulation_Extensions_2              then Put_Into_Result ("BMI2");      end if;
          if CPU.Has_Fused_Multiply_Add_3                       then Put_Into_Result ("FMA3");      end if;
          if CPU.Has_Fused_Multiply_Add_4                       then Put_Into_Result ("FMA4");      end if;
          if CPU.Has_Hyperthreading                             then Put_Into_Result ("HTT");       end if;
          if CPU.Has_High_Precision_Convert                     then Put_Into_Result ("CVT16");     end if;
          if CPU.Has_Half_Precision_Floating_Point_Convert      then Put_Into_Result ("F16C");      end if;
          if CPU.Has_Extended_States_Enabled                    then Put_Into_Result ("OSXSAVE");   end if;
          if CPU.Has_Population_Count                           then Put_Into_Result ("POPCNT");    end if;
          if CPU.Has_Context_ID_Manager                         then Put_Into_Result ("INVPCID");   end if;
          if CPU.Has_Conditional_Move                           then Put_Into_Result ("CMOV");      end if;
          if CPU.Has_Leading_Zero_Count                         then Put_Into_Result ("LZCNT");     end if;
          if CPU.Has_Extended_Operation_Support                 then Put_Into_Result ("XOP");       end if;
          case CPU.Vendor is
            when Advanced_Micro_Devices_Vendor =>
              if CPU.Has_Streaming_SIMD_Extensions_4_Supplement then Put_Into_Result ("SSE4a");     end if;
              if CPU.Has_3DNow                                  then Put_Into_Result ("3DNow!");    end if;
              if CPU.Has_3DNow_Supplement                       then Put_Into_Result ("3DNow!+");   end if;
              if CPU.Has_Multi_Media_Extensions_Supplement      then Put_Into_Result ("MMX+");      end if;
          when others => null; end case;
      when others => null; end case;
      
      return S (Result);
    end;
    
  ---------------
  -- Windowing --
  ---------------  
  
  -- System renames
  function Get_Windows      return Vector_Border.Unsafe_Array renames System.Get_Windows;
  function Only_Instance    return Bool                       renames System.Only_Instance;
  function Update_Windowing return Bool                       renames System.Update_Windowing;
  procedure Initialize_Windowing                              renames System.Initialize_Windowing;
  procedure Finalize_Windowing                                renames System.Finalize_Windowing;
  procedure Initialize_Multi_Monitor                          renames System.Initialize_Multi_Monitor;
  procedure Finalize_Multi_Monitor                            renames System.Finalize_Multi_Monitor;
  procedure Minimize                                          renames System.Minimize;
  procedure Maximize                                          renames System.Maximize;
  procedure Restore                                           renames System.Restore;

  -- Resize window respecting minimum narrow and wide aspect ratios
  MINIMUM_FACTOR : constant Int := 256;
  type Resize_Kind is (Left_Resize,         Right_Resize,       Top_Right_Resize, Top_Left_Resize, Top_Resize, 
                       Bottom_Right_Resize, Bottom_Left_Resize, Bottom_Resize,    Other_Resize);
  function Resize (Kind : Resize_Kind; Border : Border_State) return Border_State is
    Result         : Border_State := Border;
    Decoration     : Border_State := System.Get_Decoration;
    Extra_Width    : Int := Decoration.Right  + Decoration.Left;
    Extra_Height   : Int := Decoration.Bottom + Decoration.Top;
    Current_Width  : Int := (if Border.Right  - Border.Left - Extra_Width  < MINIMUM_FACTOR then MINIMUM_FACTOR else Border.Right - Border.Left - Extra_Width);
    Current_Height : Int := (if Border.Bottom - Border.Top  - Extra_Height < MINIMUM_FACTOR * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get then
                            MINIMUM_FACTOR * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get else Border.Bottom - Border.Top - Extra_Height);
    Maximum_Width  : Int := Current_Height * Aspect_Narrow_X.Get / Aspect_Narrow_Y.Get;
    Maximum_Height : Int := Current_Width  * Aspect_Wide_Y.Get   / Aspect_Wide_X.Get;
    Minimum_Width  : Int := Current_Height * Aspect_Wide_X.Get   / Aspect_Wide_Y.Get;
    Minimum_Height : Int := Current_Width  * Aspect_Narrow_Y.Get / Aspect_Narrow_X.Get;
    Fit_Width      : Int := (if Current_Width  > Maximum_Width  then Maximum_Width  elsif Current_Width  < Minimum_Width  then Minimum_Width  else Current_Width);
    Fit_Height     : Int := (if Current_Height > Maximum_Height then Maximum_Height elsif Current_Height < Minimum_Height then Minimum_Height else Current_Height);
    Resize_Factor  : Int := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) / 2;
    Resize_Extra   : Int := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) rem 2;
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
      Window_Width.Set  (Result.Right  - Result.Left - Extra_Width);
      Window_Height.Set (Result.Bottom - Result.Top  - Extra_Height);
      return Result;
    end;

  -----------
  -- Input --
  -----------
  
  -- System renames
  procedure Set_Cursor_Style (Kind    : Cursor_Kind)  renames System.Set_Cursor_Style;
  procedure Clip_Cursor      (Do_Clip : Bool := True) renames System.Clip_Cursor;
  procedure Hide_Cursor      (Do_Hide : Bool := True) renames System.Hide_Cursor;
  procedure Set_Cursor       (Pos : Cursor_State)     renames System.Set_Cursor;
  function Get_Cursor        return Cursor_state      renames System.Get_Cursor;
  
  -- Global mouse cursor operations
  function Get_Cursor_Normalized return Cursor_State is
    Main_Window : Border_State := Get_Windows (1);
    Cursor      : Cursor_State := Get_Cursor;
    begin
      return (Cursor.X - Main_Window.Left, Cursor.Y - Main_Window.Top); 
    end;

  -- Main game window operations
  function In_Main_Window (Pos : Cursor_State := Get_Cursor) return Bool is
    Main_Window : Border_State := Get_Windows (1);
    Decoration  : Border_State := (if Mode.Get = Windowed_Mode then System.Get_Decoration else (others => 0));
    begin
      return Pos.X >= Main_Window.Left + Decoration.Left and Pos.X <= Main_Window.Right  - Decoration.Right and
             Pos.Y >= Main_Window.Top  + Decoration.Top  and Pos.Y <= Main_Window.Bottom - Decoration.Bottom; 
    end;
  function Main_Window_Center return Cursor_State is
    Main_Window : Border_State := Get_Windows (1);
    begin
      return (X => (Main_Window.Left + ((Main_Window.Right  - Main_Window.Left) / 2)),
              Y => (Main_Window.Top  + ((Main_Window.Bottom - Main_Window.Top)  / 2)));
    end;
  
  package body Impulse is
      Duplicate : Exception;

      -- Rename the impulse callback to make it passable to outter scopes...
      procedure Informal_Callback (Args : Vector_Impulse_Arg.Unsafe_Array) is begin Callback (Args); end;

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
                                  Enabled  => True,
                                  Name     => U (Name),
                                  Rapid    => Rapid));
        end;
      Control : Control_State;

      -- Enable or disable triggering
      procedure Enable  is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := True;  Impulses.Replace (Name, Impulse); end;
      procedure Disable is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := False; Impulses.Replace (Name, Impulse); end;
    end;
    
  -- Each player's global state, it is a combination of all devices owned by that player
  type Player_State is record
      Triggers : Trigger_Array := (others => 0.0);
      Gamepad  : Gamepad_Array := (others => (others => <>));
      Sticks   : Stick_Array   := (others => (others => <>));
      Keys     : Key_Array     := (others => (others => <>));
      Text     : Str_Unbound   := NULL_STR_UNBOUND;
      Mouse    : Mouse_Array   := (others => (others => <>));
      Cursor   : Cursor_State  := (others => <>);
    end record;
  package Ordered_Player is new Neo.Core.Ordered (Positive, Player_State);
  Players : Ordered_Player.Safe_Map;

  -- Device operations
  Devices : aliased Ordered_Device.Safe_Map;
  function Get_Devices                   return Ordered_Device.Unsafe.Map is (Devices.Get);
  function Get_Device     (Id : Int_Ptr) return Device_State              is (Devices.Get (Id));
  function Has_Device     (Id : Int_Ptr) return Bool                      is (Devices.Has (Id));
  procedure Remove_Device (Id : Int_Ptr)                                  is begin Devices.Delete (Id); end;
  procedure Add_Device    (Id : Int_Ptr; Device  : Device_State)          is begin if not Players.Has (Device.Player) then Players.Insert (Device.Player, (others => <>)); end if; Devices.Insert (Id, Device); end;

  -- Convenience functions
  procedure Set_Device     (Id : Int_Ptr; Player  : Positive := 1)                      is Device : Device_State := Devices.Get (Id); begin Device.Player             := Player; Devices.Replace (Id, Device); end;
  procedure Inject_Text    (Id : Int_Ptr; Text    : Str_Unbound)                        is Device : Device_State := Devices.Get (Id); begin Device.Text               := Text;   Devices.Replace (Id, Device); end;
  procedure Inject_Cursor  (Id : Int_Ptr; Cursor  : Cursor_State)                       is Device : Device_State := Devices.Get (Id); begin Device.Cursor             := Cursor; Devices.Replace (Id, Device); end;
  procedure Inject_Trigger (Id : Int_Ptr; Trigger : Trigger_Kind; Press : Real_Percent) is Device : Device_State := Devices.Get (Id); begin Device.Triggers (Trigger) := Press;  Devices.Replace (Id, Device); end;
  procedure Inject_Stick   (Id : Int_Ptr; Stick   : Stick_Kind;   State : Stick_State)  is Device : Device_State := Devices.Get (Id); begin Device.Sticks   (Stick)   := State;  Devices.Replace (Id, Device); end;
  procedure Inject_Button  (Id : Int_Ptr; Button  : Mouse_Kind;   Down : Bool)          is Device : Device_State := Devices.Get (Id); begin if Device.Mouse   (Button).Down /= Down then Device.Mouse   (Button) := (Down, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Button  (Id : Int_Ptr; Button  : Gamepad_Kind; Down : Bool)          is Device : Device_State := Devices.Get (Id); begin if Device.Gamepad (Button).Down /= Down then Device.Gamepad (Button) := (Down, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Key     (Id : Int_Ptr; Key     : Key_Kind;     Down : Bool)          is Device : Device_State := Devices.Get (Id); begin if Device.Keys    (Key).Down    /= Down then Device.Keys    (Key)    := (Down, Clock); Devices.Replace (Id, Device); end if; end;

  -- Vibrate a specific player's Xbox controllers
  procedure Vibrate (Hz_High, Hz_Low : Real_Percent; Player : Positive := 1) is
    begin
      for I in Devices.Get.Iterate loop
        if Ordered_Device.Unsafe.Element (I).Player = Player then System.Vibrate (Ordered_Device.Unsafe.Key (I), Hz_High, Hz_Low); end if;
      end loop;
    end;

  -- Main input loop
  procedure Run_Input is

    -- Setters for descriminate unions
    function Changed (Rapid : Bool; Bind : Binding_State; Player, Old_Player : Player_State) return Bool is
      ((Bind.Kind = Trigger_Impulse and then (Rapid or else Player.Triggers (Bind.Trigger)      /= Old_Player.Triggers (Bind.Trigger)))      or
       (Bind.Kind = Stick_Impulse   and then (Rapid or else Player.Sticks   (Bind.Stick)        /= Old_Player.Sticks   (Bind.Stick)))        or
       (Bind.Kind = Gamepad_Impulse and then (Rapid or else Player.Gamepad  (Bind.Gamepad).Down /= Old_Player.Gamepad  (Bind.Gamepad).Down)) or
       (Bind.Kind = Mouse_Impulse   and then (Rapid or else Player.Mouse    (Bind.Mouse).Down   /= Old_Player.Mouse    (Bind.Mouse).Down))   or
       (Bind.Kind = Key_Impulse     and then (Rapid or else Player.Keys     (Bind.Key).Down     /= Old_Player.Keys     (Bind.Key).Down))     or
       (Bind.Kind = Cursor_Impulse  and then                Player.Cursor                       /= (0, 0))                                   or
       (Bind.Kind = Text_Impulse    and then                Player.Text                         /= NULL_STR_16_UNBOUND));

    function Build_Impulse_Arg (Binding : Binding_State; Player : Player_State) return Impulse_Arg_State is
      ((case Binding.Kind is
          when Mouse_Impulse   => (Mouse_Impulse,   Binding, Player.Mouse    (Binding.Mouse)),
          when Key_Impulse     => (Key_Impulse,     Binding, Player.Keys     (Binding.Key)),
          when Gamepad_Impulse => (Gamepad_Impulse, Binding, Player.Gamepad  (Binding.Gamepad)),
          when Stick_Impulse   => (Stick_Impulse,   Binding, Player.Sticks   (Binding.Stick)),
          when Trigger_Impulse => (Trigger_Impulse, Binding, Player.Triggers (Binding.Trigger)),
          when Cursor_Impulse  => (Cursor_Impulse,  Binding, Player.Cursor),
          when Text_Impulse    => (Text_Impulse,    Binding, Player.Text)));

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

    -- Indexes and temporaries
    Current_Devices : Ordered_Device.Unsafe.Map;
    Old_Players     : Ordered_Player.Unsafe.Map;
    Args            : Vector_Impulse_Arg.Unsafe.Vector;
    Player          : Player_State := (others => <>);
    
    -- Timing
    Last_Time : Time := Clock;
    
    -- Start of Run_Input
    begin
      System.Initialize_Input;
      while System.Update_Input loop
      
        -- Clear text and cursor data and store previous snapshot
        Old_Players := Players.Get;
        for New_Player of Old_Players loop
          New_Player.Text   := NULL_STR_16_UNBOUND;
          New_Player.Cursor := (0, 0);
        end loop;
        Players.Set (Old_Players);

        -- Loop through all of the devices and rebuild the players for each input frame
        Current_Devices := Devices.Get;
        for Device of Current_Devices loop
          Player := Players.Get (Device.Player);
          case Device.Kind is

            -- Combine keyboard keys and text
            when Keyboard_Device =>
              if Device.Text /= NULL_STR_UNBOUND then Player.Text := Player.Text & Device.Text; end if;
              for Key in Key_Kind'Range loop
                if Device.Keys (Key).Down and (not Player.Keys (Key).Down or Device.Keys (Key).Last < Player.Keys (Key).Last) then
                  Build_Key_Down (Key, Device, Player);
                elsif not Device.Keys (Key).Down and Device.Keys (Key).Last > Player.Keys (Key).Last then
                  Build_Key_Down (Key, Device, Player);
                end if;
              end loop;

            -- Combine mice
            when Mouse_Device =>
              Player.Cursor := (Player.Cursor.X + Device.Cursor.X, Player.Cursor.Y + Device.Cursor.Y);
              Device.Cursor := (0, 0);
              for Button in Mouse_Kind'Range loop
                if Device.Mouse (Button).Down and
                  (not Player.Mouse (Button).Down or Device.Mouse (Button).Last < Player.Mouse (Button).Last)
                then Player.Mouse (Button) := Device.Mouse (Button);
                elsif not Device.Mouse (Button).Down and Device.Mouse (Button).Last > Player.Mouse (Button).Last then
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
                if Device.Gamepad (Button).Down and
                  (not Player.Gamepad (Button).Down or Device.Gamepad (Button).Last < Player.Gamepad (Button).Last)
                then Player.Gamepad (Button) := Device.Gamepad (Button);
                elsif not Device.Gamepad (Button).Down and Device.Gamepad (Button).Last > Player.Gamepad (Button).Last then
                  Player.Gamepad (Button) := Device.Gamepad (Button);
                end if;
              end loop;

              -- Range checks need to be made for sticks and triggers
              for Side in Stick_Kind'Range loop
                Player.Sticks (Side).X := (if Player.Sticks (Side).X + Device.Sticks (Side).X > Real_Percent'Last
                                           then Real_Percent'Last
                                           else Player.Sticks (Side).X + Device.Sticks (Side).X); 
                Player.Sticks (Side).Y := (if Player.Sticks (Side).Y + Device.Sticks (Side).Y > Real_Percent'Last
                                           then Real_Percent'Last
                                           else Player.Sticks (Side).Y + Device.Sticks (Side).Y); 
              end loop;
              for Side in Trigger_Kind'Range loop
                Player.Triggers (Side) := (if Player.Triggers (Side) + Device.Triggers (Side) > Real_Percent'Last
                                           then Real_Percent'Last
                                           else Player.Triggers (Side) + Device.Triggers (Side));
              end loop;
          end case;
          Players.Replace (Device.Player, Player);
        end loop;
        Devices.Set (Current_Devices);

        -- Trigger the impulses based on each player's state
        if Input_Status.Occupied then
          for Impulse of Impulses.Get loop
            for Bind of Impulse.Bindings.Get loop
              if Changed (Impulse.Rapid, Bind, Players.Get (Bind.Player), Old_Players.Element (Bind.Player)) then
                Args.Clear; -- Dont register clicks or cursor movement if the cursor is not In_Main_Window when in Windowed_Mode!
                Args.Append (Build_Impulse_Arg (Bind, Players.Get (Bind.Player)));

                -- Handle combinations
                if Bind.Combo /= NO_COMBO then -- Failed combos get repeated, but its OK for now
                  for Combo_Bind of Impulse.Bindings.Get loop
                    if Combo_Bind.Combo = Bind.Combo then
                      if not Changed (Impulse.Rapid, 
                                      Bind,
                                      Players.Get (Combo_Bind.Player),
                                      Old_Players.Element (Combo_Bind.Player))
                      then

-- A binding in the combo is not active
goto Combo_Not_Activated;

                      end if;
                      Args.Append (Build_Impulse_Arg (Combo_Bind, Players.Get (Combo_Bind.Player)));
                    end if;
                  end loop;
                end if;
                Impulse.Callback (Vector_Impulse_Arg.To_Unsafe_Array (Args));
              end if;

-- Skip further tests for combo activation
<<Combo_Not_Activated>>

            end loop;
          end loop;

          -- Center the cursor if we are in game mode
          if Cursor_Status.Occupied then Set_Cursor (Main_Window_Center); end if;
        end if;

        -- Delay the main loop if you have some spare time
        delay WINDOW_POLLING_DURATION - (Clock - Last_Time); Last_Time := Clock;
      end loop;
      System.Finalize_Input;
    end;
  pragma Inline (Run_Input);
  
  -- Input task and external lifecycle routines
  package Input_Tasks is new Tasks (Run_Input);
  Input_Task : Input_Tasks.Safe_Task;
  procedure Initialize_Input is begin Input_Task.Initialize; end;
  procedure Finalize_Input   is begin Input_Task.Finalize;   end;
  
  --------------------
  -- Subunit Bodies --
  --------------------

  package body CPU    is separate;
  package body System is separate;
end;
