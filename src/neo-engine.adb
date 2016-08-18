
--                                                                                                                    
--                                                                N E O  E N G I N E                                                    
--                                                                                                                    
--                                                         Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

package Neo.Engine is

  ------------
  -- Import --
  ------------

  package Import is

      -- Clipboard 

      procedure Copy (Item : Str_2);
      function Paste return Str_2;

      -- Memory

      function Get_Memory return Memory_State;
 
      -- Error_Handling
    
      procedure Alert        (Val : Bool);
      procedure Open_Text    (Path : Str_2);
      procedure Open_Webpage (Path : Str_2);
      procedure Execute      (Path : Str_2; Fullscreen : Bool)
      function Last_Error    return Int_4_Unsigned;
      function Ok            (Name, Message : Str_2; Buttons : Button_Kind; Icon : Icon_Kind)
                             return Bool;

      -- Information

      function Get_Information return Information_State;

      -- Windowing

      procedure Minimize;
      procedure Fullscreen;
      procedure Assert_Only_Instance;
      procedure Initialize_Multi_Monitor;
      procedure Finalize_Multi_Monitor;
      procedure Initialize_Windowing;
      procedure Finalize_Windowing;
      procedure Clip_Cursor     (Undo : Bool := False; Hide : Bool := False);
      procedure Set_Cursor      (X, Y : Int_8_Signed);
      procedure Adjust          (X, Y : Int_4_Signed; Width, Height : Int_4_Positive; Fullscreen : Bool);
      procedure Adjust_Windowed (Width, Height : Int_4_Positive);
      function Update_Windowing return Bool;
      function Get_Borders      return Vector_Border.Unsafe.Vector;
      function Get_Decoration   return Border_State;
      function Fullscreen_Only  return Bool;

      -- Input

      procedure Initialize_Input;
      procedure Finalize_Input;
      procedure Set_Vibration (Id : Int_Address; High, Low : Real_4_Percent);
      function Get_Cursor     return Cursor_state;
      function Update_Input   return Bool;

      -- Console

      procedure Run;
    end;
  package Import is separate;

  -------------
  -- Command --
  -------------

  SINGLE_LINE_COMMENT    : constant Str_2           := "--";
  MAX_ValS_DISPLAYABLE : constant Int_32_Positive := 5;
  type Ptr_Function_Get      is access function  return Str_16;
  type Ptr_Procedure_Set     is access procedure (Item : Str_16);
  type Ptr_Procedure_Perform is access procedure (Parameters : Array_Str_16_Complex);
  type CVar_State is record
      Saved_Val : Str_16_Complex;
      Get         : Ptr_Function_Get;
      Set         : Ptr_Procedure_Set;
    end record;
  package Hashed_CVar       is new Hashed_Maps (CVar_State);
  package Hashed_Str_16_Complex is new Hashed_Maps (Str_16_Complex);
  package Hashed_Perform        is new Hashed_Maps (Ptr_Procedure_Perform);
  Duplicate : Exception;
  Parse     : Exception;
  Names     : Hashed_Str_16_Complex.Map;
  Actions   : Hashed_Perform.Map;
  CVars      : Hashed_CVar.Map;
  package body CVar is
      use Hashed_Map_CVar_State;
      type Control_State is new Controlled with null record;
      protected type Safe_Var_T is
          procedure Set (Val : in Var_T);
          function Get  return Var_T;
        private
          Current : Var_T := INIT;
        end;
      protected body Safe_Var_T is
          function Get return Var_T is (Current);
          procedure Set (Val : Var_T) is begin Current := Val; end;
        end;
      Control : Control_State;
      Data    : Safe_Var_T;
      function Get return Var_T is (Data.Get);
      procedure Set (Val : Var_T) is
        begin
          if Adjust /= null then Data.Set (Adjust.All (Get, Val));
          else Data.Set(Val); end if;
        end;
      procedure Initialize (Control : out Control_State) is
        begin
          if Actions.Has_Element (Name) or else (CVars.Get (Name).Set /= null or CVars.Get (Name).Get /= null) then raise Duplicate; end if;
          Set (To_Str_16 (CVars.Get (Name).Saved_Val));
          CVars.Replace (Name, (CVars.Get (Name).Saved_Val, Get'Unrestricted_Access, Set'Unrestricted_Access));
        exception when others => CVars.Insert (Name, (NULL_STR_16_COMPLEX, Get'Unrestricted_Access, Set'Unrestricted_Access)); end;
      procedure Finalize (Control : out Control_State) is
        begin
          if Saved then CVars.Replace (Name, (To_Str_16_Complex (Trim (Data.Get'Img, Both)), null, null));
          else CVars.Delete (Name); end if;
        end;
      procedure Set (Val : Str_16) is
        begin
          Set (Var_T'Wide_Val (Val));
        exception when Constraint_Error =>
          for I in Var_T'Range loop
            if Val = I'Img then
              Set(I);
              exit;
            elsif I = Var_T'Last then
              Line (-"Incorrect parameter" & Name & ": " & Val);
              Line (Get);
            end if;
          end loop;
        end;
      function Get return Str_16 is
        Vals : Str_16_Complex := To_Str_16_Complex (Trim (Var_T'First'Img, Both));
        begin
          if Var_T'Pos (Var_T'Last) - Var_T'Pos (Var_T'First) > MAXIMUM_POSSIBLE_ValS_DISPLAYED then
            Vals := Vals & ".." & Trim (Var_T'Last'Img, Both);
          else
            for I in Var_T'Val (Var_T'Pos (Var_T'first) + 1)..Var_T'Last loop
              Vals := Vals & ", " & To_Str_16_Complex (Trim (I'Img, Both));
            end loop;
          end if;
          return -Help                                        & EOL_16 &
                 -CURRENT_Val   & Trim (Data.Get'Img, Both) & EOL_16 &
                 -POSSIBLE_ValS & To_Str_16 (Vals);
        end;
    end;
  package body Action is
      use Hashed_Perform;
      type Control_State is new Controlled with null record;
      Control : Control_State;
      procedure Not_Formal (Parameters : Array_Str_16_Complex) renames Perform;
      procedure Finalize   (Control : out Control_State) is begin Actions.Delete (Name); end;
      procedure Initialize (Control : out Control_State) is
        begin
          if Actions.Has_Element(Name) then raise Duplicate; end if;
          Actions.Insert (Name, Not_Formal'Unrestricted_Access);
        end;
    end;
  procedure Command (Text : Str_16) is
    Line :          Array_Str_16_Complex := Split (Text);
    CMD  : constant Str_16               := To_Str_16 (Line (1));
    begin
      if Actions.Has_Element (CMD) then Actions.Get (CMD).All ((2..Line'Length));
      elsif CVars.Has_Element (CMD) then
        if Line'Length = 1 then
          if CVars.Get (CMD).Get /= null then Line (CVars.Get (CMD).Get.All); end if;
        elsif CVars.Get (CMD).Set /= null then CVars.Get (CMD).Set.All (To_Str_16 (Line (2))); end if;
      else raise Constraint_Error; end if;
    exception when others => Line (-"No such CVar or action!"); end;
  function Autocomplete (Text : Str_16) return Array_Str_16_Complex is
    begin
      for CVar in CVars loop
        if Key (CVar) ()
      end loop;
    end;
  procedure Load_Commands (Path : Str_16) is
    Data : File_Type;
    Line : Str_2_Complex;
    begin
      Open (Data, In_File, Path);
      while not End_Of_File (Data) loop
        if Index (Data.Last_Element, SINGLE_LINE_COMMENT) /= 0 then
          Line := Head (Data.Last_Element, Index (Data.Last_Element, SINGLE_LINE_COMMENT) - 1);
        end if;
        Command (To_Str_2 (Line));
      end loop;
    end;
  procedure Save_Commands (Path : Str_16) is
    begin
      Open (Data, Out_File, Path);
      for CVar in CVars loop Put_Line (Data, CVar.Key & " " & CVar.Val); end loop;
      for Action in Actions loop
        if Action.Save /= null then Put_Line (Data, Action.Save); end if;
      end loop;
    end;

  -------------------
  -- Error_Handing --
  -------------------

  procedure Trace is
    Trace  : Tracebacks_Array (Int_32_Positive);
    Length : Int_32_Natural;
    begin
      Call_Chain (Trace, Length);
      Put (Local ("Call stack:"));
      declare
      Traceback : Str_16         := To_Str_16 (Symbolic_Traceback (Trace (1..Length)));
      Skip_Next : Bool           := False;
      To_Skip   : Int_32_Signed := 3;
      Current   : Int_32_Signed := 1;
      Skips     : Int_32_Signed := 1;
      Index     : Int_32_Signed := 0;
      begin
        while Traceback (Current) /= Char_16'Val (Char_1'Pos (ASCII.LF)) and Current /= Traceback'Last loop
          if Index /= Length then Line; end if;
          for I in 2..Length'Img'Length - Length'Img'Length loop Put (" "); end loop;
          Put (Index_Image & ": " & TAB_16 & Traceback (Current..Current + 10));
          for I in Current + 11..Traceback'Last loop
            if I = Traceback'Last then Current := I; elsif Traceback (I) = Char_16'Val (Char_1'Pos (ASCII.LF)) then
              Current := I + 1;
              exit;
            end if;
            if Traceback (Current + 11) /= ' ' then
              if I + 2 <= Traceback'Last and then Traceback (I..I + 2) = "at " then
                Skip_Next := True;
                Put (END_LINE_16 & TAB_16);
              end if;
              if not Skip_Next then Put (Traceback (I));
              else
                if Skips = To_Skip then
                  Skip_Next := False;
                  Skips     := 1;
                else Skips  := Skips + 1; end if;
              end if;
            end if;
          end loop;
          Index := Index + 1;
        end loop;
        Line;
      end;
      delay 0.1; -- Assure printing is done
    end;
  procedure Handle (Occurrence : Exception_Occurrence) is
    begin
      Trace;
      Line (-To_Str_16 (Exception_Name    (Occurrence)));
      Line (-To_Str_16 (Exception_Message (Occurrence)));
      Running.Set (Crash);
    end;
  function Last_Error    return Str_16   is (-"Error number: " & Trim (Import.Get_Last_Error'Img, Both));
  function Alerting      return Bool     is (Alert_Status.Is_Doing_Something);
  function Okay          (Name, Message : Str_16; Buttons : Buttons := Okay_Button; Icon : Icon := No_Icon)
                         return Bool     is begin return Import.Is_Okay (Name, Message, Buttons, Icon);         exception when Call_Failure => Debug_Line (-"Failed is okay!"); return False; end;
  procedure Alert        (Val  : Bool)   is begin Alert_Status.Set_Is_Doing_Something (Val); Import.Alert(Val); exception when Call_Failure => Debug_Line (-"Failed to alert!");              end;
  procedure Open_Text    (Path : Str_16) is begin Import.Open_Text (Path);                                      exception when Call_Failure => Debug_Line (-"Failed to open "    & Path);     end;
  procedure Open_Webpage (Path : Str_16) is begin Import.Open_Webpage (Path);                                   exception when Call_Failure => Debug_Line (-"Failed to open "    & Path);     end;
  procedure Execute      (Path : Str_16) is begin Import.Execute (Path, Do_Fullscreen);                         exception when Call_Failure => Debug_Line (-"Failed to execute " & Path);     end;  

  -------------
  -- Text_IO --
  -------------

  RADIAN_IMAGE_STRING_SIZE         : constant Int_32_Positive := 256;
  FAILED_LOCALIZE_PREVIEW_LENGTH   : constant Int_32_Positive := 10;
  HANG_INDICATORS_DRAWN_PER_SECOND : constant Real_32   := 0.5;
  HANG_DELAY                       : constant Duration        := 3.0;
  protected type Safe_Data is
      procedure Fail;
      procedure Debug       (Val : Bool);
      procedure Input_Entry (Val : Str_16);
      procedure Line_Size   (Val : Int_4_Positive);
      procedure Tasks       (Val : Int_4_Positive);
      procedure Put         (Val : Ptr_Procedure_Put := Ada.Wide_Text_IO.Put'Access);
      procedure Put         (Item  : Str_16);
      function Log          return Str_16          is (To_Str_16 (Current_Log));
      function Input_Entry  return Str_16          is (To_Str_16 (Current_Input_Entry));
      function Line         return Int_32_Positive is (Current_Line_Size);
      function Lines        return Int_64_Natural  is (Current_Number_Of_Lines);
      function Tasks        return Int_32_Positive is (Current_Number_Of_Tasks);
    private
      Current_Put         : Ptr_Procedure_Put;
      Current_Log         : Str_16_Complex;
      Current_Input_Entry : Str_16_Complex;
      Current_Lines       : Int_64_Natural;
      Current_Tasks       : Int_32_Positive;
      Current_Line        : Int_32_Positive := 80;
    end;
  protected Safe_Data is
      procedure Line        (Val : Int_32_Positive)   is begin Current_Line        := Val; end;
      procedure Tasks       (Val : Int_32_Positive)   is begin Current_Tasks       := Val; end;
      procedure Put         (Val : Ptr_Procedure_Put) is begin Current_Put         := Val; end;
      procedure Input_Entry (Val : Str_16)            is begin Current_Input_Entry := To_Str_16_Complex (Val); end;
      procedure Put (Item : Str_16) is
        Count : Int_64_Natural := 0;
        begin
          Current_Log := Current_Log & To_Str_16_Complex (Item);
          if Current_Put /= null then Current_Put.All (Item); end if;
          for Get in Item loop
            if Get = Char_16'Val (Char_1'Pos (ASCII.CR)) then Count := Count + 1; end if;
          end loop;
          Current_Number_Of_Lines := Current_Number_Of_Lines + Count;
        end;
    end;
  Data : Safe_Data;
  procedure Line        (Count : Int_32_Positive := 1)          is begin for I in 1..Count loop Put (EOL_16); end loop; end;
  procedure Input_Entry (Val   : Str_16)                        is begin Data.Set_Input_Entry (Val);                    end;
  procedure Line_Size   (Val   : Int_32_Positive)               is begin Data.Set_Line_Size (Val);                      end;
  procedure Put         (Val   : Ptr_Procedure_Put)             is begin Data.Set_Put (Val);                            end;
  procedure Tasks       (Val   : Int_32_Positive)               is begin Data.Set_Number_Of_Tasks (Val);                end;
  procedure Put         (Item  : Char_16)                       is begin Put (Item & "");                               end;                  
  procedure Put         (Item  : Str_16)                        is begin Data.Put (Item);                               end;
  procedure Line        (Item  : Char_16)                       is begin Line (Item & "");                              end;    
  procedure Line        (Item  : Str_16_Complex)                is begin Line (To_Str_16 (Item));                       end;
  procedure Line        (Item  : Str_16)                        is begin Put (Item); Line;                              end;
  procedure Debug       (Item  : Str_16)                        is begin if Debug.Get then Data.Put (Item); end if;     end;
  procedure Debug_Line  (Item  : Str_16)                        is begin Put_Debug (Item); Line;                        end;   
  function Localize     (Item  : Str_16) return Str_16          is (Data.Localize (Item));
  function Extension    (Path  : Str_16) return Str_16          is (Path (Index (Path, ".") + 1..Path'Last));
  function Log                           return Str_16          is (Data.Get_Log);
  function Input_Entry                   return Str_16          is (Data.Get_Input_Entry); 
  function Lines                         return Int_64_Natural  is (Data.Get_Number_Of_Lines);
  function Line_Size                     return Int_32_Positive is (Data.Get_Line_Size);
  function Tasks                         return Int_32_Positive is (Data.Get_Number_Of_Tasks);
  procedure Title (Item : Str_16) is
    Spaces : Int_32_Signed := 0;
    Count  : Int_32_Signed := Item'Length * 3 - Item'Length / 3;
    begin
      if Item'Length > Get_Line_Size then raise Title_Is_Too_Long; end if;
      Put                                    (Char_16'Val (16#250C#));
      for I in 1..Get_Line_Size - 2 loop Put (Char_16'Val (16#2500#)); end loop;
      Line                                   (Char_16'Val (16#2510#));
      Put                                    (Char_16'Val (16#2502#));
      for I in 1..Item'Length loop
        if Item (I) = ' ' then Spaces := Spaces + 1; end if;
      end loop;
      if Count + Spaces >= Get_Line_Size then
        for I in 1..Get_Line_Size / 2 - Item'Length / 2 - 1 loop Put (" "); end loop;
        Put (Item);
        for I in 1..Get_Line_Size - Item'Length - (Get_Line_Size / 2 - Item'Length / 2) - 1 loop Put (" "); end loop;
      else
        for I in 1..Get_Line_Size / 2 - (Count + Spaces) / 2 - 1 loop Put (" "); end loop;
        for I in 1..Item'Length loop
          Put (Item (I) & "  ");
          if Item (I) = ' ' then Put (" "); end if;
        end loop;
        for I in 1..Get_Line_Size - (Get_Line_Size / 2 - (Count + Spaces) / 2 - 1) - Item'Length * 3 - Spaces - 2 loop Put (" "); end loop;
      end if;
      Line                                   (Char_16'Val (16#2502#));
      Put                                    (Char_16'Val (16#2514#));
      for I in 1..Get_Line_Size - 2 loop Put (Char_16'Val (16#2500#)); end loop;
      Line                                   (Char_16'Val (16#2518#));
    end;
  function Localize (Item : Str_16) return Str_16 is
    Result : Str_16 := Input_Output.L(Item);
    begin
      if Result = NULL_Str_16 then
        if DO_PUT_L_FAILURE then
          Debug_Line ("Failed to -""" & Item (Item'First..
            (if Item'Length >= FAILED_L_PREVIEW_LENGTH then Item'first + FAILED_L_PREVIEW_LENGTH - 1 else Item'Last)) & """");
        end if;
        return Item;
      end if;
      return Result;
    end;

  -------------
  -- Console --
  -------------

  procedure Run        is begin Import.Run;                        exception when Call_Failure => Debug_Line (-"Failed to initialize console!"); end;
  procedure Send_Log   is begin Open_Webpage(ERROR_REPORTING_URL); exception when others       => Debug_Line (-"Failed to send log!");           end;
  procedure Save_Log is
    File        : File_Type;
    File_Stream : Stream_Access;
    Buffer      : Str_16_Complex;
    Offset      : Time_Offset := UTC_Time_Offset (Clock);
    Path        : Str_8       := To_Str_8 (To_Str_16 (Get_Information.Username)) & "_" &
      Trim (Month (Clock, Offset)'Img, Both) & "-" & Trim (Day (Clock, Offset)'Img, Both) & "-" & Trim (Year (Clock, Offset)'Img, Both) & "_" &
      Trim (Hour  (Clock, Offset)'Img, Both) & "-" & Trim (Minute (Clock, 0)'Img,   Both) & "-" & Trim (Second (Clock)'Img,       Both) & ".txt";
    begin
      Create (File, Out_File, To_Str_8 (To_Str_16 (Get_Information.Path) & Get_Information.Separator & PATH_LOGS & Get_Information.Separator) & Path);
      File_Stream := Stream (File);
      Buffer := To_Str_16_Complex (Get_Log);
      for I in 1..Length (Buffer) loop Char_16'Write (File_Stream, Get (Buffer, I)); end loop;
      Close (File);
      Open_Text (To_Str_16(Get_Information.Path) & Get_Information.Separator & PATH_LOGS & Get_Information.Separator & To_Str_16(Path));
    exception when Call_Failure => Debug_Line (-"Failed to save log!"); end;

  ---------------
  -- Clipboard --
  ---------------

  function Paste return Str_16   is begin return Import.Paste; exception when Call_Failure => Debug_Line (-"Failed paste!"); return NULL_Str_16; end;
  procedure Copy (Item : Str_16) is begin Import.Copy (Item);  exception when Call_Failure => Debug_Line (-"Failed copy!");                      end;

  ------------
  -- Memory --
  ------------

  INITIAL_MEMORY : constant Memory_State := Import.Get_Memory;
  function Get_Initial_Memory return Memory_State is (INITIAL_MEMORY_STATE);
  function Get_Memory         return Memory_State renames Import.Get_Memory;

  ---------------
  -- Processor --
  ---------------

  function Clock_Ticks return Int_64_Unsigned renames Import.Clock_Ticks;
  package body Tasks is
      task type Task_Unsafe is entry Initialize (ID : in out Task_ID); end;
      type Ptr_Task_Unsafe is access all Task_Unsafe;
      procedure Finalize is new Ada.Unchecked_Deallocation (Task_Unsafe, Ptr_Task_Unsafe);
      task body Task_Unsafe is
        begin
          accept Initialize (Id : out Task_Id) do Id := Current_Task; end;
          begin Run; exception when Occurrence: others => Handle (Occurrence); end;
          Set_Tasks (Get_Tasks - 1);
        end;
      protected body Safe_Task is
pragma Warnings (Off); -- "Potentially blocking operating in protected type"
          procedure Initialize is
            begin
              if Current_Id /= NULL_TASK_ID and then not Is_Terminated (Current_id) then raise Task_Error with -"Initialized without being finalized"; end if;
              Current_Task := new Task_Unsafe;
              Current_Task.Initialize (Current_Id);
              Set_Number_Of_Tasks (Get_Number_Of_Tasks + 1);
            end;
          procedure Finalize is
            begin
              if Current_Id = NULL_TASK_ID or else Is_Terminated (Current_id) then raise Task_Error with -"Finalized without being initialized"; end if;
              Abort_Task (Current_Id);
              Current_Id := NULL_TASK_ID;
              Finalize (Current_Task);
              Set_Number_Of_Tasks (Get_Number_Of_Tasks - 1);
            end;
pragma Warnings (On);
          function Running return Bool is
            begin
              return Current_Task /= null and not Is_Terminated(Current_Id);
            exception when others => return False; end;
        end;
    end;

  -----------
  -- Input --
  -----------

  DURATION_FOR_MUTEX_WAIT : constant Duration := 0.0001;
  DURATION_BEFORE_POLLING : constant Duration := 0.002;
  package body Impulse is
      Duplicate    : Exception;
      Out_Of_Scope : Exception;
      Name : Str_16_Complex := To_Lower (Name);
      function Totally_Not_A_Formal_Subprogram renames Callback; -- Its *totally* not ;)
      procedure Enable                                   is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := True;  Impulses.Replace (Name, Impulse); end;
      procedure Disable                                  is Impulse : Impulse_State := Impulses.Get (Name); begin Impulse.Enabled := False; Impulses.Replace (Name, Impulse); end;
      procedure Finalize   (Gamepad : out Gamepad_state) is                                                       begin if Running.Get                 then raise Out_Of_Scope; end if; Impulses.Delete (Name); end;
      procedure Initialize (Gamepad : out Gamepad_state) is                                                       begin if Impulses.Has_Element (Name) then raise Duplicate;    end if; Impulses.Insert (Name,
                                                                                                                  (Totally_Not_A_Formal_Subprogram'Unrestricted_Access, Bindings'Unrestricted_Access, True)); end;
    end;
  function Get_Cursor                      return Cursor_State                              renames Import.Get_Cursor;
  function Get_Devices                     return Map_Device.Unsafe.Map                     is (Devices.Get);
  function Get_Device      (Id : Int_Addr) return Record_Device                             is (Devices.Get (Id));
  function Has_Device      (Id : Int_Addr) return Bool                                      is (Devices.Has_Element (Id));
  procedure Remove_Device  (Id : Int_Addr)                                                  is begin Devices.Delete (Id); end;
  procedure Add_Device     (Id : Int_Addr; Device  : Record_Device)                         is begin if not Players.Has_Element (Device.Player) then Players.Insert (Device.Player, (others => <>)); end if; Devices.Insert (Id, Device); end;
  procedure Set_Device     (Id : Int_Addr; Player  : Int_32_Positive := 1)                  is Device : Record_Device := Devices.Get (Id); begin Device.Player              := Player;           Devices.Replace (Id, Device); end;
  procedure Inject_Text    (Id : Int_Addr; Text    : Str_16_Complex)                        is Device : Record_Device := Devices.Get (Id); begin Device.Text                := Text;             Devices.Replace (Id, Device); end;
  procedure Inject_Cursor  (Id : Int_Addr; Cursor  : Cursor_State)                          is Device : Record_Device := Devices.Get (Id); begin Device.Cursor              := Cursor;           Devices.Replace (Id, Device); end;
  procedure Inject_Trigger (Id : Int_Addr; Trigger : Trigger_Kind; Press : Real_32_Percent) is Device : Record_Device := Devices.Get (Id); begin Device.Triggers (Trigger)  := Press;            Devices.Replace (Id, Device); end;
  procedure Inject_Stick   (Id : Int_Addr; Stick   : Stick_Kind;   Axis  : Cursor_State)    is Device : Record_Device := Devices.Get (Id); begin Device.Sticks (Stick)      := Axis;             Devices.Replace (Id, Device); end;
  procedure Inject_Button  (Id : Int_Addr; Button  : Mouse;   Pressed : Bool)               is begin if Device.Mouse (Key).Pressed    /= Pressed then Device.Mouse (Key)    := (Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Button  (Id : Int_Addr; Button  : Gamepad; Pressed : Bool)               is begin if Device.Keyboard (Key).Pressed /= Pressed then Device.Keyboard (Key) := (Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Key     (Id : Int_Addr; Key     : Key;     Pressed : Bool)               is begin if Device.Keyboard (Key).Pressed /= Pressed then Device.Keyboard (Key) := (Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject (Binding : Binding_State) is
    begin
      while Status.Is_Doing_Something loop delay DURATION_FOR_MUTEX_WAIT; end loop; Status.Set_Is_Doing_Something(True);
      case Binding.Kind is
        when Key_Kind     => Injection.Keyboard (Binding.Key)      := Binding.Mode;
        when Trigger_Kind => Injection.Triggers (Binding.Trigger)  := Binding.Press;
        when Stick_Kind   => Injection.Sticks   (Binding.Stick)    := Binding.Axis;
        when Gamepad_Kind => Injection.Gamepad  (Binding.Gamepad)  := Binding.Mode;
        when Mouse_Kind   => Injection.Mouse    (Binding.Mouse)    := Binding.Mode;
        when Cursor_Kind  => Injection.Cursor                      := Binding.Cursor;
        when Text_Kind    => if In_Menu.Get then Injection.Text := Injection.Text & Binding.Text; end if;
      end case;
      Status.Set_Is_Doing_Something (False);
    end;
  procedure Set_Vibration (Player : Int_32_Positive := 1; Frequency_High, Frequency_Low : Real_4_Percent) is
    Current_Device : Ordered_Map_Record_Device.Cursor := Devices.First;
    begin
      while Devices.Has_Element (Current_Device) loop
        if Devices.Get (Current_Device).Player = Player then Import.Set_Vibratio n(Devices.Key (Current_Device), Frequency_High, Frequency_Low); end if;
        Devices.Next (Current_Device);
      end loop;
    end;
  procedure Unbind (Args : Array_Str_16_Complex) is
    begin

    end;
  procedure Bind (Args : Array_Str_16_Complex) is
    begin
      case Line'Length is
        when 1 =>
          Line (-"Bind an impulse to an input Val");
          Line ("bind [player#] impulse Val");
        when 3 =>
          Player  := 1;
          Impulse := Impulses.Get (To_Str_16 (Lines(2)));
        when 4 =>
          Player := Line(2);
          Line(2).Delete;
          Impulse := Impulses.Get (To_Str_16 (Lines(3)));
        when 5 =>
      when others => null; end case;
    end;
  procedure Run is
    procedure Clear_State (Binding : in out Binding_State; Impulse : in out Impulse_State) is
      begin
        Impulse.Bindings.Replace (Current_Binding, Binding);
        if Binding.Combo /= NO_COMBO then
          for Other of Impulse.Bindings.Get loop
            if Other.Combo = Binding.Combo then
              case Other.Kind is
                when Text_Kind                 => if Other.Text = NULL_STR_16_COMPLEX then return; end if;
                when Stick_Kind | Cursor_Kind  => if Other.Cursor = (others => <>)    then return; end if;
                when Mouse_Kind | Gamepad_Kind => if not Other.Mode.Pressed           then return; end if;
                when Trigger_Kind              => if Other.Press = 0.0                then return; end if;
              end case;
            end if;
          end loop;
        end if;
        Impulse.Trip (Binding);
        if Binding.Kind = Text_Kind then Binding.Text := NULL_STR_16_COMPLEX; Impulse.Bindings.Replace (Current_Binding, Binding); end if;
      end;
    Last_Time : Time := Clock;
    begin
      Status.Set_Active (False);
      Import.Initialize;
      while Running.Get and Import.Update loop
        if Active.Get then
          for Player in Players loop
            Player := Injection;
          end loop;
          for Device in Devices loop
            Player := Players.Get (Device.Player);
            if Device.Text /= NULL_STR_16_COMPLEX then Player.Text := Player.Text & Device.Text; end if;
            case Device.Kind is
              when Keyboard_Device =>
                for Key in Key_Kind'Range loop
                  if Player.Mouse (Button).Pressed and not Device.Mouse (Button).Last < Player.Mouse (Button).Last) then 
                    Player.Keyboard (Key) := Device.Keyboard (Key);
                    case Key is
                      when Left_Shift_Key | Right_Shift_Key => Player.Keyboard (Shift_Key) := Player.Keyboard (Key);
                      when Left_Ctrl_Key  | Right_Ctrl_Key  => Player.Keyboard (Ctrl_Key)  := Player.Keyboard (Key);
                      when Left_Alt_Key   | Right_Alt_Key   => Player.Keyboard (Alt_Key)   := Player.Keyboard (Key);
                    when others => null; end case;
                  elsif not Device.Keyboard (Key).Pressed and Device.Keyboard (Key).Last > Player.Keyboard (Key).Last then 
                    Player.Keyboard (Key) := Device.Keyboard (Key);
                  end if;
                end loop;
              when Mouse_Device =>
                for Button in Mouse_Kind'Range loop
                  if Device.Mouse (Button).Pressed and Device.Keyboard (Key).Pressed and not Player.Keyboard (Key).Pressed or Device.Keyboard (Key).Last < Player.Keyboard (Key).Last) then 
                    Player.Mouse (Button) := Device.Mouse (Button);
                  end if;
                  if Device.Mouse (Button).Pressed and Button in Wheel_Left_Key..Wheel_Down_Key then
                    Device.Mouse (Button).Pressed := False;
                  end if;
                end loop;
                Player.Cursor := (Player.Cursor.X + Device.Cursor.X, Player.Cursor.Y + Device.Cursor.Y);
              when Gamepad_Device =>
                for Button in Gamepad_Kind'Range loop
                  if Device.Gamepad (Button).Pressed and not Player.Gamepad (Button).Pressed or Device.Gamepad (Button).Last < Player.Gamepad (Button).Last) then 
                    Player.Gamepad (Button) := Device.Gamepad (Button); 
                  end if;
                end loop;
                for Side in Stick_Kind'Range loop
                  Player.Stick (Side) := (Player.Stick (Side).X + Device.Stick (Side).X, Player.Stick (Side).Y + Device.Stick (Side).Y);
                end loop;
                for Side in Trigger_Kind'Range loop
                  if Player.Trigger (Side) + Device.Trigger (Side) > 100.0 then Player.Trigger (Side) := 100.0;
                  else Player.Trigger (Side) := Player.Trigger (Side) + Device.Trigger (Side); end if;
                end loop;
            end case;
          end loop;
          for Impulse in Impulses loop
            for Binding in Impulse.Bindings loop
              Player := Players.Get (Binding.Player);
              case Binding.Kind is
                when Trigger_Kind => if Player.Triggers (Binding.Trigger)         /= Binding.Press        then Binding.Press  := Player.Trigger  (Binding.Trigger); Clear_State (Binding, Impulse); end if;
                when Stick_Kind   => if Player.Sticks   (Binding.Stick)           /= Binding.Axis         then Binding.Axis   := Player.Sticks   (Binding.Stick);   Clear_State (Binding, Impulse); end if;
                when Gamepad_Kind => if Player.Gamepad  (Binding.Gamepad).Pressed /= Binding.Mode.Pressed then Binding.Mode   := Player.Gamepad  (Binding.Gamepad); Clear_State (Binding, Impulse); end if;
                when Mouse_Kind   => if Player.Mouse    (Binding.Mouse).Pressed   /= Binding.Mode.Pressed then Binding.Mode   := Player.Mouse    (Binding.Mouse);   Clear_State (Binding, Impulse); end if;
                when Key_Kind     => if Player.Keyboard (Binding.Key).Pressed     /= Binding.Mode.Pressed then Binding.Mode   := Player.Keyboard (Binding.Key);     Clear_State (Binding, Impulse); end if;
                when Cursor_Kind  => if Player.Cursor                             /= Binding.Cursor       then Binding.Cursor := Player.Cursor;                     Clear_State (Binding, Impulse); end if;
                when Text_Kind    => if Player.Text                               /= Binding.Text         then Binding.Text   := Player.Text;                       Clear_State (Binding, Impulse); end if;
              end case;
            end loop;
          end loop;
          Injection := (Text => NULL_STR_16_COMPLEX, Mouse => (Vertical_Up_Button..Horizontal_Right_Button => False, others => Injection.Mouse), others => Injection);
          Status.Set_Active (False);
        end if;
        delay DURATION_TO_WAIT_BEFORE_POLLING - (Clock - Last_Time); Last_Time := Clock;
      end loop;
      Import.Finalize;
    end;

  ---------------
  -- Windowing --
  ---------------

  function Borders    return Vector_Border.Unsafe.Vector renames Import.Borders;
  function Decoration return Border_State                renames Import.Decoration;
  procedure Perform_Exit_Menu (Binding : Binding_State) is
    begin
      if Binding.Kind = Mouse_Kind and then Binding.Mode.Pressed then
        In_Menu.Set (not In_Menu.Get);
      end if;
    end;
  procedure Preform_Fullscreen (Binding : Binding_State) is
    begin
      if Binding.Mode.Pressed then
        Mode.Set ((case Mode.Get is when Multi_Monitor_Mode | Fullscreen_Mode => Windowed_Mode, when Windowed_Mode => Fullscreen_Mode));
      end if;
    end;
  procedure Perform_Enter_Menu (Binding : Binding_State) is
    Cursor : Cursor_State := Get_Cursor;
    Border : Border_State := Get_Borders.Get (1);
    begin
      Impulse_Detect_Menu_Mode_Entry.Disable;
    end;
  procedure Change_State (Kind : Change) is
    begin
      case Kind is
        when Iconic_Change     => Minimized.Set (True);
        when Windowed_Change   => Minimized.Set (False); Mode.Set (Windowed_Mode);
        when Fullscreen_Change => Minimized.Set (False); Mode.Set (Fullscreen_Mode);
      end case;
    end;
  function Get_Cursor return Cursor_State is
    Cursor : 
    begin
      return (others => <>);
    end;
  procedure Activate (Do_Activate, Do_Detect_Click : Bool; X, Y : Int_64) is
    begin
      if Do_Activate then
        Active.Set(True);
        if In_Menu.Get then Import.Set_Cursor_Style (Inactive_Cursor); end if;
      else
        Active.Set (False);
        Import.Set_Cursor_Style (System_Cursor);
        --Import.Clip_Mouse(UWindowed_Statendo => True);
        if Mode.Get /=  then
          --if Mode.Get = Multi_Monitor_State then Import.Finalize_Multi_Monitor; end if;
          Import.Iconize;
        end if;
      end if;
    end;
  function Resize (Kind : Resize; Border : Border_State) return Border_State is
    Result         : Border_State  := Border;
    Decoration     : Border_State  := Get_Decoration;
    Extra_Width    : Int_64 := Decoration.Right  + Decoration.Left;
    Extra_Height   : Int_64 := Decoration.Bottom + Decoration.Top;
    Current_Width  : Int_64 := (if Border.Right  - Border.Left - Extra_Width  < MINIMUM_FACTOR then MINIMUM_FACTOR else Border.Right - Border.Left - Extra_Width);
    Current_Height : Int_64 := (if Border.Bottom - Border.Top  - Extra_Height < Int_64 (Int_32_Positive (MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get / Aspect_Narrow_Horizontal.Get) then
                               Int_64 (Int_32_Positive (MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get) else Border.Bottom - Border.Top  - Extra_Height);
    Maximum_Width  : Int_64 := Int_64 (Int_32_Positive (Current_Height) * Aspect_Narrow_Horizontal.Get / Aspect_Narrow_Vertical.Get);
    Maximum_Height : Int_64 := Int_64 (Int_32_Positive (Current_Width)  * Aspect_Wide_Vertical.Get     / Aspect_Wide_Horizontal.Get);
    Minimum_Width  : Int_64 := Int_64 (Int_32_Positive (Current_Height) * Aspect_Wide_Horizontal.Get   / Aspect_Wide_Vertical.Get);
    Minimum_Height : Int_64 := Int_64 (Int_32_Positive (Current_Width)  * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get);
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
      Width.Set  (Int_32_Positive (Result.Right  - Result.Left - Extra_Width));
      Height.Set (Int_32_Positive (Result.Bottom - Result.Top  - Extra_Height));
      return Result;
    end;
  procedure Run_Multi_Monitor_Window is
    begin
      --Current_Number_Of_Monitors.Set(Index + 1);
      while Import.Update and Running.Get loop null;
        --Render_Backend;
      end loop;
    end;

  ----------
  -- Main --
  ----------

  Border : Border_State;
  Menued : Bool      := In_Menu.Get;
  Mode   : Mode_Kind := Mode.Get;
  begin
    Import.Assert_Only_Instance;
    Import.Initialize_Windowing;
    Enter_Menu.Bindings.Append (Mouse    (Left_Button));
    Exit_Menu.Bindings.Append  (Keyboard (Escape_Key));
    Fullscreen.Bindings.Append (Keyboard (F11_Key));
    Title (-"Configuration");
    Line;
    Put   (-"Loading " & PATH_CONFIGURATION & "..."); Load_Vars  (PATH_CONFIGURATION); Line (-"OK!");
    Put   (-"Loading " & PATH_LOCALIZATION  & "..."); Load_Sheet (PATH_LOCALIZATION);  Line (-"OK!");
    Line;
    Title (-"Information");
    Line;
    Line  (-"Version: "             & Information.Version));
    Line  (-"Username: "            & To_Str_16 (Information.Username));
    Line  (-"Directory: "           & To_Str_16 (Information.Path));
    Line  (-"Name: "                & To_Str_16 (Information.Name));
    Line  (-"Application bit size"  & WORD_SIZE));
    Line  (-"System bit size:"      & Information.Bit_Size));
    Line;
    Title (-"Clipboard");
    Line;
    Line  (-"Load:")                & Real_4_Percent'Wide_Image (Initial_Memory.Load));
    Line;
    Title (-"PROCESSOR");
    Line;
    Title (-"MEMORY");
    Line;
    Line  (-"Load:"                 & Initial_Memory.Load'Img);
    Line  (-"Disk:"                 & Initial_Memory.Disk'Img);
    Line  (-"Disk available:"       & Initial_Memory.Disk_Available'Img);
    Line  (-"Physical:"             & Initial_Memory.Physical'Img);
    Line  (-"Physical available:"   & Initial_Memory.Physical_Available'Img);
    Line  (-"Page file:")           & Initial_Memory.Page_File'Img);
    Line  (-"Page file available:"  & Initial_Memory.Page_File_Available'Img);
    Line  (-"Virtual total:"        & Initial_Memory.Virtual'Img);
    Line  (-"Virtual available:"    & Initial_Memory.Virtual_Available'Img);
    Line  (-"Virtual available ex:" & Initial_Memory.Virtual_Available_Ex'Img);
    Line;
    Title (-"INPUT");
    Line;
    Line  (-"Load:"                  & );
    Line;
    Title (-"GRAPHICS");
    Line;
    Line  (-"Load:"                  & );
    Line;
    Title (-"SOUND");
    Line;
    Line  (-"Load:"                  & );
    Line;
    while Running.Get loop
      case Current_State is
        when Fullscreen_State | Multi_Monitor_State => Import.Adjust_Fullscreen;
          --if Current_State = Multi_Monitor_State then Import.Initialize_Multi_Monitor; end if;
        when Windowed_State =>
          Border := Import.Get_Decoration;
          Import.Adjust_Windowed(
            Width  => Width.Get  + Int_32_Positive (Border.Right  + Border.Left),
            Height => Height.Get + Int_32_Positive (Border.Bottom + Border.Top));
      end case;
      Initialize (1);
      --Previous_State := Mode.Get;
      --if In_Menu_Mode.Get then
      --  Import.Hide_Mouse(False, False);
      --  Import.Set_Custom_Mouse(False);
      --else
      ----  Import.Hide_Mouse(True, True);
      --end if;
      while Current_State = Mode.Get and Running.Get loop
        if not Import.Update then Running.Set (False); end if;
        --if Current_Menu /= In_Menu.Get then
        --  if Current_Menu then
        --    Current_Menu := In_Menu.Get;
        --    Border := Get_Borders.First_Element;
        --    Import.Set_Cursor(X => Border.Left + (Border.Right  - Border.Left) / 2,Y => Border.Top  + (Border.Bottom - Border.Top)  / 2);
        --    if Current_State = Fullscreen_State then Import.Clip_Mouse(Undo => False, Do_Hide => False);
        --    else Import.Clip_Mouse(Undo => True, Do_Hide => False); end if;
        --  elsif Do_Hide_Mouse then
        --    Current_Menu := In_Menu.Get;
        --    Import.Clip_Mouse(Undo => False, Do_Hide => True);
        --  end if;
        --end if;
      end loop;
      --if Current_State = Multi_Monitor_State and not Minimized.Get then Import.Finalize_Multi_Monitor; end if;
      Current_State := Mode.Get;
    end loop;
    Import.Finalize;
  exception when Occurrence: others =>
    Handle (Occurrence);
    if not Running_Console then Ok (Icon    => Error_Icon,
                                    Name    => To_String_2 (Get_Information.Name),
                                    Message => -"An error has occurred, would you like to view more information?",
                                    Buttons => Yes_No_Buttons) then Initialize_Console; end if;
  end;