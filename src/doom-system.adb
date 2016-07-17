
--                                                                                                                    
--                                                 A D A  D O O M  III                                                    
--                                                                                                                    
--                                         Copyright (C) 2016 Justin Squirek                                          
-- 
-- AdaDoom3 is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- AdaDoom3 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with AdaDoom3. If not, see gnu.org/licenses     
--

package Neo.System is

  ------------
  -- Import --
  ------------

  package Import is
      function Get_State        return Record_State;
      function Get_Specifics   return Record_Specifics;
      function Get_Clock_Ticks return Int_8_Unsigned;
      function Is_Stack_Empty  return Bool;
      procedure Open_Webpage  (Path : in Str_16);
      procedure Open_Text     (Path : in Str_16);
      procedure Execute       (Path : in Str_16; Do_Fullscreen : in Bool);
      procedure Set_Alert     (Value : in Bool);
      function Get_Specifics  return Record_Specifics;
      function Get_Last_Error return Int_4_Unsigned;
      function Is_Okay(Name, Message : in Str_16; Buttons : in Enumerated_Buttons; Icon : in Enumerated_Icon) return Bool with pre => Name'Length > 0 and Message'Length > 0;
      procedure Set_Clipboard(Item : in Str_16);
      function Get_Clipboard return Str_16;
    end;
  package Import is separate;

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
  procedure Handle_Exception (Occurrence : Exception_Occurrence) is
    begin
      Line (To_Str_16 (Exception_Name    (Occurrence)));
      Line (To_Str_16 (Exception_Message (Occurrence)));
      Set_Do_Fail;
      Is_Running.Set (False);
    end;
  function Get_Last_Error return Str_16             is (Local ("Error number: ") & Trim (Import.Get_Last_Error'Img, Both));
  function Is_Alerting    return Bool               is (Alert_Status.Is_Doing_Something);
  function Is_Okay (Name, Message : Str_16; Buttons : Buttons := Okay_Button; Icon : Icon := No_Icon)
                          return Bool               is begin return Import.Is_Okay (Name, Message, Buttons, Icon);                 exception when Call_Failure => Put_Debug_Line (Local ("Failed is okay!")); return False; end;
  procedure Set_Alert     (Value : Bool)            is begin Alert_Status.Set_Is_Doing_Something (Value); Import.Set_Alert(Value); exception when Call_Failure => Put_Debug_Line (Local ("Failed to alert!"));              end;
  procedure Open_Text     (Path : Str_16)           is begin Import.Open_Text (Path);                                              exception when Call_Failure => Put_Debug_Line (Local ("Failed to open ")    & Path);     end;
  procedure Open_Webpage  (Path : Str_16)           is begin Import.Open_Webpage (Path);                                           exception when Call_Failure => Put_Debug_Line (Local ("Failed to open ")    & Path);     end;
  procedure Execute       (Path : Str_16)           is begin Import.Execute (Path, Do_Fullscreen);                                 exception when Call_Failure => Put_Debug_Line (Local ("Failed to execute ") & Path);     end;  
  procedure Assert        (Value : Int_32_Signed_C) is begin Assert (Value /= C_FALSE);                           end;
  procedure Assert        (Value : Address)         is begin Assert (Value /= NULL_ADDRESS);                      end;
  procedure Assert        (Value : Bool)            is begin if not Value then Trace; raise Call_Failure; end if; end;

  -------------
  -- Console --
  -------------


  function Is_Running  return Bool is (Main_Task.Is_Running);
  procedure Initialize is begin Main_Task.Initialize;                                                                                            end;
  procedure Finalize   is begin Main_Task.Finalize;                                                                                              end;
  procedure Run        is begin Import.Run;                        exception when Call_Failure => Put_Debug_Line (Local (FAILED_INITIALIZE)); end;
  procedure Send_Log   is begin Open_Webpage(ERROR_REPORTING_URL); exception when others       => Put_Debug_Line (Local (FAILED_SEND_LOG));   end;
  procedure Save_Log is
    File_Stream : Stream_Access;
    File        : File_Type;
    Buffer      : Str_16_Complex;
    Offset      : Time_Offset := UTC_Time_Offset (Clock);
    Path        : Str_8    := To_Str_8 (To_Str_16 (SPECIFICS.Username)) & "_" &
      Trim (Month (Clock, Offset)'Img, Both) & "-" & Trim (Day (Clock, Offset)'Img, Both) & "-" & Trim (Year (Clock, Offset)'Img, Both) & "_" &
      Trim (Hour  (Clock, Offset)'Img, Both) & "-" & Trim (Minute (Clock, 0)'Img,   Both) & "-" & Trim (Second (Clock)'Img,       Both) & ".txt";
    begin
      Create (File, Out_File, To_Str_8 (To_Str_16 (Neo.System.SPECIFICS.Path) & SPECIFICS.Separator & PATH_LOGS & SPECIFICS.Separator) & Path);
      File_Stream := Stream (File);
      Buffer := To_Str_16_Complex (Get_Log);
      for I in 1..Length (Buffer) loop Char_16'Write (File_Stream, Element (Buffer, I)); end loop;
      Close (File);
      Open_Text (To_Str_16(Neo.System.SPECIFICS.Path) & SPECIFICS.Separator & PATH_LOGS & SPECIFICS.Separator & To_Str_16(Path));
    exception when Call_Failure => Put_Debug_Line (Local (FAILED_SAVE_LOG)); end;

  ---------------
  -- Clipboard --
  ---------------

  function Get_Clipboard return Str_16 is begin return Import.Get_Clipboard; exception when Call_Failure => Put_Debug_Line (Local (FAILED_GET_CLIPBOARD)); return NULL_Str_16; end;
  procedure Set_Clipboard (Item : Str_16) is begin Import.Set_Clipboard (Item); exception when Call_Failure => Put_Debug_Line (Local (FAILED_SET_CLIPBOARD)); end;

  -------------
  -- Text_IO --
  -------------

  RADIAN_IMAGE_STRING_SIZE         : constant Int_32_Positive := 256;
  FAILED_Local_PREVIEW_LENGTH   : constant Int_32_Positive := 10;
  HANG_INDICATORS_DRAWN_PER_SECOND : constant Float_32_Real       := 0.5;
  HANG_DELAY                       : constant Duration            := 3.0;
  protected type Protected_Data is
      procedure Fail;
      procedure Debug           (Value : Bool);
      procedure Input_Entry     (Value : Str_16);
      procedure Line_Size       (Value : Int_4_Positive);
      procedure Tasks           (Value : Int_4_Positive);
      procedure Put             (Value : Ptr_Procedure_Put := Ada.Wide_Text_IO.Put'Access);
      procedure Local           (Value : Ptr_Function_Local);
      procedure Put             (Item  : Str_16);
      function Local            (Item  : Str_16) return Str_16;
      function Get_Log                           return Str_16;
      function Get_Input_Entry                   return Str_16;
      function Get_Line_Size                     return Int_4_Positive;
      function Get_Tasks                         return Int_4_Positive;
      function Get_Lines                         return Int_8_Natural;
      function Do_Put_Debug                      return Bool;
      function Do_Fail                           return Bool;
    private
      Current_Put             : Ptr_Procedure_Put;
      Current_Local           : Ptr_Function_Local;
      Current_Log             : Str_16_Complex;
      Current_Input_Entry     : Str_16_Complex;
      Current_Number_Of_Lines : Int_64_Natural;
      Current_Number_of_Tasks : Int_32_Positive;
      Current_Line_Size       : Int_32_Positive := 80;
      Current_Do_Put_Debug    : Bool            := False;
      Current_Do_Fail         : Bool            := False;
    end;
  protected Protected_Data is
      procedure Fail                                                    is begin Current_Do_Fail      := True;  end;
      procedure Set_Debug       (Value : Bool)                          is begin Current_Debug        := Value; end;
      procedure Set_Line_Size   (Value : Int_32_Positive)               is begin Current_Line_Size    := Value; end;
      procedure Set_Tasks       (Value : Int_32_Positive)               is begin Current_Tasks        := Value; end;
      procedure Set_Put         (Value : Ptr_Procedure_Put)             is begin Current_Put          := Value; end;
      procedure Set_Local       (Value : Ptr_Function_Local)            is begin Current_Local        := Value; end;
      procedure Set_Input_Entry (Value : Str_16)                        is begin Current_Input_Entry  := To_Str_16_Complex (Value); end;
      function Local            (Item : Str_16)  return Str_16          is ((if Current_Local = null then Item else Current_Local.All (Item)));
      function Get_Log                           return Str_16          is (To_Str_16 (Current_Log));
      function Get_Input_Entry                   return Str_16          is (To_Str_16 (Current_Input_Entry));
      function Do_Fail                           return Bool            is (Current_Do_Fail);
      function Do_Put_Debug                      return Bool            is (Current_Do_Put_Debug);
      function Get_Line_Size                     return Int_32_Positive is (Current_Line_Size);
      function Get_Lines                         return Int_64_Natural  is (Current_Number_Of_Lines);
      function Get_Tasks                         return Int_32_Positive is (Current_Number_Of_Tasks);
      procedure Put (Item : Str_16) is
        Count : Int_64_Natural := 0;
        begin
          Current_Log := Current_Log & To_Str_16_Complex (Item);
          if Current_Put /= null then Current_Put.all (Item); end if;
          for Element in Item loop
            if Element = Char_16'Val (Char_1'Pos (ASCII.CR)) then Count := Count + 1; end if;
          end loop;
          Current_Number_Of_Lines := Current_Number_Of_Lines + Count;
        end;
    end;
  Data : Protected_Data;
  procedure Fail                                                    is begin Data.Set_Do_Fail;                                   end;
  procedure Line            (Count : Int_32_Positive := 1)          is begin for I in 1..Count loop Put (END_LINE_16); end loop; end;
  procedure Put_Debug       (Item  : Char_16)                       is begin Put_Debug (Item & "");                              end;
  procedure Set_Input_Entry (Value : Str_16)                        is begin Data.Set_Input_Entry (Value);                       end;
  procedure Set_Debug       (Value : Bool)                          is begin Data.Set_Do_Put_Debug (Value);                      end; 
  procedure Set_Line_Size   (Value : Int_32_Positive)               is begin Data.Set_Line_Size (Value);                         end;
  procedure Set_Put         (Value : Ptr_Procedure_Put)             is begin Data.Set_Put (Value);                               end;
  procedure Set_Tasks       (Value : Int_32_Positive)               is begin Data.Set_Number_Of_Tasks (Value);                   end;
  procedure Put             (Item  : Char_16)                       is begin Put (Item & "");                                    end;                  
  procedure Put             (Item  : Str_16)                        is begin Data.Put (Item);                                    end;
  procedure Line            (Item  : Char_16)                       is begin Line (Item & "");                                   end;    
  procedure Line            (Item  : Str_16_Complex)                is begin Line (To_Str_16 (Item));                            end;
  procedure Line            (Item  : Str_16)                        is begin Put (Item); Line;                                   end;
  procedure Put_Debug       (Item  : Str_16)                        is begin if Data.Do_Put_Debug then Data.Put (Item); end if;  end;
  procedure Put_Debug_Line  (Item  : Str_16)                        is begin Put_Debug (Item); Line;                             end;   
  function Local            (Item  : Str_16) return Str_16          is (Data.Local (Item));
  function Get_Extension    (Path  : Str_16) return Str_16          is (Path (Index (Path, ".") + 1..Path'Last));
  function Get_Log                           return Str_16          is (Data.Get_Log);
  function Get_Input_Entry                   return Str_16          is (Data.Get_Input_Entry); 
  function Get_Lines                         return Int_64_Natural  is (Data.Get_Number_Of_Lines);
  function Get_Line_Size                     return Int_32_Positive is (Data.Get_Line_Size);
  function Get_Tasks                         return Int_32_Positive is (Data.Get_Number_Of_Tasks);
  function Do_Put_Debug                      return Bool            is (Data.Do_Put_Debug);
  function Do_Fail                           return Bool            is (Data.Do_Fail);
  procedure Title (Item : Str_16) is
    Space_Count : Int_32_Signed := 0;
    Count       : Int_32_Signed := Item'Length * 3 - Item'Length / 3;
    begin
      if Item'Length > Get_Line_Size then raise Title_Is_Too_Long; end if;
      Put                                    (Char_16'Val (16#250C#));
      for I in 1..Get_Line_Size - 2 loop Put (Char_16'Val (16#2500#)); end loop;
      Line                                   (Char_16'Val (16#2510#));
      Put                                    (Char_16'Val (16#2502#));
      for I in 1..Item'Length loop
        if Item (I) = ' ' then Space_Count := Space_Count + 1; end if;
      end loop;
      if Count + Space_Count >= Get_Line_Size then
        for I in 1..Get_Line_Size / 2 - Item'Length / 2 - 1 loop Put (" "); end loop;
        Put (Item);
        for I in 1..Get_Line_Size - Item'Length - (Get_Line_Size / 2 - Item'Length / 2) - 1 loop Put (" "); end loop;
      else
        for I in 1..Get_Line_Size / 2 - (Count + Space_Count) / 2 - 1 loop Put (" "); end loop;
        for I in 1..Item'Length loop
          Put (Item (I) & "  ");
          if Item (I) = ' ' then Put (" "); end if;
        end loop;
        for I in 1..Get_Line_Size - (Get_Line_Size / 2 - (Count + Space_Count) / 2 - 1) - Item'Length * 3 - Space_Count - 2 loop Put (" "); end loop;
      end if;
      Line                                   (Char_16'Val (16#2502#));
      Put                                    (Char_16'Val (16#2514#));
      for I in 1..Get_Line_Size - 2 loop Put (Char_16'Val (16#2500#)); end loop;
      Line                                   (Char_16'Val (16#2518#));
    end;
  function Local (Item : Str_16) return Str_16 is
    Result : Str_16 := Input_Output.Local(Item);
    begin
      if Result = NULL_Str_16 then
        if DO_PUT_Local_FAILURE then
          Put_Debug_Line ("Failed to Local """ & Item (Item'First..
            (if Item'Length >= FAILED_Local_PREVIEW_LENGTH then Item'first + FAILED_Local_PREVIEW_LENGTH - 1 else Item'Last)) & """");
        end if;
        return Item;
      end if;
      return Result;
    end;

  ------------
  -- Memory --
  ------------

  INITIAL_MEMORY_STATE : constant Memory_State := Import.Get_Memory_State;
  function Get_Initial_Memory_State return Memory_State is (INITIAL_MEMORY_STATE);
  function Get_Memory_State         return Memory_State renames Import.Get_Memory_Stat;

  ---------------
  -- Processor --
  ---------------

  function Get_Clock_Ticks return Int_64_Unsigned renames Import.Get_Clock_Ticks;
  package body Tasks is
      task type Task_Unsafe is entry Initialize (ID : in out Task_ID); end;
      type Ptr_Task_Unsafe is access all Task_Unsafe;
      procedure Finalize is new Ada.Unchecked_Deallocation (Task_Unsafe, Ptr_Task_Unsafe);
      task body Task_Unsafe is
        begin
          accept Initialize (Id : out Task_Id) do Id := Current_Task; end Initialize;
          begin Run; exception when Occurrence: others => Handle_Exception (Occurrence); end;
          Set_Number_Of_Tasks (Get_Number_Of_Tasks - 1);
        end;
      protected body Protected_Task is
pragma Warnings (Off); -- "Potentially blocking operating in protected type"
          procedure Initialize is
            begin
              if Current_Id /= NULL_TASK_ID and then not Is_Terminated (Current_id) then raise Task_Error with "Initialized without being finalized"; end if;
              Current_Task := new Task_Unsafe;
              Current_Task.Initialize (Current_Id);
              Set_Number_Of_Tasks (Get_Number_Of_Tasks + 1);
            end;
          procedure Finalize is
            begin
              if Current_Id = NULL_TASK_ID or else Is_Terminated (Current_id) then raise Task_Error with "Finalized without being initialized"; end if;
              Abort_Task (Current_Id);
              Current_Id := NULL_TASK_ID;
              Finalize (Current_Task);
              Set_Number_Of_Tasks (Get_Number_Of_Tasks - 1);
            end;
pragma Warnings (On);
          function Is_Running return Bool is
            begin
              return Current_Task /= null and not Is_Terminated(Current_Id);
            exception when others => return False; end;
        end;
    end;

  -------------
  -- Command --
  -------------

  MAXIMUM_POSSIBLE_VALUES_DISPLAYED : constant Int_32_Positive := 5;
  type Ptr_Function_Get      is access function  return Str_16;
  type Ptr_Procedure_Set     is access procedure (Item : Str_16);
  type Ptr_Procedure_Perform is access procedure (Parameters : Array_Str_16_Complex);
  type Variable_State is record
      Saved_Value : Str_16_Complex;
      Get         : Ptr_Function_Get;
      Set         : Ptr_Procedure_Set;
    end record;
  package Hashed_Variable              is new Hashed_Maps (Variable_State);
  package Hashed_Str_16_Complex        is new Hashed_Maps (Str_16_Complex);
  package Hashed_Ptr_Procedure_Perform is new Hashed_Maps (Ptr_Procedure_Perform);
  Duplicate : Exception;
  Parse     : Exception;
  Names     : Hashed_Map_Str_16_Complex.Protected_Map;
  Actions   : Hashed_Map_Ptr_Procedure_Perform.Protected_Map;
  Variables : Hashed_Map_Variable_State.Protected_Map;
  package body Variable is
      use Hashed_Map_Variable_State;
      type Control_State is new Controlled with null record;
      protected type Protected_Var_T is
          procedure Set (Value : in Var_T);
          function Get return Var_T;
        private
          Current : Var_T := Initial;
        end Protected_Var_T;
      procedure Initialize (Control : in out Control_State);
      procedure Finalize   (Control : in out Control_State);
      procedure Handle_Set (Value : Str_16);
      function Handle_Get  return Str_16;
      LOWER_NAME : constant Str_16 := To_Lower(Name);
      Control    : Control_State;
      Data       : Protected_Var_T;
      protected body Protected_Var_T is
          function Get return Var_T is (Current);
          procedure Set(Value : Var_T) is begin Current := Value; end;
        end Protected_Var_T;
      procedure Next     is begin null; end Next;
      procedure Previous is begin null; end Previous;
      procedure Set (value : Float_4_Percent) is begin null; end Set;
      function Get return Var_T is (Data.Get);
      procedure Set(Value : Var_T) is
        begin
          if Adjust /= null then Data.Set (Adjust.All(Get, Value));
          else Data.Set(Value); end if;
        end;
      procedure Initialize (Control : out Control_State) is
        begin
          if Actions.Has_Element (LOWER_NAME) or else (Variables.Element (LOWER_NAME).Set /= null or Variables.Element (LOWER_NAME).Get /= null) then
            raise Duplicate;
          end if;
          Handle_Set (To_Str_16 (Variables.Element (LOWER_NAME).Saved_Value));
          Variables.Replace (LOWER_NAME, (Variables.Element (LOWER_NAME).Saved_Value, Handle_Get'Unrestricted_Access, Handle_Set'Unrestricted_Access));
        exception
          --when Duplicate => raise Duplicate;
          when others => Variables.Insert (LOWER_NAME, (NULL_Str_16_COMPLEX, Handle_Get'Unrestricted_Access, Handle_Set'Unrestricted_Access));
        end;
      procedure Finalize (Control : out Control_State) is
        begin
          if Is_Saved then Variables.Replace (LOWER_NAME, (To_Str_16_Complex (Trim (Var_T'Wide_Image (Data.Get), Both)), null, null));
          else Variables.Delete (LOWER_NAME); end if;
        end;
      procedure Handle_Set (Value : Str_16) is
        begin
          Set (Var_T'Wide_Value (Value));
        exception when Constraint_Error =>
          for I in Var_T'Range loop
            if Value = Var_T'Wide_Image (I) then
              Set(I);
              exit;
            elsif I = Var_T'Last then
              Line (Local (INCORRECT_PARAMETER) & LOWER_NAME & ": " & Value);
              Line (Handle_Get);
            end if;
          end loop;
        end;
      function Handle_Get return Str_16 is
        Values : Str_16_Complex := To_Str_16_Complex (Trim (Var_T'Wide_Image (Var_T'first), Both));
        begin
          if Var_T'Pos (Var_T'Last) - Var_T'Pos (Var_T'first) > MAXIMUM_POSSIBLE_VALUES_DISPLAYED then
            Values := Values & ".." & Trim (Var_T'Wide_Image (Var_T'Last), Both);
          else
            for I in Var_T'Val (Var_T'Pos (Var_T'first) + 1)..Var_T'Last loop
              Values := Values & ", " & To_Str_16_Complex (Trim (Var_T'Wide_Image (I), Both));
            end loop;
          end if;
          return Local (Description)                                                & END_LINE_16 &
                 Local (CURRENT_VALUE)   & Trim (Var_T'Wide_Image (Data.Get), Both) & END_LINE_16 &
                 Local (POSSIBLE_VALUES) & To_Str_16 (Values);
        end;
    end Variable;
  package body Action is
      use Hashed_Map_Ptr_Procedure_Perform;
      type Control_State is new Controlled with null record;
      procedure Initialize              (Control : in out Control_State);
      procedure Finalize                (Control : in out Control_State);
      procedure Not_A_Formal_Subprogram (Parameters : Array_Str_16_Complex) renames Perform;
      LOWER_NAME : constant Str_16 := To_Lower (Name);
      Control : Control_State;
      procedure Finalize (Control : out Control_State) is begin Actions.Delete (LOWER_NAME); end;
      procedure Initialize (Control : out Control_State) is
        begin
          if Actions.Has_Element(LOWER_NAME) then raise Duplicate; end if;
          Actions.Insert(LOWER_NAME, Not_A_Formal_Subprogram'Unrestricted_Access);
        end;
    end;
  procedure Load (Path : Str_16) is
    begin
      null;
    end;
  procedure Handle (Text : Str_16) is
    Line    :          Array_Str_16_Complex := Split (Text);
    COMMAND : constant Str_16               := To_Str_16 (Line (1));
    begin
      if Actions.Has_Element (COMMAND) then Actions.Element (COMMAND).all ( (2..Line'Length));
      elsif Variables.Has_Element (COMMAND) then
        if Line'Length = 1 then
          if Variables.Element (COMMAND).Get /= null then Line (Variables.Element (COMMAND).Get.all); end if;
        elsif Variables.Element (COMMAND).Set /= null then Variables.Element (COMMAND).Set.all (To_Str_16 (Line (2))); end if;
      else raise Constraint_Error; end if;
    exception when others => Line (Local (NO_SUCH_VARIABLE_OR_ACTION));
    end;
  function Autocomplete (Text : Str_16; Limit : Int_32_Positive := 1) return Array_Str_16_Complex is
    begin
      return (NULL_Str_16_COMPLEX, NULL_Str_16_COMPLEX);
    end;
  procedure Load_Variables (Path : Str_16) is
    begin
      null;
    end;
  procedure Save_Variables (Path : Str_16) is
    begin
      null;
    end;

  -----------
  -- Input --
  -----------

  DURATION_FOR_MUTEX_WAIT : constant Duration := 0.0001;
  DURATION_BEFORE_POLLING : constant Duration := 0.002;
  package body Impulse is
      Duplicate            : Exception;
      Impulse_Out_Of_Scope : Exception;
      LOWER_NAME : Str_16_Complex := To_Lower (Name);
      function Totally_Not_A_Formal_Subprogram renames ;
      procedure Enable                                    is Impulse : Record_Impulse := Impulses.Element (LOWER_NAME); begin Impulse.Is_Enabled := True;  Impulses.Replace (LOWER_NAME, Impulse); end;
      procedure Disable                                   is Impulse : Record_Impulse := Impulses.Element (LOWER_NAME); begin Impulse.Is_Enabled := False; Impulses.Replace (LOWER_NAME, Impulse); end;
      procedure Finalize   (Gamepad : out Record_Gamepad) is                                                            begin if Is_Running.Get                   then raise Impulse_Out_Of_Scope; end if; Impulses.Delete (LOWER_NAME); end;
      procedure Initialize (Gamepad : out Record_Gamepad) is                                                            begin if Impulses.Has_Element(LOWER_NAME) then raise Duplicate;            end if; Impulses.Insert (LOWER_NAME,
                                                                                                                                (Totally_Not_A_Formal_Subprogram'Unrestricted_Access, Bindings'Unrestricted_Access, True)); end;
    end;
  function Keyboard        (Key     : Key_Kind;     Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Key_Kind,     Player, Combo, Key     => Key,     others => <>));
  function Gamepad         (Trigger : Trigger_Kind; Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Trigger_Kind, Player, Combo, Trigger => Trigger, others => <>));
  function Gamepad         (Stick   : Stick_Kind;   Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Stick_Kind,   Player, Combo, Stick   => Stick,   others => <>));
  function Gamepad         (Button  : Gamepad_Kind; Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Gamepad_Kind, Player, Combo, Gamepad => Button,  others => <>));
  function Mouse           (Button  : Mouse_Kind;   Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Mouse_Kind,   Player, Combo, Mouse   => Button,  others => <>));
  function Mouse                                   (Combo : Int_32_Natural := NO_COMBO; Player : Int_32_Positive := 1) return Record_Binding is ((Cursor_Kind,  Player, Combo,                     others => <>));
  function Get_Cursor                      return Record_Location                                renames Import.Get_Cursor;
  function Get_Devices                     return Map_Device.Unprotected.Map                     is (Devices.Get);
  function Get_Device      (Id : Int_Addr) return Record_Device                                  is (Devices.Element (Id));
  function Has_Device      (Id : Int_Addr) return Bool                                           is (Devices.Has_Element (Id));
  procedure Remove_Device  (Id : Int_Addr)                                                       is begin Devices.Delete (Id); end;
  procedure Add_Device     (Id : Int_Addr; Device   : Record_Device)                             is begin if not Players.Has_Element (Device.Player) then Players.Insert (Device.Player, (others => <>)); end if; Devices.Insert (Id, Device); end;
  procedure Set_Device     (Id : Int_Addr; Player   : Int_32_Positive := 1)                      is Device : Record_Device := Devices.Element (Id); begin Device.Player             := Player;            Devices.Replace (Id, Device); end;
  procedure Inject_Text    (Id : Int_Addr; Text     : Str_16_Complex)                            is Device : Record_Device := Devices.Element (Id); begin Device.Text               := Text;              Devices.Replace (Id, Device); end;
  procedure Inject_Cursor  (Id : Int_Addr; Location : Record_Location)                           is Device : Record_Device := Devices.Element (Id); begin Device.Cursor             := Location;          Devices.Replace (Id, Device); end;
  procedure Inject_Trigger (Id : Int_Addr; Trigger  : Trigger_Kind; Position : Float_32_Percent) is Device : Record_Device := Devices.Element (Id); begin Device.Triggers (Trigger) := Position;          Devices.Replace (Id, Device); end;
  procedure Inject_Stick   (Id : Int_Addr; Stick    : Stick_Kind;   Axis     : Cursor_State)     is Device : Record_Device := Devices.Element (Id); begin Device.Sticks (Stick)     := Axis;              Devices.Replace (Id, Device); end;
  procedure Inject_Button  (Id : Int_Addr; Button   : Mouse;   Is_Pressed : Bool)                is begin if Device.Mouse (Key).Is_Pressed    /= Is_Pressed then Device.Mouse (Key)    := (Is_Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Button  (Id : Int_Addr; Button   : Gamepad; Is_Pressed : Bool)                is begin if Device.Keyboard (Key).Is_Pressed /= Is_Pressed then Device.Keyboard (Key) := (Is_Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject_Key     (Id : Int_Addr; Key      : Key;     Is_Pressed : Bool)                is begin if Device.Keyboard (Key).Is_Pressed /= Is_Pressed then Device.Keyboard (Key) := (Is_Pressed, Clock); Devices.Replace (Id, Device); end if; end;
  procedure Inject (Binding : Record_Binding) is
    begin
      while Status.Is_Doing_Something loop delay DURATION_FOR_MUTEX_WAIT; end loop; Status.Set_Is_Doing_Something(True);
      case Binding.Kind is
        when Key_Kind     => Injection.Keyboard (Binding.Key)      := Binding.State;
        when Trigger_Kind => Injection.Triggers (Binding.Trigger)  := Binding.Position;
        when Stick_Kind   => Injection.Sticks   (Binding.Stick)    := Binding.Axis;
        when Gamepad_Kind => Injection.Gamepad  (Binding.Gamepad)  := Binding.State;
        when Mouse_Kind   => Injection.Mouse    (Binding.Mouse)    := Binding.State;
        when Cursor_Kind  => Injection.Cursor                      := Binding.Location;
        when Text_Kind    => if Is_In_Menu.Get then Injection.Text := Injection.Text & Binding.Text; end if;
      end case;
      Status.Set_Is_Doing_Something (False);
    end;
  procedure Set_Vibration (Player : Int_32_Positive := 1; Frequency_High, Frequency_Low : Float_4_Percent) is
    Current_Device : Ordered_Map_Record_Device.Cursor := Devices.First;
    begin
      while Devices.Has_Element (Current_Device) loop
        if Devices.Element (Current_Device).Player = Player then Import.Set_Vibratio n(Devices.Key (Current_Device), Frequency_High, Frequency_Low); end if;
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
          Line (Local ("Bind an impulse to an input value"));
          Line ("bind [player#] impulse value"));
        when 3 =>
          Player  := 1;
          Impulse := Impulses.Element (To_Str_16 (Lines(2)));
        when 4 =>
          Player := Line(2);
          Line(2).Delete;
          Impulse := Impulses.Element (To_Str_16 (Lines(3)));
        when 5 =>
      when others => null; end case;
    end;
  procedure Run is
    Last_Time : Time := Clock;
    begin
      Status.Set_Doing_Something (False);
      Import.Initialize;
      while Is_Running.Get and Import.Update loop
        if Is_Active.Get then
          for Player in Players loop
            Player := Injection;
            for Device in Devices loop
              Player := Players.Element (Device.Player);
              if Device.Text /= NULL_Str_16_COMPLEX then Player.Text := Player.Text & Device.Text; end if;
              case Device.Kind is
                when Keyboard_Device =>
                  for Key in Key_Kind'Range loop
                    if Player.Mouse (Button).Is_Pressed and not Device.Mouse (Button).Last < Player.Mouse (Button).Last) then 
                      Player.Keyboard (Key) := Device.Keyboard (Key);
                      case Key is
                        when Left_Shift_Key | Right_Shift_Key => Player.Keyboard (Shift_Key) := Player.Keyboard (Key);
                        when Left_Ctrl_Key  | Right_Ctrl_Key  => Player.Keyboard (Ctrl_Key)  := Player.Keyboard (Key);
                        when Left_Alt_Key   | Right_Alt_Key   => Player.Keyboard (Alt_Key)   := Player.Keyboard (Key);
                      when others => null; end case;
                    elsif not Device.Keyboard (Key).Is_Pressed and Device.Keyboard (Key).Last > Player.Keyboard (Key).Last then 
                      Player.Keyboard (Key) := Device.Keyboard (Key);
                    end if;
                  end loop;
                when Mouse_Device =>
                  for Button in Mouse_Kind'Range loop
                    if Device.Mouse (Button).Is_Pressed and Device.Keyboard (Key).Is_Pressed and not Player.Keyboard (Key).Is_Pressed or Device.Keyboard (Key).Last < Player.Keyboard (Key).Last) then 
                      Player.Mouse (Button) := Device.Mouse (Button);
                    end if;
                    if Device.Mouse (Button).Is_Pressed and Button in Horizontal_Wheel_Left_Key..Vertical_Wheel_Down_Key then
                      Device.Mouse (Button).Is_Pressed := False;
                    end if;
                  end loop;
                  Player.Cursor := (Player.Cursor.X + Device.Cursor.X, Player.Cursor.Y + Device.Cursor.Y);
                when Gamepad_Device =>
                  for Button in Gamepad_Kind'Range loop
                    if Device.Gamepad (Button).Is_Pressed and not Player.Gamepad (Button).Is_Pressed or Device.Gamepad (Button).Last < Player.Gamepad (Button).Last) then 
                      Player.Gamepad (Button) := Device.Gamepad (Button); 
                    end if;
                  end loop;
                  for Kind in Stick_Kind'Range loop
                    Player.Stick (Kind) := (Player.Stick (Kind).X + Device.Stick (Kind).X, Player.Stick (Kind).Y + Device.Stick (Kind).Y);
                  end loop;
                  for Kind in Trigger'Range loop
                    if Player.Trigger (Kind) + Device.Trigger (Kind) > 100.0 then Player.Trigger (Kind) := 100.0;
                    else Player.Trigger (Kind) := Player.Trigger (Kind) + Device.Trigger (Kind); end if;
                  end loop;
              end case;
            end loop;
            for Impulse in Impulses loop
              for Binding in Impulse.Bindings loop
                Player := Players.Element (Binding.Player);
                case Binding.Kind is
                  when Trigger_Kind => if Player.Triggers (Binding.Trigger)            /= Binding.Position         then Binding.Position := Player.Trigger  (Binding.Trigger); goto <<CDMBINE>>; end if;
                  when Stick_Kind   => if Player.Sticks   (Binding.Stick)              /= Binding.Axis             then Binding.Axis     := Player.Sticks   (Binding.Stick);   goto <<CDMBINE>>; end if;
                  when Gamepad_Kind => if Player.Gamepad  (Binding.Gamepad).Is_Pressed /= Binding.State.Is_Pressed then Binding.State    := Player.Gamepad  (Binding.Gamepad); goto <<CDMBINE>>; end if;
                  when Mouse_Kind   => if Player.Mouse    (Binding.Mouse).Is_Pressed   /= Binding.State.Is_Pressed then Binding.State    := Player.Mouse    (Binding.Mouse);   goto <<CDMBINE>>; end if;
                  when Key_Kind     => if Player.Keyboard (Binding.Key).Is_Pressed     /= Binding.State.Is_Pressed then Binding.State    := Player.Keyboard (Binding.Key);     goto <<CDMBINE>>; end if;
                  when Cursor_Kind  => if Player.Cursor                                /= Binding.Location         then Binding.Location := Player.Cursor;                     goto <<CDMBINE>>; end if;
                  when Text_Kind    => if Player.Text                                  /= Binding.Text             then Binding.Text     := Player.Text;                       goto <<CDMBINE>>; end if;
                end case;
goto <<COMBINE>>; <<CDMBINE>>
                Impulse.Bindings.Replace (Current_Binding, Binding);
                if Binding.Combo /= NO_COMBO then
                  for Other_Binding of Impulse.Bindings.Get loop
                    if Other_Binding.Combo = Binding.Combo then
                      case Other_Binding.Kind is
                        when Text_Kind                 => if Other_Binding.Text = NULL_Str_16_COMPLEX then goto <<COMBINE>>; end if;
                        when Stick_Kind | Cursor_Kind  => if Other_Binding.Location = (others => <>)    then goto <<COMBINE>>; end if;
                        when Mouse_Kind | Gamepad_Kind => if not Other_Binding.State.Is_Pressed         then goto <<COMBINE>>; end if;
                        when Trigger_Kind              => if Other_Binding.Position = 0.0               then goto <<COMBINE>>; end if;
                      end case;
                    end if;
                  end loop;
                end if;
                Impulse.Trip (Binding);
                if Binding.Kind = Text_Kind then Binding.Text := NULL_Str_16_COMPLEX; Impulse.Bindings.Replace (Current_Binding, Binding); end if;
<<COMBINE>>
              end loop;
            end loop;
          end loop;
          Injection.Text                                                 := NULL_Str_16_COMPLEX;
          Injection.Mouse (Vertical_Wheel_Down_Key).Is_Pressed   := False;
          Injection.Mouse (Horizontal_Wheel_Left_Key).Is_Pressed := False;
          Status.Set_Is_Doing_Something (False);
        end if;
        delay DURATION_TO_WAIT_BEFORE_POLLING - (Clock - Last_Time);
        Last_Time := Clock;
      end loop;
      Import.Finalize;
    end Run;


  ---------------
  -- Windowing --
  ---------------

  function Get_Borders    return Vector_Border_State.Unprotected.Vector renames Import.Get_Borders;
  function Get_Decoration return Border_State                           renames Import.Get_Decoration;
  procedure Preform_Exit_To_Menu (Binding : Record_Binding) is
    begin
      if Binding.Kind = Mouse_Kind and then Binding.State.Is_Pressed then
        Is_In_Menu.Set (not Is_In_Menu.Get);
      end if;
    end;
  procedure Preform_Toggle_Fullscreen (Binding : Record_Binding) is
    begin
      if Binding.State.Is_Pressed then
        State.Set ((case State.Get is when Multi_Monitor_State | Fullscreen_State => Windowed_State, when Windowed_State => Fullscreen_State));
      end if;
    end;
  procedure Preform_Detect_Menu_Mode_Entry (Binding : Record_Binding) is
    Location : Record_Location := Get_Cursor;
    Border   : Border_State   := Get_Borders.Element (1);
    begin
      Impulse_Detect_Menu_Mode_Entry.Disable;
    end;
  procedure Change_State (Kind : Change) is
    begin
      case Kind is
        when Iconic_Change     => Is_Iconized.Set (True);
        when Windowed_Change   => Is_Iconized.Set (False); State.Set(Windowed_State);
        when Fullscreen_Change => Is_Iconized.Set (False); State.Set(Fullscreen_State);
      end case;
    end;
  function Get_Normalized_Cursor return Record_Location is
    begin
      return (others => <>);
    end;
  procedure Activate (Do_Activate, Do_Detect_Click : Bool; X, Y : Int_64_Signed) is
    begin
      if Do_Activate then
        Is_Active.Set (True);
        if Is_In_Menu.Get then Import.Set_Cursor_Style(Inactive_Cursor); end if;
      else
        Is_Active.Set (False);
        Import.Set_Cursor_Style (System_Cursor);
        --Import.Clip_Mouse(Undo => True);
        if State.Get /= Windowed_State then
          --if State.Get = Multi_Monitor_State then Import.Finalize_Multi_Monitor; end if;
          Import.Iconize;
        end if;
      end if;
    end;
  function Resize (Kind : Resize; Border : Border_State) return Border_State is
    Result         : Border_State  := Border;
    Decoration     : Border_State  := Get_Decoration;
    Extra_Width    : Int_64_Signed := Decoration.Right  + Decoration.Left;
    Extra_Height   : Int_64_Signed := Decoration.Bottom + Decoration.Top;
    Current_Width  : Int_64_Signed := (if Border.Right  - Border.Left - Extra_Width  < MINIMUM_FACTOR then MINIMUM_FACTOR else Border.Right - Border.Left - Extra_Width);
    Current_Height : Int_64_Signed := (if Border.Bottom - Border.Top  - Extra_Height < Int_64_Signed (Int_32_Positive (MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get / Aspect_Narrow_Horizontal.Get) then
                                      Int_64_Signed (Int_32_Positive (MINIMUM_FACTOR) * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get) else Border.Bottom - Border.Top  - Extra_Height);
    Maximum_Width  : Int_64_Signed := Int_64_Signed (Int_32_Positive (Current_Height) * Aspect_Narrow_Horizontal.Get / Aspect_Narrow_Vertical.Get);
    Maximum_Height : Int_64_Signed := Int_64_Signed (Int_32_Positive (Current_Width)  * Aspect_Wide_Vertical.Get     / Aspect_Wide_Horizontal.Get);
    Minimum_Width  : Int_64_Signed := Int_64_Signed (Int_32_Positive (Current_Height) * Aspect_Wide_Horizontal.Get   / Aspect_Wide_Vertical.Get);
    Minimum_Height : Int_64_Signed := Int_64_Signed (Int_32_Positive (Current_Width)  * Aspect_Narrow_Vertical.Get   / Aspect_Narrow_Horizontal.Get);
    Fit_Width      : Int_64_Signed := (if Current_Width  > Maximum_Width  then Maximum_Width  elsif Current_Width  < Minimum_Width  then Minimum_Width  else Current_Width);
    Fit_Height     : Int_64_Signed := (if Current_Height > Maximum_Height then Maximum_Height elsif Current_Height < Minimum_Height then Minimum_Height else Current_Height);
    Resize_Factor  : Int_64_Signed := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) / 2;
    Resize_Extra   : Int_64_Signed := (Fit_Width + Extra_Width - (Border.Right - Border.Left)) rem 2;
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
      while Import.Update and Is_Running.Get loop null;
        --Render_Backend;
      end loop;
    end;
  Border        : Border_State;
  Current_Menu  : Bool  := Is_In_Menu.Get;
  Current_State : State := State.Get;
  begin
    Title (Local ("CONFIGURATION"));
    Line;
    Put (Local ("Loading ") & PATH_CONFIGURATION & "..."); Load_Variables (PATH_CONFIGURATION); Line (Local ("OK!"));
    Put (Local ("Loading ") & PATH_LOCALIZATION  & "..."); Load_Sheet     (PATH_LOCALIZATION);  Line (Local ("OK!"));
    Line;
    Title (Local ("INFORMATION"));
    Line;
    Line (Local ("Version: ")                   & System'Wide_Image          (SPECIFICS.Version));
    Line (Local ("Username: ")                  & To_Str_16                  (SPECIFICS.Username));
    Line (Local ("Directory: ")                 & To_Str_16                  (SPECIFICS.Path));
    Line (Local ("Name: ")                      & To_Str_16                  (SPECIFICS.Name));
    Line (Local ("Application bit size")        & Int_32_Signed'Wide_Image   (WORD_SIZE));
    Line (Local ("System bit size:")            & Int_32_Positive'Wide_Image (SPECIFICS.Bit_Size));
    Line;
    Title (Local ("CLIPBOARD"));
    Line;
    Line (Local ("Load:")                       & Float_4_Percent'Wide_Image     (Initial.Load));
    Line;
    Title (Local ("PROCESSOR"));
    Line;
    Title (Local ("MEMORY"));
    Line;
    Line (Local ("Load:")                       & Get_Initial.Load'Img);
    Line (Local ("Disk total:")                 & Initial.Disk_Bytes_Total'Img);
    Line (Local ("Disk available:")             & Initial.Disk_Bytes_Available'Img);
    Line (Local ("Physical total:")             & Initial.Physical_Bytes_Total'Img);
    Line (Local ("Physical available:")         & Initial.Physical_Bytes_Available'Img);
    Line (Local ("Page file total:")            & Initial.Page_File_Bytes_Total'Img);
    Line (Local ("Page file available:")        & Initial.Page_File_Bytes_Available'Img);
    Line (Local ("Virtual total:")              & Initial.Virtual_Bytes_Total'Img);
    Line (Local ("Virtual available:")          & Initial.Virtual_Bytes_Available'Img);
    Line (Local ("Virtual available extended:") & Initial.Virtual_Bytes_Available_Extended'Img);
    Line;
    Line (Local ("Load:")                       & Float_4_Percent'Wide_Image (Initial.Load));
    Line;
    Title (Local ("INPUT"));
    Line;
    Line (Local ("Load:")                       & Float_4_Percent'Wide_Image     (Initial.Load));
    Line;
    Title (Local ("GRAPHICS"));
    Line;
    Line (Local ("Load:")                       & Float_4_Percent'Wide_Image     (Initial.Load));
    Line;
    Title (Local ("SOUND"));
    Line;
    Line (Local ("Load:")                       & Float_4_Percent'Wide_Image     (Initial.Load));
    Line;
      Import.Assert_Only_Instance;
      Import.Initialize;
      Detect_Menu_Mode_Entry.Bindings.Append(Mouse(Left_Mouse));
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
              Width  => Width.Get  + Int_32_Positive (Border.Right  + Border.Left),
              Height => Height.Get + Int_32_Positive (Border.Bottom + Border.Top));
        end case;
        Initialize(1);
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
  end;
