package body Neo.System is
  package body Import is separate;
  package body Tasks is
      task body Task_Unsafe is
        begin
          accept Initialize(Id : in out Task_Id) do Id := Current_Task; end Initialize;
          Run;
          Set_Number_Of_Tasks(Get_Number_Of_Tasks - 1);
        exception when Occurrence: others =>
          Handle_Exception(Occurrence);
          Set_Number_Of_Tasks(Get_Number_Of_Tasks - 1);
        end Task_Unsafe;
      protected body Protected_Task is
          pragma Warnings(Off); -- Potentially blocking operating in protected type
          procedure Initialize is
            begin
              if Current_Id /= NULL_TASK_ID and then not Is_Terminated(Current_id) then raise Task_Initialized_Without_Being_Finalized; end if;
              Current_Task := new Task_Unsafe;
              Current_Task.Initialize(Current_Id);
              Set_Number_Of_Tasks(Get_Number_Of_Tasks + 1);
            end Initialize;
          procedure Finalize is
            begin
              if Current_Id = NULL_TASK_ID or else Is_Terminated(Current_id) then raise Task_Finalized_Without_Begin_Initialized; end if;
              Abort_Task(Current_Id);
              Current_Id := NULL_TASK_ID;
              Finalize(Current_Task);
              Set_Number_Of_Tasks(Get_Number_Of_Tasks - 1);
            end Finalize;
          pragma Warnings(On);
          function Is_Running return Boolean is
            begin
              return Current_Task /= null and not Is_Terminated(Current_Id);
            exception when others => return False;
            end Is_Running;
        end Protected_Task;
    end Tasks;
  procedure Assert        (Value : in Integer_4_Signed_C)   renames Import.Assert;
  procedure Assert        (Value : in Address)              is begin if Value = NULL_ADDRESS then raise Call_Failure; end if;                                                                                                                   end Assert;
  procedure Assert        (Value : in Boolean)              is begin if not Value then raise Call_Failure; end if;                                                                                                                              end Assert;
  procedure Assert_Dummy  (Value : in Integer_4_Signed_C)   is begin null;                                                                                                                                                                      end Assert_Dummy;
  procedure Assert_Dummy  (Value : in Integer_Address)      is begin null;                                                                                                                                                                      end Assert_Dummy;
  procedure Assert_Dummy  (Value : in Integer_4_Unsigned_C) is begin null;                                                                                                                                                                      end Assert_Dummy;
  procedure Assert_Dummy  (Value : in Address)              is begin null;                                                                                                                                                                      end Assert_Dummy;
  procedure Assert_Dummy  (Value : in Boolean)              is begin null;                                                                                                                                                                      end Assert_Dummy;
  procedure Set_Alert     (Value : in Boolean)              is begin Alert_Status.Set_Is_Doing_Something(Value); Import.Set_Alert(Value); exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SET_ALERT));                            end Set_Alert;
  procedure Execute       (Path : in String_2; Do_Fullscreen : in Boolean := False) is begin Import.Execute(Path, Do_Fullscreen);         exception when Call_Failure => Put_Debug_Line(Localize(FAILED_EXECUTE) & Path);                       end Execute;
  procedure Open_Text     (Path : in String_2)              is begin Import.Open_Text(Path);                                              exception when Call_Failure => Put_Debug_Line(Localize(FAILED_OPEN) & Path);                          end Open_Text;
  procedure Open_Webpage  (Path : in String_2)              is begin Import.Open_Webpage(Path);                                           exception when Call_Failure => Put_Debug_Line(Localize(FAILED_OPEN) & Path);                          end Open_Webpage;
  function Get_Specifics  return Record_Specifics           is begin return Import.Get_Specifics;                                         exception when Call_Failure => Put_Debug_Line(Localize(FAILED_GET_SPECIFICS)); return (others => <>); end Get_Specifics;
  function Is_Alerting    return Boolean                    is begin return Alert_Status.Is_Doing_Something;                                                                                                                                    end Is_Alerting;
  function Get_Last_Error return String_2                   is begin return Localize(PREFIX_ERROR_NUMBER) & Trim(Integer_4_Unsigned'wide_image(Import.Get_Last_Error), Both);                                                                   end Get_Last_Error;
  function Is_Okay(Name : in String_2; Message : in String_2; Buttons : in Enumerated_Buttons := Okay_Button; Icon : in Enumerated_Icon := No_Icon) return Boolean is
    begin
      return Import.Is_Okay(Name, Message, Buttons, Icon);
    exception when Call_Failure => Put_Debug_Line(Localize(FAILED_IS_OKAY)); return False;
    end Is_Okay;
  function Is_Supported(Requirements : in Record_Requirements) return Boolean is
    begin
      if SPECIFICS.Version    in Enumerated_Linux_System'range     then return SPECIFICS.Version >= Requirements.Minimum_Linux;
      elsif SPECIFICS.Version in Enumerated_Windows_System'range   then return SPECIFICS.Version >= Requirements.Minimum_Windows;
      elsif SPECIFICS.Version in Enumerated_Macintosh_System'range then return SPECIFICS.Version >= Requirements.Minimum_Macintosh; end if;
      return False;
    end Is_Supported;
  procedure Handle_Exception(Occurrence : in Exception_Occurrence) is
    begin
      Set_Errors(
        To_String_2(Exception_Name(Occurrence))    & END_LINE_2 &
        To_String_2(Exception_Message(Occurrence)) & END_LINE_2 &(
        if Exception_Name(Occurrence) = "NEO.SYSTEM.CALL_FAILURE" then Get_Last_Error & END_LINE_2 else NULL_STRING_2));
      Put_Line(Get_Errors);
      Is_Running.Set(False);
    end Handle_Exception;
  --function Localize(Item : in String_2) return String_2 is
  --  Result : String_2 := Input_Output.Localize(Item);
  --  begin
  --    if Result = NULL_STRING_2 then
  --      if DO_PUT_LOCALIZE_FAILURE then
  --        Put_Debug_Line(FAILED_LOCALIZE_PREFIX & Item(Item'first..(
  --          if Item'length >= FAILED_LOCALIZE_PREVIEW_LENGTH then Item'first + FAILED_LOCALIZE_PREVIEW_LENGTH - 1 else Item'last)) & FAILED_LOCALIZE_POSTFIX);
  --      end if;
  --      return Item;
  --    end if;
  --    return Result;
  -- end Localize;
begin
  Put_Title(Localize("SYSTEM"));
  New_Line;
  Put_Line(Localize("Version: ")            & Enumerated_System'wide_image(SPECIFICS.Version));
  Put_Line(Localize("Username: ")           & To_String_2(SPECIFICS.Username));
  Put_Line(Localize("Directory: ")          & To_String_2(SPECIFICS.Path));
  Put_Line(Localize("Name: ")               & To_String_2(SPECIFICS.Name));
  Put_Line(Localize("Application bit size") & Integer_4_Signed'wide_image(WORD_SIZE));
  Put_Line(Localize("System bit size:")     & Integer_4_Positive'wide_image(SPECIFICS.Bit_Size));
  New_Line;
end Neo.System;
