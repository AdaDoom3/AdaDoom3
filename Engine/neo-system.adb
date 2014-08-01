package body Neo.System is
    procedure Test is
      begin
        Put_Title(Localize("SYSTEM TEST"));
        Put_Line(Localize("Version: ")            & Enumerated_System'wide_image(SPECIFICS.Version));
        Put_Line(Localize("Username: ")           & To_String_2(SPECIFICS.Username));
        Put_Line(Localize("Directory: ")          & To_String_2(SPECIFICS.Path));
        Put_Line(Localize("Name: ")               & To_String_2(SPECIFICS.Name));
        Put_Line(Localize("Application bit size") & Integer_4_Signed'wide_image(WORD_SIZE));
        Put_Line(Localize("System bit size:")     & Integer_4_Positive'wide_image(SPECIFICS.Bit_Size));
        for I in 1..1000 loop
          exit when not
            Is_Okay(
              Name    => "" & Character_2'val(16#221E#),
              Message => Localize("Continue?!"),
              Buttons => Yes_No_Buttons,
              Icon    => Enumerated_Icon'val(I mod (1 + Enumerated_Icon'pos(Enumerated_Icon'last))));
        end loop;
        --if not Is_Alerting then
        --  Set_Alert(True);
        --end if;
        --delay 10.0;
        --Set_Alert(False);
        if SPECIFICS.Version in Enumerated_Windows_System'range then
          Put_Line(Localize("Launching task manager..."));
          --Execute("taskmgr");
        elsif SPECIFICS.Version in Enumerated_Linux_System'range then
          Put_Line(Localize("Launching ???..."));
          --Execute_Application("???");
        elsif SPECIFICS.Version in Enumerated_Macintosh_System'range then
          Put_Line(Localize("Launching ???..."));
          --Execute_Application("???");
        end if;
        Put_Line(Localize("Opening ") & "google.com...");
        --Open_Webpage("http://www.google.com");
      end Test;
    package body Import is separate;
    procedure Assert          (Value : in Integer_4_Signed_C) renames Import.Assert;
    procedure Assert          (Value : in Address)            is begin if Value = NULL_ADDRESS then raise Call_Failure; end if; end Assert;
    procedure Assert          (Value : in Boolean)            is begin if not Value then raise Call_Failure; end if;            end Assert;
    procedure Assert_Dummy    (Value : in Integer_4_Signed_C) is begin null;                                                    end Assert_Dummy;
    procedure Assert_Dummy    (Value : in Boolean)            is begin null;                                                    end Assert_Dummy;
    procedure Assert_Dummy    (Value : in Address)            is begin null;                                                    end Assert_Dummy;
    function Is_Okay(Name : in String_2; Message : in String_2; Buttons : in Enumerated_Buttons := Okay_Button; Icon : in Enumerated_Icon := No_Icon) return Boolean is
      begin
        return Import.Is_Okay(Name, Message, Buttons, Icon);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_IS_OKAY)); return False;
      end Is_Okay;
    function Is_Alerting return Boolean is
      begin
        return Alert_Status.Is_Doing_Something;
      end Is_Alerting;
    procedure Set_Alert(Value : in Boolean) is
      begin
        Alert_Status.Set_Is_Doing_Something(Value);
        Import.Set_Alert(Value);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SET_ALERT));
      end Set_Alert;
    function Get_Last_Error return String_2 is
      begin
        return Localize(PREFIX_ERROR_NUMBER) & Trim(Integer_4_Unsigned'wide_image(Import.Get_Last_Error), Both);
      end Get_Last_Error;
    procedure Open_Text(Path : in String_2) is
      begin
        Import.Open_Text(Path);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_OPEN) & Path);
      end Open_Text;
    procedure Open_Webpage(Path : in String_2) is
      begin
        Import.Open_Webpage(Path);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_OPEN) & Path);
      end Open_Webpage;
    procedure Execute(Path : in String_2; Do_Fullscreen : in Boolean := False) is
      begin
        Import.Execute(Path, Do_Fullscreen);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_EXECUTE) & Path);
      end Execute;
    function Get_Specifics return Record_Specifics is
      begin
        return Import.Get_Specifics;
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_GET_SPECIFICS)); return (others => <>);
      end Get_Specifics;
    function Is_Supported(Requirements : in Record_Requirements) return Boolean is
      begin
        if SPECIFICS.Version    in Enumerated_Linux_System'range     then return SPECIFICS.Version >= Requirements.Minimum_Linux;
        elsif SPECIFICS.Version in Enumerated_Windows_System'range   then return SPECIFICS.Version >= Requirements.Minimum_Windows;
        elsif SPECIFICS.Version in Enumerated_Macintosh_System'range then return SPECIFICS.Version >= Requirements.Minimum_Macintosh; end if;
        return False;
      end Is_Supported;
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
    null;--Set_Localize(Localize'access);
  end Neo.System;
