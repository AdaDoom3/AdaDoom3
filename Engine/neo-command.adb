package body Neo.Command is
    procedure Test is
      procedure Put_Success(Parameters : in String_2) is begin Put_Line(Localize("success") & Parameters); end Put_Success;
      procedure Adjust(P, C : in Boolean) is
        begin
          Put_Line(Localize("Variable_Test is being adjusted from ") & Boolean'wide_image(P) & Localize(" to ") & Boolean'wide_image(C));
        end Adjust;
      package Y is new Action("Command_Test", Put_Success);
      package X is new Variable("Variable_Test",  "Its a test",       Boolean, False, Adjust => Adjust'access);
      package Z is new Variable("Variable_Test2", "Its another test", Integer, 0);
      begin
        Put_Title(Localize("COMMAND TEST"));
        Put(Localize("An error message for unknown command: "));
        Handle("Makeanerror!");
        Handle("Variable_Test True123");
        Handle("Variable_Test True");
        Put_Line(Localize("Value after handle: ") & Boolean'wide_image(X.Get));
        X.Set(False);
        Put_Line(Localize("Value after set: ") & Boolean'wide_image(X.Get));
        Handle("Variable_Test2");
        Put(Localize("The command system executed with "));
        Handle("Command_Test !");
      end Test;
    package body Variable is
        use Mapped_Variables;
        protected body Protected_Type_To_Vary is
            function Get return Type_To_Vary is begin return Current; end Get;
            procedure Set(Item : in Type_To_Vary) is begin Current := Item; end Set;
          end Protected_Type_To_Vary;
        function Get return Type_To_Vary is begin return Data.Get; end Get;
        procedure Set(Value : in Type_To_Vary) is Previous : Type_To_Vary := Get;
          begin
            Data.Set(Value);
            if Adjust /= null then
              Adjust.all(Previous, Get);
            end if;
          end Set;
        function Handle_Get return String_2 is
          Values : String_2_Unbounded := To_String_2_Unbounded(Trim(Type_To_Vary'wide_image(Type_To_Vary'first), Both));
          begin
            if Type_To_Vary'pos(Type_To_Vary'last) - Type_To_Vary'pos(Type_To_Vary'first) > MAXIMUM_POSSIBLE_VALUES_DISPLAYED then
              Values := Values & ".." & Trim(Type_To_Vary'wide_image(Type_To_Vary'last), Both);
            else
              for I in Type_To_Vary'val(Type_To_Vary'pos(Type_To_Vary'first) + 1)..Type_To_Vary'last loop
                Values := Values & ", " & To_String_2_Unbounded(Trim(Type_To_Vary'wide_image(I), Both));
              end loop;
            end if;
            return LOWER_NAME & ": " & Localize(Description)                                 & END_LINE_2 &
                   Localize(CURRENT_VALUE)   & Trim(Type_To_Vary'wide_image(Data.Get), Both) & END_LINE_2 &
                   Localize(POSSIBLE_VALUES) & To_String_2(Values);
          end Handle_Get;
        procedure Handle_Set(Value : in String_2) is
          begin
            Set(Type_To_Vary'wide_value(Value));
          exception
            when Constraint_Error => -- Has to be failed input or an enumeration
              for I in Type_To_Vary'range loop
                if Value = Type_To_Vary'wide_image(I) then
                  Set(I);
                  exit;
                elsif I = Type_To_Vary'last then
                  Put_Line(Localize(INCORRECT_PARAMETER) & LOWER_NAME & ": " & Value);
                  Put_Line(Handle_Get);
                end if;
              end loop;
          end Handle_Set;
        overriding procedure Initialize(Controller : in out Record_Controller) is
          begin
            if Actions.Has_Element(LOWER_NAME) or else (Variables.Element(LOWER_NAME).Set /= null or Variables.Element(LOWER_NAME).Get /= null) then
              raise Duplicate;
            end if;
            Handle_Set(To_String_2(Variables.Element(LOWER_NAME).Saved_Value));
            Variables.Replace(LOWER_NAME, (Variables.Element(LOWER_NAME).Saved_Value, Handle_Get'unrestricted_access, Handle_Set'unrestricted_access));
          exception
            --when Duplicate => raise Duplicate;
            when others => Variables.Insert(LOWER_NAME, (NULL_STRING_2_UNBOUNDED, Handle_Get'unrestricted_access, Handle_Set'unrestricted_access));
          end Initialize;
        overriding procedure Finalize(Controller : in out Record_Controller) is
          begin
            if Is_Saved then
              Variables.Replace(LOWER_NAME, (To_String_2_Unbounded(Trim(Type_To_Vary'wide_image(Data.Get), Both)), null, null));
            else
              Variables.Delete(LOWER_NAME);
            end if;
            Put_Debug_Line(Localize(VARIABLE_OUT_OF_SCOPE) & LOWER_NAME);
          end Finalize;
      end Variable;
    package body Action is
        use Mapped_Actions;
        overriding procedure Finalize(Controller : in out Record_Controller) is begin Actions.Delete(LOWER_NAME); end Finalize;
        overriding procedure Initialize(Controller : in out Record_Controller) is
          begin
            if Actions.Has_Element(LOWER_NAME) then raise Duplicate; end if;
            Actions.Insert(LOWER_NAME, Not_A_Formal_Subprogram'unrestricted_access);
          end Initialize;
      end Action;
    procedure Handle(Input : in String_2) is
      TRIMMED    : constant String_2 := To_Lower(Trim(Input, Both));
      SPLIT      : constant Natural  := Index(TRIMMED, " ");
      COMMAND    : constant String_2 := TRIMMED(TRIMMED'first..(if SPLIT = 0 then TRIMMED'last else SPLIT - 1));
      PARAMETERS : constant String_2 := (if SPLIT = 0 then NULL_STRING_2 else Trim(TRIMMED(SPLIT..TRIMMED'last), Both));
      begin
        if Actions.Has_Element(COMMAND) then
          Actions.Element(COMMAND).all(PARAMETERS);
        elsif Variables.Has_Element(COMMAND) then
          if PARAMETERS'length = 0 then
            if Variables.Element(COMMAND).Get /= null then Put_Line(Variables.Element(COMMAND).Get.all); end if;
          elsif Variables.Element(COMMAND).Set /= null then
            Variables.Element(COMMAND).Set.all(PARAMETERS);
          end if;
        else
          raise Constraint_Error;
        end if;
      exception when others => Put_Line(Localize(NO_SUCH_VARIABLE_OR_ACTION));
      end Handle;
  --  function Localize(
  --    Item : in String_2)
  --    return String_2
  --    is
  --    Result : String_2 := Input_Output.Localize(Item);
  --    begin
  --      if Result = NULL_STRING_2 then
  --        if DO_PUT_LOCALIZE_FAILURE then
  --          Put_Debug_Line(
  --            FAILED_LOCALIZE_PREFIX & Item(Item'first..(
  --              if Item'length >= FAILED_LOCALIZE_PREVIEW_LENGTH then
  --                Item'first + FAILED_LOCALIZE_PREVIEW_LENGTH - 1
  --              else
  --                Item'last)) &
  --            FAILED_LOCALIZE_POSTFIX);
  --        end if;
  --        return Item;
  --      end if;
  --      return Result;
  --   end Localize;
    function Autocomplete(Input : in String_2; Limit : in Integer_4_Positive := 1) return Array_String_2_Unbounded is
      begin
        return (NULL_STRING_2_UNBOUNDED, NULL_STRING_2_UNBOUNDED);
      end Autocomplete;
    procedure Load_Variables(Path : in String_2) is
      begin
        null;
      end Load_Variables;
    procedure Save_Variables(Path : in String_2) is
      begin
        null;
      end Save_Variables;
  end Neo.Command;
