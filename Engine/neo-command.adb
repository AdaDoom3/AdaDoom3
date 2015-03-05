package body Neo.Command is
  package body Variable is
      use Hashed_Map_Record_Variable;
      protected body Protected_Type_To_Vary is
          function Get return Type_To_Vary is begin return Current; end Get;
          procedure Set(Value : in Type_To_Vary) is begin Current := Value; end Set;
        end Protected_Type_To_Vary;
      procedure Next     is begin null; end Next;
      procedure Previous is begin null; end Previous;
      procedure Set (value : in Float_4_Percent) is begin null; end Set;
      function Get return Type_To_Vary is begin return Data.Get; end Get;
      procedure Set(Value : in Type_To_Vary) is
        begin
          if Adjust /= null then Data.Set(Adjust.all(Get, Value));
          else Data.Set(Value); end if;
        end Set;
      procedure Initialize(Controller : in out Record_Controller) is
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
      procedure Finalize(Controller : in out Record_Controller) is
        begin
          if Is_Saved then Variables.Replace(LOWER_NAME, (To_String_2_Unbounded(Trim(Type_To_Vary'wide_image(Data.Get), Both)), null, null));
          else Variables.Delete(LOWER_NAME); end if;
        end Finalize;
      procedure Handle_Set(Value : in String_2) is
        begin
          Set(Type_To_Vary'wide_value(Value));
        exception when Constraint_Error =>
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
          return Localize(Description)                                                     & END_LINE_2 &
                 Localize(CURRENT_VALUE)   & Trim(Type_To_Vary'wide_image(Data.Get), Both) & END_LINE_2 &
                 Localize(POSSIBLE_VALUES) & To_String_2(Values);
        end Handle_Get;
    end Variable;
  package body Action is use Hashed_Map_Access_Procedure_Perform;
      procedure Finalize(Controller : in out Record_Controller) is begin Actions.Delete(LOWER_NAME); end Finalize;
      procedure Initialize(Controller : in out Record_Controller) is
        begin
          if Actions.Has_Element(LOWER_NAME) then raise Duplicate; end if;
          Actions.Insert(LOWER_NAME, Not_A_Formal_Subprogram'unrestricted_access);
        end Initialize;
    end Action;
  procedure Load(Path : in String_2) is
    begin
      null;
    end Load;
  procedure Handle(Text : in String_2) is
    Line    :          Array_String_2_Unbounded := Split(Text);
    COMMAND : constant String_2                 := To_String_2(Line(1));
    begin
      if Actions.Has_Element(COMMAND) then Actions.Element(COMMAND).all(Line(2..Line'length));
      elsif Variables.Has_Element(COMMAND) then
        if Line'length = 1 then
          if Variables.Element(COMMAND).Get /= null then Put_Line(Variables.Element(COMMAND).Get.all); end if;
        elsif Variables.Element(COMMAND).Set /= null then Variables.Element(COMMAND).Set.all(To_String_2(Line(2))); end if;
      else raise Constraint_Error; end if;
    exception when others => Put_Line(Localize(NO_SUCH_VARIABLE_OR_ACTION));
    end Handle;
  function Autocomplete(Text : in String_2; Limit : in Integer_4_Positive := 1) return Array_String_2_Unbounded is
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
