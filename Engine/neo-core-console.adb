
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

package body Neo.Core.Console is

  --------
  -- IO --
  --------

  -- Internal protected structure of task-safe output
  protected Safe_IO is
      function Log          return Str_Unbound        is (Current_Log);
      function Log          return Str                is (To_Str (Current_Log));
      function Input_Entry  return Str                is (To_Str (Current_Input_Entry));
      function Line_Size    return Positive           is (Current_Line_Size);
      function Lines        return Int_64_Natural     is (Current_Lines);
      procedure Set_Put     (Val : Ptr_Procedure_Put) is begin Current_Put         := Val;                  end;
      procedure Line_Size   (Val : Positive)          is begin Current_Line_Size   := Val;                  end;
      procedure Input_Entry (Val : Str)               is begin Current_Input_Entry := To_Str_Unbound (Val); end;
      procedure Put (Item : Str) is
        Count : Int_64_Natural := 0;
        begin
          Current_Log := Current_Log & To_Str_Unbound (Item);
          if Current_Put /= null then Current_Put.All (Item); end if;
          for Get of Item loop
            if Get = Char_16'Val (Char_8'Pos (ASCII.CR)) then Count := Count + 1; end if;
          end loop;
          Current_Lines := Current_Lines + Count;
        end;
    private
      Current_Put         : Ptr_Procedure_Put;
      Current_Log         : Str_Unbound;
      Current_Input_Entry : Str_Unbound;
      Current_Lines       : Int_64_Natural;
      Current_Tasks       : Positive;
      Current_Line_Size   : Positive := 80;
    end;

  -- Public Safe_IO interface that wraps the protected type
  procedure Use_Ada_Put                                    is begin Set_Put (Ada.Wide_Text_IO.Put'Access);    end; 
  procedure Line        (Num  : Positive := 1)             is begin for I in 1..Num loop Put (EOL); end loop; end;
  procedure Input_Entry (Val  : Str)                       is begin Safe_IO.Input_Entry (Val);                end;
  procedure Line_Size   (Val  : Positive)                  is begin Safe_IO.Line_Size (Val);                  end;
  procedure Set_Put     (Val  : Ptr_Procedure_Put)         is begin Safe_IO.Set_Put (Val);                    end;
  procedure Put         (Item : Str_Unbound)               is begin Put (To_Str (Item));                      end;
  procedure Put         (Item : Char_16)                   is begin Put (Item & "");                          end;                  
  procedure Put         (Item : Str)                       is begin Safe_IO.Put (Item);                       end;
  procedure Line        (Item : Char_16)                   is begin Line (Item & "");                         end;    
  procedure Line        (Item : Str_Unbound)               is begin Line (To_Str (Item));                     end;
  procedure Line        (Item : Str)                       is begin Put (Item); Line;                         end;
  function Extension    (Path : Str) return Str            is (Path (Index (Path, ".") + 1..Path'Last));
  function Log                       return Str            is (Safe_IO.Log);
  function Input_Entry               return Str            is (Safe_IO.Input_Entry); 
  function Lines                     return Int_64_Natural is (Safe_IO.Lines);
  function Line_Size                 return Positive       is (Safe_IO.Line_Size);

  --------------------
  -- Internal State --
  --------------------

  -- Maximum cvar values displayable after query
  MAX_VALUES_DISPLAYABLE : constant Positive := 5; 

  -- Internal data structures
  type Command_State is record 
      Save     : access function return Str;
      Callback : access procedure (Args : Array_Str_Unbound);
    end record;
  type CVar_State is record 
      Val : Str_Unbound; -- A cvar that goes out of scope is not forgotten
      Get : access function return Str;
      Set : access procedure (Val : Str);
    end record;

  -- Data types
  package Hashed_CVar    is new Hashed (CVar_State);
  package Hashed_Command is new Hashed (Command_State);
  use Hashed_CVar.Unsafe; use Hashed_Command.Unsafe;

  -- Global maps
  Commands : Hashed_Command.Safe_Map;
  CVars    : Hashed_CVar.Safe_Map;

  ----------
  -- CVar --
  ----------

  package body CVar is
      Duplicate, Parse : Exception;

      -- Controller
      type Control_State is new Controlled with null record;
      procedure Initialize (Control : in out Control_State);
      procedure Finalize   (Control : in out Control_State);
      procedure Initialize (Control : in out Control_State) is
        begin
          if Commands.Has (Name) then
            if CVars.Get (Name).Set /= null or CVars.Get (Name).Get /= null then raise Duplicate; end if;
            CVars.Replace (Name, (Val => CVars.Get (Name).Val,
                                  Get => Handle_Get'Unrestricted_Access,
                                  Set => Handle_Set'Unrestricted_Access));
            Handle_Set (To_Str (CVars.Get (Name).Val));
          else
            CVars.Insert (Name, (Val => NULL_STR_UNBOUND,
                                 Get => Handle_Get'Unrestricted_Access,
                                 Set => Handle_Set'Unrestricted_Access));
            Set (Initial);
          end if;
        end;
      procedure Finalize (Control : in out Control_State) is
        begin
          if Settable then CVars.Replace (Name, (Val => To_Str_Unbound (Trim (Safe_Var_T.Get'Img, Both)),
                                                 Get => null,
                                                 Set => null));
          else CVars.Delete (Name); end if;
        end;
      Controller : Control_State;

      -- Internal protected type to maintain task safety
      protected Safe_Var_T with Lock_Free is
          function Get return Var_T is (Current);
          procedure Set (Val : Var_T) is begin Current := Val; end;
        private
          Current : Var_T;
        end;

      -- Get
      function Get return Var_T is (Safe_Var_T.Get);
      function Handle_Get return Str is
        Vals : Str_Unbound := To_Str_Unbound (Trim (Var_T'First'Img, Both));
        begin
          if Var_T'Pos (Var_T'Last) - Var_T'Pos (Var_T'First) > MAX_VALUES_DISPLAYABLE then
            Vals := Vals & ".." & To_Str (Trim (Var_T'Last'Img, Both));
          else
            for I in Var_T'Val (Var_T'Pos (Var_T'first) + 1)..Var_T'Last loop
              Vals := Vals & ", " & To_Str_Unbound (Trim (I'Img, Both));
            end loop;
          end if;
          return Help & EOL & "Current value: " & To_Str (Trim (Safe_Var_T.Get'Img, Both)) & EOL & "Possible values: " & To_Str (Vals);
        end;

      -- Set
      procedure Set (Val : Var_T) is begin Safe_Var_T.Set (Val); end;
      procedure Handle_Set (Val : Str) is
        begin
          if not Settable then
            Line (Name & " is not settable!");
            return;
          end if;
          Set (Var_T'Wide_Value (Val));
        exception when Constraint_Error =>
          for I in Var_T'Range loop
            if Val = To_Str (I'Img) then
              Set (I);
              exit;
            elsif I = Var_T'Last then
              Line ("Incorrect parameter for cvar """ & Name & """: " & Val);
              Line (Handle_Get);
            end if;
          end loop;
        end;
    end;

  -------------
  -- Command --
  -------------

  package body Command is
      Duplicate, Parse : Exception;

      -- Callback is a generic formal so a rename needs to be present to pass out function pointers
      procedure Informal (Args : Array_Str_Unbound) renames Callback; 

      -- Controller
      type Control_State is new Controlled with null record;
      procedure Finalize   (Control : in out Control_State);
      procedure Initialize (Control : in out Control_State);
      procedure Finalize   (Control : in out Control_State) is begin Commands.Delete (Name); end;
      procedure Initialize (Control : in out Control_State) is
        begin
          if Commands.Has (Name) then raise Duplicate; end if;
          Commands.Insert (Name, (Informal'Unrestricted_Access, Save));
        end;
      Controller : Control_State;
    end;

  -- Input entry parsing
  procedure Submit (Text : Str) is
    Tokens : Array_Str_Unbound := Split (Text);
    CMD    : constant Str := To_Str (Tokens (1));
    begin
      if Commands.Has (CMD) then Commands.Get (CMD).Callback.All (Tokens (2..Tokens'Length));
      elsif CVars.Has (CMD) then
        if Tokens'Length = 1 then
          if CVars.Get (CMD).Get /= null then Line (CVars.Get (CMD).Get.All); end if;
        elsif CVars.Get (CMD).Set /= null then CVars.Get (CMD).Set.All (To_Str (Tokens (2))); end if;
      else raise Constraint_Error; end if;
    exception when others => Line ("No such cvar or command!"); end;

  -- Autocomplete aid
  function Autocomplete (Text : Str) return Array_Str_Unbound is
    Result : Vector_Str_Unbound.Unsafe.Vector;
    begin
      for CVar of CVars.Keys loop
        if To_Str (CVar)(1..Text'Length) = Text then Result.Append (CVar); end if;
      end loop;
      return Vector_Str_Unbound.To_Unsafe_Array (Result);
    end;

  ------------------
  -- Localization --
  ------------------

  -- Data types
  package Hashed_Language is new Hashed (Str_Unbound);                use Hashed_Language.Unsafe;
  package Hashed_Locale   is new Hashed (Hashed_Language.Unsafe.Map); use Hashed_Locale.Unsafe; 
  
  -- Initialization
  function Initialize_Localization return Hashed_Locale.Unsafe.Map is
    function Split_Columns (Text : Str) return Vector_Str_Unbound.Unsafe.Vector is
      Result     : Vector_Str_Unbound.Unsafe.Vector;
      begin
        return Result;
      end;
    package Hashed_Indexes is new Hashed (Positive);
    J         : Int  := 1;
    In_Quote  : Bool := False;
    In_Column : Bool := True;
    Data      : File_Type;
    Column    : Str_Unbound;
    Indexes   : Hashed_Indexes.Unsafe.Map;
    Locales   : Hashed_Locale.Unsafe.Map;
    Language  : Hashed_Language.Unsafe.Map;
    Entries   : Vector_Str_Unbound.Unsafe.Vector;
    ENG       : constant Str_Unbound := To_Str_Unbound ("eng" & NULL_STR);
    begin
      Open (Data, In_File, To_Str_8 (PATH_LOCALE)); -- Str_8 !!!
      for I of Split (Get_Line (Data), ",") loop
        Indexes.Insert (I, J);
        Locales.Insert (I, Language);
        J := J + 1;
      end loop;
      while not End_Of_File (Data) loop
        for I of Get_Line (Data) loop
          case I is
            when '"' =>
              if not In_Quote then
                Column   := NULL_STR_UNBOUND;
                In_Quote := True;
              else
                Entries.Append (Column);
                Column    := NULL_STR_UNBOUND;
                In_Quote  := False;
                In_Column := False;
              end if;
            when ',' =>
              if In_Quote then Column := Column & I;
              elsif not In_Column then In_Column := True;
              else
                Entries.Append (Trim (Column, Both));
                Column := NULL_STR_UNBOUND;
              end if;
            when others => Column := Column & I;
          end case;
        end loop;
        if Trim (Column, Both) /= NULL_STR_UNBOUND then Entries.Append (Trim (Column, Both)); end if;
        for I in Locales.Iterate loop
          Language := Element (I);
          Language.Insert (Entries (Indexes.Element (ENG)), (if Int (Entries.Length) >= Indexes.Element (Key (I)) then
                                                               Entries (Indexes.Element (Key (I)))
                                                             else NULL_STR_UNBOUND));
          Locales.Replace (Key (I), Language);
        end loop;
      end loop;
      Close (Data);
      return Locales;
    exception when others => return Locales; end;

  -- Initalized data
  LOCALE : constant Hashed_Locale.Unsafe.Map := Initialize_Localization;

  -- Locale lookup
  function Localize (Item : Str) return Str is
    CODE : constant Language_Code := Language;
    LANG : constant Str_Unbound   := To_Str_Unbound (CODE (1) & CODE (2) & CODE (3)); -- This is stupid...
    begin
      return (if To_Str (LANG) /= "eng"
                and then LOCALE.Contains (LANG)
                and then LOCALE.Element (LANG).Contains (To_Str_Unbound (Item))
              then To_Str (LOCALE.Element (LANG).Element (To_Str_Unbound (Item)))
              else Item);
    end;

  -------------------
  -- Configuration --
  -------------------

  -- Controller
  type Control_State is new Controlled with null record;
  procedure Initialize (Control : in out Control_State);
  procedure Finalize   (Control : in out Control_State);
  procedure Initialize (Control : in out Control_State) is
    package Parse_Config is new Parser (PATH_CONFIG, "--"); use Parse_Config;
    begin
      while not At_EOF loop Submit (Next_Line); end loop;
    exception when others => null; end; -- There is no configuration file.. automatically use defaults
  procedure Finalize (Control : in out Control_State) is
    Data : File_Type;
    begin
      Open (Data, Out_File, To_Str_8 (PATH_CONFIG)); -- Str_8 !!!

      -- Header
      Put_Line (Data, "-- " & NAME_ID & " " & VERSION & " config: " & Date_Str);
      Put_Line (Data, "-- Note: To restore default values simply delete this file.");
      New_Line (Data);
      Put_Line (Data, "-- CVars");

      -- CVars
      for I in CVars.Get.Iterate loop
        Put_Line (Data, To_Str (Key (I) & " " & Element (I).Val));
      end loop;

      -- Commands
      for I in Commands.Get.Iterate loop
        if Element (I).Save /= null then 
          New_Line (Data);
          Put_Line (Data, "-- " & To_Str (Key (I)));
          Put_Line (Data, Element (I).Save.All);
        end if;
      end loop;
      Close (Data);
    exception when others => Line ("Configuration save failed!"); end;
  Controller : Control_State;
end;