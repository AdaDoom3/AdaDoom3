
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

package body Neo.Parsing is

  ---------------
  -- Splitting --
  ---------------

  -- Split a string into an array of strings based on a separator (e.g. comma) ??? Could this be done without vectors?
  function Split (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector is
    Result    : Vector_Str_16_Unbound.Unsafe.Vector;
    TRIMMED   : constant Str  := Trim (Item, Both);
    REMAINDER : constant Natural := Index (TRIMMED, On);
    begin
      if REMAINDER = 0 then
        Result.Append (To_Str_Unbound (TRIMMED));
        return Result;
      else Result.Append (To_Str_Unbound (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
      Result.Append (Split (TRIMMED (REMAINDER..TRIMMED'Last), On));
      return Result;
    end;
  function Split (Item : Str; On : Str := " ") return Array_Str_Unbound is
    begin
      return Vector_Str_16_Unbound.To_Unsafe_Array (Split (Item, On));
    end;

  -------------
  -- Handler --
  -------------

  package body Handler is

      -- Exceptions for bad formats or incorrect extensions
      Unsupported, Duplicate_Format : Exception;

      -- Internal format for relating extensions to callbacks
      type Format_State is record
          Load       : not null access function (Path : Str) return T;
          Extensions : Str_Unbound;
        end record;
      package Ordered_Format is new Ordered (Format_T, Format_State);
      Formats : Ordered_Format.Safe_Map;

      -- Dispatch to a stored callback based on the extension of the path
      function Load (Path : Str) return T is 
        Ext_Val : Str := Path (Index (Path, ".")..Path'Last);
        begin
          for Format of Formats.Get loop
            for Extension of Split (To_Str (Format.Extensions), ",") loop
              if Extension = Ext_Val then return Formats.Get (Format_T'Wide_Value (To_Str (Extension))).Load (Path); end if;
            end loop;
          end loop;
          raise Unsupported;
        end;

      -- Package for registering format callbacks
      package body Format is
          function Informal_Load (Path : Str) return T renames Format.Load;
          type Controller_State is new Controlled with null record;
          procedure Finalize   (Item : in out Controller_State);
          procedure Initialize (Item : in out Controller_State);
          procedure Finalize   (Item : in out Controller_State) is begin Formats.Delete (Kind); end;
          procedure Initialize (Item : in out Controller_State) is
            begin
              if Formats.Has (Kind) then raise Duplicate_Format; end if;
              Formats.Insert (Kind, (Informal_Load'Access, To_Str_Unbound (To_Lower (Extensions))));
            end;
        end;
    end;

  ------------
  -- Parser --
  ------------

  package body Parser is

    -- Load file and pre-process text (possibly remove comments and replace tabs)
    function Load return Array_Str_Unbound is
      Data                  : File_Type;
      In_Multiline_Comment  : Bool := False;
      Trimmed_Comment_Start : Str  := Trim (Comment_Start, Both);
      Trimmed_Comment_End   : Str  := Trim (Comment_End,   Both);
      Trimmed_Comment       : Str  := Trim (Comment,       Both);
      This                  : Vector_Str_16_Unbound.Unsafe.Vector;
      begin

        -- Verify generic package foramls
        Assert ((if Trimmed_Comment_Start = NULL_STR then Trimmed_Comment_End = NULL_STR else Trimmed_Comment_End /= NULL_STR));

        -- Open the file
        Open (Data, In_File, To_Str_8 (Path)); -- Str_8 !!!
        while not End_Of_File (Data) loop
          This.Append (To_Str_Unbound (Get_Line (Data)));

          -- Single-line comment removal
          if Trimmed_Comment /= NULL_STR and then Index (This.Last_Element, Trimmed_Comment) /= 0 then
            This.Replace_Element (This.Last_Index, Head (This.Last_Element, Index (This.Last_Element, Trimmed_Comment) - 1));
          end if;

          -- Multi-line comment removal
          if Trimmed_Comment_Start /= NULL_STR then
            if In_Multiline_Comment and then
              Index (This.Last_Element, Trimmed_Comment_End) + Trimmed_Comment_End'Length /= This.Last_Index
            then
              In_Multiline_Comment := False;
              This.Replace_Element (This.Last_Index, Head (This.Last_Element, Index (This.Last_Element, Trimmed_Comment_End)));
            elsif Index (This.Last_Element, Trimmed_Comment_Start) /= 0 then
              In_Multiline_Comment := True;
              This.Replace_Element (This.Last_Index, Tail (This.Last_Element, Index (This.Last_Element, Trimmed_Comment_End)));
            else This.Replace_Element (This.Last_Index, NULL_STR_UNBOUND); end if;
          end if;

          -- Tab replacement
          if Tab_Replacement /= NULL_CHAR_16 then
            while Index (This.Last_Element, TAB_16) /= 0 loop
              This.Replace_Element (This.Last_Index, Overwrite (This.Last_Element, Index (This.Last_Element, TAB_16), " "));
            end loop;
          end if;
        end loop;

        -- Close the file
        Close (Data);
        return Vector_Str_16_Unbound.To_Unsafe_Array (This);
      end;

      -- Internal variables
      This    : Array_Str_Unbound := Load;
      Row     : Positive          := 1;
      Column  : Natural           := 0;
      Invalid : Exception;

      -- Internal procedure for skipping whitespace
      procedure Seek is
        begin
          Column := Column + 1;
          while not At_EOF loop
            loop
              if Column > Length (This (Row)) or else This (Row) = NULL_STR_UNBOUND then
                Column := 1;
                Row    := Row + 1;
                exit;
              end if;
              if Index (Slice (This (Row), Column, Length (This (Row))), "" & Separator) /= Column then return; end if;
              Column := Column + 1;
            end loop;
          end loop;
        end;

      -- Position
      function At_EOF return Bool is ((if Row > This'Last then True else False));
      function At_EOL return Bool is
        Previous_Column : Positive    := Column;
        Previous_Row    : Positive    := Row;
        Result          : Str_Unbound := Next;
        begin
          Column          := Previous_Column;
          Previous_Column := Row;
          Row             := Previous_Row;
          return Previous_Column /= Row;
        end;

      -- Skip
      procedure Skip_Set (Starting, Ending : Str) is Junk : Str_Unbound := Next_Set (Starting, Ending); begin null; end;
      procedure Skip_Set (Ending : Str) is begin Skip_Set ("" & Element (This (Row), Column), Ending); end;
      procedure Skip_Line is begin Row := Row + 1; Column := 1; end;
      procedure Skip (Amount : Positive := 1) is
        Junk : Str_Unbound;
        begin for I in 1..Amount loop Junk := Next; end loop; end;

      -- Assert
      procedure Assert (T1, T2             : Str) is begin Assert (T1);             Assert (T2); end;
      procedure Assert (T1, T2, T3         : Str) is begin Assert (T1, T2);         Assert (T3); end;
      procedure Assert (T1, T2, T3, T4     : Str) is begin Assert (T1, T2, T3);     Assert (T4); end;
      procedure Assert (T1, T2, T3, T4, T5 : Str) is begin Assert (T1, T2, T3, T4); Assert (T5); end;
      procedure Assert (Text : Str) is
        begin
          if Index (Slice (This (Row), Column, Length (This (Row))), Text) /= Column then raise Invalid; end if;
          Column := Column + Text'length - 1;
          Seek;
        end;

      -- Check ahead without 
      function Peek return Str_Unbound is
        Previous_Column : Positive    := Column;
        Previous_Row    : Positive    := Row;
        Result          : Str_Unbound := Next;
        begin
          Row    := Previous_Row;
          Column := Previous_Column;
          return Result;
        end;

      -- Next number
      function Next_Internal return Real_64 is
        Found_Decimal : Bool := False;
        Found_Digit   : Bool := False;
        Found_Sign    : Bool := False;
        Result        : Str_Unbound;
        begin
          while Column <= Length (This (Row)) loop
            if not Is_Digit (Element (This (Row), Column)) then
              case Element (This (Row), Column) is
                when '-' | '+' => exit when Found_Digit or Found_Sign;
                                  Found_Sign := True;                    
                when '.' =>       exit when Found_Decimal;
                                  Found_Digit   := True;
                                  Found_Decimal := True;
                when others =>    exit;
              end case;
            elsif not Found_Digit then Found_Digit := True; end if;
            Result := Result & Element (This (Row), Column);
            Column := Column + 1;
          end loop;
          if Result = NULL_STR_UNBOUND then raise Invalid; end if;
          Seek;
          return Real_64'Wide_Value (To_Str (Result));
        end;
      function Next return Int     is (Int    (Next_Internal));
      function Next return Real    is (Real   (Next_Internal));
      function Next return Int_64  is (Int_64 (Next_Internal));
      function Next return Real_64 renames Next_Internal;

      -- Next string
      function Next_Line return Str_Unbound is
        Result : Str_Unbound := To_Str_Unbound (Slice (This (Row), Column, Length (This (Row))));
        begin
          Parser.Skip_Line;
          return Result;
        end;
      function Next return Str_Unbound is
        Result : Str_Unbound := To_Str_Unbound (Slice (This (Row), Column, Length (This (Row))));
        I      : Natural := Index (Result, "" & Separator);
        begin
          if I /= 0 then Delete (Result, I, Length (Result)); end if;
          Column := Column + Length (Result);
          Seek;
          return Result;
        end;

      -- Next delimited group or set
      function Next_Set (Ending : Str) return Str_Unbound is (Next_Set ("" & Element (This (Row), Column), Ending));
      function Next_Set (Starting, Ending : Str) return Str_Unbound is
        Result : Str_Unbound;
        Buffer : Str_Unbound;
        I      : Natural;
        begin
          Assert (Starting);
          while not At_EOF loop
            Buffer := To_Str_Unbound (Slice (This (Row), Column, Length (This (Row))));
            I      := Index (Buffer, Ending);
            if I = 0 then
              Column := 1;
              Row    := Row + 1;
              Result := Result & Buffer & EOL;
            else
              Buffer := Delete (Buffer, I, Length (Buffer));
              Column := Column + Length (Buffer) + Ending'length;
              Seek;
              return Result & Buffer;
            end if;
          end loop;
          raise Invalid;
        end;
    end;
end;