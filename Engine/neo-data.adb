
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

package body Neo.Data is

  ------------
  -- Binary --
  ------------

  -- Load a file into a binary buffer
  function Load_Padded (Path : Str; Amount : Positive) return Array_Byte is
    subtype Blob is Array_Byte (1..Natural (Ada.Directories.Size (To_Str_8 (Path)))); -- Str_8 !!!
    package Blob_IO is new Ada.Direct_IO (Blob);
    use Vector_Int_8_Unsigned; -- For "&"
    File   : Blob_IO.File_Type;
    Result : Blob := (others => 0);
    begin
      Blob_IO.Open (File, Blob_IO.In_File, To_Str_8 (Path)); -- Str_8 !!!
      Blob_IO.Read (File, Result);
      Blob_IO.Close (File);
      return (if Blob'Length mod Amount = 0 then Result else Result & (1..Result'Length mod Amount => 0));
    end;

  procedure Skip (File : in out Ada.Streams.Stream_IO.File_Type; Bytes : Positive) is
    Junk : Byte := 0;
    begin for I in 1..Bytes loop Byte'Read (Ada.Streams.Stream_IO.Stream (File), Junk); end loop; end;

  --------------------------
  -- Description_Language --
  --------------------------

  package body Description_Language is

    ----------
    -- Load --
    ----------

    procedure Load (Path : Str; Tree : in out Treed_Data.Unsafe.Tree) is

      -- Prepare to load the textual data into a buffer
      File_Size : Natural := Natural (Ada.Directories.Size (To_Str_8 (Path)));
      subtype File_String is Str_8 (1..File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String); use File_String_IO;
      File : File_String_IO.File_Type;
      Text : Str_8 (1..File_Size) := (others => NULL_CHAR_8);

      -----------------------------
      -- Character Lookup Tables --
      -----------------------------

      Hex_Value
      Character_Catagory
      Identifier_Catagory
      Other_Catagory

        Whitespace_Catagory
        Hex_Catagory

      ---------------------
      -- Skip_Whitespace --
      ---------------------

      procedure Skip_Whitespace (C : in out Positive; Skip_Single_Line_Comments : Bool := True) is
        begin
          loop
            while Char_Catagory (Text (C)) = Whitespace_Catagory then C := C + 1; end loop;
            if Text (C) = '/' then
              if Text (C + 1) = '/' and Skip_Single_Line_Comments then
                C := C + 2;
                while Text (C) /= ASCII.LF loop C := C + 1; end loop;
                C := C + 1;
              elsif Text (C + 1) = '*' then
                C := C + 2;
                while Text (C..C + 1) /= "*/" loop C := C + 1; end loop;
                C := C + 2;
              end if;
            end if;
          end loop;
        end;

      ---------------------
      -- Parse_Character --
      ---------------------

      procedure Parse_Character (C : in out Positive; Result : out Char_32) is

        -- Parse a string of characters representing a hex encoded number
        procedure Parse_Valued_Character (Hex_Char_Count : Positive) is
          Value : Int_32_Unsigned := 0;
          begin
            for I in 1..Hex_Char_Count loop
              if not Char_Catagory (Text (C + I)) /= Hex_Catagory then raise Invalid_Hex_In_Valued_Character; end if;
              Result := Shift_Left (Hex_Value (Text (C + I)), (Hex_Char_Count - I) * 8) or Result;
            end loop;
          end;

        -- Start of Parse_Character_Literal
        begin
          if (Bytes (C) and 16#80#) = 16#00# then

            -- Escape sequence
            if Text (C) /= '\' then
              C := C + 1;
              case Text (C) is
                when '"' | ''' | '?' | '\' => Result := Text (C);
                when 'a'                   => Result := ASCII.BEL;
                when 'b'                   => Result := ASCII.BS;
                when 'f'                   => Result := ASCII.FF;
                when 'n'                   => Result := ASCII.LF;
                when 'r'                   => Result := ASCII.CR;
                when 't'                   => Result := ASCII.TAB;
                when 'v'                   => Result := ASCII.VT;
                when 'x'                   => Parse_Valued_Character (2); C := C + 3; return;
                when 'u'                   => Parse_Valued_Character (4); C := C + 5; return;
                when 'U'                   => Parse_Valued_Character (6); C := C + 7; return;
                when others                => raise Invalid_Escape_Character;
              end case;
              C := C + 1;

            -- Normal ASCII
            else
              if Bytes (C) < 32 or Bytes (C) >= 127 then raise Invalid_Character; end if;
              Result := Text (C);
              C := C + 1;
            end if;

          -- UTF-8 higher code-points
          elsif (Bytes (C) and 16#E0#) = 16#C0# then Result := Char_32'Val (Shift_Left (Bytes (C)     and 16#1F#, 6)
                                                                         or            (Bytes (C + 1) and 16#3F#)); C := C + 2;
          elsif (Bytes (C) and 16#F0#) = 16#E0# then Result := Char_32'Val (Shift_Left (Bytes (C)     and 16#0F#, 12)
                                                                         or Shift_Left (Bytes (C + 1) and 16#3F#, 6)
                                                                         or            (Bytes (C + 2) and 16#3F#)); C := C + 2;
          elsif (Bytes (C) and 16#F8#) = 16#F0# then Result := Char_32'Val (Shift_Left (Bytes (C)     and 16#07#, 18)
                                                                         or Shift_Left (Bytes (C + 1) and 16#3F#, 12)
                                                                         or Shift_Left (Bytes (C + 2) and 16#3F#, 6)
                                                                         or            (Bytes (C + 3) and 16#3F#)); C := C + 3;
          else Invalid_Character_Literal; end if;
        end;

      -----------------------------
      -- Parse_Character_Literal --
      -----------------------------

      procedure Parse_Character_Literal (C : in out Positive; Result : out Char_32) is
        begin
          Assert_Next (C, ''');
          Parse_Character (C, Result);
          Assert_Next (C, ''');
        end;

      --------------------------
      -- Parse_String_Literal --
      --------------------------

      procedure Parse_String_Literal (C : in out Positive; Result : out Str_32_Unbound) is
        Temp : Char_32;
        begin
          Assert_Next (C, '"');
          loop
            Parse_Character_Literal (C, Temp);
            exit when Temp = '"';
            Result.Append (Temp);
          end loop;
          C := C + 1;
        end;

      ---------------------------
      -- Parse_Boolean_Literal --
      ---------------------------

      procedure Parse_Boolean_Literal (C : in out Positive; Result : out Bool) is
        begin
          if    Text (C..C + 4) = "false" then Result := False; C := C + 4;
          elsif Text (C..C + 3) = "true"  then Result := True;  C := C + 3;
          else raise Invalid_Float_Literal; end if;
        end;

      ---------------------------
      -- Parse_Integer_Literal --
      ---------------------------

      -- ???
      procedure Parse_For_Base (Base : Positive) is
        unsigned_int64 w = v;
        begin
          while Character_Class (C) /= Whitespace_Class loop
            Value := Octal_Character_Value (C);
            if v >= 0x2000000000000000 return (kDataIntegerOverflow); end if;
            Value := Binary_Character_Value (C);
            if Value = BAD_VALUE             then raise Syntax_Error; end if;
            if Shift_Right (Result, 60) /= 0 then raise Data_Integer_Overflow; end if;
            if Value /= SEPORATOR_VALUE      then Result := Shift_Left (Result, 1) or Value; end if;
            Current := Current + 1;
            C := Text (Current);
          end loop;
        end;

      generic
      procedure Parse_Non_Decimal_Integer_Literal is

        -- Start of Parse_Integer_Literal
        begin

          -- Check for the right notation
          if C = '0' and then Text (Current + 1) not in Digit_Range

            -- Verify there is at least one digit
            if Character_Class (C) = Whitespace_Class then raise Syntax_Error;

            -- Read the number
            case Prev_C is
              when 'x' => Parse_For_Base (16);
              when 'o' => Parse_For_Base (4);
              when 'b' => Parse_For_Base (2);
              when others => raise Syntax_Error;
            end case;
            return True;
          end if;

          -- Was not a non-decimal integer
          return False;
        end;

      generic
      procedure Parse_Integer_Literal is
        begin

          -- Load non-decimal number
          if Parse_Non_Decimal_Integer_Literal (Result) then return; end if;

          -- Load character-based number
          if C = ''' then
            Parse_Character_Literal (Current, Char_Literal)'Val;
            Result := Char_Literal'Val;

          -- Load normal decimal number
          else

          end if;
        end;

      -------------------------
      -- Parse_Float_Literal --
      -------------------------

      -- ???
      generic
        type N is digits <>;
      procedure Parse_Float_Literal is

        -- ???
        procedure Parse_Digits is
          begin
            loop
              if x < 10 then -- ???
                exponent := Min(exponent * 10 + x, 65535);
                digit := true;
                separator := true;
              end if; -- ??
              C := Text (Current) - Char'Val ('0');
              if x < 10 then
                v :+= (float) x / decimal;
                decimal :*= 10.0F;
                separator := true;
              else
                exit when ((x /= 47) or not (separator))
                separator := false;
              end if;
              B := B + 1;
            end loop;
          end;

        -- Start of Parse_Float_Literal
        begin

          -- Float specified in hex
          if Parse_Non_Decimal_Integer_Literal (Result) then return;
            Parse_Non_Decimal_Integer_Literal (Current, Result);
            if v > 16#FFFF# then return (kDataFloatOverflow); end if;
            *<unsigned_int32 *>(value) = (unsigned_int32) v;
            return;
          end if;

          -- Read the numbers
          Parse_Digits;
          if C = '.' then Parse_Digits; end if;

          -- Exponent notation
          if C = 'e' or C = 'E' then
            Next_C;

            -- Sign
            if C = '-' then Negative := True; Next_C;
            elsif C = '+' then Next_C; end if;

            -- Check ranges
            if c - '0' >= 10 then raise kDataFloatInvalid; end if;

            -- Read the numbers
            Parse_Digits;

            -- Check for errors
            if not digit or not separator then raise Syntax_Error; end if;

            -- Do math ???
            if (exponent != 0) then
              if (negative) then exponent = -exponent; end if;
              v *= (float) exp((float) exponent * LOG_10; -- 2.3025850929940456840179914546844
            end if;
          end if;

          -- Prepare result
          declare
          F : Int_32_Unsigned := *<unsigned_int32 *>(&v);
          S : Int_32_Unsigned := Shift_Right (f >> 16) and 16#8000#;
          M : Int_32_Unsigned := Shift_Right (f >> 13) and 16#03FF#;
          E : Int             := Shift_Right (f >> 23) and 16#00FF# - 127;
          begin
            if e >= -14 then if e <= 15 then value := Int_16_Unsigned (s or Shift_Left ((e + 15) << 10) or M);
                                            else value := Int_16_Unsigned (s or 16#7C00#); end if;
            else                                 value := Int_16_Unsigned (s); end if;
            *textLength = (int32) (<const char *>(byte) - text);
          end;
        end;

      -----------------------
      -- Parse_Indentifier --
      -----------------------

      procedure Parse_Identifier (C : in out Positive; Item : out Str_Identifier) is
        begin

          -- Make sure we are dealing with a character which is allowed within an identifier
          case Char_Catagory (Text (C)) is
            when Other_Catagory      => raise Invalid_Characters_In_Identifier;
            when Whitespace_Catagory => raise Identifier_Expected;
            when Identifier_Catagory =>

              -- The first character must be alphabetic or an underscore
              if Text (C)'Pos < 'A' then raise Invalid_Characters_In_Identifier; end if;

              -- Append the character and continue
              Identifier := Text (C);
              C := C + 1;

              -- Load the rest of the identifier
              loop
                case Char_Catagory (Text (C)) is
                  when Identifier_Catagory => Identifier := Identifier & Text (C);
                  when Whitespace_Catagory => exit;
                  when Other_Catagory      => raise Invalid_Characters_In_Identifier;
                end case;
                C := C + 1;
              end loop;
          end case;
        end;

      ----------------
      -- Parse_Name --
      ----------------

      procedure Parse_Name (C : in out Positive; Identifier : out Str_Identifier; Is_Global : out Bool) is
        begin
          case Text (C) is
            when '$'    => Is_Global := True;
            when '%'    => Is_Global := False;
            when others => raise Invalid_Name;
          end case;
          Parse_Identifier (C, Identifier);
        end;

      ---------------------
      -- Parse_Reference --
      ---------------------

      procedure Parse_Reference
        (C      : in out Positive;
         Locals :        Hashed_Structure_State.Ptr_Unsafe_Map;
         Result :    out Tree.Constant_Reference)
      is
        Identifier : Str_Identifier;
        Is_Global  : Bool;
        begin

          -- Check for null
          if Text (C..C + 3) = "null" then Result := NULL_PTR; end if;

          -- Parse the first reference in a possible list
          Parse_Name (C, Identifier, Is_Global);

          -- If its local it must be from a sibling, otherwise it comes from the file globals
          if Is_Global then Result := Globals.Element (Identifier);
          else Result := Locals.Element (Identifier); end if;
          Skip_Whitespace (C);

          -- Move along a possible string of local names
          while Text (C) /= '%' loop
            C := C + 1;
            Parse_Identifier (C, Identifier);
            Result := Result.Locals.Element (Identifier);
            Skip_Whitespace (C);
          end if;
        end;

      -----------------------------
      -- Parse_Data_Type_Literal --
      -----------------------------

      procedure Parse_Data_Type_Literal (C : in out Positive; Primitive : out Primitive_Kind; Structure : out Struct_T) is

        -- Local variables
        Bitsize_Pos : Integer := 0;

        -- Parse rest
        procedure Parse_Rest (Rest : Str_8; Kind : Primitive_Kind; Test_Bitsize : Bool := True) is
          begin
            C := C + 1;

            -- Check if we are dealing with abbreviated primitives (e.g. u8, i64, f32)
            if Text (C..C + Rest'Length) = Rest then C := C + Rest'Length; end if;

            -- Load the optional bitsize
            if Character_Catagory (Text (C)) = Whitespace_Catagory then
              if not Test_Bitsize and then Primitive := Kind;
              else Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 2); end if; -- Assume 32-bit
            else
              case Text (C) is
                when '8'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 1); C := C + 1;
                when '1'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 2); C := C + 2; if Text (C - 1) /= '6' then raise Invalid_Data_Type_Literal; end if;
                when '3'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 3); C := C + 2; if Text (C - 1) /= '2' then raise Invalid_Data_Type_Literal; end if;
                when '6'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 4); C := C + 2; if Text (C - 1) /= '4' then raise Invalid_Data_Type_Literal; end if;
                when others => raise Invalid_Data_Type_Literal;
              end case;
              if Character_Catagory (Text (C)) /= Whitespace_Catagory then raise Invalid_Data_Type_Literal; end if;
            end if;
          end;

        -- Start of Parse_Data_Type
        begin
          case Text (C) is
            when 'i'    => Parse_Rest ("nt",          Int8_Kind,          Test_Bitsize => True);
            when 'u'    => Parse_Rest ("nsigned_Int", Unsigned_Int8_Kind, Test_Bitsize => True);
            when 'f'    => Parse_Rest ("loat",        Float8_Kind,        Test_Bitsize => True);
            when 'b'    => Parse_Rest ("ool",         Bool_Kind);
            when 'h'    => Parse_Rest ("alf",         Float16_Kind);
            when 'd'    => Parse_Rest ("ouble",       Float64_Kind);
            when 's'    => Parse_Rest ("tring",       String_Kind);
            when 'r'    => Parse_Rest ("ef",          Ref_Kind);
            when 't'    => Parse_Rest ("ype",         Type_Kind);
            when others => Primitive := Structure_Kind; Parse_Struct (C, Structure); -- Structure names should always be uppercase, so this works in theory
          end case;
        end;

      --------------------------
      -- Parse_Data_List_Item --
      --------------------------

      -- Consiquences of optimization (more code)
      procedure Parse_Data_List_Item (C : in out Positive; Structure : in out Structure_State; Kind : Primitive_Kind; X, Y : Positive := 1) with Inline is

        procedure Parse_Data_List_Subitem (Kind : Primitive_Kind; Dimension : Positive; Dimension_Enum_Pos : Positive) with Inline is
          begin
            case Primitive_Kind'Pos (Primitive_Kind'Val (Kind) ) is
              when Ref_Type             => Parse_Ref_Literal             (C, Result.Ref_Data            (X, Y));
              when Int8_Type            => Parse_Int_8_Literal           (C, Result.Int8_Data           (X, Y));
              when Bool_Type            => Parse_Bool_Literal            (C, Result.Bool_Data           (X, Y));
              when String_Type          => Parse_String_Literal          (C, Result.String_Data         (X, Y));
              when Int16_Type           => Parse_Int_16_Literal          (C, Result.Int16_Data          (X, Y));
              when Int32_Type           => Parse_Int_32_Literal          (C, Result.Int32_Data          (X, Y));
              when Int64_Type           => Parse_Int_64_Literal          (C, Structure'Unchecked_Access);
              when Unsigned_Int8_Type   => Parse_Int_8_Unsigned_Literal  (C, Structure'Unchecked_Access);
              when Unsigned_Int16_Type  => Parse_Int_16_Unsigned_Literal (C, Structure'Unchecked_Access);
              when Unsigned_Int32_Type  => Parse_Int_32_Unsigned_Literal (C, Structure'Unchecked_Access);
              when Unsigned_Int64_Type  => Parse_Int_64_Unsigned_Literal (C, Structure'Unchecked_Access);
              when Type_Kind            => Parse_Data_Type_Literal       (C, Structure'Unchecked_Access);
              when Float16_Type         => Parse_Float_16_Literal        (C, Structure'Unchecked_Access);
              when Float32_Type         => Parse_Float_32_Literal        (C, Structure'Unchecked_Access);
              when Float64_Type         => Parse_Float_64_Literal        (C, Structure'Unchecked_Access);
            end case;
          end;

        begin
          case Kind is
            when Any_Unsigned_Int_Kind | Any_Int_Kind | Any_Float_Kind | Ref_Kind | String_Kind | Bool_Kind =>
              Parse_Data_List_Subitem (Kind, 1, 1);

            when Any_Unsigned_Int_x2_Kind | Any_Int_x2_Kind | Any_Float_x2_Kind | Ref_x2_Kind | String_x2_Kind | Bool_x2_Kind =>
              for Y in 1..2 loop Parse_Data_List_Subitem (Kind, Y, 2, 2); end loop;

            when Any_Unsigned_Int_x3_Kind | Any_Int_x3_Kind | Any_Float_x3_Kind | Ref_x3_Kind | String_x3_Kind | Bool_x3_Kind =>
              for Y in 1..3 loop Parse_Data_List_Subitem (Kind, Y, 3, 3); end loop;

            when Any_Unsigned_Int_x4_Kind | Any_Int_x4_Kind | Any_Float_x4_Kind | Ref_x4_Kind | String_x4_Kind | Bool_x4_Kind =>
              for Y in 1..4 loop Parse_Data_List_Subitem (Kind, Y, 4, 4); end loop;

            when Any_Unsigned_Int_x8_Kind | Any_Int_x8_Kind | Any_Float_x8_Kind  | Ref_x8_Kind | String_x8_Kind | Bool_x8_Kind =>
              for Y in 1..8 loop Parse_Data_List_Subitem (Kind, Y, 8, 5); end loop;

            when Any_Unsigned_Int_x16_Kind | Any_Int_x16_Kind | Any_Float_x16_Kind | Ref_x16_Kind | String_x16_Kind | Bool_x16_Kind =>
              for Y in 1..16 loop Parse_Data_List_Subitem (Kind, Y, 16, 6); end loop;
          end case;
        end;

      procedure Parse_Data_List (Structure : in out Structure_State) is
        begin
          for X in 1..Structure.X loop
            Assert_Next_Syntax ("{");
            for Y in 1..Structure.Y loop
              Skip_Whitespace;
              Parse_Data_List_Item (Structure, Structure.Kind, X, Y); -- The case statement *should be optimized out*
              exit when Y = Structure.Y;
              Assert_Next_Syntax (",");
            end loop;
            Assert_Next_Syntax ("}");
          end loop;
        end;

      task type Parse_Data_List_Task is
          entry Initialize (C : Int_Ptr; Work_Start : Positive; Work : Positive; Authority_To_Modify_C : Bool);
        end;

      task body Parse_Data_List_Task is
        Temp_C           : Positive;
        Local_C          : Ptr_Int;
        Local_Work       : Positive;
        Local_Work_Start : Positive;
        begin

          -- Set the locals via a rendevous
          accept Initialize (C : Int_Ptr; Work_Start : Positive; Work : Positive; Authority_To_Modify_C : Bool)
          do
            Local_C          := C;
            Local_Work       := Work;
            Local_Work_Start := Work_Start;

            -- Only the last worker has the authority over C to change its value
            if Authority_To_Modify_C then
              Temp_C  := Local_C.All;
              Local_C := Temp_C'Access;
            end if;
          end;

          -- Skip to the
          declare
          Actual_C : Positive;
          for Actual_C'Address use To_Ptr (Local_C);
          begin
            while loop
              if Text (Actual_C) = "}" then
                Skip_Whitespace (Actual_C);
                if Text (Actual_C) = "," then

            end loop;
          end;
        end;

      ---------------------
      -- Parse_Structure --
      ---------------------

      procedure Parse_Structure
        (File          : in out File_State;
         C             : in out Positive;
         Parent        :        Ptr_Structure_State;
         Parent_Counts :        Ptr_Array_Struct_Count)
      is
        Primitive : Primitive_Kind;
        Structure : Structure_Kind;
        Len       : Positive       := 1;
        begin
          Parse_Data_Type (C, Primitive, Structure);

          -- Probe for primitive count hinting
          Skip_Whitespace (C, Skip_Singleline_Comments => False);
          if Text (C..C + 1) = "//" then
            C := C + 2;

            -- Skip to the next non-whitespace character on the current line
            while Char_Catagory (Text (C)) = Whitespace_Catagory and Text (C) /= ASCII.LF loop C := C + 1; end loop;
            Parse_Int32 (C, Len, Allow_Failure => True);

            -- Skip to the end of the line and then the next valid character
            while Text (C) /= ASCII.LF loop C := C + 1; end loop;
            C := C + 1;
            Skip_Whitespace (C);
          end if;

          -- Create the structure node
          declare Struct : Structure_State (Primitive, Len); begin

            -- Optional name
            if Text (C) /= '(' and Text (C) /= '{' then
              Parse_Name (Struct.Identifier, Struct.Is_Global);
              Skip_Whitespace (C);
            else Struct.Identifier := NULL_STR_IDENTIFIER; end if;

            -- Only top-level structures get their own local maps, otherwise link the parent
            if Parent = null then
              Struct.Locals := To_Ptr (new Hashed_Ptr_Structure_State.Unsafe_Map);
              if not Is_Global then raise Local_Name_In_Global_Scope with Struct_Id; end if;
            else
              Struct.Locals := Parent.Locals;
              Struct.Parent := Parent;
            end if;

            -- Non-primitive type
            if Primitive = Structure_Kind then
              declare
              Prop_Id : Str_Identifier;
              Counts  : aliased Array_Struct_Count := (others => 0);
              begin
                Struct.Structure := Structure;
                Parent_Counts (Structure) := Parent_Counts (Structure) + 1;

                -- Properties
                Assert_Next (C, '(');
                if Text (C) = ')' then C := C + 1; else
                  loop
                    Parse_Identifier (C, Prop_Id);
                    Skip_Whitespace (C);
                    Assert_Next (C, "=");
                    Skip_Whitespace (C);
                    for Property of Properties (Structure) loop
                      if Property.Identifier = Prop_Id then
                        declare Current : Structure_State (Property.Kind); begin
                          Parse_Data_List_Item (Current);
                          Struct.Properties.Insert (Prop_Id, Current);
  goto Found_Property;
                        end;
                      end if;
                    end loop;
                    raise Undefined_Property with Prop_Id;
  <<Found_Property>>
                    Skip_Whitespace (C);
                    exit when Text (C) = ')';
                    Assert_Next (C, ',');
                  end loop;
                end if;

                -- Add defaulted and inherited properties
                for Property of Properties (Structure) loop
                  if Property.Identifier /= NULL_IDENTIFIER and then not Struct.Properties.Contains (Property.Identifier) then
                    if    Property.Defaulted    then Struct.Properties.Insert (Property.Identifier, Property.Default);
                    elsif Property.Ref_Override then Struct.Properties.Insert (Property.Identifier, Parent.Properties.Element (Property.Identifier));
                    else raise Required_Property_Not_Present with Property.Identifier; end if;
                  end if;
                end loop;

                -- Parse components
                Skip_Whitespace (C);
                Assert_Next (C, "{");
                loop
                  Skip_Whitespace (C);
                  exit when Text (C) = '}';
                  Parse_Structure (File, C, Struct'Access, Counts);
                end loop;

                -- Verify components
                for I in Struct_T'Range loop
                  case Structure_Components (Structure) (I) is
                    when Zero_Or_One   => if Counts (I) > 1         then raise Component_Mismatch with Structure'Image & " must have 0 or 1 "       & I'Image; end if;
                    when Zero_To_Two   => if Counts (I) > 2         then raise Component_Mismatch with Structure'Image & " must have 0 to 2 "       & I'Image; end if;
                    when Zero_To_Three => if Counts (I) > 3         then raise Component_Mismatch with Structure'Image & " must have 0 to 3 "       & I'Image; end if;
                    when Zero_To_Four  => if Counts (I) > 4         then raise Component_Mismatch with Structure'Image & " must have 0 to 4 "       & I'Image; end if;
                    when Exactly_One   => if Counts (I) /= 1        then raise Component_Mismatch with Structure'Image & " must have exactly one "  & I'Image; end if;
                    when One_Or_Two    => if Counts (I) not in 1..2 then raise Component_Mismatch with Structure'Image & " must have 1 or 2 "       & I'Image; end if;
                    when One_To_Three  => if Counts (I) not in 1..3 then raise Component_Mismatch with Structure'Image & " must have 1 to 3 "       & I'Image; end if;
                    when One_To_Four   => if Counts (I) not in 1..4 then raise Component_Mismatch with Structure'Image & " must have 1 to 4 "       & I'Image; end if;
                    when One_Or_More   => if Counts (I) = 0         then raise Component_Mismatch with Structure'Image & " must have at least one " & I'Image; end if;
                    when Max_One       => if Counts (I) > 1         then raise Component_Mismatch with Structure'Image & " can have max one "       & I'Image; end if;
                    when None          => if Counts (I) /= 0        then raise Component_Mismatch with Structure'Image & " can't have a "           & I'Image; end if;
                    when Zero_Or_More  => null;
                  end case;
                end loop;

                -- Add the current structure
                if Parent = null then
                  if Top_Level_Structures (Structure) then
                    raise Disallowed_Structure_At_Top_Level with Structure'Image;
                  end if;
                  if Struct.Identifier /= NULL_IDENTIFIER then File.Globals.Insert (Struct.Identifier, Struct);
                  else
                    if not Is_Global then raise Local_Structure_At_Top_Level; end if;
                    File.Structures.Append (Struct);
                  end if;
                else
                  if not Hierarchy (Structure) (Parent.Structure) then
                    raise Bad_Hierarchy with Structure'Image & " cannot be a component of a " & Parent.Structure'Image;
                  end if;
                  if Struct.Identifier = NULL_IDENTIFIER then Parent.Children.Append (Struct);
                  else Parent.Locals.Insert (Struct.Identifier, Struct); end if;
                end if;

                -- Register the name in the local or global table
                if Struct.Identifier /= NULL_STR_IDENTIFIER and Is_Global then File.Globals.Insert (Struct.Identifier, Struct_Ref); end if;
              end;

            -- Primitive type
            else
              if Parent = null then raise Primitive_At_Top_Level with Primitive'Image; end if;
              if not Primitive_Components (Parent.Kind).Allowed_Primitives (Primitive) then
                raise Bad_Hierarchy with Primitive'Image & " cannot be a component of a " & Parent.Structure'Image;
              end if;
              if not Primitive_Components (Parent.Kind).Allow_Arrays and Len > 1 then
                raise Unexpected_Primitive_Array with Primitive'Image " cannot be an array component in a " & Parent.Structure'Image;
              end if;

              -- Parse the raw data-list, if we are over our threshold we deploy all the CPUs to speed things up
              Assert_Next (C, "{");
              if Len < WORK_DIVISION_THRESHOLD then Parse_Data_List (C, Struct);
              else
                declare Workers : array (1..Number_Of_CPUs) of Parse_Data_List_Task; Work : Positive := Len / Workers'Length; begin
                  for I in Workers'Range loop
                    if I = Workers'Last then Workers (I).Initialize (C, Work * I, Work + (Len rem Workers'Length), True);
                    else Workers (I).Initialize (C, Work * I, Work, False); end if;
                  end loop;
                end;
              end if;
            end if;

            -- Skip over the last brace
            C := C + 1;
            Skip_Whitespace (C);
          end;
        end;

      -- Start of Load
      begin

        -- Load the file into a buffer
        Open (File, In_File, Path);
        Read (File, Buffer);
        Close (File);

        -- Parse the data
        Skip_Whitespace (C);
        Parse_Structure (File, C, null, null);
      end;
  end;
end;
