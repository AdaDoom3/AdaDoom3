
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

with System.Memory;
package body Neo.Data is

  ------------
  -- Binary --
  ------------

  -- Load a file into a binary buffer
  function Load (Path : Str; Padding : Positive := 1) return Array_Byte is
    use Vector_Int_8_Unsigned; -- For "&"

    -- Setup IO
    subtype Blob is Array_Byte (1..Natural (Ada.Directories.Size (To_Str_8 (Path)))); -- Str_8 !!!
    package Blob_IO is new Ada.Direct_IO (Blob);
    File   : Blob_IO.File_Type;
    Result : Blob := (others => 0);

    begin
      Blob_IO.Open (File, Blob_IO.In_File, To_Str_8 (Path)); -- Str_8 !!!
      Blob_IO.Read (File, Result);
      Blob_IO.Close (File);
      return (if Blob'Length mod Padding = 0 then Result else Result & (1..Result'Length mod Padding => 0));
    end;

  procedure Skip (File : in out Ada.Streams.Stream_IO.File_Type; Bytes : Positive) is
    Junk : Byte := 0;
    begin for I in 1..Bytes loop Byte'Read (Ada.Streams.Stream_IO.Stream (File), Junk); end loop; end;

  --------------------------
  -- Description_Language --
  --------------------------

  package body Description_Language is

    ---------
    -- Get --
    ---------

    function Get (Structure : Ptr_Structure_State; Kind : Struct_T) return Ptr_Structure_State is
      begin
        if Structure.Counts (Kind) /= 1 then raise Program_Error; end if;-- with "Too many " & Kind'Wide_Image & " in node!"; end if;

        if Structure.Named_Children /= null then
          for I of Structure.Named_Children.all loop
            if I.Kind = Structure_Kind and then I.Class = Kind then return I; end if;
          end loop;
        end if;

        if Structure.Children /= null then
          for I of Structure.Children.all loop
            if I.Kind = Structure_Kind and then I.Class = Kind then return I; end if;
          end loop;
        end if;
        return null;
      end;

    function Get (Structure : Ptr_Structure_State; Primitive : Primitive_Kind) return Ptr_Structure_State is
      begin
        if Structure.Named_Children /= null then
          for I of Structure.Named_Children.all loop
            if I.Kind /= Structure_Kind and then I.Kind = Primitive then return I; end if;
          end loop;
        end if;
        if Structure.Children /= null then
          for I of Structure.Children.all loop
            if I.Kind /= Structure_Kind and then I.Kind = Primitive then return I; end if;
          end loop;
        end if;
        return null;
      end;

    function Get (Structure : Ptr_Structure_State; Kind : Struct_T) return Array_Ptr_Structure_State is
      Result  : Array_Ptr_Structure_State (1..Structure.Counts (Kind)) := (others => null);
      Current : Positive := 1;
      begin
        if Structure.Named_Children /= null then
          for I of Structure.Named_Children.all loop
            if I.Kind = Structure_Kind and then I.Class = Kind then
              Result (Current) := I;
              Current := Current + 1;
            end if;
          end loop;
        end if;

        if Structure.Children /= null then
          for I of Structure.Children.all loop
            if I.Kind = Structure_Kind and then I.Class = Kind then
              Result (Current) := I;
              Current := Current + 1;
            end if;
          end loop;
        end if;

        return Result;
      end;

    function Get (Structure : Ptr_Structure_State; Class : Struct_T; Primitive : Primitive_Kind) return Ptr_Structure_State is
      begin
        return Get (Get (Structure, Class), Primitive);
      end;
    function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T) return Array_Ptr_Structure_State is
      (Get (Get (Structure, Path (Path'First..Path'Last - 1)), Path (Path'Last)));

    function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T) return Ptr_Structure_State is
      Result : Ptr_Structure_State := Structure;
      begin
        for I of Path loop
          Result := Get (Result, I);
        end loop;
        return Result;
      end;

    function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T; Primitive : Primitive_Kind) return Ptr_Structure_State is
      Result : Ptr_Structure_State := Structure;
      begin
        for I of Path loop
          Result := Get (Result, I);
        end loop;
        for I of Result.Children.all loop
          if I.Kind /= Structure_Kind and then I.Kind = Primitive then return I; end if;
        end loop;
        return null;
      end;

    ------------------------------
    -- Is_Dimensional_Primitive --
    ------------------------------

    function Is_Dimensional_Primitive (Kind : Primitive_Kind) return Bool is
      (Kind not in Bool_Kind | String_Kind | Ref_Kind | Type_Kind | Any_Float_Kind | Any_Int_Kind | Any_Unsigned_Int_Kind);

    ----------
    -- File --
    ----------

    procedure Finalize (File : in out File_State) is

      -- Recursive deletion
      procedure Finalize (Structure : in out Ptr_Structure_State) with Inline is
        begin
          if Structure.Name /= null then Free (Structure.Name); end if;

          -- Free children if we are looking at a non-primitive
          if Structure.Kind = Structure_Kind then

            -- Kill children
            if Structure.Children /= null then
              for I of Structure.Children.all loop Finalize (I); end loop;
              Free (Structure.Children);
            end if;

            -- Kill properties
            if Structure.Properties /= null then
              for I of Structure.Properties.all loop Finalize (I); end loop;
              Free (Structure.Properties);
            end if;

            -- Kill locals
            if Structure.Named_Children /= null then
              for I of Structure.Named_Children.all loop Finalize (I); end loop;
              Free (Structure.Named_Children);
            end if;
          end if;

          -- Free the structure itself
          Free (Structure);
        end;
      begin
        Free (File.Name);
        for I of File.Structures       loop Finalize (I); end loop;
        for I of File.Named_Structures loop Finalize (I); end loop;
      end;

    ----------
    -- Load --
    ----------

    procedure Load (Path : Str; File : out File_State) is

      -----------------------------
      -- Character Lookup Tables --
      -----------------------------

      -- ???
      type Character_Kind is (Identifier_Catagory, Whitespace_Catagory, Other_Catagory);
      CATAGORY : array (Char_8'First..Char_8'Val (127)) of Character_Kind :=
        ('a'..'z' | 'A'..'Z' | '_' | '0'..'9' => Identifier_Catagory, ' ' | HT | LF | CR | VT => Whitespace_Catagory, others => Other_Catagory);

      -- Valued character based value lookups
      type Array_Lookup_Value is array (Char_8'First..Char_8'Val (127)) of Int_64_Unsigned;
      INVALID_VALUE   : constant Int_64_Unsigned := Int_64_Unsigned'Last;
      SEPORATOR_VALUE : constant Int_64_Unsigned := Int_64_Unsigned'Last - 1;
      HEX_VALUE       : constant Array_Lookup_Value :=
        ('0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
         'A' | 'a' => 10, 'B' | 'b' => 11, 'C' | 'c' => 12, 'D' | 'd' => 13, 'E' | 'e' => 14, 'F' | 'f' => 15,
         '_' => SEPORATOR_VALUE, others => INVALID_VALUE);
      DECIMAL_VALUE   : constant Array_Lookup_Value :=
        ('0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
         '_' => SEPORATOR_VALUE, others => INVALID_VALUE);
      OCTAL_VALUE     : constant Array_Lookup_Value :=
        ('0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7,
         '_' => SEPORATOR_VALUE, others => INVALID_VALUE);
      BINARY_VALUE    : constant Array_Lookup_Value :=
        ('0' => 0, '1' => 1, '_' => SEPORATOR_VALUE, others => INVALID_VALUE);

      --------
      -- IO --
      --------

      function To_Ptr is new Unchecked_Conversion (Ptr_Structure_State, Ptr);

      -- Prepare to load the textual data into a buffer
      subtype File_String is Str_8 (1..Natural (Ada.Directories.Size (To_Str_8 (Path))));
      package File_String_IO is new Ada.Direct_IO (File_String); use File_String_IO;
      Data_File  : File_String_IO.File_Type;
      Text       : File_String;
      Text_Index : aliased Positive := 1;

      ----------------
      -- References --
      ----------------

      -- In OpenDLL forward references are allowed - so we have some overhead associated with a double pass
      type Reference_State is record
          Starts_With_Global : Bool := False;
          Data               : Ptr_Ptr;
          Structure          : Ptr_Structure_State;
          Names              : Vector_Str_Unbound.Unsafe.Vector;
        end record;
      package Vector_Reference_State is new Neo.Core.Vectors (Reference_State);
      References : Vector_Reference_State.Unsafe.Vector;

      -----------------
      -- Assert_Next --
      -----------------

      procedure Assert_Next (C : in out Positive; Val : Char_8) with Inline is
        begin
          if Text (C) = Val then
            C := C + 1;
            return;
          end if;
          raise Syntax_Error with "expected " & Val & " but found " & Text(C) & " as part of " & Text (C - 15..C);
        end;

      ---------------------
      -- Skip_Whitespace --
      ---------------------

      procedure Skip_Whitespace (C : in out Positive; Skip_Single_Line_Comments : Bool := True) with Inline is
        begin
          loop
            while CATAGORY (Text (C)) = Whitespace_Catagory loop C := C + 1; end loop;
            if Text (C) = '/' then
              if Text (C + 1) = '/' and Skip_Single_Line_Comments then
                C := C + 2;
                while Text (C) /= ASCII.LF loop C := C + 1; end loop;
                C := C + 1;
              elsif Text (C + 1) = '*' then
                C := C + 2;
                while Text (C..C + 1) /= "*/" loop C := C + 1; end loop;
                C := C + 2;
              else exit; end if;
            else exit; end if;
          end loop;
        end;

      ----------------
      -- Skip_Until --
      ----------------

      function Skip_Until (C : in out Positive; Val_1, Val_2 : Char_8) return Char_8 with Inline is
        begin
          loop
            while CATAGORY (Text (C)) = Whitespace_Catagory loop C := C + 1; end loop;
            if Text (C) = '/' then
              if Text (C + 1) = '/' then
                C := C + 2;
                while Text (C) /= ASCII.LF loop C := C + 1; end loop;
                C := C + 1;
              elsif Text (C + 1) = '*' then
                C := C + 2;
                while Text (C..C + 1) /= "*/" loop C := C + 1; end loop;
                C := C + 2;
              elsif Text (C) = Val_1 then return Val_1;
              elsif Text (C) = Val_2 then return Val_2;
              else C := C + 1; end if;
            elsif Text (C) = Val_1 then return Val_1;
            elsif Text (C) = Val_2 then return Val_2;
            else C := C + 1; end if;
          end loop;
        end;

      procedure Skip_Until (C : in out Positive; Val : Char_8) with Inline is
        begin
          loop
            while CATAGORY (Text (C)) = Whitespace_Catagory loop C := C + 1; end loop;
            if Text (C) = '/' then
              if Text (C + 1) = '/' then
                C := C + 2;
                while Text (C) /= ASCII.LF loop C := C + 1; end loop;
                C := C + 1;
              elsif Text (C + 1) = '*' then
                C := C + 2;
                while Text (C..C + 1) /= "*/" loop C := C + 1; end loop;
                C := C + 2;
              elsif Text (C) = Val then return;
              else C := C + 1; end if;
            elsif Text (C) = Val then return;
            else C := C + 1; end if;
          end loop;
        end;

      ---------------------
      -- Parse_Character --
      ---------------------

      procedure Parse_Character (C : in out Positive; Result : out Char_32) with Inline is

        function To_Byte (C : Positive) return Int_8_Unsigned with Inline is
          function Internal is new Unchecked_Conversion (Char_8, Int_8_Unsigned);
          begin
            return Internal (Text (C));
          end;

        -- Parse a string of characters representing a hex encoded number
        procedure Parse_Valued_Character (Hex_Char_Count : Positive) with Inline is
          Value : Int_32_Unsigned := 0;
          begin
            for I in 1..Hex_Char_Count loop
              if HEX_VALUE (Text (C + I)) = INVALID_VALUE then raise Invalid_Hex_In_Valued_Character; end if;
              Result := Char_32'Val (Shift_Left (HEX_VALUE (Text (C + I)), (Hex_Char_Count - I) * 8) or Char_32'Pos (Result));
            end loop;
          end;

        -- Start of Parse_Character
        begin
          if (To_Byte (C) and 16#80#) = 16#00# then

            -- Escape sequence
            if Text (C) = '\' then
              C := C + 1;
              case Text (C) is
                when '"' | ''' | '?' | '\' => Result := Char_32'Val (Char_8'Pos (Text (C)));
                when 'a'                   => Result := Latin_32.BEL;
                when 'b'                   => Result := Latin_32.BS;
                when 'f'                   => Result := Latin_32.FF;
                when 'n'                   => Result := Latin_32.LF;
                when 'r'                   => Result := Latin_32.CR;
                when 't'                   => Result := Latin_32.HT;
                when 'v'                   => Result := Latin_32.VT;
                when 'x'                   => Parse_Valued_Character (2); C := C + 3; return;
                when 'u'                   => Parse_Valued_Character (4); C := C + 5; return;
                when 'U'                   => Parse_Valued_Character (6); C := C + 7; return;
                when others                => raise Invalid_Escape_Character;
              end case;
              C := C + 1;

            -- Normal ASCII
            else
              if To_Byte (C) < 32 or To_Byte (C) >= 127 then raise Invalid_Character; end if;
              Result := Char_32'Val (Char_8'Pos (Text (C)));
              C := C + 1;
            end if;

          -- UTF-8 higher code-points
          elsif (To_Byte (C) and 16#E0#) = 16#C0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#1F#,  6)
                                                                           or            (To_Byte (C + 1) and 16#3F#)); C := C + 2;
          elsif (To_Byte (C) and 16#F0#) = 16#E0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#0F#, 12)
                                                                           or Shift_Left (To_Byte (C + 1) and 16#3F#,  6)
                                                                           or            (To_Byte (C + 2) and 16#3F#)); C := C + 2;
          elsif (To_Byte (C) and 16#F8#) = 16#F0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#07#, 18)
                                                                           or Shift_Left (To_Byte (C + 1) and 16#3F#, 12)
                                                                           or Shift_Left (To_Byte (C + 2) and 16#3F#,  6)
                                                                           or            (To_Byte (C + 3) and 16#3F#)); C := C + 3;
          else raise Invalid_Character_Literal; end if;
        end;

      -----------------------------
      -- Parse_Character_Literal --
      -----------------------------

      procedure Parse_Character_Literal (C : in out Positive; Result : out Char_32) with Inline is
        begin
          Assert_Next (C, ''');
          Parse_Character (C, Result);
          Assert_Next (C, ''');
        end;

      --------------------------
      -- Parse_String_Literal --
      --------------------------

      procedure Parse_String_Literal (C : in out Positive; Result : out Str_32_Unbound) with Inline is
        Temp : Char_32;
        begin
          Assert_Next (C, '"');
          loop
            Parse_Character (C, Temp);
            exit when Temp = '"';
            Append (Result, Temp);
          end loop;
        end;

      ------------------------
      -- Parse_Bool_Literal --
      ------------------------

      procedure Parse_Bool_Literal (C : in out Positive; Result : out Bool) with Inline is
        begin
          if    Text (C..C + 4) = "false" then Result := False; C := C + 4;
          elsif Text (C..C + 3) = "true"  then Result := True;  C := C + 3;
          else raise Invalid_Float_Literal; end if;
        end;

      -------------------------
      -- Parse_Based_Literal --
      -------------------------

      generic
        type Base_T is private;
      function Parse_Based_Literal (C : in out Positive; Result : out Base_T) return Bool with Inline;
      function Parse_Based_Literal (C : in out Positive; Result : out Base_T) return Bool is
        Temp_Result : Int_64_Unsigned := 0;

pragma Warnings (Off); -- warning: high order bits will be ignored
        function To_Base_T is new Ada.Unchecked_Conversion (Int_64_Unsigned, Base_T);
pragma Warnings (On);

        procedure Parse_For_Base (Base : Positive; Shift_Amount : Natural; Lookup_Table : Array_Lookup_Value) is
          begin

            -- Load the digits
            while Lookup_Table (Text (C)) /= INVALID_VALUE loop

              -- Skip over separators if present
              if Lookup_Table (Text (C)) /= SEPORATOR_VALUE then
                Temp_Result := Shift_Left (Temp_Result, Shift_Amount) or Lookup_Table (Text (C));
              end if;

              C := C + 1;
            end loop;
          end;

        -- Start of Parse_Based_Literal
        begin

          -- Check for a different base notation
          if Text (C) = '0' and then Text (C + 1) in 'o' | 'x' | 'b' then
            C := C + 1;

            -- Read the number and return success
            case Text (C) is
              when 'x' => Parse_For_Base (16, 4, HEX_VALUE);
              when 'o' => Parse_For_Base (4,  2, OCTAL_VALUE);
              when 'b' => Parse_For_Base (2,  1, BINARY_VALUE);
              when others => raise Syntax_Error;
            end case;
            Result := To_Base_T (Temp_Result);
            return True;
          end if;

          -- This is not a based literal
          return False;
        end;

      ---------------------------
      -- Parse_Integer_Literal --
      ---------------------------

      generic
        type Int_T is (<>);
      procedure Parse_Integer_Literal (C : in out Positive; Result : out Int_T) with Inline;
      procedure Parse_Integer_Literal (C : in out Positive; Result : out Int_T) is
        function Parse_Based_Integer_Literal is new Parse_Based_Literal (Int_T);
        begin

          -- Handle hex, octal, or binary literal
          if Parse_Based_Integer_Literal (C, Result) then null;

          -- Non-decimal integer, load character-based number
          elsif Text (C) = ''' then
            declare Char_Literal : Char_32; begin
              Parse_Character_Literal (C, Char_Literal);
              Result := Int_T'Val (Char_32'Pos (Char_Literal));
            end;

          -- Load normal decimal number
          else
            declare
            Temp_Result : Int_64_Unsigned := 0;
            Is_Negative : Bool;
            begin

              -- Check for sign
              if Text (C) = '-' then C := C + 1; Is_Negative := True;
              else
                 Is_Negative := False;
                 if Text (C) = '+' then C := C + 1; end if;
              end if;

              -- Load the digits
              while DECIMAL_VALUE (Text (C)) /= INVALID_VALUE loop

                -- Skip over separators if present
                if DECIMAL_VALUE (Text (C)) /= SEPORATOR_VALUE then
                  Temp_Result := Temp_Result * 10 + DECIMAL_VALUE (Text (C));
                end if;

                C := C + 1;
              end loop;

              -- Set the result accordingly
              if Is_Negative then Result := Int_T'Val (-Int_64 (Temp_Result));
              else                Result := Int_T'Val (Temp_Result); end if;
            end;
          end if;
        end;

      -- Instantiations
      procedure Parse_Int8_Literal           is new Parse_Integer_Literal (Int_8);
      procedure Parse_Int16_Literal          is new Parse_Integer_Literal (Int_16);
      procedure Parse_Int32_Literal          is new Parse_Integer_Literal (Int);
      procedure Parse_Int64_Literal          is new Parse_Integer_Literal (Int_64);
      procedure Parse_Unsigned_Int8_Literal  is new Parse_Integer_Literal (Int_8_Unsigned);
      procedure Parse_Unsigned_Int16_Literal is new Parse_Integer_Literal (Int_16_Unsigned);
      procedure Parse_Unsigned_Int32_Literal is new Parse_Integer_Literal (Int_32_Unsigned);
      procedure Parse_Unsigned_Int64_Literal is new Parse_Integer_Literal (Int_64_Unsigned);

      -------------------------
      -- Parse_Float_Literal --
      -------------------------

      -- ???
      generic
        type Real_T is digits <>;
      procedure Parse_Float_Literal (C : in out Positive; Result : out Real_T) with Inline;
      procedure Parse_Float_Literal (C : in out Positive; Result : out Real_T) is
        function Parse_Based_Float_Literal is new Parse_Based_Literal (Real_T);

        LOG_10      : constant Real_64 := 2.3025850929940456840179914546844;
        Decimal     :          Real_T  := 10.0;
        Exponent    :          Int     := 0;
        Is_Negative :          Bool;

        procedure Parse_Real_Digits (Before_Deciaml_Point : Bool) is
          begin

            -- Load the digits
            while DECIMAL_VALUE (Text (C)) /= INVALID_VALUE loop

              -- Skip over separators if present
              if DECIMAL_VALUE (Text (C)) /= SEPORATOR_VALUE then
                if Before_Deciaml_Point then
                  Result  := Result  * 10.0 + Real_T (DECIMAL_VALUE (Text (C)));
                else
                  Result  := Result  + (Real_T (DECIMAL_VALUE (Text (C))) / Decimal);
                  Decimal := Decimal * 10.0;
                end if;
              end if;

              C := C + 1;
            end loop;
          end;

        -- Start of Parse_Float_Literal
        begin

          -- Check for a float specified in hex
          if Parse_Based_Float_Literal (C, Result) then return; end if;

          -- Check for sign
          if Text (C) = '-' then C := C + 1; Is_Negative := True;
          else
            Is_Negative := False;
            if Text (C) = '+' then C := C + 1; end if;
          end if;

          -- Read the numbers
          Result := 0.0;
          Parse_Real_Digits (Before_Deciaml_Point => True);
          if Text (C) = '.' then
            C := C + 1;
            Parse_Real_Digits (Before_Deciaml_Point => False);
          end if;

          -- Negate if necessary
          if Is_Negative then Result := -Result; end if;

          -- Exponent notation
          if Text (C) = 'e' or Text (C) = 'E' then
            C := C + 1;
            Parse_Int32_Literal (C, Exponent);
            Result := Result * Real_T (Calc_64.Exp (Real_64 (Exponent) * LOG_10));
          end if;
        end;

      -- Instantiations
      procedure Parse_Float16_Literal is new Parse_Float_Literal (Real_16);
      procedure Parse_Float32_Literal is new Parse_Float_Literal (Real_32);
      procedure Parse_Float64_Literal is new Parse_Float_Literal (Real_64);

      -----------------------
      -- Parse_Indentifier --
      -----------------------

      procedure Parse_Identifier (C : in out Positive; Result : out Str_8_Super) with Inline is
        begin

          -- Make sure we are dealing with a character which is allowed within an identifier
          case CATAGORY (Text (C)) is
            when Other_Catagory      => raise Invalid_Characters_In_Identifier;
            when Whitespace_Catagory => raise Identifier_Expected;
            when Identifier_Catagory =>

              -- Append the character and continue
              Super_Append (Result, Text (C));
              C := C + 1;

              -- Load the rest of the identifier
              loop
                if CATAGORY (Text (C)) /= Identifier_Catagory then exit; end if;
                Super_Append (Result, Text (C));
                C := C + 1;
              end loop;
          end case;
        end;

      ----------------
      -- Parse_Name --
      ----------------

      procedure Parse_Name (C : in out Positive; Identifier : out Str_8_Super; Is_Global : out Bool) with Inline is
        begin
          case Text (C) is
            when '$'    => Is_Global := True;
            when '%'    => Is_Global := False;
            when others => raise Invalid_Name;
          end case;
          C := C + 1;
          Parse_Identifier (C, Identifier);
        end;

      ---------------------
      -- Parse_Reference --
      ---------------------

      procedure Parse_Reference
        (C         : in out Positive;
         Ref_Ptr   :        Ptr_Ptr;
         Structure :        Ptr_Structure_State) with Inline
      is
        Reference  : Reference_State;
        Identifier : Str_8_Super (IDENTIFIER_MAX);
        begin

          -- Check for null
          if Text (C..C + 3) = "null" then C := C + 4; Ref_Ptr.all := NULL_PTR; return; end if;

          -- Parse the first reference in a possible list
          Reference.Data      := Ref_Ptr;
          Reference.Structure := Structure;
          Parse_Name (C, Identifier, Reference.Starts_With_Global);
          Reference.Names.Append (To_Str_Unbound (Identifier));
          Skip_Whitespace (C);

          -- Move along a possible string of local names
          while Text (C) = '%' loop
            C := C + 1;
            Parse_Identifier (C, Identifier);
            Reference.Names.Append (To_Str_Unbound (Identifier));
            Skip_Whitespace (C);
          end loop;
          References.Append (Reference);
        end;

      -----------------------------
      -- Parse_Data_Type_Literal --
      -----------------------------

      procedure Parse_Data_Type_Literal (C : in out Positive; Primitive : out Primitive_Kind; Structure : out Struct_T) with Inline is

        -- ???
        procedure Parse_Rest (Rest : Str_8; Kind : Primitive_Kind; Test_Bitsize : Bool := False; Disble_8_Bit : Bool := False) with Inline is
          begin
            C := C + 1;

            -- Check if we are dealing with abbreviated primitives (e.g. u8, i64, f32)
            if Text (C..C + Rest'Length - 1) = Rest then C := C + Rest'Length; end if;

            -- Load the optional bitsize
            if CATAGORY (Text (C)) /= Identifier_Catagory then
              if not Test_Bitsize then Primitive := Kind;
              else Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Kind) + 2); end if; -- Assume 32-bit
            else
              case Text (C) is
                when '8'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Kind));     C := C + 1; if Disble_8_Bit        then raise Invalid_Data_Type_Literal; end if;
                when '1'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Kind) + 1); C := C + 2; if Text (C - 1) /= '6' then raise Invalid_Data_Type_Literal; end if;
                when '3'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Kind) + 2); C := C + 2; if Text (C - 1) /= '2' then raise Invalid_Data_Type_Literal; end if;
                when '6'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Kind) + 3); C := C + 2; if Text (C - 1) /= '4' then raise Invalid_Data_Type_Literal; end if;
                when others => raise Invalid_Data_Type_Literal;
              end case;
              if CATAGORY (Text (C)) = Identifier_Catagory then raise Invalid_Data_Type_Literal; end if;
            end if;

            -- Handle possible array types
            if Text (C) = '[' then
              C := C + 1;
              declare
              Enum_Traverse_Multiplier : Positive;
              begin
                case Primitive is
                  when Any_Float_Kind                                 => Enum_Traverse_Multiplier := 3;
                  when Any_Int_Kind | Any_Unsigned_Int_Kind           => Enum_Traverse_Multiplier := 4;
                  when Bool_Kind | String_Kind | Ref_Kind | Type_Kind => Enum_Traverse_Multiplier := 1;
                  when others => raise Program_Error;
                end case;
                case Text (C) is
                  when '2'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 1 * Enum_Traverse_Multiplier); C := C + 1;
                  when '3'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 2 * Enum_Traverse_Multiplier); C := C + 1;
                  when '4'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 3 * Enum_Traverse_Multiplier); C := C + 1;
                  when '8'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 4 * Enum_Traverse_Multiplier); C := C + 1;
                  when '1'    => Primitive := Primitive_Kind'Val (Primitive_Kind'Pos (Primitive) + 5 * Enum_Traverse_Multiplier); C := C + 2;
                                 if Text (C - 1) /= '6' then raise Invalid_Data_Type_Literal; end if;
                  when others => raise Invalid_Data_Type_Literal;
                end case;
              end;
              Assert_Next (C, ']');
            end if;
          end;

        -- Start of Parse_Data_Type_Literal
        begin
          case Text (C) is
            when 'i'    => Parse_Rest ("nt",          Int8_Kind,          Test_Bitsize => True);
            when 'u'    => Parse_Rest ("nsigned_int", Unsigned_Int8_Kind, Test_Bitsize => True);
            when 'f'    => Parse_Rest ("loat",        Type_x16_Kind,      Test_Bitsize => True, Disble_8_Bit => True); -- Dirty hack here for Kind
            when 'b'    => Parse_Rest ("ool",         Bool_Kind);
            when 'h'    => Parse_Rest ("alf",         Float16_Kind);
            when 'd'    => Parse_Rest ("ouble",       Float64_Kind);
            when 's'    => Parse_Rest ("tring",       String_Kind);
            when 'r'    => Parse_Rest ("ef",          Ref_Kind);
            when 't'    => Parse_Rest ("ype",         Type_Kind);
            when others => Primitive := Structure_Kind; Struct_Types.Parse_Data_Type (Text, C, Structure); -- Structure names should always be uppercase
          end case;
        end;

      -------------------------
      -- Construct_Structure --
      -------------------------

      procedure Construct_Structure (Item : in out Ptr_Structure_State; Kind : Primitive_Kind; Len : Positive) is
        begin
          case Kind is
            when Bool_Kind               => Item := new Structure_State'(Bool_Kind,               Len, others => <>);
            when Bool_x2_Kind            => Item := new Structure_State'(Bool_x2_Kind,            Len, others => <>);
            when Bool_x3_Kind            => Item := new Structure_State'(Bool_x3_Kind,            Len, others => <>);
            when Bool_x4_Kind            => Item := new Structure_State'(Bool_x4_Kind,            Len, others => <>);
            when Bool_x8_Kind            => Item := new Structure_State'(Bool_x8_Kind,            Len, others => <>);
            when Bool_x16_Kind           => Item := new Structure_State'(Bool_x16_Kind,           Len, others => <>);
            when String_Kind             => Item := new Structure_State'(String_Kind,             Len, others => <>);
            when String_x2_Kind          => Item := new Structure_State'(String_x2_Kind,          Len, others => <>);
            when String_x3_Kind          => Item := new Structure_State'(String_x3_Kind,          Len, others => <>);
            when String_x4_Kind          => Item := new Structure_State'(String_x4_Kind,          Len, others => <>);
            when String_x8_Kind          => Item := new Structure_State'(String_x8_Kind,          Len, others => <>);
            when String_x16_Kind         => Item := new Structure_State'(String_x16_Kind,         Len, others => <>);
            when Ref_Kind                => Item := new Structure_State'(Ref_Kind,                Len, others => <>);
            when Ref_x2_Kind             => Item := new Structure_State'(Ref_x2_Kind,             Len, others => <>);
            when Ref_x3_Kind             => Item := new Structure_State'(Ref_x3_Kind,             Len, others => <>);
            when Ref_x4_Kind             => Item := new Structure_State'(Ref_x4_Kind,             Len, others => <>);
            when Ref_x8_Kind             => Item := new Structure_State'(Ref_x8_Kind,             Len, others => <>);
            when Ref_x16_Kind            => Item := new Structure_State'(Ref_x16_Kind,            Len, others => <>);
            when Type_Kind               => Item := new Structure_State'(Type_Kind,               Len, others => <>);
            when Type_x2_Kind            => Item := new Structure_State'(Type_x2_Kind,            Len, others => <>);
            when Type_x3_Kind            => Item := new Structure_State'(Type_x3_Kind,            Len, others => <>);
            when Type_x4_Kind            => Item := new Structure_State'(Type_x4_Kind,            Len, others => <>);
            when Type_x8_Kind            => Item := new Structure_State'(Type_x8_Kind,            Len, others => <>);
            when Type_x16_Kind           => Item := new Structure_State'(Type_x16_Kind,           Len, others => <>);
            when Float16_Kind            => Item := new Structure_State'(Float16_Kind,            Len, others => <>);
            when Float32_Kind            => Item := new Structure_State'(Float32_Kind,            Len, others => <>);
            when Float64_Kind            => Item := new Structure_State'(Float64_Kind,            Len, others => <>);
            when Float16_x2_Kind         => Item := new Structure_State'(Float16_x2_Kind,         Len, others => <>);
            when Float32_x2_Kind         => Item := new Structure_State'(Float32_x2_Kind,         Len, others => <>);
            when Float64_x2_Kind         => Item := new Structure_State'(Float64_x2_Kind,         Len, others => <>);
            when Float16_x3_Kind         => Item := new Structure_State'(Float16_x3_Kind,         Len, others => <>);
            when Float32_x3_Kind         => Item := new Structure_State'(Float32_x3_Kind,         Len, others => <>);
            when Float64_x3_Kind         => Item := new Structure_State'(Float64_x3_Kind,         Len, others => <>);
            when Float16_x4_Kind         => Item := new Structure_State'(Float16_x4_Kind,         Len, others => <>);
            when Float32_x4_Kind         => Item := new Structure_State'(Float32_x4_Kind,         Len, others => <>);
            when Float64_x4_Kind         => Item := new Structure_State'(Float64_x4_Kind,         Len, others => <>);
            when Float16_x8_Kind         => Item := new Structure_State'(Float16_x8_Kind,         Len, others => <>);
            when Float32_x8_Kind         => Item := new Structure_State'(Float32_x8_Kind,         Len, others => <>);
            when Float64_x8_Kind         => Item := new Structure_State'(Float64_x8_Kind,         Len, others => <>);
            when Float16_x16_Kind        => Item := new Structure_State'(Float16_x16_Kind,        Len, others => <>);
            when Float32_x16_Kind        => Item := new Structure_State'(Float32_x16_Kind,        Len, others => <>);
            when Float64_x16_Kind        => Item := new Structure_State'(Float64_x16_Kind,        Len, others => <>);
            when Int8_Kind               => Item := new Structure_State'(Int8_Kind,               Len, others => <>);
            when Int16_Kind              => Item := new Structure_State'(Int16_Kind,              Len, others => <>);
            when Int32_Kind              => Item := new Structure_State'(Int32_Kind,              Len, others => <>);
            when Int64_Kind              => Item := new Structure_State'(Int64_Kind,              Len, others => <>);
            when Int8_x2_Kind            => Item := new Structure_State'(Int8_x2_Kind,            Len, others => <>);
            when Int16_x2_Kind           => Item := new Structure_State'(Int16_x2_Kind,           Len, others => <>);
            when Int32_x2_Kind           => Item := new Structure_State'(Int32_x2_Kind,           Len, others => <>);
            when Int64_x2_Kind           => Item := new Structure_State'(Int64_x2_Kind,           Len, others => <>);
            when Int8_x3_Kind            => Item := new Structure_State'(Int8_x3_Kind,            Len, others => <>);
            when Int16_x3_Kind           => Item := new Structure_State'(Int16_x3_Kind,           Len, others => <>);
            when Int32_x3_Kind           => Item := new Structure_State'(Int32_x3_Kind,           Len, others => <>);
            when Int64_x3_Kind           => Item := new Structure_State'(Int64_x3_Kind,           Len, others => <>);
            when Int8_x4_Kind            => Item := new Structure_State'(Int8_x4_Kind,            Len, others => <>);
            when Int16_x4_Kind           => Item := new Structure_State'(Int16_x4_Kind,           Len, others => <>);
            when Int32_x4_Kind           => Item := new Structure_State'(Int32_x4_Kind,           Len, others => <>);
            when Int64_x4_Kind           => Item := new Structure_State'(Int64_x4_Kind,           Len, others => <>);
            when Int8_x8_Kind            => Item := new Structure_State'(Int8_x8_Kind,            Len, others => <>);
            when Int16_x8_Kind           => Item := new Structure_State'(Int16_x8_Kind,           Len, others => <>);
            when Int32_x8_Kind           => Item := new Structure_State'(Int32_x8_Kind,           Len, others => <>);
            when Int64_x8_Kind           => Item := new Structure_State'(Int64_x8_Kind,           Len, others => <>);
            when Int8_x16_Kind           => Item := new Structure_State'(Int8_x16_Kind,           Len, others => <>);
            when Int16_x16_Kind          => Item := new Structure_State'(Int16_x16_Kind,          Len, others => <>);
            when Int32_x16_Kind          => Item := new Structure_State'(Int32_x16_Kind,          Len, others => <>);
            when Int64_x16_Kind          => Item := new Structure_State'(Int64_x16_Kind,          Len, others => <>);
            when Unsigned_Int8_Kind      => Item := new Structure_State'(Unsigned_Int8_Kind,      Len, others => <>);
            when Unsigned_Int16_Kind     => Item := new Structure_State'(Unsigned_Int16_Kind,     Len, others => <>);
            when Unsigned_Int32_Kind     => Item := new Structure_State'(Unsigned_Int32_Kind,     Len, others => <>);
            when Unsigned_Int64_Kind     => Item := new Structure_State'(Unsigned_Int64_Kind,     Len, others => <>);
            when Unsigned_Int8_x2_Kind   => Item := new Structure_State'(Unsigned_Int8_x2_Kind,   Len, others => <>);
            when Unsigned_Int16_x2_Kind  => Item := new Structure_State'(Unsigned_Int16_x2_Kind,  Len, others => <>);
            when Unsigned_Int32_x2_Kind  => Item := new Structure_State'(Unsigned_Int32_x2_Kind,  Len, others => <>);
            when Unsigned_Int64_x2_Kind  => Item := new Structure_State'(Unsigned_Int64_x2_Kind,  Len, others => <>);
            when Unsigned_Int8_x3_Kind   => Item := new Structure_State'(Unsigned_Int8_x3_Kind,   Len, others => <>);
            when Unsigned_Int16_x3_Kind  => Item := new Structure_State'(Unsigned_Int16_x3_Kind,  Len, others => <>);
            when Unsigned_Int32_x3_Kind  => Item := new Structure_State'(Unsigned_Int32_x3_Kind,  Len, others => <>);
            when Unsigned_Int64_x3_Kind  => Item := new Structure_State'(Unsigned_Int64_x3_Kind,  Len, others => <>);
            when Unsigned_Int8_x4_Kind   => Item := new Structure_State'(Unsigned_Int8_x4_Kind,   Len, others => <>);
            when Unsigned_Int16_x4_Kind  => Item := new Structure_State'(Unsigned_Int16_x4_Kind,  Len, others => <>);
            when Unsigned_Int32_x4_Kind  => Item := new Structure_State'(Unsigned_Int32_x4_Kind,  Len, others => <>);
            when Unsigned_Int64_x4_Kind  => Item := new Structure_State'(Unsigned_Int64_x4_Kind,  Len, others => <>);
            when Unsigned_Int8_x8_Kind   => Item := new Structure_State'(Unsigned_Int8_x8_Kind,   Len, others => <>);
            when Unsigned_Int16_x8_Kind  => Item := new Structure_State'(Unsigned_Int16_x8_Kind,  Len, others => <>);
            when Unsigned_Int32_x8_Kind  => Item := new Structure_State'(Unsigned_Int32_x8_Kind,  Len, others => <>);
            when Unsigned_Int64_x8_Kind  => Item := new Structure_State'(Unsigned_Int64_x8_Kind,  Len, others => <>);
            when Unsigned_Int8_x16_Kind  => Item := new Structure_State'(Unsigned_Int8_x16_Kind,  Len, others => <>);
            when Unsigned_Int16_x16_Kind => Item := new Structure_State'(Unsigned_Int16_x16_Kind, Len, others => <>);
            when Unsigned_Int32_x16_Kind => Item := new Structure_State'(Unsigned_Int32_x16_Kind, Len, others => <>);
            when Unsigned_Int64_x16_Kind => Item := new Structure_State'(Unsigned_Int64_x16_Kind, Len, others => <>);
            when Structure_Kind          => Item := new Structure_State'(Structure_Kind,          Len, others => <>);
          end case;
        end;

      --------------------------
      -- Parse_Data_List_Item --
      --------------------------

      procedure List (C : in out Positive; J, Last : Positive) with Inline is
        begin
          Skip_Whitespace (C);
          if J /= Last then
            Assert_Next (C, ',');
            Skip_Whitespace (C);
          end if;
        end;

      -- Cost of static typing, totally worth
      procedure Parse_Data_List_Item (C : in out Positive; Structure : in out Ptr_Structure_State; I : Positive := 1) with Inline is
        procedure L (C : in out Positive; J, Last : Positive) renames List;
        P    : Primitive_State renames Structure.Primitive;
        Junk : Struct_T;
        begin
          case Structure.Kind is
            when Bool_Kind               =>                     Parse_Bool_Literal           (C, P.Bool_Val               (I));
            when Bool_x2_Kind            => for J in 1..2  loop Parse_Bool_Literal           (C, P.Bool_x2_Val            (I, J)); L (C, J, 2);  end loop;
            when Bool_x3_Kind            => for J in 1..3  loop Parse_Bool_Literal           (C, P.Bool_x3_Val            (I, J)); L (C, J, 3);  end loop;
            when Bool_x4_Kind            => for J in 1..4  loop Parse_Bool_Literal           (C, P.Bool_x4_Val            (I, J)); L (C, J, 4);  end loop;
            when Bool_x8_Kind            => for J in 1..8  loop Parse_Bool_Literal           (C, P.Bool_x8_Val            (I, J)); L (C, J, 8);  end loop;
            when Bool_x16_Kind           => for J in 1..16 loop Parse_Bool_Literal           (C, P.Bool_x16_Val           (I, J)); L (C, J, 16); end loop;
            when String_Kind             =>                     Parse_String_Literal         (C, P.String_Val             (I));
            when String_x2_Kind          => for J in 1..2  loop Parse_String_Literal         (C, P.String_x2_Val          (I, J)); L (C, J, 2);  end loop;
            when String_x3_Kind          => for J in 1..3  loop Parse_String_Literal         (C, P.String_x3_Val          (I, J)); L (C, J, 3);  end loop;
            when String_x4_Kind          => for J in 1..4  loop Parse_String_Literal         (C, P.String_x4_Val          (I, J)); L (C, J, 4);  end loop;
            when String_x8_Kind          => for J in 1..8  loop Parse_String_Literal         (C, P.String_x8_Val          (I, J)); L (C, J, 8);  end loop;
            when String_x16_Kind         => for J in 1..16 loop Parse_String_Literal         (C, P.String_x16_Val         (I, J)); L (C, J, 16); end loop;
            when Ref_Kind                =>                     Parse_Reference              (C, P.Ref_Val                (I)'Unchecked_Access,    Structure);
            when Ref_x2_Kind             => for J in 1..2  loop Parse_Reference              (C, P.Ref_x2_Val             (I, J)'Unchecked_Access, Structure); L (C, J, 2);  end loop;
            when Ref_x3_Kind             => for J in 1..3  loop Parse_Reference              (C, P.Ref_x3_Val             (I, J)'Unchecked_Access, Structure); L (C, J, 3);  end loop;
            when Ref_x4_Kind             => for J in 1..4  loop Parse_Reference              (C, P.Ref_x4_Val             (I, J)'Unchecked_Access, Structure); L (C, J, 4);  end loop;
            when Ref_x8_Kind             => for J in 1..8  loop Parse_Reference              (C, P.Ref_x8_Val             (I, J)'Unchecked_Access, Structure); L (C, J, 8);  end loop;
            when Ref_x16_Kind            => for J in 1..16 loop Parse_Reference              (C, P.Ref_x16_Val            (I, J)'Unchecked_Access, Structure); L (C, J, 16); end loop;
            when Type_Kind               =>                     Parse_Data_Type_Literal      (C, P.Type_Val               (I),    Junk);
            when Type_x2_Kind            => for J in 1..2  loop Parse_Data_Type_Literal      (C, P.Type_x2_Val            (I, J), Junk); L (C, J, 2);  end loop;
            when Type_x3_Kind            => for J in 1..3  loop Parse_Data_Type_Literal      (C, P.Type_x3_Val            (I, J), Junk); L (C, J, 3);  end loop;
            when Type_x4_Kind            => for J in 1..4  loop Parse_Data_Type_Literal      (C, P.Type_x4_Val            (I, J), Junk); L (C, J, 4);  end loop;
            when Type_x8_Kind            => for J in 1..8  loop Parse_Data_Type_Literal      (C, P.Type_x8_Val            (I, J), Junk); L (C, J, 8);  end loop;
            when Type_x16_Kind           => for J in 1..16 loop Parse_Data_Type_Literal      (C, P.Type_x16_Val           (I, J), Junk); L (C, J, 16); end loop;
            when Float16_Kind            =>                     Parse_Float16_Literal        (C, P.Float16_Val            (I));
            when Float32_Kind            =>                     Parse_Float32_Literal        (C, P.Float32_Val            (I));
            when Float64_Kind            =>                     Parse_Float64_Literal        (C, P.Float64_Val            (I));
            when Float16_x2_Kind         => for J in 1..2  loop Parse_Float16_Literal        (C, P.Float16_x2_Val         (I, J)); L (C, J, 2);  end loop;
            when Float32_x2_Kind         => for J in 1..2  loop Parse_Float32_Literal        (C, P.Float32_x2_Val         (I, J)); L (C, J, 2);  end loop;
            when Float64_x2_Kind         => for J in 1..2  loop Parse_Float64_Literal        (C, P.Float64_x2_Val         (I, J)); L (C, J, 2);  end loop;
            when Float16_x3_Kind         => for J in 1..3  loop Parse_Float16_Literal        (C, P.Float16_x3_Val         (I, J)); L (C, J, 3);  end loop;
            when Float32_x3_Kind         => for J in 1..3  loop Parse_Float32_Literal        (C, P.Float32_x3_Val         (I, J)); L (C, J, 3);  end loop;
            when Float64_x3_Kind         => for J in 1..3  loop Parse_Float64_Literal        (C, P.Float64_x3_Val         (I, J)); L (C, J, 3);  end loop;
            when Float16_x4_Kind         => for J in 1..4  loop Parse_Float16_Literal        (C, P.Float16_x4_Val         (I, J)); L (C, J, 4);  end loop;
            when Float32_x4_Kind         => for J in 1..4  loop Parse_Float32_Literal        (C, P.Float32_x4_Val         (I, J)); L (C, J, 4);  end loop;
            when Float64_x4_Kind         => for J in 1..4  loop Parse_Float64_Literal        (C, P.Float64_x4_Val         (I, J)); L (C, J, 4);  end loop;
            when Float16_x8_Kind         => for J in 1..8  loop Parse_Float16_Literal        (C, P.Float16_x8_Val         (I, J)); L (C, J, 8);  end loop;
            when Float32_x8_Kind         => for J in 1..8  loop Parse_Float32_Literal        (C, P.Float32_x8_Val         (I, J)); L (C, J, 8);  end loop;
            when Float64_x8_Kind         => for J in 1..8  loop Parse_Float64_Literal        (C, P.Float64_x8_Val         (I, J)); L (C, J, 8);  end loop;
            when Float16_x16_Kind        => for J in 1..16 loop Parse_Float16_Literal        (C, P.Float16_x16_Val        (I, J)); L (C, J, 16); end loop;
            when Float32_x16_Kind        => for J in 1..16 loop Parse_Float32_Literal        (C, P.Float32_x16_Val        (I, J)); L (C, J, 16); end loop;
            when Float64_x16_Kind        => for J in 1..16 loop Parse_Float64_Literal        (C, P.Float64_x16_Val        (I, J)); L (C, J, 16); end loop;
            when Int8_Kind               =>                     Parse_Int8_Literal           (C, P.Int8_Val               (I));
            when Int16_Kind              =>                     Parse_Int16_Literal          (C, P.Int16_Val              (I));
            when Int32_Kind              =>                     Parse_Int32_Literal          (C, P.Int32_Val              (I));
            when Int64_Kind              =>                     Parse_Int64_Literal          (C, P.Int64_Val              (I));
            when Int8_x2_Kind            => for J in 1..2  loop Parse_Int8_Literal           (C, P.Int8_x2_Val            (I, J)); L (C, J, 2);  end loop;
            when Int16_x2_Kind           => for J in 1..2  loop Parse_Int16_Literal          (C, P.Int16_x2_Val           (I, J)); L (C, J, 2);  end loop;
            when Int32_x2_Kind           => for J in 1..2  loop Parse_Int32_Literal          (C, P.Int32_x2_Val           (I, J)); L (C, J, 2);  end loop;
            when Int64_x2_Kind           => for J in 1..2  loop Parse_Int64_Literal          (C, P.Int64_x2_Val           (I, J)); L (C, J, 2);  end loop;
            when Int8_x3_Kind            => for J in 1..3  loop Parse_Int8_Literal           (C, P.Int8_x3_Val            (I, J)); L (C, J, 3);  end loop;
            when Int16_x3_Kind           => for J in 1..3  loop Parse_Int16_Literal          (C, P.Int16_x3_Val           (I, J)); L (C, J, 3);  end loop;
            when Int32_x3_Kind           => for J in 1..3  loop Parse_Int32_Literal          (C, P.Int32_x3_Val           (I, J)); L (C, J, 3);  end loop;
            when Int64_x3_Kind           => for J in 1..3  loop Parse_Int64_Literal          (C, P.Int64_x3_Val           (I, J)); L (C, J, 3);  end loop;
            when Int8_x4_Kind            => for J in 1..4  loop Parse_Int8_Literal           (C, P.Int8_x4_Val            (I, J)); L (C, J, 4);  end loop;
            when Int16_x4_Kind           => for J in 1..4  loop Parse_Int16_Literal          (C, P.Int16_x4_Val           (I, J)); L (C, J, 4);  end loop;
            when Int32_x4_Kind           => for J in 1..4  loop Parse_Int32_Literal          (C, P.Int32_x4_Val           (I, J)); L (C, J, 4);  end loop;
            when Int64_x4_Kind           => for J in 1..4  loop Parse_Int64_Literal          (C, P.Int64_x4_Val           (I, J)); L (C, J, 4);  end loop;
            when Int8_x8_Kind            => for J in 1..8  loop Parse_Int8_Literal           (C, P.Int8_x8_Val            (I, J)); L (C, J, 8);  end loop;
            when Int16_x8_Kind           => for J in 1..8  loop Parse_Int16_Literal          (C, P.Int16_x8_Val           (I, J)); L (C, J, 8);  end loop;
            when Int32_x8_Kind           => for J in 1..8  loop Parse_Int32_Literal          (C, P.Int32_x8_Val           (I, J)); L (C, J, 8);  end loop;
            when Int64_x8_Kind           => for J in 1..8  loop Parse_Int64_Literal          (C, P.Int64_x8_Val           (I, J)); L (C, J, 8);  end loop;
            when Int8_x16_Kind           => for J in 1..16 loop Parse_Int8_Literal           (C, P.Int8_x16_Val           (I, J)); L (C, J, 16); end loop;
            when Int16_x16_Kind          => for J in 1..16 loop Parse_Int16_Literal          (C, P.Int16_x16_Val          (I, J)); L (C, J, 16); end loop;
            when Int32_x16_Kind          => for J in 1..16 loop Parse_Int32_Literal          (C, P.Int32_x16_Val          (I, J)); L (C, J, 16); end loop;
            when Int64_x16_Kind          => for J in 1..16 loop Parse_Int64_Literal          (C, P.Int64_x16_Val          (I, J)); L (C, J, 16); end loop;
            when Unsigned_Int8_Kind      =>                     Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_Val      (I));
            when Unsigned_Int16_Kind     =>                     Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_Val     (I));
            when Unsigned_Int32_Kind     =>                     Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_Val     (I));
            when Unsigned_Int64_Kind     =>                     Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_Val     (I));
            when Unsigned_Int8_x2_Kind   => for J in 1..2  loop Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_x2_Val   (I, J)); L (C, J, 2);  end loop;
            when Unsigned_Int16_x2_Kind  => for J in 1..2  loop Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_x2_Val  (I, J)); L (C, J, 2);  end loop;
            when Unsigned_Int32_x2_Kind  => for J in 1..2  loop Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_x2_Val  (I, J)); L (C, J, 2);  end loop;
            when Unsigned_Int64_x2_Kind  => for J in 1..2  loop Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_x2_Val  (I, J)); L (C, J, 2);  end loop;
            when Unsigned_Int8_x3_Kind   => for J in 1..3  loop Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_x3_Val   (I, J)); L (C, J, 3);  end loop;
            when Unsigned_Int16_x3_Kind  => for J in 1..3  loop Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_x3_Val  (I, J)); L (C, J, 3);  end loop;
            when Unsigned_Int32_x3_Kind  => for J in 1..3  loop Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_x3_Val  (I, J)); L (C, J, 3);  end loop;
            when Unsigned_Int64_x3_Kind  => for J in 1..3  loop Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_x3_Val  (I, J)); L (C, J, 3);  end loop;
            when Unsigned_Int8_x4_Kind   => for J in 1..4  loop Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_x4_Val   (I, J)); L (C, J, 4);  end loop;
            when Unsigned_Int16_x4_Kind  => for J in 1..4  loop Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_x4_Val  (I, J)); L (C, J, 4);  end loop;
            when Unsigned_Int32_x4_Kind  => for J in 1..4  loop Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_x4_Val  (I, J)); L (C, J, 4);  end loop;
            when Unsigned_Int64_x4_Kind  => for J in 1..4  loop Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_x4_Val  (I, J)); L (C, J, 4);  end loop;
            when Unsigned_Int8_x8_Kind   => for J in 1..8  loop Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_x8_Val   (I, J)); L (C, J, 8);  end loop;
            when Unsigned_Int16_x8_Kind  => for J in 1..8  loop Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_x8_Val  (I, J)); L (C, J, 8);  end loop;
            when Unsigned_Int32_x8_Kind  => for J in 1..8  loop Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_x8_Val  (I, J)); L (C, J, 8);  end loop;
            when Unsigned_Int64_x8_Kind  => for J in 1..8  loop Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_x8_Val  (I, J)); L (C, J, 8);  end loop;
            when Unsigned_Int8_x16_Kind  => for J in 1..16 loop Parse_Unsigned_Int8_Literal  (C, P.Unsigned_Int8_x16_Val  (I, J)); L (C, J, 16); end loop;
            when Unsigned_Int16_x16_Kind => for J in 1..16 loop Parse_Unsigned_Int16_Literal (C, P.Unsigned_Int16_x16_Val (I, J)); L (C, J, 16); end loop;
            when Unsigned_Int32_x16_Kind => for J in 1..16 loop Parse_Unsigned_Int32_Literal (C, P.Unsigned_Int32_x16_Val (I, J)); L (C, J, 16); end loop;
            when Unsigned_Int64_x16_Kind => for J in 1..16 loop Parse_Unsigned_Int64_Literal (C, P.Unsigned_Int64_x16_Val (I, J)); L (C, J, 16); end loop;
            when Structure_Kind          => raise Program_Error;
          end case;
        end;

      procedure Parse_Data_List (C : in out Positive; Structure : in out Ptr_Structure_State; Start_Index, End_Index : Positive) with Inline is
        begin

          -- Handle multi-dimension
          if Is_Dimensional_Primitive (Structure.Kind) then
            for I in Start_Index..End_Index loop -- Structure.Primitive.Len

              -- Multi-dimensionals are wrapped in curlies
              Assert_Next (C, '{');
              Skip_Whitespace (C);
              Parse_Data_List_Item (C, Structure, I);
              Assert_Next (C, '}');
              List (C, I, Structure.Primitive.Len);
            end loop;

          -- Single dimensional
          else
            for I in Start_Index..End_Index loop
              Parse_Data_List_Item (C, Structure, I);
              List (C, I, Structure.Primitive.Len);
            end loop;
          end if;
        end;

      ---------------------
      -- Parse_Structure --
      ---------------------

      procedure Parse_Structure
        (C      : in out Positive;
         Parent :        Ptr_Structure_State) with Inline
      is
        Primitive   : Primitive_Kind;
        Struct_Kind : Struct_T;
        Structure   : Ptr_Structure_State;
        Property    : Ptr_Structure_State;
        Identifier  : Str_8_Super (IDENTIFIER_MAX);
        Len         : Positive := 1;

        -- Workers
        task type Parse_Data_List_Task is
            entry Run (Start_Index, Work_Amount : Positive; Is_Last : Bool := False);
          end;
        task body Parse_Data_List_Task is
          Local_Is_Last     : Bool;
          Local_C           : Positive;
          Local_End_Index   : Positive;
          Local_Start_Index : Positive;
          begin

            -- Set the locals via a rendevous
            accept Run (Start_Index, Work_Amount : Positive; Is_Last : Bool := False)
            do
              Local_C           := C;
              Local_Start_Index := Start_Index;
              Local_Is_Last     := Is_Last;
              Local_End_Index   := Start_Index - 1 + Work_Amount;
              if Is_Last then Local_End_Index := Local_End_Index + (Len rem WORKER_COUNT); end if;
            end;

            if Local_Is_Last then
              Parse_Data_List (C, Structure, Local_Start_Index, Local_End_Index);
            else
              Parse_Data_List (Local_C, Structure, Local_Start_Index, Local_End_Index);
            end if;
          end;

        -- Start of Parse_Structure
        begin
          Parse_Data_Type_Literal (C, Primitive, Struct_Kind);

          -- Probe for primitive count hinting
          Skip_Whitespace (C, Skip_Single_Line_Comments => False);
          if Primitive /= Structure_Kind then
            if Text (C..C + 1) = "//" then
              C := C + 2;

              -- Skip to the next non-whitespace character on the current line
              while CATAGORY (Text (C)) = Whitespace_Catagory and Text (C) /= ASCII.LF loop C := C + 1; end loop;
              declare Old_C : Positive := C; begin Parse_Int32_Literal (C, Len); exception when others => C := Old_C; end;

              -- Skip to the end of the line and then the next valid character
              while Text (C) /= ASCII.LF loop C := C + 1; end loop;
              C := C + 1;
              Skip_Whitespace (C);

            -- No hinting... ugh - count them manually
            else
              declare Temp_C : Positive := C; begin
                if Is_Dimensional_Primitive (Primitive) then
                  loop
                    Skip_Until (Temp_C, '}');
                    Temp_C := Temp_C + 1;
                    Skip_Whitespace (Temp_C);
                    exit when Text (Temp_C) = '}';
                    Len := Len + 1;
                  end loop;
                else
                  while Skip_Until (Temp_C, '}', ',') /= '}' loop
                    Temp_C := Temp_C + 1;
                    Len    := Len    + 1;
                  end loop;
                end if;
              end;
            end if;
          end if;

          -- Create the structure node
          Construct_Structure (Structure, Primitive, Len);

          -- Optional name
          if Text (C) not in '(' | '{' | '[' then
            Parse_Name (C, Identifier, Structure.Is_Global);
            Structure.Name := new Str_8'(To_Str_8 (Identifier));
            Skip_Whitespace (C);
            Identifier := To_Str_8_Super (NULL_STR_8, IDENTIFIER_MAX);
          end if;

          -- Non-primitive type
          if Primitive = Structure_Kind then

            -- Setup the local map and link the parent
            if Parent = null then
              if Structure.Name /= null and then not Structure.Is_Global then raise Local_Name_In_Global_Scope with Structure.Name.all; end if;
              File.Counts (Struct_Kind) := File.Counts (Struct_Kind) + 1;
            else
              Parent.Counts (Struct_Kind) := Parent.Counts (Struct_Kind) + 1;
              Structure.Parent := Parent;
            end if;
            Structure.Counts := (others => 0);
            Structure.Class  := Struct_Kind;

            -- Properties
            if Text (C) = '(' then
              C := C + 1;
              Skip_Whitespace (C);
              if Text (C) = ')' then C := C + 1;
              else
                if Structure.Properties = null then Structure.Properties := new Hashed_Ptr_Structure_State.Unsafe.Map; end if;
                loop

                  -- Property identifier
                  Parse_Identifier (C, Identifier);
                  Skip_Whitespace (C);

                  -- Assignment for property
                  Assert_Next (C, '=');
                  Skip_Whitespace (C);

                  -- Grab the property value according to the rule-set determining its type via linear search
                  for Struct_Property of Properties (Struct_Kind) loop
                    if Struct_Property.Identifier = Identifier then

                      -- Read the property
                      Construct_Structure (Property, Struct_Property.Kind, 1);
                      Parse_Data_List_Item (C, Property);
                      Property.Parent := Structure;
                      Property.Name   := new Str_8'(To_Str_8 (Identifier));
                      Structure.Properties.Insert (To_Str_Unbound (Identifier), Property);
goto Found_Property;
                    end if;
                  end loop;
                  raise Undefined_Property with To_Str_8 (Identifier);
<<Found_Property>>
                  Skip_Whitespace (C);
                  exit when Text (C) = ')';
                  Assert_Next (C, ',');
                end loop;
                C := C + 1;
                Skip_Whitespace (C);
              end if;
            end if;

            -- Add defaulted properties
            for Struct_Property of Properties (Struct_Kind) loop
              if Struct_Property.Identifier /= NULL_IDENTIFIER
                and then Struct_Property.Defaulted
                and then (Structure.Properties = null
                           or else not Structure.Properties.Contains (To_Str_Unbound (Struct_Property.Identifier)))
              then
                if Structure.Properties = null then Structure.Properties := new Hashed_Ptr_Structure_State.Unsafe.Map; end if;
                Construct_Structure (Property, Struct_Property.Default.Kind, Struct_Property.Default.Len);
                Property.Parent    := Structure;
                Property.Primitive := Struct_Property.Default;
                Property.Name      := new Str_8'(To_Str_8 (Struct_Property.Identifier));
                Structure.Properties.Insert (To_Str_Unbound (Struct_Property.Identifier), Property);
              end if;
            end loop;

            -- Parse components
            Assert_Next (C, '{');
            loop
              Skip_Whitespace (C);
              exit when Text (C) = '}';
              Parse_Structure (C, Structure);
            end loop;

            -- Verify component counts
            for I in Struct_T'Range loop
              case Structure_Components (Struct_Kind) (I) is
                when Zero_Or_One   => if Structure.Counts (I) > 1         then raise Component_Mismatch with Struct_Kind'Image & " must have 0 or 1 "       & I'Image; end if;
                when Zero_To_Two   => if Structure.Counts (I) > 2         then raise Component_Mismatch with Struct_Kind'Image & " must have 0 to 2 "       & I'Image; end if;
                when Zero_To_Three => if Structure.Counts (I) > 3         then raise Component_Mismatch with Struct_Kind'Image & " must have 0 to 3 "       & I'Image; end if;
                when Zero_To_Four  => if Structure.Counts (I) > 4         then raise Component_Mismatch with Struct_Kind'Image & " must have 0 to 4 "       & I'Image; end if;
                when Exactly_One   => if Structure.Counts (I) /= 1        then raise Component_Mismatch with Struct_Kind'Image & " must have exactly one "  & I'Image; end if;
                when One_Or_Two    => if Structure.Counts (I) not in 1..2 then raise Component_Mismatch with Struct_Kind'Image & " must have 1 or 2 "       & I'Image; end if;
                when One_To_Three  => if Structure.Counts (I) not in 1..3 then raise Component_Mismatch with Struct_Kind'Image & " must have 1 to 3 "       & I'Image; end if;
                when One_To_Four   => if Structure.Counts (I) not in 1..4 then raise Component_Mismatch with Struct_Kind'Image & " must have 1 to 4 "       & I'Image; end if;
                when One_Or_More   => if Structure.Counts (I) = 0         then raise Component_Mismatch with Struct_Kind'Image & " must have at least one " & I'Image; end if;
                when Max_One       => if Structure.Counts (I) > 1         then raise Component_Mismatch with Struct_Kind'Image & " can have max one "       & I'Image; end if;
                when None          => if Structure.Counts (I) /= 0        then raise Component_Mismatch with Struct_Kind'Image & " can't have a "           & I'Image; end if;
                when Zero_Or_More  => null;
              end case;
            end loop;

          -- Parse primitive type
          else

            -- Only top-level structures get their own local maps, otherwise link the parent
            if Parent /= null then
              Structure.Parent := Parent;
            end if;

            -- Verify primitive is not allowed to be top-level
            if Parent = null then raise Primitive_At_Top_Level with Primitive'Image; end if;

            -- Verify primitive is allowed to be a child of the current parent
            if not Primitive_Components (Parent.Class).Allowed_Primitives (Primitive) then
              raise Bad_Hierarchy with Primitive'Image & " cannot be a component of a " & Parent.Class'Image;
            end if;

            -- Verify primitive is allowed to have a length more than one
            if not Primitive_Components (Parent.Class).Allow_Arrays and Len > 1 then
              raise Unexpected_Primitive_Array with Primitive'Image & " cannot be an array component in a " & Parent.Class'Image;
            end if;

            -- Parse the raw data-list, if we are over our threshold we deploy all the CPUs to speed things up
            Assert_Next (C, '{');
            Skip_Whitespace (C);
            if Len < WORKERS_THRESHOLD then
              Parse_Data_List (C , Structure, 1, Structure.Len); -- Single threaded parsing
            else

              -- Divide up the work and send them on their way
              declare
              Workers     : array (1..WORKER_COUNT) of Parse_Data_List_Task;
              WORK_AMOUNT : constant Positive := Len / Workers'Length;
              begin
                for I in Workers'Range loop
                  if I /= 1 then

                    -- Parse multi-dimensinal
                    if Is_Dimensional_Primitive (Structure.Kind) then

                      -- Jump over the curlies
                      for J in 1..WORK_AMOUNT loop
                        Skip_Until (C, '}');
                        C := C + 1;
                      end loop;

                      -- Now the comma
                      Assert_Next (C, ',');
                      C := C + 1;
                      Skip_Whitespace (C);

                    -- Parse single-dimensional
                    else

                      -- Jump over the commas
                      for J in 1..WORK_AMOUNT loop
                        Skip_Until (C, ',');
                        C := C + 1;
                      end loop;
                      Skip_Whitespace (C);
                    end if;
                  end if;

                  -- Dispatch the work load
                  Workers (I).Run (Start_Index => (I - 1) * WORK_AMOUNT + 1,
                                   Work_Amount => WORK_AMOUNT,
                                   Is_Last     => I = Workers'Last);
                end loop;
              end;
            end if;
          end if;

          -- Add to the global top-level list as indicated by the null parent
          if Parent = null then

            -- Verify loaded structure is not allowed to be top-level
            if Primitive /= Structure_Kind or not Top_Level_Structures (Struct_Kind) then
              raise Disallowed_Structure_At_Top_Level with Struct_Kind'Image;
            end if;

            -- An identifier signifies
            if Structure.Name /= null then
              if not Structure.Is_Global then raise Local_Name_In_Global_Scope; end if;
              File.Globals.Insert          (To_Str_Unbound (Structure.Name.all), Structure);
              File.Named_Structures.Insert (To_Str_Unbound (Structure.Name.all), Structure);
            else File.Structures.Append (Structure); end if;

          -- Add to a substructure
          else

            -- Verify loaded structure is allowed to be a child of the current parent
            if Primitive = Structure_Kind and then not Hierarchy (Struct_Kind) (Parent.Class) then
              raise Bad_Hierarchy with Struct_Kind'Image & " cannot be a component of a " & Parent.Class'Image;
            end if;

            -- Unnamed structures get added to the children of the parent
            if Structure.Name = null then
              if Parent.Children = null then Parent.Children := new Vector_Ptr_Structure_State.Unsafe.Vector; end if;
              Parent.Children.Append (Structure);

            -- Named structures get added to the parent's "Local" table
            else
              if Parent.Named_Children = null then Parent.Named_Children := new Hashed_Ptr_Structure_State.Unsafe.Map; end if;
              Parent.Named_Children.Insert (To_Str_Unbound (Structure.Name.all), Structure);

              -- Also register it in the global list if the name is global
              if Structure.Is_Global then File.Globals.Insert (To_Str_Unbound (Structure.Name.all), Structure); end if;
            end if;
          end if;

          -- Skip over the last brace
          Assert_Next (C, '}');
        end;

      ----------
      -- Load --
      ----------

      -- Locals for property reconciliation
      Override_Property, New_Property, Sibling_Parent : Ptr_Structure_State;

      -- Start of Load
      begin

        -- Do IO
        File.Name := new Str_16'(Path);
        File_String_IO.Open (Data_File, In_File, To_Str_8 (Path));
        Read (Data_File, Text);
        Close (Data_File);

        -- Parse the data
        File.Counts := (others => 0);
        while Text_Index < Text'Length loop
          Skip_Whitespace (Text_Index);
          Parse_Structure (Text_Index, null);
        end loop;

        -- Fixup references
        for Reference of References loop

          -- Find the first element in the reference chain - its easy if its global
          if Reference.Starts_With_Global then Reference.Data.all := To_Ref (File.Globals.Element (Reference.Names.Element (1)));

          -- We have a "local" reference the mean of which is vague in the docs and it could come from any parent really - not just the immediate
          else
            Sibling_Parent := Reference.Structure;
            loop

              -- Either we've reached the top without a hit or we can keep going
              if Sibling_Parent = null then raise Bad_Reference; end if;
              Sibling_Parent := Sibling_Parent.Parent;

              -- This is the reference we are looking for
              if Sibling_Parent.Named_Children /= null and then Sibling_Parent.Named_Children.Contains (Reference.Names.Element (1)) then
                Reference.Data.all := To_Ref (Sibling_Parent.Named_Children.Element (Reference.Names.Element (1)));
                exit;
              end if;
            end loop;
          end if;

          -- Go down the chain
          if Reference.Names.Length > 1 then
            for I in 2..Int (Reference.Names.Length) loop
              Reference.Data.all := To_Ref (Ref (Reference.Data.all).Named_Children.Element (Reference.Names.Element (I)));
            end loop;
          end if;

          -- Check for an override reference structure and add property overrides
          if Reference.Structure.Parent.Class = Struct_Types.Override_Ref and then Reference.Structure.Parent.Parent /= null then

            -- Go through the structure type's property list
            for Property of Properties (Reference.Structure.Parent.Parent.Class) loop

              -- We found an override-from-reference property and the referenced object has it!
              if Property.Identifier /= NULL_IDENTIFIER
                and then not Property.Defaulted
                and then Property.Overrides_From_Ref
                and then Ref (Reference.Structure.Primitive.Ref_Val (1)).Properties.Contains (To_Str_Unbound (Property.Identifier))
              then
                Override_Property := Ref (Reference.Structure.Primitive.Ref_Val (1)).Properties.Element (To_Str_Unbound (Property.Identifier));

                -- This override property needs to exist so create the property map if it doesn't exist
                if Reference.Structure.Parent.Parent.Properties = null then
                  Reference.Structure.Parent.Parent.Properties := new Hashed_Ptr_Structure_State.Unsafe.Map;
                end if;

                -- If we overrode it already we are fine, if not it inherits from the reference
                if not Reference.Structure.Parent.Parent.Properties.Contains (To_Str_Unbound (Property.Identifier)) then
                  Construct_Structure (New_Property, Property.Kind, 1);
                  New_Property.Parent    := Override_Property.Parent;
                  New_Property.Primitive := Override_Property.Primitive;
                  New_Property.Name      := new Str_8'(Override_Property.Name.all);
                  Reference.Structure.Parent.Parent.Properties.Insert (To_Str_Unbound (Property.Identifier), New_Property);
                end if;
              end if;
            end loop;
          end if;
        end loop;
      end;

    ----------
    -- Dump --
    ----------

    -- Crap for debugging - could be deleted
    procedure Dump (Path : Str; File : File_State) is

      -- IO
      Data : Ada_IO.File_Type;
      package FIO is new Ada.Wide_Text_IO.Float_IO (Real_32);

      -- Recursive dumping
      procedure Put_Structure (Item : Ptr_Structure_State; Level : Positive := 1) is

        -- Comma stuff
        procedure L (J, Last : Integer) is
          begin
            if J /= Last then
              Ada_IO.Put (Data, ", ");
            end if;
          end;

        -- Start of Put_Structure
        begin

          -- Class or kind
          if Item.Kind = Structure_Kind then
            Ada_IO.Put (Data, Item.Class'Wide_Image);
          else
            Ada_IO.Put (Data, Item.Kind'Wide_Image);
          end if;

          -- Length
          if Item.Len > 1 then
            Ada_IO.Put (Data, "[" & Item.Len'Wide_Image & "] ");
          else
            Ada_IO.Put (Data, " ");
          end if;

          -- Name
          if Item.Name /= null then
            Ada_IO.Put (Data, To_Str (Item.Name.all) & " ");
          end if;

          Ada_IO.New_Line (Data);
          Ada_IO.Put_Line (Data, "{");

          -- Recurse into substructures
          if Item.Kind = Structure_Kind then

            -- Properties
            if Item.Properties /= null then
              Ada_IO.Put (Data, "(");
              for I of Item.Properties.all loop
                Put_Structure (I);
              end loop;
              Ada_IO.Put (Data, ")");
            end if;

            Ada_IO.New_Line (Data);
            Ada_IO.Put_Line (Data, "{");

            -- Locals
            if Item.Children /= null then
              for I of Item.Children.all loop Put_Structure (I); end loop;
            end if;

            -- Children
            if Item.Named_Children /= null then
              for I of Item.Named_Children.all loop Put_Structure (I); end loop;
            end if;
          else
            Ada_IO.New_Line (Data);
            Ada_IO.Put_Line (Data, "{");
            for I in 1..Item.Len loop

              -- Multi-dimensionals have extra curlies
              if Is_Dimensional_Primitive (Item.Kind) then
                Ada_IO.Put (Data, "{");
              end if;

              -- Dump the values
              case Item.Kind is
                when Bool_Kind               =>                     Ada_IO.Put (Data, Item.Primitive.Bool_Val               (I)'Wide_Image);
                when Bool_x2_Kind            => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Bool_x2_Val            (I, J)'Wide_Image); L (J, 2);  end loop;
                when Bool_x3_Kind            => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Bool_x3_Val            (I, J)'Wide_Image); L (J, 3);  end loop;
                when Bool_x4_Kind            => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Bool_x4_Val            (I, J)'Wide_Image); L (J, 4);  end loop;
                when Bool_x8_Kind            => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Bool_x8_Val            (I, J)'Wide_Image); L (J, 8);  end loop;
                when Bool_x16_Kind           => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Bool_x16_Val           (I, J)'Wide_Image); L (J, 16); end loop;
--                  when String_Kind             =>                     Ada_IO.Put (Data, Item.Primitive.String_Val             (I)'Wide_Image);
--                  when String_x2_Kind          => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.String_x2_Val          (I, J)'Wide_Image); L (J, 2);  end loop;
--                  when String_x3_Kind          => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.String_x3_Val          (I, J)'Wide_Image); L (J, 3);  end loop;
--                  when String_x4_Kind          => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.String_x4_Val          (I, J)'Wide_Image); L (J, 4);  end loop;
--                  when String_x8_Kind          => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.String_x8_Val          (I, J)'Wide_Image); L (J, 8);  end loop;
--                  when String_x16_Kind         => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.String_x16_Val         (I, J)'Wide_Image); L (J, 16); end loop;
--                  when Ref_Kind                =>                     Ada_IO.Put (Data, Item.Primitive.Ref_Val                (I)'Wide_Image'Unchecked_Access,    Structure);
--                  when Ref_x2_Kind             => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Ref_x2_Val             (I, J)'Wide_Image'Unchecked_Access, Structure); L (J, 2);  end loop;
--                  when Ref_x3_Kind             => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Ref_x3_Val             (I, J)'Wide_Image'Unchecked_Access, Structure); L (J, 3);  end loop;
--                  when Ref_x4_Kind             => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Ref_x4_Val             (I, J)'Wide_Image'Unchecked_Access, Structure); L (J, 4);  end loop;
--                  when Ref_x8_Kind             => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Ref_x8_Val             (I, J)'Wide_Image'Unchecked_Access, Structure); L (J, 8);  end loop;
--                  when Ref_x16_Kind            => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Ref_x16_Val            (I, J)'Wide_Image'Unchecked_Access, Structure); L (J, 16); end loop;
--                  when Type_Kind               =>                     Ada_IO.Put (Data, Item.Primitive.Type_Val               (I)'Wide_Image,    Junk);
--                  when Type_x2_Kind            => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Type_x2_Val            (I, J)'Wide_Image, Junk); L (J, 2);  end loop;
--                  when Type_x3_Kind            => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Type_x3_Val            (I, J)'Wide_Image, Junk); L (J, 3);  end loop;
--                  when Type_x4_Kind            => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Type_x4_Val            (I, J)'Wide_Image, Junk); L (J, 4);  end loop;
--                  when Type_x8_Kind            => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Type_x8_Val            (I, J)'Wide_Image, Junk); L (J, 8);  end loop;
--                  when Type_x16_Kind           => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Type_x16_Val           (I, J)'Wide_Image, Junk); L (J, 16); end loop;
                when Float16_Kind            =>                     Ada_IO.Put (Data, Item.Primitive.Float16_Val            (I)'Wide_Image);
                when Float32_Kind            =>                     FIO.Put (Data, Item.Primitive.Float32_Val            (I), 9, 16, 9);
                when Float64_Kind            =>                     Ada_IO.Put (Data, Item.Primitive.Float64_Val            (I)'Wide_Image);
                when Float16_x2_Kind         => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Float16_x2_Val         (I, J)'Wide_Image); L (J, 2);  end loop;
                when Float32_x2_Kind         => for J in 1..2  loop FIO.Put (Data, Item.Primitive.Float32_x2_Val         (I, J), 9, 16, 9); L (J, 2);  end loop;
                when Float64_x2_Kind         => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Float64_x2_Val         (I, J)'Wide_Image); L (J, 2);  end loop;
                when Float16_x3_Kind         => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Float16_x3_Val         (I, J)'Wide_Image); L (J, 3);  end loop;
                when Float32_x3_Kind         => for J in 1..3  loop FIO.Put (Data, Item.Primitive.Float32_x3_Val         (I, J), 9, 16, 9); L (J, 3);  end loop;
                when Float64_x3_Kind         => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Float64_x3_Val         (I, J)'Wide_Image); L (J, 3);  end loop;
                when Float16_x4_Kind         => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Float16_x4_Val         (I, J)'Wide_Image); L (J, 4);  end loop;
                when Float32_x4_Kind         => for J in 1..4  loop FIO.Put (Data, Item.Primitive.Float32_x4_Val         (I, J), 9, 16, 9); L (J, 4);  end loop;
                when Float64_x4_Kind         => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Float64_x4_Val         (I, J)'Wide_Image); L (J, 4);  end loop;
                when Float16_x8_Kind         => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Float16_x8_Val         (I, J)'Wide_Image); L (J, 8);  end loop;
                when Float32_x8_Kind         => for J in 1..8  loop FIO.Put (Data, Item.Primitive.Float32_x8_Val         (I, J), 9, 16, 9); L (J, 8);  end loop;
                when Float64_x8_Kind         => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Float64_x8_Val         (I, J)'Wide_Image); L (J, 8);  end loop;
                when Float16_x16_Kind        => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Float16_x16_Val        (I, J)'Wide_Image); L (J, 16); end loop;
                when Float32_x16_Kind        => for J in 1..16 loop FIO.Put (Data, Item.Primitive.Float32_x16_Val        (I, J), 9, 16, 9); L (J, 16); end loop;
                when Float64_x16_Kind        => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Float64_x16_Val        (I, J)'Wide_Image); L (J, 16); end loop;
                when Int8_Kind               =>                     Ada_IO.Put (Data, Item.Primitive.Int8_Val               (I)'Wide_Image);
                when Int16_Kind              =>                     Ada_IO.Put (Data, Item.Primitive.Int16_Val              (I)'Wide_Image);
                when Int32_Kind              =>                     Ada_IO.Put (Data, Item.Primitive.Int32_Val              (I)'Wide_Image);
                when Int64_Kind              =>                     Ada_IO.Put (Data, Item.Primitive.Int64_Val              (I)'Wide_Image);
                when Int8_x2_Kind            => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Int8_x2_Val            (I, J)'Wide_Image); L (J, 2);  end loop;
                when Int16_x2_Kind           => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Int16_x2_Val           (I, J)'Wide_Image); L (J, 2);  end loop;
                when Int32_x2_Kind           => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Int32_x2_Val           (I, J)'Wide_Image); L (J, 2);  end loop;
                when Int64_x2_Kind           => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Int64_x2_Val           (I, J)'Wide_Image); L (J, 2);  end loop;
                when Int8_x3_Kind            => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Int8_x3_Val            (I, J)'Wide_Image); L (J, 3);  end loop;
                when Int16_x3_Kind           => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Int16_x3_Val           (I, J)'Wide_Image); L (J, 3);  end loop;
                when Int32_x3_Kind           => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Int32_x3_Val           (I, J)'Wide_Image); L (J, 3);  end loop;
                when Int64_x3_Kind           => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Int64_x3_Val           (I, J)'Wide_Image); L (J, 3);  end loop;
                when Int8_x4_Kind            => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Int8_x4_Val            (I, J)'Wide_Image); L (J, 4);  end loop;
                when Int16_x4_Kind           => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Int16_x4_Val           (I, J)'Wide_Image); L (J, 4);  end loop;
                when Int32_x4_Kind           => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Int32_x4_Val           (I, J)'Wide_Image); L (J, 4);  end loop;
                when Int64_x4_Kind           => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Int64_x4_Val           (I, J)'Wide_Image); L (J, 4);  end loop;
                when Int8_x8_Kind            => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Int8_x8_Val            (I, J)'Wide_Image); L (J, 8);  end loop;
                when Int16_x8_Kind           => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Int16_x8_Val           (I, J)'Wide_Image); L (J, 8);  end loop;
                when Int32_x8_Kind           => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Int32_x8_Val           (I, J)'Wide_Image); L (J, 8);  end loop;
                when Int64_x8_Kind           => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Int64_x8_Val           (I, J)'Wide_Image); L (J, 8);  end loop;
                when Int8_x16_Kind           => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Int8_x16_Val           (I, J)'Wide_Image); L (J, 16); end loop;
                when Int16_x16_Kind          => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Int16_x16_Val          (I, J)'Wide_Image); L (J, 16); end loop;
                when Int32_x16_Kind          => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Int32_x16_Val          (I, J)'Wide_Image); L (J, 16); end loop;
                when Int64_x16_Kind          => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Int64_x16_Val          (I, J)'Wide_Image); L (J, 16); end loop;
                when Unsigned_Int8_Kind      =>                     Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_Val      (I)'Wide_Image);
                when Unsigned_Int16_Kind     =>                     Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_Val     (I)'Wide_Image);
                when Unsigned_Int32_Kind     =>                     Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_Val     (I)'Wide_Image);
                when Unsigned_Int64_Kind     =>                     Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_Val     (I)'Wide_Image);
                when Unsigned_Int8_x2_Kind   => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_x2_Val   (I, J)'Wide_Image); L (J, 2);  end loop;
                when Unsigned_Int16_x2_Kind  => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_x2_Val  (I, J)'Wide_Image); L (J, 2);  end loop;
                when Unsigned_Int32_x2_Kind  => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_x2_Val  (I, J)'Wide_Image); L (J, 2);  end loop;
                when Unsigned_Int64_x2_Kind  => for J in 1..2  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_x2_Val  (I, J)'Wide_Image); L (J, 2);  end loop;
                when Unsigned_Int8_x3_Kind   => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_x3_Val   (I, J)'Wide_Image); L (J, 3);  end loop;
                when Unsigned_Int16_x3_Kind  => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_x3_Val  (I, J)'Wide_Image); L (J, 3);  end loop;
                when Unsigned_Int32_x3_Kind  => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_x3_Val  (I, J)'Wide_Image); L (J, 3);  end loop;
                when Unsigned_Int64_x3_Kind  => for J in 1..3  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_x3_Val  (I, J)'Wide_Image); L (J, 3);  end loop;
                when Unsigned_Int8_x4_Kind   => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_x4_Val   (I, J)'Wide_Image); L (J, 4);  end loop;
                when Unsigned_Int16_x4_Kind  => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_x4_Val  (I, J)'Wide_Image); L (J, 4);  end loop;
                when Unsigned_Int32_x4_Kind  => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_x4_Val  (I, J)'Wide_Image); L (J, 4);  end loop;
                when Unsigned_Int64_x4_Kind  => for J in 1..4  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_x4_Val  (I, J)'Wide_Image); L (J, 4);  end loop;
                when Unsigned_Int8_x8_Kind   => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_x8_Val   (I, J)'Wide_Image); L (J, 8);  end loop;
                when Unsigned_Int16_x8_Kind  => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_x8_Val  (I, J)'Wide_Image); L (J, 8);  end loop;
                when Unsigned_Int32_x8_Kind  => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_x8_Val  (I, J)'Wide_Image); L (J, 8);  end loop;
                when Unsigned_Int64_x8_Kind  => for J in 1..8  loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_x8_Val  (I, J)'Wide_Image); L (J, 8);  end loop;
                when Unsigned_Int8_x16_Kind  => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int8_x16_Val  (I, J)'Wide_Image); L (J, 16); end loop;
                when Unsigned_Int16_x16_Kind => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int16_x16_Val (I, J)'Wide_Image); L (J, 16); end loop;
                when Unsigned_Int32_x16_Kind => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int32_x16_Val (I, J)'Wide_Image); L (J, 16); end loop;
                when Unsigned_Int64_x16_Kind => for J in 1..16 loop Ada_IO.Put (Data, Item.Primitive.Unsigned_Int64_x16_Val (I, J)'Wide_Image); L (J, 16); end loop;
                when others          => null;
              end case;
              if Is_Dimensional_Primitive (Item.Kind) then
                Ada_IO.Put_Line (Data, "}");
              end if;
              L (I, Item.Len);
            end loop;
          end if;
          Ada_IO.Put_Line (Data, "}");
        end;

      -- Start of Dump
      begin
        Ada_IO.Create (Data, Ada_IO.Out_File, To_Str_8 (Path));
        for I of File.Structures       loop Put_Structure (I); end loop;
        for I of File.Named_Structures loop Put_Structure (I); end loop;
        Ada_IO.Close (Data);
      end;
  end;
end;






















