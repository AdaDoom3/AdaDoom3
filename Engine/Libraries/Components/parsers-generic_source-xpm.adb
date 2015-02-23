--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.XPM                  Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  19:53 12 Jan 2008  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

package body Parsers.Generic_Source.XPM is
   use Color_Tables;

   procedure Skip
             (  Code      : in out Source_Type;
                In_String : Boolean
             )  is
      type Chain_Type is (Slash, Catenation);
      Chain    : Chain_Type;
      Error_At : Location_Type;
   begin
      if not In_String then
         declare
            Got_It : Boolean;
            Error  : Boolean;
         begin
            Skip (Code, Got_It, Error, Error_At);
            if Error then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Unclosed comment starts at "
                  &  Image (Error_At & Link (Code))
               )  );
            end if;
            return;
         end;
      end if;
      loop
         declare
            Line    : Line_Ptr_Type;
            Pointer : Integer;
            Last    : Integer;
         begin
            Get_Line (Code, Line, Pointer, Last);
            loop
               if Pointer > Last then
                  Set_Pointer (Code, Pointer);
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Malformed string literal at "
                     &  Image (Link (Code))
                  )  );
               end if;
               case Line (Pointer) is
                  when ' ' | HT | LF | CR | VT | FF =>
                     Pointer := Pointer + 1;
                  when '\' =>
                     Pointer := Pointer + 1;
                     if Pointer > Last then
                        Chain := Slash;
                        exit;
                     end if;
                     if (  Line (Pointer) = CR
                        or else
                           Line (Pointer) = LF
                        )
                     then
                        Pointer := Pointer + 1;
                     else
                        Set_Pointer (Code, Pointer - 1);
                        return;
                     end if;
                  when '"' =>
                     Chain := Catenation;
                     Set_Pointer (Code, Pointer + 1);
                     exit;
                  when others =>
                     Set_Pointer (Code, Pointer);
                     return;
               end case;
            end loop;
         end;
         case Chain is
            when Catenation =>
               Skip (Code, False);
               declare
                  Got_It : Boolean;
               begin
                  Get (Code, """", Got_It);
                  if not Got_It then
                     Raise_Exception
                     (  Use_Error'Identity,
                        (  "String literal continuation is expected at "
                        &  Image (Link (Code))
                     )  );
                  end if;
               end;
            when Slash =>
               begin
                  Next_Line (Code);
               exception
                  when End_Error =>
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Unclosed string literal at "
                        &  Image (Link (Code))
                     )  );
               end;
         end case;
      end loop;
   end Skip;

   procedure Match
             (  Code      : in out Source_Type;
                Text      : String;
                In_String : Boolean
             )  is
      Got_It : Boolean;
   begin
      Skip (Code, In_String);
      Get (Code, Text, Got_It);
      if not Got_It then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "'"
            &  Text
            &  "' is expected at "
            &  Image (Link (Code))
         )  );
      end if;
   end Match;

   procedure Match_End (Code : in out Source_Type) is
   begin
      Skip (Code, True);
      Raise_Exception
      (  Syntax_Error'Identity,
          """ is expected at " & Image (Link (Code))
      );
   exception
      when Use_Error =>
         return;
   end Match_End;

   procedure Get_Decimal
             (  Code   : in out Source_Type;
                Number : out Integer;
                Name   : String;
                Got_It : out Boolean;
                First  : Integer := 1;
                Last   : Integer := Integer'Last
             )  is
   begin
      Skip (Code, True);
      declare
         Line         : Line_Ptr_Type;
         Pointer      : Integer;
         Last_Pointer : Integer;
      begin
         Get_Line (Code, Line, Pointer, Last_Pointer);
         Set_Pointer (Code, Pointer);
         Get
         (  Line (Line'First..Last_Pointer),
            Pointer,
            Number,
            First => First,
            Last  => Last
         );
         Set_Pointer (Code, Pointer);
         Got_It := True;
      exception
         when End_Error =>
            Got_It := False;
         when Data_Error =>
            Set_Pointer (Code, Pointer);
            Raise_Exception
            (  Syntax_Error'Identity,
               (  Name
               &  " is expected at "
               &  Image (Link (Code))
            )  );
         when Constraint_Error =>
            Set_Pointer (Code, Pointer);
            Raise_Exception
            (  Syntax_Error'Identity,
               (  Name
               &  " as a positive number "
               &  Image (First)
               &  ".."
               &  Image (Last)
               &  " is expected "
               &  Image (Link (Code))
            )  );
      end;
   end Get_Decimal;

   procedure Match_Decimal
             (  Code   : in out Source_Type;
                Number : out Integer;
                Name   : String;
                First  : Integer := 1;
                Last   : Integer := Integer'Last
             )  is
      Got_It : Boolean;
   begin
      Get_Decimal (Code, Number, Name, Got_It, First, Last);
      if not Got_It then
         Raise_Exception
         (  Syntax_Error'Identity,
            Name & " is expected at " & Image (Link (Code))
         );
      end if;
   exception
      when Error : Use_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  Exception_Message (Error)
            &  " to contain "
            &  Name
         )  );
   end Match_Decimal;

   function Get_Header (Code : access Source_Type; Name : String)
      return Descriptor is
      This       : Source_Type renames Code.all;
      Width      : Positive;
      Height     : Positive;
      Pixel_Size : Positive;
      Map_Size   : Positive;
      X_Hotspot  : Natural;
      Y_Hotspot  : Natural;
      Extended   : Boolean;
      Hotspot    : Boolean;
   begin
      Skip (This, False);
      Match (This, "[",  False);
      Match (This, "]",  False);
      Match (This, "=",  False);
      Match (This, "{",  False);
      Match (This, """", False);
      Match_Decimal (This, Width,      "Width");
      Match_Decimal (This, Height,     "Height");
      Match_Decimal (This, Map_Size,   "Colors number");
      Match_Decimal (This, Pixel_Size, "Characters per pixel");
      begin
         Get_Decimal
         (  This,
            X_Hotspot,
            "x-hot spot",
            Hotspot,
            First => 0,
            Last  => Width - 1
         );
         if Hotspot then
            Match_Decimal
            (  This,
               Y_Hotspot,
               "y-hot spot",
               First => 0,
               Last  => Height - 1
            );
            Get (This, "XPMEXT", Extended);
            Match_End (This);
            return
            (  Has_Hotspot => True,
               Length      => Name'Length,
               Name        => Name,
               Width       => Width,
               Height      => Height,
               Pixel_Size  => Pixel_Size,
               Map_Size    => Map_Size,
               Extended    => Extended,
               X_Hotspot   => X_Hotspot,
               Y_Hotspot   => Y_Hotspot
            );
         else
            Get (This, "XPMEXT", Extended);
            Match_End (This);
            return
            (  Has_Hotspot => False,
               Length      => Name'Length,
               Name        => Name,
               Width       => Width,
               Height      => Height,
               Pixel_Size  => Pixel_Size,
               Map_Size    => Map_Size,
               Extended    => Extended
            );
         end if;
      exception
         when Use_Error =>
            return
            (  Has_Hotspot => False,
               Length      => Name'Length,
               Name        => Name,
               Width       => Width,
               Height      => Height,
               Pixel_Size  => Pixel_Size,
               Map_Size    => Map_Size,
               Extended    => False
            );
      end;
   end Get_Header;

   function Get (Code : access Source_Type) return Descriptor is
      This    : Source_Type renames Code.all;
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      Start   : Integer;
   begin
      Match (This, "static", False);
      Match (This, "char",   False);
      Match (This, "*",      False);
      Skip (This, False);
      Get_Line (This, Line, Pointer, Last);
      Start := Pointer;
      while (  Pointer < Last
            and then
               (  Is_Alphanumeric (Line (Pointer))
               or else
                  '_' = Line (Pointer)
            )  )  loop
         Pointer := Pointer + 1;
      end loop;
      if Pointer = Start then
         Set_Pointer (This, Pointer);
         Raise_Exception
         (  Syntax_Error'Identity,
            "Identifier is expected at " & Image (Link (This))
         );
      end if;
      declare
         Name : String := Line (Start..Pointer - 1);
      begin
         Set_Pointer (This, Pointer);
         return Get_Header (Code, Name);
      end;
   end Get;

   procedure Get_Color
             (  Code    : in out Source_Type;
                Map     : in out Table;
                Name    : String;
                Name_At : Location_Type
             )  is
      Mode     : Color_Type;
      Symbolic : Color_Name;
      RGB      : RGB_Color;
      Got_It   : Boolean;
   begin
      Skip (Code, True);
      Color_Types.Get (Code, Mode, Got_it);
      if not Got_It then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Color type is expected at " & Image (Link (Code))
         );
      end if;
      Skip (Code, True);
      Color_Names.Get (Code, Symbolic, Got_It);
      if Got_It then
         case Symbolic is
            when None  => RGB := RGB_Color'Last;
            when Black => RGB := 16#000000#;
            when Blue  => RGB := 16#0000FF#;
            when Green => RGB := 16#00FF00#;
            when Red   => RGB := 16#FF0000#;
            when White => RGB := 16#FFFFFF#;
         end case;
      else
         declare
            Line    : Line_Ptr_Type;
            Pointer : Integer;
            Last    : Integer;
         begin
            Get_Line (Code, Line, Pointer, Last);
            if Pointer > Last then
               Raise_Exception
               (  Syntax_Error'Identity,
                  "Color value is expected at " & Image (Link (Code))
               );
            end if;
            if Line (Pointer) = '#' then
               Pointer := Pointer + 1;
               declare
                  Value : Natural;
               begin
                  Get
                  (  Line (Line'First..Last),
                     Pointer,
                     Value,
                     Base  => 16,
                     First => 0,
                     Last  => 16#FFFFFF#
                  );
                  RGB := RGB_Color (Value);
               exception
                  when End_Error | Data_Error | Constraint_Error =>
                     Set_Pointer (Code, Pointer);
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "RGB value 0..FFFFFF is expected at "
                        &  Image (Link (Code))
                     )  );
               end;
               Set_Pointer (Code, Pointer);
            elsif Line (Pointer) = '%' then
               Pointer := Pointer + 1;
               Set_Pointer (Code, Pointer);
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "HSV colors aren't supported, as met at "
                  &  Image (Link (Code))
               )  );
            else
               Set_Pointer (Code, Pointer);
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Color value is expected at "
                  &  Image (Link (Code))
               )  );
            end if;
         end;
      end if;
      Match_End (Code);
      begin
         Add (Map, Name, RGB);
      exception
         when Name_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Duplicate color name '"
               &  Name
               &  "' at "
               &  Image (Name_At)
            )  );
      end;
   end Get_Color;

   function Get
            (  Code   : access Source_Type;
               Header : Descriptor
            )  return Table is
      This      : Source_Type renames Code.all;
      Name_At   : Location_Type;
      Map       : Table;
      Line      : Line_Ptr_Type;
      Pointer   : Integer;
      Last      : Integer;
      Color_Beg : Integer;
      Color_End : Integer;
   begin
      for Color_Entry in 1..Header.Map_Size loop
         Match (This, ",",  False);
         Match (This, """", False);
         Name_At := Link (This);
         Get_Line (This, Line, Pointer, Last);
         Color_Beg := Pointer;
         Color_End := Color_Beg + Header.Pixel_Size - 1;
         if Color_End - 1 > Last then
            Set_Pointer (This, Last + 1);
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Color definition is expected at "
               &  Image (Link (This))
            )  );
         end if;
         declare
            Name : String := Line (Color_Beg..Color_End);
         begin
            Set_Pointer (This, Color_End + 1);
            Get_Color (This, Map, Name, Name_At);
         end;
      end loop;
      return Map;
   end Get;

   function Get
            (  Code   : access Source_Type;
               Header : Descriptor;
               Map    : Table
            )  return Pixel_Buffer is
      This    : Source_Type renames Code.all;
      Result  : Pixel_Buffer (1..Header.Height, 1..Header.Width);
      Line    : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      for Row in Result'Range (1) loop
         Match (This, ",",  False);
         Match (This, """", False);
         Get_Line (This, Line, Pointer, Last);
         for Column in Result'Range (2) loop
            begin
               Get
               (  Line (Line'First..Last),
                  Pointer,
                  Map,
                  Result (Row, Column)
               );
               Set_Pointer (This, Pointer);
            exception
               when End_Error =>
                  Set_Pointer
                  (  This,
                     (  Pointer
                     +  Integer'Min (Header.Pixel_Size, Last)
                  )  );
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Undefined color name at "
                     &  Image (Link (This))
                  )  );
            end;
         end loop;
         Match_End (This);
      end loop;
      if Header.Extended then
         declare
            Got_It : Boolean := False;
         begin
            while not Got_It loop
               Skip (This, False);
               Get (This, """", Got_It);
               exit when not Got_It;
               Got_It := False;
               while not Got_It loop
                  Get_Line (This, Line, Pointer, Last);
                  while Pointer < Last loop
                     if Line (Pointer) = '\' then
                        Pointer := Pointer + 1;
                        exit when Pointer > Last;
                        Pointer := Pointer + 1;
                     elsif Line (Pointer) = '"' then
                        Got_It := True;
                        exit;
                     end if;
                  end loop;
               end loop;
               Skip (This, False);
               Get (This, ",", Got_It);
            end loop;
         end;
      end if;
      Match (This, "}", False);
      Match (This, ";", False);
      return Result;
   end Get;

end Parsers.Generic_Source.XPM;
