--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source                    Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  11:56 13 Oct 2012  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Parsers.Multiline_Source is

   Initial_Size : constant Integer := 512;

   function "<" (Left, Right : Position) return Boolean is
   begin
      return
      (  Left.Line < Right.Line
      or else
         (  Left.Line = Right.Line
         and then
            Left.Column < Right.Column
      )  );
   end "<";

   procedure Finalize (Code : in out Source) is
   begin
      Free (Code.Buffer);
   end Finalize;

   function End_Of (Code : Source'Class) return Boolean is
   begin
      return Code.Buffer = null;
   end End_Of;

   function Get_Line (Code : Source'Class) return String is
   begin
      if Code.Buffer = null then
         raise End_Error;
      else
         return Code.Buffer (1..Code.Length);
      end if;
   end Get_Line;

   procedure Get_Line
             (  Code    : Source'Class;
                Line    : out Line_Ptr;
                Pointer : out Integer;
                Last    : out Integer
             )  is
   begin
      if Code.Buffer = null then
         raise End_Error;
      else
         Line    := Code.Buffer.all'Unchecked_Access;
         Pointer := Code.Pointer;
         Last    := Code.Length;
      end if;
   end Get_Line;

   function Get_Backup_Pointer (Code : Source'Class) return Integer is
   begin
      return Code.Last;
   end Get_Backup_Pointer;

   function Get_Location
            (  Message : String;
               Prefix  : String := "at "
            )  return Location is
      Pointer : Integer  := Message'Last;
      Result  : Location := ((0, 0), (0, 0));
   begin
      loop -- An occurence of Prefix
         loop -- Searching backwards Message for Prefix
            if Pointer < Message'First + Prefix'Length - 1 then
               return Result;
            end if;
            exit when
               Message (Pointer - Prefix'Length + 1..Pointer) = Prefix;
            Pointer := Pointer - 1;
         end loop;
         Pointer := Pointer + 1;
            -- After the prefix
         begin
            Get (Message, Pointer, Integer (Result.First.Line));
            Result.Next.Line := Result.First.Line;
            if (  Pointer >= Message'Last
               or else
                  Message (Pointer) /= ':'
               )
            then
               return Result;
            end if;
               -- Line:
            Pointer := Pointer + 1;
            begin
               Get (Message, Pointer, Result.First.Column, First => 1);
            exception
               when others =>
                  return Result;
            end;
               -- Line:Column
            Result.Next.Column := Result.First.Column;
            if (  Pointer + 1 >= Message'Last
               or else
                  Message (Pointer..Pointer + 1) /= ".."
               )
            then
               return Result;
            end if;
            Pointer := Pointer + 2;
               -- Line:Column..
            begin
               Get (Message, Pointer, Integer (Result.Next.Line));
            exception
               when others =>
                  return Result;
            end;
               -- Line:Column..Line
            if (  Pointer >= Message'Last
               or else
                  Message (Pointer) /= ':'
               )
            then
               Result.Next.Column := Integer (Result.Next.Line);
               Result.Next.Line   := Result.First.Line;
               return Result;
            end if;
            Pointer := Pointer + 1;
               -- Line:Column..Line:
            begin
               Get (Message, Pointer, Result.Next.Column);
                  -- Line:Column..Line:Column
               return Result;
            exception
               when others =>
                  Result.Next.Column := Integer (Result.Next.Line);
                  Result.Next.Line   := Result.First.Line;
                  return Result;
            end;
         exception
            when others =>
               return Result;
         end;
      end loop;
   end Get_Location;

   function Get_Pointer (Code : Source'Class) return Integer is
   begin
      return Code.Pointer;
   end Get_Pointer;

   function Image (Link : Location) return String is
   begin
      if Link.First = Link.Next then
         return
         (  Image (Integer (Link.First.Line))
         &  ":"
         &  Image (Link.First.Column)
         );
      elsif Link.First.Line = Link.Next.Line then
         return
         (  Image (Integer (Link.First.Line))
         &  ":"
         &  Image (Link.First.Column)
         &  ".."
         &  Image (Link.Next.Column - 1)
         );
      else
         return
         (  Image (Integer (Link.First.Line))
         &  ":"
         &  Image (Link.First.Column)
         &  ".."
         &  Image (Integer (Link.Next.Line))
         &  ":"
         &  Image (Link.Next.Column - 1)
         );
      end if;
   end Image;

   procedure Initialize (Code : in out Source) is
   begin
      Code.Buffer := new String (1..Initial_Size);
      Next_Line (Code);
   exception
      when Storage_Error =>
         raise;
      when others =>
         null;
   end Initialize;

   function Link (Code : Source'Class) return Location is
   begin
      return ((Code.Line, Code.Last), (Code.Line, Code.Pointer));
   end Link;

   function "&" (Left, Right : Location) return Location is
      Result : Location;
   begin
      if Left.First < Right.First then
         Result.First := Left.First;
      else
         Result.First := Right.First;
      end if;
      if Left.Next < Right.Next then
         Result.Next := Right.Next;
      else
         Result.Next := Left.Next;
      end if;
      return Result;
   end "&";

   procedure Next_Line (Code : in out Source'Class) is
   begin
      Code.Pointer := 1;
      Code.Last := 1;
      Get_Line (Code);
      Code.Line := Code.Line + 1;
   exception
      when End_Error =>
         Free (Code.Buffer);
         raise;
   end Next_Line;

   procedure Reset_Pointer (Code : in out Source'Class) is
   begin
      Code.Pointer := Code.Last;
   end Reset_Pointer;

   procedure Set_Pointer
             (  Code    : in out Source'Class;
                Pointer : Integer
             )  is
   begin
      if Code.Buffer = null then
         if Pointer /= 1 then
            raise Ada.IO_Exceptions.Layout_Error;
         end if;
      else
         if Pointer not in Code.Last..Code.Length + 1 then
            raise Ada.IO_Exceptions.Layout_Error;
         end if;
      end if;
      Code.Last    := Code.Pointer;
      Code.Pointer := Pointer;
   end Set_Pointer;

   procedure Skip (Code : in out Source'Class; Link : Location) is
   begin
      if Code.Line > Link.First.Line then
         raise Ada.IO_Exceptions.Layout_Error;
      end if;
      while Code.Line < Link.First.Line loop
         Next_Line (Code);
      end loop;
      Set_Pointer (Code, Link.First.Column);
      while Code.Line < Link.Next.Line loop
         Next_Line (Code);
      end loop;
      Set_Pointer (Code, Link.Next.Column);
   end Skip;

end Parsers.Multiline_Source;
