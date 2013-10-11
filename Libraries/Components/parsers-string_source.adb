--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.String_Source                       Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

with Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Parsers.String_Source is

   function End_Of (Code : Source) return Boolean is
   begin
      return Code.Pointer > Code.Text'Last;
   end End_Of;

   function Get_Line (Code : Source) return String is
   begin
      return Code.Text.all;
   end Get_Line;

   procedure Get_Line
             (  Code    : Source;
                Line    : out Line_Ptr;
                Pointer : out Integer;
                Last    : out Integer
             )  is
   begin
      Line    := Code.Text.all'Unchecked_Access;
      Pointer := Code.Pointer;
      Last    := Code.Text'Last;
   end Get_Line;

   function Get_Backup_Pointer (Code : Source) return Integer is
   begin
      return Code.Last;
   end Get_Backup_Pointer;

   function Get_Pointer (Code : Source) return Integer is
   begin
      return Code.Pointer;
   end Get_Pointer;

   function Image (Link : Location) return String is
   begin
      if Link.Length = 0 then
         return Image (Link.From);
      else
         return
         (  Image (Link.From)
         &  ".."
         &  Image (Link.From + Link.Length - 1)
         );
      end if;
   end Image;

   function Link (Code : Source) return Location is
   begin
      return (Code.Last, Code.Pointer - Code.Last);
   end Link;

   function "&" (Left, Right : Location) return Location is
      From : constant Integer := Integer'Min (Left.From, Right.From);
      To   : constant Integer :=
                         Integer'Max
                         (  Left.From  + Left.Length,
                            Right.From + Right.Length
                         );
   begin
      return (From, To - From);
   end "&";
 
   procedure Next_Line (Code : in out Source) is
   begin
      raise Ada.IO_Exceptions.End_Error;
   end Next_Line;

   procedure Reset_Pointer (Code : in out Source) is
   begin
      Code.Pointer := Code.Last;
   end Reset_Pointer;

   procedure Set_Pointer (Code : in out Source; Pointer : Integer) is 
   begin
      if Pointer not in Code.Last..Code.Text'Last + 1 then
         raise Ada.IO_Exceptions.Layout_Error;
      end if;
      Code.Last    := Code.Pointer;
      Code.Pointer := Pointer;
   end Set_Pointer;

end Parsers.String_Source;
