--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        Get_UTF8_Text                            Winter, 2009       --
--  Implementation                                                    --
--                                Last revision :  22:28 15 Feb 2009  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Strings_Edit.UTF8;  use Strings_Edit.UTF8;

procedure Parsers.Generic_Source.Get_UTF8_Text
          (  Code   : in out Source_Type;
             Text   : String;
             Got_It : out Boolean;
             Map    : Unicode_Mapping := Identity
          )  is
   Line    : Line_Ptr_Type;
   Code_1  : UTF8_Code_Point;
   Code_2  : UTF8_Code_Point;
   Pointer : Integer;
   Index   : Integer := Text'First;
   Last    : Integer;
begin
   Get_Line (Code, Line, Pointer, Last);
   if Last - Pointer >= Text'Length - 1 then
      while Index <= Text'Last loop
         begin
            Get (Line.all, Pointer, Code_1);
         exception
            when Data_Error =>
               Set_Pointer (Code, Pointer);
               Set_Pointer (Code, Pointer);
               Raise_Exception
               (  Syntax_Error'Identity,
                  "Invalid UTF-8 encoding at " & Image (Link (Code))
               );
         end;
         Get (Text, Index, Code_2);
         if Value (Map, Code_1) /= Value (Map, Code_2) then
            Got_It := False;
            return;
         end if;
      end loop;
      Got_It := True;
      Set_Pointer (Code, Pointer);
   else
      Got_It := False;
   end if;
exception
   when Data_Error | End_Error =>
      Got_It := False;
end Parsers.Generic_Source.Get_UTF8_Text;
