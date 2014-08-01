--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Get_Blank            Luebeck            --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Strings_Edit;            use Strings_Edit;

procedure Parsers.Generic_Source.Get_Blank
          (  Code   : in out Source_Type;
             Got_It : out Boolean
          )  is 
   To_Skip : constant Character_Set :=
                         To_Set (' ' & HT & CR & LF & VT & FF);
   Buffer  : Line_Ptr_Type;
   Pointer : Integer;
   Last    : Integer;
begin
   loop
      Get_Line (Code, Buffer, Pointer, Last);
      declare
         Line : String renames Buffer (Buffer'First..Last);
      begin
         Get (Line, Pointer, To_Skip);
         Set_Pointer (Code, Pointer);
         if Pointer <= Line'Last then
            Got_It := True;
            return;
         end if;
      end;
      Next_Line (Code); 
   end loop;
exception
   when End_Error =>
      Got_It := False;
end Parsers.Generic_Source.Get_Blank;
