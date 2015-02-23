--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Get_Text             Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

procedure Parsers.Generic_Source.Get_Text
          (  Code   : in out Source_Type;
             Text   : String;
             Got_It : out Boolean;
             Map    : Character_Mapping := Identity
          )  is
   Line    : Line_Ptr_Type;
   Pointer : Integer;
   Last    : Integer;
begin
   Get_Line (Code, Line, Pointer, Last);
   if Last - Pointer >= Text'Length - 1 then
      for Index in Text'Range loop
         if Value (Map, Text (Index)) /= Value (Map, Line (Pointer))
         then
            Got_It := False;
            return;
         end if;
         Pointer := Pointer + 1;
      end loop;
      Got_It := True;
      Set_Pointer (Code, Pointer);
   else
      Got_It := False;
   end if;
exception
   when End_Error =>
      Got_It := False;
end Parsers.Generic_Source.Get_Text;
