--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        Get_Cpp_Blank                            Winter, 2004       --
--  Implementation                                                    --
--                                Last revision :  19:13 09 Jul 2009  --
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

procedure Parsers.Generic_Source.Get_Cpp_Blank
          (  Code     : in out Source_Type;
             Got_It   : out Boolean;
             Error    : out Boolean;
             Error_At : out Location_Type
          )  is
   Line       : Line_Ptr_Type;
   Pointer    : Integer;
   Last       : Integer;
   In_Comment : Boolean := False;
begin
Chain_Of_Blanks :
   loop
      Get_Line (Code, Line, Pointer, Last);
      while Pointer <= Last loop
         if In_Comment then
            while Pointer < Last loop
               if Line (Pointer..Pointer + 1) = "*/" then
                  Pointer := Pointer + 2;
                  In_Comment := False;
                  exit;
               end if;
               Pointer := Pointer + 1;
            end loop;
            exit when In_Comment; -- Comment does not end at this line
         else
            case Line (Pointer) is
               when ' ' | HT | VT | FF | CR | LF =>
                  Pointer := Pointer + 1;
               when '/' =>
                  exit Chain_Of_Blanks when Pointer = Last;
                  case Line (Pointer + 1) is
                     when '/' =>
                        exit;
                     when '*' =>
                        In_Comment := True;
                        Set_Pointer (Code, Pointer);
                        Pointer := Pointer + 2;
                        Set_Pointer (Code, Pointer);
                        Error_At := Link (Code);
                     when others =>
                        exit Chain_Of_Blanks;
                  end case;
               when others =>
                  exit Chain_Of_Blanks;
            end case;
         end if;
      end loop;
      Set_Pointer (Code, Last + 1);
      Next_Line (Code);
   end loop Chain_Of_Blanks;
   Got_It := True;
   Error  := False;
   Set_Pointer (Code, Pointer);
exception
   when End_Error =>
      Got_It := False;
      Error  := In_Comment;
end Parsers.Generic_Source.Get_Cpp_Blank;
