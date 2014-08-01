--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        Get_Ada_2005_Blank                       Winter, 2009       --
--  Implementation                                                    --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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
with Ada.Strings.Maps;   use Ada.Strings.Maps;
with Strings_Edit;       use Strings_Edit;
with Strings_Edit.UTF8;  use Strings_Edit.UTF8;

with Strings_Edit.UTF8.Categorization;

procedure Parsers.Generic_Source.Get_Ada_2005_Blank
          (  Code   : in out Source_Type;
             Got_It : out Boolean
          )  is
   use Strings_Edit.UTF8.Categorization;
   Buffer  : Line_Ptr_Type;
   Pointer : Integer;
   Start   : Integer;
   Last    : Integer;
begin
   loop
      Get_Line (Code, Buffer, Pointer, Last);
      declare
         Line   : String renames Buffer (Buffer'First..Last);
         Symbol : UTF8_Code_Point;
      begin
         while Pointer <= Line'Last loop
            begin
               Start := Pointer;
               Get (Line, Pointer, Symbol);
            exception
               when Data_Error =>
                  Set_Pointer (Code, Start);
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     "Illegal UTF-8 encoding at " & Image (Link (Code))
                  );
            end;
            case Symbol is
               when 16#09# | 16#20# => -- HT or SP (blank)
                  null;
               when 16#0A#..16#0D# =>  -- LF, VT, FF, CR (line end)
                  exit;
               when 16#2D# =>          -- Minus
                  exit when
                       (  Pointer <= Line'Last -- Ada comment
                       and then
                          Line (Pointer) = '-'
                       );
                  Got_It := True;
                  Set_Pointer (Code, Start);
                  return;
               when others =>
                  if Category (Symbol) /= Zs then -- Not a space
                     Got_It := True;
                     Set_Pointer (Code, Start);
                     return;
                  end if;
            end case;
         end loop;
         Set_Pointer (Code, Line'Last + 1);
      end;
      Next_Line (Code);
   end loop;
exception
   when End_Error =>
      Got_It := False;
end Parsers.Generic_Source.Get_Ada_2005_Blank;
