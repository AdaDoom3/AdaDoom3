--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Keywords             Luebeck            --
--  Interface                                      Summer, 2005       --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;

with Parsers.Generic_Source.Get_Token;

package body Parsers.Generic_Source.Keywords is
   use Keyword_Tables;
   
   Folder : Dictionary;

   procedure Check_Spelling (Name : String) is
      Symbol : Character;
   begin
      if Name'Length = 0 or else not Is_Letter (Name (Name'First)) then
         raise Constraint_Error;
      end if;
      for Index in Name'First + 1 .. Name'Last loop
         Symbol := Name (Index);
         if not (Is_Alphanumeric (Symbol) or else Symbol = '_') then
            raise Constraint_Error;
         end if;
      end loop;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      return
         not
         (  Is_Alphanumeric (Source (Pointer))
         or else
            Source (Pointer) = '_'
         );
   end Check_Matched;

   procedure Get_Keyword is
      new Parsers.Generic_Source.Get_Token (Keyword_Raw_Tables);

   procedure Get
             (  Code   : in out Source_Type;
                Token  : out Keyword;
                Got_It : out Boolean
             )  is
   begin
      Get_Keyword (Code, Folder, Token, Got_It);
   end Get;

begin
   for Item in Keyword'Range loop
      Add (Folder, Keyword'Image (Item), Item);
   end loop;
end Parsers.Generic_Source.Keywords;
