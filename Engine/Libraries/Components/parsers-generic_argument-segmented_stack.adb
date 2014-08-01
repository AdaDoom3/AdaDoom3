--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Argument.                   Luebeck            --
--        Segmented_Stack                          Winter, 2004       --
--  Implementation                                                    --
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

package body Parsers.Generic_Argument.Segmented_Stack is
   use Stub_Stack;

   function Is_Empty (Container : Stack) return Boolean is
   begin
      return Mark (Container.Data) - Top (Container.Stubs) < 1;
   end Is_Empty;

   procedure Mark (Container : in out Stack) is
   begin
      Push (Container.Stubs, Mark (Container.Data));
   end Mark;

   procedure Pop
             (  Container : in out Stack;
                List      : in out Frame
             )  is
   begin
      if (  (  Mark (Container.Data)
            -  Top  (Container.Stubs)
            )
         >= List'Length
         )
      then
         for Index in reverse List'Range loop
            List (Index) := Top (Container.Data);
            Pop (Container.Data);
         end loop;
      else
         raise Constraint_Error;
      end if;
   end Pop;

   procedure Push
             (  Container : in out Stack;
                Argument  : Argument_Type
             )  is
   begin
      Push (Container.Data, Argument);
   end Push;

   procedure Release (Container : in out Stack) is
   begin
      Release (Container.Data, Top (Container.Stubs));
      Pop (Container.Stubs);
   end Release;

end Parsers.Generic_Argument.Segmented_Stack;
