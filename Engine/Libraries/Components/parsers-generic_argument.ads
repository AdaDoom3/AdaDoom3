--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Argument                    Luebeck            --
--  Interface                                      Winter, 2004       --
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
--
--  This generic package defines the base  abstract  type  for  argument
--  stacks. The generic parameter Argument_Type identifies appearance of
--  an  argument  in the source. Usually it is the argument and a source
--  location link. 
--
with Ada.Finalization;

generic
   type Argument_Type is private;
package Parsers.Generic_Argument is
--
-- Frame -- The argument list of an operation
--
   type Argument_No is new Positive;
   type Frame is array (Argument_No range <>) of Argument_Type;
--
-- Stack -- The argument stack
--   
   type Stack is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Is_Empty -- Check if there are any arguments in the current fragment
--
--    Container - The argument stack
--
-- Returns :
--
--    True - if the current stack fragment is empty
--
   function Is_Empty (Container : Stack) return Boolean is abstract;
--
-- Mark -- Create a new stack fragment
--
--    Container - The argument stack
--
-- A  stack  fragment represents  an  independent  argument  stack.   No
-- arguments  below  mark  can  be  accessed in any way until Release is
-- called. 
--
   procedure Mark (Container : in out Stack) is abstract;
--
-- Pop -- Argument frame from the stack top
--
--    Container - The argument stack
--    List      - The argument list to fill in
--
-- Exceptions :
--
--    Constraint_Error - Not enough arguments
--
   procedure Pop
             (  Container : in out Stack;
                List      : in out Frame
             )  is abstract;
--
-- Push -- An argument
--
--    Container - The argument stack
--    Argument  - To push
--
   procedure Push
             (  Container : in out Stack;
                Argument  : Argument_Type
             )  is abstract;
--
-- Release -- Remove the stack segment
--
--    Container - The argument stack
--
-- This  procedure  should be called for each call to Mark. If there are
-- any arguments on the stack pushed after the mark, they are removed. 
--
-- Exceptions :
--
--    Constraint_Error - Empty stack
--
   procedure Release (Container : in out Stack) is abstract;

private
   type Stack is abstract
      new Ada.Finalization.Limited_Controlled with null record;

end Parsers.Generic_Argument;
