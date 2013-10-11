--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Operation.                  Luebeck            --
--        Segmented_Stack                          Winter, 2004       --
--  Interface                                                         --
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
--  This  child  package  provides  an implementation of operation stack
--  based    on    the    segmented    stacks    from    the     package
--  Generic_Segmented_Stack.  The implementation is provided by the type
--  package  Operation in the package public part. The stack type can be
--  denoted as ...Operation.Stack. 
--
with Generic_Segmented_Stack;
with Parsers.Generic_Operation.Generic_Stack;

generic
   Segment_Size : Positive := 128;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Parsers.Generic_Operation.Segmented_Stack is
   type Raw_Index is new Integer;
   package Raw_Operation_Stack is
      new Generic_Segmented_Stack
          (  Index_Type   => Raw_Index,
             Object_Type  => Descriptor,
             Null_Element => (Class => Stub),
             Segment_Size => Segment_Size,
             Minimal_Size => Minimal_Size,
             Increment    => Increment
          );
   use Raw_Operation_Stack.Segmented_Stack;
--
-- Operation -- The operation stack package
--
   package Operation is
      new Generic_Stack
          (  Raw_Operation_Stack.Segmented_Stack.Stack,
             Raw_Index
          );
end Parsers.Generic_Operation.Segmented_Stack;
