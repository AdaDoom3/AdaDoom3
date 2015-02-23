--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Argument.                   Luebeck            --
--        Segmented_Stack                          Winter, 2004       --
--  Interface                                                         --
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
--  This child package provides  an  implementation  of  argument  stack
--  using segmented stacks from the package Generic_Segmented_Stack. The
--  implementation is provided by the type Stack.  
--
with Generic_Segmented_Stack;
with Generic_Stack;
with Generic_Unbounded_Array;

generic
   Frame_Segment_Size : Positive := 128;
   Frame_Minimal_Size : Positive := 64;
   Frame_Increment    : Natural  := 50;
   Stub_Minimal_Size  : Positive := 64;
   Stub_Increment     : Natural  := 50;
package Parsers.Generic_Argument.Segmented_Stack is
--
-- Stack -- Argument stack
--
   type Stack is new Parsers.Generic_Argument.Stack with private;
--
-- Is_Empty -- Overrides Parsers.Generic_Argument...
--
   function Is_Empty (Container : Stack) return Boolean;
   pragma Inline (Is_Empty);
--
-- Mark -- Overrides Parsers.Generic_Argument...
--
   procedure Mark (Container : in out Stack);
   pragma Inline (Mark);
--
-- Pop -- Overrides Parsers.Generic_Argument...
--
   procedure Pop
             (  Container : in out Stack;
                List      : in out Frame
             );
--
-- Push -- Overrides Parsers.Generic_Argument...
--
   procedure Push
             (  Container : in out Stack;
                Argument  : Argument_Type
             );
   pragma Inline (Push);
--
-- Release -- Overrides Parsers.Generic_Argument...
--
   procedure Release (Container : in out Stack);
   pragma Inline (Release);

private
   type Index is new Integer;
   Default : Argument_Type;
--
-- Argument_Stack -- Stack of arguments
--
   package Argument_Stack is
      new Generic_Segmented_Stack
          (  Index_Type   => Index,
             Object_Type  => Argument_Type,
             Null_Element => Default,
             Segment_Size => Frame_Segment_Size,
             Minimal_Size => Frame_Minimal_Size,
             Increment    => Frame_Increment
          );
   use Argument_Stack.Segmented_Stack;

   type Stub_Index is new Integer;
   type Stub_Array is array (Stub_Index range <>) of Index;
   package Unbounded_Stub_Array is
      new Generic_Unbounded_Array
          (  Index_Type        => Stub_Index,
             Object_Type       => Index,
             Object_Array_Type => Stub_Array,
             Null_Element      => 0,
             Minimal_Size      => Stub_Minimal_Size,
             Increment         => Stub_Increment
          );
   use Unbounded_Stub_Array;
--
-- Stub_Stacks -- Stack of stubs
--
   package Stub_Stack is
      new Generic_Stack
          (  Index_Type   => Stub_Index,
             Object_Type  => Index,
             Array_Type   => Unbounded_Array,
             Null_Element => 0
          );

   type Stack is new Parsers.Generic_Argument.Stack with record
      Data  : Argument_Stack.Segmented_Stack.Stack;
      Stubs : Stub_Stack.Stack;   
   end record;

end Parsers.Generic_Argument.Segmented_Stack;
