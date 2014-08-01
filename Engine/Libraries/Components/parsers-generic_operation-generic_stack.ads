--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Operation.                  Luebeck            --
--        Generic_Stack                            Winter, 2004       --
--  Interface                                                         --
--                                Last revision :  15:35 29 Apr 2012  --
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
--  This package defines the stack of operations to be used by a  lexer.
--  The operation stack is  defined  upon  an  raw  stack  of  operation
--  descriptors provided by the generic parameters:
--
--  (o)  Descriptor_Stack is the type of the raw stack
--  (o)  Index_Type is the type to index items on the stack
--  (o)  Get returns a stack item by its index
--  (o)  Is_Empty check if the raw stack is empty
--  (o)  Mark returns the index of the next to top stack item
--  (o)  Pop pops one operation descriptor from the raw stack top
--  (o)  Push pushes one operation descriptor onto the raw stack top
--  (o)  Put replaces a stack item by index
--  (o)  Top gets the raw stack top
--
--  The operation stack is provided by  the  type  Stack.  The  type  is
--  abstract. It  has  the  abstract  operations  Call  and  Enclose  to
--  implement.
--
with Ada.Finalization;

generic
   type Descriptor_Stack is limited private;
   type Index_Type is (<>);
   with function Get
                 (  Container : Descriptor_Stack;
                    Index     : Index_Type
                 )  return Descriptor is <>;
   with function Is_Empty (Container : Descriptor_Stack)
      return Boolean is <>;
   with function Mark (Container : Descriptor_Stack)
      return Index_Type is <>;
   with procedure Pop
                  (  Container : in out Descriptor_Stack;
                     Count     : Natural := 1
                  )  is <>;
   with procedure Push
                  (  Container : in out Descriptor_Stack;
                     Item      : Descriptor
                  )  is <>;
   with procedure Put
                  (  Container : in out Descriptor_Stack;
                     Index     : Index_Type;
                     Element   : Descriptor
                  )  is <>;
   with function Top (Container : Descriptor_Stack)
      return Descriptor is <>;
package Parsers.Generic_Operation.Generic_Stack is
--
-- Stack -- The operation stack type
--
   type Stack is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Call -- To an operator
--
--    Container - The operation stack
--    Operation - To call to
--    Count     - The number of parameters
--
-- This procedure  is  called  to  execute  an  operator  when  all  its
-- arguments   are   known.  Binary  commutative  operations  for  which
-- Is_Commutative  returns  true are optimized, so that one Call is used
-- instead of a sequence of calls in cases like A+B+C. Which will result
-- in "+"(A,B,C) instead of "+"("+"(A,B),C).
--
   procedure Call
             (  Container : in out Stack;
                Operation : Operation_Type;
                Count     : Natural
             )  is abstract;
--
-- Get -- Operation on the stack
--
--    Context - The parsing context
--    Depth   - Of the operation, 0 corresponds to the stack top
--
-- Returns :
--
--    The operation
--
-- Exceptions :
--
--    Constraint_Error - Depth >= Get_Depth
--
   function Get (Container : Stack; Depth : Natural) return Descriptor;
--
-- Get_Depth -- The stack depth
--
--    Container - The stack
--
-- Returns :
--
--    The number of operations on the stack
--
   function Get_Depth (Container : Stack) return Natural;
--
-- Is_Expected -- Confirm an operator out of brackets
--
--    Container - The operation stack
--    Operator  - To verify
--
-- The default implementation returns True.
--
-- Returns :
--
--    True if the operator is accepted
--
   function Is_Expected
            (  Container : Stack;
               Operator  : Operation_Type
            )  return Boolean;
--
-- Enclose -- Call to an argument tuple
--
--    Container - The operation stack
--    Operation - To call to
--    Count     - The number of arguments
--
-- This procedure is called for brackets, sublists and function calls.
--
   procedure Enclose
             (  Container : in out Stack;
                Left      : Operation_Type;
                Right     : Operation_Type;
                Count     : Natural
             )  is abstract;
--
-- Is_Empty -- Check if the stack is empty
--
--    Container - The operation stack
--
-- Returns :
--
--    True if the operation stack is empty
--
   function Is_Empty (Container : Stack) return Boolean;
--
-- Pop -- Remove the stack top item
--
--    Container   - The operation stack
--
-- When  stub is on the top, then the stack is semantically empty and so
-- stub it cannot be popped.
--
-- Exceptions :
--
--    Constraint_Error - An empty stack
--
   procedure Pop (Container : in out Stack'Class);
--
-- Push_Abort -- Abort evaluation of an expression
--
--    Container - The operation stack
--
-- This  procedure cleans the stack to remove the side-effects of a call
-- to Push_Start. It is used upon an unrecoverable expression evaluation
-- errors.
--
   procedure Push_Abort (Container : in out Stack'Class);
--
-- Push_End -- Finish evaluation of an expression
--
--    Container - The operation stack
--
-- This procedure is called when the  right  margin  of  the  expression
-- reached.  This can be a source end or a reserved keyword. It can also
-- be an extra delimiter (see Unexpected_Comma, Unexpected_Right_Bracket
-- exceptions). After successful completion the stack is returned to its
-- state  before  the  call  to  Push_Start.  Missing_Right_Bracket   is
-- propagated  when  some left brackets of the expression remain open. A
-- handler should either close them using  Push_Right_Bracket  and  then
-- try Push_End again or call Push_Abort.
--
-- Exceptions :
--
--    Missing_Right_Bracket - Missing right bracket

   procedure Push_End (Container : in out Stack'Class);
--
-- Push_Start -- Initiate expression evaluation
--
--    Container - The operation stack
--
-- This  procedure  pushes  a  stub onto the stack. A stub is removed by
-- either a successful call to Push_End or by a call to Push_Abort.
--
   procedure Push_Start (Container : in out Stack'Class);
--
-- Push_Binary -- A binary operator
--
--    Container - The operation stack
--    Operation - The binary operator
--    Left      - The left association priority
--    Right     - The right association priority
--    Unchecked - Perform no association checks
--    Explicit  - The operator was recognized
--
-- Binary operators are ones having two arguments, such as addition  and
-- subtraction. They appear in the infix context. The parameter Explicit
-- should  be  False  if  the operator was assumed in place of a missing
-- one.
--
-- Exceptions :
--
--    Association_Error - An incompatible operator on the left
--
   procedure Push_Binary
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False;
                Explicit  : Boolean := True
             );
--
-- Push_Comma -- A comma
--
--    Container - The operation stack
--    Operation - The comma
--    Comma     - The comma, when true; ligature, when false
--    Unchecked - Perform no association checks
--
-- There are two kinds of commas supported: plain commas and  ligatures.
-- When  a plain comma matches the left bracket, it increases the number
-- of  arguments  the  list  in  the  brackets  has. A ligature does not
-- increase  the  number  of  arguments,  but  binds  two  arguments  it
-- separates. For example:
--
--    Foo (X, Left => Y + 2);      "(" ")"
--                                 /  |  \
--    "," is a comma             Foo  X  "=>"
--                                       /  \
--    "=>" is a ligature              Left  "+"
--                                          / \
--                                         Y   2
--
-- Ligatures can be viewed as binary  non-commutative  operations  which
-- may appear only within brackets and have no priority. Call is applied
-- to ligatures.
--
-- Exceptions :
--
--    Unexpected_Comma - There is no any left bracket to match
--    Wrong_Comma_Type - The left bracket does not match
--
   procedure Push_Comma
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Comma     : Boolean;
                Unchecked : Boolean := False
             );
--
-- Push_Left_Bracket -- A left bracket in the prefix context
--
--    Container - The operation stack
--    Operation - The bracket
--
-- A  prefix  context  bracket  is  one  like an order bracket or a left
-- bracket of an aggregate.
--
   procedure Push_Left_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type
             );
--
-- Push_Left_Bracket -- A left bracket in the infix context
--
--    Container - The operation stack
--    Operation - The bracket
--    Left      - The left association priority of the bracket
--    Unchecked - Perform no association checks
--
-- An infix context bracket is one of a function call or an array index.
--
-- Exceptions :
--
--    Association_Error - An incompatible operator on the left
--
   procedure Push_Left_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Unchecked : Boolean := False
             );
--
-- Push_Right_Bracket -- A right bracket
--
--    Container - The operation stack
--    Operation - The bracket
--    Unchecked - Perform no association checks
--
-- Exceptions :
--
--    Unexpected_Right_Bracket - There is no any left bracket to match
--    Wrong_Right_Bracket_Type - The left bracket does not match
--
   procedure Push_Right_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Unchecked : Boolean := False
             );
--
-- Push_Postfix -- A postfix unary operator
--
--    Container - The operation stack
--    Operation - The unary operator
--    Left      - The left association priority
--    Right     - The right association priority
--    Unchecked - Perform no association checks
--
-- Postfix unary operators have only one argument. They appear after the
-- argument.
--
-- Exceptions :
--
--    Association_Error - An incompatible operator on the left
--
   procedure Push_Postfix
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False
             );
--
-- Push_Prefix -- A prefix unary operator
--
--    Container - The operation stack
--    Operation - The unary operator
--    Left      - The left association priority
--    Right     - The right association priority
--    Unchecked - Perform no association checks
--
-- Prefix unary operators have only one argument. They appear before the
-- argument.
--
-- Exceptions :
--
--    Association_Error - An incompatible operator on the left
--
   procedure Push_Prefix
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False
             );
--
-- Push_Semicolon -- A semicolon (sublists separator)
--
--    Container - The operation stack
--    Operation - The sublist operation
--    Class     - The class of the separator
--    Priority  - The association priority
--    Unchecked - Perform no association checks
--
-- This procedure is used to process a sublist separator. For example:
--
--   F (X1, X2; Y1, Y2)   F (X1, X2 # Y1, Y2)   F (X1, X2 with Y1, Y2)
--
--        "(" ")"               "(" ")"           "(" ")"
--        / |   \               / | | \           / | | \
--       F  |    \             F  | Y1 Y2        F  | X2 \
--          |     \               |                X1     \
--      "(" ";"   ";" ")"      "(" "#"                "with" ")"
--       /   \     /   \        /   \                     /  \
--      X1   X2   Y1   Y2      X1   X2                   Y1  Y2
--
-- Here  ";", "#" and "with" are sublist separators. ";" breaks the list
-- of F into two sublists. "#" closes the sublist of the list  of  F  on
-- the left. "with" creates a sublist of the arguments on the left.  The
-- sublists are grouped according to the separator priorities. Higher is
-- the priority, higher is its associativity  with  the  arguments.  The
-- operation  specified  for  a  sublist  is  used  in  Enclose when the
-- operands: items of a sublist become all known.
--
-- Exceptions :
--
--    Association_Error - Incompatible operator on the left
--    Unexpected_Comma  - There is no any left bracket to match
--    Wrong_Comma_Type  - The left bracket or sublist does not match
--
   procedure Push_Semicolon
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type;
                Unchecked : Boolean := False
             );
--
-- Replace -- The stack top
--
--    Container   - The operation stack
--    Replacement - The descriptor to replace the stack top
--
-- When stub is on the top, then the stack is semantically empty.
--
-- Exceptions :
--
--    Constraint_Error - An empty stack
--
   procedure Replace
             (  Container   : in out Stack'Class;
                Replacement : Descriptor
             );
--
-- Top -- The stack top
--
--    Container - The operation stack
--
-- Note that when stub is on  the  top,  then  the  operation  stack  is
-- semantically empty.
--
-- Returns :
--
--    The descriptor of the last operation on the stack
--
-- Exceptions :
--
--    Constraint_Error - An physically empty stack
--
   function Top (Container : Stack) return Descriptor;

private
   pragma Inline (Get);
   pragma Inline (Get_Depth);
   pragma Inline (Is_Expected);
   pragma Inline (Is_Empty);
   pragma Inline (Pop);
   pragma Inline (Replace);
   pragma Inline (Top);

   type Stack is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Raw : Descriptor_Stack;
   end record;

end Parsers.Generic_Operation.Generic_Stack;
