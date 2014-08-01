--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Token.                      Luebeck            --
--        Segmented_Lexer                          Winter, 2004       --
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
--  This  generic  package  provides  an  abstract type for table driven
--  infix expression  lexers  using  segmented  argument  and  operation
--  stacks. The package has the following formal parameters:
--
--  (o)  Argument_Frame_Segment_Size,
--  (o)  Argument_Frame_Minimal_Size,
--  (o)  Argument_Frame_Increment,
--  (o)  Argument_Stub_Minimal_Size,
--  (o)  Argument_Stub_Increment,
--  (o)  Operation_Segment_Size,
--  (o)  Operation_Minimal_Size,
--  (o)  Operation_Increment.
--
--  The formal parameters Argument_* control argument  stack  allocation
--  policy,  see  Parsers.Generic_Argument.Segmented_Stack.  The  formal
--  parameters Operation_* control operation  stack  allocation  policy,
--  see Parsers.Generic_Operation.Segmented_Stack.
--
--  The  package  instantiates Parsers.Generic_Token.Generic_Lexer under
--  name Token_Lexer. It defines the type  Lexer  used  as  an  abstract
--  base.  A  type  derived  from  Lexer  has to implement the following
--  abstract  subroutines  defined  in  the  package Lexers (which is an
--  instance of Parsers.Generic_Lexer, instantiated in the  public  part
--  of this package):
--
--       Call        - To an operator
--       Enclose     - Call to a function, index, aggregate, brackets
--       Get_Blank   - Skip blanks and comments in the source
--       Get_Operand - Get an operand
--
--  and the most of the following error handlers,  also  implemented  as
--  abstract subroutines:
--
--       On_Association_Error
--       On_Missing_Operand
--       On_Unexpected (non-abstract, but can be overridden)
--       On_Wrong_Comma
--       On_Wrong_Right_Bracket
--
--  The discriminants of the type identify the source to parse  and  the
--  tables of prefix, infix and postifix operations. When recognized, an
--  operation  is  matched  from  the  corresponding  table. This causes
--  pushing it onto the operation stack.
--
with Parsers.Generic_Argument.Segmented_Stack;
with Parsers.Generic_Operation.Segmented_Stack;
with Parsers.Generic_Token.Generic_Token_Lexer;

generic
   Argument_Frame_Segment_Size : Positive := 128;
   Argument_Frame_Minimal_Size : Positive := 64;
   Argument_Frame_Increment    : Natural  := 50;
   Argument_Stub_Minimal_Size  : Positive := 64;
   Argument_Stub_Increment     : Natural  := 50;
   Operation_Segment_Size      : Positive := 128;
   Operation_Minimal_Size      : Positive := 64;
   Operation_Increment         : Natural  := 50;
package Parsers.Generic_Token.Segmented_Lexer is
   package Lexical_Arguments is
      new Arguments.Segmented_Stack
          (  Frame_Segment_Size => Argument_Frame_Segment_Size,
             Frame_Minimal_Size => Argument_Frame_Minimal_Size,
             Frame_Increment    => Argument_Frame_Increment,
             Stub_Minimal_Size  => Argument_Stub_Minimal_Size,
             Stub_Increment     => Argument_Stub_Increment
          );
   package Lexical_Descriptors is
      new Descriptors.Segmented_Stack
          (  Segment_Size => Operation_Segment_Size,
             Minimal_Size => Operation_Minimal_Size,
             Increment    => Operation_Increment
          );
   package Token_Lexer is
      new Generic_Token_Lexer
          (  Operations      => Lexical_Descriptors.Operation,
             Argument_Stack  => Lexical_Arguments.Stack,
             Operation_Stack => Lexical_Descriptors.Operation.Stack
          );
--
-- Lexer -- The base type for user-defined lexers
--
--    Prefixes  - The table of prefix operations
--    Infixes   - The table of infix operations
--    Postfixes - The table of postfix operations
--
   subtype Lexer is Token_Lexer.Lexer;
--
-- Parse -- Expression in the source
--
--    Context - The parsing context
--    Code    - The source to parse
--    Result  - The expression result
--
-- Upon successful completion Result  is  one  of  the  expression.  The
-- context state indicates how far the expression parsing gone. Parse is
-- recursive-call safe  as  long  as  implementations  of  the  abstract
-- operations do not change Context and Code in an inappropriate way. It
-- means that an implementation of an operation may in turn  call  Parse
-- to get a subexpression from source if that necessary.
--
-- Exceptions :
--
--    Any - Propagated from the handlers
--
   procedure Parse
             (  Context : in out Token_Lexer.Implementation.Lexer'Class;
                Code    : in out Source_Type;
                Result  : out Argument_Token
             )  renames Token_Lexer.Implementation.Parse;

end Parsers.Generic_Token.Segmented_Lexer;
