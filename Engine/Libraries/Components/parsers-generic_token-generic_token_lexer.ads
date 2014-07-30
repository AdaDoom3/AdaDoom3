--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Token.                      Luebeck            --
--        Generic_Token_Lexer                      Winter, 2004       --
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
--  This  generic  package  provides  an  abstract type for table driven
--  infix expression  lexers.  The  package  has  the  following  formal
--  parameters:
--
--  (o)  Argument_Stack implements argument stack. It is a descendant of
--       Stack  defined in the package Arguments (see the parent package
--       Parsers.Generic_Token);
--  (o)  Operations is an implementation of operation stacks. It  is  an
--       instance of  Parsers.Generic_Operation.Generic_Stack  based  on
--       the  package  Descriptors  (a  parent   package   instance   of
--       Parsers.Generic_Operation, see Parsers.Generic_Token);
--  (o)  Operation_Stack is the operation stack type,  a  descendant  of
--       the abstract type defined in Operations;
--
--  The package defines the type Lexer used as an abstract base. A  type
--  derived   from   Lexer  has  to  implement  the  following  abstract
--  subroutines  defined  in  the  package  Implementation  (which is an
--  instance of Parsers.Generic_Lexer, instantiated in the  public  part
--  of this package) :
--
--       Call        - To an operator, ligature, semicolon
--       Enclose     - Call to a function, index, aggregate, brackets
--       Get_Blank   - Skip blanks and comments in the source
--       Get_Operand - Get an operand
--
--  The following error handlers can be  overridden  if  other  behavior
--  required:
--
--       On_Association_Error raises Syntax_Error
--       On_Missing_Operand raises Syntax_Error
--       On_Missing_Operation raises Syntax_Error
--       On_Missing_Right_Bracket raises Syntax_Error
--       On_Unexpected returns comma or bracket back
--       On_Wrong_Comma raises Syntax_Error
--       On_Wrong_Right_Bracket raises Syntax_Error
--
--  The  discriminants  of the type identify the tables of prefix, infix
--  and postifix operations. When recognized, an  operation  is  matched
--  from  the  corresponding  table.  This  causes  pushing  it onto the
--  operation stack.
--
with Parsers.Generic_Lexer;
with Parsers.Generic_Operation.Generic_Stack;

generic
   type Argument_Stack is new Arguments.Stack with private;
   with package Operations is new Descriptors.Generic_Stack (<>);
   type Operation_Stack is abstract new Operations.Stack with private;
package Parsers.Generic_Token.Generic_Token_Lexer is

   package Implementation is
      new Parsers.Generic_Lexer
          (  Arguments       => Arguments,
             Descriptors     => Descriptors,
             Operations      => Operations,
             Argument_Stack  => Argument_Stack,
             Operation_Stack => Operation_Stack,
             Sources         => Sources
          );
--
-- Lexer -- The base type for user-defined lexers
--
--    Prefixes  - The table of prefix operations
--    Infixes   - The table of infix operations
--    Postfixes - The table of postfix operations
--
   type Lexer
        (  Prefixes  : access Vocabulary.Table'Class;
           Infixes   : access Vocabulary.Table'Class;
           Postfixes : access Vocabulary.Table'Class
        )  is abstract new Implementation.Lexer with private;
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
             (  Context : in out Implementation.Lexer'Class;
                Code    : in out Source_Type;
                Result  : out Argument_Token
             )  renames Implementation.Parse;
--
-- Get_Infix -- Overrides Parsers.Generic_Lexer...
--
   procedure Get_Infix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Implementation.Lexical_Token;
                Got_It  : out Boolean
             );
--
-- Get_Postfix -- Overrides Parsers.Generic_Lexer...
--
   procedure Get_Postfix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Implementation.Lexical_Token;
                Got_It  : out Boolean
             );
--
-- Get_Prefix -- Overrides Parsers.Generic_Lexer...
--
   procedure Get_Prefix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Implementation.Lexical_Token;
                Got_It  : out Boolean
             );
--
-- On_Association_Error -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Association_Error
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token
             );
--
-- On_Missing_Operand -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Missing_Operand
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Argument : out Argument_Token
             );
   procedure On_Missing_Operand
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Operation : Operation_Token;
                Argument  : out Argument_Token
             );
--
-- On_Missing_Operation -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Missing_Operation
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Modifier  : Operation_Token;
                Operation : out Implementation.Lexical_Token;
                Got_It    : out Boolean
             );
--
-- On_Missing_Right_Bracket -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Missing_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : out Operation_Token
             );
--
-- On_Wrong_Comma -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Wrong_Comma
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Comma   : in out Operation_Token
             );
--
-- On_Wrong_Right_Bracket -- Overrides Parsers.Generic_Lexer...
--
   procedure On_Wrong_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token;
                Got_It  : out Boolean
             );
private
--
-- Lexer -- Type completion
--
   type Lexer
        (  Prefixes  : access Vocabulary.Table'Class;
           Infixes   : access Vocabulary.Table'Class;
           Postfixes : access Vocabulary.Table'Class
        )  is abstract new Implementation.Lexer with null record;
end Parsers.Generic_Token.Generic_Token_Lexer;
