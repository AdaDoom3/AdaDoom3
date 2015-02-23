--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Lexer                       Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
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
--  This  generic  package  provides  an  abstract  base  type for infix
--  expression lexers. It has the following generic parameters:
--
--  (o)  Arguments is an instance of Parsers.Generic_Argument  providing
--       an implementation of argument stacks;
--  (o)  Argument_Stack is the argument stack type. It is  a  descendant
--       of the base type defined in the package Arguments;
--  (o)  Descriptors  is  the  parent  of an implementation of operation
--       stacks. It is an instance of Parsers.Generic_Operation;
--  (o)  Operations is an implementation of operation stacks. It  is  an
--       instance of Parsers.Generic_Operation.Generic_Stack;
--  (o)  Operation_Stack is the operation stack type,  a  descendant  of
--       the abstract type defined in Operations;
--  (o)  Sources  is an instance of Parsers.Generic_Source, defining the
--       type Source_Type.
--
--  The package defines the abstract type Lexer. A derived type  has  to
--  implement the following abstract subroutines:
--
--       Call        - To an operator
--       Enclose     - Call to a function, index, aggregate, brackets
--       Get_Blank   - Skip blanks and comments in the source
--       Get_Infix   - Get an infix token (infix operator, comma etc)
--       Get_Operand - Get an operand
--       Get_Postfix - Get a postfix operator, right bracket etc
--       Get_Prefix  - Get a prefix operator, left bracket etc
--
--  and the most of the following error handlers,  also  implemented  as
--  abstract subroutines:
--
--       On_Association_Error
--       On_Missing_Operand
--       On_Missing_Operation
--       On_Missing_Right_Bracket
--       On_Modifier   (non-abstract, but can be overridden)
--       On_Unexpected (non-abstract, but can be overridden)
--       On_Wrong_Comma
--       On_Wrong_Right_Bracket
--
with Ada.Finalization;
with Parsers.Generic_Argument;
with Parsers.Generic_Operation.Generic_Stack;
with Parsers.Generic_Source;

generic
   with package Arguments is new Generic_Argument (<>);
   type Argument_Stack is new Arguments.Stack with private;
   with package Descriptors is new Generic_Operation (<>);
   with package Operations is new Descriptors.Generic_Stack (<>);
   type Operation_Stack is abstract new Operations.Stack with private;
   with package Sources is new Generic_Source (<>);
package Parsers.Generic_Lexer is
   use Arguments;
   use Sources;
   use Descriptors;
   use Operations;
--
-- Lexical_Token -- A lexeme recognized in the source
--
   type Lexical_Token (Class : Token_Class := Operator) is record
      Operation : Operation_Type;
      case Class is
         when Operator =>
            Left  : Priority_Type;
            Right : Priority_Type;
         when Index | Semicolon_Class'Range =>
            Priority : Priority_Type;
         when Bracket | Comma | Ligature | Modifier_Class'Range =>
            null;
      end case;
   end record;
--
-- Lexer -- The base type for user-defined lexers
--
   type Lexer is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Call -- To an operator, ligature or semicolon
--
--    Context   - The parsing context
--    Operation - To call to
--    List      - The parameter list
--
-- The  implementation  should  evaluate  Operation  on  the  arguments.
-- Operator is either a prefix, infix postfix one or a ligature.
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   function Call
            (  Context   : access Lexer;
               Operation : Operation_Type;
               List      : Frame
            )  return Argument_Type is abstract;
--
-- Enclose -- Call to a function, index operation or brackets
--
--    Context - The parsing context
--    Left    - The operation associated with the left bracket
--    Right   - The one associated with the right bracket
--    List    - The parameter list
--
-- The  implementation  should  evaluate the brackets Left, Right on the
-- arguments specified by List. The brackets could be of any type:
--
-- (o)  Order and aggregate brackets. In this case List is the  list  of
--      arguments put in the brackets.
-- (o)  Index  brackets.  In  this case the first element of List is the
--      indexed name, the rest of list is the indices.
-- (o)  Function brackets. In this case the first element of List is the
--      function name, the rest of list is the indices.
--
-- Retunrs :
--
--    The result
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   function Enclose
            (  Context : access Lexer;
               Left    : Operation_Type;
               Right   : Operation_Type;
               List    : Frame
            )  return Argument_Type is abstract;
--
-- Get_Blank -- Skip the blanks and comments in the source
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Got_It  - End of expression flag
--
-- An  implementation should skip everything till the next valid lexeme.
-- The parameter Got_It is set to  False  when  the  end  of  expression
-- reached. This could be the end of file or a reserved keyword.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure Get_Blank
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Got_It  : out Boolean
             )  is abstract;
--
-- Get_Infix -- Get an infix token in the source
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Token   - The result
--    Got_It  - True when successful
--
-- An implementation should recognize a valid infix token and skip it in
-- the source. Got_It indicates success. If Got_It is  True  then  Token
-- should contain a valid infix token. That is either of:
--
-- (o)  Binary operators    (Token.Class = Operator)
-- (o)  Left index brackets (Token.Class = Index)
-- (o)  Commas              (Token.Class = Comma)
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure Get_Infix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is abstract;
--
-- Get_Operand -- Get an operand in the source
--
--    Context  - The parsing context
--    Code     - The source being parsed
--    Argument - The result
--    Got_It   - True when successful
--
-- An implementation should recognize a valid operand token and skip  it
-- in  the  source.  Got_It  indicates  success.  If Got_It is True then
-- Argument should contain the operand such as literal, name etc.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure Get_Operand
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Argument : out Argument_Type;
                Got_It   : out Boolean
             )  is abstract;
--
-- Get_Postfix -- Get a postfix token in the source
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Token   - The result
--    Got_It  - True when successful
--
-- An implementation should recognize a valid postfix token and skip  it
-- in the source. Got_It indicates success. If Got_It is True then Token
-- should contain a valid postfix token. That is either of:
--
-- (o)  Postfix operators (Token.Class = Operator)
-- (o)  Right brackets    (Token.Class = Bracket)
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure Get_Postfix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is abstract;
--
-- Get_Prefix -- Get a prefix token in the source
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Token   - The result
--    Got_It  - True when successful
--
-- An  implementation  should recognize a valid prefix token and skip it
-- in the source. Got_It indicates success. If Got_It is True then Token
-- should contain a valid prefix token. That is either of:
--
-- (o)  Prefix operators (Token.Class = Operator)
-- (o)  Left brackets    (Token.Class = Bracket)
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure Get_Prefix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is abstract;
--
-- Is_Expected -- Check if an operation is allowed ouside brackets
--
--    Context  - The parsing context
--    Operator - The operator
--
-- The default implementation returns True. It can be overridden to make
-- certain  operators   disallowed  outside  brackets. In that case this
-- function should return False, which causes a call  to  On_Unexpected,
-- which would decide what to do about it.   Note  that  operators  with
-- higher   association   priorities  can  be  disallowed  only  if  the
-- operators of  lower priorities  are  disallowed  as  well  (or  their
-- association  is  made  illegal).  This  is  because  a lower priority
-- operator  may  protect  the  higher  priority  operation  from  being
-- detected outside the brackets, e.g. in A+B*C.
--
-- Exceptions :
--
--    True if the operator is allowed
--
   function Is_Expected
            (  Context  : Lexer;
               Operator : Operation_Type
            )  return Boolean;
--
-- On_Association_Error -- Handler
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Left    - The first operation
--    Right   - The second operation
--
-- This procedure is called when two operators sharing or associated  in
-- an argument are incompatible. For example, in Ada, the expression:
--
--    X and Y or Z
--
-- is illegal because "and" cannot be associated with "or". The  handler
-- may  modify  any  of  the  parameters  Left  and  Right  to make them
-- compatible.  Alternatively it may raise an exception which would then
-- abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Association_Error
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Type;
                Right   : in out Operation_Type
             )  is abstract;
--
-- On_Missing_Operand -- Handler
--
--    Context     - The parsing context
--    Code        - The source being parsed
--  [ Operation ] - The operation expecting an argument
--    Argument    - The default
--
-- This  procedure is called when an operand is expected. That is either
-- when no expression was recognized or  after  an  infix  operation  or
-- comma. It can return the default operand into the parameter Argument.
-- Alternatively  it  may  raise  an  exception  which  would then abort
-- parsing. The parameter Operation is optional. It is passed when there
-- is an operation expecting the argument on the stack top.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Missing_Operand
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Argument  : out Argument_Type
             )  is abstract;
   procedure On_Missing_Operand
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Operation : Operation_Type;
                Argument  : out Argument_Type
             )  is abstract;
--
-- On_Missing_Operation -- Handler
--
--    Context  - The parsing context
--    Code     - The source being parsed
--    Modifier - The modifier the operation is expected after
--    Token    - The operation token to assume
--    Got_It   - True when Token is set
--
-- This procedure is called when an operation expected after a  modifier
-- was not found there. The handler may ignore the modifier and continue
-- parsing  the  expression  as  if  there  where no modifier by setting
-- Got_It  to  false. It may simulate an operation by setting it to true
-- and  placing  the  operation  token  into Token. Alternatively it may
-- raise an exception which would then abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Missing_Operation
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Modifier : Operation_Type;
                Token    : out Lexical_Token;
                Got_It   : out Boolean
             )  is abstract;
--
-- On_Missing_Right_Bracket -- Handler
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Left    - The left bracket
--    Right   - The right bracket (output)
--
-- This procedure is called when the  lexer  detects  an  unclosed  left
-- bracket by finishing expression  parsing.  It  can  modify  the  left
-- bracket and should specify  a  suggested  right  one  (the  parameter
-- Right).  Alternatively  it  may  raise  an exception which would then
-- abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Missing_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Type;
                Right   : out Operation_Type
             )  is abstract;
--
-- On_Postmodifier -- Handler
--
--    Context              - The parsing context
--    Code                 - The source being parsed
--    Operation / Argument - To modify
--    Modifier             - The modifier operation
--    Got_It               - True when accepted
--
-- These  procedures  are called to process postmodifiers. The parameter
-- Modifier   is   the   operation   associated  with  the  modifier.  A
-- postmodifier  can be applied to either an operation or an argument it
-- follows. The parameter Operation / Argument refers to  the  thing  to
-- modify.  The  procedure  may  observe it and change it. The parameter
-- Got_It  is set to true to indicate that the modifier was successfully
-- processed. It is set to false to finish parsing. In  which  case  the
-- modifier is usually returned back by the procedure. Note that setting
-- Got_It  to false when handling an operation modifier may cause a call
-- to  the  On_Missing_Operand handler if it is not a postfix operation.
-- For an  argument  modifier  it  will  finish  parsing  without  that.
-- On_Postodifier may raise an exception which would then abort parsing.
-- The  default implementation returns the modifier back and sets Got_It
-- to false.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Postmodifier
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Operation : in out Operation_Type;
                Modifier  : Operation_Type;
                Got_It    : out Boolean
             );
   procedure On_Postmodifier
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Argument : in out Argument_Type;
                Modifier : Operation_Type;
                Got_It   : out Boolean
             );
--
-- On_Postmodifier -- Modifier handler
--
--    Context  - The parsing context
--    Code     - The source being parsed
--    Token    - To modify
--    Modifier - The modifier operation
--    Got_It   - True when accepted
--
-- This  procedure  is  called  to  process a premodifier. The parameter
-- Modifier is the operation associated with the modifier. A premodifier
-- is  applied  to the operation it precedes. The operation is specified
-- by the parameter Token. The handler sets Got_It to true  to  indicate
-- that Token was modified as necessary. When Got_It  is  set  to  false
-- then the operation is discarded and parsing proceeds  either  to  the
-- next  context  allowing  modifier  to  be  applied to an operation of
-- another class or by discarding the modifier. For a postfix operation,
-- an  attempt  made  to  apply  the  modifier to an infix one which may
-- follow. In case of a prefix  operation  the  modifier  is  discarded,
-- which has the effect of ignoring it. For an infix  operation,  it  is
-- discarded and  parsing  is  finished.  On_Premodifier  may  raise  an
-- exception which would then abort parsing. The default  implementation
-- returns the modifier back and sets Got_It to false.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Premodifier
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Token    : in out Lexical_Token;
                Modifier : Operation_Type;
                Got_It   : out Boolean
             );
--
-- On_Unexpected -- Handler
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Right   - The unexpected delimiter
--
-- This procedure is called when the lexer meets  an  unexpected  comma,
-- semicolon,  ligature  or  right  bracket.  The default implementation
-- returns the unexpected delimiter back and then tries to complete  the
-- expression evaluation. An override may raise an exception which would
-- so abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Unexpected
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Right   : Operation_Type
             );
--
-- On_Wrong_Comma -- Handler
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Left    - The left bracket
--    Comma   - The comma, ligature, semicolon
--
-- This procedure is called when the  lexer  finds  incompatible  comma,
-- ligature or semicolon. It can modify the left bracket, the  comma  or
-- both to make them compatible. For this it can modify  the  parameters
-- Left, Comma. Alternatively it may raise an exception which would then
-- abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Wrong_Comma
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Type;
                Comma   : in out Operation_Type
             )  is abstract;
--
-- On_Wrong_Right_Bracket -- Handler
--
--    Context - The parsing context
--    Code    - The source being parsed
--    Left    - The left bracket
--    Right   - The right bracket
--    Got_It  - The bracket was consumed
--
-- This  procedure is called when the lexer finds incompatible brackets.
-- It can modify any of the brackets to make their types compatible. For
-- this it can modify the parameters Left, Right and set Got_It to True.
-- When Got_It is set to False, and return  the  bracket  resetting  the
-- pointer  of  the  source.  That will make the parser to switch to the
-- infix context at which the token might be reinterpreted as  something
-- else, e.g. as an infix operation.  Alternatively  the  procedure  may
-- raise an exception which would then abort parsing.
--
-- Exceptions :
--
--    Any - Aborts parsing
--
   procedure On_Wrong_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Type;
                Right   : in out Operation_Type;
                Got_It  : out Boolean
             )  is abstract;
--
-- Parse -- Expression in the source
--
--    Context - The parsing context
--    Code    - The source to parse
--    Result  - The expression result
--
-- Upon successful completion Result is one of the expression. The  Code
-- cursor  indicates  how  far  the  expression  parsing  gone. Parse is
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
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Result  : out Argument_Type
             );
------------------------------------------------------------------------
-- The following operations are called upon parsing as the corresponding
-- lexemes are recognized. They should not be called directly.
--
-- Do_{Binary|Postfix|Prefix} -- Process an operator
--
--    Context  - The context
--    Code     - The source code being parsed
--    Operator - The operator
--    Left     - Priority of the operator
--    Right    - Priority of the operator
--    Got_It   - True if accepted, otherwise it has to be returned back
--
-- On  an  association  error  a corresponding handler is called and one
-- more attempt is made.
--
   procedure Do_Binary
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             );
   procedure Do_Postfix
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             );
   procedure Do_Prefix
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             );
--
-- Do_Comma -- Process a comma
--
--    Context - The context
--    Code    - The source code being parsed
--    Comma   - The comma operation
--    Plain   - True - comma, False - ligature
--    Got_It  - True if accepted, otherwise it has to be returned back
--
-- On  an  association  error  a corresponding handler is called and one
-- more attempt is made.
--
   procedure Do_Comma
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Comma   : in out Operation_Type;
                Plain   : Boolean;
                Got_It  : out Boolean
             );
--
-- Do_Left_Bracket -- Process a left index bracket
--
--    Context - The context
--    Code    - The source code being parsed
--    Bracket - The bracket
--    Left    - The left priority
--
-- On  an  association  error  a corresponding handler is called and one
-- more attempt is made.
--
   procedure Do_Left_Bracket
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Bracket : in out Operation_Type;
                Left    : Priority_Type
             );
--
-- Do_Operand -- Process an operand
--
--    Context  - The context
--    Argument - The source code being parsed
--
-- This procedure pushes an operand onto the argument stack
--
   procedure Do_Operand
             (  Context  : in out Lexer'Class;
                Argument : Argument_Type
             );
--
-- Do_Postmodifier -- Process a postmodifier
--
--    Context  - The context
--    Code     - The source code being parsed
--    Argument - True if modifies an argument
--    Modifier - The modifier
--    Got_It   - Set to false if modifier is discarded
--
   procedure Do_Postmodifier
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Argument : Boolean;
                Modifier : in out Operation_Type;
                Got_It   : out Boolean
             );
--
-- Do_Right_Bracket -- Process an right bracket
--
--    Context - The context
--    Code    - The source code being parsed
--    Bracket - The bracket
--    Got_It  - Set to false if bracket is discarded
--
-- On an error a corresponding handler is called and one more attempt is
-- made. Also a handler is called when the bracket is unexpected.
--
   procedure Do_Right_Bracket
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Bracket : in out Operation_Type;
                Got_It  : out Boolean
             );
--
-- Do_Sublist -- Process a sublist separator
--
--    Context   - The context
--    Code      - The source code being parsed
--    Separator - The separator
--    Class     - The class of the separator
--    Priority  - The association priority
--    Got_It    - True if accepted, otherwise it has to be returned back
--
-- On  an  association  error  a corresponding handler is called and one
-- more attempt is made.
--
   procedure Do_Sublist
             (  Context   : in out Lexer'Class;
                Code      : in out Source_Type;
                Separator : in out Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type;
                Got_It    : out Boolean
             );
------------------------------------------------------------------------
-- Informational operations
--
-- Get_Operation_Stack_Depth -- The number of operation on the stack
--
--    Context - The parsing context
--
-- Note that the context may keep several parsing sessions separated  by
-- stubs. The result of this operation is the total depth of the  stack,
-- which includes all sessions.
--
-- Returns :
--
--    The number of operations on the stack including stubs
--
   function Get_Operation_Stack_Depth (Context : Lexer) return Natural;
--
-- Get_Operation_Stack_Item
--
--    Context - The parsing context
--    Depth   - Of the operation to query, 0 corresponds to the top
--
-- Returns :
--
--    The operation
--
-- Exceptions :
--
--    Constraint_Error - Depth >= Get_Operation_Stack_Size
--
   function Get_Operation_Stack_Item
            (  Context : Lexer;
               Depth   : Natural := 0
            )  return Descriptor;

private
   pragma Inline (Do_Binary, Do_Postfix, Do_Prefix);
   pragma Inline (Do_Comma);
   pragma Inline (Do_Left_Bracket);
   pragma Inline (Do_Operand);
   pragma Inline (Do_Postmodifier);
   pragma Inline (Do_Right_Bracket);
   pragma Inline (Do_Sublist);
   pragma Inline (Get_Operation_Stack_Depth);
   pragma Inline (Get_Operation_Stack_Item);
   pragma Inline (Is_Expected);

   type Lexer_Operation_Stack (Context : access Lexer'Class) is
      new Operation_Stack with null record;
--
-- Call -- Overrides Parsers.Generic_Operation.Stack...
--
   procedure Call
             (  Stack     : in out Lexer_Operation_Stack;
                Operation : Operation_Type;
                Count     : Natural
             );
--
-- Enclose -- Overrides Parsers.Generic_Operation.Stack...
--
   procedure Enclose
             (  Stack   : in out Lexer_Operation_Stack;
                Left    : Operation_Type;
                Right   : Operation_Type;
                Count   : Natural
             );
--
-- Is_Expected -- Overrides Parsers.Generic_Operation.Stack...
--
   function Is_Expected
            (  Stack    : Lexer_Operation_Stack;
               Operator : Operation_Type
            )  return Boolean;
--
-- Lexer -- Type completion
--
--    Arguments  - The argument stack
--    Operations - The operation stack
--
   type Lexer is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Arguments  : Argument_Stack;
      Operations : Lexer_Operation_Stack (Lexer'Unchecked_Access);
   end record;

end Parsers.Generic_Lexer;
