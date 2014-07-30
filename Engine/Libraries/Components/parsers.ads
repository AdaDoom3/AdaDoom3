--                                                                    --
--  package Parsers                 Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
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

package Parsers is
   pragma Pure (Parsers);
--
-- Token_Class -- Lexical tokens
--
-- (o)  Bracket is an order or aggregate bracket; left or right
-- (o)  Comma
-- (o)  Index is a left index or function bracket
-- (o)  Operator is a prefix, postfix or infix one
-- (o)  Ligature (a comma with an attached  binary  operation,  such  as
--      "=>" in Ada)
-- (o)  Sublist_Close is an argument separator that closes a sublist
-- (o)  Sublist_Separator separates sublists of arguments
-- (o)  Sublist_Open is an argument separator that opens a  sublist,  as
--      "with" does in Ada's extension aggregates
-- (o)  Postmodifier of an operation or argument
-- (o)  Premodifier of an operation
--
   type Token_Class is
        (  Operator,
           Bracket,
           Comma,
           Ligature,
           Index,
           Sublist_Close,
           Sublist_Separator,
           Sublist_Open,
           Postmodifier,
           Premodifier
        );
   subtype Semicolon_Class is Token_Class
      range Sublist_Close..Sublist_Open;
   subtype Modifier_Class is Token_Class
      range Postmodifier..Premodifier;
--
-- Syntax_Error -- The exception used by token lexer  (see  the  package
--                 Parsers.Generic_Token.Generic_Lexer). This  exception
-- has  information  attached  containing  the  error  description   and
-- location.
--
   Syntax_Error : exception;
--
-- The  following  exceptions  are  used by the operation stack. The are
-- low-level ones that never not propagate out of a lexer.
--
   Association_Error        : exception;
   Missing_Right_Bracket    : exception;
   Unexpected_Operation     : exception;
   Unexpected_Comma         : exception;
   Unexpected_Right_Bracket : exception;
   Wrong_Comma_Type         : exception;
   Wrong_Right_Bracket_Type : exception;
--
-- Association_Error.  This exception indicates that the operation being
--    pushed onto the stack cannot be associated with  an  operation  on
--    the left, i.e when "and" returns false. The left operation  is  on
--    the stack top. A handler may push a compatible operation instead.
--
-- Missing_Right_Bracket.  The  left  bracket  is  on  the  stack top. A
--    handler may push an assumed right bracket to close it and then try
--    to close the operation stack again.
--
-- Unexpected_Comma |  Unexpected_Right_Bracket.  The  stub  is  on  the
--    operation stack top. A handler has an option to get the unexpected
--    delimiter  back   and   successfully   complete   the   expression
--    evaluation.
--
-- Unexpected_Operation.  The operation is not allowed outside brackets.
--    The stub  is  on  the operation stack top. A handler has an option
--    push another operation instead.
--
-- Wrong_Comma_Type. The right bracket descriptor is  on  the  operation
--    stack top. A handler may push a proper delimiter instead.
--
-- Wrong_Right_Bracket_Type.  The  right  bracket  descriptor  is on the
--    operation stack top. A handler may push a proper right bracket and
--    then either discard the improper one or try to push it once again.
--
end Parsers;
