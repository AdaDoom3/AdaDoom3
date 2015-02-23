--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Token.                      Luebeck            --
--        Generic_Token_Lexer                      Winter, 2004       --
--  Implementation                                                    --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Parsers.Generic_Token.Generic_Token_Lexer is
   use Implementation;
   use Vocabulary;
--
-- Get_Token -- Get lexic token from the source and a table token
--
--    Code  - The source being parsed
--    Token - The table token recognized there
--
-- Returns :
--
--    The corresponding lexic token
--
   function Get_Token
            (  Code  : Source_Type;
               Token : Table_Token
            )  return Lexical_Token;
   pragma Inline (Get_Token);

   function Get_Token
            (  Code  : Source_Type;
               Token : Table_Token
            )  return Lexical_Token is
   begin
      case Token.Class is
         when Operator =>
            return
            (  Operator,
               (Token.Operation, Sources.Link (Code)),
               Token.Left,
               Token.Right
            );
         when Index =>
            return
            (  Index,
               (Token.Operation, Sources.Link (Code)),
               Token.Priority
            );
         when Comma =>
            return
            (  Comma,
               (Token.Operation, Sources.Link (Code))
            );
         when Ligature =>
            return
            (  Ligature,
               (Token.Operation, Sources.Link (Code))
            );
         when Bracket =>
            return
            (  Bracket,
               (Token.Operation, Sources.Link (Code))
            );
         when Postmodifier =>
            return
            (  Postmodifier,
               (Token.Operation, Sources.Link (Code))
            );
         when Premodifier =>
            return
            (  Premodifier,
               (Token.Operation, Sources.Link (Code))
            );
         when Sublist_Close =>
            return
            (  Sublist_Close,
               (Token.Operation, Sources.Link (Code)),
               Token.Priority
            );
         when Sublist_Open =>
            return
            (  Sublist_Open,
               (Token.Operation, Sources.Link (Code)),
               Token.Priority
            );
         when Sublist_Separator =>
            return
            (  Sublist_Separator,
               (Token.Operation, Sources.Link (Code)),
               Token.Priority
            );
      end case;
   end Get_Token;

   procedure Get_Infix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is
      Line    : Sources.Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      Result  : Table_Token;
   begin
      Sources.Get_Line (Code, Line, Pointer, Last);
      Get
      (  Line (Line'First..Last),
         Pointer,
         Context.Infixes.all,
         Result
      );
      Sources.Set_Pointer (Code, Pointer);
      Token  := Get_Token (Code, Result);
      Got_It := True;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Infix;

   procedure Get_Postfix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is
      Line    : Sources.Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      Result  : Table_Token;
   begin
      Sources.Get_Line (Code, Line, Pointer, Last);
      Get
      (  Line (Line'First..Last),
         Pointer,
         Context.Postfixes.all,
         Result
      );
      Sources.Set_Pointer (Code, Pointer);
      Token  := Get_Token (Code, Result);
      Got_It := True;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Postfix;

   procedure Get_Prefix
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Token   : out Lexical_Token;
                Got_It  : out Boolean
             )  is
      Line    : Sources.Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
      Result  : Table_Token;
   begin
      Sources.Get_Line (Code, Line, Pointer, Last);
      Get
      (  Line (Line'First..Last),
         Pointer,
         Context.Prefixes.all,
         Result
      );
      Sources.Set_Pointer (Code, Pointer);
      Token  := Get_Token (Code, Result);
      Got_It := True;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Prefix;

   procedure On_Association_Error
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Operation at "
         &  Sources.Image (Left.Location)
         &  " cannot be associated with one at "
         &  Sources.Image (Right.Location)
      )  );
   end On_Association_Error;

   procedure On_Missing_Operand
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Argument : out Argument_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Expression is expected at "
         &  Sources.Image (Sources.Link (Code))
      )  );
   end On_Missing_Operand;

   procedure On_Missing_Operand
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Operation : Operation_Token;
                Argument  : out Argument_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         "Operand is expected at " & Sources.Image (Sources.Link (Code))
      );
   end On_Missing_Operand;

   procedure On_Missing_Operation
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Modifier  : Operation_Token;
                Operation : out Lexical_Token;
                Got_It    : out Boolean
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Operation expected at "
         &  Sources.Image (Sources.Link (Code))
         &  " after a reserved word at "
         &  Sources.Image (Modifier.Location)
      )  );
   end On_Missing_Operation;

   procedure On_Missing_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : out Operation_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "The missing right bracket at "
         &  Sources.Image (Sources.Link (Code))
         &  " to match the left one at "
         &  Sources.Image (Left.Location)
      )  );
   end On_Missing_Right_Bracket;

   procedure On_Wrong_Comma
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Comma   : in out Operation_Token
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "The comma at "
         &  Sources.Image (Comma.Location)
         &  " does not match the left bracket at "
         &  Sources.Image (Left.Location)
      )  );
   end On_Wrong_Comma;

   procedure On_Wrong_Right_Bracket
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Left    : in out Operation_Token;
                Right   : in out Operation_Token;
                Got_It  : out Boolean
             )  is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "The right bracket at "
         &  Sources.Image (Right.Location)
         &  " does not match the left one at "
         &  Sources.Image (Left.Location)
      )  );
   end On_Wrong_Right_Bracket;

end Parsers.Generic_Token.Generic_Token_Lexer;
