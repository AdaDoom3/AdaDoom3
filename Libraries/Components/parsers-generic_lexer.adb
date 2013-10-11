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

package body Parsers.Generic_Lexer is

   procedure Do_Binary
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             )  is
      Explicit : constant Boolean :=
         Get_Pointer (Code) /= Get_Backup_Pointer (Code);
   begin
      begin
         Push_Binary
         (  Container => Context.Operations,
            Operation => Operator,
            Left      => Left,
            Right     => Right,
            Explicit  => Explicit
         );
         Got_It := True;
      exception
         when Association_Error =>
            declare
               Other : Descriptor := Top (Context.Operations);
            begin
               On_Association_Error
               (  Context,
                  Code,
                  Other.Operation,
                  Operator
               );
               Replace (Context.Operations, Other);
               Push_Binary
               (  Container => Context.Operations,
                  Operation => Operator,
                  Left      => Left,
                  Right     => Right,
                  Unchecked => True,
                  Explicit  => Explicit
               );
            end;
            Got_It := True;
         when Unexpected_Operation =>
            On_Unexpected (Context, Code, Operator);
            Got_It := False;
      end;
   end Do_Binary;

   procedure Do_Postfix
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             )  is
   begin
      begin
         Push_Postfix (Context.Operations, Operator, Left, Right);
         Got_It := True;
      exception
         when Association_Error =>
            declare
               Other : Descriptor := Top (Context.Operations);
            begin
               On_Association_Error
               (  Context,
                  Code,
                  Other.Operation,
                  Operator
               );
               Replace (Context.Operations, Other);
               Push_Postfix
               (  Context.Operations,
                  Operator,
                  Left,
                  Right,
                  True
               );
            end;
            Got_It := True;
         when Unexpected_Operation =>
            On_Unexpected (Context, Code, Operator);
            Got_It := False;
      end;
   end Do_Postfix;

   procedure Do_Prefix
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Operator : in out Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type;
                Got_It   : out Boolean
             )  is
   begin
      begin
         Push_Prefix (Context.Operations, Operator, Left, Right);
         Got_It := True;
      exception
         when Association_Error =>
            declare
               Other : Descriptor := Top (Context.Operations);
            begin
               On_Association_Error
               (  Context,
                  Code,
                  Other.Operation,
                  Operator
               );
               Replace (Context.Operations, Other);
               Push_Prefix
               (  Context.Operations,
                  Operator,
                  Left,
                  Right,
                  True
               );
            end;
            Got_It := True;
         when Unexpected_Operation =>
            On_Unexpected (Context, Code, Operator);
            Got_It := False;
      end;
   end Do_Prefix;

   procedure Do_Left_Bracket
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Bracket : in out Operation_Type;
                Left    : Priority_Type
             )  is
   begin
      begin
         Push_Left_Bracket (Context.Operations, Bracket, Left);
      exception
         when Association_Error =>
            declare
               Other : Descriptor := Top (Context.Operations);
            begin
               On_Association_Error
               (  Context,
                  Code,
                  Other.Operation,
                  Bracket
               );
               Replace (Context.Operations, Other);
               Push_Left_Bracket
               (  Context.Operations,
                  Bracket,
                  Left,
                  True
               );
            end;
      end;
   end Do_Left_Bracket;

   procedure Do_Comma
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Comma   : in out Operation_Type;
                Plain   : Boolean;
                Got_It  : out Boolean
             )  is
   begin
      Push_Comma (Context.Operations, Comma, Plain);
      Got_It := True;
   exception
      when Unexpected_Comma =>
         On_Unexpected (Context, Code, Comma);
         Got_It := False;
      when Wrong_Comma_Type =>
         declare
            Other : Descriptor := Top (Context.Operations);
         begin
            On_Wrong_Comma (Context, Code, Other.Operation, Comma);
            Replace (Context.Operations, Other);
            Push_Comma (Context.Operations, Comma, Plain, True);
            Got_It := True;
         end;
   end Do_Comma;

   procedure Do_Operand
             (  Context  : in out Lexer'Class;
                Argument : Argument_Type
             )  is
   begin
      Push (Context.Arguments, Argument);
   end Do_Operand;

   procedure Do_Postmodifier
             (  Context  : in out Lexer'Class;
                Code     : in out Source_Type;
                Argument : Boolean;
                Modifier : in out Operation_Type;
                Got_It   : out Boolean
             )  is
   begin
      if Argument then
         if Is_Empty (Context.Arguments) then
            Got_It := False;
         else
            declare
               Left : Frame (1..1);
            begin
               Pop (Context.Arguments, Left);
               On_Postmodifier
               (  Context,
                  Code,
                  Left (1),
                  Modifier,
                  Got_It
               );
               Push (Context.Arguments, Left (1));
            end;
         end if;
      else
         if Is_Empty (Context.Operations) then
            Got_It := False;
         else
            declare
               Left : Descriptor := Top (Context.Operations);
            begin
               case Left.Class is
                  when Stub =>
                     Got_It := False;
                  when others =>
                     On_Postmodifier
                     (  Context,
                        Code,
                        Left.Operation,
                        Modifier,
                        Got_It
                     );
               end case;
               if Got_It then
                  Replace (Context.Operations, Left);
               end if;
            end;
         end if;
      end if;
   end Do_Postmodifier;

   procedure Do_Right_Bracket
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Bracket : in out Operation_Type;
                Got_It  : out Boolean
             )  is
   begin
      Push_Right_Bracket (Context.Operations, Bracket);
      Got_It := True;
   exception
      when Unexpected_Right_Bracket =>
         On_Unexpected (Context, Code, Bracket);
         Got_It := False;
      when Wrong_Right_Bracket_Type =>
         declare
            Other : Descriptor := Top (Context.Operations);
         begin
            On_Wrong_Right_Bracket
            (  Context,
               Code,
               Other.Operation,
               Bracket,
               Got_It
            );
            if Got_It then
               Replace (Context.Operations, Other);
               Push_Right_Bracket (Context.Operations, Bracket, True);
            end if;
         end;
   end Do_Right_Bracket;

   procedure Do_Sublist
             (  Context   : in out Lexer'Class;
                Code      : in out Source_Type;
                Separator : in out Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type;
                Got_It    : out Boolean
             )  is
   begin
      Push_Semicolon (Context.Operations, Separator, Class, Priority);
      Got_It := True;
   exception
      when Unexpected_Comma =>
         On_Unexpected (Context, Code, Separator);
         Got_It := False;
      when Wrong_Comma_Type =>
         declare
            Other : Descriptor := Top (Context.Operations);
         begin
            On_Wrong_Comma (Context, Code, Other.Operation, Separator);
            Replace (Context.Operations, Other);
            Push_Semicolon
            (  Context.Operations,
               Separator,
               Class,
               Priority,
               True
            );
            Got_It := True;
         end;
   end Do_Sublist;

   procedure On_Postmodifier
             (  Context   : in out Lexer;
                Code      : in out Source_Type;
                Operation : in out Operation_Type;
                Modifier  : Operation_Type;
                Got_It    : out Boolean
             )  is
   begin
      Reset_Pointer (Code);
      Got_It := False;
   end On_Postmodifier;

   procedure On_Postmodifier
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Argument : in out Argument_Type;
                Modifier : Operation_Type;
                Got_It   : out Boolean
             )  is
   begin
      Reset_Pointer (Code);
      Got_It := False;
   end On_Postmodifier;

   procedure On_Premodifier
             (  Context  : in out Lexer;
                Code     : in out Source_Type;
                Token    : in out Lexical_Token;
                Modifier : Operation_Type;
                Got_It   : out Boolean
             )  is
   begin
      Reset_Pointer (Code);
      Got_It := False;
   end On_Premodifier;

   procedure On_Unexpected
             (  Context : in out Lexer;
                Code    : in out Source_Type;
                Right   : Operation_Type
             )  is
   begin
      Reset_Pointer (Code);
   end On_Unexpected;

   procedure Call
             (  Stack     : in out Lexer_Operation_Stack;
                Operation : Operation_Type;
                Count     : Natural
             )  is
      List : Frame (1..Argument_No (Count));
   begin
      Pop (Stack.Context.Arguments, List);
      Push
      (  Stack.Context.Arguments,
         Call (Stack.Context, Operation, List)
      );
    end Call;

   procedure Enclose
             (  Stack : in out Lexer_Operation_Stack;
                Left  : Operation_Type;
                Right : Operation_Type;
                Count : Natural
             )  is
      List : Frame (1..Argument_No (Count));
   begin
      Pop (Stack.Context.Arguments, List);
      Push
      (  Stack.Context.Arguments,
         Enclose (Stack.Context, Left, Right, List)
      );
   end Enclose;

   function Get_Operation_Stack_Depth (Context : Lexer) return Natural is
   begin
      return Get_Depth (Context.Operations);
   end Get_Operation_Stack_Depth;

   function Get_Operation_Stack_Item
            (  Context : Lexer;
               Depth   : Natural := 0
            )  return Descriptor is
   begin
      return Get (Context.Operations, Depth);
   end Get_Operation_Stack_Item;

   function Is_Expected
            (  Stack    : Lexer_Operation_Stack;
               Operator : Operation_Type
            )  return Boolean is
   begin
      return Is_Expected (Stack.Context.all, Operator);
   end Is_Expected;

   function Is_Expected
            (  Context  : Lexer;
               Operator : Operation_Type
            )  return Boolean is
   begin
      return True;
   end Is_Expected;

   procedure Parse
             (  Context : in out Lexer'Class;
                Code    : in out Source_Type;
                Result  : out Argument_Type
             )  is
      Got_Modifier : Boolean := False;
      Got_Argument : Boolean := False;
      Got_It       : Boolean;
      Modifier     : Operation_Type;
      Token        : Lexical_Token;
   begin
      Push_Start (Context.Operations);
      Mark (Context.Arguments);
      Turnover : loop
         --
         -- One turnover: prefixes-operand-postfixes-infix
         --
         loop
            --
            -- Getting a prefix operation
            --
            Get_Blank (Context, Code, Got_It);
            if not Got_It then
               --
               -- Missing operand due to end of source
               --
               Set_Pointer (Code, Get_Pointer (Code));
               case Top (Context.Operations).Class is
                  when Default =>
                     Pop (Context.Operations);
                  when Stub =>
                     On_Missing_Operand (Context, Code, Result);
                     Push (Context.Arguments, Result);
                  when Operator | Sublist | Tuple | Ligature =>
                     On_Missing_Operand
                     (  Context,
                        Code,
                        Top (Context.Operations).Operation,
                        Result
                     );
                     Push (Context.Arguments, Result);
               end case;
               exit Turnover;
            end if;
            Get_Prefix (Context, Code, Token, Got_It);
            if Got_Modifier then
               Got_Modifier := False;
               if Got_It then
                  On_Premodifier
                  (  Context,
                     Code,
                     Token,
                     Modifier,
                     Got_It
                  );
               else
                  On_Missing_Operation
                  (  Context,
                     Code,
                     Modifier,
                     Token,
                     Got_It
                  );
               end if;
            end if;
            exit when not Got_It;
            case Token.Class is
               when Bracket =>
                  Push_Left_Bracket
                  (  Context.Operations,
                     Token.Operation
                  );
                  Got_Argument := False;
               when Operator =>
                  Do_Prefix
                  (  Context,
                     Code,
                     Token.Operation,
                     Token.Left,
                     Token.Right,
                     Got_It
                  );
                  exit when not Got_It;
                  Got_Argument := False;
               when Postmodifier =>
                  Do_Postmodifier
                  (  Context,
                     Code,
                     Got_Argument,
                     Token.Operation,
                     Got_It
                  );
                  if not Got_It then
                     Set_Pointer (Code, Get_Pointer (Code));
                     On_Missing_Operand
                     (  Context,
                        Code,
                        Token.Operation,
                        Result
                     );
                     exit Turnover;
                  end if;
               when Premodifier =>
                  Modifier     := Token.Operation;
                  Got_Modifier := True;
               when others =>
                  raise Constraint_Error;
            end case;
         end loop;
         --
         -- Getting an operand
         --
         Get_Operand (Context, Code, Result, Got_It);
         Got_Argument := True;
         if not Got_It then
            Set_Pointer (Code, Get_Pointer (Code));
            case Top (Context.Operations).Class is
               when Default =>
                  Pop (Context.Operations);
                  exit Turnover;
               when Stub =>
                  On_Missing_Operand (Context, Code, Result);
               when Operator | Sublist | Tuple | Ligature =>
                  On_Missing_Operand
                  (  Context,
                     Code,
                     Top (Context.Operations).Operation,
                     Result
                  );
            end case;
         end if;
         Push (Context.Arguments, Result);
         loop
            --
            -- Getting a postfix operation
            --
            Get_Blank (Context, Code, Got_It);
            exit Turnover when not Got_It;
            Get_Postfix (Context, Code, Token, Got_It);
            if Got_Modifier then
               if Got_It then
                  On_Premodifier
                  (  Context,
                     Code,
                     Token,
                     Modifier,
                     Got_It
                  );
                  Got_Modifier := not Got_It;
               end if;
            end if;
            exit when not Got_It;
            case Token.Class is
               when Bracket =>
                  Do_Right_Bracket
                  (  Context,
                     Code,
                     Token.Operation,
                     Got_It
                  );
                  exit when not Got_It;
                  Got_Argument := False;
               when Operator =>
                  Do_Postfix
                  (  Context,
                     Code,
                     Token.Operation,
                     Token.Left,
                     Token.Right,
                     Got_It
                  );
                  exit when not Got_It;
                  Got_Argument := False;
               when Postmodifier =>
                  Do_Postmodifier
                  (  Context,
                     Code,
                     Got_Argument,
                     Token.Operation,
                     Got_It
                  );
                  exit Turnover when not Got_It;
               when Premodifier =>
                  Modifier     := Token.Operation;
                  Got_Modifier := True;
               when others =>
                  raise Constraint_Error;
            end case;
         end loop;
         --
         -- Getting an infix operation
         --
         Get_Infix (Context, Code, Token, Got_It);
         if Got_Modifier then
            Got_Modifier := False;
            if Got_It then
               On_Premodifier
               (  Context,
                  Code,
                  Token,
                  Modifier,
                  Got_It
               );
            else
               On_Missing_Operation
               (  Context,
                  Code,
                  Modifier,
                  Token,
                  Got_It
               );
            end if;
         end if;
         exit when not Got_It;
         case Token.Class is
            when Comma =>
               Do_Comma
               (  Context,
                  Code,
                  Token.Operation,
                  True,
                  Got_It
               );
               exit when not Got_It;
               Got_Argument := False;
            when Ligature =>
               Do_Comma
               (  Context,
                  Code,
                  Token.Operation,
                  False,
                  Got_It
               );
               exit when not Got_It;
               Got_Argument := False;
            when Operator =>
               Do_Binary
               (  Context,
                  Code,
                  Token.Operation,
                  Token.Left,
                  Token.Right,
                  Got_It
               );
               exit when not Got_It;
               Got_Argument := False;
            when Index =>
               Do_Left_Bracket
               (  Context,
                  Code,
                  Token.Operation,
                  Token.Priority
               );
               Got_Argument := False;
            when Semicolon_Class'Range =>
               Do_Sublist
               (  Context,
                  Code,
                  Token.Operation,
                  Token.Class,
                  Token.Priority,
                  Got_It
               );
               exit when not Got_It;
               Got_Argument := False;
            when Premodifier | Postmodifier =>
               Reset_Pointer (Code);
               exit;
            when others =>
               raise Constraint_Error;
         end case;
      end loop Turnover;
      Set_Pointer (Code, Get_Pointer (Code));
      loop
         begin
            Push_End (Context.Operations);
            exit;
         exception
            when Missing_Right_Bracket =>
               declare
                  Left  : Descriptor := Top (Context.Operations);
                  Right : Operation_Type;
               begin
                  On_Missing_Right_Bracket
                  (  Context,
                     Code,
                     Left.Operation,
                     Right
                  );
                  Replace (Context.Operations, Left);
                  Push_Right_Bracket (Context.Operations, Right);
               end;
         end;
      end loop;
      declare
         List : Frame (1..1);
      begin
         Pop (Context.Arguments, List);
         Result := List (List'First);
      end;
      Release (Context.Arguments);
   exception
      when others =>
         Push_Abort (Context.Operations);
         Release (Context.Arguments);
         raise;
   end Parse;

end Parsers.Generic_Lexer;
