--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Token                       Luebeck            --
--  Implementation                                 Winter, 2004       --
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

package body Parsers.Generic_Token is
   use Arguments;
   use Vocabulary;

   function Is_Commutative (Left, Right : Operation_Token)
      return Boolean is
   begin
      return Is_Commutative (Left.Operation, Right.Operation);
   end Is_Commutative;

   function Is_Inverse (Binary_Operator : Operation_Token)
      return Boolean is
   begin
      return Is_Inverse (Binary_Operator.Operation);
   end Is_Inverse;

   function Group_Inverse (Binary_Operator : Operation_Token)
      return Operation_Token is
   begin
      return
      (  Group_Inverse (Binary_Operator.Operation),
         Binary_Operator.Location
      );
   end Group_Inverse;

   function "and" (Left, Right : Operation_Token) return Boolean is
   begin
      return Left.Operation and Right.Operation;
   end "and";

   procedure Add_Operator
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Operator : Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Operator, Operator, Left, Right));
   end Add_Operator;

   procedure Add_Bracket
             (  Table   : in out Vocabulary.Table'Class;
                Name    : String;
                Bracket : Operation_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Bracket, Bracket));
   end Add_Bracket;

   procedure Add_Comma
             (  Table : in out Vocabulary.Table'Class;
                Name  : String;
                Comma : Operation_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Comma, Comma));
   end Add_Comma;

   procedure Add_Index
             (  Table : in out Vocabulary.Table'Class;
                Name  : String;
                Index : Operation_Type;
                Left  : Priority_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Index, Index, Left));
   end Add_Index;

   procedure Add_Postmodifier
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Modifier : Operation_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Postmodifier, Modifier));
   end Add_Postmodifier;

   procedure Add_Premodifier
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Modifier : Operation_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Premodifier, Modifier));
   end Add_Premodifier;

   procedure Add_Ligature
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Ligature : Operation_Type
             )  is
   begin
      Add (Table, Name, (Parsers.Ligature, Ligature));
   end Add_Ligature;

   procedure Add_Semicolon
             (  Table     : in out Vocabulary.Table'Class;
                Name      : String;
                Semicolon : Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type
             )  is
   begin
      case Class is
         when Sublist_Close =>
            Add (Table, Name, (Sublist_Close, Semicolon, Priority));
         when Sublist_Separator =>
            Add (Table, Name, (Sublist_Separator, Semicolon, Priority));
         when Sublist_Open =>
            Add (Table, Name, (Sublist_Open, Semicolon, Priority));
      end case;
   end Add_Semicolon;

   function Link (List : Arguments.Frame) return Location_Type is
      Result : Location_Type := List (List'First).Location;
   begin
      for Index in List'First + 1..List'Last loop
         Result := Result & List (Index).Location;
      end loop;
      return Result;
   end Link;

end Parsers.Generic_Token;
