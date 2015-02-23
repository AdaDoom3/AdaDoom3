--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Token                       Luebeck            --
--  Interface                                      Winter, 2004       --
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
--  This generic package has the following parameters:
--
--  (o)  Argument_Type   the   type  of  the  object  used  to  identify
--       expression arguments;
--  (o)  Operation_Type  identifies  an  operation such as unary, binary
--       operator, bracket etc;
--  (o)  The operation "and" defined on the type Operation_Type  returns
--       true   if   the   argument   are  association  compatible.  See
--       Parser.Generic_Operation;
--  (o)  Is_Commutative  returns  True  if  in  the given pair of binary
--       operators   each   operation  is  either  the  operation  of  a
--       commutative group or an inverse operation  of  the  group.  See
--       Parser.Generic_Operation;
--  (o)  Is_Inverse   is   called   for   binary   operators   on  which
--       Is_Commutative  is  true. It returns true if the operator is an
--       inverse operation of the corresponding commutative  group.  See
--       Parser.Generic_Operation;
--  (o)  Group_Inverse  is  called  for  binary   operators   on   which
--       Is_Commutative is true. It returns the unary inverse  operation
--       of a commutative group. See Parser.Generic_Operation;
--  (o)  Priority_Type  is  the  operation  priority.  Higher   priority
--       operations have higher associativity with the arguments;
--  (o)  Priorities are ordered using "<";
--  (o)  Sources  is an instance of Parsers.Generic_Source, defining the
--       type Source_Type. It also defines Location_Type,  the  type  of
--       the source code links. A source code link refers a code slice.
--
--  The package provides instantiations:
--
--  (o)  Arguments  is  an  instance  of Parsers.Generic_Argument, which
--       defines the abstract base type for the argument stacks;
--  (o)  Descriptors is an instance of Parsers.Generic_Operation,  which
--       serves  as the parent package for instantiations of the generic
--       packages implementing operation stacks ;
--  (o)  Vocabulary is an instance of the package Table providing tables
--       of tokens .
--
with Parsers.Generic_Argument;
with Parsers.Generic_Operation;
with Parsers.Generic_Source;
with Tables;

generic
   type Argument_Type  is private;
   type Operation_Type is private;
   type Priority_Type  is private;
   with package Sources is new Generic_Source (<>);
   with function "and" (Left, Right : Operation_Type)
      return Boolean is <>;
   with function Is_Commutative (Left, Right : Operation_Type)
      return Boolean is <>;
   with function Is_Inverse (Binary_Operator : Operation_Type)
      return Boolean is <>;
   with function Group_Inverse (Binary_Operator : Operation_Type)
      return Operation_Type is <>;
   with function "<" (Left, Right : Priority_Type)
      return Boolean is <>;
package Parsers.Generic_Token is
   use Sources;
--
-- Table_Token -- A lexeme to recognize in the source
--
   type Table_Token (Class : Token_Class := Operator) is record
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
-- Argument_Token -- Identifies appearance of an argument in the source
--
   type Argument_Token is record
      Value    : Argument_Type;
      Location : Location_Type;
   end record;
--
-- Operation_Token -- Identifies  appearance  of  an  operation  in  the
--                    source
--
   type Operation_Token is record
      Operation : Operation_Type;
      Location  : Location_Type;
   end record;
--
-- Is_Commutative -- Commutativity check
--
--    Left  - An operation appearance
--    Right - An operation appearance
--
-- Returns :
--
--    True if the operations belong to a commutative group
--
   function Is_Commutative (Left, Right : Operation_Token)
      return Boolean;
   pragma Inline (Is_Commutative);
--
-- Is_Inverse -- Commutative group inverse operation check
--
--    Binary_Operator - An operator appearance
--
-- Returns :
--
--    True if the operation is a commutative operation inversion
--
   function Is_Inverse (Binary_Operator : Operation_Token)
      return Boolean;
   pragma Inline (Is_Inverse);
--
-- Group_Inverse -- Commutative group inverse operation
--
--    Binary_Operator - An operator appearance
--
-- The operation location will be inherited from Operation.
--
-- Returns :
--
--    The unary inversion operation of the group
--
   function Group_Inverse (Binary_Operator : Operation_Token)
      return Operation_Token;
   pragma Inline (Group_Inverse);
--
-- and -- Check associativity compatibility
--
--    Left  - Left operation appearance
--    Right - The right one
--
-- Returns :
--
--    True if Left can be associated with Right
--
   function "and" (Left, Right : Operation_Token) return Boolean;
   pragma Inline ("and");
--
-- Arguments -- Instantiation of Parsers.Generic_Argument
--
   package Arguments is new Generic_Argument (Argument_Token);
--
-- Descriptors -- Instantiation of Parsers.Generic_Operation
--
   package Descriptors is
      new Generic_Operation (Operation_Token, Priority_Type);
--
-- Vocabulary -- Instantiation of Tables
--
   package Vocabulary is new Tables (Table_Token);
--
-- Add_Operator -- Add operator to a table
--
--    Table    - The table (of prefixes, infixes or postfixes)
--    Name     - Of the operator
--    Operator - The operator
--    Left     - The left association priority
--    Right    - The right association priority
--
-- This  procedure  is  used  to add either a prefix unary operator to a
-- table  of  prefixes  or an infix operator to an table of infixes or a
-- postfix unary operator to a table of postfixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Operator
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Operator : Operation_Type;
                Left     : Priority_Type;
                Right    : Priority_Type
             );
--
-- Add_Bracket -- Add left or right bracket to a table
--
--    Table   - The table (of prefixes or postfixes)
--    Name    - Of the bracket
--    Bracket - The bracket
--
-- This procedure is used to add a left  order  bracket  or  one  of  an
-- aggregate to a table of prefixes. It can also be used to add a  right
-- bracket of any kind to a table of postfixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Bracket
             (  Table   : in out Vocabulary.Table'Class;
                Name    : String;
                Bracket : Operation_Type
             );
--
-- Add_Comma -- Add comma to a table
--
--    Table - The table (of infixes)
--    Name  - Of the comma
--    Comma - The comma
--
-- This procedure is used to add a comma to a table of infixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Comma
             (  Table : in out Vocabulary.Table'Class;
                Name  : String;
                Comma : Operation_Type
             );
--
-- Add_Index -- Add left index bracketto a table
--
--    Table - The table (of infixes)
--    Name  - Of the operator
--    Index - The operator
--    Left  - The left association priority
--
-- This  procedure  is  used  to  add a left index bracket to a table of
-- infixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Index
             (  Table : in out Vocabulary.Table'Class;
                Name  : String;
                Index : Operation_Type;
                Left  : Priority_Type
             );
--
-- Add_Ligature -- Add ligature to a table
--
--    Table    - The table (of infixes)
--    Name     - Of the ligature
--    Ligature - The ligature
--
-- This procedure is used to add a ligature to a table of infixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Ligature
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Ligature : Operation_Type
             );
--
-- Add_Postmodifier -- Add a postmodifier to a table
--
--    Table    - The table (of prefixes or postfixes)
--    Name     - The modifier (a reserved word)
--    Modifier - The operation associated with it
--
-- This  procedure  is  used  to add a postmodifier either to a table of
-- prefixes or to a table of postfixes.
--
-- (o)  When  added to the prefixes table, the modifier causes a call to
--      On_Modifier  each  time  it is recognized. The callee may change
--      the operation it follows.  If  no  operation  precedes  it,  the
--      modifier  causes  parser  to  finish  expression processing just
--      after it calls to On_Missing_Argument.
-- (o)  When  added  to  the  postfixes table, the callee may change the
--      argument it follows.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Postmodifier
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Modifier : Operation_Type
             );
--
-- Add_Premodifier -- Add a premodifier to a table
--
--    Table    - The table (of prefixes or postfixes)
--    Name     - The modifier
--    Modifier - The operation associated with it
--
-- This procedure is used to add a premodifier  either  to  a  table  of
-- prefixes or to a table of postfixes. The premodifier when  recognized
-- causes a call to On_Modifier when the operation following it appears.
-- If  the  latter does not, On_Missing_Operation is called. Note that a
-- premodifier cannot be returned back, thus a dangling  premodifier  is
-- usually  a  severe error if it cannot be ignored. So it is preferable
-- to use postmodifiers whenever possible.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Premodifier
             (  Table    : in out Vocabulary.Table'Class;
                Name     : String;
                Modifier : Operation_Type
             );
--
-- Add_Semicolon -- Add sublist separator
--
--    Table     - The table (of infixes)
--    Name      - Of the semicolon
--    Semicolon - The operation of the sublist item
--    Class     - Of the semicolon
--    Priority  - The association priority
--
-- These procedures are used to add a semicolon to a table of infixes.
--
-- Exceptions :
--
--    Constraint_Error - Incorrectly spelled name
--    Name_Error       - The name is already in the table
--
   procedure Add_Semicolon
             (  Table     : in out Vocabulary.Table'Class;
                Name      : String;
                Semicolon : Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type
             );
--
-- Link -- Reference an argument list
--
--    List - The argument frame
--
-- This function merges the locations of all arguments in List
--
-- Retunrs :
--
--    The source location of the arguments in List
--
   function Link (List : Arguments.Frame) return Location_Type;

end Parsers.Generic_Token;
