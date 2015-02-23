--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Operation                   Luebeck            --
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
--  This package defines the type Descriptor on the basis of the package
--  generic parameters:  
--
--  (o)  Operation_Type identifies appearance of  an  operation  in  the
--       source.  Usually  it  is  an  operation identifier and a source
--       location link;
--  (o)  The operation "and" defined  on  the  type  Operation_Type.  It
--       returns  True  if the argument are association compatible. That
--       is  that  they may share an argument. In Ada "and" and "or" are
--       incompatible. For brackets and commas association compatibility
--       can  be  used  to  distinguish  different kind of brackets. For
--       instance  "(" can be compatible with ")", but not with "]". The
--       operation "and" should be defined transitive, in the sense that
--       if two operations are compatible, then a third operation should
--       be compatible or not with both;
--  (o)  Is_Commutative  returns  True  if  in  the given pair of binary
--       operators   each   operation  is  either  the  operation  of  a
--       commutative  group  or  an inverse operation of the group. Here
--       are the usual examples: are + and - (the inverse).  Commutative
--       operators  are  optimized  by  changing evaluation order of the
--       operands. If no such  optimization  required,  it  is  safe  to
--       define Is_Commutative as false for any pair of operations;   
--  (o)  Is_Inverse   is   called   for   binary   operators   on  which
--       Is_Commutative  is  true. It returns true if the operator is an
--       inverse operation of the corresponding commutative  group.  For
--       example, for + it should be False, for - it should be True;  
--  (o)  Group_Inverse  is  called  for  binary   operators   on   which
--       Is_Commutative is true. It returns the unary inverse  operation
--       of a commutative group. For example for +, - it  should  return
--       unary  minus.  For *, / it should return unary 1/x. It is never
--       called for the groups  of  operations  having  same  Is_Inverse
--       results;   
--  (o)  Priority_Type  is  the  operation  priority.  Higher   priority
--       operations have higher associativity with the arguments;
--  (o)  Priorities are ordered using "<".
--
--  About commutative  operation  optimizations.  A  commutative  binary
--  operation is one which result does not depend of the argument order.
--  For  example  a+b  =  b+a. Because the result does not depend on the
--  order, such operations can be optimized  by  choosing  a  preferable
--  order  among  many  possible.  The  preferable  order,  could be one
--  evaluating  the constants and invariants first. For example: 1+a+2 =
--  (1+2)+a =  3+a.  Optimization  may  take  advantage  of  an  inverse
--  operation  of  a commutative  group: 1+a-4 = 1+a+(-4) = (1+(-4))+a =
--  -3+a.  In  such  cases  Is_Commutative  should  return  True for all
--  combinations  of  + and -. Is_Inverse should be true for - and false
--  for +. Group_Inverse called on + or - should return unary minus.   
--
generic
   type Operation_Type is private;
   type Priority_Type is private;
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
package Parsers.Generic_Operation is
--
-- Descriptor_Class -- Classes of operation descriptors
--
   type Descriptor_Class is
        (Stub, Operator, Default, Sublist, Tuple, Ligature);
--
-- Descriptor -- Operation stack item
--
   type Descriptor (Class : Descriptor_Class := Stub) is record
      case Class is
         when Operator..Ligature => 
            Operation : Operation_Type;
            case Class is
               when Operator..Tuple =>
                  Count : Natural;
                  case Class is
                     when Operator..Sublist =>
                        Right : Priority_Type;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end record;
end Parsers.Generic_Operation;
