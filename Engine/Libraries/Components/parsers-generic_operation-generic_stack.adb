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

package body Parsers.Generic_Operation.Generic_Stack is
--
-- Clean_Up -- Clean the stack up on a fatal error
--
--    Container - The opration stack
--
-- The  stack  items  are  popped  up to a stub or empty stack. The stub
-- remains on the stack.
--
   procedure Clean_Up (Container : in out Descriptor_Stack) is
   begin
      while (  not Is_Empty (Container)
            and then
               Top (Container).Class /= Stub
            )
      loop
         Pop (Container);
      end loop;
   end Clean_Up;

   function Get (Container : Stack; Depth : Natural)
      return Descriptor is
   begin
      return Get
             (  Container.Raw,
                Index_Type'Val
                (  Index_Type'Pos (Mark (Container.Raw))
                -  Depth
             )  );
   end Get;

   function Get_Depth (Container : Stack) return Natural is
   begin
      return
      (  Index_Type'Pos (Mark (Container.Raw))
      -  Index_Type'Pos (Index_Type'First)
      );
   end Get_Depth;

   function Is_Expected
            (  Container : Stack;
               Operator  : Operation_Type
            )  return Boolean is
   begin
      return True;
   end Is_Expected;

   function Is_Empty (Container : Stack) return Boolean is
   begin
      return Is_Empty (Container.Raw);
   end Is_Empty;
--
-- Unload -- The stack with an operation
--
--    Container - The operation stack
--    Operation - The new operation
--    Left      - Its left association priority
--    Unchecked - No association checks
--
-- Exceptions :
--
--    Association_Error - Incompatible operator on the left
--
   procedure Unload
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Unchecked : Boolean
             )  is
      This : Descriptor;
   begin
      loop
         This := Top (Container);
         case This.Class is
            when Operator | Default =>
               if Unchecked or else (This.Operation and Operation) then
                  if This.Right < Left then
                     return;
                  end if;
                  Pop (Container.Raw);
                  Call (Container, This.Operation, This.Count);
               else
                  raise Association_Error;
               end if;
            when Tuple | Sublist | Ligature =>
               if Unchecked or else (This.Operation and Operation) then
                  return;
               else
                  raise Association_Error;
               end if;
            when Stub =>
               if Unchecked or else Is_Expected (Container, Operation)
               then
                  return;
               else
                  raise Unexpected_Operation;
               end if;
         end case;
      end loop;
   exception
      when Association_Error | Unexpected_Operation =>
         raise;
      when others =>
         Clean_Up (Container.Raw);
         raise;
   end Unload;
--
-- Unload -- The stack with a ligature
--
--    Container - The operation stack
--    This      - The stack top item after unloading
--    Operation - The new operation
--
-- Exceptions :
--
--    Association_Error - Incompatible operator on the left
--
   procedure Unload
             (  Container : in out Stack'Class;
                This      : in out Descriptor;
                Operation : Operation_Type
             )  is
   begin
      loop
         This := Top (Container);
         case This.Class is
            when Operator | Default =>
               if This.Operation and Operation then
                  Pop (Container.Raw);
                  Call (Container, This.Operation, This.Count);
               else
                  raise Association_Error;
               end if;
            when Ligature =>
               if This.Operation and Operation then
                  Pop (Container.Raw);
                  Call (Container, This.Operation, 2);
               else
                  raise Association_Error;
               end if;
            when Stub =>
               if Is_Expected (Container, Operation) then
                  return;
               else
                  raise Unexpected_Operation;
               end if;
            when Tuple | Sublist =>
               if This.Operation and Operation then
                  return;
               else
                  raise Association_Error;
               end if;
         end case;
      end loop;
   exception
      when Association_Error | Unexpected_Operation =>
         raise;
      when others =>
         Clean_Up (Container.Raw);
         raise;
   end Unload;
--
-- Unload -- The stack with a delimiter
--
--    Container - The operation stack
--    This      - The stack top item after unloading
--
   procedure Unload
             (  Container : in out Stack'Class;
                This      : in out Descriptor
             )  is
   begin
      loop
         This := Top (Container);
         case This.Class is
            when Operator | Default =>
               Pop (Container.Raw);
               Call (Container, This.Operation, This.Count);
            when Ligature =>
               Pop (Container.Raw);
               Call (Container, This.Operation, 2);
            when Tuple | Stub | Sublist =>
               return;
         end case;
      end loop;
   exception
      when others =>
         Clean_Up (Container.Raw);
         raise;
   end Unload;

   procedure Push_Abort (Container : in out Stack'Class) is
   begin
      Clean_Up (Container.Raw);
      Pop (Container.Raw);
   end Push_Abort;

   procedure Push_Binary
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False;
                Explicit  : Boolean := True
             )  is
      This : Descriptor;
   begin
      loop
         This := Top (Container);
         case This.Class is
            when Operator | Default =>
               if Unchecked or else (This.Operation and Operation) then
                  if Is_Commutative (This.Operation, Operation) then
                     This.Count := This.Count + 1;
                     Put (Container.Raw, Mark (Container.Raw), This);
                     if (  Is_Inverse (This.Operation)
                        xor
                           Is_Inverse (Operation)
                        )
                     then
                        declare
                           Inverse : Operation_Type :=
                              Group_Inverse (Operation);
                        begin
                           Push
                           (  Container.Raw,
                              (Operator, Inverse, 1, Right)
                           );
                        end;
                     end if;
                     return;
                  else
                     exit when This.Right < Left;
                  end if;
                  Pop (Container.Raw);
                  Call (Container, This.Operation, This.Count);
               else
                  raise Association_Error;
               end if;
            when Ligature | Tuple | Sublist =>
               if Unchecked or else (This.Operation and Operation) then
                  exit;
               else
                  raise Association_Error;
               end if;
            when Stub =>
               if Unchecked or else Is_Expected (Container, Operation)
               then
                  exit;
               else
                  raise Unexpected_Operation;
               end if;
         end case;
      end loop;
      if Explicit then
         Push (Container.Raw, (Operator, Operation, 2, Right));
      else
         Push (Container.Raw, (Default, Operation, 2, Right));
      end if;
   exception
      when Association_Error | Unexpected_Operation =>
         raise;
      when others =>
         Clean_Up (Container.Raw);
         raise;
   end Push_Binary;

   procedure Push_Comma
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Comma     : Boolean;
                Unchecked : Boolean := False
             )  is
      This : Descriptor;
   begin
      if Comma then
         Unload (Container, This);
         case This.Class is
            when Sublist | Tuple =>
               if Unchecked or else (This.Operation and Operation) then
                  Pop (Container.Raw);
                  This.Count := This.Count + 1;
                  Push (Container.Raw, This);
                  return;
               end if;
            when others =>
               raise Unexpected_Comma;
         end case;
      else
         Unload (Container, This, Operation);
         case This.Class is
            when Sublist | Tuple =>
               if Unchecked or else (This.Operation and Operation) then
                  Push (Container.Raw, (Ligature, Operation));
                  return;
               end if;
            when others =>
               raise Unexpected_Comma;
         end case;
      end if;
      raise Wrong_Comma_Type;
   exception
      when Association_Error =>
         raise Wrong_Comma_Type;
   end Push_Comma;

   procedure Push_End (Container : in out Stack'Class) is
      This : Descriptor;
   begin
      Unload (Container, This);
      if This.Class = Stub then
         Pop (Container.Raw);
         return;
      else
         raise Missing_Right_Bracket;
      end if;
   end Push_End;

   procedure Push_Left_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Unchecked : Boolean := False
             )  is
   begin
      Unload (Container, Operation, Left, Unchecked);
      Push (Container.Raw, (Tuple, Operation, 1));
   end Push_Left_Bracket;

   procedure Push_Left_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type
             )  is
   begin
      Push (Container.Raw, (Tuple, Operation, 0));
   end Push_Left_Bracket;

   procedure Push_Right_Bracket
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Unchecked : Boolean := False
             )  is
      This : Descriptor;
   begin
      loop
         Unload (Container, This);
         case This.Class is
            when Tuple | Sublist =>
               if Unchecked or else (This.Operation and Operation) then
                  Enclose
                  (  Container,
                     This.Operation,
                     Operation,
                     This.Count + 1
                  );
                  Pop (Container.Raw);
                  exit when This.Class = Tuple;
               else
                  raise Wrong_Right_Bracket_Type;
               end if;
            when others =>
               raise Unexpected_Right_Bracket;
         end case;
      end loop;
   end Push_Right_Bracket;

   procedure Push_Start (Container : in out Stack'Class) is
   begin
      Push (Container.Raw, (Class => Stub));
   end Push_Start;

   procedure Push_Prefix
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False
             )  is
      Index : Index_Type := Mark (Container.Raw);
      This  : Descriptor := Top (Container.Raw);
   begin
       case This.Class is
         when Operator | Default | Ligature | Tuple | Sublist =>
            if not (Unchecked or else (This.Operation and Operation))
            then
               raise Association_Error;
            end if;
         when Stub =>
            if not Unchecked and then
               not Is_Expected (Container, Operation)
            then
               raise Unexpected_Operation;
            end if;
      end case;
      loop
         case This.Class is
            when Operator | Default =>
               exit when This.Right < Left;
            when Ligature | Stub | Sublist | Tuple=>
               exit;
         end case;
         Index := Index_Type'Pred (Index);
         This  := Get (Container.Raw, Index);
      end loop;
      if Index = Mark (Container.Raw) then
         Push (Container.Raw, (Operator, Operation, 1, Right));
      else
         Index := Index_Type'Succ (Index);
         Push (Container.Raw, This);
         for Destination in reverse
            Index_Type'Succ (Index)..Mark (Container.Raw)
         loop
            Put
            (  Container.Raw,
               Destination,
               Get (Container.Raw, Index_Type'Pred (Destination))
            );
         end loop;
         Put (Container.Raw, Index, (Operator, Operation, 1, Right));
      end if;
   exception
      when Association_Error | Unexpected_Operation =>
         raise;
      when others =>
         Clean_Up (Container.Raw);
         raise;
   end Push_Prefix;

   procedure Push_Postfix
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Left      : Priority_Type;
                Right     : Priority_Type;
                Unchecked : Boolean := False
             )  is
   begin
      Unload (Container, Operation, Left, Unchecked);
      Push (Container.Raw, (Operator, Operation, 1, Right));
   end Push_Postfix;

   procedure Push_Semicolon
             (  Container : in out Stack'Class;
                Operation : Operation_Type;
                Class     : Semicolon_Class;
                Priority  : Priority_Type;
                Unchecked : Boolean := False
             )  is
      This  : Descriptor;
      Count : Natural;
      Left  : Operation_Type;
   begin
      case Class is
         when Sublist_Open =>
            Push_Comma (Container, Operation, True, Unchecked);
            Push (Container.Raw, (Sublist, Operation, 0, Priority));
         when Sublist_Separator | Sublist_Close =>
            loop
               Unload (Container, This);
               case This.Class is
                  when Tuple =>
                     if not
                        (  Unchecked
                        or else
                           (This.Operation and Operation)
                        )
                     then
                        raise Wrong_Comma_Type;
                     end if;
                     --
                     -- Creating a  new  sublist  that  takes  away  all
                     -- arguments of the current tuple, so  the  tuple's
                     -- count becomes 1, i.e. the result of the sublist.
                     --
                     Left  := This.Operation;
                     Count := This.Count + 1;
                     This.Count := 1;
                     exit;
                  when Sublist =>
                     if not
                        (  Unchecked
                        or else
                           (This.Operation and Operation)
                        )
                     then
                        raise Wrong_Comma_Type;
                     end if;
                     if This.Right < Priority then
                        --
                        -- Opening and closing a new sublist taking away
                        -- arguments   from   a  sublist  with  a  lower
                        -- association priority.
                        --
                        Left  := This.Operation;
                        Count := This.Count + 1;
                        This.Count := 1;
                        exit;
                     elsif Priority < This.Right then
                        --
                        -- Closing a sublist of a higher priority
                        --
                        Pop (Container.Raw);
                        Enclose
                        (  Container,
                           This.Operation,
                           Operation,
                           This.Count + 1
                        );
                        This := Top (Container.Raw);
                     else
                        --
                        -- The current  sublist  of  the  same  type  is
                        -- closed and a new  one  will  be  opened.  The
                        -- current  list  recieves  the  argument.   Its
                        -- result will a new argument of the parent.
                        --
                        Pop (Container.Raw);
                        Left  := This.Operation;
                        Count := This.Count + 1;
                        This  := Top (Container.Raw);
                        This.Count := This.Count + 1;
                        exit;
                     end if;
                  when others =>
                     raise Unexpected_Comma;
               end case;
            end loop;
            Pop  (Container.Raw);         -- Replacing the top
            Push (Container.Raw, This);
            Enclose (Container, Left, Operation, Count);
            if Class /= Sublist_Close then
               Push (Container.Raw, (Sublist, Operation, 0, Priority));
            end if;
      end case;
   end Push_Semicolon;

   procedure Pop (Container : in out Stack'Class) is
   begin
      if Top (Container).Class = Stub then
         raise Constraint_Error;
      end if;
      Pop (Container.Raw);
   end Pop;

   procedure Replace
             (  Container   : in out Stack'Class;
                Replacement : Descriptor
             )  is
   begin
      if Top (Container).Class = Stub then
         raise Constraint_Error;
      end if;
      Pop (Container.Raw);
      Push (Container.Raw, Replacement);
   end Replace;

   function Top (Container : Stack) return Descriptor is
   begin
      return Top (Container.Raw);
   end Top;

end Parsers.Generic_Operation.Generic_Stack;
