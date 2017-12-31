
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Neo.Core.Trees is
  protected body Safe_Tree is
      procedure Set             (Source : Unsafe.Tree)                                                        is begin This := Source;                                                             end;
      procedure Clear                                                                                         is begin This.Clear;                                                                 end;
      procedure Replace         (Pos    : Cursor; Item : Tree_T)                                              is begin This.Replace_Element            (Pos, Item);                                end;
      procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T))        is begin Unsafe.Query_Element            (Pos,    Process);                          end;
      procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T)) is begin This.Update_Element             (Pos,    Process);                          end;
      procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Iterate_Subtree          (Pos,    Process);                          end;
      procedure Iterate                          (Process : not null access procedure (Pos : Cursor))         is begin This.Iterate                            (Process);                          end;
      procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Iterate_Children         (Parent, Process);                          end;
      procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Reverse_Iterate_Children (Parent, Process);                          end;
      procedure Move            (Source : in out Unsafe.Tree)                                                 is begin This.Move                       (Source);                                   end;
      procedure Delete_Leaf     (Pos    : in out Cursor)                                                      is begin This.Delete_Leaf                (Pos);                                      end;
      procedure Delete_Subtree  (Pos    : in out Cursor)                                                      is begin This.Delete_Subtree             (Pos);                                      end;
      procedure Swap            (I, J   : Cursor)                                                             is begin This.Swap                       (I, J);                                     end;
      procedure Delete          (Parent         : Cursor)                                                     is begin This.Delete_Children            (Parent);                                   end;
      procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Prepend_Child              (Parent,         Item, Count_Type (Count)); end;
      procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Append_Child               (Parent,         Item, Count_Type (Count)); end;
      procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Insert_Child               (Parent, Before, Item, Count_Type (Count)); end;
      procedure Splice_Subtree  (Parent, Before, Pos           : Cursor)                                      is begin This.Splice_Subtree             (Parent, before, Pos);                      end;
      procedure Splice_Children (Parent, Before, Source_Parent : Cursor)                                      is begin This.Splice_Children            (Parent, Before, Source_Parent);            end;
      procedure Copy_Subtree    (Parent, Before, Source        : Cursor)                                      is begin This.Copy_Subtree               (Parent, Before, Source);                   end;
      procedure Next            (Pos    : in out Cursor)                                                      is begin Unsafe.Next_Sibling             (Pos);                                      end;
      procedure Previous        (Pos    : in out Cursor)                                                      is begin Unsafe.Previous_Sibling         (Pos);                                      end;
      function Subtree_Nodes    (Pos    : Cursor)                return Positive                              is (Positive (Unsafe.Subtree_Node_Count  (Pos)));
      function Depth            (Pos    : Cursor)                return Positive                              is (Positive (Unsafe.Depth               (Pos)));
      function "="              (L, R   : Cursor)                return Bool                                  is (Unsafe.Equal_Subtree                 (L, R));
      function Is_Root          (Pos    : Cursor)                return Bool                                  is (Unsafe.Is_Root                       (Pos));
      function Is_Leaf          (Pos    : Cursor)                return Bool                                  is (Unsafe.Is_Leaf                       (Pos));
      function Has              (Pos    : Cursor)                return Bool                                  is (Unsafe.Has_Element                   (Pos));
      function Equals           (Item   : Unsafe.Tree)           return Bool                                  is (This."="                             (Item));
      function Find             (Item   : Tree_T)                return Cursor                                is (This.Find                            (Item));
      function Get              (Pos    : Cursor)                return Tree_T                                is (Unsafe.Element                       (Pos));
      function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor                                is (Unsafe.Find_In_Subtree               (Pos, Item));
      function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor                                is (Unsafe.Ancestor_Find                 (Pos, Item));
      function Child_Count      (Parent        : Cursor)         return Positive                              is (Positive (Unsafe.Child_Count         (Parent)));
      function Child_Depth      (Parent, Child : Cursor)         return Positive                              is (Positive (Unsafe.Child_Depth         (Parent, Child)));
      function Parent           (Pos    : Cursor)                return Cursor                                is (Unsafe.Parent                        (Pos));
      function First            (Parent : Cursor)                return Cursor                                is (Unsafe.First_Child                   (Parent));
      function First            (Parent : Cursor)                return Tree_T                                is (Unsafe.First_Child_Element           (Parent));
      function Last             (Parent : Cursor)                return Tree_T                                is (Unsafe.Last_Child_Element            (Parent));
      function Last             (Parent : Cursor)                return Cursor                                is (Unsafe.Last_Child                    (Parent));
      function Node_Count                                        return Positive                              is (Positive (This.Node_Count));
      function Root                                              return Cursor                                is (This.Root);
      function Is_Empty                                          return Bool                                  is (This.Is_Empty);
      function Get                                               return Unsafe.Tree                           is (This);
    end;
end;