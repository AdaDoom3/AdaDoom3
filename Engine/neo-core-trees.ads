
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

with Ada.Containers.Indefinite_Multiway_Trees;

generic
  type Tree_T (<>) is private;
package Neo.Core.Trees is

  -- Base type
  package Unsafe is new Ada.Containers.Indefinite_Multiway_Trees (Tree_T, "="); 
  subtype Cursor is Unsafe.Cursor;
  NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

  -- Wrapped type
  protected type Safe_Tree is   
      procedure Clear;
      procedure Replace         (Pos    : Cursor; Item : Tree_T);
      procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T));
      procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T));
      procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor));
      procedure Iterate                          (Process : not null access procedure (Pos : Cursor));
      procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
      procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
      procedure Swap            (I, J   : Cursor);
      procedure Set             (Source : Unsafe.Tree);
      procedure Move            (Source : in out Unsafe.Tree);
      procedure Delete_Leaf     (Pos    : in out Cursor);
      procedure Delete_Subtree  (Pos    : in out Cursor);
      procedure Delete          (Parent         : Cursor);
      procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Positive := 1);
      procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Positive := 1);
      procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Positive := 1);
      procedure Splice_Subtree  (Parent, Before, Pos           : Cursor);
      procedure Splice_Children (Parent, Before, Source_Parent : Cursor);
      procedure Copy_Subtree    (Parent, Before, Source        : Cursor);
      procedure Next            (Pos    : in out Cursor);
      procedure Previous        (Pos    : in out Cursor);
      function Subtree_Nodes    (Pos    : Cursor)                return Positive;
      function Depth            (Pos    : Cursor)                return Positive;
      function "="              (L, R   : Cursor)                return Bool;
      function Is_Root          (Pos    : Cursor)                return Bool;
      function Is_Leaf          (Pos    : Cursor)                return Bool;
      function Has              (Pos    : Cursor)                return Bool;
      function Equals           (Item   : Unsafe.Tree)           return Bool;
      function Find             (Item   : Tree_T)                return Cursor;
      function Get              (Pos    : Cursor)                return Tree_T;
      function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor;
      function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor;
      function Child_Count      (Parent        : Cursor)         return Positive;
      function Child_Depth      (Parent, Child : Cursor)         return Positive;
      function Parent           (Pos    : Cursor)                return Cursor;
      function First            (Parent : Cursor)                return Cursor;
      function First            (Parent : Cursor)                return Tree_T;
      function Last             (Parent : Cursor)                return Tree_T;
      function Last             (Parent : Cursor)                return Cursor;
      function Is_Empty                                          return Bool;
      function Node_Count                                        return Positive;
      function Root                                              return Cursor;
      function Get                                               return Unsafe.Tree;
    private
      This : Unsafe.Tree;
    end;
end;
