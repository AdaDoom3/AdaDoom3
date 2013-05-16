-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Unchecked_Deallocation;

package body Glib.Graphs is

   type Search_Color is (White, Gray, Black);
   type Color_Array  is array (Natural range <>) of Search_Color;

   procedure Move_To_Next (E : in out Edge_Iterator);
   --  nove the next matching edge, starting at the one pointed to by E
   --  E should be the first potential candidate for the next item (ie should
   --  already have been moved to the next edge).

   ------------------
   -- Set_Directed --
   ------------------

   procedure Set_Directed (G : in out Graph; Directed : Boolean) is
   begin
      G.Directed := Directed;
   end Set_Directed;

   -----------------
   -- Is_Directed --
   -----------------

   function Is_Directed (G : Graph) return Boolean is
   begin
      return G.Directed;
   end Is_Directed;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (V : access Vertex) return Natural is
   begin
      return V.Index;
   end Get_Index;

   ---------------
   -- Max_Index --
   ---------------

   function Max_Index (G : Graph) return Natural is
   begin
      return G.Last_Vertex_Index;
   end Max_Index;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex (G : in out Graph; V : access Vertex'Class) is
   begin
      V.Index             := G.Last_Vertex_Index;
      G.Last_Vertex_Index := G.Last_Vertex_Index + 1;
      G.Num_Vertices      := G.Num_Vertices + 1;
      Add (G.Vertices, V);
   end Add_Vertex;

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge
     (G            : in out Graph;
      E            : access Edge'Class;
      Source, Dest : access Vertex'Class)
   is
      pragma Unreferenced (G);
   begin
      pragma Assert (E.Src = null and then E.Dest = null);
      E.Src  := Vertex_Access (Source);
      E.Dest := Vertex_Access (Dest);
      Add (Source.Out_Edges, E);
      Add (Dest.In_Edges, E);
   end Add_Edge;

   ------------
   -- Remove --
   ------------

   procedure Remove (G : in out Graph; E : access Edge'Class) is
      pragma Unreferenced (G);

      procedure Free is new Unchecked_Deallocation (Edge'Class, Edge_Access);
      E2 : Edge_Access := Edge_Access (E);

   begin
      Remove (E.Src.Out_Edges, E);
      Remove (E.Dest.In_Edges, E);
      Destroy (E.all);
      Free (E2);
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (G : in out Graph) is
   begin
      Clear (G);
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (G : in out Graph) is
   begin
      while G.Vertices /= null loop
         Remove (G, G.Vertices.V);
      end loop;
   end Clear;

   ------------
   -- Remove --
   ------------

   procedure Remove (G : in out Graph; V : access Vertex'Class) is
      procedure Free is new Unchecked_Deallocation
        (Vertex'Class, Vertex_Access);
      E : Edge_Iterator;
      E2 : Edge_Access;
      V2 : Vertex_Access := Vertex_Access (V);
   begin
      --  Destroy all outgoing edges
      E := First (G, Src => Vertex_Access (V));
      while not At_End (E) loop
         E2 := Get (E);
         Next (E);
         Remove (G, E2);
      end loop;

      --  Destroy all ingoing edges
      E := First (G, Dest => Vertex_Access (V));
      while not At_End (E) loop
         E2 := Get (E);
         Next (E);
         Remove (G, E2);   --  ??? Could be more efficient, since we have
                           --  the pointer into the list directly
      end loop;

      --  Free the vertex
      Internal_Remove (G, V);
      Destroy (V.all);
      Free (V2);
   end Remove;

   ---------
   -- Add --
   ---------

   procedure Add (List : in out Edge_List; E : access Edge'Class) is
      L : Edge_List := List;
   begin
      --  Insert the item in the list so that items with equal ends are next to
      --  each other.

      while L /= null loop
         if L.E.Src = E.Src and then L.E.Dest = E.Dest then
            L.Next := new Edge_List_Record'
              (E => Edge_Access (E), Next => L.Next);
            return;
         end if;
         L := L.Next;
      end loop;

      List := new Edge_List_Record'(E => Edge_Access (E), Next => List);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add    (List : in out Vertex_List; V : access Vertex'Class) is
   begin
      List := new Vertex_List_Record'(V => Vertex_Access (V), Next => List);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : in out Edge_List; E : access Edge'Class) is
      procedure Internal is new Unchecked_Deallocation
        (Edge_List_Record, Edge_List);
      Tmp      : Edge_List := List;
      Previous : Edge_List;
   begin
      while Tmp /= null
        and then Tmp.E /= Edge_Access (E)
      loop
         Previous := Tmp;
         Tmp := Tmp.Next;
      end loop;

      if Tmp /= null then
         if Previous = null then
            pragma Assert (Tmp = List);
            Previous := List;
            List := List.Next;
            Internal (Previous);
         else
            Previous.Next := Tmp.Next;
            Internal (Tmp);
         end if;
      end if;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Internal_Remove (G : in out Graph; V : access Vertex'Class) is
      procedure Internal is new Unchecked_Deallocation
        (Vertex_List_Record, Vertex_List);
      Tmp      : Vertex_List := G.Vertices;
      Previous : Vertex_List := null;
   begin
      while Tmp /= null
        and then Tmp.V /= Vertex_Access (V)
      loop
         Previous := Tmp;
         Tmp := Tmp.Next;
      end loop;

      if Tmp /= null then
         if Previous = null then
            --  The list contains only one item which is the one to be removed.
            --  Once it has been removed the list must be reset to null.
            pragma Assert (Tmp = G.Vertices, "Remove vertex");
            Previous := G.Vertices;
            G.Vertices := G.Vertices.Next;
            Internal (Previous);
         else
            Previous.Next := Tmp.Next;
            Internal (Tmp);
         end if;
         G.Num_Vertices := G.Num_Vertices - 1;
      end if;
   end Internal_Remove;

   -----------
   -- First --
   -----------

   function First (G : Graph) return Vertex_Iterator is
   begin
      return Vertex_Iterator (G.Vertices);
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (V : in out Vertex_Iterator) is
   begin
      V := Vertex_Iterator (V.Next);
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (V : Vertex_Iterator) return Boolean is
   begin
      return V = null;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (V : Vertex_Iterator) return Vertex_Access is
   begin
      return V.V;
   end Get;

   -----------
   -- First --
   -----------

   function First (G : Graph;
                   Src, Dest : Vertex_Access := null;
                   Directed : Boolean := True)
      return Edge_Iterator
   is
      Va : Edge_Iterator :=
        (Directed       => Directed and then G.Directed,
         Repeat_Count   => 1,
         Src            => Src,
         Dest           => Dest,
         Current_Vertex => null,
         Current_Edge   => null,
         First_Pass     => True);

   begin
      if Src /= null then
         --  If Src /= null and then Dest = null then
         --     Result is the whole list from Src.Out_Nodes
         --     If not directed: add to it the list of Dest.Out_Nodes
         --     Duplicates in the following case: a given node has two links
         --     to and from Src, and the graph is not oriented.
         --        Src  <----->   B
         --  Elsif Src /= null and then Dest /= null then
         --     Result is the whole list from Src.Out_Nodes that match Dest
         --     If not directed: add to it the list of Dest.In_Nodes that
         --     match Src.
         --     Duplicates when there is a link from src to dest, and one from
         --     dest to src.
         Va.Current_Edge := Src.Out_Edges;

      elsif Dest = null then
         --  If Src = null and then Dest = null then
         --     Result is the concatenation for all edges of Edge.Out_Nodes
         if G.Vertices /= null then
            Va.Current_Vertex := G.Vertices;
            Va.Current_Edge := G.Vertices.V.Out_Edges;
         end if;

      else
         --  If Src = null and then Dest /= null then
         --     Result is the whole list from Dest.In_Nodes
         Va.Current_Edge := Dest.In_Edges;
      end if;

      Move_To_Next (Va);
      return Va;
   end First;

   ------------------
   -- Move_To_Next --
   ------------------

   procedure Move_To_Next (E : in out Edge_Iterator) is
   begin
      if E.Src /= null then
         if E.Dest = null then
            if E.Current_Edge = null
              and then not E.Directed
              and then E.First_Pass
            then
               E.First_Pass   := False;
               E.Current_Edge := E.Src.In_Edges;
            end if;

         else
            while E.Current_Edge /= null
              and then (E.First_Pass or else E.Current_Edge.E.Src /= E.Dest)
              and then (not E.First_Pass
                        or else E.Current_Edge.E.Dest /= E.Dest)
            loop
               E.Current_Edge := E.Current_Edge.Next;
            end loop;

            if E.Current_Edge = null
              and then not E.Directed
              and then E.First_Pass
            then
               E.First_Pass := False;
               E.Current_Edge := E.Src.In_Edges;
               Move_To_Next (E);
            end if;
         end if;

         --  In the second pass, we must ignore the recursive links to the
         --  item, since they have already been counted.
         if not E.First_Pass then
            while E.Current_Edge /= null
              and then E.Current_Edge.E.Src = E.Current_Edge.E.Dest
            loop
               E.Current_Edge := E.Current_Edge.Next;
            end loop;
         end if;

      elsif E.Dest = null then
         if E.Current_Vertex /= null then
            while E.Current_Edge = null loop
               E.Current_Vertex := E.Current_Vertex.Next;
               exit when E.Current_Vertex = null;
               E.Current_Edge := E.Current_Vertex.V.Out_Edges;
            end loop;
         end if;

      else
         if E.Current_Edge = null
           and then not E.Directed
           and then E.First_Pass
         then
            E.First_Pass   := False;
            E.Current_Edge := E.Dest.Out_Edges;
         end if;

         --  In the second pass, we must ignore the recursive links to the
         --  item, since they have already been counted.
         if not E.First_Pass then
            while E.Current_Edge /= null
              and then E.Current_Edge.E.Src = E.Current_Edge.E.Dest
            loop
               E.Current_Edge := E.Current_Edge.Next;
            end loop;
         end if;
      end if;
   end Move_To_Next;

   ----------
   -- Next --
   ----------

   procedure Next (E : in out Edge_Iterator) is
      Save : constant Edge_Access := E.Current_Edge.E;
   begin
      E.Current_Edge := E.Current_Edge.Next;
      Move_To_Next (E);

      if E.Current_Edge /= null then
         if E.Current_Edge.E.Src = Save.Src
           and then E.Current_Edge.E.Dest = Save.Dest
         then
            E.Repeat_Count := E.Repeat_Count + 1;
         else
            E.Repeat_Count := 1;
         end if;
      else
         E.Repeat_Count := 1;
      end if;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (E : Edge_Iterator) return Boolean is
   begin
      return E.Current_Vertex = null
        and then E.Current_Edge = null;
   end At_End;

   ------------------
   -- Repeat_Count --
   ------------------

   function Repeat_Count (E : Edge_Iterator) return Positive is
   begin
      return E.Repeat_Count;
   end Repeat_Count;

   ---------
   -- Get --
   ---------

   function Get (E : Edge_Iterator) return Edge_Access is
   begin
      pragma Assert (not At_End (E));
      return E.Current_Edge.E;
   end Get;

   -------------
   -- Get_Src --
   -------------

   function Get_Src (E : access Edge) return Vertex_Access is
   begin
      return E.Src;
   end Get_Src;

   --------------
   -- Get_Dest --
   --------------

   function Get_Dest (E : access Edge) return Vertex_Access is
   begin
      return E.Dest;
   end Get_Dest;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   function Breadth_First_Search
     (G : Graph; Root : access Vertex'Class)
      return Breadth_Vertices_Array
   is
      Colors : Color_Array (0 .. G.Last_Vertex_Index - 1) := (others => White);
      Distances : array (0 .. G.Last_Vertex_Index - 1) of Natural :=
        (others => Natural'Last);
      Predecessors : Vertices_Array (0 .. G.Last_Vertex_Index - 1) :=
        (others => null);
      Queue : Vertices_Array (0 .. G.Num_Vertices - 1);
      Queue_Index : Integer := 0;
      Queue_First : Integer := 0;
      Result : Breadth_Vertices_Array (0 .. G.Num_Vertices - 1);
      Result_Index : Natural := 0;

      V, U : Vertex_Access;
      Eit : Edge_Iterator;
   begin
      --  Initialize the root
      Distances (Root.Index) := 0;
      Queue (Queue_Index) := Vertex_Access (Root);
      Queue_Index := Queue_Index + 1;

      while Queue_First < Queue_Index loop
         U := Queue (Queue_First);
         Eit := First (G, Src => U);
         while not At_End (Eit) loop
            V := Get_Dest (Get (Eit));
            if V = U then
               V := Get_Src (Get (Eit));
            end if;

            if Colors (V.Index) = White then
               Colors (V.Index) := Gray;
               Distances (V.Index) := Distances (U.Index) + 1;
               Predecessors (V.Index) := U;
               Queue (Queue_Index) := V;
               Queue_Index := Queue_Index + 1;
            end if;
            Next (Eit);
         end loop;
         Queue_First := Queue_First + 1;
         Colors (U.Index) := Black;
         Result (Result_Index) :=
           (U, Distances (U.Index), Predecessors (U.Index));
         Result_Index := Result_Index + 1;
      end loop;

      return Result (Result'First .. Result_Index - 1);
   end Breadth_First_Search;

   ------------------------
   -- Depth_First_Search --
   ------------------------

   function Depth_First_Search (G : Graph) return Depth_Vertices_Array is
      Acyclic : aliased Boolean;
   begin
      return Depth_First_Search (G, Acyclic'Access);
   end Depth_First_Search;

   ------------------------
   -- Depth_First_Search --
   ------------------------

   function Depth_First_Search
     (G : Graph;
      Acyclic : access Boolean;
      Reverse_Edge_Cb : Reverse_Edge_Callback := null)
      return Depth_Vertices_Array
   is
      Colors : Color_Array (0 .. G.Last_Vertex_Index - 1) := (others => White);
      Predecessors : Vertices_Array (0 .. G.Last_Vertex_Index - 1);
      Start : array (0 .. G.Last_Vertex_Index - 1) of Natural;
      Result : Depth_Vertices_Array (0 .. G.Num_Vertices - 1);
      Result_Index : Integer := Result'Last;
      Time : Natural := 0;

      procedure Depth_First_Visit (U : Vertex_Access);
      --  Process the node U

      procedure Depth_First_Visit (U : Vertex_Access) is
         V : Vertex_Access;
         Eit : Edge_Iterator;
      begin
         Colors (U.Index) := Gray;
         Time := Time + 1;
         Start (U.Index) := Time;
         Eit := First (G, Src => U);
         while not At_End (Eit) loop
            V := Get_Dest (Get (Eit));
            if V = U then
               V := Get_Src (Get (Eit));
            end if;

            if Colors (V.Index) = White then
               Predecessors (V.Index) := U;
               --  ??? Would be nice to have a non-recursive implementation, to
               --  ??? support larger graphs
               Depth_First_Visit (V);
               Next (Eit);

            elsif Colors (V.Index) = Gray then
               --  Make the graph acyclic by reversing the edge.
               if Reverse_Edge_Cb /= null then
                  declare
                     E : constant Edge_Access := Get (Eit);
                  begin
                     --  We need to first move the iterator, otherwise it will
                     --  become invalid when the two edges have been reversed.
                     Next (Eit);
                     Reverse_Edge_Cb (G, E);
                  end;
               else
                  Acyclic.all := False;
                  Next (Eit);
               end if;

            else
               Next (Eit);
            end if;
         end loop;
         Colors (U.Index) := Black;
         Time := Time + 1;
         Result (Result_Index) :=
           (U, Start (U.Index), Time, Predecessors (U.Index));
         Result_Index := Result_Index - 1;
      end Depth_First_Visit;

      U : Vertex_List;
   begin
      Acyclic.all := True;
      U := G.Vertices;
      while U /= null loop
         if Colors (U.V.Index) = White then
            Depth_First_Visit (U.V);
         end if;
         U := U.Next;
      end loop;
      return Result;
   end Depth_First_Search;

   ----------------
   -- Is_Acyclic --
   ----------------

   function Is_Acyclic (G : Graph) return Boolean is
      Colors : Color_Array (0 .. G.Last_Vertex_Index - 1) := (others => White);
      Acyclic : Boolean := True;

      procedure Depth_First_Visit (U : Vertex_Access);
      --  Process the node U

      procedure Depth_First_Visit (U : Vertex_Access) is
         V : Vertex_Access;
         Eit : Edge_Iterator;
      begin
         Colors (U.Index) := Gray;
         Eit := First (G, Src => U);
         while not At_End (Eit) loop
            V := Get_Dest (Get (Eit));
            if V = U then
               V := Get_Src (Get (Eit));
            end if;

            if Colors (V.Index) = White then
               Depth_First_Visit (V);
               if not Acyclic then
                  return;
               end if;
            elsif Colors (V.Index) = Gray then
               Acyclic := False;
               return;
            end if;
            Next (Eit);
         end loop;
         Colors (U.Index) := Black;
      end Depth_First_Visit;

      U : Vertex_List;
   begin
      pragma Assert (G.Directed);

      U := G.Vertices;
      while U /= null loop
         if Colors (U.V.Index) = White then
            Depth_First_Visit (U.V);
         end if;
         U := U.Next;
      end loop;
      return Acyclic;
   end Is_Acyclic;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Connected_Component_List) is
      procedure Internal is new Unchecked_Deallocation
        (Connected_Component, Connected_Component_List);
      L : Connected_Component_List;
   begin
      while List /= null loop
         L := List.Next;
         Internal (List);
         List := L;
      end loop;
   end Free;

   -----------------------------------
   -- Strongly_Connected_Components --
   -----------------------------------

   function Strongly_Connected_Components (G : Graph)
      return Connected_Component_List is
   begin
      return Strongly_Connected_Components (G, Depth_First_Search (G));
   end Strongly_Connected_Components;

   -----------------------------------
   -- Strongly_Connected_Components --
   -----------------------------------
   --  Basically, we do another depth-first search, but on the transpose of
   --  G (i.e with all edges inverted).

   function Strongly_Connected_Components
     (G : Graph; DFS : Depth_Vertices_Array)
      return Connected_Component_List
   is
      Colors : Color_Array (0 .. G.Last_Vertex_Index - 1) := (others => White);
      Result : Vertices_Array (0 .. G.Num_Vertices - 1);
      Result_Index : Integer := Result'Last;

      procedure Depth_First_Visit (U : Vertex_Access);
      --  Process the node U

      procedure Depth_First_Visit (U : Vertex_Access) is
         V : Vertex_Access;
         Eit : Edge_Iterator;
      begin
         Colors (U.Index) := Gray;
         Eit := First (G, Dest => U);
         while not At_End (Eit) loop
            V := Get_Src (Get (Eit));
            if V = U then
               V := Get_Dest (Get (Eit));
            end if;

            if Colors (V.Index) = White then
               --  ??? Would be nice to have a non-recursive implementation, to
               --  ??? support larger graphs
               Depth_First_Visit (V);
            end if;
            Next (Eit);
         end loop;
         Colors (U.Index) := Black;
         Result (Result_Index) := U;
         Result_Index := Result_Index - 1;
      end Depth_First_Visit;

      List : Connected_Component_List := null;
   begin
      pragma Assert (G.Directed);
      for U in DFS'Range loop
         if Colors (DFS (U).Vertex.Index) = White then
            Depth_First_Visit (DFS (U).Vertex);
            List := new Connected_Component'
              (Num_Vertices => Result'Last - Result_Index,
               Vertices     => Result (Result_Index + 1 .. Result'Last),
               Next         => List);
            Result_Index := Result'Last;
         end if;
      end loop;
      return List;
   end Strongly_Connected_Components;

   -------------
   -- Kruskal --
   -------------

   function Kruskal (G : Graph) return Edges_Array is
      Result : Edges_Array (0 .. G.Num_Vertices - 2);
      Result_Index : Natural := Result'First;
      Eit : Edge_Iterator;
      U, V : Vertex_Access;

      Sets : array (0 .. G.Last_Vertex_Index - 1) of Natural;
      --  This is used to represent the sets that will contain the
      --  vertices. Probably not the faster method (the union operation is
      --  quite slow), but the easiest to implement.

      V_Set : Natural;

   begin
      --  First put all vertices in their own set
      for S in Sets'Range loop
         Sets (S) := S;
      end loop;

      --  ??? Should sort the edges by increasing weight
      --  ??? and do the loop in that order

      Eit := First (G, Src => Vertex_Access'(null));
      while not At_End (Eit) loop
         U := Get_Src (Get (Eit));
         V := Get_Dest (Get (Eit));

         if Sets (U.Index) /= Sets (V.Index) then
            Result (Result_Index) := Get (Eit);
            Result_Index := Result_Index + 1;

            --  Merge the two sets
            V_Set := Sets (V.Index);
            for S in Sets'Range loop
               if Sets (S) = V_Set then
                  Sets (S) := Sets (U.Index);
               end if;
            end loop;
         end if;

         Next (Eit);
      end loop;

      return Result;
   end Kruskal;

   ------------
   -- Length --
   ------------

   function Length (List : Edge_List) return Natural is
      E : Edge_List := List;
      Count : Natural := 0;
   begin
      while E /= null loop
         Count := Count + 1;
         E := E.Next;
      end loop;
      return Count;
   end Length;

   ---------------
   -- In_Degree --
   ---------------

   function In_Degree (G : Graph; V : access Vertex'Class) return Natural is
      pragma Unreferenced (G);
   begin
      return Length (V.In_Edges);
   end In_Degree;

   ----------------
   -- Out_Degree --
   ----------------

   function Out_Degree (G : Graph; V : access Vertex'Class) return Natural is
      pragma Unreferenced (G);
   begin
      return Length (V.Out_Edges);
   end Out_Degree;

   -------------------
   -- Move_To_Front --
   -------------------

   procedure Move_To_Front (G : in out Graph; V : access Vertex'Class) is
      Iter : Vertex_List := G.Vertices;
      Tmp : Vertex_List;
   begin
      --  No or only one item => nothing to do
      if G.Vertices = null
        or else G.Vertices.V = Vertex_Access (V)
        or else G.Vertices.Next = null
      then
         return;
      end if;

      while Iter.Next /= null and then Iter.Next.V /= Vertex_Access (V) loop
         Iter := Iter.Next;
      end loop;

      if Iter.Next /= null then
         Tmp := Iter.Next;
         Iter.Next := Tmp.Next;
         Tmp.Next := G.Vertices;
         G.Vertices := Tmp;
      end if;
   end Move_To_Front;

   ------------------
   -- Move_To_Back --
   ------------------

   procedure Move_To_Back (G : in out Graph; V : access Vertex'Class) is
      Iter : Vertex_List;
      Old   : Vertex_List := null;
   begin
      if G.Vertices = null or else G.Vertices.Next = null then
         return;
      end if;

      if G.Vertices.V = Vertex_Access (V) then
         Old := G.Vertices;
         G.Vertices := G.Vertices.Next;
      end if;

      Iter := G.Vertices;

      while Iter.Next /= null loop
         if Iter.Next.V = Vertex_Access (V) then
            Old := Iter.Next;
            Iter.Next := Old.Next;
         else
            Iter := Iter.Next;
         end if;
      end loop;

      if Old /= null then
         Old.Next := null;
         Iter.Next := Old;
      end if;
   end Move_To_Back;

   -----------------
   -- Revert_Edge --
   -----------------

   procedure Revert_Edge (G : Graph; E : Edge_Access) is
      pragma Unreferenced (G);

      Src  : constant Vertex_Access := E.Src;
      Dest : constant Vertex_Access := E.Dest;

   begin
      Remove (E.Src.Out_Edges, E);
      Remove (E.Dest.In_Edges, E);
      E.Src  := Dest;
      E.Dest := Src;
      Add (E.Src.Out_Edges, E);
      Add (E.Dest.In_Edges, E);
   end Revert_Edge;

end Glib.Graphs;
