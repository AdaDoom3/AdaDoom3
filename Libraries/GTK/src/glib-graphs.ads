-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001-2005 AdaCore                 --
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

--  <description>
--  General implementation for a graph.
--  This provides a representation for a graph structure, with nodes (vertices)
--  connected by links (edges).
--  It is not intended for huges, highly-connected graphs, since there are
--  several lists provided for efficient access to ancestor and children nodes.
--  </description>
--  <group>Glib, the general-purpose library</group>

package Glib.Graphs is

   type Graph  is private;
   type Vertex is abstract tagged private;
   type Edge   is abstract tagged private;

   type Vertex_Access is access all Vertex'Class;
   type Edge_Access   is access all Edge'Class;
   --  General access types to vertices and edges

   type Vertex_Iterator is private;
   type Edge_Iterator   is private;
   --  Iterators other the vertices and edges of the graph

   Null_Vertex_Iterator : constant Vertex_Iterator;
   Null_Edge_Iterator : constant Edge_Iterator;

   type Vertices_Array is array (Natural range <>) of Vertex_Access;
   type Edges_Array    is array (Natural range <>) of Edge_Access;

   -----------------------
   -- Modifying a graph --
   -----------------------

   procedure Set_Directed (G : in out Graph; Directed : Boolean);
   --  Indicate whether the graph is oriented.

   function Is_Directed (G : Graph) return Boolean;
   --  Return True if the graph is oriented

   procedure Add_Vertex (G : in out Graph; V : access Vertex'Class);
   --  Add a new vertex to the graph.

   procedure Add_Edge
     (G            : in out Graph;
      E            : access Edge'Class;
      Source, Dest : access Vertex'Class);
   --  Add a new edge to the graph.

   procedure Destroy (E : in out Edge) is abstract;
   --  Destroy the memory occupied by the edge. This doesn't remove the edge
   --  from the graph. You should override this subprogram for the specific
   --  edge type you are using.
   --  This subprogram shouldn't (and in fact can't) free E itself.

   procedure Destroy (V : in out Vertex) is abstract;
   --  Destroy the memory occupied by the vertex. This doesn't remove the
   --  vertex from the graph. This subprogram must be overriden.
   --  This subprogram shouldn't (and in fact can't) free V itself.

   procedure Destroy (G : in out Graph);
   --  Destroy all the nodes and edges of the graph, and then free the memory
   --  occupied by the graph itself

   procedure Clear (G : in out Graph);
   --  Remove all the nodes and edges of the graph.

   procedure Remove (G : in out Graph; E : access Edge'Class);
   --  Remove the edge from the graph. The primitive
   --  subprogram Destroy is called for the edge.
   --  Any iterator currently pointing to E becomes invalid

   procedure Remove (G : in out Graph; V : access Vertex'Class);
   --  Remove the vertex from the graph.
   --  Destroy is called for the vertex.
   --  Note that all the edges to or from the vertex are destroyed (see
   --  Remove above).
   --  Any iterator currently pointing to V becomes invalid

   function Is_Acyclic (G : Graph) return Boolean;
   --  Return True if G contains no cycle. Note that this requires a
   --  depth-first search, the running time is thus
   --    O (edges + vertices).
   --  G must be oriented

   function Get_Src  (E : access Edge) return Vertex_Access;
   function Get_Dest (E : access Edge) return Vertex_Access;
   --  Return the source and destination for a given edge

   function In_Degree  (G : Graph; V : access Vertex'Class) return Natural;
   function Out_Degree (G : Graph; V : access Vertex'Class) return Natural;
   --  Return the number of edges ending on V, or starting from V.

   procedure Move_To_Front (G : in out Graph; V : access Vertex'Class);
   --  Move V to the front of the list of vertices in the graph, so that the
   --  iterators will return this item first.
   --  All iterators become obsolete.

   procedure Move_To_Back (G : in out Graph; V : access Vertex'Class);
   --  Move V to the back of the list of vertices in the graph, so that the
   --  iterators will return this item last.
   --  All iterators become obsolete.

   function Get_Index (V : access Vertex) return Natural;
   --  Return the uniq index associated with the vertex. Each vertex has a
   --  different index from 0 to Max_Index (Graph)

   function Max_Index (G : Graph) return Natural;
   --  Return the maximum index used for vertices in the graph.

   --------------------------
   -- Breadth First Search --
   --------------------------
   --  This search algorithm traverse the tree layer after layer (first the
   --  nodes closer to the specified root, then the grand-children of this
   --  root, and so on).

   type Breadth_Record is record
      Vertex      : Vertex_Access;
      Distance    : Natural;
      Predecessor : Vertex_Access;
   end record;
   --  Distance is the shortest distance from the root of the breadth-first
   --  search to Vertex. The graph is considered as unweighted. Thus, Distance
   --  is 1 for direct children of Root, 2 for grand-children,...
   --  Predecessor is the parent of Vertex used when computing the distance.

   type Breadth_Vertices_Array is array (Natural range <>) of Breadth_Record;

   function Breadth_First_Search (G : Graph; Root : access Vertex'Class)
      return Breadth_Vertices_Array;
   --  Traverse the tree Breadth_First, and sort the nodes accordingly.
   --  The returned list is sorted so that all nodes at a distance k from Root
   --  are found before the nodes at a distance (k+1).
   --  The running time is O(vertices + edges).

   ------------------------
   -- Depth First Search --
   ------------------------
   --  This algorithm traverse the tree in depth, ie all the descendents of the
   --  first child are found before the second child.
   --  This algorithm has several properties: it can indicate whether the graph
   --  is cyclic. Moreover, the subgraph formed by all the nodes and the edges
   --  between a vertex and its predecessor (see the structure Depth_Record) is
   --  a tree.
   --  If the graph is acyclic, then the resulting array is sorted
   --  topologically: if G contains an edge (u, v), then u appears before v.
   --
   --  The running time for this algorithm is O(vertices + edges)

   type Depth_Record is record
      Vertex : Vertex_Access;
      First_Discovered, End_Search : Natural;
      Predecessor : Vertex_Access;
   end record;
   --  First_Discovered and End_Search are the two timestamps computed during
   --  the search. The former is the time Vertex was first discovered. The
   --  latter is the time when all the children of vertex were fully
   --  processed.
   --  Predecessor is the parent of Vertex.

   type Depth_Vertices_Array is array (Natural range <>) of Depth_Record;

   type Reverse_Edge_Callback is access
     procedure (G : Graph; Edge : Edge_Access);
   --  Callback called when the two ends of the edge should be reverted, so as
   --  to make the graph acyclick

   procedure Revert_Edge (G : Graph; E : Edge_Access);
   --  Revert the two ends of Edge. This is meant to be used as a callback for
   --  Depth_First_Search so as to make the graph acyclic.

   function Depth_First_Search (G : Graph) return Depth_Vertices_Array;
   --  Traverse the tree Depth_First.

   function Depth_First_Search
     (G : Graph;
      Acyclic : access Boolean;
      Reverse_Edge_Cb : Reverse_Edge_Callback := null)
      return Depth_Vertices_Array;
   --  Same as above, but Acyclic is also modified to indicate whether G is
   --  acyclic.
   --  If Reverse_Edge_Cb is not null, then it is called to reverse the ends of
   --  selected edges, so that the final graph is acyclic. Note that you *must*
   --  revert the ends, or there will be an infinite loop. You might also want
   --  to mark the edge as reverted somehow, so as to draw the arrows on the
   --  correct side, if your application is graphical.
   --
   --  If Reverse_Edge_Cb is null, no edge is reverted, and the graph is
   --  unmodified.

   -----------------------------------
   -- Strongly connected components --
   -----------------------------------
   --  Strongly connected components in a directed graph are the maximal set of
   --  vertices such that for every pair {u, v} of vertices in the set, there
   --  exist a path from u to v and a path from v to u.
   --  Two vertices are in different strongly connected components if there
   --  exist at most one of these paths.

   type Connected_Component;
   type Connected_Component_List is access Connected_Component;
   type Connected_Component (Num_Vertices : Natural) is record
      Vertices : Vertices_Array (1 .. Num_Vertices);
      Next     : Connected_Component_List;
   end record;

   procedure Free (List : in out Connected_Component_List);
   --  Free the list of strongly connected components

   function Strongly_Connected_Components (G : Graph)
      return Connected_Component_List;
   --  Return the list of strongly connected components.
   --  This is a linear time algorithm O(vertices + edges).

   function Strongly_Connected_Components
     (G : Graph; DFS : Depth_Vertices_Array)
      return Connected_Component_List;
   --  Same as above, but a depth-first search has already been run on G, and
   --  we reuse the result. This is of course more efficient than the previous
   --  function.

   ----------------------------
   -- Minimum spanning trees --
   ----------------------------
   --  A minimum spanning tree is a subset of the edges of G that forms a
   --  tree (acyclic) and connects all the vertices of G.
   --  Note that the number of edges in the resulting tree is always
   --      (number of vertices of G) - 1

   function Kruskal (G : Graph) return Edges_Array;
   --  Return a minimum spanning tree of G using Kruskal's algorithm.
   --  This algorithm runs in O(E * log E), with E = number of edges.

   ---------------------
   -- Vertex iterator --
   ---------------------

   function First (G : Graph) return Vertex_Iterator;
   --  Return a pointer to the first vertex.

   procedure Next (V : in out Vertex_Iterator);
   --  Moves V to the next vertex in the graph.

   function At_End (V : Vertex_Iterator) return Boolean;
   --  Return True if V points after the last vertex

   function Get (V : Vertex_Iterator) return Vertex_Access;
   --  Get the vertex pointed to by V

   -------------------
   -- Edge iterator --
   -------------------

   function First
     (G : Graph;
      Src, Dest : Vertex_Access := null;
      Directed : Boolean := True)
      return Edge_Iterator;
   --  Return a pointer to the first edge from Src to Dest.
   --  If either Src or Dest is null, then any vertex matches. Thus, if both
   --  parameters are nulll, this iterator will traverse the whole graph.
   --  Note: there might be duplicates returned by this iterator, especially
   --  when the graph is not oriented.
   --  Directed can be used to temporarily overrides the setting in the graph:
   --  If Directed is True, the setting of G is taken into account.
   --  If Directed is False, the setting of G is ignored, and the graph is
   --  considered as not directed.

   procedure Next (E : in out Edge_Iterator);
   --  Moves V to the next edge in the graph.

   function At_End (E : Edge_Iterator) return Boolean;
   --  Return True if V points after the last edge

   function Get (E : Edge_Iterator) return Edge_Access;
   --  Get the edge pointed to by E.

   function Repeat_Count (E : Edge_Iterator) return Positive;
   --  Return the number of similar edges (same ends) that were found before,
   --  and including this one).
   --  For instance, if there two edges from A to B, then the first one will
   --  have a Repeat_Count of 1, and the second 2.

private

   --  Note: we do not use a generic list, since that would require a separate
   --  package so that we can instanciate it in this package. Doesn't seem
   --  worth adding this to GtkAda. Nor does it seem interesting to use
   --  Glib.Glist.

   type Edge_List_Record;
   type Edge_List is access Edge_List_Record;
   type Edge_List_Record is record
      E    : Edge_Access;
      Next : Edge_List;
   end record;

   procedure Add    (List : in out Edge_List; E : access Edge'Class);
   --  Add a new element to List.
   --  Edges are inserted in the list so that edges with similar ends are next
   --  to each other.

   procedure Remove (List : in out Edge_List; E : access Edge'Class);
   --  Remove an element from List

   function Length (List : Edge_List) return Natural;
   --  Return the number of elements in the list

   type Vertex_List_Record;
   type Vertex_List is access Vertex_List_Record;
   type Vertex_List_Record is record
      V    : Vertex_Access;
      Next : Vertex_List;
   end record;

   type Graph is record
      Vertices          : Vertex_List;
      Num_Vertices      : Natural := 0;
      Directed          : Boolean := False;
      Last_Vertex_Index : Natural := 0;
   end record;

   procedure Add    (List : in out Vertex_List; V : access Vertex'Class);
   procedure Internal_Remove (G : in out Graph; V : access Vertex'Class);

   type Edge is abstract tagged record
      Src, Dest : Vertex_Access;
   end record;

   type Vertex is abstract tagged record
      Index               : Natural; --  Internal unique index for the vertex
      In_Edges, Out_Edges : Edge_List;
   end record;

   type Vertex_Iterator is new Vertex_List;
   type Edge_Iterator is record
      Directed       : Boolean;
      Src, Dest      : Vertex_Access;
      Current_Edge   : Edge_List;
      Current_Vertex : Vertex_List;
      First_Pass     : Boolean;
      Repeat_Count   : Positive := 1;
   end record;

   Null_Vertex_Iterator : constant Vertex_Iterator := null;
   Null_Edge_Iterator : constant Edge_Iterator :=
     (Directed       => False,
      Src            => null,
      Dest           => null,
      Current_Edge   => null,
      Current_Vertex => null,
      First_Pass     => True,
      Repeat_Count   => 1);

   pragma Inline (Get_Index);
   pragma Inline (Max_Index);
end Glib.Graphs;
