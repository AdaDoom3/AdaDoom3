-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Unchecked_Conversion;

package body Glib.Gnodes is

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Node : in out Gnode) is
      procedure Internal (Node : in Gnode);
      pragma Import (C, Internal, "g_node_destroy");
   begin
      Internal (Node);
      Node := null;
   end Destroy;

   -------------------
   --  Is_Ancestor  --
   -------------------

   function Is_Ancestor (Node       : in Gnode;
                         Descendant : in Gnode) return Boolean is
      function Internal (Node       : in Gnode;
                         Descendant : in Gnode) return Gboolean;
      pragma Import (C, Internal, "g_node_is_ancestor");
   begin
      return Internal (Node, Descendant) /= 0;
   end Is_Ancestor;

   ---------------
   --  Is_Leaf  --
   ---------------

   function Is_Leaf (Node : in Gnode) return Boolean is
      function Internal (Node : in Gnode) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_leaf");
   begin
      return Internal (Node) /= 0;
   end Is_Leaf;

   ---------------
   --  Is_Root  --
   ---------------

   function Is_Root (Node : in Gnode) return Boolean is
      function Internal (Node : in Gnode) return Gboolean;
      pragma Import (C, Internal, "ada_gnode_is_root");
   begin
      return Internal (Node) /= 0;
   end Is_Root;

   ---------------
   --  N_Nodes  --
   ---------------

   function N_Nodes (Root  : in Gnode;
                     Flags : in Glib_Traverse_Flags) return Guint is
      function Internal
        (Root : Gnode; Flags : Glib_Traverse_Flags) return Guint;
      pragma Import (C, Internal, "g_node_n_nodes");

   begin
      return Internal (Root, Flags);
   end N_Nodes;

   ----------------
   -- Gnode_Data --
   ----------------

   package body Gnode_Data is

      function Convert is new Unchecked_Conversion (Element_Access, Gnode);

      ----------------
      --  Glib_New  --
      ----------------

      procedure Glib_New (Node :    out Gnode;
                          Data : in     Element_Access) is
      begin
         Node := Convert (Data);
      end Glib_New;

   end Gnode_Data;

end Glib.Gnodes;
