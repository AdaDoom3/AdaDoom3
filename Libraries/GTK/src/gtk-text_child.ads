-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2006 AdaCore                    --
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
--  A GtkTextChildAnchor is a spot in the buffer where child widgets can be
--  "anchored" (inserted inline, as if they were characters). The anchor can
--  have multiple widgets anchored, to allow for multiple views.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Multiline Text Editor</group>

with Gtk; use Gtk;
with Gtk.Widget;

package Gtk.Text_Child is

   type Gtk_Text_Child_Anchor_Record is new GObject_Record with private;
   type Gtk_Text_Child_Anchor is access all Gtk_Text_Child_Anchor_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Text_Child_Anchor);
   procedure Initialize (Widget : access Gtk_Text_Child_Anchor_Record'Class);
   --  Creates or initializes a Gtk_Text_Child_Anchor widget.
   --  Usually you would then insert it into a Gtk_Text_Buffer with
   --  Gtk.Text_Buffer.Insert_Child_Anchor.
   --  To perform the creation and insertion in one step, use the
   --  convenience function Gtk.Text_Buffer.Create_Child_Anchor.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Text_Child_Anchor.

   function Get_Widgets
     (Anchor : access Gtk_Text_Child_Anchor_Record)
      return Gtk.Widget.Widget_List.Glist;
   --  Return the list of widgets attached at anchor. The returned list should
   --  be freed by the caller.

   function Get_Deleted
     (Anchor : access Gtk_Text_Child_Anchor_Record) return Boolean;
   --  Determines whether a child anchor has been deleted from the buffer. Keep
   --  in mind that the child anchor will be unreferenced when removed from the
   --  buffer, so you need to hold your own reference (with Ref()) if you plan
   --  to use this function; otherwise all deleted child anchors will
   --  also be finalized.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Text_Child_Anchor_Record is new GObject_Record with null record;
   pragma Import (C, Get_Type, "gtk_text_child_anchor_get_type");
end Gtk.Text_Child;
