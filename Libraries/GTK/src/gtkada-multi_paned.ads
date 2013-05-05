-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2003-2013, AdaCore                  --
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
--  This widget implements a multi-paned widget, similar to the standard
--  Gtk_Paned widget, but which can contain several children side to side.
--  This widget can mix vertical and horizontal splits
--  </description>
--  <group>Layout containers</group>
--  <testgtk>create_splittable.adb</testgtk>

with Glib;       use Glib;
with Gdk.Cursor;
with Gdk.GC;
with Gtk.Enums;
with Gtk.Fixed;
with Gtk.Widget;

package Gtkada.Multi_Paned is
   type Gtkada_Multi_Paned_Record is new Gtk.Fixed.Gtk_Fixed_Record
     with private;
   type Gtkada_Multi_Paned is access all Gtkada_Multi_Paned_Record'Class;

   Handle_Width : constant := 6;
   --  Width, in pixels, of the resizing handles.
   --  ??? Should be read from theme with
   --     gtk_widget_style_get (gtk_paned, "handle_size", &handle_size, NULL)

   type Pane is private;
   --  An area of the window, which can is splitted either horizontally or
   --  vertically. It can contain one or several children, next to each other,
   --  or on top of one another.

   Root_Pane : constant Pane;
   --  The root pane. If you split this one, the newly added window will be
   --  next to all other windows. For instance, if you split vertically with
   --  the main_pane the following window, you will get:
   --     +-----+------+        +-----+------+
   --     |  1  |      |        |  1  |      |
   --     +-----+  3   |    =>  +-----+  3   |
   --     |  2  |      |        |  2  |      |
   --     +-----+------+        +-----+------+
   --                           |     4      |
   --                           +------------+

   procedure Gtk_New (Win : out Gtkada_Multi_Paned);
   procedure Initialize (Win : access Gtkada_Multi_Paned_Record'Class);
   --  Create a new paned window.

   procedure Set_Opaque_Resizing
     (Win : access Gtkada_Multi_Paned_Record; Opaque : Boolean);
   --  Whether resizing of the widgets should be opaque or not. The default
   --  is not to do opaque resizing for efficiency reasons

   procedure Add_Child
     (Win           : access Gtkada_Multi_Paned_Record;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation :=
        Gtk.Enums.Orientation_Horizontal;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True);
   --  Add new child, splitting as needed.
   --  This should be used when there is no child yet
   --  The window is splitted in two by default. However, if Width and Height
   --  are specified (or left to -1 for automatic computation), the window is
   --  splitted so that amount of screen space is left to the widget
   --  (leaving some minimum amount of space to other children as needed).
   --  If Fixed_Size is true, then the size of the dock will not change when
   --  Win is resized. Otherwise, it will keep its relative size (x% of the
   --  total size of Win). This Fixed_Size setting will be reset to False
   --  as soon as the user has resized a pane with the mouse.

   procedure Split
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True);
   --  Split the pane containing Ref_Widget, and add New_Child
   --  in the new pane (on the right or at the bottom if After is True, on the
   --  left or at the top if After is False).

   procedure Set_Size
     (Win           : access Gtkada_Multi_Paned_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Width, Height : Glib.Gint := -1;
      Fixed_Size    : Boolean := False);
   --  Force a specific size for Widget

   function Splitted_Area
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      After         : Boolean := True) return Gtk.Widget.Gtk_Widget;
   --  Return the widget in the splitted area next to Ref_Widget if any exist.
   --  Orientation and After define which splitted area we are looking at.
   --  null is returned if there are no such splitted area.

   function Get_Pane
     (Win    : access Gtkada_Multi_Paned_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Pane;
   function Get_Pane (Current_Pane : Pane) return Pane;
   --  Return the pane that contains the widget. See comment for Split below.

   procedure Split
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Pane      : Pane;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True);
   --  Split Ref_Pane to display New_Child to one of its sides.
   --  See the comments for Root_Pane above.
   --  The examples below assume that you are using one of the two split
   --  procedures, either with a Ref_Pane or a Ref_Widget. In the former case,
   --  the pane is obtained with a call to Get_Pane(Ref_Widget).
   --  As you will see, the results are different (although they might appear
   --  similar sometimes on this simple example.
   --  In all these examples, we split either vertically or horizontally, and
   --  add a new widget "4".
   --
   --  Given the following setup:
   --     +---+---+
   --     | 1 |   |
   --     +---+ 3 |
   --     | 2 |   |
   --     +---+---+
   --
   --  Ref_Pane = Get_Pane ("1")              Ref_Widget = "1"
   --  Split vertically
   --    After=True  After=False        After=True   After=False
   --    +---+---+   +---+---+          +---+---+    +---+---+
   --    | 1 | 3 |   | 4 | 3 |          | 1 | 3 |    | 4 | 3 |
   --    +---+   |   +---+   |          +---+   |    +---+   |
   --    | 2 |   |   | 1 |   |          | 4 |   |    | 1 |   |
   --    +---+   |   +---+   |          +---+   |    +---+   |
   --    | 4 |   |   | 2 |   |          | 2 |   |    | 2 |   |
   --    +---+---+   +---+---+          +---+---+    +---+---+
   --
   --  Split horizontally
   --    After=True     After=False     After=True     After=False
   --    +---+---+---+  +---+---+---+   +---+---+---+  +---+---+---+
   --    | 1 | 4 | 3 |  | 4 | 1 | 3 |   | 1 | 4 | 3 |  | 4 | 1 | 3 |
   --    +---+   |   |  |   +---+   |   +---+---+   |  +---+---+   |
   --    | 2 |   |   |  |   | 2 |   |   |   2   |   |  |   2   |   |
   --    +---+---+---+  +---+---+---+   +-------+---+  +-------+---+
   --
   --
   --  Ref_Pane = Get_Pane ("3")             Ref_Widget = "3"
   --  Split vertically
   --    After=True   After=False       After=True     After=False
   --    +---+---+    +-------+         +---+---+      +---+---+
   --    | 1 | 3 |    |   4   |         | 1 | 3 |      | 1 | 4 |
   --    +---+   |    +---+---+         +---+---+      +---+---+
   --    | 2 |   |    | 1 | 3 |         | 2 | 4 |      | 2 | 3 |
   --    +---+---+    +---+   |         +---+---+      +---+---+
   --    |   4   |    | 2 |   |
   --    +-------+    +---+---+
   --
   --  Split horizontally
   --    After=True     After=False     After=True     After=False
   --    +---+---+---+  +---+---+---+   +---+---+---+  +---+---+---+
   --    | 1 | 3 | 4 |  | 4 | 1 | 3 |   | 1 | 3 | 4 |  | 1 | 4 | 3 |
   --    +---+   |   |  |   +---+   |   +---+   |   |  +---+   |   |
   --    | 2 |   |   |  |   | 2 |   |   | 2 |   |   |  | 2 |   |   |
   --    +---+---+---+  +---+---+---+   +---+---+---+  +---+---+---+

   procedure Freeze (Win : access Gtkada_Multi_Paned_Record);
   --  Freeze the window, ie when a child is inserted, no computation of its
   --  size is done, and will not generate immediate resizing.
   --  You only need to call this procedure when restoring Win to a previously
   --  state saved, and never if you are using the GtkAda.MDI which takes care
   --  of it on its own.

   procedure Thaw (Win : access Gtkada_Multi_Paned_Record);
   --  Opposite of Freeze. You should call Size_Allocate on Win afterward to
   --  force a recomputation of the size

   ---------------
   -- Iterators --
   ---------------

   type Child_Iterator is private;

   function Start
     (Win : access Gtkada_Multi_Paned_Record) return Child_Iterator;
   --  Return an iterator to the first child of the window. This also returns
   --  children which are not widget, but are used to organize the window into
   --  horizontal and vertical panes

   function At_End (Iter : Child_Iterator) return Boolean;
   --  True if there is no more child to be returned

   procedure Next (Iter : in out Child_Iterator);
   --  Move to the next child of Iterator

   function Get_Widget (Iter : Child_Iterator) return Gtk.Widget.Gtk_Widget;
   --  Return the widget embedded in the current child. This returns null if
   --  the current child is only used as a pane separator (horizontal or
   --  vertical). You mustn't remove the widget from the paned widget, or the
   --  iterator becomes invalid.

   function Get_Orientation
     (Iter : Child_Iterator) return Gtk.Enums.Gtk_Orientation;
   --  Return the orientation of the current child. This is only relevant if
   --  the child doesn't contain a widget (and therefore Get_Widget has
   --  returned null).

   function Get_Depth (Iter : Child_Iterator) return Natural;
   --  Return the depth of the current child (0 means the child is at the
   --  toplevel, 1 that this is a child directly underneath,...).
   --  This can be used to detect when the Iter has finished traversing one
   --  of the panes.

   procedure Get_Size
     (Iter                        : Child_Iterator;
      Width, Height               : out Gint;
      Parent_Width, Parent_Height : out Gint;
      Parent_Orientation          : out Gtk.Enums.Gtk_Orientation);
   --  Return the size of the current element (pane or widget), as well as the
   --  parent's pane (the resizable area that contains notebooks or other
   --  panes). The parent size is the total size devoted to its children,
   --  omitting the size occupied by resize handles.

   procedure Dump (Split : access Gtkada_Multi_Paned_Record'Class);
   --  Dump the configuration of Split to stdout. This is only intended for
   --  testing purposes. If you want to save and restore this configuration,
   --  you should look at Gtkada.MDI instead, which contains all the
   --  subprograms needed to handle desktops.

private
   type Child_Description;
   type Child_Description_Access is access Child_Description;

   type Pane is new Child_Description_Access;
   Root_Pane : constant Pane := null;

   type Child_Iterator is record
      Current : Child_Description_Access;
      Depth   : Natural := 0;
   end record;

   type Gtkada_Multi_Paned_Record is new Gtk.Fixed.Gtk_Fixed_Record with record
      Frozen      : Boolean := False;
      Children    : Child_Description_Access;
      GC          : Gdk.GC.Gdk_GC;

      Initial_Pos  : Gint;
      Selected     : Child_Description_Access;
      Selected_Pos : Gtk.Widget.Gtk_Allocation;

      Cursor_Double_H_Arrow : Gdk.Cursor.Gdk_Cursor;
      Cursor_Double_V_Arrow : Gdk.Cursor.Gdk_Cursor;

      Opaque_Resizing        : Boolean := False;
   end record;

end Gtkada.Multi_Paned;
