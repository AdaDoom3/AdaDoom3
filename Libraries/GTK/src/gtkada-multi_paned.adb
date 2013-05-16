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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.IO;              use GNAT.IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;
with System.Address_Image;

with Gdk.Cursor;           use Gdk, Gdk.Cursor;
with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.GC;               use Gdk.GC;
with Gdk.Main;             use Gdk.Main;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Window;           use Gdk.Window;
with Gdk.Window_Attr;      use Gdk.Window_Attr;

with Glib.Object;          use Glib.Object;
with Glib.Types;           use Glib.Types;

with Gtk.Arguments;        use Gtk.Arguments;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Fixed;            use Gtk.Fixed;
with Gtk.Notebook;         use Gtk.Notebook;
with Gtk.Style;            use Gtk.Style;
with Gtk.Widget;           use Gtk.Widget;

with Gtkada.Handlers;      use Gtkada.Handlers;

package body Gtkada.Multi_Paned is

   Traces : constant Boolean := False;
   --  Whether debug traces should be displayed on stdout

   Minimum_Width : constant := 1;
   --  Minimum width for a child

   Paned_Class_Record : Glib.Object.GObject_Class :=
     Glib.Object.Uninitialized_Class;

   type Resize_Handle is record
      Position : Gtk.Widget.Gtk_Allocation;
      Win      : Gdk.Window.Gdk_Window;
   end record;
   No_Handle : constant Resize_Handle := ((0, 0, 0, 0), null);

   type Child_Description (Is_Widget : Boolean) is record
      Parent : Child_Description_Access;
      Next   : Child_Description_Access;
      Width, Height : Float;
      --  Either the size request for the widget, or the allocated size

      Visible : Boolean := True;
      --  Visibility status the last time we computed the size request

      Handle : Resize_Handle;
      --  The handle following the child. This might be invisible when the
      --  child is the last in its parent

      case Is_Widget is
         when True  =>
            Widget      : Gtk.Widget.Gtk_Widget;
            Fixed_Size  : Boolean;
         when False =>
            Orientation : Gtk.Enums.Gtk_Orientation;
            First_Child : Child_Description_Access;
            X, Y        : Gint;  --  Location of the top-left corner
      end case;
   end record;

   procedure Free
     (Child     : in out Child_Description_Access;
      Recursive : Boolean);
   --  Free Child, but not its Next or parent nodes.
   --  If Recursive is True, the children of Child are also destroyed

   procedure Size_Allocate_Paned
     (Paned : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_Paned);
   --  Window was resized, need to resize and reposition the children.

   procedure Size_Allocate
     (Split         : access Gtkada_Multi_Paned_Record'Class;
      Current       : Child_Description_Access;
      Width, Height : Float);
   --  Handle the size allocation for a specific pane. The actual
   --  position are given by Current.X and Current.Y

   procedure Size_Request (Current : Child_Description_Access);
   --  Compute the size requisition for a specific type of children.
   --  Sets Current.Width and Current.Height appropriately.
   --  Children that have a size_request of 0 do not count in the overall size
   --  request (will be taken into account at Size_Allocate). Neither does
   --  their handle.

   procedure Set_Size_Request
     (Current : Child_Description_Access; Width, Height : Float);
   --  Set the size requisition to use for Current and its children.
   --  This assumes however that Size_Allocate has been called at least once
   --  before, or panes with 0 width or height will not be handled properly.

   procedure Realize_Paned (Paned : access Gtk_Widget_Record'Class);
   --  Called when the window was realized.

   procedure Set_Handle_Cursor
     (Split   : access Gtkada_Multi_Paned_Record'Class;
      Current : Child_Description_Access);
   --  Reset the cursor used for the mouse when it is over the handle for
   --  Current.

   function Count_Children (Current : Child_Description_Access) return Gint;
   --  Return the number of children Current has

   procedure Resize_Child_And_Siblings
     (Parent       : Child_Description_Access;
      Child        : Child_Description_Access);
   --  Resize a child and its neighbors to take into account the new position
   --  of the handle associated with Child. This handle's position must
   --  have been changed before calling this subprogram.

   function Expose_Paned
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Redraw all the handles

   procedure Move_Handle
     (Split   : access Gtkada_Multi_Paned_Record'Class;
      Current : Child_Description_Access);
   --  Move the window associated with a given handle to its position.
   --  If Show is False, then the handle is hidden

   function Button_Pressed
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Released
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Motion
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;

   procedure Remove_Child
     (Paned : access Gtk_Widget_Record'Class;
      Args  : Gtk_Args);
   --  A child was removed from Paned

   procedure Remove_Child
      (Split : access Gtkada_Multi_Paned_Record'Class;
       Pane : Child_Description_Access);
   --  Remove a specific pane

   procedure Draw_Resize_Line (Split : access Gtkada_Multi_Paned_Record'Class);
   --  Draw, in xor mode, the resizing line

   procedure Destroy_Paned
     (Paned : access Gtk_Widget_Record'Class);
   --  The Paned window is being destroyed.

   function Get (Iter : Child_Iterator) return Child_Description_Access;
   --  Return the current child. You must move to Next before destroying
   --  the returned value, if you need to.
   --  Null is returned when there are no more children.

   procedure Split_Internal
     (Win         : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget  : Gtk_Widget;
      Ref_Pane    : Pane;
      Use_Ref_Pane  : Boolean;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Fixed_Size  : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After       : Boolean := True);
   --  Internal version of Split_Horizontal and Split_Vertical.
   --  If Use_Ref_Pane is true, then all split are done with regards to
   --  Ref_Pane, otherwise they are done relative to Ref_Widget.

   function Is_Visible (Child : Child_Description_Access) return Boolean;
   --  Return True if Child is visible (or if any of its children is visible).

   function Is_Last_Visible
     (Current : Child_Description_Access) return Boolean;
   --  Return True if Current is the right-most visible child in its parent

   procedure Dump
     (Split  : access Gtkada_Multi_Paned_Record'Class;
      Child  : Child_Description_Access;
      Prefix : String := "");
   --  Dump to stdout the status of the multipaned

   ----------
   -- Dump --
   ----------

   procedure Dump (Split : access Gtkada_Multi_Paned_Record'Class) is
   begin
      Dump (Split, Split.Children);
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Child : Child_Description_Access;
      Prefix : String := "")
   is
      Tmp : Child_Description_Access;

      function Image (Orient : Gtk_Orientation) return String;
      --  Return the string to display for Orient

      function Image (Str : String; Value : Boolean) return String;
      --  Return Str if Value is True

      function Image (Value : Gint) return String;
      --  Return Value, without the leading space

      function Image (Value : Gint) return String is
         S : constant String := Gint'Image (Value);
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Image;

      function Image (Orient : Gtk_Orientation) return String is
      begin
         case Orient is
            when Orientation_Horizontal => return "horiz";
            when Orientation_Vertical   => return "vert";
         end case;
      end Image;

      function Image (Str : String; Value : Boolean) return String is
      begin
         if Value then
            return Str & " ";
         else
            return "";
         end if;
      end Image;

   begin
      if Child = null then
         Put_Line ("<null>");

      elsif Child.Is_Widget then
         Put (Prefix & "<w req=("
              & Image (Gint (Child.Width))
              & Gint'Image (Gint (Child.Height))
              & ") alloc=("
              & Image (Get_Allocation_Width (Child.Widget))
              & Gint'Image (Get_Allocation_Height (Child.Widget))
              & ") "
              & Image ("HIDDEN", not Child.Visible)
              & Image ("FIXED", Child.Fixed_Size)
              & Image ("NoHandle", Child.Handle.Win = null));

         if Child.Widget.all in Gtk_Notebook_Record'Class then
            Put ("pages="
                 & Image (Get_N_Pages (Gtk_Notebook (Child.Widget)))
                 & " ");
         end if;

         Put_Line ("handle=("
                   & Image (Child.Handle.Position.X)
                   & Gint'Image (Child.Handle.Position.Y)
                   & ") w=" & System.Address_Image (Child.Widget.all'Address)
                   & " C=" & System.Address_Image (Get_Object (Child.Widget))
                   & ">");

         if Child.Widget.all in Gtkada_Multi_Paned_Record'Class then
            Dump (Gtkada_Multi_Paned (Child.Widget),
                  Gtkada_Multi_Paned (Child.Widget).Children,
                  Prefix & "  nested:");
         end if;

      else
         Put_Line (Prefix & "<" & Image (Child.Orientation)
                   & " req=(" & Image (Gint (Child.Width))
                   & Image (Gint (Child.Height))
                   & ") x,y=(" & Image (Child.X) & Gint'Image (Child.Y) & ")"
                   & " handle=("
                   & Image (Child.Handle.Position.X)
                   & Gint'Image (Child.Handle.Position.Y)
                   & ")>");
         Tmp := Child.First_Child;
         while Tmp /= null loop
            Dump (Split, Tmp, Prefix & "  ");
            Tmp := Tmp.Next;
         end loop;
      end if;
   end Dump;

   -------------------------
   -- Set_Opaque_Resizing --
   -------------------------

   procedure Set_Opaque_Resizing
     (Win : access Gtkada_Multi_Paned_Record; Opaque : Boolean)
   is
   begin
      Win.Opaque_Resizing := Opaque;
   end Set_Opaque_Resizing;

   ----------
   -- Free --
   ----------

   procedure Free
     (Child     : in out Child_Description_Access;
      Recursive : Boolean)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Child_Description, Child_Description_Access);
      Tmp : Child_Description_Access;
   begin
      if Child /= null then
         if Child.Handle.Win /= null then
            Destroy (Child.Handle.Win);
         end if;

         if Recursive and then not Child.Is_Widget then
            while Child.First_Child /= null loop
               Tmp := Child.First_Child.Next;
               Free (Child.First_Child, Recursive);
               Child.First_Child := Tmp;
            end loop;
         end if;

         Unchecked_Free (Child);
      end if;
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Win : access Gtkada_Multi_Paned_Record) return Child_Iterator is
   begin
      return (Current => Win.Children, Depth => 0);
   end Start;

   ---------
   -- Get --
   ---------

   function Get (Iter : Child_Iterator) return Child_Description_Access is
   begin
      return Iter.Current;
   end Get;

   ---------------
   -- Get_Depth --
   ---------------

   function Get_Depth (Iter : Child_Iterator) return Natural is
   begin
      return Iter.Depth;
   end Get_Depth;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Iter                        : Child_Iterator;
      Width, Height               : out Gint;
      Parent_Width, Parent_Height : out Gint;
      Parent_Orientation          : out Gtk_Orientation)
   is
      Count : Gint := 0;
      Tmp   : Child_Description_Access;
   begin
      --  Assert (Iter.Current /= null);

      Width := Gint (Iter.Current.Width);
      Height := Gint (Iter.Current.Height);

      if Iter.Current.Parent /= null then
         Tmp := Iter.Current.Parent.First_Child;
         while Tmp /= null loop
            Count := Count + 1;
            Tmp := Tmp.Next;
         end loop;

         Parent_Orientation := Iter.Current.Parent.Orientation;

         case Parent_Orientation is
            when Orientation_Horizontal =>
               Parent_Width  := Gint (Iter.Current.Parent.Width)
                 - Count * Handle_Width;
               Parent_Height := Gint (Iter.Current.Parent.Height);
            when Orientation_Vertical =>
               Parent_Width  := Gint (Iter.Current.Parent.Width);
               Parent_Height := Gint (Iter.Current.Parent.Height)
                 - Count * Handle_Width;
         end case;

      else
         Parent_Width       := Width;
         Parent_Height      := Height;
         Parent_Orientation := Orientation_Horizontal;
      end if;
   end Get_Size;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Child_Iterator) return Boolean is
   begin
      return Iter.Current = null;
   end At_End;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Iter : Child_Iterator) return Gtk.Widget.Gtk_Widget is
   begin
      if Iter.Current /= null and then Iter.Current.Is_Widget then
         return Iter.Current.Widget;
      else
         return null;
      end if;
   end Get_Widget;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Iter : Child_Iterator) return Gtk.Enums.Gtk_Orientation is
   begin
      if Iter.Current /= null and then not Iter.Current.Is_Widget then
         return Iter.Current.Orientation;
      else
         return Orientation_Horizontal;
      end if;
   end Get_Orientation;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Child_Iterator) is
   begin
      if Iter.Current = null then
         null;
      elsif not Iter.Current.Is_Widget
        and then Iter.Current.First_Child /= null
      then
         Iter.Current := Iter.Current.First_Child;
         Iter.Depth   := Iter.Depth + 1;
      else
         while Iter.Current /= null
           and then Iter.Current.Next = null
         loop
            Iter.Current := Iter.Current.Parent;

            if Iter.Current /= null then
               Iter.Depth   := Iter.Depth - 1;
            end if;
         end loop;

         if Iter.Current /= null then
            Iter.Current := Iter.Current.Next;
         end if;
      end if;
   end Next;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Win : out Gtkada_Multi_Paned) is
   begin
      Win := new Gtkada_Multi_Paned_Record;
      Gtkada.Multi_Paned.Initialize (Win);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Win : access Gtkada_Multi_Paned_Record'Class) is
   begin
      Gtk.Fixed.Initialize (Win);
      Glib.Object.Initialize_Class_Record
        (Win,
         Signals      => (1 .. 0 => Null_Ptr),
         Class_Record => Paned_Class_Record,
         Type_Name    => "GtkAdaMultiPaned",
         Parameters   => (1 .. 0 => (1 => GType_None)));

      Set_Default_Size_Allocate_Handler
        (Paned_Class_Record, Size_Allocate_Paned'Access);

      Widget_Callback.Connect
        (Win, "realize",
         Widget_Callback.To_Marshaller (Realize_Paned'Access));
      Return_Callback.Connect
        (Win, "expose_event",
         Return_Callback.To_Marshaller (Expose_Paned'Access));
      Return_Callback.Connect
        (Win, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Win, "button_release_event",
         Return_Callback.To_Marshaller (Button_Released'Access));
      Return_Callback.Connect
        (Win, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Widget_Callback.Connect (Win, "remove", Remove_Child'Access);
      Widget_Callback.Connect
        (Win, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Paned'Access));
   end Initialize;

   ------------------------
   -- Destroy_Paned --
   ------------------------

   procedure Destroy_Paned
     (Paned : access Gtk_Widget_Record'Class)
   is
      use type Widget_List.Glist;
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
   begin
      --  Destruction of children would be done automatically by the default
      --  "destroy" handler of the ancestor of Gtkada_Multi_Paned (ie GtkFixed)

      Free (Split.Children, Recursive => True);

      if Split.GC /= null then
         Unref (Split.GC);
      end if;
   end Destroy_Paned;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Paned : access Gtk_Widget_Record'Class;
      Args  : Gtk_Args)
   is
      Split   : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Child   : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      Iter    : Child_Iterator := Start (Split);
      Current : Child_Description_Access;
   begin
      --  Split might have been destroyed as part of the global interface, in
      --  which case its children are destroyed only after Split itself.
      if Split.Children = null then
         return;
      end if;

      loop
         Current := Get (Iter);

         exit when Current = null
           or else (Current.Is_Widget and then Current.Widget = Child);
         Next (Iter);
      end loop;

      if Current /= null then
         Remove_Child (Split, Current);
      end if;
   end Remove_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
      (Split : access Gtkada_Multi_Paned_Record'Class;
       Pane : Child_Description_Access)
   is
      procedure Merge_With_Parent_If_Single_Child
        (Child : in out Child_Description_Access);
      --  If Child has a single child itself, merge it with its parent, and
      --  free Child. Child is set to its first child if they are the same.

      procedure Merge_With_Parent_If_Same
        (Child : in out Child_Description_Access);
      --  Merge Child with its parent if they have the same orientation

      ---------------------------------------
      -- Merge_With_Parent_If_Single_Child --
      ---------------------------------------

      procedure Merge_With_Parent_If_Single_Child
        (Child : in out Child_Description_Access)
      is
         Tmp : Child_Description_Access;
      begin
         if Child.First_Child /= null
           and then Child.First_Child.Next = null
         then
            if Child.Parent /= null then
               Child.First_Child.Parent := Child.Parent;
               Set_Handle_Cursor (Split, Child.First_Child);

               if Child.Parent.First_Child = Child then
                  Child.Parent.First_Child := Child.First_Child;
                  Child.First_Child.Next := Child.Next;
               else
                  Tmp := Child.Parent.First_Child;
                  while Tmp.Next /= Child loop
                     Tmp := Tmp.Next;
                  end loop;

                  Child.First_Child.Next := Child.Next;
                  Tmp.Next := Child.First_Child;
               end if;

               Tmp := Child;
               Child := Child.First_Child;
               Free (Tmp, Recursive => False);
            elsif not Child.First_Child.Is_Widget then
               Split.Children := Child.First_Child;
               Split.Children.Parent := null;
               Free (Child, Recursive => False);
               Child := Split.Children;
            end if;
         end if;
      end Merge_With_Parent_If_Single_Child;

      -------------------------------
      -- Merge_With_Parent_If_Same --
      -------------------------------

      procedure Merge_With_Parent_If_Same
        (Child : in out Child_Description_Access)
      is
         Tmp, Previous : Child_Description_Access;
      begin
         if not Child.Is_Widget
           and then Child.Parent /= null
           and then Child.Orientation = Child.Parent.Orientation
         then
            Tmp := Child.First_Child;
            while Tmp /= null loop
               Tmp.Parent := Child.Parent;
               Previous := Tmp;
               Tmp := Tmp.Next;
            end loop;

            --  Previous is now the last child of Child

            if Previous /= null then
               if Child.Parent.First_Child = Child then
                  Previous.Next := Child.Next;
                  Child.Parent.First_Child := Child.First_Child;
               else
                  Tmp := Child.Parent.First_Child;
                  while Tmp.Next /= Child loop
                     Tmp := Tmp.Next;
                  end loop;

                  Previous.Next := Tmp.Next.Next;
                  Tmp.Next := Child.First_Child;
               end if;
            end if;

            Free (Child, Recursive => False);
         end if;
      end Merge_With_Parent_If_Same;

      Current : Child_Description_Access := Pane;
      Tmp, Parent : Child_Description_Access;
   begin
      if Traces then
         if Pane.Is_Widget then
            Put_Line ("## Removing widget "
                      & System.Address_Image (Pane.Widget.all'Address));
         else
            Put_Line ("## Removing pane");
         end if;
      end if;

      if Current /= null then
         Parent := Current.Parent;

         if Parent.First_Child = Current then
            Parent.First_Child := Current.Next;
         else
            Tmp := Parent.First_Child;
            while Tmp.Next /= Current loop
               Tmp := Tmp.Next;
            end loop;
            Tmp.Next := Current.Next;
         end if;

         Free (Current, Recursive => False);

         Merge_With_Parent_If_Single_Child (Parent);
         Merge_With_Parent_If_Same (Parent);

         if Parent /= null
            and then not Parent.Is_Widget
            and then Parent.First_Child = null
         then
            if Parent.Parent /= null then
               Remove_Child (Split, Parent);
            else
               Free (Parent, Recursive => False);
               Split.Children := null;
            end if;
         end if;

         if Split.Children /= null
            and then Realized_Is_Set (Split)
            and then Split.Children.Width  > 0.0
            and then Split.Children.Height > 0.0
         then
            --  We have to redo some of the work done by Size_Allocate_Paned.
            --  The reason is that the latter will do nothing since the
            --  allocation has changed, but we might have changed the top-level
            --  Child, and need to reset the allocation properties anyway

            if Get_Has_Window (Split) then
               Split.Children.X := 0;
               Split.Children.Y := 0;
            else
               Split.Children.X := Get_Allocation_X (Split);
               Split.Children.Y := Get_Allocation_Y (Split);
            end if;

            Size_Allocate (Split, Split.Children,
                           Float (Get_Allocation_Width (Split)),
                           Float (Get_Allocation_Height (Split)));
         else
            Queue_Resize (Split);
         end if;
      end if;

      if Traces then
         Put_Line ("After Remove_Child");
         Dump (Split, Split.Children);
      end if;
   end Remove_Child;

   ----------------------
   -- Draw_Resize_Line --
   ----------------------

   procedure Draw_Resize_Line
     (Split : access Gtkada_Multi_Paned_Record'Class) is
   begin
      if not Split.Opaque_Resizing then
         Draw_Line
           (Get_Window (Split),
            Split.GC, Split.Selected_Pos.X,
            Split.Selected_Pos.Y,
            Split.Selected_Pos.X + Split.Selected_Pos.Width,
            Split.Selected_Pos.Y + Split.Selected_Pos.Height);
      end if;
   end Draw_Resize_Line;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Split   : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Tmp     : Gdk_Grab_Status;
      Cursor  : Gdk_Cursor;
      Iter    : Child_Iterator := Start (Split);
      Current : Child_Description_Access;
      pragma Unreferenced (Tmp);
   begin
      if Get_Button (Event) /= 1 then
         return False;
      end if;

      Split.Selected := null;

      while Get (Iter) /= null loop
         Current := Get (Iter);
         if Current.Handle.Win = Get_Window (Event) then
            Split.Selected := Current;
            exit;
         end if;

         Next (Iter);
      end loop;

      if Traces then
         Put_Line ("Button_Pressed in multi_panned has_selected_handle="
                   & Boolean'Image (Split.Selected /= null));
      end if;

      if Split.Selected = null then
         return False;
      end if;

      --  Make sure none of the widgets has a fixed size, or
      --  the resizing won't take place.
      if Current.Is_Widget then
         Current.Fixed_Size := False;
      end if;

      if Current.Next /= null and then Current.Next.Is_Widget then
         Current.Next.Fixed_Size := False;
      end if;

      Split.Selected_Pos := Split.Selected.Handle.Position;

      case Split.Selected.Parent.Orientation is
         when Orientation_Vertical =>
            if Split.Cursor_Double_V_Arrow = null then
               Gdk_New (Split.Cursor_Double_V_Arrow, Sb_V_Double_Arrow);
            end if;
            Cursor := Split.Cursor_Double_V_Arrow;
            Split.Selected_Pos.Height := 0;
            Split.Selected_Pos.Y :=
              Split.Selected_Pos.Y + Gint (Get_Y (Event));
            Split.Initial_Pos := Split.Selected.Handle.Position.Y
              - Gint (Get_Y_Root (Event));
         when Orientation_Horizontal =>
            if Split.Cursor_Double_H_Arrow = null then
               Gdk_New (Split.Cursor_Double_H_Arrow, Sb_H_Double_Arrow);
            end if;
            Cursor := Split.Cursor_Double_H_Arrow;
            Split.Selected_Pos.Width := 0;
            Split.Selected_Pos.X :=
              Split.Selected_Pos.X + Gint (Get_X (Event));
            Split.Initial_Pos := Split.Selected.Handle.Position.X
              - Gint (Get_X_Root (Event));
      end case;

      Tmp := Pointer_Grab
        (Get_Window (Event),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time   => 0);

      Draw_Resize_Line (Split);
      return False;
   end Button_Pressed;

   -------------------------------
   -- Resize_Child_And_Siblings --
   -------------------------------

   procedure Resize_Child_And_Siblings
     (Parent       : Child_Description_Access;
      Child        : Child_Description_Access)
   is
      Size         : Gint;
      Count        : Natural := 0;
      Tmp          : Child_Description_Access := Parent.First_Child;
      Handle_Index : Natural := 0;
   begin
      --  We'll need to traverse the list of children in reverse order,
      --  so we store it in an array for ease of use
      while Tmp /= null loop
         Count := Count + 1;
         Tmp := Tmp.Next;
      end loop;

      declare
         Children : array (1 .. Count) of Child_Description_Access;
      begin
         Tmp := Parent.First_Child;
         Count := Children'First;
         while Tmp /= null loop
            Children (Count) := Tmp;
            if Tmp = Child then
               Handle_Index := Count;
            end if;
            Count := Count + 1;
            Tmp := Tmp.Next;
         end loop;

         --  Now adjust the size
         if Parent.Orientation = Orientation_Horizontal then
            for H in reverse Children'First .. Handle_Index loop
               if H - 1 >= Children'First then
                  Size := Children (H).Handle.Position.X
                    - Children (H - 1).Handle.Position.X - Handle_Width;
               else
                  Size := Children (H).Handle.Position.X - Parent.X;
               end if;
               if Size >= Minimum_Width then
                  Set_Size_Request
                    (Children (H), Float (Size), Children (H).Height);
                  exit;
               else
                  Set_Size_Request
                    (Children (H), Float (Minimum_Width), Children (H).Height);
                  if H - 1 >= Children'First then
                     Children (H - 1).Handle.Position.X :=
                       Children (H).Handle.Position.X
                       - Gint (Children (H).Width) - Handle_Width;
                  end if;
               end if;
            end loop;

            for H in Handle_Index + 1 .. Children'Last loop
               Size := Children (H).Handle.Position.X
                 - Children (H - 1).Handle.Position.X - Handle_Width;
               Children (H).Handle.Position.X :=
                 Children (H - 1).Handle.Position.X + Gint (Children (H).Width)
                 + Handle_Width;
               if Size >= Minimum_Width then
                  Set_Size_Request
                    (Children (H), Float (Size), Children (H).Height);
                  exit;
               else
                  Set_Size_Request
                    (Children (H), Float (Minimum_Width),
                     Children (H).Height);
                  Children (H).Handle.Position.X :=
                    Children (H - 1).Handle.Position.X + Minimum_Width
                    + Handle_Width;
               end if;
            end loop;

         else --  Orientation_Vertical
            for H in reverse Children'First .. Handle_Index loop
               if H - 1 >= Children'First then
                  Size := Children (H).Handle.Position.Y
                    - Children (H - 1).Handle.Position.Y - Handle_Width;
               else
                  Size := Children (H).Handle.Position.Y - Parent.Y;
               end if;
               if Size >= Minimum_Width then
                  Set_Size_Request
                    (Children (H), Children (H).Width, Float (Size));
                  exit;
               else
                  Set_Size_Request
                    (Children (H), Children (H).Width, Float (Minimum_Width));
                  if H - 1 >= Children'First then
                     Children (H - 1).Handle.Position.Y :=
                       Children (H).Handle.Position.Y
                       - Gint (Children (H).Height) - Handle_Width;
                  end if;
               end if;
            end loop;

            for H in Handle_Index + 1 .. Children'Last loop
               Size := Children (H).Handle.Position.Y
                 - Children (H - 1).Handle.Position.Y - Handle_Width;
               Children (H).Handle.Position.Y :=
                 Children (H - 1).Handle.Position.Y
                 + Gint (Children (H).Height) + Handle_Width;
               if Size >= Minimum_Width then
                  Set_Size_Request
                    (Children (H), Children (H).Width, Float (Size));
                  exit;
               else
                  Set_Size_Request
                    (Children (H), Children (H).Width, Float (Minimum_Width));
                  Children (H).Handle.Position.Y :=
                    Children (H - 1).Handle.Position.Y + Minimum_Width
                    + Handle_Width;
               end if;
            end loop;

         end if;
      end;
   end Resize_Child_And_Siblings;

   ---------------------
   -- Button_Released --
   ---------------------

   function Button_Released
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
   begin
      if Split.Selected /= null then
         Draw_Resize_Line (Split);
         Pointer_Ungrab (Time => 0);

         case Split.Selected.Parent.Orientation is
            when Orientation_Horizontal =>
               Split.Selected.Handle.Position.X := Split.Selected_Pos.X;
            when Orientation_Vertical =>
               Split.Selected.Handle.Position.Y := Split.Selected_Pos.Y;
         end case;

         Resize_Child_And_Siblings (Split.Selected.Parent, Split.Selected);
         Size_Allocate (Split,
                        Split.Selected.Parent,
                        Split.Selected.Parent.Width,
                        Split.Selected.Parent.Height);

         if Traces then
            Put_Line ("After button_release.size_allocate");
            Dump (Split, Split.Children);
         end if;

         Split.Selected := null;
      end if;
      return False;
   end Button_Released;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      New_Pos : Gint;
   begin
      if Split.Selected /= null then
         Draw_Resize_Line (Split);

         case Split.Selected.Parent.Orientation is
            when Orientation_Horizontal =>
               New_Pos := Gint (Get_X_Root (Event)) + Split.Initial_Pos;
               if New_Pos >= Split.Selected.Parent.X
                 and then New_Pos <= Split.Selected.Parent.X
                   + Gint (Split.Selected.Parent.Width)
               then
                  Split.Selected_Pos.X := New_Pos;
               end if;
            when Orientation_Vertical =>
               New_Pos := Gint (Get_Y_Root (Event)) + Split.Initial_Pos;
               if New_Pos >= Split.Selected.Parent.Y
                 and then New_Pos <= Split.Selected.Parent.Y
                   + Gint (Split.Selected.Parent.Height)
               then
                  Split.Selected_Pos.Y := New_Pos;
               end if;
         end case;

         if Split.Opaque_Resizing then
            case Split.Selected.Parent.Orientation is
               when Orientation_Horizontal =>
                  Split.Selected.Handle.Position.X := Split.Selected_Pos.X;
               when Orientation_Vertical =>
                  Split.Selected.Handle.Position.Y := Split.Selected_Pos.Y;
            end case;
            Resize_Child_And_Siblings (Split.Selected.Parent, Split.Selected);

            Size_Allocate (Split,
                           Split.Selected.Parent,
                           Split.Selected.Parent.Width,
                           Split.Selected.Parent.Height);
         end if;

         Draw_Resize_Line (Split);
      end if;
      return False;
   end Button_Motion;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Child : Child_Description_Access) return Boolean is
      Tmp     : Child_Description_Access;
   begin
      if Child.Is_Widget then
         return Get_Child_Visible (Child.Widget)
           and then Visible_Is_Set (Child.Widget);
      else
         Tmp := Child.First_Child;
         while Tmp /= null loop
            if Is_Visible (Tmp) then
               return True;
            end if;
            Tmp := Tmp.Next;
         end loop;
         return False;
      end if;
   end Is_Visible;

   ---------------------
   -- Is_Last_Visible --
   ---------------------

   function Is_Last_Visible
     (Current : Child_Description_Access) return Boolean
   is
      Tmp : Child_Description_Access := Current.Next;
   begin
      while Tmp /= null loop
         if Tmp.Visible then
            return False;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return True;
   end Is_Last_Visible;

   -----------------------
   -- Expose_Paned --
   -----------------------

   function Expose_Paned
     (Paned : access Gtk_Widget_Record'Class;
      Event      : Gdk_Event) return Boolean
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Area  : constant Gdk_Rectangle := Get_Area (Event);
      Iter        : Child_Iterator := Start (Split);
      Current     : Child_Description_Access;
      Orientation : Gtk_Orientation;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Is_Last_Visible (Current)
           and then Current.Visible
         then
            case Current.Parent.Orientation is
               when Orientation_Vertical =>
                  Orientation := Orientation_Horizontal;
               when Orientation_Horizontal =>
                  Orientation := Orientation_Vertical;
            end case;

            Paint_Box
              (Get_Style (Split),
               Get_Window (Split),
               State_Normal,
               Shadow_None,
               Area,
               Split,
               X           => Current.Handle.Position.X,
               Y           => Current.Handle.Position.Y,
               Width       => Current.Handle.Position.Width,
               Height      => Current.Handle.Position.Height);

            Paint_Handle
              (Get_Style (Split),
               Get_Window (Split),
               State_Normal,
               Shadow_None,
               Area,
               Split,
               X           => Current.Handle.Position.X,
               Y           => Current.Handle.Position.Y,
               Width       => Current.Handle.Position.Width,
               Height      => Current.Handle.Position.Height,
               Orientation => Orientation);

         --  Hide could cause another Expose event to be sent, resulting in an
         --  infinite loop. So we first check whether it is already visible

         elsif Current.Handle.Win /= null
           and then Is_Visible (Current.Handle.Win)
         then
            Hide (Current.Handle.Win);
         end if;

         Next (Iter);
      end loop;

      return Default_Expose_Event_Handler
        (Glib.Object.GObject_Class (Class_Ref (Parent (Get_Type (Split)))))
          (Get_Object (Split), Event);
   end Expose_Paned;

   ------------------------
   -- Realize_Paned --
   ------------------------

   procedure Realize_Paned
     (Paned : access Gtk_Widget_Record'Class)
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
   begin
      if Traces then
         Put_Line ("REALIZE_PANED");
      end if;
      Gdk_New       (Split.GC, Get_Window (Split));
      Set_Function  (Split.GC, Invert);
      Set_Exposures (Split.GC, False);
      Set_Subwindow (Split.GC, Include_Inferiors);
   end Realize_Paned;

   -----------------------
   -- Set_Handle_Cursor --
   -----------------------

   procedure Set_Handle_Cursor
     (Split   : access Gtkada_Multi_Paned_Record'Class;
      Current : Child_Description_Access)
   is
      Cursor      : Gdk_Cursor;
   begin
      if Current.Handle.Win /= null then
         case Current.Parent.Orientation is
            when Orientation_Vertical =>
               if Split.Cursor_Double_V_Arrow = null then
                  Gdk_New (Split.Cursor_Double_V_Arrow, Sb_V_Double_Arrow);
               end if;
               Cursor := Split.Cursor_Double_V_Arrow;
            when Orientation_Horizontal =>
               if Split.Cursor_Double_H_Arrow = null then
                  Gdk_New (Split.Cursor_Double_H_Arrow, Sb_H_Double_Arrow);
               end if;
               Cursor := Split.Cursor_Double_H_Arrow;
         end case;
         Set_Cursor (Current.Handle.Win, Cursor);
      end if;
   end Set_Handle_Cursor;

   -----------------
   -- Move_Handle --
   -----------------

   procedure Move_Handle
     (Split   : access Gtkada_Multi_Paned_Record'Class;
      Current : Child_Description_Access)
   is
      Window_Attr : Gdk.Window_Attr.Gdk_Window_Attr;
   begin
      if Current.Handle.Win = null
         and then Get_Window (Split) /= null
      then
         Gdk_New (Window_Attr,
                  Window_Type => Window_Child,
                  Wclass      => Input_Only,   --  Let it be transparent
                  Event_Mask  => Get_Events (Split)
                  or Button_Press_Mask
                  or Button_Release_Mask
                  or Button_Motion_Mask);
         Gdk_New (Current.Handle.Win,
                  Parent          => Get_Window (Split),
                  Attributes      => Window_Attr,
                  Attributes_Mask => 0);

         Set_User_Data (Current.Handle.Win, Split);
         Gdk.Window.Show (Current.Handle.Win);

         Destroy (Window_Attr);
         Set_Handle_Cursor (Split, Current);
      end if;

      if Realized_Is_Set (Split)
         and then Current.Handle.Win /= null
         and then Current.Visible
      then
         Gdk.Window.Show (Current.Handle.Win);
         Gdk.Window.Move_Resize
           (Current.Handle.Win,
            X      => Current.Handle.Position.X,
            Y      => Current.Handle.Position.Y,
            Width  => Current.Handle.Position.Width,
            Height => Current.Handle.Position.Height);

         --  For some reason, some parts of the handles are
         --  sometimes incorrectly exposed when a child is removed. So we
         --  just force an update

         Invalidate_Rect
           (Get_Window (Split),
            (X      => Current.Handle.Position.X,
             Y      => Current.Handle.Position.Y,
             Width  => Current.Handle.Position.Width,
             Height => Current.Handle.Position.Height),
            False);
      end if;
   end Move_Handle;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
     (Current : Child_Description_Access; Width, Height : Float)
   is
      Tmp   : Child_Description_Access;
      Count : Gint;
      Size, Size2 : Float;
      Total : Float := 0.0;
      Item  : Float;
   begin
      if Current.Width = Width and then Current.Height = Height then
         null;
      elsif Current.Is_Widget then
         Set_Size_Request
           (Current.Widget,
            Gint (Float'Max (1.0, Width)),
            Gint (Float'Max (1.0, Height)));
      elsif Current.Orientation = Orientation_Horizontal then
         Count := Count_Children (Current) - 1;
         Size  := Current.Width - Float (Count * Handle_Width);
         if Size > 0.0 then
            Tmp := Current.First_Child;
            Size2 := Width - Float (Count * Handle_Width);
            while Tmp.Next /= null loop
               Item := Size2 * Tmp.Width / Size;
               Set_Size_Request (Tmp, Item, Height);
               Total := Total + Item + Float (Handle_Width);
               Tmp   := Tmp.Next;
            end loop;
            Set_Size_Request (Tmp, Width - Total, Height);
         end if;

      else
         Count := Count_Children (Current) - 1;
         Size  := Current.Height - Float (Count * Handle_Width);
         Size2 := Height - Float (Count * Handle_Width);
         if Size > 0.0 then
            Tmp := Current.First_Child;
            while Tmp.Next /= null loop
               Item := Size2 * Tmp.Height / Size;
               Set_Size_Request (Tmp, Width, Item);
               Total := Total + Item + Float (Handle_Width);
               Tmp   := Tmp.Next;
            end loop;
            Set_Size_Request (Tmp, Width, Height - Total);
         end if;
      end if;
      Current.Width  := Width;
      Current.Height := Height;
   end Set_Size_Request;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request (Current : Child_Description_Access) is
      Requisition : Gtk_Requisition;
      Tmp         : Child_Description_Access;
      W, H        : Float := 0.0;
   begin
      if Current.Is_Widget then
         if Current.Visible then
            if Current.Width < 0.0 or else Current.Height < 0.0 then
               Size_Request (Current.Widget, Requisition);

               if Current.Width < 0.0 then
                  Current.Width := Float (Requisition.Width);
               end if;

               if Current.Height < 0.0 then
                  Current.Height := Float (Requisition.Height);
               end if;
            else
               null;  --  Keep current size
            end if;
         else
            Current.Width  := 0.0;
            Current.Height := 0.0;
         end if;

      else
         Tmp := Current.First_Child;
         while Tmp /= null loop
            Tmp.Visible := Is_Visible (Tmp);
            if Tmp.Visible then
               Size_Request (Tmp);

               if Current.Orientation = Orientation_Horizontal then
                  W  := W + Tmp.Width;
                  if Tmp.Width /= 0.0
                    and then not Is_Last_Visible (Tmp)
                  then
                     W := W + Float (Handle_Width);
                  end if;
                  H := Float'Max (H, Tmp.Height);
               else
                  W  := Float'Max (W, Tmp.Width);
                  H := H + Tmp.Height;
                  if Tmp.Height /= 0.0
                    and then not Is_Last_Visible (Tmp)
                  then
                     H := H + Float (Handle_Width);
                  end if;
               end if;
            end if;

            Tmp := Tmp.Next;
         end loop;

         --  If none of the children have requested some size, we keep the
         --  current request (so that when splitting a window, the resulting
         --  pane occupies exactly the same amoung of space as splitted window
         --  used to occupy).
         if W /= 0.0 then
            Current.Width  := W;
         end if;
         if H /= 0.0 then
            Current.Height := H;
         end if;
      end if;
   end Size_Request;

   --------------------
   -- Count_Children --
   --------------------

   function Count_Children (Current : Child_Description_Access) return Gint is
      Count : Gint := 0;
      Tmp   : Child_Description_Access;
   begin
      if not Current.Is_Widget then
         Tmp := Current.First_Child;
         while Tmp /= null loop
            if Tmp.Visible then
               Count := Count + 1;
            end if;
            Tmp := Tmp.Next;
         end loop;
      end if;
      return Count;
   end Count_Children;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Split         : access Gtkada_Multi_Paned_Record'Class;
      Current       : Child_Description_Access;
      Width, Height : Float)
   is
      Xchild : Gint := Current.X;
      Ychild : Gint := Current.Y;
      Tmp    : Child_Description_Access := Current.First_Child;
      Children_Count : Gint := 0;
      Child  : Float;
      Ratio, Fixed_Ratio  : Float := 1.0;
      Has_Request_Child : Boolean := False;
      Unrequested : Float := 0.0; --  Size for children that didn't request one
      Fixed_Width, Fixed_Height : Float := 0.0;
      Req_Width, Req_Height : Float := 0.0;
      Handles_Size : Float;

      procedure Compute_Ratios (Total, Requested, Fixed  : Float);
      --  Compute the various ratios to apply, given the total size allocated
      --  to the widget, its requested size, and the size dedicated to fixed
      --  size children

      procedure Compute_Ratios (Total, Requested, Fixed  : Float) is
         T : Float;
      begin
         if Handles_Size > Total then
            Ratio       := 0.0;
            Fixed_Ratio := 0.0;
         else
            if Fixed > Total - Handles_Size then
               Fixed_Ratio := (Total - Handles_Size) / Fixed;
            end if;

            T := Total - Handles_Size - Fixed * Fixed_Ratio;

            if Unrequested = 0.0 then
               Ratio       := T / (Requested - Fixed * Fixed_Ratio);
            elsif T > Requested or else not Has_Request_Child then
               Ratio       := 1.0;
               Unrequested := (T - Requested) / Unrequested;
            else
               Ratio := (T - Unrequested * Float (Minimum_Width))
                 / (Requested - Fixed * Fixed_Ratio);
               Unrequested := Float (Minimum_Width);
            end if;
         end if;
      end Compute_Ratios;

   begin
      if Split.Frozen then
         return;
      end if;

      --  Algorithm is the following:
      --    - Each handle gets its constant size (Handle_Width).
      --    - Each fixed_size widget gets its requested size. If the total
      --      size would be wider than the box, then they are shrunk
      --    - The remaining space is split among the children that requested a
      --      specific size. If the total requested size is wider than the box,
      --      then they are shrunk
      --    - The remaining space is split equaly among the children that
      --      didn't request a specific size.

      while Tmp /= null loop
         if Tmp.Visible then
            Children_Count := Children_Count + 1;

            if Tmp.Is_Widget and then Tmp.Fixed_Size then
               Fixed_Width  := Fixed_Width  + Tmp.Width;
               Fixed_Height := Fixed_Height + Tmp.Height;
            end if;

            Req_Width  := Req_Width + Tmp.Width;
            Req_Height := Req_Height + Tmp.Height;

            if Tmp.Width = 0.0 or else Tmp.Height = 0.0 then
               Unrequested         := Unrequested + 1.0;
            else
               Has_Request_Child := True;
            end if;
         end if;
         Tmp := Tmp.Next;
      end loop;

      Handles_Size := Float ((Children_Count - 1) * Handle_Width);
      Tmp := Current.First_Child;

      if Current.Orientation = Orientation_Horizontal then
         Compute_Ratios (Width, Req_Width, Fixed_Width);
         if Traces then
            Put_Line ("Horiz Ratio=" & Float'Image (Ratio)
                      & " Fixed_Ratio=" & Float'Image (Fixed_Ratio)
                      & " Total=" & Float'Image (Width)
                      & " Requested=" & Float'Image (Req_Width)
                      & " Fixed=" & Float'Image (Fixed_Width)
                      & " Handles=" & Float'Image (Handles_Size)
                      & " Unrequested=" & Float'Image (Unrequested));
         end if;

         while Tmp /= null loop
            if Tmp.Visible then
               if Tmp.Width = 0.0 then
                  Child := Unrequested;
               elsif Tmp.Is_Widget and then Tmp.Fixed_Size then
                  Child := Tmp.Width * Fixed_Ratio;
               else
                  Child := Tmp.Width * Ratio;
               end if;

               if Tmp.Is_Widget then
                  Size_Allocate
                    (Tmp.Widget,
                     (Xchild, Current.Y,
                      Gint'Max (1, Gint (Child)), Gint (Height)));
                  Tmp.Width  := Float'Max (1.0, Child);
                  Tmp.Height := Height;
               else
                  Tmp.X := Xchild;
                  Tmp.Y := Current.Y;
                  Size_Allocate (Split, Tmp, Child, Height);
               end if;

               Tmp.Handle.Position :=
                 (X      => Xchild + Gint (Child),
                  Y      => Current.Y,
                  Width  => Handle_Width,
                  Height => Gint (Height));
               Move_Handle (Split, Tmp);

               Xchild := Xchild + Gint (Child) + Handle_Width;
            elsif Tmp.Handle.Win /= null then
               Hide (Tmp.Handle.Win);
            end if;

            Tmp := Tmp.Next;
         end loop;

      else
         Compute_Ratios (Height, Req_Height, Fixed_Height);
         if Traces then
            Put_Line ("Vert Ratio=" & Float'Image (Ratio)
                      & " Fixed_Ratio=" & Float'Image (Fixed_Ratio)
                      & " Total=" & Float'Image (Height)
                      & " Requested=" & Float'Image (Req_Height)
                      & " Fixed=" & Float'Image (Fixed_Height)
                      & " Handles=" & Float'Image (Handles_Size)
                      & " Unrequested=" & Float'Image (Unrequested));
         end if;
         while Tmp /= null loop
            if Tmp.Visible then
               if Tmp.Height = 0.0 then
                  Child := Unrequested;
               elsif Tmp.Is_Widget and then Tmp.Fixed_Size then
                  Child := Tmp.Height * Fixed_Ratio;
               else
                  Child := Tmp.Height * Ratio;
               end if;
               if Tmp.Is_Widget then
                  Size_Allocate
                    (Tmp.Widget,
                     (Current.X, Ychild, Gint (Width),
                      Gint'Max (1, Gint (Child))));
                  Tmp.Width  := Width;
                  Tmp.Height := Float'Max (1.0, Child);
               else
                  Tmp.X := Current.X;
                  Tmp.Y := Ychild;
                  Size_Allocate (Split, Tmp, Width, Child);
               end if;

               Tmp.Handle.Position :=
                 (X      => Current.X,
                  Y      => Ychild + Gint (Child),
                  Width  => Gint (Width),
                  Height => Handle_Width);
               Move_Handle (Split, Tmp);

               Ychild := Ychild + Gint (Child) + Handle_Width;
            elsif Tmp.Handle.Win /= null then
               Hide (Tmp.Handle.Win);
            end if;

            Tmp := Tmp.Next;
         end loop;
      end if;

      Current.Width  := Width;
      Current.Height := Height;
   end Size_Allocate;

   -------------------------
   -- Size_Allocate_Paned --
   -------------------------

   procedure Size_Allocate_Paned
     (Paned : System.Address; Alloc : Gtk_Allocation)
   is
      Split        : constant Gtkada_Multi_Paned :=
        Gtkada_Multi_Paned (Gtk.Widget.Convert (Paned));
      Visibility_Changed : Boolean := False;
      --  True if the visibility of one of the children has changed.
      Iter : Child_Iterator;
   begin
      if not Realized_Is_Set (Split)
        or else Split.Frozen
        or else Alloc.Width <= 1 --  Uninitialized yet
      then
         return;

      elsif Split.Children = null then
         --  With nested multi_paned, we must make sure we still properly store
         --  the allocation.

         Set_Allocation (Split, Alloc);
         return;
      end if;

      if Split.Children.Width = -1.0 then
         --  After a thaw
         Visibility_Changed := True;
      else
         Iter := Start (Split);
         while not At_End (Iter) loop
            if Iter.Current.Visible /= Is_Visible (Iter.Current) then
               Visibility_Changed := True;
               exit;
            end if;
            Next (Iter);
         end loop;
      end if;

      if Traces then
         Put_Line ("SIZE_ALLOCATE_PANED "
                   & Gint'Image (Alloc.X)
                   & Gint'Image (Alloc.Y)
                   & Allocation_Int'Image (Alloc.Width)
                   & "x" & Allocation_Int'Image (Alloc.Height)
                   & " visibility_changed="
                   & Boolean'Image (Visibility_Changed));
      end if;

      if not Visibility_Changed
        and then Get_Allocation_X (Split)      = Alloc.X
        and then Get_Allocation_Y (Split)      = Alloc.Y
        and then Get_Allocation_Width (Split)  = Alloc.Width
        and then Get_Allocation_Height (Split) = Alloc.Height
      then
         --  We need to propagate the size_allocate to children, since
         --  otherwise they are not properly refreshed, for instance the
         --  notebooks in the MDI.
         Iter := Start (Split);
         while not At_End (Iter) loop
            if Iter.Current.Is_Widget then
               Size_Allocate
                  (Iter.Current.Widget,
                   (Get_Allocation_X (Iter.Current.Widget),
                    Get_Allocation_Y (Iter.Current.Widget),
                    Get_Allocation_Width (Iter.Current.Widget),
                    Get_Allocation_Height (Iter.Current.Widget)));
            end if;
            Next (Iter);
         end loop;
         return;
      end if;

      Set_Allocation (Split, Alloc);

      if Get_Has_Window (Split) then
         Gdk.Window.Move_Resize
           (Get_Window (Split),
            X      => Alloc.X,
            Y      => Alloc.Y,
            Width  => Alloc.Width,
            Height => Alloc.Height);
      end if;

      Size_Request (Split.Children);

      if Traces then
         Put_Line ("## after Size_Request");
         Dump (Split, Split.Children);
      end if;

      if Get_Has_Window (Split) then
         Split.Children.X := 0;
         Split.Children.Y := 0;
      else
         Split.Children.X := Alloc.X;
         Split.Children.Y := Alloc.Y;
      end if;

      Size_Allocate (Split, Split.Children,
                     Float (Alloc.Width), Float (Alloc.Height));

      if Traces then
         Put_Line ("## after Size_Allocate");
         Dump (Split, Split.Children);
      end if;

   exception
      when E : others =>
         if Traces then
            Put_Line ("Unexpected exception " & Exception_Information (E));
         end if;
   end Size_Allocate_Paned;

   -------------------
   -- Splitted_Area --
   -------------------

   function Splitted_Area
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      After         : Boolean := True) return Gtk.Widget.Gtk_Widget
   is
      Current, Tmp : Child_Description_Access;
      Iter    : Child_Iterator := Start (Win);
   begin
      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget
                    and then Current.Widget = Gtk_Widget (Ref_Widget));
         Next (Iter);
      end loop;

      if Current /= null
        and then Current.Parent.Orientation = Orientation
      then
         if After then
            Current := Current.Next;
         else
            Tmp := Current.Parent.First_Child;
            while Tmp /= null
              and then Tmp.Next /= Current
            loop
               Tmp := Tmp.Next;
            end loop;

            Current := Tmp;
         end if;

         while Current /= null and then not Current.Is_Widget loop
            Current := Current.First_Child;
         end loop;

         if Current /= null then
            return Current.Widget;
         end if;
      end if;

      return null;
   end Splitted_Area;

   --------------
   -- Get_Pane --
   --------------

   function Get_Pane
     (Win    : access Gtkada_Multi_Paned_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Pane
   is
      Iter    : Child_Iterator := Start (Win);
      Current : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget
                    and then Current.Widget = Gtk_Widget (Widget));
         Next (Iter);
      end loop;

      if Current /= null then
         return Pane (Current.Parent);
      else
         return null;
      end if;
   end Get_Pane;

   --------------
   -- Get_Pane --
   --------------

   function Get_Pane (Current_Pane : Pane) return Pane is
   begin
      return Pane (Current_Pane.Parent);
   end Get_Pane;

   --------------------
   -- Split_Internal --
   --------------------

   procedure Split_Internal
     (Win           : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget    : Gtk_Widget;
      Ref_Pane      : Pane;
      Use_Ref_Pane  : Boolean;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True)
   is
      New_Pane : Child_Description_Access;

      procedure Add_After_All_Children (Parent : Child_Description_Access);
      --  Add the new child at the end of the child list for Parent

      procedure Add_As_First_Child (Parent : Child_Description_Access);
      --  Add the new child as the first child of parent.
      --  It will share the space of the current first child of Parent

      procedure Add_In_List
        (Parent   : Child_Description_Access;
         Ref_Item : Child_Description_Access;
         After    : Boolean);
      --  Add a new child to Parent, that contains New_Child, before or after
      --  Ref_Item. The new child shares the space previously occupied by
      --  Ref_Item, which is thus resized as needed.

      function Create_Or_Get_Parent
        (Current          : Child_Description_Access;
         Orientation      : Gtk_Orientation) return Child_Description_Access;
      --  Create a new parent for Current with the orientation specified in
      --  parameter to Split_Internal. This new parent takes the place of
      --  Current in the tree.

      --------------------------
      -- Create_Or_Get_Parent --
      --------------------------

      function Create_Or_Get_Parent
        (Current          : Child_Description_Access;
         Orientation      : Gtk_Orientation) return Child_Description_Access
      is
         Pane, Tmp2 : Child_Description_Access;
      begin
         if Current.Parent /= null
            and then Current.Parent.Orientation = Orientation
         then
            return Current.Parent;
         end if;

         --  The new parent occupies the same area as Current, therefore we
         --  adjust the size request accordingly. The size of Current doesn't
         --  change either, since there is no extra handle added

         Pane := new Child_Description'
           (Parent      => Current.Parent,
            Next        => Current.Next,
            Is_Widget   => False,
            Orientation => Orientation,
            First_Child => Current,
            Width       => Current.Width,
            Height      => Current.Height,
            X           => 0,
            Y           => 0,
            Visible     => True,
            Handle      => No_Handle);
         Current.Parent := Pane;

         if Pane.Parent = null then
            Win.Children := Pane;
         elsif Pane.Parent.First_Child = Current then
            Pane.Parent.First_Child := Pane;
         else
            Tmp2 := Pane.Parent.First_Child;
            while Tmp2.Next /= Current loop
               Tmp2 := Tmp2.Next;
            end loop;
            Tmp2.Next := Pane;
         end if;

         Set_Handle_Cursor (Win, Current);
         Current.Next := null;
         return Pane;
      end Create_Or_Get_Parent;

      -----------------
      -- Add_In_List --
      -----------------

      procedure Add_In_List
        (Parent   : Child_Description_Access;
         Ref_Item : Child_Description_Access;
         After    : Boolean)
      is
         Tmp      : Child_Description_Access := Parent.First_Child;
         Tmp2     : Child_Description_Access;
      begin
         if Traces then
            Put_Line ("Add_In_List");
         end if;

         Tmp2 := new Child_Description'
           (Parent      => Parent,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Float (Width),
            Height      => Float (Height),
            Visible     => True,
            Handle      => No_Handle,
            Widget      => Gtk_Widget (New_Child));
         New_Pane := Tmp2;

         if After then
            while Tmp /= Ref_Item loop
               Tmp := Tmp.Next;
            end loop;
            Tmp2.Next := Tmp.Next;
            Tmp.Next := Tmp2;
         else
            if Parent.First_Child = Ref_Item then
               Tmp2.Next := Parent.First_Child;
               Parent.First_Child := Tmp2;
            else
               while Tmp.Next /= Ref_Item loop
                  Tmp := Tmp.Next;
               end loop;
               Tmp2.Next := Tmp.Next;
               Tmp.Next := Tmp2;
            end if;
         end if;

         if Realized_Is_Set (Win)
           and then Win.Children.Width > 0.0
           and then not Win.Frozen
         then
            if Traces then
               Put_Line ("Adjusting sizes since window is realized");
            end if;
            if Ref_Item.Width > Float (Width) then
               Ref_Item.Width := Ref_Item.Width - Float (Width);
            else
               Tmp2.Width := Float (Width - Minimum_Width);
               Ref_Item.Width := Float (Minimum_Width);
            end if;

            if Ref_Item.Height > Float (Height) then
               Ref_Item.Height := Ref_Item.Height - Float (Height);
            else
               Tmp2.Height := Float (Height - Minimum_Width);
               Ref_Item.Height := Float (Minimum_Width);
            end if;
         end if;
      end Add_In_List;

      ------------------------
      -- Add_As_First_Child --
      ------------------------

      procedure Add_As_First_Child (Parent : Child_Description_Access) is
      begin
         if Traces then
            Put_Line ("Add_As_First_Child");
         end if;
         if Parent.First_Child = null then
            Parent.First_Child := new Child_Description'
              (Parent      => Parent,
               Next        => Parent.First_Child,
               Is_Widget   => True,
               Fixed_Size  => Fixed_Size,
               Width       => Float (Width),
               Height      => Float (Height),
               Handle      => No_Handle,
               Visible     => True,
               Widget      => Gtk_Widget (New_Child));
            New_Pane := Parent.First_Child;
         else
            Add_In_List (Parent, Parent.First_Child, After => False);
         end if;
      end Add_As_First_Child;

      ----------------------------
      -- Add_After_All_Children --
      ----------------------------

      procedure Add_After_All_Children (Parent : Child_Description_Access) is
         Tmp : Child_Description_Access;
      begin
         if Traces then
            Put_Line ("Add_After_All_Children");
         end if;
         if Parent.First_Child = null then
            Parent.First_Child := new Child_Description'
              (Parent      => Parent,
               Next        => null,
               Is_Widget   => True,
               Fixed_Size  => Fixed_Size,
               Width       => Float (Width),
               Height      => Float (Height),
               Handle      => No_Handle,
               Visible     => True,
               Widget      => Gtk_Widget (New_Child));
            New_Pane := Parent.First_Child;
         else
            Tmp := Parent.First_Child;
            while Tmp.Next /= null loop
               Tmp := Tmp.Next;
            end loop;
            Add_In_List (Parent, Tmp, After => True);
         end if;
      end Add_After_All_Children;

      Current, Pane : Child_Description_Access;
   begin
      if Traces then
         Put_Line ("Split_Internal: Orientation="
                   & Gtk_Orientation'Image (Orientation)
                   & " Width=" & Gint'Image (Width)
                   & " Height=" & Gint'Image (Height)
                   & " After=" & Boolean'Image (After)
                   & " Fixed=" & Boolean'Image (Fixed_Size)
                   & " Ref_Widget=" & Boolean'Image (Ref_Widget /= null)
                   & " Ref_Pane=" & Boolean'Image (Ref_Pane /= null));
      end if;

      if Ref_Pane /= null then
         --  Split specific pane
         Current := Child_Description_Access (Ref_Pane);
      elsif Ref_Widget = null then
         if Use_Ref_Pane then
            if Traces then
               Put_Line ("Split_Internal: Splitting main window");
            end if;

            --  Split main window
            Current := Win.Children;
         else
            Current := null;
         end if;
      else
         declare
            Iter : Child_Iterator := Start (Win);
         begin
            loop
               Current := Get (Iter);
               exit when Current = null
                 or else (Current.Is_Widget
                          and then Current.Widget = Ref_Widget);
               Next (Iter);
            end loop;
         end;
      end if;

      if Current /= null then
         if not Current.Is_Widget then
            if Current.Orientation = Orientation then
               if After then
                  Add_After_All_Children (Current);
               else
                  Add_As_First_Child (Current);
               end if;
            else  --  Current.Orientation /= Orientation
               Pane := Create_Or_Get_Parent (Current, Orientation);
               Add_In_List (Pane, Current, After);
            end if;

         else  --  Current.Is_Widget
            Pane := Create_Or_Get_Parent (Current, Orientation);
            Add_In_List (Pane, Current, After);
         end if;

         if Width = 0 then
            Current.Width := 0.0;
         end if;

         if Height = 0 then
            Current.Height := 0.0;
         end if;

      --   Current = null => Do nothing unless there is no child yet
      elsif Win.Children = null then
         if Traces then
            Put_Line ("No children yet");
         end if;
         New_Pane := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Float (Width),
            Height      => Float (Height),
            Handle      => No_Handle,
            Visible     => True,
            Widget      => Gtk_Widget (New_Child));
         Win.Children := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => False,
            Orientation => Orientation,
            Width       => -1.0,
            Height      => -1.0,
            X           => 0,
            Y           => 0,
            First_Child => New_Pane,
            Visible     => True,
            Handle      => No_Handle);
         New_Pane.Parent := Win.Children;

      elsif Win.Children /= null then
         if Traces then
            Put_Line ("Added as first child of Win");
         end if;
         Add_As_First_Child (Win.Children);
      end if;

      Put (Win, New_Child, 0, 0);

      if not Realized_Is_Set (Win)
        or else Win.Frozen
        or else Win.Children.Width <= 0.0
      then
         --  Reset to -1, so that other operations like Set_Size will not try
         --  to do a Size_Allocate, even though there was no Size_Request first
         Win.Children.Width := -1.0;
         Queue_Resize (Win);
      else
         --  In some case, we might need to force a recomputation of the size,
         --  since the widget might not have specified an explicit size
         if Width <= 0 or else Height <= 0 then
            Size_Request (New_Pane.Parent);
         end if;

         --  In case the toplevel child has changed, we need to simulate the
         --  full allocation mechanism, without redoing the size_request to
         --  avoid resizing unwanted widgets
         if Get_Has_Window (Win) then
            Win.Children.X := 0;
            Win.Children.Y := 0;
         else
            Win.Children.X := Get_Allocation_X (Win);
            Win.Children.Y := Get_Allocation_Y (Win);
         end if;

         Size_Allocate (Win, Win.Children,
                        Float (Get_Allocation_Width (Win)),
                        Float (Get_Allocation_Height (Win)));
      end if;

      if Traces then
         Put_Line ("Split_Internal: After inserting "
                   & System.Address_Image (New_Child.all'Address)
                   & " Width=" & Gint'Image (Width)
                   & " Height=" & Gint'Image (Height));
         Dump (Win, Win.Children);
      end if;
   end Split_Internal;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Win : access Gtkada_Multi_Paned_Record) is
   begin
      Win.Frozen := True;

      if Traces then
         Put_Line ("Multi_Paned: Freeze");
      end if;
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Win : access Gtkada_Multi_Paned_Record) is
   begin
      Win.Frozen := False;

      --  So that Size_Allocate_Paned detects a visibility change, even
      --  though the actual size of the window probably has not changed
      if Win.Children /= null then
         Win.Children.Width   := -1.0;
      end if;

      if Traces then
         Put_Line ("Multi_Paned: Thaw");
      end if;
   end Thaw;

   -----------
   -- Split --
   -----------

   procedure Split
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Pane      : Pane;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True) is
   begin
      Split_Internal
        (Win, null, Ref_Pane, True, New_Child, Orientation,
         Fixed_Size, Width, Height, After);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Win         : access Gtkada_Multi_Paned_Record;
      Ref_Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After       : Boolean := True) is
   begin
      Split_Internal
        (Win, Gtk_Widget (Ref_Widget), null, False, New_Child, Orientation,
         Fixed_Size, Width, Height, After);
   end Split;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Win        : access Gtkada_Multi_Paned_Record;
      New_Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation :=
        Gtk.Enums.Orientation_Horizontal;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := -1;
      After         : Boolean := True) is
   begin
      Split_Internal
        (Win, null, null, False,
         New_Child, Orientation, Fixed_Size, Width, Height, After);
   end Add_Child;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Win           : access Gtkada_Multi_Paned_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Width, Height : Glib.Gint := -1;
      Fixed_Size    : Boolean := False)
   is
      Iter              : Child_Iterator := Start (Win);
      Current, Previous : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         if Current.Is_Widget
           and then Current.Widget = Gtk_Widget (Widget)
         then
            Current.Fixed_Size := Fixed_Size;

            --  If we have gone through a whole Size_Request/Size_Allocate
            --  cycle already, we don't do this again, to preserve as much as
            --  possible the current size of windows
            if Realized_Is_Set (Win)
              and then Win.Children.Width > 0.0
            then
               --  We need to adjust the size of several widgets, to take into
               --  account the fact that resizing one will impact its neighbors
               --  We use the same algorithm as when dragging handles.

               if Current.Next = null then
                  Previous := Current.Parent.First_Child;
                  while Previous /= null and then Previous.Next /= Current loop
                     Previous := Previous.Next;
                  end loop;

                  if Previous /= null then
                     if Current.Parent.Orientation =
                       Orientation_Horizontal
                     then
                        Previous.Handle.Position.X :=
                          Previous.Handle.Position.X - Width
                            + Gint (Current.Width);
                     else
                        --  Move the handle up so that the number of pixels is
                        --  the difference between the old size and the new
                        --  one.
                        Previous.Handle.Position.Y :=
                          Previous.Handle.Position.Y - Height
                            + Gint (Current.Height);
                     end if;
                     Resize_Child_And_Siblings (Current.Parent, Previous);
                  end if;

               else
                  if Current.Parent.Orientation = Orientation_Horizontal then
                     Current.Handle.Position.X := Current.Handle.Position.X
                       + Width - Gint (Current.Width);
                  else
                     Current.Handle.Position.Y := Current.Handle.Position.Y
                       + Height - Gint (Current.Height);
                  end if;
                  Resize_Child_And_Siblings (Current.Parent, Current);
               end if;

               Size_Allocate
                 (Win, Current.Parent,
                  Current.Parent.Width, Current.Parent.Height);
            else
               Current.Width  := Float (Width);
               Current.Height := Float (Height);
               Queue_Resize (Win);
            end if;
            exit;
         end if;

         Next (Iter);
      end loop;

      if Traces then
         Put_Line ("After Set_Size on "
                   & System.Address_Image (Widget.all'Address)
                   & " to " & Gint'Image (Width)
                   & Gint'Image (Height));
         Dump (Win, Win.Children);
      end if;
   end Set_Size;

end Gtkada.Multi_Paned;
