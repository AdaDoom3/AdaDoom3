-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Paned is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Paned_Record);
   pragma Unreferenced (Type_Conversion);

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Paned : out Gtk_Hpaned) is
   begin
      Paned := new Gtk_Hpaned_Record;
      Gtk.Paned.Initialize_Hpaned (Paned);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Paned : out Gtk_Vpaned) is
   begin
      Paned := new Gtk_Vpaned_Record;
      Gtk.Paned.Initialize_Vpaned (Paned);
   end Gtk_New_Vpaned;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned (Paned : access Gtk_Hpaned_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      Set_Object (Paned, Internal);
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned (Paned : access Gtk_Vpaned_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      Set_Object (Paned, Internal);
   end Initialize_Vpaned;

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add2;

   ----------------------
   -- Compute_Position --
   ----------------------

   procedure Compute_Position
      (Paned      : access Gtk_Paned_Record;
       Allocation : Gint;
       Child1_Req : Gint;
       Child2_Req : Gint)
   is
      procedure Internal
         (Paned      : System.Address;
          Allocation : Gint;
          Child1_Req : Gint;
          Child2_Req : Gint);
      pragma Import (C, Internal, "gtk_paned_compute_position");
   begin
      Internal (Get_Object (Paned), Allocation, Child1_Req, Child2_Req);
   end Compute_Position;

   ----------------
   -- Get_Child1 --
   ----------------

   function Get_Child1
      (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child1");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub));
   end Get_Child1;

   ----------------
   -- Get_Child2 --
   ----------------

   function Get_Child2
      (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child2");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub));
   end Get_Child2;

   -----------------------
   -- Get_Handle_Window --
   -----------------------

   function Get_Handle_Window
      (Paned : access Gtk_Paned_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
         (Paned : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_paned_get_handle_window");
   begin
      return Internal (Get_Object (Paned));
   end Get_Handle_Window;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Paned : access Gtk_Paned_Record) return Gint is
      function Internal (Paned : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_paned_get_position");
   begin
      return Internal (Get_Object (Paned));
   end Get_Position;

   -----------
   -- Pack1 --
   -----------

   procedure Pack1
      (Paned  : access Gtk_Paned_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := True)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Integer;
          Shrink : Integer);
      pragma Import (C, Internal, "gtk_paned_pack1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack1;

   -----------
   -- Pack2 --
   -----------

   procedure Pack2
      (Paned  : access Gtk_Paned_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := False)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Integer;
          Shrink : Integer);
      pragma Import (C, Internal, "gtk_paned_pack2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack2;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Paned : access Gtk_Paned_Record; Position : Gint) is
      procedure Internal (Paned : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_paned_set_position");
   begin
      Internal (Get_Object (Paned), Position);
   end Set_Position;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Paned_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Paned;
