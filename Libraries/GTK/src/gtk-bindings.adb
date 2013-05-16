-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Gdk.Event;  use Gdk.Event;
with Gdk.Types;  use Gdk.Types;

package body Gtk.Bindings is

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : String)
   is
      procedure Internal
        (Binding_Set : Gtk_Binding_Set;
         Keyval      : Guint;
         Modifiers   : Gdk_Modifier_Type;
         Signal_Name : String);
      pragma Import (C, Internal, "ada_gtk_binding_entry_add_signal_NO");
   begin
      Internal (Binding_Set, Keyval, Modifiers, Signal_Name & ASCII.NUL);
   end Add_Signal;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : String;
      Arg1        : Gint)
   is
      procedure Internal
        (Binding_Set : Gtk_Binding_Set;
         Keyval      : Guint;
         Modifiers   : Gdk_Modifier_Type;
         Signal_Name : String;
         Arg1        : Gint);
      pragma Import (C, Internal, "ada_gtk_binding_entry_add_signal_int");
   begin
      Internal (Binding_Set, Keyval, Modifiers, Signal_Name & ASCII.NUL, Arg1);
   end Add_Signal;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : String;
      Arg1        : Gint;
      Arg2        : Gint)
   is
      procedure Internal
        (Binding_Set : Gtk_Binding_Set;
         Keyval      : Guint;
         Modifiers   : Gdk_Modifier_Type;
         Signal_Name : String;
         Arg1, Arg2  : Gint);
      pragma Import (C, Internal, "ada_gtk_binding_entry_add_signal_int_int");
   begin
      Internal
        (Binding_Set, Keyval, Modifiers, Signal_Name & ASCII.NUL, Arg1, Arg2);
   end Add_Signal;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : String;
      Arg1        : Boolean)
   is
      procedure Internal
        (Binding_Set : Gtk_Binding_Set;
         Keyval      : Guint;
         Modifiers   : Gdk_Modifier_Type;
         Signal_Name : String;
         Arg1        : Gboolean);
      pragma Import (C, Internal, "ada_gtk_binding_entry_add_signal_bool");
   begin
      Internal
        (Binding_Set, Keyval, Modifiers, Signal_Name & ASCII.NUL,
         Boolean'Pos (Arg1));
   end Add_Signal;

   --------------------------
   -- Binding_Set_Activate --
   --------------------------

   function Binding_Set_Activate
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk_Modifier_Type;
      Object      : access GObject_Record'Class)
      return Boolean
   is
      function Internal
        (Binding_Set : Gtk_Binding_Set;
         Keyval      : Guint;
         Modifiers   : Gdk_Modifier_Type;
         Object      : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_binding_set_activate");
   begin
      return Boolean'Val
        (Internal (Binding_Set, Keyval, Modifiers, Get_Object (Object)));
   end Binding_Set_Activate;

   ----------------------
   -- Binding_Set_Find --
   ----------------------

   function Binding_Set_Find (Set_Name : String) return Gtk_Binding_Set is
      function Internal (Set_Name : String) return Gtk_Binding_Set;
      pragma Import (C, Internal, "gtk_binding_set_find");
   begin
      return Internal (Set_Name & ASCII.NUL);
   end Binding_Set_Find;

   ---------------------
   -- Binding_Set_New --
   ---------------------

   function Binding_Set_New (Set_Name : String) return Gtk_Binding_Set is
      function Internal (Set_Name : String) return Gtk_Binding_Set;
      pragma Import (C, Internal, "gtk_binding_set_new");
   begin
      return Internal (Set_Name & ASCII.NUL);
   end Binding_Set_New;

   --------------
   -- Activate --
   --------------

   function Activate
     (Object    : access GObject_Record'Class;
      Keyval    : Guint;
      Modifiers : Gdk_Modifier_Type)
      return Boolean
   is
      function Internal
        (Object    : System.Address;
         Keyval    : Guint;
         Modifiers : Gdk_Modifier_Type)
         return Gboolean;
      pragma Import (C, Internal, "gtk_bindings_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Object), Keyval, Modifiers));
   end Activate;

   --------------------
   -- Activate_Event --
   --------------------

   function Activate_Event
     (Object : access GObject_Record;
      Event  : Gdk_Event_Key)
      return Boolean
   is
      function Internal
        (Object : System.Address;
         Event  : Gdk_Event_Key)
         return Gboolean;
      pragma Import (C, Internal, "gtk_bindings_activate_event");
   begin
      return Boolean'Val (Internal (Get_Object (Object), Event));
   end Activate_Event;

end Gtk.Bindings;
