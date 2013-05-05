-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2000-2013, AdaCore                 --
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

package body Gtk.Arguments is

   use Glib.Values;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (Args : Gtk_Args; Num : Positive) return Gint is
   begin
      return Get_Int (Nth (Args, Guint (Num)));
   end To_Gint;

   --------------
   -- To_Guint --
   --------------

   function To_Guint (Args : Gtk_Args; Num : Positive) return Guint is
   begin
      return Get_Uint (Nth (Args, Guint (Num)));
   end To_Guint;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean is
   begin
      return Get_Boolean (Nth (Args, Guint (Num)));
   end To_Boolean;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Args : Gtk_Args; Num : Positive)
     return Glib.Object.GObject
   is
      Stub : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Get_Address (Nth (Args, Guint (Num))), Stub);
   end To_Object;

   ----------------
   -- To_C_Proxy --
   ----------------

   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Gdk.C_Proxy is
   begin
      return Get_Proxy (Nth (Args, Guint (Num)));
   end To_C_Proxy;

   --------------
   -- To_Event --
   --------------

   function To_Event (Args : Gtk_Args; Num : Positive)
     return Gdk.Event.Gdk_Event is
   begin
      return Gdk.Event.Gdk_Event (Get_Proxy (Nth (Args, Guint (Num))));
   end To_Event;

   ----------------------
   -- To_Notebook_Page --
   ----------------------

   function To_Notebook_Page
     (Args : Gtk_Args; Num : Positive) return Gtk_Notebook_Page is
   begin
      return Gtk_Notebook_Page (Get_Proxy (Nth (Args, Guint (Num))));
   end To_Notebook_Page;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Args : Gtk_Args; Num : Positive)
     return System.Address is
   begin
      return Get_Address (Nth (Args, Guint (Num)));
   end To_Address;

   --------------------
   -- To_Requisition --
   --------------------

   function To_Requisition
     (Args : Gtk_Args; Num : Positive) return Gtk.Widget.Gtk_Requisition_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Internal is new Unchecked_Conversion
        (System.Address, Gtk.Widget.Gtk_Requisition_Access);
      pragma Warnings (On);

   begin
      return Internal (Get_Address (Nth (Args, Guint (Num))));
   end To_Requisition;

   ----------------
   -- To_Address --
   ----------------

   function To_Allocation
     (Args : Gtk_Args; Num : Positive)
      return Gtk.Widget.Gtk_Allocation_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Internal is new Unchecked_Conversion
        (System.Address, Gtk.Widget.Gtk_Allocation_Access);
      pragma Warnings (On);

   begin
      return Internal (Get_Address (Nth (Args, Guint (Num))));
   end To_Allocation;

   ---------------
   -- To_String --
   ---------------

   function To_String  (Args : Gtk_Args; Num : Positive) return UTF8_String is
   begin
      return Get_String (Nth (Args, Guint (Num)));
   end To_String;

end Gtk.Arguments;
