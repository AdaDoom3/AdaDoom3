-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

package body Gtk.Builder is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Builder : out Gtk_Builder) is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize (Builder);
   end Gtk_New;

   -------------------
   -- Add_From_File --
   -------------------

   function Add_From_File
     (Builder  : access Gtk_Builder_Record;
      Filename : String)
      return Glib.Error.GError
   is
      function Internal
        (Builder  : System.Address;
         Filename : String;
         Error    : access Glib.Error.GError)
         return Guint;
      pragma Import (C, Internal, "gtk_builder_add_from_file");
      Err : aliased Glib.Error.GError;
   begin
      if Internal
        (Get_Object (Builder), Filename & ASCII.NUL, Err'Access) = 0
      then
         return Err;
      else
         return null;
      end if;
   end Add_From_File;

   ---------------------
   -- Add_From_String --
   ---------------------

   function Add_From_String
     (Builder : access Gtk_Builder_Record;
      Buffer  : String;
      Length  : Gsize)
      return Glib.Error.GError
   is
      function Internal
        (Builder : System.Address;
         Buffer  : String;
         Length  : Gsize;
         Error   : access Glib.Error.GError)
         return Guint;
      pragma Import (C, Internal, "gtk_builder_add_from_string");

      Err : aliased Glib.Error.GError;
   begin
      if Internal
        (Get_Object (Builder), Buffer & ASCII.NUL, Length, Err'Access) = 0
      then
         return Err;
      else
         return null;
      end if;
   end Add_From_String;

   --------------------------
   -- Connect_Signals_Full --
   --------------------------

   procedure Connect_Signals_Full
     (Builder         : access Gtk_Builder_Record;
      Signal_Function : Gtk_Builder_Connect_Func;
      User_Data       : System.Address)
   is
      procedure Internal
        (Builder          : System.Address;
         Handler_Function : Gtk_Builder_Connect_Func;
         User_Data        : System.Address);
      pragma Import (C, Internal, "gtk_builder_connect_signals_full");
   begin
      Internal
        (Glib.Object.Get_Object (Builder),
         Signal_Function,
         User_Data);
   end Connect_Signals_Full;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (Builder     : access Gtk_Builder_Record;
      Object_Name : String)
      return Glib.Object.GObject
   is
      function Internal
        (Builder     : System.Address;
         Object_Name : String)
         return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_object");
      Addr : System.Address;
   begin
      Addr := Internal
        (Glib.Object.Get_Object (Builder),
         Object_Name & ASCII.NUL);
      if Addr /= System.Null_Address then
         return Glib.Object.Convert (Addr);
      else
         return null;
      end if;
   end Get_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Builder : access Gtk_Builder_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_builder_new");
   begin
      Set_Object (Builder, Internal);
   end Initialize;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Builder : access Gtk_Builder_Record;
      Name    : String) return Gtk.Widget.Gtk_Widget
   is
      Object : constant GObject := Get_Object (Builder, Name);
   begin
      if Object = null then
         return null;
      else
         return Gtk.Widget.Convert (Glib.Object.Convert (Object));
      end if;
   end Get_Widget;

end Gtk.Builder;
