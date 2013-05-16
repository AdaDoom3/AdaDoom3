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
with Gdk.Color;                  use Gdk.Color;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Pixmap is
   Dummy_Pixmap : constant GtkAda.Types.chars_ptr_array :=
     (New_String ("1 1 1 1"),
      New_String ("c None"),
      New_String (" "));
   --  This is a dummy pixmap we use when a pixmap can't be found.

   function Create_Pixmap
     (Filename : String;
      Window   : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap
   is
      Gdkpixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;

      use Gtk.Widget;
      use Gtk.Window;

   begin
      if not Realized_Is_Set (Window) then
         Gtk.Window.Realize (Window);
      end if;

      if Filename = "" then
         Gdk.Pixmap.Create_From_Xpm_D
           (Gdkpixmap, Get_Window (Window), Mask,
            Gdk.Color.Null_Color, Dummy_Pixmap);
      else
         Gdk.Pixmap.Create_From_Xpm
           (Gdkpixmap, Get_Window (Window), Mask,
            Gdk.Color.Null_Color, Filename);
      end if;

      Gtk_New (Pixmap, Gdkpixmap, Mask);
      return Pixmap;
   end Create_Pixmap;

   function Create_Pixmap
     (Data   : Gtkada.Types.Chars_Ptr_Array;
      Window : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap
   is
      Gdkpixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;

      use Gtk.Widget;
      use Gtk.Window;

   begin
      if not Realized_Is_Set (Window) then
         Gtk.Window.Realize (Window);
      end if;

      Gdk.Pixmap.Create_From_Xpm_D
        (Gdkpixmap, Get_Window (Window), Mask, Gdk.Color.Null_Color, Data);
      Gtk_New (Pixmap, Gdkpixmap, Mask);

      return Pixmap;
   end Create_Pixmap;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Pixmap_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Pixmap : out Gtk_Pixmap;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
   begin
      Pixmap := new Gtk_Pixmap_Record;
      Gtk.Pixmap.Initialize (Pixmap, Val, Mask);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Pixmap : access Gtk_Pixmap_Record'Class;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
         (Val  : Gdk.Pixmap.Gdk_Pixmap;
          Mask : Gdk.Bitmap.Gdk_Bitmap) return System.Address;
      pragma Import (C, Internal, "gtk_pixmap_new");
   begin
      Set_Object (Pixmap, Internal (Val, Mask));
   end Initialize;

   ---------
   -- Get --
   ---------

   procedure Get
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : out Gdk.Pixmap.Gdk_Pixmap;
       Mask   : out Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Pixmap : System.Address;
          Val    : out Gdk.Pixmap.Gdk_Pixmap;
          Mask   : out Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_pixmap_get");
   begin
      Internal (Get_Object (Pixmap), Val, Mask);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Pixmap : System.Address;
          Val    : Gdk.Pixmap.Gdk_Pixmap;
          Mask   : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_pixmap_set");
   begin
      Internal (Get_Object (Pixmap), Val, Mask);
   end Set;

   ---------------------------
   -- Set_Build_Insensitive --
   ---------------------------

   procedure Set_Build_Insensitive
      (Pixmap : access Gtk_Pixmap_Record;
       Build  : Boolean)
   is
      procedure Internal (Pixmap : System.Address; Build : Integer);
      pragma Import (C, Internal, "gtk_pixmap_set_build_insensitive");
   begin
      Internal (Get_Object (Pixmap), Boolean'Pos (Build));
   end Set_Build_Insensitive;

end Gtk.Pixmap;
