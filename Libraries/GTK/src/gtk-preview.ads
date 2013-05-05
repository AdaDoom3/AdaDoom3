-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>
--  <testgtk>create_preview_color.adb</testgtk>

with Glib.Properties;
with Gdk.Color;
with Gdk.GC;
with Gdk.Visual;
with Gdk.Window;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Preview is
   pragma Obsolescent;

   type Gtk_Preview_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Preview is access all Gtk_Preview_Record'Class;

   type Gtk_Preview_Info_Record is new Gtk.Widget.Gtk_Widget_Record
     with private;
   type Gtk_Preview_Info is access all Gtk_Preview_Info_Record'Class;

   procedure Draw_Row
     (Preview : access Gtk_Preview_Record;
      Data    : Guchar_Array;
      X       : Gint;
      Y       : Gint;
      W       : Gint);
   --  The size required for Data depends of the color depth of the
   --  preview. No verification is done by Ada, everything is left to
   --  gtk. You might get some segmentation fault !
   --  for a color preview, Data'Length = W * 3  (for R, G ,B)
   --  for a grey preview, Data'Length = W;

   function Get_Cmap return Gdk.Color.Gdk_Colormap;

   function Get_Info return Gtk_Preview_Info;

   function Get_Visual return Gdk.Visual.Gdk_Visual;

   procedure Gtk_New
     (Preview  : out Gtk_Preview; The_Type : Gtk_Preview_Type);

   procedure Initialize
     (Preview  : access Gtk_Preview_Record'Class;
      The_Type : Gtk_Preview_Type);

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Preview.

   procedure Put
     (Preview : access Gtk_Preview_Record;
      Window  : Gdk.Window.Gdk_Window;
      Gc      : Gdk.GC.Gdk_GC;
      Srcx    : Gint;
      Srcy    : Gint;
      Destx   : Gint;
      Desty   : Gint;
      Width   : Gint;
      Height  : Gint);

   procedure Reset;

   procedure Set_Color_Cube
     (Nred_Shades   : Guint;
      Ngreen_Shades : Guint;
      Nblue_Shades  : Guint;
      Ngray_Shades  : Guint);

   procedure Set_Expand
     (Preview : access Gtk_Preview_Record;
      Expand  : Boolean);

   procedure Set_Gamma (Gamma : Gdouble);

   procedure Set_Install_Cmap (Install_Cmap : Gint);

   procedure Set_Reserved (Nreserved : Gint);

   procedure Size
     (Preview : access Gtk_Preview_Record;
      Width   : Gint;
      Height  : Gint);

   procedure Uninit;

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Expand_Property
   --  Type:  Boolean
   --  Descr: Whether the preview widget should take up the entire space it is
   --         allocated
   --
   --  </properties>

   Expand_Property : constant Glib.Properties.Property_Boolean;

private
   type Gtk_Preview_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   type Gtk_Preview_Info_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expand");

   pragma Import (C, Get_Visual, "gtk_preview_get_visual");
   pragma Import (C, Get_Cmap, "gtk_preview_get_cmap");
   pragma Import (C, Get_Type, "gtk_preview_get_type");
   pragma Import (C, Reset, "gtk_preview_reset");
   pragma Import (C, Set_Gamma, "gtk_preview_set_gamma");
   pragma Import (C, Set_Install_Cmap, "gtk_preview_set_install_cmap");
   pragma Import (C, Set_Reserved, "gtk_preview_set_reserved");
   pragma Import (C, Uninit, "gtk_preview_uninit");

end Gtk.Preview;

--  No binding: gtk_preview_set_dither
